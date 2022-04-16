{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE FlexibleContexts #-}

module Dep.Resource.Loader
  ( 
    -- * Resource loader.
    Loader (..),
    ResourceKey (..),
    DatatypeName,
    ModuleName,
    load,
    ResourceNotFound (..),
    mapLoader,
    -- * Datatypes tied to resources.
    FromResource (..),
    -- * Loaders for maps of resources.
    resourceMapLoader,
    ResourceMap (..),
    resource,
    pureResource,
    loaderResource, 
    fileResource,
    envVarResource,
    -- * Loaders for resources in a directory.
    dataDirLoader,
    FileExtension,
    DataDir,
    dataDir,
    extendDataDir,
    -- * building 'Value's
    fromResource,
    fromJSONResource,
    JSONResourceDecodeError (..),
    fromUtf8TextResource,
    TextResourceDecodeError (..),
    -- * Internals.
    readFileMaybe,
    MonoidalMap (..)
  )
where

import Control.Exception (Exception, throw, throwIO)
import Control.Monad.IO.Class
import Control.Monad.Trans.Maybe
import Data.ByteString
import Data.Functor
import Data.List.Split
import Data.Map.Strict qualified as Map
import Data.Monoid
import Data.Proxy
import GHC.Generics qualified as G
import GHC.TypeLits (KnownSymbol, symbolVal)
import System.Directory (doesFileExist)
import System.FilePath
import Data.Coerce
import GHC.Generics qualified
import System.Console.GetOpt (getOpt)
import Data.Foldable qualified
import Data.Typeable
import Data.Proxy
import System.Environment (lookupEnv)
import Dep.Has
import Dep.Value
import Data.Aeson qualified 
import Data.Text
import Data.Text.Encoding (decodeUtf8')
import Data.Text.Encoding.Error
import Data.List.Split

newtype Loader v m =
   Loader { loadMaybe :: ResourceKey -> m (Maybe v) }
   deriving G.Generic

-- | Throws 'ResourceNotFound'.
load :: forall r v m . (FromResource r, Typeable r, Typeable v, Monad m) => Loader v m -> m v
load loader = do
  let key = resourceKey @r
  mb <- loadMaybe loader key
  case mb of
    Nothing -> throw $ ResourceNotFound (typeRep (Proxy @r)) key (typeRep (Proxy @v)) 
    Just b -> pure b

mapLoader :: Monad m => (ResourceKey -> v -> m w) -> Loader v m -> Loader w m
mapLoader f (Loader l) = Loader \key -> do
  mv <- l key
  case mv of
    Nothing -> do 
      pure Nothing
    Just v -> do
      w <- f key v 
      pure $ Just w

-- | The left 'Loader' is consulted first.
instance Monad m => Semigroup (Loader v m) where
  -- KnownKeysLoader l1 <> KnownKeysLoader l2 = KnownKeysLoader (l1 <> l2)
   Loader f <> Loader g = Loader \key -> do
     let Alt (MaybeT m) = (coerce f <> coerce g) key
     m

instance Monad m => Monoid (Loader v m) where
  mempty = Loader \_ -> pure Nothing

data ResourceKey = ResourceKey
  { modulePath :: [ModuleName],
    datatypeName :: DatatypeName
  }
  deriving (Show, Eq, Ord)

type DatatypeName = String

type ModuleName = String

type FileExtension = String

class FromResource a where
  resourceKey :: ResourceKey
  default resourceKey ::
    forall name mod p n nt x.
    ( G.Generic a,
      G.Rep a ~ G.D1 ('G.MetaData name mod p nt) x,
      KnownSymbol name,
      KnownSymbol mod
    ) =>
    ResourceKey
  resourceKey = ResourceKey (Data.List.Split.splitOn "." (symbolVal (Proxy @mod))) (symbolVal (Proxy @name))

data ResourceNotFound = ResourceNotFound TypeRep ResourceKey TypeRep deriving (Show)

instance Exception ResourceNotFound

resourceMapLoader :: Monad m => ResourceMap v m -> Loader v m
resourceMapLoader (ResourceMap mm) = Loader \key -> do
  let Alt (MaybeT action) =  findWithDefault mempty key mm
  action
  where
  findWithDefault :: Ord k => a -> k -> MonoidalMap k a -> a
  findWithDefault def k = Map.findWithDefault def k . getMonoidalMap
  {-# INLINE findWithDefault #-}

resource :: forall r v m. FromResource r => m (Maybe v) -> ResourceMap v m
resource action = do
  let key = resourceKey @r
  ResourceMap $ singleton key (Alt (MaybeT action))
  where 
  singleton :: k -> a -> MonoidalMap k a
  singleton k a = MonoidalMap $ Map.singleton k a

pureResource :: forall r v m. (Applicative m, FromResource r) => v -> ResourceMap v m
pureResource v = resource @r @v (pure $ Just v)

-- | Pick a resource from an existing 'Loader', usually for the purpose of
-- giving it special treatment.
loaderResource :: forall r v m . FromResource r => Loader v m -> ResourceMap v m
loaderResource loader = do
  resource @r (loadMaybe loader (resourceKey @r))

fileResource :: forall r m. (FromResource r, MonadIO m) => FilePath -> ResourceMap ByteString m
fileResource path = do
  resource @r (readFileMaybe path)

envVarResource :: forall r m . (FromResource r, MonadIO m) => String -> ResourceMap String m
envVarResource varName = do
  resource @r (liftIO $ lookupEnv varName)

-- | Function that completes a relative `FilePath` pointing to a data file,
-- and returns its absolute path.
--
-- The @getDataFileName@ function from @Paths_pkgname@ is a valid 'DataDir'.
-- You can also create a 'DataDir' by using 'dataDir'.
type DataDir = FilePath -> IO FilePath

-- | Build a 'DataDir' out of a base directory path.
dataDir :: FilePath -> DataDir
dataDir dirPath filePath = pure (dirPath </> filePath)

-- | Given a relative path to a subdirectory of a 'DataDir', return a 'DataDir'
-- that completes paths within that subdirectory.
extendDataDir :: DataDir -> FilePath -> DataDir
extendDataDir dataDir relDir filePath = dataDir (relDir </> filePath)

dataDirLoader :: MonadIO m => [FileExtension] -> DataDir -> Loader ByteString m
dataDirLoader extensions base = Loader \ResourceKey {modulePath, datatypeName} -> do
  let go [] = do 
        pure Nothing
      go (ext : exts) = do
        let relative = joinPath modulePath </> addExtension datatypeName ext
        absolute <- liftIO $ base relative
        mbytes <- readFileMaybe absolute
        case mbytes of
          Just bs -> pure $ Just bs
          Nothing -> go exts
  go extensions

readFileMaybe :: MonadIO m => FilePath -> m (Maybe ByteString)
readFileMaybe absolute = do
  exists <- liftIO (doesFileExist absolute)
  if not exists
    then do
      pure Nothing
    else do
      bytes <- liftIO $ Data.ByteString.readFile absolute
      pure (Just bytes)

newtype ResourceMap v m = ResourceMap (MonoidalMap ResourceKey (Alt (MaybeT m) v))

-- | Entries in the left map are consulted first.
deriving newtype instance Monad m => Semigroup (ResourceMap v m)
deriving newtype instance Monad m => Monoid (ResourceMap v m)

newtype MonoidalMap k a = MonoidalMap {getMonoidalMap :: Map.Map k a}
  deriving (Show, Read)

instance (Ord k, Semigroup a) => Semigroup (MonoidalMap k a) where
  MonoidalMap a <> MonoidalMap b = MonoidalMap $ Map.unionWith (<>) a b
  {-# INLINE (<>) #-}

instance (Ord k, Semigroup a) => Monoid (MonoidalMap k a) where
  mempty = MonoidalMap mempty
  {-# INLINE mempty #-}


fromResource ::
  forall r m e.
  ( Has (Loader r) m e,
    Typeable r,
    FromResource r,
    Monad m
  ) =>
  e ->
  Value r m
fromResource (dep -> loader) = Value do
  load @r loader

fromJSONResource ::
  forall r m e.
  ( Has (Loader ByteString) m e,
    Typeable r,
    FromResource r,
    Data.Aeson.FromJSON r,
    Monad m
  ) =>
  e ->
  Value r m
fromJSONResource (dep -> loader) = Value do
  bytes <- load @r loader 
  case Data.Aeson.eitherDecodeStrict' bytes of
    Left errMsg -> throw (JSONResourceDecodeError (typeRep (Proxy @r)) (resourceKey @r) errMsg)
    Right r -> pure r

data JSONResourceDecodeError = JSONResourceDecodeError TypeRep ResourceKey String deriving (Show)

instance Exception JSONResourceDecodeError

fromUtf8TextResource ::
  forall r m e.
  ( Has (Loader ByteString) m e,
    Typeable r,
    FromResource r,
    Monad m
  ) =>
  (Text -> r) ->
  e ->
  Value r m
fromUtf8TextResource ctor (dep -> loader) = Value do
  bytes <- load @r loader
  case decodeUtf8' bytes of
    Left uex -> throw (TextResourceDecodeError (typeRep (Proxy @r)) (resourceKey @r) uex)
    Right v -> pure (ctor v)

data TextResourceDecodeError = TextResourceDecodeError TypeRep ResourceKey UnicodeException deriving (Show)

instance Exception TextResourceDecodeError
