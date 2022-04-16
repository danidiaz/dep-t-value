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

module Dep.Loader.Internal where

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

newtype Loader v m =
--  = KnownKeysLoader (MonoidalMap (ResourceKey, FileExtension) (Alt (MaybeT m) ByteString))
   Loader { loadMaybe :: ResourceKey -> m (Maybe v) }
   deriving G.Generic

load :: Monad m => Loader v m -> ResourceKey -> m v
load loader key = do
  mb <- loadMaybe loader key
  case mb of
    Nothing -> throw $ ResourceNotFound key
    Just b -> pure b

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
    forall a name mod p n nt x.
    ( G.Generic a,
      G.Rep a ~ G.D1 ('G.MetaData name mod p nt) x,
      KnownSymbol name,
      KnownSymbol mod
    ) =>
    ResourceKey
  resourceKey = ResourceKey (splitOn "." (symbolVal (Proxy @mod))) (symbolVal (Proxy @name))

newtype ResourceNotFound = ResourceNotFound ResourceKey deriving (Show)

instance Exception ResourceNotFound

resourceMapLoader :: Monad m => ResourceMap v m -> Loader v m
resourceMapLoader (ResourceMap mm) = Loader \key -> do
  let Alt (MaybeT action) =  Dep.Loader.Internal.findWithDefault mempty key mm
  action

resource :: forall r v m. FromResource r => m (Maybe v) -> ResourceMap v m
resource action = do
  let key = resourceKey @r
  ResourceMap $ Dep.Loader.Internal.singleton key (Alt (MaybeT action))

loaderResource :: forall r v m . FromResource r => Loader v m -> ResourceMap v m
loaderResource loader = do
  let key = resourceKey @r
  ResourceMap $ Dep.Loader.Internal.singleton key (Alt (MaybeT (loadMaybe loader key)))

fileResource :: forall r m. (FromResource r, MonadIO m) => FilePath -> ResourceMap ByteString m
fileResource path = do
  let key = resourceKey @r
  ResourceMap $ Dep.Loader.Internal.singleton key (Alt (MaybeT (readFileMaybe path)))

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
extendDataDir :: FilePath -> DataDir -> DataDir
extendDataDir relDir dataDir filePath = dataDir (relDir </> filePath)

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

singleton :: k -> a -> MonoidalMap k a
singleton k a = MonoidalMap $ Map.singleton k a

findWithDefault :: Ord k => a -> k -> MonoidalMap k a -> a
findWithDefault def k = Map.findWithDefault def k . getMonoidalMap
{-# INLINE findWithDefault #-}
