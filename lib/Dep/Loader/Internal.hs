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

newtype Loader m =
--  = KnownKeysLoader (MonoidalMap (ResourceKey, FileExtension) (Alt (MaybeT m) ByteString))
   Loader { loadMaybe :: ResourceKey -> FileExtension -> m (Maybe ByteString) }

load :: Monad m => Loader m -> ResourceKey -> FileExtension -> m ByteString
load loader key ext = do
  mb <- loadMaybe loader key ext
  case mb of
    Nothing -> throw (ResourceNotFound key ext)
    Just b -> pure b

-- | The left 'Loader' is consulted first.
instance Monad m => Semigroup (Loader m) where
  -- KnownKeysLoader l1 <> KnownKeysLoader l2 = KnownKeysLoader (l1 <> l2)
   Loader f <> Loader g = Loader \key ext -> do
     let Alt (MaybeT m) = (coerce f <> coerce g) key ext
     m

instance Monad m => Monoid (Loader m) where
  mempty = Loader \_ _ -> pure Nothing

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

data ResourceNotFound = ResourceNotFound ResourceKey FileExtension deriving (Show)

instance Exception ResourceNotFound

resourceMapLoader :: Monad m => ResourceMap m -> Loader m
resourceMapLoader rmap = Loader \key ext -> do
  let Alt (MaybeT action) =  Dep.Loader.Internal.findWithDefault mempty (key, ext) rmap
  action

fileResource :: forall r m. (FromResource r, MonadIO m) => FilePath -> ResourceMap m
fileResource path = do
  let key = resourceKey @r
      ext = takeExtension path
  Dep.Loader.Internal.singleton (key, ext) (Alt (MaybeT (readFileMaybe path)))

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

dataDirLoader :: MonadIO m => DataDir -> Loader m
dataDirLoader base = Loader \ResourceKey {modulePath, datatypeName} fileExt -> do
  let relative = joinPath modulePath </> addExtension datatypeName fileExt
  do
     absolute <- liftIO $ base relative
     readFileMaybe absolute

readFileMaybe :: MonadIO m => FilePath -> m (Maybe ByteString)
readFileMaybe absolute = do
  exists <- liftIO (doesFileExist absolute)
  if not exists
    then do
      pure Nothing
    else do
      bytes <- liftIO $ Data.ByteString.readFile absolute
      pure (Just bytes)

type ResourceMap m = MonoidalMap (ResourceKey, FileExtension) (Alt (MaybeT m) ByteString)

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
