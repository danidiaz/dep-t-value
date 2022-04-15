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

data Loader m
  = KnownKeysLoader (MonoidalMap (ResourceKey, FileExtension) (Alt (MaybeT m) ByteString))
  | Loader (ResourceKey -> FileExtension -> Alt (MaybeT m) ByteString)

loadMaybe :: Monad m => Loader m -> ResourceKey -> FileExtension -> m (Maybe ByteString)
loadMaybe loader key ext = do
  let Alt (MaybeT action) = loadMaybe' loader key ext
  action

loadMaybe' :: Monad m => Loader m -> ResourceKey -> FileExtension -> Alt (MaybeT m) ByteString
loadMaybe' loader key ext = case loader of
  KnownKeysLoader mm -> do
    Dep.Loader.Internal.findWithDefault mempty (key, ext) mm
  Loader f -> do
    f key ext

load :: Monad m => Loader m -> ResourceKey -> FileExtension -> m ByteString
load loader key ext = do
  mb <- loadMaybe loader key ext
  case mb of
    Nothing -> throw (ResourceNotFound key ext)
    Just b -> pure b

-- | The left 'Loader' is consulted first.
instance Monad m => Semigroup (Loader m) where
  KnownKeysLoader l1 <> KnownKeysLoader l2 = KnownKeysLoader (l1 <> l2)
  l1 <> l2 = Loader (loadMaybe' l1 <> loadMaybe' l2)

instance Monad m => Monoid (Loader m) where
  mempty = KnownKeysLoader mempty

data ResourceKey = ResourceKey
  { modulePath :: [ModuleName],
    datatypeName :: DatatypeName
  }
  deriving (Show, Eq, Ord)

type DatatypeName = String

type ModuleName = String

type FileExtension = String

class IsResource a where
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

file :: forall r m. (IsResource r, MonadIO m) => FilePath -> Loader m
file path = do
  let key = resourceKey @r
      ext = takeExtension path
  KnownKeysLoader $ Dep.Loader.Internal.singleton (key, ext) (Alt (MaybeT (readFileMaybe path)))

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

fromDir :: MonadIO m => DataDir -> Loader m
fromDir base = Loader \ResourceKey {modulePath, datatypeName} fileExt -> do
  let relative = joinPath modulePath </> addExtension datatypeName fileExt
  Alt
    ( MaybeT
        ( do
            absolute <- liftIO $ base relative
            readFileMaybe absolute
        )
    )

readFileMaybe :: MonadIO m => FilePath -> m (Maybe ByteString)
readFileMaybe absolute = do
  exists <- liftIO (doesFileExist absolute)
  if not exists
    then do
      pure Nothing
    else do
      bytes <- liftIO $ Data.ByteString.readFile absolute
      pure (Just bytes)

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
