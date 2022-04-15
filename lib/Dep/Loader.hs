{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE NamedFieldPuns #-}

module Dep.Loader
  ( Loader (..),
    load,
    IsResource (..),
    ResourceKey (..),
    DatatypeName,
    ModuleName,
    FileExtension,
    ResourceMissing (..),
    -- * Load resource from directory following the module structure.
    dir,
    DataDir,
    base,
    extendBase
  )
where

import Control.Exception (Exception, throw)
import Data.List.Split
import Data.Monoid
import Data.Proxy
import GHC.Generics qualified as G
import GHC.TypeLits (KnownSymbol, symbolVal)
import System.FilePath
import Control.Monad.IO.Class
import System.Directory ( doesFileExist )

import Data.ByteString

newtype Loader m = Loader
  { loadMaybe :: ResourceKey -> FileExtension -> m (Maybe ByteString)
  }
  deriving (G.Generic)

load :: Monad m => Loader m -> ResourceKey -> FileExtension -> m ByteString
load loader key ext = do
  mb <- loadMaybe loader key ext
  case mb of
    Nothing -> throw (ResourceMissing key ext)
    Just b -> pure b

instance Monad m => Semigroup (Loader m) where
  l1 <> l2 = Loader \resourceKey ext -> do
    mb <- loadMaybe l1 resourceKey ext
    case mb of
      Nothing -> loadMaybe l2 resourceKey ext
      Just b -> pure (Just b)

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

class IsResource a where
  resourceKey :: ResourceKey
  default resourceKey :: forall a name mod p n nt x.
      ( G.Generic a,
        G.Rep a ~ G.D1 ('G.MetaData name mod p nt) x,
        KnownSymbol name,
        KnownSymbol mod
      ) => ResourceKey 
  resourceKey = ResourceKey (splitOn "." (symbolVal (Proxy @mod))) (symbolVal (Proxy @name))

data ResourceMissing = ResourceMissing ResourceKey FileExtension deriving (Show)

instance Exception ResourceMissing

-- | Function that completes a relative `FilePath` pointing to a data file, 
-- and returns its absolute path.
--
-- The @getDataFileName@ function from @Paths_pkgname@ is a valid 'DataDir'. 
-- You can also create a 'DataDir' by using 'dataDir'.
type DataDir = FilePath -> IO FilePath

-- | Build a 'DataDir' out of a base directory path.
base :: FilePath -> DataDir
base dirPath filePath = pure (dirPath </> filePath)

-- | Given a relative path to a subdirectory of a 'DataDir', return a 'DataDir' 
-- that completes paths within that subdirectory.
extendBase :: FilePath -> DataDir -> DataDir
extendBase relDir dataDir filePath = dataDir (relDir </> filePath)

dir :: MonadIO m => DataDir -> Loader m
dir dataDir = Loader \ResourceKey {modulePath,datatypeName} fileExt -> liftIO do
    let relative = joinPath modulePath </> addExtension datatypeName fileExt
    absolute <- dataDir relative
    exists <- doesFileExist absolute
    if exists 
        then pure Nothing
        else Just <$> Data.ByteString.readFile absolute
