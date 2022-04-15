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
    ResourceNotFound (..),
    -- * Load resources by specifying routes.
    fromRoutes,
    ResourceRoute(..),
    RouteDescription,
    mandatory,
    MandatoryResourceMissing(..),
    fileRoute,
    -- * Load resources by following the module structure.
    fromDataDir,
    DataDir,
    dataDir,
    extendDataDir
  )
where

import Control.Exception (Exception, throw, throwIO)
import Data.List.Split
import Data.Monoid
import Data.Proxy
import GHC.Generics qualified as G
import GHC.TypeLits (KnownSymbol, symbolVal)
import System.FilePath
import Control.Monad.IO.Class
import System.Directory ( doesFileExist )
import Data.Functor
import Data.Map.Strict
import Data.ByteString
import Control.Monad.Trans.Maybe

newtype Loader m = Loader
  { loadMaybe :: ResourceKey -> FileExtension -> m (Maybe ByteString)
  }
  deriving (G.Generic)

load :: Monad m => Loader m -> ResourceKey -> FileExtension -> m ByteString
load loader key ext = do
  mb <- loadMaybe loader key ext
  case mb of
    Nothing -> throw (ResourceNotFound key ext)
    Just b -> pure b

-- | The left 'Loader' is consulted first.
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

data ResourceNotFound = ResourceNotFound ResourceKey FileExtension deriving (Show)

instance Exception ResourceNotFound

type RouteDescription = String

data ResourceRoute = ResourceRoute ResourceKey FileExtension (IO (Maybe ByteString)) RouteDescription

fromRoutes :: MonadIO m => [ResourceRoute] -> Loader m
fromRoutes routes = do
  let routes' :: [((ResourceKey,FileExtension), IO (Maybe ByteString))] 
      routes' = routes <&> \(ResourceRoute key ext action _) -> ((key,ext), action)
  let routeMap = Data.Map.Strict.fromList routes'
  Loader \key ext -> do
    case Data.Map.Strict.lookup (key,ext) routeMap of
      Nothing -> do
        pure Nothing
      Just action -> do
        liftIO action

data MandatoryResourceMissing = MandatoryResourceMissing ResourceKey FileExtension RouteDescription deriving (Show)

instance Exception MandatoryResourceMissing

mandatory :: ResourceRoute -> ResourceRoute
mandatory (ResourceRoute key ext action description) = do
  let action' = do
        mbytes <- action
        case mbytes of
          Nothing -> do
            throwIO (MandatoryResourceMissing key ext description)
          Just bytes -> do 
            pure (Just bytes)
  ResourceRoute key ext action' description

fileRoute :: forall r. IsResource r => FilePath -> ResourceRoute
fileRoute path = do
  let key = resourceKey @r
      ext = takeExtension path
      description = "File route: " ++ path
  ResourceRoute key ext (readFileMaybe path) description

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

fromDataDir :: MonadIO m => DataDir -> Loader m
fromDataDir base = Loader \ResourceKey {modulePath,datatypeName} fileExt -> liftIO do
    let relative = joinPath modulePath </> addExtension datatypeName fileExt
    absolute <- base relative
    readFileMaybe absolute

readFileMaybe :: FilePath -> IO (Maybe ByteString)
readFileMaybe absolute = do
    exists <- doesFileExist absolute
    if exists 
        then pure Nothing
        else Just <$> Data.ByteString.readFile absolute
