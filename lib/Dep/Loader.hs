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
{-# LANGUAGE DeriveFunctor #-}

-- | Define resource 'Loader's and wire them into 'Value's.
--
-- Typically, this module only needs to be imported when creating the global
-- application environment.
module Dep.Loader
  ( 
    -- * Resource loader.
    Loader (..),
    ResourceKey (..),
    DatatypeName,
    ModuleName,
    load,
    ResourceNotFound (..),
    -- * Datatypes tied to resources.
    FromResource (..),
    -- * Loaders for resources in a directory.
    dataDirLoader,
    FileExtension,
    DataDir,
    dataDir,
    extendDataDir,
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

-- | Class for datatypes tied to resources.
--
-- Derive it with @DeriveAnyClass@. The datatype must have a "GHC.Generics.Generic" instance.
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

-- | Function that completes a relative `FilePath` pointing to a data file,
-- and returns its absolute path.
--
-- The [@getDataFileName@ function from @Paths_pkgname@](https://cabal.readthedocs.io/en/latest/cabal-package.html#accessing-data-files-from-package-code) is a valid 'DataDir'.
-- You can also create a 'DataDir' by using 'dataDir'.
type DataDir = FilePath -> IO FilePath

-- | Build a 'DataDir' out of a base directory path.
dataDir :: FilePath -> DataDir
dataDir dirPath filePath = pure (dirPath </> filePath)

-- | Given a relative path to a subdirectory of a 'DataDir', return a 'DataDir'
-- that completes paths within that subdirectory.
extendDataDir :: DataDir -> FilePath -> DataDir
extendDataDir dataDir relDir filePath = dataDir (relDir </> filePath)

-- | A @dataDirLoader ["js", "json"] (dataDir "conf")@ 'Loader' will, for a datatype @Baz@ defined
-- in module @Foo.Bar@, look for the files @conf\/Foo\/Bar\/Baz.js@ and @conf\/Foo\/Bar\/Baz.json@,
-- in that order.
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
  where
  readFileMaybe :: MonadIO m => FilePath -> m (Maybe ByteString)
  readFileMaybe absolute = do
    exists <- liftIO (doesFileExist absolute)
    if not exists
      then do
        pure Nothing
      else do
        bytes <- liftIO $ Data.ByteString.readFile absolute
        pure (Just bytes)
