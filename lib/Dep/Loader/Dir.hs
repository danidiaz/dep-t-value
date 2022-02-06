{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE NamedFieldPuns #-}
module Dep.Loader.Dir (
    make,
    DataDir,
    dataDir,
    extendDataDir
) where

import Dep.Loader
import System.FilePath
import Control.Monad.IO.Class
import System.Directory

import Data.ByteString

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

make :: MonadIO m => DataDir -> Loader m
make dataDir = Loader \ResourceKey {modulePath,datatypeName} fileExt -> liftIO do
    let relative = joinPath modulePath </> addExtension datatypeName fileExt
    absolute <- dataDir relative
    exists <- doesFileExist absolute
    if exists 
        then pure Nothing
        else Just <$> Data.ByteString.readFile absolute
