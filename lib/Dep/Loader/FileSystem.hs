{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE NamedFieldPuns #-}
module Dep.Loader.FileSystem where

import Control.Monad.IO.Class
import Dep.Loader
import System.FilePath

make :: MonadIO m => [String] -> FilePath -> Loader m
make extensions root = Loader \ResourceKey {modulePath, datatypeName} -> do
    let candidates = do
            extension <- extensions
            pure $ joinPath $ [root] ++ modulePath ++ [addExtension datatypeName extension]
    undefined 
 