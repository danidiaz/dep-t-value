module Dep.Loader
  ( 
    -- * Resource loader.
    Loader (..),
    ResourceKey,
    load,
    ResourceNotFound (..),
    -- * Datatypes tied to resources.
    FromResource (..),
    -- * Loaders for known key sets.
    resourceMapLoader,
    ResourceMap,
    pick, 
    pickMany,
    file,
    -- * Loaders for resources in a directory.
    dataDirLoader,
    FileExtension,
    DataDir,
    dataDir,
    extendDataDir,
  )
where

import Dep.Loader.Internal
