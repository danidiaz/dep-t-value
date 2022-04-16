module Dep.Loader
  ( 
    -- * Resource loader.
    Loader (..),
    ResourceKey,
    load,
    ResourceNotFound (..),
    -- * Datatypes tied to resources.
    FromResource (..),
    -- * Designate specific files as resources.
    resourceMapLoader,
    ResourceMap,
    pick, 
    pickMany,
    file,
    -- * Search for resources in a directory.
    dataDirLoader,
    FileExtension,
    DataDir,
    dataDir,
    extendDataDir,
  )
where

import Dep.Loader.Internal
