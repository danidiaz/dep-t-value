module Dep.Loader
  ( 
    -- * Resource loader.
    Loader,
    ResourceKey,
    FileExtension,
    loadMaybe,
    load,
    ResourceNotFound (..),
    -- * Datatypes tied to resources.
    FromResource (..),
    -- * Designate specific files as resources.
    resourceMapLoader,
    ResourceMap,
    fileResource,
    -- * Search for resources in a directory.
    dataDirLoader,
    DataDir,
    dataDir,
    extendDataDir,
  )
where

import Dep.Loader.Internal
