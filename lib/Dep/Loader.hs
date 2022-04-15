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
    IsResource (..),
    -- * Designate specific files as resources.
    file,
    -- * Search for resources in a directory.
    fromDir,
    DataDir,
    dataDir,
    extendDataDir,
  )
where

import Dep.Loader.Internal
