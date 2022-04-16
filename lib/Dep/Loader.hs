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
    -- * Loaders for maps of resources.
    resourceMapLoader,
    ResourceMap,
    resource,
    loaderResource, 
    fileResource,
    -- * Loaders for resources in a directory.
    dataDirLoader,
    FileExtension,
    DataDir,
    dataDir,
    extendDataDir,
  )
where

import Dep.Loader.Internal
