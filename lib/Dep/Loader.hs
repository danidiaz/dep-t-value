{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

module Dep.Loader
  ( Loader (..),
    load,
    ResourceKey (..),
    resourceKey,
    DatatypeName,
    ModuleName,
    FileExtension,
    ResourceMissing (..),
  )
where

import Control.Exception (Exception, throw)
import Data.ByteString (ByteString)
import Data.List.Split
import Data.Monoid
import Data.Proxy
import GHC.Generics qualified as G
import GHC.TypeLits (KnownSymbol, symbolVal)

newtype Loader m = Loader
  { loadMaybe :: ResourceKey -> FileExtension -> m (Maybe ByteString)
  }
  deriving (G.Generic)

load :: Monad m => Loader m -> ResourceKey -> FileExtension -> m (Maybe ByteString)
load loader key ext = do
  mb <- loadMaybe loader key ext
  case mb of
    Nothing -> throw (ResourceMissing key ext)
    Just b -> pure (Just b)

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

resourceKey ::
  forall a name mod p nt x.
  ( G.Generic a,
    G.Rep a ~ G.D1 ('G.MetaData name mod p nt) x,
    KnownSymbol name,
    KnownSymbol mod
  ) =>
  ResourceKey
resourceKey = ResourceKey (splitOn "." (symbolVal (Proxy @mod))) (symbolVal (Proxy @name))

data ResourceMissing = ResourceMissing ResourceKey FileExtension deriving (Show)

instance Exception ResourceMissing
