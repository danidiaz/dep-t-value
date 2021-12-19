{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ExplicitForAll #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Dep.Loader where

import GHC.Generics qualified as G
import Data.ByteString 
import Data.Monoid
import GHC.TypeLits (KnownSymbol, symbolVal)
import Data.Proxy
import Data.List.Split
import Control.Exception ( Exception, throw )

type DatatypeName = String

type ModuleName = String

data ResourceKey = ResourceKey [ModuleName] DatatypeName deriving (Show, Eq, Ord)

newtype Loader m = Loader {
        loadE :: ResourceKey -> m (Maybe ByteString)
    } deriving G.Generic

instance Monad m => Semigroup (Loader m) where
  l1 <> l2 = Loader \resourceKey -> do
    mb <- loadE l1 resourceKey
    case mb of 
      Nothing -> loadE l2 resourceKey
      Just b -> pure (Just b)

instance Monad m => Monoid (Loader m) where  
    mempty = Loader \_ -> pure Nothing

load :: Monad m => Loader m -> ResourceKey -> m (Maybe ByteString)
load loader key = do
    mb <- loadE loader key
    case mb of 
      Nothing -> throw (ResourceMissing key)
      Just b -> pure (Just b)

resourceKey :: forall a name mod p nt x . (G.Generic a, G.Rep a ~ G.D1 ('G.MetaData name mod p nt) x, KnownSymbol name, KnownSymbol mod)  => ResourceKey
resourceKey = ResourceKey (splitOn "." (symbolVal (Proxy @mod))) (symbolVal (Proxy @name))

newtype ResourceMissing = ResourceMissing ResourceKey deriving (Show)

instance Exception ResourceMissing 


