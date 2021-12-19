{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE DeriveGeneric #-}
module Dep.Loader where

import GHC.Generics qualified as G
import Data.ByteString 

newtype Loader m = Loader {
        loadE :: m (Maybe ByteString)
    } deriving G.Generic
