{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE DeriveGeneric #-}
module Dep.Value where

import GHC.Generics qualified as G

newtype Value v m = Value { 
        askValue :: m v 
    } deriving G.Generic

