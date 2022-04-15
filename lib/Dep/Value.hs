{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
module Dep.Value where
import GHC.Generics qualified as G

newtype Value v m = Value { 
        value :: m v 
    } deriving G.Generic
