{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
module Dep.Value (
       -- * A provider of values.
        Value (..)
    ) where
import GHC.Generics qualified as G

newtype Value v m = Value { 
        value :: m v 
    } deriving G.Generic
