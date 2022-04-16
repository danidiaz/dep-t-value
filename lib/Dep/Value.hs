{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
module Dep.Value (
       -- * A provider of values.
        Value (..)
    ) where
import GHC.Generics qualified as G

-- | Typically injected into program logic using "Dep.Has.Has".
newtype Value v m = Value { 
        value :: m v 
    } deriving G.Generic
