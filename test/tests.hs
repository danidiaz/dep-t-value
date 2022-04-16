{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
module Main where

import Test.Tasty
import Test.Tasty.HUnit

import Dep.Env
import Dep.Value
import Dep.Value.Basic
import Dep.Loader
import Data.Functor.Identity
import Data.Text
import Data.ByteString
import GHC.Generics qualified as G

-- Orphan instance, just for tests.
deriving anyclass instance FromResource (Identity Text)

tests :: TestTree
tests =
  testGroup
    "All"
    [    
        testCase "loadUtf8" textResourceLoads
    ]

textResourceLoads :: Assertion
textResourceLoads = do
    let loader :: Loader ByteString IO 
        loader = dataDirLoader ["txt"] $ dataDir "test/conf/" 
        v = fromUtf8TextResource Identity (loader `addDep` emptyEnv)
    Identity txt <- value v
    assertEqual "text loaded correctly" (Data.Text.pack "Lorem Ipsum") txt


main :: IO ()
main = defaultMain tests
