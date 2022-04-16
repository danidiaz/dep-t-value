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
{-# LANGUAGE TypeApplications #-}
module Main where

import Test.Tasty
import Test.Tasty.HUnit

import Dep.Env
import Dep.Value
import Dep.Value.Cached
import Dep.Resource.Loader
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
        testCase "loadUtf8" textResourceLoads,
        testCase "loadUtf8Precedencie" textResourcePrecedence
    ]

textResourceLoads :: Assertion
textResourceLoads = do
    let loader :: Loader ByteString IO 
        loader = dataDirLoader ["zzz","txt"] $ dataDir "test" `extendDataDir` "conf"
        v = fromUtf8TextResource Identity (loader `addDep` emptyEnv)
    Identity txt <- value v
    assertEqual "text loaded correctly" (Data.Text.pack "Lorem Ipsum") txt

textResourcePrecedence :: Assertion
textResourcePrecedence = do
    let loader :: Loader ByteString IO 
        loader = dataDirLoader ["zzz","txt"] $ dataDir "test" `extendDataDir` "conf"
        loader2 = resourceMapLoader 
                        (fileResource @(Identity Text) "test/conf/Identity2.txt"  
                            <> fileResource @(Identity Text) "test/conf/Identity3.txt")
                  <> loader
        v = fromUtf8TextResource Identity (loader2 `addDep` emptyEnv)
    Identity txt <- value v
    assertEqual "text loaded correctly" (Data.Text.pack "foo") txt


main :: IO ()
main = defaultMain tests
