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
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Main where

import Test.Tasty
import Test.Tasty.HUnit

import Dep.Env
import Dep.Value
import Dep.Value.Cached
import Dep.Value.JSON
import Dep.Value.Text
import Dep.Loader
import Data.Functor.Identity
import Data.Text
import Data.ByteString
import GHC.Generics qualified as G
import Data.IORef
import Control.Exception
import Control.Monad.Trans.Cont

-- Orphan instance, just for tests.
deriving anyclass instance FromResource (Identity Text)

tests :: TestTree
tests =
  testGroup
    "All"
    [    
        testCase "loadUtf8" textResourceLoads,
        testCase "loadUtf8Precedence" textResourcePrecedence,
        testCase "valueIsCached" valueIsCached
    ]

textResourceLoads :: Assertion
textResourceLoads = do
    let loader :: Loader ByteString IO 
        loader = dataDirLoader ["zzz","txt"] $ dataDir "test" `extendDataDir` "conf"
        v = Dep.Value.Text.fromUtf8 Identity (load @(Identity Text) loader)
    Identity txt <- value v
    assertEqual "text loaded correctly" (Data.Text.pack "Lorem Ipsum") txt

textResourcePrecedence :: Assertion
textResourcePrecedence = do
    let loader :: Loader ByteString IO 
        loader = dataDirLoader ["zzz","txt"] (dataDir "test" `extendDataDir` "conf2")
                 <> dataDirLoader ["zzz","txt"] (dataDir "test" `extendDataDir` "conf")
        v = Dep.Value.Text.fromUtf8 Identity (load @(Identity Text) loader)
    Identity txt <- value v
    assertEqual "text loaded correctly" (Data.Text.pack "alternative text") txt

valueIsCached :: Assertion
valueIsCached = do
  let bombs = pure () : repeat (throwIO $ userError "boom!")
  bombsRef <- newIORef @[IO ()] bombs
  let attempt = do
        action <- atomicModifyIORef bombsRef \(b : bs) -> (bs, b)
        action
  runContT allocateRef \valueRef -> do
    let v :: Value () IO = Dep.Value.Cached.cache valueRef $ Value attempt
    () <- value v
    () <- value v
    pure ()

main :: IO ()
main = defaultMain tests
