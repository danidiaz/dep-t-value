{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ViewPatterns #-}

module Dep.Value.Cached
  ( 
    -- * caching 'Value's
    Ref,
    allocateRef,
    cache,
  )
where

import Control.Concurrent.MVar
import Control.Exception
import Control.Monad.IO.Class
import Control.Monad.IO.Unlift
import Control.Monad.Trans.Cont
import Dep.Has
import Dep.Resource.Loader
import Dep.Value
import Data.ByteString
import Data.Typeable
import Data.Proxy

type Ref v = MVar (Maybe v)

allocateRef :: MonadIO m => ContT () m (Ref v)
allocateRef = ContT \f -> do
  ref <- liftIO $ newMVar Nothing
  f ref

cache :: MonadUnliftIO m => Ref v -> Value v m -> Value v m
cache ref Value {value} = Value do
  run <- askRunInIO
  liftIO $ modifyMVar ref \case
    Just v -> do
      pure (Just v, v)
    Nothing -> do
      v <- run value
      pure (Just v, v)
