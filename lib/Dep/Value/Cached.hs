{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
module Dep.Value.Cached where

import Dep.Value
import Control.Monad.IO.Class
import Control.Concurrent.MVar
import Control.Monad.IO.Unlift
import Control.Monad.Trans.Cont

type Ref v = MVar (Maybe v)

allocateCacheRef :: MonadIO m => ContT () m (Ref v)
allocateCacheRef = ContT \f -> do
    ref <- liftIO $ newMVar Nothing  
    f ref

cache :: MonadUnliftIO m => Ref v -> Value v m  -> Value v m
cache ref Value {askValue} = Value do
    run <- askRunInIO
    liftIO $ modifyMVar ref \case
        Just v -> do
            pure (Just v, v)
        Nothing -> do
            v <- run askValue
            pure (Just v, v)
