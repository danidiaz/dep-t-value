{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ViewPatterns #-}

module Dep.Value.Basic
  ( 
    -- * building 'Value's
    fromResource,
    fromJSONResource,
    JSONResourceDecodeError (..),
    fromUtf8TextResource,
    TextResourceDecodeError (..),
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
import qualified Data.Aeson
import Data.Text
import Data.Text.Encoding (decodeUtf8')
import Data.Text.Encoding.Error
import Dep.Has
import Dep.Loader
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

fromResource ::
  forall r m e.
  ( Has (Loader r) m e,
    Typeable r,
    FromResource r,
    Monad m
  ) =>
  e ->
  Value r m
fromResource (dep -> loader) = Value do
  load @r loader

fromJSONResource ::
  forall r m e.
  ( Has (Loader ByteString) m e,
    Typeable r,
    FromResource r,
    Data.Aeson.FromJSON r,
    Monad m
  ) =>
  e ->
  Value r m
fromJSONResource (dep -> loader) = Value do
  bytes <- load @r loader 
  case Data.Aeson.eitherDecodeStrict' bytes of
    Left errMsg -> throw (JSONResourceDecodeError (typeRep (Proxy @r)) (resourceKey @r) errMsg)
    Right r -> pure r

data JSONResourceDecodeError = JSONResourceDecodeError TypeRep ResourceKey String deriving (Show)

instance Exception JSONResourceDecodeError

fromUtf8TextResource ::
  forall r m e.
  ( Has (Loader ByteString) m e,
    Typeable r,
    FromResource r,
    Monad m
  ) =>
  (Text -> r) ->
  e ->
  Value r m
fromUtf8TextResource ctor (dep -> loader) = Value do
  bytes <- load @r loader
  case decodeUtf8' bytes of
    Left uex -> throw (TextResourceDecodeError (typeRep (Proxy @r)) (resourceKey @r) uex)
    Right v -> pure (ctor v)

data TextResourceDecodeError = TextResourceDecodeError TypeRep ResourceKey UnicodeException deriving (Show)

instance Exception TextResourceDecodeError