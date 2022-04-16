{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ViewPatterns #-}

module Dep.Value.Basic
  ( -- * cache a value
    Ref,
    allocateRef,
    cache,

    -- * JSON resources
    fromJSONResource,
    JSONResourceDecodeError (..),

    -- * text resources
    fromUtf8TextResource,
    TextResourceDecodeError (..),
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
  forall v m e.
  ( Has (Loader v) m e,
    FromResource v,
    Monad m
  ) =>
  e ->
  Value v m
fromResource (dep -> loader) = Value do
  load loader (resourceKey @v)

fromJSONResource ::
  forall v m e.
  ( Has (Loader ByteString) m e,
    FromResource v,
    Data.Aeson.FromJSON v,
    Monad m
  ) =>
  e ->
  Value v m
fromJSONResource (dep -> loader) = Value do
  bytes <- load loader (resourceKey @v)
  case Data.Aeson.eitherDecodeStrict' bytes of
    Left errMsg -> throw (JSONResourceDecodeError (resourceKey @v) errMsg)
    Right v -> pure v

data JSONResourceDecodeError = JSONResourceDecodeError ResourceKey String deriving (Show)

instance Exception JSONResourceDecodeError

fromUtf8TextResource ::
  forall v m e.
  ( Has (Loader ByteString) m e,
    FromResource v,
    Monad m
  ) =>
  (Text -> v) ->
  e ->
  Value v m
fromUtf8TextResource ctor (dep -> loader) = Value do
  bytes <- load loader (resourceKey @v)
  case decodeUtf8' bytes of
    Left uex -> throw (TextResourceDecodeError (resourceKey @v) uex)
    Right v -> pure (ctor v)

data TextResourceDecodeError = TextResourceDecodeError ResourceKey UnicodeException deriving (Show)

instance Exception TextResourceDecodeError