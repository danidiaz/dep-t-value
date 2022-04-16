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
    json,
    JSONResourceDecodeError (..),

    -- * text resources
    text,
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

json ::
  forall m e v.
  ( Has Loader m e,
    Monad m,
    FromResource v,
    Data.Aeson.FromJSON v
  ) =>
  v ->
  e ->
  Value v m
json ctor (dep -> loader) = Value do
  bytes <- load @v loader 
  case Data.Aeson.eitherDecodeStrict' bytes of
    Left errMsg -> throw (JSONResourceDecodeError (resourceKey @v) errMsg)
    Right v -> pure v

data JSONResourceDecodeError = JSONResourceDecodeError ResourceKey String deriving (Show)

instance Exception JSONResourceDecodeError

text ::
  forall m e v.
  ( Has Loader m e,
    Monad m,
    FromResource v
  ) =>
  (Text -> v) ->
  e ->
  Value v m
text ctor (dep -> loader) = Value do
  bytes <- load @v loader 
  case decodeUtf8' bytes of
    Left uex -> throw (TextResourceDecodeError (resourceKey @v) uex)
    Right v -> pure (ctor v)

data TextResourceDecodeError = TextResourceDecodeError ResourceKey UnicodeException deriving (Show)

instance Exception TextResourceDecodeError