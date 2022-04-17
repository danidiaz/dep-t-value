{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE TypeApplications #-}

module Dep.Value.Text (
    -- * Construct 'Text' 'Value's.
    fromUtf8,
    TextValueDecodeError (..)
) where

import Data.Typeable
import Data.ByteString
import Dep.Value
import Control.Exception
import Data.Text
import Data.Text.Encoding (decodeUtf8')
import Data.Text.Encoding.Error

fromUtf8 ::
  forall v m.
  ( 
    Typeable v,
    Monad m
  ) =>
  (Text -> v) ->
  m ByteString ->
  Value v m
fromUtf8 ctor action = Value do
  bytes <- action
  case decodeUtf8' bytes of
    Left uex -> throw (TextValueDecodeError (typeRep (Proxy @v)) uex)
    Right v -> pure (ctor v)

data TextValueDecodeError = TextValueDecodeError TypeRep UnicodeException deriving (Show)

instance Exception TextValueDecodeError
