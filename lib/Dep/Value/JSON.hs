{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE TypeApplications #-}

module Dep.Value.JSON (
    -- * Constuct 'Value's parsed from JSON.
    fromBytes,
    JSONValueDecodeError (..)
) where

import Data.Aeson qualified 
import Data.ByteString
import Data.Typeable
import Dep.Value
import Control.Exception

fromBytes ::
  forall v m e.
  ( 
    Typeable v,
    Data.Aeson.FromJSON v,
    Monad m
  ) =>
  m ByteString ->
  Value v m
fromBytes action = Value do
  bytes <- action
  case Data.Aeson.eitherDecodeStrict' bytes of
    Left errMsg -> throw (JSONValueDecodeError (typeRep (Proxy @v)) errMsg)
    Right r -> pure r

data JSONValueDecodeError = JSONValueDecodeError TypeRep String deriving (Show)

instance Exception JSONValueDecodeError
