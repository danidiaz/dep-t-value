{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE FlexibleContexts #-}
module Dep.Value.Text where

import Dep.Value (Value(Value))
import Dep.Has
import Dep.Loader
import Data.Text
import Data.Text.Encoding (decodeUtf8)

loadUtf8Text :: (Has Loader m e, Monad m) => (Text -> v) -> e -> Value v m
loadUtf8Text ctor (dep -> loader) = Value $
    ctor . decodeUtf8 <$> load loader _ "txt"

