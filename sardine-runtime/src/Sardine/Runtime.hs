{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -funbox-strict-fields #-}
module Sardine.Runtime (
    module X
  ) where

import           Sardine.Runtime.Internal as X (Decode(..), runDecodeStrict)
import           Sardine.Runtime.Internal as X (Encode(..), runEncodeStrict, runEncodeStrict')
import           Sardine.Runtime.Internal as X (DecodeError(..), decodeFail)
import           Sardine.Runtime.VarInt as X
import           Sardine.Runtime.Primitive as X
import           Sardine.Runtime.Thrift as X
