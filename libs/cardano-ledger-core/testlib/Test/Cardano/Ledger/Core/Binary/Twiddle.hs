{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Test.Cardano.Ledger.Core.Binary.Twiddle (
  module Test.Cardano.Ledger.Binary.Twiddle,
) where

import Cardano.Ledger.Address
import Cardano.Ledger.BaseTypes
import Cardano.Ledger.Binary (DecCBOR (..), decodeFullDecoder)
import Cardano.Ledger.Coin
import Cardano.Ledger.Core
import Cardano.Ledger.MemoBytes (MemoBytes, pattern Memo)
import Cardano.Ledger.TxIn
import Data.ByteString.Lazy (fromStrict)
import Data.ByteString.Short (fromShort)
import Data.Typeable (Typeable)
import Test.Cardano.Ledger.Binary.Twiddle

instance Twiddle SlotNo where
  twiddle = twiddleTerm

instance Twiddle Withdrawals where
  twiddle = twiddleTerm

instance Twiddle TxAuxDataHash where
  twiddle = twiddleTerm

instance Typeable t => Twiddle (KeyHash t) where
  twiddle = twiddleTerm

instance Twiddle Network where
  twiddle = twiddleTerm

instance Twiddle TxIn where
  twiddle = twiddleTerm

instance Twiddle Coin where
  twiddle = twiddleTerm

instance Twiddle (CompactForm Coin) where
  twiddle = twiddleTerm

instance Twiddle CompactAddr where
  twiddle = twiddleTerm

-- | The @Twiddle@ instance for @MemoBytes@ just encodes the stored bytes as a
--   @Term@ and then returns that without any modifications.
instance Twiddle (MemoBytes a) where
  twiddle v (Memo _ sbs) = case decResult of
    Right x -> pure x
    Left e -> error $ "Failed to decode memoized bytes:\n" <> show e
    where
      decResult = decodeFullDecoder v "Memo" decCBOR . fromStrict $ fromShort sbs
