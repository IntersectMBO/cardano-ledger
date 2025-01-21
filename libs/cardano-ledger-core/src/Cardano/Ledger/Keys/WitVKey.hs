{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}

module Cardano.Ledger.Keys.WitVKey (
  WitVKey (WitVKey),
  witVKeyBytes,
  witVKeyHash,
  eqWitVKeyRaw,
)
where

import Cardano.Crypto.DSIGN.Class (
  SignedDSIGN,
  decodeSignedDSIGN,
  encodeSignedDSIGN,
 )
import Cardano.Ledger.Binary (
  Annotator (..),
  DecCBOR (..),
  EncCBOR (..),
  ToCBOR (..),
  annotatorSlice,
  decodeRecordNamed,
  fromPlainDecoder,
 )
import qualified Cardano.Ledger.Binary.Crypto (decodeSignedDSIGN)
import qualified Cardano.Ledger.Binary.Plain as Plain
import Cardano.Ledger.Hashes (
  EraIndependentTxBody,
  HASH,
  Hash,
  KeyHash (..),
  hashKey,
  hashTxBodySignature,
 )
import Cardano.Ledger.Keys.Internal (DSIGN, KeyRole (..), VKey, asWitness)
import Cardano.Ledger.MemoBytes (EqRaw (..), MemoBytes (Memo), decodeMemoized)
import Control.DeepSeq
import qualified Data.ByteString.Lazy as BSL
import qualified Data.ByteString.Short as SBS
import Data.Ord (comparing)
import Data.Typeable (Typeable)
import GHC.Generics (Generic)
import NoThunks.Class (AllowThunksIn (..), NoThunks (..))

-- | Proof/Witness that a transaction is authorized by the given key holder.
data WitVKey kr = WitVKeyInternal
  { wvkKey :: !(VKey kr)
  , wvkSig :: !(SignedDSIGN DSIGN (Hash HASH EraIndependentTxBody))
  , wvkKeyHash :: KeyHash 'Witness
  -- ^ Hash of the witness vkey. We store this here to avoid repeated hashing
  --   when used in ordering.
  , wvkBytes :: BSL.ByteString
  }
  deriving (Generic, Show, Eq)

deriving via
  AllowThunksIn '["wvkBytes", "wvkKeyHash"] (WitVKey kr)
  instance
    Typeable kr => NoThunks (WitVKey kr)

instance NFData (WitVKey kr) where
  rnf WitVKeyInternal {wvkKeyHash, wvkBytes} = wvkKeyHash `deepseq` rnf wvkBytes

instance Typeable kr => Ord (WitVKey kr) where
  compare x y =
    -- It is advised against comparison on keys and signatures directly,
    -- therefore we use hashes of verification keys and signatures for
    -- implementing this Ord instance. Note that we do not need to memoize the
    -- hash of a signature, like it is done with the hash of a key, because Ord
    -- instance is only used for Sets of WitVKeys and it would be a mistake to
    -- have two WitVKeys in a same Set for different transactions. Therefore
    -- comparison on signatures is unlikely to happen and is only needed for
    -- compliance with Ord laws.
    comparing wvkKeyHash x y <> comparing (hashTxBodySignature . wvkSig) x y

instance Typeable kr => Plain.ToCBOR (WitVKey kr) where
  toCBOR = Plain.encodePreEncoded . BSL.toStrict . wvkBytes

-- | Encodes memoized bytes created upon construction.
instance Typeable kr => EncCBOR (WitVKey kr)

instance Typeable kr => DecCBOR (Annotator (WitVKey kr)) where
  decCBOR =
    annotatorSlice $
      fromPlainDecoder $
        Plain.decodeRecordNamed "WitVKey" (const 2) $
          fmap pure $
            mkWitVKey <$> Plain.fromCBOR <*> decodeSignedDSIGN
    where
      mkWitVKey k sig = WitVKeyInternal k sig (asWitness $ hashKey k)
      {-# INLINE mkWitVKey #-}
  {-# INLINE decCBOR #-}

data WitVKeyRaw kr
  = WitVKeyRaw
      !(VKey kr)
      !(SignedDSIGN DSIGN (Hash HASH EraIndependentTxBody))
      (KeyHash 'Witness)

instance Typeable kr => DecCBOR (WitVKeyRaw kr) where
  decCBOR = decodeRecordNamed "WitVKey" (const 2) $ do
    key <- decCBOR
    sig <- Cardano.Ledger.Binary.Crypto.decodeSignedDSIGN
    pure $ WitVKeyRaw key sig (asWitness $ hashKey key)

instance Typeable kr => DecCBOR (WitVKey kr) where
  decCBOR = do
    Memo (WitVKeyRaw k s kh) bs <- decodeMemoized (decCBOR @(WitVKeyRaw kr))
    pure $ WitVKeyInternal k s kh (BSL.fromStrict (SBS.fromShort bs))

instance Typeable kr => EqRaw (WitVKey kr) where
  eqRaw = eqWitVKeyRaw

pattern WitVKey ::
  Typeable kr =>
  VKey kr ->
  SignedDSIGN DSIGN (Hash HASH EraIndependentTxBody) ->
  WitVKey kr
pattern WitVKey k s <-
  WitVKeyInternal k s _ _
  where
    WitVKey k s =
      let bytes =
            Plain.serialize $
              Plain.encodeListLen 2
                <> Plain.toCBOR k
                <> encodeSignedDSIGN s
          hash = asWitness $ hashKey k
       in WitVKeyInternal k s hash bytes

{-# COMPLETE WitVKey #-}

-- | Access computed hash. Evaluated lazily
witVKeyHash :: WitVKey kr -> KeyHash 'Witness
witVKeyHash = wvkKeyHash

-- | Access CBOR encoded representation of the witness. Evaluated lazily
witVKeyBytes :: WitVKey kr -> BSL.ByteString
witVKeyBytes = wvkBytes

eqWitVKeyRaw :: Typeable kr => WitVKey kr -> WitVKey kr -> Bool
eqWitVKeyRaw (WitVKey k1 s1) (WitVKey k2 s2) = k1 == k2 && s1 == s2
