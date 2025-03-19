{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ViewPatterns #-}

module Cardano.Ledger.Keys.WitVKey (
  WitVKey (WitVKey),
  WitVKeyRaw,
  witVKeyBytes,
  witVKeyHash,
)
where

import Cardano.Crypto.DSIGN.Class (
  SignedDSIGN,
 )
import Cardano.Ledger.Binary (
  Annotator (..),
  DecCBOR (..),
  EncCBOR (..),
  decodeRecordNamed,
  encodeListLen,
 )
import Cardano.Ledger.Binary.Crypto (decodeSignedDSIGN, encodeSignedDSIGN)
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
import Cardano.Ledger.MemoBytes (
  EqRaw (..),
  Mem,
  MemoBytes,
  Memoized (..),
  getMemoRawBytes,
  getMemoRawType,
  getterMemoRawType,
  mkMemoized,
 )
import Control.DeepSeq
import qualified Data.ByteString.Lazy as BSL
import qualified Data.ByteString.Short as SBS
import Data.Ord (comparing)
import Data.Typeable (Typeable)
import GHC.Generics (Generic)
import Lens.Micro ((^.))
import NoThunks.Class (AllowThunksIn (..), NoThunks (..))

-- | Proof/Witness that a transaction is authorized by the given key holder.
data WitVKeyRaw kr = WitVKeyRaw
  { wvkrKey :: !(VKey kr)
  , wvkrSignature :: !(SignedDSIGN DSIGN (Hash HASH EraIndependentTxBody))
  , wvkrKeyHash :: KeyHash 'Witness
  -- ^ Hash of the witness vkey. We store this here to avoid repeated hashing
  --   when used in ordering.
  }
  deriving (Generic, Show, Eq)

deriving via
  AllowThunksIn '["wvkrKeyHash"] (WitVKeyRaw kr)
  instance
    Typeable kr => NoThunks (WitVKeyRaw kr)

instance NFData (WitVKeyRaw kr) where
  rnf = rnf . wvkrKey

instance Typeable kr => Ord (WitVKeyRaw kr) where
  compare x y =
    -- It is advised against comparison on keys and signatures directly,
    -- therefore we use hashes of verification keys and signatures for
    -- implementing this Ord instance. Note that we do not need to memoize the
    -- hash of a signature, like it is done with the hash of a key, because Ord
    -- instance is only used for Sets of WitVKeys and it would be a mistake to
    -- have two WitVKeys in a same Set for different transactions. Therefore
    -- comparison on signatures is unlikely to happen and is only needed for
    -- compliance with Ord laws.
    comparing wvkrKeyHash x y <> comparing (hashTxBodySignature . wvkrSignature) x y

instance Typeable kr => EncCBOR (WitVKeyRaw kr) where
  encCBOR wvkr@(WitVKeyRaw _ _ _) =
    let WitVKeyRaw {..} = wvkr
     in encodeListLen 2
          <> encCBOR wvkrKey
          <> encodeSignedDSIGN wvkrSignature

instance Typeable kr => DecCBOR (WitVKeyRaw kr) where
  decCBOR = decodeRecordNamed "WitVKey" (const 2) $ do
    mkWitVKey <$> decCBOR <*> Cardano.Ledger.Binary.Crypto.decodeSignedDSIGN
    where
      mkWitVKey k sig = WitVKeyRaw k sig (asWitness $ hashKey k)
      {-# INLINE mkWitVKey #-}
  {-# INLINE decCBOR #-}

instance Typeable kr => DecCBOR (Annotator (WitVKeyRaw kr)) where
  decCBOR = pure <$> decCBOR

newtype WitVKey kr = WitVKeyConstr (MemoBytes (WitVKeyRaw kr))
  deriving (Generic)
  deriving newtype (Show, Eq, NFData, NoThunks, Plain.ToCBOR, DecCBOR)

instance Memoized (WitVKey kr) where
  type RawType (WitVKey kr) = WitVKeyRaw kr

instance Typeable kr => EncCBOR (WitVKey kr)

deriving via
  Mem (WitVKeyRaw kr)
  instance
    Typeable kr =>
    DecCBOR (Annotator (WitVKey kr))

instance Typeable kr => Ord (WitVKey kr) where
  compare x y = compare (getMemoRawType x) (getMemoRawType y)

instance EqRaw (WitVKey kr) where
  eqRaw x y =
    x ^. getterMemoRawType wvkrKey == y ^. getterMemoRawType wvkrKey
      && x ^. getterMemoRawType wvkrSignature == y ^. getterMemoRawType wvkrSignature

pattern WitVKey ::
  Typeable kr =>
  VKey kr ->
  SignedDSIGN DSIGN (Hash HASH EraIndependentTxBody) ->
  WitVKey kr
pattern WitVKey key signature <-
  ( getMemoRawType ->
      WitVKeyRaw key signature _
    )
  where
    WitVKey wvkKey wvkSignature =
      mkMemoized minBound $
        WitVKeyRaw wvkKey wvkSignature (asWitness $ hashKey wvkKey)
{-# COMPLETE WitVKey #-}

-- | Access computed hash. Evaluated lazily
witVKeyHash :: WitVKey kr -> KeyHash 'Witness
witVKeyHash x = x ^. getterMemoRawType wvkrKeyHash

-- | Access CBOR encoded representation of the witness. Evaluated lazily
{-# DEPRECATED witVKeyBytes "In favor of `getMemoRawBytes`" #-}
witVKeyBytes :: WitVKey kr -> BSL.ByteString
witVKeyBytes = BSL.fromStrict . SBS.fromShort . getMemoRawBytes
