{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}

module Cardano.Ledger.Keys.WitVKey (
  WitVKey (WitVKey),
  witVKeyHash,
  wvkSig,
) where

import Cardano.Crypto.DSIGN.Class (
  SignedDSIGN,
  decodeSignedDSIGN,
  encodeSignedDSIGN,
 )
import Cardano.Ledger.Binary (Annotator, DecCBOR (..), EncCBOR (..))
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
import Control.DeepSeq
import Data.Ord (comparing)
import Data.Typeable (Typeable)
import GHC.Generics (Generic)
import NoThunks.Class (AllowThunksIn (..), NoThunks (..))

-- | Proof/Witness that a transaction is authorized by the given key holder.
data WitVKey kr = WitVKeyInternal
  { wvkKey :: !(VKey kr)
  , wvkSignature :: !(SignedDSIGN DSIGN (Hash HASH EraIndependentTxBody))
  , wvkKeyHash :: KeyHash 'Witness
  -- ^ Hash of the witness vkey. We store this here to avoid repeated hashing
  --   when used in ordering.
  }
  deriving (Generic, Show, Eq)

wvkSig :: WitVKey kr -> SignedDSIGN DSIGN (Hash HASH EraIndependentTxBody)
wvkSig = wvkSignature
{-# DEPRECATED wvkSig "In favor of `wvkSignature`" #-}

deriving via
  AllowThunksIn '["wvkKeyHash"] (WitVKey kr)
  instance
    Typeable kr => NoThunks (WitVKey kr)

instance NFData (WitVKey kr) where
  rnf WitVKeyInternal {wvkKeyHash} = wvkKeyHash `seq` ()

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
    comparing wvkKeyHash x y <> comparing (hashTxBodySignature . wvkSignature) x y

instance Typeable kr => Plain.ToCBOR (WitVKey kr) where
  toCBOR WitVKeyInternal {wvkKey, wvkSignature} =
    Plain.encodeListLen 2
      <> Plain.toCBOR wvkKey
      <> encodeSignedDSIGN wvkSignature

instance Typeable kr => Plain.FromCBOR (WitVKey kr) where
  fromCBOR =
    Plain.decodeRecordNamed "WitVKey" (const 2) $
      WitVKey <$> Plain.fromCBOR <*> decodeSignedDSIGN

instance Typeable kr => EncCBOR (WitVKey kr)

instance Typeable kr => DecCBOR (WitVKey kr)

instance Typeable kr => DecCBOR (Annotator (WitVKey kr)) where
  decCBOR = pure <$> decCBOR
  {-# INLINE decCBOR #-}

pattern WitVKey ::
  VKey kr ->
  SignedDSIGN DSIGN (Hash HASH EraIndependentTxBody) ->
  WitVKey kr
pattern WitVKey k s <-
  WitVKeyInternal k s _
  where
    WitVKey k s =
      let hash = asWitness $ hashKey k
       in WitVKeyInternal k s hash

{-# COMPLETE WitVKey #-}

-- | Access computed hash. Evaluated lazily
witVKeyHash :: WitVKey kr -> KeyHash 'Witness
witVKeyHash = wvkKeyHash
