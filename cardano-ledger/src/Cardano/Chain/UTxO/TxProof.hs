{-# LANGUAGE DeriveAnyClass     #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE OverloadedStrings  #-}

module Cardano.Chain.UTxO.TxProof
  ( TxProof(..)
  , mkTxProof
  , recoverTxProof
  )
where

import Cardano.Prelude

import Formatting (bprint, build)
import qualified Formatting.Buildable as B

import Cardano.Binary
  ( FromCBOR(..)
  , FromCBORAnnotated(..)
  , ToCBOR(..)
  , encodeListLen
  , enforceSize
  )
import Cardano.Chain.Common.Merkle (MerkleRoot, mkMerkleTree, mtRoot)
import Cardano.Chain.UTxO.Tx (Tx)
import Cardano.Chain.UTxO.TxPayload
  ( TxPayload
  , recoverHashedBytes
  , txpTxs
  , txpWitnesses
  )
import Cardano.Chain.UTxO.TxWitness (TxWitness)
import Cardano.Crypto (Hash, hash, hashDecoded)


data TxProof = TxProof
  { txpNumber        :: !Word32
  , txpRoot          :: !(MerkleRoot Tx)
  , txpWitnessesHash :: !(Hash [TxWitness])
  } deriving (Show, Eq, Generic, NoUnexpectedThunks)
    deriving anyclass NFData

instance B.Buildable TxProof where
  build proof = bprint
    ("<TxProof: " . build . ", " . build . ", " . build . ">")
    (txpNumber proof)
    (txpRoot proof)
    (txpWitnessesHash proof)

instance ToCBOR TxProof where
  toCBOR proof =
    encodeListLen 3
      <> toCBOR (txpNumber proof)
      <> toCBOR (txpRoot proof)
      <> toCBOR (txpWitnessesHash proof)

instance FromCBORAnnotated TxProof where
  fromCBORAnnotated' =
    TxProof <$ lift (enforceSize "TxProof" 3)
      <*> lift fromCBOR
      <*> lift fromCBOR
      <*> lift fromCBOR

-- | Construct 'TxProof' which proves given 'TxPayload'
--
--   This will construct a Merkle tree, which can be very expensive. Use with
--   care.
mkTxProof :: TxPayload -> TxProof
mkTxProof payload = TxProof
  { txpNumber        = fromIntegral (length $ txpTxs payload)
  , txpRoot          = mtRoot (mkMerkleTree $ txpTxs payload)
  , txpWitnessesHash = hash $ txpWitnesses payload
  }

recoverTxProof :: TxPayload -> TxProof
recoverTxProof payload = TxProof
  { txpNumber        = fromIntegral (length $ txpTxs payload)
  , txpRoot          = mtRoot (mkMerkleTree $ txpTxs payload)
  , txpWitnessesHash = hashDecoded $ recoverHashedBytes payload
  }
