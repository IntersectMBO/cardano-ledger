{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE OverloadedStrings #-}

module Cardano.Chain.UTxO.TxProof (
  TxProof (..),
  mkTxProof,
  recoverTxProof,
) where

import Cardano.Chain.Common.Merkle (
  MerkleRoot,
  mkMerkleTree,
  mkMerkleTreeDecoded,
  mtRoot,
 )
import Cardano.Chain.UTxO.Tx (Tx)
import Cardano.Chain.UTxO.TxPayload (
  ATxPayload,
  TxPayload,
  recoverHashedBytes,
  txpAnnotatedTxs,
  txpTxs,
  txpWitnesses,
 )
import Cardano.Chain.UTxO.TxWitness (TxWitness)
import Cardano.Crypto (Hash, hashDecoded, serializeCborHash)
import Cardano.Ledger.Binary (
  DecCBOR (..),
  EncCBOR (..),
  FromCBOR (..),
  ToCBOR (..),
  encodeListLen,
  enforceSize,
  fromByronCBOR,
  toByronCBOR,
 )
import Cardano.Prelude
import Data.Aeson (ToJSON)
import Formatting (bprint, build)
import qualified Formatting.Buildable as B
import NoThunks.Class (NoThunks (..))

data TxProof = TxProof
  { txpNumber :: !Word32
  , txpRoot :: !(MerkleRoot Tx)
  , txpWitnessesHash :: !(Hash [TxWitness])
  }
  deriving (Show, Eq, Generic, NoThunks)
  deriving anyclass (NFData)

-- Used for debugging purposes only
instance ToJSON TxProof

instance B.Buildable TxProof where
  build proof =
    bprint
      ("<TxProof: " . build . ", " . build . ", " . build . ">")
      (txpNumber proof)
      (txpRoot proof)
      (txpWitnessesHash proof)

instance ToCBOR TxProof where
  toCBOR = toByronCBOR

instance FromCBOR TxProof where
  fromCBOR = fromByronCBOR

instance EncCBOR TxProof where
  encCBOR proof =
    encodeListLen 3
      <> encCBOR (txpNumber proof)
      <> encCBOR (txpRoot proof)
      <> encCBOR (txpWitnessesHash proof)
  encodedSizeExpr size proof =
    1
      + encodedSizeExpr size (txpNumber <$> proof)
      + encodedSizeExpr size (txpRoot <$> proof)
      + encodedSizeExpr size (txpWitnessesHash <$> proof)

instance DecCBOR TxProof where
  decCBOR = do
    enforceSize "TxProof" 3
    TxProof <$> decCBOR <*> decCBOR <*> decCBOR

-- | Construct 'TxProof' which proves given 'TxPayload'
--
--   This will construct a Merkle tree, which can be very expensive. Use with
--   care.
mkTxProof :: TxPayload -> TxProof
mkTxProof payload =
  TxProof
    { txpNumber = fromIntegral (length $ txpTxs payload)
    , txpRoot = mtRoot (mkMerkleTree $ txpTxs payload)
    , txpWitnessesHash = serializeCborHash $ txpWitnesses payload
    }

recoverTxProof :: ATxPayload ByteString -> TxProof
recoverTxProof payload =
  TxProof
    { txpNumber = fromIntegral (length $ txpTxs payload)
    , txpRoot = mtRoot (mkMerkleTreeDecoded $ txpAnnotatedTxs payload)
    , txpWitnessesHash = hashDecoded $ recoverHashedBytes payload
    }
