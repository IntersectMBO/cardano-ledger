{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}

module Cardano.Chain.Txp.TxProof
       ( TxProof (..)
       , mkTxProof
       ) where

import           Cardano.Prelude

import           Formatting (bprint, build, (%))
import qualified Formatting.Buildable as B

import           Cardano.Binary.Class (Bi (..), encodeListLen, enforceSize)
import           Cardano.Chain.Common.Merkle (MerkleRoot, mkMerkleTree, mtRoot)
import           Cardano.Chain.Txp.Tx (Tx)
import           Cardano.Chain.Txp.TxPayload (TxPayload (..))
import           Cardano.Chain.Txp.TxWitness (TxWitness)
import           Cardano.Crypto (Hash, hash)


data TxProof = TxProof
  { txpNumber        :: !Word32
  , txpRoot          :: !(MerkleRoot Tx)
  , txpWitnessesHash :: !(Hash [TxWitness])
  } deriving (Show, Eq, Generic)

instance B.Buildable TxProof where
  build proof = bprint
    ("<TxProof: " % build % ", " % build % ", " % build % ">")
    (txpNumber proof)
    (txpRoot proof)
    (txpWitnessesHash proof)

instance Bi TxProof where
  encode proof =
    encodeListLen 3
      <> encode (txpNumber proof)
      <> encode (txpRoot proof)
      <> encode (txpWitnessesHash proof)

  decode = do
    enforceSize "TxProof" 3
    TxProof <$> decode <*> decode <*> decode

instance NFData TxProof

-- | Construct 'TxProof' which proves given 'TxPayload'
--
--   This will construct a merkle tree, which can be very expensive. Use with
--   care. Bi constraints arise because we need to hash these things.
mkTxProof :: TxPayload -> TxProof
mkTxProof payload = TxProof
  { txpNumber        = fromIntegral (length $ _txpTxs payload)
  , txpRoot          = mtRoot (mkMerkleTree $ _txpTxs payload)
  , txpWitnessesHash = hash $ _txpWitnesses payload
  }
