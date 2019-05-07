{-# LANGUAGE DeriveAnyClass    #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}

module Cardano.Chain.Block.Proof
  ( Proof(..)
  , mkProof
  , recoverProof
  )
where

import Cardano.Prelude

import Formatting (bprint, build, shown)
import qualified Formatting.Buildable as B

import Cardano.Binary (FromCBOR(..), ToCBOR(..), encodeListLen, enforceSize)
import Cardano.Chain.Block.Body
  (ABody(..), Body, bodyDlgPayload, bodyTxPayload, bodyUpdatePayload)
import qualified Cardano.Chain.Delegation.Payload as Delegation
import Cardano.Chain.Ssc (SscProof(..))
import Cardano.Chain.UTxO.TxProof (TxProof, mkTxProof, recoverTxProof)
import qualified Cardano.Chain.Update.Proof as Update
import Cardano.Crypto (Hash, hash, hashDecoded)


-- | Proof of everything contained in the payload
data Proof = Proof
  { proofUTxO        :: !TxProof
  , proofSsc        :: !SscProof
  , proofDelegation :: !(Hash Delegation.Payload)
  , proofUpdate     :: !Update.Proof
  } deriving (Eq, Show, Generic, NFData)

instance B.Buildable Proof where
  build proof = bprint
    ("<Proof: " . build . ", " . shown . ", " . build . ", " . build . ">")
    (proofUTxO proof)
    (proofSsc proof)
    (proofDelegation proof)
    (proofUpdate proof)

instance ToCBOR Proof where
  toCBOR bc =
    encodeListLen 4
      <> toCBOR (proofUTxO bc)
      <> toCBOR (proofSsc bc)
      <> toCBOR (proofDelegation bc)
      <> toCBOR (proofUpdate bc)

instance FromCBOR Proof where
  fromCBOR = do
    enforceSize "Proof" 4
    Proof <$> fromCBOR <*> fromCBOR <*> fromCBOR <*> fromCBOR

mkProof :: Body -> Proof
mkProof body = Proof
  { proofUTxO        = mkTxProof $ bodyTxPayload body
  , proofSsc        = SscProof
  , proofDelegation = hash $ bodyDlgPayload body
  , proofUpdate     = Update.mkProof $ bodyUpdatePayload body
  }

-- TODO: Should we be using this somewhere?
recoverProof :: ABody ByteString -> Proof
recoverProof body = Proof
  { proofUTxO        = recoverTxProof $ bodyTxPayload body
  , proofSsc        = SscProof
  , proofDelegation = hashDecoded $ bodyDlgPayload body
  , proofUpdate     = Update.recoverProof $ bodyUpdatePayload body
  }
