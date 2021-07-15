{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}

module Cardano.Chain.Block.Proof
  ( Proof (..),
    ProofValidationError (..),
    mkProof,
    recoverProof,
  )
where

import Cardano.Binary (FromCBOR (..), ToCBOR (..), encodeListLen, enforceSize)
import Cardano.Chain.Block.Body
  ( ABody (..),
    Body,
    bodyDlgPayload,
    bodyTxPayload,
    bodyUpdatePayload,
  )
import qualified Cardano.Chain.Delegation.Payload as Delegation
import Cardano.Chain.Ssc (SscProof (..))
import Cardano.Chain.UTxO.TxProof (TxProof, mkTxProof, recoverTxProof)
import qualified Cardano.Chain.Update.Proof as Update
import Cardano.Crypto (Hash, hashDecoded, serializeCborHash)
import Cardano.Prelude
import Data.Aeson (ToJSON)
import Formatting (bprint, build, shown)
import qualified Formatting.Buildable as B
import NoThunks.Class (NoThunks (..))

-- | Proof of everything contained in the payload
data Proof = Proof
  { proofUTxO :: !TxProof,
    proofSsc :: !SscProof,
    proofDelegation :: !(Hash Delegation.Payload),
    proofUpdate :: !Update.Proof
  }
  deriving (Eq, Show, Generic, NFData, NoThunks)

instance B.Buildable Proof where
  build proof =
    bprint
      ("<Proof: " . build . ", " . shown . ", " . build . ", " . build . ">")
      (proofUTxO proof)
      (proofSsc proof)
      (proofDelegation proof)
      (proofUpdate proof)

-- Used for debugging purposes only
instance ToJSON Proof

instance ToCBOR Proof where
  toCBOR bc =
    encodeListLen 4
      <> toCBOR (proofUTxO bc)
      <> toCBOR (proofSsc bc)
      <> toCBOR (proofDelegation bc)
      <> toCBOR (proofUpdate bc)

  encodedSizeExpr size bc =
    1
      + encodedSizeExpr size (proofUTxO <$> bc)
      + encodedSizeExpr size (proofSsc <$> bc)
      + encodedSizeExpr size (proofDelegation <$> bc)
      + encodedSizeExpr size (proofUpdate <$> bc)

instance FromCBOR Proof where
  fromCBOR = do
    enforceSize "Proof" 4
    Proof <$> fromCBOR <*> fromCBOR <*> fromCBOR <*> fromCBOR

mkProof :: Body -> Proof
mkProof body =
  Proof
    { proofUTxO = mkTxProof $ bodyTxPayload body,
      proofSsc = SscProof,
      proofDelegation = serializeCborHash $ bodyDlgPayload body,
      proofUpdate = Update.mkProof $ bodyUpdatePayload body
    }

-- TODO: Should we be using this somewhere?
recoverProof :: ABody ByteString -> Proof
recoverProof body =
  Proof
    { proofUTxO = recoverTxProof $ bodyTxPayload body,
      proofSsc = SscProof,
      proofDelegation = hashDecoded $ bodyDlgPayload body,
      proofUpdate = Update.recoverProof $ bodyUpdatePayload body
    }

-- | Error which can result from attempting to validate an invalid payload
-- proof.
data ProofValidationError
  = -- | The delegation payload proof did not match
    DelegationProofValidationError
  | -- | The UTxO payload proof did not match
    UTxOProofValidationError
  | -- | The update payload proof did not match
    UpdateProofValidationError
  deriving (Eq, Show)
