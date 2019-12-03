{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE DeriveAnyClass    #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DerivingVia       #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms   #-}

module Cardano.Chain.Block.Proof
  ( Proof
    ( Proof
    , proofUTxO
    , proofSsc
    , proofDelegation
    , proofUpdate
    , proofSerialized
    )
  , ProofValidationError (..)
  , mkProof
  , recoverProof
  )
where

import Cardano.Prelude

import Formatting (bprint, build, shown)
import qualified Formatting.Buildable as B

import Cardano.Binary
  ( FromCBOR(..)
  , FromCBORAnnotated(..)
  , ToCBOR(..)
  , encodeListLen
  , enforceSize
  , serializeEncoding'
  , encodePreEncoded
  , withSlice'
  )
import Cardano.Chain.Block.Body
  (Body(..), bodyDlgPayload, bodyTxPayload, bodyUpdatePayload)
import qualified Cardano.Chain.Delegation.Payload as Delegation
import Cardano.Chain.Ssc (SscProof(..))
import Cardano.Chain.UTxO.TxProof (TxProof, mkTxProof, recoverTxProof)
import qualified Cardano.Chain.Update.Proof as Update
import Cardano.Crypto (Hash, hash)


-- | Proof of everything contained in the payload
data Proof = Proof'
  { proofUTxO'      :: !TxProof
  , proofSsc'       :: !SscProof
  , proofDelegation':: !(Hash Delegation.Payload)
  , proofUpdate'    :: !Update.Proof
  , proofSerialized :: ByteString
  } deriving (Eq, Show, Generic, NFData)
    deriving NoUnexpectedThunks via AllowThunksIn '["proofSerialized"] Proof

{-# COMPLETE Proof #-}
pattern Proof :: TxProof -> SscProof -> (Hash Delegation.Payload) -> Update.Proof -> Proof
pattern Proof{ proofUTxO, proofSsc, proofDelegation, proofUpdate } <-
  Proof' proofUTxO proofSsc proofDelegation proofUpdate _
  where
  Proof utxo ssc delegation update =
    let bytes = serializeEncoding' $ encodeListLen 4
          <> toCBOR utxo
          <> toCBOR ssc
          <> toCBOR delegation
          <> toCBOR update
    in Proof' utxo ssc delegation update bytes

instance B.Buildable Proof where
  build proof = bprint
    ("<Proof: " . build . ", " . shown . ", " . build . ", " . build . ">")
    (proofUTxO proof)
    (proofSsc proof)
    (proofDelegation proof)
    (proofUpdate proof)

instance ToCBOR Proof where
  toCBOR = encodePreEncoded . proofSerialized

instance FromCBORAnnotated Proof where
  fromCBORAnnotated' = withSlice' $
     Proof' <$ lift (enforceSize "Proof" 4)
     <*> fromCBORAnnotated'
     <*> lift fromCBOR
     <*> lift fromCBOR
     <*> lift fromCBOR

mkProof :: Body -> Proof
mkProof body = Proof
   (mkTxProof $ bodyTxPayload body)
   SscProof
   (hash $ bodyDlgPayload body)
   (Update.mkProof $ bodyUpdatePayload body)

-- TODO: Should we be using this somewhere?
recoverProof :: Body -> Proof
recoverProof body = Proof
   (recoverTxProof $ bodyTxPayload body)
   SscProof
   (hash $ bodyDlgPayload body)
   (Update.recoverProof $ bodyUpdatePayload body)

-- | Error which can result from attempting to validate an invalid payload
-- proof.
data ProofValidationError
  = DelegationProofValidationError
  -- ^ The delegation payload proof did not match
  | UTxOProofValidationError
  -- ^ The UTxO payload proof did not match
  | UpdateProofValidationError
  -- ^ The update payload proof did not match
  deriving (Eq, Show)
