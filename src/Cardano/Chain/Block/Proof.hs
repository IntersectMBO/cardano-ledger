{-# LANGUAGE DeriveAnyClass    #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}

module Cardano.Chain.Block.Proof
  ( Proof(..)
  , mkProof
  , ProofError(..)
  , checkProof
  )
where

import Cardano.Prelude

import Control.Monad.Except (MonadError(..))
import Formatting (bprint, build, shown)
import qualified Formatting.Buildable as B

import Cardano.Binary.Class (Bi(..), encodeListLen, enforceSize)
import Cardano.Chain.Block.Body
  (ABody(..), Body, bodyDlgPayload, bodyTxPayload, bodyUpdatePayload)
import qualified Cardano.Chain.Delegation.Payload as Delegation
import Cardano.Chain.Ssc (SscProof(..))
import Cardano.Chain.Txp.TxProof (TxProof, mkTxProof, recoverTxProof)
import qualified Cardano.Chain.Update.Proof as Update
import Cardano.Crypto (Hash, hash, hashDecoded)


-- | Proof of everything contained in the payload
data Proof = Proof
  { proofTxp        :: !TxProof
  , proofSsc        :: !SscProof
  , proofDelegation :: !(Hash Delegation.Payload)
  , proofUpdate     :: !Update.Proof
  } deriving (Eq, Show, Generic, NFData)

instance B.Buildable Proof where
  build proof = bprint
    ("<Proof: " . build . ", " . shown . ", " . build . ", " . build . ">")
    (proofTxp proof)
    (proofSsc proof)
    (proofDelegation proof)
    (proofUpdate proof)

instance Bi Proof where
  encode bc =
    encodeListLen 4
      <> encode (proofTxp bc)
      <> encode (proofSsc bc)
      <> encode (proofDelegation bc)
      <> encode (proofUpdate bc)

  decode = do
    enforceSize "Proof" 4
    Proof <$> decode <*> decode <*> decode <*> decode

mkProof :: Body -> Proof
mkProof body = Proof
  { proofTxp        = mkTxProof $ bodyTxPayload body
  , proofSsc        = SscProof
  , proofDelegation = hash $ bodyDlgPayload body
  , proofUpdate     = Update.mkProof $ bodyUpdatePayload body
  }

recoverProof :: ABody ByteString -> Proof
recoverProof body = Proof
  { proofTxp        = recoverTxProof $ bodyTxPayload body
  , proofSsc        = SscProof
  , proofDelegation = hashDecoded $ bodyDlgPayload body
  , proofUpdate     = Update.recoverProof $ bodyUpdatePayload body
  }

data ProofError = ProofIncorrect Proof Proof

instance B.Buildable ProofError where
  build = \case
    ProofIncorrect p p' -> bprint
      ( "Incorrect proof of Body.\n"
      . "Proof in header:\n"
      . build . "\n"
      . "Calculated proof:\n"
      . build . "\n"
      )
      p
      p'

checkProof :: MonadError ProofError m => ABody ByteString -> Proof -> m ()
checkProof body proof =
  (calculatedProof == proof) `orThrowError` ProofIncorrect proof calculatedProof
  where calculatedProof = recoverProof body
