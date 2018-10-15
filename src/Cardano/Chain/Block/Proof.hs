{-# LANGUAGE DeriveAnyClass    #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}

module Cardano.Chain.Block.Proof
       ( Proof (..)
       , mkProof
       , checkProof
       ) where

import           Cardano.Prelude

import           Control.Monad.Except (MonadError (..))
import           Formatting (bprint, build, sformat, shown, (%))
import qualified Formatting.Buildable as B

import           Cardano.Binary.Class (Bi (..), encodeListLen, enforceSize)
import           Cardano.Chain.Block.Body (Body (..))
import qualified Cardano.Chain.Delegation.Payload as Delegation
import           Cardano.Chain.Ssc (SscProof (..))
import           Cardano.Chain.Txp.TxProof (TxProof, mkTxProof)
import qualified Cardano.Chain.Update.Proof as Update
import           Cardano.Crypto (Hash, hash)


-- | Proof of everything contained in the payload
data Proof = Proof
  { proofTxp        :: !TxProof
  , proofSsc        :: !SscProof
  , proofDelegation :: !(Hash Delegation.Payload)
  , proofUpdate     :: !Update.Proof
  } deriving (Eq, Show, Generic, NFData)

instance B.Buildable Proof where
  build proof = bprint
    ("<Proof: " % build % ", " % shown % ", " % build % ", " % build % ">")
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

checkProof :: MonadError Text m => Body -> Proof -> m ()
checkProof body proof = do
  let calculatedProof = mkProof body
  let
    errMsg = sformat
      ( "Incorrect proof of body. "
      % "Proof in header: "
      % build
      % ", calculated proof: "
      % build
      )
      proof
      calculatedProof
  unless (calculatedProof == proof) $ throwError errMsg
