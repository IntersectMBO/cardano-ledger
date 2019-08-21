{-# LANGUAGE OverloadedStrings #-}

module Test.Cardano.Chain.MempoolPayload.Example
  ( exampleMempoolPayload
  , exampleMempoolPayload1
  , exampleMempoolPayload2
  , exampleMempoolPayload3
  )
where

import Data.List ((!!))

import Cardano.Chain.MempoolPayload (AMempoolPayload (..), MempoolPayload)

import Test.Cardano.Chain.Delegation.Example as Delegation
    (exampleCertificates)
import Test.Cardano.Chain.Update.Example as Update
    (exampleProposal, exampleVote)
import Test.Cardano.Chain.UTxO.Example (exampleTxAux)


exampleMempoolPayload :: MempoolPayload
exampleMempoolPayload = MempoolTxPayload exampleTxAux

exampleMempoolPayload1 :: MempoolPayload
exampleMempoolPayload1 = MempoolDlgPayload (Delegation.exampleCertificates !! 0)

exampleMempoolPayload2 :: MempoolPayload
exampleMempoolPayload2 = MempoolUpdateProposalPayload Update.exampleProposal

exampleMempoolPayload3 :: MempoolPayload
exampleMempoolPayload3 = MempoolUpdateVotePayload Update.exampleVote
