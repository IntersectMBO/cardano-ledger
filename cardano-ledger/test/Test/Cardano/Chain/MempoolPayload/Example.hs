{-# LANGUAGE OverloadedStrings #-}

module Test.Cardano.Chain.MempoolPayload.Example
  ( exampleMempoolPayload
  , exampleMempoolPayload1
  , exampleMempoolPayload2
  )
where

import Cardano.Prelude

import Cardano.Chain.Delegation as Delegation (unsafePayload)
import Cardano.Chain.MempoolPayload (AMempoolPayload (..), MempoolPayload)

import Test.Cardano.Chain.Delegation.Example as Delegation
    (exampleCertificates)
import Test.Cardano.Chain.Update.Example as Update (examplePayload)
import Test.Cardano.Chain.UTxO.Example (exampleTxPayload)


exampleMempoolPayload :: MempoolPayload
exampleMempoolPayload = MempoolTxPayload exampleTxPayload

exampleMempoolPayload1 :: MempoolPayload
exampleMempoolPayload1 = MempoolDlgPayload dp
 where
  dp = Delegation.unsafePayload (take 4 Delegation.exampleCertificates)

exampleMempoolPayload2 :: MempoolPayload
exampleMempoolPayload2 = MempoolUpdatePayload Update.examplePayload
