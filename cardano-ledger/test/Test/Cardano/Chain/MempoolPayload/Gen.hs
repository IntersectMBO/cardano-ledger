module Test.Cardano.Chain.MempoolPayload.Gen
  ( genMempoolPayload
  )
where

import Cardano.Prelude

import Hedgehog
import qualified Hedgehog.Gen as Gen

import Cardano.Chain.MempoolPayload (AMempoolPayload (..), MempoolPayload)
import Cardano.Crypto (ProtocolMagicId)

import Test.Cardano.Chain.Delegation.Gen as Delegation (genPayload)
import Test.Cardano.Chain.Update.Gen as Update (genPayload)
import Test.Cardano.Chain.UTxO.Gen (genTxPayload)

genMempoolPayload :: ProtocolMagicId -> Gen MempoolPayload
genMempoolPayload pmi = Gen.choice
  [ MempoolTxPayload <$> genTxPayload pmi
  , MempoolDlgPayload <$> Delegation.genPayload pmi
  , MempoolUpdatePayload <$> Update.genPayload pmi
  ]
