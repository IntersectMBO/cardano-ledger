
module Test.Cardano.Chain.Byron.Auxiliary
  ( genApplyMempoolPayloadErr
  , ts_roundTripApplyMempoolPayloadErrCompat
  )
  where

import Cardano.Prelude
  -- import Test.Cardano.Prelude

import Cardano.Crypto (ProtocolMagicId)
import Cardano.Chain.Byron.Auxiliary (ApplyMempoolPayloadErr (..))


import Test.Cardano.Chain.UTxO.Gen (genUTxOValidationError)
import qualified Test.Cardano.Chain.Delegation.Gen as Dlg
import qualified Test.Cardano.Chain.Update.Gen as UpdateIface
import Test.Cardano.Crypto.Gen (feedPM)
import Test.Options (eachOfTS, TSProperty)
import Test.Cardano.Binary.Helpers.GoldenRoundTrip (roundTripsCBORShow)

import Hedgehog (Gen)
import qualified Hedgehog.Gen as Gen


ts_roundTripApplyMempoolPayloadErrCompat :: TSProperty
ts_roundTripApplyMempoolPayloadErrCompat = eachOfTS
  20
  (feedPM genApplyMempoolPayloadErr)
  roundTripsCBORShow




genApplyMempoolPayloadErr :: ProtocolMagicId -> Gen ApplyMempoolPayloadErr
genApplyMempoolPayloadErr pm = Gen.choice
  [ MempoolTxErr <$> genUTxOValidationError
  , MempoolDlgErr <$> Dlg.genError
  , MempoolUpdateProposalErr <$> UpdateIface.genError pm
  , MempoolUpdateVoteErr <$> UpdateIface.genError pm
  ]
