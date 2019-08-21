module Test.Cardano.Chain.MempoolPayload.Gen
  ( genMempoolPayload
  )
where

import Cardano.Prelude

import Hedgehog
import qualified Hedgehog.Gen as Gen

import Cardano.Chain.MempoolPayload (AMempoolPayload (..), MempoolPayload)
import Cardano.Crypto (ProtocolMagicId)

import Test.Cardano.Chain.Delegation.Gen as Delegation (genCertificate)
import Test.Cardano.Chain.Update.Gen as Update (genProposal, genVote)
import Test.Cardano.Chain.UTxO.Gen (genTxAux)

genMempoolPayload :: ProtocolMagicId -> Gen MempoolPayload
genMempoolPayload pmi = Gen.choice
  [ MempoolTxPayload <$> genTxAux pmi
  , MempoolDlgPayload <$> Delegation.genCertificate pmi
  , MempoolUpdateProposalPayload <$> Update.genProposal pmi
  , MempoolUpdateVotePayload <$> Update.genVote pmi
  ]
