module Test.Cardano.Chain.Delegation.Gen
  ( genCertificate
  , genCertificateDistinctList
  , genPayload
  , genUndo
  )
where

import Cardano.Prelude

import Hedgehog (Gen)
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range

import Cardano.Chain.Delegation (Certificate, Payload, Undo(..), unsafePayload)
import Cardano.Crypto (ProtocolMagic, createPsk)
import Data.List (nub)

import Test.Cardano.Chain.Common.Gen (genStakeholderId)
import Test.Cardano.Chain.Slotting.Gen (genEpochIndex)
import Test.Cardano.Crypto.Gen (genPublicKey, genSafeSigner)


genCertificate :: ProtocolMagic -> Gen Certificate
genCertificate pm =
  createPsk pm <$> genSafeSigner <*> genPublicKey <*> genEpochIndex

genCertificateDistinctList :: ProtocolMagic -> Gen [Certificate]
genCertificateDistinctList pm = do
  let
    pSKList = Gen.list
      (Range.linear 0 5)
      (createPsk pm <$> genSafeSigner <*> genPublicKey <*> genEpochIndex)
  Gen.filter allDistinct pSKList
 where
  allDistinct :: Eq a => [a] -> Bool
  allDistinct ls = length (nub ls) == length ls

genPayload :: ProtocolMagic -> Gen Payload
genPayload pm =
  unsafePayload <$> Gen.list (Range.linear 0 5) (genCertificate pm)

genUndo :: ProtocolMagic -> Gen Undo
genUndo pm =
  Undo
    <$> Gen.list (Range.linear 1 10) (genCertificate pm)
    <*> Gen.set (Range.linear 0 10) genStakeholderId
