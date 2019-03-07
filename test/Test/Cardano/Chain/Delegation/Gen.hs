module Test.Cardano.Chain.Delegation.Gen
  ( genCanonicalCertificate
  , genCertificate
  , genCanonicalCertificateDistinctList
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
import Cardano.Chain.Slotting (EpochIndex(..))
import Cardano.Crypto (AProxyVerificationKey(..), ProtocolMagicId, createPsk)
import Data.List (nub)

import Test.Cardano.Chain.Common.Gen (genStakeholderId)
import Test.Cardano.Chain.Slotting.Gen (genEpochIndex)
import Test.Cardano.Crypto.Gen (genPublicKey, genSafeSigner)


genCanonicalCertificate :: ProtocolMagicId -> Gen Certificate
genCanonicalCertificate pm =
  createPsk pm
    <$> genSafeSigner
    <*> genPublicKey
    <*> (EpochIndex <$> Gen.word64 (Range.constant 0 1000000000000000))

genCertificate :: ProtocolMagicId -> Gen Certificate
genCertificate pm =
  createPsk pm <$> genSafeSigner <*> genPublicKey <*> genEpochIndex

genCanonicalCertificateDistinctList :: ProtocolMagicId -> Gen [Certificate]
genCanonicalCertificateDistinctList pm = do
  let pSKList = Gen.list (Range.linear 0 5) (genCanonicalCertificate pm)
  noSelfSigningCerts <$> Gen.filter allDistinct pSKList
 where
  allDistinct :: Eq a => [a] -> Bool
  allDistinct ls = length (nub ls) == length ls

  noSelfSigningCerts :: [Certificate] -> [Certificate]
  noSelfSigningCerts = filter (\x -> pskIssuerPk x /= pskDelegatePk x)

genCertificateDistinctList :: ProtocolMagicId -> Gen [Certificate]
genCertificateDistinctList pm = do
  let pSKList = Gen.list (Range.linear 0 5) (genCertificate pm)
  noSelfSigningCerts <$> Gen.filter allDistinct pSKList
 where
  allDistinct :: Eq a => [a] -> Bool
  allDistinct ls = length (nub ls) == length ls

  noSelfSigningCerts :: [Certificate] -> [Certificate]
  noSelfSigningCerts = filter (\x -> pskIssuerPk x /= pskDelegatePk x)

genPayload :: ProtocolMagicId -> Gen Payload
genPayload pm =
  unsafePayload <$> Gen.list (Range.linear 0 5) (genCertificate pm)

genUndo :: ProtocolMagicId -> Gen Undo
genUndo pm =
  Undo
    <$> Gen.list (Range.linear 1 10) (genCertificate pm)
    <*> Gen.set (Range.linear 0 10) genStakeholderId
