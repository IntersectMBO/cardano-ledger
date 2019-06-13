module Test.Cardano.Chain.Delegation.Gen
  ( genCanonicalCertificate
  , genCertificate
  , genCanonicalCertificateDistinctList
  , genCertificateDistinctList
  , genPayload
  )
where

import Cardano.Prelude

import Hedgehog (Gen)
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range

import Cardano.Chain.Delegation
  ( ACertificate(delegateVK, issuerVK)
  , Certificate
  , Payload
  , mkCertificate
  , unsafePayload
  )
import Cardano.Chain.Slotting (EpochNumber(..))
import Cardano.Crypto (ProtocolMagicId)
import Data.List (nub)

import Test.Cardano.Chain.Slotting.Gen (genEpochNumber)
import Test.Cardano.Crypto.Gen (genVerificationKey, genSafeSigner)


genCanonicalCertificate :: ProtocolMagicId -> Gen Certificate
genCanonicalCertificate pm =
  mkCertificate pm
    <$> genSafeSigner
    <*> genVerificationKey
    <*> (EpochNumber <$> Gen.word64 (Range.constant 0 1000000000000000))

genCertificate :: ProtocolMagicId -> Gen Certificate
genCertificate pm =
  mkCertificate pm <$> genSafeSigner <*> genVerificationKey <*> genEpochNumber

genCanonicalCertificateDistinctList :: ProtocolMagicId -> Gen [Certificate]
genCanonicalCertificateDistinctList pm = do
  let pSKList = Gen.list (Range.linear 0 5) (genCanonicalCertificate pm)
  noSelfSigningCerts <$> Gen.filter allDistinct pSKList
 where
  allDistinct :: Eq a => [a] -> Bool
  allDistinct ls = length (nub ls) == length ls

  noSelfSigningCerts :: [Certificate] -> [Certificate]
  noSelfSigningCerts = filter (\x -> issuerVK x /= delegateVK x)

genCertificateDistinctList :: ProtocolMagicId -> Gen [Certificate]
genCertificateDistinctList pm = do
  let pSKList = Gen.list (Range.linear 0 5) (genCertificate pm)
  noSelfSigningCerts <$> Gen.filter allDistinct pSKList
 where
  allDistinct :: Eq a => [a] -> Bool
  allDistinct ls = length (nub ls) == length ls

  noSelfSigningCerts :: [Certificate] -> [Certificate]
  noSelfSigningCerts = filter (\x -> issuerVK x /= delegateVK x)

genPayload :: ProtocolMagicId -> Gen Payload
genPayload pm =
  unsafePayload <$> Gen.list (Range.linear 0 5) (genCertificate pm)
