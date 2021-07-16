module Test.Cardano.Chain.Delegation.Gen
  ( genCanonicalCertificate,
    genCertificate,
    genCanonicalCertificateDistinctList,
    genCertificateDistinctList,
    genError,
    genPayload,
  )
where

import Cardano.Chain.Delegation
  ( ACertificate (delegateVK, issuerVK),
    Certificate,
    Payload,
    signCertificate,
    unsafePayload,
  )
import Cardano.Chain.Delegation.Validation.Scheduling (Error (..))
import Cardano.Chain.Slotting (EpochNumber (..))
import Cardano.Crypto (ProtocolMagicId)
import Cardano.Prelude
import Data.List (nub)
import Hedgehog (Gen)
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import Test.Cardano.Chain.Common.Gen (genKeyHash)
import Test.Cardano.Chain.Slotting.Gen (genEpochNumber, genSlotNumber)
import Test.Cardano.Crypto.Gen (genSafeSigner, genVerificationKey)

genCanonicalCertificate :: ProtocolMagicId -> Gen Certificate
genCanonicalCertificate pm =
  signCertificate pm
    <$> genVerificationKey
    <*> (EpochNumber <$> Gen.word64 (Range.constant 0 1000000000000000))
    <*> genSafeSigner

genCertificate :: ProtocolMagicId -> Gen Certificate
genCertificate pm =
  signCertificate pm <$> genVerificationKey <*> genEpochNumber <*> genSafeSigner

genCanonicalCertificateDistinctList :: ProtocolMagicId -> Gen [Certificate]
genCanonicalCertificateDistinctList pm =
  noSelfSigningCerts <$> Gen.filter allDistinct pSKList
  where
    pSKList = Gen.list (Range.linear 0 5) (genCanonicalCertificate pm)

    allDistinct :: Eq a => [a] -> Bool
    allDistinct ls = length (nub ls) == length ls

    noSelfSigningCerts :: [Certificate] -> [Certificate]
    noSelfSigningCerts = filter (\x -> issuerVK x /= delegateVK x)

genCertificateDistinctList :: ProtocolMagicId -> Gen [Certificate]
genCertificateDistinctList pm =
  noSelfSigningCerts <$> Gen.filter allDistinct pSKList
  where
    pSKList = Gen.list (Range.linear 0 5) (genCertificate pm)

    allDistinct :: Eq a => [a] -> Bool
    allDistinct ls = length (nub ls) == length ls

    noSelfSigningCerts :: [Certificate] -> [Certificate]
    noSelfSigningCerts = filter (\x -> issuerVK x /= delegateVK x)

genError :: Gen Error
genError =
  Gen.choice
    [ pure InvalidCertificate,
      MultipleDelegationsForEpoch <$> genEpochNumber <*> genKeyHash,
      MultipleDelegationsForSlot <$> genSlotNumber <*> genKeyHash,
      NonGenesisDelegator <$> genKeyHash,
      WrongEpoch <$> genEpochNumber <*> genEpochNumber
    ]

genPayload :: ProtocolMagicId -> Gen Payload
genPayload pm =
  unsafePayload <$> Gen.list (Range.linear 0 5) (genCertificate pm)
