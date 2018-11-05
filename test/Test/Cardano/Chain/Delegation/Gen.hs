module Test.Cardano.Chain.Delegation.Gen
       ( genPayload
       , genHeavyDlgIndex
       , genLightDlgIndices
       , genProxySKBlockInfo
       , genProxySKHeavy
       , genProxySKHeavyDistinctList
       , genUndo
       ) where

import           Cardano.Prelude

import           Hedgehog (Gen)
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range

import           Cardano.Chain.Delegation (HeavyDlgIndex (..),
                     LightDlgIndices (..), Payload (..), ProxySKBlockInfo,
                     ProxySKHeavy, Undo (..))
import           Cardano.Crypto (ProtocolMagic, safeCreatePsk)
import           Data.List (nub)

import           Test.Cardano.Chain.Common.Gen (genStakeholderId)
import           Test.Cardano.Chain.Slotting.Gen (genEpochIndex)
import           Test.Cardano.Crypto.Gen (genPublicKey, genSafeSigner)


genPayload :: ProtocolMagic -> Gen Payload
genPayload pm =
  UnsafePayload <$> Gen.list (Range.linear 0 5) (genProxySKHeavy pm)

genHeavyDlgIndex :: Gen HeavyDlgIndex
genHeavyDlgIndex = HeavyDlgIndex <$> genEpochIndex

genLightDlgIndices :: Gen LightDlgIndices
genLightDlgIndices =
  LightDlgIndices <$> ((,) <$> genEpochIndex <*> genEpochIndex)

genProxySKBlockInfo :: ProtocolMagic -> Gen ProxySKBlockInfo
genProxySKBlockInfo pm = Gen.maybe $ do
  pSKHeavy <- genProxySKHeavy pm
  pubKey   <- genPublicKey
  pure (pSKHeavy, pubKey)

genProxySKHeavy :: ProtocolMagic -> Gen ProxySKHeavy
genProxySKHeavy pm =
  safeCreatePsk pm <$> genSafeSigner <*> genPublicKey <*> genHeavyDlgIndex

genProxySKHeavyDistinctList :: ProtocolMagic -> Gen [ProxySKHeavy]
genProxySKHeavyDistinctList pm = do
    let pSKList = Gen.list
                    (Range.linear 0 5)
                    (safeCreatePsk pm <$> genSafeSigner
                                      <*> genPublicKey
                                      <*> genHeavyDlgIndex)
    Gen.filter allDistinct pSKList
  where
    allDistinct :: Eq a => [a] -> Bool
    allDistinct ls = length (nub ls) == length ls

genUndo :: ProtocolMagic -> Gen Undo
genUndo pm =
  Undo
    <$> Gen.list (Range.linear 1 10) (genProxySKHeavy pm)
    <*> Gen.set (Range.linear 0 10) genStakeholderId
