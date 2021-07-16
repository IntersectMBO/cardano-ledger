{-# LANGUAGE TemplateHaskell #-}

module Test.Cardano.Crypto.Signing.Redeem
  ( tests,
  )
where

import Cardano.Crypto.Signing (SignTag (..))
import Cardano.Crypto.Signing.Redeem
  ( redeemSign,
    redeemToVerification,
    verifyRedeemSig,
  )
import Cardano.Prelude
import Hedgehog
  ( Gen,
    Property,
    assert,
    checkParallel,
    discover,
    forAll,
    property,
  )
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import qualified Test.Cardano.Crypto.Dummy as Dummy
import Test.Cardano.Crypto.Gen
  ( genRedeemKeypair,
    genRedeemSigningKey,
    genRedeemVerificationKey,
  )

--------------------------------------------------------------------------------
-- Main Test Action
--------------------------------------------------------------------------------

tests :: IO Bool
tests = checkParallel $$discover

--------------------------------------------------------------------------------
-- Redeem Signature Properties
--------------------------------------------------------------------------------

-- | Signing and verification with a redeem keys works
prop_redeemSign :: Property
prop_redeemSign = property $ do
  (vk, sk) <- forAll genRedeemKeypair
  a <- forAll genData

  assert $
    verifyRedeemSig Dummy.protocolMagicId SignForTestingOnly vk a $
      redeemSign Dummy.protocolMagicId SignForTestingOnly sk a

-- | Signing fails when the wrong 'RedeemVerificationKey' is used
prop_redeemSignDifferentKey :: Property
prop_redeemSignDifferentKey = property $ do
  sk <- forAll genRedeemSigningKey
  vk <- forAll $ Gen.filter (/= redeemToVerification sk) genRedeemVerificationKey
  a <- forAll genData

  assert
    . not
    $ verifyRedeemSig Dummy.protocolMagicId SignForTestingOnly vk a $
      redeemSign Dummy.protocolMagicId SignForTestingOnly sk a

-- | Signing fails when then wrong signature data is used
prop_redeemSignDifferentData :: Property
prop_redeemSignDifferentData = property $ do
  (vk, sk) <- forAll genRedeemKeypair
  a <- forAll genData
  b <- forAll $ Gen.filter (/= a) genData

  assert
    . not
    $ verifyRedeemSig Dummy.protocolMagicId SignForTestingOnly vk b $
      redeemSign Dummy.protocolMagicId SignForTestingOnly sk a

genData :: Gen [Int32]
genData = Gen.list (Range.constant 0 50) (Gen.int32 Range.constantBounded)
