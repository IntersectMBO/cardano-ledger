{-# LANGUAGE TemplateHaskell  #-}
{-# LANGUAGE TypeApplications #-}

module Test.Cardano.Crypto.Limits
  ( tests
  )
where

import Cardano.Prelude
import Test.Cardano.Prelude

import Crypto.Hash (Blake2b_224, Blake2b_256)
import qualified Data.ByteString as BS

import Cardano.Binary (Limit, ToCBOR, serialize')
import Cardano.Crypto.Limits (mlAbstractHash, mlVerificationKey, mlSignature)

import Hedgehog
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range

import Test.Cardano.Crypto.Gen
  (feedPM, genAbstractHash, genVerificationKey, genSignature)


--------------------------------------------------------------------------------
-- Main Test Action
--------------------------------------------------------------------------------

tests :: IO Bool
tests = checkParallel $$discover


--------------------------------------------------------------------------------
-- Message Length Properties
--------------------------------------------------------------------------------

prop_pubKeyLenLimited :: Property
prop_pubKeyLenLimited = eachOf 1000 genVerificationKey (msgLenLimited mlVerificationKey)

prop_signatureLenLimited :: Property
prop_signatureLenLimited = eachOf
  1000
  (feedPM (\pm -> genSignature pm (Gen.list (Range.constant 0 1000) (pure ()))))
  (msgLenLimited mlSignature)

prop_abstractHash224LenLimited :: Property
prop_abstractHash224LenLimited = eachOf
  1000
  (genAbstractHash @Int32 @Blake2b_224 (Gen.int32 Range.constantBounded))
  (msgLenLimited mlAbstractHash)

prop_abstractHash256LenLimited :: Property
prop_abstractHash256LenLimited = eachOf
  1000
  (genAbstractHash @Int32 @Blake2b_256 (Gen.int32 Range.constantBounded))
  (msgLenLimited mlAbstractHash)

msgLenLimited :: ToCBOR a => Limit a -> a -> PropertyT IO ()
msgLenLimited limit a = assert $ BS.length (serialize' a) <= fromIntegral limit
