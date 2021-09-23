{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}

module Test.Cardano.Crypto.Limits
  ( tests,
  )
where

import Cardano.Binary (ToCBOR, serialize')
import Cardano.Crypto (AbstractHash, Signature (..), VerificationKey)
import qualified Cardano.Crypto.Wallet as CC
import Cardano.Prelude
import Crypto.Hash (Blake2b_224, Blake2b_256)
import Crypto.Hash.IO (HashAlgorithm, hashDigestSize)
import qualified Data.ByteString as BS
import Hedgehog
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import Test.Cardano.Crypto.Gen
  ( feedPM,
    genAbstractHash,
    genSignature,
    genVerificationKey,
  )
import Test.Cardano.Prelude

--------------------------------------------------------------------------------
-- Main Test Action
--------------------------------------------------------------------------------

tests :: IO Bool
tests = checkParallel $$discover

---------------------------------------------------------------------------
-- Limit
---------------------------------------------------------------------------

-- | A limit on the length of something (in bytes).
--   TODO should check for overflow in the Num instance.
--   Although, if the limit is anywhere near maxBound :: Word32 then something
--   is almost certainly amiss.
newtype Limit t = Limit
  { getLimit :: Word32
  }
  deriving (Eq, Ord, Show, Num, Enum, Real, Integral)

instance Functor Limit where
  fmap _ (Limit x) = Limit x

--------------------------------------------------------------------------------
-- Helper Functions
--------------------------------------------------------------------------------

mlAbstractHash ::
  forall algo a. HashAlgorithm algo => Limit (AbstractHash algo a)
mlAbstractHash =
  fromIntegral (hashDigestSize (panic "AbstractHash limit" :: algo) + 4)

mlVerificationKey :: Limit VerificationKey
mlVerificationKey = 66

mlXSignature :: Limit CC.XSignature
mlXSignature = 66

mlSignature :: Limit (Signature a)
mlSignature = Signature <$> mlXSignature

--------------------------------------------------------------------------------
-- Message Length Properties
--------------------------------------------------------------------------------

prop_pubKeyLenLimited :: Property
prop_pubKeyLenLimited = eachOf 1000 genVerificationKey (msgLenLimited mlVerificationKey)

prop_signatureLenLimited :: Property
prop_signatureLenLimited =
  eachOf
    1000
    (feedPM (\pm -> genSignature pm (Gen.list (Range.constant 0 1000) (pure ()))))
    (msgLenLimited mlSignature)

prop_abstractHash224LenLimited :: Property
prop_abstractHash224LenLimited =
  eachOf
    1000
    (genAbstractHash @Int32 @Blake2b_224 (Gen.int32 Range.constantBounded))
    (msgLenLimited mlAbstractHash)

prop_abstractHash256LenLimited :: Property
prop_abstractHash256LenLimited =
  eachOf
    1000
    (genAbstractHash @Int32 @Blake2b_256 (Gen.int32 Range.constantBounded))
    (msgLenLimited mlAbstractHash)

msgLenLimited :: ToCBOR a => Limit a -> a -> PropertyT IO ()
msgLenLimited limit a = assert $ BS.length (serialize' a) <= fromIntegral limit
