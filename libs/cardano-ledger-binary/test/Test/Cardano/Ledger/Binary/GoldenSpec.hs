{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

module Test.Cardano.Ledger.Binary.GoldenSpec (spec) where

import Cardano.Crypto.DSIGN (
  DSIGNAlgorithm (deriveVerKeyDSIGN),
  genKeyDSIGN,
  seedSizeDSIGN,
  signDSIGN,
 )
import Cardano.Crypto.Leios (
  LeiosCert,
  LeiosCommittee (..),
  LeiosDSIGN,
  LeiosSignature,
  LeiosSigningKey,
  LeiosVoter (..),
  LeiosVoterId (..),
  Weight,
  aggregateLeiosCert,
  leiosSignContext,
 )
import Cardano.Crypto.Seed (mkSeedFromBytes)
import qualified Data.ByteString as BS
import Data.Foldable (toList)
import Data.List.NonEmpty (NonEmpty, fromList)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Proxy (Proxy (Proxy))
import qualified Data.Vector.Strict as V
import Data.Word (Word16, Word8)
import Paths_cardano_ledger_binary (getDataFileName)
import Test.Cardano.Ledger.Binary.Golden (cborGoldenSpec)
import Test.Hspec (Spec, describe)

spec :: Spec
spec =
  describe "Golden" $
    cborGoldenSpec getDataFileName "test/golden/LeiosCert" maxBound exampleCert

-- | Certificate over a fixed committee and message, pinned to the golden file that
-- originally lived in @cardano-base@'s @cardano-crypto-leios@ test suite. Its
-- construction must not change, otherwise the golden encoding no longer matches.
exampleCert :: LeiosCert
exampleCert = case aggregateLeiosCert committee contributions of
  Right c -> c
  Left e -> error ("exampleCert: aggregation failed: " <> show e)
  where
    (sks, committee) = fixedCommittee 1000
    msg = "leios-golden-message" :: BS.ByteString
    contributions = signContribs msg (zip [0 ..] (toList sks))

-- | Equal-weighted committee of @n@ voters derived from a fixed seed pattern.
-- Returns the signing keys alongside the committee so that contributions can be
-- produced.
fixedCommittee :: Int -> (NonEmpty LeiosSigningKey, LeiosCommittee)
fixedCommittee n =
  ( sks
  , LeiosCommittee
      ( V.fromList
          [LeiosVoter (1 / fromIntegral @Int @Weight n) (deriveVerKeyDSIGN sk) | sk <- toList sks]
      )
  )
  where
    seedLen = fromIntegral @Word @Int (seedSizeDSIGN (Proxy @LeiosDSIGN))
    sks =
      fromList
        [ genKeyDSIGN @LeiosDSIGN (mkSeedFromBytes (BS.replicate seedLen (fromIntegral @Int @Word8 i)))
        | i <- [1 .. max 1 n]
        ]

-- | Sign @msg@ with each of the given keys and pack them into a 'Map' keyed
-- by 'LeiosVoterId', matching the input shape of 'aggregateLeiosCert'.
signContribs :: BS.ByteString -> [(Int, LeiosSigningKey)] -> Map LeiosVoterId LeiosSignature
signContribs msg pairs =
  Map.fromList
    [(LeiosVoterId (fromIntegral @Int @Word16 i), signDSIGN leiosSignContext msg sk) | (i, sk) <- pairs]
