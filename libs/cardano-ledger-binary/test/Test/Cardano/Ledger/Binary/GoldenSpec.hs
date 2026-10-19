{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

module Test.Cardano.Ledger.Binary.GoldenSpec (spec) where

import Cardano.Crypto.DSIGN (
  createPossessionProofDSIGN,
  deriveVerKeyDSIGN,
  genKeyDSIGN,
  seedSizeDSIGN,
  signDSIGN,
 )
import Cardano.Crypto.Leios (
  LeiosCert,
  LeiosCommittee (..),
  LeiosDSIGN,
  LeiosSeat (..),
  LeiosSeatId (..),
  LeiosSignature,
  LeiosSigningKey,
  Weight,
  aggregateLeiosCert,
  mkLeiosCommittee,
 )
import Cardano.Crypto.Seed (mkSeedFromBytes)
import Cardano.Ledger.Binary (getVersion, natVersion)
import Control.Monad (forM_)
import qualified Data.ByteString as BS
import Data.Foldable (Foldable (..))
import Data.List.NonEmpty (NonEmpty)
import qualified Data.List.NonEmpty as NE
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe.Strict (StrictMaybe (..))
import Data.Proxy (Proxy (..))
import Data.Ratio ((%))
import qualified Data.Vector.Strict as V
import Data.Word
import Paths_cardano_ledger_binary (getDataFileName)
import Test.Cardano.Ledger.Binary.Golden (cborGoldenSpec)
import Test.Hspec (Spec, describe)

spec :: Spec
spec =
  describe "Golden" $ do
    forM_ [natVersion @12 .. maxBound] $ \version -> describe (show version) $ do
      let fileName = "golden/LeiosCertPV" <> show (getVersion @Word32 version) <> ".cbor"
          exampleLeiosCert =
            let sk = genKeyDSIGN (mkSeedFromBytes (BS.replicate 32 0))
                vk = deriveVerKeyDSIGN sk
                pop = createPossessionProofDSIGN sk
                committee = mkLeiosCommittee (V.fromList [(SJust (vk, pop), 1 % 1)])
                msg = "golden" :: BS.ByteString
                sigs = Map.singleton (LeiosSeatId 0) (signDSIGN () msg sk)
             in case aggregateLeiosCert committee sigs of
                  Right cert -> cert
                  Left e -> error ("exampleLeiosCert: " <> show e)
       in cborGoldenSpec getDataFileName fileName version exampleLeiosCert
      let fileName = "golden/LeiosSeatPV" <> show (getVersion @Word32 version) <> ".cbor"
          exampleLeiosSeat =
            LeiosSeat
              { seatWeight = 1 % 3
              , seatVKey = SJust (deriveVerKeyDSIGN (genKeyDSIGN (mkSeedFromBytes (BS.replicate 32 0))))
              }
       in cborGoldenSpec getDataFileName fileName version exampleLeiosSeat
      let fileName = "golden/LeiosCommitteePV" <> show (getVersion @Word32 version) <> ".cbor"
          exampleLeiosCommittee =
            UnsafeLeiosCommittee $
              V.fromList
                [ LeiosSeat (1 % 2) SNothing
                , LeiosSeat (1 % 2) (SJust (deriveVerKeyDSIGN (genKeyDSIGN (mkSeedFromBytes (BS.replicate 32 0)))))
                ]
       in cborGoldenSpec getDataFileName fileName version exampleLeiosCommittee
      cborGoldenSpec getDataFileName "test/golden/LeiosCert" version exampleCert

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
  , mkLeiosCommittee
      ( V.fromList
          [ ( SJust (deriveVerKeyDSIGN sk, createPossessionProofDSIGN leiosSignContext sk)
            , 1 / fromIntegral @Int @Weight n
            )
          | sk <- toList sks
          ]
      )
  )
  where
    seedLen = fromIntegral @Word @Int (seedSizeDSIGN (Proxy @LeiosDSIGN))
    sks =
      NE.fromList
        [ genKeyDSIGN @LeiosDSIGN (mkSeedFromBytes (BS.replicate seedLen (fromIntegral @Int @Word8 i)))
        | i <- [1 .. max 1 n]
        ]

-- | Sign @msg@ with each of the given keys and pack them into a 'Map' keyed
-- by 'LeiosVoterId', matching the input shape of 'aggregateLeiosCert'.
signContribs :: BS.ByteString -> [(Int, LeiosSigningKey)] -> Map LeiosSeatId LeiosSignature
signContribs msg pairs =
  Map.fromList
    [(LeiosSeatId (fromIntegral @Int @Word16 i), signDSIGN leiosSignContext msg sk) | (i, sk) <- pairs]
