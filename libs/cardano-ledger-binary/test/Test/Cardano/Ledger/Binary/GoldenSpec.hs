{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

module Test.Cardano.Ledger.Binary.GoldenSpec (spec) where

import Cardano.Crypto.DSIGN (
  createPossessionProofDSIGN,
  deriveVerKeyDSIGN,
  genKeyDSIGN,
  signDSIGN,
 )
import Cardano.Crypto.Leios (
  LeiosCommittee (..),
  LeiosSeat (..),
  LeiosSeatId (..),
  aggregateLeiosCert,
  leiosSignContext,
  mkLeiosCommittee,
 )
import Cardano.Crypto.Seed (mkSeedFromBytes)
import Cardano.Ledger.Binary (getVersion, natVersion)
import Control.Monad (forM_)
import qualified Data.ByteString as BS
import qualified Data.Map.Strict as Map
import Data.Maybe.Strict (StrictMaybe (..))
import Data.Ratio ((%))
import qualified Data.Vector.Strict as V
import Data.Word
import Paths_cardano_ledger_binary (getDataFileName)
import Test.Cardano.Ledger.Binary.Golden (cborGoldenSpec)
import Test.Hspec (Spec, describe)

spec :: Spec
spec =
  describe "Golden" $ do
    forM_ [natVersion @12 .. maxBound] $ \version ->
      let fileName = "golden/LeiosCertPV" <> show (getVersion @Word32 version) <> ".cbor"
          exampleLeiosCert =
            let sk = genKeyDSIGN (mkSeedFromBytes (BS.replicate 32 0))
                vk = deriveVerKeyDSIGN sk
                pop = createPossessionProofDSIGN leiosSignContext sk
                committee = mkLeiosCommittee (V.fromList [(SJust (vk, pop), 1 % 1)])
                msg = "golden" :: BS.ByteString
                sigs = Map.singleton (LeiosSeatId 0) (signDSIGN leiosSignContext msg sk)
             in case aggregateLeiosCert committee sigs of
                  Right cert -> cert
                  Left e -> error ("exampleLeiosCert: " <> show e)
       in cborGoldenSpec getDataFileName fileName version exampleLeiosCert
    forM_ [natVersion @12 .. maxBound] $ \version ->
      let fileName = "golden/LeiosSeatPV" <> show (getVersion @Word32 version) <> ".cbor"
          exampleLeiosSeat =
            LeiosSeat
              { seatWeight = 1 % 3
              , seatVKey = SJust (deriveVerKeyDSIGN (genKeyDSIGN (mkSeedFromBytes (BS.replicate 32 0))))
              }
       in cborGoldenSpec getDataFileName fileName version exampleLeiosSeat
    forM_ [natVersion @12 .. maxBound] $ \version ->
      let fileName = "golden/LeiosCommitteePV" <> show (getVersion @Word32 version) <> ".cbor"
          exampleLeiosCommittee =
            UnsafeLeiosCommittee $
              V.fromList
                [ LeiosSeat (1 % 2) SNothing
                , LeiosSeat (1 % 2) (SJust (deriveVerKeyDSIGN (genKeyDSIGN (mkSeedFromBytes (BS.replicate 32 0)))))
                ]
       in cborGoldenSpec getDataFileName fileName version exampleLeiosCommittee
