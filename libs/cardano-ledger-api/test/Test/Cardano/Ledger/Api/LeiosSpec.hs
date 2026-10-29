{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE TypeApplications #-}

module Test.Cardano.Ledger.Api.LeiosSpec (spec) where

import Cardano.Crypto.DSIGN (signDSIGN)
import Cardano.Crypto.Hash.Class (hashToBytes)
import Cardano.Crypto.Leios (LeiosSeatId (..), aggregateLeiosCert, leiosSignContext)
import Cardano.Ledger.Api.Era (DijkstraEra)
import Cardano.Ledger.Api.Leios
import Cardano.Ledger.BaseTypes
import Cardano.Ledger.Hashes (extractHash)
import Cardano.Ledger.Shelley.LedgerState (nesELL)
import Cardano.Ledger.Slot (epochInfoFirst)
import Cardano.Slotting.EpochInfo (EpochInfo (epochInfoSlotLength_))
import Cardano.Slotting.Time (mkSlotLength)
import Data.List.NonEmpty (NonEmpty (..))
import qualified Data.Map.Strict as Map
import Data.Proxy (Proxy (..))
import Lens.Micro.Mtl (use)
import Test.Cardano.Crypto.Leios.Gen (TestSeatType (..), mkCommitteeFromTestSeats)
import Test.Cardano.Ledger.Core.Rational ((%!))
import Test.Cardano.Ledger.Dijkstra.ImpTest
import Test.Cardano.Ledger.Imp.Common

mkValidLeiosCertWithEnv :: ImpTestM DijkstraEra (LeiosCertEnv, LeiosCert)
mkValidLeiosCertWithEnv = do
  headerHash <- arbitrary
  annSlot <- getCurSlotNo
  let (keys, committee) =
        mkCommitteeFromTestSeats $ replicate 3 (WithKey, 1 / 4) <> [(NoKey, 1 / 4)]
      env =
        LeiosCertEnv
          { lseCurrentSlotNo = annSlot + 14
          , lseAnnouncementSlotNo = annSlot
          , lseAnnouncedInBlockHeader = headerHash
          , lseLeiosCommittee = committee
          , lseThreshold = 3 %! 4
          , lseAnnouncementPeriodLength = Milliseconds32 1_000
          , lseVotePeriodLength = Milliseconds32 4_000
          , lseDiffusionPeriodLength = Milliseconds32 7_000
          }
      msg = hashToBytes $ extractHash headerHash
  cert <-
    expectRightDeep $
      aggregateLeiosCert committee $
        Map.fromList
          [ (LeiosSeatId i, signDSIGN leiosSignContext msg key)
          | (i, key) <- zip [0 ..] (take 3 keys)
          ]
  pure (env, cert)

spec :: Spec
spec = withImpInitEachEraVersion (Proxy @DijkstraEra) $
  describe "validateLeiosCert" $ do
    it "Accepts a valid certificate at the earliest slot" $ do
      (env, cert) <- mkValidLeiosCertWithEnv
      globals <- use impGlobalsL
      validateLeiosCert @DijkstraEra globals env cert `shouldBe` Right ()

    it "Returns a signature failure for a different announcing header" $ do
      (env, cert) <- mkValidLeiosCertWithEnv
      globals <- use impGlobalsL
      otherHash <- arbitrary
      validateLeiosCert @DijkstraEra globals env {lseAnnouncedInBlockHeader = otherHash} cert
        `shouldBe` Left (LeiosCertInvalidSignature :| [])

    it "Preserves multiple predicate failures" $ do
      (env, cert) <- mkValidLeiosCertWithEnv
      globals <- use impGlobalsL
      epoch <- getsNES nesELL
      let cur = epochInfoFirst (epochInfoPure globals) (epoch + 1)
          ann = cur - 1
          env' = env {lseCurrentSlotNo = cur, lseAnnouncementSlotNo = ann}
      validateLeiosCert @DijkstraEra globals env' cert
        `shouldBe` Left
          ( LeiosCertTooEarly (Mismatch cur (ann + 14))
              :| [LeiosCertAnnouncedInPreviousEpoch (Mismatch (epoch + 1) epoch)]
          )

    it "Uses the slot length from the supplied Globals" $ do
      (env, cert) <- mkValidLeiosCertWithEnv
      globals <- use impGlobalsL
      let cur = lseCurrentSlotNo env - 1
          env' = env {lseCurrentSlotNo = cur}
          longerSlots =
            globals
              { epochInfo =
                  (epochInfo globals) {epochInfoSlotLength_ = const $ pure $ mkSlotLength 2}
              }
      validateLeiosCert @DijkstraEra globals env' cert
        `shouldBe` Left (LeiosCertTooEarly (Mismatch cur (lseCurrentSlotNo env)) :| [])
      validateLeiosCert @DijkstraEra longerSlots env' cert `shouldBe` Right ()
