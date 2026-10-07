{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Test.Cardano.Ledger.Conformance.Spec.Dijkstra.Foreign (spec) where

import Cardano.Crypto.Util (bytesToNatural)
import Cardano.Ledger.BaseTypes (
  EpochInterval (..),
  EpochNo (..),
  EpochSize (..),
  Network (Testnet),
  StrictMaybe (..),
  unsafeNonZero,
 )
import Cardano.Ledger.Binary (FixedSizeCodec (..))
import Cardano.Ledger.Coin (Coin (..), CompactForm (..))
import Cardano.Ledger.Core (KeyHash, StakePool)
import Cardano.Ledger.Credential (Credential (..))
import Cardano.Ledger.Dijkstra (DijkstraEra)
import Cardano.Ledger.Dijkstra.PParams (ppLeiosCommitteeSizeL)
import Cardano.Ledger.Dijkstra.Transition (seatInitialLeiosCommittee)
import Cardano.Ledger.Shelley.Genesis (ShelleyGenesis (..))
import Cardano.Ledger.Shelley.LedgerState (
  NewEpochState,
  curPParamsEpochStateL,
  esSnapshotsL,
  nesELL,
  nesEsL,
  nesStakePoolDistrG,
 )
import Cardano.Ledger.Shelley.Transition (EraTransition (TransitionConfig, tcShelleyGenesisL))
import Cardano.Ledger.State (
  ActiveStake (..),
  BlsKey (..),
  BlsKeyState (..),
  LeiosCommittee (..),
  LeiosSeat (..),
  MarkSnapShot (..),
  SetSnapShot (..),
  SnapShot (..),
  SnapShots (..),
  StakePoolSnapShot (..),
  StakeWithDelegation (..),
  calculatePoolDistr,
  mkGoSnapShot,
  mkMarkSnapShot,
  mkSetSnapShot,
  mkSnapShot,
  mkStakePoolSnapShot,
  msSnapShotL,
  ssLeiosCommitteeL,
  ssStakeGoL,
  ssStakeMarkL,
  ssStakeSetL,
 )
import Data.Default (def)
import Data.Either (isLeft)
import Data.List (sort)
import Data.Ratio ((%))
import Data.Text (Text)
import Data.Word (Word64)
import GHC.Exts (fromList)
import Lens.Micro ((&), (.~), (^.))
import qualified MAlonzo.Code.Ledger.Core.Foreign.API as Agda
import qualified MAlonzo.Code.Ledger.Dijkstra.Foreign.API as Dijkstra
import Test.Cardano.Ledger.Common
import Test.Cardano.Ledger.Conformance (SpecTranslate (..), externalFunctions, runSpecTransM)
import Test.Cardano.Ledger.Conformance.SpecTranslate.Dijkstra ()
import Test.Cardano.Ledger.Core.Arbitrary ()
import Test.Cardano.Ledger.Core.KeyPair (mkKeyHash)
import Test.Cardano.Ledger.Dijkstra.Arbitrary ()
import Test.Cardano.Ledger.Shelley.Examples (testShelleyGenesis)

spec :: Spec
spec = describe "Foreign interface premises" $ do
  prop "accepts an independently generated matching proof" $ \(BlsKey key proof) ->
    Agda.extIsValidPoP externalFunctions (encodeInteger key) (encodeInteger proof) === True
  prop "rejects a proof for a different verification key" $ \(BlsKey key _) (BlsKey otherKey proof) ->
    key
      /= otherKey
        ==> Agda.extIsValidPoP externalFunctions (encodeInteger key) (encodeInteger proof)
        === False
  it "rejects malformed natural-number encodings" $ do
    Agda.extIsValidPoP externalFunctions (-1) 0 `shouldBe` False
    Agda.extIsValidPoP externalFunctions 0 (-1) `shouldBe` False
    Agda.extIsValidPoP externalFunctions 0 0 `shouldBe` False
    Agda.extIsValidPoP externalFunctions (256 ^ (96 :: Int)) 0 `shouldBe` False
    Agda.extIsValidPoP externalFunctions 0 (256 ^ (48 :: Int)) `shouldBe` False

  describe "Snapshot cache and formal projection" $ do
    it "projects the current set pool amounts rather than the different mark pools" $ do
      let poolId = mkKeyHash 20
          setSnap = activeSnapshotWith poolId SNothing
          set = mkSetSnapShot $ mkMarkSnapShot setSnap (EpochNo 0) 1 (EpochInterval 10)
          mark = mkMarkSnapShot (activeSnapshotWith (mkKeyHash 21) SNothing) (EpochNo 1) 1 (EpochInterval 10)
          state =
            (def :: NewEpochState DijkstraEra)
              & nesELL
                .~ EpochNo 1
              & nesEsL
                . esSnapshotsL
                . ssStakeMarkL
                .~ mark
              & nesEsL
                . esSnapshotsL
                . ssStakeSetL
                .~ set
      (state ^. nesStakePoolDistrG) `shouldBe` calculatePoolDistr setSnap
      (state ^. nesStakePoolDistrG) `shouldNotBe` msStakePoolDistr mark
      expectedPool <- expectRight $ runSpecTransM () $ toSpecRep @DijkstraEra poolId
      model <- expectRight $ runSpecTransM (Testnet, EpochInterval 10) $ toSpecRep @DijkstraEra state
      -- The foreign pool-distribution contract stores coin amounts, not stake ratios.
      Dijkstra.nesPd model `shouldBe` Agda.MkHSMap [(expectedPool, 100)]

    prop "refreshes both mark memoizations when its snapshot lens changes" $ \key -> ioProperty $ do
      let original = mkMarkSnapShot (activeSnapshotWith (mkKeyHash 22) SNothing) (EpochNo 0) 1 (EpochInterval 10)
          replacement = activeSnapshotWith (mkKeyHash 23) (SJust (BlsKeyState key (EpochNo 0)))
          updated = original & msSnapShotL .~ replacement
          rebuilt =
            mkMarkSnapShot
              replacement
              (msEpochNo original)
              (msLeiosCommitteeSize original)
              (msLeiosMaxKeyAge original)
      updated `shouldBe` rebuilt
      msStakePoolDistr updated `shouldNotBe` msStakePoolDistr original
      msLeiosCommittee updated `shouldNotBe` msLeiosCommittee original
      pure True

    prop "seeds genesis set and go with the live epoch-zero committee and pool cache" $
      \(config :: TransitionConfig DijkstraEra) key@(BlsKey verificationKey _) -> ioProperty $ do
        let cfg = config & tcShelleyGenesisL .~ testShelleyGenesis
            EpochSize slotsPerEpoch = sgEpochLength testShelleyGenesis
            -- Genesis activation delay: ceil(KES lifetime / epoch length) plus two epochs.
            maxKeyAge =
              EpochInterval $
                ceiling
                  ((sgMaxKESEvolutions testShelleyGenesis * sgSlotsPerKESPeriod testShelleyGenesis) % slotsPerEpoch)
                  + 2
            poolId = mkKeyHash 24
            snap = activeSnapshotWith poolId (SJust (BlsKeyState key (EpochNo 0)))
            initial =
              (def :: NewEpochState DijkstraEra)
                & nesEsL
                  . curPParamsEpochStateL
                  . ppLeiosCommitteeSizeL
                  .~ 1
                & nesEsL
                  . esSnapshotsL
                  . ssStakeMarkL
                  .~ mkMarkSnapShot snap (EpochNo 0) 1 maxKeyAge
            seeded = seatInitialLeiosCommittee cfg initial
            mark = seeded ^. nesEsL . esSnapshotsL . ssStakeMarkL
            set = seeded ^. nesEsL . esSnapshotsL . ssStakeSetL
        (seeded ^. nesELL) `shouldBe` EpochNo 0
        msEpochNo mark `shouldBe` EpochNo 0
        msLeiosMaxKeyAge mark `shouldBe` maxKeyAge
        set `shouldBe` mkSetSnapShot mark
        (seeded ^. nesEsL . esSnapshotsL . ssStakeGoL) `shouldBe` mkGoSnapShot set
        (seeded ^. nesStakePoolDistrG) `shouldBe` calculatePoolDistr snap
        expectedPool <- expectRight $ runSpecTransM () $ toSpecRep @DijkstraEra poolId
        model <- expectRight $ runSpecTransM (Testnet, maxKeyAge) $ toSpecRep @DijkstraEra seeded
        Dijkstra.nesPd model `shouldBe` Agda.MkHSMap [(expectedPool, 100)]
        map Dijkstra.lsPool (Dijkstra.nesLeiosCommittee model) `shouldBe` [expectedPool]
        map Dijkstra.lsWeight (Dijkstra.nesLeiosCommittee model) `shouldBe` [1]
        map Dijkstra.lsKey (Dijkstra.nesLeiosCommittee model)
          `shouldBe` [Just (encodeInteger verificationKey)]
        pure True

  describe "Leios committee representation boundary" $ do
    it "translates an empty committee" $
      case runSpecTransM
        (Testnet, EpochInterval 10)
        (toSpecRep @DijkstraEra (def :: NewEpochState DijkstraEra)) of
        Left err -> expectationFailure (show err)
        Right model -> Dijkstra.nesLeiosCommittee model `shouldBe` []
    it "rejects a committee whose pool IDs are absent rather than inventing seat identity" $ do
      let state =
            (def :: NewEpochState DijkstraEra)
              & nesEsL
                . esSnapshotsL
                . ssStakeSetL
                . ssLeiosCommitteeL
                .~ UnsafeLeiosCommittee (fromList [LeiosSeat 0 SNothing])
      runSpecTransM (Testnet, EpochInterval 10) (toSpecRep @DijkstraEra state) `shouldSatisfy` isLeft
    it "recovers keyless identities by descending stake and ascending pool ID at ties" $ do
      let tiedPools = sort [mkKeyHash 1, mkKeyHash 2]
          pools =
            [(mkKeyHash 0, poolWith 10 SNothing)]
              <> [(poolId, poolWith 40 SNothing) | poolId <- reverse tiedPools]
          snapshot =
            snapshotWith
              pools
              [LeiosSeat (40 % 100) SNothing, LeiosSeat (40 % 100) SNothing, LeiosSeat (10 % 100) SNothing]
      translated <- expectRight $ translateAt 0 snapshot
      expected <-
        expectRight $ traverse (runSpecTransM () . toSpecRep @DijkstraEra) (tiedPools <> [mkKeyHash 0])
      map Dijkstra.lsPool translated `shouldBe` expected
    it "preserves registered zero-stake seats" $ do
      let snapshot = snapshotWith [(mkKeyHash 0, poolWith 0 SNothing)] [LeiosSeat 0 SNothing]
      translated <-
        expectRight $
          translateAt 0 snapshot
      map Dijkstra.lsWeight translated `shouldBe` [0]
      length translated `shouldBe` 1
      let state =
            (def :: NewEpochState DijkstraEra)
              & nesEsL
                . esSnapshotsL
                . ssStakeSetL
                .~ snapshot
      model <- expectRight $ runSpecTransM (Testnet, EpochInterval 10) $ toSpecRep @DijkstraEra state
      Dijkstra.nesLeiosCommittee model `shouldBe` translated
    it "uses the retained seat count as a top-ranking selection bound" $ do
      translated <-
        expectRight $
          translateAt 0 $
            snapshotWith
              [(mkKeyHash 0, poolWith 10 SNothing), (mkKeyHash 1, poolWith 90 SNothing)]
              [LeiosSeat (90 % 100) SNothing]
      expected <-
        expectRight $ runSpecTransM () $ toSpecRep @DijkstraEra (mkKeyHash 1 :: KeyHash StakePool)
      map Dijkstra.lsPool translated `shouldBe` [expected]
    it "accepts a historical zero-size selection even when the snapshot has pools" $
      translateAt 0 (snapshotWith [(mkKeyHash 0, poolWith 100 SNothing)] []) `shouldBe` Right []
    it "rejects a seat weight that disagrees with its ranked pool" $
      translateAt 0 (snapshotWith [(mkKeyHash 0, poolWith 100 SNothing)] [LeiosSeat (1 % 2) SNothing])
        `shouldSatisfy` isLeft
    it "rejects extra seats instead of truncating them to the snapshot" $
      translateAt
        0
        (snapshotWith [(mkKeyHash 0, poolWith 100 SNothing)] [LeiosSeat 1 SNothing, LeiosSeat 1 SNothing])
        `shouldSatisfy` isLeft
    prop "recovers separate pool identities when voting keys are duplicated" $ \key@(BlsKey verificationKey _) -> ioProperty $ do
      let registeredKey = SJust (BlsKeyState key (EpochNo 0))
          snapshot =
            snapshotWith
              [(mkKeyHash 0, poolWith 60 registeredKey), (mkKeyHash 1, poolWith 40 registeredKey)]
              [LeiosSeat (60 % 100) (SJust verificationKey), LeiosSeat (40 % 100) (SJust verificationKey)]
      translated <- expectRight $ translateAt 0 snapshot
      expected <-
        expectRight $
          traverse (runSpecTransM () . toSpecRep @DijkstraEra) [mkKeyHash 0 :: KeyHash StakePool, mkKeyHash 1]
      map Dijkstra.lsPool translated `shouldBe` expected
      map Dijkstra.lsKey translated `shouldBe` replicate 2 (Just (encodeInteger verificationKey))
      pure True
    prop "rejects a different voting key instead of assigning identity by key lookup" $ \key@(BlsKey verificationKey _) (BlsKey otherKey _) ->
      verificationKey
        /= otherKey
          ==> isLeft
            ( translateAt
                0
                ( snapshotWith
                    [(mkKeyHash 0, poolWith 100 (SJust (BlsKeyState key (EpochNo 0))))]
                    [LeiosSeat 1 (SJust otherKey)]
                )
            )
    prop "rejects an absent key that is still honoured" $ \key ->
      isLeft
        ( translateAt
            0
            ( snapshotWith
                [(mkKeyHash 0, poolWith 100 (SJust (BlsKeyState key (EpochNo 0))))]
                [LeiosSeat 1 SNothing]
            )
        )
    prop "honours the exact registration-age boundary" $ \key@(BlsKey verificationKey _) -> ioProperty $ do
      let pools = [(mkKeyHash 0, poolWith 100 (SJust (BlsKeyState key (EpochNo 0))))]
          live = snapshotWith pools [LeiosSeat 1 (SJust verificationKey)]
          expired = snapshotWith pools [LeiosSeat 1 SNothing]
      translated <- expectRight $ translateAt 9 live
      map Dijkstra.lsKey translated `shouldBe` [Just (encodeInteger verificationKey)]
      translateAt 10 live `shouldSatisfy` isLeft
      expiredTranslated <- expectRight $ translateAt 10 expired
      map Dijkstra.lsKey expiredTranslated `shouldBe` [Nothing]
      pure True
    prop "preserves a keyless seat when the registered proof does not match" $ \(BlsKey verificationKey _) (BlsKey otherKey proof) ->
      verificationKey
        /= otherKey
          ==> ioProperty
            ( do
                let badKey = BlsKey verificationKey proof
                translated <-
                  expectRight $
                    translateAt 0 $
                      snapshotWith
                        [(mkKeyHash 0, poolWith 100 (SJust (BlsKeyState badKey (EpochNo 0))))]
                        [LeiosSeat 1 SNothing]
                map Dijkstra.lsKey translated `shouldBe` [Nothing]
                pure True
            )

poolWith :: Word64 -> StrictMaybe BlsKeyState -> StakePoolSnapShot
poolWith stake key =
  (mkStakePoolSnapShot (ActiveStake mempty) (unsafeNonZero (Coin 100)) def)
    { spssStake = CompactCoin stake
    , spssStakeRatio = toInteger stake % 100
    , spssBlsKey = key
    }

snapshotWith :: [(KeyHash StakePool, StakePoolSnapShot)] -> [LeiosSeat] -> SetSnapShot
snapshotWith pools seats =
  let initial = ssStakeSet (def :: SnapShots DijkstraEra)
      snap = (ssSnapShot initial) {ssStakePoolsSnapShot = fromList pools}
   in initial
        { ssSnapShot = snap
        , ssStakePoolDistr = calculatePoolDistr snap
        , ssLeiosCommittee = UnsafeLeiosCommittee (fromList seats)
        }

-- A real active delegator makes the pool survive calculatePoolDistr's eligibility filter.
activeSnapshotWith :: KeyHash StakePool -> StrictMaybe BlsKeyState -> SnapShot
activeSnapshotWith poolId key =
  let active =
        ActiveStake $
          fromList [(KeyHashObj (mkKeyHash 25), StakeWithDelegation (unsafeNonZero (CompactCoin 100)) poolId)]
      pool = (poolWith 100 key) {spssNumDelegators = 1}
   in mkSnapShot active (fromList [(poolId, pool)])

translateAt :: Word64 -> SetSnapShot -> Either Text [Dijkstra.LeiosSeat]
translateAt epoch snapshot =
  let state =
        (def :: NewEpochState DijkstraEra)
          & nesELL
            .~ EpochNo epoch
          & nesEsL
            . esSnapshotsL
            . ssStakeSetL
            .~ snapshot
   in Dijkstra.nesLeiosCommittee
        <$> runSpecTransM (Testnet, EpochInterval 10) (toSpecRep @DijkstraEra state)

encodeInteger :: FixedSizeCodec a => a -> Integer
encodeInteger = toInteger . bytesToNatural . rawEncodeFixedSized
