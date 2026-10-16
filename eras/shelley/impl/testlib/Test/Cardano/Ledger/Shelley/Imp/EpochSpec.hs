{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Test.Cardano.Ledger.Shelley.Imp.EpochSpec (
  spec,
) where

import Cardano.Ledger.BaseTypes (EpochInterval (..), addEpochInterval, epochInfoPure, unNonZero)
import Cardano.Ledger.Coin (Coin (..))
import Cardano.Ledger.Compactible (fromCompact)
import Cardano.Ledger.Core
import Cardano.Ledger.Credential (Credential (..))
import Cardano.Ledger.Shelley.API.Forecast (futureForecast, poolDistrForecastL)
import Cardano.Ledger.Shelley.LedgerState (
  esLStateL,
  esSnapshotsL,
  gsSnapShotL,
  lsCertStateL,
  lsUTxOStateL,
  msSnapShotL,
  nesELL,
  nesEsL,
  nesStakePoolDistrG,
  ssSnapShotL,
  ssStakeGoL,
  ssStakeMarkL,
  ssStakeSetL,
  totalObligation,
  utxosDepositedL,
  utxosGovStateL,
 )
import Cardano.Ledger.Slot (epochInfoFirst)
import Cardano.Ledger.State (
  PoolDistr (..),
  SnapShot (..),
  StakePoolSnapShot (..),
 )
import Cardano.Ledger.Val (Val (..))
import Data.Foldable (fold)
import qualified Data.Map.Strict as Map
import qualified Data.VMap as VMap
import Lens.Micro ((^.))
import Lens.Micro.Mtl (use)
import Test.Cardano.Ledger.Imp.Common
import Test.Cardano.Ledger.Shelley.ImpTest

spec ::
  forall era.
  ShelleyEraImp era =>
  SpecWith (ImpInit (LedgerSpec era))
spec = describe "EPOCH" $ do
  it "Runs basic transaction" $ do
    do
      certState <- getsNES $ nesEsL . esLStateL . lsCertStateL
      govState <- getsNES $ nesEsL . esLStateL . lsUTxOStateL . utxosGovStateL
      totalObligation certState govState `shouldBe` zero
    do
      deposited <- getsNES $ nesEsL . esLStateL . lsUTxOStateL . utxosDepositedL
      deposited `shouldBe` zero
    submitTxAnn_ "simple transaction" $ mkBasicTx mkBasicTxBody
    passEpoch

  it "Crosses epoch boundaries" $ do
    startEpochNo <- getsNES nesELL
    Positive n <- arbitrary
    passNEpochs $ fromIntegral n
    getsNES nesELL `shouldReturn` addEpochInterval startEpochNo (EpochInterval n)

  it "Forecast across the epoch boundary agrees with TICK" $ do
    -- Delegate stake to a fresh pool and cross a boundary, so that the mark snapshot holds that
    -- stake while the set snapshot does not
    pool <- freshKeyHash
    registerPool pool
    stakingCred <- KeyHashObj <$> freshKeyHash
    _ <- registerStakeCredential stakingCred
    delegateStake stakingCred pool
    paymentKeyHash <- freshKeyHash @Payment
    sendCoinTo_ (mkAddr paymentKeyHash stakingCred) (Coin 1_000_000_000)
    passEpoch
    markSnapShot <- getsNES $ nesEsL . esSnapshotsL . ssStakeMarkL . msSnapShotL
    setSnapShot <- getsNES $ nesEsL . esSnapshotsL . ssStakeSetL . ssSnapShotL
    markSnapShot `shouldNotBe` setSnapShot

    globals <- use impGlobalsL
    nes <- getsNES id
    -- The first slot of the next epoch, where TICKF crosses the boundary
    let nextEpochFirstSlot = epochInfoFirst (epochInfoPure globals) (succ (nes ^. nesELL))
        forecastPoolDistr = futureForecast globals nextEpochFirstSlot nes ^. poolDistrForecastL
    forecastPoolDistr `shouldNotBe` (nes ^. nesStakePoolDistrG)

    passEpoch
    getsNES nesStakePoolDistrG `shouldReturn` forecastPoolDistr

  it "Genesis staking seeds the mark, set and go snapshots" $ do
    poolStake <- injectFreshGenesisStakePools 3
    snapShots <- getsNES $ nesEsL . esSnapshotsL
    let markSnapShot = snapShots ^. ssStakeMarkL . msSnapShotL
    -- SNAP rotates the snapshots, but at genesis there is nothing to rotate yet, so all of
    -- them are seeded with the same stake distribution
    snapShots ^. ssStakeSetL . ssSnapShotL `shouldBe` markSnapShot
    snapShots ^. ssStakeGoL . gsSnapShotL `shouldBe` markSnapShot
    -- the total active stake agrees with the sum of the delegated stake pools (#5788)
    fromCompact . spssStake
      <$> VMap.toMap (ssStakePoolsSnapShot markSnapShot)
        `shouldBe` poolStake
    unNonZero (ssTotalActiveStake markSnapShot) `shouldBe` fold poolStake
    -- every genesis pool is in the stake distribution that leader election uses right away
    poolDistr <- getsNES nesStakePoolDistrG
    Map.keysSet (unPoolDistr poolDistr) `shouldBe` Map.keysSet poolStake
