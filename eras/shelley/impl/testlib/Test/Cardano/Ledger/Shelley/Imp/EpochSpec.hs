{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Test.Cardano.Ledger.Shelley.Imp.EpochSpec (
  spec,
) where

import Cardano.Ledger.BaseTypes (EpochInterval (..), addEpochInterval, epochInfoPure)
import Cardano.Ledger.Coin (Coin (..))
import Cardano.Ledger.Core
import Cardano.Ledger.Credential (Credential (..))
import Cardano.Ledger.Shelley.API.Forecast (futureForecast, poolDistrForecastL)
import Cardano.Ledger.Shelley.LedgerState (
  esLStateL,
  esSnapshotsL,
  lsCertStateL,
  lsUTxOStateL,
  msSnapShotL,
  nesELL,
  nesEsL,
  nesStakePoolDistrG,
  ssSnapShotL,
  ssStakeMarkL,
  ssStakeSetL,
  totalObligation,
  utxosDepositedL,
  utxosGovStateL,
 )
import Cardano.Ledger.Slot (epochInfoFirst)
import Cardano.Ledger.Val (Val (..))
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
