{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}

module Test.Shelley.Spec.Ledger.Examples.MirTransfer
  ( testMIRTransfer,
  )
where

import Cardano.Ledger.Coin (Coin (..), DeltaCoin (..))
import Cardano.Ledger.Keys
  ( KeyRole (..),
    hashKey,
  )
import Cardano.Ledger.Shelley (ShelleyEra)
import Cardano.Ledger.Slot (SlotNo (..))
import Control.State.Transition.Extended hiding (Assertion)
import Control.State.Transition.Trace (checkTrace, (.-), (.->))
import Data.Default.Class (def)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Shelley.Spec.Ledger.API
  ( AccountState (..),
    Credential (..),
    DCert (..),
    DELEG,
    DState (..),
    DelegEnv (..),
    InstantaneousRewards (..),
    MIRCert (..),
    MIRPot (..),
    MIRTarget (..),
    Ptr (..),
  )
import Shelley.Spec.Ledger.PParams (PParams' (..), ProtVer (..), emptyPParams)
import Shelley.Spec.Ledger.STS.Deleg (DelegPredicateFailure (..))
import Test.Shelley.Spec.Ledger.ConcreteCryptoTypes (C_Crypto)
import Test.Shelley.Spec.Ledger.Utils (RawSeed (..), applySTSTest, mkKeyPair, runShelleyBase)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (Assertion, testCase, (@?=))

type ShelleyTest = ShelleyEra C_Crypto

ignoreAllButIRWD ::
  Either [PredicateFailure (DELEG ShelleyTest)] (DState C_Crypto) ->
  Either [PredicateFailure (DELEG ShelleyTest)] (InstantaneousRewards C_Crypto)
ignoreAllButIRWD = fmap _irwd

env :: ProtVer -> AccountState -> DelegEnv ShelleyTest
env pv acnt =
  DelegEnv
    { slotNo = SlotNo 50,
      ptr_ = Ptr (SlotNo 50) 0 0,
      acnt_ = acnt,
      ppDE = emptyPParams {_protocolVersion = pv}
    }

shelleyPV :: ProtVer
shelleyPV = ProtVer 2 0

alonzoPV :: ProtVer
alonzoPV = ProtVer 5 0

testMirTransfer ::
  ProtVer ->
  MIRPot ->
  MIRTarget C_Crypto ->
  InstantaneousRewards C_Crypto ->
  AccountState ->
  Either [PredicateFailure (DELEG ShelleyTest)] (InstantaneousRewards C_Crypto) ->
  Assertion
testMirTransfer pv pot target ir acnt (Right expected) = do
  checkTrace @(DELEG ShelleyTest) runShelleyBase (env pv acnt) $
    (pure (def {_irwd = ir})) .- (DCertMir (MIRCert pot target)) .-> (def {_irwd = expected})
testMirTransfer pv pot target ir acnt predicateFailure@(Left _) = do
  let st =
        runShelleyBase $
          applySTSTest @(DELEG ShelleyTest)
            (TRC (env pv acnt, def {_irwd = ir}, DCertMir (MIRCert pot target)))
  (ignoreAllButIRWD st) @?= predicateFailure

alice :: Credential 'Staking C_Crypto
alice = (KeyHashObj . hashKey . snd) $ mkKeyPair (RawSeed 0 0 0 0 1)

aliceOnlyReward :: Integer -> Map (Credential 'Staking C_Crypto) Coin
aliceOnlyReward c = Map.fromList [(alice, Coin c)]

aliceOnlyDelta :: Integer -> Map (Credential 'Staking C_Crypto) DeltaCoin
aliceOnlyDelta c = Map.fromList [(alice, DeltaCoin c)]

bob :: Credential 'Staking C_Crypto
bob = (KeyHashObj . hashKey . snd) $ mkKeyPair (RawSeed 0 0 0 0 2)

bobOnlyReward :: Integer -> Map (Credential 'Staking C_Crypto) Coin
bobOnlyReward c = Map.fromList [(bob, Coin c)]

bobOnlyDelta :: Integer -> Map (Credential 'Staking C_Crypto) DeltaCoin
bobOnlyDelta c = Map.fromList [(bob, DeltaCoin c)]

testMIRTransfer :: TestTree
testMIRTransfer =
  testGroup
    "MIR cert transfers"
    [ testGroup
        "MIR cert embargos"
        [ testCase "embargo reserves to treasury transfer" $
            testMirTransfer
              shelleyPV
              ReservesMIR
              (SendToOppositePotMIR $ Coin 1)
              (InstantaneousRewards mempty mempty mempty mempty)
              (AccountState {_reserves = Coin 1, _treasury = Coin 0})
              (Left [MIRTransferNotCurrentlyAllowed]),
          testCase "embargo treasury to reserves transfer" $
            testMirTransfer
              shelleyPV
              TreasuryMIR
              (SendToOppositePotMIR $ Coin 1)
              (InstantaneousRewards mempty mempty mempty mempty)
              (AccountState {_reserves = Coin 0, _treasury = Coin 1})
              (Left [MIRTransferNotCurrentlyAllowed]),
          testCase "embargo decrements from reserves" $
            testMirTransfer
              shelleyPV
              ReservesMIR
              (StakeAddressesMIR $ aliceOnlyDelta (-1))
              (InstantaneousRewards (aliceOnlyReward 1) mempty mempty mempty)
              (AccountState {_reserves = Coin 1, _treasury = Coin 0})
              (Left [MIRNegativesNotCurrentlyAllowed]),
          testCase "embargo decrements from treasury" $
            testMirTransfer
              shelleyPV
              TreasuryMIR
              (StakeAddressesMIR $ aliceOnlyDelta (-1))
              (InstantaneousRewards mempty (aliceOnlyReward 1) mempty mempty)
              (AccountState {_reserves = Coin 0, _treasury = Coin 1})
              (Left [MIRNegativesNotCurrentlyAllowed])
        ],
      testGroup
        "MIR cert alonzo"
        [ testCase "increment reserves too much" $
            testMirTransfer
              alonzoPV
              ReservesMIR
              (StakeAddressesMIR $ aliceOnlyDelta 1)
              (InstantaneousRewards (aliceOnlyReward 1) mempty mempty mempty)
              (AccountState {_reserves = Coin 1, _treasury = Coin 0})
              (Left [InsufficientForInstantaneousRewardsDELEG ReservesMIR (Coin 2) (Coin 1)]),
          testCase "increment treasury too much" $
            testMirTransfer
              alonzoPV
              TreasuryMIR
              (StakeAddressesMIR $ aliceOnlyDelta 1)
              (InstantaneousRewards mempty (aliceOnlyReward 1) mempty mempty)
              (AccountState {_reserves = Coin 0, _treasury = Coin 1})
              (Left [InsufficientForInstantaneousRewardsDELEG TreasuryMIR (Coin 2) (Coin 1)]),
          testCase "increment reserves too much with delta" $
            testMirTransfer
              alonzoPV
              ReservesMIR
              (StakeAddressesMIR $ aliceOnlyDelta 1)
              (InstantaneousRewards (aliceOnlyReward 1) mempty (DeltaCoin (-1)) (DeltaCoin 1))
              (AccountState {_reserves = Coin 2, _treasury = Coin 0})
              (Left [InsufficientForInstantaneousRewardsDELEG ReservesMIR (Coin 2) (Coin 1)]),
          testCase "increment treasury too much with delta" $
            testMirTransfer
              alonzoPV
              TreasuryMIR
              (StakeAddressesMIR $ aliceOnlyDelta 1)
              (InstantaneousRewards mempty (aliceOnlyReward 1) (DeltaCoin 1) (DeltaCoin (-1)))
              (AccountState {_reserves = Coin 0, _treasury = Coin 2})
              (Left [InsufficientForInstantaneousRewardsDELEG TreasuryMIR (Coin 2) (Coin 1)]),
          testCase "negative balance in reserves mapping" $
            testMirTransfer
              alonzoPV
              ReservesMIR
              (StakeAddressesMIR $ aliceOnlyDelta (-1))
              (InstantaneousRewards mempty mempty mempty mempty)
              (AccountState {_reserves = Coin 1, _treasury = Coin 0})
              (Left [MIRProducesNegativeUpdate]),
          testCase "negative balance in treasury mapping" $
            testMirTransfer
              alonzoPV
              TreasuryMIR
              (StakeAddressesMIR $ aliceOnlyDelta (-1))
              (InstantaneousRewards mempty mempty mempty mempty)
              (AccountState {_reserves = Coin 0, _treasury = Coin 1})
              (Left [MIRProducesNegativeUpdate]),
          testCase "transfer reserves to treasury" $
            testMirTransfer
              alonzoPV
              ReservesMIR
              (SendToOppositePotMIR (Coin 1))
              (InstantaneousRewards mempty mempty mempty mempty)
              (AccountState {_reserves = Coin 1, _treasury = Coin 0})
              (Right (InstantaneousRewards mempty mempty (DeltaCoin (-1)) (DeltaCoin 1))),
          testCase "transfer treasury to reserves" $
            testMirTransfer
              alonzoPV
              TreasuryMIR
              (SendToOppositePotMIR (Coin 1))
              (InstantaneousRewards mempty mempty mempty mempty)
              (AccountState {_reserves = Coin 0, _treasury = Coin 1})
              (Right (InstantaneousRewards mempty mempty (DeltaCoin 1) (DeltaCoin (-1)))),
          testCase "insufficient transfer reserves to treasury" $
            testMirTransfer
              alonzoPV
              ReservesMIR
              (SendToOppositePotMIR (Coin 1))
              (InstantaneousRewards (aliceOnlyReward 1) mempty (DeltaCoin (-1)) (DeltaCoin 1))
              (AccountState {_reserves = Coin 2, _treasury = Coin 0})
              (Left [InsufficientForTransferDELEG ReservesMIR (Coin 1) (Coin 0)]),
          testCase "insufficient transfer treasury to reserves" $
            testMirTransfer
              alonzoPV
              TreasuryMIR
              (SendToOppositePotMIR (Coin 1))
              (InstantaneousRewards mempty (aliceOnlyReward 1) (DeltaCoin 1) (DeltaCoin (-1)))
              (AccountState {_reserves = Coin 0, _treasury = Coin 2})
              (Left [InsufficientForTransferDELEG TreasuryMIR (Coin 1) (Coin 0)]),
          testCase "increment reserves mapping" $
            testMirTransfer
              alonzoPV
              ReservesMIR
              (StakeAddressesMIR $ (aliceOnlyDelta 1 `Map.union` bobOnlyDelta 1))
              (InstantaneousRewards (aliceOnlyReward 1) mempty mempty mempty)
              (AccountState {_reserves = Coin 3, _treasury = Coin 0})
              ( Right
                  ( InstantaneousRewards
                      (aliceOnlyReward 2 `Map.union` bobOnlyReward 1)
                      mempty
                      mempty
                      mempty
                  )
              ),
          testCase "increment treasury mapping" $
            testMirTransfer
              alonzoPV
              TreasuryMIR
              (StakeAddressesMIR $ (aliceOnlyDelta 1 `Map.union` bobOnlyDelta 1))
              (InstantaneousRewards mempty (aliceOnlyReward 1) mempty mempty)
              (AccountState {_reserves = Coin 0, _treasury = Coin 3})
              ( Right
                  ( InstantaneousRewards
                      mempty
                      (aliceOnlyReward 2 `Map.union` bobOnlyReward 1)
                      mempty
                      mempty
                  )
              )
        ]
    ]
