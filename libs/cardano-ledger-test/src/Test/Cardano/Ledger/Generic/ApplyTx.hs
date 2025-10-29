{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE UndecidableSuperClasses #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Test.Cardano.Ledger.Generic.ApplyTx where

import Cardano.Ledger.Address (RewardAccount (..))
import Cardano.Ledger.Alonzo.Plutus.Context (EraPlutusTxInfo)
import Cardano.Ledger.Alonzo.Scripts (AlonzoPlutusPurpose (..), ExUnits (ExUnits))
import Cardano.Ledger.Alonzo.TxWits (TxDats (..))
import Cardano.Ledger.BaseTypes (ProtVer (..), StrictMaybe (..), TxIx, natVersion)
import Cardano.Ledger.Coin (Coin (..), addDeltaCoin, compactCoinOrError)
import Cardano.Ledger.Compactible (fromCompact)
import Cardano.Ledger.Conway.Core
import Cardano.Ledger.Conway.Scripts (ConwayPlutusPurpose (..))
import Cardano.Ledger.Credential (Credential)
import Cardano.Ledger.Plutus.Data (Data (..), hashData)
import Cardano.Ledger.Plutus.Language (Language (..))
import Cardano.Ledger.Shelley.Rewards (aggregateRewards)
import Cardano.Ledger.State
import Cardano.Ledger.TxIn (TxId (..), TxIn (..))
import Cardano.Ledger.Val (Val ((<+>), (<->)), inject)
import Cardano.Slotting.Slot (EpochNo (..))
import Data.Foldable (Foldable (..), fold, toList)
import Data.Map (Map)
import qualified Data.Map.Strict as Map
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Word (Word32)
import Lens.Micro
import qualified PlutusLedgerApi.V1 as PV1
import Test.Cardano.Ledger.Alonzo.Scripts (alwaysFails)
import Test.Cardano.Ledger.Common
import Test.Cardano.Ledger.Core.KeyPair (mkWitnessVKey)
import Test.Cardano.Ledger.Examples.STSTestUtils (
  EraModel (..),
  mkGenesisTxIn,
  mkTxDats,
  someAddr,
  someKeys,
 )
import Test.Cardano.Ledger.Generic.Functions (
  createRUpdNonPulsing',
  txInBalance,
 )
import Test.Cardano.Ledger.Generic.GenState (PlutusPurposeTag (..))
import Test.Cardano.Ledger.Generic.ModelState (
  Model,
  ModelNewEpochState (..),
 )
import Test.Cardano.Ledger.Generic.Proof hiding (lift)
import Test.Cardano.Ledger.Plutus (zeroTestingCostModels)
import Test.Cardano.Ledger.Shelley.Rewards (RewardUpdateOld (deltaFOld), rsOld)

applyTxSimple :: forall era. EraModel era => Int -> Model era -> Tx TopTx era -> Model era
applyTxSimple count model tx = applyTxBody count model $ tx ^. bodyTxL

applyTxFail ::
  (Reflect era, AlonzoEraTxBody era, EraModel era) =>
  Int -> TxIx -> Model era -> Tx TopTx era -> Model era
applyTxFail count nextTxIx model tx = updateInfo info model
  where
    info = collInfo count nextTxIx model emptyCollInfo $ tx ^. bodyTxL

collInfo ::
  (HasCallStack, AlonzoEraTxBody era, EraModel era) =>
  Int ->
  TxIx ->
  Model era ->
  CollInfo era ->
  TxBody TopTx era ->
  CollInfo era
collInfo count firstTxIx model info txbody =
  afterColReturn
    { ciDelset = inputs
    , ciBal = txInBalance inputs $ mUTxO model
    }
  where
    inputs = txbody ^. collateralInputsTxBodyL
    afterColReturn =
      case txbody ^. collateralReturnTxBodyT of
        SNothing -> info
        SJust txOut ->
          case Map.lookup count (mIndex model) of
            Nothing -> error ("Output not found phase2: " ++ show (count, mIndex model))
            Just (TxId hash) ->
              info
                { ciRet = txOut ^. coinTxOutL
                , ciAddmap = newstuff
                }
              where
                newstuff = additions hash firstTxIx [txOut]

-- ========================================================================

defaultPPs :: AlonzoEraPParams era => PParams era -> PParams era
defaultPPs pp =
  pp
    & ppCostModelsL .~ zeroTestingCostModels [PlutusV1]
    & ppMaxValSizeL .~ 1000000000
    & ppMaxTxExUnitsL .~ ExUnits 1000000 1000000
    & ppMaxBlockExUnitsL .~ ExUnits 1000000 1000000
    & ppProtocolVersionL .~ ProtVer (natVersion @5) 0
    & ppKeyDepositL .~ Coin 2
    & ppPoolDepositL .~ Coin 5
    & ppCollateralPercentageL .~ 100

pparams :: AlonzoEraPParams era => PParams era
pparams = defaultPPs emptyPParams

epochBoundary ::
  forall era. (EraPParams era, EraStake era) => EpochNo -> EpochNo -> Model era -> Model era
epochBoundary transactionEpoch modelEpoch model =
  if transactionEpoch > modelEpoch
    then
      applyRUpd ru $
        model
          { mEL = transactionEpoch
          }
    else model
  where
    ru = createRUpdNonPulsing' @era model

applyTxBody :: EraModel era => Int -> Model era -> TxBody TopTx era -> Model era
applyTxBody count model txbody =
  Map.foldlWithKey' applyWithdrawals (foldl' applyCert model' $ txbody ^. certsTxBodyL)
    . unWithdrawals
    $ txbody ^. withdrawalsTxBodyL
  where
    mUTxOInputs = Map.withoutKeys (mUTxO model) $ txbody ^. inputsTxBodyL
    mUTxOOutputs = case Map.lookup count (mIndex model) of
      Nothing -> error ("Output not found phase1: " ++ show (mIndex model))
      Just (TxId hash) -> Map.union newstuff mUTxOInputs
        where
          newstuff = additions hash minBound . toList $ txbody ^. outputsTxBodyL
    model' =
      model
        { mUTxO = mUTxOOutputs
        , mFees = mFees model <+> (txbody ^. feeTxBodyL)
        }

applyWithdrawals :: EraAccounts era => Model era -> RewardAccount -> Coin -> Model era
applyWithdrawals model (RewardAccount _network cred) coin =
  model
    { mAccounts =
        adjustAccountState
          (balanceAccountStateL %~ (\balance -> compactCoinOrError (fromCompact balance <-> coin)))
          cred
          (mAccounts model)
    }

-- =========================================================
-- What to do if the second phase does not validatate.
-- Process and use Collateral to pay fees

data CollInfo era = CollInfo
  { ciBal :: Coin -- Balance of all the collateral inputs
  , ciRet :: Coin -- Coin amount of the collateral return output
  , ciDelset :: Set TxIn -- The set of inputs to delete from the UTxO
  , ciAddmap :: Map TxIn (TxOut era) -- Things to Add to the UTxO
  }

emptyCollInfo :: CollInfo era
emptyCollInfo = CollInfo (Coin 0) (Coin 0) Set.empty Map.empty

updateInfo :: CollInfo era -> Model era -> Model era
updateInfo info m =
  m
    { mUTxO = Map.union (ciAddmap info) (Map.withoutKeys (mUTxO m) (ciDelset info))
    , mFees = mFees m <+> ciBal info <-> ciRet info
    }

-- =======================================

additions ::
  SafeHash EraIndependentTxBody ->
  TxIx ->
  [TxOut era] ->
  Map TxIn (TxOut era)
additions bodyhash firstTxIx outputs =
  Map.fromList
    [ (TxIn (TxId bodyhash) idx, out)
    | (out, idx) <- zip outputs [firstTxIx ..]
    ]

filterRewards ::
  EraPParams era =>
  PParams era ->
  Map (Credential 'Staking) (Set Reward) ->
  ( Map (Credential 'Staking) (Set Reward)
  , Map (Credential 'Staking) (Set Reward)
  )
filterRewards pp rewards =
  if pvMajor (pp ^. ppProtocolVersionL) > natVersion @2
    then (rewards, Map.empty)
    else
      let mp = Map.map Set.deleteFindMin rewards
       in (Map.map (Set.singleton . fst) mp, Map.filter (not . Set.null) $ Map.map snd mp)

filterAllRewards ::
  (EraPParams era, EraAccounts era) =>
  Map (Credential 'Staking) (Set Reward) ->
  Model era ->
  ( Map (Credential 'Staking) (Set Reward)
  , Map (Credential 'Staking) (Set Reward)
  , Set (Credential 'Staking)
  , Coin
  )
filterAllRewards rs' m =
  (registered, eraIgnored, unregistered, totalUnregistered)
  where
    pp = mPParams m
    (regRU, unregRU) =
      Map.partitionWithKey
        (\cred _ -> isAccountRegistered cred (mAccounts m))
        rs'
    totalUnregistered = fold $ aggregateRewards (pp ^. ppProtocolVersionL) unregRU
    unregistered = Map.keysSet unregRU

    (registered, eraIgnored) = filterRewards pp regRU

applyRUpd ::
  forall era.
  EraAccounts era =>
  RewardUpdateOld ->
  Model era ->
  Model era
applyRUpd ru m =
  m
    { mFees = mFees m `addDeltaCoin` deltaFOld ru
    , mAccounts = addToBalanceAccounts (Map.map compactCoinOrError $ rsOld ru) (mAccounts m)
    }

notValidatingTx ::
  forall era.
  ( AlonzoEraTxWits era
  , EraPlutusTxInfo PlutusV1 era
  , AlonzoEraTxBody era
  , EraModel era
  ) =>
  Tx TopTx era
notValidatingTx =
  let s = alwaysFails @PlutusV1 1
      dat = Data (PV1.I 0)
   in mkBasicTx notValidatingBody
        & witsTxL . addrTxWitsL .~ [mkWitnessVKey (hashAnnotated notValidatingBody) someKeys]
        & witsTxL . scriptTxWitsL .~ [(hashScript s, s)]
        & witsTxL . datsTxWitsL .~ TxDats [(hashData dat, dat)]
        & witsTxL . rdmrsTxWitsL .~ redeemers
  where
    notValidatingBody =
      mkBasicTxBody
        & inputsTxBodyL .~ [mkGenesisTxIn 2]
        & collateralInputsTxBodyL .~ [mkGenesisTxIn 12]
        & outputsTxBodyL .~ [mkBasicTxOut someAddr (inject $ Coin 2995)]
        & feeTxBodyL .~ Coin 5
        & scriptIntegrityHashTxBodyL
          .~ newScriptIntegrityHash pparams [PlutusV1] redeemers (mkTxDats (Data (PV1.I 0)))
    redeemers = mkRedeemersFromTags [((Spending, 0), (Data (PV1.I 1), ExUnits 5000 5000))]

mkAlonzoPlutusPurposePointer ::
  forall era.
  Era era =>
  PlutusPurposeTag ->
  Word32 ->
  AlonzoPlutusPurpose AsIx era
mkAlonzoPlutusPurposePointer tag i =
  case tag of
    Spending -> AlonzoSpending (AsIx i)
    Minting -> AlonzoMinting (AsIx i)
    Certifying -> AlonzoCertifying (AsIx i)
    Rewarding -> AlonzoRewarding (AsIx i)
    _ -> error $ "Unsupported tag: " ++ show tag ++ " in era " ++ eraName @era

mkConwayPlutusPurposePointer :: PlutusPurposeTag -> Word32 -> ConwayPlutusPurpose AsIx era
mkConwayPlutusPurposePointer tag i =
  case tag of
    Spending -> ConwaySpending (AsIx i)
    Minting -> ConwayMinting (AsIx i)
    Certifying -> ConwayCertifying (AsIx i)
    Rewarding -> ConwayRewarding (AsIx i)
    Voting -> ConwayVoting (AsIx i)
    Proposing -> ConwayProposing (AsIx i)
