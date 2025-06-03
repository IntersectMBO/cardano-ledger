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

module Test.Cardano.Ledger.Generic.ApplyTx where

import Cardano.Ledger.Address (RewardAccount (..))
import Cardano.Ledger.Alonzo.Scripts (ExUnits (ExUnits))
import Cardano.Ledger.BaseTypes (ProtVer (..), TxIx, mkTxIxPartial, natVersion)
import Cardano.Ledger.Coin (Coin (..), addDeltaCoin)
import Cardano.Ledger.Conway.Core (
  AlonzoEraTxWits (..),
  ppCollateralPercentageL,
  ppCostModelsL,
  ppMaxBlockExUnitsL,
  ppMaxTxExUnitsL,
  ppMaxValSizeL,
 )
import Cardano.Ledger.Core
import Cardano.Ledger.Credential (Credential)
import Cardano.Ledger.Plutus.Data (Data (..))
import Cardano.Ledger.Plutus.Language (Language (PlutusV1))
import Cardano.Ledger.PoolParams (PoolParams (..))
import Cardano.Ledger.Shelley.Rewards (aggregateRewards)
import Cardano.Ledger.Shelley.TxCert (ShelleyDelegCert (..), ShelleyTxCert (..))
import Cardano.Ledger.TxIn (TxId (..), TxIn (..))
import Cardano.Ledger.Val (Val ((<+>), (<->)), inject)
import Cardano.Slotting.Slot (EpochNo (..), SlotNo (..))
import Control.Iterate.Exp (dom, (∈))
import Control.Iterate.SetAlgebra (eval)
import Data.Foldable (fold, toList)
import qualified Data.List as List
import Data.Map (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe.Strict (StrictMaybe (..))
import Data.Set (Set)
import qualified Data.Set as Set
import Lens.Micro
import qualified PlutusLedgerApi.V1 as PV1
import Test.Cardano.Ledger.Common
import Test.Cardano.Ledger.Conway.Era ()
import Test.Cardano.Ledger.Core.KeyPair (mkWitnessVKey)
import Test.Cardano.Ledger.Examples.STSTestUtils (
  mkGenesisTxIn,
  mkTxDats,
  someAddr,
  someKeys,
 )
import Test.Cardano.Ledger.Generic.Functions (
  createRUpdNonPulsing',
  getBody,
  txInBalance,
 )
import Test.Cardano.Ledger.Generic.GenState (PlutusPurposeTag (..), mkRedeemersFromTags)
import Test.Cardano.Ledger.Generic.ModelState (
  Model,
  ModelNewEpochState (..),
 )
import Test.Cardano.Ledger.Generic.Proof hiding (lift)
import Test.Cardano.Ledger.Generic.Updaters (
  newScriptIntegrityHash,
 )
import Test.Cardano.Ledger.Plutus (zeroTestingCostModels)
import Test.Cardano.Ledger.Shelley.Rewards (RewardUpdateOld (deltaFOld), rsOld)
import Test.Cardano.Ledger.Shelley.Utils (epochFromSlotNo)

-- ========================================================================

defaultPPs :: Era era => PParams era -> PParams era
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

pparams :: EraPParams era => PParams era
pparams = defaultPPs emptyPParams

applyTx :: forall era. Reflect era => Int -> SlotNo -> Model era -> Tx era -> Model era
applyTx count slot model tx = case hasValid fields of
  Nothing -> List.foldl' (applyTxSimple proof count) epochAccurateModel fields
  Just True -> List.foldl' (applyTxSimple proof count) epochAccurateModel fields
  Just False -> List.foldl' (applyTxFail proof count nextTxIx) epochAccurateModel fields
  where
    proof = reify @era
    transactionEpoch = epochFromSlotNo slot
    modelEpoch = mEL model
    epochAccurateModel = epochBoundary proof transactionEpoch modelEpoch model
    txbody = getBody proof tx
    outputs = txbody ^. outputsTxBodyL
    nextTxIx = mkTxIxPartial (fromIntegral (length outputs)) -- When IsValid is false, ColRet will get this TxIx

epochBoundary :: forall era. Proof era -> EpochNo -> EpochNo -> Model era -> Model era
epochBoundary proof transactionEpoch modelEpoch model =
  if transactionEpoch > modelEpoch
    then
      applyRUpd ru $
        model
          { mEL = transactionEpoch
          }
    else model
  where
    ru = createRUpdNonPulsing' proof model

applyTxBody :: Reflect era => Proof era -> Int -> Model era -> TxBody era -> Model era
applyTxBody proof count model tx = List.foldl' (applyField proof count) model (abstractTxBody proof tx)

applyWithdrawals :: Proof era -> Model era -> RewardAccount -> Coin -> Model era
applyWithdrawals _proof model (RewardAccount _network cred) coin =
  model {mRewards = Map.adjust (<-> coin) cred (mRewards model)}

applyCert :: forall era. Reflect era => Model era -> TxCert era -> Model era
applyCert = case reify @era of
  Shelley -> applyShelleyCert
  Mary -> applyShelleyCert
  Allegra -> applyShelleyCert
  Alonzo -> applyShelleyCert
  Babbage -> applyShelleyCert
  Conway -> error "applyCert, not yet in Conway"

applyShelleyCert :: forall era. EraPParams era => Model era -> ShelleyTxCert era -> Model era
applyShelleyCert model dcert = case dcert of
  ShelleyTxCertDelegCert (ShelleyRegCert x) ->
    model
      { mRewards = Map.insert x (Coin 0) (mRewards model)
      , mKeyDeposits = Map.insert x (pp ^. ppKeyDepositL) (mKeyDeposits model)
      , mDeposited = mDeposited model <+> pp ^. ppKeyDepositL
      }
    where
      pp = mPParams model
  ShelleyTxCertDelegCert (ShelleyUnRegCert x) -> case Map.lookup x (mRewards model) of
    Nothing -> error ("DeRegKey not in rewards: " <> show (toExpr x))
    Just (Coin 0) ->
      model
        { mRewards = Map.delete x (mRewards model)
        , mKeyDeposits = Map.delete x (mKeyDeposits model)
        , mDeposited = mDeposited model <-> keyDeposit
        }
      where
        keyDeposit = Map.findWithDefault mempty x (mKeyDeposits model)
    Just (Coin _n) -> error "DeRegKey with non-zero balance"
  ShelleyTxCertDelegCert (ShelleyDelegCert cred hash) ->
    model {mDelegations = Map.insert cred hash (mDelegations model)}
  ShelleyTxCertPool (RegPool poolparams) ->
    model
      { mPoolParams = Map.insert hk poolparams (mPoolParams model)
      , mDeposited =
          if Map.member hk (mPoolDeposits model)
            then mDeposited model
            else mDeposited model <+> pp ^. ppPoolDepositL
      , mPoolDeposits -- Only add if it isn't already there
        =
          if Map.member hk (mPoolDeposits model)
            then mPoolDeposits model
            else Map.insert hk (pp ^. ppPoolDepositL) (mPoolDeposits model)
      }
    where
      hk = ppId poolparams
      pp = mPParams model
  ShelleyTxCertPool (RetirePool keyhash epoch) ->
    model
      { mRetiring = Map.insert keyhash epoch (mRetiring model)
      , mDeposited = mDeposited model <-> pp ^. ppPoolDepositL
      }
    where
      pp = mPParams model
  ShelleyTxCertGenesisDeleg _ -> model
  ShelleyTxCertMir _ -> model

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
  EraPParams era =>
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
        (\k _ -> eval (k ∈ dom (mRewards m)))
        rs'
    totalUnregistered = fold $ aggregateRewards (pp ^. ppProtocolVersionL) unregRU
    unregistered = Map.keysSet unregRU

    (registered, eraIgnored) = filterRewards pp regRU

applyRUpd ::
  forall era.
  RewardUpdateOld ->
  Model era ->
  Model era
applyRUpd ru m =
  m
    { mFees = mFees m `addDeltaCoin` deltaFOld ru
    , mRewards = Map.unionWith (<>) (mRewards m) (rsOld ru)
    }

notValidatingTx ::
  ( EraTx era
  , AlonzoEraTxWits era
  ) =>
  Proof era ->
  Tx era
notValidatingTx pf =
  let s = never 0 pf
   in mkBasicTx notValidatingBody
        & witsTxL . addrTxWitsL .~ [mkWitnessVKey (hashAnnotated notValidatingBody) (someKeys pf)]
        & witsTxL . scriptTxWitsL .~ [(hashScript s, s)]
        & witsTxL . datsTxWitsL .~ [Data (PV1.I 0)]
        & witsTxL . rdmrsTxWitsL .~ redeemers
  where
    notValidatingBody =
      newTxBody
        pf
        [ Inputs' [mkGenesisTxIn 2]
        , Collateral' [mkGenesisTxIn 12]
        , Outputs' [newTxOut pf [Address (someAddr pf), Amount (inject $ Coin 2995)]]
        , Txfee (Coin 5)
        , WppHash (newScriptIntegrityHash pf (pparams pf) [PlutusV1] redeemers (mkTxDats (Data (PV1.I 0))))
        ]
    redeemers =
      mkRedeemersFromTags pf [((Spending, 0), (Data (PV1.I 1), ExUnits 5000 5000))]
