{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Test.Cardano.Ledger.Generic.Instances () where

import Cardano.Ledger.Allegra (AllegraEra)
import Cardano.Ledger.Alonzo (AlonzoEra)
import Cardano.Ledger.Alonzo.Core (
  AllegraEraTxBody (..),
  AlonzoEraPParams,
  AlonzoEraTx (..),
  AlonzoEraTxWits (..),
  EraPParams,
  EraScript (..),
  EraTxBody (..),
  EraTxWits (..),
  PParams,
  PoolCert (..),
  ValidityInterval (..),
  ppCollateralPercentageL,
  ppCostModelsL,
  ppKeyDepositL,
  ppMaxBlockExUnitsL,
  ppMaxTxExUnitsL,
  ppMaxValSizeL,
  ppPoolDepositL,
 )
import Cardano.Ledger.Alonzo.Tx (IsValid (..))
import Cardano.Ledger.Babbage (BabbageEra)
import Cardano.Ledger.BaseTypes (SlotNo (..), StrictMaybe (..), mkTxIxPartial)
import Cardano.Ledger.Coin (Coin (..))
import Cardano.Ledger.Conway (ConwayEra)
import Cardano.Ledger.Conway.Core (
  AlonzoEraTxBody (..),
  BabbageEraTxBody (..),
  EraTx (..),
  ScriptIntegrityHash,
  ShelleyEraTxBody (..),
  ppMaxCollateralInputsL,
 )
import Cardano.Ledger.Mary (MaryEra)
import Cardano.Ledger.Plutus (ExUnits (..), Language (..))
import Cardano.Ledger.PoolParams (PoolParams (..))
import Cardano.Ledger.Shelley (ShelleyEra)
import Cardano.Ledger.Shelley.Scripts (pattern RequireAllOf, pattern RequireAnyOf)
import Cardano.Ledger.Shelley.TxCert (ShelleyDelegCert (..), ShelleyTxCert (..))
import Cardano.Ledger.Val (Val (..))
import Control.Monad.RWS.Strict (gets)
import Control.Monad.Trans (MonadTrans (..))
import qualified Data.Foldable as F
import qualified Data.Map.Strict as Map
import Data.Ratio ((%))
import Data.TreeDiff (ToExpr (..))
import Lens.Micro ((.~), (^.))
import qualified Lens.Micro as L
import Test.Cardano.Ledger.Alonzo.Scripts (alwaysFails, alwaysSucceeds)
import Test.Cardano.Ledger.Alonzo.Tools (EraModel (..))
import Test.Cardano.Ledger.Generic.ApplyTx (applyTxBody, applyTxFail, applyTxSimple, epochBoundary)
import Test.Cardano.Ledger.Generic.GenState (GenEnv (..), GenRS, GenState (..))
import Test.Cardano.Ledger.Generic.ModelState (Model, ModelNewEpochState (..))
import Test.Cardano.Ledger.Generic.Proof (Reflect)
import Test.Cardano.Ledger.Generic.TxGen (
  EraGenericGen (..),
  alonzoMkRedeemers,
  alonzoMkRedeemersFromTags,
  alonzoNewScriptIntegrityHash,
  mkAlonzoPlutusPurposePointer,
  mkConwayPlutusPurposePointer,
 )
import Test.Cardano.Ledger.Shelley.Generator.Core (genNatural)
import Test.Cardano.Ledger.Shelley.Utils (epochFromSlotNo)

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

timeToLive :: ValidityInterval -> SlotNo
timeToLive (ValidityInterval _ (SJust n)) = n
timeToLive (ValidityInterval _ SNothing) = SlotNo maxBound

shelleySetValidity :: ValidityInterval -> TxBody ShelleyEra -> TxBody ShelleyEra
shelleySetValidity vi = ttlTxBodyL .~ timeToLive vi

alonzoSetValidity :: AllegraEraTxBody era => ValidityInterval -> TxBody era -> TxBody era
alonzoSetValidity vi = vldtTxBodyL .~ vi

instance EraModel ShelleyEra where
  applyTx = shelleyApplyTx
  applyCert = applyShelleyCert
  always _ = fromNativeScript $ RequireAllOf []
  never _ = fromNativeScript $ RequireAnyOf []

instance EraModel AllegraEra where
  applyTx = shelleyApplyTx
  applyCert = applyShelleyCert
  always _ = fromNativeScript $ RequireAllOf []
  never _ = fromNativeScript $ RequireAnyOf []

instance EraModel MaryEra where
  applyTx = shelleyApplyTx
  applyCert = applyShelleyCert
  always _ = fromNativeScript $ RequireAllOf []
  never _ = fromNativeScript $ RequireAnyOf []

instance EraModel AlonzoEra where
  applyTx = shelleyApplyTx
  applyCert = applyShelleyCert
  mkRedeemersFromTags = alonzoMkRedeemersFromTags
  mkRedeemers = alonzoMkRedeemers
  newScriptIntegrityHash = alonzoNewScriptIntegrityHash
  mkPlutusPurposePointer = mkAlonzoPlutusPurposePointer
  always = alwaysSucceeds @PlutusV1
  never = alwaysFails @PlutusV1

instance EraModel BabbageEra where
  applyTx = babbageApplyTx
  applyCert = applyShelleyCert
  mkRedeemersFromTags = alonzoMkRedeemersFromTags
  mkRedeemers = alonzoMkRedeemers
  newScriptIntegrityHash = alonzoNewScriptIntegrityHash
  mkPlutusPurposePointer = mkAlonzoPlutusPurposePointer
  always = alwaysSucceeds @PlutusV2
  never = alwaysFails @PlutusV2

instance EraModel ConwayEra where
  applyTx = babbageApplyTx
  applyCert = error "Not yet implemented"
  mkRedeemersFromTags = alonzoMkRedeemersFromTags
  mkRedeemers = alonzoMkRedeemers
  newScriptIntegrityHash = alonzoNewScriptIntegrityHash
  mkPlutusPurposePointer = mkConwayPlutusPurposePointer
  always = alwaysSucceeds @PlutusV2
  never = alwaysFails @PlutusV2

instance EraGenericGen ShelleyEra where
  setValidity = shelleySetValidity

instance EraGenericGen MaryEra

instance EraGenericGen AllegraEra

alonzoMkScriptIntegrityHash ::
  ( EraModel era
  , AlonzoEraTxWits era
  ) =>
  PParams era -> [Language] -> TxWits era -> StrictMaybe ScriptIntegrityHash
alonzoMkScriptIntegrityHash pp langs wits =
  newScriptIntegrityHash pp langs (wits ^. rdmrsTxWitsL) (wits ^. datsTxWitsL)

-- | Generate a list of specified length with randomish `ExUnit`s where the sum
--   of all values produced will not exceed the maxTxExUnits.
alonzoGenExUnits :: AlonzoEraPParams era => Int -> GenRS era [ExUnits]
alonzoGenExUnits n = do
  GenEnv {gePParams} <- gets gsGenEnv
  let ExUnits maxMemUnits maxStepUnits = gePParams ^. ppMaxTxExUnitsL
  memUnits <- lift $ genSequenceSum maxMemUnits
  stepUnits <- lift $ genSequenceSum maxStepUnits
  pure $ zipWith ExUnits memUnits stepUnits
  where
    un = fromIntegral n
    genUpTo maxVal (!totalLeft, !acc) _
      | totalLeft == 0 = pure (0, 0 : acc)
      | otherwise = do
          x <- min totalLeft . round . (% un) <$> genNatural 0 maxVal
          pure (totalLeft - x, x : acc)
    genSequenceSum maxVal
      | maxVal == 0 = pure $ replicate n 0
      | otherwise = snd <$> F.foldlM (genUpTo maxVal) (maxVal, []) ([1 .. n] :: [Int])

instance EraGenericGen AlonzoEra where
  setValidity = alonzoSetValidity
  setCollateralInputs = L.set collateralInputsTxBodyL
  setRedeemers = L.set rdmrsTxWitsL
  genExUnits = alonzoGenExUnits
  setNetworkIdTxBody = L.set networkIdTxBodyL
  ppMaxCollateralInputsT = ppMaxCollateralInputsL
  ppCollateralPercentageT = ppCollateralPercentageL
  mkScriptIntegrityHash = alonzoMkScriptIntegrityHash
  ppCostModelsT = ppCostModelsL
  ppMaxTxExUnitsT = ppMaxTxExUnitsL
  ppMaxBlockExUnitsT = ppMaxBlockExUnitsL
  ppMaxValSizeT = ppMaxValSizeL

shelleyApplyTx :: EraModel era => Int -> SlotNo -> Model era -> Tx era -> Model era
shelleyApplyTx count slot model tx = applyTxBody count epochAccurateModel $ tx ^. bodyTxL
  where
    modelEpoch = mEL model
    transactionEpoch = epochFromSlotNo slot
    epochAccurateModel = epochBoundary transactionEpoch modelEpoch model

babbageApplyTx ::
  forall era.
  (EraModel era, AlonzoEraTx era, Reflect era, BabbageEraTxBody era) =>
  Int -> SlotNo -> Model era -> Tx era -> Model era
babbageApplyTx count slot model tx = case tx ^. isValidTxL of
  IsValid True -> applyTxSimple count epochAccurateModel tx
  IsValid False -> applyTxFail count nextTxIx epochAccurateModel tx
  where
    transactionEpoch = epochFromSlotNo slot
    modelEpoch = mEL model
    epochAccurateModel = epochBoundary transactionEpoch modelEpoch model
    txbody = tx ^. bodyTxL
    outputs = txbody ^. outputsTxBodyL
    nextTxIx = mkTxIxPartial (fromIntegral (length outputs)) -- When IsValid is false, ColRet will get this TxIx

instance EraGenericGen BabbageEra where
  setValidity = alonzoSetValidity
  setRefernceInputs = L.set referenceInputsTxBodyL
  setCollateralInputs = L.set collateralInputsTxBodyL
  setTotalCollateral = L.set totalCollateralTxBodyL
  setCollateralReturn = L.set collateralReturnTxBodyL
  setRedeemers = L.set rdmrsTxWitsL
  genExUnits = alonzoGenExUnits
  setNetworkIdTxBody = L.set networkIdTxBodyL
  ppMaxCollateralInputsT = ppMaxCollateralInputsL
  ppCollateralPercentageT = ppCollateralPercentageL
  mkScriptIntegrityHash = alonzoMkScriptIntegrityHash
  ppCostModelsT = ppCostModelsL
  ppMaxTxExUnitsT = ppMaxTxExUnitsL
  ppMaxBlockExUnitsT = ppMaxBlockExUnitsL
  ppMaxValSizeT = ppMaxValSizeL

instance EraGenericGen ConwayEra where
  setValidity = alonzoSetValidity
  setRefernceInputs = L.set referenceInputsTxBodyL
  setCollateralInputs = L.set collateralInputsTxBodyL
  setTotalCollateral = L.set totalCollateralTxBodyL
  setCollateralReturn = L.set collateralReturnTxBodyL
  setRedeemers = L.set rdmrsTxWitsL
  genExUnits = alonzoGenExUnits
  setNetworkIdTxBody = L.set networkIdTxBodyL
  ppMaxCollateralInputsT = ppMaxCollateralInputsL
  ppCollateralPercentageT = ppCollateralPercentageL
  mkScriptIntegrityHash = alonzoMkScriptIntegrityHash
  ppCostModelsT = ppCostModelsL
  ppMaxTxExUnitsT = ppMaxTxExUnitsL
  ppMaxBlockExUnitsT = ppMaxBlockExUnitsL
  ppMaxValSizeT = ppMaxValSizeL
