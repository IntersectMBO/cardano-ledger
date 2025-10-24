{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Test.Cardano.Ledger.Generic.Instances () where

import Cardano.Ledger.Address (Addr (..))
import Cardano.Ledger.Allegra (AllegraEra)
import Cardano.Ledger.Alonzo (AlonzoEra)
import Cardano.Ledger.Alonzo.Scripts (isPlutusScript)
import Cardano.Ledger.Babbage (BabbageEra)
import Cardano.Ledger.BaseTypes (
  EpochInterval (..),
  ProtVer (..),
  SlotNo (..),
  StrictMaybe (..),
  mkTxIxPartial,
 )
import Cardano.Ledger.Coin (Coin (..), compactCoinOrError)
import Cardano.Ledger.Compactible (fromCompact)
import Cardano.Ledger.Conway (ConwayEra)
import Cardano.Ledger.Conway.Core
import Cardano.Ledger.Conway.State
import Cardano.Ledger.Credential (Credential (..), Ptr (..))
import Cardano.Ledger.Mary (MaryEra)
import Cardano.Ledger.Plutus (ExUnits (..), Language (..))
import Cardano.Ledger.Shelley (ShelleyEra)
import Cardano.Ledger.Shelley.Scripts (pattern RequireAllOf, pattern RequireAnyOf)
import Cardano.Ledger.Shelley.TxCert (ShelleyDelegCert (..), ShelleyTxCert (..))
import Cardano.Ledger.Val (Val (..))
import Control.Monad.RWS.Strict (gets)
import Control.Monad.Trans (MonadTrans (..))
import qualified Data.Foldable as F
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Ratio ((%))
import Data.TreeDiff (ToExpr (..))
import Lens.Micro (Lens', (&), (.~), (<>~), (?~), (^.))
import qualified Lens.Micro as L
import Test.Cardano.Ledger.Alonzo.Era (AlonzoEraTest, EraTest (..), registerTestAccount)
import Test.Cardano.Ledger.Alonzo.Scripts (alwaysFails, alwaysSucceeds)
import Test.Cardano.Ledger.Common (Arbitrary (..), Gen, choose, chooseInt, elements)
import Test.Cardano.Ledger.Examples.STSTestUtils (EraModel (..))
import Test.Cardano.Ledger.Generic.ApplyTx (applyTxBody, applyTxFail, applyTxSimple, epochBoundary)
import Test.Cardano.Ledger.Generic.GenState (
  EraGenericGen (..),
  GenEnv (..),
  GenRS,
  GenSize (..),
  GenState (..),
 )
import Test.Cardano.Ledger.Generic.ModelState (Model, ModelNewEpochState (..))
import Test.Cardano.Ledger.Generic.Proof (Reflect)
import Test.Cardano.Ledger.Generic.TxGen (
  alonzoMkRedeemers,
  alonzoMkRedeemersFromTags,
  mkAlonzoPlutusPurposePointer,
  mkConwayPlutusPurposePointer,
 )
import Test.Cardano.Ledger.Generic.Updaters (alonzoNewScriptIntegrityHash)
import Test.Cardano.Ledger.Shelley.Generator.Core (genNatural)
import Test.Cardano.Ledger.Shelley.Utils (epochFromSlotNo)

shelleyGenPParams :: EraPParams era => Gen (PParams era)
shelleyGenPParams = do
  minfeeA <- Coin <$> choose (0, 1000)
  minfeeB <- Coin <$> choose (0, 10000)
  pure $
    emptyPParams
      & ppMinFeeAL .~ minfeeA
      & ppMinFeeBL .~ minfeeB
      & ppMaxTxSizeL .~ fromIntegral (maxBound :: Int)
      & ppProtocolVersionL .~ ProtVer (eraProtVerLow @ShelleyEra) 0
      & ppPoolDepositL .~ Coin 5
      & ppKeyDepositL .~ Coin 2
      & ppEMaxL .~ EpochInterval 5

applyShelleyCert :: forall era. EraTest era => Model era -> ShelleyTxCert era -> Model era
applyShelleyCert model dcert = case dcert of
  ShelleyTxCertDelegCert (ShelleyRegCert cred) ->
    model
      { mAccounts =
          registerTestAccount
            cred
            (Just (Ptr minBound minBound minBound))
            (compactCoinOrError (pp ^. ppKeyDepositL))
            Nothing
            Nothing
            (mAccounts model)
      , mDeposited = mDeposited model <+> pp ^. ppKeyDepositL
      }
    where
      pp = mPParams model
  ShelleyTxCertDelegCert (ShelleyUnRegCert cred) ->
    case unregisterAccount cred (mAccounts model) of
      (Nothing, _) -> error ("DeRegKey not in rewards: " <> show (toExpr cred))
      (Just accountState, accounts)
        | accountState ^. balanceAccountStateL == mempty ->
            model
              { mAccounts = accounts
              , mDeposited = mDeposited model <-> fromCompact (accountState ^. depositAccountStateL)
              }
      _ -> error "DeRegKey with non-zero balance"
  ShelleyTxCertDelegCert (ShelleyDelegCert cred poolId) ->
    model
      { mAccounts =
          adjustAccountState (stakePoolDelegationAccountStateL ?~ poolId) cred (mAccounts model)
      }
  ShelleyTxCertPool (RegPool stakePoolParams) ->
    model
      { mStakePools =
          Map.insert
            hk
            (mkStakePoolState (pp ^. ppPoolDepositCompactL) stakePoolParams)
            (mStakePools model)
      , mDeposited =
          if Map.member hk (mStakePools model)
            then mDeposited model
            else mDeposited model <+> pp ^. ppPoolDepositL
      }
    where
      hk = sppId stakePoolParams
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

allegraSetValidity :: AllegraEraTxBody era => ValidityInterval -> TxBody era -> TxBody era
allegraSetValidity vi = vldtTxBodyL .~ vi

allegraValidTxOut :: EraTxOut era => Map ScriptHash (Script era) -> TxOut era -> Bool
allegraValidTxOut _ txOut = case txOut ^. addrTxOutL of
  Addr _ KeyHashObj {} _ -> True
  _ -> False

alonzoValidTxOut ::
  ( EraTxOut era
  , AlonzoEraScript era
  ) =>
  Map ScriptHash (Script era) -> TxOut era -> Bool
alonzoValidTxOut scripts txOut = case txOut ^. addrTxOutL of
  Addr _ KeyHashObj {} _ -> True
  Addr _ (ScriptHashObj sh) _ ->
    case Map.lookup sh scripts of
      Just s -> isPlutusScript s
      _ -> False
  AddrBootstrap {} -> False

alonzoGenPParams :: forall era. AlonzoEraTest era => GenSize -> Gen (PParams era)
alonzoGenPParams gsize = do
  pp <- shelleyGenPParams
  maxTxExUnits <- arbitrary :: Gen ExUnits
  maxCollateralInputs <- elements [1 .. collInputsMax gsize]
  collateralPercentage <- fromIntegral <$> chooseInt (1, 10000)
  pure $
    pp
      & ppMaxTxExUnitsL .~ maxTxExUnits
      & ppCostModelsL .~ zeroCostModels @era
      & ppMaxValSizeL .~ 1000
      & ppMaxCollateralInputsL .~ maxCollateralInputs
      & ppCollateralPercentageL .~ collateralPercentage
      & ppProtocolVersionL .~ ProtVer (eraProtVerLow @era) 0

instance EraModel ShelleyEra where
  applyTx = shelleyApplyTx
  applyCert = applyShelleyCert
  always _ = fromNativeScript $ RequireAllOf []
  never _ = fromNativeScript $ RequireAnyOf []
  collateralReturnTxBodyT = dummyLens SNothing
  validTxOut scripts txOut =
    case txOut ^. addrTxOutL of
      Addr _ (KeyHashObj _) _ -> True
      Addr _ (ScriptHashObj sh) _ -> Map.member sh scripts
      AddrBootstrap {} -> False

instance EraModel AllegraEra where
  applyTx = shelleyApplyTx
  applyCert = applyShelleyCert
  always _ = fromNativeScript $ RequireAllOf []
  never _ = fromNativeScript $ RequireAnyOf []
  collateralReturnTxBodyT = dummyLens SNothing
  validTxOut = allegraValidTxOut

instance EraModel MaryEra where
  applyTx = shelleyApplyTx
  applyCert = applyShelleyCert
  always _ = fromNativeScript $ RequireAllOf []
  never _ = fromNativeScript $ RequireAnyOf []
  collateralReturnTxBodyT = dummyLens SNothing
  validTxOut = allegraValidTxOut

instance EraModel AlonzoEra where
  applyTx = alonzoApplyTx
  applyCert = applyShelleyCert
  mkRedeemersFromTags = alonzoMkRedeemersFromTags
  mkRedeemers = alonzoMkRedeemers
  newScriptIntegrityHash = alonzoNewScriptIntegrityHash
  mkPlutusPurposePointer = mkAlonzoPlutusPurposePointer
  always = alwaysSucceeds @PlutusV1
  never = alwaysFails @PlutusV1
  collateralReturnTxBodyT = dummyLens SNothing
  validTxOut = alonzoValidTxOut

instance EraModel BabbageEra where
  applyTx = alonzoApplyTx
  applyCert = applyShelleyCert
  mkRedeemersFromTags = alonzoMkRedeemersFromTags
  mkRedeemers = alonzoMkRedeemers
  newScriptIntegrityHash = alonzoNewScriptIntegrityHash
  mkPlutusPurposePointer = mkAlonzoPlutusPurposePointer
  always = alwaysSucceeds @PlutusV1
  never = alwaysFails @PlutusV1
  collateralReturnTxBodyT = collateralReturnTxBodyL
  validTxOut = alonzoValidTxOut

instance EraModel ConwayEra where
  applyTx = alonzoApplyTx
  applyCert = error "Not yet implemented"
  mkRedeemersFromTags = alonzoMkRedeemersFromTags
  mkRedeemers = alonzoMkRedeemers
  newScriptIntegrityHash = alonzoNewScriptIntegrityHash
  mkPlutusPurposePointer = mkConwayPlutusPurposePointer
  always = alwaysSucceeds @PlutusV1
  never = alwaysFails @PlutusV1
  collateralReturnTxBodyT = collateralReturnTxBodyL
  validTxOut = alonzoValidTxOut

instance EraGenericGen ShelleyEra where
  setValidity = shelleySetValidity
  setReferenceInputs = const id
  setCollateralInputs = const id
  setTotalCollateral = const id
  setCollateralReturn = const id
  addRedeemers = const id
  setScriptIntegrityHash = const id
  setNetworkIdTxBody = const id
  genExUnits = const $ pure []
  ppMaxCollateralInputsT = dummyLens 0
  ppCollateralPercentageT = dummyLens 0
  ppCostModelsT = dummyLens mempty
  ppMaxTxExUnitsT = dummyLens mempty
  ppMaxBlockExUnitsT = dummyLens mempty
  ppMaxValSizeT = dummyLens 0
  mkScriptIntegrityHash _ _ _ = SNothing
  genPParams _ = shelleyGenPParams

instance EraGenericGen MaryEra where
  setValidity = allegraSetValidity
  setReferenceInputs = const id
  setCollateralInputs = const id
  setTotalCollateral = const id
  setCollateralReturn = const id
  addRedeemers = const id
  setScriptIntegrityHash = const id
  setNetworkIdTxBody = const id
  genExUnits = const $ pure []
  ppMaxCollateralInputsT = dummyLens 0
  ppCollateralPercentageT = dummyLens 0
  ppCostModelsT = dummyLens mempty
  ppMaxTxExUnitsT = dummyLens mempty
  ppMaxBlockExUnitsT = dummyLens mempty
  ppMaxValSizeT = dummyLens 0
  mkScriptIntegrityHash _ _ _ = SNothing
  genPParams _ = shelleyGenPParams

instance EraGenericGen AllegraEra where
  setValidity = allegraSetValidity
  setReferenceInputs = const id
  setCollateralInputs = const id
  setTotalCollateral = const id
  setCollateralReturn = const id
  addRedeemers = const id
  setScriptIntegrityHash = const id
  setNetworkIdTxBody = const id
  genExUnits = const $ pure []
  ppMaxCollateralInputsT = dummyLens 0
  ppCollateralPercentageT = dummyLens 0
  ppCostModelsT = dummyLens mempty
  ppMaxTxExUnitsT = dummyLens mempty
  ppMaxBlockExUnitsT = dummyLens mempty
  ppMaxValSizeT = dummyLens 0
  mkScriptIntegrityHash _ _ _ = SNothing
  genPParams _ = shelleyGenPParams

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
  setValidity = allegraSetValidity
  setCollateralInputs = L.set collateralInputsTxBodyL
  addRedeemers x = rdmrsTxWitsL <>~ x
  genExUnits = alonzoGenExUnits
  setNetworkIdTxBody = L.set networkIdTxBodyL
  ppMaxCollateralInputsT = ppMaxCollateralInputsL
  ppCollateralPercentageT = ppCollateralPercentageL
  mkScriptIntegrityHash = alonzoMkScriptIntegrityHash
  ppCostModelsT = ppCostModelsL
  ppMaxTxExUnitsT = ppMaxTxExUnitsL
  ppMaxBlockExUnitsT = ppMaxBlockExUnitsL
  ppMaxValSizeT = ppMaxValSizeL
  setScriptIntegrityHash = L.set scriptIntegrityHashTxBodyL
  setReferenceInputs = const id
  setTotalCollateral = const id
  setCollateralReturn = const id
  genPParams = alonzoGenPParams

shelleyApplyTx :: EraModel era => Int -> SlotNo -> Model era -> Tx era -> Model era
shelleyApplyTx count slot model tx = applyTxBody count epochAccurateModel $ tx ^. bodyTxL
  where
    modelEpoch = mEL model
    transactionEpoch = epochFromSlotNo slot
    epochAccurateModel = epochBoundary transactionEpoch modelEpoch model

alonzoApplyTx ::
  forall era.
  (EraModel era, AlonzoEraTx era, Reflect era) =>
  Int -> SlotNo -> Model era -> Tx era -> Model era
alonzoApplyTx count slot model tx = case tx ^. isValidTxL of
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
  setValidity = allegraSetValidity
  setReferenceInputs = L.set referenceInputsTxBodyL
  setCollateralInputs = L.set collateralInputsTxBodyL
  setTotalCollateral = L.set totalCollateralTxBodyL
  setCollateralReturn = L.set collateralReturnTxBodyL
  addRedeemers x = rdmrsTxWitsL <>~ x
  genExUnits = alonzoGenExUnits
  setNetworkIdTxBody = L.set networkIdTxBodyL
  ppMaxCollateralInputsT = ppMaxCollateralInputsL
  ppCollateralPercentageT = ppCollateralPercentageL
  mkScriptIntegrityHash = alonzoMkScriptIntegrityHash
  ppCostModelsT = ppCostModelsL
  ppMaxTxExUnitsT = ppMaxTxExUnitsL
  ppMaxBlockExUnitsT = ppMaxBlockExUnitsL
  ppMaxValSizeT = ppMaxValSizeL
  setScriptIntegrityHash = L.set scriptIntegrityHashTxBodyL
  genPParams = alonzoGenPParams

instance EraGenericGen ConwayEra where
  setValidity = allegraSetValidity
  setReferenceInputs = L.set referenceInputsTxBodyL
  setCollateralInputs = L.set collateralInputsTxBodyL
  setTotalCollateral = L.set totalCollateralTxBodyL
  setCollateralReturn = L.set collateralReturnTxBodyL
  addRedeemers x = rdmrsTxWitsL <>~ x
  genExUnits = alonzoGenExUnits
  setNetworkIdTxBody = L.set networkIdTxBodyL
  ppMaxCollateralInputsT = ppMaxCollateralInputsL
  ppCollateralPercentageT = ppCollateralPercentageL
  mkScriptIntegrityHash = alonzoMkScriptIntegrityHash
  ppCostModelsT = ppCostModelsL
  ppMaxTxExUnitsT = ppMaxTxExUnitsL
  ppMaxBlockExUnitsT = ppMaxBlockExUnitsL
  ppMaxValSizeT = ppMaxValSizeL
  setScriptIntegrityHash = L.set scriptIntegrityHashTxBodyL
  genPParams = alonzoGenPParams

-- Utils

-- | Create an unlawful "lens" that returns the specified value when used as a
-- getter and does nothing when used as a setter
dummyLens :: b -> Lens' a b
dummyLens val = L.lens (const val) const
