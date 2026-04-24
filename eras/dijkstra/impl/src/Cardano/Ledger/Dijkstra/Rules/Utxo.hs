{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Cardano.Ledger.Dijkstra.Rules.Utxo (
  DijkstraUTXO,
  DijkstraUtxoEnv (..),
  DijkstraUtxoPredFailure (..),
  conwayToDijkstraUtxoPredFailure,
  validateWrongNetworkInDirectDeposit,
) where

import Cardano.Ledger.Address (DirectDeposits (..))
import Cardano.Ledger.Allegra.Rules (AllegraUtxoPredFailure, shelleyToAllegraUtxoPredFailure)
import qualified Cardano.Ledger.Allegra.Rules as Allegra
import Cardano.Ledger.Alonzo.Rules (
  AlonzoUtxoEvent,
  AlonzoUtxoPredFailure,
  AlonzoUtxosPredFailure,
 )
import qualified Cardano.Ledger.Alonzo.Rules as Alonzo
import Cardano.Ledger.Alonzo.TxWits (unRedeemersL)
import Cardano.Ledger.Babbage.Rules (
  BabbageUtxoPredFailure,
  updateUTxOStateByTxValidity,
  validateTotalCollateral,
 )
import qualified Cardano.Ledger.Babbage.Rules as Babbage (
  validateOutputTooSmallUTxO,
 )
import Cardano.Ledger.BaseTypes (
  Mismatch (..),
  Network,
  Relation (..),
  ShelleyBase,
  SlotNo,
  StrictMaybe (..),
  epochInfo,
  networkId,
  systemStart,
 )
import Cardano.Ledger.Binary (
  DecCBOR (..),
  EncCBOR (..),
  sizedValue,
 )
import Cardano.Ledger.Binary.Coders (
  Decode (..),
  Encode (..),
  decode,
  encode,
  (!>),
  (<!),
 )
import Cardano.Ledger.Coin (Coin, DeltaCoin)
import Cardano.Ledger.Compactible (fromCompact)
import Cardano.Ledger.Conway.Core
import Cardano.Ledger.Conway.Rules (
  ConwayUTXOS,
  ConwayUtxoPredFailure,
  ConwayUtxosEnv (..),
  ConwayUtxosPredFailure (..),
  allegraToConwayUtxoPredFailure,
  alonzoToConwayUtxoPredFailure,
  babbageToConwayUtxoPredFailure,
  updateTreasuryDonation,
 )
import qualified Cardano.Ledger.Conway.Rules as Conway
import Cardano.Ledger.Conway.State
import Cardano.Ledger.Credential (StakeReference (..))
import Cardano.Ledger.Dijkstra.Era (DijkstraEra, DijkstraUTXO)
import Cardano.Ledger.Dijkstra.Rules.Utxos ()
import Cardano.Ledger.Dijkstra.TxBody (DijkstraEraTxBody (..))
import Cardano.Ledger.Plutus (ExUnits)
import Cardano.Ledger.Rules.ValidationMode (Test, failOnJustStatic, runTest, runTestOnSignal)
import Cardano.Ledger.Shelley.LedgerState (UTxOState (..))
import Cardano.Ledger.Shelley.Rules (
  ShelleyUtxoPredFailure,
  validSizeComputationCheck,
 )
import qualified Cardano.Ledger.Shelley.Rules as Shelley
import Cardano.Ledger.TxIn (TxIn)
import Control.DeepSeq (NFData)
import Control.Monad (when)
import Control.Monad.Trans.Reader (asks)
import Control.State.Transition.Extended (
  Embed (..),
  Rule,
  STS (..),
  TRC (..),
  TransitionRule,
  failureOnNonEmptyMap,
  failureOnNonEmptySet,
  judgmentContext,
  liftSTS,
  trans,
  validate,
 )
import Data.List.NonEmpty (NonEmpty)
import Data.Map.NonEmpty (NonEmptyMap)
import qualified Data.Map.Strict as Map
import qualified Data.OMap.Strict as OMap
import Data.Set.NonEmpty (NonEmptySet)
import Data.Word (Word16, Word32)
import GHC.Generics (Generic)
import Lens.Micro ((^.))

data DijkstraUtxoEnv era = DijkstraUtxoEnv
  { dueSlot :: SlotNo
  , duePParams :: PParams era
  , dueCertState :: CertState era
  , dueOriginalUtxo :: UTxO era
  , dueScriptsProvided :: ScriptsProvided era
  -- ^ aggregated scripts provided across all transaction levels
  }

-- | Predicate failure for the Dijkstra Era
data DijkstraUtxoPredFailure era
  = -- | Subtransition Failures
    UtxosFailure (PredicateFailure (EraRule "UTXOS" era))
  | -- | The bad transaction inputs
    BadInputsUTxO (NonEmptySet TxIn)
  | OutsideValidityIntervalUTxO
      -- | transaction's validity interval
      ValidityInterval
      -- | current slot
      SlotNo
  | MaxTxSizeUTxO (Mismatch RelLTEQ Word32)
  | InputSetEmptyUTxO
  | FeeTooSmallUTxO
      (Mismatch RelGTEQ Coin)
  | ValueNotConservedUTxO
      (Mismatch RelEQ (Value era)) -- Serialise consumed first, then produced
  | -- | the set of addresses with incorrect network IDs
    WrongNetwork
      -- | the expected network id
      Network
      -- | the set of addresses with incorrect network IDs
      (NonEmptySet Addr)
  | WrongNetworkWithdrawal
      -- | the expected network id
      Network
      -- | the set of reward addresses with incorrect network IDs
      (NonEmptySet AccountAddress)
  | -- | list of supplied bad transaction outputs
    OutputBootAddrAttrsTooBig (NonEmpty (TxOut era))
  | -- | list of supplied bad transaction output triples (actualSize,PParameterMaxValue,TxOut)
    OutputTooBigUTxO (NonEmpty (Int, Int, TxOut era))
  | InsufficientCollateral
      -- | balance computed
      DeltaCoin
      -- | the required collateral for the given fee
      Coin
  | -- | The UTxO entries which have the wrong kind of script
    ScriptsNotPaidUTxO (NonEmptyMap TxIn (TxOut era))
  | ExUnitsTooBigUTxO
      (Mismatch RelLTEQ ExUnits)
  | -- | The inputs marked for use as fees contain non-ADA tokens
    CollateralContainsNonADA (Value era)
  | -- | Wrong Network ID in body
    WrongNetworkInTxBody
      (Mismatch RelEQ Network)
  | -- | slot number outside consensus forecast range
    OutsideForecast SlotNo
  | -- | There are too many collateral inputs
    TooManyCollateralInputs
      (Mismatch RelLTEQ Word16)
  | NoCollateralInputs
  | -- | The collateral is not equivalent to the total collateral asserted by the transaction
    IncorrectTotalCollateralField
      -- | collateral provided
      DeltaCoin
      -- | collateral amount declared in transaction body
      Coin
  | -- | list of supplied transaction outputs that are too small,
    -- together with the minimum value for the given output.
    BabbageOutputTooSmallUTxO (NonEmpty (TxOut era, Coin))
  | -- | TxIns that appear in both inputs and reference inputs
    BabbageNonDisjointRefInputs (NonEmpty TxIn)
  | PtrPresentInCollateralReturn (TxOut era)
  | WrongNetworkInDirectDeposit
      -- | the expected network id
      Network
      -- | the set of account addresses with incorrect network IDs
      (NonEmptySet AccountAddress)
  | -- | Total withdrawals per account that exceed the original account balance
    WithdrawalsExceedAccountBalance (NonEmptyMap AccountAddress (Mismatch RelLTEQ Coin))
  deriving (Generic)

type instance EraRuleFailure "UTXO" DijkstraEra = DijkstraUtxoPredFailure DijkstraEra

type instance EraRuleEvent "UTXO" DijkstraEra = AlonzoUtxoEvent DijkstraEra

instance InjectRuleFailure "UTXO" DijkstraUtxoPredFailure DijkstraEra

instance InjectRuleFailure "UTXO" ConwayUtxoPredFailure DijkstraEra where
  injectFailure = conwayToDijkstraUtxoPredFailure

instance InjectRuleFailure "UTXO" BabbageUtxoPredFailure DijkstraEra where
  injectFailure = conwayToDijkstraUtxoPredFailure . babbageToConwayUtxoPredFailure

instance InjectRuleFailure "UTXO" AlonzoUtxoPredFailure DijkstraEra where
  injectFailure = conwayToDijkstraUtxoPredFailure . alonzoToConwayUtxoPredFailure

instance InjectRuleFailure "UTXO" ShelleyUtxoPredFailure DijkstraEra where
  injectFailure =
    conwayToDijkstraUtxoPredFailure . allegraToConwayUtxoPredFailure . shelleyToAllegraUtxoPredFailure

instance InjectRuleFailure "UTXO" Allegra.AllegraUtxoPredFailure DijkstraEra where
  injectFailure = conwayToDijkstraUtxoPredFailure . allegraToConwayUtxoPredFailure

instance InjectRuleFailure "UTXO" ConwayUtxosPredFailure DijkstraEra where
  injectFailure = UtxosFailure

instance InjectRuleFailure "UTXO" AlonzoUtxosPredFailure DijkstraEra where
  injectFailure =
    conwayToDijkstraUtxoPredFailure
      . alonzoToConwayUtxoPredFailure
      . Alonzo.UtxosFailure
      . injectFailure

deriving instance
  ( Era era
  , Show (Value era)
  , Show (PredicateFailure (EraRule "UTXOS" era))
  , Show (TxOut era)
  , Show (Script era)
  , Show TxIn
  ) =>
  Show (DijkstraUtxoPredFailure era)

deriving instance
  ( Era era
  , Eq (Value era)
  , Eq (PredicateFailure (EraRule "UTXOS" era))
  , Eq (TxOut era)
  , Eq (Script era)
  , Eq TxIn
  ) =>
  Eq (DijkstraUtxoPredFailure era)

instance
  ( Era era
  , NFData (Value era)
  , NFData (TxOut era)
  , NFData (PredicateFailure (EraRule "UTXOS" era))
  ) =>
  NFData (DijkstraUtxoPredFailure era)

validateNoPtrInCollateralReturn ::
  ( BabbageEraTxBody era
  , InjectRuleFailure rule DijkstraUtxoPredFailure era
  ) =>
  TxBody TopTx era ->
  Rule (EraRule rule era) ctx ()
validateNoPtrInCollateralReturn txBody = do
  let hasCollateralTxOut = do
        SJust collateralReturn <- pure $ txBody ^. collateralReturnTxBodyL
        Addr _ _ (StakeRefPtr {}) <- pure $ collateralReturn ^. addrTxOutL
        Just collateralReturn
  failOnJustStatic hasCollateralTxOut (injectFailure . PtrPresentInCollateralReturn)

-- | For each account, the total withdrawals across the entire batch should not exceed the original account balance.
-- Unregistered accounts are treated as having 0 balance.
validateBatchWithdrawals ::
  ( EraTx era
  , EraAccounts era
  , DijkstraEraTxBody era
  ) =>
  Accounts era ->
  Tx TopTx era ->
  Test (DijkstraUtxoPredFailure era)
validateBatchWithdrawals accounts tx =
  let allWithdrawals =
        Map.unionsWith (<>) $
          unWithdrawals (tx ^. bodyTxL . withdrawalsTxBodyL)
            : [ unWithdrawals $ subTx ^. bodyTxL . withdrawalsTxBodyL
              | subTx <- OMap.elems $ tx ^. bodyTxL . subTransactionsTxBodyL
              ]
      badWithdrawals =
        Map.mapMaybeWithKey
          ( \acctAddr withdrawn ->
              let balance = getAccountBalance acctAddr
               in if withdrawn > balance
                    then Just Mismatch {mismatchSupplied = withdrawn, mismatchExpected = balance}
                    else Nothing
          )
          allWithdrawals
   in failureOnNonEmptyMap badWithdrawals WithdrawalsExceedAccountBalance
  where
    getAccountBalance (AccountAddress _ (AccountId cred)) =
      case lookupAccountState cred accounts of
        Nothing -> mempty -- unregistered account, 0 balance
        Just accountState -> fromCompact $ accountState ^. balanceAccountStateL

-- | Validate collateral if any transaction in the batch has redeemers.
validateBatchCollateral ::
  forall era rule.
  ( AlonzoEraTx era
  , DijkstraEraTxBody era
  , InjectRuleFailure rule AlonzoUtxoPredFailure era
  , InjectRuleFailure rule BabbageUtxoPredFailure era
  ) =>
  PParams era ->
  Tx TopTx era ->
  UTxO era ->
  Test (EraRuleFailure rule era)
validateBatchCollateral pp tx (UTxO utxo) =
  -- TODO OPTIMIZATION: Rewrite in a way that doesn't require this check when rules are executed without validation
  when (hasAnyRedeemers tx) $
    validateTotalCollateral pp (tx ^. bodyTxL) utxoCollateral
  where
    utxoCollateral = Map.restrictKeys utxo (tx ^. bodyTxL . collateralInputsTxBodyL)
    hasAnyRedeemers t =
      hasRedeemers t || any hasRedeemers (t ^. bodyTxL . subTransactionsTxBodyL)
    hasRedeemers = not . null . (^. witsTxL . rdmrsTxWitsL . unRedeemersL)

validateWrongNetworkInDirectDeposit ::
  DijkstraEraTxBody era =>
  Network ->
  TxBody t era ->
  Test (DijkstraUtxoPredFailure era)
validateWrongNetworkInDirectDeposit netId txb =
  failureOnNonEmptySet depositsWrongNetwork (WrongNetworkInDirectDeposit netId)
  where
    depositsWrongNetwork =
      Map.keysSet $
        Map.filterWithKey
          (\a _ -> aaNetworkId a /= netId)
          (unDirectDeposits $ txb ^. directDepositsTxBodyL)

dijkstraUtxoTransition ::
  forall era.
  ( EraUTxO era
  , EraCertState era
  , DijkstraEraTxBody era
  , AlonzoEraTx era
  , EraStake era
  , InjectRuleFailure "UTXO" ShelleyUtxoPredFailure era
  , InjectRuleFailure "UTXO" AllegraUtxoPredFailure era
  , InjectRuleFailure "UTXO" AlonzoUtxoPredFailure era
  , InjectRuleFailure "UTXO" BabbageUtxoPredFailure era
  , InjectRuleFailure "UTXO" DijkstraUtxoPredFailure era
  , Environment (EraRule "UTXO" era) ~ DijkstraUtxoEnv era
  , State (EraRule "UTXO" era) ~ UTxOState era
  , Signal (EraRule "UTXO" era) ~ Tx TopTx era
  , BaseM (EraRule "UTXO" era) ~ ShelleyBase
  , STS (EraRule "UTXO" era)
  , Event (EraRule "UTXO" era) ~ AlonzoUtxoEvent era
  , -- In this function we call the UTXOS rule, so we need some assumptions
    Environment (EraRule "UTXOS" era) ~ ConwayUtxosEnv era
  , State (EraRule "UTXOS" era) ~ ()
  , Signal (EraRule "UTXOS" era) ~ Tx TopTx era
  , Embed (EraRule "UTXOS" era) (EraRule "UTXO" era)
  ) =>
  TransitionRule (EraRule "UTXO" era)
dijkstraUtxoTransition = do
  TRC (DijkstraUtxoEnv slot pp certState originalUtxo _scriptsProvided, utxos, tx) <- judgmentContext
  -- this is the original Accounts, before any transactions were applied
  let accounts = certState ^. certDStateL . accountsL

  let txBody = tx ^. bodyTxL

  {- inInterval (SlotOf Γ) (ValidIntervalOf txTop) -}
  runTest $ Allegra.validateOutsideValidityIntervalUTxO slot txBody

  sysSt <- liftSTS $ asks systemStart
  ei <- liftSTS $ asks epochInfo

  runTest $ Alonzo.validateOutsideForecast ei slot sysSt tx

  {- SpendInputs ≠ ∅ -}
  runTestOnSignal $ Shelley.validateInputSetEmptyUTxO txBody

  let allInputs = txBody ^. allInputsTxBodyF
      inputs = txBody ^. inputsTxBodyL

  {- SpendInputsOf txTop ∪ RefInputsOf txTop ∪ CollInputsOf txTop ⊆ dom(utxo₀) -}
  runTest $ Shelley.validateBadInputsUTxO originalUtxo allInputs

  {- SpendInputsOf txTop ⊆ dom(utxo_s) — prevents double-spend with subtxs -}
  runTest $ Shelley.validateBadInputsUTxO (utxosUtxo utxos) inputs

  {- minfee pp txTop utxo₀ ≤ txfee txb -}
  runTest $ Shelley.validateFeeTooSmallUTxO pp tx originalUtxo

  {- (RedeemersOf txTop ≠ ∅ ⊎ Any (λ txSub → RedeemersOf txSub ≠ ∅) subtxs) → collateralCheck -}
  validate $ validateBatchCollateral pp tx originalUtxo

  runTest $ validateBatchWithdrawals accounts tx

  {- consumed pp utxo₀ txb = produced pp certState txb -}
  runTest $ Shelley.validateValueNotConservedUTxO pp originalUtxo certState txBody

  {- ∀ txout ∈ allOuts txb, getValue txout ≥ inject (serSize txout * coinsPerUTxOByte pp) -}
  let allSizedOutputs = txBody ^. allSizedOutputsTxBodyF
  runTest $ Babbage.validateOutputTooSmallUTxO pp allSizedOutputs

  let allOutputs = fmap sizedValue allSizedOutputs
  {- ∀ txout ∈ allOuts txb, serSize (getValue txout) ≤ maxValSize pp -}
  runTest $ Alonzo.validateOutputTooBigUTxO pp allOutputs

  {- ∀ ( _ ↦ (a,_)) ∈ allOuts txb, a ∈ Addrbootstrap → bootstrapAttrsSize a ≤ 64 -}
  runTestOnSignal $ Shelley.validateOutputBootAddrAttrsTooBig allOutputs

  netId <- liftSTS $ asks networkId

  {- ∀(_ → (a, _)) ∈ allOuts txb, netId a = NetworkId -}
  runTestOnSignal $ Shelley.validateWrongNetwork netId allOutputs

  {- ∀(a → ) ∈ txwdrls txb, netId a = NetworkId -}
  runTestOnSignal $ Shelley.validateWrongNetworkWithdrawal netId txBody

  {- (txnetworkid txb = NetworkId) ∨ (txnetworkid txb = ◇) -}
  runTestOnSignal $ Alonzo.validateWrongNetworkInTxBody netId txBody

  {- direct deposit network IDs -}
  runTestOnSignal $ validateWrongNetworkInDirectDeposit netId txBody

  {- no Ptr in collateral return -}
  validateNoPtrInCollateralReturn txBody

  {- txsize tx ≤ maxTxSize pp -}
  runTestOnSignal $ Shelley.validateMaxTxSizeUTxO pp tx

  {- totExunits tx ≤ maxTxExUnits pp -}
  runTest $ Alonzo.validateExUnitsTooBigUTxO pp tx

  {- ‖collateral tx‖ ≤ maxCollInputs pp -}
  runTest $ Alonzo.validateTooManyCollateralInputs pp txBody

  () <- trans @(EraRule "UTXOS" era) $ TRC (ConwayUtxosEnv pp originalUtxo, (), tx)
  updateUTxOStateByTxValidity
    pp
    certState
    (utxosGovState utxos)
    tx
    (updateTreasuryDonation tx utxos)

--------------------------------------------------------------------------------
-- DijkstraUTXO STS
--------------------------------------------------------------------------------

instance
  forall era.
  ( EraTx era
  , EraUTxO era
  , EraStake era
  , DijkstraEraTxBody era
  , AlonzoEraTx era
  , EraRule "UTXO" era ~ DijkstraUTXO era
  , InjectRuleFailure "UTXO" ShelleyUtxoPredFailure era
  , InjectRuleFailure "UTXO" AllegraUtxoPredFailure era
  , InjectRuleFailure "UTXO" AlonzoUtxoPredFailure era
  , InjectRuleFailure "UTXO" BabbageUtxoPredFailure era
  , InjectRuleFailure "UTXO" ConwayUtxoPredFailure era
  , InjectRuleFailure "UTXO" DijkstraUtxoPredFailure era
  , Environment (EraRule "UTXO" era) ~ DijkstraUtxoEnv era
  , State (EraRule "UTXO" era) ~ UTxOState era
  , Signal (EraRule "UTXO" era) ~ Tx TopTx era
  , BaseM (EraRule "UTXO" era) ~ ShelleyBase
  , STS (EraRule "UTXO" era)
  , -- In this function we we call the UTXOS rule, so we need some assumptions
    Embed (EraRule "UTXOS" era) (DijkstraUTXO era)
  , Environment (EraRule "UTXOS" era) ~ ConwayUtxosEnv era
  , State (EraRule "UTXOS" era) ~ ()
  , Signal (EraRule "UTXOS" era) ~ Tx TopTx era
  , EraCertState era
  , EraRule "UTXO" era ~ DijkstraUTXO era
  , SafeToHash (TxWits era)
  ) =>
  STS (DijkstraUTXO era)
  where
  type State (DijkstraUTXO era) = UTxOState era
  type Signal (DijkstraUTXO era) = Tx TopTx era
  type Environment (DijkstraUTXO era) = DijkstraUtxoEnv era
  type BaseM (DijkstraUTXO era) = ShelleyBase
  type PredicateFailure (DijkstraUTXO era) = DijkstraUtxoPredFailure era
  type Event (DijkstraUTXO era) = AlonzoUtxoEvent era

  initialRules = []

  transitionRules = [dijkstraUtxoTransition @era]

  assertions = [validSizeComputationCheck]

instance
  ( STS (ConwayUTXOS era)
  , PredicateFailure (EraRule "UTXOS" era) ~ ConwayUtxosPredFailure era
  , Event (EraRule "UTXOS" era) ~ Event (ConwayUTXOS era)
  ) =>
  Embed (ConwayUTXOS era) (DijkstraUTXO era)
  where
  wrapFailed = UtxosFailure
  wrapEvent = Alonzo.UtxosEvent

--------------------------------------------------------------------------------
-- Serialisation
--------------------------------------------------------------------------------

instance
  ( Era era
  , EncCBOR (TxOut era)
  , EncCBOR (Value era)
  , EncCBOR (PredicateFailure (EraRule "UTXOS" era))
  ) =>
  EncCBOR (DijkstraUtxoPredFailure era)
  where
  encCBOR =
    encode . \case
      UtxosFailure a -> Sum (UtxosFailure @era) 0 !> To a
      BadInputsUTxO ins -> Sum (BadInputsUTxO @era) 1 !> To ins
      OutsideValidityIntervalUTxO a b -> Sum OutsideValidityIntervalUTxO 2 !> To a !> To b
      MaxTxSizeUTxO mm -> Sum MaxTxSizeUTxO 3 !> To mm
      InputSetEmptyUTxO -> Sum InputSetEmptyUTxO 4
      FeeTooSmallUTxO mm -> Sum FeeTooSmallUTxO 5 !> To mm
      ValueNotConservedUTxO mm -> Sum (ValueNotConservedUTxO @era) 6 !> To mm
      WrongNetwork right wrongs -> Sum (WrongNetwork @era) 7 !> To right !> To wrongs
      WrongNetworkWithdrawal right wrongs -> Sum (WrongNetworkWithdrawal @era) 8 !> To right !> To wrongs
      OutputBootAddrAttrsTooBig outs -> Sum (OutputBootAddrAttrsTooBig @era) 9 !> To outs
      OutputTooBigUTxO outs -> Sum (OutputTooBigUTxO @era) 10 !> To outs
      InsufficientCollateral a b -> Sum InsufficientCollateral 11 !> To a !> To b
      ScriptsNotPaidUTxO a -> Sum ScriptsNotPaidUTxO 12 !> To a
      ExUnitsTooBigUTxO mm -> Sum ExUnitsTooBigUTxO 13 !> To mm
      CollateralContainsNonADA a -> Sum CollateralContainsNonADA 14 !> To a
      WrongNetworkInTxBody mm -> Sum WrongNetworkInTxBody 15 !> To mm
      OutsideForecast a -> Sum OutsideForecast 16 !> To a
      TooManyCollateralInputs mm -> Sum TooManyCollateralInputs 17 !> To mm
      NoCollateralInputs -> Sum NoCollateralInputs 18
      IncorrectTotalCollateralField c1 c2 -> Sum IncorrectTotalCollateralField 19 !> To c1 !> To c2
      BabbageOutputTooSmallUTxO x -> Sum BabbageOutputTooSmallUTxO 20 !> To x
      BabbageNonDisjointRefInputs x -> Sum BabbageNonDisjointRefInputs 21 !> To x
      PtrPresentInCollateralReturn x -> Sum PtrPresentInCollateralReturn 22 !> To x
      WrongNetworkInDirectDeposit right wrongs -> Sum (WrongNetworkInDirectDeposit @era) 23 !> To right !> To wrongs
      WithdrawalsExceedAccountBalance mm -> Sum WithdrawalsExceedAccountBalance 24 !> To mm

instance
  ( Era era
  , DecCBOR (TxOut era)
  , EncCBOR (Value era)
  , DecCBOR (Value era)
  , DecCBOR (PredicateFailure (EraRule "UTXOS" era))
  ) =>
  DecCBOR (DijkstraUtxoPredFailure era)
  where
  decCBOR = decode . Summands "DijkstraUtxoPredFailure" $ \case
    0 -> SumD UtxosFailure <! From
    1 -> SumD BadInputsUTxO <! From
    2 -> SumD OutsideValidityIntervalUTxO <! From <! From
    3 -> SumD MaxTxSizeUTxO <! From
    4 -> SumD InputSetEmptyUTxO
    5 -> SumD FeeTooSmallUTxO <! From
    6 -> SumD ValueNotConservedUTxO <! From
    7 -> SumD WrongNetwork <! From <! From
    8 -> SumD WrongNetworkWithdrawal <! From <! From
    9 -> SumD OutputBootAddrAttrsTooBig <! From
    10 -> SumD OutputTooBigUTxO <! From
    11 -> SumD InsufficientCollateral <! From <! From
    12 -> SumD ScriptsNotPaidUTxO <! From
    13 -> SumD ExUnitsTooBigUTxO <! From
    14 -> SumD CollateralContainsNonADA <! From
    15 -> SumD WrongNetworkInTxBody <! From
    16 -> SumD OutsideForecast <! From
    17 -> SumD TooManyCollateralInputs <! From
    18 -> SumD NoCollateralInputs
    19 -> SumD IncorrectTotalCollateralField <! From <! From
    20 -> SumD BabbageOutputTooSmallUTxO <! From
    21 -> SumD BabbageNonDisjointRefInputs <! From
    22 -> SumD PtrPresentInCollateralReturn <! From
    23 -> SumD WrongNetworkInDirectDeposit <! From <! From
    24 -> SumD WithdrawalsExceedAccountBalance <! From
    n -> Invalid n

-- =====================================================
-- Injecting from one PredicateFailure to another

conwayToDijkstraUtxoPredFailure ::
  forall era.
  ConwayUtxoPredFailure era ->
  DijkstraUtxoPredFailure era
conwayToDijkstraUtxoPredFailure = \case
  Conway.BadInputsUTxO x -> BadInputsUTxO x
  Conway.OutsideValidityIntervalUTxO vi slotNo -> OutsideValidityIntervalUTxO vi slotNo
  Conway.MaxTxSizeUTxO m -> MaxTxSizeUTxO m
  Conway.InputSetEmptyUTxO -> InputSetEmptyUTxO
  Conway.FeeTooSmallUTxO m -> FeeTooSmallUTxO m
  Conway.ValueNotConservedUTxO m -> ValueNotConservedUTxO m
  Conway.WrongNetwork x y -> WrongNetwork x y
  Conway.WrongNetworkWithdrawal x y -> WrongNetworkWithdrawal x y
  Conway.OutputTooSmallUTxO _ -> error "Impossible: `OutputTooSmallUTxO` for DijkstraUTXO"
  Conway.UtxosFailure x -> UtxosFailure x
  Conway.OutputBootAddrAttrsTooBig xs -> OutputBootAddrAttrsTooBig xs
  Conway.OutputTooBigUTxO xs -> OutputTooBigUTxO xs
  Conway.InsufficientCollateral c1 c2 -> InsufficientCollateral c1 c2
  Conway.ScriptsNotPaidUTxO u -> ScriptsNotPaidUTxO u
  Conway.ExUnitsTooBigUTxO m -> ExUnitsTooBigUTxO m
  Conway.CollateralContainsNonADA v -> CollateralContainsNonADA v
  Conway.WrongNetworkInTxBody m -> WrongNetworkInTxBody m
  Conway.OutsideForecast sno -> OutsideForecast sno
  Conway.TooManyCollateralInputs m -> TooManyCollateralInputs m
  Conway.NoCollateralInputs -> NoCollateralInputs
  Conway.IncorrectTotalCollateralField dc c -> IncorrectTotalCollateralField dc c
  Conway.BabbageOutputTooSmallUTxO x -> BabbageOutputTooSmallUTxO x
  Conway.BabbageNonDisjointRefInputs txin -> BabbageNonDisjointRefInputs txin
