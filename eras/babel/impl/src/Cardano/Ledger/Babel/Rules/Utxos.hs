{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Cardano.Ledger.Babel.Rules.Utxos (
  BabelUTXOS,
  BabelUtxosPredFailure (..),
  BabelUtxosEvent (..),
) where

import Cardano.Ledger.Alonzo.Plutus.Context (ContextError, EraPlutusContext (..))
import Cardano.Ledger.Alonzo.Plutus.Evaluate (
  CollectError (..),
  evalPlutusScripts,
  lookupPlutusScript,
 )
import Cardano.Ledger.Alonzo.Rules (
  AlonzoUtxoEvent (..),
  AlonzoUtxoPredFailure (..),
  AlonzoUtxosEvent,
  AlonzoUtxosPredFailure,
  TagMismatchDescription (..),
  invalidBegin,
  invalidEnd,
  validBegin,
  validEnd,
  when2Phase,
 )
import qualified Cardano.Ledger.Alonzo.Rules as Alonzo (
  AlonzoUtxosEvent (..),
  AlonzoUtxosPredFailure (..),
 )
import Cardano.Ledger.Alonzo.Scripts (plutusScriptLanguage, toAsItem, toAsIx)
import Cardano.Ledger.Alonzo.TxWits (lookupRedeemer)
import Cardano.Ledger.Alonzo.UTxO (
  AlonzoEraUTxO (..),
  AlonzoScriptsNeeded (..),
 )
import Cardano.Ledger.Babbage.Collateral (collAdaBalance, collOuts)
import Cardano.Ledger.Babbage.Rules (
  BabbageUTXO,
  BabbageUtxoPredFailure (..),
  expectScriptsToPass,
 )
import Cardano.Ledger.Babbage.Tx
import Cardano.Ledger.Babel.Core
import Cardano.Ledger.Babel.Era (BabelEra, BabelUTXOS)
import Cardano.Ledger.Babel.FRxO (getBabelScriptsNeededFrxo, getScriptsProvidedFrxo, txfrxo)
import Cardano.Ledger.Babel.LedgerState.Types (UTxOStateTemp (..), utxostDonationL)
import Cardano.Ledger.Babel.TxInfo ()
import Cardano.Ledger.BaseTypes
import Cardano.Ledger.Binary (
  DecCBOR (..),
  EncCBOR (..),
 )
import Cardano.Ledger.Binary.Coders
import Cardano.Ledger.CertState (certsTotalDepositsTxBody, certsTotalRefundsTxBody)
import Cardano.Ledger.Coin (Coin (Coin), DeltaCoin (..))
import Cardano.Ledger.Conway.Core (ConwayEraPParams, ConwayEraTxBody (treasuryDonationTxBodyL))
import Cardano.Ledger.Conway.Governance (ConwayGovState (..))
import Cardano.Ledger.FRxO (FRxO (FRxO, unFRxO))
import Cardano.Ledger.Plutus (
  PlutusWithContext (..),
  ScriptFailure (..),
  costModelsValid,
  getPlutusData,
 )
import Cardano.Ledger.Plutus.Evaluate (PlutusDatums (..), ScriptResult (..))
import Cardano.Ledger.SafeHash (SafeHash, hashAnnotated)
import Cardano.Ledger.Shelley.LedgerState (
  CertState,
  UTxOState (..),
  updateStakeDistribution,
 )
import Cardano.Ledger.Shelley.Rules (UtxoEnv (..))
import Cardano.Ledger.Slot (EpochInfo)
import Cardano.Ledger.UTxO (EraUTxO (..), ScriptsProvided (..), UTxO (UTxO, unUTxO))
import Cardano.Ledger.Val ((<->))
import Cardano.Slotting.Time (SystemStart)
import Control.DeepSeq (NFData)
import Control.Monad (guard)
import Control.Monad.Trans.Reader (asks)
import Control.State.Transition.Extended
import Data.List.NonEmpty (NonEmpty, nonEmpty)
import qualified Data.Map as Map
import Data.MapExtras (extractKeys)
import Data.Maybe (mapMaybe)
import qualified Data.Set as Set
import Data.Text (Text)
import Debug.Trace (traceEvent)
import GHC.Generics (Generic)
import Lens.Micro
import NoThunks.Class (NoThunks)
import PlutusLedgerApi.V4 ()

data BabelUtxosPredFailure era
  = -- | The 'isValid' tag on the transaction is incorrect. The tag given
    --   here is that provided on the transaction (whereas evaluation of the
    --   scripts gives the opposite.). The Text tries to explain why it failed.
    ValidationTagMismatch IsValid TagMismatchDescription
  | -- | We could not find all the necessary inputs for a Plutus Script.
    -- Previous PredicateFailure tests should make this impossible, but the
    -- consequences of not detecting this means scripts get dropped, so things
    -- might validate that shouldn't. So we double check in the function
    -- collectTwoPhaseScriptInputs, it should find data for every Script.
    CollectErrors [CollectError era]
  deriving
    (Generic)

data BabelUtxosEvent era
  = TotalDeposits (SafeHash (EraCrypto era) EraIndependentTxBody) Coin
  | SuccessfulPlutusScriptsEvent (NonEmpty (PlutusWithContext (EraCrypto era)))
  | FailedPlutusScriptsEvent (NonEmpty (PlutusWithContext (EraCrypto era)))
  | -- | The UTxOs consumed and created by a signal tx
    TxUTxODiff
      -- | UTxO consumed
      (UTxO era)
      -- | UTxO created
      (UTxO era)
  | -- | The FRxOs consumed and created by a signal tx
    TxFRxODiff
      -- | FRxO consumed
      (FRxO era)
      -- | FRxO created
      (FRxO era)
  deriving (Generic)

deriving instance (Era era, Eq (TxOut era)) => Eq (BabelUtxosEvent era)

instance (Era era, NFData (TxOut era)) => NFData (BabelUtxosEvent era)

type instance EraRuleFailure "UTXOS" (BabelEra c) = BabelUtxosPredFailure (BabelEra c)

type instance EraRuleEvent "UTXOS" (BabelEra c) = BabelUtxosEvent (BabelEra c)

instance InjectRuleFailure "UTXOS" BabelUtxosPredFailure (BabelEra c)

instance InjectRuleEvent "UTXOS" BabelUtxosEvent (BabelEra c)

instance InjectRuleFailure "UTXOS" AlonzoUtxosPredFailure (BabelEra c) where
  injectFailure = alonzoToBabelUtxosPredFailure

instance InjectRuleEvent "UTXOS" AlonzoUtxosEvent (BabelEra c) where
  injectEvent = alonzoToBabelUtxosEvent

alonzoToBabelUtxosPredFailure ::
  forall era.
  EraRuleFailure "PPUP" era ~ VoidEraRule "PPUP" era =>
  Alonzo.AlonzoUtxosPredFailure era ->
  BabelUtxosPredFailure era
alonzoToBabelUtxosPredFailure = \case
  Alonzo.ValidationTagMismatch t x -> ValidationTagMismatch t x
  Alonzo.CollectErrors x -> CollectErrors x
  Alonzo.UpdateFailure x -> absurdEraRule @"PPUP" @era x

alonzoToBabelUtxosEvent ::
  forall era.
  EraRuleEvent "PPUP" era ~ VoidEraRule "PPUP" era =>
  Alonzo.AlonzoUtxosEvent era ->
  BabelUtxosEvent era
alonzoToBabelUtxosEvent = \case
  Alonzo.AlonzoPpupToUtxosEvent x -> absurdEraRule @"PPUP" @era x
  Alonzo.TotalDeposits h c -> TotalDeposits h c
  Alonzo.SuccessfulPlutusScriptsEvent l -> SuccessfulPlutusScriptsEvent l
  Alonzo.FailedPlutusScriptsEvent l -> FailedPlutusScriptsEvent l
  Alonzo.TxUTxODiff x y -> TxUTxODiff x y

instance
  ( EraTxCert era
  , BabelEraScript era
  , EncCBOR (ContextError era)
  ) =>
  EncCBOR (BabelUtxosPredFailure era)
  where
  encCBOR =
    encode . \case
      ValidationTagMismatch v descr -> Sum ValidationTagMismatch 0 !> To v !> To descr
      CollectErrors cs -> Sum (CollectErrors @era) 1 !> To cs

instance
  ( EraTxCert era
  , BabelEraScript era
  , DecCBOR (ContextError era)
  ) =>
  DecCBOR (BabelUtxosPredFailure era)
  where
  decCBOR = decode (Summands "BabelUtxosPredicateFailure" dec)
    where
      dec 0 = SumD ValidationTagMismatch <! From <! From
      dec 1 = SumD (CollectErrors @era) <! From
      dec n = Invalid n

deriving stock instance
  ( BabelEraScript era
  , Show (TxCert era)
  , Show (ContextError era)
  , Show (UTxOState era)
  ) =>
  Show (BabelUtxosPredFailure era)

deriving stock instance
  ( BabelEraScript era
  , Eq (TxCert era)
  , Eq (ContextError era)
  , Eq (UTxOState era)
  ) =>
  Eq (BabelUtxosPredFailure era)

instance
  ( BabelEraScript era
  , NoThunks (TxCert era)
  , NoThunks (ContextError era)
  , NoThunks (UTxOState era)
  ) =>
  NoThunks (BabelUtxosPredFailure era)

instance
  ( BabelEraScript era
  , NFData (TxCert era)
  , NFData (ContextError era)
  , NFData (UTxOState era)
  ) =>
  NFData (BabelUtxosPredFailure era)

instance
  ( AlonzoEraTx era
  , AlonzoEraUTxO era
  , BabelEraTxBody era
  , ConwayEraPParams era
  , EraGov era
  , EraPlutusContext era
  , GovState era ~ ConwayGovState era
  , ScriptsNeeded era ~ AlonzoScriptsNeeded era
  , Signal (BabelUTXOS era) ~ Tx era
  , EraRule "UTXOS" era ~ BabelUTXOS era
  , InjectRuleFailure "UTXOS" AlonzoUtxosPredFailure era
  , InjectRuleEvent "UTXOS" AlonzoUtxosEvent era
  , InjectRuleEvent "UTXOS" BabelUtxosEvent era
  ) =>
  STS (BabelUTXOS era)
  where
  type BaseM (BabelUTXOS era) = Cardano.Ledger.BaseTypes.ShelleyBase
  type Environment (BabelUTXOS era) = UtxoEnv era
  type State (BabelUTXOS era) = UTxOStateTemp era
  type Signal (BabelUTXOS era) = AlonzoTx era
  type PredicateFailure (BabelUTXOS era) = BabelUtxosPredFailure era
  type Event (BabelUTXOS era) = BabelUtxosEvent era

  transitionRules = [utxosTransition]

instance
  ( AlonzoEraTx era
  , AlonzoEraUTxO era
  , BabelEraTxBody era
  , ConwayEraPParams era
  , EraGov era
  , EraPlutusContext era
  , GovState era ~ ConwayGovState era
  , PredicateFailure (EraRule "UTXOS" era) ~ BabelUtxosPredFailure era
  , ScriptsNeeded era ~ AlonzoScriptsNeeded era
  , Signal (BabelUTXOS era) ~ Tx era
  , EraRule "UTXOS" era ~ BabelUTXOS era
  , InjectRuleFailure "UTXOS" AlonzoUtxosPredFailure era
  , InjectRuleEvent "UTXOS" AlonzoUtxosEvent era
  , InjectRuleEvent "UTXOS" BabelUtxosEvent era
  ) =>
  Embed (BabelUTXOS era) (BabbageUTXO era)
  where
  wrapFailed = AlonzoInBabbageUtxoPredFailure . UtxosFailure
  wrapEvent = UtxosEvent

{- CIP-0118#UTXOS-rule

Everything interesting here is in the `updateUTxOState` function.

However, it's not actually all that interesting; what we do with the FRxO is identical
to what we already do with the UTxO.

Note that I'm unsure if we actually need to do anything with `deletedFrxO`, and so
the `txFrxODiffEvent` argument, and the event it uses, `TxFRxODiff`, might be redundant.
I've included these elements to keep parity with the UTxO logic. -}
utxosTransition ::
  forall era.
  ( AlonzoEraTx era
  , AlonzoEraUTxO era
  , BabelEraTxBody era
  , EraPlutusContext era
  , ScriptsNeeded era ~ AlonzoScriptsNeeded era
  , Signal (EraRule "UTXOS" era) ~ Tx era
  , STS (EraRule "UTXOS" era)
  , Environment (EraRule "UTXOS" era) ~ UtxoEnv era
  , State (EraRule "UTXOS" era) ~ UTxOStateTemp era
  , InjectRuleFailure "UTXOS" AlonzoUtxosPredFailure era
  , BaseM (EraRule "UTXOS" era) ~ ShelleyBase
  , InjectRuleEvent "UTXOS" AlonzoUtxosEvent era
  , InjectRuleEvent "UTXOS" BabelUtxosEvent era
  , Event (EraRule "UTXOS" era) ~ BabelUtxosEvent era
  , PredicateFailure (EraRule "UTXOS" era) ~ BabelUtxosPredFailure era
  ) =>
  TransitionRule (EraRule "UTXOS" era)
utxosTransition =
  judgmentContext >>= \(TRC (_, _, tx)) -> do
    case tx ^. isValidTxL of
      IsValid True -> babelEvalScriptsTxValid
      IsValid False -> babelEvalScriptsTxInvalid

babelEvalScriptsTxValid ::
  forall era.
  ( AlonzoEraTx era
  , AlonzoEraUTxO era
  , BabelEraTxBody era
  , EraPlutusContext era
  , ScriptsNeeded era ~ AlonzoScriptsNeeded era
  , Signal (EraRule "UTXOS" era) ~ Tx era
  , STS (EraRule "UTXOS" era)
  , State (EraRule "UTXOS" era) ~ UTxOStateTemp era
  , Environment (EraRule "UTXOS" era) ~ UtxoEnv era
  , InjectRuleFailure "UTXOS" AlonzoUtxosPredFailure era
  , BaseM (EraRule "UTXOS" era) ~ ShelleyBase
  , InjectRuleEvent "UTXOS" AlonzoUtxosEvent era
  , InjectRuleEvent "UTXOS" BabelUtxosEvent era
  ) =>
  TransitionRule (EraRule "UTXOS" era)
babelEvalScriptsTxValid = do
  TRC (UtxoEnv _ pp certState, utxos@(UTxOStateTemp utxo _frxo _ _ govState _ _), tx) <-
    judgmentContext
  let txBody = tx ^. bodyTxL

  () <- pure $! traceEvent validBegin ()
  expectScriptsToPass pp tx utxo
  () <- pure $! traceEvent validEnd ()

  utxos' <-
    updateUTxOState
      pp
      utxos
      txBody
      certState
      govState
      (tellEvent . injectEvent . TotalDeposits (hashAnnotated txBody))
      (\a b -> tellEvent . injectEvent $ TxUTxODiff a b)
      (\a b -> tellEvent . injectEvent $ TxFRxODiff a b)
  pure $! utxos' & utxostDonationL <>~ txBody ^. treasuryDonationTxBodyL

-- | This monadic action captures the final stages of the UTXO(S) rule. In particular it
-- applies all of the UTxO related aditions and removals, gathers all of the fees into the
-- fee pot `utxosFees` and updates the `utxosDeposited` field. Continuation supplied will
-- be called on the @deposit - refund@ change, which is normally used to emit the
-- `TotalDeposits` event.
updateUTxOState ::
  (BabelEraTxBody era, Monad m) =>
  PParams era ->
  UTxOStateTemp era ->
  TxBody era ->
  CertState era ->
  GovState era ->
  (Coin -> m ()) ->
  (UTxO era -> UTxO era -> m ()) ->
  (FRxO era -> FRxO era -> m ()) ->
  m (UTxOStateTemp era)
updateUTxOState pp utxos txBody certState govState depositChangeEvent txUtxODiffEvent txFrxODiffEvent = do
  let UTxOStateTemp
        { utxostUtxo
        , utxostFrxo
        , utxostDeposited
        , utxostFees
        , utxostStakeDistr
        , utxostDonation
        } = utxos
      UTxO utxo = utxostUtxo
      !utxoAdd = txouts txBody -- These will be inserted into the UTxO
      {- utxoDel  = txins txb ◁ utxo -}
      !(utxoWithout, utxoDel) = extractKeys utxo (txBody ^. inputsTxBodyL)
      {- newUTxO = (txins txb ⋪ utxo) ∪ outs txb -}
      newUTxO = utxoWithout `Map.union` unUTxO utxoAdd
      FRxO frxo = utxostFrxo
      !frxoAdd = txfrxo txBody -- These will be inserted into the FRxO
      {- utxoDel  = txins txb ◁ utxo -}
      !(frxoWithout, frxoDel) = extractKeys frxo (txBody ^. fulfillsTxBodyL)
      {- newUTxO = (txins txb ⋪ utxo) ∪ outs txb -}
      newFRxO = frxoWithout `Map.union` unFRxO frxoAdd
      deletedUTxO = UTxO utxoDel
      deletedFRxO = FRxO frxoDel
      newIncStakeDistro = updateStakeDistribution pp utxostStakeDistr deletedUTxO utxoAdd
      totalRefunds = certsTotalRefundsTxBody pp certState txBody
      totalDeposits = certsTotalDepositsTxBody pp certState txBody
      depositChange = totalDeposits <-> totalRefunds
  depositChangeEvent depositChange
  txUtxODiffEvent deletedUTxO utxoAdd
  txFrxODiffEvent deletedFRxO frxoAdd
  pure $!
    UTxOStateTemp
      { utxostUtxo = UTxO newUTxO
      , utxostFrxo = FRxO newFRxO
      , utxostDeposited = utxostDeposited <> depositChange
      , utxostFees = utxostFees <> txBody ^. feeTxBodyL
      , utxostGovState = govState
      , utxostStakeDistr = newIncStakeDistro
      , utxostDonation = utxostDonation
      }

babelEvalScriptsTxInvalid ::
  forall era.
  ( AlonzoEraTx era
  , EraPlutusContext era
  , AlonzoEraUTxO era
  , STS (EraRule "UTXOS" era)
  , Environment (EraRule "UTXOS" era) ~ UtxoEnv era
  , Signal (EraRule "UTXOS" era) ~ Tx era
  , State (EraRule "UTXOS" era) ~ UTxOStateTemp era
  , BaseM (EraRule "UTXOS" era) ~ ShelleyBase
  , Event (EraRule "UTXOS" era) ~ BabelUtxosEvent era
  , PredicateFailure (EraRule "UTXOS" era) ~ BabelUtxosPredFailure era
  , BabelEraTxBody era
  ) =>
  TransitionRule (EraRule "UTXOS" era)
babelEvalScriptsTxInvalid = do
  TRC (UtxoEnv _ pp _, us@(UTxOStateTemp utxo frxo _ fees _ _ _), tx) <- judgmentContext
  {- txb := txbody tx -}
  let txBody = tx ^. bodyTxL
  sysSt <- liftSTS $ asks systemStart
  ei <- liftSTS $ asks epochInfo

  () <- pure $! traceEvent invalidBegin ()

  case collectPlutusScriptsWithContextFrxo ei sysSt pp tx utxo frxo of
    Right sLst ->
      {- sLst := collectTwoPhaseScriptInputs pp tx utxo -}
      {- isValid tx = evalScripts tx sLst = False -}
      whenFailureFree $
        when2Phase $ case evalPlutusScripts tx sLst of
          Passes _ ->
            failBecause $
              ValidationTagMismatch (tx ^. isValidTxL) PassedUnexpectedly
          Fails ps fs -> do
            mapM_ (tellEvent . SuccessfulPlutusScriptsEvent @era) (nonEmpty ps)
            tellEvent (FailedPlutusScriptsEvent (scriptFailurePlutus <$> fs))
    Left info -> failBecause (CollectErrors info)

  () <- pure $! traceEvent invalidEnd ()

  {- utxoKeep = txBody ^. collateralInputsTxBodyL ⋪ utxo -}
  {- utxoDel  = txBody ^. collateralInputsTxBodyL ◁ utxo -}
  let !(utxoKeep, utxoDel) = extractKeys (unUTxO utxo) (txBody ^. collateralInputsTxBodyL)
      UTxO collouts = collOuts txBody
      DeltaCoin collateralFees = collAdaBalance txBody utxoDel -- NEW to Babbage
  pure $!
    us {- (collInputs txb ⋪ utxo) ∪ collouts tx -}
      { utxostUtxo = UTxO (Map.union utxoKeep collouts) -- NEW to Babbage
      {- fees + collateralFees -}
      , utxostFees = fees <> Coin collateralFees -- NEW to Babbage
      , utxostStakeDistr = updateStakeDistribution pp (utxostStakeDistr us) (UTxO utxoDel) (UTxO collouts)
      }

-- To Babel Fees implementers: This function is NOT in the right place.
-- Given more time, I'd do something with the EraUTxO class.
collectPlutusScriptsWithContextFrxo ::
  forall era.
  ( AlonzoEraTxWits era
  , AlonzoEraUTxO era
  , EraPlutusContext era
  , BabelEraTxBody era
  ) =>
  EpochInfo (Either Text) ->
  SystemStart ->
  PParams era ->
  Tx era ->
  UTxO era ->
  FRxO era ->
  Either [CollectError era] [PlutusWithContext (EraCrypto era)]
collectPlutusScriptsWithContextFrxo epochInfo sysStart pp tx utxo frxo =
  -- TODO: remove this whole complicated check when we get into Conway. It is much simpler
  -- to fail on a CostModel lookup in the `apply` function (already implemented).
  let missingCostModels = Set.filter (`Map.notMember` costModels) usedLanguages
   in case guard (protVerMajor < natVersion @9) >> Set.lookupMin missingCostModels of
        Just l -> Left [NoCostModel l]
        Nothing ->
          merge
            apply
            (map getScriptWithRedeemer neededPlutusScripts)
            (Right [])
  where
    -- Check on a protocol version to preserve failure mode (a single NoCostModel failure
    -- for languages with missing cost models) until we are in Conway era. After we hard
    -- fork into Conway it will be safe to remove this check together with the
    -- `missingCostModels` lookup
    --
    -- We also need to pass major protocol version to the script for script evaluation
    protVerMajor = pvMajor (pp ^. ppProtocolVersionL)
    costModels = costModelsValid $ pp ^. ppCostModelsL

    ScriptsProvided scriptsProvided = getScriptsProvided utxo tx <> getScriptsProvidedFrxo frxo tx
    AlonzoScriptsNeeded scriptsNeeded = getBabelScriptsNeededFrxo utxo frxo (tx ^. bodyTxL)
    neededPlutusScripts =
      mapMaybe (\(sp, sh) -> (,) (sh, sp) <$> lookupPlutusScript scriptsProvided sh) scriptsNeeded
    usedLanguages = Set.fromList $ map (plutusScriptLanguage . snd) neededPlutusScripts

    getScriptWithRedeemer ((plutusScriptHash, plutusPurpose), plutusScript) =
      let redeemerIndex = hoistPlutusPurpose toAsIx plutusPurpose
       in case lookupRedeemer redeemerIndex $ tx ^. witsTxL . rdmrsTxWitsL of
            Just (d, exUnits) -> Right (plutusScript, plutusPurpose, d, exUnits, plutusScriptHash)
            Nothing -> Left (NoRedeemer (hoistPlutusPurpose toAsItem plutusPurpose))
    apply (plutusScript, plutusPurpose, d, exUnits, plutusScriptHash) = do
      let lang = plutusScriptLanguage plutusScript
      costModel <- maybe (Left (NoCostModel lang)) Right $ Map.lookup lang costModels
      case mkPlutusScriptContext plutusScript plutusPurpose pp epochInfo sysStart utxo tx of
        Right scriptContext ->
          let spendingDatum = getSpendingDatum utxo tx $ hoistPlutusPurpose toAsItem plutusPurpose
              datums = maybe id (:) spendingDatum [d, scriptContext]
           in Right $
                withPlutusScript plutusScript $ \plutus ->
                  PlutusWithContext
                    { pwcProtocolVersion = protVerMajor
                    , pwcScript = Left plutus
                    , pwcScriptHash = plutusScriptHash
                    , pwcDatums = PlutusDatums (getPlutusData <$> datums)
                    , pwcExUnits = exUnits
                    , pwcCostModel = costModel
                    }
        Left te -> Left $ BadTranslation te
    merge :: forall t b a. (t -> Either a b) -> [Either a t] -> Either [a] [b] -> Either [a] [b]
    merge _f [] answer = answer
    merge f (x : xs) zs = merge f xs (gg x zs)
      where
        gg :: Either a t -> Either [a] [b] -> Either [a] [b]
        gg (Right t) (Right cs) =
          case f t of
            Right c -> Right $ c : cs
            Left e -> Left [e]
        gg (Left a) (Right _) = Left [a]
        gg (Right _) (Left cs) = Left cs
        gg (Left a) (Left cs) = Left (a : cs)