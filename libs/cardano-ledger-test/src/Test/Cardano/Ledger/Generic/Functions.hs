{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

-- | Functions in this module take a (Proof era) as their first
--   parameter and do something potentially different in each Era.
module Test.Cardano.Ledger.Generic.Functions where

import Cardano.Ledger.Address (Addr (..))
import Cardano.Ledger.Alonzo.Plutus.Context (EraPlutusContext, mkSupportedLanguageM)
import Cardano.Ledger.Alonzo.Scripts (plutusScriptLanguage)
import Cardano.Ledger.Alonzo.TxBody (AlonzoTxOut (..))
import Cardano.Ledger.Babbage.TxBody (BabbageTxOut (..))
import Cardano.Ledger.Babbage.UTxO (getReferenceScripts)
import Cardano.Ledger.BaseTypes (
  BlocksMade (BlocksMade),
  Globals (epochInfo),
 )
import Cardano.Ledger.Coin (Coin (..))
import Cardano.Ledger.Compactible (fromCompact)
import Cardano.Ledger.Conway.Core
import Cardano.Ledger.Conway.State
import Cardano.Ledger.Credential (Credential, StakeReference (..))
import Cardano.Ledger.Plutus.Data (Datum (..), binaryDataToData, hashData)
import Cardano.Ledger.Plutus.Language (Language (..))
import Cardano.Ledger.Shelley.AdaPots (AdaPots (..), totalAdaPotsES)
import Cardano.Ledger.Shelley.LedgerState (
  EpochState (..),
  LedgerState (..),
  NewEpochState (..),
  UTxOState (..),
 )
import Cardano.Ledger.Shelley.Scripts (pattern RequireAllOf, pattern RequireAnyOf)
import Cardano.Ledger.Shelley.TxOut (ShelleyTxOut (..))
import Cardano.Ledger.TxIn (TxIn (..))
import Cardano.Ledger.Val (Val ((<+>), (<->)), inject)
import Cardano.Slotting.EpochInfo.API (epochInfoSize)
import Control.Monad.Reader (runReader)
import Control.Monad.Trans.Fail.String (errorFail)
import Data.Default (Default (def))
import qualified Data.Foldable as Fold (fold, toList)
import qualified Data.List as List
import qualified Data.Map.Strict as Map
import Data.Maybe (maybeToList)
import Data.Maybe.Strict (StrictMaybe (..))
import Data.Set (Set)
import qualified Data.Set as Set
import Lens.Micro
import Numeric.Natural
import Test.Cardano.Ledger.Alonzo.Arbitrary (alwaysFailsLang, alwaysSucceedsLang)
import Test.Cardano.Ledger.Generic.ModelState (MUtxo, Model, ModelNewEpochState (..))
import Test.Cardano.Ledger.Generic.Proof (Proof (..), Reflect (..))
import Test.Cardano.Ledger.Shelley.Rewards (RewardUpdateOld, createRUpdOld_)
import Test.Cardano.Ledger.Shelley.Utils (testGlobals)

-- ====================================================================
-- Era agnostic actions on (PParams era) (TxOut era) and
-- other XX types Mostly by pattern matching against Proof objects

-- | Positive numbers are "deposits owed", negative amounts are "refunds gained"
depositsAndRefunds ::
  (EraAccounts era, EraPParams era, ShelleyEraTxCert era) =>
  PParams era ->
  [TxCert era] ->
  Accounts era ->
  Coin
depositsAndRefunds pp certificates accounts = List.foldl' accum (Coin 0) certificates
  where
    accum ans (RegTxCert _) = pp ^. ppKeyDepositL <+> ans
    accum ans (UnRegTxCert cred) =
      case lookupAccountState cred accounts of
        Nothing -> ans
        Just accountState -> ans <-> fromCompact (accountState ^. depositAccountStateL)
    accum ans (RegPoolTxCert _) = pp ^. ppPoolDepositL <+> ans
    accum ans (RetirePoolTxCert _ _) = ans -- The pool reward is refunded at the end of the epoch
    accum ans _ = ans

-- | Compute the set of ScriptHashes for which there should be ScriptWitnesses. In Babbage
--  Era and later, where inline Scripts are allowed, they should not appear in this set.
scriptWitsNeeded' :: Proof era -> MUtxo era -> TxBody TopTx era -> Set ScriptHash
scriptWitsNeeded' Conway utxo txBody = regularScripts `Set.difference` inlineScripts
  where
    theUtxo = UTxO utxo
    inputs = txBody ^. inputsTxBodyL
    inlineScripts = Map.keysSet $ getReferenceScripts theUtxo inputs
    regularScripts = getScriptsHashesNeeded (getScriptsNeeded theUtxo txBody)
scriptWitsNeeded' Babbage utxo txBody = regularScripts `Set.difference` inlineScripts
  where
    theUtxo = UTxO utxo
    inputs = (txBody ^. inputsTxBodyL) `Set.union` (txBody ^. referenceInputsTxBodyL)
    inlineScripts = Map.keysSet $ getReferenceScripts theUtxo inputs
    regularScripts = getScriptsHashesNeeded (getScriptsNeeded theUtxo txBody)
scriptWitsNeeded' Alonzo utxo txBody =
  getScriptsHashesNeeded (getScriptsNeeded (UTxO utxo) txBody)
scriptWitsNeeded' Mary utxo txBody =
  getScriptsHashesNeeded (getScriptsNeeded (UTxO utxo) txBody)
scriptWitsNeeded' Allegra utxo txBody =
  getScriptsHashesNeeded (getScriptsNeeded (UTxO utxo) txBody)
scriptWitsNeeded' Shelley utxo txBody =
  getScriptsHashesNeeded (getScriptsNeeded (UTxO utxo) txBody)
{-# NOINLINE scriptWitsNeeded' #-}

scriptsNeeded' :: EraUTxO era => MUtxo era -> TxBody TopTx era -> Set ScriptHash
scriptsNeeded' utxo txBody =
  getScriptsHashesNeeded (getScriptsNeeded (UTxO utxo) txBody)
{-# NOINLINE scriptsNeeded' #-}

txInBalance ::
  forall era.
  EraTxOut era =>
  Set TxIn ->
  MUtxo era ->
  Coin
txInBalance txinSet m = sumCoinUTxO (UTxO (Map.restrictKeys m txinSet))

injectFee :: EraTxOut era => Coin -> TxOut era -> TxOut era
injectFee fee txOut = txOut & valueTxOutL %~ (<+> inject fee)

getTxOutRefScript :: Proof era -> TxOut era -> StrictMaybe (Script era)
getTxOutRefScript Conway (BabbageTxOut _ _ _ ms) = ms
getTxOutRefScript Babbage (BabbageTxOut _ _ _ ms) = ms
getTxOutRefScript _ _ = SNothing
{-# NOINLINE getTxOutRefScript #-}

emptyPPUPstate :: forall era. Proof era -> ShelleyGovState era
emptyPPUPstate Conway = def
emptyPPUPstate Babbage = def
emptyPPUPstate Alonzo = def
emptyPPUPstate Mary = def
emptyPPUPstate Allegra = def
emptyPPUPstate Shelley = def
{-# NOINLINE emptyPPUPstate #-}

maxRefInputs :: Proof era -> Int
maxRefInputs Babbage = 3
maxRefInputs _ = 0

isValid' :: Proof era -> Tx TopTx era -> IsValid
isValid' Conway x = x ^. isValidTxL
isValid' Babbage x = x ^. isValidTxL
isValid' Alonzo x = x ^. isValidTxL
isValid' _ _ = IsValid True
{-# NOINLINE isValid' #-}

-- | Does the TxOut have evidence of credentials and data.
--   Evidence of data is either ScriptHash or (in Babbage) an inline Datum
--   Evidence of credentials can come from the Addr
txoutEvidence ::
  forall era.
  Proof era ->
  TxOut era ->
  ([Credential 'Payment], Maybe DataHash)
txoutEvidence Alonzo (AlonzoTxOut addr _ (SJust dh)) =
  (addrCredentials addr, Just dh)
txoutEvidence Alonzo (AlonzoTxOut addr _ SNothing) =
  (addrCredentials addr, Nothing)
txoutEvidence Conway (BabbageTxOut addr _ NoDatum _) =
  (addrCredentials addr, Nothing)
txoutEvidence Conway (BabbageTxOut addr _ (DatumHash dh) _) =
  (addrCredentials addr, Just dh)
txoutEvidence Conway (BabbageTxOut addr _ (Datum _d) _) =
  (addrCredentials addr, Just (hashData @era (binaryDataToData _d)))
txoutEvidence Babbage (BabbageTxOut addr _ NoDatum _) =
  (addrCredentials addr, Nothing)
txoutEvidence Babbage (BabbageTxOut addr _ (DatumHash dh) _) =
  (addrCredentials addr, Just dh)
txoutEvidence Babbage (BabbageTxOut addr _ (Datum _d) _) =
  (addrCredentials addr, Just (hashData @era (binaryDataToData _d)))
txoutEvidence Mary (ShelleyTxOut addr _) =
  (addrCredentials addr, Nothing)
txoutEvidence Allegra (ShelleyTxOut addr _) =
  (addrCredentials addr, Nothing)
txoutEvidence Shelley (ShelleyTxOut addr _) =
  (addrCredentials addr, Nothing)
{-# NOINLINE txoutEvidence #-}

addrCredentials :: Addr -> [Credential 'Payment]
addrCredentials addr = maybeToList (paymentCredAddr addr)

paymentCredAddr :: Addr -> Maybe (Credential 'Payment)
paymentCredAddr (Addr _ cred _) = Just cred
paymentCredAddr _ = Nothing

stakeCredAddr :: Addr -> Maybe (Credential 'Staking)
stakeCredAddr (Addr _ _ (StakeRefBase cred)) = Just cred
stakeCredAddr _ = Nothing

getBody :: EraTx era => Proof era -> Tx TopTx era -> TxBody TopTx era
getBody _ tx = tx ^. bodyTxL

getCollateralInputs :: Proof era -> TxBody TopTx era -> Set TxIn
getCollateralInputs Conway txBody = txBody ^. collateralInputsTxBodyL
getCollateralInputs Babbage txBody = txBody ^. collateralInputsTxBodyL
getCollateralInputs Alonzo txBody = txBody ^. collateralInputsTxBodyL
getCollateralInputs Mary _ = Set.empty
getCollateralInputs Allegra _ = Set.empty
getCollateralInputs Shelley _ = Set.empty
{-# NOINLINE getCollateralInputs #-}

getCollateralOutputs :: Proof era -> TxBody TopTx era -> [TxOut era]
getCollateralOutputs Conway txBody =
  case txBody ^. collateralReturnTxBodyL of
    SNothing -> []
    SJust x -> [x]
getCollateralOutputs Babbage txBody =
  case txBody ^. collateralReturnTxBodyL of
    SNothing -> []
    SJust x -> [x]
getCollateralOutputs Alonzo _ = []
getCollateralOutputs Mary _ = []
getCollateralOutputs Allegra _ = []
getCollateralOutputs Shelley _ = []
{-# NOINLINE getCollateralOutputs #-}

alwaysSucceedsLang' :: forall era. EraPlutusContext era => Language -> Natural -> Script era
alwaysSucceedsLang' l =
  fromPlutusScript . alwaysSucceedsLang (errorFail (mkSupportedLanguageM @era l))

alwaysFailsLang' :: forall era. EraPlutusContext era => Language -> Natural -> Script era
alwaysFailsLang' l =
  fromPlutusScript . alwaysFailsLang (errorFail (mkSupportedLanguageM @era l))

alwaysTrue :: forall era. EraPlutusContext era => Maybe Language -> Natural -> Script era
alwaysTrue (Just l) n = alwaysSucceedsLang' @era l n
alwaysTrue Nothing _ = fromNativeScript $ RequireAllOf mempty
{-# NOINLINE alwaysTrue #-}

alwaysFalse :: forall era. EraPlutusContext era => Maybe Language -> Natural -> Script era
alwaysFalse (Just l) n = alwaysFailsLang' @era l n
alwaysFalse Nothing _ = fromNativeScript $ RequireAnyOf mempty
{-# NOINLINE alwaysFalse #-}

certs :: (ShelleyEraTxBody era, EraTx era) => Proof era -> Tx TopTx era -> [TxCert era]
certs _ tx = Fold.toList $ tx ^. bodyTxL . certsTxBodyL

-- | Create an old style RewardUpdate to be used in tests, in any Era.
createRUpdNonPulsing' ::
  forall era.
  ( EraPParams era
  , EraAccounts era
  ) =>
  Model era ->
  RewardUpdateOld
createRUpdNonPulsing' model =
  let bm = BlocksMade $ mBcur model -- TODO or should this be mBprev?
      ss = mSnapshots model
      as = mChainAccountState model
      reserves = casReserves as
      pp = mPParams model
      totalStake = fromCompact $ foldMap (^. balanceAccountStateL) (mAccounts model ^. accountsMapL)
      rs = Map.keysSet (mAccounts model ^. accountsMapL) -- TODO or should we look at delegated keys instead?
      en = mEL model

      -- We use testGlobals here, since this generic function is used only in tests.
      slotsPerEpoch = case epochInfoSize (epochInfo testGlobals) en of
        Left err -> error ("Failed to calculate slots per epoch:\n" ++ show err)
        Right x -> x
   in (`runReader` testGlobals) $
        createRUpdOld_ @era slotsPerEpoch bm ss reserves pp totalStake rs def
{-# NOINLINE createRUpdNonPulsing' #-}

languagesUsed ::
  forall era.
  Proof era ->
  Tx TopTx era ->
  UTxO era ->
  Set ScriptHash ->
  Set Language
languagesUsed proof tx utxo sNeeded = case proof of
  Shelley -> Set.empty
  Allegra -> Set.empty
  Mary -> Set.empty
  Alonzo -> languages tx utxo sNeeded
  Babbage -> languages tx utxo sNeeded
  Conway -> languages tx utxo sNeeded
{-# NOINLINE languagesUsed #-}

-- | Compute the Set of Languages in an era, where 'AlonzoScripts' are used
languages ::
  forall era.
  (EraUTxO era, AlonzoEraScript era) =>
  Tx TopTx era ->
  UTxO era ->
  Set ScriptHash ->
  Set Language
languages tx utxo sNeeded = Map.foldl' accum Set.empty allScripts
  where
    allScripts = Map.restrictKeys (unScriptsProvided $ getScriptsProvided utxo tx) sNeeded
    accum ans script =
      case toPlutusScript script of
        Nothing -> ans
        Just plutusScript -> Set.insert (plutusScriptLanguage plutusScript) ans

-- | Compute the total Ada from Ada pots within 't'
class TotalAda t where
  totalAda :: t -> Coin

instance TotalAda ChainAccountState where
  totalAda (ChainAccountState treasury reserves) = treasury <+> reserves

instance Reflect era => TotalAda (UTxOState era) where
  totalAda (UTxOState utxo _deposits fees gs _ donations) =
    totalAda utxo <+> fees <+> govStateTotalAda gs <+> donations

-- we don't add in the _deposits, because it is invariant that this
-- is equal to the sum of the key deposit map and the pool deposit map
-- So these are accounted for in the instance (TotalAda (CertState era))
-- TODO I'm not sure this is true ^
-- Imp conformance tests show in logs that totalAda is off by the deposit amount

instance Reflect era => TotalAda (UTxO era) where
  totalAda (UTxO m) = foldl accum mempty m
    where
      accum ans txOut = (txOut ^. coinTxOutL) <+> ans

instance EraAccounts era => TotalAda (DState era) where
  totalAda dState =
    fromCompact $
      foldMap (\as -> (as ^. balanceAccountStateL) <> (as ^. depositAccountStateL)) $
        dState ^. accountsL . accountsMapL

instance TotalAda (PState era) where
  totalAda pstate = Fold.fold (fromCompact . spsDeposit <$> psStakePools pstate)

instance TotalAda (VState era) where
  totalAda _ = mempty

instance EraAccounts era => TotalAda (ShelleyCertState era) where
  totalAda (ShelleyCertState ps ds) = totalAda ds <> totalAda ps

instance TotalAda (ShelleyGovState era) where
  totalAda _ = mempty

govStateTotalAda :: forall era. Reflect era => GovState era -> Coin
govStateTotalAda = case reify @era of
  Shelley -> totalAda
  Mary -> totalAda
  Allegra -> totalAda
  Alonzo -> totalAda
  Babbage -> totalAda
  Conway -> mempty

certStateTotalAda :: forall era. Reflect era => CertState era -> Coin
certStateTotalAda = case reify @era of
  Shelley -> totalAda
  Mary -> totalAda
  Allegra -> totalAda
  Alonzo -> totalAda
  Babbage -> totalAda
  Conway -> mempty

instance Reflect era => TotalAda (LedgerState era) where
  totalAda (LedgerState utxos dps) = totalAda utxos <+> certStateTotalAda dps

instance Reflect era => TotalAda (EpochState era) where
  totalAda eps = totalAda (esLState eps) <+> totalAda (esChainAccountState eps)

instance Reflect era => TotalAda (NewEpochState era) where
  totalAda nes = totalAda (nesEs nes)

adaPots :: Proof era -> EpochState era -> AdaPots
adaPots Conway es = totalAdaPotsES es
adaPots Babbage es = totalAdaPotsES es
adaPots Alonzo es = totalAdaPotsES es
adaPots Mary es = totalAdaPotsES es
adaPots Allegra es = totalAdaPotsES es
adaPots Shelley es = totalAdaPotsES es
{-# NOINLINE adaPots #-}
