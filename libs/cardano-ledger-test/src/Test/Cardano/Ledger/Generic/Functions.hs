{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

-- | Functions in this module take a (Proof era) as their first
--   parameter and do something potentially different in each Era.
module Test.Cardano.Ledger.Generic.Functions where

import Cardano.Ledger.Address (Addr (..))
import Cardano.Ledger.Alonzo.Scripts (plutusScriptLanguage)
import Cardano.Ledger.Alonzo.Tx (AlonzoTx (..), IsValid (..))
import Cardano.Ledger.Alonzo.TxBody (AlonzoTxOut (..), collateral')
import Cardano.Ledger.Babbage.TxBody (
  BabbageTxOut (..),
  collateralInputs',
  collateralReturn',
  referenceInputs',
  spendInputs',
 )
import Cardano.Ledger.Babbage.UTxO (getReferenceScripts)
import Cardano.Ledger.BaseTypes (
  BlocksMade (BlocksMade),
  Globals (epochInfo),
  ProtVer (..),
  natVersion,
 )
import Cardano.Ledger.Coin (Coin (..))
import Cardano.Ledger.Conway.Core
import Cardano.Ledger.Credential (Credential, StakeReference (..))
import Cardano.Ledger.Keys (KeyRole (..))
import Cardano.Ledger.Plutus.Data (Datum (..), binaryDataToData, hashData)
import Cardano.Ledger.Plutus.ExUnits (ExUnits (..))
import Cardano.Ledger.Plutus.Language (Language (..))
import Cardano.Ledger.Shelley.AdaPots (AdaPots (..), totalAdaPotsES)
import Cardano.Ledger.Shelley.LedgerState (
  AccountState (..),
  CertState (..),
  DState (..),
  EpochState (..),
  LedgerState (..),
  NewEpochState (..),
  PState (..),
  UTxOState (..),
  VState (..),
 )
import Cardano.Ledger.Shelley.TxOut (ShelleyTxOut (..))
import Cardano.Ledger.TxIn (TxIn (..))
import qualified Cardano.Ledger.UMap as UM
import Cardano.Ledger.UTxO (EraUTxO (..), UTxO (..), coinBalance, unScriptsProvided)
import Cardano.Ledger.Val (Val ((<+>), (<->)), inject)
import Cardano.Slotting.EpochInfo.API (epochInfoSize)
import Control.Monad.Reader (runReader)
import Data.Default.Class (Default (def))
import qualified Data.Foldable as Fold (fold, toList)
import qualified Data.List as List
import Data.Map (Map, keysSet, restrictKeys)
import qualified Data.Map.Strict as Map
import Data.Maybe.Strict (StrictMaybe (..))
import Data.Sequence.Strict (StrictSeq)
import Data.Set (Set)
import qualified Data.Set as Set
import Lens.Micro
import Numeric.Natural
import Test.Cardano.Ledger.Alonzo.Arbitrary (alwaysFailsLang, alwaysSucceedsLang)
import Test.Cardano.Ledger.Generic.Fields (TxOutField (..))
import Test.Cardano.Ledger.Generic.ModelState (MUtxo, Model, ModelNewEpochState (..))
import Test.Cardano.Ledger.Generic.Proof (Proof (..), Reflect (..))
import Test.Cardano.Ledger.Generic.Scriptic (Scriptic (..))
import Test.Cardano.Ledger.Shelley.Rewards (RewardUpdateOld, createRUpdOld_)
import Test.Cardano.Ledger.Shelley.Utils (testGlobals)

-- ====================================================================
-- Era agnostic actions on (PParams era) (TxOut era) and
-- other XX types Mostly by pattern matching against Proof objects

maxCollateralInputs' :: Proof era -> PParams era -> Natural
maxCollateralInputs' Alonzo pp = pp ^. ppMaxCollateralInputsL
maxCollateralInputs' Babbage pp = pp ^. ppMaxCollateralInputsL
maxCollateralInputs' Conway pp = pp ^. ppMaxCollateralInputsL
maxCollateralInputs' _proof _pp = 0
{-# NOINLINE maxCollateralInputs' #-}

maxTxExUnits' :: Proof era -> PParams era -> ExUnits
maxTxExUnits' Alonzo pp = pp ^. ppMaxTxExUnitsL
maxTxExUnits' Babbage pp = pp ^. ppMaxTxExUnitsL
maxTxExUnits' Conway pp = pp ^. ppMaxTxExUnitsL
maxTxExUnits' _proof _x = mempty
{-# NOINLINE maxTxExUnits' #-}

collateralPercentage' :: Proof era -> PParams era -> Natural
collateralPercentage' Alonzo pp = pp ^. ppCollateralPercentageL
collateralPercentage' Babbage pp = pp ^. ppCollateralPercentageL
collateralPercentage' Conway pp = pp ^. ppCollateralPercentageL
collateralPercentage' _proof _pp = 0
{-# NOINLINE collateralPercentage' #-}

protocolVersion :: Proof era -> ProtVer
protocolVersion Conway = ProtVer (natVersion @9) 0
protocolVersion Babbage = ProtVer (natVersion @7) 0
protocolVersion Alonzo = ProtVer (natVersion @6) 0
protocolVersion Mary = ProtVer (natVersion @4) 0
protocolVersion Allegra = ProtVer (natVersion @3) 0
protocolVersion Shelley = ProtVer (natVersion @2) 0
{-# NOINLINE protocolVersion #-}

-- | Positive numbers are "deposits owed", negative amounts are "refunds gained"
depositsAndRefunds ::
  (EraPParams era, ShelleyEraTxCert era) =>
  PParams era ->
  [TxCert era] ->
  Map (Credential 'Staking (EraCrypto era)) Coin ->
  Coin
depositsAndRefunds pp certificates keydeposits = List.foldl' accum (Coin 0) certificates
  where
    accum ans (RegTxCert _) = pp ^. ppKeyDepositL <+> ans
    accum ans (UnRegTxCert hk) =
      case Map.lookup hk keydeposits of
        Nothing -> ans
        Just c -> ans <-> c
    accum ans (RegPoolTxCert _) = pp ^. ppPoolDepositL <+> ans
    accum ans (RetirePoolTxCert _ _) = ans -- The pool reward is refunded at the end of the epoch
    accum ans _ = ans

-- | Compute the set of ScriptHashes for which there should be ScriptWitnesses. In Babbage
--  Era and later, where inline Scripts are allowed, they should not appear in this set.
scriptWitsNeeded' :: Proof era -> MUtxo era -> TxBody era -> Set (ScriptHash (EraCrypto era))
scriptWitsNeeded' Conway utxo txBody = regularScripts `Set.difference` inlineScripts
  where
    theUtxo = UTxO utxo
    inputs = txBody ^. inputsTxBodyL
    inlineScripts = keysSet $ getReferenceScripts theUtxo inputs
    regularScripts = getScriptsHashesNeeded (getScriptsNeeded theUtxo txBody)
scriptWitsNeeded' Babbage utxo txBody = regularScripts `Set.difference` inlineScripts
  where
    theUtxo = UTxO utxo
    inputs = spendInputs' txBody `Set.union` referenceInputs' txBody
    inlineScripts = keysSet $ getReferenceScripts theUtxo inputs
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

scriptsNeeded' :: Proof era -> MUtxo era -> TxBody era -> Set (ScriptHash (EraCrypto era))
scriptsNeeded' Conway utxo txBody =
  getScriptsHashesNeeded (getScriptsNeeded (UTxO utxo) txBody)
scriptsNeeded' Babbage utxo txBody =
  getScriptsHashesNeeded (getScriptsNeeded (UTxO utxo) txBody)
scriptsNeeded' Alonzo utxo txBody =
  getScriptsHashesNeeded (getScriptsNeeded (UTxO utxo) txBody)
scriptsNeeded' Mary utxo txBody =
  getScriptsHashesNeeded (getScriptsNeeded (UTxO utxo) txBody)
scriptsNeeded' Allegra utxo txBody =
  getScriptsHashesNeeded (getScriptsNeeded (UTxO utxo) txBody)
scriptsNeeded' Shelley utxo txBody =
  getScriptsHashesNeeded (getScriptsNeeded (UTxO utxo) txBody)
{-# NOINLINE scriptsNeeded' #-}

txInBalance ::
  forall era.
  EraTxOut era =>
  Set (TxIn (EraCrypto era)) ->
  MUtxo era ->
  Coin
txInBalance txinSet m = coinBalance (UTxO (restrictKeys m txinSet))

-- | Break a TxOut into its mandatory and optional parts
txoutFields :: Proof era -> TxOut era -> (Addr (EraCrypto era), Value era, [TxOutField era])
txoutFields Conway (BabbageTxOut addr val d h) = (addr, val, [FDatum d, RefScript h])
txoutFields Babbage (BabbageTxOut addr val d h) = (addr, val, [FDatum d, RefScript h])
txoutFields Alonzo (AlonzoTxOut addr val dh) = (addr, val, [DHash dh])
txoutFields Mary (ShelleyTxOut addr val) = (addr, val, [])
txoutFields Allegra (ShelleyTxOut addr val) = (addr, val, [])
txoutFields Shelley (ShelleyTxOut addr val) = (addr, val, [])
{-# NOINLINE txoutFields #-}

injectFee :: EraTxOut era => Proof era -> Coin -> TxOut era -> TxOut era
injectFee _ fee txOut = txOut & valueTxOutL %~ (<+> inject fee)

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

isValid' :: Proof era -> Tx era -> IsValid
isValid' Conway x = isValid x
isValid' Babbage x = isValid x
isValid' Alonzo x = isValid x
isValid' _ _ = IsValid True
{-# NOINLINE isValid' #-}

-- | Does the TxOut have evidence of credentials and data.
--   Evidence of data is either ScriptHash or (in Babbage) an inline Datum
--   Evidence of credentials can come from the Addr
txoutEvidence ::
  forall era.
  Proof era ->
  TxOut era ->
  ([Credential 'Payment (EraCrypto era)], Maybe (DataHash (EraCrypto era)))
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

addrCredentials :: Addr c -> [Credential 'Payment c]
addrCredentials addr = maybe [] (: []) (paymentCredAddr addr)

paymentCredAddr :: Addr c -> Maybe (Credential 'Payment c)
paymentCredAddr (Addr _ cred _) = Just cred
paymentCredAddr _ = Nothing

stakeCredAddr :: Addr c -> Maybe (Credential 'Staking c)
stakeCredAddr (Addr _ _ (StakeRefBase cred)) = Just cred
stakeCredAddr _ = Nothing

getBody :: EraTx era => Proof era -> Tx era -> TxBody era
getBody _ tx = tx ^. bodyTxL

getCollateralInputs :: Proof era -> TxBody era -> Set (TxIn (EraCrypto era))
getCollateralInputs Conway tx = tx ^. collateralInputsTxBodyL
getCollateralInputs Babbage tx = collateralInputs' tx
getCollateralInputs Alonzo tx = collateral' tx
getCollateralInputs Mary _ = Set.empty
getCollateralInputs Allegra _ = Set.empty
getCollateralInputs Shelley _ = Set.empty
{-# NOINLINE getCollateralInputs #-}

getCollateralOutputs :: Proof era -> TxBody era -> [TxOut era]
getCollateralOutputs Conway tx = case tx ^. collateralReturnTxBodyL of SNothing -> []; SJust x -> [x]
getCollateralOutputs Babbage tx = case collateralReturn' tx of SNothing -> []; SJust x -> [x]
getCollateralOutputs Alonzo _ = []
getCollateralOutputs Mary _ = []
getCollateralOutputs Allegra _ = []
getCollateralOutputs Shelley _ = []
{-# NOINLINE getCollateralOutputs #-}

getInputs :: EraTxBody era => Proof era -> TxBody era -> Set (TxIn (EraCrypto era))
getInputs _ tx = tx ^. inputsTxBodyL

getOutputs :: EraTxBody era => Proof era -> TxBody era -> StrictSeq (TxOut era)
getOutputs _ tx = tx ^. outputsTxBodyL

getScriptWits ::
  EraTxWits era => Proof era -> TxWits era -> Map (ScriptHash (EraCrypto era)) (Script era)
getScriptWits _ tx = tx ^. scriptTxWitsL

allInputs :: EraTxBody era => Proof era -> TxBody era -> Set (TxIn (EraCrypto era))
allInputs _ txb = txb ^. allInputsTxBodyF

getWitnesses :: EraTx era => Proof era -> Tx era -> TxWits era
getWitnesses _ tx = tx ^. witsTxL

primaryLanguage :: Proof era -> Maybe Language
primaryLanguage Conway = Just PlutusV2
primaryLanguage Babbage = Just PlutusV2
primaryLanguage Alonzo = Just PlutusV1
primaryLanguage _ = Nothing
{-# NOINLINE primaryLanguage #-}

alwaysTrue :: forall era. Proof era -> Maybe Language -> Natural -> Script era
alwaysTrue Conway (Just l) n = alwaysSucceedsLang @era l n
alwaysTrue p@Conway Nothing _ = fromNativeScript $ allOf [] p
alwaysTrue Babbage (Just l) n = alwaysSucceedsLang @era l n
alwaysTrue p@Babbage Nothing _ = fromNativeScript $ allOf [] p
alwaysTrue Alonzo (Just l) n = alwaysSucceedsLang @era l n
alwaysTrue p@Alonzo Nothing _ = fromNativeScript $ allOf [] p
alwaysTrue p@Mary _ n = always n p
alwaysTrue p@Allegra _ n = always n p
alwaysTrue p@Shelley _ n = always n p
{-# NOINLINE alwaysTrue #-}

alwaysFalse :: forall era. Proof era -> Maybe Language -> Natural -> Script era
alwaysFalse Conway (Just l) n = alwaysFailsLang @era l n
alwaysFalse p@Conway Nothing _ = fromNativeScript $ anyOf [] p
alwaysFalse Babbage (Just l) n = alwaysFailsLang @era l n
alwaysFalse p@Babbage Nothing _ = fromNativeScript $ anyOf [] p
alwaysFalse Alonzo (Just l) n = alwaysFailsLang @era l n
alwaysFalse p@Alonzo Nothing _ = fromNativeScript $ anyOf [] p
alwaysFalse p@Mary _ n = never n p
alwaysFalse p@Allegra _ n = never n p
alwaysFalse p@Shelley _ n = never n p
{-# NOINLINE alwaysFalse #-}

certs :: (ShelleyEraTxBody era, EraTx era) => Proof era -> Tx era -> [TxCert era]
certs _ tx = Fold.toList $ tx ^. bodyTxL . certsTxBodyL

-- | Create an old style RewardUpdate to be used in tests, in any Era.
createRUpdNonPulsing' ::
  forall era.
  Proof era ->
  Model era ->
  RewardUpdateOld (EraCrypto era)
createRUpdNonPulsing' proof model =
  let bm = BlocksMade $ mBcur model -- TODO or should this be mBprev?
      ss = mSnapshots model
      as = mAccountState model
      reserves = asReserves as
      pp = mPParams model
      totalStake = Map.foldr (<+>) (Coin 0) $ mRewards model
      rs = Map.keysSet $ mRewards model -- TODO or should we look at delegated keys instead?
      en = mEL model

      -- We use testGlobals here, since this generic function is used only in tests.
      slotsPerEpoch = case epochInfoSize (epochInfo testGlobals) en of
        Left err -> error ("Failed to calculate slots per epoch:\n" ++ show err)
        Right x -> x
   in (`runReader` testGlobals) $ case proof of
        Conway -> createRUpdOld_ @era slotsPerEpoch bm ss reserves pp totalStake rs def
        Babbage -> createRUpdOld_ @era slotsPerEpoch bm ss reserves pp totalStake rs def
        Alonzo -> createRUpdOld_ @era slotsPerEpoch bm ss reserves pp totalStake rs def
        Mary -> createRUpdOld_ @era slotsPerEpoch bm ss reserves pp totalStake rs def
        Allegra -> createRUpdOld_ @era slotsPerEpoch bm ss reserves pp totalStake rs def
        Shelley -> createRUpdOld_ @era slotsPerEpoch bm ss reserves pp totalStake rs def
{-# NOINLINE createRUpdNonPulsing' #-}

languagesUsed ::
  forall era.
  Proof era ->
  Tx era ->
  UTxO era ->
  Set (ScriptHash (EraCrypto era)) ->
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
  Tx era ->
  UTxO era ->
  Set (ScriptHash (EraCrypto era)) ->
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

instance TotalAda AccountState where
  totalAda (AccountState treasury reserves) = treasury <+> reserves

instance Reflect era => TotalAda (UTxOState era) where
  -- TODO WG don't need to do anything with frxo here right?
  totalAda (UTxOState utxo _ _deposits fees gs _ donations) =
    totalAda utxo <+> fees <+> govStateTotalAda gs <+> donations

-- we don't add in the _deposits, because it is invariant that this
-- is equal to the sum of the key deposit map and the pool deposit map
-- So these are accounted for in the instance (TotalAda (CertState era))

instance Reflect era => TotalAda (UTxO era) where
  totalAda (UTxO m) = foldl accum mempty m
    where
      accum ans txOut = (txOut ^. coinTxOutL) <+> ans

instance TotalAda (DState era) where
  totalAda dstate =
    (UM.fromCompact $ UM.sumRewardsUView (UM.RewDepUView (dsUnified dstate)))
      <> (UM.fromCompact $ UM.sumDepositUView (UM.RewDepUView (dsUnified dstate)))

instance TotalAda (PState era) where
  totalAda pstate = Fold.fold (psDeposits pstate)

instance TotalAda (VState era) where
  totalAda _ = mempty

instance TotalAda (CertState era) where
  totalAda (CertState ds ps vs) = totalAda ds <> totalAda ps <> totalAda vs

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

instance Reflect era => TotalAda (LedgerState era) where
  totalAda (LedgerState utxos dps) = totalAda utxos <+> totalAda dps

instance Reflect era => TotalAda (EpochState era) where
  totalAda eps = totalAda (esLState eps) <+> totalAda (esAccountState eps)

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
