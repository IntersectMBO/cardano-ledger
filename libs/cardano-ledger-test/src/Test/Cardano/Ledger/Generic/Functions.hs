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
import Cardano.Ledger.Alonzo.Language (Language (..))
import Cardano.Ledger.Alonzo.Scripts (ExUnits (..))
import Cardano.Ledger.Alonzo.Scripts.Data (binaryDataToData, hashData)
import Cardano.Ledger.Alonzo.Tx (AlonzoTx (..), IsValid (..))
import Cardano.Ledger.Alonzo.TxBody (AlonzoTxOut (..), collateral')
import Cardano.Ledger.Alonzo.TxInfo (languages)
import Cardano.Ledger.Babbage.Tx (refScripts)
import Cardano.Ledger.Babbage.TxBody (
  BabbageTxOut (..),
  Datum (..),
  collateralInputs',
  collateralReturn',
  referenceInputs',
  spendInputs',
 )
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
import Cardano.Ledger.Shelley.TxBody (
  ShelleyTxOut (..),
 )
import Cardano.Ledger.Shelley.TxCert (
  pattern RegTxCert,
  pattern UnRegTxCert,
 )
import Cardano.Ledger.TxIn (TxIn (..))
import qualified Cardano.Ledger.UMap as UM
import Cardano.Ledger.UTxO (EraUTxO (..), UTxO (..), coinBalance)
import Cardano.Ledger.Val (Val (inject, (<+>), (<->)))
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
import Test.Cardano.Ledger.Alonzo.Arbitrary (alwaysFails, alwaysSucceeds)
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
maxCollateralInputs' (Alonzo _) pp = pp ^. ppMaxCollateralInputsL
maxCollateralInputs' (Babbage _) pp = pp ^. ppMaxCollateralInputsL
maxCollateralInputs' (Conway _) pp = pp ^. ppMaxCollateralInputsL
maxCollateralInputs' _proof _pp = 0
{-# NOINLINE maxCollateralInputs' #-}

maxTxExUnits' :: Proof era -> PParams era -> ExUnits
maxTxExUnits' (Alonzo _) pp = pp ^. ppMaxTxExUnitsL
maxTxExUnits' (Babbage _) pp = pp ^. ppMaxTxExUnitsL
maxTxExUnits' (Conway _) pp = pp ^. ppMaxTxExUnitsL
maxTxExUnits' _proof _x = mempty
{-# NOINLINE maxTxExUnits' #-}

collateralPercentage' :: Proof era -> PParams era -> Natural
collateralPercentage' (Alonzo _) pp = pp ^. ppCollateralPercentageL
collateralPercentage' (Babbage _) pp = pp ^. ppCollateralPercentageL
collateralPercentage' (Conway _) pp = pp ^. ppCollateralPercentageL
collateralPercentage' _proof _pp = 0
{-# NOINLINE collateralPercentage' #-}

protocolVersion :: Proof era -> ProtVer
protocolVersion (Conway _) = ProtVer (natVersion @9) 0
protocolVersion (Babbage _) = ProtVer (natVersion @7) 0
protocolVersion (Alonzo _) = ProtVer (natVersion @6) 0
protocolVersion (Mary _) = ProtVer (natVersion @4) 0
protocolVersion (Allegra _) = ProtVer (natVersion @3) 0
protocolVersion (Shelley _) = ProtVer (natVersion @2) 0
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
scriptWitsNeeded' (Conway _) utxo txBody = regularScripts `Set.difference` inlineScripts
  where
    theUtxo = UTxO utxo
    inputs = txBody ^. inputsTxBodyL
    inlineScripts = keysSet $ refScripts inputs theUtxo
    regularScripts = getScriptsHashesNeeded (getScriptsNeeded theUtxo txBody)
scriptWitsNeeded' (Babbage _) utxo txBody = regularScripts `Set.difference` inlineScripts
  where
    theUtxo = UTxO utxo
    inputs = spendInputs' txBody `Set.union` referenceInputs' txBody
    inlineScripts = keysSet $ refScripts inputs theUtxo
    regularScripts = getScriptsHashesNeeded (getScriptsNeeded theUtxo txBody)
scriptWitsNeeded' (Alonzo _) utxo txBody =
  getScriptsHashesNeeded (getScriptsNeeded (UTxO utxo) txBody)
scriptWitsNeeded' (Mary _) utxo txBody =
  getScriptsHashesNeeded (getScriptsNeeded (UTxO utxo) txBody)
scriptWitsNeeded' (Allegra _) utxo txBody =
  getScriptsHashesNeeded (getScriptsNeeded (UTxO utxo) txBody)
scriptWitsNeeded' (Shelley _) utxo txBody =
  getScriptsHashesNeeded (getScriptsNeeded (UTxO utxo) txBody)
{-# NOINLINE scriptWitsNeeded' #-}

scriptsNeeded' :: Proof era -> MUtxo era -> TxBody era -> Set (ScriptHash (EraCrypto era))
scriptsNeeded' (Conway _) utxo txBody =
  getScriptsHashesNeeded (getScriptsNeeded (UTxO utxo) txBody)
scriptsNeeded' (Babbage _) utxo txBody =
  getScriptsHashesNeeded (getScriptsNeeded (UTxO utxo) txBody)
scriptsNeeded' (Alonzo _) utxo txBody =
  getScriptsHashesNeeded (getScriptsNeeded (UTxO utxo) txBody)
scriptsNeeded' (Mary _) utxo txBody =
  getScriptsHashesNeeded (getScriptsNeeded (UTxO utxo) txBody)
scriptsNeeded' (Allegra _) utxo txBody =
  getScriptsHashesNeeded (getScriptsNeeded (UTxO utxo) txBody)
scriptsNeeded' (Shelley _) utxo txBody =
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
txoutFields (Conway _) (BabbageTxOut addr val d h) = (addr, val, [FDatum d, RefScript h])
txoutFields (Babbage _) (BabbageTxOut addr val d h) = (addr, val, [FDatum d, RefScript h])
txoutFields (Alonzo _) (AlonzoTxOut addr val dh) = (addr, val, [DHash dh])
txoutFields (Mary _) (ShelleyTxOut addr val) = (addr, val, [])
txoutFields (Allegra _) (ShelleyTxOut addr val) = (addr, val, [])
txoutFields (Shelley _) (ShelleyTxOut addr val) = (addr, val, [])
{-# NOINLINE txoutFields #-}

injectFee :: EraTxOut era => Proof era -> Coin -> TxOut era -> TxOut era
injectFee _ fee txOut = txOut & valueTxOutL %~ (<+> inject fee)

getTxOutRefScript :: Proof era -> TxOut era -> StrictMaybe (Script era)
getTxOutRefScript (Conway _) (BabbageTxOut _ _ _ ms) = ms
getTxOutRefScript (Babbage _) (BabbageTxOut _ _ _ ms) = ms
getTxOutRefScript _ _ = SNothing
{-# NOINLINE getTxOutRefScript #-}

emptyPPUPstate :: forall era. Proof era -> ShelleyPPUPState era
emptyPPUPstate (Conway _) = def
emptyPPUPstate (Babbage _) = def
emptyPPUPstate (Alonzo _) = def
emptyPPUPstate (Mary _) = def
emptyPPUPstate (Allegra _) = def
emptyPPUPstate (Shelley _) = def
{-# NOINLINE emptyPPUPstate #-}

maxRefInputs :: Proof era -> Int
maxRefInputs (Babbage _) = 3
maxRefInputs _ = 0

isValid' :: Proof era -> Tx era -> IsValid
isValid' (Conway _) x = isValid x
isValid' (Babbage _) x = isValid x
isValid' (Alonzo _) x = isValid x
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
txoutEvidence (Alonzo _) (AlonzoTxOut addr _ (SJust dh)) =
  (addrCredentials addr, Just dh)
txoutEvidence (Alonzo _) (AlonzoTxOut addr _ SNothing) =
  (addrCredentials addr, Nothing)
txoutEvidence (Conway _) (BabbageTxOut addr _ NoDatum _) =
  (addrCredentials addr, Nothing)
txoutEvidence (Conway _) (BabbageTxOut addr _ (DatumHash dh) _) =
  (addrCredentials addr, Just dh)
txoutEvidence (Conway _) (BabbageTxOut addr _ (Datum _d) _) =
  (addrCredentials addr, Just (hashData @era (binaryDataToData _d)))
txoutEvidence (Babbage _) (BabbageTxOut addr _ NoDatum _) =
  (addrCredentials addr, Nothing)
txoutEvidence (Babbage _) (BabbageTxOut addr _ (DatumHash dh) _) =
  (addrCredentials addr, Just dh)
txoutEvidence (Babbage _) (BabbageTxOut addr _ (Datum _d) _) =
  (addrCredentials addr, Just (hashData @era (binaryDataToData _d)))
txoutEvidence (Mary _) (ShelleyTxOut addr _) =
  (addrCredentials addr, Nothing)
txoutEvidence (Allegra _) (ShelleyTxOut addr _) =
  (addrCredentials addr, Nothing)
txoutEvidence (Shelley _) (ShelleyTxOut addr _) =
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
getCollateralInputs (Conway _) tx = tx ^. collateralInputsTxBodyL
getCollateralInputs (Babbage _) tx = collateralInputs' tx
getCollateralInputs (Alonzo _) tx = collateral' tx
getCollateralInputs (Mary _) _ = Set.empty
getCollateralInputs (Allegra _) _ = Set.empty
getCollateralInputs (Shelley _) _ = Set.empty
{-# NOINLINE getCollateralInputs #-}

getCollateralOutputs :: Proof era -> TxBody era -> [TxOut era]
getCollateralOutputs (Conway _) tx = case tx ^. collateralReturnTxBodyL of SNothing -> []; SJust x -> [x]
getCollateralOutputs (Babbage _) tx = case collateralReturn' tx of SNothing -> []; SJust x -> [x]
getCollateralOutputs (Alonzo _) _ = []
getCollateralOutputs (Mary _) _ = []
getCollateralOutputs (Allegra _) _ = []
getCollateralOutputs (Shelley _) _ = []
{-# NOINLINE getCollateralOutputs #-}

getInputs :: EraTxBody era => Proof era -> TxBody era -> Set (TxIn (EraCrypto era))
getInputs _ tx = tx ^. inputsTxBodyL

getOutputs :: EraTxBody era => Proof era -> TxBody era -> StrictSeq (TxOut era)
getOutputs _ tx = tx ^. outputsTxBodyL

getScriptWits :: EraTxWits era => Proof era -> TxWits era -> Map (ScriptHash (EraCrypto era)) (Script era)
getScriptWits _ tx = tx ^. scriptTxWitsL

allInputs :: EraTxBody era => Proof era -> TxBody era -> Set (TxIn (EraCrypto era))
allInputs _ txb = txb ^. allInputsTxBodyF

getWitnesses :: EraTx era => Proof era -> Tx era -> TxWits era
getWitnesses _ tx = tx ^. witsTxL

primaryLanguage :: Proof era -> Maybe Language
primaryLanguage (Conway _) = Just PlutusV2
primaryLanguage (Babbage _) = Just PlutusV2
primaryLanguage (Alonzo _) = Just PlutusV1
primaryLanguage _ = Nothing
{-# NOINLINE primaryLanguage #-}

alwaysTrue :: forall era. Proof era -> Maybe Language -> Natural -> Script era
alwaysTrue (Conway _) (Just l) n = alwaysSucceeds @era l n
alwaysTrue p@(Conway _) Nothing _ = allOf [] p
alwaysTrue (Babbage _) (Just l) n = alwaysSucceeds @era l n
alwaysTrue p@(Babbage _) Nothing _ = allOf [] p
alwaysTrue (Alonzo _) (Just l) n = alwaysSucceeds @era l n
alwaysTrue p@(Alonzo _) Nothing _ = allOf [] p
alwaysTrue p@(Mary _) _ n = always n p
alwaysTrue p@(Allegra _) _ n = always n p
alwaysTrue p@(Shelley _) _ n = always n p
{-# NOINLINE alwaysTrue #-}

alwaysFalse :: forall era. Proof era -> Maybe Language -> Natural -> Script era
alwaysFalse (Conway _) (Just l) n = alwaysFails @era l n
alwaysFalse p@(Conway _) Nothing _ = anyOf [] p
alwaysFalse (Babbage _) (Just l) n = alwaysFails @era l n
alwaysFalse p@(Babbage _) Nothing _ = anyOf [] p
alwaysFalse (Alonzo _) (Just l) n = alwaysFails @era l n
alwaysFalse p@(Alonzo _) Nothing _ = anyOf [] p
alwaysFalse p@(Mary _) _ n = never n p
alwaysFalse p@(Allegra _) _ n = never n p
alwaysFalse p@(Shelley _) _ n = never n p
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
        Conway _ -> createRUpdOld_ @era slotsPerEpoch bm ss reserves pp totalStake rs def
        Babbage _ -> createRUpdOld_ @era slotsPerEpoch bm ss reserves pp totalStake rs def
        Alonzo _ -> createRUpdOld_ @era slotsPerEpoch bm ss reserves pp totalStake rs def
        Mary _ -> createRUpdOld_ @era slotsPerEpoch bm ss reserves pp totalStake rs def
        Allegra _ -> createRUpdOld_ @era slotsPerEpoch bm ss reserves pp totalStake rs def
        Shelley _ -> createRUpdOld_ @era slotsPerEpoch bm ss reserves pp totalStake rs def
{-# NOINLINE createRUpdNonPulsing' #-}

languagesUsed ::
  forall era.
  Era era =>
  Proof era ->
  Tx era ->
  UTxO era ->
  Set (ScriptHash (EraCrypto era)) ->
  Set Language
languagesUsed proof tx utxo sNeeded = case proof of
  (Shelley _) -> Set.empty
  (Allegra _) -> Set.empty
  (Mary _) -> Set.empty
  (Alonzo _) -> Cardano.Ledger.Alonzo.TxInfo.languages tx utxo sNeeded
  (Babbage _) -> Cardano.Ledger.Alonzo.TxInfo.languages tx utxo sNeeded
  (Conway _) -> Cardano.Ledger.Alonzo.TxInfo.languages tx utxo sNeeded
{-# NOINLINE languagesUsed #-}

-- | Compute the total Ada from Ada pots within 't'
class TotalAda t where
  totalAda :: t -> Coin

instance TotalAda AccountState where
  totalAda (AccountState treasury reserves) = treasury <+> reserves

instance Reflect era => TotalAda (UTxOState era) where
  totalAda (UTxOState utxo _deposits fees gs _) =
    totalAda utxo <+> fees <+> governanceStateTotalAda gs

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

instance TotalAda (ShelleyPPUPState era) where
  totalAda _ = mempty

governanceStateTotalAda :: forall era. Reflect era => GovernanceState era -> Coin
governanceStateTotalAda = case reify @era of
  Shelley _ -> totalAda
  Mary _ -> totalAda
  Allegra _ -> totalAda
  Alonzo _ -> totalAda
  Babbage _ -> totalAda
  Conway _ -> mempty

instance Reflect era => TotalAda (LedgerState era) where
  totalAda (LedgerState utxos dps) = totalAda utxos <+> totalAda dps

instance Reflect era => TotalAda (EpochState era) where
  totalAda eps = totalAda (esLState eps) <+> totalAda (esAccountState eps)

instance Reflect era => TotalAda (NewEpochState era) where
  totalAda nes = totalAda (nesEs nes)

adaPots :: Proof era -> EpochState era -> AdaPots
adaPots (Conway _) es = totalAdaPotsES es
adaPots (Babbage _) es = totalAdaPotsES es
adaPots (Alonzo _) es = totalAdaPotsES es
adaPots (Mary _) es = totalAdaPotsES es
adaPots (Allegra _) es = totalAdaPotsES es
adaPots (Shelley _) es = totalAdaPotsES es
{-# NOINLINE adaPots #-}
