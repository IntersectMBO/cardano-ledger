{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Test.Cardano.Ledger.Generic.Updaters where

import Cardano.Crypto.DSIGN.Class ()
import Cardano.Ledger.Alonzo.Scripts (emptyCostModels)
import Cardano.Ledger.Alonzo.Tx (AlonzoTx (..), hashScriptIntegrity)
import qualified Cardano.Ledger.Alonzo.Tx as Alonzo
import Cardano.Ledger.Alonzo.TxBody (AlonzoTxOut (..))
import Cardano.Ledger.Alonzo.TxWits (AlonzoTxWits (..), Redeemers (..), TxDats (..))
import Cardano.Ledger.Babbage.Core
import Cardano.Ledger.Babbage.TxBody (BabbageTxOut (..))
import Cardano.Ledger.Conway.PParams (
  ppCommitteeMaxTermLengthL,
  ppCommitteeMinSizeL,
  ppDRepActivityL,
  ppDRepDepositL,
  ppDRepVotingThresholdsL,
  ppGovActionDepositL,
  ppGovActionLifetimeL,
  ppPoolVotingThresholdsL,
 )
import Cardano.Ledger.Conway.TxBody (ConwayEraTxBody (..))
import Cardano.Ledger.Plutus.Data (Datum (..))
import Cardano.Ledger.Plutus.Language (Language (..))
import Cardano.Ledger.Shelley.Tx as Shelley (
  ShelleyTx (..),
 )
import Cardano.Ledger.Shelley.TxOut as Shelley (ShelleyTxOut (..))
import Cardano.Ledger.Shelley.TxWits as Shelley (
  addrWits,
  bootWits,
  scriptWits,
 )
import qualified Data.List as List
import Data.Map (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe.Strict (StrictMaybe (..))
import Data.Set (Set)
import qualified Data.Set as Set
import Lens.Micro
import Test.Cardano.Ledger.Generic.Fields
import Test.Cardano.Ledger.Generic.Proof
import Test.Cardano.Ledger.Plutus (zeroTestingCostModels)

-- ===========================================================================
-- Upaters and the use of Policy to specify Merge Semantics and use of [t] as inputs.
-- When using the Updaters, one will usually consruct the fields by hand.
-- So if a Field consists of (Set t), (StrictSeq t), [t], (Maybe t), (StrictMaybe t), or (Map key t)
-- we will use a list, and convert to the appropriate type for each Field and Era.
-- Several of these: (Map key t), (Maybe t) and (StrictMaybe t) can be problematic
-- since they only have a well defined Merge semantics when (SemiGroup t) .
-- So we define specialized functions applyMap, applyMaybe and applySMaybe that raise
-- an error if a Merge semantics finds more than one copy of the elements being combined.
-- Users may choose what merge semantics they want by passing the right Policy
-- =============================================================================

-- =======================================================================
-- A Policy lets you choose to keep the old (first) or the new (override)
-- or combine (merge) of two values. We only use this for elements in the
-- WitnessesField data type. That is because we assemble witnesses in small
-- pieces and we combine the pieces together. Every field in ShelleyTxWits and
-- AlonzoTxWits has clear way of being merged. We don't use Policies in the other
-- xxxField types because most of those parts cannot be safely combined.
-- (The only execeptions are Coin and Value, but they both have Monoid
-- instances, where we can easliy use (<>) instead.).

class Merge t where
  first :: t -> t -> t
  first x _ = x
  override :: t -> t -> t
  override _ y = y
  merge :: t -> t -> t

type Policy = (forall t. Merge t => t -> t -> t)

-- We need just these 4 instances to merge components of TxWitnesses

instance Ord a => Merge (Set a) where
  merge = Set.union

instance Era era => Merge (TxDats era) where
  merge (TxDats x) (TxDats y) = TxDats (Map.union x y)

instance AlonzoEraScript era => Merge (Redeemers era) where
  merge (Redeemers x) (Redeemers y) = Redeemers (Map.union x y)

instance Merge (Map (ScriptHash c) v) where
  merge = Map.union

-- ====================================================================
-- Building Era parametric Records
-- ====================================================================

-- Updaters for Tx

updateTx :: Proof era -> Tx era -> TxField era -> Tx era
updateTx wit@Shelley tx@(ShelleyTx b w d) dt =
  case dt of
    Body fbody -> ShelleyTx fbody w d
    BodyI bfields -> ShelleyTx (newTxBody wit bfields) w d
    TxWits fwit -> ShelleyTx b fwit d
    WitnessesI wfields -> ShelleyTx b (newWitnesses override wit wfields) d
    AuxData faux -> ShelleyTx b w faux
    Valid _ -> tx
updateTx wit@Allegra tx@(ShelleyTx b w d) dt =
  case dt of
    Body fbody -> ShelleyTx fbody w d
    BodyI bfields -> ShelleyTx (newTxBody wit bfields) w d
    TxWits fwit -> ShelleyTx b fwit d
    WitnessesI wfields -> ShelleyTx b (newWitnesses override wit wfields) d
    AuxData faux -> ShelleyTx b w faux
    Valid _ -> tx
updateTx wit@Mary tx@(ShelleyTx b w d) dt =
  case dt of
    Body fbody -> ShelleyTx fbody w d
    BodyI bfields -> ShelleyTx (newTxBody wit bfields) w d
    TxWits fwit -> ShelleyTx b fwit d
    WitnessesI wfields -> ShelleyTx b (newWitnesses override wit wfields) d
    AuxData faux -> ShelleyTx b w faux
    Valid _ -> tx
updateTx wit@Alonzo (Alonzo.AlonzoTx b w iv d) dt =
  case dt of
    Body fbody -> Alonzo.AlonzoTx fbody w iv d
    BodyI bfields -> Alonzo.AlonzoTx (newTxBody wit bfields) w iv d
    TxWits fwit -> Alonzo.AlonzoTx b fwit iv d
    WitnessesI wfields -> Alonzo.AlonzoTx b (newWitnesses override wit wfields) iv d
    AuxData faux -> Alonzo.AlonzoTx b w iv faux
    Valid iv' -> Alonzo.AlonzoTx b w iv' d
updateTx wit@Babbage (AlonzoTx b w iv d) dt =
  case dt of
    Body fbody -> AlonzoTx fbody w iv d
    BodyI bfields -> AlonzoTx (newTxBody wit bfields) w iv d
    TxWits fwit -> AlonzoTx b fwit iv d
    WitnessesI wfields -> AlonzoTx b (newWitnesses override wit wfields) iv d
    AuxData faux -> AlonzoTx b w iv faux
    Valid iv' -> AlonzoTx b w iv' d
updateTx wit@Conway (AlonzoTx b w iv d) dt =
  case dt of
    Body fbody -> AlonzoTx fbody w iv d
    BodyI bfields -> AlonzoTx (newTxBody wit bfields) w iv d
    TxWits fwit -> AlonzoTx b fwit iv d
    WitnessesI wfields -> AlonzoTx b (newWitnesses override wit wfields) iv d
    AuxData faux -> AlonzoTx b w iv faux
    Valid iv' -> AlonzoTx b w iv' d
{-# NOINLINE updateTx #-}

newTx :: Proof era -> [TxField era] -> Tx era
newTx era = List.foldl' (updateTx era) (initialTx era)

--------------------------------------------------------------------
-- Updaters for TxBody

updateTxBody :: EraTxBody era => Proof era -> TxBody era -> TxBodyField era -> TxBody era
updateTxBody pf txBody dt =
  case pf of
    _ | Inputs ins <- dt -> txBody & inputsTxBodyL .~ ins
    _ | Outputs outs <- dt -> txBody & outputsTxBodyL .~ outs
    _ | Txfee fee <- dt -> txBody & feeTxBodyL .~ fee
    _ | AdHash auxDataHash <- dt -> txBody & auxDataHashTxBodyL .~ auxDataHash
    Shelley -> case dt of
      Certs certs -> txBody & certsTxBodyL .~ certs
      Withdrawals' withdrawals -> txBody & withdrawalsTxBodyL .~ withdrawals
      TTL ttl -> txBody & ttlTxBodyL .~ ttl
      Update update -> txBody & updateTxBodyL .~ update
      _ -> txBody
    Allegra -> case dt of
      Certs certs -> txBody & certsTxBodyL .~ certs
      Withdrawals' withdrawals -> txBody & withdrawalsTxBodyL .~ withdrawals
      Vldt vldt -> txBody & vldtTxBodyL .~ vldt
      Update update -> txBody & updateTxBodyL .~ update
      _ -> txBody
    Mary -> case dt of
      Certs certs -> txBody & certsTxBodyL .~ certs
      Withdrawals' withdrawals -> txBody & withdrawalsTxBodyL .~ withdrawals
      Vldt vldt -> txBody & vldtTxBodyL .~ vldt
      Update update -> txBody & updateTxBodyL .~ update
      Mint mint -> txBody & mintTxBodyL .~ mint
      _ -> txBody
    Alonzo -> case dt of
      Certs certs -> txBody & certsTxBodyL .~ certs
      Withdrawals' withdrawals -> txBody & withdrawalsTxBodyL .~ withdrawals
      Vldt vldt -> txBody & vldtTxBodyL .~ vldt
      Update update -> txBody & updateTxBodyL .~ update
      Mint mint -> txBody & mintTxBodyL .~ mint
      Collateral collateral -> txBody & collateralInputsTxBodyL .~ collateral
      ReqSignerHashes reqSignerHashes -> txBody & reqSignerHashesTxBodyL .~ reqSignerHashes
      WppHash scriptIntegrityHash -> txBody & scriptIntegrityHashTxBodyL .~ scriptIntegrityHash
      Txnetworkid networkId -> txBody & networkIdTxBodyL .~ networkId
      _ -> txBody
    Babbage -> case dt of
      Certs certs -> txBody & certsTxBodyL .~ certs
      Withdrawals' withdrawals -> txBody & withdrawalsTxBodyL .~ withdrawals
      Vldt vldt -> txBody & vldtTxBodyL .~ vldt
      Update update -> txBody & updateTxBodyL .~ update
      Mint mint -> txBody & mintTxBodyL .~ mint
      Collateral collateral -> txBody & collateralInputsTxBodyL .~ collateral
      ReqSignerHashes reqSignerHashes -> txBody & reqSignerHashesTxBodyL .~ reqSignerHashes
      WppHash scriptIntegrityHash -> txBody & scriptIntegrityHashTxBodyL .~ scriptIntegrityHash
      Txnetworkid networkId -> txBody & networkIdTxBodyL .~ networkId
      RefInputs refInputs -> txBody & referenceInputsTxBodyL .~ refInputs
      TotalCol totalCol -> txBody & totalCollateralTxBodyL .~ totalCol
      CollateralReturn collateralReturn -> txBody & collateralReturnTxBodyL .~ collateralReturn
      _ -> txBody
    Conway -> case dt of
      Certs certs -> txBody & certsTxBodyL .~ certs
      Withdrawals' withdrawals -> txBody & withdrawalsTxBodyL .~ withdrawals
      Vldt vldt -> txBody & vldtTxBodyL .~ vldt
      Mint mint -> txBody & mintTxBodyL .~ mint
      Collateral collateral -> txBody & collateralInputsTxBodyL .~ collateral
      ReqSignerHashes reqSignerHashes -> txBody & reqSignerHashesTxBodyL .~ reqSignerHashes
      WppHash scriptIntegrityHash -> txBody & scriptIntegrityHashTxBodyL .~ scriptIntegrityHash
      Txnetworkid networkId -> txBody & networkIdTxBodyL .~ networkId
      RefInputs refInputs -> txBody & referenceInputsTxBodyL .~ refInputs
      TotalCol totalCol -> txBody & totalCollateralTxBodyL .~ totalCol
      CollateralReturn collateralReturn -> txBody & collateralReturnTxBodyL .~ collateralReturn
      VotingProc vp -> txBody & votingProceduresTxBodyL .~ vp
      ProposalProc pp -> txBody & proposalProceduresTxBodyL .~ pp
      _ -> txBody
{-# NOINLINE updateTxBody #-}

newTxBody :: EraTxBody era => Proof era -> [TxBodyField era] -> TxBody era
newTxBody era = List.foldl' (updateTxBody era) (initialTxBody era)

--------------------------------------------------------------------
-- Updaters for TxWits

updateWitnesses :: forall era. Policy -> Proof era -> TxWits era -> WitnessesField era -> TxWits era
updateWitnesses p Shelley w dw = case dw of
  (AddrWits ks) -> w {Shelley.addrWits = p (Shelley.addrWits w) ks}
  (BootWits boots) -> w {Shelley.bootWits = p (Shelley.bootWits w) boots}
  (ScriptWits ss) -> w {Shelley.scriptWits = p (Shelley.scriptWits w) ss}
  _ -> w
updateWitnesses p Allegra w dw = case dw of
  (AddrWits ks) -> w {Shelley.addrWits = p (Shelley.addrWits w) ks}
  (BootWits boots) -> w {Shelley.bootWits = p (Shelley.bootWits w) boots}
  (ScriptWits ss) -> w {Shelley.scriptWits = p (Shelley.scriptWits w) ss}
  _ -> w
updateWitnesses p Mary w dw = case dw of
  (AddrWits ks) -> w {Shelley.addrWits = p (Shelley.addrWits w) ks}
  (BootWits boots) -> w {Shelley.bootWits = p (Shelley.bootWits w) boots}
  (ScriptWits ss) -> w {Shelley.scriptWits = p (Shelley.scriptWits w) ss}
  _ -> w
updateWitnesses p Alonzo w dw = case dw of
  (AddrWits ks) -> w {txwitsVKey = p (txwitsVKey w) ks}
  (BootWits boots) -> w {txwitsBoot = p (txwitsBoot w) boots}
  (ScriptWits ss) -> w {txscripts = p (txscripts w) ss}
  (DataWits ds) -> w {txdats = p (txdats w) ds}
  (RdmrWits r) -> w {txrdmrs = p (txrdmrs w) r}
updateWitnesses p Babbage w dw = case dw of
  (AddrWits ks) -> w {txwitsVKey = p (txwitsVKey w) ks}
  (BootWits boots) -> w {txwitsBoot = p (txwitsBoot w) boots}
  (ScriptWits ss) -> w {txscripts = p (txscripts w) ss}
  (DataWits ds) -> w {txdats = p (txdats w) ds}
  (RdmrWits r) -> w {txrdmrs = p (txrdmrs w) r}
updateWitnesses p Conway w dw = case dw of
  (AddrWits ks) -> w {txwitsVKey = p (txwitsVKey w) ks}
  (BootWits boots) -> w {txwitsBoot = p (txwitsBoot w) boots}
  (ScriptWits ss) -> w {txscripts = p (txscripts w) ss}
  (DataWits ds) -> w {txdats = p (txdats w) ds}
  (RdmrWits r) -> w {txrdmrs = p (txrdmrs w) r}
{-# NOINLINE updateWitnesses #-}

newWitnesses :: Era era => Policy -> Proof era -> [WitnessesField era] -> TxWits era
newWitnesses p era = List.foldl' (updateWitnesses p era) (initialWitnesses era)

--------------------------------------------------------------------
-- Updaters for TxOut

notAddress :: TxOutField era -> Bool
notAddress (Address _) = False
notAddress _ = True

updateTxOut :: Proof era -> TxOut era -> TxOutField era -> TxOut era
updateTxOut Shelley (out@(ShelleyTxOut a v)) txoutd = case txoutd of
  Address addr -> ShelleyTxOut addr v
  Amount val -> ShelleyTxOut a val
  _ -> out
updateTxOut Allegra (out@(ShelleyTxOut a v)) txoutd = case txoutd of
  Address addr -> ShelleyTxOut addr v
  Amount val -> ShelleyTxOut a val
  _ -> out
updateTxOut Mary (out@(ShelleyTxOut a v)) txoutd = case txoutd of
  Address addr -> ShelleyTxOut addr v
  Amount val -> ShelleyTxOut a val
  _ -> out
updateTxOut Alonzo (out@(AlonzoTxOut a v h)) txoutd = case txoutd of
  Address addr -> AlonzoTxOut addr v h
  Amount val -> AlonzoTxOut a val h
  DHash mdh -> AlonzoTxOut a v mdh
  FDatum d -> error ("This feature is only available from Babbage onward " ++ show d)
  _ -> out
updateTxOut Babbage (BabbageTxOut a v h refscript) txoutd = case txoutd of
  Address addr -> BabbageTxOut addr v h refscript
  Amount val -> BabbageTxOut a val h refscript
  DHash SNothing -> BabbageTxOut a v NoDatum refscript
  DHash (SJust dh) -> BabbageTxOut a v (DatumHash dh) refscript
  FDatum d -> BabbageTxOut a v d refscript
  RefScript s -> BabbageTxOut a v h s
updateTxOut Conway (BabbageTxOut a v h refscript) txoutd = case txoutd of
  Address addr -> BabbageTxOut addr v h refscript
  Amount val -> BabbageTxOut a val h refscript
  DHash SNothing -> BabbageTxOut a v NoDatum refscript
  DHash (SJust dh) -> BabbageTxOut a v (DatumHash dh) refscript
  FDatum d -> BabbageTxOut a v d refscript
  RefScript s -> BabbageTxOut a v h s
{-# NOINLINE updateTxOut #-}

newTxOut :: Era era => Proof era -> [TxOutField era] -> TxOut era
newTxOut _ dts | all notAddress dts = error ("A call to newTxOut must have an (Address x) field.")
-- This is because we don't have a good story about an initial Address, so the user MUST supply one
newTxOut era dts = List.foldl' (updateTxOut era) (initialTxOut era) dts

-- =====================================================

-- | updatePParams uses the Override policy exclusively
updatePParams :: EraPParams era => Proof era -> PParams era -> PParamsField era -> PParams era
updatePParams proof pp' ppf =
  -- update all of the common fields first
  let pp = case ppf of
        MinfeeA minFeeA -> pp' & ppMinFeeAL .~ minFeeA
        MinfeeB minFeeB -> pp' & ppMinFeeBL .~ minFeeB
        MaxBBSize maxBBSize -> pp' & ppMaxBBSizeL .~ maxBBSize
        MaxTxSize maxTxSize -> pp' & ppMaxTxSizeL .~ maxTxSize
        MaxBHSize maxBHSize -> pp' & ppMaxBHSizeL .~ maxBHSize
        KeyDeposit keyDeposit -> pp' & ppKeyDepositL .~ keyDeposit
        PoolDeposit poolDeposit -> pp' & ppPoolDepositL .~ poolDeposit
        EMax e -> pp' & ppEMaxL .~ e
        NOpt nat -> pp' & ppNOptL .~ nat
        A0 a0 -> pp' & ppA0L .~ a0
        Rho rho -> pp' & ppRhoL .~ rho
        Tau tau -> pp' & ppTauL .~ tau
        ProtocolVersion pv -> pp' & ppProtocolVersionL .~ pv
        MinPoolCost coin -> pp' & ppMinPoolCostL .~ coin
        _ -> pp'
   in case proof of
        Shelley ->
          case ppf of
            D d -> pp & ppDL .~ d
            ExtraEntropy nonce -> pp & ppExtraEntropyL .~ nonce
            MinUTxOValue mu -> pp & ppMinUTxOValueL .~ mu
            _ -> pp
        Allegra ->
          case ppf of
            D d -> pp & ppDL .~ d
            ExtraEntropy nonce -> pp & ppExtraEntropyL .~ nonce
            MinUTxOValue mu -> pp & ppMinUTxOValueL .~ mu
            _ -> pp
        Mary ->
          case ppf of
            D d -> pp & ppDL .~ d
            ExtraEntropy nonce -> pp & ppExtraEntropyL .~ nonce
            MinUTxOValue mu -> pp & ppMinUTxOValueL .~ mu
            _ -> pp
        Alonzo ->
          case ppf of
            D d -> pp & ppDL .~ d
            ExtraEntropy nonce -> pp & ppExtraEntropyL .~ nonce
            CoinPerUTxOWord coinPerWord -> pp & ppCoinsPerUTxOWordL .~ coinPerWord
            Costmdls costModels -> pp & ppCostModelsL .~ costModels
            Prices prices -> pp & ppPricesL .~ prices
            MaxTxExUnits maxTxExUnits -> pp & ppMaxTxExUnitsL .~ maxTxExUnits
            MaxBlockExUnits maxBlockExUnits -> pp & ppMaxBlockExUnitsL .~ maxBlockExUnits
            MaxValSize maxValSize -> pp & ppMaxValSizeL .~ maxValSize
            CollateralPercentage colPerc -> pp & ppCollateralPercentageL .~ colPerc
            MaxCollateralInputs maxColInputs -> pp & ppMaxCollateralInputsL .~ maxColInputs
            _ -> pp
        Babbage ->
          case ppf of
            CoinPerUTxOByte coinPerByte -> pp & ppCoinsPerUTxOByteL .~ coinPerByte
            Costmdls costModels -> pp & ppCostModelsL .~ costModels
            Prices prices -> pp & ppPricesL .~ prices
            MaxTxExUnits maxTxExUnits -> pp & ppMaxTxExUnitsL .~ maxTxExUnits
            MaxBlockExUnits maxBlockExUnits -> pp & ppMaxBlockExUnitsL .~ maxBlockExUnits
            MaxValSize maxValSize -> pp & ppMaxValSizeL .~ maxValSize
            CollateralPercentage colPerc -> pp & ppCollateralPercentageL .~ colPerc
            MaxCollateralInputs maxColInputs -> pp & ppMaxCollateralInputsL .~ maxColInputs
            _ -> pp
        Conway ->
          case ppf of
            CoinPerUTxOByte coinPerByte -> pp & ppCoinsPerUTxOByteL .~ coinPerByte
            Costmdls costModels -> pp & ppCostModelsL .~ costModels
            Prices prices -> pp & ppPricesL .~ prices
            MaxTxExUnits maxTxExUnits -> pp & ppMaxTxExUnitsL .~ maxTxExUnits
            MaxBlockExUnits maxBlockExUnits -> pp & ppMaxBlockExUnitsL .~ maxBlockExUnits
            MaxValSize maxValSize -> pp & ppMaxValSizeL .~ maxValSize
            CollateralPercentage colPerc -> pp & ppCollateralPercentageL .~ colPerc
            MaxCollateralInputs maxColInputs -> pp & ppMaxCollateralInputsL .~ maxColInputs
            GovActionDeposit c -> pp & ppGovActionDepositL .~ c
            DRepDeposit c -> pp & ppDRepDepositL .~ c
            DRepActivity c -> pp & ppDRepActivityL .~ c
            PoolVotingThreshold c -> pp & ppPoolVotingThresholdsL .~ c
            DRepVotingThreshold c -> pp & ppDRepVotingThresholdsL .~ c
            MinCommitteeSize c -> pp & ppCommitteeMinSizeL .~ c
            CommitteeTermLimit c -> pp & ppCommitteeMaxTermLengthL .~ c
            GovActionExpiration c -> pp & ppGovActionLifetimeL .~ c
            _ -> pp

newPParams :: EraPParams era => Proof era -> [PParamsField era] -> PParams era
newPParams era = List.foldl' (updatePParams era) emptyPParams

-- ====================================

-- | This only make sense in the Alonzo era and forward, all other Eras return Nothing
newScriptIntegrityHash ::
  Proof era ->
  PParams era ->
  [Language] ->
  Redeemers era ->
  TxDats era ->
  StrictMaybe (Alonzo.ScriptIntegrityHash (EraCrypto era))
newScriptIntegrityHash Conway pp ls rds dats =
  hashScriptIntegrity (Set.map (Alonzo.getLanguageView pp) (Set.fromList ls)) rds dats
newScriptIntegrityHash Babbage pp ls rds dats =
  hashScriptIntegrity (Set.map (Alonzo.getLanguageView pp) (Set.fromList ls)) rds dats
newScriptIntegrityHash Alonzo pp ls rds dats =
  hashScriptIntegrity (Set.map (Alonzo.getLanguageView pp) (Set.fromList ls)) rds dats
newScriptIntegrityHash _wit _pp _ls _rds _dats = SNothing

defaultCostModels :: Proof era -> PParamsField era
defaultCostModels Shelley = Costmdls emptyCostModels
defaultCostModels Allegra = Costmdls emptyCostModels
defaultCostModels Mary = Costmdls emptyCostModels
defaultCostModels Alonzo = Costmdls $ zeroTestingCostModels [PlutusV1]
defaultCostModels Babbage = Costmdls $ zeroTestingCostModels [PlutusV1, PlutusV2]
defaultCostModels Conway = Costmdls $ zeroTestingCostModels [PlutusV1, PlutusV2]

languages :: Proof era -> [Language]
languages Shelley = []
languages Allegra = []
languages Mary = []
languages Alonzo = [PlutusV1]
languages Babbage = [PlutusV1, PlutusV2]
languages Conway = [PlutusV1, PlutusV2]
