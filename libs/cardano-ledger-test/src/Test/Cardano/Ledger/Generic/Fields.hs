{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ViewPatterns #-}

module Test.Cardano.Ledger.Generic.Fields (
  TxField (.., AuxData', Valid'),
  TxBodyField (
    ..,
    Inputs',
    Collateral',
    RefInputs',
    Outputs',
    Certs',
    CollateralReturn',
    Update',
    ReqSignerHashes',
    WppHash',
    AdHash',
    Txnetworkid'
  ),
  WitnessesField (.., AddrWits', BootWits', ScriptWits', DataWits'),
  PParamsField (..),
  TxOutField (.., DHash', RefScript'),
  initVI,
  initWithdrawals,
  initialTxBody,
  initialWitnesses,
  initialTx,
  initialTxOut,
  valid,
  abstractTx,
  abstractTxBody,
  abstractTxOut,
  abstractWitnesses,
  abstractPParams,
)
where

import Cardano.Ledger.Address (Addr (..))
import Cardano.Ledger.Allegra.TxBody (AllegraTxBody (..))
import Cardano.Ledger.Alonzo.PParams (AlonzoPParams (..), unOrdExUnits)
import Cardano.Ledger.Alonzo.Scripts (CostModels, ExUnits (..), Prices)
import Cardano.Ledger.Alonzo.Tx (AlonzoTx (..), IsValid (..))
import Cardano.Ledger.Alonzo.TxAuxData (AuxiliaryDataHash)
import Cardano.Ledger.Alonzo.TxBody (AlonzoTxBody (..), AlonzoTxOut (..))
import Cardano.Ledger.Alonzo.TxWits (AlonzoTxWits (..), Redeemers (..), TxDats (..))
import Cardano.Ledger.Babbage.PParams (BabbagePParams (..))
import Cardano.Ledger.Babbage.TxBody (BabbageTxBody (..), BabbageTxOut (..))
import Cardano.Ledger.BaseTypes (
  EpochInterval (..),
  Network (..),
  NonNegativeInterval,
  Nonce,
  ProtVer (..),
  StrictMaybe (..),
  UnitInterval,
 )
import Cardano.Ledger.Binary (sizedValue)
import Cardano.Ledger.Coin (Coin (..))
import Cardano.Ledger.Conway.Core
import Cardano.Ledger.Conway.Governance (GovProcedures (..))
import Cardano.Ledger.Conway.PParams (ConwayPParams (..))
import Cardano.Ledger.Conway.TxBody (ConwayTxBody (..))
import Cardano.Ledger.Credential (Credential (..), StakeReference (..))
import Cardano.Ledger.Keys (KeyHash, KeyRole (..), WitVKey (..), hashKey)
import Cardano.Ledger.Keys.Bootstrap (BootstrapWitness (..))
import Cardano.Ledger.Mary.TxBody (MaryTxBody (..))
import Cardano.Ledger.Mary.Value (MultiAsset (..))
import Cardano.Ledger.Plutus.Data (Data (..), Datum (..), hashData)
import Cardano.Ledger.Shelley.PParams (ShelleyPParams (..))
import qualified Cardano.Ledger.Shelley.PParams as PP (Update)
import Cardano.Ledger.Shelley.Tx (ShelleyTx (..))
import Cardano.Ledger.Shelley.TxBody (ShelleyTxBody (..))
import Cardano.Ledger.Shelley.TxOut (ShelleyTxOut (..))
import Cardano.Ledger.Shelley.TxWits (pattern ShelleyTxWits)
import Cardano.Ledger.TxIn (TxIn (..))
import Cardano.Slotting.Slot (SlotNo (..))
import Data.Map (Map)
import qualified Data.Map.Strict as Map
import Data.Sequence.Strict (StrictSeq (..))
import qualified Data.Sequence.Strict as SSeq (fromList)
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Word (Word16, Word32)
import Lens.Micro ((^.))
import Numeric.Natural (Natural)
import Test.Cardano.Ledger.Core.KeyPair (KeyPair (..))
import Test.Cardano.Ledger.Generic.Indexed (theKeyPair)
import Test.Cardano.Ledger.Generic.Proof

-- =======================================================
-- Fields are used to hold a single field of record. So the Field
-- data type (Core.T era) holds the union of all fields of (Core.T era)
-- across all eras Shelley, Allegra, Mary, Alonzo, Babbage.
-- Pattern constructors (with primed names, like C') allow users to use [a], to stand
-- for (Set a) (Maybe a) (StrictSeq a) (StrictMaybe a) (Map (hash a) a)
-- and hide the conversion details from the user. This is very convenient when
-- using Fields to construct (Core.Txx era) by hand in an era agnostic way.

data TxField era
  = Body (TxBody era)
  | BodyI [TxBodyField era] -- Inlines TxBody Fields
  | TxWits (TxWits era)
  | WitnessesI [WitnessesField era] -- Inlines Witnesess Fields
  | AuxData (StrictMaybe (TxAuxData era))
  | Valid IsValid

pattern AuxData' :: [TxAuxData era] -> TxField era

pattern Valid' :: Bool -> TxField era

-- =================
data TxBodyField era
  = Inputs (Set (TxIn (EraCrypto era)))
  | Collateral (Set (TxIn (EraCrypto era)))
  | RefInputs (Set (TxIn (EraCrypto era)))
  | Outputs (StrictSeq (TxOut era))
  | CollateralReturn (StrictMaybe (TxOut era))
  | TotalCol (StrictMaybe Coin)
  | Certs (StrictSeq (TxCert era))
  | Withdrawals' (Withdrawals (EraCrypto era))
  | Txfee Coin
  | Vldt ValidityInterval
  | TTL SlotNo
  | Update (StrictMaybe (PP.Update era))
  | ReqSignerHashes (Set (KeyHash 'Witness (EraCrypto era)))
  | Mint (MultiAsset (EraCrypto era))
  | WppHash (StrictMaybe (ScriptIntegrityHash (EraCrypto era)))
  | AdHash (StrictMaybe (AuxiliaryDataHash (EraCrypto era)))
  | Txnetworkid (StrictMaybe Network)
  | GovProcs (GovProcedures era)
  | CurrentTreasuryValue (StrictMaybe Coin)
  | TreasuryDonation Coin

pattern Inputs' :: [TxIn (EraCrypto era)] -> TxBodyField era -- Set

pattern Collateral' :: [TxIn (EraCrypto era)] -> TxBodyField era -- Set

pattern RefInputs' :: [TxIn (EraCrypto era)] -> TxBodyField era -- Set

pattern Outputs' :: [TxOut era] -> TxBodyField era -- StrictSeq

pattern Certs' :: [TxCert era] -> TxBodyField era -- StrictSeq

pattern CollateralReturn' :: [TxOut era] -> TxBodyField era -- 0 or 1 element

pattern Update' :: [PP.Update era] -> TxBodyField era -- 0 or 1 element

pattern ReqSignerHashes' :: [KeyHash 'Witness (EraCrypto era)] -> TxBodyField era -- A set

pattern WppHash' :: [ScriptIntegrityHash (EraCrypto era)] -> TxBodyField era -- 0 or 1 element

pattern AdHash' :: [AuxiliaryDataHash (EraCrypto era)] -> TxBodyField era -- 0 or 1 element

pattern Txnetworkid' :: [Network] -> TxBodyField era -- 0 or 1 element

-- ====================
data WitnessesField era
  = AddrWits (Set (WitVKey 'Witness (EraCrypto era)))
  | BootWits (Set (BootstrapWitness (EraCrypto era)))
  | ScriptWits (Map (ScriptHash (EraCrypto era)) (Script era))
  | DataWits (TxDats era)
  | RdmrWits (Redeemers era)

pattern AddrWits' :: Era era => [WitVKey 'Witness (EraCrypto era)] -> WitnessesField era -- Set

pattern BootWits' :: Era era => [BootstrapWitness (EraCrypto era)] -> WitnessesField era -- Set

pattern ScriptWits' :: forall era. EraScript era => [Script era] -> WitnessesField era -- Map

pattern DataWits' :: Era era => [Data era] -> WitnessesField era -- Map

-- ================
data TxOutField era
  = Address (Addr (EraCrypto era))
  | Amount (Value era)
  | DHash (StrictMaybe (DataHash (EraCrypto era)))
  | FDatum (Datum era)
  | RefScript (StrictMaybe (Script era))

pattern DHash' :: [DataHash (EraCrypto era)] -> TxOutField era -- 0 or 1 element

pattern RefScript' :: [Script era] -> TxOutField era -- 0 or 1 element

-- ================================================================
-- PParam Fields

data PParamsField era
  = MinfeeA Coin
  | -- | The constant factor for the minimum fee calculation
    MinfeeB Coin
  | -- | Maximal block body size
    MaxBBSize Word32
  | -- | Maximal transaction size
    MaxTxSize Word32
  | -- | Maximal block header size
    MaxBHSize Word16
  | -- | The amount of a key registration deposit
    KeyDeposit Coin
  | -- | The amount of a pool registration deposit
    PoolDeposit Coin
  | -- | epoch bound on pool retirement
    EMax EpochInterval
  | -- | Desired number of pools
    NOpt Natural
  | -- | Pool influence
    A0 NonNegativeInterval
  | -- | Monetary expansion
    Rho UnitInterval
  | -- | Treasury expansion
    Tau UnitInterval
  | -- | Decentralization parameter
    D UnitInterval -- Dropped in Babbage
  | -- | Extra entropy
    ExtraEntropy Nonce -- Dropped in Babbage
  | -- | Protocol version
    ProtocolVersion ProtVer
  | -- | Minimum Stake Pool Cost
    MinPoolCost Coin
  | -- | Minimum Lovelace in a UTxO deprecated by AdaPerUTxOWord
    MinUTxOValue Coin
  | -- | Cost in ada per 8 bytes of UTxO storage instead of _minUTxOValue
    CoinPerUTxOWord CoinPerWord -- Dropped in Babbage
  | -- | Cost in ada per 1 byte of UTxO storage instead of _coinsPerUTxOWord
    CoinPerUTxOByte CoinPerByte -- New in Babbage
  | -- | Cost models for non-native script languages
    Costmdls CostModels
  | -- | Prices of execution units for non-native script languages
    Prices Prices
  | -- | Max total script execution resources units allowed per tx
    MaxTxExUnits ExUnits
  | -- | Max total script execution resources units allowed per block
    MaxBlockExUnits ExUnits
  | -- | Max size of a Value in an output
    MaxValSize Natural
  | -- | The scaling percentage of the collateral relative to the fee
    CollateralPercentage Natural
  | -- | Maximum number of collateral inputs allowed in a transaction
    MaxCollateralInputs Natural
  | -- | These are new to Conway
    PoolVotingThreshold PoolVotingThresholds
  | DRepVotingThreshold DRepVotingThresholds
  | MinCommitteeSize Natural
  | CommitteeTermLimit EpochInterval
  | GovActionExpiration EpochInterval
  | GovActionDeposit Coin
  | DRepDeposit Coin
  | DRepActivity EpochInterval

abstractPParams :: Proof era -> PParams era -> [PParamsField era]
abstractPParams proof ppp = case (whichPParams proof, ppp) of
  (PParamsShelleyToMary, PParams pp@(ShelleyPParams {})) ->
    [ MinfeeA (sppMinFeeA pp)
    , MinfeeB (sppMinFeeB pp)
    , MaxBBSize (sppMaxBBSize pp)
    , MaxTxSize (sppMaxTxSize pp)
    , MaxBHSize (sppMaxBHSize pp)
    , KeyDeposit (sppKeyDeposit pp)
    , PoolDeposit (sppPoolDeposit pp)
    , EMax (sppEMax pp)
    , NOpt (sppNOpt pp)
    , A0 (sppA0 pp)
    , Rho (sppRho pp)
    , Tau (sppTau pp)
    , D (sppD pp)
    , ExtraEntropy (sppExtraEntropy pp)
    , ProtocolVersion (sppProtocolVersion pp)
    , MinUTxOValue (sppMinUTxOValue pp)
    , MinPoolCost (sppMinPoolCost pp)
    ]
  (PParamsAlonzoToAlonzo, PParams pp@(AlonzoPParams {})) ->
    [ MinfeeA (appMinFeeA pp)
    , MinfeeB (appMinFeeB pp)
    , MaxBBSize (appMaxBBSize pp)
    , MaxTxSize (appMaxTxSize pp)
    , MaxBHSize (appMaxBHSize pp)
    , KeyDeposit (appKeyDeposit pp)
    , PoolDeposit (appPoolDeposit pp)
    , EMax (appEMax pp)
    , NOpt (appNOpt pp)
    , A0 (appA0 pp)
    , Rho (appRho pp)
    , Tau (appTau pp)
    , D (appD pp)
    , ExtraEntropy (appExtraEntropy pp)
    , ProtocolVersion (appProtocolVersion pp)
    , MinPoolCost (appMinPoolCost pp)
    , CoinPerUTxOWord (appCoinsPerUTxOWord pp)
    , Costmdls (appCostModels pp)
    , Prices (appPrices pp)
    , MaxTxExUnits (unOrdExUnits (appMaxTxExUnits pp))
    , MaxBlockExUnits (unOrdExUnits (appMaxBlockExUnits pp))
    , MaxValSize (appMaxValSize pp)
    , CollateralPercentage (appCollateralPercentage pp)
    , MaxCollateralInputs (appMaxCollateralInputs pp)
    ]
  (PParamsBabbageToBabbage, PParams pp@(BabbagePParams {})) ->
    [ MinfeeA (bppMinFeeA pp)
    , MinfeeB (bppMinFeeB pp)
    , MaxBBSize (bppMaxBBSize pp)
    , MaxTxSize (bppMaxTxSize pp)
    , MaxBHSize (bppMaxBHSize pp)
    , KeyDeposit (bppKeyDeposit pp)
    , PoolDeposit (bppPoolDeposit pp)
    , EMax (bppEMax pp)
    , NOpt (bppNOpt pp)
    , A0 (bppA0 pp)
    , Rho (bppRho pp)
    , Tau (bppTau pp)
    , ProtocolVersion (bppProtocolVersion pp)
    , MinPoolCost (bppMinPoolCost pp)
    , CoinPerUTxOByte (bppCoinsPerUTxOByte pp)
    , Costmdls (bppCostModels pp)
    , Prices (bppPrices pp)
    , MaxTxExUnits (unOrdExUnits (bppMaxTxExUnits pp))
    , MaxBlockExUnits (unOrdExUnits (bppMaxBlockExUnits pp))
    , MaxValSize (bppMaxValSize pp)
    , CollateralPercentage (bppCollateralPercentage pp)
    , MaxCollateralInputs (bppMaxCollateralInputs pp)
    ]
  (PParamsConwayToConway, pp@(PParams (ConwayPParams {}))) ->
    [ MinfeeA (pp ^. ppMinFeeAL)
    , MinfeeB (pp ^. ppMinFeeBL)
    , MaxBBSize (pp ^. ppMaxBBSizeL)
    , MaxTxSize (pp ^. ppMaxTxSizeL)
    , MaxBHSize (pp ^. ppMaxBHSizeL)
    , KeyDeposit (pp ^. ppKeyDepositL)
    , PoolDeposit (pp ^. ppPoolDepositL)
    , EMax (pp ^. ppEMaxL)
    , NOpt (pp ^. ppNOptL)
    , A0 (pp ^. ppA0L)
    , Rho (pp ^. ppRhoL)
    , Tau (pp ^. ppTauL)
    , ProtocolVersion (pp ^. ppProtocolVersionL)
    , MinPoolCost (pp ^. ppMinPoolCostL)
    , CoinPerUTxOByte (pp ^. ppCoinsPerUTxOByteL)
    , Costmdls (pp ^. ppCostModelsL)
    , Prices (pp ^. ppPricesL)
    , MaxTxExUnits (pp ^. ppMaxTxExUnitsL)
    , MaxBlockExUnits (pp ^. ppMaxBlockExUnitsL)
    , MaxValSize (pp ^. ppMaxValSizeL)
    , CollateralPercentage (pp ^. ppCollateralPercentageL)
    , MaxCollateralInputs (pp ^. ppMaxCollateralInputsL)
    , PoolVotingThreshold (pp ^. ppPoolVotingThresholdsL)
    , DRepVotingThreshold (pp ^. ppDRepVotingThresholdsL)
    , MinCommitteeSize (pp ^. ppCommitteeMinSizeL)
    , CommitteeTermLimit (pp ^. ppCommitteeMaxTermLengthL)
    , GovActionExpiration (pp ^. ppGovActionLifetimeL)
    , GovActionDeposit (pp ^. ppGovActionDepositL)
    , DRepDeposit (pp ^. ppDRepDepositL)
    , DRepActivity (pp ^. ppDRepActivityL)
    ]

-- =========================================================================
-- Era parametric "empty" or initial values.

initVI :: ValidityInterval
initVI = ValidityInterval SNothing SNothing

initWithdrawals :: Withdrawals c
initWithdrawals = Withdrawals Map.empty

initialTxBody :: Era era => Proof era -> TxBody era
initialTxBody (Shelley _) = mkBasicTxBody
initialTxBody (Allegra _) = mkBasicTxBody
initialTxBody (Mary _) = mkBasicTxBody
initialTxBody (Alonzo _) = mkBasicTxBody
initialTxBody (Babbage _) = mkBasicTxBody
initialTxBody (Conway _) = mkBasicTxBody

initialWitnesses :: Era era => Proof era -> TxWits era
initialWitnesses (Shelley _) = mkBasicTxWits
initialWitnesses (Allegra _) = mkBasicTxWits
initialWitnesses (Mary _) = mkBasicTxWits
initialWitnesses (Alonzo _) = mkBasicTxWits
initialWitnesses (Babbage _) = mkBasicTxWits
initialWitnesses (Conway _) = mkBasicTxWits

initialTx :: forall era. Proof era -> Tx era
initialTx era@(Shelley _) = mkBasicTx (initialTxBody era)
initialTx era@(Allegra _) = mkBasicTx (initialTxBody era)
initialTx era@(Mary _) = mkBasicTx (initialTxBody era)
initialTx era@(Alonzo _) = mkBasicTx (initialTxBody era)
initialTx era@(Babbage _) = mkBasicTx (initialTxBody era)
initialTx era@(Conway _) = mkBasicTx (initialTxBody era)

-- | A Meaningless Addr.
initialAddr :: Era era => Proof era -> Addr (EraCrypto era)
initialAddr _wit = Addr Testnet pCred sCred
  where
    (KeyPair svk _ssk) = theKeyPair 0
    pCred = KeyHashObj . hashKey . vKey $ theKeyPair 1
    sCred = StakeRefBase . KeyHashObj . hashKey $ svk

initialTxOut :: Era era => Proof era -> TxOut era
initialTxOut wit@(Shelley _) = mkBasicTxOut (initialAddr wit) mempty
initialTxOut wit@(Allegra _) = mkBasicTxOut (initialAddr wit) mempty
initialTxOut wit@(Mary _) = mkBasicTxOut (initialAddr wit) mempty
initialTxOut wit@(Alonzo _) = mkBasicTxOut (initialAddr wit) mempty
initialTxOut wit@(Babbage _) = mkBasicTxOut (initialAddr wit) mempty
initialTxOut wit@(Conway _) = mkBasicTxOut (initialAddr wit) mempty

-- ============================================================

abstractTx :: Proof era -> Tx era -> [TxField era]
abstractTx (Conway _) (AlonzoTx txBody wit v auxdata) =
  [Body txBody, TxWits wit, Valid v, AuxData auxdata]
abstractTx (Babbage _) (AlonzoTx txBody wit v auxdata) =
  [Body txBody, TxWits wit, Valid v, AuxData auxdata]
abstractTx (Alonzo _) (AlonzoTx txBody wit v auxdata) =
  [Body txBody, TxWits wit, Valid v, AuxData auxdata]
abstractTx (Shelley _) (ShelleyTx txBody wit auxdata) =
  [Body txBody, TxWits wit, AuxData auxdata]
abstractTx (Mary _) (ShelleyTx txBody wit auxdata) =
  [Body txBody, TxWits wit, AuxData auxdata]
abstractTx (Allegra _) (ShelleyTx txBody wit auxdata) =
  [Body txBody, TxWits wit, AuxData auxdata]

abstractTxBody :: Proof era -> TxBody era -> [TxBodyField era]
abstractTxBody (Alonzo _) (AlonzoTxBody inp col out cert wdrl fee vldt up req mnt sih adh net) =
  [ Inputs inp
  , Collateral col
  , Outputs out
  , Certs cert
  , Withdrawals' wdrl
  , Txfee fee
  , Vldt vldt
  , Update up
  , ReqSignerHashes req
  , Mint mnt
  , WppHash sih
  , AdHash adh
  , Txnetworkid net
  ]
abstractTxBody (Conway _) (ConwayTxBody inp col ref out colret totcol cert wdrl fee vldt req mnt sih adh net vp pp ctv td) =
  [ Inputs inp
  , Collateral col
  , RefInputs ref
  , Outputs (sizedValue <$> out)
  , CollateralReturn (sizedValue <$> colret)
  , TotalCol totcol
  , Certs cert
  , Withdrawals' wdrl
  , Txfee fee
  , Vldt vldt
  , ReqSignerHashes req
  , Mint mnt
  , WppHash sih
  , AdHash adh
  , Txnetworkid net
  , GovProcs $ GovProcedures vp pp
  , CurrentTreasuryValue ctv
  , TreasuryDonation td
  ]
abstractTxBody (Babbage _) (BabbageTxBody inp col ref out colret totcol cert wdrl fee vldt up req mnt sih adh net) =
  [ Inputs inp
  , Collateral col
  , RefInputs ref
  , Outputs (sizedValue <$> out)
  , CollateralReturn (sizedValue <$> colret)
  , TotalCol totcol
  , Certs cert
  , Withdrawals' wdrl
  , Txfee fee
  , Vldt vldt
  , Update up
  , ReqSignerHashes req
  , Mint mnt
  , WppHash sih
  , AdHash adh
  , Txnetworkid net
  ]
abstractTxBody (Shelley _) (ShelleyTxBody inp out cert wdrl fee ttlslot up adh) =
  [Inputs inp, Outputs out, Certs cert, Withdrawals' wdrl, Txfee fee, TTL ttlslot, Update up, AdHash adh]
abstractTxBody (Mary _) (MaryTxBody inp out cert wdrl fee vldt up adh mnt) =
  [Inputs inp, Outputs out, Certs cert, Withdrawals' wdrl, Txfee fee, Vldt vldt, Update up, AdHash adh, Mint mnt]
abstractTxBody (Allegra _) (AllegraTxBody inp out cert wdrl fee vldt up adh) =
  [Inputs inp, Outputs out, Certs cert, Withdrawals' wdrl, Txfee fee, Vldt vldt, Update up, AdHash adh]

abstractWitnesses :: Proof era -> TxWits era -> [WitnessesField era]
abstractWitnesses (Shelley _) (ShelleyTxWits keys scripts boot) = [AddrWits keys, ScriptWits scripts, BootWits boot]
abstractWitnesses (Allegra _) (ShelleyTxWits keys scripts boot) = [AddrWits keys, ScriptWits scripts, BootWits boot]
abstractWitnesses (Mary _) (ShelleyTxWits keys scripts boot) = [AddrWits keys, ScriptWits scripts, BootWits boot]
abstractWitnesses (Alonzo _) (AlonzoTxWits key boot scripts dats red) =
  [AddrWits key, ScriptWits scripts, BootWits boot, DataWits dats, RdmrWits red]
abstractWitnesses (Babbage _) (AlonzoTxWits key boot scripts dats red) =
  [AddrWits key, ScriptWits scripts, BootWits boot, DataWits dats, RdmrWits red]
abstractWitnesses (Conway _) (AlonzoTxWits key boot scripts dats red) =
  [AddrWits key, ScriptWits scripts, BootWits boot, DataWits dats, RdmrWits red]

abstractTxOut :: Era era => Proof era -> TxOut era -> [TxOutField era]
abstractTxOut (Shelley _) (ShelleyTxOut addr c) = [Address addr, Amount c]
abstractTxOut (Allegra _) (ShelleyTxOut addr c) = [Address addr, Amount c]
abstractTxOut (Mary _) (ShelleyTxOut addr val) = [Address addr, Amount val]
abstractTxOut (Alonzo _) (AlonzoTxOut addr val d) = [Address addr, Amount val, DHash d]
abstractTxOut (Babbage _) (BabbageTxOut addr val d refscr) =
  [Address addr, Amount val, FDatum d, RefScript refscr]
abstractTxOut (Conway _) (BabbageTxOut addr val d refscr) =
  [Address addr, Amount val, FDatum d, RefScript refscr]

-- =================================================================
-- coercion functions for defining Primed Field constructor patterns

valid :: IsValid -> Bool
valid (IsValid b) = b

toSet :: Ord a => [a] -> Set a
toSet = Set.fromList

fromSet :: Set a -> [a]
fromSet = Set.toList

toStrictSeq :: [a] -> StrictSeq a
toStrictSeq = SSeq.fromList

fromStrictSeq :: StrictSeq a -> [a]
fromStrictSeq s = foldr (:) [] s

toStrictMaybe :: [a] -> StrictMaybe a
toStrictMaybe [] = SNothing
toStrictMaybe [x] = SJust x
toStrictMaybe _xs = error "toStrictMaybe applied to list with 2 or more elements"

fromStrictMaybe :: StrictMaybe a -> [a]
fromStrictMaybe SNothing = []
fromStrictMaybe (SJust x) = [x]

-- Coercing from [T era] to (Map (Hash (T era)) (T era)), for different version of T that are Hashable

toMapDat :: Era era => [Data era] -> TxDats era
toMapDat ds = TxDats (Map.fromList (map (\d -> (hashData d, d)) ds))

fromMapScript :: forall era. Map (ScriptHash (EraCrypto era)) (Script era) -> [Script era]
fromMapScript m = Map.elems m

toMapScript :: forall era. EraScript era => [Script era] -> Map (ScriptHash (EraCrypto era)) (Script era)
toMapScript scripts = Map.fromList (map (\s -> (hashScript @era s, s)) scripts)

-- =============================================================================
-- Patterns (with primed names, like C') allow users to use [a], to stand
-- for (Set a) (Maybe a) (StrictSeq a) (StrictMaybe a) (Map (hash a) a)
-- The pattern signatures are just underneath the data declarations

-- ========================
-- TxBody patterns

netview :: TxBodyField era -> Maybe [Network]
netview (Txnetworkid x) = Just (fromStrictMaybe x)
netview _ = Nothing

pattern Txnetworkid' x <-
  (netview -> Just x)
  where
    Txnetworkid' x = Txnetworkid (toStrictMaybe x)

adhashview :: TxBodyField era -> Maybe [AuxiliaryDataHash (EraCrypto era)]
adhashview (AdHash x) = Just (fromStrictMaybe x)
adhashview _ = Nothing

pattern AdHash' x <-
  (adhashview -> Just x)
  where
    AdHash' x = AdHash (toStrictMaybe x)

wppview :: TxBodyField era -> Maybe [ScriptIntegrityHash (EraCrypto era)]
wppview (WppHash x) = Just (fromStrictMaybe x)
wppview _ = Nothing

pattern WppHash' x <-
  (wppview -> Just x)
  where
    WppHash' x = WppHash (toStrictMaybe x)

signview :: TxBodyField era -> Maybe [KeyHash 'Witness (EraCrypto era)]
signview (ReqSignerHashes x) = Just (fromSet x)
signview _ = Nothing

pattern ReqSignerHashes' x <-
  (signview -> Just x)
  where
    ReqSignerHashes' x = ReqSignerHashes (toSet x)

updateview :: TxBodyField era -> Maybe [PP.Update era]
updateview (Update x) = Just (fromStrictMaybe x)
updateview _ = Nothing

pattern Update' x <-
  (updateview -> Just x)
  where
    Update' x = Update (toStrictMaybe x)

certsview :: TxBodyField era -> Maybe [TxCert era]
certsview (Certs x) = Just (fromStrictSeq x)
certsview _ = Nothing

pattern Certs' x <-
  (certsview -> Just x)
  where
    Certs' x = Certs (toStrictSeq x)

colretview :: TxBodyField era -> Maybe [TxOut era]
colretview (CollateralReturn x) = Just (fromStrictMaybe x)
colretview _ = Nothing

pattern CollateralReturn' x <-
  (colretview -> Just x)
  where
    CollateralReturn' x = CollateralReturn (toStrictMaybe x)

outputview :: TxBodyField era -> Maybe [TxOut era]
outputview (Outputs x) = Just (fromStrictSeq x)
outputview _ = Nothing

pattern Outputs' x <-
  (outputview -> Just x)
  where
    Outputs' x = Outputs (toStrictSeq x)

inputsview :: TxBodyField era -> Maybe [TxIn (EraCrypto era)]
inputsview (Inputs x) = Just (fromSet x)
inputsview _ = Nothing

pattern Inputs' x <-
  (inputsview -> Just x)
  where
    Inputs' x = Inputs (toSet x)

colview :: TxBodyField era -> Maybe [TxIn (EraCrypto era)]
colview (Collateral x) = Just (fromSet x)
colview _ = Nothing

pattern Collateral' x <-
  (colview -> Just x)
  where
    Collateral' x = Collateral (toSet x)

refview :: TxBodyField era -> Maybe [TxIn (EraCrypto era)]
refview (RefInputs x) = Just (fromSet x)
refview _ = Nothing

pattern RefInputs' x <-
  (refview -> Just x)
  where
    RefInputs' x = RefInputs (toSet x)

-- =============================
-- Tx patterns

validview :: TxField era -> Maybe Bool
validview (Valid x) = Just (valid x)
validview _ = Nothing

pattern Valid' x <-
  (validview -> Just x)
  where
    Valid' x = Valid (IsValid x)

auxdataview :: TxField era -> Maybe [TxAuxData era]
auxdataview (AuxData x) = Just (fromStrictMaybe x)
auxdataview _ = Nothing

pattern AuxData' x <-
  (auxdataview -> Just x)
  where
    AuxData' x = AuxData (toStrictMaybe x)

-- =======================
-- WitnessesField Patterns

datawitsview :: forall era. Era era => WitnessesField era -> Maybe [Data era]
datawitsview (DataWits (TxDats x)) = Just (Map.elems x)
datawitsview _ = Nothing

pattern DataWits' x <-
  (datawitsview -> Just x)
  where
    DataWits' x = DataWits (toMapDat x)

scriptview :: forall era. WitnessesField era -> Maybe [Script era]
scriptview (ScriptWits x) = Just (fromMapScript @era x)
scriptview _ = Nothing

pattern ScriptWits' x <-
  (scriptview -> Just x)
  where
    ScriptWits' x = ScriptWits (toMapScript @era x)

addrview :: WitnessesField era -> Maybe [WitVKey 'Witness (EraCrypto era)]
addrview (AddrWits x) = Just (fromSet x)
addrview _ = Nothing

pattern AddrWits' x <-
  (addrview -> Just x)
  where
    AddrWits' x = AddrWits (toSet x)

bootview :: WitnessesField era -> Maybe [BootstrapWitness (EraCrypto era)]
bootview (BootWits x) = Just (fromSet x)
bootview _ = Nothing

pattern BootWits' x <-
  (bootview -> Just x)
  where
    BootWits' x = BootWits (toSet x)

-- ========================================
-- TxOut patterns

refscriptview :: TxOutField era -> Maybe [Script era]
refscriptview (RefScript x) = Just (fromStrictMaybe x)
refscriptview _ = Nothing

pattern RefScript' x <-
  (refscriptview -> Just x)
  where
    RefScript' x = RefScript (toStrictMaybe x)

dhashview :: TxOutField era -> Maybe [DataHash (EraCrypto era)]
dhashview (DHash x) = Just (fromStrictMaybe x)
dhashview _ = Nothing

pattern DHash' x <-
  (dhashview -> Just x)
  where
    DHash' x = DHash (toStrictMaybe x)
