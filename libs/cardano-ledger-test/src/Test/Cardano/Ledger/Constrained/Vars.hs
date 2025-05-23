{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}

module Test.Cardano.Ledger.Constrained.Vars where

import Cardano.Crypto.Signing (SigningKey)
import Cardano.Ledger.Address (Addr (..), Withdrawals (..))
import Cardano.Ledger.Allegra.Scripts (ValidityInterval (..))
import Cardano.Ledger.Alonzo.PParams (AlonzoEraPParams, ppCollateralPercentageL, ppMaxTxExUnitsL)
import Cardano.Ledger.Alonzo.Tx (IsValid (..), ScriptIntegrityHash)
import Cardano.Ledger.Alonzo.TxWits (TxDats (..))
import Cardano.Ledger.Alonzo.UTxO (AlonzoScriptsNeeded (..))
import Cardano.Ledger.BaseTypes (
  BlocksMade (..),
  EpochNo,
  Globals (..),
  Network (..),
  ProtVer (..),
  SlotNo (..),
  StrictMaybe (..),
  UnitInterval,
  knownNonZero,
  mulNonZero,
  toIntegerNonZero,
  (%.),
 )
import qualified Cardano.Ledger.BaseTypes as Base (EpochInterval (..), Globals (..))
import Cardano.Ledger.Coin (Coin (..), CompactForm (CompactCoin), DeltaCoin)
import Cardano.Ledger.Conway.Governance hiding (GovState)
import Cardano.Ledger.Conway.PParams (
  ConwayEraPParams,
  ppDRepActivityL,
  ppDRepDepositL,
  ppGovActionDepositL,
 )
import Cardano.Ledger.Conway.State (
  ChainAccountState (..),
  CommitteeAuthorization,
  CommitteeState (..),
  ConwayCertState (..),
  ConwayEraCertState (..),
  EraStake (..),
  IndividualPoolStake (..),
  PoolDistr (..),
  ShelleyInstantStake (..),
  SnapShot (..),
  SnapShots (..),
  Stake (..),
  VState (..),
  casReservesL,
  casTreasuryL,
  chainAccountStateL,
  conwayCertDStateL,
  conwayCertPStateL,
  conwayCertVStateL,
  csCommitteeCredsL,
  instantStakeL,
  poolDistrDistrL,
  reservesL,
  shelleyCertDStateL,
  shelleyCertPStateL,
  treasuryL,
  vsCommitteeStateL,
  vsDRepsL,
  vsNumDormantEpochsL,
 )
import Cardano.Ledger.Core (
  Era,
  PParams,
  TxOut,
  TxWits,
  Value,
  addrTxOutL,
  coinTxOutL,
  ppEMaxL,
  ppKeyDepositL,
  ppMaxBBSizeL,
  ppMaxBHSizeL,
  ppMaxTxSizeL,
  ppMinFeeAL,
  ppMinFeeBL,
  ppPoolDepositL,
  ppProtocolVersionL,
  valueTxOutL,
 )
import Cardano.Ledger.Credential (Credential, Ptr)
import Cardano.Ledger.DRep (DRep (..), DRepState (..))
import Cardano.Ledger.Hashes (
  DataHash,
  EraIndependentScriptIntegrity,
  SafeHash,
  ScriptHash (..),
  TxAuxDataHash (..),
 )
import Cardano.Ledger.Keys (GenDelegPair, GenDelegs (..), KeyHash, KeyRole (..))
import Cardano.Ledger.Keys.Bootstrap (BootstrapWitness)
import Cardano.Ledger.Keys.WitVKey (WitVKey (..))
import Cardano.Ledger.Mary.Value (AssetName (..), MaryValue (..), MultiAsset (..), PolicyID (..))
import Cardano.Ledger.Plutus (ExUnits (..))
import Cardano.Ledger.Plutus.Data (Data (..), Datum (..))
import Cardano.Ledger.PoolParams (PoolParams)
import Cardano.Ledger.Shelley.Governance (FuturePParams (..))
import qualified Cardano.Ledger.Shelley.Governance as Gov
import Cardano.Ledger.Shelley.HardForks as HardForks (allowMIRTransfer)
import Cardano.Ledger.Shelley.LedgerState hiding (
  delegations,
  deltaReserves,
  deltaTreasury,
  rewards,
 )
import Cardano.Ledger.Shelley.PParams (ProposedPPUpdates (..))
import Cardano.Ledger.Shelley.PoolRank (NonMyopic (..))
import qualified Cardano.Ledger.Shelley.RewardUpdate as RU
import Cardano.Ledger.Shelley.Rewards (Reward (..))
import Cardano.Ledger.Shelley.TxBody (RewardAccount (..))
import Cardano.Ledger.Shelley.UTxO (EraUTxO (..), ShelleyScriptsNeeded (..))
import Cardano.Ledger.TxIn (TxIn (..))
import Cardano.Ledger.UMap (compactCoinOrError, fromCompact, ptrMap, rdPairMap, sPoolMap, unify)
import Cardano.Ledger.Val (Val (..))
import Control.Arrow (first)
import Data.Default (Default (def))
import Data.Foldable (toList)
import qualified Data.Foldable as F
import Data.Functor.Identity (Identity)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe.Strict (maybeToStrictMaybe, strictMaybeToMaybe)
import qualified Data.OMap.Strict as OMap
import qualified Data.Sequence.Strict as SS
import Data.Set (Set)
import qualified Data.VMap as VMap
import Data.Word (Word16, Word32, Word64)
import GHC.Stack (HasCallStack)
import Lens.Micro
import Numeric.Natural (Natural)
import Test.Cardano.Ledger.Babbage.Serialisation.Generators ()
import Test.Cardano.Ledger.Constrained.Ast
import Test.Cardano.Ledger.Constrained.Classes (
  CertStateF (..),
  GovState (..),
  PParamsF (..),
  PParamsUpdateF (..),
  PlutusPointerF (..),
  PlutusPurposeF (..),
  ScriptF (..),
  ScriptsNeededF (..),
  TxAuxDataF (..),
  TxBodyF (..),
  TxCertF (..),
  TxF (..),
  TxOutF (..),
  TxWitsF (..),
  ValueF (..),
  liftUTxO,
  pparamsWrapperL,
  unCertStateF,
  unPParamsUpdate,
  unPlutusPointerF,
  unPlutusPurposeF,
  unScriptF,
  unTxCertF,
  unTxOut,
  unValue,
 )
import Test.Cardano.Ledger.Constrained.Env (
  Access (..),
  AnyF (..),
  Field (..),
  Name (..),
  V (..),
  pV,
 )
import Test.Cardano.Ledger.Constrained.Lenses
import Test.Cardano.Ledger.Constrained.TypeRep (Rep (..), testEql, (:~:) (Refl))
import Test.Cardano.Ledger.Core.KeyPair (KeyPair (..))
import Test.Cardano.Ledger.Generic.Fields (TxBodyField (..), TxField (..), WitnessesField (..))
import qualified Test.Cardano.Ledger.Generic.Fields as Fields
import Test.Cardano.Ledger.Generic.Functions (protocolVersion)
import Test.Cardano.Ledger.Generic.GenState (mkRedeemers)
import Test.Cardano.Ledger.Generic.PrettyCore (ppString, withEraPParams)
import Test.Cardano.Ledger.Generic.Proof
import Test.Cardano.Ledger.Generic.Updaters (merge, newPParams, newTx, newTxBody, newWitnesses)
import Test.Cardano.Ledger.Shelley.Utils (testGlobals)
import qualified Test.Cardano.Ledger.Shelley.Utils as Utils (testGlobals)
import Type.Reflection (Typeable, typeRep)

-- =======================

-- | Used in Component constraints to turn a Var Term into a component (AnyF era s)
-- E.g.  (Component foo [ field fooRep fooPart1, field fooRep fooPart2])
-- Where fooPart1 :: Term era a, and fooPart2 :: Term era b
-- And fooPart1 has an (Access foo a)
-- And fooPart2 has an (Access foo b)
field :: Era era => Rep era s -> Term era t -> AnyF era s
field repS1 (Var (V name rept (Yes repS2 l))) = case testEql repS1 repS2 of
  Just Refl -> AnyF (Field name rept repS2 l)
  Nothing ->
    error
      ( unlines
          [ "Given rep and lens target do not match: "
          , "rep: " ++ show repS1
          , "lens target: " ++ show repS2
          ]
      )
field _ term = error ("field can only be applied to variable terms: " ++ show term)

getName :: Term era t -> Name era
getName (Var v) = Name v
getName x = error ("nameOf can't find the name in: " ++ show x)

-- ==============================================================
-- NewEpochState fields

type NELens era t = Lens' (NewEpochState era) t

currentEpoch :: Era era => Term era EpochNo
currentEpoch = Var (V "currentEpoch" EpochR (Yes NewEpochStateR nesELL))

prevBlocksMade :: Era era => Term era (Map (KeyHash 'StakePool) Natural)
prevBlocksMade = Var $ V "prevBlocksMade" (MapR PoolHashR NaturalR) (Yes NewEpochStateR nesBprevL)

currBlocksMade :: Era era => Term era (Map (KeyHash 'StakePool) Natural)
currBlocksMade = Var $ V "currBlocksMade" (MapR PoolHashR NaturalR) (Yes NewEpochStateR nesBcurL)

poolDistr ::
  Era era => Term era (Map (KeyHash 'StakePool) IndividualPoolStake)
poolDistr = Var $ V "poolDistr" (MapR PoolHashR IPoolStakeR) (Yes NewEpochStateR poolDistrL)

-- | For tests only, Like PoolDistr but has a Rational (rather than a IndividualPoolStake).
mockPoolDistr :: Era era => Term era (Map (KeyHash 'StakePool) Rational)
mockPoolDistr = Var $ V "mockPoolDistr" (MapR PoolHashR RationalR) No

poolDistrL ::
  NELens era (Map (KeyHash 'StakePool) IndividualPoolStake)
poolDistrL = nesPdL . poolDistrDistrL

-- CertState - DState

rewards :: EraCertState era => Term era (Map (Credential 'Staking) Coin)
rewards = Var $ V "rewards" (MapR CredR CoinR) (Yes NewEpochStateR rewardsL)

rewardsL :: EraCertState era => NELens era (Map (Credential 'Staking) Coin)
rewardsL = nesEsL . esLStateL . lsCertStateL . certDStateL . dsUnifiedL . rewardsUMapL

delegations ::
  EraCertState era => Term era (Map (Credential 'Staking) (KeyHash 'StakePool))
delegations = Var $ V "delegations" (MapR CredR PoolHashR) (Yes NewEpochStateR delegationsL)

delegationsL ::
  EraCertState era => NELens era (Map (Credential 'Staking) (KeyHash 'StakePool))
delegationsL = nesEsL . esLStateL . lsCertStateL . certDStateL . dsUnifiedL . delegationsUMapL

stakeDeposits :: EraCertState era => Term era (Map (Credential 'Staking) Coin)
stakeDeposits = Var $ V "stakeDeposits" (MapR CredR CoinR) (Yes NewEpochStateR stakeDepositsL)

stakeDepositsL :: EraCertState era => NELens era (Map (Credential 'Staking) Coin)
stakeDepositsL = nesEsL . esLStateL . lsCertStateL . certDStateL . dsUnifiedL . stakeDepositsUMapL

ptrs :: EraCertState era => Term era (Map Ptr (Credential 'Staking))
ptrs = Var $ V "ptrs" (MapR PtrR CredR) (Yes NewEpochStateR ptrsL)

ptrsL :: EraCertState era => NELens era (Map Ptr (Credential 'Staking))
ptrsL = nesEsL . esLStateL . lsCertStateL . certDStateL . dsUnifiedL . ptrsUMapL

currentDRepState ::
  ConwayEraCertState era => Term era (Map (Credential 'DRepRole) DRepState)
currentDRepState = Var $ V "currentDRepState" (MapR VCredR DRepStateR) (Yes NewEpochStateR drepsL)

drepsL :: ConwayEraCertState era => NELens era (Map (Credential 'DRepRole) DRepState)
drepsL = nesEsL . esLStateL . lsCertStateL . certVStateL . vsDRepsL

drepDelegation ::
  EraCertState era => Term era (Map (Credential 'Staking) DRep)
drepDelegation = Var $ V "drepDelegation" (MapR CredR DRepR) (Yes NewEpochStateR drepDelegationL)

drepDelegationL :: EraCertState era => NELens era (Map (Credential 'Staking) DRep)
drepDelegationL = nesEsL . esLStateL . lsCertStateL . certDStateL . dsUnifiedL . drepUMapL

futureGenDelegs ::
  EraCertState era => Term era (Map FutureGenDeleg GenDelegPair)
futureGenDelegs =
  Var $
    V
      "futureGenDelegs"
      (MapR FutureGenDelegR GenDelegPairR)
      (Yes NewEpochStateR futureGenDelegsL)

futureGenDelegsL :: EraCertState era => NELens era (Map FutureGenDeleg GenDelegPair)
futureGenDelegsL = nesEsL . esLStateL . lsCertStateL . certDStateL . dsFutureGenDelegsL

genDelegs ::
  EraCertState era => Term era (Map (KeyHash 'Genesis) GenDelegPair)
genDelegs = Var $ V "genDelegs" (MapR GenHashR GenDelegPairR) (Yes NewEpochStateR genDelegsL)

genDelegsL :: EraCertState era => NELens era (Map (KeyHash 'Genesis) GenDelegPair)
genDelegsL = nesEsL . esLStateL . lsCertStateL . certDStateL . dsGenDelegsL . unGenDelegsL

-- DState - InstantaneousRewards

instanReserves :: EraCertState era => Term era (Map (Credential 'Staking) Coin)
instanReserves = Var $ V "instanReserves" (MapR CredR CoinR) (Yes NewEpochStateR instanReservesL)

instanReservesL :: EraCertState era => NELens era (Map (Credential 'Staking) Coin)
instanReservesL = nesEsL . esLStateL . lsCertStateL . certDStateL . dsIRewardsL . iRReservesL

instanReservesSum :: Era era => Term era Coin
instanReservesSum = Var (V "instanReservesSum" CoinR No)

instanTreasury :: EraCertState era => Term era (Map (Credential 'Staking) Coin)
instanTreasury = Var $ V "instanTreasury" (MapR CredR CoinR) (Yes NewEpochStateR instanTreasuryL)

instanTreasuryL :: EraCertState era => NELens era (Map (Credential 'Staking) Coin)
instanTreasuryL = nesEsL . esLStateL . lsCertStateL . certDStateL . dsIRewardsL . iRTreasuryL

instanTreasurySum :: Era era => Term era Coin
instanTreasurySum = Var (V "instanTreasurySum" CoinR No)

deltaReserves :: EraCertState era => Term era DeltaCoin
deltaReserves = Var $ V "deltaReserves" DeltaCoinR (Yes NewEpochStateR deltaReservesNEL)

deltaReservesNEL :: EraCertState era => NELens era DeltaCoin
deltaReservesNEL = nesEsL . esLStateL . lsCertStateL . certDStateL . dsIRewardsL . deltaReservesL

deltaTreasury :: EraCertState era => Term era DeltaCoin
deltaTreasury = Var $ V "deltaTreasury" DeltaCoinR (Yes NewEpochStateR deltaTreasuryNEL)

deltaTreasuryNEL :: EraCertState era => NELens era DeltaCoin
deltaTreasuryNEL = nesEsL . esLStateL . lsCertStateL . certDStateL . dsIRewardsL . deltaTreasuryL

-- CertState - PState

regPools :: EraCertState era => Term era (Map (KeyHash 'StakePool) PoolParams)
regPools = Var $ V "regPools" (MapR PoolHashR PoolParamsR) (Yes NewEpochStateR regPoolsL)

regPoolsL :: EraCertState era => NELens era (Map (KeyHash 'StakePool) PoolParams)
regPoolsL = nesEsL . esLStateL . lsCertStateL . certPStateL . psStakePoolParamsL

futureRegPools :: EraCertState era => Term era (Map (KeyHash 'StakePool) PoolParams)
futureRegPools = Var $ V "futureRegPools" (MapR PoolHashR PoolParamsR) (Yes NewEpochStateR futureRegPoolsL)

futureRegPoolsL :: EraCertState era => NELens era (Map (KeyHash 'StakePool) PoolParams)
futureRegPoolsL = nesEsL . esLStateL . lsCertStateL . certPStateL . psFutureStakePoolParamsL

retiring :: EraCertState era => Term era (Map (KeyHash 'StakePool) EpochNo)
retiring = Var $ V "retiring" (MapR PoolHashR EpochR) (Yes NewEpochStateR retiringL)

retiringL :: EraCertState era => NELens era (Map (KeyHash 'StakePool) EpochNo)
retiringL = nesEsL . esLStateL . lsCertStateL . certPStateL . psRetiringL

poolDeposits :: EraCertState era => Term era (Map (KeyHash 'StakePool) Coin)
poolDeposits = Var $ V "poolDeposits" (MapR PoolHashR CoinR) (Yes NewEpochStateR poolDepositsL)

poolDepositsL :: EraCertState era => NELens era (Map (KeyHash 'StakePool) Coin)
poolDepositsL = nesEsL . esLStateL . lsCertStateL . certPStateL . psDepositsL

committeeState ::
  ConwayEraCertState era =>
  Term era (Map (Credential 'ColdCommitteeRole) CommitteeAuthorization)
committeeState =
  Var $
    V "committeeState" (MapR CommColdCredR CommitteeAuthorizationR) (Yes NewEpochStateR committeeStateL)

committeeStateL ::
  ConwayEraCertState era => NELens era (Map (Credential 'ColdCommitteeRole) CommitteeAuthorization)
committeeStateL = nesEsL . esLStateL . lsCertStateL . certVStateL . vsCommitteeStateL . csCommitteeCredsL

numDormantEpochs :: ConwayEraCertState era => Term era EpochNo
numDormantEpochs = Var $ V "numDormantEpochs" EpochR (Yes NewEpochStateR numDormantEpochsL)

numDormantEpochsL :: ConwayEraCertState era => NELens era EpochNo
numDormantEpochsL = nesEsL . esLStateL . lsCertStateL . certVStateL . vsNumDormantEpochsL

-- UTxOState

utxo :: Era era => Proof era -> Term era (Map TxIn (TxOutF era))
utxo p = Var $ pV p "utxo" (MapR TxInR (TxOutR p)) (Yes NewEpochStateR (utxoL' p))

utxoL' :: Proof era -> NELens era (Map TxIn (TxOutF era))
utxoL' proof = utxoL . unUtxoL proof

unUtxoL :: Proof era -> Lens' (UTxO era) (Map TxIn (TxOutF era))
unUtxoL p = lens (Map.map (TxOutF p) . unUTxO) (\(UTxO _) new -> liftUTxO new)

deposits :: Era era => Term era Coin
deposits = Var $ V "deposits" CoinR (Yes NewEpochStateR depositsL)

depositsL :: NELens era Coin
depositsL = nesEsL . esLStateL . lsUTxOStateL . utxosDepositedL

fees :: Era era => Term era Coin
fees = Var $ V "fees" CoinR (Yes NewEpochStateR feesL)

feesL :: NELens era Coin
feesL = nesEsL . esLStateL . lsUTxOStateL . utxosFeesL

donation :: Era era => Term era Coin
donation = Var $ V "donation" CoinR (Yes NewEpochStateR donationL)

donationL :: NELens era Coin
donationL = nesEsL . esLStateL . lsUTxOStateL . utxosDonationL

ppup :: Era era => Proof era -> Term era (ShelleyGovState era)
ppup p = Var $ pV p "ppup" (PPUPStateR p) (Yes NewEpochStateR (ppupsL p))

ppupsL :: Proof era -> NELens era (ShelleyGovState era)
ppupsL Shelley = nesEsL . esLStateL . lsUTxOStateL . utxosGovStateL
ppupsL Allegra = nesEsL . esLStateL . lsUTxOStateL . utxosGovStateL
ppupsL Mary = nesEsL . esLStateL . lsUTxOStateL . utxosGovStateL
ppupsL Alonzo = nesEsL . esLStateL . lsUTxOStateL . utxosGovStateL
ppupsL Babbage = nesEsL . esLStateL . lsUTxOStateL . utxosGovStateL
ppupsL Conway = error "Conway era does not have a PPUPState, in ppupsL"

pparamProposals ::
  Era era => Proof era -> Term era (Map (KeyHash 'Genesis) (PParamsUpdateF era))
pparamProposals p = Var (pV p "pparamProposals" (MapR GenHashR (PParamsUpdateR p)) No)

futurePParamProposals ::
  Era era => Proof era -> Term era (Map (KeyHash 'Genesis) (PParamsUpdateF era))
futurePParamProposals p = Var (pV p "futurePParamProposals" (MapR GenHashR (PParamsUpdateR p)) No)

currPParams :: Era era => Proof era -> Term era (PParamsF era)
currPParams p = Var (pV p "currPParams" (PParamsR p) No)

futurePParams :: Era era => Proof era -> Term era (FuturePParams era)
futurePParams p = Var (pV p "futurePParams" (FuturePParamsR p) No)

prevPParams :: Gov.EraGov era => Proof era -> Term era (PParamsF era)
prevPParams p =
  Var (V "prevPParams" (PParamsR p) (Yes NewEpochStateR (nesEsL . prevPParamsEpochStateL . ppFL p)))

proposalsL :: Lens' (ShelleyGovState era) (ProposedPPUpdates era)
proposalsL = lens sgsCurProposals (\sgov x -> sgov {sgsCurProposals = x})

futureProposalsL :: Lens' (ShelleyGovState era) (ProposedPPUpdates era)
futureProposalsL = lens sgsFutureProposals (\sgov x -> sgov {sgsFutureProposals = x})

ppupStateT ::
  forall era.
  ( Gov.GovState era ~ ShelleyGovState era
  , Gov.EraGov era
  ) =>
  Proof era ->
  RootTarget era (ShelleyGovState era) (ShelleyGovState era)
ppupStateT p =
  Invert "PPUPState" (typeRep @(ShelleyGovState era)) ppupfun
    :$ Lensed (pparamProposals p) (proposalsL . proposedMapL p)
    :$ Lensed (futurePParamProposals p) (futureProposalsL . proposedMapL p)
    :$ Lensed (currPParams p) (Gov.curPParamsGovStateL . pparamsFL p)
    :$ Lensed (prevPParams p) (Gov.prevPParamsGovStateL . pparamsFL p)
    :$ Lensed (futurePParams p) (Gov.futurePParamsGovStateL)
  where
    ppupfun x y (PParamsF _ pp) (PParamsF _ prev) z =
      ShelleyGovState
        (ProposedPPUpdates (Map.map unPParamsUpdate x))
        (ProposedPPUpdates (Map.map unPParamsUpdate y))
        pp
        prev
        z

govL :: Lens' (GovState era) (Gov.GovState era)
govL = lens f g
  where
    f :: GovState era -> Gov.GovState era
    f (GovState Shelley x) = x
    f (GovState Allegra x) = x
    f (GovState Mary x) = x
    f (GovState Alonzo x) = x
    f (GovState Babbage x) = x
    f (GovState Conway x) = x
    g :: GovState era -> Gov.GovState era -> GovState era
    g (GovState p@Shelley _) y = GovState p y
    g (GovState p@Allegra _) y = GovState p y
    g (GovState p@Mary _) y = GovState p y
    g (GovState p@Alonzo _) y = GovState p y
    g (GovState p@Babbage _) y = GovState p y
    g (GovState p@Conway _) y = GovState p y

govStateT :: forall era. Era era => Proof era -> RootTarget era (GovState era) (GovState era)
govStateT p@Shelley = Invert "GovState" (typeRep @(GovState era)) (GovState p) :$ Shift (ppupStateT p) govL
govStateT p@Allegra = Invert "GovState" (typeRep @(GovState era)) (GovState p) :$ Shift (ppupStateT p) govL
govStateT p@Mary = Invert "GovState" (typeRep @(GovState era)) (GovState p) :$ Shift (ppupStateT p) govL
govStateT p@Alonzo = Invert "GovState" (typeRep @(GovState era)) (GovState p) :$ Shift (ppupStateT p) govL
govStateT p@Babbage = Invert "GovState" (typeRep @(GovState era)) (GovState p) :$ Shift (ppupStateT p) govL
govStateT p@Conway =
  Invert "GovState" (typeRep @(GovState era)) (GovState p) :$ Shift (unReflect conwayGovStateT p) govL

individualPoolStakeL :: Lens' IndividualPoolStake Rational
individualPoolStakeL = lens individualPoolStake (\ds u -> ds {individualPoolStake = u})

-- Incremental Stake

isPtrMapT :: (Era era, InstantStake era ~ ShelleyInstantStake era) => Term era (Map Ptr Coin)
isPtrMapT = Var $ V "ptrMap" (MapR PtrR CoinR) (Yes NewEpochStateR ptrMapL)

ptrMapL :: InstantStake era ~ ShelleyInstantStake era => Lens' (NewEpochState era) (Map Ptr Coin)
ptrMapL = instantStakeL . isPtrMapL

isCredMapT :: EraStake era => Term era (Map (Credential 'Staking) Coin)
isCredMapT = Var $ V "credMap" (MapR CredR CoinR) (Yes NewEpochStateR credMapL)

credMapL :: EraStake era => Lens' (NewEpochState era) (Map (Credential 'Staking) Coin)
credMapL = instantStakeL . isCredMapL

-- | This variable is computed from the UTxO and the PParams,
--   It represents the instant stake that is computed by 'smartUTxO'
--   in the UTxOState Target UTxOStateT
--   The domain of this map is the complete set of credentials used to delegate Coin
--   in the TxOuts in the UTxO.
instantStakeTerm :: Era era => Term era (Map (Credential 'Staking) Coin)
instantStakeTerm = Var $ V "instantStake" (MapR CredR CoinR) No

instantStakeTarget ::
  Reflect era => Proof era -> Target era (Map (Credential 'Staking) Coin)
instantStakeTarget proof = Constr "computeInstantStake" get ^$ utxo proof
  where
    get utxom =
      let instantStakeMap = addInstantStake (liftUTxO utxom) mempty ^. instantStakeCredentialsL
       in Map.map fromCompact instantStakeMap

-- ==========================
-- ChainAccountState

treasury :: Era era => Term era Coin
treasury = Var $ V "treasury" CoinR (Yes NewEpochStateR treasuryL)

reserves :: Era era => Term era Coin
reserves = Var $ V "reserves" CoinR (Yes NewEpochStateR reservesL)

-- | The Coin availabe for a MIR transfer to/from the Treasury
--   Computed from 'treasury' + 'deltaTreasury' - sum('instanTreasury')
mirAvailTreasury :: Era era => Term era Coin
mirAvailTreasury = Var (V "mirAvailTreasury" CoinR No)

-- | The Coin availabe for a MIR transfer to/from the Reserves
--   Computed from 'reserves' + 'deltaReserves' - sum('instanReserves')
mirAvailReserves :: Era era => Term era Coin
mirAvailReserves = Var (V "mirAvailReserves" CoinR No)

-- EpochState

snapshots :: Era era => Term era SnapShots
snapshots = Var (V "snapshots" SnapShotsR (Yes NewEpochStateR snapshotsL))

snapshotsL :: NELens era SnapShots
snapshotsL = nesEsL . esSnapshotsL

-- TODO: remove this duplication of `pparamsFL`

-- | Lens' from the Core PParams to the Model PParamsF which embeds a (Proof era)
ppFL :: Proof era -> Lens' (PParams era) (PParamsF era)
ppFL p = lens (\pp -> PParamsF p pp) (\_ (PParamsF _ qq) -> qq)

pparamsVar :: Gov.EraGov era => Proof era -> V era (PParamsF era)
pparamsVar p = (V "pparams" (PParamsR p) (Yes NewEpochStateR (nesEsL . curPParamsEpochStateL . ppFL p)))

pparams :: Gov.EraGov era => Proof era -> Term era (PParamsF era)
pparams p = Var $ pparamsVar p

nmLikelihoodsT :: Era era => Term era (Map (KeyHash 'StakePool) [Float])
nmLikelihoodsT =
  Var
    ( V
        "likelihoodsNM"
        (MapR PoolHashR (ListR FloatR))
        (Yes NewEpochStateR (nesEsL . esNonMyopicL . nmLikelihoodsL))
    )

nmRewardPotT :: Era era => Term era Coin
nmRewardPotT = Var $ V "rewardPotNM" CoinR (Yes NewEpochStateR (nesEsL . esNonMyopicL . nmRewardPotL))

-- ===== SnapShots

-- | Helper lens that deals with the Stake newtype, and the shift from Map to VMap
stakeL :: Lens' Stake (Map (Credential 'Staking) Coin)
stakeL =
  lens
    (Map.map fromCompact . VMap.toMap . unStake)
    (\_ u -> Stake . VMap.fromMap . Map.map compactCoinOrError $ u)

-- | Helper lens that deals with the shift from Map to VMap
vmapL :: Lens' (VMap.VMap VMap.VB VMap.VB k v) (Map k v)
vmapL = lens VMap.toMap (\_ u -> VMap.fromMap u)

markStakeL :: NELens era (Map (Credential 'Staking) Coin)
markStakeL = nesEsL . esSnapshotsL . ssStakeMarkL . ssStakeL . stakeL

markStake :: Era era => Term era (Map (Credential 'Staking) Coin)
markStake = Var (V "markStake" (MapR CredR CoinR) (Yes NewEpochStateR markStakeL))

markDelegs ::
  Era era => Term era (Map (Credential 'Staking) (KeyHash 'StakePool))
markDelegs = Var (V "markDelegs" (MapR CredR PoolHashR) (Yes NewEpochStateR markDelegsL))

markDelegsL ::
  NELens era (Map (Credential 'Staking) (KeyHash 'StakePool))
markDelegsL = nesEsL . esSnapshotsL . ssStakeMarkL . ssDelegationsL . vmapL

markPools ::
  Era era => Term era (Map (KeyHash 'StakePool) PoolParams)
markPools = Var (V "markPools" (MapR PoolHashR PoolParamsR) (Yes NewEpochStateR markPoolsL))

markPoolsL :: NELens era (Map (KeyHash 'StakePool) PoolParams)
markPoolsL = nesEsL . esSnapshotsL . ssStakeMarkL . ssPoolParamsL . vmapL

markSnapShotT ::
  forall era. Era era => RootTarget era SnapShot SnapShot
markSnapShotT =
  Invert "SnapShot" (typeRep @SnapShot) snapfun
    :$ Lensed markStake (ssStakeL . stakeL)
    :$ Lensed markDelegs (ssDelegationsL . vmapL)
    :$ Lensed markPools (ssPoolParamsL . vmapL)
  where
    snapfun x y z =
      SnapShot
        (Stake (VMap.fromMap (Map.map compactCoinOrError x)))
        (VMap.fromMap y)
        (VMap.fromMap z)

setStake :: Era era => Term era (Map (Credential 'Staking) Coin)
setStake = Var (V "setStake" (MapR CredR CoinR) (Yes NewEpochStateR setStakeL))

setStakeL :: NELens era (Map (Credential 'Staking) Coin)
setStakeL = nesEsL . esSnapshotsL . ssStakeSetL . ssStakeL . stakeL

setDelegs ::
  Era era => Term era (Map (Credential 'Staking) (KeyHash 'StakePool))
setDelegs = Var (V "setDelegs" (MapR CredR PoolHashR) (Yes NewEpochStateR setDelegsL))

setDelegsL ::
  NELens era (Map (Credential 'Staking) (KeyHash 'StakePool))
setDelegsL = nesEsL . esSnapshotsL . ssStakeSetL . ssDelegationsL . vmapL

setPools ::
  Era era => Term era (Map (KeyHash 'StakePool) PoolParams)
setPools = Var (V "setPools" (MapR PoolHashR PoolParamsR) (Yes NewEpochStateR setPoolsL))

setPoolsL :: NELens era (Map (KeyHash 'StakePool) PoolParams)
setPoolsL = nesEsL . esSnapshotsL . ssStakeSetL . ssPoolParamsL . vmapL

setSnapShotT ::
  forall era. Era era => RootTarget era SnapShot SnapShot
setSnapShotT =
  Invert "SnapShot" (typeRep @SnapShot) snapfun
    :$ Lensed setStake (ssStakeL . stakeL)
    :$ Lensed setDelegs (ssDelegationsL . vmapL)
    :$ Lensed setPools (ssPoolParamsL . vmapL)
  where
    snapfun x y z =
      SnapShot
        (Stake (VMap.fromMap (Map.map compactCoinOrError x)))
        (VMap.fromMap y)
        (VMap.fromMap z)

goStake :: Era era => Term era (Map (Credential 'Staking) Coin)
goStake = Var (V "goStake" (MapR CredR CoinR) (Yes NewEpochStateR goStakeL))

goStakeL :: NELens era (Map (Credential 'Staking) Coin)
goStakeL = nesEsL . esSnapshotsL . ssStakeGoL . ssStakeL . stakeL

goDelegs ::
  Era era => Term era (Map (Credential 'Staking) (KeyHash 'StakePool))
goDelegs = Var (V "goDelegs" (MapR CredR PoolHashR) (Yes NewEpochStateR goDelegsL))

goDelegsL ::
  NELens era (Map (Credential 'Staking) (KeyHash 'StakePool))
goDelegsL = nesEsL . esSnapshotsL . ssStakeGoL . ssDelegationsL . vmapL

goPools ::
  Era era => Term era (Map (KeyHash 'StakePool) PoolParams)
goPools = Var (V "goPools" (MapR PoolHashR PoolParamsR) (Yes NewEpochStateR goPoolsL))

goPoolsL :: NELens era (Map (KeyHash 'StakePool) PoolParams)
goPoolsL = nesEsL . esSnapshotsL . ssStakeGoL . ssPoolParamsL . vmapL

goSnapShotT ::
  forall era. Era era => RootTarget era SnapShot SnapShot
goSnapShotT =
  Invert "SnapShot" (typeRep @SnapShot) snapfun
    :$ Lensed goStake (ssStakeL . stakeL)
    :$ Lensed goDelegs (ssDelegationsL . vmapL)
    :$ Lensed goPools (ssPoolParamsL . vmapL)
  where
    snapfun x y z =
      SnapShot
        (Stake (VMap.fromMap (Map.map compactCoinOrError x)))
        (VMap.fromMap y)
        (VMap.fromMap z)

markPoolDistr ::
  Era era => Term era (Map (KeyHash 'StakePool) IndividualPoolStake)
markPoolDistr = Var (V "markPoolDistr" (MapR PoolHashR IPoolStakeR) No)

markPoolDistrL ::
  NELens era (Map (KeyHash 'StakePool) IndividualPoolStake)
markPoolDistrL = nesEsL . esSnapshotsL . ssStakeMarkPoolDistrL . poolDistrDistrL

snapShotFee :: Era era => Term era Coin
snapShotFee = Var (V "snapShotFee" CoinR No)

snapShotsT ::
  forall era. Era era => RootTarget era SnapShots SnapShots
snapShotsT =
  Invert "SnapShots" (typeRep @SnapShots) shotsfun
    :$ Shift markSnapShotT ssStakeMarkL
    :$ Lensed markPoolDistr (ssStakeMarkPoolDistrL . poolDistrDistrL)
    :$ Shift setSnapShotT ssStakeSetL
    :$ Shift goSnapShotT ssStakeGoL
    :$ Lensed snapShotFee ssFeeL
  where
    shotsfun w x = SnapShots w (PoolDistr x $ CompactCoin 1)

-- ==================================================================
-- RewardUpdate

deltaT :: Era era => Term era (Maybe DeltaCoin)
deltaT = Var (V "deltaT" (MaybeR DeltaCoinR) (Yes NewEpochStateR deltaTL))

deltaTL :: NELens era (Maybe DeltaCoin)
deltaTL = nesRuL . help
  where
    help :: Lens' (StrictMaybe PulsingRewUpdate) (Maybe DeltaCoin)
    help = lens view update
      where
        view SNothing = Nothing
        view (SJust (Complete x)) = Just (RU.deltaT x)
        view (SJust _) = Nothing
        update (SJust (Complete ru)) (Just change) = SJust (Complete (ru {RU.deltaT = change}))
        update _ _ = SNothing

deltaR :: Era era => Term era (Maybe DeltaCoin)
deltaR = Var (V "deltaR" (MaybeR DeltaCoinR) (Yes NewEpochStateR deltaRL))

deltaRL :: NELens era (Maybe DeltaCoin)
deltaRL = nesRuL . help
  where
    help :: Lens' (StrictMaybe PulsingRewUpdate) (Maybe DeltaCoin)
    help = lens view update
      where
        view SNothing = Nothing
        view (SJust (Complete x)) = Just (RU.deltaR x)
        view (SJust _) = Nothing
        update (SJust (Complete ru)) (Just change) = SJust (Complete (ru {RU.deltaR = change}))
        update _ _ = SNothing

deltaF :: Era era => Term era (Maybe DeltaCoin)
deltaF = Var (V "deltaF" (MaybeR DeltaCoinR) (Yes NewEpochStateR deltaFL))

deltaFL :: NELens era (Maybe DeltaCoin)
deltaFL = nesRuL . help
  where
    help :: Lens' (StrictMaybe PulsingRewUpdate) (Maybe DeltaCoin)
    help = lens view update
      where
        view SNothing = Nothing
        view (SJust (Complete x)) = Just (RU.deltaF x)
        view (SJust _) = Nothing
        update (SJust (Complete ru)) (Just change) = SJust (Complete (ru {RU.deltaF = change}))
        update _ _ = SNothing

rewardSet ::
  Era era => Term era (Map (Credential 'Staking) (Set Reward))
rewardSet = Var (V "rewardSet" (MapR CredR (SetR RewardR)) No)

rewardSetL ::
  NELens era (Maybe (Map (Credential 'Staking) (Set Reward)))
rewardSetL = nesRuL . help
  where
    help ::
      Lens' (StrictMaybe PulsingRewUpdate) (Maybe (Map (Credential 'Staking) (Set Reward)))
    help = lens view update
      where
        view SNothing = Nothing
        view (SJust (Complete x)) = Just (RU.rs x)
        view (SJust _) = Nothing
        update (SJust (Complete ru)) (Just change) = SJust (Complete (ru {RU.rs = change}))
        update _ _ = SNothing

-- ===================================================================
-- Non Access variables

totalAda :: Era era => Term era Coin
totalAda = Var $ V "totalAda" CoinR No

utxoCoin :: Era era => Term era Coin
utxoCoin = Var $ V "utxoCoin" CoinR No

-- | The universe of Staking Credentials. A credential is either KeyHash of a ScriptHash
--   Any Plutus scripts in this Universe are NOT Spending scripts, so they do not need a Redeemer
credsUniv :: Era era => Term era (Set (Credential 'Staking))
credsUniv = Var $ V "credsUniv" (SetR CredR) No

-- | The universe of Staking Credentials. A credential is either KeyHash of a ScriptHash
--   All Plutus scripts in this Universe are SPENDING scripts, so they will need a Redeemer
--   Use this ONLY in the Pay-part of an Address (Do not use this in the Stake-part of an Address)
spendCredsUniv :: Era era => Term era (Set (Credential 'Payment))
spendCredsUniv = Var $ V "spendCredsUniv" (SetR PCredR) No

-- | The universe of Voting Credentials. A credential is either KeyHash of a ScriptHash
voteUniv :: Era era => Term era (Set (Credential 'DRepRole))
voteUniv = Var $ V "voteUniv" (SetR VCredR) No

-- | The universe of DReps
drepUniv :: Era era => Term era (Set DRep)
drepUniv = Var $ V "drepUniv" (SetR DRepR) No

-- | The universe of Credentials used in voting for constitutional committee changes.
hotCommitteeCredsUniv :: Era era => Term era (Set (Credential 'HotCommitteeRole))
hotCommitteeCredsUniv = Var $ V "hotCommitteeCredsUniv" (SetR CommHotCredR) No

-- | The universe of Credentials used in voting for constitutional committee changes.
coldCommitteeCredsUniv :: Era era => Term era (Set (Credential 'ColdCommitteeRole))
coldCommitteeCredsUniv = Var $ V "coldCommitteeCredsUniv" (SetR CommColdCredR) No

-- | The universe of Payment Credentials. A credential is either KeyHash of a ScriptHash
--   We only find payment credentials in the Payment part of an Addr.
payUniv :: Era era => Term era (Set (Credential 'Payment))
payUniv = Var $ V "payUniv" (SetR PCredR) No

-- | The universe of Scripts (and their hashes) useable in spending contexts
--  That means if they are Plutus scripts then they will be passed an additional
--  argument (the TxInfo context)
spendscriptUniv :: Era era => Proof era -> Term era (Map ScriptHash (ScriptF era))
spendscriptUniv p = Var (pV p "spendscriptUniv" (MapR ScriptHashR (ScriptR p)) No)

-- | The universe of Scripts (and their hashes) useable in contexts other than Spending
nonSpendScriptUniv ::
  Era era => Proof era -> Term era (Map ScriptHash (ScriptF era))
nonSpendScriptUniv p = Var (pV p "nonSpendScriptUniv" (MapR ScriptHashR (ScriptR p)) No)

-- | The union of 'spendscriptUniv' and 'nonSpendScriptUniv'. All possible scripts in any context
allScriptUniv :: Era era => Proof era -> Term era (Map ScriptHash (ScriptF era))
allScriptUniv p = Var (pV p "allScriptUniv" (MapR ScriptHashR (ScriptR p)) No)

-- | The universe of Data (and their hashes)
dataUniv :: Era era => Term era (Map DataHash (Data era))
dataUniv = Var (V "dataUniv" (MapR DataHashR DataR) No)

-- | The universe of StakePool key hashes. These hashes hash the cold key of the
--   Pool operators.
poolHashUniv :: Era era => Term era (Set (KeyHash 'StakePool))
poolHashUniv = Var $ V "poolHashUniv" (SetR PoolHashR) No

-- | The universe of StakePool key hashes. These hashes hash are hashes of the Owners of a PoolParam
stakeHashUniv :: Era era => Term era (Set (KeyHash 'Staking))
stakeHashUniv = Var $ V "stakeHashUniv" (SetR StakeHashR) No

-- | The universe of DRep key hashes. These hashes hash are hashes of the DReps
drepHashUniv :: Era era => Term era (Set (KeyHash 'DRepRole))
drepHashUniv = Var $ V "drepHashUniv" (SetR DRepHashR) No

-- | The universe of the Genesis key hashes and their signing and validating GenDelegPairs
genesisHashUniv ::
  Era era => Term era (Map (KeyHash 'Genesis) GenDelegPair)
genesisHashUniv = Var $ V "genesisHashUniv" (MapR GenHashR GenDelegPairR) No

voteCredUniv :: Era era => Term era (Set (Credential 'ColdCommitteeRole))
voteCredUniv = Var $ V "voteHashUniv" (SetR CommColdCredR) No

-- | The universe of TxIns. Pairs of TxId: hashes of previously run transaction bodies,
--   and TxIx: indexes of one of the bodies outputs.
txinUniv :: Era era => Term era (Set TxIn)
txinUniv = Var $ V "txinUniv" (SetR TxInR) No

-- | The universe of GovActionId. Pairs of TxId: hashes of previously run transaction bodies,
--   and GovActionIx: indexes of one of the bodies Proposals .
govActionIdUniv :: Era era => Term era (Set GovActionId)
govActionIdUniv = Var $ V "govActionIdUniv" (SetR GovActionIdR) No

-- | The universe of TxOuts.
--   It contains 'colTxoutUniv' as a sublist and 'feeOutput' as an element
--   See also 'feeOutput' which is defined by the universes, and is related.
txoutUniv :: Era era => Proof era -> Term era (Set (TxOutF era))
txoutUniv p = Var (pV p "txoutUniv" (SetR (TxOutR p)) No)

-- | The universe of TxOuts useable for collateral
--   The collateral TxOuts consists only of VKey addresses
--   and The collateral TxOuts do not contain any non-ADA part
colTxoutUniv :: Era era => Proof era -> Term era (Set (TxOutF era))
colTxoutUniv p = Var (pV p "colTxoutUniv" (SetR (TxOutR p)) No)

-- | A TxOut, guaranteed to have
--   1) no scripts in its Addr, and
--   2) It's Addr is in the addrUniv
--   3) 'bigCoin' is stored in the Addr Value, and
--   4) the Addr Value has empty MutiAssets
--   5) be a member of the txoutUniv
feeTxOut :: Reflect era => Term era (TxOutF era)
feeTxOut = Var (V "feeTxOut" (TxOutR reify) No)

-- | A TxIn, guaranteed to have
--  1) be a member of the txinUniv
feeTxIn :: Era era => Term era TxIn
feeTxIn = Var (V "feeTxIn" TxInR No)

-- | A Coin large enough to pay almost any fee.
--   See also 'feeOutput' which is related.
bigCoin :: Era era => Term era Coin
bigCoin = Var (V "bigCoin" CoinR No)

datumsUniv :: Era era => Term era [Datum era]
datumsUniv = Var (V "datumsUniv" (ListR DatumR) No)

multiAssetUniv :: Era era => Term era [MultiAsset]
multiAssetUniv = Var (V "multiAssetUniv" (ListR MultiAssetR) No)

-- | The universe of key hashes, and the signing and validating key pairs they represent.
keymapUniv ::
  Era era => Term era (Map (KeyHash 'Witness) (KeyPair 'Witness))
keymapUniv = Var (V "keymapUniv" (MapR WitHashR KeyPairR) No)

currentSlot :: Era era => Term era SlotNo
currentSlot = Var (V "currentSlot" SlotNoR No)

endSlotDelta :: Era era => Term era SlotNo
endSlotDelta = Var (V "endSlotDelta" SlotNoR No)

beginSlotDelta :: Era era => Term era SlotNo
beginSlotDelta = Var (V "beginSlotDelta" SlotNoR No)

-- See also currentEpoch in NewEpochState fields

-- | From Globals
network :: Era era => Term era Network
network = Var (V "network" NetworkR No)

-- | This not really a variable, But a constant that is set by the 'testGlobals'
--   we reflect this into a Term, so we can refer to it in the Preds.
quorumConstant :: Word64
quorumConstant = Base.quorum Utils.testGlobals

-- | From Globals. Reflected here at type Int, This is set to 'quorumConstant' in CertState.
--   because is is used to compare the Size of things, which are computed as Int
quorum :: Era era => Term era Int
quorum = Var (V "quorum" IntR No)

addrUniv :: Era era => Term era (Set Addr)
addrUniv = Var $ V "addrUniv" (SetR AddrR) No

ptrUniv :: Era era => Term era (Set Ptr)
ptrUniv = Var $ V "ptrUniv" (SetR PtrR) No

plutusUniv :: Reflect era => Term era (Map ScriptHash (IsValid, ScriptF era))
plutusUniv = Var $ V "plutusUniv" (MapR ScriptHashR (PairR IsValidR (ScriptR reify))) No

spendPlutusUniv :: Reflect era => Term era (Map ScriptHash (IsValid, ScriptF era))
spendPlutusUniv = Var $ V "spendPlutusUniv" (MapR ScriptHashR (PairR IsValidR (ScriptR reify))) No

-- | The universe of all Byron addresses. In Eras, Babbage, Conway we avoid these Adresses,
--   as they do not play well with Plutus Scripts.
byronAddrUniv ::
  Era era => Term era (Map (KeyHash 'Payment) (Addr, SigningKey))
byronAddrUniv = Var $ V "byronAddrUniv" (MapR PayHashR (PairR AddrR SigningKeyR)) No

-- ====================================================================
-- Targets for sub types of NewEpochState
-- A Target assembles variables into data stuctures. The main concern
-- is transforming the types used in the variable model into the real types
-- stored in the data structures. 4 examples of such transformation
-- 1) Wrapping and unwraping newtypes like StakeDistr
-- 2) Transforming Coin into (CompactForm Coin) a Word64
-- 3) Transforming Data.Map into Data.VMap
-- 4) Transforming the Models view of Data Families (TxOut, Value, PParams, PParamsUpdate) into
--    the data structures view.
--
-- The strategy we use is to define a "constructor function" which accepts the model types,
-- and which converts the model types into the data structure types. We then wrap this
-- "constructor function" in the "Constr" of Target. See 'newEpochStateConstr',
-- 'utxofun' and 'dstate' for examples of how this is done.

-- | Abstract constuctor function for NewEpochState
newEpochStateConstr ::
  Proof era ->
  EpochNo ->
  Map (KeyHash 'StakePool) Natural ->
  Map (KeyHash 'StakePool) Natural ->
  EpochState era ->
  Map (KeyHash 'StakePool) IndividualPoolStake ->
  NewEpochState era
newEpochStateConstr
  proof
  nesEL'
  nesBprev'
  nesBcur'
  nesEs'
  nesPd' =
    NewEpochState
      nesEL'
      (BlocksMade nesBprev')
      (BlocksMade nesBcur')
      nesEs'
      SNothing
      (PoolDistr nesPd' $ CompactCoin 1)
      ( case proof of
          Shelley -> UTxO Map.empty
          Allegra -> ()
          Mary -> ()
          Alonzo -> ()
          Babbage -> ()
          Conway -> ()
      )

-- | Target for NewEpochState
newEpochStateT ::
  forall era. Reflect era => Proof era -> RootTarget era (NewEpochState era) (NewEpochState era)
newEpochStateT proof =
  Invert "NewEpochState" (typeRep @(NewEpochState era)) (newEpochStateConstr proof)
    :$ Lensed currentEpoch nesELL
    :$ Lensed prevBlocksMade nesBprevL
    :$ Lensed currBlocksMade nesBcurL
    :$ Shift (epochStateT proof) nesEsL
    :$ Lensed poolDistr (nesPdL . poolDistrDistrL)

-- | Target for EpochState
epochStateT ::
  forall era. Reflect era => Proof era -> RootTarget era (EpochState era) (EpochState era)
epochStateT proof =
  Invert "EpochState" (typeRep @(EpochState era)) epochStateFun
    :$ Shift accountStateT chainAccountStateL
    :$ Shift (ledgerStateT proof) esLStateL
    :$ Shift snapShotsT esSnapshotsL
  where
    epochStateFun a s l = EpochState a s l (NonMyopic Map.empty (Coin 0))

-- | Target for ChainAccountState
accountStateT :: Era era => RootTarget era ChainAccountState ChainAccountState
accountStateT =
  Invert "ChainAccountState" (typeRep @ChainAccountState) ChainAccountState
    :$ Lensed treasury casTreasuryL
    :$ Lensed reserves casReservesL

-- | Target for LedgerState
ledgerStateT ::
  forall era. Reflect era => Proof era -> RootTarget era (LedgerState era) (LedgerState era)
ledgerStateT proof =
  Invert "LedgerState" (typeRep @(LedgerState era)) ledgerStateFun
    :$ Shift (utxoStateT proof) lsUTxOStateL
    :$ Shift certStateT (lsCertStateL . unCertStateFL)
  where
    ledgerStateFun ::
      UTxOState era ->
      CertStateF era ->
      LedgerState era
    ledgerStateFun utxoState (CertStateF _ x) = LedgerState utxoState x

ledgerState :: Reflect era => Term era (LedgerState era)
ledgerState = Var $ V "ledgerState" (LedgerStateR reify) No

-- | Target for UTxOState
utxoStateT ::
  forall era. Gov.EraGov era => Proof era -> RootTarget era (UTxOState era) (UTxOState era)
utxoStateT p =
  Invert "UTxOState" (typeRep @(UTxOState era)) (unReflect utxofun p)
    :$ Lensed (utxo p) (utxoL . unUtxoL p)
    :$ Lensed deposits utxosDepositedL
    :$ Lensed fees utxosFeesL
    :$ Shift (govStateT p) (utxosGovStateL . unGovL p)
    :$ Lensed donation utxosDonationL
  where
    utxofun ::
      Reflect era =>
      Proof era ->
      Map TxIn (TxOutF era) ->
      Coin ->
      Coin ->
      GovState era ->
      Coin ->
      UTxOState era
    utxofun proof u c1 c2 (GovState _ x) = smartUTxOState (justProtocolVersion proof) (liftUTxO u) c1 c2 x

unGovL :: Proof era -> Lens' (Gov.GovState era) (GovState era)
unGovL p = lens (\x -> GovState p x) (\_ (GovState _ y) -> y)

justProtocolVersion :: forall era. Reflect era => Proof era -> PParams era
justProtocolVersion proof = newPParams proof [Fields.ProtocolVersion $ protocolVersion proof]

unCertStateFL :: Reflect era => Lens' (CertState era) (CertStateF era)
unCertStateFL = lens (CertStateF reify) (\_ (CertStateF _ x) -> x)

certStateFL :: Lens' (CertStateF era) (CertState era)
certStateFL = lens unCertStateF (\(CertStateF p _) y -> CertStateF p y)

-- | Target for CertState
certStateT :: forall era. Reflect era => RootTarget era (CertStateF era) (CertStateF era)
certStateT = case reify @era of
  Shelley ->
    Invert "CertState" (typeRep @(CertStateF era)) (CertStateF reify)
      :$ Shift shelleyCertStateT certStateFL
  Allegra ->
    Invert "CertState" (typeRep @(CertStateF era)) (CertStateF reify)
      :$ Shift shelleyCertStateT certStateFL
  Mary ->
    Invert "CertState" (typeRep @(CertStateF era)) (CertStateF reify)
      :$ Shift shelleyCertStateT certStateFL
  Alonzo ->
    Invert "CertState" (typeRep @(CertStateF era)) (CertStateF reify)
      :$ Shift shelleyCertStateT certStateFL
  Babbage ->
    Invert "CertState" (typeRep @(CertStateF era)) (CertStateF reify)
      :$ Shift shelleyCertStateT certStateFL
  Conway ->
    Invert "CertState" (typeRep @(CertStateF era)) (CertStateF reify)
      :$ Shift conwayCertStateT certStateFL
  where
    shelleyCertStateT :: RootTarget era (ShelleyCertState era) (ShelleyCertState era)
    shelleyCertStateT =
      Invert "ShelleyCertState" (typeRep @(ShelleyCertState era)) ShelleyCertState
        :$ (Shift pstateT shelleyCertPStateL)
        :$ (Shift dstateT shelleyCertDStateL)
    conwayCertStateT ::
      ConwayEraCertState era => RootTarget era (ConwayCertState era) (ConwayCertState era)
    conwayCertStateT =
      Invert "ConwayCertState" (typeRep @(ConwayCertState era)) ConwayCertState
        :$ (Shift vstateT conwayCertVStateL)
        :$ (Shift pstateT conwayCertPStateL)
        :$ (Shift dstateT conwayCertDStateL)

-- | Target for VState
vstateT :: forall era. ConwayEraCertState era => RootTarget era (VState era) (VState era)
vstateT =
  Invert "VState" (typeRep @(VState era)) (\x y z -> VState x (CommitteeState y) z)
    :$ Lensed currentDRepState vsDRepsL
    :$ Lensed committeeState (vsCommitteeStateL . csCommitteeCredsL)
    :$ Lensed numDormantEpochs vsNumDormantEpochsL

committeeL ::
  Lens' (Map (Credential 'ColdCommitteeRole) CommitteeAuthorization) (CommitteeState era)
committeeL = lens CommitteeState (\_ (CommitteeState x) -> x)

-- | Target for PState
pstateT :: forall era. EraCertState era => RootTarget era (PState era) (PState era)
pstateT =
  Invert "PState" (typeRep @(PState era)) PState
    :$ Lensed regPools psStakePoolParamsL
    :$ Lensed futureRegPools psFutureStakePoolParamsL
    :$ Lensed retiring psRetiringL
    :$ Lensed poolDeposits psDepositsL

-- | Target for DState
dstateT :: forall era. EraCertState era => RootTarget era (DState era) (DState era)
dstateT =
  Invert "DState" (typeRep @(DState era)) dstate
    :$ Lensed rewards (dsUnifiedL . rewardsUMapL)
    :$ Lensed stakeDeposits (dsUnifiedL . stakeDepositsUMapL)
    :$ Lensed delegations (dsUnifiedL . delegationsUMapL)
    :$ Lensed drepDelegation (dsUnifiedL . drepUMapL)
    :$ Lensed ptrs (dsUnifiedL . ptrsUMapL)
    :$ Lensed futureGenDelegs dsFutureGenDelegsL
    :$ Lensed genDelegs (dsGenDelegsL . unGenDelegsL)
    :$ Shift instantaneousRewardsT dsIRewardsL

-- | Abstract construcor function for DState
dstate ::
  Map (Credential 'Staking) Coin ->
  Map (Credential 'Staking) Coin ->
  Map (Credential 'Staking) (KeyHash 'StakePool) ->
  Map (Credential 'Staking) DRep ->
  Map Ptr (Credential 'Staking) ->
  Map FutureGenDeleg GenDelegPair ->
  Map (KeyHash 'Genesis) GenDelegPair ->
  InstantaneousRewards ->
  DState era
dstate rew dep deleg drepdeleg ptr fgen gen =
  DState
    (unSplitUMap (Split rew dep deleg drepdeleg (error "Not implemented") ptr))
    fgen
    (GenDelegs gen)

instantaneousRewardsT ::
  forall era.
  EraCertState era =>
  RootTarget era InstantaneousRewards InstantaneousRewards
instantaneousRewardsT =
  Invert "InstanRew" (typeRep @InstantaneousRewards) InstantaneousRewards
    :$ Lensed instanReserves iRReservesL
    :$ Lensed instanTreasury iRTreasuryL
    :$ Lensed deltaReserves deltaReservesL
    :$ Lensed deltaTreasury deltaTreasuryL

-- | A String that pretty prints the complete set of variables of the NewEpochState
allvars :: String
allvars = show (ppTarget (newEpochStateT Conway))

printTarget :: RootTarget era root t -> IO ()
printTarget t = putStrLn (show (ppTarget t))

-- =====================================================================
-- PParams fields

-- | ProtVer in pparams
protVer :: Era era => Proof era -> Term era ProtVer
protVer proof =
  Var
    ( pV
        proof
        "protVer"
        (ProtVerR proof)
        (Yes (PParamsR proof) $ withEraPParams proof (pparamsWrapperL . ppProtocolVersionL))
    )

-- | ProtVer in prevPParams
prevProtVer :: Era era => Proof era -> Term era ProtVer
prevProtVer proof =
  Var
    ( pV
        proof
        "prevProtVer"
        (ProtVerR proof)
        (Yes (PParamsR proof) $ withEraPParams proof (pparamsWrapperL . ppProtocolVersionL))
    )

minFeeA :: Era era => Proof era -> Term era Coin
minFeeA proof =
  Var
    ( pV
        proof
        "minFeeA"
        CoinR
        (Yes (PParamsR proof) $ withEraPParams proof (pparamsWrapperL . ppMinFeeAL))
    )

minFeeB :: Era era => Proof era -> Term era Coin
minFeeB proof =
  Var
    ( pV
        proof
        "minFeeB"
        CoinR
        (Yes (PParamsR proof) $ withEraPParams proof (pparamsWrapperL . ppMinFeeBL))
    )

-- | Max Block Body Size
maxBBSize :: Era era => Proof era -> Term era Natural
maxBBSize p =
  Var
    ( pV
        p
        "maxBBSize"
        NaturalR
        (Yes (PParamsR p) (withEraPParams p (pparamsWrapperL . ppMaxBBSizeL . word32NaturalL)))
    )

-- | Max Tx Size
maxTxSize :: Era era => Proof era -> Term era Natural
maxTxSize p =
  Var
    ( pV
        p
        "maxTxSize"
        NaturalR
        (Yes (PParamsR p) (withEraPParams p (pparamsWrapperL . ppMaxTxSizeL . word32NaturalL)))
    )

fromIntegralBounded ::
  forall a b.
  (HasCallStack, Integral a, Show a, Integral b, Bounded b, Show b) =>
  String ->
  a ->
  b
fromIntegralBounded name x
  | toInteger (minBound :: b) <= xi && xi <= toInteger (maxBound :: b) = fromIntegral x
  | otherwise =
      error $
        "While converting "
          ++ name
          ++ ", "
          ++ show x <> " is out of bounds: " <> show (minBound :: b, maxBound :: b)
  where
    xi = toInteger x

word32NaturalL :: Lens' Word32 Natural
word32NaturalL = lens fromIntegral (\_ y -> fromIntegralBounded "word32NaturaL" (toInteger y))

word16NaturalL :: Lens' Word16 Natural
word16NaturalL = lens fromIntegral (\_ y -> fromIntegralBounded "word16NaturalL" (toInteger y))

-- | Max Block Header Size
maxBHSize :: Era era => Proof era -> Term era Natural
maxBHSize p =
  Var
    ( pV
        p
        "maxBHSize"
        NaturalR
        (Yes (PParamsR p) (withEraPParams p (pparamsWrapperL . ppMaxBHSizeL . word16NaturalL)))
    )

poolDepAmt :: Era era => Proof era -> Term era Coin
poolDepAmt p =
  Var $
    pV
      p
      "poolDepAmt"
      CoinR
      (Yes (PParamsR p) (withEraPParams p (pparamsWrapperL . ppPoolDepositL)))

keyDepAmt :: Era era => Proof era -> Term era Coin
keyDepAmt p =
  Var $
    pV
      p
      "keyDepAmt"
      CoinR
      (Yes (PParamsR p) (withEraPParams p (pparamsWrapperL . ppKeyDepositL)))

proposalDeposit :: ConwayEraPParams era => Proof era -> Term era Coin
proposalDeposit p =
  Var $
    pV
      p
      "proposalDeposit"
      CoinR
      (Yes (PParamsR p) (withEraPParams p (pparamsWrapperL . ppGovActionDepositL)))

maxTxExUnits :: AlonzoEraPParams era => Proof era -> Term era ExUnits
maxTxExUnits p =
  Var $
    pV
      p
      "maxTxExUnits"
      ExUnitsR
      (Yes (PParamsR p) (withEraPParams p (pparamsWrapperL . ppMaxTxExUnitsL)))

collateralPercentage :: AlonzoEraPParams era => Proof era -> Term era Natural
collateralPercentage p =
  Var $
    pV
      p
      "collateralPercentage"
      NaturalR
      (Yes (PParamsR p) (withEraPParams p (pparamsWrapperL . ppCollateralPercentageL)))

drepDeposit :: ConwayEraPParams era => Proof era -> Term era Coin
drepDeposit p =
  Var $
    pV p "drepDeposit" CoinR (Yes (PParamsR p) (withEraPParams p (pparamsWrapperL . ppDRepDepositL)))

drepActivity :: ConwayEraPParams era => Proof era -> Term era Base.EpochInterval
drepActivity p =
  Var $
    pV
      p
      "drepActivty"
      EpochIntervalR
      (Yes (PParamsR p) (withEraPParams p (pparamsWrapperL . ppDRepActivityL)))

maxEpoch :: Era era => Proof era -> Term era Base.EpochInterval
maxEpoch p =
  Var $
    pV
      p
      "maxEpoch"
      EpochIntervalR
      (Yes (PParamsR p) (withEraPParams p (pparamsWrapperL . ppEMaxL)))

-- =================================================================
-- TxBody vars

txbodyterm :: Reflect era => Term era (TxBodyF era)
txbodyterm = Var $ V "txbodyterm" (TxBodyR reify) No

inputs :: Era era => Term era (Set TxIn)
inputs = Var $ V "inputs" (SetR TxInR) No

collateral :: Era era => Term era (Set TxIn)
collateral = Var $ V "collateral" (SetR TxInR) No

refInputs :: Era era => Term era (Set TxIn)
refInputs = Var $ V "refInputs" (SetR TxInR) No

outputs :: Era era => Proof era -> Term era [TxOutF era]
outputs p = Var $ pV p "outputs" (ListR (TxOutR p)) No

collateralReturn :: Era era => Proof era -> Term era (TxOutF era)
collateralReturn p = Var $ pV p "collateralReturn" (TxOutR p) No

-- | The sum of all the 'collateral' inputs. The Tx is constucted
--   by SNothing or wrapping 'SJust' around this value.
totalCol :: Era era => Term era Coin
totalCol = Var $ V "totalCol" CoinR No

certs :: Reflect era => Term era [TxCertF era]
certs = Var $ V "certs" (ListR (TxCertR reify)) No

withdrawals :: forall era. Era era => Term era (Map RewardAccount Coin)
withdrawals = Var $ V "withdrawals" (MapR (RewardAccountR @era) CoinR) No

txfee :: Era era => Term era Coin
txfee = Var $ V "txfee" CoinR No

ttl :: Era era => Term era SlotNo
ttl = Var $ V "ttl" SlotNoR No

validityInterval :: Era era => Term era ValidityInterval
validityInterval = Var $ V "validityInterval" ValidityIntervalR No

mint :: Era era => Term era (Map ScriptHash (Map AssetName Integer))
mint = Var $ V "mint" (MapR ScriptHashR (MapR AssetNameR IntegerR)) No

reqSignerHashes :: Era era => Term era (Set (KeyHash 'Witness))
reqSignerHashes = Var $ V "reqSignerHashes" (SetR WitHashR) No

networkID :: Era era => Term era (Maybe Network)
networkID = Var $ V "networkID" (MaybeR NetworkR) No

adHash :: Era era => Term era (Maybe TxAuxDataHash)
adHash = Var $ V "adHash" (MaybeR TxAuxDataHashR) No

wppHash :: Era era => Term era (Maybe (SafeHash EraIndependentScriptIntegrity))
wppHash = Var $ V "wppHash" (MaybeR ScriptIntegrityHashR) No

txDonation :: Era era => Term era Coin
txDonation = Var $ V "txDonation" CoinR No

-- | lift the model type of 'mint' into a MultiAsset
liftMultiAsset :: Map ScriptHash (Map AssetName Integer) -> MultiAsset
liftMultiAsset m = MultiAsset (Map.mapKeys PolicyID m)

scriptsNeeded :: Reflect era => Term era (ScriptsNeededF era)
scriptsNeeded = Var $ V "scriptsNeeded" (ScriptsNeededR reify) No

smNeededL ::
  ScriptsNeeded era ~ ShelleyScriptsNeeded era =>
  Lens' (ScriptsNeededF era) (Set ScriptHash)
smNeededL =
  lens
    (\(ScriptsNeededF _ (ShelleyScriptsNeeded s)) -> s)
    (\(ScriptsNeededF p _) s -> ScriptsNeededF p (ShelleyScriptsNeeded s))

acNeededL ::
  ScriptsNeeded era ~ AlonzoScriptsNeeded era =>
  Lens' (ScriptsNeededF era) [(PlutusPurposeF era, ScriptHash)]
acNeededL =
  lens
    (\(ScriptsNeededF p (AlonzoScriptsNeeded s)) -> map (first (PlutusPurposeF p)) s)
    ( \(ScriptsNeededF p _) s ->
        ScriptsNeededF p (AlonzoScriptsNeeded (map (first unPlutusPurposeF) s))
    )

-- ===============
-- Auxliary Vars to compute collateral

-- | A Coin that needs to be added to the range of the colInputs in the UtxO
--   that will make sure the collateral is large enough to pay the fees if needed
extraCol :: Era era => Term era Coin
extraCol = Var $ V "extraCol" CoinR No

-- | The sum of all the 'collateral' inputs, total colateral of the Tx is computed by adding (SJust _) to this value.
sumCol :: Era era => Term era Coin
sumCol = Var $ V "sumCol" CoinR No

colRetAddr :: Era era => Term era Addr
colRetAddr = Var $ V "colRetAddr" AddrR No

-- | The Coin in the 'collateralReturn' TxOut
colRetCoin :: Era era => Term era Coin
colRetCoin = Var $ V "colRetCoin" CoinR No

-- | The amount that the collateral must cover if there is a two phase error.
--   This is roughly the 'collateralPercentage' * 'txfee' . The calculation deals with rounding,
--   but you don't need those details to understand what is going on.
owed :: Era era => Term era Coin
owed = Var $ V "owed" CoinR No

-- ==============================================================
-- Tx Vars

txbody :: Reflect era => Term era (TxBodyF era)
txbody = Var $ V "txbody" (TxBodyR reify) No

txwits :: Reflect era => Term era (TxWitsF era)
txwits = Var $ V "txwits" (TxWitsR reify) No

txauxdata :: Reflect era => Term era (Maybe (TxAuxDataF era))
txauxdata = Var $ V "txauxdata" (MaybeR (TxAuxDataR reify)) No

txisvalid :: Era era => Term era IsValid
txisvalid = Var $ V "txisvalid" IsValidR No

valids :: Era era => Term era [IsValid]
valids = Var $ V "valids" (ListR IsValidR) No

txterm :: Reflect era => Term era (TxF era)
txterm = Var $ V "txterm" (TxR reify) No

-- ==============================================================
-- Terms and Fields for use in TxOut and TxBody

-- Lenses for use in TxBody

getRwdCredL :: Lens' RewardAccount (Credential 'Staking)
getRwdCredL = lens raCredential (\r c -> r {raCredential = c})

txOutFL :: Lens' (TxOutF era) (TxOut era)
txOutFL = lens unTxOut (\(TxOutF p _) y -> TxOutF p y)

valueFL :: Reflect era => Lens' (Value era) (ValueF era)
valueFL = lens (ValueF reify) (\_ (ValueF _ u) -> u)

lensVC :: Val t => Lens' t Coin
lensVC = lens coin $ \t c -> modifyCoin (const c) t

valueFCoinL :: (HasCallStack, Reflect era) => Lens' (ValueF era) Coin
valueFCoinL =
  lens
    (coin . unValue)
    ( \(ValueF p v) c@(Coin i) ->
        if i < 0
          then error ("Coin is less than 0 " ++ show i ++ " in valueFCoinL")
          else (ValueF p (modifyCoin (const c) v))
    )

outputCoinL :: (HasCallStack, Reflect era) => Lens' (TxOutF era) Coin
outputCoinL =
  lens
    (\(TxOutF _ out) -> out ^. coinTxOutL)
    (\(TxOutF p out) c -> TxOutF p (out & coinTxOutL .~ c))

-- | a Field from (ValueF era) to Coin
valCoinF :: (HasCallStack, Reflect era) => Field era (ValueF era) Coin
valCoinF = Field "valCoin" CoinR (ValueR reify) valueFCoinL

valCoin :: (HasCallStack, Reflect era) => Term era Coin
valCoin = fieldToTerm valCoinF

maryValueMultiAssetL :: Lens' MaryValue MultiAsset
maryValueMultiAssetL =
  lens
    (\(MaryValue _ ma) -> ma)
    (\(MaryValue c _) ma -> MaryValue c ma)

valueFMultiAssetL :: Lens' (ValueF era) MultiAsset
valueFMultiAssetL = lens get put
  where
    get :: ValueF era -> MultiAsset
    get (ValueF p x) = case whichValue p of
      ValueShelleyToAllegra -> MultiAsset Map.empty
      ValueMaryToConway -> x ^. maryValueMultiAssetL

    put :: ValueF era -> MultiAsset -> ValueF era
    put (ValueF p x) new = case whichValue p of
      ValueShelleyToAllegra -> ValueF p x
      ValueMaryToConway -> ValueF p (x & maryValueMultiAssetL .~ new)

-- | a Field from (ValueF era) to MultiAsset
valueFMultiAssetF :: Reflect era => Field era (ValueF era) MultiAsset
valueFMultiAssetF = Field "valueFMultiAsset" MultiAssetR (ValueR reify) valueFMultiAssetL

valueFMultiAsset :: Reflect era => Term era MultiAsset
valueFMultiAsset = fieldToTerm valueFMultiAssetF

-- | a Field from (TxOut era) to (Addr era)
txoutAddressF :: Reflect era => Field era (TxOutF era) Addr
txoutAddressF = Field "txoutAddress" AddrR (TxOutR reify) (txOutFL . addrTxOutL)

txoutAddress :: Reflect era => Term era Addr
txoutAddress = fieldToTerm txoutAddressF

-- | a Field from (TxOutF era) to Coin
txoutCoinF :: (HasCallStack, Reflect era) => Field era (TxOutF era) Coin
txoutCoinF = Field "txoutCoin" CoinR (TxOutR reify) outputCoinL

txoutCoin :: (HasCallStack, Reflect era) => Term era Coin
txoutCoin = fieldToTerm txoutCoinF

-- | a Field from (TxOutF era) to (ValueF era)
txoutAmountF :: Reflect era => Field era (TxOutF era) (ValueF era)
txoutAmountF = Field "txoutAmount" (ValueR reify) (TxOutR reify) (txOutFL . valueTxOutL . valueFL)

txoutAmount :: Reflect era => Term era (ValueF era)
txoutAmount = fieldToTerm txoutAmountF

-- =================================
-- Witnesses

scriptWits :: Reflect era => Term era (Map ScriptHash (ScriptF era))
scriptWits = Var $ V "scriptWits" (MapR ScriptHashR (ScriptR reify)) No

redeemers :: Reflect era => Term era (Map (PlutusPointerF era) (Data era, ExUnits))
redeemers = Var $ V "redeemers" (MapR (RdmrPtrR reify) (PairR DataR ExUnitsR)) No

bootWits :: forall era. Reflect era => Term era (Set BootstrapWitness)
bootWits = Var $ V "bootWits" (SetR (BootstrapWitnessR @era)) No

dataWits :: Reflect era => Term era (Map DataHash (Data era))
dataWits = Var $ V "dataWits" (MapR DataHashR DataR) No

keyWits :: Reflect era => Term era (Set (WitVKey 'Witness))
keyWits = Var $ V "keyWits" (SetR (WitVKeyR reify)) No

-- =======================================================================================
-- Targets for building Transactions and their components. Since we compute these in two
-- passes, the targets are parameterized by the things that change between the first and
-- second passes. Here is an accounting of the things that change
-- 1) witsTarget: The witnesses that depend on the hash of the TxBody 'bootWits' and 'keyWits'
-- 2) txbodyTarget: 'txfee' , 'totaland 'wppHash'
-- 3) txTarget:  'txbodyterm', 'bootWits', and 'keyWits', since a Tx has both a body and witnesses

witsTarget ::
  Reflect era =>
  Term era (Set BootstrapWitness) ->
  Term era (Set (WitVKey 'Witness)) ->
  Target era (TxWits era)
witsTarget bootWitsParam keyWitsParam =
  Constr "TxWits" witsf ^$ scriptWits ^$ redeemers ^$ bootWitsParam ^$ dataWits ^$ keyWitsParam
  where
    proof = reify
    witsf script redeem boot dataw key =
      newWitnesses
        merge
        proof
        [ AddrWits key
        , BootWits boot
        , ScriptWits (Map.map unScriptF script)
        , DataWits (TxDats dataw)
        , RdmrWits (mkRedeemers proof $ map (first unPlutusPointerF) $ Map.toList $ redeem)
        ]

txTarget ::
  Reflect era =>
  Term era (TxBodyF era) ->
  Term era (Set BootstrapWitness) ->
  Term era (Set (WitVKey 'Witness)) ->
  Target era (TxF era)
txTarget bodyparam bootWitsParam keyWitsParam =
  Constr "tx" txf ^$ bodyparam :$ wits ^$ txauxdata ^$ txisvalid
  where
    wits = witsTarget bootWitsParam keyWitsParam
    txf (TxBodyF proof txb) w auxs isvalid =
      TxF proof (newTx proof [Body txb, TxWits w, AuxData' (fixM auxs), Valid isvalid])
    fixM Nothing = []
    fixM (Just (TxAuxDataF _ x)) = [x]

-- | Need to build the TxBody with different terms that control the fee and wppHash so we
--   parameterise this target over those two terms
txbodyTarget ::
  Reflect era =>
  Term era Coin ->
  Term era (Maybe ScriptIntegrityHash) ->
  Term era Coin ->
  Target era (TxBodyF era)
txbodyTarget feeparam wpphashparam totalColParam =
  Constr "txbody" txbodyf
    ^$ inputs
    ^$ collateral
    ^$ refInputs
    ^$ (outputs proof)
    ^$ (collateralReturn proof)
    -- \^$ updates
    ^$ totalColParam
    ^$ certs
    ^$ withdrawals
    ^$ ttl
    ^$ validityInterval
    ^$ mint
    ^$ reqSignerHashes
    ^$ networkID
    ^$ adHash
    ^$ wpphashparam
    ^$ feeparam
    ^$ txDonation
  where
    proof = reify
    txbodyf
      ins
      col
      refs
      out
      (TxOutF _ colret)
      totcol
      --    updates
      cs
      ws
      tt
      vi
      mnt
      req
      net
      adh
      wpp
      fee
      donate =
        TxBodyF
          proof
          ( newTxBody
              proof
              [ Inputs ins
              , Collateral col
              , RefInputs refs
              , Outputs' (map unTxOut out)
              , CollateralReturn (SJust colret)
              , -- , Update upd
                TotalCol (SJust totcol)
              , Certs' (map unTxCertF cs)
              , Withdrawals' (Withdrawals ws)
              , Txfee fee
              , TTL tt
              , Vldt vi
              , Fields.Mint (liftMultiAsset mnt)
              , ReqSignerHashes req
              , Txnetworkid (maybeToStrictMaybe net)
              , AdHash (maybeToStrictMaybe adh)
              , WppHash (maybeToStrictMaybe wpp)
              , TreasuryDonation donate
              ]
          )

-- ==================================================
-- Hardforks

allowMIRTransfer :: Proof era -> Term era Bool
allowMIRTransfer p = Lit BoolR (HardForks.allowMIRTransfer (protocolVersion p))

-- ====================================
-- ConwayGovState variables

constitution :: Era era => Term era (Constitution era)
constitution = Var $ V "constitution" ConstitutionR No

enactTreasury :: Era era => Term era Coin
enactTreasury = Var $ V "enactTreasury" CoinR No

enactWithdrawals :: forall era. Era era => Term era (Map (Credential 'Staking) Coin)
enactWithdrawals = Var $ V "enactWithdrawals" (MapR CredR CoinR) No

currentGovActionStates ::
  Era era => Term era (Map GovActionId (GovActionState era))
currentGovActionStates = Var $ V "currentGovActionStates" (MapR GovActionIdR GovActionStateR) No

currentProposalOrder :: Era era => Term era [GovActionId]
currentProposalOrder = Var $ V "currentProposalOrder" (ListR GovActionIdR) No

prevGovActionStates :: Era era => Term era (Map GovActionId (GovActionState era))
prevGovActionStates = Var $ V "prevGovActionStates" (MapR GovActionIdR GovActionStateR) No

prevProposalOrder :: Era era => Term era [GovActionId]
prevProposalOrder = Var $ V "prevProposalOrder" (ListR GovActionIdR) No

previousCommitteeState ::
  Era era =>
  Term
    era
    ( Map
        (Credential 'ColdCommitteeRole)
        (Maybe (Credential 'HotCommitteeRole))
    )
previousCommitteeState = Var $ V "previousCommitteeState" (MapR CommColdCredR (MaybeR CommHotCredR)) No

commMembers :: Era era => Term era (Map (Credential 'ColdCommitteeRole) EpochNo)
commMembers = Var $ V "commMembers" (MapR CommColdCredR EpochR) No

commQuorum :: Era era => Term era UnitInterval
commQuorum = Var $ V "commQuorum" UnitIntervalR No

committeeVar :: Era era => Term era (Maybe (Committee era))
committeeVar = Var $ V "committeeVar" (MaybeR CommitteeR) No

-- ====================================
-- ConwayGovState Targets

-- ================
-- The DRepPulsingState has two forms
-- 1. DRPulsing
-- 2. DRComplete
-- They both act as Snapshots, storing information from previous epochs
-- DRPulsing stores each 'prevXXX' as a field, and
-- DRComplete stores them in a dedicated datatype PulsingSnapshot

-- | There are 2 forms of DRepPulsingState. This is part of the first one where the pulsing is
--   not complete, and the snapshots are stored as fields in the datatype 'DRepPulser'.
--   Note that the function part of 'Invert' : 'initPulser' makes many transformations from the
--   types used in the Model, and the types stored in the implementation types.
--   In order to construct a valid DRepPulser we need the UTxO (to compute the IncrementalStake)
--   But we cannot find a Lens that can recover the UTxO from a DRepPulser. So we introduce this
--   type ' UtxoPulse' that pairs the two (which makes the recovery possible). W
type UtxoPulse era =
  (Map TxIn (TxOutF era), DRepPulser era Identity (RatifyState era))

-- | We also introduce an intermediate variable 'utxoPulse' which can constrain this value
--   by using the predicate [ utxoPulse p :<-: pulsingPair p ]
utxoPulse :: (RunConwayRatify era, Reflect era) => Proof era -> Term era (UtxoPulse era)
utxoPulse p = Var $ V "utxoPulse" (PairR (MapR TxInR (TxOutR p)) DRepPulserR) No

-- | an invertable RootTarget to compute a (UtxoPulse era)
pulsingPairT ::
  forall era.
  (RunConwayRatify era, Reflect era, ConwayEraCertState era) =>
  Proof era ->
  RootTarget era (UtxoPulse era) (UtxoPulse era)
pulsingPairT proof =
  Invert
    "DRepPulser"
    (typeRep @(UtxoPulse era))
    (\utx a b c d e f g h -> (utx, initPulser proof utx a b c d e f g h))
    :$ Lensed (utxo proof) _1
    :$ Virtual drepDelegation (ppString "prevDRepDelegations") (_2 . prevDRepDelegationsL)
    :$ Virtual poolDistr (ppString "prevPoolDistr") (_2 . prevPoolDistrL)
    :$ Virtual currentDRepState (ppString "prevDRepState") (_2 . prevDRepStateL)
    :$ Virtual currentEpoch (ppString "prevEpoch") (_2 . prevEpochL)
    :$ Virtual committeeState (ppString "prevCommitteeState") (_2 . prevCommitteeStateL)
    :$ Shift enactStateT (_2 . prevEnactStateL)
    :$ Virtual currGovStates (ppString "prevProposals") (_2 . ratifyGovActionStatesL)
    :$ Virtual regPools (ppString "prevPoolParams") (_2 . prevRegPoolsL)

-- TODO access prevTreasury from the EnactState
--  :$ Virtual treasury (ppString "prevTreasury") (_2 . prevTreasuryL)

justPulser ::
  forall era.
  (Reflect era, RunConwayRatify era, ConwayEraCertState era) =>
  Proof era ->
  RootTarget
    era
    (DRepPulser era Identity (RatifyState era))
    (DRepPulser era Identity (RatifyState era))
justPulser p =
  Invert "DRepPulser" (typeRep @(DRepPulser era Identity (RatifyState era))) (initPulser p Map.empty)
    :$ Virtual drepDelegation (ppString "prevDRepDelegations") prevDRepDelegationsL
    :$ Virtual poolDistr (ppString "prevPoolDistr") prevPoolDistrL
    :$ Virtual currentDRepState (ppString "prevDRepState") prevDRepStateL
    :$ Virtual currentEpoch (ppString "prevEpoch") prevEpochL
    :$ Virtual committeeState (ppString "prevCommitteeState") prevCommitteeStateL
    :$ Shift enactStateT prevEnactStateL
    :$ Virtual currGovStates (ppString "prevProposals") ratifyGovActionStatesL
    :$ Virtual regPools (ppString "prevPoolParams") prevRegPoolsL

-- TODO access prevTreasury from the EnactState
-- :$ Virtual treasury (ppString "prevTreasury") (prevTreasuryL)

-- | Variable used to constrain the DRepPulser
drepPulser ::
  (RunConwayRatify era, Reflect era) => Term era (DRepPulser era Identity (RatifyState era))
drepPulser = Var $ V "drepPulser" DRepPulserR No

-- | Predicates that constrain the DRepPuser and all its 'prevXXX' snapshots
--   These ensure we generate state just passing the epoch boundary
prevPulsingPreds ::
  (RunConwayRatify era, Reflect era, ConwayEraCertState era) => Proof era -> [Pred era]
prevPulsingPreds p =
  [ Sized (ExactSize 0) (Dom enactWithdrawals)
  , Lit CoinR (Coin 0) :=: enactTreasury
  , utxoPulse p :<-: pulsingPairT p
  , prevGovActionIds
      :<-: ( Constr "PrevGovActionIdsFromProposals" (\cp -> toPrevGovActionIds (cp ^. pRootsL))
               ^$ (currProposals p)
           )
  , currGovStates :<-: (Constr "proposalsActions" (toList . proposalsActions) ^$ currProposals p)
  , select drepPulser (utxoPulse p) _2
  , select partialDRepDistr drepPulser partialDRepDistrL
  , select prevDRepDelegations drepPulser prevDRepDelegationsL
  , select prevPoolDistr drepPulser prevPoolDistrL
  , select prevDRepState drepPulser prevDRepStateL
  , select prevEpoch drepPulser prevEpochL
  , select prevCommitteeState drepPulser prevCommitteeStateL
  , select prevEnactState drepPulser prevEnactStateL
  , currProposals p :=: prevProposals p
  -- TODO access prevTreasury from the EnactState
  -- , select prevTreasury drepPulser prevTreasuryL
  ]

-- | Target for assembling 'DRPulsing' form of (DRepPulsingState era)
--   from 'drepPulser' :: forall era. Term era (DRepPulser era Identity (RatifyState era))
pulsingPulsingStateT ::
  forall era.
  (RunConwayRatify era, Reflect era, ConwayEraCertState era) =>
  RootTarget era (DRepPulsingState era) (DRepPulsingState era)
pulsingPulsingStateT =
  Invert "DRPulsing" (typeRep @(DRepPulsingState era)) DRPulsing
    :$ Virtual drepPulser (ppTarget (justPulser @era reify)) pulsingStatePulserL

-- | The Lens' used in pulsingPulsingStateT
pulsingStatePulserL :: Lens' (DRepPulsingState era) (DRepPulser era Identity (RatifyState era))
pulsingStatePulserL = lens getter setter
  where
    getter (DRPulsing x) = x
    getter (DRComplete _ _) =
      error ("Can't turn a DRCompete into a DRPulsing in pulsingStatePulserL lens.")
    {- There is a way we could do this, by partitioning the prev parts of 'DRPulsing' into
       3 parts RatifyState, RatifySignal, and RatifyEnv, and then making Store those 3
       instead of storing a single PulsingSnapshot. Then we could reassemble a DRPulsing
       that was ready for completion.
    -}
    setter (DRPulsing _) x = DRPulsing x
    setter (DRComplete _ _) x = DRPulsing x

-- | The abstract form of DRepPulser that transforms from the Model types
--   used in the inputs, and the concrete types actually stored in 'DRepPulser'
initPulser ::
  forall era.
  (Reflect era, RunConwayRatify era) =>
  Proof era ->
  Map TxIn (TxOutF era) ->
  Map (Credential 'Staking) DRep ->
  Map (KeyHash 'StakePool) IndividualPoolStake ->
  Map (Credential 'DRepRole) DRepState ->
  EpochNo ->
  Map (Credential 'ColdCommitteeRole) CommitteeAuthorization ->
  EnactState era ->
  [GovActionState era] ->
  -- Coin ->
  Map (KeyHash 'StakePool) PoolParams ->
  DRepPulser era Identity (RatifyState era)
initPulser proof utx credDRepMap poold credDRepStateMap epoch commstate enactstate govstates {- treas -} poolParams =
  let umap = unify Map.empty Map.empty Map.empty credDRepMap
      umapSize = Map.size credDRepMap
      k = securityParameter testGlobals
      instantStake = addInstantStake (utx ^. utxoFL proof) mempty
   in DRepPulser
        (max 1 (ceiling (toInteger umapSize %. (knownNonZero @8 `mulNonZero` toIntegerNonZero k))))
        umap
        0
        instantStake
        (PoolDistr poold $ CompactCoin 1)
        Map.empty
        credDRepStateMap
        epoch
        (CommitteeState commstate)
        enactstate
        (SS.fromList govstates)
        (proposalsDeposits $ def & pPropsL .~ OMap.fromFoldable govstates)
        -- treas
        testGlobals
        poolParams

proposalsT :: forall era. Era era => Proof era -> RootTarget era (Proposals era) (Proposals era)
proposalsT proof =
  Invert "Proposals" (typeRep @(Proposals era)) id
    :$ Lensed (currProposals proof) (lens id $ const id)

-- ==================================================
-- Second form of DRepPulsingState 'DRComplete'
-- ==================================================

-- | The snapshot dedicated datatype (PulsingSnapshot era) stored inside 'DRComplete'
--   Note this is used in 'dRepPulsingStateT', the second  DRepPulsingState form.
pulsingSnapshotT ::
  forall era. Era era => RootTarget era (PulsingSnapshot era) (PulsingSnapshot era)
pulsingSnapshotT =
  Invert
    "PulsingSnapshot"
    (typeRep @(PulsingSnapshot era))
    ( \a b c d -> PulsingSnapshot (SS.fromList a) (Map.map compactCoinOrError b) c (Map.map compactCoinOrError d)
    )
    :$ Lensed currGovStates (psProposalsL . strictSeqListL)
    :$ Lensed partialDRepDistr (psDRepDistrL . mapCompactFormCoinL)
    :$ Lensed prevDRepState psDRepStateL
    :$ Lensed partialIndividualPoolStake (psPoolDistrL . mapCompactFormCoinL)

pulsingSnapshotL :: EraStake era => Lens' (DRepPulsingState era) (PulsingSnapshot era)
pulsingSnapshotL = lens getter setter
  where
    getter (DRComplete x _) = x
    getter other = fst (finishDRepPulser other)
    setter (DRComplete _ y) x = DRComplete x y
    setter other x = DRComplete x y
      where
        (_, y) = finishDRepPulser other

-- | There are 2 forms of DRepPulsingState. This is the second one
--   where the pulsing is complete
completePulsingStateT ::
  forall era.
  Reflect era =>
  Proof era ->
  RootTarget era (DRepPulsingState era) (DRepPulsingState era)
completePulsingStateT _p =
  Invert "DRComplete" (typeRep @(DRepPulsingState era)) DRComplete
    :$ Shift pulsingSnapshotT pulsingSnapshotL
    :$ Lensed ratifyState ratifyStateL

ratifyState :: Reflect era => Term era (RatifyState era)
ratifyState = Var $ V "ratifyState" RatifyStateR No

ratifyStateL :: Reflect era => Lens' (DRepPulsingState era) (RatifyState era)
ratifyStateL = lens getter setter
  where
    getter (DRComplete _ y) = y
    getter x@(DRPulsing {}) = snd (finishDRepPulser x)
    setter (DRComplete x _) y = DRComplete x y
    setter z@(DRPulsing {}) y = case finishDRepPulser z of
      (x, _) -> DRComplete x y

prevProposals :: Era era => Proof era -> Term era (Proposals era)
prevProposals p = Var $ V "prevProposals" (ProposalsR p) No

ratifyGovActionStatesL :: Lens' (DRepPulser era Identity (RatifyState era)) [GovActionState era]
ratifyGovActionStatesL =
  lens
    (F.toList . dpProposals)
    (\x y -> x {dpProposals = SS.fromList y})

-- | Partially computed DRepDistr inside the pulser
partialDRepDistr :: Era era => Term era (Map DRep Coin)
partialDRepDistr = Var $ V "partialDRepDistr" (MapR DRepR CoinR) No

partialDRepDistrL ::
  Lens' (DRepPulser era Identity (RatifyState era)) (Map DRep Coin)
partialDRepDistrL =
  lens
    (Map.map fromCompact . dpDRepDistr)
    (\x y -> x {dpDRepDistr = Map.map compactCoinOrError y})

-- | Snapshot of 'dreps' from the start of the current epoch
prevDRepState ::
  Era era => Term era (Map (Credential 'DRepRole) DRepState)
prevDRepState = Var $ V "prevDRepState" (MapR VCredR DRepStateR) No

prevDRepStateL ::
  Lens'
    (DRepPulser era Identity (RatifyState era))
    (Map (Credential 'DRepRole) DRepState)
prevDRepStateL = lens dpDRepState (\x y -> x {dpDRepState = y})

-- | snapshot of 'poolDistr' from the start of the current epoch
prevPoolDistr ::
  Era era => Term era (Map (KeyHash 'StakePool) IndividualPoolStake)
prevPoolDistr = Var $ V "prevPoolDistr" (MapR PoolHashR IPoolStakeR) No

prevPoolDistrL ::
  Lens'
    (DRepPulser era Identity (RatifyState era))
    (Map (KeyHash 'StakePool) IndividualPoolStake)
prevPoolDistrL =
  lens
    (\x -> unPoolDistr (dpStakePoolDistr x))
    (\x y -> x {dpStakePoolDistr = PoolDistr y $ CompactCoin 1})

-- | Snapshot of the 'drepDelegation' from he start of the current epoch.
prevDRepDelegations ::
  Era era => Term era (Map (Credential 'Staking) DRep)
prevDRepDelegations = Var $ V "prevDRepDelegations" (MapR CredR DRepR) No

-- | Snapshot of 'drepDelegation' from the start of the current epoch
prevDRepDelegationsL ::
  Lens'
    (DRepPulser era Identity (RatifyState era))
    (Map (Credential 'Staking) DRep)
prevDRepDelegationsL =
  lens
    (\x -> dpUMap x ^. drepUMapL)
    ( \x y ->
        x
          { dpUMap =
              unify
                (rdPairMap (dpUMap x))
                (ptrMap (dpUMap x))
                (sPoolMap (dpUMap x))
                y
          }
    )

-- | Snapshot of 'committeeState' from the start of the current epoch
prevCommitteeState ::
  Era era =>
  Term
    era
    (Map (Credential 'ColdCommitteeRole) CommitteeAuthorization)
prevCommitteeState = Var $ V "prevCommitteeState" (MapR CommColdCredR CommitteeAuthorizationR) No

prevCommitteeStateL ::
  Lens'
    (DRepPulser era Identity (RatifyState era))
    (Map (Credential 'ColdCommitteeRole) CommitteeAuthorization)
prevCommitteeStateL =
  lens
    (csCommitteeCreds . dpCommitteeState)
    (\x y -> x {dpCommitteeState = CommitteeState y})

-- | Snapshot of the enactState built by 'enactStateT' assembled from data at the start the current epoch
prevEnactState :: Reflect era => Term era (EnactState era)
prevEnactState = Var $ V "prevEnactState" EnactStateR No

prevEnactStateL :: Lens' (DRepPulser era Identity (RatifyState era)) (EnactState era)
prevEnactStateL = lens dpEnactState (\x y -> x {dpEnactState = y})

-- | Snapshot of 'currentEpoch' just before the start of the current epoch. (currenEpoch - 1)
prevEpoch :: Era era => Term era EpochNo
prevEpoch = Var $ V "prevEpoch" EpochR No

prevEpochL :: Lens' (DRepPulser era Identity (RatifyState era)) EpochNo
prevEpochL = lens dpCurrentEpoch (\x y -> x {dpCurrentEpoch = y})

prevTreasury :: Era era => Term era Coin
prevTreasury = Var $ V "prevTreasury" CoinR No

{-
-- TODO access prevTreasury from the EnactState
prevTreasuryL :: Lens' (DRepPulser era Identity (RatifyState era)) Coin
prevTreasuryL = lens dpTreasury (\x y -> x {dpTreasury = y})
-}

partialIndividualPoolStake :: Era era => Term era (Map (KeyHash 'StakePool) Coin)
partialIndividualPoolStake = Var $ V "partialIndividualPoolStake" (MapR PoolHashR CoinR) No

prevRegPools ::
  Era era => Term era (Map (KeyHash 'StakePool) PoolParams)
prevRegPools = Var $ V "prevRegPools" (MapR PoolHashR PoolParamsR) No

prevRegPoolsL ::
  Lens'
    (DRepPulser era Identity (RatifyState era))
    (Map (KeyHash 'StakePool) PoolParams)
prevRegPoolsL =
  lens
    dpPoolParams
    (\x y -> x {dpPoolParams = y})

-- ======================================
-- ConwayGovState

conwayGovStateT ::
  forall era.
  (RunConwayRatify era, Reflect era, ConwayEraCertState era) =>
  Proof era ->
  RootTarget era (ConwayGovState era) (ConwayGovState era)
conwayGovStateT p =
  Invert
    "ConwayGovState"
    (typeRep @(ConwayGovState era))
    ( \pr com con (PParamsF _ cpp) (PParamsF _ ppp) pu -> ConwayGovState pr (maybeToStrictMaybe com) con cpp ppp pu
    )
    :$ Lensed (currProposals p) cgsProposalsL
    :$ Lensed committeeVar (cgsCommitteeL . strictMaybeToMaybeL) -- see 'committeeT' to construct a binding for committeeVar
    :$ Lensed constitution cgsConstitutionL
    :$ Lensed (currPParams reify) (cgsCurPParamsL . pparamsFL reify)
    :$ Lensed (prevPParams reify) (cgsPrevPParamsL . pparamsFL reify)
    :$ Lensed (futurePParams reify) cgsFuturePParamsL
    :$ Shift pulsingPulsingStateT cgsDRepPulsingStateL

-- | The sum of all the 'gasDeposit' fields of 'currProposals'
proposalDeposits :: Era era => Term era Coin
proposalDeposits = Var (V "proposalDeposits" CoinR No)

-- | A view of 'currentDRepState' (sum of the drepDeposit field of in the range of 'currentDRepState')
drepDepositsView :: Era era => Term era (Map (Credential 'DRepRole) Coin)
drepDepositsView = Var (V "drepDepositsView" (MapR VCredR CoinR) No)

-- | The current set of proposals. Proposals has a serious set of invariants.
--   We do not attempt to state these proposals (Yes I know that is cheating)
--   We get random Proposals (that meets its invariants) by using (genSizedRep n (ProposalsR p))
currProposals :: Era era => Proof era -> Term era (Proposals era)
currProposals p = Var $ V "currProposals" (ProposalsR p) No

-- | Part of the EnactState, it is computed by selecting from currProposals
prevGovActionIds :: forall era. Reflect era => Term era (GovRelation StrictMaybe era)
prevGovActionIds = Var $ V "prevGovActionIds" PrevGovActionIdsR No

-- | This is a view of currProposals, so will compute it once
--   once currProposals is defined
currGovStates :: Era era => Term era [GovActionState era]
currGovStates = Var (V "currGovStates" (ListR GovActionStateR) No)

enactStateT :: forall era. Reflect era => RootTarget era (EnactState era) (EnactState era)
enactStateT =
  Invert
    "EnactState"
    (typeRep @(EnactState era))
    (\x y (PParamsF _ z) (PParamsF _ w) a b c -> EnactState (maybeToStrictMaybe x) y z w a b c)
    :$ Lensed committeeVar (ensCommitteeL . strictMaybeToMaybeL) -- see 'committeeT' to construct a binding for committeeVar
    :$ Lensed constitution ensConstitutionL
    :$ Lensed (currPParams reify) (ensCurPParamsL . pparamsFL reify)
    :$ Lensed (prevPParams reify) (ensPrevPParamsL . pparamsFL reify)
    :$ Lensed enactTreasury ensTreasuryL
    :$ Lensed enactWithdrawals ensWithdrawalsL
    :$ Lensed prevGovActionIds ensPrevGovActionIdsL

-- | One can use this Target, to make a constraint for 'committeeVar' from the
--   vars 'commMembers' and 'commQuorum'
committeeT :: forall era. Era era => RootTarget era (Committee era) (Committee era)
committeeT =
  Invert "Committee" (typeRep @(Committee era)) Committee
    :$ Lensed commMembers committeeMembersL
    :$ Lensed commQuorum committeeThresholdL

{-
prevGovActionIdsT =
  Invert
    "prevGovActionIds"
    (typeRep @(PrevGovActionIds era))
    (\w x y z -> PrevGovActionIds $ PForest (maybeToStrictMaybe w) (maybeToStrictMaybe x) (maybeToStrictMaybe y) (maybeToStrictMaybe z))
    :$ Lensed prevPParamUpdate (prevGovActionIdsL . pfPParamUpdateL . strictMaybeToMaybeL)
    :$ Lensed prevHardFork (prevGovActionIdsL . pfHardForkL . strictMaybeToMaybeL)
    :$ Lensed prevCommittee (prevGovActionIdsL . pfCommitteeL . strictMaybeToMaybeL)
    :$ Lensed prevConstitution (prevGovActionIdsL . pfConstitutionL . strictMaybeToMaybeL)

prevPParamUpdate :: Era era => Term era (Maybe (GovPurposeId 'PParamUpdatePurpose era))
prevPParamUpdate = Var $ V "prevPParamUpdate" (MaybeR PrevPParamUpdateR) No

prevHardFork :: Era era => Term era (Maybe (GovPurposeId 'HardForkPurpose era))
prevHardFork = Var $ V "prevHardFork" (MaybeR PrevHardForkR) No

-- | Snapshot of 'committeeState' from the start of the current epoch
prevCommittee :: Era era => Term era (Maybe (GovPurposeId 'CommitteePurpose era))
prevCommittee = Var $ V "prevCommittee" (MaybeR PrevCommitteeR) No

prevConstitution :: Era era => Term era (Maybe (GovPurposeId 'ConstitutionPurpose era))
prevConstitution = Var $ V "prevConstitution" (MaybeR PrevConstitutionR) No

-}

ppUpdateChildren :: Era era => Term era (Set GovActionId)
ppUpdateChildren = Var $ V "ppUpdateChildren" (SetR GovActionIdR) No

hardForkChildren :: Era era => Term era (Set GovActionId)
hardForkChildren = Var $ V "hardForkChildren" (SetR GovActionIdR) No

committeeChildren :: Era era => Term era (Set GovActionId)
committeeChildren = Var $ V "committeeChildren" (SetR GovActionIdR) No

constitutionChildren :: Era era => Term era (Set GovActionId)
constitutionChildren = Var $ V "constitutionChildren" (SetR GovActionIdR) No

-- ================
-- Lenses

pparamsFL :: Proof era -> Lens' (PParams era) (PParamsF era)
pparamsFL p = lens (PParamsF p) (\_ (PParamsF _ x) -> x)

pparamsMaybeFL :: Proof era -> Lens' (Maybe (PParams era)) (Maybe (PParamsF era))
pparamsMaybeFL p =
  lens
    (fmap (PParamsF p))
    (\_ -> fmap (\(PParamsF _ x) -> x))

smCommL :: Lens' (StrictMaybe (Committee era)) (Committee era)
smCommL = lens getter (\_ t -> SJust t)
  where
    getter SNothing = Committee Map.empty maxBound
    getter (SJust x) = x

proposedMapL ::
  Proof era ->
  Lens' (ProposedPPUpdates era) (Map (KeyHash 'Genesis) (PParamsUpdateF era))
proposedMapL p =
  lens
    (\(ProposedPPUpdates x) -> Map.map (PParamsUpdateF p) x)
    (\(ProposedPPUpdates _) y -> ProposedPPUpdates (Map.map unPParamsUpdate y))

pair1 :: Era era => Rep era a -> Term era a
pair1 rep = Var (V "pair1" rep No)

pair2 :: Era era => Rep era b -> Term era b
pair2 rep = Var (V "pair2" rep No)

pairT ::
  forall era a b.
  (Typeable a, Typeable b, Era era) =>
  Rep era a ->
  Rep era b ->
  RootTarget era (a, b) (a, b)
pairT repa repb =
  Invert "(,)" (typeRep @(a, b)) (,)
    :$ Lensed (pair1 repa) fstL
    :$ Lensed (pair2 repb) sndL

-- ==========================================
-- Targets for GovActionState
-- The variables xxV align with the field selectors gasXx

idV :: Era era => Term era GovActionId
idV = Var (V "idV" GovActionIdR No)

committeeVotesV :: Era era => Term era (Map (Credential 'HotCommitteeRole) Vote)
committeeVotesV = Var (V "committeeVotesV" (MapR CommHotCredR VoteR) No)

drepVotesV :: Era era => Term era (Map (Credential 'DRepRole) Vote)
drepVotesV = Var (V "drepVotesV" (MapR VCredR VoteR) No)

stakePoolVotesV :: Era era => Term era (Map (KeyHash 'StakePool) Vote)
stakePoolVotesV = Var (V "stakePoolVotesV" (MapR PoolHashR VoteR) No)

depositV :: Era era => Term era Coin
depositV = Var (V "depositV" CoinR No)

returnAddrV :: Era era => Term era RewardAccount
returnAddrV = Var (V "returnAddrV" RewardAccountR No)

actionV :: Era era => Term era (GovAction era)
actionV = Var (V "actionV" GovActionR No)

proposedInV :: Era era => Term era EpochNo
proposedInV = Var (V "proposedInV" EpochR No)

expiresAfterV :: Era era => Term era EpochNo
expiresAfterV = Var (V "expiresAfterV" EpochR No)

childrenV :: Era era => Term era (Set GovActionId)
childrenV = Var (V "childrenV" (SetR GovActionIdR) No)

anchorV :: Era era => Term era Anchor
anchorV = Var (V "anchorV" AnchorR No)

govActionStateTarget ::
  forall era. Era era => RootTarget era (GovActionState era) (GovActionState era)
govActionStateTarget =
  Invert "GovActionState" (typeRep @(GovActionState era)) GovActionState
    :$ Lensed idV gasIdL
    :$ Lensed committeeVotesV gasCommitteeVotesL
    :$ Lensed drepVotesV gasDRepVotesL
    :$ Lensed stakePoolVotesV gasStakePoolVotesL
    :$ Shift
      ( Invert "ProposalProcedure" (typeRep @(ProposalProcedure era)) ProposalProcedure
          :$ Lensed depositV pProcDepositL
          :$ Lensed returnAddrV pProcReturnAddrL
          :$ Lensed actionV pProcGovActionL
          :$ Lensed anchorV pProcAnchorL
      )
      gasProposalProcedureL
    :$ Lensed proposedInV gasProposedInL
    :$ Lensed expiresAfterV gasExpiresAfterL

-- ==============================================================
-- Targets for GovAction, The model does not make the distinction
-- the newtype (PrevGovActionId era) and (GovActionId era), The
-- targets provide the coercions to produce the real data from the Model

-- | Lift the Model to the real type
liftId :: Maybe GovActionId -> StrictMaybe (GovPurposeId p era)
liftId x = GovPurposeId <$> (maybeToStrictMaybe x)

-- | Drop the real type back to the Model
dropId :: StrictMaybe (GovPurposeId p era) -> Maybe GovActionId
dropId x = unGovPurposeId <$> (strictMaybeToMaybe x)

-- =====================
-- Variables for the fields of GovAction

gaPrevId :: Era era => Term era (Maybe GovActionId)
gaPrevId = Var (V "gaPrevId" (MaybeR GovActionIdR) No)

gaPParamsUpdate :: Reflect era => Term era (PParamsUpdateF era)
gaPParamsUpdate = Var (V "gsPParamsUpdate" (PParamsUpdateR reify) No)

gaProtVer :: Reflect era => Term era ProtVer
gaProtVer = Var (V "gaProtVer" (ProtVerR reify) No)

gaRewardAccount :: Era era => Term era (Map RewardAccount Coin)
gaRewardAccount = Var (V "gaRewardAccount" (MapR RewardAccountR CoinR) No)

gaRemMember :: Era era => Term era (Set (Credential 'ColdCommitteeRole))
gaRemMember = Var (V "gaRemMember" (SetR CommColdCredR) No)

gaAddMember :: Era era => Term era (Map (Credential 'ColdCommitteeRole) EpochNo)
gaAddMember = Var (V "gaAddMember" (MapR CommColdCredR EpochR) No)

gaThreshold :: Era era => Term era UnitInterval
gaThreshold = Var (V "gaThreshold" UnitIntervalR No)

gaPolicy :: Era era => Term era (Maybe ScriptHash)
gaPolicy = Var (V "gaPolicy" (MaybeR ScriptHashR) No)

gaConstitutionAnchor :: Era era => Term era Anchor
gaConstitutionAnchor = Var (V "gaConstitutionAnchor" AnchorR No)

gaNewConstitution :: Era era => Term era (Constitution era)
gaNewConstitution = Var (V "gaNewConstitution" ConstitutionR No)

-- ===================================
-- The partial Targets, one for each constructor of GovAction

constitutionT :: forall era. Reflect era => RootTarget era (Constitution era) (Constitution era)
constitutionT =
  Invert "Constitution" (typeRep @(Constitution era)) (\x y -> Constitution x $ maybeToStrictMaybe y)
    :$ Lensed gaConstitutionAnchor constitutionAnchorL
    :$ Lensed gaPolicy (constitutionScriptL . strictMaybeToMaybeL)

parameterChangeT :: forall era. Reflect era => RootTarget era (GovAction era) (GovAction era)
parameterChangeT =
  Invert
    "ParameterChange"
    (typeRep @(GovAction era))
    (\x y c -> ParameterChange (liftId x) (unPParamsUpdate y) (maybeToStrictMaybe c))
    :$ Partial gaPrevId (\case (ParameterChange x _ _) -> Just $ dropId x; _ -> Nothing)
    :$ Partial
      gaPParamsUpdate
      (\case (ParameterChange _ y _) -> Just $ PParamsUpdateF reify y; _ -> Nothing)
    :$ Partial gaPolicy (\case (ParameterChange _ _ x) -> Just $ strictMaybeToMaybe x; _ -> Nothing)

hardForkInitiationT :: forall era. Reflect era => RootTarget era (GovAction era) (GovAction era)
hardForkInitiationT =
  Invert "HardForkInitiation" (typeRep @(GovAction era)) (\x y -> HardForkInitiation (liftId x) y)
    :$ Partial gaPrevId (\case (HardForkInitiation x _) -> Just $ dropId x; _ -> Nothing)
    :$ Partial gaProtVer (\case (HardForkInitiation _ y) -> Just y; _ -> Nothing)

treasuryWithdrawalsT :: forall era. Reflect era => RootTarget era (GovAction era) (GovAction era)
treasuryWithdrawalsT =
  Invert
    "TreasuryWithdrawals"
    (typeRep @(GovAction era))
    (\x y -> TreasuryWithdrawals x $ maybeToStrictMaybe y)
    :$ Partial gaRewardAccount (\case (TreasuryWithdrawals x _) -> Just x; _ -> Nothing)
    :$ Partial gaPolicy (\case (TreasuryWithdrawals _ y) -> Just $ strictMaybeToMaybe y; _ -> Nothing)

noConfidenceT :: forall era. Reflect era => RootTarget era (GovAction era) (GovAction era)
noConfidenceT =
  Invert "NoConfidence" (typeRep @(GovAction era)) (\x -> NoConfidence (liftId x))
    :$ Partial gaPrevId (\case (NoConfidence x) -> Just $ dropId x; _ -> Nothing)

updateCommitteeT :: forall era. Reflect era => RootTarget era (GovAction era) (GovAction era)
updateCommitteeT =
  Invert "UpdateCommittee" (typeRep @(GovAction era)) (\w x y z -> UpdateCommittee (liftId w) x y z)
    :$ Partial gaPrevId (\case (UpdateCommittee x _ _ _) -> Just $ dropId x; _ -> Nothing)
    :$ Partial gaRemMember (\case (UpdateCommittee _ x _ _) -> Just x; _ -> Nothing)
    :$ Partial gaAddMember (\case (UpdateCommittee _ _ x _) -> Just x; _ -> Nothing)
    :$ Partial gaThreshold (\case (UpdateCommittee _ _ _ x) -> Just x; _ -> Nothing)

newConstitutionT :: forall era. Reflect era => RootTarget era (GovAction era) (GovAction era)
newConstitutionT =
  Invert "NewConstitution" (typeRep @(GovAction era)) (\x y -> NewConstitution (liftId x) y)
    :$ Partial gaPrevId (\case (UpdateCommittee x _ _ _) -> Just $ dropId x; _ -> Nothing)
    :$ Partial gaNewConstitution (\case (NewConstitution _ y) -> Just y; _ -> Nothing)

infoActionT :: forall era. Reflect era => RootTarget era (GovAction era) (GovAction era)
infoActionT =
  Invert "InfoAction" (typeRep @(GovAction era)) (\() -> InfoAction)
    :$ Lensed (Lit UnitR ()) (to (const ()))
