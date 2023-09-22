{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}

module Test.Cardano.Ledger.Constrained.Vars where

import Cardano.Crypto.Signing (SigningKey)
import Cardano.Ledger.Address (Addr (..), Withdrawals (..))
import Cardano.Ledger.Allegra.Scripts (ValidityInterval (..))
import Cardano.Ledger.Alonzo.PParams (AlonzoEraPParams, ppCollateralPercentageL, ppMaxTxExUnitsL)
import Cardano.Ledger.Alonzo.Scripts (ExUnits (..))
import Cardano.Ledger.Alonzo.Scripts.Data (Data (..), Datum (..))
import Cardano.Ledger.Alonzo.Tx (IsValid (..), ScriptIntegrityHash, ScriptPurpose (..))
import Cardano.Ledger.Alonzo.TxWits (RdmrPtr (..), Redeemers (..), TxDats (..))
import Cardano.Ledger.Alonzo.UTxO (AlonzoScriptsNeeded (..))
import Cardano.Ledger.AuxiliaryData (AuxiliaryDataHash)
import Cardano.Ledger.BaseTypes (BlocksMade (..), EpochNo, Network (..), ProtVer (..), SlotNo (..), StrictMaybe (..), UnitInterval)
import qualified Cardano.Ledger.BaseTypes as Base (Globals (..))
import Cardano.Ledger.CertState (CommitteeState (..), csCommitteeCredsL, vsNumDormantEpochsL)
import Cardano.Ledger.Coin (Coin (..), DeltaCoin)
import Cardano.Ledger.Conway.Governance hiding (GovState, ensTreasuryL, ensWithdrawalsL)
import Cardano.Ledger.Core (
  EraPParams,
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
import Cardano.Ledger.DRepDistr (DRepDistr (..), DRepState)
import Cardano.Ledger.EpochBoundary (SnapShot (..), SnapShots (..), Stake (..))
import Cardano.Ledger.Era (Era (EraCrypto))
import Cardano.Ledger.Hashes (DataHash, EraIndependentScriptIntegrity, ScriptHash (..))
import Cardano.Ledger.Keys (GenDelegPair, GenDelegs (..), KeyHash, KeyRole (..))
import Cardano.Ledger.Keys.Bootstrap (BootstrapWitness)
import Cardano.Ledger.Keys.WitVKey (WitVKey (..))
import Cardano.Ledger.Mary.Value (AssetName (..), MaryValue (..), MultiAsset (..), PolicyID (..))
import Cardano.Ledger.PoolDistr (IndividualPoolStake (..), PoolDistr (..))
import Cardano.Ledger.PoolParams (PoolParams)
import Cardano.Ledger.SafeHash (SafeHash)
import qualified Cardano.Ledger.Shelley.Governance as Gov
import Cardano.Ledger.Shelley.HardForks as HardForks (allowMIRTransfer)
import Cardano.Ledger.Shelley.LedgerState hiding (credMapL, delegations, deltaReserves, deltaTreasury, rewards)
import Cardano.Ledger.Shelley.PParams (ProposedPPUpdates (..))
import Cardano.Ledger.Shelley.PoolRank (NonMyopic (..))
import qualified Cardano.Ledger.Shelley.RewardUpdate as RU
import Cardano.Ledger.Shelley.Rewards (Reward (..))
import Cardano.Ledger.Shelley.TxBody (RewardAcnt (..))
import Cardano.Ledger.Shelley.UTxO (EraUTxO (..), ShelleyScriptsNeeded (..))
import Cardano.Ledger.TxIn (TxIn)
import Cardano.Ledger.UMap (compactCoinOrError, fromCompact)
import Cardano.Ledger.UTxO (UTxO (..))
import Cardano.Ledger.Val (Val (..))
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe.Strict (maybeToStrictMaybe)
import qualified Data.Sequence.Strict as SS
import Data.Set (Set)
import qualified Data.VMap as VMap
import Data.Word (Word64)
import GHC.Stack (HasCallStack)
import Lens.Micro
import Numeric.Natural (Natural)
import Test.Cardano.Ledger.Babbage.Serialisation.Generators ()
import Test.Cardano.Ledger.Constrained.Ast (
  RootTarget (..),
  Target,
  Term (Lit, Var),
  fieldToTerm,
  ppTarget,
  (^$),
 )
import Test.Cardano.Ledger.Constrained.Classes (
  GovState (..),
  PParamsF (..),
  PParamsUpdateF (..),
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
  unPParamsUpdate,
  unScriptF,
  unTxCertF,
  unTxOut,
  unValue,
 )
import Test.Cardano.Ledger.Constrained.Env (Access (..), AnyF (..), Field (..), Name (..), V (..), pV)
import Test.Cardano.Ledger.Constrained.Lenses
import Test.Cardano.Ledger.Constrained.TypeRep (Rep (..), testEql, (:~:) (Refl))
import Test.Cardano.Ledger.Core.KeyPair (KeyPair (..))
import Test.Cardano.Ledger.Generic.Fields (TxBodyField (..), TxField (..), WitnessesField (..))
import qualified Test.Cardano.Ledger.Generic.Fields as Fields
import Test.Cardano.Ledger.Generic.Functions (protocolVersion)
import Test.Cardano.Ledger.Generic.Proof
import Test.Cardano.Ledger.Generic.Updaters (merge, newPParams, newTx, newTxBody, newWitnesses)
import qualified Test.Cardano.Ledger.Shelley.Utils as Utils (testGlobals)
import Type.Reflection (typeRep)

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

prevBlocksMade :: Era era => Term era (Map (KeyHash 'StakePool (EraCrypto era)) Natural)
prevBlocksMade = Var $ V "prevBlocksMade" (MapR PoolHashR NaturalR) (Yes NewEpochStateR nesBprevL)

currBlocksMade :: Era era => Term era (Map (KeyHash 'StakePool (EraCrypto era)) Natural)
currBlocksMade = Var $ V "currBlocksMade" (MapR PoolHashR NaturalR) (Yes NewEpochStateR nesBcurL)

poolDistr :: Era era => Term era (Map (KeyHash 'StakePool (EraCrypto era)) (IndividualPoolStake (EraCrypto era)))
poolDistr = Var $ V "poolDistr" (MapR PoolHashR IPoolStakeR) (Yes NewEpochStateR poolDistrL)

-- | For tests only, Like PoolDistr but has a Rational (rather than a IndividualPoolStake).
mockPoolDistr :: Era era => Term era (Map (KeyHash 'StakePool (EraCrypto era)) Rational)
mockPoolDistr = Var $ V "mockPoolDistr" (MapR PoolHashR RationalR) No

poolDistrL :: NELens era (Map (KeyHash 'StakePool (EraCrypto era)) (IndividualPoolStake (EraCrypto era)))
poolDistrL = nesPdL . unPoolDistrL

unPoolDistrL :: Lens' (PoolDistr c) (Map (KeyHash 'StakePool c) (IndividualPoolStake c))
unPoolDistrL = lens unPoolDistr (\(PoolDistr _) x -> PoolDistr x)

-- CertState - DState

rewards :: Era era => Term era (Map (Credential 'Staking (EraCrypto era)) Coin)
rewards = Var $ V "rewards" (MapR CredR CoinR) (Yes NewEpochStateR rewardsL)

rewardsL :: NELens era (Map (Credential 'Staking (EraCrypto era)) Coin)
rewardsL = nesEsL . esLStateL . lsCertStateL . certDStateL . dsUnifiedL . rewardsUMapL

delegations :: Era era => Term era (Map (Credential 'Staking (EraCrypto era)) (KeyHash 'StakePool (EraCrypto era)))
delegations = Var $ V "delegations" (MapR CredR PoolHashR) (Yes NewEpochStateR delegationsL)

delegationsL :: NELens era (Map (Credential 'Staking (EraCrypto era)) (KeyHash 'StakePool (EraCrypto era)))
delegationsL = nesEsL . esLStateL . lsCertStateL . certDStateL . dsUnifiedL . delegationsUMapL

stakeDeposits :: Era era => Term era (Map (Credential 'Staking (EraCrypto era)) Coin)
stakeDeposits = Var $ V "stakeDeposits" (MapR CredR CoinR) (Yes NewEpochStateR stakeDepositsL)

stakeDepositsL :: NELens era (Map (Credential 'Staking (EraCrypto era)) Coin)
stakeDepositsL = nesEsL . esLStateL . lsCertStateL . certDStateL . dsUnifiedL . stakeDepositsUMapL

ptrs :: Era era => Term era (Map Ptr (Credential 'Staking (EraCrypto era)))
ptrs = Var $ V "ptrs" (MapR PtrR CredR) (Yes NewEpochStateR ptrsL)

ptrsL :: NELens era (Map Ptr (Credential 'Staking (EraCrypto era)))
ptrsL = nesEsL . esLStateL . lsCertStateL . certDStateL . dsUnifiedL . ptrsUMapL

futureGenDelegs :: Era era => Term era (Map (FutureGenDeleg (EraCrypto era)) (GenDelegPair (EraCrypto era)))
futureGenDelegs =
  Var $
    V
      "futureGenDelegs"
      (MapR FutureGenDelegR GenDelegPairR)
      (Yes NewEpochStateR futureGenDelegsL)

futureGenDelegsL :: NELens era (Map (FutureGenDeleg (EraCrypto era)) (GenDelegPair (EraCrypto era)))
futureGenDelegsL = nesEsL . esLStateL . lsCertStateL . certDStateL . dsFutureGenDelegsL

genDelegs :: Era era => Term era (Map (KeyHash 'Genesis (EraCrypto era)) (GenDelegPair (EraCrypto era)))
genDelegs = Var $ V "genDelegs" (MapR GenHashR GenDelegPairR) (Yes NewEpochStateR genDelegsL)

genDelegsL :: NELens era (Map (KeyHash 'Genesis (EraCrypto era)) (GenDelegPair (EraCrypto era)))
genDelegsL = nesEsL . esLStateL . lsCertStateL . certDStateL . dsGenDelegsL . unGenDelegsL

-- DState - InstantaneousRewards

instanReserves :: Era era => Term era (Map (Credential 'Staking (EraCrypto era)) Coin)
instanReserves = Var $ V "instanReserves" (MapR CredR CoinR) (Yes NewEpochStateR instanReservesL)

instanReservesL :: NELens era (Map (Credential 'Staking (EraCrypto era)) Coin)
instanReservesL = nesEsL . esLStateL . lsCertStateL . certDStateL . dsIRewardsL . iRReservesL

instanReservesSum :: Era era => Term era Coin
instanReservesSum = Var (V "instanReservesSum" CoinR No)

instanTreasury :: Era era => Term era (Map (Credential 'Staking (EraCrypto era)) Coin)
instanTreasury = Var $ V "instanTreasury" (MapR CredR CoinR) (Yes NewEpochStateR instanTreasuryL)

instanTreasuryL :: NELens era (Map (Credential 'Staking (EraCrypto era)) Coin)
instanTreasuryL = nesEsL . esLStateL . lsCertStateL . certDStateL . dsIRewardsL . iRTreasuryL

instanTreasurySum :: Era era => Term era Coin
instanTreasurySum = Var (V "instanTreasurySum" CoinR No)

deltaReserves :: Era era => Term era DeltaCoin
deltaReserves = Var $ V "deltaReserves" DeltaCoinR (Yes NewEpochStateR deltaReservesNEL)

deltaReservesNEL :: NELens era DeltaCoin
deltaReservesNEL = nesEsL . esLStateL . lsCertStateL . certDStateL . dsIRewardsL . deltaReservesL

deltaTreasury :: Era era => Term era DeltaCoin
deltaTreasury = Var $ V "deltaTreasury" DeltaCoinR (Yes NewEpochStateR deltaTreasuryNEL)

deltaTreasuryNEL :: NELens era DeltaCoin
deltaTreasuryNEL = nesEsL . esLStateL . lsCertStateL . certDStateL . dsIRewardsL . deltaTreasuryL

-- CertState - PState

regPools :: Era era => Term era (Map (KeyHash 'StakePool (EraCrypto era)) (PoolParams (EraCrypto era)))
regPools = Var $ V "regPools" (MapR PoolHashR PoolParamsR) (Yes NewEpochStateR regPoolsL)

regPoolsL :: NELens era (Map (KeyHash 'StakePool (EraCrypto era)) (PoolParams (EraCrypto era)))
regPoolsL = nesEsL . esLStateL . lsCertStateL . certPStateL . psStakePoolParamsL

futureRegPools :: Era era => Term era (Map (KeyHash 'StakePool (EraCrypto era)) (PoolParams (EraCrypto era)))
futureRegPools = Var $ V "futureRegPools" (MapR PoolHashR PoolParamsR) (Yes NewEpochStateR futureRegPoolsL)

futureRegPoolsL :: NELens era (Map (KeyHash 'StakePool (EraCrypto era)) (PoolParams (EraCrypto era)))
futureRegPoolsL = nesEsL . esLStateL . lsCertStateL . certPStateL . psFutureStakePoolParamsL

retiring :: Era era => Term era (Map (KeyHash 'StakePool (EraCrypto era)) EpochNo)
retiring = Var $ V "retiring" (MapR PoolHashR EpochR) (Yes NewEpochStateR retiringL)

retiringL :: NELens era (Map (KeyHash 'StakePool (EraCrypto era)) EpochNo)
retiringL = nesEsL . esLStateL . lsCertStateL . certPStateL . psRetiringL

poolDeposits :: Era era => Term era (Map (KeyHash 'StakePool (EraCrypto era)) Coin)
poolDeposits = Var $ V "poolDeposits" (MapR PoolHashR CoinR) (Yes NewEpochStateR poolDepositsL)

poolDepositsL :: NELens era (Map (KeyHash 'StakePool (EraCrypto era)) Coin)
poolDepositsL = nesEsL . esLStateL . lsCertStateL . certPStateL . psDepositsL

dreps :: Era era => Term era (Map (Credential 'DRepRole (EraCrypto era)) (DRepState (EraCrypto era)))
dreps = Var $ V "dreps" (MapR VCredR DRepStateR) (Yes NewEpochStateR drepsL)

drepsL :: NELens era (Map (Credential 'DRepRole (EraCrypto era)) (DRepState (EraCrypto era)))
drepsL = nesEsL . esLStateL . lsCertStateL . certVStateL . vsDRepsL

committeeState :: Era era => Term era (Map (Credential 'ColdCommitteeRole (EraCrypto era)) (Maybe (Credential 'HotCommitteeRole (EraCrypto era))))
committeeState = Var $ V "committeeState" (MapR CommColdCredR (MaybeR CommHotCredR)) (Yes NewEpochStateR committeeStateL)

committeeStateL :: NELens era (Map (Credential 'ColdCommitteeRole (EraCrypto era)) (Maybe (Credential 'HotCommitteeRole (EraCrypto era))))
committeeStateL = nesEsL . esLStateL . lsCertStateL . certVStateL . vsCommitteeStateL . csCommitteeCredsL

numDormantEpochs :: Era era => Term era EpochNo
numDormantEpochs = Var $ V "numDormantEpochs" EpochR (Yes NewEpochStateR numDormantEpochsL)

numDormantEpochsL :: NELens era EpochNo
numDormantEpochsL = nesEsL . esLStateL . lsCertStateL . certVStateL . vsNumDormantEpochsL

-- UTxOState

utxo :: Era era => Proof era -> Term era (Map (TxIn (EraCrypto era)) (TxOutF era))
utxo p = Var $ pV p "utxo" (MapR TxInR (TxOutR p)) (Yes NewEpochStateR (utxoL p))

utxoL :: Proof era -> NELens era (Map (TxIn (EraCrypto era)) (TxOutF era))
utxoL proof = nesEsL . esLStateL . lsUTxOStateL . utxosUtxoL . unUtxoL proof

unUtxoL :: Proof era -> Lens' (UTxO era) (Map (TxIn (EraCrypto era)) (TxOutF era))
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
ppupsL (Shelley _) = nesEsL . esLStateL . lsUTxOStateL . utxosGovStateL
ppupsL (Allegra _) = nesEsL . esLStateL . lsUTxOStateL . utxosGovStateL
ppupsL (Mary _) = nesEsL . esLStateL . lsUTxOStateL . utxosGovStateL
ppupsL (Alonzo _) = nesEsL . esLStateL . lsUTxOStateL . utxosGovStateL
ppupsL (Babbage _) = nesEsL . esLStateL . lsUTxOStateL . utxosGovStateL
ppupsL (Conway _) = error "Conway era does not have a PPUPState, in ppupsL"

-- nesEsL . esLStateL . lsUTxOStateL . utxosGovStateL . ???

proposalsT :: Era era => Proof era -> Term era (Map (KeyHash 'Genesis (EraCrypto era)) (PParamsUpdateF era))
proposalsT p = Var (pV p "proposals" (MapR GenHashR (PParamsUpdateR p)) No)

futureProposalsT ::
  Era era => Proof era -> Term era (Map (KeyHash 'Genesis (EraCrypto era)) (PParamsUpdateF era))
futureProposalsT p = Var (pV p "futureProposals" (MapR GenHashR (PParamsUpdateR p)) No)

currPParams :: Era era => Proof era -> Term era (PParamsF era)
currPParams p = Var (pV p "currPParams" (PParamsR p) No)

prevPParams :: Gov.EraGov era => Proof era -> Term era (PParamsF era)
prevPParams p = Var (V "prevPParams" (PParamsR p) (Yes NewEpochStateR (nesEsL . prevPParamsEpochStateL . ppFL p)))

ppupStateT ::
  forall era.
  ( Gov.GovState era ~ ShelleyGovState era
  , Gov.EraGov era
  ) =>
  Proof era ->
  RootTarget era (ShelleyGovState era) (ShelleyGovState era)
ppupStateT p =
  Invert "PPUPState" (typeRep @(ShelleyGovState era)) ppupfun
    :$ Lensed (proposalsT p) (proposalsL . proposedMapL p)
    :$ Lensed (futureProposalsT p) (futureproposalsL . proposedMapL p)
    :$ Lensed (currPParams p) (Gov.curPParamsGovStateL . pparamsFL p)
    :$ Lensed (prevPParams p) (Gov.curPParamsGovStateL . pparamsFL p)
  where
    ppupfun x y (PParamsF _ pp) (PParamsF _ prev) =
      ShelleyGovState
        (ProposedPPUpdates (Map.map unPParamsUpdate x))
        (ProposedPPUpdates (Map.map unPParamsUpdate y))
        pp
        prev

govL :: Lens' (GovState era) (Gov.GovState era)
govL = lens f g
  where
    f :: GovState era -> Gov.GovState era
    f (GovState (Shelley _) x) = x
    f (GovState (Allegra _) x) = x
    f (GovState (Mary _) x) = x
    f (GovState (Alonzo _) x) = x
    f (GovState (Babbage _) x) = x
    f (GovState (Conway _) x) = x
    g :: GovState era -> Gov.GovState era -> GovState era
    g (GovState p@(Shelley _) _) y = GovState p y
    g (GovState p@(Allegra _) _) y = GovState p y
    g (GovState p@(Mary _) _) y = GovState p y
    g (GovState p@(Alonzo _) _) y = GovState p y
    g (GovState p@(Babbage _) _) y = GovState p y
    g (GovState p@(Conway _) _) y = GovState p y

govStateT :: forall era. Proof era -> RootTarget era (GovState era) (GovState era)
govStateT p@(Shelley _) = Invert "GovState" (typeRep @(GovState era)) (GovState p) :$ Shift (ppupStateT p) govL
govStateT p@(Allegra _) = Invert "GovState" (typeRep @(GovState era)) (GovState p) :$ Shift (ppupStateT p) govL
govStateT p@(Mary _) = Invert "GovState" (typeRep @(GovState era)) (GovState p) :$ Shift (ppupStateT p) govL
govStateT p@(Alonzo _) = Invert "GovState" (typeRep @(GovState era)) (GovState p) :$ Shift (ppupStateT p) govL
govStateT p@(Babbage _) = Invert "GovState" (typeRep @(GovState era)) (GovState p) :$ Shift (ppupStateT p) govL
govStateT p@(Conway _) = Invert "GovState" (typeRep @(GovState era)) (GovState p) :$ Shift (unReflect conwayGovStateT p) govL

individualPoolStakeL :: Lens' (IndividualPoolStake c) Rational
individualPoolStakeL = lens individualPoolStake (\ds u -> ds {individualPoolStake = u})

-- Incremental Stake

isPtrMapT :: Era era => Term era (Map Ptr Coin)
isPtrMapT = Var $ V "ptrMap" (MapR PtrR CoinR) (Yes NewEpochStateR ptrMapL)

ptrMapL :: Lens' (NewEpochState era) (Map Ptr Coin)
ptrMapL = nesEsL . esLStateL . lsUTxOStateL . utxosStakeDistrL . isPtrMapL

isCredMapT :: Era era => Term era (Map (Credential 'Staking (EraCrypto era)) Coin)
isCredMapT = Var $ V "credMap" (MapR CredR CoinR) (Yes NewEpochStateR credMapL)

credMapL :: Lens' (NewEpochState era) (Map (Credential 'Staking (EraCrypto era)) Coin)
credMapL = nesEsL . esLStateL . lsUTxOStateL . utxosStakeDistrL . isCredMapL

-- AccountState

treasury :: Era era => Term era Coin
treasury = Var $ V "treasury" CoinR (Yes NewEpochStateR treasuryL)

treasuryL :: NELens era Coin
treasuryL = nesEsL . esAccountStateL . asTreasuryL

reserves :: Era era => Term era Coin
reserves = Var $ V "reserves" CoinR (Yes NewEpochStateR reservesL)

reservesL :: NELens era Coin
reservesL = nesEsL . esAccountStateL . asReservesL

-- | The Coin availabe for a MIR transfer to/from the Treasury
--   Computed from 'treasury' + 'deltaTreasury' - sum('instanTreasury')
mirAvailTreasury :: Era era => Term era Coin
mirAvailTreasury = Var (V "mirAvailTreasury" CoinR No)

-- | The Coin availabe for a MIR transfer to/from the Reserves
--   Computed from 'reserves' + 'deltaReserves' - sum('instanReserves')
mirAvailReserves :: Era era => Term era Coin
mirAvailReserves = Var (V "mirAvailReserves" CoinR No)

-- EpochState

snapshots :: Era era => Term era (SnapShots (EraCrypto era))
snapshots = Var (V "snapshots" SnapShotsR (Yes NewEpochStateR snapshotsL))

snapshotsL :: NELens era (SnapShots (EraCrypto era))
snapshotsL = nesEsL . esSnapshotsL

-- | Lens' from the Core PParams to the Model PParamsF which embeds a (Proof era)
ppFL :: Proof era -> Lens' (PParams era) (PParamsF era)
ppFL p = lens (\pp -> PParamsF p pp) (\_ (PParamsF _ qq) -> qq)

pparams :: Gov.EraGov era => Proof era -> Term era (PParamsF era)
pparams p = Var (V "pparams" (PParamsR p) (Yes NewEpochStateR (nesEsL . curPParamsEpochStateL . ppFL p)))

nmLikelihoodsT :: Era era => Term era (Map (KeyHash 'StakePool (EraCrypto era)) [Float])
nmLikelihoodsT = Var (V "likelihoodsNM" (MapR PoolHashR (ListR FloatR)) (Yes NewEpochStateR (nesEsL . esNonMyopicL . nmLikelihoodsL)))

nmRewardPotT :: Era era => Term era Coin
nmRewardPotT = Var $ V "rewardPotNM" CoinR (Yes NewEpochStateR (nesEsL . esNonMyopicL . nmRewardPotL))

-- ===== SnapShots

-- | Helper lens that deals with the Stake newtype, and the shift from Map to VMap
stakeL :: Lens' (Stake c) (Map (Credential 'Staking c) Coin)
stakeL =
  lens
    (Map.map fromCompact . VMap.toMap . unStake)
    (\_ u -> Stake . VMap.fromMap . Map.map compactCoinOrError $ u)

-- | Helper lens that deals with the shift from Map to VMap
vmapL :: Lens' (VMap.VMap VMap.VB VMap.VB k v) (Map k v)
vmapL = lens VMap.toMap (\_ u -> VMap.fromMap u)

markStakeL :: NELens era (Map (Credential 'Staking (EraCrypto era)) Coin)
markStakeL = nesEsL . esSnapshotsL . ssStakeMarkL . ssStakeL . stakeL

markStake :: Era era => Term era (Map (Credential 'Staking (EraCrypto era)) Coin)
markStake = Var (V "markStake" (MapR CredR CoinR) (Yes NewEpochStateR markStakeL))

markDelegs :: Era era => Term era (Map (Credential 'Staking (EraCrypto era)) (KeyHash 'StakePool (EraCrypto era)))
markDelegs = Var (V "markDelegs" (MapR CredR PoolHashR) (Yes NewEpochStateR markDelegsL))

markDelegsL :: NELens era (Map (Credential 'Staking (EraCrypto era)) (KeyHash 'StakePool (EraCrypto era)))
markDelegsL = nesEsL . esSnapshotsL . ssStakeMarkL . ssDelegationsL . vmapL

markPools :: Era era => Term era (Map (KeyHash 'StakePool (EraCrypto era)) (PoolParams (EraCrypto era)))
markPools = Var (V "markPools" (MapR PoolHashR PoolParamsR) (Yes NewEpochStateR markPoolsL))

markPoolsL :: NELens era (Map (KeyHash 'StakePool (EraCrypto era)) (PoolParams (EraCrypto era)))
markPoolsL = nesEsL . esSnapshotsL . ssStakeMarkL . ssPoolParamsL . vmapL

markSnapShotT :: forall era. Era era => RootTarget era (SnapShot (EraCrypto era)) (SnapShot (EraCrypto era))
markSnapShotT =
  Invert "SnapShot" (typeRep @(SnapShot (EraCrypto era))) snapfun
    :$ Lensed markStake (ssStakeL . stakeL)
    :$ Lensed markDelegs (ssDelegationsL . vmapL)
    :$ Lensed markPools (ssPoolParamsL . vmapL)
  where
    snapfun x y z =
      SnapShot
        (Stake (VMap.fromMap (Map.map compactCoinOrError x)))
        (VMap.fromMap y)
        (VMap.fromMap z)

setStake :: Era era => Term era (Map (Credential 'Staking (EraCrypto era)) Coin)
setStake = Var (V "setStake" (MapR CredR CoinR) (Yes NewEpochStateR setStakeL))

setStakeL :: NELens era (Map (Credential 'Staking (EraCrypto era)) Coin)
setStakeL = nesEsL . esSnapshotsL . ssStakeSetL . ssStakeL . stakeL

setDelegs :: Era era => Term era (Map (Credential 'Staking (EraCrypto era)) (KeyHash 'StakePool (EraCrypto era)))
setDelegs = Var (V "setDelegs" (MapR CredR PoolHashR) (Yes NewEpochStateR setDelegsL))

setDelegsL :: NELens era (Map (Credential 'Staking (EraCrypto era)) (KeyHash 'StakePool (EraCrypto era)))
setDelegsL = nesEsL . esSnapshotsL . ssStakeSetL . ssDelegationsL . vmapL

setPools :: Era era => Term era (Map (KeyHash 'StakePool (EraCrypto era)) (PoolParams (EraCrypto era)))
setPools = Var (V "setPools" (MapR PoolHashR PoolParamsR) (Yes NewEpochStateR setPoolsL))

setPoolsL :: NELens era (Map (KeyHash 'StakePool (EraCrypto era)) (PoolParams (EraCrypto era)))
setPoolsL = nesEsL . esSnapshotsL . ssStakeSetL . ssPoolParamsL . vmapL

setSnapShotT :: forall era. Era era => RootTarget era (SnapShot (EraCrypto era)) (SnapShot (EraCrypto era))
setSnapShotT =
  Invert "SnapShot" (typeRep @(SnapShot (EraCrypto era))) snapfun
    :$ Lensed setStake (ssStakeL . stakeL)
    :$ Lensed setDelegs (ssDelegationsL . vmapL)
    :$ Lensed setPools (ssPoolParamsL . vmapL)
  where
    snapfun x y z =
      SnapShot
        (Stake (VMap.fromMap (Map.map compactCoinOrError x)))
        (VMap.fromMap y)
        (VMap.fromMap z)

goStake :: Era era => Term era (Map (Credential 'Staking (EraCrypto era)) Coin)
goStake = Var (V "goStake" (MapR CredR CoinR) (Yes NewEpochStateR goStakeL))

goStakeL :: NELens era (Map (Credential 'Staking (EraCrypto era)) Coin)
goStakeL = nesEsL . esSnapshotsL . ssStakeGoL . ssStakeL . stakeL

goDelegs :: Era era => Term era (Map (Credential 'Staking (EraCrypto era)) (KeyHash 'StakePool (EraCrypto era)))
goDelegs = Var (V "goDelegs" (MapR CredR PoolHashR) (Yes NewEpochStateR goDelegsL))

goDelegsL :: NELens era (Map (Credential 'Staking (EraCrypto era)) (KeyHash 'StakePool (EraCrypto era)))
goDelegsL = nesEsL . esSnapshotsL . ssStakeGoL . ssDelegationsL . vmapL

goPools :: Era era => Term era (Map (KeyHash 'StakePool (EraCrypto era)) (PoolParams (EraCrypto era)))
goPools = Var (V "goPools" (MapR PoolHashR PoolParamsR) (Yes NewEpochStateR goPoolsL))

goPoolsL :: NELens era (Map (KeyHash 'StakePool (EraCrypto era)) (PoolParams (EraCrypto era)))
goPoolsL = nesEsL . esSnapshotsL . ssStakeGoL . ssPoolParamsL . vmapL

goSnapShotT :: forall era. Era era => RootTarget era (SnapShot (EraCrypto era)) (SnapShot (EraCrypto era))
goSnapShotT =
  Invert "SnapShot" (typeRep @(SnapShot (EraCrypto era))) snapfun
    :$ Lensed goStake (ssStakeL . stakeL)
    :$ Lensed goDelegs (ssDelegationsL . vmapL)
    :$ Lensed goPools (ssPoolParamsL . vmapL)
  where
    snapfun x y z =
      SnapShot
        (Stake (VMap.fromMap (Map.map compactCoinOrError x)))
        (VMap.fromMap y)
        (VMap.fromMap z)

markPoolDistr :: Era era => Term era (Map (KeyHash 'StakePool (EraCrypto era)) (IndividualPoolStake (EraCrypto era)))
markPoolDistr = Var (V "markPoolDistr" (MapR PoolHashR IPoolStakeR) No)

markPoolDistrL :: NELens era (Map (KeyHash 'StakePool (EraCrypto era)) (IndividualPoolStake (EraCrypto era)))
markPoolDistrL = nesEsL . esSnapshotsL . ssStakeMarkPoolDistrL . pooldistrHelpL

pooldistrHelpL :: Lens' (PoolDistr c) (Map (KeyHash 'StakePool c) (IndividualPoolStake c))
pooldistrHelpL = lens unPoolDistr (\_ u -> PoolDistr u)

snapShotFee :: Era era => Term era Coin
snapShotFee = Var (V "snapShotFee" CoinR No)

snapShotsT :: forall era. Era era => RootTarget era (SnapShots (EraCrypto era)) (SnapShots (EraCrypto era))
snapShotsT =
  Invert "SnapShots" (typeRep @(SnapShots (EraCrypto era))) shotsfun
    :$ Shift markSnapShotT ssStakeMarkL
    :$ Lensed markPoolDistr (ssStakeMarkPoolDistrL . pooldistrHelpL)
    :$ Shift setSnapShotT ssStakeSetL
    :$ Shift goSnapShotT ssStakeGoL
    :$ Lensed snapShotFee ssFeeL
  where
    shotsfun w x = SnapShots w (PoolDistr x)

-- ==================================================================
-- RewardUpdate

deltaT :: Era era => Term era (Maybe DeltaCoin)
deltaT = Var (V "deltaT" (MaybeR DeltaCoinR) (Yes NewEpochStateR deltaTL))

deltaTL :: NELens era (Maybe DeltaCoin)
deltaTL = nesRuL . help
  where
    help :: Lens' (StrictMaybe (PulsingRewUpdate c)) (Maybe DeltaCoin)
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
    help :: Lens' (StrictMaybe (PulsingRewUpdate c)) (Maybe DeltaCoin)
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
    help :: Lens' (StrictMaybe (PulsingRewUpdate c)) (Maybe DeltaCoin)
    help = lens view update
      where
        view SNothing = Nothing
        view (SJust (Complete x)) = Just (RU.deltaF x)
        view (SJust _) = Nothing
        update (SJust (Complete ru)) (Just change) = SJust (Complete (ru {RU.deltaF = change}))
        update _ _ = SNothing

rewardSet :: Era era => Term era (Map (Credential 'Staking (EraCrypto era)) (Set (Reward (EraCrypto era))))
rewardSet = Var (V "rewardSet" (MapR CredR (SetR RewardR)) No)

rewardSetL :: NELens era (Maybe (Map (Credential 'Staking (EraCrypto era)) (Set (Reward (EraCrypto era)))))
rewardSetL = nesRuL . help
  where
    help :: Lens' (StrictMaybe (PulsingRewUpdate c)) (Maybe (Map (Credential 'Staking c) (Set (Reward c))))
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
credsUniv :: Era era => Term era (Set (Credential 'Staking (EraCrypto era)))
credsUniv = Var $ V "credsUniv" (SetR CredR) No

-- | The universe of Staking Credentials. A credential is either KeyHash of a ScriptHash
--   All Plutus scripts in this Universe are SPENDING scripts, so they will need a Redeemer
--   Use this ONLY in the Pay-part of an Address (Do not use this in the Stake-part of an Address)
spendCredsUniv :: Era era => Term era (Set (Credential 'Payment (EraCrypto era)))
spendCredsUniv = Var $ V "spendCredsUniv" (SetR PCredR) No

-- | The universe of Voting Credentials. A credential is either KeyHash of a ScriptHash
voteUniv :: Era era => Term era (Set (Credential 'DRepRole (EraCrypto era)))
voteUniv = Var $ V "voteUniv" (SetR VCredR) No

-- | The universe of Payment Credentials. A credential is either KeyHash of a ScriptHash
--   We only find payment credentials in the Payment part of an Addr.
payUniv :: Era era => Term era (Set (Credential 'Payment (EraCrypto era)))
payUniv = Var $ V "payUniv" (SetR PCredR) No

-- | The universe of Scripts (and their hashes) useable in spending contexts
--  That means if they are Plutus scripts then they will be passed an additional
--  argument (the TxInfo context)
spendscriptUniv :: Era era => Proof era -> Term era (Map (ScriptHash (EraCrypto era)) (ScriptF era))
spendscriptUniv p = Var (pV p "spendscriptUniv" (MapR ScriptHashR (ScriptR p)) No)

-- | The universe of Scripts (and their hashes) useable in contexts other than Spending
nonSpendScriptUniv :: Era era => Proof era -> Term era (Map (ScriptHash (EraCrypto era)) (ScriptF era))
nonSpendScriptUniv p = Var (pV p "nonSpendScriptUniv" (MapR ScriptHashR (ScriptR p)) No)

-- | The union of 'spendscriptUniv' and 'nonSpendScriptUniv'. All possible scripts in any context
allScriptUniv :: Era era => Proof era -> Term era (Map (ScriptHash (EraCrypto era)) (ScriptF era))
allScriptUniv p = Var (pV p "allScriptUniv" (MapR ScriptHashR (ScriptR p)) No)

-- | The universe of Data (and their hashes)
dataUniv :: Era era => Term era (Map (DataHash (EraCrypto era)) (Data era))
dataUniv = Var (V "dataUniv" (MapR DataHashR DataR) No)

-- | The universe of StakePool key hashes. These hashes hash the cold key of the
--   Pool operators.
poolHashUniv :: Era era => Term era (Set (KeyHash 'StakePool (EraCrypto era)))
poolHashUniv = Var $ V "poolHashUniv" (SetR PoolHashR) No

-- | The universe of StakePool key hashes. These hashes hash are hashes of the Owners of a PoolParam
stakeHashUniv :: Era era => Term era (Set (KeyHash 'Staking (EraCrypto era)))
stakeHashUniv = Var $ V "stakeHashUniv" (SetR StakeHashR) No

-- | The universe of the Genesis key hashes and their signing and validating GenDelegPairs
genesisHashUniv :: Era era => Term era (Map (KeyHash 'Genesis (EraCrypto era)) (GenDelegPair (EraCrypto era)))
genesisHashUniv = Var $ V "genesisHashUniv" (MapR GenHashR GenDelegPairR) No

voteCredUniv :: Era era => Term era (Set (Credential 'ColdCommitteeRole (EraCrypto era)))
voteCredUniv = Var $ V "voteHashUniv" (SetR CommColdCredR) No

-- | The universe of TxIns. Pairs of TxId: hashes of previously run transaction bodies,
--   and TxIx: indexes of one of the bodies outputs.
txinUniv :: Era era => Term era (Set (TxIn (EraCrypto era)))
txinUniv = Var $ V "txinUniv" (SetR TxInR) No

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
feeTxIn :: Era era => Term era (TxIn (EraCrypto era))
feeTxIn = Var (V "feeTxIn" TxInR No)

-- | A Coin large enough to pay almost any fee.
--   See also 'feeOutput' which is related.
bigCoin :: Era era => Term era Coin
bigCoin = Var (V "bigCoin" CoinR No)

datumsUniv :: Era era => Term era [Datum era]
datumsUniv = Var (V "datumsUniv" (ListR DatumR) No)

multiAssetUniv :: Era era => Term era [MultiAsset (EraCrypto era)]
multiAssetUniv = Var (V "multiAssetUniv" (ListR MultiAssetR) No)

-- | The universe of key hashes, and the signing and validating key pairs they represent.
keymapUniv :: Era era => Term era (Map (KeyHash 'Witness (EraCrypto era)) (KeyPair 'Witness (EraCrypto era)))
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

addrUniv :: Era era => Term era (Set (Addr (EraCrypto era)))
addrUniv = Var $ V "addrUniv" (SetR AddrR) No

ptrUniv :: Era era => Term era (Set Ptr)
ptrUniv = Var $ V "ptrUniv" (SetR PtrR) No

plutusUniv :: Reflect era => Term era (Map (ScriptHash (EraCrypto era)) (IsValid, ScriptF era))
plutusUniv = Var $ V "plutusUniv" (MapR ScriptHashR (PairR IsValidR (ScriptR reify))) No

spendPlutusUniv :: Reflect era => Term era (Map (ScriptHash (EraCrypto era)) (IsValid, ScriptF era))
spendPlutusUniv = Var $ V "spendPlutusUniv" (MapR ScriptHashR (PairR IsValidR (ScriptR reify))) No

-- | The universe of all Byron addresses. In Eras, Babbage, Conway we avoid these Adresses,
--   as they do not play well with Plutus Scripts.
byronAddrUniv :: Era era => Term era (Map (KeyHash 'Payment (EraCrypto era)) (Addr (EraCrypto era), SigningKey))
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
  Map (KeyHash 'StakePool (EraCrypto era)) Natural ->
  Map (KeyHash 'StakePool (EraCrypto era)) Natural ->
  EpochState era ->
  Map (KeyHash 'StakePool (EraCrypto era)) (IndividualPoolStake (EraCrypto era)) ->
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
      (PoolDistr nesPd')
      ( case proof of
          Shelley _ -> UTxO Map.empty
          Allegra _ -> ()
          Mary _ -> ()
          Alonzo _ -> ()
          Babbage _ -> ()
          Conway _ -> ()
      )

-- | Target for NewEpochState
newEpochStateT :: forall era. Gov.EraGov era => Proof era -> RootTarget era (NewEpochState era) (NewEpochState era)
newEpochStateT proof =
  Invert "NewEpochState" (typeRep @(NewEpochState era)) (newEpochStateConstr proof)
    :$ Lensed currentEpoch nesELL
    :$ Lensed prevBlocksMade nesBprevL
    :$ Lensed currBlocksMade nesBcurL
    :$ Shift (epochStateT proof) nesEsL
    :$ Lensed poolDistr (nesPdL . unPoolDistrL)

-- | Target for EpochState
epochStateT :: forall era. Gov.EraGov era => Proof era -> RootTarget era (EpochState era) (EpochState era)
epochStateT proof =
  Invert "EpochState" (typeRep @(EpochState era)) epochStateFun
    :$ Shift accountStateT esAccountStateL
    :$ Shift (ledgerStateT proof) esLStateL
    :$ Shift snapShotsT esSnapshotsL
  where
    epochStateFun a s l = EpochState a s l (NonMyopic Map.empty (Coin 0))

-- | Target for AccountState
accountStateT :: Era era => RootTarget era AccountState AccountState
accountStateT =
  Invert "AccountState" (typeRep @AccountState) AccountState
    :$ Lensed treasury asTreasuryL
    :$ Lensed reserves asReservesL

-- | Target for LedgerState
ledgerStateT :: forall era. Gov.EraGov era => Proof era -> RootTarget era (LedgerState era) (LedgerState era)
ledgerStateT proof =
  Invert "LedgerState" (typeRep @(LedgerState era)) LedgerState
    :$ Shift (utxoStateT proof) lsUTxOStateL
    :$ Shift certstateT lsCertStateL

ledgerState :: Reflect era => Term era (LedgerState era)
ledgerState = Var $ V "ledgerState" (LedgerStateR reify) No

-- | Target for UTxOState
utxoStateT :: forall era. Gov.EraGov era => Proof era -> RootTarget era (UTxOState era) (UTxOState era)
utxoStateT p =
  Invert "UTxOState" (typeRep @(UTxOState era)) (unReflect utxofun p)
    :$ Lensed (utxo p) (utxosUtxoL . unUtxoL p)
    :$ Lensed deposits utxosDepositedL
    :$ Lensed fees utxosFeesL
    :$ Shift (govStateT p) (utxosGovStateL . unGovL p)
    :$ Lensed donation utxosDonationL
  where
    utxofun ::
      Reflect era =>
      Proof era ->
      Map (TxIn (EraCrypto era)) (TxOutF era) ->
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

-- | Target for CertState
certstateT :: forall era. Era era => RootTarget era (CertState era) (CertState era)
certstateT =
  Invert "CertState" (typeRep @(CertState era)) CertState
    :$ (Shift vstateT certVStateL)
    :$ (Shift pstateT certPStateL)
    :$ (Shift dstateT certDStateL)

-- | Target for VState
vstateT :: forall era. Era era => RootTarget era (VState era) (VState era)
vstateT =
  Invert "VState" (typeRep @(VState era)) (\x y z -> VState x (CommitteeState y) z)
    :$ Lensed dreps vsDRepsL
    :$ Lensed committeeState (vsCommitteeStateL . csCommitteeCredsL)
    :$ Lensed numDormantEpochs vsNumDormantEpochsL

committeeL ::
  Lens'
    ( Map
        (Credential 'ColdCommitteeRole (EraCrypto era))
        (Maybe (Credential 'HotCommitteeRole (EraCrypto era)))
    )
    (CommitteeState era)
committeeL = lens CommitteeState (\_ (CommitteeState x) -> x)

{-
committeeState :: Era era => Term era (Map (Credential 'ColdCommitteeRole (EraCrypto era)) (Maybe (Credential 'HotCommitteeRole (EraCrypto era))))
committeeState = Var $ V "committeeState" (MapR CommColdCredR (MaybeR CommHotCredR)) (Yes NewEpochStateR committeeStateL)

committeeStateL :: NELens era (Map (Credential 'ColdCommitteeRole (EraCrypto era)) (Maybe (Credential 'HotCommitteeRole (EraCrypto era))))
committeeStateL = nesEsL . esLStateL . lsCertStateL . certVStateL . vsCommitteeStateL . csCommitteeCredsL
-}

-- | Target for PState
pstateT :: forall era. Era era => RootTarget era (PState era) (PState era)
pstateT =
  Invert "PState" (typeRep @(PState era)) PState
    :$ Lensed regPools psStakePoolParamsL
    :$ Lensed futureRegPools psFutureStakePoolParamsL
    :$ Lensed retiring psRetiringL
    :$ Lensed poolDeposits psDepositsL

-- | Target for DState
dstateT :: forall era. Era era => RootTarget era (DState era) (DState era)
dstateT =
  Invert "DState" (typeRep @(DState era)) dstate
    :$ Lensed rewards (dsUnifiedL . rewardsUMapL)
    :$ Lensed stakeDeposits (dsUnifiedL . stakeDepositsUMapL)
    :$ Lensed delegations (dsUnifiedL . delegationsUMapL)
    :$ Lensed ptrs (dsUnifiedL . ptrsUMapL)
    :$ Lensed futureGenDelegs dsFutureGenDelegsL
    :$ Lensed genDelegs (dsGenDelegsL . unGenDelegsL)
    :$ Shift instantaneousRewardsT dsIRewardsL

-- | Abstract construcor function for DState
dstate ::
  Map (Credential 'Staking (EraCrypto era)) Coin ->
  Map (Credential 'Staking (EraCrypto era)) Coin ->
  Map (Credential 'Staking (EraCrypto era)) (KeyHash 'StakePool (EraCrypto era)) ->
  Map Ptr (Credential 'Staking (EraCrypto era)) ->
  Map (FutureGenDeleg (EraCrypto era)) (GenDelegPair (EraCrypto era)) ->
  Map (KeyHash 'Genesis (EraCrypto era)) (GenDelegPair (EraCrypto era)) ->
  InstantaneousRewards (EraCrypto era) ->
  DState era
dstate rew dep deleg ptr fgen gen =
  DState (unSplitUMap (Split rew dep deleg Map.empty (error "Not implemented") ptr)) fgen (GenDelegs gen)

instantaneousRewardsT ::
  forall era.
  Era era =>
  RootTarget era (InstantaneousRewards (EraCrypto era)) (InstantaneousRewards (EraCrypto era))
instantaneousRewardsT =
  Invert "InstanRew" (typeRep @(InstantaneousRewards (EraCrypto era))) InstantaneousRewards
    :$ Lensed instanReserves iRReservesL
    :$ Lensed instanTreasury iRTreasuryL
    :$ Lensed deltaReserves deltaReservesL
    :$ Lensed deltaTreasury deltaTreasuryL

-- | A String that pretty prints the complete set of variables of the NewEpochState
allvars :: String
allvars = show (ppTarget (newEpochStateT (Conway Standard)))

printTarget :: Target era t -> IO ()
printTarget t = putStrLn (show (ppTarget t))

-- =====================================================================
-- PParams fields

withEraPParams :: forall era a. Proof era -> (EraPParams era => a) -> a
withEraPParams (Shelley _) x = x
withEraPParams (Mary _) x = x
withEraPParams (Allegra _) x = x
withEraPParams (Alonzo _) x = x
withEraPParams (Babbage _) x = x
withEraPParams (Conway _) x = x

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
        (Yes (PParamsR p) (withEraPParams p (pparamsWrapperL . ppMaxBBSizeL)))
    )

-- | Max Tx Size
maxTxSize :: Era era => Proof era -> Term era Natural
maxTxSize p =
  Var
    ( pV
        p
        "maxTxSize"
        NaturalR
        (Yes (PParamsR p) (withEraPParams p (pparamsWrapperL . ppMaxTxSizeL)))
    )

-- | Max Block Header Size
maxBHSize :: Era era => Proof era -> Term era Natural
maxBHSize p =
  Var
    ( pV
        p
        "maxBHSize"
        NaturalR
        (Yes (PParamsR p) (withEraPParams p (pparamsWrapperL . ppMaxBHSizeL)))
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

maxEpoch :: Era era => Proof era -> Term era EpochNo
maxEpoch p =
  Var $
    pV
      p
      "maxEpoch"
      EpochR
      (Yes (PParamsR p) (withEraPParams p (pparamsWrapperL . ppEMaxL)))

-- =================================================================
-- TxBody vars

txbodyterm :: Reflect era => Term era (TxBodyF era)
txbodyterm = Var $ V "txbodyterm" (TxBodyR reify) No

inputs :: Era era => Term era (Set (TxIn (EraCrypto era)))
inputs = Var $ V "inputs" (SetR TxInR) No

collateral :: Era era => Term era (Set (TxIn (EraCrypto era)))
collateral = Var $ V "collateral" (SetR TxInR) No

refInputs :: Era era => Term era (Set (TxIn (EraCrypto era)))
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

withdrawals :: forall era. Era era => Term era (Map (RewardAcnt (EraCrypto era)) Coin)
withdrawals = Var $ V "withdrawals" (MapR (RewardAcntR @era) CoinR) No

txfee :: Era era => Term era Coin
txfee = Var $ V "txfee" CoinR No

ttl :: Era era => Term era SlotNo
ttl = Var $ V "ttl" SlotNoR No

validityInterval :: Era era => Term era ValidityInterval
validityInterval = Var $ V "validityInterval" ValidityIntervalR No

mint :: Era era => Term era (Map (ScriptHash (EraCrypto era)) (Map AssetName Integer))
mint = Var $ V "mint" (MapR ScriptHashR (MapR AssetNameR IntegerR)) No

reqSignerHashes :: Era era => Term era (Set (KeyHash 'Witness (EraCrypto era)))
reqSignerHashes = Var $ V "reqSignerHashes" (SetR WitHashR) No

networkID :: Era era => Term era (Maybe Network)
networkID = Var $ V "networkID" (MaybeR NetworkR) No

adHash :: Era era => Term era (Maybe (AuxiliaryDataHash (EraCrypto era)))
adHash = Var $ V "adHash" (MaybeR AuxiliaryDataHashR) No

wppHash :: Era era => Term era (Maybe (SafeHash (EraCrypto era) EraIndependentScriptIntegrity))
wppHash = Var $ V "wppHash" (MaybeR ScriptIntegrityHashR) No

-- | lift the model type of 'mint' into a MultiAsset
liftMultiAsset :: Map (ScriptHash c) (Map AssetName Integer) -> MultiAsset c
liftMultiAsset m = MultiAsset (Map.mapKeys PolicyID m)

scriptsNeeded :: Reflect era => Term era (ScriptsNeededF era)
scriptsNeeded = Var $ V "scriptsNeeded" (ScriptsNeededR reify) No

smNeededL ::
  ScriptsNeeded era ~ ShelleyScriptsNeeded era =>
  Lens' (ScriptsNeededF era) (Set (ScriptHash (EraCrypto era)))
smNeededL =
  lens
    (\(ScriptsNeededF _ (ShelleyScriptsNeeded s)) -> s)
    (\(ScriptsNeededF p _) s -> ScriptsNeededF p (ShelleyScriptsNeeded s))

acNeededL ::
  ScriptsNeeded era ~ AlonzoScriptsNeeded era =>
  Lens' (ScriptsNeededF era) [(ScriptPurpose era, ScriptHash (EraCrypto era))]
acNeededL =
  lens
    (\(ScriptsNeededF _ (AlonzoScriptsNeeded s)) -> s)
    (\(ScriptsNeededF p _) s -> ScriptsNeededF p (AlonzoScriptsNeeded s))

-- ===============
-- Auxliary Vars to compute collateral

-- | A Coin that needs to be added to the range of the colInputs in the UtxO
--   that will make sure the collateral is large enough to pay the fees if needed
extraCol :: Era era => Term era Coin
extraCol = Var $ V "extraCol" CoinR No

-- | The sum of all the 'collateral' inputs, total colateral of the Tx is computed by adding (SJust _) to this value.
sumCol :: Era era => Term era Coin
sumCol = Var $ V "sumCol" CoinR No

colRetAddr :: Era era => Term era (Addr (EraCrypto era))
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

getRwdCredL :: Lens' (RewardAcnt c) (Credential 'Staking c)
getRwdCredL = lens getRwdCred (\r c -> r {getRwdCred = c})

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

maryValueMultiAssetL :: Lens' (MaryValue c) (MultiAsset c)
maryValueMultiAssetL =
  lens
    (\(MaryValue _ ma) -> ma)
    (\(MaryValue c _) ma -> MaryValue c ma)

valueFMultiAssetL :: Lens' (ValueF era) (MultiAsset (EraCrypto era))
valueFMultiAssetL = lens get put
  where
    get :: ValueF era -> MultiAsset (EraCrypto era)
    get (ValueF p x) = case whichValue p of
      ValueShelleyToAllegra -> MultiAsset Map.empty
      ValueMaryToConway -> x ^. maryValueMultiAssetL

    put :: ValueF era -> MultiAsset (EraCrypto era) -> ValueF era
    put (ValueF p x) new = case whichValue p of
      ValueShelleyToAllegra -> ValueF p x
      ValueMaryToConway -> ValueF p (x & maryValueMultiAssetL .~ new)

-- | a Field from (ValueF era) to MultiAsset
valueFMultiAssetF :: Reflect era => Field era (ValueF era) (MultiAsset (EraCrypto era))
valueFMultiAssetF = Field "valueFMultiAsset" MultiAssetR (ValueR reify) valueFMultiAssetL

valueFMultiAsset :: Reflect era => Term era (MultiAsset (EraCrypto era))
valueFMultiAsset = fieldToTerm valueFMultiAssetF

-- | a Field from (TxOut era) to (Addr era)
txoutAddressF :: Reflect era => Field era (TxOutF era) (Addr (EraCrypto era))
txoutAddressF = Field "txoutAddress" AddrR (TxOutR reify) (txOutFL . addrTxOutL)

txoutAddress :: Reflect era => Term era (Addr (EraCrypto era))
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

scriptWits :: Reflect era => Term era (Map (ScriptHash (EraCrypto era)) (ScriptF era))
scriptWits = Var $ V "scriptWits" (MapR ScriptHashR (ScriptR reify)) No

redeemers :: Reflect era => Term era (Map RdmrPtr (Data era, ExUnits))
redeemers = Var $ V "redeemers" (MapR RdmrPtrR (PairR DataR ExUnitsR)) No

bootWits :: forall era. Reflect era => Term era (Set (BootstrapWitness (EraCrypto era)))
bootWits = Var $ V "bootWits" (SetR (BootstrapWitnessR @era)) No

dataWits :: Reflect era => Term era (Map (DataHash (EraCrypto era)) (Data era))
dataWits = Var $ V "dataWits" (MapR DataHashR DataR) No

keyWits :: Reflect era => Term era (Set (WitVKey 'Witness (EraCrypto era)))
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
  Term era (Set (BootstrapWitness (EraCrypto era))) ->
  Term era (Set (WitVKey 'Witness (EraCrypto era))) ->
  Target era (TxWits era)
witsTarget bootWitsParam keyWitsParam = Constr "TxWits" witsf ^$ scriptWits ^$ redeemers ^$ bootWitsParam ^$ dataWits ^$ keyWitsParam
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
        , RdmrWits (Redeemers redeem)
        ]

txTarget ::
  Reflect era =>
  Term era (TxBodyF era) ->
  Term era (Set (BootstrapWitness (EraCrypto era))) ->
  Term era (Set (WitVKey 'Witness (EraCrypto era))) ->
  Target era (TxF era)
txTarget bodyparam bootWitsParam keyWitsParam = Constr "tx" txf ^$ bodyparam :$ wits ^$ txauxdata ^$ txisvalid
  where
    wits = witsTarget bootWitsParam keyWitsParam
    txf (TxBodyF proof txb) w auxs isvalid =
      TxF proof (newTx proof [Body txb, TxWits w, AuxData' (fixM auxs), Valid isvalid])
    fixM Nothing = []
    fixM (Just (TxAuxDataF _ x)) = [x]

-- | Need to build the TxBody with different terms that control the fee and wppHash so we
--   parameterise this target over those two terms
txbodyTarget :: Reflect era => Term era Coin -> Term era (Maybe (ScriptIntegrityHash (EraCrypto era))) -> Term era Coin -> Target era (TxBodyF era)
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
      fee =
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

enactWithdrawals :: forall era. Era era => Term era (Map (Credential 'Staking (EraCrypto era)) Coin)
enactWithdrawals = Var $ V "enactWithdrawals" (MapR CredR CoinR) No

currentGovActionStates :: Era era => Term era (Map (GovActionId (EraCrypto era)) (GovActionState era))
currentGovActionStates = Var $ V "currentGovActionStates" (MapR GovActionIdR GovActionStateR) No

currentProposalOrder :: Era era => Term era [GovActionId (EraCrypto era)]
currentProposalOrder = Var $ V "currentProposalOrder" (ListR GovActionIdR) No

prevGovActionStates :: Era era => Term era (Map (GovActionId (EraCrypto era)) (GovActionState era))
prevGovActionStates = Var $ V "prevGovActionStates" (MapR GovActionIdR GovActionStateR) No

prevProposalOrder :: Era era => Term era [GovActionId (EraCrypto era)]
prevProposalOrder = Var $ V "prevProposalOrder" (ListR GovActionIdR) No

currentGovSnapshots :: Era era => Term era [GovActionState era]
currentGovSnapshots = Var $ V "currentGovSnapshots" (ListR GovActionStateR) No

previousGovSnapshots :: Era era => Term era [GovActionState era]
previousGovSnapshots = Var $ V "previousGovSnapshots" (ListR GovActionStateR) No

previousDRepsState :: Era era => Term era (Map (Credential 'DRepRole (EraCrypto era)) (DRepState (EraCrypto era)))
previousDRepsState = Var $ V "previousDRepsState" (MapR VCredR DRepStateR) No

currentDRepsState :: Era era => Term era (Map (Credential 'DRepRole (EraCrypto era)) (DRepState (EraCrypto era)))
currentDRepsState = Var $ V "currentDRepsState" (MapR VCredR DRepStateR) No

previousCommitteeState :: Era era => Term era (Map (Credential 'ColdCommitteeRole (EraCrypto era)) (Maybe (Credential 'HotCommitteeRole (EraCrypto era))))
previousCommitteeState = Var $ V "previousCommitteeState" (MapR CommColdCredR (MaybeR CommHotCredR)) No

commMembers :: Era era => Term era (Map (Credential 'ColdCommitteeRole (EraCrypto era)) EpochNo)
commMembers = Var $ V "commMembers" (MapR CommColdCredR EpochR) No

commQuorum :: Era era => Term era UnitInterval
commQuorum = Var $ V "commQuorum" UnitIntervalR No

committeeVar :: Era era => Term era (Maybe (Committee era))
committeeVar = Var $ V "committeeVar" (MaybeR CommitteeR) No

-- ====================================
-- ConwayGovState Targets

drepDistr :: Era era => Term era (DRepDistr (EraCrypto era))
drepDistr = Var $ V "drepDistr" DRepDistrR No

conwayGovStateT :: forall era. Reflect era => Proof era -> RootTarget era (ConwayGovState era) (ConwayGovState era)
conwayGovStateT _p =
  Invert "ConwayGovState" (typeRep @(ConwayGovState era)) ConwayGovState
    :$ Shift govSnapshotsT cgGovSnapshotsL
    :$ Shift enactStateT cgEnactStateL
    :$ Lensed drepDistr cgDRepDistrL

govSnapshotsT :: forall era. Era era => RootTarget era (GovSnapshots era) (GovSnapshots era)
govSnapshotsT =
  Invert "GovSnapshots" (typeRep @(GovSnapshots era)) modelGovSnapshots
    :$ Lensed currentGovSnapshots (curGovSnapshotsL . snapshotL)
    :$ Lensed previousGovSnapshots (prevGovSnapshotsL . snapshotL)
    :$ Lensed previousDRepsState prevDRepsStateL
    :$ Lensed previousCommitteeState (prevCommitteeStateL . csCommitteeCredsL)

-- | Function for constructing (GovSnapshots era) from the model types
modelGovSnapshots ::
  [GovActionState era] ->
  [GovActionState era] ->
  Map (Credential 'DRepRole (EraCrypto era)) (DRepState (EraCrypto era)) ->
  Map (Credential 'ColdCommitteeRole (EraCrypto era)) (Maybe (Credential 'HotCommitteeRole (EraCrypto era))) ->
  GovSnapshots era
modelGovSnapshots cs ps pd pc =
  GovSnapshots
    (fromGovActionStateSeq (SS.fromList cs))
    (fromGovActionStateSeq (SS.fromList ps))
    pd
    (CommitteeState pc)

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
    :$ Shift prevGovActionIdsT ensPrevGovActionIdsL

-- | One can use this Target, to make a constraint for 'committeeVar' from the
--   vars 'commMembers' and 'commQuorum'
committeeT :: forall era. Era era => RootTarget era (Committee era) (Committee era)
committeeT =
  Invert "Committee" (typeRep @(Committee era)) Committee
    :$ Lensed commMembers committeeMembersL
    :$ Lensed commQuorum committeeQuorumL

prevGovActionIdsT :: forall era. Era era => RootTarget era (PrevGovActionIds era) (PrevGovActionIds era)
prevGovActionIdsT =
  Invert
    "prevGovActionIds"
    (typeRep @(PrevGovActionIds era))
    (\w x y z -> PrevGovActionIds (maybeToStrictMaybe w) (maybeToStrictMaybe x) (maybeToStrictMaybe y) (maybeToStrictMaybe z))
    :$ Lensed prevPParamUpdate (pgaPParamUpdateL . strictMaybeToMaybeL)
    :$ Lensed prevHardFork (pgaHardForkL . strictMaybeToMaybeL)
    :$ Lensed prevCommittee (pgaCommitteeL . strictMaybeToMaybeL)
    :$ Lensed prevConstitution (pgaConstitutionL . strictMaybeToMaybeL)

prevPParamUpdate :: Era era => Term era (Maybe (PrevGovActionId 'PParamUpdatePurpose (EraCrypto era)))
prevPParamUpdate = Var $ V "prevPParamUpdate" (MaybeR PrevPParamUpdateR) No

prevHardFork :: Era era => Term era (Maybe (PrevGovActionId 'HardForkPurpose (EraCrypto era)))
prevHardFork = Var $ V "prevHardFork" (MaybeR PrevHardForkR) No

prevCommittee :: Era era => Term era (Maybe (PrevGovActionId 'CommitteePurpose (EraCrypto era)))
prevCommittee = Var $ V "prevCommittee" (MaybeR PrevCommitteeR) No

prevConstitution :: Era era => Term era (Maybe (PrevGovActionId 'ConstitutionPurpose (EraCrypto era)))
prevConstitution = Var $ V "prevConstitution" (MaybeR PrevConstitutionR) No

-- ================
-- Lenses

snapshotL :: Lens' (ProposalsSnapshot era) [GovActionState era]
snapshotL =
  lens
    (\s -> foldr (:) [] (snapshotActions s))
    (\_ l -> fromGovActionStateSeq (SS.fromList l))

unSnapshotL :: Lens' [GovActionState era] (ProposalsSnapshot era)
unSnapshotL =
  lens
    (\l -> fromGovActionStateSeq (SS.fromList l))
    (\_ s -> foldr (:) [] (snapshotActions s))

pparamsFL :: Proof era -> Lens' (PParams era) (PParamsF era)
pparamsFL p = lens (PParamsF p) (\_ (PParamsF _ x) -> x)

committeeMembersL :: Lens' (Committee era) (Map (Credential 'ColdCommitteeRole (EraCrypto era)) EpochNo)
committeeMembersL = lens committeeMembers (\x s -> x {committeeMembers = s})

committeeQuorumL :: Lens' (Committee era) UnitInterval
committeeQuorumL = lens committeeQuorum (\x s -> x {committeeQuorum = s})

ensTreasuryL :: Lens' (EnactState era) Coin
ensTreasuryL = lens ensTreasury (\x y -> x {ensTreasury = y})

ensWithdrawalsL :: Lens' (EnactState era) (Map (Credential 'Staking (EraCrypto era)) Coin)
ensWithdrawalsL = lens ensWithdrawals (\x y -> x {ensWithdrawals = y})

smCommL :: Lens' (StrictMaybe (Committee era)) (Committee era)
smCommL = lens foo (\_ t -> SJust t)
  where
    foo SNothing = Committee Map.empty maxBound
    foo (SJust x) = x

strictMaybeToMaybeL :: Lens' (StrictMaybe x) (Maybe x)
strictMaybeToMaybeL = lens ff gg
  where
    ff (SJust x) = Just x
    ff SNothing = Nothing
    gg _ (Just x) = SJust x
    gg _ Nothing = SNothing

idLens :: Lens' a a
idLens = lens (\x -> x) (\_ y -> y)

proposalsL :: Lens' (ShelleyGovState era) (ProposedPPUpdates era)
proposalsL = lens proposals (\x y -> x {proposals = y})

futureproposalsL :: Lens' (ShelleyGovState era) (ProposedPPUpdates era)
futureproposalsL = lens futureProposals (\x y -> x {futureProposals = y})

proposedMapL :: Proof era -> Lens' (ProposedPPUpdates era) (Map (KeyHash 'Genesis (EraCrypto era)) (PParamsUpdateF era))
proposedMapL p =
  lens
    (\(ProposedPPUpdates x) -> Map.map (PParamsUpdateF p) x)
    (\(ProposedPPUpdates _) y -> ProposedPPUpdates (Map.map unPParamsUpdate y))

pgaPParamUpdateL :: Lens' (PrevGovActionIds era) (StrictMaybe (PrevGovActionId 'PParamUpdatePurpose (EraCrypto era)))
pgaPParamUpdateL = lens pgaPParamUpdate (\x y -> x {pgaPParamUpdate = y})

pgaHardForkL :: Lens' (PrevGovActionIds era) (StrictMaybe (PrevGovActionId 'HardForkPurpose (EraCrypto era)))
pgaHardForkL = lens pgaHardFork (\x y -> x {pgaHardFork = y})

pgaCommitteeL :: Lens' (PrevGovActionIds era) (StrictMaybe (PrevGovActionId 'CommitteePurpose (EraCrypto era)))
pgaCommitteeL = lens pgaCommittee (\x y -> x {pgaCommittee = y})

pgaConstitutionL :: Lens' (PrevGovActionIds era) (StrictMaybe (PrevGovActionId 'ConstitutionPurpose (EraCrypto era)))
pgaConstitutionL = lens pgaConstitution (\x y -> x {pgaConstitution = y})

-- ======================================================
