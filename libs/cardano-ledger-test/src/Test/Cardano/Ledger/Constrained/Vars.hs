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
import Cardano.Ledger.Alonzo.Scripts (ExUnits (..))
import Cardano.Ledger.Alonzo.Tx (IsValid (..), ScriptIntegrityHash, ScriptPurpose (..))
import Cardano.Ledger.Alonzo.TxWits (RdmrPtr (..), Redeemers (..), TxDats (..))
import Cardano.Ledger.Alonzo.UTxO (AlonzoScriptsNeeded (..))
import Cardano.Ledger.AuxiliaryData (AuxiliaryDataHash)
import Cardano.Ledger.BaseTypes (
  BlocksMade (..),
  EpochNo,
  Globals (..),
  Network (..),
  ProtVer (..),
  SlotNo (..),
  StrictMaybe (..),
  UnitInterval,
 )
import qualified Cardano.Ledger.BaseTypes as Base (EpochInterval (..), Globals (..))
import Cardano.Ledger.CertState (CommitteeState (..), csCommitteeCredsL, vsNumDormantEpochsL)
import Cardano.Ledger.Coin (Coin (..), DeltaCoin)
import Cardano.Ledger.Conway.Governance hiding (GovState)
import Cardano.Ledger.Conway.PParams (ConwayEraPParams, ppDRepActivityL, ppDRepDepositL, ppGovActionDepositL)
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
import Cardano.Ledger.DRep (DRep (..), DRepState (..))
import Cardano.Ledger.EpochBoundary (SnapShot (..), SnapShots (..), Stake (..))
import Cardano.Ledger.Era (Era (EraCrypto))
import Cardano.Ledger.Hashes (DataHash, EraIndependentScriptIntegrity, ScriptHash (..))
import Cardano.Ledger.Keys (GenDelegPair, GenDelegs (..), KeyHash, KeyRole (..))
import Cardano.Ledger.Keys.Bootstrap (BootstrapWitness)
import Cardano.Ledger.Keys.WitVKey (WitVKey (..))
import Cardano.Ledger.Mary.Value (AssetName (..), MaryValue (..), MultiAsset (..), PolicyID (..))
import Cardano.Ledger.Plutus.Data (Data (..), Datum (..))
import Cardano.Ledger.PoolDistr (IndividualPoolStake (..), PoolDistr (..))
import Cardano.Ledger.PoolParams (PoolParams)
import Cardano.Ledger.SafeHash (SafeHash)
import qualified Cardano.Ledger.Shelley.Governance as Gov
import Cardano.Ledger.Shelley.HardForks as HardForks (allowMIRTransfer)
import Cardano.Ledger.Shelley.LedgerState hiding (credMapL, delegations, deltaReserves, deltaTreasury, ptrMap, rewards)
import Cardano.Ledger.Shelley.PParams (ProposedPPUpdates (..))
import Cardano.Ledger.Shelley.PoolRank (NonMyopic (..))
import qualified Cardano.Ledger.Shelley.RewardUpdate as RU
import Cardano.Ledger.Shelley.Rewards (Reward (..))
import Cardano.Ledger.Shelley.TxBody (RewardAcnt (..))
import Cardano.Ledger.Shelley.UTxO (EraUTxO (..), ShelleyScriptsNeeded (..))
import Cardano.Ledger.TxIn (TxIn (..))
import Cardano.Ledger.UMap (compactCoinOrError, fromCompact, ptrMap, rdPairMap, sPoolMap, unify)
import Cardano.Ledger.UTxO (UTxO (..))
import Cardano.Ledger.Val (Val (..))
import Data.Default.Class (Default (def))
import qualified Data.Foldable as F
import Data.Functor.Identity (Identity)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe.Strict (maybeToStrictMaybe, strictMaybeToMaybe)
import Data.Ratio ((%))
import qualified Data.Sequence.Strict as SS
import Data.Set (Set)
import qualified Data.Set as Set
import qualified Data.VMap as VMap
import Data.Word (Word16, Word32, Word64)
import GHC.Stack (HasCallStack)
import Lens.Micro
import Numeric.Natural (Natural)
import Test.Cardano.Ledger.Babbage.Serialisation.Generators ()
import Test.Cardano.Ledger.Constrained.Ast
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
field :: EraPParams era => Rep era s -> Term era t -> AnyF era s
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

getName :: EraPParams era => Term era t -> Name era
getName (Var v) = Name v
getName x = error ("nameOf can't find the name in: " ++ show x)

-- ==============================================================
-- NewEpochState fields

type NELens era t = Lens' (NewEpochState era) t

currentEpoch :: EraPParams era => Term era EpochNo
currentEpoch = Var (V "currentEpoch" EpochR (Yes NewEpochStateR nesELL))

prevBlocksMade :: EraPParams era => Term era (Map (KeyHash 'StakePool (EraCrypto era)) Natural)
prevBlocksMade = Var $ V "prevBlocksMade" (MapR PoolHashR NaturalR) (Yes NewEpochStateR nesBprevL)

currBlocksMade :: EraPParams era => Term era (Map (KeyHash 'StakePool (EraCrypto era)) Natural)
currBlocksMade = Var $ V "currBlocksMade" (MapR PoolHashR NaturalR) (Yes NewEpochStateR nesBcurL)

poolDistr :: EraPParams era => Term era (Map (KeyHash 'StakePool (EraCrypto era)) (IndividualPoolStake (EraCrypto era)))
poolDistr = Var $ V "poolDistr" (MapR PoolHashR IPoolStakeR) (Yes NewEpochStateR poolDistrL)

-- | For tests only, Like PoolDistr but has a Rational (rather than a IndividualPoolStake).
mockPoolDistr :: EraPParams era => Term era (Map (KeyHash 'StakePool (EraCrypto era)) Rational)
mockPoolDistr = Var $ V "mockPoolDistr" (MapR PoolHashR RationalR) No

poolDistrL :: NELens era (Map (KeyHash 'StakePool (EraCrypto era)) (IndividualPoolStake (EraCrypto era)))
poolDistrL = nesPdL . unPoolDistrL

unPoolDistrL :: Lens' (PoolDistr c) (Map (KeyHash 'StakePool c) (IndividualPoolStake c))
unPoolDistrL = lens unPoolDistr (\(PoolDistr _) x -> PoolDistr x)

-- CertState - DState

rewards :: EraPParams era => Term era (Map (Credential 'Staking (EraCrypto era)) Coin)
rewards = Var $ V "rewards" (MapR CredR CoinR) (Yes NewEpochStateR rewardsL)

rewardsL :: NELens era (Map (Credential 'Staking (EraCrypto era)) Coin)
rewardsL = nesEsL . esLStateL . lsCertStateL . certDStateL . dsUnifiedL . rewardsUMapL

delegations :: EraPParams era => Term era (Map (Credential 'Staking (EraCrypto era)) (KeyHash 'StakePool (EraCrypto era)))
delegations = Var $ V "delegations" (MapR CredR PoolHashR) (Yes NewEpochStateR delegationsL)

delegationsL :: NELens era (Map (Credential 'Staking (EraCrypto era)) (KeyHash 'StakePool (EraCrypto era)))
delegationsL = nesEsL . esLStateL . lsCertStateL . certDStateL . dsUnifiedL . delegationsUMapL

stakeDeposits :: EraPParams era => Term era (Map (Credential 'Staking (EraCrypto era)) Coin)
stakeDeposits = Var $ V "stakeDeposits" (MapR CredR CoinR) (Yes NewEpochStateR stakeDepositsL)

stakeDepositsL :: NELens era (Map (Credential 'Staking (EraCrypto era)) Coin)
stakeDepositsL = nesEsL . esLStateL . lsCertStateL . certDStateL . dsUnifiedL . stakeDepositsUMapL

ptrs :: EraPParams era => Term era (Map Ptr (Credential 'Staking (EraCrypto era)))
ptrs = Var $ V "ptrs" (MapR PtrR CredR) (Yes NewEpochStateR ptrsL)

ptrsL :: NELens era (Map Ptr (Credential 'Staking (EraCrypto era)))
ptrsL = nesEsL . esLStateL . lsCertStateL . certDStateL . dsUnifiedL . ptrsUMapL

currentDRepState :: EraPParams era => Term era (Map (Credential 'DRepRole (EraCrypto era)) (DRepState (EraCrypto era)))
currentDRepState = Var $ V "currentDRepState" (MapR VCredR DRepStateR) (Yes NewEpochStateR drepsL)

drepsL :: NELens era (Map (Credential 'DRepRole (EraCrypto era)) (DRepState (EraCrypto era)))
drepsL = nesEsL . esLStateL . lsCertStateL . certVStateL . vsDRepsL

drepDelegation :: EraPParams era => Term era (Map (Credential 'Staking (EraCrypto era)) (DRep (EraCrypto era)))
drepDelegation = Var $ V "drepDelegation" (MapR CredR DRepR) (Yes NewEpochStateR drepDelegationL)

drepDelegationL :: NELens era (Map (Credential 'Staking (EraCrypto era)) (DRep (EraCrypto era)))
drepDelegationL = nesEsL . esLStateL . lsCertStateL . certDStateL . dsUnifiedL . drepUMapL

futureGenDelegs :: EraPParams era => Term era (Map (FutureGenDeleg (EraCrypto era)) (GenDelegPair (EraCrypto era)))
futureGenDelegs =
  Var $
    V
      "futureGenDelegs"
      (MapR FutureGenDelegR GenDelegPairR)
      (Yes NewEpochStateR futureGenDelegsL)

futureGenDelegsL :: NELens era (Map (FutureGenDeleg (EraCrypto era)) (GenDelegPair (EraCrypto era)))
futureGenDelegsL = nesEsL . esLStateL . lsCertStateL . certDStateL . dsFutureGenDelegsL

genDelegs :: EraPParams era => Term era (Map (KeyHash 'Genesis (EraCrypto era)) (GenDelegPair (EraCrypto era)))
genDelegs = Var $ V "genDelegs" (MapR GenHashR GenDelegPairR) (Yes NewEpochStateR genDelegsL)

genDelegsL :: NELens era (Map (KeyHash 'Genesis (EraCrypto era)) (GenDelegPair (EraCrypto era)))
genDelegsL = nesEsL . esLStateL . lsCertStateL . certDStateL . dsGenDelegsL . unGenDelegsL

-- DState - InstantaneousRewards

instanReserves :: EraPParams era => Term era (Map (Credential 'Staking (EraCrypto era)) Coin)
instanReserves = Var $ V "instanReserves" (MapR CredR CoinR) (Yes NewEpochStateR instanReservesL)

instanReservesL :: NELens era (Map (Credential 'Staking (EraCrypto era)) Coin)
instanReservesL = nesEsL . esLStateL . lsCertStateL . certDStateL . dsIRewardsL . iRReservesL

instanReservesSum :: EraPParams era => Term era Coin
instanReservesSum = Var (V "instanReservesSum" CoinR No)

instanTreasury :: EraPParams era => Term era (Map (Credential 'Staking (EraCrypto era)) Coin)
instanTreasury = Var $ V "instanTreasury" (MapR CredR CoinR) (Yes NewEpochStateR instanTreasuryL)

instanTreasuryL :: NELens era (Map (Credential 'Staking (EraCrypto era)) Coin)
instanTreasuryL = nesEsL . esLStateL . lsCertStateL . certDStateL . dsIRewardsL . iRTreasuryL

instanTreasurySum :: EraPParams era => Term era Coin
instanTreasurySum = Var (V "instanTreasurySum" CoinR No)

deltaReserves :: EraPParams era => Term era DeltaCoin
deltaReserves = Var $ V "deltaReserves" DeltaCoinR (Yes NewEpochStateR deltaReservesNEL)

deltaReservesNEL :: NELens era DeltaCoin
deltaReservesNEL = nesEsL . esLStateL . lsCertStateL . certDStateL . dsIRewardsL . deltaReservesL

deltaTreasury :: EraPParams era => Term era DeltaCoin
deltaTreasury = Var $ V "deltaTreasury" DeltaCoinR (Yes NewEpochStateR deltaTreasuryNEL)

deltaTreasuryNEL :: NELens era DeltaCoin
deltaTreasuryNEL = nesEsL . esLStateL . lsCertStateL . certDStateL . dsIRewardsL . deltaTreasuryL

-- CertState - PState

regPools :: EraPParams era => Term era (Map (KeyHash 'StakePool (EraCrypto era)) (PoolParams (EraCrypto era)))
regPools = Var $ V "regPools" (MapR PoolHashR PoolParamsR) (Yes NewEpochStateR regPoolsL)

regPoolsL :: NELens era (Map (KeyHash 'StakePool (EraCrypto era)) (PoolParams (EraCrypto era)))
regPoolsL = nesEsL . esLStateL . lsCertStateL . certPStateL . psStakePoolParamsL

futureRegPools :: EraPParams era => Term era (Map (KeyHash 'StakePool (EraCrypto era)) (PoolParams (EraCrypto era)))
futureRegPools = Var $ V "futureRegPools" (MapR PoolHashR PoolParamsR) (Yes NewEpochStateR futureRegPoolsL)

futureRegPoolsL :: NELens era (Map (KeyHash 'StakePool (EraCrypto era)) (PoolParams (EraCrypto era)))
futureRegPoolsL = nesEsL . esLStateL . lsCertStateL . certPStateL . psFutureStakePoolParamsL

retiring :: EraPParams era => Term era (Map (KeyHash 'StakePool (EraCrypto era)) EpochNo)
retiring = Var $ V "retiring" (MapR PoolHashR EpochR) (Yes NewEpochStateR retiringL)

retiringL :: NELens era (Map (KeyHash 'StakePool (EraCrypto era)) EpochNo)
retiringL = nesEsL . esLStateL . lsCertStateL . certPStateL . psRetiringL

poolDeposits :: EraPParams era => Term era (Map (KeyHash 'StakePool (EraCrypto era)) Coin)
poolDeposits = Var $ V "poolDeposits" (MapR PoolHashR CoinR) (Yes NewEpochStateR poolDepositsL)

poolDepositsL :: NELens era (Map (KeyHash 'StakePool (EraCrypto era)) Coin)
poolDepositsL = nesEsL . esLStateL . lsCertStateL . certPStateL . psDepositsL

committeeState :: EraPParams era => Term era (Map (Credential 'ColdCommitteeRole (EraCrypto era)) (Maybe (Credential 'HotCommitteeRole (EraCrypto era))))
committeeState = Var $ V "committeeState" (MapR CommColdCredR (MaybeR CommHotCredR)) (Yes NewEpochStateR committeeStateL)

committeeStateL :: NELens era (Map (Credential 'ColdCommitteeRole (EraCrypto era)) (Maybe (Credential 'HotCommitteeRole (EraCrypto era))))
committeeStateL = nesEsL . esLStateL . lsCertStateL . certVStateL . vsCommitteeStateL . csCommitteeCredsL

numDormantEpochs :: EraPParams era => Term era EpochNo
numDormantEpochs = Var $ V "numDormantEpochs" EpochR (Yes NewEpochStateR numDormantEpochsL)

numDormantEpochsL :: NELens era EpochNo
numDormantEpochsL = nesEsL . esLStateL . lsCertStateL . certVStateL . vsNumDormantEpochsL

-- UTxOState

utxo :: EraPParams era => Proof era -> Term era (Map (TxIn (EraCrypto era)) (TxOutF era))
utxo p = Var $ pV p "utxo" (MapR TxInR (TxOutR p)) (Yes NewEpochStateR (utxoL p))

utxoL :: Proof era -> NELens era (Map (TxIn (EraCrypto era)) (TxOutF era))
utxoL proof = nesEsL . esLStateL . lsUTxOStateL . utxosUtxoL . unUtxoL proof

unUtxoL :: Proof era -> Lens' (UTxO era) (Map (TxIn (EraCrypto era)) (TxOutF era))
unUtxoL p = lens (Map.map (TxOutF p) . unUTxO) (\(UTxO _) new -> liftUTxO new)

deposits :: EraPParams era => Term era Coin
deposits = Var $ V "deposits" CoinR (Yes NewEpochStateR depositsL)

depositsL :: NELens era Coin
depositsL = nesEsL . esLStateL . lsUTxOStateL . utxosDepositedL

fees :: EraPParams era => Term era Coin
fees = Var $ V "fees" CoinR (Yes NewEpochStateR feesL)

feesL :: NELens era Coin
feesL = nesEsL . esLStateL . lsUTxOStateL . utxosFeesL

donation :: EraPParams era => Term era Coin
donation = Var $ V "donation" CoinR (Yes NewEpochStateR donationL)

donationL :: NELens era Coin
donationL = nesEsL . esLStateL . lsUTxOStateL . utxosDonationL

ppup :: EraPParams era => Proof era -> Term era (ShelleyGovState era)
ppup p = Var $ pV p "ppup" (PPUPStateR p) (Yes NewEpochStateR (ppupsL p))

ppupsL :: Proof era -> NELens era (ShelleyGovState era)
ppupsL (Shelley _) = nesEsL . esLStateL . lsUTxOStateL . utxosGovStateL
ppupsL (Allegra _) = nesEsL . esLStateL . lsUTxOStateL . utxosGovStateL
ppupsL (Mary _) = nesEsL . esLStateL . lsUTxOStateL . utxosGovStateL
ppupsL (Alonzo _) = nesEsL . esLStateL . lsUTxOStateL . utxosGovStateL
ppupsL (Babbage _) = nesEsL . esLStateL . lsUTxOStateL . utxosGovStateL
ppupsL (Conway _) = error "Conway era does not have a PPUPState, in ppupsL"

pparamProposals :: EraPParams era => Proof era -> Term era (Map (KeyHash 'Genesis (EraCrypto era)) (PParamsUpdateF era))
pparamProposals p = Var (pV p "pparamProposals" (MapR GenHashR (PParamsUpdateR p)) No)

futurePParamProposals ::
  EraPParams era => Proof era -> Term era (Map (KeyHash 'Genesis (EraCrypto era)) (PParamsUpdateF era))
futurePParamProposals p = Var (pV p "futurePParamProposals" (MapR GenHashR (PParamsUpdateR p)) No)

currPParams :: EraPParams era => Proof era -> Term era (PParamsF era)
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
    :$ Lensed (pparamProposals p) (shellyGovStateProposedPPUpdatesL . proposedMapL p)
    :$ Lensed (futurePParamProposals p) (futureproposalsL . proposedMapL p)
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

govStateT :: forall era. EraPParams era => Proof era -> RootTarget era (GovState era) (GovState era)
govStateT p@(Shelley _) = Invert "GovState" (typeRep @(GovState era)) (GovState p) :$ Shift (ppupStateT p) govL
govStateT p@(Allegra _) = Invert "GovState" (typeRep @(GovState era)) (GovState p) :$ Shift (ppupStateT p) govL
govStateT p@(Mary _) = Invert "GovState" (typeRep @(GovState era)) (GovState p) :$ Shift (ppupStateT p) govL
govStateT p@(Alonzo _) = Invert "GovState" (typeRep @(GovState era)) (GovState p) :$ Shift (ppupStateT p) govL
govStateT p@(Babbage _) = Invert "GovState" (typeRep @(GovState era)) (GovState p) :$ Shift (ppupStateT p) govL
govStateT p@(Conway _) = Invert "GovState" (typeRep @(GovState era)) (GovState p) :$ Shift (unReflect conwayGovStateT p) govL

individualPoolStakeL :: Lens' (IndividualPoolStake c) Rational
individualPoolStakeL = lens individualPoolStake (\ds u -> ds {individualPoolStake = u})

-- Incremental Stake

isPtrMapT :: EraPParams era => Term era (Map Ptr Coin)
isPtrMapT = Var $ V "ptrMap" (MapR PtrR CoinR) (Yes NewEpochStateR ptrMapL)

ptrMapL :: Lens' (NewEpochState era) (Map Ptr Coin)
ptrMapL = nesEsL . esLStateL . lsUTxOStateL . utxosStakeDistrL . isPtrMapL

isCredMapT :: EraPParams era => Term era (Map (Credential 'Staking (EraCrypto era)) Coin)
isCredMapT = Var $ V "credMap" (MapR CredR CoinR) (Yes NewEpochStateR credMapL)

credMapL :: Lens' (NewEpochState era) (Map (Credential 'Staking (EraCrypto era)) Coin)
credMapL = nesEsL . esLStateL . lsUTxOStateL . utxosStakeDistrL . isCredMapL

-- AccountState

treasury :: EraPParams era => Term era Coin
treasury = Var $ V "treasury" CoinR (Yes NewEpochStateR treasuryL)

treasuryL :: NELens era Coin
treasuryL = nesEsL . esAccountStateL . asTreasuryL

reserves :: EraPParams era => Term era Coin
reserves = Var $ V "reserves" CoinR (Yes NewEpochStateR reservesL)

reservesL :: NELens era Coin
reservesL = nesEsL . esAccountStateL . asReservesL

-- | The Coin availabe for a MIR transfer to/from the Treasury
--   Computed from 'treasury' + 'deltaTreasury' - sum('instanTreasury')
mirAvailTreasury :: EraPParams era => Term era Coin
mirAvailTreasury = Var (V "mirAvailTreasury" CoinR No)

-- | The Coin availabe for a MIR transfer to/from the Reserves
--   Computed from 'reserves' + 'deltaReserves' - sum('instanReserves')
mirAvailReserves :: EraPParams era => Term era Coin
mirAvailReserves = Var (V "mirAvailReserves" CoinR No)

-- EpochState

snapshots :: EraPParams era => Term era (SnapShots (EraCrypto era))
snapshots = Var (V "snapshots" SnapShotsR (Yes NewEpochStateR snapshotsL))

snapshotsL :: NELens era (SnapShots (EraCrypto era))
snapshotsL = nesEsL . esSnapshotsL

-- | Lens' from the Core PParams to the Model PParamsF which embeds a (Proof era)
ppFL :: Proof era -> Lens' (PParams era) (PParamsF era)
ppFL p = lens (\pp -> PParamsF p pp) (\_ (PParamsF _ qq) -> qq)

pparamsVar :: Gov.EraGov era => Proof era -> V era (PParamsF era)
pparamsVar p = (V "pparams" (PParamsR p) (Yes NewEpochStateR (nesEsL . curPParamsEpochStateL . ppFL p)))

pparams :: Gov.EraGov era => Proof era -> Term era (PParamsF era)
pparams p = Var $ pparamsVar p

nmLikelihoodsT :: EraPParams era => Term era (Map (KeyHash 'StakePool (EraCrypto era)) [Float])
nmLikelihoodsT = Var (V "likelihoodsNM" (MapR PoolHashR (ListR FloatR)) (Yes NewEpochStateR (nesEsL . esNonMyopicL . nmLikelihoodsL)))

nmRewardPotT :: EraPParams era => Term era Coin
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

markStake :: EraPParams era => Term era (Map (Credential 'Staking (EraCrypto era)) Coin)
markStake = Var (V "markStake" (MapR CredR CoinR) (Yes NewEpochStateR markStakeL))

markDelegs :: EraPParams era => Term era (Map (Credential 'Staking (EraCrypto era)) (KeyHash 'StakePool (EraCrypto era)))
markDelegs = Var (V "markDelegs" (MapR CredR PoolHashR) (Yes NewEpochStateR markDelegsL))

markDelegsL :: NELens era (Map (Credential 'Staking (EraCrypto era)) (KeyHash 'StakePool (EraCrypto era)))
markDelegsL = nesEsL . esSnapshotsL . ssStakeMarkL . ssDelegationsL . vmapL

markPools :: EraPParams era => Term era (Map (KeyHash 'StakePool (EraCrypto era)) (PoolParams (EraCrypto era)))
markPools = Var (V "markPools" (MapR PoolHashR PoolParamsR) (Yes NewEpochStateR markPoolsL))

markPoolsL :: NELens era (Map (KeyHash 'StakePool (EraCrypto era)) (PoolParams (EraCrypto era)))
markPoolsL = nesEsL . esSnapshotsL . ssStakeMarkL . ssPoolParamsL . vmapL

markSnapShotT :: forall era. EraPParams era => RootTarget era (SnapShot (EraCrypto era)) (SnapShot (EraCrypto era))
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

setStake :: EraPParams era => Term era (Map (Credential 'Staking (EraCrypto era)) Coin)
setStake = Var (V "setStake" (MapR CredR CoinR) (Yes NewEpochStateR setStakeL))

setStakeL :: NELens era (Map (Credential 'Staking (EraCrypto era)) Coin)
setStakeL = nesEsL . esSnapshotsL . ssStakeSetL . ssStakeL . stakeL

setDelegs :: EraPParams era => Term era (Map (Credential 'Staking (EraCrypto era)) (KeyHash 'StakePool (EraCrypto era)))
setDelegs = Var (V "setDelegs" (MapR CredR PoolHashR) (Yes NewEpochStateR setDelegsL))

setDelegsL :: NELens era (Map (Credential 'Staking (EraCrypto era)) (KeyHash 'StakePool (EraCrypto era)))
setDelegsL = nesEsL . esSnapshotsL . ssStakeSetL . ssDelegationsL . vmapL

setPools :: EraPParams era => Term era (Map (KeyHash 'StakePool (EraCrypto era)) (PoolParams (EraCrypto era)))
setPools = Var (V "setPools" (MapR PoolHashR PoolParamsR) (Yes NewEpochStateR setPoolsL))

setPoolsL :: NELens era (Map (KeyHash 'StakePool (EraCrypto era)) (PoolParams (EraCrypto era)))
setPoolsL = nesEsL . esSnapshotsL . ssStakeSetL . ssPoolParamsL . vmapL

setSnapShotT :: forall era. EraPParams era => RootTarget era (SnapShot (EraCrypto era)) (SnapShot (EraCrypto era))
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

goStake :: EraPParams era => Term era (Map (Credential 'Staking (EraCrypto era)) Coin)
goStake = Var (V "goStake" (MapR CredR CoinR) (Yes NewEpochStateR goStakeL))

goStakeL :: NELens era (Map (Credential 'Staking (EraCrypto era)) Coin)
goStakeL = nesEsL . esSnapshotsL . ssStakeGoL . ssStakeL . stakeL

goDelegs :: EraPParams era => Term era (Map (Credential 'Staking (EraCrypto era)) (KeyHash 'StakePool (EraCrypto era)))
goDelegs = Var (V "goDelegs" (MapR CredR PoolHashR) (Yes NewEpochStateR goDelegsL))

goDelegsL :: NELens era (Map (Credential 'Staking (EraCrypto era)) (KeyHash 'StakePool (EraCrypto era)))
goDelegsL = nesEsL . esSnapshotsL . ssStakeGoL . ssDelegationsL . vmapL

goPools :: EraPParams era => Term era (Map (KeyHash 'StakePool (EraCrypto era)) (PoolParams (EraCrypto era)))
goPools = Var (V "goPools" (MapR PoolHashR PoolParamsR) (Yes NewEpochStateR goPoolsL))

goPoolsL :: NELens era (Map (KeyHash 'StakePool (EraCrypto era)) (PoolParams (EraCrypto era)))
goPoolsL = nesEsL . esSnapshotsL . ssStakeGoL . ssPoolParamsL . vmapL

goSnapShotT :: forall era. EraPParams era => RootTarget era (SnapShot (EraCrypto era)) (SnapShot (EraCrypto era))
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

markPoolDistr :: EraPParams era => Term era (Map (KeyHash 'StakePool (EraCrypto era)) (IndividualPoolStake (EraCrypto era)))
markPoolDistr = Var (V "markPoolDistr" (MapR PoolHashR IPoolStakeR) No)

markPoolDistrL :: NELens era (Map (KeyHash 'StakePool (EraCrypto era)) (IndividualPoolStake (EraCrypto era)))
markPoolDistrL = nesEsL . esSnapshotsL . ssStakeMarkPoolDistrL . pooldistrHelpL

pooldistrHelpL :: Lens' (PoolDistr c) (Map (KeyHash 'StakePool c) (IndividualPoolStake c))
pooldistrHelpL = lens unPoolDistr (\_ u -> PoolDistr u)

snapShotFee :: EraPParams era => Term era Coin
snapShotFee = Var (V "snapShotFee" CoinR No)

snapShotsT :: forall era. EraPParams era => RootTarget era (SnapShots (EraCrypto era)) (SnapShots (EraCrypto era))
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

deltaT :: EraPParams era => Term era (Maybe DeltaCoin)
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

deltaR :: EraPParams era => Term era (Maybe DeltaCoin)
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

deltaF :: EraPParams era => Term era (Maybe DeltaCoin)
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

rewardSet :: EraPParams era => Term era (Map (Credential 'Staking (EraCrypto era)) (Set (Reward (EraCrypto era))))
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

totalAda :: EraPParams era => Term era Coin
totalAda = Var $ V "totalAda" CoinR No

utxoCoin :: EraPParams era => Term era Coin
utxoCoin = Var $ V "utxoCoin" CoinR No

-- | The universe of Staking Credentials. A credential is either KeyHash of a ScriptHash
--   Any Plutus scripts in this Universe are NOT Spending scripts, so they do not need a Redeemer
credsUniv :: EraPParams era => Term era (Set (Credential 'Staking (EraCrypto era)))
credsUniv = Var $ V "credsUniv" (SetR CredR) No

-- | The universe of Staking Credentials. A credential is either KeyHash of a ScriptHash
--   All Plutus scripts in this Universe are SPENDING scripts, so they will need a Redeemer
--   Use this ONLY in the Pay-part of an Address (Do not use this in the Stake-part of an Address)
spendCredsUniv :: EraPParams era => Term era (Set (Credential 'Payment (EraCrypto era)))
spendCredsUniv = Var $ V "spendCredsUniv" (SetR PCredR) No

-- | The universe of Voting Credentials. A credential is either KeyHash of a ScriptHash
voteUniv :: EraPParams era => Term era (Set (Credential 'DRepRole (EraCrypto era)))
voteUniv = Var $ V "voteUniv" (SetR VCredR) No

-- | The universe of DReps
drepUniv :: EraPParams era => Term era (Set (DRep (EraCrypto era)))
drepUniv = Var $ V "drepUniv" (SetR DRepR) No

-- | The universe of Credentials used in voting for constitutional committee changes.
hotCommitteeCredsUniv :: EraPParams era => Term era (Set (Credential 'HotCommitteeRole (EraCrypto era)))
hotCommitteeCredsUniv = Var $ V "hotCommitteeCredsUniv" (SetR CommHotCredR) No

-- | The universe of Credentials used in voting for constitutional committee changes.
coldCommitteeCredsUniv :: EraPParams era => Term era (Set (Credential 'ColdCommitteeRole (EraCrypto era)))
coldCommitteeCredsUniv = Var $ V "coldCommitteeCredsUniv" (SetR CommColdCredR) No

-- | The universe of Payment Credentials. A credential is either KeyHash of a ScriptHash
--   We only find payment credentials in the Payment part of an Addr.
payUniv :: EraPParams era => Term era (Set (Credential 'Payment (EraCrypto era)))
payUniv = Var $ V "payUniv" (SetR PCredR) No

-- | The universe of Scripts (and their hashes) useable in spending contexts
--  That means if they are Plutus scripts then they will be passed an additional
--  argument (the TxInfo context)
spendscriptUniv :: EraPParams era => Proof era -> Term era (Map (ScriptHash (EraCrypto era)) (ScriptF era))
spendscriptUniv p = Var (pV p "spendscriptUniv" (MapR ScriptHashR (ScriptR p)) No)

-- | The universe of Scripts (and their hashes) useable in contexts other than Spending
nonSpendScriptUniv :: EraPParams era => Proof era -> Term era (Map (ScriptHash (EraCrypto era)) (ScriptF era))
nonSpendScriptUniv p = Var (pV p "nonSpendScriptUniv" (MapR ScriptHashR (ScriptR p)) No)

-- | The union of 'spendscriptUniv' and 'nonSpendScriptUniv'. All possible scripts in any context
allScriptUniv :: EraPParams era => Proof era -> Term era (Map (ScriptHash (EraCrypto era)) (ScriptF era))
allScriptUniv p = Var (pV p "allScriptUniv" (MapR ScriptHashR (ScriptR p)) No)

-- | The universe of Data (and their hashes)
dataUniv :: EraPParams era => Term era (Map (DataHash (EraCrypto era)) (Data era))
dataUniv = Var (V "dataUniv" (MapR DataHashR DataR) No)

-- | The universe of StakePool key hashes. These hashes hash the cold key of the
--   Pool operators.
poolHashUniv :: EraPParams era => Term era (Set (KeyHash 'StakePool (EraCrypto era)))
poolHashUniv = Var $ V "poolHashUniv" (SetR PoolHashR) No

-- | The universe of StakePool key hashes. These hashes hash are hashes of the Owners of a PoolParam
stakeHashUniv :: EraPParams era => Term era (Set (KeyHash 'Staking (EraCrypto era)))
stakeHashUniv = Var $ V "stakeHashUniv" (SetR StakeHashR) No

-- | The universe of DRep key hashes. These hashes hash are hashes of the DReps
drepHashUniv :: EraPParams era => Term era (Set (KeyHash 'DRepRole (EraCrypto era)))
drepHashUniv = Var $ V "drepHashUniv" (SetR DRepHashR) No

-- | The universe of the Genesis key hashes and their signing and validating GenDelegPairs
genesisHashUniv :: EraPParams era => Term era (Map (KeyHash 'Genesis (EraCrypto era)) (GenDelegPair (EraCrypto era)))
genesisHashUniv = Var $ V "genesisHashUniv" (MapR GenHashR GenDelegPairR) No

voteCredUniv :: EraPParams era => Term era (Set (Credential 'ColdCommitteeRole (EraCrypto era)))
voteCredUniv = Var $ V "voteHashUniv" (SetR CommColdCredR) No

-- | The universe of TxIns. Pairs of TxId: hashes of previously run transaction bodies,
--   and TxIx: indexes of one of the bodies outputs.
txinUniv :: EraPParams era => Term era (Set (TxIn (EraCrypto era)))
txinUniv = Var $ V "txinUniv" (SetR TxInR) No

-- | The universe of GovActionId. Pairs of TxId: hashes of previously run transaction bodies,
--   and GovActionIx: indexes of one of the bodies Proposals .
govActionIdUniv :: EraPParams era => Term era (Set (GovActionId (EraCrypto era)))
govActionIdUniv = Var $ V "govActionIdUniv" (SetR GovActionIdR) No

-- | The universe of TxOuts.
--   It contains 'colTxoutUniv' as a sublist and 'feeOutput' as an element
--   See also 'feeOutput' which is defined by the universes, and is related.
txoutUniv :: EraPParams era => Proof era -> Term era (Set (TxOutF era))
txoutUniv p = Var (pV p "txoutUniv" (SetR (TxOutR p)) No)

-- | The universe of TxOuts useable for collateral
--   The collateral TxOuts consists only of VKey addresses
--   and The collateral TxOuts do not contain any non-ADA part
colTxoutUniv :: EraPParams era => Proof era -> Term era (Set (TxOutF era))
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
feeTxIn :: EraPParams era => Term era (TxIn (EraCrypto era))
feeTxIn = Var (V "feeTxIn" TxInR No)

-- | A Coin large enough to pay almost any fee.
--   See also 'feeOutput' which is related.
bigCoin :: EraPParams era => Term era Coin
bigCoin = Var (V "bigCoin" CoinR No)

datumsUniv :: EraPParams era => Term era [Datum era]
datumsUniv = Var (V "datumsUniv" (ListR DatumR) No)

multiAssetUniv :: EraPParams era => Term era [MultiAsset (EraCrypto era)]
multiAssetUniv = Var (V "multiAssetUniv" (ListR MultiAssetR) No)

-- | The universe of key hashes, and the signing and validating key pairs they represent.
keymapUniv :: EraPParams era => Term era (Map (KeyHash 'Witness (EraCrypto era)) (KeyPair 'Witness (EraCrypto era)))
keymapUniv = Var (V "keymapUniv" (MapR WitHashR KeyPairR) No)

currentSlot :: EraPParams era => Term era SlotNo
currentSlot = Var (V "currentSlot" SlotNoR No)

endSlotDelta :: EraPParams era => Term era SlotNo
endSlotDelta = Var (V "endSlotDelta" SlotNoR No)

beginSlotDelta :: EraPParams era => Term era SlotNo
beginSlotDelta = Var (V "beginSlotDelta" SlotNoR No)

-- See also currentEpoch in NewEpochState fields

-- | From Globals
network :: EraPParams era => Term era Network
network = Var (V "network" NetworkR No)

-- | This not really a variable, But a constant that is set by the 'testGlobals'
--   we reflect this into a Term, so we can refer to it in the Preds.
quorumConstant :: Word64
quorumConstant = Base.quorum Utils.testGlobals

-- | From Globals. Reflected here at type Int, This is set to 'quorumConstant' in CertState.
--   because is is used to compare the Size of things, which are computed as Int
quorum :: EraPParams era => Term era Int
quorum = Var (V "quorum" IntR No)

addrUniv :: EraPParams era => Term era (Set (Addr (EraCrypto era)))
addrUniv = Var $ V "addrUniv" (SetR AddrR) No

ptrUniv :: EraPParams era => Term era (Set Ptr)
ptrUniv = Var $ V "ptrUniv" (SetR PtrR) No

plutusUniv :: Reflect era => Term era (Map (ScriptHash (EraCrypto era)) (IsValid, ScriptF era))
plutusUniv = Var $ V "plutusUniv" (MapR ScriptHashR (PairR IsValidR (ScriptR reify))) No

spendPlutusUniv :: Reflect era => Term era (Map (ScriptHash (EraCrypto era)) (IsValid, ScriptF era))
spendPlutusUniv = Var $ V "spendPlutusUniv" (MapR ScriptHashR (PairR IsValidR (ScriptR reify))) No

-- | The universe of all Byron addresses. In Eras, Babbage, Conway we avoid these Adresses,
--   as they do not play well with Plutus Scripts.
byronAddrUniv :: EraPParams era => Term era (Map (KeyHash 'Payment (EraCrypto era)) (Addr (EraCrypto era), SigningKey))
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
accountStateT :: EraPParams era => RootTarget era AccountState AccountState
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
certstateT :: forall era. EraPParams era => RootTarget era (CertState era) (CertState era)
certstateT =
  Invert "CertState" (typeRep @(CertState era)) CertState
    :$ (Shift vstateT certVStateL)
    :$ (Shift pstateT certPStateL)
    :$ (Shift dstateT certDStateL)

-- | Target for VState
vstateT :: forall era. EraPParams era => RootTarget era (VState era) (VState era)
vstateT =
  Invert "VState" (typeRep @(VState era)) (\x y z -> VState x (CommitteeState y) z)
    :$ Lensed currentDRepState vsDRepsL
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

-- | Target for PState
pstateT :: forall era. EraPParams era => RootTarget era (PState era) (PState era)
pstateT =
  Invert "PState" (typeRep @(PState era)) PState
    :$ Lensed regPools psStakePoolParamsL
    :$ Lensed futureRegPools psFutureStakePoolParamsL
    :$ Lensed retiring psRetiringL
    :$ Lensed poolDeposits psDepositsL

-- | Target for DState
dstateT :: forall era. EraPParams era => RootTarget era (DState era) (DState era)
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
  Map (Credential 'Staking (EraCrypto era)) Coin ->
  Map (Credential 'Staking (EraCrypto era)) Coin ->
  Map (Credential 'Staking (EraCrypto era)) (KeyHash 'StakePool (EraCrypto era)) ->
  Map (Credential 'Staking (EraCrypto era)) (DRep (EraCrypto era)) ->
  Map Ptr (Credential 'Staking (EraCrypto era)) ->
  Map (FutureGenDeleg (EraCrypto era)) (GenDelegPair (EraCrypto era)) ->
  Map (KeyHash 'Genesis (EraCrypto era)) (GenDelegPair (EraCrypto era)) ->
  InstantaneousRewards (EraCrypto era) ->
  DState era
dstate rew dep deleg drepdeleg ptr fgen gen =
  DState (unSplitUMap (Split rew dep deleg drepdeleg (error "Not implemented") ptr)) fgen (GenDelegs gen)

instantaneousRewardsT ::
  forall era.
  EraPParams era =>
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

printTarget :: EraPParams era => RootTarget era root t -> IO ()
printTarget t = putStrLn (show (ppTarget t))

-- =====================================================================
-- PParams fields

-- | ProtVer in pparams
protVer :: EraPParams era => Proof era -> Term era ProtVer
protVer proof =
  Var
    ( pV
        proof
        "protVer"
        (ProtVerR proof)
        (Yes (PParamsR proof) $ withEraPParams proof (pparamsWrapperL . ppProtocolVersionL))
    )

-- | ProtVer in prevPParams
prevProtVer :: EraPParams era => Proof era -> Term era ProtVer
prevProtVer proof =
  Var
    ( pV
        proof
        "prevProtVer"
        (ProtVerR proof)
        (Yes (PParamsR proof) $ withEraPParams proof (pparamsWrapperL . ppProtocolVersionL))
    )

minFeeA :: EraPParams era => Proof era -> Term era Coin
minFeeA proof =
  Var
    ( pV
        proof
        "minFeeA"
        CoinR
        (Yes (PParamsR proof) $ withEraPParams proof (pparamsWrapperL . ppMinFeeAL))
    )

minFeeB :: EraPParams era => Proof era -> Term era Coin
minFeeB proof =
  Var
    ( pV
        proof
        "minFeeB"
        CoinR
        (Yes (PParamsR proof) $ withEraPParams proof (pparamsWrapperL . ppMinFeeBL))
    )

-- | Max Block Body Size
maxBBSize :: EraPParams era => Proof era -> Term era Natural
maxBBSize p =
  Var
    ( pV
        p
        "maxBBSize"
        NaturalR
        (Yes (PParamsR p) (withEraPParams p (pparamsWrapperL . ppMaxBBSizeL . word32NaturalL)))
    )

-- | Max Tx Size
maxTxSize :: EraPParams era => Proof era -> Term era Natural
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
      error $ "While converting " ++ name ++ ", " ++ show x <> " is out of bounds: " <> show (minBound :: b, maxBound :: b)
  where
    xi = toInteger x

word32NaturalL :: Lens' Word32 Natural
word32NaturalL = lens fromIntegral (\_ y -> fromIntegralBounded "word32NaturaL" (toInteger y))

word16NaturalL :: Lens' Word16 Natural
word16NaturalL = lens fromIntegral (\_ y -> fromIntegralBounded "word16NaturalL" (toInteger y))

-- | Max Block Header Size
maxBHSize :: EraPParams era => Proof era -> Term era Natural
maxBHSize p =
  Var
    ( pV
        p
        "maxBHSize"
        NaturalR
        (Yes (PParamsR p) (withEraPParams p (pparamsWrapperL . ppMaxBHSizeL . word16NaturalL)))
    )

poolDepAmt :: EraPParams era => Proof era -> Term era Coin
poolDepAmt p =
  Var $
    pV
      p
      "poolDepAmt"
      CoinR
      (Yes (PParamsR p) (withEraPParams p (pparamsWrapperL . ppPoolDepositL)))

keyDepAmt :: EraPParams era => Proof era -> Term era Coin
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
drepDeposit p = Var $ pV p "drepDeposit" CoinR (Yes (PParamsR p) (withEraPParams p (pparamsWrapperL . ppDRepDepositL)))

drepActivity :: ConwayEraPParams era => Proof era -> Term era Base.EpochInterval
drepActivity p = Var $ pV p "drepActivty" EpochIntervalR (Yes (PParamsR p) (withEraPParams p (pparamsWrapperL . ppDRepActivityL)))

maxEpoch :: EraPParams era => Proof era -> Term era Base.EpochInterval
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

inputs :: EraPParams era => Term era (Set (TxIn (EraCrypto era)))
inputs = Var $ V "inputs" (SetR TxInR) No

collateral :: EraPParams era => Term era (Set (TxIn (EraCrypto era)))
collateral = Var $ V "collateral" (SetR TxInR) No

refInputs :: EraPParams era => Term era (Set (TxIn (EraCrypto era)))
refInputs = Var $ V "refInputs" (SetR TxInR) No

outputs :: EraPParams era => Proof era -> Term era [TxOutF era]
outputs p = Var $ pV p "outputs" (ListR (TxOutR p)) No

collateralReturn :: EraPParams era => Proof era -> Term era (TxOutF era)
collateralReturn p = Var $ pV p "collateralReturn" (TxOutR p) No

-- | The sum of all the 'collateral' inputs. The Tx is constucted
--   by SNothing or wrapping 'SJust' around this value.
totalCol :: EraPParams era => Term era Coin
totalCol = Var $ V "totalCol" CoinR No

certs :: Reflect era => Term era [TxCertF era]
certs = Var $ V "certs" (ListR (TxCertR reify)) No

withdrawals :: forall era. EraPParams era => Term era (Map (RewardAcnt (EraCrypto era)) Coin)
withdrawals = Var $ V "withdrawals" (MapR (RewardAcntR @era) CoinR) No

txfee :: EraPParams era => Term era Coin
txfee = Var $ V "txfee" CoinR No

ttl :: EraPParams era => Term era SlotNo
ttl = Var $ V "ttl" SlotNoR No

validityInterval :: EraPParams era => Term era ValidityInterval
validityInterval = Var $ V "validityInterval" ValidityIntervalR No

mint :: EraPParams era => Term era (Map (ScriptHash (EraCrypto era)) (Map AssetName Integer))
mint = Var $ V "mint" (MapR ScriptHashR (MapR AssetNameR IntegerR)) No

reqSignerHashes :: EraPParams era => Term era (Set (KeyHash 'Witness (EraCrypto era)))
reqSignerHashes = Var $ V "reqSignerHashes" (SetR WitHashR) No

networkID :: EraPParams era => Term era (Maybe Network)
networkID = Var $ V "networkID" (MaybeR NetworkR) No

adHash :: EraPParams era => Term era (Maybe (AuxiliaryDataHash (EraCrypto era)))
adHash = Var $ V "adHash" (MaybeR AuxiliaryDataHashR) No

wppHash :: EraPParams era => Term era (Maybe (SafeHash (EraCrypto era) EraIndependentScriptIntegrity))
wppHash = Var $ V "wppHash" (MaybeR ScriptIntegrityHashR) No

txDonation :: EraPParams era => Term era Coin
txDonation = Var $ V "txDonation" CoinR No

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
extraCol :: EraPParams era => Term era Coin
extraCol = Var $ V "extraCol" CoinR No

-- | The sum of all the 'collateral' inputs, total colateral of the Tx is computed by adding (SJust _) to this value.
sumCol :: EraPParams era => Term era Coin
sumCol = Var $ V "sumCol" CoinR No

colRetAddr :: EraPParams era => Term era (Addr (EraCrypto era))
colRetAddr = Var $ V "colRetAddr" AddrR No

-- | The Coin in the 'collateralReturn' TxOut
colRetCoin :: EraPParams era => Term era Coin
colRetCoin = Var $ V "colRetCoin" CoinR No

-- | The amount that the collateral must cover if there is a two phase error.
--   This is roughly the 'collateralPercentage' * 'txfee' . The calculation deals with rounding,
--   but you don't need those details to understand what is going on.
owed :: EraPParams era => Term era Coin
owed = Var $ V "owed" CoinR No

-- ==============================================================
-- Tx Vars

txbody :: Reflect era => Term era (TxBodyF era)
txbody = Var $ V "txbody" (TxBodyR reify) No

txwits :: Reflect era => Term era (TxWitsF era)
txwits = Var $ V "txwits" (TxWitsR reify) No

txauxdata :: Reflect era => Term era (Maybe (TxAuxDataF era))
txauxdata = Var $ V "txauxdata" (MaybeR (TxAuxDataR reify)) No

txisvalid :: EraPParams era => Term era IsValid
txisvalid = Var $ V "txisvalid" IsValidR No

valids :: EraPParams era => Term era [IsValid]
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

constitution :: EraPParams era => Term era (Constitution era)
constitution = Var $ V "constitution" ConstitutionR No

enactTreasury :: EraPParams era => Term era Coin
enactTreasury = Var $ V "enactTreasury" CoinR No

enactWithdrawals :: forall era. EraPParams era => Term era (Map (Credential 'Staking (EraCrypto era)) Coin)
enactWithdrawals = Var $ V "enactWithdrawals" (MapR CredR CoinR) No

currentGovActionStates :: EraPParams era => Term era (Map (GovActionId (EraCrypto era)) (GovActionState era))
currentGovActionStates = Var $ V "currentGovActionStates" (MapR GovActionIdR GovActionStateR) No

currentProposalOrder :: EraPParams era => Term era [GovActionId (EraCrypto era)]
currentProposalOrder = Var $ V "currentProposalOrder" (ListR GovActionIdR) No

prevGovActionStates :: EraPParams era => Term era (Map (GovActionId (EraCrypto era)) (GovActionState era))
prevGovActionStates = Var $ V "prevGovActionStates" (MapR GovActionIdR GovActionStateR) No

prevProposalOrder :: EraPParams era => Term era [GovActionId (EraCrypto era)]
prevProposalOrder = Var $ V "prevProposalOrder" (ListR GovActionIdR) No

previousCommitteeState ::
  EraPParams era =>
  Term
    era
    ( Map
        (Credential 'ColdCommitteeRole (EraCrypto era))
        (Maybe (Credential 'HotCommitteeRole (EraCrypto era)))
    )
previousCommitteeState = Var $ V "previousCommitteeState" (MapR CommColdCredR (MaybeR CommHotCredR)) No

commMembers :: EraPParams era => Term era (Map (Credential 'ColdCommitteeRole (EraCrypto era)) EpochNo)
commMembers = Var $ V "commMembers" (MapR CommColdCredR EpochR) No

commQuorum :: EraPParams era => Term era UnitInterval
commQuorum = Var $ V "commQuorum" UnitIntervalR No

committeeVar :: EraPParams era => Term era (Maybe (Committee era))
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
type UtxoPulse era = (Map (TxIn (EraCrypto era)) (TxOutF era), DRepPulser era Identity (RatifyState era))

-- | We also introduce an intermediate variable 'utxoPulse' which can constrain this value
--   by using the predicate [ utxoPulse p :<-: pulsingPair p ]
utxoPulse :: (RunConwayRatify era, Reflect era) => Proof era -> Term era (UtxoPulse era)
utxoPulse p = Var $ V "utxoPulse" (PairR (MapR TxInR (TxOutR p)) DRepPulserR) No

-- | an invertable RootTarget to compute a (UtxoPulse era)
pulsingPairT ::
  forall era.
  (RunConwayRatify era, Reflect era) =>
  Proof era ->
  RootTarget era (UtxoPulse era) (UtxoPulse era)
pulsingPairT proof =
  Invert "DRepPulser" (typeRep @(UtxoPulse era)) (\utx a b c d e f g -> (utx, initPulser proof utx a b c d e f g))
    :$ Lensed (utxo proof) _1
    :$ Virtual drepDelegation (ppString "prevDRepDelegations") (_2 . prevDRepDelegationsL)
    :$ Virtual poolDistr (ppString "prevPoolDistr") (_2 . prevPoolDistrL)
    :$ Virtual currentDRepState (ppString "prevDRepState") (_2 . prevDRepStateL)
    :$ Virtual currentEpoch (ppString "prevEpoch") (_2 . prevEpochL)
    :$ Virtual committeeState (ppString "prevCommitteeState") (_2 . prevCommitteeStateL)
    :$ Shift enactStateT (_2 . prevEnactStateL)
    :$ Virtual currGovActionStates (ppString "prevProposals") (_2 . prevProposalsL)

-- TODO access prevTreasury from the EnactState
--  :$ Virtual treasury (ppString "prevTreasury") (_2 . prevTreasuryL)

justPulser ::
  forall era.
  (Reflect era, RunConwayRatify era) =>
  Proof era ->
  RootTarget
    era
    (DRepPulser era Identity (RatifyState era))
    (DRepPulser era Identity (RatifyState era))
justPulser p =
  Invert "DRepPulser" (typeRep @(DRepPulser era Identity (RatifyState era))) (initPulser p Map.empty)
    :$ Virtual drepDelegation (ppString "prevDRepDelegations") (prevDRepDelegationsL)
    :$ Virtual poolDistr (ppString "prevPoolDistr") (prevPoolDistrL)
    :$ Virtual currentDRepState (ppString "prevDRepState") (prevDRepStateL)
    :$ Virtual currentEpoch (ppString "prevEpoch") (prevEpochL)
    :$ Virtual committeeState (ppString "prevCommitteeState") (prevCommitteeStateL)
    :$ Shift enactStateT (prevEnactStateL)
    :$ Virtual currGovActionStates (ppString "prevProposals") (prevProposalsL)

-- TODO access prevTreasury from the EnactState
-- :$ Virtual treasury (ppString "prevTreasury") (prevTreasuryL)

-- | Variable used to constrain the DRepPulser
drepPulser :: (RunConwayRatify era, Reflect era) => Term era (DRepPulser era Identity (RatifyState era))
drepPulser = Var $ V "drepPulser" DRepPulserR No

-- | Predicates that constrain the DRepPuser and all its 'prevXXX' snapshots
--   These ensure we generate state just passing the epoch boundary
prevPulsingPreds :: (RunConwayRatify era, Reflect era) => Proof era -> [Pred era]
prevPulsingPreds p =
  [ Sized (ExactSize 0) (Dom enactWithdrawals)
  , Lit CoinR (Coin 0) :=: enactTreasury
  , utxoPulse p :<-: pulsingPairT p
  , select drepPulser (utxoPulse p) _2
  , select partialDRepDistr drepPulser partialDRepDistrL
  , select prevDRepDelegations drepPulser prevDRepDelegationsL
  , select prevPoolDistr drepPulser prevPoolDistrL
  , select prevDRepState drepPulser prevDRepStateL
  , select prevEpoch drepPulser prevEpochL
  , select prevCommitteeState drepPulser prevCommitteeStateL
  , select prevEnactState drepPulser prevEnactStateL
  , select prevProposals drepPulser prevProposalsL
  -- TODO access prevTreasury from the EnactState
  -- , select prevTreasury drepPulser prevTreasuryL
  ]

-- | Target for assembling 'DRPulsing' form of (DRepPulsingState era)
--   from 'drepPulser' :: forall era. Term era (DRepPulser era Identity (RatifyState era))
pulsingPulsingStateT ::
  forall era.
  (RunConwayRatify era, Reflect era) =>
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
  Map (TxIn (EraCrypto era)) (TxOutF era) ->
  Map (Credential 'Staking (EraCrypto era)) (DRep (EraCrypto era)) ->
  Map (KeyHash 'StakePool (EraCrypto era)) (IndividualPoolStake (EraCrypto era)) ->
  Map (Credential 'DRepRole (EraCrypto era)) (DRepState (EraCrypto era)) ->
  EpochNo ->
  Map (Credential 'ColdCommitteeRole (EraCrypto era)) (Maybe (Credential 'HotCommitteeRole (EraCrypto era))) ->
  EnactState era ->
  [GovActionState era] ->
  -- Coin ->
  DRepPulser era Identity (RatifyState era)
initPulser proof utx credDRepMap poold credDRepStateMap epoch commstate enactstate proposalS {- treas -} =
  let stakeSize = Map.size stakeDistr
      k = securityParameter testGlobals
      pp :: PParams era
      pp = def & ppProtocolVersionL .~ (protocolVersion proof)
      IStake stakeDistr _ = updateStakeDistribution pp mempty mempty (utx ^. utxoFL proof)
   in DRepPulser
        (max 1 (ceiling (toInteger stakeSize % (8 * toInteger k))))
        (unify Map.empty Map.empty Map.empty credDRepMap)
        stakeDistr
        stakeDistr
        (PoolDistr poold)
        Map.empty
        credDRepStateMap
        epoch
        (CommitteeState commstate)
        enactstate
        (SS.fromList proposalS)
        -- treas
        testGlobals

proposalsT :: forall era. EraPParams era => RootTarget era (Proposals era) (Proposals era)
proposalsT =
  Invert "Proposals" (typeRep @(Proposals era)) id
    :$ Lensed prevProposals' (lens id $ const id)

-- govRuleStateT :: forall era. EraPParams era => RootTarget era (GovRuleState era) (GovRuleState era)
-- govRuleStateT =
--   Invert "GovRuleState" (typeRep @(GovRuleState era)) (flip GovRuleState def . fromGovActionStateSeq . SS.fromList)
--     :$ Lensed prevProposals (lens (F.toList . proposalsActions . grsProposals) $ const (flip GovRuleState def . fromGovActionStateSeq . SS.fromList))

-- ==================================================
-- Second form of DRepPulsingState 'DRComplete'
-- ==================================================

-- | The snapshot dedicated datatype (PulsingSnapshot era) stored inside 'DRComplete'
--   Note this is used in 'dRepPulsingStateT', the second  DRepPulsingState form.
pulsingSnapshotT :: forall era. EraPParams era => RootTarget era (PulsingSnapshot era) (PulsingSnapshot era)
pulsingSnapshotT =
  Invert "PulsingSnapshot" (typeRep @(PulsingSnapshot era)) (\x y -> PulsingSnapshot (SS.fromList x) (Map.map compactCoinOrError y))
    :$ Lensed prevProposals (psProposalsL . strictSeqListL)
    :$ Lensed partialDRepDistr (psDRepDistrL . mapCompactFormCoinL)
    :$ Lensed prevDRepState psDRepStateL

pulsingSnapshotL :: Lens' (DRepPulsingState era) (PulsingSnapshot era)
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

ratifyStateL :: Lens' (DRepPulsingState era) (RatifyState era)
ratifyStateL = lens getter setter
  where
    getter (DRComplete _ y) = y
    getter (x@(DRPulsing {})) = snd (finishDRepPulser x)
    setter (DRComplete x _) y = DRComplete x y
    setter (z@(DRPulsing {})) y = case finishDRepPulser z of
      (x, _) -> DRComplete x y

-- | Snapshot of 'currentProposals' from the start of the current Epoch
prevProposals :: EraPParams era => Term era [GovActionState era]
prevProposals = Var $ V "prevProposals" (ListR GovActionStateR) No

prevProposals' :: EraPParams era => Term era (Proposals era)
prevProposals' = Var $ V "prevProposals'" (ProposalsR) No

prevProposalsL :: Lens' (DRepPulser era Identity (RatifyState era)) [GovActionState era]
prevProposalsL =
  lens
    (\x -> F.toList (dpProposals x))
    (\x y -> x {dpProposals = SS.fromList y})

-- | Partially computed DRepDistr inside the pulser
partialDRepDistr :: EraPParams era => Term era (Map (DRep (EraCrypto era)) Coin)
partialDRepDistr = Var $ V "partialDRepDistr" (MapR DRepR CoinR) No

partialDRepDistrL :: Lens' (DRepPulser era Identity (RatifyState era)) (Map (DRep (EraCrypto era)) Coin)
partialDRepDistrL =
  lens
    (\x -> Map.map fromCompact (dpDRepDistr x))
    (\x y -> x {dpDRepDistr = Map.map compactCoinOrError y})

-- | Snapshot of 'dreps' from the start of the current epoch
prevDRepState :: EraPParams era => Term era (Map (Credential 'DRepRole (EraCrypto era)) (DRepState (EraCrypto era)))
prevDRepState = Var $ V "prevDRepState" (MapR VCredR DRepStateR) No

prevDRepStateL ::
  Lens'
    (DRepPulser era Identity (RatifyState era))
    (Map (Credential 'DRepRole (EraCrypto era)) (DRepState (EraCrypto era)))
prevDRepStateL = lens dpDRepState (\x y -> x {dpDRepState = y})

-- | Snapshot of 'committeeState' from the start of the current epoch
prevCommittee :: EraPParams era => Term era (Maybe (GovPurposeId 'CommitteePurpose era))
prevCommittee = Var $ V "prevCommittee" (MaybeR PrevCommitteeR) No

prevConstitution :: EraPParams era => Term era (Maybe (GovPurposeId 'ConstitutionPurpose era))
prevConstitution = Var $ V "prevConstitution" (MaybeR PrevConstitutionR) No

-- | snapshot of 'poolDistr' from the start of the current epoch
prevPoolDistr :: EraPParams era => Term era (Map (KeyHash 'StakePool (EraCrypto era)) (IndividualPoolStake (EraCrypto era)))
prevPoolDistr = Var $ V "prevPoolDistr" (MapR PoolHashR IPoolStakeR) No

prevPoolDistrL ::
  Lens'
    (DRepPulser era Identity (RatifyState era))
    (Map (KeyHash 'StakePool (EraCrypto era)) (IndividualPoolStake (EraCrypto era)))
prevPoolDistrL =
  lens
    (\x -> unPoolDistr (dpStakePoolDistr x))
    (\x y -> x {dpStakePoolDistr = PoolDistr y})

-- | Snapshot of the 'drepDelegation' from he start of the current epoch.
prevDRepDelegations :: EraPParams era => Term era (Map (Credential 'Staking (EraCrypto era)) (DRep (EraCrypto era)))
prevDRepDelegations = Var $ V "prevDRepDelegations" (MapR CredR DRepR) No

-- | Snapshot of 'drepDelegation' from the start of the current epoch
prevDRepDelegationsL ::
  Lens'
    (DRepPulser era Identity (RatifyState era))
    (Map (Credential 'Staking (EraCrypto era)) (DRep (EraCrypto era)))
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
prevCommitteeState :: EraPParams era => Term era (Map (Credential 'ColdCommitteeRole (EraCrypto era)) (Maybe (Credential 'HotCommitteeRole (EraCrypto era))))
prevCommitteeState = Var $ V "prevCommitteeState" (MapR CommColdCredR (MaybeR CommHotCredR)) No

prevCommitteeStateL ::
  Lens'
    (DRepPulser era Identity (RatifyState era))
    (Map (Credential 'ColdCommitteeRole (EraCrypto era)) (Maybe (Credential 'HotCommitteeRole (EraCrypto era))))
prevCommitteeStateL =
  lens
    (\x -> csCommitteeCreds (dpCommitteeState x))
    (\x y -> x {dpCommitteeState = CommitteeState y})

-- | Snapshot of the enactState built by 'enactStateT' assembled from data at the start the current epoch
prevEnactState :: Reflect era => Term era (EnactState era)
prevEnactState = Var $ V "prevEnactState" EnactStateR No

prevEnactStateL :: Lens' (DRepPulser era Identity (RatifyState era)) (EnactState era)
prevEnactStateL = lens dpEnactState (\x y -> x {dpEnactState = y})

-- | Snapshot of 'currentEpoch' just before the start of the current epoch. (currenEpoch - 1)
prevEpoch :: EraPParams era => Term era EpochNo
prevEpoch = Var $ V "prevEpoch" EpochR No

prevEpochL :: Lens' (DRepPulser era Identity (RatifyState era)) EpochNo
prevEpochL = lens dpCurrentEpoch (\x y -> x {dpCurrentEpoch = y})

prevTreasury :: EraPParams era => Term era Coin
prevTreasury = Var $ V "prevTreasury" CoinR No

{-
-- TODO access prevTreasury from the EnactState
prevTreasuryL :: Lens' (DRepPulser era Identity (RatifyState era)) Coin
prevTreasuryL = lens dpTreasury (\x y -> x {dpTreasury = y})
-}

-- ======================================
-- ConwayGovState

conwayGovStateT ::
  forall era.
  (RunConwayRatify era, Reflect era) =>
  Proof era ->
  RootTarget era (ConwayGovState era) (ConwayGovState era)
conwayGovStateT _ =
  Invert "ConwayGovState" (typeRep @(ConwayGovState era)) ConwayGovState
    :$ Lensed currProposals cgProposalsL
    :$ Shift enactStateT cgEnactStateL
    -- :$ Shift (completePulsingStateT p) cgDRepPulsingStateL
    :$ Shift pulsingPulsingStateT cgDRepPulsingStateL

-- | The sum of all the 'gasDeposit' fields of 'currProposals'
proposalDeposits :: EraPParams era => Term era Coin
proposalDeposits = Var (V "proposalDeposits" CoinR No)

-- | A view of 'currentDRepState' (sum of the drepDeposit field of in the range of 'currentDRepState')
drepDepositsView :: EraPParams era => Term era (Map (Credential 'DRepRole (EraCrypto era)) Coin)
drepDepositsView = Var (V "drepDepositsView" (MapR VCredR CoinR) No)

currProposals :: EraPParams era => Term era (Proposals era)
currProposals = Var $ V "currProposals" (ProposalsR) No

currGovActionStates :: EraPParams era => Term era [GovActionState era]
currGovActionStates = Var $ V "currGovActionStates" (ListR GovActionStateR) No

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
committeeT :: forall era. EraPParams era => RootTarget era (Committee era) (Committee era)
committeeT =
  Invert "Committee" (typeRep @(Committee era)) Committee
    :$ Lensed commMembers committeeMembersL
    :$ Lensed commQuorum committeeQuorumL

prevGovActionIdsT :: forall era. EraPParams era => RootTarget era (PrevGovActionIds era) (PrevGovActionIds era)
prevGovActionIdsT =
  Invert
    "prevGovActionIds"
    (typeRep @(PrevGovActionIds era))
    (\w x y z -> PrevGovActionIds $ PForest (maybeToStrictMaybe w) (maybeToStrictMaybe x) (maybeToStrictMaybe y) (maybeToStrictMaybe z))
    :$ Lensed prevPParamUpdate (prevGovActionIdsL . pfPParamUpdateL . strictMaybeToMaybeL)
    :$ Lensed prevHardFork (prevGovActionIdsL . pfHardForkL . strictMaybeToMaybeL)
    :$ Lensed prevCommittee (prevGovActionIdsL . pfCommitteeL . strictMaybeToMaybeL)
    :$ Lensed prevConstitution (prevGovActionIdsL . pfConstitutionL . strictMaybeToMaybeL)

prevPParamUpdate :: EraPParams era => Term era (Maybe (GovPurposeId 'PParamUpdatePurpose era))
prevPParamUpdate = Var $ V "prevPParamUpdate" (MaybeR PrevPParamUpdateR) No

prevHardFork :: EraPParams era => Term era (Maybe (GovPurposeId 'HardForkPurpose era))
prevHardFork = Var $ V "prevHardFork" (MaybeR PrevHardForkR) No

ppUpdateChildren :: EraPParams era => Term era (Set (GovActionId (EraCrypto era)))
ppUpdateChildren = Var $ V "ppUpdateChildren" (SetR GovActionIdR) No

hardForkChildren :: EraPParams era => Term era (Set (GovActionId (EraCrypto era)))
hardForkChildren = Var $ V "hardForkChildren" (SetR GovActionIdR) No

committeeChildren :: EraPParams era => Term era (Set (GovActionId (EraCrypto era)))
committeeChildren = Var $ V "committeeChildren" (SetR GovActionIdR) No

constitutionChildren :: EraPParams era => Term era (Set (GovActionId (EraCrypto era)))
constitutionChildren = Var $ V "constitutionChildren" (SetR GovActionIdR) No

-- ================
-- Lenses

pparamsFL :: Proof era -> Lens' (PParams era) (PParamsF era)
pparamsFL p = lens (PParamsF p) (\_ (PParamsF _ x) -> x)

committeeMembersL :: Lens' (Committee era) (Map (Credential 'ColdCommitteeRole (EraCrypto era)) EpochNo)
committeeMembersL = lens committeeMembers (\x s -> x {committeeMembers = s})

committeeQuorumL :: Lens' (Committee era) UnitInterval
committeeQuorumL = lens committeeQuorum (\x s -> x {committeeQuorum = s})

smCommL :: Lens' (StrictMaybe (Committee era)) (Committee era)
smCommL = lens getter (\_ t -> SJust t)
  where
    getter SNothing = Committee Map.empty maxBound
    getter (SJust x) = x

shellyGovStateProposedPPUpdatesL :: Lens' (ShelleyGovState era) (ProposedPPUpdates era)
shellyGovStateProposedPPUpdatesL = lens proposals (\x y -> x {proposals = y})

futureproposalsL :: Lens' (ShelleyGovState era) (ProposedPPUpdates era)
futureproposalsL = lens futureProposals (\x y -> x {futureProposals = y})

proposedMapL :: Proof era -> Lens' (ProposedPPUpdates era) (Map (KeyHash 'Genesis (EraCrypto era)) (PParamsUpdateF era))
proposedMapL p =
  lens
    (\(ProposedPPUpdates x) -> Map.map (PParamsUpdateF p) x)
    (\(ProposedPPUpdates _) y -> ProposedPPUpdates (Map.map unPParamsUpdate y))

gaiToPgac :: Set (GovActionId (EraCrypto era)) -> Set (GovPurposeId p era)
gaiToPgac = Set.map GovPurposeId

pgacToGai :: Set (GovPurposeId p era) -> Set (GovActionId (EraCrypto era))
pgacToGai = Set.map unGovPurposeId

pair1 :: EraPParams era => Rep era a -> Term era a
pair1 rep = Var (V "pair1" rep No)

pair2 :: EraPParams era => Rep era b -> Term era b
pair2 rep = Var (V "pair2" rep No)

pairT :: forall era a b. (Typeable a, Typeable b, EraPParams era) => Rep era a -> Rep era b -> RootTarget era (a, b) (a, b)
pairT repa repb =
  Invert "(,)" (typeRep @(a, b)) (,)
    :$ Lensed (pair1 repa) fstL
    :$ Lensed (pair2 repb) sndL

-- ==========================================
-- Targets for GovActionState
-- The variables xxV align with the field selectors gasXx

idV :: EraPParams era => Term era (GovActionId (EraCrypto era))
idV = Var (V "idV" GovActionIdR No)

committeeVotesV :: EraPParams era => Term era (Map (Credential 'HotCommitteeRole (EraCrypto era)) Vote)
committeeVotesV = Var (V "committeeVotesV" (MapR CommHotCredR VoteR) No)

drepVotesV :: EraPParams era => Term era (Map (Credential 'DRepRole (EraCrypto era)) Vote)
drepVotesV = Var (V "drepVotesV" (MapR VCredR VoteR) No)

stakePoolVotesV :: EraPParams era => Term era (Map (KeyHash 'StakePool (EraCrypto era)) Vote)
stakePoolVotesV = Var (V "stakePoolVotesV" (MapR PoolHashR VoteR) No)

depositV :: EraPParams era => Term era Coin
depositV = Var (V "depositV" CoinR No)

returnAddrV :: EraPParams era => Term era (RewardAcnt (EraCrypto era))
returnAddrV = Var (V "returnAddrV" RewardAcntR No)

actionV :: EraPParams era => Term era (GovAction era)
actionV = Var (V "actionV" GovActionR No)

proposedInV :: EraPParams era => Term era EpochNo
proposedInV = Var (V "proposedInV" EpochR No)

expiresAfterV :: EraPParams era => Term era EpochNo
expiresAfterV = Var (V "expiresAfterV" EpochR No)

childrenV :: EraPParams era => Term era (Set (GovActionId (EraCrypto era)))
childrenV = Var (V "childrenV" (SetR GovActionIdR) No)

govActionStateTarget :: forall era. EraPParams era => RootTarget era (GovActionState era) (GovActionState era)
govActionStateTarget =
  Invert "GovActionState" (typeRep @(GovActionState era)) GovActionState
    :$ Lensed idV gasIdL
    :$ Lensed committeeVotesV gasCommitteeVotesL
    :$ Lensed drepVotesV gasDRepVotesL
    :$ Lensed stakePoolVotesV gasStakePoolVotesL
    :$ Lensed depositV gasDepositL
    :$ Lensed returnAddrV gasReturnAddrL
    :$ Lensed actionV gasActionL
    :$ Lensed proposedInV gasProposedInL
    :$ Lensed expiresAfterV gasExpiresAfterL

govstates :: EraPParams era => Term era [GovActionState era]
govstates = Var (V "govstates" (ListR GovActionStateR) No)

-- ==============================================================
-- Targets for GovAction, The model does not make the distinction
-- the newtype (PrevGovActionId era) and (GovActionId era), The
-- targets provide the coercions to produce the real data from the Model

-- | Lift the Model to the real type
liftId :: Maybe (GovActionId (EraCrypto era)) -> StrictMaybe (GovPurposeId p era)
liftId x = GovPurposeId <$> (maybeToStrictMaybe x)

-- | Drop the real type back to the Model
dropId :: StrictMaybe (GovPurposeId p era) -> Maybe (GovActionId (EraCrypto era))
dropId x = unGovPurposeId <$> (strictMaybeToMaybe x)

-- =====================
-- Variables for the fields of GovAction

gaPrevId :: EraPParams era => Term era (Maybe (GovActionId (EraCrypto era)))
gaPrevId = Var (V "gaPrevId" (MaybeR GovActionIdR) No)

gaPParamsUpdate :: Reflect era => Term era (PParamsUpdateF era)
gaPParamsUpdate = Var (V "gsPParamsUpdate" (PParamsUpdateR reify) No)

gaProtVer :: Reflect era => Term era ProtVer
gaProtVer = Var (V "gaProtVer" (ProtVerR reify) No)

gaRewardAcnt :: EraPParams era => Term era (Map (RewardAcnt (EraCrypto era)) Coin)
gaRewardAcnt = Var (V "gaRewardAcnt" (MapR RewardAcntR CoinR) No)

gaRemMember :: EraPParams era => Term era (Set (Credential 'ColdCommitteeRole (EraCrypto era)))
gaRemMember = Var (V "gaRemMember" (SetR CommColdCredR) No)

gaAddMember :: EraPParams era => Term era (Map (Credential 'ColdCommitteeRole (EraCrypto era)) EpochNo)
gaAddMember = Var (V "gaAddMember" (MapR CommColdCredR EpochR) No)

gaThreshold :: EraPParams era => Term era UnitInterval
gaThreshold = Var (V "gaThreshold" UnitIntervalR No)

gaConstitution :: EraPParams era => Term era (Constitution era)
gaConstitution = Var (V "gaConstitution" ConstitutionR No)

-- ===================================
-- The partial Targets, one for each constructor of GovAction

parameterChangeT :: forall era. Reflect era => RootTarget era (GovAction era) (GovAction era)
parameterChangeT =
  Invert "ParameterChange" (typeRep @(GovAction era)) (\x y -> ParameterChange (liftId x) (unPParamsUpdate y))
    :$ Partial gaPrevId (\case (ParameterChange x _) -> Just $ dropId x; _ -> Nothing)
    :$ Partial gaPParamsUpdate (\case (ParameterChange _ y) -> Just $ PParamsUpdateF reify y; _ -> Nothing)

hardForkInitiationT :: forall era. Reflect era => RootTarget era (GovAction era) (GovAction era)
hardForkInitiationT =
  Invert "HardForkInitiation" (typeRep @(GovAction era)) (\x y -> HardForkInitiation (liftId x) y)
    :$ Partial gaPrevId (\case (HardForkInitiation x _) -> Just $ dropId x; _ -> Nothing)
    :$ Partial gaProtVer (\case (HardForkInitiation _ y) -> Just y; _ -> Nothing)

treasuryWithdrawalsT :: forall era. Reflect era => RootTarget era (GovAction era) (GovAction era)
treasuryWithdrawalsT =
  Invert "TreasuryWithdrawals" (typeRep @(GovAction era)) TreasuryWithdrawals
    :$ Partial gaRewardAcnt (\case (TreasuryWithdrawals x) -> Just x; _ -> Nothing)

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
    :$ Partial gaConstitution (\case (NewConstitution _ y) -> Just y; _ -> Nothing)

infoActionT :: forall era. Reflect era => RootTarget era (GovAction era) (GovAction era)
infoActionT =
  Invert "InfoAction" (typeRep @(GovAction era)) (\() -> InfoAction)
    :$ Lensed (Lit UnitR ()) (to (const ()))
