{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Test.Cardano.Ledger.Constrained.Vars where

import Cardano.Ledger.BaseTypes (BlocksMade (..), EpochNo, ProtVer (..), StrictMaybe (..))
import Cardano.Ledger.CertState (CertState (..), DState (..), FutureGenDeleg (..), PState (..), VState (..))
import qualified Cardano.Ledger.CertState as DPS (InstantaneousRewards (..))
import Cardano.Ledger.Coin (Coin (..), DeltaCoin)
import Cardano.Ledger.Core (
  EraPParams,
  PParams,
  ppMaxBBSizeL,
  ppMaxBHSizeL,
  ppMaxTxSizeL,
  ppMinFeeAL,
  ppMinFeeBL,
  ppProtocolVersionL,
 )
import Cardano.Ledger.Credential (Credential, Ptr)
import Cardano.Ledger.EpochBoundary (SnapShot (..), SnapShots (..), Stake (..))
import Cardano.Ledger.Era (Era (EraCrypto))
import Cardano.Ledger.Keys (GenDelegPair, GenDelegs (..), KeyHash, KeyRole (..))
import Cardano.Ledger.PoolDistr (IndividualPoolStake (..), PoolDistr (..))
import Cardano.Ledger.PoolParams (PoolParams)
import Cardano.Ledger.Shelley.LedgerState (
  AccountState (..),
  EpochState (..),
  LedgerState (..),
  NewEpochState (..),
  ShelleyPPUPState (..),
  UTxOState (..),
  smartUTxOState,
 )
import Cardano.Ledger.Shelley.PParams (ProposedPPUpdates (..))
import Cardano.Ledger.Shelley.PoolRank (NonMyopic (..))
import Cardano.Ledger.Shelley.RewardUpdate (PulsingRewUpdate (Complete))
import qualified Cardano.Ledger.Shelley.RewardUpdate as RU
import Cardano.Ledger.Shelley.Rewards (Reward (..))
import Cardano.Ledger.TxIn (TxIn)
import Cardano.Ledger.UMap (compactCoinOrError, fromCompact)
import Cardano.Ledger.UTxO (UTxO (..))
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Set (Set)
import qualified Data.VMap as VMap
import Lens.Micro
import Numeric.Natural (Natural)
import Test.Cardano.Ledger.Babbage.Serialisation.Generators ()
import Test.Cardano.Ledger.Constrained.Ast (Target (..), Term (Var), constTarget, ppTarget, (^$))
import Test.Cardano.Ledger.Constrained.Classes (
  GovernanceState (..),
  PParamsF (..),
  PParamsUpdateF (..),
  TxOutF (..),
  governanceProposedL,
  liftUTxO,
  pparamsWrapperL,
  unPParams,
  unPParamsUpdate,
 )
import Test.Cardano.Ledger.Constrained.Env (Access (..), AnyF (..), Field (..), Name (..), V (..))
import Test.Cardano.Ledger.Constrained.Lenses
import Test.Cardano.Ledger.Constrained.TypeRep (Rep (..), testEql, (:~:) (Refl))
import Test.Cardano.Ledger.Generic.Proof (Evidence (..), Proof (..))

import Cardano.Ledger.Conway.Governance (ConwayGovState (..))
import Cardano.Ledger.Shelley.Governance (ShelleyPPUPState (..))
import qualified Cardano.Ledger.Shelley.Governance as Core (GovernanceState (..))
import qualified Cardano.Ledger.Shelley.PParams as Core (ProposedPPUpdates (..))

-- ================================================================

-- | Used in Component constraints to turn a Var Term into a component (AnyF era s)
-- E.g.  (Component foo [ field fooRep fooPart1, field fooRep fooPart2])
-- Where fooPart1 :: Term era a, and fooPart2 :: Term era b
-- And fooPart1 has an (Access foo a)
-- And fooPart2 has an (Access foo b)
field :: (Eq t, Show t) => Rep era s -> Term era t -> AnyF era s
field repS1 (Var (V name rept access@(Yes repS2 _))) = case testEql repS1 repS2 of
  Just Refl -> AnyF (Field name rept access)
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

epochNo :: Term era EpochNo
epochNo = Var $ V "epochNo" EpochR (Yes NewEpochStateR nesELL)

prevBlocksMade :: Term era (Map (KeyHash 'StakePool (EraCrypto era)) Natural)
prevBlocksMade = Var $ V "prevBlocksMade" (MapR PoolHashR NaturalR) (Yes NewEpochStateR nesBprevL)

currBlocksMade :: Term era (Map (KeyHash 'StakePool (EraCrypto era)) Natural)
currBlocksMade = Var $ V "currBlocksMade" (MapR PoolHashR NaturalR) (Yes NewEpochStateR nesBcurL)

poolDistr :: Term era (Map (KeyHash 'StakePool (EraCrypto era)) (IndividualPoolStake (EraCrypto era)))
poolDistr = Var $ V "poolDistr" (MapR PoolHashR IPoolStakeR) (Yes NewEpochStateR poolDistrL)

-- | For tests only, Like PoolDistr but has a Rational (rather than a IndividualPoolStake).
mockPoolDistr :: Term era (Map (KeyHash 'StakePool (EraCrypto era)) Rational)
mockPoolDistr = Var $ V "mockPoolDistr" (MapR PoolHashR RationalR) No

poolDistrL :: NELens era (Map (KeyHash 'StakePool (EraCrypto era)) (IndividualPoolStake (EraCrypto era)))
poolDistrL = nesPdL . unPoolDistrL

unPoolDistrL :: Lens' (PoolDistr c) (Map (KeyHash 'StakePool c) (IndividualPoolStake c))
unPoolDistrL = lens unPoolDistr (\(PoolDistr _) x -> PoolDistr x)

-- CertState - DState

rewards :: Term era (Map (Credential 'Staking (EraCrypto era)) Coin)
rewards = Var $ V "rewards" (MapR CredR CoinR) (Yes NewEpochStateR rewardsL)

rewardsL :: NELens era (Map (Credential 'Staking (EraCrypto era)) Coin)
rewardsL = nesEsL . esLStateL . lsCertStateL . certDStateL . dsUnifiedL . rewardsUMapL

delegations :: Term era (Map (Credential 'Staking (EraCrypto era)) (KeyHash 'StakePool (EraCrypto era)))
delegations = Var $ V "delegations" (MapR CredR PoolHashR) (Yes NewEpochStateR delegationsL)

delegationsL :: NELens era (Map (Credential 'Staking (EraCrypto era)) (KeyHash 'StakePool (EraCrypto era)))
delegationsL = nesEsL . esLStateL . lsCertStateL . certDStateL . dsUnifiedL . delegationsUMapL

stakeDeposits :: Term era (Map (Credential 'Staking (EraCrypto era)) Coin)
stakeDeposits = Var $ V "stakeDeposits" (MapR CredR CoinR) (Yes NewEpochStateR stakeDepositsL)

stakeDepositsL :: NELens era (Map (Credential 'Staking (EraCrypto era)) Coin)
stakeDepositsL = nesEsL . esLStateL . lsCertStateL . certDStateL . dsUnifiedL . stakeDepositsUMapL

ptrs :: Term era (Map Ptr (Credential 'Staking (EraCrypto era)))
ptrs = Var $ V "ptrs" (MapR PtrR CredR) (Yes NewEpochStateR ptrsL)

ptrsL :: NELens era (Map Ptr (Credential 'Staking (EraCrypto era)))
ptrsL = nesEsL . esLStateL . lsCertStateL . certDStateL . dsUnifiedL . ptrsUMapL

futureGenDelegs :: Term era (Map (FutureGenDeleg (EraCrypto era)) (GenDelegPair (EraCrypto era)))
futureGenDelegs =
  Var $
    V
      "futureGenDelegs"
      (MapR FutureGenDelegR GenDelegPairR)
      (Yes NewEpochStateR futureGenDelegsL)

futureGenDelegsL :: NELens era (Map (FutureGenDeleg (EraCrypto era)) (GenDelegPair (EraCrypto era)))
futureGenDelegsL = nesEsL . esLStateL . lsCertStateL . certDStateL . dsFutureGenDelegsL

genDelegs :: Term era (Map (KeyHash 'Genesis (EraCrypto era)) (GenDelegPair (EraCrypto era)))
genDelegs = Var $ V "genDelegs" (MapR GenHashR GenDelegPairR) (Yes NewEpochStateR genDelegsL)

genDelegsL :: NELens era (Map (KeyHash 'Genesis (EraCrypto era)) (GenDelegPair (EraCrypto era)))
genDelegsL = nesEsL . esLStateL . lsCertStateL . certDStateL . dsGenDelegsL . unGenDelegsL

-- DState - InstantaneousRewards

instanReserves :: Term era (Map (Credential 'Staking (EraCrypto era)) Coin)
instanReserves = Var $ V "instanReserves" (MapR CredR CoinR) (Yes NewEpochStateR instanReservesL)

instanReservesL :: NELens era (Map (Credential 'Staking (EraCrypto era)) Coin)
instanReservesL = nesEsL . esLStateL . lsCertStateL . certDStateL . dsIRewardsL . iRReservesL

instanTreasury :: Term era (Map (Credential 'Staking (EraCrypto era)) Coin)
instanTreasury = Var $ V "instanTreasury" (MapR CredR CoinR) (Yes NewEpochStateR instanTreasuryL)

instanTreasuryL :: NELens era (Map (Credential 'Staking (EraCrypto era)) Coin)
instanTreasuryL = nesEsL . esLStateL . lsCertStateL . certDStateL . dsIRewardsL . iRTreasuryL

deltaReserves :: Term era DeltaCoin
deltaReserves = Var $ V "deltaReserves" DeltaCoinR (Yes NewEpochStateR deltaReservesNEL)

deltaReservesNEL :: NELens era DeltaCoin
deltaReservesNEL = nesEsL . esLStateL . lsCertStateL . certDStateL . dsIRewardsL . deltaReservesL

deltaTreasury :: Term era DeltaCoin
deltaTreasury = Var $ V "deltaTreasury" DeltaCoinR (Yes NewEpochStateR deltaTreasuryNEL)

deltaTreasuryNEL :: NELens era DeltaCoin
deltaTreasuryNEL = nesEsL . esLStateL . lsCertStateL . certDStateL . dsIRewardsL . deltaTreasuryL

-- CertState - PState

regPools :: Term era (Map (KeyHash 'StakePool (EraCrypto era)) (PoolParams (EraCrypto era)))
regPools = Var $ V "regPools" (MapR PoolHashR PoolParamsR) (Yes NewEpochStateR regPoolsL)

regPoolsL :: NELens era (Map (KeyHash 'StakePool (EraCrypto era)) (PoolParams (EraCrypto era)))
regPoolsL = nesEsL . esLStateL . lsCertStateL . certPStateL . psStakePoolParamsL

futureRegPools :: Term era (Map (KeyHash 'StakePool (EraCrypto era)) (PoolParams (EraCrypto era)))
futureRegPools = Var $ V "futureRegPools" (MapR PoolHashR PoolParamsR) (Yes NewEpochStateR futureRegPoolsL)

futureRegPoolsL :: NELens era (Map (KeyHash 'StakePool (EraCrypto era)) (PoolParams (EraCrypto era)))
futureRegPoolsL = nesEsL . esLStateL . lsCertStateL . certPStateL . psFutureStakePoolParamsL

retiring :: Term era (Map (KeyHash 'StakePool (EraCrypto era)) EpochNo)
retiring = Var $ V "retiring" (MapR PoolHashR EpochR) (Yes NewEpochStateR retiringL)

retiringL :: NELens era (Map (KeyHash 'StakePool (EraCrypto era)) EpochNo)
retiringL = nesEsL . esLStateL . lsCertStateL . certPStateL . psRetiringL

poolDeposits :: Term era (Map (KeyHash 'StakePool (EraCrypto era)) Coin)
poolDeposits = Var $ V "poolDeposits" (MapR PoolHashR CoinR) (Yes NewEpochStateR poolDepositsL)

poolDepositsL :: NELens era (Map (KeyHash 'StakePool (EraCrypto era)) Coin)
poolDepositsL = nesEsL . esLStateL . lsCertStateL . certPStateL . psDepositsL

dreps :: Term era (Set (Credential 'Voting (EraCrypto era)))
dreps = Var $ V "dreps" (SetR VCredR) (Yes NewEpochStateR drepsL)

drepsL :: NELens era (Set (Credential 'Voting (EraCrypto era)))
drepsL = nesEsL . esLStateL . lsCertStateL . certVStateL . vsDRepsL

ccHotKeys :: Term era (Map (KeyHash 'CommitteeColdKey (EraCrypto era)) (Maybe (Credential 'CommitteeHotKey (EraCrypto era))))
ccHotKeys = Var $ V "dreps" (MapR CommColdHashR (MaybeR CommHotHashR)) (Yes NewEpochStateR ccHotKeysL)

ccHotKeysL :: NELens era (Map (KeyHash 'CommitteeColdKey (EraCrypto era)) (Maybe (Credential 'CommitteeHotKey (EraCrypto era))))
ccHotKeysL = nesEsL . esLStateL . lsCertStateL . certVStateL . vsCommitteeHotKeysL

-- UTxOState

utxo :: Proof era -> Term era (Map (TxIn (EraCrypto era)) (TxOutF era))
utxo p = Var $ V "utxo" (MapR TxInR (TxOutR p)) (Yes NewEpochStateR (utxoL p))

utxoL :: Proof era -> NELens era (Map (TxIn (EraCrypto era)) (TxOutF era))
utxoL proof = nesEsL . esLStateL . lsUTxOStateL . utxosUtxoL . unUtxoL proof

unUtxoL :: Proof era -> Lens' (UTxO era) (Map (TxIn (EraCrypto era)) (TxOutF era))
unUtxoL p = lens (Map.map (TxOutF p) . unUTxO) (\(UTxO _) new -> (liftUTxO new))

deposits :: Term era Coin
deposits = Var $ V "deposits" CoinR (Yes NewEpochStateR depositsL)

depositsL :: NELens era Coin
depositsL = nesEsL . esLStateL . lsUTxOStateL . utxosDepositedL

fees :: Term era Coin
fees = Var $ V "fees" CoinR (Yes NewEpochStateR feesL)

feesL :: NELens era Coin
feesL = nesEsL . esLStateL . lsUTxOStateL . utxosFeesL

ppup :: Proof era -> Term era (ShelleyPPUPState era)
ppup p = Var $ (V "ppup" (PPUPStateR p) (Yes NewEpochStateR (ppupsL p)))

ppupsL :: Proof era -> NELens era (ShelleyPPUPState era)
ppupsL (Shelley _) = nesEsL . esLStateL . lsUTxOStateL . utxosGovernanceL
ppupsL (Allegra _) = nesEsL . esLStateL . lsUTxOStateL . utxosGovernanceL
ppupsL (Mary _) = nesEsL . esLStateL . lsUTxOStateL . utxosGovernanceL
ppupsL (Alonzo _) = nesEsL . esLStateL . lsUTxOStateL . utxosGovernanceL
ppupsL (Babbage _) = nesEsL . esLStateL . lsUTxOStateL . utxosGovernanceL
ppupsL (Conway _) = error "Conway era does not have a PPUPState, in ppupsL"

-- nesEsL . esLStateL . lsUTxOStateL . utxosGovernanceL . ???

proposalsT :: Proof era -> Term era (Map (KeyHash 'Genesis (EraCrypto era)) (PParamsUpdateF era))
proposalsT p = Var (V "proposals" (MapR GenHashR (PParamsUpdateR p)) No)

futureProposalsT ::
  Proof era -> Term era (Map (KeyHash 'Genesis (EraCrypto era)) (PParamsUpdateF era))
futureProposalsT p = Var (V "futureProposals" (MapR GenHashR (PParamsUpdateR p)) No)

ppupStateT :: Proof era -> Target era (ShelleyPPUPState era)
ppupStateT p = Constr "PPUPState" ppupfun ^$ proposalsT p ^$ futureProposalsT p
  where
    ppupfun x y =
      ShelleyPPUPState
        (ProposedPPUpdates (Map.map unPParamsUpdate x))
        (ProposedPPUpdates (Map.map unPParamsUpdate y))

governanceStateT :: forall era. Proof era -> Target era (GovernanceState era)
governanceStateT p@(Shelley _) = Constr "GovernanceState" (GovernanceState p) :$ (ppupStateT p)
governanceStateT p@(Allegra _) = Constr "GovernanceState" (GovernanceState p) :$ (ppupStateT p)
governanceStateT p@(Mary _) = Constr "GovernanceState" (GovernanceState p) :$ (ppupStateT p)
governanceStateT p@(Alonzo _) = Constr "GovernanceState" (GovernanceState p) :$ (ppupStateT p)
governanceStateT p@(Babbage _) = Constr "GovernanceState" (GovernanceState p) :$ (ppupStateT p)
governanceStateT p@(Conway _) =
  Constr "GovernanceState" (GovernanceState p)
    :$ constTarget (Core.emptyGovernanceState @era)
individualPoolStakeL :: Lens' (IndividualPoolStake c) Rational
individualPoolStakeL = lens individualPoolStake (\ds u -> ds {individualPoolStake = u})

-- Incremental Stake

isPtrMapT :: Term era (Map Ptr Coin)
isPtrMapT = Var $ V "ptrMap" (MapR PtrR CoinR) (Yes NewEpochStateR ptrMapL)

ptrMapL :: Lens' (NewEpochState era) (Map Ptr Coin)
ptrMapL = nesEsL . esLStateL . lsUTxOStateL . utxosStakeDistrL . isPtrMapL

isCredMapT :: Term era (Map (Credential 'Staking (EraCrypto era)) Coin)
isCredMapT = Var $ V "credMap" (MapR CredR CoinR) (Yes NewEpochStateR credMapL)

credMapL :: Lens' (NewEpochState era) (Map (Credential 'Staking (EraCrypto era)) Coin)
credMapL = nesEsL . esLStateL . lsUTxOStateL . utxosStakeDistrL . isCredMapL

-- AccountState

treasury :: Term era Coin
treasury = Var $ V "treasury" CoinR (Yes NewEpochStateR treasuryL)

treasuryL :: NELens era Coin
treasuryL = nesEsL . esAccountStateL . asTreasuryL

reserves :: Term era Coin
reserves = Var $ V "reserves" CoinR (Yes NewEpochStateR reservesL)

reservesL :: NELens era Coin
reservesL = nesEsL . esAccountStateL . asReservesL

-- EpochState

snapshots :: Term era (SnapShots (EraCrypto era))
snapshots = Var (V "snapshots" SnapShotsR (Yes NewEpochStateR snapshotsL))

snapshotsL :: NELens era (SnapShots (EraCrypto era))
snapshotsL = nesEsL . esSnapshotsL

-- | Lens' from the Core PParams to the Model PParamsF which embeds a (Proof era)
ppFL :: Proof era -> Lens' (PParams era) (PParamsF era)
ppFL p = lens (\pp -> PParamsF p pp) (\_ (PParamsF _ qq) -> qq)

prevpparams :: Proof era -> Term era (PParamsF era)
prevpparams p = Var (V "prevpparams" (PParamsR p) (Yes NewEpochStateR (nesEsL . esPrevPpL . ppFL p)))

pparams :: Proof era -> Term era (PParamsF era)
pparams p = Var (V "pparams" (PParamsR p) (Yes NewEpochStateR (nesEsL . esPpL . ppFL p)))

nmLikelihoodsT :: Term era (Map (KeyHash 'StakePool (EraCrypto era)) [Float])
nmLikelihoodsT = Var (V "likelihoodsNM" (MapR PoolHashR (ListR FloatR)) (Yes NewEpochStateR (nesEsL . esNonMyopicL . nmLikelihoodsL)))

nmRewardPotT :: Term era Coin
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

markStake :: Term era (Map (Credential 'Staking (EraCrypto era)) Coin)
markStake = Var (V "markStake" (MapR CredR CoinR) (Yes NewEpochStateR markStakeL))

markDelegs :: Term era (Map (Credential 'Staking (EraCrypto era)) (KeyHash 'StakePool (EraCrypto era)))
markDelegs = Var (V "markDelegs" (MapR CredR PoolHashR) (Yes NewEpochStateR markDelegsL))

markDelegsL :: NELens era (Map (Credential 'Staking (EraCrypto era)) (KeyHash 'StakePool (EraCrypto era)))
markDelegsL = nesEsL . esSnapshotsL . ssStakeMarkL . ssDelegationsL . vmapL

markPools :: Term era (Map (KeyHash 'StakePool (EraCrypto era)) (PoolParams (EraCrypto era)))
markPools = Var (V "markPools" (MapR PoolHashR PoolParamsR) (Yes NewEpochStateR markPoolsL))

markPoolsL :: NELens era (Map (KeyHash 'StakePool (EraCrypto era)) (PoolParams (EraCrypto era)))
markPoolsL = nesEsL . esSnapshotsL . ssStakeMarkL . ssPoolParamsL . vmapL

markSnapShotT :: Target era (SnapShot (EraCrypto era))
markSnapShotT = Constr "SnapShot" snapfun ^$ markStake ^$ markDelegs ^$ markPools
  where
    snapfun x y z =
      SnapShot
        (Stake (VMap.fromMap (Map.map compactCoinOrError x)))
        (VMap.fromMap y)
        (VMap.fromMap z)

setStake :: Term era (Map (Credential 'Staking (EraCrypto era)) Coin)
setStake = Var (V "setStake" (MapR CredR CoinR) (Yes NewEpochStateR setStakeL))

setStakeL :: NELens era (Map (Credential 'Staking (EraCrypto era)) Coin)
setStakeL = nesEsL . esSnapshotsL . ssStakeSetL . ssStakeL . stakeL

setDelegs :: Term era (Map (Credential 'Staking (EraCrypto era)) (KeyHash 'StakePool (EraCrypto era)))
setDelegs = Var (V "setDelegs" (MapR CredR PoolHashR) (Yes NewEpochStateR setDelegsL))

setDelegsL :: NELens era (Map (Credential 'Staking (EraCrypto era)) (KeyHash 'StakePool (EraCrypto era)))
setDelegsL = nesEsL . esSnapshotsL . ssStakeSetL . ssDelegationsL . vmapL

setPools :: Term era (Map (KeyHash 'StakePool (EraCrypto era)) (PoolParams (EraCrypto era)))
setPools = Var (V "setPools" (MapR PoolHashR PoolParamsR) (Yes NewEpochStateR setPoolsL))

setPoolsL :: NELens era (Map (KeyHash 'StakePool (EraCrypto era)) (PoolParams (EraCrypto era)))
setPoolsL = nesEsL . esSnapshotsL . ssStakeSetL . ssPoolParamsL . vmapL

setSnapShotT :: Target era (SnapShot (EraCrypto era))
setSnapShotT = Constr "SnapShot" snapfun ^$ setStake ^$ setDelegs ^$ setPools
  where
    snapfun x y z =
      SnapShot
        (Stake (VMap.fromMap (Map.map compactCoinOrError x)))
        (VMap.fromMap y)
        (VMap.fromMap z)

goStake :: Term era (Map (Credential 'Staking (EraCrypto era)) Coin)
goStake = Var (V "goStake" (MapR CredR CoinR) (Yes NewEpochStateR goStakeL))

goStakeL :: NELens era (Map (Credential 'Staking (EraCrypto era)) Coin)
goStakeL = nesEsL . esSnapshotsL . ssStakeGoL . ssStakeL . stakeL

goDelegs :: Term era (Map (Credential 'Staking (EraCrypto era)) (KeyHash 'StakePool (EraCrypto era)))
goDelegs = Var (V "goDelegs" (MapR CredR PoolHashR) (Yes NewEpochStateR goDelegsL))

goDelegsL :: NELens era (Map (Credential 'Staking (EraCrypto era)) (KeyHash 'StakePool (EraCrypto era)))
goDelegsL = nesEsL . esSnapshotsL . ssStakeGoL . ssDelegationsL . vmapL

goPools :: Term era (Map (KeyHash 'StakePool (EraCrypto era)) (PoolParams (EraCrypto era)))
goPools = Var (V "goPools" (MapR PoolHashR PoolParamsR) (Yes NewEpochStateR goPoolsL))

goPoolsL :: NELens era (Map (KeyHash 'StakePool (EraCrypto era)) (PoolParams (EraCrypto era)))
goPoolsL = nesEsL . esSnapshotsL . ssStakeGoL . ssPoolParamsL . vmapL

goSnapShotT :: Target era (SnapShot (EraCrypto era))
goSnapShotT = Constr "SnapShot" snapfun ^$ goStake ^$ goDelegs ^$ goPools
  where
    snapfun x y z =
      SnapShot
        (Stake (VMap.fromMap (Map.map compactCoinOrError x)))
        (VMap.fromMap y)
        (VMap.fromMap z)

markPoolDistr :: Term era (Map (KeyHash 'StakePool (EraCrypto era)) (IndividualPoolStake (EraCrypto era)))
markPoolDistr = Var (V "markPoolDistr" (MapR PoolHashR IPoolStakeR) No)

markPoolDistrL :: NELens era (Map (KeyHash 'StakePool (EraCrypto era)) (IndividualPoolStake (EraCrypto era)))
markPoolDistrL = nesEsL . esSnapshotsL . ssStakeMarkPoolDistrL . pooldistrHelpL

pooldistrHelpL :: Lens' (PoolDistr c) (Map (KeyHash 'StakePool c) (IndividualPoolStake c))
pooldistrHelpL = lens unPoolDistr (\_ u -> PoolDistr u)

snapShotFee :: Term era Coin
snapShotFee = Var (V "snapShotFee" CoinR No)

snapShotsT :: Target era (SnapShots (EraCrypto era))
snapShotsT =
  Constr "SnapShots" shotsfun
    :$ markSnapShotT
    :$ (Simple markPoolDistr)
    :$ setSnapShotT
    :$ goSnapShotT
    :$ (Simple snapShotFee)
  where
    shotsfun w x y z f = SnapShots w (PoolDistr x) y z f

-- ==================================================================
-- RewardUpdate

deltaT :: Term era (Maybe DeltaCoin)
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

deltaR :: Term era (Maybe DeltaCoin)
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

deltaF :: Term era (Maybe DeltaCoin)
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

rewardSet :: Term era (Map (Credential 'Staking (EraCrypto era)) (Set (Reward (EraCrypto era))))
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

totalAda :: Term era Coin
totalAda = Var $ V "totalAda" CoinR No

utxoCoin :: Term era Coin
utxoCoin = Var $ V "utxoCoin" CoinR No

credsUniv :: Term era (Set (Credential 'Staking (EraCrypto era)))
credsUniv = Var $ V "credsUniv" (SetR CredR) No

poolsUniv :: Term era (Set (KeyHash 'StakePool (EraCrypto era)))
poolsUniv = Var $ V "poolsUniv" (SetR PoolHashR) No

genesisUniv :: Term era (Set (KeyHash 'Genesis (EraCrypto era)))
genesisUniv = Var $ V "genesisUniv" (SetR GenHashR) No

txinUniv :: Term era (Set (TxIn (EraCrypto era)))
txinUniv = Var $ V "txinUniv" (SetR TxInR) No

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
newEpochStateT :: Proof era -> Target era (NewEpochState era)
newEpochStateT proof =
  Constr "NewEpochState" (newEpochStateConstr proof)
    ^$ epochNo
    ^$ prevBlocksMade
    ^$ currBlocksMade
    :$ epochStateT proof
    ^$ poolDistr

-- | Target for EpochState
epochStateT :: Proof era -> Target era (EpochState era)
epochStateT proof =
  Constr "EpochState" epochStateFun
    :$ accountStateT
    :$ snapShotsT
    :$ ledgerStateT proof
    ^$ prevpparams proof
    ^$ pparams proof
  where
    epochStateFun a s l pp p = EpochState a s l (unPParams pp) (unPParams p) (NonMyopic Map.empty (Coin 0))

-- | Target for AccountState
accountStateT :: Target era AccountState
accountStateT = Constr "AccountState" AccountState ^$ treasury ^$ reserves

-- | Target for LedgerState
ledgerStateT :: Proof era -> Target era (LedgerState era)
ledgerStateT proof = Constr "LedgerState" LedgerState :$ utxoStateT proof :$ dpstateT

-- | Target for UTxOState
utxoStateT :: Proof era -> Target era (UTxOState era)
utxoStateT p = Constr "UTxOState" (utxofun p) ^$ (pparams p) ^$ utxo p ^$ deposits ^$ fees :$ governanceStateT p
  where
    utxofun ::
      Proof era ->
      PParamsF era ->
      Map (TxIn (EraCrypto era)) (TxOutF era) ->
      Coin ->
      Coin ->
      GovernanceState era ->
      UTxOState era
    utxofun (Shelley _) (PParamsF _ pp) u c1 c2 (GovernanceState _ x) = smartUTxOState pp (liftUTxO u) c1 c2 x
    utxofun (Mary _) (PParamsF _ pp) u c1 c2 (GovernanceState _ x) = smartUTxOState pp (liftUTxO u) c1 c2 x
    utxofun (Allegra _) (PParamsF _ pp) u c1 c2 (GovernanceState _ x) = smartUTxOState pp (liftUTxO u) c1 c2 x
    utxofun (Alonzo _) (PParamsF _ pp) u c1 c2 (GovernanceState _ x) = smartUTxOState pp (liftUTxO u) c1 c2 x
    utxofun (Babbage _) (PParamsF _ pp) u c1 c2 (GovernanceState _ x) = smartUTxOState pp (liftUTxO u) c1 c2 x
    utxofun (Conway _) (PParamsF _ pp) u c1 c2 (GovernanceState _ x) = smartUTxOState pp (liftUTxO u) c1 c2 x

-- | Target for CertState
dpstateT :: Target era (CertState era)
dpstateT = Constr "CertState" CertState :$ vstateT :$ pstateT :$ dstateT

-- | Target for VState
vstateT :: Target era (VState era)
vstateT = Constr "VState" VState ^$ dreps ^$ ccHotKeys

-- | Target for PState
pstateT :: Target era (PState era)
pstateT = Constr "PState" PState ^$ regPools ^$ futureRegPools ^$ retiring ^$ poolDeposits

-- | Target for DState
dstateT :: Target era (DState era)
dstateT =
  Constr "DState" dstate
    ^$ rewards
    ^$ stakeDeposits
    ^$ delegations
    ^$ ptrs
    ^$ futureGenDelegs
    ^$ genDelegs
    :$ instantaneousRewardsT

-- | Abstract construcor function for DState
dstate ::
  Map (Credential 'Staking (EraCrypto era)) Coin ->
  Map (Credential 'Staking (EraCrypto era)) Coin ->
  Map (Credential 'Staking (EraCrypto era)) (KeyHash 'StakePool (EraCrypto era)) ->
  Map Ptr (Credential 'Staking (EraCrypto era)) ->
  Map (FutureGenDeleg (EraCrypto era)) (GenDelegPair (EraCrypto era)) ->
  Map (KeyHash 'Genesis (EraCrypto era)) (GenDelegPair (EraCrypto era)) ->
  DPS.InstantaneousRewards (EraCrypto era) ->
  DState era
dstate rew dep deleg ptr fgen gen instR =
  DState (unSplitUMap (Split rew dep deleg Map.empty undefined ptr)) fgen (GenDelegs gen) instR

instantaneousRewardsT :: Target era (DPS.InstantaneousRewards (EraCrypto era))
instantaneousRewardsT =
  Constr "InstantaneousRewards" DPS.InstantaneousRewards
    ^$ instanReserves
    ^$ instanTreasury
    ^$ deltaReserves
    ^$ deltaTreasury

-- | A String that pretty prints the complete set of variables
allvars :: String
allvars = show (ppTarget (newEpochStateT (Shelley Standard)))

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
protVer :: Proof era -> Term era ProtVer
protVer proof =
  Var
    ( V
        "protVer"
        (ProtVerR proof)
        (Yes (PParamsR proof) $ withEraPParams proof (pparamsWrapperL . ppProtocolVersionL))
    )

-- | ProtVer in prevpparams
prevProtVer :: Proof era -> Term era ProtVer
prevProtVer proof =
  Var
    ( V
        "prevProtVer"
        (ProtVerR proof)
        (Yes (PParamsR proof) $ withEraPParams proof (pparamsWrapperL . ppProtocolVersionL))
    )

minFeeA :: Proof era -> Term era Coin
minFeeA proof =
  Var
    ( V
        "minFeeA"
        CoinR
        (Yes (PParamsR proof) $ withEraPParams proof (pparamsWrapperL . ppMinFeeAL))
    )

minFeeB :: Proof era -> Term era Coin
minFeeB proof =
  Var
    ( V
        "minFeeB"
        CoinR
        (Yes (PParamsR proof) $ withEraPParams proof (pparamsWrapperL . ppMinFeeBL))
    )

-- | Max Block Body Size
maxBBSize :: Proof era -> Term era Natural
maxBBSize p =
  Var
    ( V
        "maxBBSize"
        NaturalR
        (Yes (PParamsR p) (withEraPParams p (pparamsWrapperL . ppMaxBBSizeL)))
    )

-- | Max Tx Size
maxTxSize :: Proof era -> Term era Natural
maxTxSize p =
  Var
    ( V
        "maxTxSize"
        NaturalR
        (Yes (PParamsR p) (withEraPParams p (pparamsWrapperL . ppMaxTxSizeL)))
    )

-- | Max Block Header Size
maxBHSize :: Proof era -> Term era Natural
maxBHSize p =
  Var
    ( V
        "maxBHSize"
        NaturalR
        (Yes (PParamsR p) (withEraPParams p (pparamsWrapperL . ppMaxBHSizeL)))
    )
