{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RankNTypes #-}

module Test.Cardano.Ledger.Constrained.Vars where

import Cardano.Ledger.BaseTypes (BlocksMade (..), EpochNo, StrictMaybe (..))
import Cardano.Ledger.Coin (DeltaCoin (..))
import Cardano.Ledger.Credential (Credential, Ptr)
import Cardano.Ledger.DPState (FutureGenDeleg)
import Cardano.Ledger.EpochBoundary (SnapShots (..))
import Cardano.Ledger.Era (Era (EraCrypto))
import Cardano.Ledger.Keys (GenDelegPair, KeyHash, KeyRole (..))
import Cardano.Ledger.PoolDistr (IndividualPoolStake (..), PoolDistr (..))
import Cardano.Ledger.PoolParams (PoolParams)
import Cardano.Ledger.Shelley.LedgerState (
  EpochState (..),
  NewEpochState (..),
  PPUPState (..),
 )
import Cardano.Ledger.TxIn (TxIn)
import Cardano.Ledger.UTxO (UTxO (..))
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import qualified Data.VMap as VMap
import Lens.Micro
import Numeric.Natural (Natural)
import Test.Cardano.Ledger.Babbage.Serialisation.Generators ()
import Test.Cardano.Ledger.Constrained.Ast (Term (Var))
import Test.Cardano.Ledger.Constrained.Classes (TxOut (..), liftUTxO)
import Test.Cardano.Ledger.Constrained.Env (Access (..), Field, V (..))
import Test.Cardano.Ledger.Constrained.Lenses
import Test.Cardano.Ledger.Constrained.TypeRep (Rep (..))
import Test.Cardano.Ledger.Generic.Proof (Proof (..))

-- import qualified Cardano.Ledger.Core as Core
-- import Cardano.Ledger.Shelley.LedgerState(PulsingRewUpdate, StashedAVVMAddresses)

-- ================================================================

-- NewEpochState fields

epochNo :: Term era EpochNo
epochNo = Var $ V "epochNo" undefined (Yes nesELL)

--prevBlocksMade :: Term era (Map (KeyHash 'StakePool (EraCrypto era)) Natural)
--prevBlocksMade = Var $ V "prevBlocksMade" (MapR (SimpleR typeRep) NaturalR) (Yes nesBprevL)

--currBlocksMade :: Term era (Map (KeyHash 'StakePool (EraCrypto era)) Natural)
--currBlocksMade = Var $ V "currBlocksMade" (MapR (SimpleR typeRep) NaturalR) (Yes nesBcurL)

--poolDistr :: Term era (Map (KeyHash 'StakePool (EraCrypto era)) Rational)
--poolDistr = Var $ V "poolDistr" (MapR (SimpleR typeRep) RationalR) (Yes poolDistrL)

poolDistrL :: Field era (Map (KeyHash 'StakePool (EraCrypto era)) Rational)
poolDistrL = nesPdL . unPoolDistrL . mapL individualPoolStakeL

unPoolDistrL :: Lens' (PoolDistr c) (Map (KeyHash 'StakePool c) (IndividualPoolStake c))
unPoolDistrL = lens unPoolDistr (\(PoolDistr _) x -> PoolDistr x)

-- DPState - DState

--rewards :: Term era (Map (Credential 'Staking (EraCrypto era)) Integer)
--rewards = Var $ V "rewards" (MapR (SimpleR typeRep) IntegerR) (Yes rewardsL)

rewardsL :: Field era (Map (Credential 'Staking (EraCrypto era)) Integer)
rewardsL = nesEsL . esLStateL . lsDPStateL . dpsDStateL . dsUnifiedL . rewardsUMapL . mapL coinL

--delegations :: Term era (Map (Credential 'Staking (EraCrypto era)) (KeyHash 'StakePool (EraCrypto era)))
--delegations = Var $ V "delegations" (MapR (SimpleR typeRep) (SimpleR typeRep)) (Yes delegationsL)

delegationsL :: Field era (Map (Credential 'Staking (EraCrypto era)) (KeyHash 'StakePool (EraCrypto era)))
delegationsL = nesEsL . esLStateL . lsDPStateL . dpsDStateL . dsUnifiedL . delegationsUMapL

--stakeDeposits :: Term era (Map (Credential 'Staking (EraCrypto era)) Integer)
--stakeDeposits = Var $ V "stakeDeposits" (MapR (SimpleR typeRep) IntegerR) (Yes stakeDepositsL)

stakeDepositsL :: Field era (Map (Credential 'Staking (EraCrypto era)) Integer)
stakeDepositsL = nesEsL . esLStateL . lsDPStateL . dpsDStateL . dsUnifiedL . stakeDepositsUMapL . mapL coinL

--ptrs :: Term era (Map Ptr (Credential 'Staking (EraCrypto era)))
--ptrs = Var $ V "ptrs" (MapR (SimpleR typeRep) (SimpleR typeRep)) (Yes ptrsL)

ptrsL :: Field era (Map Ptr (Credential 'Staking (EraCrypto era)))
ptrsL = nesEsL . esLStateL . lsDPStateL . dpsDStateL . dsUnifiedL . ptrsUMapL

--futureGenDelegs :: Term era (Map (FutureGenDeleg era) (GenDelegPair (EraCrypto era)))
--futureGenDelegs =
--  Var $
--    V
--      "futureGenDelegs"
--      (MapR (SimpleR typeRep) GenDelegPairR)
--      (Yes futureGenDelegsL)

futureGenDelegsL :: Field era (Map (FutureGenDeleg (EraCrypto era)) (GenDelegPair (EraCrypto era)))
futureGenDelegsL = nesEsL . esLStateL . lsDPStateL . dpsDStateL . dsFutureGenDelegsL

--genDelegs :: Term era (Map (KeyHash 'Genesis (EraCrypto era)) (GenDelegPair (EraCrypto era)))
--genDelegs = Var $ V "genDelegs" (MapR GenHashR GenDelegPairR) (Yes genDelegsL)

genDelegsL :: Field era (Map (KeyHash 'Genesis (EraCrypto era)) (GenDelegPair (EraCrypto era)))
genDelegsL = nesEsL . esLStateL . lsDPStateL . dpsDStateL . dsGenDelegsL . unGenDelegsL

-- DState - InstantaneousRewards

--instanReserves :: Term era (Map (Credential 'Staking (EraCrypto era)) Integer)
--instanReserves = Var $ V "instanReserves" (MapR (SimpleR typeRep) IntegerR) (Yes instanReservesL)

instanReservesL :: Field era (Map (Credential 'Staking (EraCrypto era)) Integer)
instanReservesL = nesEsL . esLStateL . lsDPStateL . dpsDStateL . dsIRewardsL . iRReservesL

--instanTreasury :: Term era (Map (Credential 'Staking (EraCrypto era)) Integer)
--instanTreasury = Var $ V "instanTreasury" (MapR (SimpleR typeRep) IntegerR) (Yes instanTreasuryL)

instanTreasuryL :: Field era (Map (Credential 'Staking (EraCrypto era)) Integer)
instanTreasuryL = nesEsL . esLStateL . lsDPStateL . dpsDStateL . dsIRewardsL . iRTreasuryL

--deltaReserves :: Term era DeltaCoin
--deltaReserves = Var $ V "deltaReserves" DeltaCoinR (Yes deltaReservesL)

deltaReservesL :: Field era DeltaCoin
deltaReservesL = nesEsL . esLStateL . lsDPStateL . dpsDStateL . dsIRewardsL . deltaResL

--deltaTreasury :: Term era DeltaCoin
--deltaTreasury = Var $ V "deltaTreasury" DeltaCoinR (Yes deltaTreasuryL)

deltaTreasuryL :: Field era DeltaCoin
deltaTreasuryL = nesEsL . esLStateL . lsDPStateL . dpsDStateL . dsIRewardsL . deltaTreasL

-- DPState - PState

--regPools :: Term era (Map (KeyHash 'StakePool (EraCrypto era)) (PoolParams (EraCrypto era)))
--regPools = Var $ V "regPools" (MapR (SimpleR typeRep) PoolParamsR) (Yes regPoolsL)

regPoolsL :: Field era (Map (KeyHash 'StakePool (EraCrypto era)) (PoolParams (EraCrypto era)))
regPoolsL = nesEsL . esLStateL . lsDPStateL . dpsPStateL . psStakePoolParamsL

--futureRegPools :: Term era (Map (KeyHash 'StakePool (EraCrypto era)) (PoolParams (EraCrypto era)))
--futureRegPools = Var $ V "futureRegPools" (MapR (SimpleR typeRep) PoolParamsR) (Yes futureRegPoolsL)

futureRegPoolsL :: Field era (Map (KeyHash 'StakePool (EraCrypto era)) (PoolParams (EraCrypto era)))
futureRegPoolsL = nesEsL . esLStateL . lsDPStateL . dpsPStateL . psFutureStakePoolParamsL

--retiring :: Term era (Map (KeyHash 'StakePool (EraCrypto era)) EpochNo)
--retiring = Var $ V "retiring" (MapR (SimpleR typeRep) EpochR) (Yes retiringL)

retiringL :: Field era (Map (KeyHash 'StakePool (EraCrypto era)) EpochNo)
retiringL = nesEsL . esLStateL . lsDPStateL . dpsPStateL . psRetiringL

--poolDeposits :: Term era (Map (KeyHash 'StakePool (EraCrypto era)) Integer)
--poolDeposits = Var $ V "poolDeposits" (MapR (SimpleR typeRep) IntegerR) (Yes poolDepositsL)

--poolDepositsL :: Field era (Map (KeyHash 'StakePool (EraCrypto era)) Integer)
--poolDepositsL = nesEsL . esLStateL . lsDPStateL . dpsPStateL . psDepositsL

-- UTxOState

--utxo :: Proof era -> Term era (Map (TxIn (EraCrypto era)) (TxOut era))
--utxo p = Var $ V "utxo" (MapR TxInR (TxOutR p)) (Yes (utxoL p))

utxoL :: Proof era -> Field era (Map (TxIn (EraCrypto era)) (TxOut era))
utxoL proof = nesEsL . esLStateL . lsUTxOStateL . utxosUtxoL . unUtxoL proof

unUtxoL :: Proof era -> Lens' (UTxO era) (Map (TxIn (EraCrypto era)) (TxOut era))
unUtxoL p = lens (Map.map (TxOut p) . unUTxO) (\(UTxO _) new -> (liftUTxO new))

--deposits :: Term era Integer
--deposits = Var $ V "deposits" IntegerR (Yes depositsL)

--depositsL :: Field era Integer
--depositsL = nesEsL . esLStateL . lsUTxOStateL . utxosDepositedL
--
--fees :: Term era Integer
--fees = Var $ V "fees" IntegerR (Yes feesL)

--feesL :: Field era Integer
--feesL = nesEsL . esLStateL . lsUTxOStateL . utxosFeesL

--ppup :: Proof era -> Term era (PPUPState era)
--ppup p = Var $ (V "ppup" PPUPStateR (Yes (ppupsL p)))

--proposalsT :: Proof era -> Term era (Map (KeyHash 'Genesis (EraCrypto era)) (PParamsUpdate era))
--proposalsT p = Var (V "proposals" (MapR GenHashR (PParamsUpdateR p)) No)

--futureProposalsT ::
  --Proof era -> Term era (Map (KeyHash 'Genesis (EraCrypto era)) (PParamsUpdate era))
--futureProposalsT p = Var (V "futureProposals" (MapR GenHashR (PParamsUpdateR p)) No)

--ppupStateT :: Proof era -> Target era (PPUPState era)
--ppupStateT p = Constr "PPUPState" ppupfun $> proposalsT p $> futureProposalsT p
--  where
--    ppupfun x y =
--      PPUPState
--        (ProposedPPUpdates (Map.map unPParamsUpdate x))
--        (ProposedPPUpdates (Map.map unPParamsUpdate y))

ppupsL :: Proof era -> Field era (PPUPState era)
ppupsL (Shelley _) = nesEsL . esLStateL . lsUTxOStateL . utxosPpupsL
ppupsL (Allegra _) = nesEsL . esLStateL . lsUTxOStateL . utxosPpupsL
ppupsL (Mary _) = nesEsL . esLStateL . lsUTxOStateL . utxosPpupsL
ppupsL (Alonzo _) = nesEsL . esLStateL . lsUTxOStateL . utxosPpupsL
ppupsL (Babbage _) = nesEsL . esLStateL . lsUTxOStateL . utxosPpupsL
ppupsL (Conway _) = nesEsL . esLStateL . lsUTxOStateL . utxosPpupsL

individualPoolStakeL :: Lens' (IndividualPoolStake c) Rational
individualPoolStakeL = lens individualPoolStake (\ds u -> ds {individualPoolStake = u})

-- Incremental Stake

--isPtrMapT :: Term era (Map Ptr Integer)
--isPtrMapT = Var $ V "ptrMap" (MapR PtrR IntegerR) (Yes ptrMapL)

--ptrMapL :: Lens' (NewEpochState era) (Map Ptr Integer)
--ptrMapL = nesEsL . esLStateL . lsUTxOStateL . utxosStakeDistrL . isPtrMapL

--isCredMapT :: Term era (Map (Credential 'Staking (EraCrypto era)) Integer)
--isCredMapT = Var $ V "credMap" (MapR (SimpleR typeRep) IntegerR) (Yes credMapL)

--credMapL :: Lens' (NewEpochState era) (Map (Credential 'Staking (EraCrypto era)) Integer)
--credMapL = nesEsL . esLStateL . lsUTxOStateL . utxosStakeDistrL . isCredMapL

-- AccountState

--treasury :: Term era Integer
--treasury = Var $ V "treasury" IntegerR (Yes treasuryL)

--treasuryL :: Field era Integer
--treasuryL = nesEsL . esAccountStateL . asTreasuryL

--reserves :: Term era Integer
--reserves = Var $ V "reserves" IntegerR (Yes reservesL)

--reservesL :: Field era Integer
--reservesL = nesEsL . esAccountStateL . asReservesL

-- EpochState

--snapshots :: Term era (SnapShots (EraCrypto era))
--snapshots = Var (V "snapshots" SnapShotsR (Yes snapshotsL))

snapshotsL :: Field era (SnapShots (EraCrypto era))
snapshotsL = nesEsL . esSnapshotsL

--prevpparams :: Proof era -> Term era (PParams era)
--prevpparams p = Var (V "prevpparams" (PParamsR p) No) -- (Yes (nesEsL . esPrevPpL)))

--pparams :: Proof era -> Term era (PParams era)
--pparams p = Var (V "pparams" (PParamsR p) No) -- (Yes (nesEsL . esPpL)))

--nmLikelihoodsT :: Term era (Map (KeyHash 'StakePool (EraCrypto era)) [Float])
--nmLikelihoodsT = Var (V "likelihoodsNM" (MapR (SimpleR typeRep) (ListR FloatR)) (Yes (nesEsL . esNonMyopicL . nmLikelihoodsL)))

--nmRewardPotT :: Term era Integer
--nmRewardPotT = Var $ V "rewardPotNM" IntegerR (Yes (nesEsL . esNonMyopicL . nmRewardPotL))

-- ===== SnapShots

-- | Helper lens that deals with the Stake newtype, and the shift from Map to VMap
--stakeL :: Lens' (Stake c) (Map (Credential 'Staking c) Integer)
--stakeL =
--  lens
--    (Map.map fromCompact . VMap.toMap . unStake)
--    (\_ u -> Stake . VMap.fromMap . Map.map compactIntegerOrError $ u)

-- | Helper lens that deals with the shift from Map to VMap
vmapL :: Lens' (VMap.VMap VMap.VB VMap.VB k v) (Map k v)
vmapL = lens VMap.toMap (\_ u -> VMap.fromMap u)

--markStakeL :: Field era (Map (Credential 'Staking (EraCrypto era)) Integer)
--markStakeL = nesEsL . esSnapshotsL . ssStakeMarkL . ssStakeL . stakeL

--markStake :: Term era (Map (Credential 'Staking (EraCrypto era)) Integer)
--markStake = Var (V "markStake" (MapR (SimpleR typeRep) IntegerR) (Yes markStakeL))

--markDelegs :: Term era (Map (Credential 'Staking (EraCrypto era)) (KeyHash 'StakePool (EraCrypto era)))
--markDelegs = Var (V "markDelegs" (MapR (SimpleR typeRep) (SimpleR typeRep)) (Yes markDelegsL))

markDelegsL :: Field era (Map (Credential 'Staking (EraCrypto era)) (KeyHash 'StakePool (EraCrypto era)))
markDelegsL = nesEsL . esSnapshotsL . ssStakeMarkL . ssDelegationsL . vmapL

--markPools :: Term era (Map (KeyHash 'StakePool (EraCrypto era)) (PoolParams (EraCrypto era)))
--markPools = Var (V "markPools" (MapR (SimpleR typeRep) PoolParamsR) (Yes markPoolsL))

markPoolsL :: Field era (Map (KeyHash 'StakePool (EraCrypto era)) (PoolParams (EraCrypto era)))
markPoolsL = nesEsL . esSnapshotsL . ssStakeMarkL . ssPoolParamsL . vmapL

--markSnapShotT :: Target era (SnapShot (EraCrypto era))
--markSnapShotT = Constr "SnapShot" snapfun $> markStake $> markDelegs $> markPools
--  where
--    snapfun x y z =
--      SnapShot
--        (Stake (VMap.fromMap (Map.map compactIntegerOrError x)))
--        (VMap.fromMap y)
--        (VMap.fromMap z)

--setStake :: Term era (Map (Credential 'Staking (EraCrypto era)) Integer)
--setStake = Var (V "setStake" (MapR (SimpleR typeRep) IntegerR) (Yes setStakeL))

--setStakeL :: Field era (Map (Credential 'Staking (EraCrypto era)) Integer)
--setStakeL = nesEsL . esSnapshotsL . ssStakeSetL . ssStakeL . stakeL

--setDelegs :: Term era (Map (Credential 'Staking (EraCrypto era)) (KeyHash 'StakePool (EraCrypto era)))
--setDelegs = Var (V "setDelegs" (MapR (SimpleR typeRep) (SimpleR typeRep)) (Yes setDelegsL))

setDelegsL :: Field era (Map (Credential 'Staking (EraCrypto era)) (KeyHash 'StakePool (EraCrypto era)))
setDelegsL = nesEsL . esSnapshotsL . ssStakeSetL . ssDelegationsL . vmapL

--setPools :: Term era (Map (KeyHash 'StakePool (EraCrypto era)) (PoolParams (EraCrypto era)))
--setPools = Var (V "setPools" (MapR (SimpleR typeRep) PoolParamsR) (Yes setPoolsL))

setPoolsL :: Field era (Map (KeyHash 'StakePool (EraCrypto era)) (PoolParams (EraCrypto era)))
setPoolsL = nesEsL . esSnapshotsL . ssStakeSetL . ssPoolParamsL . vmapL

--setSnapShotT :: Target era (SnapShot (EraCrypto era))
--setSnapShotT = Constr "SnapShot" snapfun $> setStake $> setDelegs $> setPools
--  where
--    snapfun x y z =
--      SnapShot
--        (Stake (VMap.fromMap (Map.map compactIntegerOrError x)))
--        (VMap.fromMap y)
--        (VMap.fromMap z)

--goStake :: Term era (Map (Credential 'Staking (EraCrypto era)) Integer)
--goStake = Var (V "goStake" (MapR (SimpleR typeRep) IntegerR) (Yes goStakeL))

--goStakeL :: Field era (Map (Credential 'Staking (EraCrypto era)) Integer)
--goStakeL = nesEsL . esSnapshotsL . ssStakeGoL . ssStakeL . stakeL

--goDelegs :: Term era (Map (Credential 'Staking (EraCrypto era)) (KeyHash 'StakePool (EraCrypto era)))
--goDelegs = Var (V "goDelegs" (MapR (SimpleR typeRep) (SimpleR typeRep)) (Yes goDelegsL))

goDelegsL :: Field era (Map (Credential 'Staking (EraCrypto era)) (KeyHash 'StakePool (EraCrypto era)))
goDelegsL = nesEsL . esSnapshotsL . ssStakeGoL . ssDelegationsL . vmapL

--goPools :: Term era (Map (KeyHash 'StakePool (EraCrypto era)) (PoolParams (EraCrypto era)))
--goPools = Var (V "goPools" (MapR (SimpleR typeRep) PoolParamsR) (Yes goPoolsL))

goPoolsL :: Field era (Map (KeyHash 'StakePool (EraCrypto era)) (PoolParams (EraCrypto era)))
goPoolsL = nesEsL . esSnapshotsL . ssStakeGoL . ssPoolParamsL . vmapL

--goSnapShotT :: Target era (SnapShot (EraCrypto era))
--goSnapShotT = Constr "SnapShot" snapfun $> goStake $> goDelegs $> goPools
--  where
--    snapfun x y z =
--      SnapShot
--        (Stake (VMap.fromMap (Map.map compactIntegerOrError x)))
--        (VMap.fromMap y)
--        (VMap.fromMap z)

--markPoolDistr :: Term era (Map (KeyHash 'StakePool (EraCrypto era)) (IndividualPoolStake (EraCrypto era)))
--markPoolDistr = Var (V "markPoolDistr" (MapR (SimpleR typeRep) IPoolStakeR) No)

markPoolDistrL :: Field era (Map (KeyHash 'StakePool (EraCrypto era)) (IndividualPoolStake (EraCrypto era)))
markPoolDistrL = nesEsL . esSnapshotsL . ssStakeMarkPoolDistrL . pooldistrHelpL

pooldistrHelpL :: Lens' (PoolDistr c) (Map (KeyHash 'StakePool c) (IndividualPoolStake c))
pooldistrHelpL = lens unPoolDistr (\_ u -> PoolDistr u)

snapShotFee :: Term era Integer
snapShotFee = Var (V "snapShotFee" IntegerR No)

--snapShotsT :: Target era (SnapShots (EraCrypto era))
--snapShotsT =
--  Constr "SnapShots" shotsfun
--    :$ markSnapShotT
--    :$ (Simple markPoolDistr)
--    :$ setSnapShotT
--    :$ goSnapShotT
--    :$ (Simple snapShotFee)
--  where
--    shotsfun w x y z f = SnapShots w (PoolDistr x) y z f

-- ===================================================================
-- Non Access variables

totalAda :: Term era Integer
totalAda = Var $ V "totalAda" IntegerR No

utxoInteger :: Term era Integer
utxoInteger = Var $ V "utxoInteger" IntegerR No

--credsUniv :: Term era (Set (Credential 'Staking (EraCrypto era)))
--credsUniv = Var $ V "credsUniv" (SetR (SimpleR typeRep)) No

--poolsUniv :: Term era (Set (KeyHash 'StakePool (EraCrypto era)))
--poolsUniv = Var $ V "poolsUniv" (SetR (SimpleR typeRep)) No

-- ====================================================================
-- Targets for sub types of NewEpochState
-- A Target assembles variables into data stuctures. The main concern
-- is transforming the types used in the variable model into the real types
-- stored in the data structures. 4 examples of such transformation
-- 1) Wrapping and unwraping newtypes like StakeDistr
-- 2) Transforming Integer into (CompactForm Integer) a Word64
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

-- | Target for AccountState
--accountStateT :: Target era AccountState
--accountStateT = Constr "AccountState" AccountState $> treasury $> reserves

-- | Target for LedgerState
--ledgerStateT :: Proof era -> Target era (LedgerState era)
--ledgerStateT proof = Constr "LedgerState" LedgerState :$ utxoStateT proof :$ dpstateT

-- | Target for UTxOState
--utxoStateT :: Proof era -> Target era (UTxOState era)
--utxoStateT p = Constr "UTxOState" (utxofun p) $> utxo p $> deposits $> fees :$ ppupStateT p
--  where
--    utxofun ::
--      Proof era ->
--      Map (TxIn (EraCrypto era)) (TxOut era) ->
--      Integer ->
--      Integer ->
--      (PPUPState era) ->
--      UTxOState era
--    utxofun (Shelley _) u = smartUTxOState (liftUTxO u)
--    utxofun (Mary _) u = smartUTxOState (liftUTxO u)
--    utxofun (Allegra _) u = smartUTxOState (liftUTxO u)
--    utxofun (Alonzo _) u = smartUTxOState (liftUTxO u)
--    utxofun (Babbage _) u = smartUTxOState (liftUTxO u)
--    utxofun (Conway _) u = smartUTxOState (liftUTxO u)

-- | Target for DPState
--dpstateT :: Target era (DPState (EraCrypto era))
--dpstateT = Constr "DPState" DPState :$ dstateT :$ pstateT

-- | Target for PState
--pstateT :: Target era (PState (EraCrypto era))
--pstateT = Constr "PState" PState $> regPools $> futureRegPools $> retiring $> poolDeposits

-- | Target for DState
--dstateT :: Target era (DState (EraCrypto era))
--dstateT =
--  Constr "DState" dstate
--    $> rewards
--    $> stakeDeposits
--    $> delegations
--    $> ptrs
--    $> futureGenDelegs
--    $> genDelegs
--    :$ instantaneousRewardsT

-- | Abstract construcor function for DState
--dstate ::
--  Map (Credential 'Staking c) Integer ->
--  Map (Credential 'Staking c) Integer ->
--  Map (Credential 'Staking c) (KeyHash 'StakePool c) ->
--  Map Ptr (Credential 'Staking c) ->
--  Map (FutureGenDeleg c) (GenDelegPair c) ->
--  Map (KeyHash 'Genesis c) (GenDelegPair c) ->
--  DPS.InstantaneousRewards c ->
--  DState c
--dstate rew dep deleg ptr fgen gen instR =
--  DState (unSplitUMap (Split rew dep deleg undefined ptr)) fgen (GenDelegs gen) instR

--instantaneousRewardsT :: Target era (DPS.InstantaneousRewards (EraCrypto era))
--instantaneousRewardsT =
--  Constr "InstantaneousRewards" DPS.InstantaneousRewards
--    $> instanReserves
--    $> instanTreasury
--    $> deltaReserves
--    $> deltaTreasury

-- | A String that pretty prints the complete set of variables
--allvars :: String
--allvars = show (ppTarget (newEpochStateT (Shelley Standard)))
