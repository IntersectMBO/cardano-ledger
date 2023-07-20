{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Test.Cardano.Ledger.Constrained.Preds.Certs where

import Cardano.Crypto.Hash.Class (Hash)
import Cardano.Crypto.VRF.Class (VerKeyVRF)
import Cardano.Ledger.Address (RewardAcnt (..))
import Cardano.Ledger.BaseTypes (EpochNo (..), maybeToStrictMaybe)
import Cardano.Ledger.Coin (Coin (..), DeltaCoin (..))
import Cardano.Ledger.Conway.TxCert (
  ConwayDelegCert (..),
  ConwayTxCert (..),
  Delegatee (..),
 )
import Cardano.Ledger.Core (DRep (..))
import Cardano.Ledger.Credential (Credential (..), StakeCredential)
import Cardano.Ledger.Crypto (HASH, VRF)
import Cardano.Ledger.Era (Era (EraCrypto))
import Cardano.Ledger.Keys (KeyHash, KeyRole (..))
import Cardano.Ledger.PoolParams (PoolMetadata, PoolParams (..))
import Cardano.Ledger.Pretty (ppList)
import Cardano.Ledger.Shelley.LedgerState (AccountState, InstantaneousRewards, availableAfterMIR)
import Cardano.Ledger.Shelley.TxCert (
  GenesisDelegCert (..),
  MIRCert (..),
  MIRPot (..),
  MIRTarget (..),
  PoolCert (..),
  ShelleyDelegCert (..),
  ShelleyTxCert (..),
 )
import Data.Map (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe.Strict (StrictMaybe (..))
import Lens.Micro (Lens', lens)
import Test.Cardano.Ledger.Constrained.Ast
import Test.Cardano.Ledger.Constrained.Classes
import Test.Cardano.Ledger.Constrained.Env
import Test.Cardano.Ledger.Constrained.Monad (generateWithSeed, monadTyped)
import Test.Cardano.Ledger.Constrained.Preds.CertState (dstateStage, pstateStage, vstateStage)
import Test.Cardano.Ledger.Constrained.Preds.PParams (pParamsStage)
import Test.Cardano.Ledger.Constrained.Preds.Universes (universeStage)
import Test.Cardano.Ledger.Constrained.Rewrite
import Test.Cardano.Ledger.Constrained.Solver (toolChainSub)
import Test.Cardano.Ledger.Constrained.TypeRep
import Test.Cardano.Ledger.Constrained.Vars
import Test.Cardano.Ledger.Generic.PrettyCore (pcTxCert)
import Test.Cardano.Ledger.Generic.Proof
import Test.QuickCheck

-- =============================================
-- Shelley Cert Targets

sRegKey :: Target era (StakeCredential (EraCrypto era) -> ShelleyTxCert era)
sRegKey = Constr "sRegKey" (\x -> ShelleyTxCertDelegCert (ShelleyRegCert x))

sUnRegKey :: Target era (StakeCredential (EraCrypto era) -> ShelleyTxCert era)
sUnRegKey = Constr "UnRegKey" (\x -> ShelleyTxCertDelegCert (ShelleyUnRegCert x))

sDelegStake :: Target era (StakeCredential (EraCrypto era) -> KeyHash 'StakePool (EraCrypto era) -> ShelleyTxCert era)
sDelegStake = Constr "sDelegStake" (\x y -> ShelleyTxCertDelegCert (ShelleyDelegCert x y))

sRegPool :: Target era (PoolParams (EraCrypto era) -> ShelleyTxCert era)
sRegPool = Constr "sRegPool" (\x -> ShelleyTxCertPool (RegPool x))

sRetirePool :: Target era (KeyHash 'StakePool (EraCrypto era) -> EpochNo -> ShelleyTxCert era)
sRetirePool = Constr "sRetirePool" (\x e -> ShelleyTxCertPool (RetirePool x e))

sMirShift :: Target era (Coin -> MIRPot -> Coin -> ShelleyTxCert era)
sMirShift = Constr "sMirShift" (\_avail x c -> ShelleyTxCertMir (MIRCert x (SendToOppositePotMIR c)))

sGovern ::
  Target
    era
    ( KeyHash 'Genesis (EraCrypto era) ->
      KeyHash 'GenesisDelegate (EraCrypto era) ->
      Hash (HASH (EraCrypto era)) (VerKeyVRF (VRF (EraCrypto era))) ->
      ShelleyTxCert era
    )
sGovern = Constr "sGovern" (\a b c -> ShelleyTxCertGenesisDeleg (GenesisDelegCert a b c))

-- ==========================================
-- Conway Cert Targets

cRegKey :: Target era (StakeCredential (EraCrypto era) -> Maybe Coin -> ConwayTxCert era)
cRegKey = Constr "cRegKey" (\x y -> ConwayTxCertDeleg (ConwayRegCert x (maybeToStrictMaybe y)))

cUnRegKey :: Target era (StakeCredential (EraCrypto era) -> Maybe Coin -> ConwayTxCert era)
cUnRegKey = Constr "cUnRegKey" (\x y -> ConwayTxCertDeleg (ConwayUnRegCert x (maybeToStrictMaybe y)))

cDelegStake :: Target era (StakeCredential (EraCrypto era) -> KeyHash 'StakePool (EraCrypto era) -> ConwayTxCert era)
cDelegStake = Constr "cDelegStake" (\x y -> ConwayTxCertDeleg (ConwayDelegCert x (DelegStake y)))

cDelegVote :: Target era (StakeCredential (EraCrypto era) -> DRep (EraCrypto era) -> a -> ConwayTxCert era)
cDelegVote = Constr "cDelegVote" (\x y _ -> ConwayTxCertDeleg (ConwayDelegCert x (DelegVote y)))

cDelegStakeVote ::
  Target
    era
    ( StakeCredential (EraCrypto era) ->
      KeyHash 'StakePool (EraCrypto era) ->
      DRep (EraCrypto era) ->
      a ->
      ConwayTxCert era
    )
cDelegStakeVote = Constr "cDelegStakeVote" (\x y z _ -> ConwayTxCertDeleg (ConwayDelegCert x (DelegStakeVote y z)))

cRegDelegStake :: Target era (StakeCredential (EraCrypto era) -> KeyHash 'StakePool (EraCrypto era) -> Coin -> ConwayTxCert era)
cRegDelegStake = Constr "cRegDelegStake" (\x y c -> ConwayTxCertDeleg (ConwayRegDelegCert x (DelegStake y) c))

cRegDelegVote :: Target era (StakeCredential (EraCrypto era) -> DRep (EraCrypto era) -> Coin -> a -> ConwayTxCert era)
cRegDelegVote = Constr "cRegDelegVote" (\x y c _ -> ConwayTxCertDeleg (ConwayRegDelegCert x (DelegVote y) c))

cRegDelegStakeVote ::
  Target
    era
    ( StakeCredential (EraCrypto era) ->
      KeyHash 'StakePool (EraCrypto era) ->
      DRep (EraCrypto era) ->
      Coin ->
      a ->
      ConwayTxCert era
    )
cRegDelegStakeVote = Constr "cRegDelegStakeVote" (\w x y c _ -> ConwayTxCertDeleg (ConwayRegDelegCert w (DelegStakeVote x y) c))

cRegPool :: Target era (PoolParams (EraCrypto era) -> ConwayTxCert era)
cRegPool = Constr "cRegPool" (\x -> ConwayTxCertPool (RegPool x))

cRetirePool :: Target era (KeyHash 'StakePool (EraCrypto era) -> EpochNo -> ConwayTxCert era)
cRetirePool = Constr "cRetirePool" (\x e -> ConwayTxCertPool (RetirePool x e))

-- | Transform some SubMap of  instanReserves (or instanTreasury) into a partB map
--   with invariants:  (partB ! key) <+> (instanReserves ! key) >= (Coin 0)
partBfromPartA :: Ord k => Map k Coin -> Map k DeltaCoin
partBfromPartA mp = Map.fromList (fixer (Map.toList mp))
  where
    fixer [] = []
    fixer [(k, Coin n)] = [(k, DeltaCoin (-(n - 1)))]
    fixer ((k, Coin n) : (j, Coin _) : _) = [(k, DeltaCoin (-(n - 1))), (j, DeltaCoin 8)]

-- | A user defined Predicate that Binds 'drep' to a random DRep
--   The parameter 'vote' should be existentially bound
--   in the surrounding context (inside a Choose Target perhaps)
makeDRepPred ::
  Term era (DRep (EraCrypto era)) ->
  Term era (Credential 'Voting (EraCrypto era)) ->
  Pred era
makeDRepPred drep vote =
  Oneof
    drep
    [ (1, constTarget DRepAlwaysAbstain, [])
    , (1, constTarget DRepAlwaysNoConfidence, [])
    , (5, Constr "" DRepCredential ^$ vote, [Member (Left vote) voteUniv])
    ]

-- =====================================

minusCoinDeltaCoin :: Coin -> DeltaCoin -> DeltaCoin
minusCoinDeltaCoin (Coin n) (DeltaCoin m) = DeltaCoin (n - m)

availableForDistrC :: DeltaCoin -> MIRPot -> AccountState -> InstantaneousRewards c -> DeltaCoin
availableForDistrC sumb p act irew = minusCoinDeltaCoin (availableAfterMIR p act irew) sumb

txCertMir ::
  Target era (MIRPot -> Map (Credential 'Staking (EraCrypto era)) DeltaCoin -> any -> ShelleyTxCert era)
txCertMir = Constr "txCertMir" (\x m1 _ -> ShelleyTxCertMir (MIRCert x (StakeAddressesMIR m1)))

{-
The  'mirdistr' has type (Map (Credential 'Staking c) DeltaCoin) In Eras Alonzo and Babbage
The key invaraint is  Sum(union <+> instanReserves mirdistr) <= available
This is the same as: (Sum instanReserves) <+> (Sum mirdistr) <= available
Their are two parts to the mirdistr
  1) The key is in (Dom instanReserves). We call this partB.
  2) The key is disjoint from (Dom instanReserves). We call this partC.
So now the invariant is: (Sum instanReserves) <+> (Sum partB) <+> (Sum partC) <= available
For partB we have the additional invariant:  (partB ! key) <+> (instanReserves ! key) >= (Coin 0)
For partC we have the range must be positive:  (partC ! key) >= (Coin 0)
The stateful action is to update instanReserves with (union <+> instanReserves mirdistr)
To maintain:  (Sum instanReserves) <+> (Sum partB) <+> (Sum partC) <= available
We introduce sumB and availableC,
such that: (Sum instanReserves) <+> sumB <+> availableC <= available
And generate: partB suchthat: (Sum partB) = sumB and
                              (Subset (dom partB) (Dom instanReserves)) ans
                              ((partB ! key) <+> (instanReserves ! key) >= (Coin 0))
And generate: partC suchthat: (Sum partC) = availableC
                              (Disjoint (dom partC) (Dom instanReserves))

-}

certsPreds :: forall era. Reflect era => Proof era -> [Pred era]
certsPreds p = case whichTxCert p of
  TxCertShelleyToBabbage ->
    [ certs :<-: (Constr "TxCertF" (fmap (TxCertF p)) ^$ shelleycerts)
    , Sized (Range 1 6) epochDelta -- Note that last Epoch is stored in 'maxEpoch' which was 100 on 7/7/23 see PParams.hs
    , Choose
        (Range 4 4) -- (Range 0 5)
        shelleycerts
        [
          ( 2
          , (Constr "RegUnRegOrDelegate" (\x -> x) ^$ shelleycert)
          ,
            [ Oneof
                shelleycert -- Can only have one of these at a time
                [ (1, sRegKey ^$ stakeCred, [NotMember stakeCred (Dom rewards)])
                , (1, sUnRegKey ^$ deregkey, [MapMember deregkey (Lit CoinR (Coin 0)) (Left rewards)])
                ,
                  ( 1
                  , sDelegStake ^$ stakeCred ^$ poolHash
                  , [Member (Left stakeCred) (Dom rewards), Member (Left poolHash) (Dom regPools)]
                  )
                ]
            ]
          )
        ,
          ( 1
          , sRetirePool ^$ poolHash ^$ epoch
          , [Member (Left poolHash) (Dom regPools), epoch :<-: (Constr "+" (+) ^$ currentEpoch ^$ epochDelta)]
          )
        ,
          ( 1
          , sRegPool ^$ poolParams
          ,
            [ -- Pick a random PoolParams, except constrain the fields poolId, poolRewAcnt, poolOwners, and pooMetadaa
              -- by the additional Preds. Note that the (genRep (PoolMetadataR p)) function knows how to handle the
              -- 'SoftForks.restrictPoolMetadataHash' issue on '(poolMetadata p)'. So using 'Random' which uses genRep
              -- should produce the right PoolMetadata format, by obseriving the Proof 'p'.
              Component
                (Right poolParams)
                [ field PoolParamsR poolId
                , field PoolParamsR poolRewAcnt
                , field PoolParamsR poolOwners
                , field PoolParamsR (poolMetadata p)
                ]
            , Member (Left poolId) poolHashUniv
            , poolRewAcnt :<-: (Constr "mkRewAcnt" RewardAcnt ^$ network ^$ rewCred)
            , Member (Right rewCred) credsUniv
            , Subset poolOwners stakeHashUniv
            , Maybe (poolMetadata p) (Simple localpool) [Random localpool]
            ]
          )
        ,
          ( 4
          , idTarget mircert
          ,
            [ Oneof
                mircert -- Either a StakeAddressesMIR or a SendToOppositePotMIR, choose just 1
                [
                  ( 1
                  , txCertMir ^$ pot ^$ mirdistr :$ (constTarget ())
                  ,
                    [ Random pot -- choose Treasury or Reserves
                    , available :<-: (Constr "available" availableAfterMIR ^$ pot :$ accountStateT :$ instantaneousRewardsT)
                    , If
                        (Constr "potIsTreasury" (== TreasuryMIR) ^$ pot)
                        (instanTreasurySum :=: instanSum)
                        (instanReservesSum :=: instanSum)
                    , If
                        (Constr "potIsTreasury" (== TreasuryMIR) ^$ pot)
                        (SubMap mirdistrA instanTreasury)
                        (SubMap mirdistrA instanReserves)
                    , Sized (Range 1 2) mirdistrA -- Never more than 2 in partB
                    , mirdistrB :<-: (Constr "toPartB" partBfromPartA ^$ mirdistrA)
                    , SumsTo (Right (DeltaCoin 1)) sumB EQL [SumMap mirdistrB]
                    , Sized (Range 1 3) mirdistrC
                    , Subset (Dom mirdistrC) credsUniv
                    , If
                        (Constr "potIsTreasury" (== TreasuryMIR) ^$ pot)
                        (Disjoint (Dom mirdistrC) (Dom instanTreasury))
                        (Disjoint (Dom mirdistrC) (Dom instanReserves))
                    , Before sumB mirdistrC
                    , SumsTo (Left (DeltaCoin 1)) (Delta available) GTH [One (Delta instanSum), One sumB, SumMap mirdistrC]
                    , mirdistr :<-: (Constr "union" (Map.unionWith (<>)) ^$ mirdistrC ^$ mirdistrB)
                    ]
                  )
                ,
                  ( 1
                  , sMirShift ^$ available ^$ pot ^$ mircoin
                  ,
                    [ Random pot
                    , available :<-: (Constr "available" availableAfterMIR ^$ pot :$ accountStateT :$ instantaneousRewardsT)
                    , SumsTo (Left (Coin 1)) available GTE [One mircoin]
                    ]
                  )
                ]
            ]
          )
        ]
    ]
  TxCertConwayToConway ->
    [ certs :<-: (Constr "TxCertF" (fmap (TxCertF p)) ^$ conwaycerts)
    , Sized (Range 1 6) epochDelta
    , Choose
        (Range 4 6)
        conwaycerts
        [
          ( 1
          , cRegKey ^$ stakeCred ^$ mkeydeposit
          ,
            [ NotMember stakeCred (Dom rewards)
            , Maybe mkeydeposit (idTarget kd) [kd :=: (keyDepAmt p)]
            ]
          )
        ,
          ( 1
          , cUnRegKey ^$ stakeCred ^$ mkeydeposit
          ,
            [ MapMember stakeCred (Lit CoinR (Coin 0)) (Left rewards)
            , Maybe mkeydeposit (idTarget kd) [MapMember stakeCred kd (Left stakeDeposits)]
            ]
          )
        ,
          ( 1
          , cDelegStake ^$ stakeCred ^$ poolHash
          , [Member (Left stakeCred) (Dom rewards), Member (Left poolHash) (Dom regPools)]
          )
        ,
          ( 1
          , cDelegVote ^$ stakeCred ^$ drep ^$ vote
          , [Member (Left stakeCred) (Dom rewards), makeDRepPred drep vote]
          )
        ,
          ( 1
          , cRegDelegStake ^$ stakeCred ^$ poolHash ^$ kd
          , [Member (Left stakeCred) (Dom rewards), Member (Left poolHash) (Dom regPools), kd :=: (keyDepAmt p)]
          )
        ,
          ( 1
          , cRegDelegVote ^$ stakeCred ^$ drep ^$ kd ^$ vote
          ,
            [ Member (Left stakeCred) (Dom rewards)
            , kd :=: (keyDepAmt p)
            , makeDRepPred drep vote
            ]
          )
        ,
          ( 1
          , cRegDelegStakeVote ^$ stakeCred ^$ poolHash ^$ drep ^$ kd ^$ vote
          ,
            [ Member (Left stakeCred) (Dom rewards)
            , makeDRepPred drep vote
            , Member (Left poolHash) (Dom regPools)
            , kd :=: (keyDepAmt p)
            ]
          )
        ,
          ( 1
          , cRegPool ^$ poolParams
          ,
            [ -- Pick a random PoolParams, except constrain the poolId and the poolRewAcnt, by the additional Preds
              Component
                (Right poolParams)
                [field PoolParamsR poolId, field PoolParamsR poolRewAcnt, field PoolParamsR poolOwners]
            , Member (Left poolId) poolHashUniv
            , poolRewAcnt :<-: (Constr "mkRewAcnt" RewardAcnt ^$ network ^$ rewCred)
            , Member (Right rewCred) credsUniv
            , Subset poolOwners stakeHashUniv
            ]
          )
        ,
          ( 1
          , cRetirePool ^$ poolHash ^$ epoch
          , [Member (Left poolHash) (Dom regPools), epoch :<-: (Constr "+" (+) ^$ currentEpoch ^$ epochDelta)]
          )
        ]
    ]
  where
    stakeCred = var "stakeCred" CredR
    deregkey = var "destakeCred" CredR
    poolHash = var "poolHash" PoolHashR
    epoch = var "epoch" EpochR
    shelleycerts = var "shelleycerts" (ListR ShelleyTxCertR)
    shelleycert = var "shelleycert" ShelleyTxCertR
    conwaycerts = var "conwaycerts" (ListR ConwayTxCertR)
    poolParams = var "poolParams" PoolParamsR
    pot = var "pot" MIRPotR
    mirdistrA = var "mirdistrA" (MapR CredR CoinR)
    mirdistrB = var "mirdistrB" (MapR CredR DeltaCoinR)
    mirdistrC = var "mirdistrC" (MapR CredR DeltaCoinR)
    mirdistr = var "mirdistr" (MapR CredR DeltaCoinR)
    mkeydeposit = var "mkeyDeposit" (MaybeR CoinR)
    kd = var "kd" CoinR
    vote = var "vote" VCredR
    epochDelta = var "epochDelta" EpochR
    poolId = Var (V "poolId" PoolHashR (Yes PoolParamsR (lens ppId (\x i -> x {ppId = i}))))
    poolOwners = Var (V "poolOwners" (SetR StakeHashR) (Yes PoolParamsR (lens ppOwners (\x i -> x {ppOwners = i}))))
    poolRewAcnt = Var (V "poolRewAcnt" RewardAcntR (Yes PoolParamsR (lens ppRewardAcnt (\x r -> x {ppRewardAcnt = r}))))
    localpool = (Var (V "localpool" (PoolMetadataR p) No))
    rewCred = Var (V "rewCred" CredR No)
    available = Var (V "available" CoinR No)
    sumB = var "sumB" DeltaCoinR
    instanSum = Var (V "instanSum" CoinR No)
    mircoin = var "mircoin" CoinR
    mircert = Var (V "mircert" ShelleyTxCertR No)
    drep = Var (V "drep" DRepR No)

certsStage ::
  Reflect era =>
  Proof era ->
  Subst era ->
  Gen (Subst era)
certsStage proof subst0 = do
  let preds = certsPreds proof
  toolChainSub proof standardOrderInfo preds subst0

main :: Int -> IO ()
main seed = do
  let proof =
        -- Conway Standard
        Babbage Standard
  env <-
    generateWithSeed
      seed
      ( pure emptySubst
          >>= pParamsStage proof
          >>= universeStage proof
          >>= vstateStage proof
          >>= pstateStage proof
          >>= dstateStage proof
          >>= certsStage proof
          >>= (\subst -> monadTyped (substToEnv subst emptyEnv))
      )
  -- rewritten <- snd <$> generate (rewriteGen (1,  certsPreds proof))
  -- putStrLn (show rewritten)
  certsv <- monadTyped (findVar (unVar certs) env)
  putStrLn (show (ppList (\(TxCertF _ x) -> pcTxCert proof x) certsv))
  pure ()

sMaybeL :: Lens' (StrictMaybe a) (Maybe a)
sMaybeL = lens foo bar
  where
    foo (SJust x) = Just x
    foo SNothing = Nothing
    bar _ Nothing = SNothing
    bar _ (Just x) = SJust x

maybeSL :: Lens' (Maybe a) (StrictMaybe a)
maybeSL = lens foo bar
  where
    foo (Just x) = SJust x
    foo Nothing = SNothing
    bar _ SNothing = Nothing
    bar _ (SJust x) = Just x

poolMetaL :: Lens' (PoolParams era) (StrictMaybe PoolMetadata)
poolMetaL = lens ppMetadata (\x r -> x {ppMetadata = r})

poolMetadata :: Proof era -> Term era (Maybe PoolMetadata)
poolMetadata p = Var (V "poolMetadata" (MaybeR (PoolMetadataR p)) (Yes PoolParamsR (poolMetaL . sMaybeL)))
