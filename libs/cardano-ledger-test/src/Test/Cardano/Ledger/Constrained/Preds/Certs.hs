{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Test.Cardano.Ledger.Constrained.Preds.Certs where

import Cardano.Ledger.Shelley.HardForks as HardForks (allowMIRTransfer)
import Test.Cardano.Ledger.Generic.Functions (protocolVersion)

import Cardano.Crypto.Hash.Class (Hash)
import Cardano.Crypto.VRF.Class (VerKeyVRF)
import Cardano.Ledger.Address (RewardAcnt (..))
import Cardano.Ledger.BaseTypes (EpochNo (..), maybeToStrictMaybe)
import Cardano.Ledger.Coin (Coin (..), DeltaCoin (..))
import Cardano.Ledger.Conway.TxCert (
  ConwayDelegCert (..),
  ConwayEraTxCert,
  ConwayTxCert (..),
  Delegatee (..),
  pattern RegDepositDelegTxCert,
 )
import Cardano.Ledger.Credential (Credential (..), StakeCredential)
import Cardano.Ledger.Crypto (HASH, VRF)
import Cardano.Ledger.DRep (DRep (..))
import Cardano.Ledger.Era (Era (EraCrypto))
import Cardano.Ledger.Keys (KeyHash, KeyRole (..))
import Cardano.Ledger.PoolParams (PoolMetadata, PoolParams (..))
import Cardano.Ledger.Pretty (ppList)
import Cardano.Ledger.Shelley.LedgerState (AccountState, InstantaneousRewards, availableAfterMIR)
import Cardano.Ledger.Shelley.TxCert (
  EraTxCert (..),
  GenesisDelegCert (..),
  MIRCert (..),
  MIRPot (..),
  MIRTarget (..),
  PoolCert (..),
  ShelleyDelegCert (..),
  ShelleyTxCert (..),
 )
import Control.Monad (when)
import Data.Default.Class (Default (def))
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
import Test.Cardano.Ledger.Constrained.Preds.Repl (ReplMode (..), modeRepl)
import Test.Cardano.Ledger.Constrained.Preds.Universes (UnivSize (..), universeStage)
import Test.Cardano.Ledger.Constrained.Rewrite
import Test.Cardano.Ledger.Constrained.Solver (toolChainSub)
import Test.Cardano.Ledger.Constrained.TypeRep
import Test.Cardano.Ledger.Constrained.Utils (testIO)
import Test.Cardano.Ledger.Constrained.Vars
import Test.Cardano.Ledger.Generic.PrettyCore (pcTxCert)
import Test.Cardano.Ledger.Generic.Proof
import Test.QuickCheck
import Test.Tasty (TestTree, defaultMain)
import Type.Reflection (Typeable, typeRep)

-- =============================================
-- Shelley Cert Targets

sRegKey ::
  forall era.
  Typeable era =>
  RootTarget era (ShelleyTxCert era) (StakeCredential (EraCrypto era) -> ShelleyTxCert era)
sRegKey = Invert "sRegKey" (typeRep @(ShelleyTxCert era)) (\x -> ShelleyTxCertDelegCert (ShelleyRegCert x))

sUnRegKey ::
  forall era.
  Typeable era =>
  RootTarget era (ShelleyTxCert era) (StakeCredential (EraCrypto era) -> ShelleyTxCert era)
sUnRegKey = Invert "UnRegKey" (typeRep @(ShelleyTxCert era)) (\x -> ShelleyTxCertDelegCert (ShelleyUnRegCert x))

sDelegStake ::
  forall era.
  Typeable era =>
  RootTarget
    era
    (ShelleyTxCert era)
    (StakeCredential (EraCrypto era) -> KeyHash 'StakePool (EraCrypto era) -> ShelleyTxCert era)
sDelegStake = Invert "sDelegStake" (typeRep @(ShelleyTxCert era)) (\x y -> ShelleyTxCertDelegCert (ShelleyDelegCert x y))

sRegPool :: Target era (PoolParams (EraCrypto era) -> ShelleyTxCert era)
sRegPool = Constr "sRegPool" (\x -> ShelleyTxCertPool (RegPool x))

sRetirePool :: Target era (KeyHash 'StakePool (EraCrypto era) -> EpochNo -> ShelleyTxCert era)
sRetirePool = Constr "sRetirePool" (\x e -> ShelleyTxCertPool (RetirePool x e))

sMirShift :: forall era. Era era => RootTarget era (ShelleyTxCert era) (Coin -> MIRPot -> Coin -> ShelleyTxCert era)
sMirShift = Invert "sMirShift" (typeRep @(ShelleyTxCert era)) (\_avail x c -> ShelleyTxCertMir (MIRCert x (SendToOppositePotMIR c)))

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

cRegDeleg ::
  forall era.
  ConwayEraTxCert era =>
  Target era (StakeCredential (EraCrypto era) -> Delegatee (EraCrypto era) -> Coin -> TxCert era)
cRegDeleg = Constr "cRegDeleg" RegDepositDelegTxCert

cDelegateeStake ::
  forall era.
  Era era =>
  RootTarget era (Delegatee (EraCrypto era)) (KeyHash 'StakePool (EraCrypto era) -> Delegatee (EraCrypto era))
cDelegateeStake = Invert "cDelegateeStake" (typeRep @(Delegatee (EraCrypto era))) DelegStake

cDelegateeVote ::
  forall era.
  Era era =>
  RootTarget era (Delegatee (EraCrypto era)) (DRep (EraCrypto era) -> Delegatee (EraCrypto era))
cDelegateeVote = Invert "cDelegateeVote" (typeRep @(Delegatee (EraCrypto era))) DelegVote

cDelegateeStakeVote ::
  forall era.
  Era era =>
  RootTarget era (Delegatee (EraCrypto era)) (KeyHash 'StakePool (EraCrypto era) -> DRep (EraCrypto era) -> Delegatee (EraCrypto era))
cDelegateeStakeVote = Invert "cDelegateeVote" (typeRep @(Delegatee (EraCrypto era))) DelegStakeVote

cRegPool :: Target era (PoolParams (EraCrypto era) -> ConwayTxCert era)
cRegPool = Constr "cRegPool" (\x -> ConwayTxCertPool (RegPool x))

cRetirePool :: Target era (KeyHash 'StakePool (EraCrypto era) -> EpochNo -> ConwayTxCert era)
cRetirePool = Constr "cRetirePool" (\x e -> ConwayTxCertPool (RetirePool x e))

-- | Transform some SubMap of  instanReserves (or instanTreasury) into a partB map
--   with invariants:  (partB ! key) <+> (instanReserves ! key) >= (Coin 0)
-- Note in Era before Alonzo, Negative transfers are not allowed.
partBfromPartA :: Ord k => Proof era -> Map k Coin -> Map k DeltaCoin
partBfromPartA p mp = Map.fromList (fixer (Map.toList mp))
  where
    fixer [] = []
    fixer [(k, Coin n)] =
      case whichTxOut p of
        TxOutShelleyToMary -> [(k, DeltaCoin 6)]
        _ -> [(k, DeltaCoin (-(n - 1)))]
    fixer ((k, Coin n) : (j, Coin _) : _) =
      case whichTxOut p of
        TxOutShelleyToMary -> [(k, DeltaCoin 5), (j, DeltaCoin 3)]
        _ -> [(k, DeltaCoin (-(n - 1))), (j, DeltaCoin 8)]

-- | A user defined Predicate that Binds 'drep' to a random DRep
--   The parameter 'vote' should be existentially bound
--   in the surrounding context (inside a Choose Target perhaps)
makeDRepPred ::
  forall era.
  Era era =>
  Term era (DRep (EraCrypto era)) ->
  Term era (Credential 'DRepRole (EraCrypto era)) ->
  Pred era
makeDRepPred drep vote =
  Oneof
    drep
    [ (1, constRootTarget DRepAlwaysAbstain, [Random vote])
    , (1, constRootTarget DRepAlwaysNoConfidence, [Random vote])
    ,
      ( 5
      , Invert "" (typeRep @(DRep (EraCrypto era))) DRepCredential
          :$ Partial vote (\case DRepCredential x -> Just x; _ -> Nothing)
      , [Member (Left vote) voteUniv]
      )
    ]

-- =====================================

minusCoinDeltaCoin :: Coin -> DeltaCoin -> DeltaCoin
minusCoinDeltaCoin (Coin n) (DeltaCoin m) = DeltaCoin (n - m)

availableForDistrC :: DeltaCoin -> MIRPot -> AccountState -> InstantaneousRewards c -> DeltaCoin
availableForDistrC sumb p act irew = minusCoinDeltaCoin (availableAfterMIR p act irew) sumb

txCertMir ::
  forall era any.
  Era era =>
  RootTarget era (ShelleyTxCert era) (MIRPot -> Map (Credential 'Staking (EraCrypto era)) DeltaCoin -> any -> ShelleyTxCert era)
txCertMir = Invert "txCertMir" (typeRep @(ShelleyTxCert era)) (\pot distr _ -> ShelleyTxCertMir (MIRCert pot (StakeAddressesMIR distr)))

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

certsPreds :: forall era. Reflect era => UnivSize -> Proof era -> [Pred era]
certsPreds UnivSize {..} p = case whichTxCert p of
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
                [
                  ( 1
                  , sRegKey :$ Partial stakeCred (\case (ShelleyTxCertDelegCert (ShelleyRegCert x)) -> Just x; _ -> Nothing)
                  , [NotMember stakeCred (Dom rewards)]
                  )
                ,
                  ( 1
                  , sUnRegKey :$ Partial deregkey (\case (ShelleyTxCertDelegCert (ShelleyUnRegCert x)) -> Just x; _ -> Nothing)
                  , [MapMember deregkey (Lit CoinR (Coin 0)) (Left rewards)]
                  )
                ,
                  ( 1
                  , sDelegStake
                      :$ Partial stakeCred (\case (ShelleyTxCertDelegCert (ShelleyDelegCert x _)) -> Just x; _ -> Nothing)
                      :$ Partial poolHash (\case (ShelleyTxCertDelegCert (ShelleyDelegCert _ y)) -> Just y; _ -> Nothing)
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
            [ -- Pick a random PoolParams, except constrain the fields poolId, poolRewAcnt, poolOwners, and poolMetadata
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
                  , txCertMir
                      :$ Partial pot (\case (ShelleyTxCertMir (MIRCert pt (StakeAddressesMIR _))) -> Just pt; _ -> Nothing)
                      :$ Partial mirdistr (\case (ShelleyTxCertMir (MIRCert _ (StakeAddressesMIR distr))) -> Just distr; _ -> Nothing)
                      :$ Partial (Lit UnitR ()) (const (Just ()))
                  ,
                    [ Random pot -- choose Treasury or Reserves
                    , If
                        (Constr "hardfork" HardForks.allowMIRTransfer ^$ (protVer p))
                        (available :<-: (Constr "available" availableAfterMIR ^$ pot :$ Mask accountStateT :$ Mask instantaneousRewardsT))
                        ( If
                            (Constr "potIsTreasury" (== TreasuryMIR) ^$ pot)
                            (treasury :=: available)
                            (reserves :=: available)
                        )
                    , If
                        (Constr "potIsTreasury" (== TreasuryMIR) ^$ pot)
                        (instanTreasurySum :=: instanSum)
                        (instanReservesSum :=: instanSum)
                    , If
                        (Constr "potIsTreasury" (== TreasuryMIR) ^$ pot)
                        (SubMap mirdistrA instanTreasury)
                        (SubMap mirdistrA instanReserves)
                    , Sized (Range 1 2) mirdistrA -- Never more than 2 in partB
                    , mirdistrB :<-: (Constr "toPartB" (partBfromPartA p) ^$ mirdistrA)
                    , SumsTo (Right (DeltaCoin 1)) sumB EQL [SumMap mirdistrB]
                    , Sized (Range 1 3) mirdistrC
                    , Subset (Dom mirdistrC) credsUniv
                    , If
                        (Constr "potIsTreasury" (== TreasuryMIR) ^$ pot)
                        (Disjoint (Dom mirdistrC) (Dom instanTreasury))
                        (Disjoint (Dom mirdistrC) (Dom instanReserves))
                    , Before sumB mirdistrC
                    , SumsTo (Left (DeltaCoin 1)) (Delta available) GTH [One (Delta instanSum), One sumB, SumMap mirdistrC]
                    , If
                        (Constr "hardfork" HardForks.allowMIRTransfer ^$ (protVer p))
                        (mirdistr :<-: (Constr "union" (Map.unionWith (<>)) ^$ mirdistrC ^$ mirdistrB))
                        (mirdistr :<-: (Constr "union" Map.union ^$ mirdistrC ^$ mirdistrB))
                    ]
                  )
                ,
                  ( if (HardForks.allowMIRTransfer (protocolVersion p)) then 1 else 0 -- Not allowed before Alonzo, so make frequency 0
                  , sMirShift
                      :$ Partial available (\case (ShelleyTxCertMir (MIRCert _ (SendToOppositePotMIR _))) -> Just (Coin 99); _ -> Nothing)
                      :$ Partial pot (\case (ShelleyTxCertMir (MIRCert pt (SendToOppositePotMIR _))) -> Just pt; _ -> Nothing)
                      :$ Partial mircoin (\case (ShelleyTxCertMir (MIRCert _ (SendToOppositePotMIR c))) -> Just c; _ -> Nothing)
                  ,
                    [ Random pot
                    , available :<-: (Constr "available" availableAfterMIR ^$ pot :$ Mask accountStateT :$ Mask instantaneousRewardsT)
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
        (Range usMinCerts usMaxCerts)
        conwaycerts
        [
          ( 1
          , cDelegStake ^$ stakeCred ^$ poolHash
          ,
            [ Member (Left stakeCred) (Dom rewards)
            , Member (Left poolHash) (Dom regPools)
            ]
          )
        ,
          ( 1
          , cDelegVote ^$ stakeCred ^$ drep ^$ vote
          ,
            [ Member (Left stakeCred) (Dom rewards)
            , makeDRepPred drep vote
            , Member (Left vote) voteUniv
            ]
          )
        ,
          ( 1
          , cRegDeleg ^$ stakeCred ^$ delegatee ^$ kd
          ,
            [ Oneof
                delegatee
                [
                  ( 3
                  , cDelegateeStake :$ Partial poolHash (\case (DelegStake x) -> Just x; _ -> Nothing)
                  ,
                    [ Member (Left poolHash) (Dom regPools)
                    ]
                  )
                ,
                  ( 1
                  , cDelegateeVote :$ Partial drep1 (\case (DelegVote x) -> Just x; _ -> Nothing)
                  ,
                    [ makeDRepPred drep1 vote1
                    , Member (Left vote1) voteUniv
                    ]
                  )
                ,
                  ( 1
                  , cDelegateeVote :$ Partial drep1a (\case (DelegVote x) -> Just x; _ -> Nothing)
                  ,
                    [ drep1a :<-: constTarget DRepAlwaysAbstain
                    ]
                  )
                ,
                  ( 1
                  , cDelegateeVote :$ Partial drep1b (\case (DelegVote x) -> Just x; _ -> Nothing)
                  ,
                    [ drep1b :<-: constTarget DRepAlwaysNoConfidence
                    ]
                  )
                ,
                  ( 1
                  , cDelegateeStakeVote
                      :$ Partial poolHash (\case (DelegStakeVote x _) -> Just x; _ -> Nothing)
                      :$ Partial drep2 (\case (DelegStakeVote _ x) -> Just x; _ -> Nothing)
                  ,
                    [ Member (Left poolHash) (Dom regPools)
                    , makeDRepPred drep2 vote2
                    , Member (Left vote2) voteUniv
                    ]
                  )
                ,
                  ( 1
                  , cDelegateeStakeVote
                      :$ Partial poolHash (\case (DelegStakeVote x _) -> Just x; _ -> Nothing)
                      :$ Partial drep2a (\case (DelegStakeVote _ x) -> Just x; _ -> Nothing)
                  ,
                    [ Member (Left poolHash) (Dom regPools)
                    , drep2a :<-: constTarget DRepAlwaysAbstain
                    ]
                  )
                ,
                  ( 1
                  , cDelegateeStakeVote
                      :$ Partial poolHash (\case (DelegStakeVote x _) -> Just x; _ -> Nothing)
                      :$ Partial drep2b (\case (DelegStakeVote _ x) -> Just x; _ -> Nothing)
                  ,
                    [ Member (Left poolHash) (Dom regPools)
                    , drep2b :<-: constTarget DRepAlwaysNoConfidence
                    ]
                  )
                ]
            , Member (Left stakeCred) credsUniv
            , NotMember stakeCred (Dom rewards)
            , kd :=: keyDepAmt p
            ]
          )
        ,
          ( 1
          , cRegPool ^$ poolParams
          , [ -- Pick a random PoolParams, except constrain the poolId and the poolRewAcnt, by the additional Preds
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
              ++ [NotMember poolId (Dom regPools) | not usAllowReRegisterPool]
          )
        ,
          ( 1
          , cRetirePool ^$ poolHash ^$ epoch
          ,
            [ Member (Left poolHash) (Dom regPools)
            , epoch :<-: (Constr "+" (+) ^$ currentEpoch ^$ epochDelta)
            ]
          )
        ,
          ( usRegKeyFreq
          , cRegKey ^$ stakeCred ^$ mkeydeposit
          ,
            [ NotMember stakeCred (Dom rewards)
            , Member (Left stakeCred) credsUniv
            , Maybe mkeydeposit (idTarget kd) [kd :=: (keyDepAmt p)]
            ]
          )
        ,
          ( usUnRegKeyFreq
          , cUnRegKey ^$ stakeCred ^$ mkeydeposit
          ,
            [ MapMember stakeCred (Lit CoinR (Coin 0)) (Left rewards)
            , Maybe mkeydeposit (idTarget kd) [MapMember stakeCred kd (Left stakeDeposits)]
            ]
          )
        ]
    ]
  where
    delegatee = Var $ pV p "delegatee" DelegateeR No
    stakeCred = Var $ pV p "stakeCred" CredR No
    deregkey = Var $ pV p "destakeCred" CredR No
    poolHash = Var $ pV p "poolHash" PoolHashR No
    epoch = Var $ pV p "epoch" EpochR No
    shelleycerts = Var $ pV p "shelleycerts" (ListR ShelleyTxCertR) No
    shelleycert = Var $ pV p "shelleycert" ShelleyTxCertR No
    conwaycerts = Var $ pV p "conwaycerts" (ListR ConwayTxCertR) No
    poolParams = Var $ pV p "poolParams" PoolParamsR No
    pot = Var $ pV p "pot" MIRPotR No
    mirdistrA = Var $ pV p "mirdistrA" (MapR CredR CoinR) No
    mirdistrB = Var $ pV p "mirdistrB" (MapR CredR DeltaCoinR) No
    mirdistrC = Var $ pV p "mirdistrC" (MapR CredR DeltaCoinR) No
    mirdistr = Var $ pV p "mirdistr" (MapR CredR DeltaCoinR) No
    mkeydeposit = Var $ pV p "mkeyDeposit" (MaybeR CoinR) No
    kd = Var $ pV p "kd" CoinR No
    vote = Var $ pV p "vote" VCredR No
    vote1 = Var $ pV p "vote1" VCredR No
    vote2 = Var $ pV p "vote2" VCredR No
    epochDelta = Var $ pV p "epochDelta" EpochR No
    poolId = Var (pV p "poolId" PoolHashR (Yes PoolParamsR (lens ppId (\x i -> x {ppId = i}))))
    poolOwners = Var (pV p "poolOwners" (SetR StakeHashR) (Yes PoolParamsR (lens ppOwners (\x i -> x {ppOwners = i}))))
    poolRewAcnt = Var (pV p "poolRewAcnt" RewardAcntR (Yes PoolParamsR (lens ppRewardAcnt (\x r -> x {ppRewardAcnt = r}))))
    localpool = Var (pV p "localpool" (PoolMetadataR p) No)
    rewCred = Var (pV p "rewCred" CredR No)
    available = Var (pV p "available" CoinR No)
    sumB = Var $ pV p "sumB" DeltaCoinR No
    instanSum = Var (pV p "instanSum" CoinR No)
    mircoin = Var $ pV p "mircoin" CoinR No
    mircert = Var (pV p "mircert" ShelleyTxCertR No)
    drep = Var (pV p "drep" DRepR No)
    drep1 = Var (pV p "drep1" DRepR No)
    drep1a = Var (pV p "drep1a" DRepR No)
    drep1b = Var (pV p "drep1b" DRepR No)
    drep2 = Var (pV p "drep2" DRepR No)
    drep2a = Var (pV p "drep2a" DRepR No)
    drep2b = Var (pV p "drep2b" DRepR No)

certsStage ::
  Reflect era =>
  UnivSize ->
  Proof era ->
  Subst era ->
  Gen (Subst era)
certsStage us proof subst0 = do
  let preds = certsPreds us proof
  toolChainSub proof standardOrderInfo preds subst0

demo :: ReplMode -> Int -> IO ()
demo mode seed = do
  let proof =
        Conway Standard
  -- Babbage Standard
  env <-
    generateWithSeed
      seed
      ( pure emptySubst
          >>= pParamsStage proof
          >>= universeStage def proof
          >>= vstateStage proof
          >>= pstateStage proof
          >>= dstateStage proof
          >>= certsStage def proof
          >>= (\subst -> monadTyped (substToEnv subst emptyEnv))
      )
  certsv <- monadTyped (findVar (unVar certs) env)
  when (mode == Interactive) $ putStrLn (show (ppList (\(TxCertF _ x) -> pcTxCert proof x) certsv))
  modeRepl mode proof env ""

demoTest :: TestTree
demoTest = testIO "Testing Certs Stage" (demo CI 99)

main :: Int -> IO ()
main n = defaultMain $ testIO "Testing Certs Stage" (demo Interactive n)

-- ========================================

sMaybeL :: Lens' (StrictMaybe a) (Maybe a)
sMaybeL = lens getter setter
  where
    getter (SJust x) = Just x
    getter SNothing = Nothing
    setter _ Nothing = SNothing
    setter _ (Just x) = SJust x

maybeSL :: Lens' (Maybe a) (StrictMaybe a)
maybeSL = lens getter setter
  where
    getter (Just x) = SJust x
    getter Nothing = SNothing
    setter _ SNothing = Nothing
    setter _ (SJust x) = Just x

poolMetaL :: Lens' (PoolParams era) (StrictMaybe PoolMetadata)
poolMetaL = lens ppMetadata (\x r -> x {ppMetadata = r})

poolMetadata :: Era era => Proof era -> Term era (Maybe PoolMetadata)
poolMetadata p = Var (pV p "poolMetadata" (MaybeR (PoolMetadataR p)) (Yes PoolParamsR (poolMetaL . sMaybeL)))
