{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE UndecidableSuperClasses #-}
{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_GHC -O0 #-}
{-# OPTIONS_GHC -Wno-orphans #-}

-- | Specs necessary to generate constrained (well formed) values of
--   types that appear in the Cardano Ledger Types. These Specifications
--   are Era parametric, and one can use them to generate well formed
--   values in any era (Shelley,Allegra,Mary,Alonzo,Babbage,Conway)
--   by type applying them to a particular era type. These specifications
--   are a usefull guide to building ones own specifications with one's own
--   idea of whats well formed.
module Test.Cardano.Ledger.Constrained.Conway.LedgerTypes.Specs where

import Cardano.Ledger.Address (accountAddressCredentialL)
import Cardano.Ledger.Api
import Cardano.Ledger.BaseTypes hiding (inject)
import Cardano.Ledger.Coin (
  Coin (..),
  CompactForm (..),
  DeltaCoin (..),
  compactCoinOrError,
  knownNonZeroCoin,
 )
import Cardano.Ledger.Conway.Rules
import Cardano.Ledger.Conway.State
import Cardano.Ledger.Credential (Credential (..))
import Cardano.Ledger.Keys (KeyHash, KeyRole (..))
import Cardano.Ledger.Shelley.LedgerState (
  EpochState (..),
  LedgerState (..),
  NewEpochState (..),
  UTxOState (..),
  lsCertStateL,
 )
import Constrained.API
import Data.Foldable
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Set (Set)
import qualified Data.Set as Set
import Data.VMap (VB, VMap, VP)
import qualified Data.VMap as VMap
import Lens.Micro ((^.))
import System.IO.Unsafe (unsafePerformIO)
import Test.Cardano.Ledger.Constrained.Conway.Deleg (isKeyHash)
import Test.Cardano.Ledger.Constrained.Conway.Gov (govProposalsSpec)
import Test.Cardano.Ledger.Constrained.Conway.Instances
import Test.Cardano.Ledger.Constrained.Conway.Instances.Basic ()
import Test.Cardano.Ledger.Constrained.Conway.ParametricSpec (
  EraSpecTxOut (..),
  txOutSpec,
 )
import Test.Cardano.Ledger.Constrained.Conway.WitnessUniverse
import Test.Cardano.Ledger.Shelley.Rewards (mkSnapShot)
import Test.QuickCheck hiding (forAll, witness)

-- ======================================================================================
-- CERTS Contexts
-- The WhoDelegates type, Withdrawals,
-- and the necessary relationships between parts of the CertState
-- ======================================================================================

type WhoDelegates = Map (Credential DRepRole) (Set (Credential Staking))

-- | In the Coway Era, the relationship between the Stake delegation and DRep delegation is
--   quite subtle. The best way to get this right during constrained generation, is to generate
--   one piece, we call `WhoDelegates` and compute the other pieces from that.
--   type WhoDelegates = Map (Credential DRepRole) (Set (Credential Staking)
--   The functions `delegatesTo`, `dRepsOf`, `credToDRep`, `delegators`, help compute the other pieces.
--   One can observe that the VState and ConwayAccounts, both have Maps whose domain come from the
--   domain of WhoDelegates and these maps have strong invariants with each other.
--   We choose the type WhoDelegates because two key components of VState and ConwayAccounts have
--   related fields that must agree in many subtle ways. The most important way in that the the
--   domain of both the maps can be computed from WhoDelegates.
--   VState {vsDReps :: !(Map (Credential DRepRole) DRepState) ..}
--   ConwayAccounts :: {caStates :: Map(Credential Staking) (ConwayAccountState era)}
--   Map.keysSet vsDReps == delegatesTo onepiece
--   Map.keysSet caStates == delegators onepiece
--   Other more subtle agreement come from fields inside DRepState and ConwayAccountState
--   All of these are captured in the Specification for VState and DState (which contains the Account map)
--   One other thing we need to consider is: newtype Withdrawals = Withdrawals {unWithdrawals :: Map AccountAddress Coin}
--   WithDrawals come from Transactions, but they have subtle interactions with rewards map and the Drep delegation map
--   So we give special Specifcations that will generate both in ways that maintain the important relationships.
whoDelegatesSpec ::
  forall era.
  Era era =>
  WitUniv era ->
  Specification (Map (Credential DRepRole) (Set (Credential Staking)))
whoDelegatesSpec univ = constrained $ \m ->
  [ assert $ sizeOf_ m >=. 10
  , assert $ sizeOf_ m <=. 20
  , witness univ (dom_ m)
  , witness univ (rng_ m)
  ]

wdrlSpec ::
  Map (Credential DRepRole) (Set (Credential Staking)) ->
  Specification (Map AccountAddress Coin)
wdrlSpec whodelegates = constrained $ \m ->
  [ assert $ sizeOf_ (dom_ m) <=. lit 5
  , forAll' m $ \ [var|accountAddress|] _ ->
      match accountAddress $ \ [var|_network|] [var|credStake|] ->
        [ assert $ _network ==. lit Testnet
        , assert $ member_ credStake (lit (delegators whodelegates))
        ]
  ]

-- | Compute the set of DRep Credentials that some (Credential Staking) delegates to
delegatesTo :: Map (Credential DRepRole) (Set (Credential Staking)) -> Set (Credential DRepRole)
delegatesTo m = Map.keysSet m

-- | Compute the set of (Credential Staking) that delegate their vote to some DRep
delegators :: Map (Credential DRepRole) (Set (Credential Staking)) -> Set (Credential Staking)
delegators m = fold (Map.elems m)

-- | Compute the set of DReps that some (Credential Staking) delegates to
--   Like `delegatesTo` but returns the DRep rather than the (Credential DRepRole)
dRepsOf :: Map (Credential DRepRole) (Set (Credential Staking)) -> Set DRep
dRepsOf m = Set.map credToDRep (delegatesTo m)

-- | Turn a DRep Credential into a DRep
credToDRep :: Credential DRepRole -> DRep
credToDRep (KeyHashObj kh) = DRepKeyHash kh
credToDRep (ScriptHashObj sh) = DRepScriptHash sh

toDelta :: Coin -> DeltaCoin
toDelta (Coin n) = DeltaCoin n

type CertContext = (Map (Credential DRepRole) (Set (Credential Staking)), Map AccountAddress Coin)

genCertContext :: forall era. Era era => WitUniv era -> Gen CertContext
genCertContext univ = do
  whodelegates <- genFromSpec (whoDelegatesSpec univ)
  wdrl <- genFromSpec (wdrlSpec whodelegates)
  pure (whodelegates, wdrl)

-- This is a hack, neccessitated by the fact that conwayGovStateSpec,
-- written for the conformance tests, requires an actual GovEnv as an input.
-- This is a very large Specification (500 lines), so we don't want to redo it.
-- The only important part of the GovEnv is that the embedded PParams matches
-- the PParams passed to conwayGovStateSpec.
testGovEnv :: PParams ConwayEra -> GovEnv ConwayEra
testGovEnv pp = unsafePerformIO $ generate $ do
  genFromSpec @(GovEnv ConwayEra) (govEnvSpec pp)

govEnvSpec ::
  PParams ConwayEra ->
  Specification (GovEnv ConwayEra)
govEnvSpec pp = constrained $ \ [var|govEnv|] ->
  match govEnv $ \_ _ [var|cppx|] _ _ _ -> [assert $ lit pp ==. cppx]

-- ================================================================================
-- Specifications for types that appear in the EraSpecLedger Ledger
-- ================================================================================

-- | Want (Rng v3) == (Dom v0), except the Rng is List and the Dom is a Set.
domEqualRng ::
  ( Ord ptr
  , Ord cred
  , HasSpec cred
  , HasSpec ptr
  , HasSpec ume
  , IsNormalType cred
  , IsNormalType ptr
  , IsNormalType ume
  ) =>
  Term (Map ptr cred) ->
  Term (Map cred ume) ->
  Pred
domEqualRng [var|mapXCred|] [var|mapCredY|] =
  fold
    [ assert $ sizeOf_ mapCredY <=. sizeOf_ mapXCred
    , assert $ sizeOf_ mapXCred >=. lit 0
    , assert $ sizeOf_ mapCredY >=. lit 0
    , assertExplain (pure "Domain mapCredX == Range  mapXCred") $
        [dependsOn mapCredY mapXCred, assert $ dom_ mapCredY ==. fromList_ (rng_ mapXCred)]
    ]

-- | The constraint for ProtVer always relates one ProtVer to another one that can follow it.
canFollow :: Term ProtVer -> Term ProtVer -> Pred
canFollow pv pv' =
  match pv $ \ [var|major1|] [var|minor1|] ->
    match pv' $ \ [var|major2|] [var|minor2|] ->
      [ dependsOn major2 major1
      , assert $ major1 <=. major2
      , ifElse
          (lit maxBound ==. major1)
          (major1 ==. major2)
          (succV_ major1 >=. major2)
      , ifElse
          (major1 ==. major2)
          (minor2 ==. minor1 + 1)
          (minor2 ==. 0)
      ]

protVersCanfollow :: Specification (ProtVer, ProtVer)
protVersCanfollow =
  constrained $ \ [var|pair|] ->
    match pair $ \ [var|protver1|] [var|protver2|] -> canFollow protver1 protver2

epochNoSpec :: Specification EpochNo
epochNoSpec = constrained $ \epoch -> epoch >=. 99

accountStateSpec :: Specification ChainAccountState
accountStateSpec =
  constrained
    ( \ [var|accountState|] ->
        match
          accountState
          (\ [var|reserves|] [var|treasury|] -> [lit (Coin 100) <=. treasury, lit (Coin 100) <=. reserves])
    )

-- ====================================================================
-- To generate a standalone VState using vstateSpec, we need a map of DRep credentials,
-- to a set of Credentials of Stakers who delegated to that DRep. If we use a completely
-- random map, it is highly likeley (almost a certainty) that the map
-- does not have witnessed credentials. So we need a spec that generates
-- a map with witnessed credentials. In a non standalone use of vstateSpec, the map
-- is computed as the inverse of the DStates (Map Credential DRep) which has
-- witnessed credentials, so we need this only for StandAlone uses of vstateSpec.

goodDrep ::
  forall era.
  Era era =>
  WitUniv era ->
  Specification (Map (Credential DRepRole) (Set.Set (Credential Staking)))
goodDrep = whoDelegatesSpec

-- ========================================================================
-- The CertState specs
-- ========================================================================

-- ======= VState ==================

-- | BE SURE the parameter whoDelegated has been witnessed with the same WitUniv as the parameter 'univ'.
vStateSpec ::
  forall era.
  Era era =>
  WitUniv era ->
  Term EpochNo ->
  Map (Credential DRepRole) (Set (Credential Staking)) ->
  Specification (VState era)
vStateSpec univ epoch whoDelegated = constrained $ \ [var|vstate|] ->
  match vstate $ \ [var|dreps|] [var|committeestate|] [var|numdormant|] ->
    [ witness univ (dom_ dreps)
    , assert $ dom_ dreps ==. lit (delegatesTo whoDelegated)
    , forAll dreps $ \ [var|pair|] ->
        match pair $ \ [var|drep|] [var|drepstate|] ->
          [ satisfies drep (witCredSpec univ)
          , match drepstate $ \ [var|expiry|] _anchor [var|drepDeposit'|] [var|delegs|] ->
              onJust (lookup_ drep (lit whoDelegated)) $ \ [var|delegSet|] ->
                [ assertExplain (pure "all delegatees have delegated") $ delegs ==. delegSet
                , witness univ delegSet
                , assertExplain (pure "epoch of expiration must follow the current epoch") $ epoch <=. expiry
                , assertExplain (pure "no deposit is 0") $ match drepDeposit' (lit 0 <=.)
                ]
          ]
    , assertExplain (pure "num dormant epochs should not be too large") $
        [epoch <=. numdormant, numdormant <=. epoch + lit (EpochNo 10)]
    , dependsOn numdormant epoch -- Solve epoch first.
    , match committeestate $ \ [var|committeemap|] ->
        [witness univ (dom_ committeemap), satisfies committeemap (hasSize (rangeSize 1 4))]
    ]

-- ======= DState ==================

conwayDStateSpec ::
  forall era.
  era ~ ConwayEra =>
  WitUniv era ->
  (Map (Credential DRepRole) (Set (Credential Staking)), Map AccountAddress Coin) ->
  Term (Map (KeyHash StakePool) StakePoolState) ->
  Specification (DState era)
conwayDStateSpec univ (whoDelegates, wdrl) poolreg =
  constrained $ \ [var| ds |] ->
    match ds $ \ [var|accounts|] [var|futureGenDelegs|] [var|genDelegs|] [var|irewards|] ->
      [ match accounts $ \ [var|accountmap|] -> satisfies accountmap (conwayAccountMapSpec univ whoDelegates poolreg wdrl)
      , -- futureGenDelegs
        assert $ sizeOf_ futureGenDelegs ==. 0
      , -- genDelegs
        match genDelegs $ \gd -> [assert $ sizeOf_ gd ==. 0]
      , -- irewards
        match irewards $ \w x y z -> [sizeOf_ w ==. 0, sizeOf_ x ==. 0, y ==. lit mempty, z ==. lit mempty]
      ]

-- | Specify the internal Map of ConwayAccounts ::  Map (Credential Staking) (ConwayAccountState era)
--   Ensure that the Staking Credential is both staked to some Pool, and Delegated to some DRep
-- | Given a set of Withdrawals:: newtype Withdrawals = Withdrawals {unWithdrawals :: Map AccountAddress Coin}
--   where:: data AccountAddress = AccountAddress {aaNetworkId :: !Network, aaAccountId :: !(AccountId)}
--   That ensures every AccountState has the propeties listed to the left
--   data ConwayAccountState era = ConwayAccountState
--       {casBalance :: (CompactForm Coin)                            -- Sometimes 0, Matches the withdrawl amount if part of a Withdrawal
--       ,casDeposit :: (CompactFormCoin)                             -- witnessed
--       ,casStakePoolDelegation :: (StrictMaybe (KeyHash StakePool)) -- The Pool Staked to exists
--       ,casDRepDelegation :: (StrictMaybe DRep)}                    -- The DRep delegated to exists
conwayAccountMapSpec ::
  forall era.
  Era era =>
  WitUniv era ->
  Map (Credential DRepRole) (Set (Credential Staking)) ->
  Term (Map (KeyHash StakePool) StakePoolState) ->
  Map AccountAddress Coin ->
  Specification (Map (Credential Staking) (ConwayAccountState era))
conwayAccountMapSpec univ whoDelegates poolreg wdrl =
  let witsize = fromIntegral @Int (wvSize univ)
      wdlsize = fromIntegral @Int (Map.size wdrl)
      minAccountsize = wdlsize + 2
      withdrawalMap :: Map (Credential Staking) (CompactForm Coin)
      withdrawalMap = Map.mapKeys (^. accountAddressCredentialL) (Map.map compactCoinOrError wdrl)
      withdrawalKeys :: Set (Credential Staking)
      withdrawalKeys = Map.keysSet (Map.mapKeys (^. accountAddressCredentialL) wdrl)
   in constrained $ \ [var|conwayAccountMap|] ->
        [ -- Size of conwayAccounts, can't be bigger than the witness set (n keys + n scripts)
          -- but it must be bigger than the withdrawal size
          if minAccountsize < witsize
            then assert $ sizeOf_ (conwayAccountMap) >=. lit minAccountsize
            else
              assertExplain
                ( pure
                    ("The size of the WitUniv (" ++ show witsize ++ ") is too small to accomodate the Accounts map.")
                )
                False
        , dependsOn conwayAccountMap poolreg
        , assert $ subset_ (lit withdrawalKeys) (dom_ conwayAccountMap)
        , forAll conwayAccountMap $
            \ [var|credAcctStatePair|] -> match credAcctStatePair $ \ [var|cred|] [var|conwayAcctState|] ->
              [ witness univ cred
              , witness univ conwayAcctState
              , match conwayAcctState $ \ [var|bal|] [var|deposit|] [var|mpool|] [var|mdrep|] ->
                  [ dependsOn deposit cred
                  , dependsOn bal cred
                  , satisfies deposit (geqSpec 0)
                  , onCon @"SJust" mpool $ \ [var|khashStakePool|] -> member_ khashStakePool (dom_ poolreg)
                  , reify cred isKeyHash $ \bool -> whenTrue bool [assert $ onCon @"SJust" mdrep $ \x -> member_ x (lit (dRepsOf whoDelegates))]
                  , (caseOn (lookup_ cred (lit withdrawalMap)))
                      -- Nothing
                      ( branch $ \_ ->
                          assertExplain
                            (pure "some reward balances in the ConwayAccountState are zero")
                            ( satisfies
                                bal
                                ( chooseSpec
                                    (1, constrained $ \ [var| x |] -> assert $ x ==. lit 0)
                                    (3, gtSpec 0)
                                )
                            )
                      )
                      -- Just
                      ( branch $ \ [var|withCoin|] -> assertExplain (pure "Reward balance must match Withdrawal amount") (bal ==. withCoin)
                      )
                  , ifElse
                      (member_ cred (lit withdrawalKeys))
                      ( satisfies
                          mdrep
                          ( constrained $ \(x :: Term (StrictMaybe DRep)) ->
                              (caseOn x)
                                -- SNothing
                                (branch $ \_ -> False)
                                -- SJust
                                (branch $ \drep -> member_ drep (lit (dRepsOf whoDelegates)))
                          )
                      )
                      (onCon @"SJust" mdrep $ \ [var|drep|] -> member_ drep (lit (dRepsOf whoDelegates)))
                  ]
              ]
        ]

pStateSpec ::
  Era era =>
  WitUniv era ->
  Term EpochNo ->
  Specification (PState era)
pStateSpec univ currepoch = constrained $ \ [var|pState|] ->
  match pState $ \_ [var|stakePoolParams|] [var|futureStakePoolParams|] [var|retiring|] ->
    [ witness univ (dom_ stakePoolParams)
    , witness univ (rng_ stakePoolParams)
    , witness univ (dom_ futureStakePoolParams)
    , witness univ (rng_ futureStakePoolParams)
    , witness univ (dom_ retiring)
    , assertExplain (pure "dom of retiring is a subset of dom of stakePoolParams") $
        dom_ retiring `subset_` dom_ stakePoolParams
    , forAll' (rng_ stakePoolParams) $ \_ _ _ _ _ _ _ _ [var|d|] _ ->
        assertExplain (pure "all deposits are greater then (Coin 0)") $ d >=. lit 0
    , assertExplain (pure "dom of stakePoolParams is disjoint from futureStakePoolParams") $
        dom_ stakePoolParams `disjoint_` dom_ futureStakePoolParams
    , assertExplain (pure "retiring after current epoch") $
        forAll (rng_ retiring) (\ [var|epoch|] -> currepoch <=. epoch)
    , assert $ sizeOf_ futureStakePoolParams <=. 4
    , assert $ 3 <=. sizeOf_ stakePoolParams
    , assert $ sizeOf_ stakePoolParams <=. 8
    ]

-- ===================== Put PState, DState, and VState together to form a CertState =================

conwayCertStateSpec ::
  WitUniv ConwayEra ->
  (Map (Credential DRepRole) (Set (Credential Staking)), Map AccountAddress Coin) ->
  Term EpochNo ->
  Specification (ConwayCertState ConwayEra)
conwayCertStateSpec univ (whodelegates, wdrl) epoch = constrained $ \ [var|convCertState|] ->
  match convCertState $ \ [var|vState|] [var|pState|] [var|dState|] ->
    [ satisfies pState (pStateSpec univ epoch)
    , reify pState psStakePools $ \ [var|poolreg|] ->
        [ dependsOn dState poolreg
        , satisfies dState (conwayDStateSpec univ (whodelegates, wdrl) poolreg)
        ]
    , satisfies vState (vStateSpec univ epoch whodelegates)
    ]

-- ==============================================================
-- Specs for UTxO and UTxOState
-- ==============================================================

utxoSpecWit ::
  forall era.
  EraSpecTxOut era =>
  WitUniv era ->
  Term (Map (Credential Staking) (KeyHash StakePool)) ->
  Specification (UTxO era)
utxoSpecWit univ delegs = constrained $ \ [var|utxo|] ->
  match utxo $ \ [var|utxomap|] ->
    [ forAll (rng_ utxomap) (\ [var|out|] -> txOutSpec univ delegs out)
    ]

utxoStateSpec ::
  forall era.
  (HasSpec (InstantStake era), era ~ ConwayEra) =>
  PParams era ->
  WitUniv era ->
  Term (CertState era) ->
  Specification (UTxOState era)
utxoStateSpec pp univ certstate =
  constrained $ \ [var|utxoState|] ->
    match utxoState $ \ [var|utxo|] [var|deposits|] [var|fees|] [var|gov|] [var|distr|] [var|donation|] ->
      [ assert $ donation ==. lit (Coin 0)
      , reify
          certstate
          (sumObligation . obligationCertState)
          (\ [var|depositsum|] -> assert $ deposits ==. depositsum)
      , assert $ lit (Coin 0) <=. fees
      , reify certstate getDelegs (\ [var|delegs|] -> satisfies utxo (utxoSpecWit univ delegs))
      , satisfies gov (conwayGovStateSpec pp (testGovEnv pp))
      , reify utxo (`addInstantStake` mempty) (\ [var|i|] -> distr ==. i)
      ]

getDelegs ::
  forall era.
  EraCertState era =>
  CertState era ->
  Map (Credential Staking) (KeyHash StakePool)
getDelegs cs =
  Map.mapMaybe
    (^. stakePoolDelegationAccountStateL)
    (cs ^. certDStateL . accountsL . accountsMapL)

-- ====================================================================
-- Specs for GovState
-- ====================================================================

conwayGovStateSpec ::
  PParams ConwayEra ->
  GovEnv ConwayEra ->
  Specification (ConwayGovState ConwayEra)
conwayGovStateSpec pp govenv =
  constrained $ \ [var|conwaygovstate|] ->
    match conwaygovstate $ \ [var|proposals|] _mcommittee _consti [var|curpp|] _prevpp _futurepp _derepPulstate ->
      [ dependsOn curpp proposals
      , dependsOn conwaygovstate proposals
      , assert $ curpp ==. lit pp
      , satisfies proposals (govProposalsSpec govenv)
      ]

-- ====================================================================
-- Specs for LedgerState
-- ====================================================================

ledgerStateSpec ::
  forall era.
  (HasSpec (InstantStake era), era ~ ConwayEra) =>
  PParams era ->
  WitUniv era ->
  CertContext ->
  Term EpochNo ->
  Specification (LedgerState era)
ledgerStateSpec pp univ ctx epoch =
  constrained $ \ [var|ledgerState|] ->
    match ledgerState $ \ [var|utxoS|] [var|csg|] ->
      [ satisfies csg (conwayCertStateSpec univ ctx epoch)
      , reify csg id (\ [var|certstate|] -> satisfies utxoS (utxoStateSpec @era pp univ certstate))
      ]

-- ===========================================================

-- TODO make this more realistic
snapShotSpec :: Specification SnapShot
snapShotSpec =
  constrained $ \ [var|snap|] ->
    match snap $ \ [var|stake|] [var|totalActiveStake|] [var|delegs|] [var|poolparams|] [var|pools|] ->
      match stake $ \ [var|stakemap|] ->
        [ assert $ stakemap ==. lit VMap.empty
        , assert $ totalActiveStake ==. lit (knownNonZeroCoin @1)
        , assert $ delegs ==. lit VMap.empty
        , assert $ poolparams ==. lit VMap.empty
        , assert $ pools ==. lit Map.empty
        ]

snapShotsSpec ::
  Term SnapShot -> Specification SnapShots
snapShotsSpec marksnap =
  constrained $ \ [var|snap|] ->
    match snap $ \ [var|mark|] [var|pooldistr|] [var|set|] [var|_go|] _fee ->
      [ assert $ mark ==. marksnap
      , satisfies set snapShotSpec
      , satisfies _go snapShotSpec
      , reify marksnap calculatePoolDistr $ \ [var|pd|] -> pooldistr ==. pd
      ]

-- | The Mark SnapShot (at the epochboundary) is a pure function of the LedgerState
getMarkSnapShot :: forall era. (EraCertState era, EraStake era) => LedgerState era -> SnapShot
getMarkSnapShot ls = mkSnapShot (Stake markStake) markDelegations markPoolParams
  where
    markStake :: VMap VB VP (Credential Staking) (CompactForm Coin)
    markStake = VMap.fromMap (ls ^. instantStakeL . instantStakeCredentialsL)
    markDelegations :: VMap VB VB (Credential Staking) (KeyHash StakePool)
    markDelegations = VMap.fromMap $ getDelegs (ls ^. lsCertStateL)
    markPoolParams :: VMap VB VB (KeyHash StakePool) StakePoolParams
    markPoolParams =
      VMap.fromMap $
        Map.mapWithKey (`stakePoolStateToStakePoolParams` Testnet) $
          psStakePools $
            ls ^. lsCertStateL . certPStateL

-- ====================================================================
-- Specs for EpochState and NewEpochState
-- ====================================================================

epochStateSpec ::
  forall era.
  (HasSpec (InstantStake era), era ~ ConwayEra) =>
  PParams era ->
  WitUniv era ->
  CertContext ->
  Term EpochNo ->
  Specification (EpochState era)
epochStateSpec pp univ certctx epoch =
  constrained $ \ [var|epochState|] ->
    match epochState $ \ [var|acctst|] [var|eLedgerState|] [var|snaps|] [var|nonmyopic|] ->
      [ dependsOn eLedgerState acctst
      , satisfies eLedgerState (ledgerStateSpec pp univ certctx epoch)
      , reify eLedgerState getMarkSnapShot $ \ [var|marksnap|] -> satisfies snaps (snapShotsSpec marksnap)
      , match nonmyopic $ \ [var|x|] [var|c|] -> [genHint 0 x, assert $ c ==. lit (Coin 0)]
      ]

getPoolDistr :: forall era. EpochState era -> PoolDistr
getPoolDistr es = ssStakeMarkPoolDistr (esSnapshots es)

-- | Used for Eras where StashedAVVMAddresses era ~ () (Allegra,Mary,Alonzo,Babbage,Conway)
-- The 'newEpochStateSpec' method (of (EraSpecLedger era) class) in the instances for (Allegra,Mary,Alonzo,Babbage,Conway)
newEpochStateSpec ::
  forall era.
  ( HasSpec (InstantStake era)
  , era ~ ConwayEra
  ) =>
  PParams era ->
  WitUniv era ->
  CertContext ->
  Specification (NewEpochState era)
newEpochStateSpec pp univ certctx =
  constrained
    ( \ [var|newEpochState|] ->
        match
          (newEpochState :: Term (NewEpochState era))
          ( \ [var|eno|] [var|blocksPrev|] [var|blocksCurr|] [var|epochstate|] _mpulser [var|pooldistr|] [var|stashAvvm|] ->
              [ satisfies epochstate (epochStateSpec @era pp univ certctx eno)
              , satisfies stashAvvm (constrained (\ [var|x|] -> x ==. lit ()))
              , reify epochstate getPoolDistr $ \ [var|pd|] -> pooldistr ==. pd
              , match blocksPrev (genHint 3)
              , match blocksCurr (genHint 3)
              ]
          )
    )
