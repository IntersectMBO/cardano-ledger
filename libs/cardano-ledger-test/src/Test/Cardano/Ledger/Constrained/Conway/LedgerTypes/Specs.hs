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

import Cardano.Ledger.Api
import Cardano.Ledger.BaseTypes hiding (inject)
import Cardano.Ledger.Coin (Coin (..))
import Cardano.Ledger.Compactible (CompactForm (..))
import Cardano.Ledger.Conway.Rules
import Cardano.Ledger.Conway.State
import Cardano.Ledger.Credential (Credential (..))
import Cardano.Ledger.Keys (KeyHash, KeyRole (..))
import Cardano.Ledger.PoolParams (PoolParams (..))
import Cardano.Ledger.Shelley.LedgerState (
  EpochState (..),
  LedgerState (..),
  NewEpochState (..),
  StashedAVVMAddresses,
  UTxOState (..),
  lsCertStateL,
 )
import Constrained.API
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Set (Set)
import qualified Data.Set as Set
import Data.VMap (VB, VMap, VP)
import qualified Data.VMap as VMap
import Lens.Micro ((^.))
import System.IO.Unsafe (unsafePerformIO)
import Test.Cardano.Ledger.Constrained.Conway.Deleg (
  conwayAccountsSpec,
  dRepDelegationsSpec,
  stakePoolDelegationsSpec,
 )
import Test.Cardano.Ledger.Constrained.Conway.Gov (govProposalsSpec)
import Test.Cardano.Ledger.Constrained.Conway.Instances
import Test.Cardano.Ledger.Constrained.Conway.ParametricSpec (
  EraSpecTxOut (..),
  txOutSpec,
 )
import Test.Cardano.Ledger.Constrained.Conway.WitnessUniverse
import Test.QuickCheck hiding (forAll, witness)

-- ===========================================================

-- | The class (EraSpecLedger era) supports Era parametric Specs over types that appear in the Cardano Ledger
--   It uses methods (see Test.Cardano.Ledger.Constrained.Conway.ParametricSpec)
--   that navigate the differences in types parameterized by 'era' that are
--   embeded as type Families in types that appear in the Cardano Ledger Types.
--   It is these components that change from one Era to another.
--   and the EraSpecLedger class has methods that asbtract over those changes.
class
  ( EraSpecTxOut era
  , EraStake era
  , HasSpec (Accounts era)
  , HasSpec (GovState era)
  , HasSpec (CertState era)
  , IsNormalType (Accounts era)
  , IsNormalType (CertState era)
  , EraCertState era
  ) =>
  EraSpecLedger era
  where
  govStateSpec :: PParams era -> Specification (GovState era)
  newEpochStateSpec :: PParams era -> WitUniv era -> Specification (NewEpochState era)
  certStateSpec ::
    WitUniv era -> Term ChainAccountState -> Term EpochNo -> Specification (CertState era)

instance EraSpecLedger ShelleyEra where
  govStateSpec = shelleyGovStateSpec
  newEpochStateSpec = newEpochStateSpecUTxO
  certStateSpec = shelleyCertStateSpec

instance EraSpecLedger AllegraEra where
  govStateSpec = shelleyGovStateSpec
  newEpochStateSpec = newEpochStateSpecUnit
  certStateSpec = shelleyCertStateSpec

instance EraSpecLedger MaryEra where
  govStateSpec = shelleyGovStateSpec
  newEpochStateSpec = newEpochStateSpecUnit
  certStateSpec = shelleyCertStateSpec

instance EraSpecLedger AlonzoEra where
  govStateSpec = shelleyGovStateSpec
  newEpochStateSpec = newEpochStateSpecUnit
  certStateSpec = shelleyCertStateSpec

instance EraSpecLedger BabbageEra where
  govStateSpec = shelleyGovStateSpec
  newEpochStateSpec = newEpochStateSpecUnit
  certStateSpec = shelleyCertStateSpec

instance EraSpecLedger ConwayEra where
  govStateSpec pp = conwayGovStateSpec pp (testGovEnv pp)
  newEpochStateSpec = newEpochStateSpecUnit
  certStateSpec univ _ = conwayCertStateSpec univ

-- This is a hack, neccessitated by the fact that conwayGovStateSpec,
-- written for the conformance tests, requires an actual GovEnv as an input.
-- This is a very large Specification (500 lines), so we don't want to redo it.
-- The only important part of the GovEnv is that the embedded PParams matches
-- the PParams passed to conwayGovStateSpec.
testGovEnv :: PParams ConwayEra -> GovEnv ConwayEra
testGovEnv pp = unsafePerformIO $ generate $ do
  genFromSpec @(GovEnv ConwayEra) (govEnvSpec pp)

-- ================================================================================
-- Specifications for types that appear in the EraSpecLedger Ledger
-- the functions  exampleXX :: IO () (or IO Bool) visualize a test run. They are crcuial
-- to eyeballing that the spes are working as expected. These are a tool that we expect
-- users writing their own specs can emulate.
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
  And
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
  WitUniv era ->
  Specification
    ( Map
        (Credential 'DRepRole)
        (Set.Set (Credential 'Staking))
    )
goodDrep univ =
  constrained $ \dRepMap ->
    [ forAll dRepMap $ \pair ->
        [ satisfies (fst_ pair) (witCredSpec @era univ)
        , satisfies (snd_ pair) (hasSize (rangeSize 1 5))
        , forAll (snd_ pair) (`satisfies` (witCredSpec @era univ))
        ]
    , satisfies dRepMap (hasSize (rangeSize 6 10))
    ]

-- ========================================================================
-- The CertState specs
-- ========================================================================

-- | BE SURE the parameter
--   delegated :: Term (Map (Credential 'DRepRole c) (Set (Credential 'Staking c))
--   has been witnessed with the same WitUniv as the parameter 'univ', or this will fail
--   For a standalone test of vstateSpec one may use goodDrep above, and pass
--   'eraUniv' as the actual parameter for the formal parameter 'univ'
--   Note, that in certStateSpec, the call to vstateSpec is passed a witnessed 'delegated'
--   that comes from the dstateSpec.
vStateSpec ::
  forall era.
  Era era =>
  WitUniv era ->
  Term EpochNo ->
  Term (Map (Credential 'DRepRole) (Set (Credential 'Staking))) ->
  Specification (VState era)
vStateSpec univ epoch delegated = constrained $ \ [var|vstate|] ->
  match vstate $ \ [var|dreps|] [var|comstate|] [var|numdormant|] ->
    [ dependsOn dreps delegated
    , witness univ (dom_ dreps)
    , assert $ dom_ dreps ==. dom_ delegated
    , forAll dreps $ \ [var|pair|] ->
        match pair $ \ [var|drep|] [var|drepstate|] ->
          [ satisfies drep (witCredSpec univ)
          , match drepstate $ \ [var|expiry|] _anchor [var|drepDdeposit|] [var|delegs|] ->
              onJust (lookup_ drep delegated) $ \ [var|delegSet|] ->
                [ assertExplain (pure "all delegatees have delegated") $ delegs ==. delegSet
                , witness univ delegSet
                , assertExplain (pure "epoch of expiration must follow the current epoch") $ epoch <=. expiry
                , assertExplain (pure "no deposit is 0") $ lit (Coin 0) <=. drepDdeposit
                ]
          ]
    , assertExplain
        (pure "num dormant epochs should not be too large")
        [epoch <=. numdormant, numdormant <=. epoch + lit (EpochNo 10)]
    , dependsOn numdormant epoch -- Solve epoch first.
    , match comstate $ \ [var|commap|] ->
        [witness univ (dom_ commap), satisfies commap (hasSize (rangeSize 1 4))]
    ]

-- | Compute the map of DReps, to those that delegate to them,
--   from the delegation map (Map (Credential 'Staking) Drep) which is stored in the DState
--   This ensures that every staking Credential, delegates to exactly one DRep.
aggregateDRep ::
  Map (Credential 'Staking) DRep ->
  Map (Credential 'DRepRole) (Set (Credential 'Staking))
aggregateDRep m = Map.foldlWithKey accum Map.empty m
  where
    accum ans cred (DRepKeyHash kh) = Map.insertWith Set.union (KeyHashObj kh) (Set.singleton cred) ans
    accum ans cred (DRepScriptHash sh) = Map.insertWith Set.union (ScriptHashObj sh) (Set.singleton cred) ans
    accum ans _ _ = ans

shelleyDStateSpec ::
  forall era.
  (EraSpecLedger era, Accounts era ~ ShelleyAccounts era) =>
  WitUniv era ->
  Term ChainAccountState ->
  Term (Map (Credential 'Staking) (KeyHash 'StakePool)) ->
  Specification (DState era)
shelleyDStateSpec  = undefined


conwayDStateSpec ::
  forall era.
  (EraSpecLedger era, Accounts era ~ ConwayAccounts era) =>
  WitUniv era ->
  Term (Map (Credential 'Staking) (KeyHash 'StakePool)) ->
  Term (Map (Credential 'Staking) DRep) ->
  Specification (DState era)
conwayDStateSpec univ stakePoolDelegations dRepDelegations =
  constrained $ \ [var| ds |] ->
    match ds $ \ [var|accounts|] [var|futureGenDelegs|] [var|genDelegs|] [var|irewards|] ->
      [ satisfies accounts (conwayAccountsSpec univ stakePoolDelegations dRepDelegations)
      , -- futureGenDelegs
        assert $ sizeOf_ futureGenDelegs ==. 0
      , -- genDelegs
        match genDelegs $ \gd -> [assert $ sizeOf_ gd ==. 0]
      , -- irewards
        match irewards $ \w x y z -> [sizeOf_ w ==. 0, sizeOf_ x ==. 0, y ==. lit mempty, z ==. lit mempty]
      ]

epochNoSpec :: Specification EpochNo
epochNoSpec = constrained $ \epoch -> epoch >=. 99

pStateSpec ::
  Era era =>
  WitUniv era ->
  Term (Map (Credential 'Staking) (KeyHash 'StakePool)) ->
  Term EpochNo ->
  Specification (PState era)
pStateSpec univ stakePoolDelegations curEpoch = constrained $ \ [var|pState|] ->
  match pState $ \ [var|stakePoolParams|] [var|futureStakePoolParams|] [var|retiring|] [var|pooldeposits|] ->
    [ witness univ (dom_ stakePoolParams)
    , witness univ (rng_ stakePoolParams)
    , witness univ (dom_ futureStakePoolParams)
    , witness univ (rng_ futureStakePoolParams)
    , witness univ (dom_ retiring)
    , witness univ (dom_ pooldeposits)
    , reify stakePoolDelegations (Set.fromList . Map.elems) $ \ [var|delegatedStakePool|] ->
        delegatedStakePool `subset_` dom_ stakePoolParams
    , assertExplain (pure "dom of retiring is a subset of dom of stakePoolParams") $
        dom_ retiring `subset_` dom_ stakePoolParams
    , assertExplain (pure "dom of deposits is dom of stakePoolParams") $
        dom_ pooldeposits ==. dom_ stakePoolParams
    , assertExplain (pure "no deposit is 0") $
        not_ $
          lit (Coin 0) `elem_` rng_ pooldeposits
    , assertExplain (pure "dom of stakePoolParams is disjoint from futureStakePoolParams") $
        dom_ stakePoolParams `disjoint_` dom_ futureStakePoolParams
    , assertExplain (pure "retiring after current epoch") $
        forAll (rng_ retiring) (\ [var|epoch|] -> curEpoch <=. epoch)
    , assert $ sizeOf_ (dom_ futureStakePoolParams) <=. 4
    , assert $ 3 <=. sizeOf_ (dom_ stakePoolParams)
    , assert $ sizeOf_ (dom_ stakePoolParams) <=. 8
    -- TODO: restrict majority of reward accounts in PoolParams to be present in dom stakePoolDelegations
    ]

accountStateSpec :: Specification ChainAccountState
accountStateSpec =
  constrained
    ( \ [var|accountState|] ->
        match
          accountState
          (\ [var|reserves|] [var|treasury|] -> [lit (Coin 100) <=. treasury, lit (Coin 100) <=. reserves])
    )

-- | The CertState spec
--   Note, that in order to be self consistent, parts of the pState is passed as an argument
--   the spec for DState spec (every stake delegation is to a registered pool)
--   and parts of the DState are passed as an argument to the spec for VState
--   (every voting delegation is to a registered DRep)
shelleyCertStateSpec ::
  forall era.
  EraSpecLedger era =>
  WitUniv era ->
  Term ChainAccountState ->
  Term EpochNo ->
  Specification (ShelleyCertState era)
shelleyCertStateSpec -- univ acct epoch = constrained $ \ [var|shelleyCertState|] ->
-- match shelleyCertState $ \ [var|pState|] [var|dState|] ->
--   [ satisfies pState (pStateSpec univ epoch)
--   , reify pState psStakePoolParams $ \ [var|poolreg|] ->
--       [ dependsOn dState poolreg
--       , satisfies dState (shelleyDStateSpec univ acct poolreg)
--       ]
--   ]
-- where
--   shelleyDStateSpec
  =
  error "Unimplemented"

conwayCertStateSpec ::
  EraSpecLedger ConwayEra =>
  WitUniv ConwayEra ->
  Term EpochNo ->
  Specification (ConwayCertState ConwayEra)
conwayCertStateSpec univ epoch = constrained $ \ [var|certState|] ->
  unsafeExists $ \ [var|stakePoolDelegations|] ->
    unsafeExists $ \ [var|dRepDelegations|] ->
      [ satisfies stakePoolDelegations (stakePoolDelegationsSpec univ)
      , satisfies dRepDelegations (dRepDelegationsSpec univ)
      , match certState $ \ [var|vState|] [var|pState|] [var|dState|] ->
          [ satisfies pState (pStateSpec univ stakePoolDelegations epoch)
          , satisfies dState (conwayDStateSpec univ stakePoolDelegations dRepDelegations)
          , reify dRepDelegations aggregateDRep $ \ [var|dRepsDelegatees|] ->
              satisfies vState (vStateSpec univ epoch dRepsDelegatees)
          ]
      ]

-- ==============================================================
-- Specs for UTxO and UTxOState
-- ==============================================================

utxoSpecWit ::
  forall era.
  -- EraSpecLedger era =>
  EraSpecTxOut era =>
  WitUniv era ->
  Term (Map (Credential 'Staking) (KeyHash 'StakePool)) ->
  Specification (UTxO era)
utxoSpecWit univ delegs = constrained $ \ [var|utxo|] ->
  match utxo $ \ [var|utxomap|] ->
    [ forAll (rng_ utxomap) (\ [var|out|] -> txOutSpec univ delegs out)
    ]

utxoStateSpec ::
  forall era.
  (EraSpecLedger era, HasSpec (InstantStake era)) =>
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
      , satisfies gov (govStateSpec @era pp)
      , reify utxo (`addInstantStake` mempty) (\ [var|i|] -> distr ==. i)
      ]

getDelegs ::
  forall era.
  EraCertState era =>
  CertState era ->
  Map (Credential 'Staking) (KeyHash 'StakePool)
getDelegs cs =
  Map.mapMaybe
    (^. stakePoolDelegationAccountStateL)
    (cs ^. certDStateL . accountsL . accountsMapL)

-- ====================================================================
-- Specs for LedgerState
-- ====================================================================

shelleyGovStateSpec ::
  forall era. EraSpecLedger era => PParams era -> Specification (ShelleyGovState era)
shelleyGovStateSpec pp =
  constrained $ \ [var|shellGovState|] ->
    match shellGovState $ \ [var|curpro|] [var|futpro|] [var|curpp|] _prevpp _futpp ->
      match curpro $ \ [var|cm|] ->
        [ satisfies cm (hasSize (rangeSize 1 2))
        , match futpro $ \ [var|fm|] -> satisfies fm (hasSize (rangeSize 1 2))
        , assert $ curpp ==. lit pp
        -- FIXME -- match _futpp (\ fpp -> canFollow (protocolVersion_ fpp) (protocolVersion_ curpp))
        ]

govEnvSpec ::
  PParams ConwayEra ->
  Specification (GovEnv ConwayEra)
govEnvSpec pp = constrained $ \ [var|govEnv|] ->
  match govEnv $ \_ _ [var|cppx|] _ _ _ -> [assert $ lit pp ==. cppx]

conwayGovStateSpec ::
  EraSpecLedger ConwayEra =>
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

-- =========================================================================

ledgerStateSpec ::
  forall era.
  (EraSpecLedger era, HasSpec (InstantStake era)) =>
  PParams era ->
  WitUniv era ->
  Term ChainAccountState ->
  Term EpochNo ->
  Specification (LedgerState era)
ledgerStateSpec pp univ acct epoch =
  constrained $ \ [var|ledgerState|] ->
    match ledgerState $ \ [var|utxoS|] [var|csg|] ->
      [ satisfies csg (certStateSpec @era univ acct epoch)
      , reify csg id (\ [var|certstate|] -> satisfies utxoS (utxoStateSpec @era pp univ certstate))
      ]

-- ===========================================================

-- TODO make this more realistic
snapShotSpec :: Specification SnapShot
snapShotSpec =
  constrained $ \ [var|snap|] ->
    match snap $ \ [var|stake|] [var|delegs|] [var|poolparams|] ->
      match stake $ \ [var|stakemap|] ->
        [ assert $ stakemap ==. lit VMap.empty
        , assert $ delegs ==. lit VMap.empty
        , assert $ poolparams ==. lit VMap.empty
        ]

snapShotsSpec ::
  Term SnapShot -> Specification SnapShots
snapShotsSpec marksnap =
  constrained $ \ [var|snap|] ->
    match snap $ \ [var|mark|] [var|pooldistr|] [var|set|] [var|go|] _fee ->
      And
        [ assert $ mark ==. marksnap
        , satisfies set snapShotSpec
        , satisfies go snapShotSpec
        , reify marksnap calculatePoolDistr $ \ [var|pd|] -> pooldistr ==. pd
        ]

-- | The Mark SnapShot (at the epochboundary) is a pure function of the LedgerState
getMarkSnapShot :: forall era. (EraCertState era, EraStake era) => LedgerState era -> SnapShot
getMarkSnapShot ls = SnapShot (Stake markStake) markDelegations markPoolParams
  where
    markStake :: VMap VB VP (Credential 'Staking) (CompactForm Coin)
    markStake = VMap.fromMap (ls ^. instantStakeL . instantStakeCredentialsL)
    markDelegations :: VMap VB VB (Credential 'Staking) (KeyHash 'StakePool)
    markDelegations = VMap.fromMap $ getDelegs (ls ^. lsCertStateL)
    markPoolParams :: VMap VB VB (KeyHash 'StakePool) PoolParams
    markPoolParams = VMap.fromMap (psStakePoolParams (ls ^. lsCertStateL . certPStateL))

-- ====================================================================
-- Specs for EpochState and NewEpochState
-- ====================================================================

epochStateSpec ::
  forall era.
  (EraSpecLedger era, HasSpec (InstantStake era)) =>
  PParams era ->
  WitUniv era ->
  Term EpochNo ->
  Specification (EpochState era)
epochStateSpec pp univ epoch =
  constrained $ \ [var|epochState|] ->
    match epochState $ \ [var|acctst|] [var|eLedgerState|] [var|snaps|] [var|nonmyopic|] ->
      And
        [ dependsOn eLedgerState acctst
        , satisfies eLedgerState (ledgerStateSpec pp univ acctst epoch)
        , reify eLedgerState getMarkSnapShot $ \ [var|marksnap|] -> satisfies snaps (snapShotsSpec marksnap)
        , match nonmyopic $ \ [var|x|] [var|c|] -> [genHint 0 x, assert $ c ==. lit (Coin 0)]
        ]

getPoolDistr :: forall era. EpochState era -> PoolDistr
getPoolDistr es = ssStakeMarkPoolDistr (esSnapshots es)

-- | Used for Eras where StashedAVVMAddresses era ~ UTxO era (Shelley)
-- The 'newEpochStateSpec' method (of (EraSpecLedger) class) in the Shelley instance
newEpochStateSpecUTxO ::
  forall era.
  ( EraSpecLedger era
  , HasSpec (InstantStake era)
  , StashedAVVMAddresses era ~ UTxO era
  ) =>
  PParams era ->
  WitUniv era ->
  Specification (NewEpochState era)
newEpochStateSpecUTxO pp univ =
  constrained
    ( \ [var|newEpochStateUTxO|] ->
        match
          (newEpochStateUTxO :: Term (NewEpochState era))
          ( \ [var|eno|] [var|blocksPrev|] [var|blocksCurr|] [var|epochstate|] _mpulser [var|pooldistr|] [var|stashAvvm|] ->
              And
                [ -- reify eno id (\ [var|epoch|] -> satisfies epochstate (epochStateSpec @era pp epoch))
                  -- dependsOn eno epochstate
                  satisfies epochstate (epochStateSpec @era pp univ eno)
                , satisfies stashAvvm (constrained (\ [var|u|] -> u ==. lit (UTxO @era Map.empty)))
                , reify epochstate getPoolDistr $ \ [var|pd|] -> pooldistr ==. pd
                , match blocksPrev (genHint 3)
                , match blocksCurr (genHint 3)
                ]
          )
    )

-- | Used for Eras where StashedAVVMAddresses era ~ () (Allegra,Mary,Alonzo,Babbage,Conway)
-- The 'newEpochStateSpec' method (of (EraSpecLedger era) class) in the instances for (Allegra,Mary,Alonzo,Babbage,Conway)
newEpochStateSpecUnit ::
  forall era.
  ( EraSpecLedger era
  , HasSpec (InstantStake era)
  , StashedAVVMAddresses era ~ ()
  ) =>
  PParams era ->
  WitUniv era ->
  Specification (NewEpochState era)
newEpochStateSpecUnit pp univ =
  constrained
    ( \ [var|newEpochStateUnit|] ->
        match
          (newEpochStateUnit :: Term (NewEpochState era))
          ( \ [var|eno|] [var|blocksPrev|] [var|blocksCurr|] [var|epochstate|] _mpulser [var|pooldistr|] [var|stashAvvm|] ->
              And
                [ satisfies epochstate (epochStateSpec @era pp univ eno)
                , satisfies stashAvvm (constrained (\ [var|x|] -> x ==. lit ()))
                , reify epochstate getPoolDistr $ \ [var|pd|] -> pooldistr ==. pd
                , match blocksPrev (genHint 3)
                , match blocksCurr (genHint 3)
                ]
          )
    )
