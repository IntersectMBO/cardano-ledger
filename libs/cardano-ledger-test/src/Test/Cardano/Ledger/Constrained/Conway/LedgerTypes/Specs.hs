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
{-# LANGUAGE TypeSynonymInstances #-}
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
import Cardano.Ledger.CertState
import Cardano.Ledger.Coin (Coin (..))
import Cardano.Ledger.Conway.Rules
import Cardano.Ledger.Credential (Credential (..))
import Cardano.Ledger.Keys (KeyHash, KeyRole (..))
import Cardano.Ledger.PoolDistr (PoolDistr (..))
import Cardano.Ledger.PoolParams (PoolParams (..))
import Cardano.Ledger.Shelley.LedgerState (
  AccountState (..),
  EpochState (..),
  IncrementalStake (..),
  LedgerState (..),
  NewEpochState (..),
  StashedAVVMAddresses,
  UTxOState (..),
  updateStakeDistribution,
 )
import Cardano.Ledger.State (SnapShot (..), SnapShots (..), Stake (..), UTxO (..), calculatePoolDistr)
import Cardano.Ledger.UMap (CompactForm (..))
import qualified Cardano.Ledger.UMap as UMap
import Constrained hiding (Value)
import Constrained.Base (Pred (..))
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Typeable
import Data.VMap (VB, VMap, VP)
import qualified Data.VMap as VMap
import System.IO.Unsafe (unsafePerformIO)
import Test.Cardano.Ledger.Constrained.Conway.Gov (govProposalsSpec)
import Test.Cardano.Ledger.Constrained.Conway.Instances
import Test.Cardano.Ledger.Constrained.Conway.WitnessUniverse
import Test.QuickCheck hiding (forAll, witness)

-- ===========================================================

-- | The class (EraSpecLedger era) supports Era parametric Specs over types that appear in the Cardano Ledger.223
--   It uses methods (see Test.Cardano.Ledger.Constrained.Conway.ParametricSpec)
--   that navigate the differences in types parameterized by 'era' that are
--   embeded as type Families in types that appear in the Cardano Ledger Types.
--   It is these components that change from one Era to another.
--   and the EraSpecLedger class has methods that asbtract over those changes.
class
  ( EraSpecTxOut era fn
  , Era era
  , HasSpec fn (GovState era)
  ) =>
  EraSpecLedger era fn
  where
  govStateSpec :: PParams era -> Specification fn (GovState era)
  newEpochStateSpec :: PParams era -> WitUniv era -> Specification fn (NewEpochState era)

instance IsConwayUniv fn => EraSpecLedger ShelleyEra fn where
  govStateSpec = shelleyGovStateSpec
  newEpochStateSpec = newEpochStateSpecUTxO

instance IsConwayUniv fn => EraSpecLedger AllegraEra fn where
  govStateSpec = shelleyGovStateSpec
  newEpochStateSpec = newEpochStateSpecUnit

instance IsConwayUniv fn => EraSpecLedger MaryEra fn where
  govStateSpec = shelleyGovStateSpec
  newEpochStateSpec = newEpochStateSpecUnit

instance IsConwayUniv fn => EraSpecLedger AlonzoEra fn where
  govStateSpec = shelleyGovStateSpec
  newEpochStateSpec = newEpochStateSpecUnit

instance IsConwayUniv fn => EraSpecLedger BabbageEra fn where
  govStateSpec = shelleyGovStateSpec
  newEpochStateSpec = newEpochStateSpecUnit

instance IsConwayUniv fn => EraSpecLedger ConwayEra fn where
  govStateSpec pp = conwayGovStateSpec pp (testGovEnv pp)
  newEpochStateSpec = newEpochStateSpecUnit

-- This is a hack, neccessitated by the fact that conwayGovStateSpec,
-- written for the conformance tests, requires an actual GovEnv as an input.
-- This is a very large Specification (500 lines), so we don't want to redo it.
-- The only important part of the GovEnv is that the embedded PParams matches
-- the PParams passed to conwayGovStateSpec.
testGovEnv :: PParams ConwayEra -> GovEnv ConwayEra
testGovEnv pp = unsafePerformIO $ generate $ do
  env <- genFromSpec @ConwayFn @(GovEnv ConwayEra) (govEnvSpec @ConwayFn pp)
  pure env

-- ================================================================================
-- Specifications for types that appear in the EraSpecLedger Ledger
-- the functions  exampleXX :: IO () (or IO Bool) visualize a test run. They are crcuial
-- to eyeballing that the spes are working as expected. These are a tool that we expect
-- users writing their own specs can emulate.
-- ================================================================================

-- | Want (Rng v3) == (Dom v0), except the Rng is List and the Dom is a Set.
domEqualRng ::
  ( IsConwayUniv fn
  , Ord ptr
  , Ord cred
  , HasSpec fn cred
  , HasSpec fn ptr
  , HasSpec fn ume
  ) =>
  Term fn (Map ptr cred) ->
  Term fn (Map cred ume) ->
  Pred fn
domEqualRng [var|mapXCred|] [var|mapCredY|] =
  Block
    [ assert $ sizeOf_ mapCredY <=. sizeOf_ mapXCred
    , assert $ sizeOf_ mapXCred >=. lit 0
    , assert $ sizeOf_ mapCredY >=. lit 0
    , assertExplain (pure "Domain mapCredX == Range  mapXCred") $
        [dependsOn mapCredY mapXCred, assert $ dom_ mapCredY ==. fromList_ (rng_ mapXCred)]
    ]

-- | The constraint for ProtVer always relates one ProtVer to another one that can follow it.
canFollow :: IsConwayUniv fn => Term fn ProtVer -> Term fn ProtVer -> Pred fn
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

protVersCanfollow :: Specification ConwayFn (ProtVer, ProtVer)
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
    ConwayFn
    ( Map
        (Credential 'DRepRole)
        (Set.Set (Credential 'Staking))
    )
goodDrep univ =
  constrained $ \dRepMap ->
    [ forAll dRepMap $ \pair ->
        [ satisfies (fst_ pair) (witCredSpec @ConwayFn @era univ)
        , satisfies (snd_ pair) (hasSize (rangeSize 1 5))
        , forAll (snd_ pair) (`satisfies` (witCredSpec @ConwayFn @era univ))
        ]
    , satisfies (dom_ dRepMap) (hasSize (rangeSize 6 10))
    ]

-- ========================================================================
-- The CertState specs
-- ========================================================================

-- | BE SURE the parameter
--   delegated :: Term fn (Map (Credential 'DRepRole c) (Set (Credential 'Staking c))
--   has been witnessed with the same WitUniv as the parameter 'univ', or this will fail
--   For a standalone test of vstateSpec one may use goodDrep above, and pass
--   'eraUniv' as the actual parameter for the formal parameter 'univ'
--   Note, that in certStateSpec, the call to vstateSpec is passed a witnessed 'delegated'
--   that comes from the dstateSpec.
vstateSpec ::
  forall fn era.
  (IsConwayUniv fn, Era era) =>
  WitUniv era ->
  Term fn EpochNo ->
  Term fn (Map (Credential 'DRepRole) (Set (Credential 'Staking))) ->
  Specification fn (VState era)
vstateSpec univ epoch delegated = constrained $ \ [var|vstate|] ->
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
    , assertExplain (pure "num dormant epochs should not be too large") $
        [epoch <=. numdormant, numdormant <=. epoch + (lit (EpochNo 10))]
    , dependsOn numdormant epoch -- Solve epoch first.
    , match comstate $ \ [var|commap|] ->
        [witness univ (dom_ commap), satisfies commap (hasSize (rangeSize 1 4))]
    ]

-- Extract the map of DReps, to those that delegate to them, from the DState
getDelegatees ::
  DState era ->
  Map (Credential 'DRepRole) (Set (Credential 'Staking))
getDelegatees dstate = aggregateDRep (UMap.dRepMap (dsUnified dstate))

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

dstateSpec ::
  forall era fn.
  EraSpecLedger era fn =>
  WitUniv era ->
  Term fn AccountState ->
  Term fn (Map (KeyHash 'StakePool) PoolParams) ->
  Specification fn (DState era)
dstateSpec univ acct poolreg = constrained $ \ [var| ds |] ->
  match ds $ \ [var|umap|] [var|futureGenDelegs|] [var|genDelegs|] [var|irewards|] ->
    match umap $ \ [var|rdMap|] [var|ptrmap|] [var|sPoolMap|] [var|dRepMap|] ->
      [ dependsOn dRepMap rdMap
      , -- The dRepMap depends on the rdMap, so it is computed afterwards, forced by the reify
        reify rdMap id $ \ [var|rdm|] ->
          [ witness univ (dom_ dRepMap)
          , witness univ (rng_ dRepMap)
          , assert $ subset_ (dom_ dRepMap) (dom_ rdm)
          , forAll dRepMap $ \ [var|pair|] ->
              match pair $ \ [var|_stakecred|] [var|drep|] ->
                (caseOn drep)
                  (branchW 3 $ \keyhash -> witness univ keyhash)
                  (branchW 3 $ \scripthash -> witness univ scripthash)
                  (branchW 1 $ \_abstain -> True)
                  (branchW 1 $ \_noconfidence -> True)
          ]
      , whenTrue (not_ (hasPtrs (Proxy @era))) (assert $ ptrmap ==. lit Map.empty)
      , whenTrue
          (hasPtrs (Proxy @era))
          [ witness univ (rng_ ptrmap)
          , dependsOn rdMap ptrmap
          , -- reify here, forces us to solve for ptrmap, before solving for rdMap
            -- If there are Ptrs, then the range of the Ptrs must equal the domain of the rdMap
            reify ptrmap id (\ [var|pm|] -> domEqualRng pm rdMap)
          ]
      , witness univ (dom_ rdMap) -- rdMap must be witnessed, whether of not there are Ptrs
      , dependsOn sPoolMap rdMap
      , -- reify here, forces us to solve for rdMap, before solving for sPoolMap
        reify rdMap Map.keysSet $ \ [var|rdcreds|] ->
          [ genHint 5 sPoolMap
          , assertExplain (pure "dom sPoolMap is a subset of dom rdMap") $ dom_ sPoolMap `subset_` rdcreds
          , assertExplain (pure "The delegations delegate to actual pools") $
              forAll (rng_ sPoolMap) (\ [var|keyhash|] -> member_ keyhash (dom_ poolreg))
          ]
      , -- futureGenDelegs and genDelegs and irewards can be solved in any order
        satisfies irewards (irewardSpec @era univ acct)
      , satisfies
          futureGenDelegs
          (hasSize (if hasGenDelegs @era [] then (rangeSize 0 3) else (rangeSize 0 0)))
      , match genDelegs $ \ [var|gdmap|] ->
          [ if hasGenDelegs @era []
              then satisfies gdmap (hasSize (rangeSize 1 4))
              else satisfies gdmap (hasSize (rangeSize 0 0))
          , witness univ (dom_ gdmap)
          , witness univ (rng_ gdmap)
          ]
      ]

epochNoSpec :: IsConwayUniv fn => Specification fn EpochNo
epochNoSpec = constrained $ \epoch -> epoch >=. 99

pstateSpec ::
  (IsConwayUniv fn, Era era) =>
  WitUniv era ->
  Term fn EpochNo ->
  Specification fn (PState era)
pstateSpec univ currepoch = constrained $ \ [var|pState|] ->
  match pState $ \ [var|stakePoolParams|] [var|futureStakePoolParams|] [var|retiring|] [var|pooldeposits|] ->
    [ witness univ (dom_ stakePoolParams)
    , witness univ (rng_ stakePoolParams)
    , witness univ (dom_ futureStakePoolParams)
    , witness univ (rng_ futureStakePoolParams)
    , witness univ (dom_ retiring)
    , witness univ (dom_ pooldeposits)
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
        forAll (rng_ retiring) (\ [var|epoch|] -> currepoch <=. epoch)
    , assert $ sizeOf_ (dom_ futureStakePoolParams) <=. 4
    , assert $ 3 <=. sizeOf_ (dom_ stakePoolParams)
    , assert $ sizeOf_ (dom_ stakePoolParams) <=. 8
    ]

accountStateSpec :: IsConwayUniv fn => Specification fn AccountState
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
certStateSpec ::
  forall era fn.
  EraSpecLedger era fn =>
  WitUniv era ->
  Term fn AccountState ->
  Term fn EpochNo ->
  Specification fn (CertState era)
certStateSpec univ acct epoch = constrained $ \ [var|certState|] ->
  match certState $ \ [var|vState|] [var|pState|] [var|dState|] ->
    [ satisfies pState (pstateSpec univ epoch)
    , reify pState psStakePoolParams $ \ [var|poolreg|] ->
        [ dependsOn dState poolreg
        , satisfies dState (dstateSpec univ acct poolreg)
        ]
    , reify dState getDelegatees $ \ [var|delegatees|] ->
        satisfies vState (vstateSpec univ epoch delegatees)
    ]

-- ==============================================================
-- Specs for UTxO and UTxOState
-- ==============================================================

utxoSpecWit ::
  forall era fn.
  -- EraSpecLedger era fn =>
  EraSpecTxOut era fn =>
  WitUniv era ->
  Term fn (Map (Credential 'Staking) (KeyHash 'StakePool)) ->
  Specification fn (UTxO era)
utxoSpecWit univ delegs = constrained $ \ [var|utxo|] ->
  match utxo $ \ [var|utxomap|] ->
    [ forAll (rng_ utxomap) (\ [var|out|] -> txOutSpec univ delegs out)
    ]

utxoStateSpec ::
  forall era fn.
  EraSpecLedger era fn =>
  PParams era ->
  WitUniv era ->
  Term fn (CertState era) ->
  Specification fn (UTxOState era)
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
      , satisfies gov (govStateSpec @era @fn pp)
      , reify utxo (updateStakeDistribution pp mempty mempty) (\ [var|i|] -> distr ==. i)
      ]

getDelegs ::
  forall era.
  CertState era ->
  Map (Credential 'Staking) (KeyHash 'StakePool)
getDelegs cs = UMap.sPoolMap (dsUnified (certDState cs))

-- ====================================================================
-- Specs for LedgerState
-- ====================================================================

shelleyGovStateSpec ::
  forall era fn. EraSpecLedger era fn => PParams era -> Specification fn (ShelleyGovState era)
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
  IsConwayUniv fn =>
  PParams ConwayEra ->
  Specification fn (GovEnv ConwayEra)
govEnvSpec pp = constrained $ \ [var|govEnv|] ->
  match govEnv $ \_ _ [var|cppx|] _ _ -> [assert $ lit pp ==. cppx]

conwayGovStateSpec ::
  forall fn.
  EraSpecLedger ConwayEra fn =>
  PParams ConwayEra ->
  GovEnv ConwayEra ->
  Specification fn (ConwayGovState ConwayEra)
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
  forall era fn.
  EraSpecLedger era fn =>
  PParams era ->
  WitUniv era ->
  Term fn AccountState ->
  Term fn EpochNo ->
  Specification fn (LedgerState era)
ledgerStateSpec pp univ acct epoch =
  constrained $ \ [var|ledgerState|] ->
    match ledgerState $ \ [var|utxoS|] [var|csg|] ->
      [ satisfies csg (certStateSpec @era @fn univ acct epoch)
      , reify csg id (\ [var|certstate|] -> satisfies utxoS (utxoStateSpec @era @fn pp univ certstate))
      ]

-- ===========================================================

-- TODO make this more realistic
snapShotSpec :: IsConwayUniv fn => Specification fn SnapShot
snapShotSpec =
  constrained $ \ [var|snap|] ->
    match snap $ \ [var|stake|] [var|delegs|] [var|poolparams|] ->
      match stake $ \ [var|stakemap|] ->
        [ assert $ stakemap ==. lit VMap.empty
        , assert $ delegs ==. lit VMap.empty
        , assert $ poolparams ==. lit VMap.empty
        ]

snapShotsSpec ::
  IsConwayUniv fn => Term fn SnapShot -> Specification fn SnapShots
snapShotsSpec marksnap =
  constrained $ \ [var|snap|] ->
    match snap $ \ [var|mark|] [var|pooldistr|] [var|set|] [var|go|] _fee ->
      Block
        [ assert $ mark ==. marksnap
        , satisfies set snapShotSpec
        , satisfies go snapShotSpec
        , reify marksnap calculatePoolDistr $ \ [var|pd|] -> pooldistr ==. pd
        ]

-- | The Mark SnapShot (at the epochboundary) is a pure function of the LedgerState
getMarkSnapShot :: forall era. LedgerState era -> SnapShot
getMarkSnapShot ls = SnapShot (Stake markStake) markDelegations markPoolParams
  where
    markStake :: VMap VB VP (Credential 'Staking) (CompactForm Coin)
    markStake = VMap.fromMap (credMap (utxosStakeDistr (lsUTxOState ls)))
    markDelegations ::
      VMap VB VB (Credential 'Staking) (KeyHash 'StakePool)
    markDelegations = VMap.fromMap (UMap.sPoolMap (dsUnified (certDState (lsCertState ls))))
    markPoolParams :: VMap VB VB (KeyHash 'StakePool) PoolParams
    markPoolParams = VMap.fromMap (psStakePoolParams (certPState (lsCertState ls)))

-- ====================================================================
-- Specs for EpochState and NewEpochState
-- ====================================================================

epochStateSpec ::
  forall era fn.
  EraSpecLedger era fn =>
  PParams era ->
  WitUniv era ->
  Term fn EpochNo ->
  Specification fn (EpochState era)
epochStateSpec pp univ epoch =
  constrained $ \ [var|epochState|] ->
    match epochState $ \ [var|acctst|] [var|eLedgerState|] [var|snaps|] [var|nonmyopic|] ->
      Block
        [ dependsOn eLedgerState acctst
        , satisfies eLedgerState (ledgerStateSpec pp univ acctst epoch)
        , reify eLedgerState getMarkSnapShot $ \ [var|marksnap|] -> satisfies snaps (snapShotsSpec marksnap)
        , match nonmyopic $ \ [var|x|] [var|c|] -> [genHint 0 x, assert $ c ==. lit (Coin 0)]
        ]

getPoolDistr :: forall era. EpochState era -> PoolDistr
getPoolDistr es = ssStakeMarkPoolDistr (esSnapshots es)

-- | Used for Eras where StashedAVVMAddresses era ~ UTxO era (Shelley)
-- The 'newEpochStateSpec' method (of (EraSpecLedger era fn) class) in the Shelley instance
newEpochStateSpecUTxO ::
  forall era fn.
  (EraSpecLedger era fn, StashedAVVMAddresses era ~ UTxO era) =>
  PParams era ->
  WitUniv era ->
  Specification fn (NewEpochState era)
newEpochStateSpecUTxO pp univ =
  constrained
    ( \ [var|newEpochStateUTxO|] ->
        match
          (newEpochStateUTxO :: Term fn (NewEpochState era))
          ( \ [var|eno|] [var|blocksPrev|] [var|blocksCurr|] [var|epochstate|] _mpulser [var|pooldistr|] [var|stashAvvm|] ->
              Block
                [ -- reify eno id (\ [var|epoch|] -> satisfies epochstate (epochStateSpec @era @fn pp epoch))
                  -- dependsOn eno epochstate
                  satisfies epochstate (epochStateSpec @era @fn pp univ eno)
                , satisfies stashAvvm (constrained (\ [var|u|] -> u ==. lit (UTxO @era Map.empty)))
                , reify epochstate getPoolDistr $ \ [var|pd|] -> pooldistr ==. pd
                , match blocksPrev (genHint 3)
                , match blocksCurr (genHint 3)
                ]
          )
    )

-- | Used for Eras where StashedAVVMAddresses era ~ () (Allegra,Mary,Alonzo,Babbage,Conway)
-- The 'newEpochStateSpec' method (of (EraSpecLedger era fn) class) in the instances for (Allegra,Mary,Alonzo,Babbage,Conway)
newEpochStateSpecUnit ::
  forall era fn.
  (EraSpecLedger era fn, StashedAVVMAddresses era ~ ()) =>
  PParams era ->
  WitUniv era ->
  Specification fn (NewEpochState era)
newEpochStateSpecUnit pp univ =
  constrained
    ( \ [var|newEpochStateUnit|] ->
        match
          (newEpochStateUnit :: Term fn (NewEpochState era))
          ( \ [var|eno|] [var|blocksPrev|] [var|blocksCurr|] [var|epochstate|] _mpulser [var|pooldistr|] [var|stashAvvm|] ->
              Block
                [ satisfies epochstate (epochStateSpec @era @fn pp univ eno)
                , satisfies stashAvvm (constrained (\ [var|x|] -> x ==. lit ()))
                , reify epochstate getPoolDistr $ \ [var|pd|] -> pooldistr ==. pd
                , match blocksPrev (genHint 3)
                , match blocksCurr (genHint 3)
                ]
          )
    )
