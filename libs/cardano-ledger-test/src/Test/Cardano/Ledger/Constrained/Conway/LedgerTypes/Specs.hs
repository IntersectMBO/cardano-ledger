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
import Cardano.Ledger.Credential (Credential)
import Cardano.Ledger.EpochBoundary (SnapShot (..), SnapShots (..), Stake (..), calculatePoolDistr)
import Cardano.Ledger.Keys (KeyHash, KeyRole (..))
import Cardano.Ledger.PoolDistr (PoolDistr (..))
import Cardano.Ledger.PoolParams (PoolParams (..))
import Cardano.Ledger.SafeHash ()
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
import Cardano.Ledger.UMap (CompactForm (..))
import qualified Cardano.Ledger.UMap as UMap
import Cardano.Ledger.UTxO (UTxO (..))
import Constrained hiding (Value)
import Constrained.Base (Pred (..), hasSize, rangeSize)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Typeable
import Data.VMap (VB, VMap, VP)
import qualified Data.VMap as VMap
import System.IO.Unsafe (unsafePerformIO)
import Test.Cardano.Ledger.Constrained.Conway.Gov (govProposalsSpec)
import Test.Cardano.Ledger.Constrained.Conway.Instances
import Test.Cardano.Ledger.Constrained.Conway.ParametricSpec (EraSpecDeleg (..), EraSpecTxOut (..))
import Test.QuickCheck (generate)

-- ===========================================================

-- | The class (EraSpecLedger era) supports Era parametric Specs over types that appear in the Cardano Ledger.223
--   It uses methods (see Test.Cardano.Ledger.Constrained.Conway.ParametricSpec)
--   that navigate the differences in types parameterized by 'era' that are
--   embeded as type Families in types that appear in the Cardano Ledger Types.
--   It is these components that change from one Era to another.
--   and the EraSpecLedger class has methods that asbtract over those changes.
class
  ( EraSpecTxOut era fn
  , HasSpec fn (GovState era)
  ) =>
  EraSpecLedger era fn
  where
  govStateSpec :: PParams era -> Specification fn (GovState era)
  newEpochStateSpec :: PParams era -> Specification fn (NewEpochState era)

instance IsConwayUniv fn => EraSpecLedger Shelley fn where
  govStateSpec = shelleyGovStateSpec
  newEpochStateSpec = newEpochStateSpecUTxO

instance IsConwayUniv fn => EraSpecLedger Allegra fn where
  govStateSpec = shelleyGovStateSpec
  newEpochStateSpec = newEpochStateSpecUnit

instance IsConwayUniv fn => EraSpecLedger Mary fn where
  govStateSpec = shelleyGovStateSpec
  newEpochStateSpec = newEpochStateSpecUnit

instance IsConwayUniv fn => EraSpecLedger Alonzo fn where
  govStateSpec = shelleyGovStateSpec
  newEpochStateSpec = newEpochStateSpecUnit

instance IsConwayUniv fn => EraSpecLedger Babbage fn where
  govStateSpec = shelleyGovStateSpec
  newEpochStateSpec = newEpochStateSpecUnit

instance IsConwayUniv fn => EraSpecLedger Conway fn where
  govStateSpec pp = conwayGovStateSpec pp (testGovEnv pp)
  newEpochStateSpec = newEpochStateSpecUnit

-- This is a hack, neccessitated by the fact that conwayGovStateSpec,
-- written for the conformance tests, requires an actual GovEnv as an input.
-- This is a very large Specification (500 lines), so we don't want to redo it.
-- The only important part of the GovEnv is that the embedded PParams matches
-- the PParams passed to conwayGovStateSpec.
testGovEnv :: PParams Conway -> GovEnv Conway
testGovEnv pp = unsafePerformIO $ generate $ do
  env <- genFromSpec @ConwayFn @(GovEnv Conway) (govEnvSpec @ConwayFn pp)
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

-- ========================================================================
-- The CertState specs
-- ========================================================================

drepStateSpec :: (IsConwayUniv fn, Crypto c) => Term fn EpochNo -> Specification fn (DRepState c)
drepStateSpec epoch = constrained $ \ [var|drepstate|] ->
  match drepstate $ \ [var|expiry|] _anchor [var|drepDdeposit|] _delegs ->
    [ assertExplain (pure "epoch of expiration must follow the current epoch") $ epoch <=. expiry
    , assertExplain (pure "no deposit is 0") $ lit (Coin 0) <=. drepDdeposit
    ]

vstateSpec :: (IsConwayUniv fn, Era era) => Term fn EpochNo -> Specification fn (VState era)
vstateSpec epoch = constrained $ \ [var|vstate|] ->
  match vstate $ \ [var|dreps|] [var|comstate|] [var|numdormant|] ->
    [ forAll (rng_ dreps) (\ [var|x|] -> x `satisfies` (drepStateSpec epoch))
    , satisfies (dom_ dreps) (hasSize (rangeSize 5 12))
    , assertExplain (pure "num dormant epochs should not be too large") $
        [epoch <=. numdormant, numdormant <=. epoch + (lit (EpochNo 10))]
    , dependsOn numdormant epoch -- Solve epoch first.
    , match comstate $ \ [var|commap|] -> satisfies commap (hasSize (rangeSize 1 4))
    ]

dstateSpec ::
  forall era fn.
  EraSpecLedger era fn =>
  Term fn AccountState ->
  Term fn (Map (KeyHash 'StakePool (EraCrypto era)) (PoolParams (EraCrypto era))) ->
  Term fn (Map (Credential 'DRepRole (EraCrypto era)) (DRepState (EraCrypto era))) ->
  Specification fn (DState era)
dstateSpec acct poolreg dreps = constrained $ \ [var| ds |] ->
  match ds $ \ [var|umap|] [var|futureGenDelegs|] [var|genDelegs|] [var|irewards|] ->
    match umap $ \ [var|rdMap|] [var|ptrmap|] [var|sPoolMap|] [var|dRepMap|] ->
      [ genHint 5 sPoolMap
      , assertExplain (pure "The delegations delegate to actual pools") $
          forAll (rng_ sPoolMap) (\ [var|keyhash|] -> member_ keyhash (dom_ poolreg))
      , assertExplain (pure "dom sPoolMap is a subset of dom rdMap") $ dom_ sPoolMap `subset_` dom_ rdMap
      , -- NOTE: Consider if this assertion (and the `dependsOn` check below it) can be removed.
        -- Commit `21215b03a - Add delegations field to the DRep state` added a TODO
        -- to add a constraint that delegs are in the UMap. The below does that but it wasn't
        -- the cause for the conformance test failures.
        forAll' dreps $
          \_ dRepState -> match dRepState $ \_ _ _ delegs ->
            assertExplain
              (pure "Delegs are present in the UMap")
              (forAll delegs (\ [var|drep|] -> drep `member_` dom_ dRepMap))
      , dreps `dependsOn` dRepMap
      , -- reify here, forces us to solve for ptrap, before sovling for rdMap
        whenTrue (hasPtrs (Proxy @era)) (reify ptrmap id (\ [var|pm|] -> domEqualRng pm rdMap))
      , whenTrue (not_ (hasPtrs (Proxy @era))) (assert $ ptrmap ==. lit Map.empty)
      , satisfies irewards (irewardSpec @era acct)
      , satisfies
          futureGenDelegs
          (hasSize (if hasGenDelegs @era [] then (rangeSize 0 3) else (rangeSize 0 0)))
      , match genDelegs $ \ [var|gd|] -> satisfies gd (hasSize (if hasGenDelegs @era [] then (rangeSize 1 4) else (rangeSize 0 0)))
      ]

epochNoSpec :: IsConwayUniv fn => Specification fn EpochNo
epochNoSpec = constrained $ \epoch -> epoch >=. 99

pstateSpec ::
  (IsConwayUniv fn, Era era) =>
  Term fn EpochNo ->
  Specification fn (PState era)
pstateSpec currepoch = constrained $ \ [var|pState|] ->
  match pState $ \ [var|stakePoolParams|] [var|futureStakePoolParams|] [var|retiring|] [var|pooldeposits|] ->
    [ assertExplain (pure "dom of retiring is a subset of dom of stakePoolParams") $
        dom_ retiring `subset_` dom_ stakePoolParams
    , assertExplain (pure "retiring after current epoch") $
        forAll (rng_ retiring) (\ [var|epoch|] -> currepoch <=. epoch)
    , assertExplain (pure "dom of deposits is dom of stakePoolParams") $
        dom_ pooldeposits ==. dom_ stakePoolParams
    , assertExplain (pure "no deposit is 0") $
        not_ $
          lit (Coin 0) `elem_` rng_ pooldeposits
    , assertExplain (pure "dom of stakePoolParams is disjoint from futureStakePoolParams") $
        dom_ stakePoolParams `disjoint_` dom_ futureStakePoolParams
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

certStateSpec ::
  forall era fn.
  EraSpecLedger era fn =>
  Term fn AccountState ->
  Term fn EpochNo ->
  Specification fn (CertState era)
certStateSpec acct epoch = constrained $ \ [var|certState|] ->
  match certState $ \ [var|vState|] [var|pState|] [var|dState|] ->
    [ satisfies vState (vstateSpec epoch)
    , satisfies pState (pstateSpec epoch)
    , reify
        pState
        psStakePoolParams
        ( \ [var|poolreg|] ->
            reify
              vState
              vsDReps
              ( \ [var|dreps|] ->
                  satisfies dState (dstateSpec acct poolreg dreps)
              )
        )
    ]

-- ==============================================================
-- Specs for UTxO and UTxOState
-- ==============================================================

utxoSpec ::
  forall era fn.
  EraSpecLedger era fn =>
  Term fn (Map (Credential 'Staking (EraCrypto era)) (KeyHash 'StakePool (EraCrypto era))) ->
  Specification fn (UTxO era)
utxoSpec delegs = constrained $ \ [var|utxo|] ->
  match utxo $ \ [var|utxomap|] ->
    [ forAll (rng_ utxomap) (\ [var|output|] -> correctTxOut delegs output)
    ]

utxoStateSpec ::
  forall era fn.
  EraSpecLedger era fn =>
  PParams era ->
  Term fn (CertState era) ->
  Specification fn (UTxOState era)
utxoStateSpec pp certstate =
  constrained $ \ [var|utxoState|] ->
    match utxoState $ \ [var|utxo|] [var|deposits|] [var|fees|] [var|gov|] [var|distr|] [var|donation|] ->
      [ assert $ donation ==. lit (Coin 0)
      , reify
          certstate
          (sumObligation . obligationCertState)
          (\ [var|depositsum|] -> assert $ deposits ==. depositsum)
      , assert $ lit (Coin 0) <=. fees
      , reify certstate getDelegs (\ [var|delegs|] -> satisfies utxo (utxoSpec delegs))
      , satisfies gov (govStateSpec @era @fn pp)
      , reify utxo (updateStakeDistribution pp mempty mempty) (\ [var|i|] -> distr ==. i)
      ]

getDelegs ::
  forall era.
  CertState era ->
  Map (Credential 'Staking (EraCrypto era)) (KeyHash 'StakePool (EraCrypto era))
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
  PParams Conway ->
  Specification fn (GovEnv Conway)
govEnvSpec pp = constrained $ \ [var|govEnv|] ->
  match govEnv $ \_ _ [var|cppx|] _ _ -> [assert $ lit pp ==. cppx]

conwayGovStateSpec ::
  forall fn.
  EraSpecLedger Conway fn =>
  PParams Conway ->
  GovEnv Conway ->
  Specification fn (ConwayGovState Conway)
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
  Term fn AccountState ->
  Term fn EpochNo ->
  Specification fn (LedgerState era)
ledgerStateSpec pp acct epoch =
  constrained $ \ [var|ledgerState|] ->
    match ledgerState $ \ [var|utxoS|] [var|csg|] ->
      [ satisfies csg (certStateSpec @era @fn acct epoch)
      , reify csg id (\ [var|certstate|] -> satisfies utxoS (utxoStateSpec @era @fn pp certstate))
      ]

-- ===========================================================

-- TODO make this more realistic
snapShotSpec :: (Crypto c, IsConwayUniv fn) => Specification fn (SnapShot c)
snapShotSpec =
  constrained $ \ [var|snap|] ->
    match snap $ \ [var|stake|] [var|delegs|] [var|poolparams|] ->
      match stake $ \ [var|stakemap|] ->
        [ assert $ stakemap ==. lit VMap.empty
        , assert $ delegs ==. lit VMap.empty
        , assert $ poolparams ==. lit VMap.empty
        ]

snapShotsSpec ::
  (Crypto c, IsConwayUniv fn) => Term fn (SnapShot c) -> Specification fn (SnapShots c)
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
getMarkSnapShot :: forall era. LedgerState era -> SnapShot (EraCrypto era)
getMarkSnapShot ls = SnapShot @(EraCrypto era) (Stake markStake) markDelegations markPoolParams
  where
    markStake :: VMap VB VP (Credential 'Staking (EraCrypto era)) (CompactForm Coin)
    markStake = VMap.fromMap (credMap (utxosStakeDistr (lsUTxOState ls)))
    markDelegations ::
      VMap VB VB (Credential 'Staking (EraCrypto era)) (KeyHash 'StakePool (EraCrypto era))
    markDelegations = VMap.fromMap (UMap.sPoolMap (dsUnified (certDState (lsCertState ls))))
    markPoolParams :: VMap VB VB (KeyHash 'StakePool (EraCrypto era)) (PoolParams (EraCrypto era))
    markPoolParams = VMap.fromMap (psStakePoolParams (certPState (lsCertState ls)))

-- ====================================================================
-- Specs for EpochState and NewEpochState
-- ====================================================================

epochStateSpec ::
  forall era fn.
  EraSpecLedger era fn =>
  PParams era ->
  Term fn EpochNo ->
  Specification fn (EpochState era)
epochStateSpec pp epoch =
  constrained $ \ [var|epochState|] ->
    match epochState $ \ [var|acctst|] [var|eLedgerState|] [var|snaps|] [var|nonmyopic|] ->
      Block
        [ dependsOn eLedgerState acctst
        , satisfies eLedgerState (ledgerStateSpec pp acctst epoch)
        , reify eLedgerState getMarkSnapShot $ \ [var|marksnap|] -> satisfies snaps (snapShotsSpec marksnap)
        , match nonmyopic $ \ [var|x|] [var|c|] -> [genHint 0 x, assert $ c ==. lit (Coin 0)]
        ]

getPoolDistr :: forall era. EpochState era -> PoolDistr (EraCrypto era)
getPoolDistr es = ssStakeMarkPoolDistr (esSnapshots es)

-- | Used for Eras where StashedAVVMAddresses era ~ UTxO era (Shelley)
-- The 'newEpochStateSpec' method (of (EraSpecLedger era fn) class) in the Shelley instance
newEpochStateSpecUTxO ::
  forall era fn.
  (EraSpecLedger era fn, StashedAVVMAddresses era ~ UTxO era) =>
  PParams era ->
  Specification fn (NewEpochState era)
newEpochStateSpecUTxO pp =
  constrained
    ( \ [var|newEpochStateUTxO|] ->
        match
          (newEpochStateUTxO :: Term fn (NewEpochState era))
          ( \ [var|eno|] [var|blocksPrev|] [var|blocksCurr|] [var|epochstate|] _mpulser [var|pooldistr|] [var|stashAvvm|] ->
              Block
                [ -- reify eno id (\ [var|epoch|] -> satisfies epochstate (epochStateSpec @era @fn pp epoch))
                  -- dependsOn eno epochstate
                  satisfies epochstate (epochStateSpec @era @fn pp eno)
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
  Specification fn (NewEpochState era)
newEpochStateSpecUnit pp =
  constrained
    ( \ [var|newEpochStateUnit|] ->
        match
          (newEpochStateUnit :: Term fn (NewEpochState era))
          ( \ [var|eno|] [var|blocksPrev|] [var|blocksCurr|] [var|epochstate|] _mpulser [var|pooldistr|] [var|stashAvvm|] ->
              Block
                [ satisfies epochstate (epochStateSpec @era @fn pp eno)
                , satisfies stashAvvm (constrained (\ [var|x|] -> x ==. lit ()))
                , reify epochstate getPoolDistr $ \ [var|pd|] -> pooldistr ==. pd
                , match blocksPrev (genHint 3)
                , match blocksCurr (genHint 3)
                ]
          )
    )
