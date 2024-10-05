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

import Cardano.Ledger.Alonzo.TxOut (AlonzoTxOut (..))
import Cardano.Ledger.Api
import Cardano.Ledger.Babbage.TxOut (BabbageTxOut (..))
import Cardano.Ledger.BaseTypes hiding (inject)
import Cardano.Ledger.CertState
import Cardano.Ledger.Coin (Coin (..), DeltaCoin (..))
import Cardano.Ledger.Conway.Rules
import Cardano.Ledger.Core (Value)
import Cardano.Ledger.Credential (Credential, StakeReference (..))
import Cardano.Ledger.EpochBoundary (SnapShot (..), SnapShots (..), Stake (..), calculatePoolDistr)
import Cardano.Ledger.Keys (KeyHash, KeyRole (..))
import Cardano.Ledger.Mary (MaryValue)
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
import Cardano.Ledger.Shelley.TxOut (ShelleyTxOut (..))
import Cardano.Ledger.UMap (CompactForm (..))
import qualified Cardano.Ledger.UMap as UMap
import Cardano.Ledger.UTxO (UTxO (..))
import Constrained hiding (Value)
import Constrained.Base (Pred (..), fromList_, hasSize, rangeSize)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Typeable
import Data.VMap (VB, VMap, VP)
import qualified Data.VMap as VMap
import Data.Word (Word64)
import System.IO.Unsafe (unsafePerformIO)
import Test.Cardano.Ledger.Constrained.Conway ()
import Test.Cardano.Ledger.Constrained.Conway.Gov (govProposalsSpec)
import Test.Cardano.Ledger.Constrained.Conway.Instances
import Test.QuickCheck (generate)

-- ===========================================================

-- | The class (LedgerEra era) supports Era parametric Specs.
--   It contains methods that navigate the differences in types parameterized
--   by 'era' that are embeded as type Families in types that appear in the
--   Cardano Ledger Types. It is these components that change from one Era to another.
--   and the LedgerEra class has methods that asbtract over those changes.
--
--   The class (EraPP era) (Defined in ‘Test.Cardano.Ledger.Constrained.Conway.SimplePParams’)
--   supports specifications over the Era parametric (PParams era) in every era.
--   The method 'correctTxOut' supports specifcations over type Family TxOut in every era.
--   The method  'govStateSpec' supports specifcations over type Family GovState in every era.
--   And additional ones for phased out Type Families like InstantaneousRewards, StashedAVVMAddresses, and Ptrs
--   Instances for every Era are supplied.
class
  ( HasSpec fn (TxOut era)
  , IsNormalType (TxOut era)
  , HasSpec fn (GovState era)
  , HasSpec fn (StashedAVVMAddresses era)
  , EraTxOut era
  , IsConwayUniv fn
  , EraPP era
  ) =>
  LedgerEra era fn
  where
  irewardSpec :: Term fn AccountState -> Specification fn (InstantaneousRewards (EraCrypto era))
  hasPtrs :: proxy era -> Term fn Bool
  correctTxOut ::
    Term fn (Map (Credential 'Staking (EraCrypto era)) (KeyHash 'StakePool (EraCrypto era))) ->
    Term fn (TxOut era) ->
    Pred fn
  govStateSpec :: PParams era -> Specification fn (GovState era)
  newEpochStateSpec :: PParams era -> Specification fn (NewEpochState era)

instance IsConwayUniv fn => LedgerEra Shelley fn where
  irewardSpec = instantaneousRewardsSpec
  hasPtrs _proxy = lit True
  correctTxOut = betterTxOutShelley
  govStateSpec = shelleyGovStateSpec
  newEpochStateSpec = newEpochStateSpecUTxO

instance IsConwayUniv fn => LedgerEra Allegra fn where
  irewardSpec = instantaneousRewardsSpec
  hasPtrs _proxy = lit True
  correctTxOut = betterTxOutShelley
  govStateSpec = shelleyGovStateSpec
  newEpochStateSpec = newEpochStateSpecUnit

instance IsConwayUniv fn => LedgerEra Mary fn where
  irewardSpec = instantaneousRewardsSpec
  hasPtrs _proxy = lit True
  correctTxOut = betterTxOutMary
  govStateSpec = shelleyGovStateSpec
  newEpochStateSpec = newEpochStateSpecUnit

instance IsConwayUniv fn => LedgerEra Alonzo fn where
  irewardSpec = instantaneousRewardsSpec
  hasPtrs _proxy = lit True
  correctTxOut = betterTxOutAlonzo
  govStateSpec = shelleyGovStateSpec
  newEpochStateSpec = newEpochStateSpecUnit

instance IsConwayUniv fn => LedgerEra Babbage fn where
  irewardSpec = instantaneousRewardsSpec
  hasPtrs _proxy = lit True
  correctTxOut = betterTxOutBabbage
  govStateSpec = shelleyGovStateSpec
  newEpochStateSpec = newEpochStateSpecUnit

instance IsConwayUniv fn => LedgerEra Conway fn where
  irewardSpec _ = constrained $ \ [var|irewards|] ->
    match irewards $ \ [var|reserves|] [var|treasury|] [var|deltaRes|] [var|deltaTreas|] ->
      [ reserves ==. lit Map.empty
      , treasury ==. lit Map.empty
      , deltaRes ==. lit (DeltaCoin 0)
      , deltaTreas ==. lit (DeltaCoin 0)
      ]
  hasPtrs _proxy = lit False
  correctTxOut = betterTxOutBabbage
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

-- ======================================================================================
-- TxOut and Value are two of the type families, whose instance changes from Era to Era.
-- We need SimpleRep for each possible TxOut (Shelley,Mary,Alonzo,Babbage)
-- We also need to define the method 'correctTxOut' for every 'LedgerEra' instance
-- These instances are tricky, since there is a unique combination of Value and TxOut in
-- each one. Observe the type equalites (like (Value era ~ Coin)), and the inputs
-- (like ShelleyTxOut, AlonzoTxOut, BabbageTxOut), that make each function applicable
-- to only specific eras.
-- ======================================================================================

betterTxOutShelley ::
  (EraTxOut era, Value era ~ Coin, IsConwayUniv fn) =>
  Term fn (Map (Credential 'Staking (EraCrypto era)) (KeyHash 'StakePool (EraCrypto era))) ->
  Term fn (ShelleyTxOut era) ->
  Pred fn
betterTxOutShelley delegs txOut =
  match txOut $ \ [var|addr|] [var|val|] ->
    [ match val $ \ [var|c|] -> [0 <. c, c <=. fromIntegral (maxBound :: Word64)]
    , (caseOn addr)
        ( branch $ \ [var|network|] _ [var|stakeref|] ->
            [ assert $ network ==. lit Testnet
            , satisfies stakeref (delegatedStakeReference delegs)
            ]
        )
        ( branch $ \bootstrapAddr ->
            match bootstrapAddr $ \_ [var|nm|] _ ->
              (caseOn nm)
                (branch $ \_ -> False)
                (branch $ \_ -> True)
        )
    ]

betterTxOutMary ::
  (EraTxOut era, Value era ~ MaryValue (EraCrypto era), IsConwayUniv fn) =>
  Term fn (Map (Credential 'Staking (EraCrypto era)) (KeyHash 'StakePool (EraCrypto era))) ->
  Term fn (ShelleyTxOut era) ->
  Pred fn
betterTxOutMary delegs txOut =
  match txOut $ \ [var|addr|] [var|val|] ->
    [ match val $ \ [var|c|] -> [0 <. c, c <=. fromIntegral (maxBound :: Word64)]
    , (caseOn addr)
        ( branch $ \ [var|network|] _ [var|stakeref|] ->
            [ assert $ network ==. lit Testnet
            , satisfies stakeref (delegatedStakeReference delegs)
            ]
        )
        ( branch $ \bootstrapAddr ->
            match bootstrapAddr $ \_ [var|nm|] _ ->
              (caseOn nm)
                (branch $ \_ -> False)
                (branch $ \_ -> True)
        )
    ]

betterTxOutAlonzo ::
  (AlonzoEraTxOut era, Value era ~ MaryValue (EraCrypto era), IsConwayUniv fn) =>
  Term fn (Map (Credential 'Staking (EraCrypto era)) (KeyHash 'StakePool (EraCrypto era))) ->
  Term fn (AlonzoTxOut era) ->
  Pred fn
betterTxOutAlonzo delegs txOut =
  match txOut $ \ [var|addr|] [var|val|] _ ->
    [ match val $ \ [var|c|] -> [0 <. c, c <=. fromIntegral (maxBound :: Word64)]
    , (caseOn addr)
        ( branch $ \ [var|network|] _ [var|stakeref|] ->
            [ assert $ network ==. lit Testnet
            , satisfies stakeref (delegatedStakeReference delegs)
            ]
        )
        ( branch $ \bootstrapAddr ->
            match bootstrapAddr $ \_ _nm _ -> False
            {-
            (caseOn nm)
              (branch $ \_ -> False)
              (branch $ \_ -> True) -}
        )
    ]

betterTxOutBabbage ::
  ( EraTxOut era
  , Value era ~ MaryValue (EraCrypto era)
  , IsNormalType (Script era)
  , HasSpec fn (Script era)
  , IsConwayUniv fn
  ) =>
  Term fn (Map (Credential 'Staking (EraCrypto era)) (KeyHash 'StakePool (EraCrypto era))) ->
  Term fn (BabbageTxOut era) ->
  Pred fn
betterTxOutBabbage delegs txOut =
  match txOut $ \ [var|addr|] [var|val|] _ _ ->
    [ match val $ \c -> [0 <. c, c <=. fromIntegral (maxBound :: Word64)]
    , (caseOn addr)
        ( branch $ \ [var|network|] _ [var|stakeref|] ->
            [ assert $ network ==. lit Testnet
            , satisfies stakeref (delegatedStakeReference delegs)
            ]
        )
        ( branch $ \bootstrapAddr ->
            match bootstrapAddr $ \_ [var|nm|] _ ->
              (caseOn nm)
                (branch $ \_ -> False)
                (branch $ \_ -> True)
        )
    ]

-- | Generate random Stake references that have a high probability of being delegated.
delegatedStakeReference ::
  (IsConwayUniv fn, Crypto c) =>
  Term fn (Map (Credential 'Staking c) (KeyHash 'StakePool c)) ->
  Specification fn (StakeReference c)
delegatedStakeReference delegs =
  constrained $ \ [var|ref|] ->
    caseOn
      ref
      (branchW 9 $ \ [var|base|] -> member_ base (dom_ delegs))
      (branchW 0 $ \_ptr -> False)
      (branchW 1 $ \_null -> True) -- just an occaisional NullRef

-- ================================================================================
-- Specifications for types that appear in the LedgerEra Ledger
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

instantaneousRewardsSpec ::
  forall c fn.
  (IsConwayUniv fn, Crypto c) =>
  Term fn AccountState ->
  Specification fn (InstantaneousRewards c)
instantaneousRewardsSpec acct = constrained $ \ [var| irewards |] ->
  match acct $ \ [var| acctRes |] [var| acctTreas |] ->
    match irewards $ \ [var| reserves |] [var| treasury |] [var| deltaRes |] [var| deltaTreas |] ->
      [ dependsOn acctRes reserves
      , dependsOn acctRes deltaRes
      , dependsOn acctTreas treasury
      , dependsOn acctTreas deltaTreas
      , assertExplain (pure "deltaTreausry and deltaReserves sum to 0") $ negate deltaRes ==. deltaTreas
      , forAll (rng_ reserves) (\ [var| x |] -> x >=. (lit (Coin 0)))
      , forAll (rng_ treasury) (\ [var| y |] -> y >=. (lit (Coin 0)))
      , assert $ (toDelta_ (foldMap_ id (rng_ reserves))) - deltaRes <=. toDelta_ acctRes
      , assert $ (toDelta_ (foldMap_ id (rng_ treasury))) - deltaTreas <=. toDelta_ acctTreas
      ]

-- ========================================================================
-- The CertState specs
-- ========================================================================

instance IsConwayUniv fn => NumLike fn EpochNo

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
  LedgerEra era fn =>
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
      , forAll' dreps $
          \_ dRepState -> match dRepState $ \_ _ _ delegs ->
            assertExplain
              (pure "Delegs are present in the UMap")
              (forAll delegs (\ [var|drep|] -> drep `member_` dom_ dRepMap))
      , dreps `dependsOn` dRepMap
      , -- reify here, forces us to solve for ptrap, before sovling for rdMap
        whenTrue (hasPtrs (Proxy @era)) (reify ptrmap id (\ [var|pm|] -> domEqualRng pm rdMap))
      , whenTrue (not_ (hasPtrs (Proxy @era))) (assert $ ptrmap ==. lit Map.empty)
      , satisfies irewards (irewardSpec @era acct)
      , satisfies futureGenDelegs (hasSize (rangeSize 0 3))
      , match genDelegs $ \ [var|gd|] -> satisfies gd (hasSize (rangeSize 1 4)) -- Strip off the newtype constructor
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
  LedgerEra era fn =>
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
  LedgerEra era fn =>
  Term fn (Map (Credential 'Staking (EraCrypto era)) (KeyHash 'StakePool (EraCrypto era))) ->
  Specification fn (UTxO era)
utxoSpec delegs = constrained $ \ [var|utxo|] ->
  match utxo $ \ [var|utxomap|] ->
    [ forAll (rng_ utxomap) (\ [var|output|] -> correctTxOut delegs output)
    ]

utxoStateSpec ::
  forall era fn.
  LedgerEra era fn =>
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
  forall era fn. LedgerEra era fn => PParams era -> Specification fn (ShelleyGovState era)
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
  LedgerEra Conway fn =>
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
  LedgerEra era fn =>
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
  LedgerEra era fn =>
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
-- The 'newEpochStateSpec' method (of (LedgerEra era fn) class) in the Shelley instance
newEpochStateSpecUTxO ::
  forall era fn.
  (LedgerEra era fn, StashedAVVMAddresses era ~ UTxO era) =>
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
-- The 'newEpochStateSpec' method (of (LedgerEra era fn) class) in the instances for (Allegra,Mary,Alonzo,Babbage,Conway)
newEpochStateSpecUnit ::
  forall era fn.
  (LedgerEra era fn, StashedAVVMAddresses era ~ ()) =>
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
