{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ExplicitForAll #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE UndecidableSuperClasses #-}
{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_GHC -Wno-orphans #-}

-- | Specs necessary to generate constrained (well formed) values of
--   types that appear in the Cardano Ledger Types. These Specifications
--   are Era parametric, and one can use them to generate well formed
--   values in any era (Shelley,Allegra,Mary,Alonzo,Babbage,Conway)
--   by type applying them to a particular era type. These specifications
--   are a usefull guide to building ones own specifications with one's own
--   idea of whats well formed. Each specification comes with an example
--   computation (e.g. exampleXXX) which has an IO type, that will construct
--   and then display the result. Such visualizations are crucial to getting
--   the specifications correct. And readers are encouraged to experiment on
--   the specifications, and them observe the changes. The module also supplies
--   Two other usefull artifacts. The class (WellFormed t)
--   And a bunch of HSpec Spec tests, which tests the soundness of every
--   WellFormed instance on every Era.
module Test.Cardano.Ledger.Constrained.Conway.StateSpecs where

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
import Constrained.Base (Pred (..), checkPredPure, fromList_, hasSize, rangeSize)
import Constrained.Env (singletonEnv)
import Control.Monad.Writer.Lazy
import Data.Default.Class (Default (def))
import qualified Data.List.NonEmpty as NE
import Data.Map (Map)
import qualified Data.Map as Map
import qualified Data.Set as Set
import Data.Typeable
import Data.VMap (VB, VMap, VP)
import qualified Data.VMap as VMap
import Data.Word (Word64)
import System.IO.Unsafe (unsafePerformIO)
import Test.Cardano.Ledger.Constrained.Conway ()
import Test.Cardano.Ledger.Constrained.Conway.Gov (govProposalsSpec)
import Test.Cardano.Ledger.Constrained.Conway.Instances
import Test.Cardano.Ledger.Constrained.Conway.PParams (pparamsSpec)
import Test.Cardano.Ledger.Generic.PrettyCore (
  PrettyA (prettyA),
  pcIRewards,
  pcSnapShotL,
  ppRecord,
 )
import Test.Hspec
import Test.QuickCheck (
  Gen,
  Property,
  arbitrary,
  counterexample,
  generate,
  property,
  withMaxSuccess,
 )

-- ===========================================================

-- | The class (CardanoState era) supports Era parametric Specs.
--   They contains methods that navigate the differences in types parameterized
--   by 'era' that are embeded as type Families in types that appear in the
--   Cardano Ledger Types. It is these components that change from one Era to another.
--   and the CardanoState class has methods that asbtract over those changes.
--
--   The class (EraPP era) supports specifications over type Family PParams in every era.
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
  CardanoState era fn
  where
  irewardSpec :: Term fn AccountState -> Specification fn (InstantaneousRewards (EraCrypto era))
  hasPtrs :: proxy era -> Term fn Bool
  correctTxOut ::
    Term fn (Map (Credential 'Staking (EraCrypto era)) (KeyHash 'StakePool (EraCrypto era))) ->
    Term fn (TxOut era) ->
    Pred fn
  govStateSpec :: PParams era -> Specification fn (GovState era)
  newEpochStateSpec :: PParams era -> Specification fn (NewEpochState era)

instance IsConwayUniv fn => CardanoState Shelley fn where
  irewardSpec = instantaneousRewardsSpec
  hasPtrs _proxy = lit True
  correctTxOut = betterTxOutShelley
  govStateSpec = shelleyGovStateSpec
  newEpochStateSpec = newEpochStateSpecUTxO

instance IsConwayUniv fn => CardanoState Allegra fn where
  irewardSpec = instantaneousRewardsSpec
  hasPtrs _proxy = lit True
  correctTxOut = betterTxOutShelley
  govStateSpec = shelleyGovStateSpec
  newEpochStateSpec = newEpochStateSpecUnit

instance IsConwayUniv fn => CardanoState Mary fn where
  irewardSpec = instantaneousRewardsSpec
  hasPtrs _proxy = lit True
  correctTxOut = betterTxOutMary
  govStateSpec = shelleyGovStateSpec
  newEpochStateSpec = newEpochStateSpecUnit

instance IsConwayUniv fn => CardanoState Alonzo fn where
  irewardSpec = instantaneousRewardsSpec
  hasPtrs _proxy = lit True
  correctTxOut = betterTxOutAlonzo
  govStateSpec = shelleyGovStateSpec
  newEpochStateSpec = newEpochStateSpecUnit

instance IsConwayUniv fn => CardanoState Babbage fn where
  irewardSpec = instantaneousRewardsSpec
  hasPtrs _proxy = lit True
  correctTxOut = betterTxOutBabbage
  govStateSpec = shelleyGovStateSpec
  newEpochStateSpec = newEpochStateSpecUnit

instance IsConwayUniv fn => CardanoState Conway fn where
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

-- ======================================================================================
-- TxOut and Value are two of the type families, whose instance changes from Era to Era.
-- We need SimpleRep for each possible TxOut (Shelley,Mary,Alonzo,Babbage)
-- We also need to define the method 'correctTxOut' for every 'CardanoState' instance
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

-- ====================================================================================
-- Some Specifications are constrained by types (say 'x') that do not appear in the type being
-- specified. We use the strategy of passing (Term fn x) as inputs to those specifcations.
-- For example, the AcountState must have sufficient capacity to support the InstantaneousRewards
-- So we pass a (Term fn AccountState) as input to 'instantaneousRewardsSpec'
-- In order to create tests, not involving the outer specifications (i.e. instantaneousRewardsSpec
-- in the example above), we make literal 'test' terms, we use by passing the test terms
--  as inputs to the tests and examples of those 'inner' specifications.
-- ====================================================================================

testAcctState :: Term fn AccountState
testAcctState = lit (AccountState (Coin 100) (Coin 100))

testGovEnv :: PParams Conway -> GovEnv Conway
testGovEnv pp = unsafePerformIO $ generate $ do
  env <- specToGen @(GovEnv Conway) (govEnvSpec @ConwayFn pp)
  pure env

testEpochNo :: Term fn EpochNo
testEpochNo = lit (EpochNo 99)

testPools ::
  forall era c.
  (c ~ EraCrypto era, CardanoState era ConwayFn) =>
  Term ConwayFn (Map (KeyHash 'StakePool c) (PoolParams c))
testPools = unsafePerformIO $ generate $ do
  ps <- specToGen @(Map (KeyHash 'StakePool c) (PoolParams c)) (hasSize (rangeSize 8 8))
  pure (lit ps)

testDelegations ::
  forall c. Crypto c => Term ConwayFn (Map (Credential 'Staking c) (KeyHash 'StakePool c))
testDelegations = unsafePerformIO $ generate $ do
  ds <- specToGen @(Map (Credential 'Staking c) (KeyHash 'StakePool c)) (hasSize (rangeSize 8 8))
  pure (lit ds)

testPP :: forall era. EraPP era => PParams era
testPP = unsafePerformIO $ generate $ specToGen @(PParams era) pparamsSpec

testCertState :: forall era. CardanoState era ConwayFn => Term ConwayFn (CertState era)
testCertState = unsafePerformIO $ generate $ do
  cs <- specToGen @(CertState era) (certStateSpec testAcctState testEpochNo)
  pure (lit cs)

testLedgerState :: forall era. CardanoState era ConwayFn => LedgerState era
testLedgerState = unsafePerformIO $ generate $ do
  ls <- specToGen @(LedgerState era) (ledgerStateSpec testPP testAcctState testEpochNo)
  pure ls

-- ================================================================================
-- Specifications for types that appear in the CardanoState Ledger
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

exampleDomEqualRng :: IO ()
exampleDomEqualRng = do
  (x, y) <-
    generate $
      genFromSpec @ConwayFn @(Map Int Int, Map Int Int)
        (constrained' $ \x y -> domEqualRng x y)
  putStrLn ("x = " ++ show (Set.fromList (Map.elems x)))
  putStrLn ("y = " ++ show (Map.keysSet y))

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

exampleCanFollow :: IO (ProtVer, ProtVer)
exampleCanFollow = generateSpec @(ProtVer, ProtVer) protVersCanfollow

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

exampleInstantaneousRewards :: IO ()
exampleInstantaneousRewards = do
  acct <- generate (arbitrary :: Gen AccountState)
  let xx :: Specification ConwayFn (InstantaneousRewards StandardCrypto)
      xx = instantaneousRewardsSpec @(EraCrypto Shelley) @ConwayFn (lit acct)
  ir <- generateSpec xx
  putStrLn (show (prettyA acct))
  putStrLn (show (pcIRewards ir))
  putStrLn ("conforms " ++ show (conformsToSpec ir xx))

-- ========================================================================
-- The CertState specs
-- ========================================================================

instance IsConwayUniv fn => NumLike fn EpochNo

drepStateSpec :: (IsConwayUniv fn, Crypto c) => Term fn EpochNo -> Specification fn (DRepState c)
drepStateSpec epoch = constrained $ \ [var|drepstate|] ->
  match drepstate $ \ [var|expiry|] _anchor [var|drepDdeposit|] ->
    [ assertExplain (pure "epoch of expiration must follow the current epoch") $ epoch <=. expiry
    , assertExplain (pure "no deposit is 0") $ lit (Coin 0) <=. drepDdeposit
    ]

exampleDrepState :: IO Bool
exampleDrepState = ioTest @(DRepState StandardCrypto) (drepStateSpec testEpochNo)

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

exampleVState :: IO Bool
exampleVState = ioTest @(VState Shelley) (vstateSpec testEpochNo)

dstateSpec ::
  forall era fn.
  CardanoState era fn =>
  Term fn AccountState ->
  Term fn (Map (KeyHash 'StakePool (EraCrypto era)) (PoolParams (EraCrypto era))) ->
  Specification fn (DState era)
dstateSpec acct poolreg = constrained $ \ [var| ds |] ->
  match ds $ \ [var|umap|] [var|futureGenDelegs|] [var|genDelegs|] [var|irewards|] ->
    match umap $ \ [var|rdMap|] [var|ptrMap|] [var|sPoolMap|] _dRepMap ->
      [ genHint 5 sPoolMap
      , assertExplain (pure "The delegations delegate to actual pools") $
          forAll (rng_ sPoolMap) (\ [var|keyhash|] -> member_ keyhash (dom_ poolreg))
      , assertExplain (pure "dom sPoolMap is a subset of dom rdMap") $ dom_ sPoolMap `subset_` dom_ rdMap
      , -- reify here, forces us to solve for ptrMap, before sovling for rdMap
        whenTrue (hasPtrs (Proxy @era)) (reify ptrMap id (\ [var|pm|] -> domEqualRng pm rdMap))
      , whenTrue (not_ (hasPtrs (Proxy @era))) (assert $ ptrMap ==. lit Map.empty)
      , satisfies irewards (irewardSpec @era acct)
      , satisfies futureGenDelegs (hasSize (rangeSize 0 3))
      , match genDelegs $ \ [var|gd|] -> satisfies gd (hasSize (rangeSize 1 4)) -- Strip off the newtype constructor
      ]

exampleDState :: IO Bool
exampleDState = do
  cs <-
    generateSpec @(Map (KeyHash 'StakePool StandardCrypto) (PoolParams StandardCrypto))
      (hasSize (rangeSize 10 10))
  putStrLn ("STAKEPOOL MAP\n" ++ show (prettyA cs))
  t <- generateSpec @(DState Conway) (dstateSpec testAcctState (lit cs))
  putStrLn ("DSTATE\n" ++ show (prettyA t))
  pure (conformsToSpec @ConwayFn t (dstateSpec testAcctState (lit cs)))

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

examplePState :: IO Bool
examplePState = ioTest @(PState Shelley) (pstateSpec testEpochNo)

accountStateSpec :: IsConwayUniv fn => Specification fn AccountState
accountStateSpec =
  constrained
    ( \ [var|accountState|] ->
        match
          accountState
          (\ [var|reserves|] [var|treasury|] -> [lit (Coin 100) <=. treasury, lit (Coin 100) <=. reserves])
    )
exampleAccountState :: IO Bool
exampleAccountState = ioTest @AccountState accountStateSpec

certStateSpec ::
  forall era fn.
  CardanoState era fn =>
  Term fn AccountState ->
  Term fn EpochNo ->
  Specification fn (CertState era)
certStateSpec acct epoch = constrained $ \ [var|certState|] ->
  match certState $ \ [var|vState|] [var|pState|] [var|dState|] ->
    [ satisfies vState (vstateSpec epoch)
    , satisfies pState (pstateSpec epoch)
    , reify pState psStakePoolParams (\ [var|poolreg|] -> satisfies dState (dstateSpec acct poolreg))
    ]

exampleCertState :: IO Bool
exampleCertState = ioTest @(CertState Conway) (certStateSpec testAcctState testEpochNo)

-- ==============================================================
-- Specs for UTxO and UTxOState
-- ==============================================================

utxoSpec ::
  forall era fn.
  CardanoState era fn =>
  Term fn (Map (Credential 'Staking (EraCrypto era)) (KeyHash 'StakePool (EraCrypto era))) ->
  Specification fn (UTxO era)
utxoSpec delegs = constrained $ \ [var|utxo|] ->
  match utxo $ \ [var|utxomap|] ->
    [ forAll (rng_ utxomap) (\ [var|output|] -> correctTxOut delegs output)
    ]

exampleUtxo :: IO Bool
exampleUtxo = do
  cs <-
    generateSpec @(Map (Credential 'Staking StandardCrypto) (KeyHash 'StakePool StandardCrypto))
      (hasSize (rangeSize 30 30))
  putStrLn ("Stake Registration MAP\n" ++ show (prettyA cs))
  t <- generateSpec @(UTxO Mary) (utxoSpec @Mary (lit cs))
  putStrLn ("UTxO\n" ++ show (prettyA t))
  pure (conformsToSpec @ConwayFn t (utxoSpec @Mary (lit cs)))

utxoStateSpec ::
  forall era fn.
  CardanoState era fn =>
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

exampleUtxoState :: IO Bool
exampleUtxoState = do
  pp <- generateSpec @(PParams Conway) pparamsSpec
  cs <- generateSpec @(CertState Conway) (certStateSpec testAcctState testEpochNo)
  let utxospec = utxoStateSpec @Conway @ConwayFn pp (lit cs)
  t <- generateSpec @(UTxOState Conway) utxospec
  putStrLn ("UTXOSTATE\n" ++ show (prettyA t))
  putStrLn ("CERTSTATE\n" ++ show (prettyA cs))
  let (ans, msgs) = runWriter (monadConformsToSpec t utxospec)
  putStrLn ("Monadic Messages\n" ++ unlines msgs) >> putStrLn (show ans)
  putStrLn ("NonMonadic ")
  pure (conformsToSpec t utxospec)

-- ====================================================================
-- Specs for LedgerState
-- ====================================================================

shelleyGovStateSpec ::
  forall era fn. CardanoState era fn => PParams era -> Specification fn (ShelleyGovState era)
shelleyGovStateSpec pp =
  constrained $ \ [var|shellGovState|] ->
    match shellGovState $ \ [var|curpro|] [var|futpro|] [var|curpp|] _prevpp _futpp ->
      match curpro $ \ [var|cm|] ->
        [ satisfies cm (hasSize (rangeSize 1 2))
        , match futpro $ \ [var|fm|] -> satisfies fm (hasSize (rangeSize 1 2))
        , assert $ curpp ==. lit pp
        -- FIXME -- match _futpp (\ fpp -> canFollow (protocolVersion_ fpp) (protocolVersion_ curpp))
        ]

exampleShelleyGovState :: IO Bool
exampleShelleyGovState = ioTest @(ShelleyGovState Mary) (shelleyGovStateSpec @Mary @ConwayFn def)

govEnvSpec ::
  IsConwayUniv fn =>
  PParams Conway ->
  Specification fn (GovEnv Conway)
govEnvSpec pp = constrained $ \ [var|govEnv|] ->
  match govEnv $ \_ _ [var|cppx|] _ _ -> [assert $ lit pp ==. cppx]

conwayGovStateSpec ::
  forall fn.
  CardanoState Conway fn =>
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

exampleConwayGovState :: IO Bool
exampleConwayGovState = do
  pp <- generateSpec @(PParams Conway) pparamsSpec
  govenv <- generateSpec @(GovEnv Conway) (govEnvSpec pp)
  ioTest @(ConwayGovState Conway) (conwayGovStateSpec pp govenv)

-- =========================================================================

ledgerStateSpec ::
  forall era fn.
  CardanoState era fn =>
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

exampleLedgerState :: IO Bool
exampleLedgerState = do
  pp <- generateSpec @(PParams Conway) pparamsSpec
  let ledgerspec = (ledgerStateSpec @Conway @ConwayFn pp testAcctState testEpochNo)
  ls <- generateSpec ledgerspec
  let d = sumObligation $ obligationCertState $ lsCertState ls
  putStrLn (show (prettyA ls))
  putStrLn ("Total certstate deposits " ++ show d)
  pure (conformsToSpec ls ledgerspec)

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

exampleSnapshot :: IO ()
exampleSnapshot = do
  -- No PrettyA instance so we write it out
  sn <- generateSpec (snapShotSpec @(EraCrypto Shelley) @ConwayFn)
  putStrLn (show (ppRecord "SnapShot" (pcSnapShotL "" sn)))

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

exampleSnapshots :: IO Bool
exampleSnapshots =
  ioTest @(SnapShots StandardCrypto)
    (snapShotsSpec (lit (getMarkSnapShot (testLedgerState @Babbage))))

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
  CardanoState era fn =>
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

exampleEpochState :: IO Bool
exampleEpochState = do
  pp <- generateSpec @(PParams Conway) pparamsSpec
  ioTest @(EpochState Conway) (epochStateSpec pp testEpochNo)

getPoolDistr :: forall era. EpochState era -> PoolDistr (EraCrypto era)
getPoolDistr es = ssStakeMarkPoolDistr (esSnapshots es)

-- | Used for Eras where StashedAVVMAddresses era ~ UTxO era (Shelley)
-- The 'newEpochStateSpec' method (of (CardanoState era fn) class) in the Shelley instance
newEpochStateSpecUTxO ::
  forall era fn.
  (CardanoState era fn, StashedAVVMAddresses era ~ UTxO era) =>
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

exampleNewEpochStateUtxo :: IO Bool
exampleNewEpochStateUtxo = do
  pp <- generateSpec @(PParams Alonzo) pparamsSpec
  ioTest @(NewEpochState Alonzo) (newEpochStateSpec pp)

-- | Used for Eras where StashedAVVMAddresses era ~ () (Allegra,Mary,Alonzo,Babbage,Conway)
-- The 'newEpochStateSpec' method (of (CardanoState era fn) class) in the instances for (Allegra,Mary,Alonzo,Babbage,Conway)
newEpochStateSpecUnit ::
  forall era fn.
  (CardanoState era fn, StashedAVVMAddresses era ~ ()) =>
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

exampleNewEpochStateUnit :: IO Bool
exampleNewEpochStateUnit = do
  pp <- generateSpec @(PParams Conway) pparamsSpec
  ioTest @(NewEpochState Conway) (newEpochStateSpec pp)

-- ==============================================================
-- The WellFormed class and instances
-- ==============================================================

class (HasSpec ConwayFn t, CardanoState era ConwayFn) => WellFormed t era where
  wffp :: PParams era -> Gen t
  wffp _ = wff @t @era
  wff :: Gen t
  wff = do
    pp <- specToGen @(PParams era) pparamsSpec
    wffp pp

instance CardanoState era ConwayFn => WellFormed (PParams era) era where
  wff = specToGen @(PParams era) pparamsSpec
  wffp p = pure p

instance CardanoState era ConwayFn => WellFormed AccountState era where
  wff = specToGen @AccountState accountStateSpec
  wffp _ = specToGen @AccountState accountStateSpec

instance CardanoState era ConwayFn => WellFormed (PState era) era where
  wff = do
    epoch <- specToGen @EpochNo epochNoSpec
    specToGen @(PState era) (pstateSpec (lit epoch))
  wffp _ = specToGen @(PState era) (pstateSpec testEpochNo)

instance CardanoState era ConwayFn => WellFormed (DState era) era where
  wff = do
    acct <- specToGen @AccountState accountStateSpec
    pools <-
      specToGen @(Map (KeyHash 'StakePool (EraCrypto era)) (PoolParams (EraCrypto era)))
        (hasSize (rangeSize 8 8))
    specToGen @(DState era) (dstateSpec (lit acct) (lit pools))

instance CardanoState era ConwayFn => WellFormed (VState era) era where
  wff = do
    epoch <- specToGen @EpochNo epochNoSpec
    specToGen @(VState era) (vstateSpec (lit epoch))

instance CardanoState era ConwayFn => WellFormed (CertState era) era where
  wff = do
    acct <- specToGen @AccountState accountStateSpec
    epoch <- specToGen @EpochNo epochNoSpec
    specToGen @(CertState era) (certStateSpec (lit acct) (lit epoch))

instance CardanoState era ConwayFn => WellFormed (UTxOState era) era where
  wffp pp = do
    certstate <- wff @(CertState era) @era
    specToGen @(UTxOState era) (utxoStateSpec pp (lit certstate))

instance CardanoState era ConwayFn => WellFormed (LedgerState era) era where
  wffp pp = do
    acct <- specToGen @AccountState accountStateSpec
    epoch <- specToGen @EpochNo epochNoSpec
    specToGen @(LedgerState era) (ledgerStateSpec pp (lit acct) (lit epoch))

instance CardanoState era ConwayFn => WellFormed (EpochState era) era where
  wffp pp = do
    epoch <- specToGen @EpochNo epochNoSpec
    specToGen @(EpochState era) (epochStateSpec pp (lit epoch))

instance CardanoState era ConwayFn => WellFormed (NewEpochState era) era where
  wffp pp = specToGen @(NewEpochState era) (newEpochStateSpec pp)

instance WellFormed (GovEnv Conway) Conway where
  wffp pp = specToGen @(GovEnv Conway) (govEnvSpec pp)

instance WellFormed (ConwayGovState Conway) Conway where
  wffp pp = do
    env <- specToGen @(GovEnv Conway) (govEnvSpec pp)
    specToGen @(ConwayGovState Conway) (conwayGovStateSpec pp env)

instance CardanoState era ConwayFn => WellFormed (ShelleyGovState era) era where
  wffp pp = specToGen @(ShelleyGovState era) (shelleyGovStateSpec pp)

instance (CardanoState era ConwayFn, c ~ EraCrypto era) => WellFormed (SnapShot c) era where
  wffp _ = specToGen @(SnapShot (EraCrypto era)) snapShotSpec
  wff = specToGen @(SnapShot (EraCrypto era)) snapShotSpec

instance (CardanoState era ConwayFn, c ~ EraCrypto era) => WellFormed (SnapShots c) era where
  wffp pp = do
    acct <- specToGen @AccountState accountStateSpec
    epoch <- specToGen @EpochNo epochNoSpec
    ls <- specToGen @(LedgerState era) (ledgerStateSpec pp (lit acct) (lit epoch))
    specToGen @(SnapShots (EraCrypto era)) (snapShotsSpec (lit (getMarkSnapShot ls)))

instance (CardanoState era ConwayFn, c ~ EraCrypto era) => WellFormed (InstantaneousRewards c) era where
  wff = do
    acct <- specToGen @AccountState accountStateSpec
    specToGen @(InstantaneousRewards (EraCrypto era)) (instantaneousRewardsSpec (lit acct))

-- =============================================================
-- helper functions for examples and tests

testwff :: forall p era. (WellFormed (p era) era, PrettyA (p era)) => IO ()
testwff = do
  x <- generate (wff @(p era) @era)
  putStrLn (show (prettyA x))

generateSpec :: forall a. HasSpec ConwayFn a => Specification ConwayFn a -> IO a
generateSpec specx = generate (genFromSpec @ConwayFn specx)

specToGen :: forall t. HasSpec ConwayFn t => Specification ConwayFn t -> Gen t
specToGen = genFromSpec

genSpec :: HasSpec ConwayFn a => Specification ConwayFn a -> IO a
genSpec = generateSpec

ioTest :: forall t. (HasSpec ConwayFn t, PrettyA t) => Specification ConwayFn t -> IO Bool
ioTest specx = do
  t <- generateSpec @t specx
  putStrLn (show (prettyA t))
  pure (conformsToSpec t specx)

{-

utxosDeposits_ ::
  ( EraTxOut era
  , IsNormalType (TxOut era)
  , HasSpec fn (TxOut era)
  , HasSpec fn (GovState era)
  , IsConwayUniv fn
  ) =>
  Term fn (UTxOState era) ->
  Term fn Coin
utxosDeposits_ = sel @1
-}

-- ===================================================================
-- HSpec tests
-- ===================================================================

soundSpec :: forall t. (HasSpec ConwayFn t, PrettyA t) => Specification ConwayFn t -> Gen Property
soundSpec specx = do
  x <- specToGen @t specx
  pure $
    property $
      counterexample (show ("Does not meet spec\n" <> prettyA x)) (conformsToSpec x specx)

soundSpecIt ::
  forall t.
  (HasSpec ConwayFn t, PrettyA t) => Int -> Specification ConwayFn t -> SpecWith (Arg Property)
soundSpecIt n specx = it (show (typeRep (Proxy @t))) $ withMaxSuccess n $ property $ (soundSpec @t specx)

spec :: Spec
spec = do
  describe "Soundnes of WellFormed types from the Cardano Ledger: " $ do
    soundSpecIt @(ProtVer, ProtVer) 100 protVersCanfollow
    soundSpecIt @(InstantaneousRewards StandardCrypto) 20 (instantaneousRewardsSpec testAcctState)

    soundSpecIt @(PState Shelley) 100 (pstateSpec testEpochNo)
    soundSpecIt @(PState Allegra) 100 (pstateSpec testEpochNo)
    soundSpecIt @(PState Mary) 100 (pstateSpec testEpochNo)
    soundSpecIt @(PState Alonzo) 100 (pstateSpec testEpochNo)
    soundSpecIt @(PState Babbage) 100 (pstateSpec testEpochNo)
    soundSpecIt @(PState Conway) 20 (pstateSpec testEpochNo)

    soundSpecIt @(DState Shelley) 50 (dstateSpec testAcctState (testPools @Shelley))
    soundSpecIt @(DState Allegra) 50 (dstateSpec testAcctState (testPools @Shelley))
    soundSpecIt @(DState Mary) 50 (dstateSpec testAcctState (testPools @Shelley))
    soundSpecIt @(DState Alonzo) 50 (dstateSpec testAcctState (testPools @Shelley))
    soundSpecIt @(DState Babbage) 50 (dstateSpec testAcctState (testPools @Shelley))
    soundSpecIt @(DState Conway) 20 (dstateSpec testAcctState (testPools @Shelley))

    soundSpecIt @(VState Shelley) 100 (vstateSpec testEpochNo)
    soundSpecIt @(VState Allegra) 100 (vstateSpec testEpochNo)
    soundSpecIt @(VState Mary) 100 (vstateSpec testEpochNo)
    soundSpecIt @(VState Alonzo) 100 (vstateSpec testEpochNo)
    soundSpecIt @(VState Babbage) 100 (vstateSpec testEpochNo)
    soundSpecIt @(VState Conway) 100 (vstateSpec testEpochNo)

    soundSpecIt @(CertState Shelley) 50 (certStateSpec testAcctState testEpochNo)
    soundSpecIt @(CertState Allegra) 50 (certStateSpec testAcctState testEpochNo)
    soundSpecIt @(CertState Mary) 50 (certStateSpec testAcctState testEpochNo)
    soundSpecIt @(CertState Alonzo) 50 (certStateSpec testAcctState testEpochNo)
    soundSpecIt @(CertState Babbage) 50 (certStateSpec testAcctState testEpochNo)
    soundSpecIt @(CertState Conway) 20 (certStateSpec testAcctState testEpochNo)

    soundSpecIt @(UTxO Shelley) 50 (utxoSpec testDelegations)
    soundSpecIt @(UTxO Allegra) 50 (utxoSpec testDelegations)
    soundSpecIt @(UTxO Mary) 50 (utxoSpec testDelegations)
    soundSpecIt @(UTxO Alonzo) 50 (utxoSpec testDelegations)
    soundSpecIt @(UTxO Babbage) 50 (utxoSpec testDelegations)
    soundSpecIt @(UTxO Conway) 50 (utxoSpec testDelegations)

    soundSpecIt @(GovState Shelley) 20 (govStateSpec @Shelley @ConwayFn testPP)
    soundSpecIt @(GovState Allegra) 20 (govStateSpec @Allegra @ConwayFn testPP)
    soundSpecIt @(GovState Mary) 20 (govStateSpec @Mary @ConwayFn testPP)
    soundSpecIt @(GovState Alonzo) 20 (govStateSpec @Alonzo @ConwayFn testPP)
    soundSpecIt @(GovState Babbage) 20 (govStateSpec @Babbage @ConwayFn testPP)
    soundSpecIt @(GovState Conway) 20 (govStateSpec @Conway @ConwayFn testPP)

    soundSpecIt @(UTxOState Shelley) 50 (utxoStateSpec testPP testCertState)
    soundSpecIt @(UTxOState Allegra) 50 (utxoStateSpec testPP testCertState)
    soundSpecIt @(UTxOState Mary) 50 (utxoStateSpec testPP testCertState)
    soundSpecIt @(UTxOState Alonzo) 50 (utxoStateSpec testPP testCertState)
    soundSpecIt @(UTxOState Babbage) 50 (utxoStateSpec testPP testCertState)
    soundSpecIt @(UTxOState Conway) 20 (utxoStateSpec testPP testCertState)

    soundSpecIt @(LedgerState Shelley) 50 (ledgerStateSpec testPP testAcctState testEpochNo)
    soundSpecIt @(LedgerState Allegra) 50 (ledgerStateSpec testPP testAcctState testEpochNo)
    soundSpecIt @(LedgerState Mary) 50 (ledgerStateSpec testPP testAcctState testEpochNo)
    soundSpecIt @(LedgerState Alonzo) 50 (ledgerStateSpec testPP testAcctState testEpochNo)
    soundSpecIt @(LedgerState Babbage) 50 (ledgerStateSpec testPP testAcctState testEpochNo)
    soundSpecIt @(LedgerState Conway) 20 (ledgerStateSpec testPP testAcctState testEpochNo)

    soundSpecIt @(EpochState Shelley) 20 (epochStateSpec testPP testEpochNo)
    soundSpecIt @(EpochState Allegra) 20 (epochStateSpec testPP testEpochNo)
    soundSpecIt @(EpochState Mary) 20 (epochStateSpec testPP testEpochNo)
    soundSpecIt @(EpochState Alonzo) 20 (epochStateSpec testPP testEpochNo)
    soundSpecIt @(EpochState Babbage) 20 (epochStateSpec testPP testEpochNo)
    soundSpecIt @(EpochState Conway) 20 (epochStateSpec testPP testEpochNo)

    soundSpecIt @(NewEpochState Shelley) 20 (newEpochStateSpec testPP)
    soundSpecIt @(NewEpochState Allegra) 20 (newEpochStateSpec testPP)
    soundSpecIt @(NewEpochState Mary) 20 (newEpochStateSpec testPP)
    soundSpecIt @(NewEpochState Alonzo) 20 (newEpochStateSpec testPP)
    soundSpecIt @(NewEpochState Babbage) 20 (newEpochStateSpec testPP)
    soundSpecIt @(NewEpochState Conway) 20 (newEpochStateSpec testPP)

    soundSpecIt @(SnapShots StandardCrypto)
      100
      (snapShotsSpec (lit (getMarkSnapShot (testLedgerState @Conway))))

-- ========================================
-- TODO FIXME The dependency on this needs to be debugged

test :: forall t era. (WellFormed t era, PrettyA t) => IO ()
test = do
  ls <- generate $ wff @t @era
  putStrLn (show (typeRep (Proxy @t)) ++ " " ++ show (length (show (prettyA ls))))

monadConformsToSpec :: forall fn a. HasSpec fn a => a -> Specification fn a -> Writer [String] Bool
monadConformsToSpec _ TrueSpec = pure True
monadConformsToSpec a (MemberSpec as) =
  if elem a as
    then pure True
    else tell [show a ++ " not an element of " ++ show as] >> pure False
monadConformsToSpec a (TypeSpec s cant) = do
  ans <- monadConformsTo @fn a s
  if notElem a cant && ans
    then pure True
    else tell [show a ++ " is an element of the cant set" ++ show cant] >> pure False
monadConformsToSpec a (SuspendedSpec v ps) =
  if checkPredPure (singletonEnv v a) ps
    then pure True
    else tell ["Suspended Spec " ++ show (SuspendedSpec v ps)] >> pure False
monadConformsToSpec _ (ErrorSpec es) = tell (NE.toList es) >> pure False

testAll :: IO ()
testAll = do
  test @(PParams Shelley) @Shelley
  test @AccountState @Shelley
  test @(PState Shelley) @Shelley
  test @(DState Shelley) @Shelley
  test @(VState Shelley) @Shelley
  test @(CertState Shelley) @Shelley
  test @(UTxOState Shelley) @Shelley
  test @(LedgerState Shelley) @Shelley
  test @(EpochState Shelley) @Shelley
  test @(NewEpochState Shelley) @Shelley
  test @(ShelleyGovState Shelley) @Shelley
  -- test @(SnapShot StandardCrypto) @Shelley
  test @(SnapShots StandardCrypto) @Shelley

  putStrLn ("\nAllega")
  test @(PParams Allegra) @Allegra
  test @AccountState @Allegra
  test @(PState Allegra) @Allegra
  test @(DState Allegra) @Allegra
  test @(VState Allegra) @Allegra
  test @(CertState Allegra) @Allegra
  test @(UTxOState Allegra) @Allegra
  test @(LedgerState Allegra) @Allegra
  test @(EpochState Allegra) @Allegra
  test @(NewEpochState Allegra) @Allegra
  test @(ShelleyGovState Allegra) @Allegra
  -- test @(SnapShot StandardCrypto) @Allegra
  test @(SnapShots StandardCrypto) @Allegra

  putStrLn ("\nMary")
  test @(PParams Mary) @Mary
  test @AccountState @Mary
  test @(PState Mary) @Mary
  test @(DState Mary) @Mary
  test @(VState Mary) @Mary
  test @(CertState Mary) @Mary
  test @(UTxOState Mary) @Mary
  test @(LedgerState Mary) @Mary
  test @(EpochState Mary) @Mary
  test @(NewEpochState Mary) @Mary
  test @(ShelleyGovState Mary) @Mary
  -- test @(SnapShot StandardCrypto) @Mary
  test @(SnapShots StandardCrypto) @Mary

  putStrLn ("\nAlonzo")
  test @(PParams Alonzo) @Alonzo
  test @AccountState @Alonzo
  test @(PState Alonzo) @Alonzo
  test @(DState Alonzo) @Alonzo
  test @(VState Alonzo) @Alonzo
  test @(CertState Alonzo) @Alonzo
  test @(UTxOState Alonzo) @Alonzo
  test @(LedgerState Alonzo) @Alonzo
  test @(EpochState Alonzo) @Alonzo
  test @(NewEpochState Alonzo) @Alonzo
  test @(ShelleyGovState Alonzo) @Alonzo
  -- test @(SnapShot StandardCrypto) @Alonzo
  test @(SnapShots StandardCrypto) @Alonzo

  putStrLn ("\nBabbage")
  test @(PParams Babbage) @Babbage
  test @AccountState @Babbage
  test @(PState Babbage) @Babbage
  test @(DState Babbage) @Babbage
  test @(VState Babbage) @Babbage
  test @(CertState Babbage) @Babbage
  test @(UTxOState Babbage) @Babbage
  test @(LedgerState Babbage) @Babbage
  test @(EpochState Babbage) @Babbage
  test @(NewEpochState Babbage) @Babbage
  test @(ShelleyGovState Babbage) @Babbage
  -- test @(SnapShot StandardCrypto) @Babbage
  test @(SnapShots StandardCrypto) @Babbage

  putStrLn ("\nConway")
  test @(PParams Conway) @Conway
  test @AccountState @Conway
  test @(PState Conway) @Conway
  test @(DState Conway) @Conway
  test @(VState Conway) @Conway
  test @(CertState Conway) @Conway
  test @(UTxOState Conway) @Conway
  test @(LedgerState Conway) @Conway
  test @(EpochState Conway) @Conway
  test @(NewEpochState Conway) @Conway
  test @(ConwayGovState Conway) @Conway
  -- test @(SnapShot StandardCrypto) @Conway
  test @(SnapShots StandardCrypto) @Conway

  test @(GovEnv Conway) @Conway
  test @(ConwayGovState Conway) @Conway
