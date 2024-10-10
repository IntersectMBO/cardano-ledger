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

-- | classes that support Era parametric Specifications.
--   I.e they work in all eras (Shelley,Allegra,Mary,Alonzo,Babbage,Conway)
--   In general, each class (except EraOut, see below) navigates the differences of a single type family.
--   The class (EraPP era) (Defined in ‘Test.Cardano.Ledger.Constrained.Conway.SimplePParams’)
--   and reExported here, supports specifications over the type Family (PParams era).
--   The class EraCert supports specifications over the type Family (TxCert era)
--   The class EraGov (with method  'govStateSpec') supports specifcations over type Family GovState.
--   The class EraOut (with method 'correctTxOut') supports specifcations over the type Family TxOut.
--   Additional support for phased out Type Families like InstantaneousRewards,
--   GenDelegs, FutureGenDelegs, StashedAVVMAddresses, and Ptrs, are handled by methods in EraOut
module Test.Cardano.Ledger.Constrained.Conway.ParametricSpec (
  module SimplePParams,
  EraOut (..),
  EraCert (..),
  EraDeleg (..),
  delegatedStakeReference,
  CertKey (..),
) where

import Cardano.Ledger.Allegra (Allegra)
import Cardano.Ledger.Alonzo (Alonzo)
import Cardano.Ledger.Alonzo.TxOut (AlonzoEraTxOut (..), AlonzoTxOut (..))
import Cardano.Ledger.Babbage (Babbage)
import Cardano.Ledger.Babbage.TxOut (BabbageTxOut (..))
import Cardano.Ledger.BaseTypes hiding (inject)
import Cardano.Ledger.CertState
import Cardano.Ledger.Coin (Coin (..), DeltaCoin (..))
import Cardano.Ledger.Conway (Conway)
import Cardano.Ledger.Conway.Rules (CertEnv (..))
import Cardano.Ledger.Conway.TxCert
import Cardano.Ledger.Core
import Cardano.Ledger.Credential (Credential, StakeReference (..))
import Cardano.Ledger.Crypto (Crypto)
import Cardano.Ledger.Keys (KeyHash, KeyRole (..))
import Cardano.Ledger.Mary (Mary, MaryValue)
import Cardano.Ledger.PoolParams (PoolParams (ppId))
import Cardano.Ledger.Shelley (Shelley)
import Cardano.Ledger.Shelley.LedgerState (AccountState (..), StashedAVVMAddresses)
import Cardano.Ledger.Shelley.TxCert
import Cardano.Ledger.Shelley.TxOut (ShelleyTxOut (..))
import Constrained hiding (Value)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Word (Word64)
import Test.Cardano.Ledger.Constrained.Conway.Cert (conwayTxCertSpec, shelleyTxCertSpec)
import Test.Cardano.Ledger.Constrained.Conway.Deleg (EraDeleg (..))
import Test.Cardano.Ledger.Constrained.Conway.Instances (ConwayFn, IsConwayUniv, toDelta_)
import Test.Cardano.Ledger.Constrained.Conway.SimplePParams
import qualified Test.Cardano.Ledger.Constrained.Conway.SimplePParams as SimplePParams

-- ===========================================================

-- | The class EraOut supports Era parametric Specifications that
--   primarily navigate the differences in types parameterized type Family TxOut.
--   Additional support for phased out Type Families like InstantaneousRewards,
--   GenDelegs, FutureGenDelegs, StashedAVVMAddresses, and Ptrs, are also provided
class
  ( HasSpec fn (TxOut era)
  , IsNormalType (TxOut era)
  , HasSpec fn (StashedAVVMAddresses era)
  , EraTxOut era
  , IsConwayUniv fn
  , EraPP era
  , EraDeleg era
  ) =>
  EraOut era fn
  where
  irewardSpec :: Term fn AccountState -> Specification fn (InstantaneousRewards (EraCrypto era))
  hasPtrs :: proxy era -> Term fn Bool
  correctTxOut ::
    Term fn (Map (Credential 'Staking (EraCrypto era)) (KeyHash 'StakePool (EraCrypto era))) ->
    Term fn (TxOut era) ->
    Pred fn

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

instance IsConwayUniv fn => EraOut Shelley fn where
  irewardSpec = instantaneousRewardsSpec
  hasPtrs _proxy = lit True
  correctTxOut = betterTxOutShelley

instance IsConwayUniv fn => EraOut Allegra fn where
  irewardSpec = instantaneousRewardsSpec
  hasPtrs _proxy = lit True
  correctTxOut = betterTxOutShelley

instance IsConwayUniv fn => EraOut Mary fn where
  irewardSpec = instantaneousRewardsSpec
  hasPtrs _proxy = lit True
  correctTxOut = betterTxOutMary

instance IsConwayUniv fn => EraOut Alonzo fn where
  irewardSpec = instantaneousRewardsSpec
  hasPtrs _proxy = lit True
  correctTxOut = betterTxOutAlonzo

instance IsConwayUniv fn => EraOut Babbage fn where
  irewardSpec = instantaneousRewardsSpec
  hasPtrs _proxy = lit True
  correctTxOut = betterTxOutBabbage

instance IsConwayUniv fn => EraOut Conway fn where
  irewardSpec _ = constrained $ \ [var|irewards|] ->
    match irewards $ \ [var|reserves|] [var|treasury|] [var|deltaRes|] [var|deltaTreas|] ->
      [ reserves ==. lit Map.empty
      , treasury ==. lit Map.empty
      , deltaRes ==. lit (DeltaCoin 0)
      , deltaTreas ==. lit (DeltaCoin 0)
      ]
  hasPtrs _proxy = lit False
  correctTxOut = betterTxOutBabbage

-- =========================================================================
-- Making Cert Era parametric with the EraCert class

class (Era era, HasSpec ConwayFn (TxCert era)) => EraCert era where
  txCertSpec :: IsConwayUniv fn => CertEnv era -> CertState era -> Specification fn (TxCert era)
  txCertKey :: TxCert era -> CertKey (EraCrypto era)

instance EraCert Shelley where txCertSpec = shelleyTxCertSpec; txCertKey = shelleyTxCertKey
instance EraCert Allegra where txCertSpec = shelleyTxCertSpec; txCertKey = shelleyTxCertKey
instance EraCert Mary where txCertSpec = shelleyTxCertSpec; txCertKey = shelleyTxCertKey
instance EraCert Alonzo where txCertSpec = shelleyTxCertSpec; txCertKey = shelleyTxCertKey
instance EraCert Babbage where txCertSpec = shelleyTxCertSpec; txCertKey = shelleyTxCertKey
instance EraCert Conway where txCertSpec = conwayTxCertSpec; txCertKey = conwayTxCertKey

-- | Used to aggregate the key used in registering a Certificate. Different
--   certificates use different kinds of Keys, that allows us to use one
--   type to represent all kinds of keys (Similar to DepositPurpose)
data CertKey c
  = StakeKey !(Credential 'Staking c)
  | PoolKey !(KeyHash 'StakePool c)
  | DRepKey !(Credential 'DRepRole c)
  | ColdKey !(Credential 'ColdCommitteeRole c)
  | GenesisKey !(KeyHash 'Genesis c)
  | MirKey !MIRPot
  deriving (Eq, Show, Ord)

-- | Compute the aggregate key type of a Certificater
conwayTxCertKey :: ConwayTxCert era -> CertKey (EraCrypto era)
conwayTxCertKey (ConwayTxCertDeleg (ConwayRegCert x _)) = StakeKey x
conwayTxCertKey (ConwayTxCertDeleg (ConwayUnRegCert x _)) = StakeKey x
conwayTxCertKey (ConwayTxCertDeleg (ConwayDelegCert x _)) = StakeKey x
conwayTxCertKey (ConwayTxCertDeleg (ConwayRegDelegCert x _ _)) = StakeKey x
conwayTxCertKey (ConwayTxCertPool (RegPool x)) = PoolKey (ppId x)
conwayTxCertKey (ConwayTxCertPool (RetirePool x _)) = PoolKey x
conwayTxCertKey (ConwayTxCertGov (ConwayRegDRep x _ _)) = DRepKey x
conwayTxCertKey (ConwayTxCertGov (ConwayUnRegDRep x _)) = DRepKey x
conwayTxCertKey (ConwayTxCertGov (ConwayUpdateDRep x _)) = DRepKey x
conwayTxCertKey (ConwayTxCertGov (ConwayAuthCommitteeHotKey x _)) = ColdKey x
conwayTxCertKey (ConwayTxCertGov (ConwayResignCommitteeColdKey x _)) = ColdKey x

shelleyTxCertKey :: ShelleyTxCert era -> CertKey (EraCrypto era)
shelleyTxCertKey (ShelleyTxCertDelegCert (ShelleyRegCert x)) = StakeKey x
shelleyTxCertKey (ShelleyTxCertDelegCert (ShelleyUnRegCert x)) = StakeKey x
shelleyTxCertKey (ShelleyTxCertDelegCert (ShelleyDelegCert x _)) = StakeKey x
shelleyTxCertKey (ShelleyTxCertPool (RegPool x)) = PoolKey (ppId x)
shelleyTxCertKey (ShelleyTxCertPool (RetirePool x _)) = PoolKey x
shelleyTxCertKey (ShelleyTxCertGenesisDeleg (GenesisDelegCert a _ _)) = GenesisKey a
shelleyTxCertKey (ShelleyTxCertMir (MIRCert p _)) = MirKey p
