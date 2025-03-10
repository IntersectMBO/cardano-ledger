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

-- | classes that support Era parametric Specifications.
--   I.e they work in all eras (Shelley,Allegra,Mary,Alonzo,Babbage,Conway)
--   In general, each class (except EraSpecTxOut, see below) navigates the differences of a single type family.
--   The class (EraSpecPParams era) (Defined in ‘Test.Cardano.Ledger.Constrained.Conway.SimplePParams’)
--   and reExported here, supports specifications over the type Family (PParams era).
--   The class EraSpecCert supports specifications over the type Family (TxCert era)
--   The class EraSpecLedger, with methods 'govStateSpec' and 'newEpochStateSpec', support Parametric Ledger types.
--   The class EraSpecTxOut (with method 'correctTxOut' and others) supports specifcations over the type Family TxOut.
--   Additional support for phased out Type Families like InstantaneousRewards,
--   GenDelegs, FutureGenDelegs, StashedAVVMAddresses, and Ptrs, are handled by methods in EraSpecTxOut
module Test.Cardano.Ledger.Constrained.Conway.ParametricSpec (
  module SimplePParams,
  EraSpecTxOut (..),
  txOutSpec,
  EraSpecCert (..),
  EraSpecDeleg (..),
  delegatedStakeReference,
  CertKey (..),
) where

import Cardano.Ledger.Address (Addr (..))
import Cardano.Ledger.Allegra (AllegraEra)
import Cardano.Ledger.Alonzo (AlonzoEra)
import Cardano.Ledger.Babbage (BabbageEra)
import Cardano.Ledger.BaseTypes hiding (inject)
import Cardano.Ledger.Coin (Coin (..), DeltaCoin (..))
import Cardano.Ledger.Conway (ConwayEra)
import Cardano.Ledger.Core
import Cardano.Ledger.Credential (Credential, StakeReference (..))
import Cardano.Ledger.Mary (MaryEra)
import Cardano.Ledger.Shelley (ShelleyEra)
import Cardano.Ledger.Shelley.LedgerState (AccountState (..), StashedAVVMAddresses)
import Constrained.API
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Word (Word64)
import Test.Cardano.Ledger.Constrained.Conway.Cert (CertKey (..), EraSpecCert (..))
import Test.Cardano.Ledger.Constrained.Conway.Deleg (EraSpecDeleg (..))
import Test.Cardano.Ledger.Constrained.Conway.Instances.Ledger (
  maryValueCoin_,
  toDelta_,
 )
import Test.Cardano.Ledger.Constrained.Conway.Instances.PParams
import qualified Test.Cardano.Ledger.Constrained.Conway.Instances.PParams as SimplePParams
import Test.Cardano.Ledger.Constrained.Conway.WitnessUniverse (
  GenScript,
  WitUniv (..),
  witBootstrapAddress,
  witness,
 )

-- ===========================================================

-- | The class EraSpecTxOut supports Era parametric Specifications that
--   primarily navigate the differences in types parameterized type Family TxOut.
--   Additional support for phased out Type Families like InstantaneousRewards,
--   GenDelegs, FutureGenDelegs, StashedAVVMAddresses, and Ptrs, are also provided
class
  ( HasSpec (StashedAVVMAddresses era)
  , EraSpecPParams era
  , EraSpecDeleg era
  , HasSpec (TxOut era)
  , IsNormalType (TxOut era)
  , EraTxOut era
  , GenScript era
  ) =>
  EraSpecTxOut era
  where
  irewardSpec ::
    WitUniv era -> Term AccountState -> Specification InstantaneousRewards
  hasPtrs :: proxy era -> Term Bool

  -- | Extract a Value from a TxOut
  txOutValue_ :: Term (TxOut era) -> Term (Value era)

  -- | Extract a Coin from a TxOut
  txOutCoin_ :: Term (TxOut era) -> Term Coin

  -- | Extract an Addr from a TxOut
  txOutAddr_ :: Term (TxOut era) -> Term Addr

instance EraSpecTxOut ShelleyEra where
  irewardSpec = instantaneousRewardsSpec
  hasPtrs _proxy = lit True

  txOutValue_ x = sel @1 x
  txOutCoin_ x = sel @1 x
  txOutAddr_ x = sel @0 x

instance EraSpecTxOut AllegraEra where
  irewardSpec = instantaneousRewardsSpec
  hasPtrs _proxy = lit True

  txOutValue_ x = sel @1 x
  txOutCoin_ x = sel @1 x
  txOutAddr_ x = sel @0 x

instance EraSpecTxOut MaryEra where
  irewardSpec = instantaneousRewardsSpec
  hasPtrs _proxy = lit True

  txOutValue_ x = sel @1 x
  txOutCoin_ x = maryValueCoin_ (sel @1 x)
  txOutAddr_ x = sel @0 x

instance EraSpecTxOut AlonzoEra where
  irewardSpec = instantaneousRewardsSpec
  hasPtrs _proxy = lit True

  txOutValue_ x = sel @1 x
  txOutCoin_ x = maryValueCoin_ (sel @1 x)
  txOutAddr_ x = sel @0 x

instance EraSpecTxOut BabbageEra where
  irewardSpec = instantaneousRewardsSpec
  hasPtrs _proxy = lit True

  txOutValue_ x = sel @1 x
  txOutCoin_ x = maryValueCoin_ (sel @1 x)
  txOutAddr_ x = sel @0 x

instance EraSpecTxOut ConwayEra where
  irewardSpec _ _ = constrained $ \ [var|irewards|] ->
    match irewards $ \ [var|reserves|] [var|treasury|] [var|deltaRes|] [var|deltaTreas|] ->
      [ reserves ==. lit Map.empty
      , treasury ==. lit Map.empty
      , deltaRes ==. lit (DeltaCoin 0)
      , deltaTreas ==. lit (DeltaCoin 0)
      ]
  hasPtrs _proxy = lit False

  txOutValue_ x = sel @1 x
  txOutCoin_ x = maryValueCoin_ (sel @1 x)
  txOutAddr_ x = sel @0 x

-- ===========================================================================

-- | An Era polymorhic Specification for type family TxOut
txOutSpec ::
  forall era.
  EraSpecTxOut era =>
  WitUniv era ->
  Term (Map (Credential 'Staking) (KeyHash 'StakePool)) ->
  Term (TxOut era) ->
  Pred
txOutSpec univ delegs txOut =
  And
    [ assert $ 0 <. txOutCoin_ @era txOut
    , assert $ txOutCoin_ @era txOut <=. fromIntegral (maxBound :: Word64)
    , (caseOn (txOutAddr_ @era txOut))
        -- Network -> Credential -> StakeRefernce -> Addr
        ( branchW 2 $ \ [var|network|] [var|payCred|] [var|stakeref|] ->
            [ witness univ payCred -- satisfies payCred (payCredSpec univ)
            , assert $ network ==. lit Testnet
            , satisfies stakeref (delegatedStakeReference delegs)
            ]
        )
        -- BootstrapAddress -> Addr
        (branchW 1 $ \bootstrapAddr -> satisfies bootstrapAddr (witBootstrapAddress univ))
    ]

-- | Generate random Stake references that have a high probability of being delegated.
delegatedStakeReference ::
  Term (Map (Credential 'Staking) (KeyHash 'StakePool)) ->
  Specification StakeReference
delegatedStakeReference delegs =
  constrained $ \ [var|ref|] ->
    caseOn
      ref
      (branchW 9 $ \ [var|base|] -> member_ base (dom_ delegs))
      (branchW 1 $ \_ptr -> True)
      (branchW 1 $ \_null -> True) -- just an occaisional NullRef

instantaneousRewardsSpec ::
  forall era.
  Era era =>
  WitUniv era ->
  Term AccountState ->
  Specification InstantaneousRewards
instantaneousRewardsSpec univ acct = constrained $ \ [var| irewards |] ->
  match acct $ \ [var| acctRes |] [var| acctTreas |] ->
    match irewards $ \ [var| reserves |] [var| treasury |] [var| deltaRes |] [var| deltaTreas |] ->
      [ dependsOn acctRes reserves
      , dependsOn acctRes deltaRes
      , dependsOn acctTreas treasury
      , dependsOn acctTreas deltaTreas
      , witness univ (dom_ reserves)
      , witness univ (dom_ treasury)
      , assertExplain (pure "deltaTreausry and deltaReserves sum to 0") $ negate deltaRes ==. deltaTreas
      , forAll (rng_ reserves) (\ [var| x |] -> x >=. (lit (Coin 0)))
      , forAll (rng_ treasury) (\ [var| y |] -> y >=. (lit (Coin 0)))
      , assert $ toDelta_ (foldMap_ id (rng_ reserves)) - deltaRes <=. toDelta_ acctRes
      , assert $ toDelta_ (foldMap_ id (rng_ treasury)) - deltaTreas <=. toDelta_ acctTreas
      ]
