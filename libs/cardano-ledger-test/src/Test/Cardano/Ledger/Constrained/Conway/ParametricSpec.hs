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
  EraSpecCert (..),
  EraSpecDeleg (..),
  delegatedStakeReference,
  CertKey (..),
) where

import Cardano.Ledger.Allegra (AllegraEra)
import Cardano.Ledger.Alonzo (AlonzoEra)
import Cardano.Ledger.Alonzo.TxOut (AlonzoEraTxOut (..), AlonzoTxOut (..))
import Cardano.Ledger.Babbage (BabbageEra)
import Cardano.Ledger.Babbage.TxOut (BabbageTxOut (..))
import Cardano.Ledger.BaseTypes hiding (inject)
import Cardano.Ledger.CertState
import Cardano.Ledger.Coin (Coin (..), DeltaCoin (..))
import Cardano.Ledger.Conway (ConwayEra)
import Cardano.Ledger.Core
import Cardano.Ledger.Credential (Credential, StakeReference (..))
import Cardano.Ledger.Keys (KeyHash, KeyRole (..))
import Cardano.Ledger.Mary (MaryEra, MaryValue)
import Cardano.Ledger.Shelley (ShelleyEra)
import Cardano.Ledger.Shelley.LedgerState (AccountState (..), StashedAVVMAddresses)
import Cardano.Ledger.Shelley.TxOut (ShelleyTxOut (..))
import Constrained hiding (Value)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Word (Word64)
import Test.Cardano.Ledger.Constrained.Conway.Cert (CertKey (..), EraSpecCert (..))
import Test.Cardano.Ledger.Constrained.Conway.Deleg (EraSpecDeleg (..))
import Test.Cardano.Ledger.Constrained.Conway.Instances.Ledger (
  IsConwayUniv,
  maryValueCoin_,
  toDelta_,
 )
import Test.Cardano.Ledger.Constrained.Conway.Instances.PParams
import qualified Test.Cardano.Ledger.Constrained.Conway.Instances.PParams as SimplePParams

-- ===========================================================

-- | The class EraSpecTxOut supports Era parametric Specifications that
--   primarily navigate the differences in types parameterized type Family TxOut.
--   Additional support for phased out Type Families like InstantaneousRewards,
--   GenDelegs, FutureGenDelegs, StashedAVVMAddresses, and Ptrs, are also provided
class
  ( HasSpec fn (StashedAVVMAddresses era)
  , EraSpecPParams era
  , EraSpecDeleg era
  , HasSpec fn (TxOut era)
  , IsNormalType (TxOut era)
  , EraTxOut era
  , IsConwayUniv fn
  ) =>
  EraSpecTxOut era fn
  where
  irewardSpec :: Term fn AccountState -> Specification fn (InstantaneousRewards)
  hasPtrs :: proxy era -> Term fn Bool
  correctTxOut ::
    Term fn (Map (Credential 'Staking) (KeyHash 'StakePool)) ->
    Term fn (TxOut era) ->
    Pred fn

  -- | Extract a Value from a TxOut
  txOutValue_ :: Term fn (TxOut era) -> Term fn (Value era)

  -- | Extract a Coin from a TxOut
  txOutCoin_ :: Term fn (TxOut era) -> Term fn Coin

betterTxOutShelley ::
  (EraTxOut era, Value era ~ Coin, IsConwayUniv fn) =>
  Term fn (Map (Credential 'Staking) (KeyHash 'StakePool)) ->
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
  (EraTxOut era, Value era ~ MaryValue, IsConwayUniv fn) =>
  Term fn (Map (Credential 'Staking) (KeyHash 'StakePool)) ->
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
  (AlonzoEraTxOut era, Value era ~ MaryValue, IsConwayUniv fn) =>
  Term fn (Map (Credential 'Staking) (KeyHash 'StakePool)) ->
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
  , Value era ~ MaryValue
  , IsNormalType (Script era)
  , HasSpec fn (Script era)
  , IsConwayUniv fn
  ) =>
  Term fn (Map (Credential 'Staking) (KeyHash 'StakePool)) ->
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
  IsConwayUniv fn =>
  Term fn (Map (Credential 'Staking) (KeyHash 'StakePool)) ->
  Specification fn StakeReference
delegatedStakeReference delegs =
  constrained $ \ [var|ref|] ->
    caseOn
      ref
      (branchW 9 $ \ [var|base|] -> member_ base (dom_ delegs))
      (branchW 0 $ \_ptr -> False)
      (branchW 1 $ \_null -> True) -- just an occaisional NullRef

instantaneousRewardsSpec ::
  forall fn.
  IsConwayUniv fn =>
  Term fn AccountState ->
  Specification fn InstantaneousRewards
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

instance IsConwayUniv fn => EraSpecTxOut ShelleyEra fn where
  irewardSpec = instantaneousRewardsSpec
  hasPtrs _proxy = lit True
  correctTxOut = betterTxOutShelley
  txOutValue_ x = sel @1 x
  txOutCoin_ x = sel @1 x

instance IsConwayUniv fn => EraSpecTxOut AllegraEra fn where
  irewardSpec = instantaneousRewardsSpec
  hasPtrs _proxy = lit True
  correctTxOut = betterTxOutShelley
  txOutValue_ x = sel @1 x
  txOutCoin_ x = sel @1 x

instance IsConwayUniv fn => EraSpecTxOut MaryEra fn where
  irewardSpec = instantaneousRewardsSpec
  hasPtrs _proxy = lit True
  correctTxOut = betterTxOutMary
  txOutValue_ x = sel @1 x
  txOutCoin_ x = maryValueCoin_ (sel @1 x)

instance IsConwayUniv fn => EraSpecTxOut AlonzoEra fn where
  irewardSpec = instantaneousRewardsSpec
  hasPtrs _proxy = lit True
  correctTxOut = betterTxOutAlonzo
  txOutValue_ x = sel @1 x
  txOutCoin_ x = maryValueCoin_ (sel @1 x)

instance IsConwayUniv fn => EraSpecTxOut BabbageEra fn where
  irewardSpec = instantaneousRewardsSpec
  hasPtrs _proxy = lit True
  correctTxOut = betterTxOutBabbage
  txOutValue_ x = sel @1 x
  txOutCoin_ x = maryValueCoin_ (sel @1 x)

instance IsConwayUniv fn => EraSpecTxOut ConwayEra fn where
  irewardSpec _ = constrained $ \ [var|irewards|] ->
    match irewards $ \ [var|reserves|] [var|treasury|] [var|deltaRes|] [var|deltaTreas|] ->
      [ reserves ==. lit Map.empty
      , treasury ==. lit Map.empty
      , deltaRes ==. lit (DeltaCoin 0)
      , deltaTreas ==. lit (DeltaCoin 0)
      ]
  hasPtrs _proxy = lit False
  correctTxOut = betterTxOutBabbage
  txOutValue_ x = sel @1 x
  txOutCoin_ x = maryValueCoin_ (sel @1 x)
