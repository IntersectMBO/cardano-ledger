{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE UndecidableSuperClasses #-}
{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Test.Cardano.Ledger.Constrained.Conway.TxBodySpec where

import Cardano.Ledger.BaseTypes (Network (..))
import Cardano.Ledger.Coin
import Cardano.Ledger.Conway (ConwayEra)
import Cardano.Ledger.Conway.Rules (CertsEnv (..))
import Cardano.Ledger.Core

-- import Cardano.Ledger.Shelley.LedgerState (AccountState (..))
import Cardano.Ledger.Shelley.TxBody (ShelleyTxBody (..))
import Cardano.Ledger.State (UTxO (..), coinBalance)
import Cardano.Ledger.TxIn (TxIn (..))
import Cardano.Ledger.Val
import Constrained hiding (Value)
import Constrained.Base
import Data.Foldable (toList)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Set (Set)
import Data.Word (Word64)

import Data.Sequence.Internal (Seq)
import Lens.Micro

import Test.Cardano.Ledger.Constrained.Conway.Cert (
  EraSpecCert (..),
  certStateSpec,
  delegateeSpec,
  shelleyTxCertSpec,
 )
import Test.Cardano.Ledger.Constrained.Conway.Certs (certsEnvSpec, projectEnv)
import Test.Cardano.Ledger.Constrained.Conway.Instances
import Test.Cardano.Ledger.Constrained.Conway.Instances.TxBody (fromShelleyBody)
import Test.Cardano.Ledger.Constrained.Conway.LedgerTypes.Specs (EraSpecLedger)

import Test.Cardano.Ledger.Constrained.Conway.ParametricSpec
import Test.Cardano.Ledger.Generic.Proof (Reflect)
import qualified Test.Cardano.Ledger.Generic.Proof as Proof
import Test.QuickCheck hiding (forAll, witness)
import Prelude hiding (seq)

import Cardano.Ledger.Address (Withdrawals (..))
import Cardano.Ledger.Allegra (AllegraEra)
import Cardano.Ledger.Alonzo (AlonzoEra)
import Cardano.Ledger.Babbage (BabbageEra)
import Cardano.Ledger.Conway.State
import Cardano.Ledger.Mary (MaryEra)
import Cardano.Ledger.Shelley (ShelleyEra)
import Cardano.Ledger.Shelley.AdaPots (Consumed (..), Produced (..), consumedTxBody, producedTxBody)
import Cardano.Ledger.Shelley.LedgerState (CertState, PState (..))
import Cardano.Ledger.Shelley.State (ShelleyCertState)
import Cardano.Ledger.State (EraCertState (..), lookupDepositDState)
import Data.Text (pack)
import Lens.Micro
import Prettyprinter (sep, vsep)
import Test.Cardano.Ledger.Generic.Fields (
  TxBodyField (..),
  abstractTxBody,
 )
import Test.Cardano.Ledger.Generic.PrettyCore (
  PDoc,
  PrettyA (..),
  pcTxBodyField,
  pcTxBodyWithUTxO,
  pcTxIn,
  pcTxOut,
  ppList,
  ppMap,
  ppRecord,
  ppString,
 )

import Test.Cardano.Ledger.Constrained.Conway.WitnessUniverse

-- =================================
-- Move these to the MapSpec

subMap ::
  (Ord k, IsNormalType v, HasSpec fn k, HasSpec fn v) =>
  Specification fn (Map k v, Map k v)
subMap = constrained $ \ [var|pair|] ->
  match pair $ \ [var|sub|] [var|super|] ->
    [ dependsOn sub super
    , assert $ super /=. lit (Map.empty)
    , assert $ subset_ (dom_ sub) (dom_ super)
    , forAll sub $ \ [var|kvpair|] ->
        match kvpair $ \k v -> [dependsOn v k, onJust (lookup_ k super) (\a -> v ==. a)]
    ]

subMapSubDependsOnSuper ::
  (Ord k, IsNormalType v, HasSpec fn k, HasSpec fn v) =>
  Term fn (Map k v) ->
  Term fn (Map k v) ->
  Pred fn
subMapSubDependsOnSuper sub super =
  Block
    [ dependsOn sub super
    , assert $ super /=. lit (Map.empty)
    , assert $ subset_ (dom_ sub) (dom_ super)
    , forAll sub $ \ [var|kvpair|] ->
        match kvpair $ \k v -> [dependsOn v k, onJust (lookup_ k super) (\a -> v ==. a)]
    ]

subMapSuperDependsOnSub ::
  (Ord k, IsNormalType v, HasSpec fn k, HasSpec fn v) =>
  Term fn (Map k v) ->
  Term fn (Map k v) ->
  Pred fn
subMapSuperDependsOnSub sub super =
  Block
    [ dependsOn super sub
    , assert $ super /=. lit (Map.empty)
    , assert $ subset_ (dom_ sub) (dom_ super)
    , forAll sub $ \ [var|kvpair|] ->
        match kvpair $ \k v -> [onJust (lookup_ k super) (\a -> v ==. a)]
    ]

sumTxOut_ :: forall fn era. EraSpecTxOut era fn => Term fn [TxOut era] -> Term fn Coin
sumTxOut_ x = foldMap_ (txOutCoin_ @era @fn) x

sumCoin_ :: forall fn. IsConwayUniv fn => Term fn [Coin] -> Term fn Coin
sumCoin_ x = foldMap_ id x

adjustTxOutCoin :: EraTxOut era => DeltaCoin -> TxOut era -> TxOut era
adjustTxOutCoin (DeltaCoin n) x = x & coinTxOutL .~ ((x ^. coinTxOutL) <+> (Coin n))

-- | Extract the total deposits and refunds from a list of TxCerts.
--   This a kind of AdaPot relative to the Certs in a Transaction body
--   It depends on the PParams (deposit ammounts for registering a staking key, a ppol, and registering a Drep)
--   and on the CertState (what deposits were made in the past)
getDepositRefund ::
  forall era.
  (EraTxCert era, ConwayEraCertState era) =>
  PParams era -> CertState era -> [TxCert era] -> (DeltaCoin, DeltaCoin)
getDepositRefund pp certState certs =
  ( delta $ getTotalDepositsTxCerts pp (`Map.member` psStakePoolParams ps) certs
  , delta $ getTotalRefundsTxCerts pp (lookupDepositDState ds) (lookupDepositVState vs) certs
  )
  where
    delta (Coin n) = DeltaCoin n
    vs = certState ^. certVStateL
    ps = certState ^. certPStateL
    ds = certState ^. certDStateL

-- | This is exactly the same as reify, except it names the existential varaible for better error messages
reifyX ::
  ( HasSpec fn a
  , HasSpec fn b
  , IsPred p fn
  ) =>
  Term fn a ->
  (a -> b) ->
  (Term fn b -> p) ->
  Pred fn
reifyX t f body =
  exists (\eval -> pure $ f (eval t)) $ \ [var|reifyvar|] ->
    -- NOTE we name the existenital variable 'reifyvar'
    [ reifies reifyvar t f
    , toPredExplain (pure ("reifies " ++ show reifyvar)) $ body reifyvar
    ]

-- =======================================================================

-- | This is the first step to generating balanced TxBodies. It illustrates several techniques
--   1) Generate a tuple of related types. Previously we relied on generating one type and then
--      passing the actual generated value as the input of the next generator. This is an alternative to that.
--      But note we still rely partly on the old technique because we take CertsEnv and CertState values as input.
--   2) Carefully using dependsOn, to make explicit the order the code writer thinks the the variables
--      should be solved in. This makes failures less mysterious. Because variable ordering failures
--      are mysterious and hard to solve
--   3) The use of nested reify (here reifyX which just makes error messages better). This supports
--      using complicated Haskell functions to extract one random value, from a previous solved variable
--      using a pure function. Examples of this are 'getDepositRefund' and 'adjustTxOutCoin'
--   4) How complicated balancing constraints can be solved declaratively, rather than algorithmically
--      i.e.  toDelta_ (sumTxOut_ @fn @era outputs) ==. inputS + with + refund - deposit - f
bodyspec ::
  forall era fn.
  ( EraSpecTxOut era fn
  , EraSpecCert era fn
  , EraSpecTxCert fn era
  , ConwayEraCertState era
  ) =>
  WitUniv era ->
  CertsEnv era ->
  CertState era ->
  Specification
    fn
    ( ShelleyTxBody era
    , Map TxIn (TxOut era)
    , TxIn
    )
bodyspec univ certsenv certstate =
  constrained' $ \ [var|shelleyBody|] [var|utxo|] [var|feeInput|] ->
    match shelleyBody $
      \ [var|inputs|] [var|outputs|] [var|certs|] [var|withdrawals|] [var|fee|] _ [var|update|] _ ->
        exists (\eval -> pure $ Map.restrictKeys (eval utxo) (eval inputs)) $ \ [var|utxosubset|] ->
          exists (\eval -> pure $ Map.adjust (adjustTxOutCoin (DeltaCoin 0)) (eval feeInput) (eval utxo)) $ \ [var|tempUtxo|] ->
            [ assert $ update ==. lit Nothing
            , satisfies utxosubset (hasSize (rangeSize 3 4))
            , forAll' utxosubset $ \_ [var|out|] -> assert $ txOutCoin_ @era @fn out >. lit (Coin 0)
            , dependsOn feeInput utxosubset
            , assert $ member_ feeInput (dom_ utxosubset)
            , dependsOn fee feeInput
            , onJust (lookup_ feeInput utxosubset) (\ [var|feeTxout|] -> fee ==. txOutCoin_ @era @fn feeTxout)
            , dependsOn inputs utxosubset
            , assert $ inputs ==. dom_ utxosubset
            , assert $ member_ feeInput inputs
            , dependsOn tempUtxo utxosubset
            , satisfies (dom_ tempUtxo) (hasSize (rangeSize 8 10))
            , subMapSuperDependsOnSub utxosubset tempUtxo
            , forAll' tempUtxo $ \_ [var|out|] -> assert $ txOutCoin_ @era @fn out >. lit (Coin 0)
            , -- Certs has no dependencies
              forAll certs $ \ [var|oneCert|] -> satisfies oneCert (witTxCert univ)
            , assert $ sizeOf_ certs ==. 4
            , -- withdrawals hs no dependencies
              assert $ sizeOf_ withdrawals ==. lit 2
            , forAll' withdrawals $ \ [var|acct|] [var|val|] ->
                [ assert $ val <=. lit (Coin 10)
                , assert $ val >. lit (Coin 0)
                , match acct $ \ [var|network|] _ -> assert $ network ==. lit Testnet
                ]
            , dependsOn outputs certs
            , dependsOn outputs fee
            , dependsOn outputs withdrawals
            , dependsOn outputs utxosubset
            , assert $ sizeOf_ outputs ==. 4
            , forAll outputs $ \ [var|oneoutput|] -> txOutCoin_ @era @fn oneoutput >=. lit (Coin 0)
            , reifyX (toDelta_ fee) id $ \ [var|f|] ->
                reifyX (toDelta_ (sumTxOut_ @fn @era (rng_ utxosubset))) id $ \ [var|inputS|] ->
                  reifyX (toDelta_ (sumCoin_ @fn (rng_ withdrawals))) id $ \ [var|with|] ->
                    reify' certs (getDepositRefund @era (certsPParams certsenv) certstate) $
                      \ [var|deposit|] [var|refund|] ->
                        toDelta_ (sumTxOut_ @fn @era outputs) ==. inputS + with + refund - deposit - f
            , dependsOn utxo tempUtxo
            , reifyX
                (pair_ tempUtxo feeInput)
                (\(m, i) -> Map.adjust (adjustTxOutCoin (DeltaCoin 0)) i m) -- mimics how we will adjust fee
                (\ [var|u|] -> utxo ==. u)
            ]

-- ==============================================================================
-- Some code to visualize what is happening, this code will disappear eventually

putPretty :: PrettyA t => [Char] -> t -> IO ()
putPretty nm x = putStrLn (nm ++ "\n" ++ show (prettyA x))

go2 ::
  forall era.
  ( EraSpecTxOut era ConwayFn
  , EraSpecCert era ConwayFn
  , EraSpecTxCert ConwayFn era
  , HasSpec ConwayFn (Tx era)
  , HasSpec ConwayFn (CertState era)
  , ConwayEraCertState era
  ) =>
  IO ()
go2 = do
  univ <- generate $ genWitUniv 25
  wdrls <- generate $ genFromSpec @ConwayFn (constrained $ \x -> witness univ x)
  delegatees <- generate $ genFromSpec @ConwayFn (delegateeSpec univ)
  certState <-
    generate $
      genFromSpec @ConwayFn @(CertState era)
        (certStateSpec @era @ConwayFn univ delegatees wdrls) -- (lit (AccountState (Coin 1000) (Coin 100))) (lit (EpochNo 100)))
        -- error "STOP"
  certsEnv <- generate $ genFromSpec @ConwayFn @(CertsEnv era) certsEnvSpec

  (body, utxomap, feeinput) <-
    generate $ genFromSpec (bodyspec @era @ConwayFn univ certsEnv certState)
  let utxo = UTxO utxomap
      txbody = fromShelleyBody body

  putStrLn
    ("Input UTxO, total " ++ show (coinBalance @era utxo) ++ ", size = " ++ show (Map.size utxomap))
  putPretty "UTxO" utxo
  putPretty "\nfeeInput" feeinput
  putStrLn (show (pcTxBodyWithUTxO utxo txbody))
  print (consumedTxBody txbody (certsPParams certsEnv) certState utxo)
  print (producedTxBody txbody (certsPParams certsEnv) certState)

testBody :: IO ()
testBody = do
  univ <- generate $ genWitUniv @AllegraEra 5
  wdrls <- generate $ genFromSpec @ConwayFn (constrained $ \x -> witness univ x)
  delegatees <- generate $ genFromSpec @ConwayFn (delegateeSpec univ)
  certsEnv <- generate $ genFromSpec @ConwayFn @(CertsEnv AllegraEra) certsEnvSpec
  certState <-
    generate $
      genFromSpec @ConwayFn @(CertState AllegraEra)
        (certStateSpec @AllegraEra @ConwayFn univ delegatees wdrls)

  cert <-
    generate $
      genFromSpec @ConwayFn @(TxCert AllegraEra) $
        (shelleyTxCertSpec @ConwayFn @AllegraEra univ (projectEnv certsEnv) certState)
          <> (witShelleyTxCert univ)
  -- The problem with this is that the CertState does not have any
  -- thing from the universe, so any Cert that requires a member_ of someting
  -- in the CertState, will never succeed,  because the Hashes are disjoint
  -- between the CertState and the Universe. So those certs with member_
  -- always fail, so the only ones that are ever generated are RegCert and RegPool
  print univ
  putStrLn (show (prettyA cert))
