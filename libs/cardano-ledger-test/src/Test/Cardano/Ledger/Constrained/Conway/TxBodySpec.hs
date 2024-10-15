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

-- import Cardano.Ledger.BaseTypes
import Cardano.Ledger.Coin
import Cardano.Ledger.Conway (Conway)
import Cardano.Ledger.Conway.Rules (CertsEnv (..))
import Cardano.Ledger.Core

-- import Cardano.Ledger.Shelley.LedgerState (AccountState (..))
import Cardano.Ledger.Shelley.TxBody (ShelleyTxBody (..))
import Cardano.Ledger.TxIn (TxIn (..))
import Cardano.Ledger.UTxO (UTxO (..), coinBalance)
import Cardano.Ledger.Val
import Constrained hiding (Value)
import Constrained.Base (Pred (..), hasSize, rangeSize)
import Data.Foldable (toList)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Set (Set)
import Data.Word (Word64)

import Data.Sequence.Internal (Seq)
import Lens.Micro

-- , certStateSpec)

import Test.Cardano.Ledger.Constrained.Conway.Cert (EraSpecCert (..), certStateSpec)
import Test.Cardano.Ledger.Constrained.Conway.Certs (certsEnvSpec, projectEnv)
import Test.Cardano.Ledger.Constrained.Conway.Instances
import Test.Cardano.Ledger.Constrained.Conway.Instances.TxBody (fromShelleyBody)
import Test.Cardano.Ledger.Constrained.Conway.LedgerTypes.Specs (EraSpecLedger (..))

import Test.Cardano.Ledger.Constrained.Conway.ParametricSpec
import Test.Cardano.Ledger.Generic.Proof (Reflect)
import qualified Test.Cardano.Ledger.Generic.Proof as Proof
import Test.QuickCheck hiding (forAll)
import Prelude hiding (seq)

import Cardano.Ledger.Allegra (Allegra)
import Cardano.Ledger.Alonzo (Alonzo)
import Cardano.Ledger.Babbage (Babbage)
import Cardano.Ledger.Mary (Mary)
import Cardano.Ledger.Shelley (Shelley)
import Cardano.Ledger.Shelley.LedgerState (CertState)
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

putPretty :: PrettyA t => [Char] -> t -> IO ()
putPretty nm x = putStrLn (nm ++ "\n" ++ show (prettyA x))

test1 :: IO ()
test1 = do
  super <- generate $ genFromSpec @ConwayFn @(Map Int Int) (hasSize (rangeSize 5 7))
  sub <-
    generate $
      genFromSpec @ConwayFn @(Map Int Int)
        ( constrained $ \sub ->
            [ assert $ sizeOf_ (dom_ sub) <. sizeOf_ (dom_ (lit super))
            , subMapSubDependsOnSuper sub (lit super)
            ]
        )
  putPretty "\nsuper" super
  putPretty "sub" sub

test2 :: IO ()
test2 = do
  sub <- generate $ genFromSpec @ConwayFn @(Map Int Int) (hasSize (rangeSize 4 4))
  super <-
    generate $
      genFromSpec @ConwayFn @(Map Int Int)
        ( constrained $ \super ->
            [ subMapSuperDependsOnSub (lit sub) super
            , assert $ sizeOf_ (dom_ super) ==. 6
            ]
        )
  putPretty "\nsuper" super
  putPretty "sub" sub

foo :: IO (Map Int Int, Map Int Int)
foo = generate $ genFromSpec $ subMap @Int @Int @ConwayFn

-- ===================================================================

bodyspec ::
  forall era fn.
  ( EraSpecTxOut era fn
  , EraSpecCert era fn
  ) =>
  UTxO era ->
  CertsEnv era ->
  CertState era ->
  Specification
    fn
    ( ShelleyTxBody era
    , Map (TxIn (EraCrypto era)) (TxOut era)
    , TxIn (EraCrypto era)
    )
bodyspec utxo certsenv certstate =
  constrained' $ \ [var|shelleyBody|] [var|inputUtxo|] [var|feeInput|] ->
    match shelleyBody $
      \ [var|inputs|] [var|outputs|] [var|certs|] [var|rewAcct|] [var|fee|] _ [var|update|] _ ->
        [ dependsOn shelleyBody fee
        , dependsOn shelleyBody inputs
        , dependsOn shelleyBody outputs
        , dependsOn feeInput inputUtxo
        , dependsOn inputs inputUtxo
        , dependsOn outputs inputUtxo
        , dependsOn fee feeInput
        , dependsOn outputs inputUtxo
        , satisfies inputUtxo (hasSize (rangeSize 2 4))
        , subMapSubDependsOnSuper inputUtxo (lit (unUTxO utxo))
        , assert $ update ==. lit Nothing
        , assert $ member_ feeInput (dom_ inputUtxo)
        , assert $ onJust (lookup_ feeInput (lit (unUTxO utxo))) (\ [var|txout|] -> fee ==. txOutCoin_ txout)
        , assert $ inputs ==. dom_ inputUtxo
        , reify inputUtxo (map snd . Map.toList) $
            \ [var|txouts|] ->
              [ dependsOn outputs txouts
              , assert $ (sumCoin_ @fn @era outputs) ==. sumCoin_ @fn @era txouts - fee
              ]
        , forAll certs $ \ [var|oneCert|] -> satisfies oneCert (txCertSpec (projectEnv certsenv) certstate)
        , assert $ sizeOf_ certs <=. 4
        , assert $ rewAcct ==. lit Map.empty
        ]

seqToList :: IsConwayUniv fn => HasSpec fn t => Specification fn (Data.Sequence.Internal.Seq t, [t])
seqToList = constrained' $ \ [var|seq|] [var|list|] -> reify seq toList (\x -> list ==. x)

testUTxO ::
  forall era. (Era era, HasSpec ConwayFn (TxOut era)) => IO (Map (TxIn (EraCrypto era)) (TxOut era))
testUTxO =
  generate $
    genFromSpec @ConwayFn @(Map (TxIn (EraCrypto era)) (TxOut era)) (hasSize (rangeSize 7 15))

-- | Exercise the 'bodyspec'
go ::
  forall era.
  ( EraSpecTxOut era ConwayFn
  , EraSpecCert era ConwayFn
  , HasSpec ConwayFn (Tx era)
  ) =>
  IO ()
go = do
  utxo <- UTxO <$> testUTxO @era
  certState <-
    generate $
      genFromSpec @ConwayFn @(CertState era)
        (certStateSpec @ConwayFn @era) -- (lit (AccountState (Coin 1000) (Coin 100))) (lit (EpochNo 100)))
        -- error "STOP"
  certsEnv <- generate $ genFromSpec @ConwayFn @(CertsEnv era) certsEnvSpec

  (basic, subutxo, feeinput) <-
    generate $ genFromSpec (bodyspec @era @ConwayFn utxo certsEnv certState)

  putStrLn
    ("Input UTxO, total " ++ show (coinBalance @era utxo) ++ ", size = " ++ show (Map.size (unUTxO utxo)))
  putPretty "\nSubMap of Utxo" (UTxO subutxo)
  putStrLn ("SubMap of UTxO, total Coin " ++ show (coinBalance @era (UTxO subutxo)))
  putPretty "\nfeeInput" feeinput
  putPretty "\nTxBody" basic

sumCoin_ :: forall fn era. EraSpecTxOut era fn => Term fn [TxOut era] -> Term fn Coin
sumCoin_ x = foldMap_ (txOutCoin_ @era @fn) x

-- ===============================================================

bodyspec2 ::
  forall era fn.
  ( EraSpecTxOut era fn
  , EraSpecCert era fn
  ) =>
  CertsEnv era ->
  CertState era ->
  Specification
    fn
    ( ShelleyTxBody era
    , Map (TxIn (EraCrypto era)) (TxOut era)
    , TxIn (EraCrypto era)
    )
bodyspec2 certsenv certstate =
  constrained' $ \ [var|shelleyBody|] [var|utxo|] [var|feeInput|] ->
    match shelleyBody $
      \ [var|inputs|] [var|outputs|] [var|certs|] [var|rewAcct|] [var|fee|] _ [var|update|] _ ->
        [ dependsOn shelleyBody fee
        , dependsOn shelleyBody (inputs :: Term fn (Set (TxIn (EraCrypto era))))
        , dependsOn shelleyBody outputs
        , dependsOn shelleyBody certs
        , dependsOn utxo inputs
        , -- , exists (\eval -> undefined) $ \ [var|feeInput|] ->
          exists (\eval -> pure (Map.restrictKeys (eval utxo) (eval inputs))) $ \ [var|utxosubset|] ->
            exists (\_eval -> undefined) $ \ [var|tempUtxo|] ->
              [ dependsOn inputs utxosubset
              , dependsOn utxo utxosubset
              , dependsOn feeInput inputs
              , dependsOn fee utxosubset
              , dependsOn fee feeInput
              , dependsOn outputs utxosubset
              , dependsOn outputs fee
              , dependsOn utxo tempUtxo
              , satisfies utxosubset (hasSize (rangeSize 3 4))
              , assert $ member_ feeInput inputs
              , assert $ inputs ==. dom_ utxosubset
              , assert $ onJust (lookup_ feeInput utxosubset) (\ [var|txout|] -> fee ==. txOutCoin_ @era @fn txout)
              , satisfies (dom_ tempUtxo) (hasSize (rangeSize 8 10))
              , subMapSuperDependsOnSub utxosubset tempUtxo
              , assert $ (sumCoin_ @fn @era outputs) ==. sumCoin_ @fn @era (rng_ utxosubset) - fee
              , forAll outputs $ \x -> txOutCoin_ @era @fn x >=. lit (Coin 0)
              , reify
                  (pair_ tempUtxo feeInput)
                  (\(m, i) -> Map.adjust baz i m)
                  (\u -> utxo ==. u)
              ]
        , assert $ update ==. lit Nothing
        , forAll certs $ \ [var|oneCert|] -> satisfies oneCert (txCertSpec (projectEnv certsenv) certstate)
        , assert $ sizeOf_ certs <=. 4
        , assert $ rewAcct ==. lit Map.empty
        , assert $ sizeOf_ outputs ==. 4
        ]

baz :: EraTxOut era => TxOut era -> TxOut era
baz x = x & coinTxOutL .~ ((x ^. coinTxOutL) <+> (Coin 100))

go2 ::
  forall era.
  ( EraSpecTxOut era ConwayFn
  , EraSpecCert era ConwayFn
  , HasSpec ConwayFn (Tx era)
  ) =>
  IO ()
go2 = do
  certState <-
    generate $
      genFromSpec @ConwayFn @(CertState era)
        (certStateSpec @ConwayFn @era) -- (lit (AccountState (Coin 1000) (Coin 100))) (lit (EpochNo 100)))
        -- error "STOP"
  certsEnv <- generate $ genFromSpec @ConwayFn @(CertsEnv era) certsEnvSpec

  (body, utxomap, feeinput) <-
    generate $ genFromSpec (bodyspec2 @era @ConwayFn certsEnv certState)
  let utxo = UTxO utxomap

  putStrLn
    ("Input UTxO, total " ++ show (coinBalance @era utxo) ++ ", size = " ++ show (Map.size utxomap))
  putPretty "UTxO" utxo
  putPretty "\nfeeInput" feeinput
  putStrLn (show (pcTxBodyWithUTxO utxo (fromShelleyBody body)))
