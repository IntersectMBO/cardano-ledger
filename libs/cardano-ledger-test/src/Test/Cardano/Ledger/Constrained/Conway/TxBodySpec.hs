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

module Test.Cardano.Ledger.Constrained.Conway.TxBodySpec where

import Cardano.Ledger.Coin
import Cardano.Ledger.Conway.State
import Cardano.Ledger.Core
import Cardano.Ledger.Val
import Constrained.API
import Data.Foldable
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.TreeDiff
import Lens.Micro
import Test.Cardano.Ledger.Constrained.Conway.Instances
import Test.Cardano.Ledger.Constrained.Conway.ParametricSpec
import Prelude hiding (seq)

-- =================================
-- Move these to the MapSpec

subMap ::
  (Ord k, IsNormalType v, IsNormalType k, HasSpec k, HasSpec v) =>
  Specification (Map k v, Map k v)
subMap = constrained $ \ [var|pair|] ->
  match pair $ \ [var|sub|] [var|super|] ->
    [ dependsOn sub super
    , assert $ super /=. lit (Map.empty)
    , assert $ subset_ (dom_ sub) (dom_ super)
    , forAll sub $ \ [var|kvpair|] ->
        match kvpair $ \k v -> [dependsOn v k, onJust (lookup_ k super) (\a -> v ==. a)]
    ]

subMapSubDependsOnSuper ::
  (Ord k, IsNormalType v, IsNormalType k, HasSpec k, HasSpec v) =>
  Term (Map k v) ->
  Term (Map k v) ->
  Pred
subMapSubDependsOnSuper sub super =
  fold
    [ dependsOn sub super
    , assert $ super /=. lit (Map.empty)
    , assert $ subset_ (dom_ sub) (dom_ super)
    , forAll sub $ \ [var|kvpair|] ->
        match kvpair $ \k v -> [dependsOn v k, onJust (lookup_ k super) (\a -> v ==. a)]
    ]

subMapSuperDependsOnSub ::
  (Ord k, IsNormalType v, IsNormalType k, HasSpec k, HasSpec v) =>
  Term (Map k v) ->
  Term (Map k v) ->
  Pred
subMapSuperDependsOnSub sub super =
  fold
    [ dependsOn super sub
    , assert $ super /=. lit (Map.empty)
    , assert $ subset_ (dom_ sub) (dom_ super)
    , forAll sub $ \ [var|kvpair|] ->
        match kvpair $ \k v -> [onJust (lookup_ k super) (\a -> v ==. a)]
    ]

sumTxOut_ :: forall era. EraSpecTxOut era => Term [TxOut era] -> Term Coin
sumTxOut_ x = foldMap_ (txOutCoin_ @era) x

sumCoin_ :: Term [Coin] -> Term Coin
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

-- ==============================================================================
-- Some code to visualize what is happening, this code will disappear eventually

putPretty :: ToExpr t => [Char] -> t -> IO ()
putPretty nm x = putStrLn (nm ++ "\n" ++ show (prettyE x))
