{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module Test.Cardano.Ledger.Era.Rules (
  EraRuleProof (..),
  UnliftRules (..),
) where

import Cardano.Ledger.Binary
import Cardano.Ledger.Core
import Control.State.Transition.Extended (STS (..))
import Data.Typeable
import GHC.TypeLits (KnownSymbol, Symbol)
import Test.Cardano.Ledger.Common

data EraRuleProof era (rs :: [Symbol]) where
  EraRuleProofEmpty :: EraRuleProof era '[]
  EraRuleProofHead ::
    ( Show (EraRuleFailure r era)
    , Eq (EraRuleFailure r era)
    , EncCBOR (EraRuleFailure r era)
    , DecCBOR (EraRuleFailure r era)
    , Arbitrary (EraRuleFailure r era)
    , EraRuleFailure r era ~ PredicateFailure (EraRule r era)
    , KnownSymbol r
    ) =>
    Proxy r ->
    EraRuleProof era xs ->
    EraRuleProof era (r ': xs)

class UnliftRules era (rs :: [Symbol]) where
  unliftEraRuleProofs :: EraRuleProof era rs

instance UnliftRules era '[] where
  unliftEraRuleProofs = EraRuleProofEmpty

instance
  ( Show (EraRuleFailure r era)
  , Eq (EraRuleFailure r era)
  , EncCBOR (EraRuleFailure r era)
  , DecCBOR (EraRuleFailure r era)
  , Arbitrary (EraRuleFailure r era)
  , EraRuleFailure r era ~ PredicateFailure (EraRule r era)
  , UnliftRules era rs
  , KnownSymbol r
  ) =>
  UnliftRules era (r ': rs)
  where
  unliftEraRuleProofs = EraRuleProofHead Proxy unliftEraRuleProofs
