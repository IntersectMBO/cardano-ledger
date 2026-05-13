{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Test.Cardano.Ledger.Conformance.SpecTranslate.Conway.Utxo () where

import Cardano.Ledger.Coin (Coin (..))
import Cardano.Ledger.Conway.Core (EraPParams (..), PParams)
import qualified Cardano.Ledger.Shelley.Rules as Shelley
import Data.Functor.Identity (Identity)
import qualified MAlonzo.Code.Ledger.Foreign.API as Agda
import Test.Cardano.Ledger.Conformance.SpecTranslate.Conway.Base (SpecTranslate (..))
import Test.Cardano.Ledger.Conformance.SpecTranslate.Conway.Cert ()

instance
  ( SpecRep (PParams era) ~ Agda.PParams
  , SpecTranslate (PParamsHKD Identity era)
  , SpecContext (PParamsHKD Identity era) ~ ()
  ) =>
  SpecTranslate (Shelley.UtxoEnv era)
  where
  type SpecRep (Shelley.UtxoEnv era) = Agda.UTxOEnv

  toSpecRep x =
    Agda.MkUTxOEnv
      <$> toSpecRep (Shelley.ueSlot x)
      <*> toSpecRep (Shelley.uePParams x)
      <*> toSpecRep (Coin 10_000_000) -- TODO: Fix generating types
