{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Test.Cardano.Ledger.Conformance.SpecTranslate.Conway.Utxo () where

import Cardano.Ledger.Coin (Coin (..))
import Cardano.Ledger.Conway (ConwayEra)
import Cardano.Ledger.Conway.Core (EraPParams (..), PParams)
import Cardano.Ledger.Shelley.Rules (UtxoEnv (..))
import Data.Functor.Identity (Identity)
import qualified MAlonzo.Code.Ledger.Foreign.API as Agda
import Test.Cardano.Ledger.Conformance.SpecTranslate.Conway.Base (SpecTranslate (..))
import Test.Cardano.Ledger.Conformance.SpecTranslate.Conway.Cert ()

instance
  ( SpecRep ConwayEra (PParams era) ~ Agda.PParams
  , SpecTranslate ctx ConwayEra (PParamsHKD Identity era)
  ) =>
  SpecTranslate ctx ConwayEra (UtxoEnv era)
  where
  type SpecRep ConwayEra (UtxoEnv era) = Agda.UTxOEnv

  toSpecRep x =
    Agda.MkUTxOEnv
      <$> toSpecRep @_ @ConwayEra (ueSlot x)
      <*> toSpecRep @_ @ConwayEra (uePParams x)
      <*> toSpecRep @_ @ConwayEra (Coin 10_000_000) -- TODO: Fix generating types
