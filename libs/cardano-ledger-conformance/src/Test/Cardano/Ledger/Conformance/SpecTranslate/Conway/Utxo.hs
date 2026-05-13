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
import qualified Cardano.Ledger.Shelley.Rules as Shelley
import qualified MAlonzo.Code.Ledger.Conway.Foreign.API as Agda
import Test.Cardano.Ledger.Conformance.SpecTranslate.Conway.Base (SpecTranslate (..))
import Test.Cardano.Ledger.Conformance.SpecTranslate.Conway.Cert ()

instance SpecTranslate ConwayEra (Shelley.UtxoEnv ConwayEra) where
  type SpecRep ConwayEra (Shelley.UtxoEnv ConwayEra) = Agda.UTxOEnv

  toSpecRep x =
    Agda.MkUTxOEnv
      <$> toSpecRep (Shelley.ueSlot x)
      <*> toSpecRep (Shelley.uePParams x)
      <*> toSpecRep (Coin 10_000_000) -- TODO: Fix generating types
