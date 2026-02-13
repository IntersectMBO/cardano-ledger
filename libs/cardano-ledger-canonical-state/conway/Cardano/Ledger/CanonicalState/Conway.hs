{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Cardano.Ledger.CanonicalState.Conway (
  mkCanonicalConstitution,
) where

import Cardano.Ledger.CanonicalState.Namespace
import Cardano.Ledger.CanonicalState.Namespace.GovConstitution.V0
import Cardano.Ledger.CanonicalState.Namespace.UTxO.V0
import Cardano.Ledger.Conway (ConwayEra)
import Cardano.Ledger.Conway.Governance (Constitution (..))
import Cardano.SCLS.NamespaceCodec

type instance NamespaceEra "blocks/v0" = ConwayEra

type instance NamespaceEra "utxo/v0" = ConwayEra

type instance NamespaceEra "gov/constitution/v0" = ConwayEra

instance KnownNamespace "utxo/v0" where
  type NamespaceKey "utxo/v0" = UtxoIn
  type NamespaceEntry "utxo/v0" = UtxoOut ConwayEra

mkCanonicalConstitution :: Constitution era -> CanonicalConstitution
mkCanonicalConstitution Constitution {..} = CanonicalConstitution {..}
