{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Cardano.Ledger.Allegra where

import Cardano.Ledger.ShelleyMA
import Cardano.Ledger.ShelleyMA.Rules.EraMapping ()
import Cardano.Ledger.ShelleyMA.TxBody ()
import Shelley.Spec.Ledger.API
  ( ApplyBlock,
    ApplyTx,
    GetLedgerView,
    PraosCrypto,
    ShelleyBasedEra,
  )

type AllegraEra = ShelleyMAEra 'Allegra

instance PraosCrypto c => ApplyTx (AllegraEra c)

instance PraosCrypto c => ApplyBlock (AllegraEra c)

instance PraosCrypto c => GetLedgerView (AllegraEra c)

instance PraosCrypto c => ShelleyBasedEra (AllegraEra c)
