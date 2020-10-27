{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Cardano.Ledger.Mary where

import Cardano.Ledger.ShelleyMA
import Cardano.Ledger.ShelleyMA.Rules.Utxo ()
import Cardano.Ledger.ShelleyMA.Rules.Utxow ()
import Cardano.Ledger.ShelleyMA.Scripts ()
import Cardano.Ledger.ShelleyMA.TxBody ()
import Shelley.Spec.Ledger.API (ApplyBlock, ApplyTx, GetLedgerView, PraosCrypto)

type MaryEra = ShelleyMAEra 'Mary

instance PraosCrypto c => ApplyTx (MaryEra c)

instance PraosCrypto c => ApplyBlock (MaryEra c)

instance PraosCrypto c => GetLedgerView (MaryEra c)
