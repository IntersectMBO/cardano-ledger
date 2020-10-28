{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Cardano.Ledger.Mary where

import Cardano.Crypto.Hash (Hash)
import Cardano.Ledger.Crypto (Crypto, HASH)
import Cardano.Ledger.ShelleyMA
import Cardano.Ledger.ShelleyMA.Rules.Utxo ()
import Cardano.Ledger.ShelleyMA.Rules.Utxow ()
import Cardano.Ledger.ShelleyMA.Scripts ()
import Cardano.Ledger.ShelleyMA.TxBody ()
import Shelley.Spec.Ledger.API (ApplyBlock, ApplyTx, GetLedgerView)
import Shelley.Spec.Ledger.Hashing (EraIndependentTxBody)
import Shelley.Spec.Ledger.Keys (DSignable)

type MaryEra = ShelleyMAEra 'Mary

instance
  ( Crypto c,
    DSignable c (Hash (HASH c) EraIndependentTxBody)
  ) =>
  ApplyTx (MaryEra c)

instance
  ( Crypto c,
    DSignable c (Hash (HASH c) EraIndependentTxBody)
  ) =>
  ApplyBlock (MaryEra c)

instance (Crypto c) => GetLedgerView (MaryEra c)
