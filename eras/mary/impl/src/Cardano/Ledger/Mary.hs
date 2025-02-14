{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Cardano.Ledger.Mary (
  Mary,
  MaryEra,
  ShelleyTx,
  ShelleyTxOut,
  MaryValue,
  MaryTxBody,
)
where

import Cardano.Ledger.Mary.Era (MaryEra)
import Cardano.Ledger.Mary.PParams ()
import Cardano.Ledger.Mary.Rules ()
import Cardano.Ledger.Mary.Scripts ()
import Cardano.Ledger.Mary.Transition ()
import Cardano.Ledger.Mary.Translation ()
import Cardano.Ledger.Mary.TxAuxData ()
import Cardano.Ledger.Mary.TxBody (MaryTxBody)
import Cardano.Ledger.Mary.TxSeq ()
import Cardano.Ledger.Mary.UTxO ()
import Cardano.Ledger.Mary.Value (MaryValue)
import Cardano.Ledger.Shelley.API

type Mary = MaryEra

{-# DEPRECATED Mary "In favor of `MaryEra`" #-}

instance ApplyTx MaryEra where
  applyTxValidation = ruleApplyTxValidation @"LEDGER"

instance ApplyBlock MaryEra
