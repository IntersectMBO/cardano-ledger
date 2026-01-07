{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Cardano.Ledger.Mary (
  MaryEra,
  ShelleyTx,
  ShelleyTxOut,
  MaryValue,
  TxBody (..),
  Tx (..),
  ApplyTxError (..),
) where

import Cardano.Ledger.Binary (DecCBOR, EncCBOR)
import Cardano.Ledger.Mary.BlockBody ()
import Cardano.Ledger.Mary.Era (MaryEra)
import Cardano.Ledger.Mary.PParams ()
import Cardano.Ledger.Mary.Rules ()
import Cardano.Ledger.Mary.Scripts ()
import Cardano.Ledger.Mary.State ()
import Cardano.Ledger.Mary.Transition ()
import Cardano.Ledger.Mary.Translation ()
import Cardano.Ledger.Mary.Tx (Tx (..))
import Cardano.Ledger.Mary.TxAuxData ()
import Cardano.Ledger.Mary.TxBody (TxBody (..))
import Cardano.Ledger.Mary.UTxO ()
import Cardano.Ledger.Mary.Value (MaryValue)
import Cardano.Ledger.Shelley.API
import Cardano.Ledger.Shelley.Rules (ShelleyLedgerPredFailure)
import Data.Bifunctor (Bifunctor (first))
import Data.List.NonEmpty (NonEmpty)

instance ApplyTx MaryEra where
  newtype ApplyTxError MaryEra = MaryApplyTxError (NonEmpty (ShelleyLedgerPredFailure MaryEra))
    deriving (Eq, Show)
    deriving newtype (EncCBOR, DecCBOR)
  applyTxValidation validationPolicy globals env state tx =
    first MaryApplyTxError $
      ruleApplyTxValidation @"LEDGER" validationPolicy globals env state tx

instance ApplyBlock MaryEra
