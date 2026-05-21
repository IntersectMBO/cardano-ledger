{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
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
import Cardano.Ledger.Block (EraBlockHeader)
import Cardano.Ledger.Mary.BlockBody ()
import Cardano.Ledger.Mary.Era (MaryEra)
import Cardano.Ledger.Mary.Forecast ()
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
import Cardano.Ledger.Rules.ValidationMode (lblStatic)
import Cardano.Ledger.Shelley.API
import Cardano.Ledger.Shelley.Rules (ledgerPpL)
import qualified Cardano.Ledger.Shelley.Rules as Shelley
import Cardano.Ledger.State (utxoG)
import Control.State.Transition.Extended (ValidationPolicy (..))
import Data.Bifunctor (Bifunctor (first))
import Data.List.NonEmpty (NonEmpty)
import GHC.Generics (Generic)
import Lens.Micro ((^.))

instance ApplyTx MaryEra where
  newtype ApplyTxError MaryEra = MaryApplyTxError (NonEmpty (Shelley.ShelleyLedgerPredFailure MaryEra))
    deriving (Eq, Show)
    deriving newtype (EncCBOR, DecCBOR, Semigroup, Generic)

  mkStAnnTx _ _ _ _ = id

  internalApplyTxWithValidation validationPolicy globals env state tx =
    let stAnnTx =
          mkStAnnTx
            (epochInfo globals)
            (systemStart globals)
            (env ^. ledgerPpL)
            (state ^. utxoG)
            tx
     in first MaryApplyTxError $
          ruleApplyTxValidation @"LEDGER" validationPolicy globals env state stAnnTx

  internalReapplyValidatedTx globals env state vtx =
    fst
      <$> first
        MaryApplyTxError
        ( ruleApplyTxValidation @"LEDGER"
            (ValidateSuchThat (notElem lblStatic))
            globals
            env
            state
            (vtStAnnTx vtx)
        )

instance ApplyTick MaryEra

instance EraBlockHeader h MaryEra => ApplyBlock h MaryEra
