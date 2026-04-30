{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableSuperClasses #-}

module Cardano.Ledger.StAnnTx (
  EraStAnnTx (..),
) where

import Cardano.Ledger.Core (EraTx, PParams, StAnnTx, TopTx, Tx)
import Cardano.Ledger.State.UTxO (UTxO)
import Cardano.Slotting.EpochInfo (EpochInfo)
import Cardano.Slotting.Time (SystemStart)
import Data.Text (Text)

class EraTx era => EraStAnnTx era where
  mkStAnnTx ::
    EpochInfo (Either Text) ->
    SystemStart ->
    PParams era ->
    UTxO era ->
    Tx TopTx era ->
    StAnnTx TopTx era
