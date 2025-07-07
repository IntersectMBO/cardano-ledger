{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Cardano.Ledger.Allegra.TxSeq () where

import Cardano.Ledger.Allegra.Era (AllegraEra)
import Cardano.Ledger.Allegra.Tx ()
import Cardano.Ledger.Core (EraBlockBody (..))
import Cardano.Ledger.Shelley.BlockBody (ShelleyBlockBody (..), bbHash)
import Lens.Micro

instance EraBlockBody AllegraEra where
  type BlockBody AllegraEra = ShelleyBlockBody AllegraEra
  txSeqBlockBodyL = txSeqBlockBodyL
  fromTxSeq = (^. txSeqBlockBodyL)
  toTxSeq = ShelleyBlockBody
  hashBlockBody = bbHash
  hashTxSeq = bbHash
  numSegComponents = 3
