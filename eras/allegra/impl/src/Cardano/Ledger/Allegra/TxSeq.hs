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
import Cardano.Ledger.Shelley.BlockBody (ShelleyBlockBody (..), bbHash, txSeqTxns)
import Lens.Micro (lens)

instance EraBlockBody AllegraEra where
  type BlockBody AllegraEra = ShelleyBlockBody AllegraEra
  txSeqBlockBodyL = lens txSeqTxns (\_ s -> ShelleyBlockBody s)
  fromTxSeq = txSeqTxns
  toTxSeq = ShelleyBlockBody
  hashBlockBody = bbHash
  hashTxSeq = bbHash
  numSegComponents = 3
