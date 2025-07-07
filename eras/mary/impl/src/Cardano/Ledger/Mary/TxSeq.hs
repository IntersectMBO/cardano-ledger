{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Cardano.Ledger.Mary.TxSeq () where

import Cardano.Ledger.Core (EraBlockBody (..))
import Cardano.Ledger.Mary.Era (MaryEra)
import Cardano.Ledger.Mary.Tx ()
import Cardano.Ledger.Shelley.BlockBody (ShelleyBlockBody (..), bbHash, txSeqTxns)
import Lens.Micro (lens)

instance EraBlockBody MaryEra where
  type BlockBody MaryEra = ShelleyBlockBody MaryEra
  txSeqBlockBodyL = lens txSeqTxns (\_ s -> ShelleyBlockBody s)
  fromTxSeq = txSeqTxns
  toTxSeq = ShelleyBlockBody
  hashBlockBody = bbHash
  hashTxSeq = bbHash
  numSegComponents = 3
