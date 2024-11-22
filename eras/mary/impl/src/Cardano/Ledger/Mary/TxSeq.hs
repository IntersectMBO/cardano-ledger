{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Cardano.Ledger.Mary.TxSeq () where

import Cardano.Ledger.Core (EraSegWits (..))
import Cardano.Ledger.Mary.Era (MaryEra)
import Cardano.Ledger.Mary.Tx ()
import Cardano.Ledger.Shelley.BlockChain (ShelleyTxSeq (..), bbHash, txSeqTxns)

instance EraSegWits MaryEra where
  {-# SPECIALIZE instance EraSegWits MaryEra #-}
  type TxSeq MaryEra = ShelleyTxSeq MaryEra
  fromTxSeq = txSeqTxns
  toTxSeq = ShelleyTxSeq
  hashTxSeq = bbHash
  numSegComponents = 3
