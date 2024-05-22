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
import Cardano.Ledger.Crypto (Crypto, StandardCrypto)
import Cardano.Ledger.Mary.Era (MaryEra)
import Cardano.Ledger.Mary.Tx ()
import Cardano.Ledger.Shelley.BlockChain (ShelleyTxSeq (..), bbHash, txSeqTxns)
import Control.Monad ((<=<))
import qualified Data.Sequence.Strict as StrictSeq

instance Crypto c => EraSegWits (MaryEra c) where
  {-# SPECIALIZE instance EraSegWits (MaryEra StandardCrypto) #-}
  type TxZones (MaryEra c) = ShelleyTxSeq (MaryEra c)
  fromTxZones = fmap StrictSeq.singleton . txSeqTxns
  toTxZones = ShelleyTxSeq . StrictSeq.forceToStrict . (StrictSeq.fromStrict <=< StrictSeq.fromStrict)
  hashTxZones = bbHash
  numSegComponents = 3
