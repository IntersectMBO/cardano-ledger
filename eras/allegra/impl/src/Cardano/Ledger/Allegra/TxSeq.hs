{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Cardano.Ledger.Allegra.TxSeq () where

import Cardano.Ledger.Allegra.Era (AllegraEra)
import Cardano.Ledger.Allegra.Tx ()
import Cardano.Ledger.Core (EraSegWits (..))
import Cardano.Ledger.Crypto (Crypto, StandardCrypto)
import Cardano.Ledger.Shelley.BlockChain (ShelleyTxSeq (..), bbHash, txSeqTxns)
import Control.Monad ((<=<))
import qualified Data.Sequence.Strict as StrictSeq

instance Crypto c => EraSegWits (AllegraEra c) where
  {-# SPECIALIZE instance EraSegWits (AllegraEra StandardCrypto) #-}
  type TxZones (AllegraEra c) = ShelleyTxSeq (AllegraEra c)
  fromTxZones = fmap StrictSeq.singleton . txSeqTxns
  toTxZones = ShelleyTxSeq . StrictSeq.forceToStrict . (StrictSeq.fromStrict <=< StrictSeq.fromStrict)
  hashTxZones = bbHash
  numSegComponents = 3
