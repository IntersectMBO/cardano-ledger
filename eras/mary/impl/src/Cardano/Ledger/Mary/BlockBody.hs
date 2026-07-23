{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Cardano.Ledger.Mary.BlockBody () where

import Cardano.Ledger.BaseTypes (ProtVer (..))
import Cardano.Ledger.Binary (EncCBORGroup (..), serialize')
import Cardano.Ledger.Core (EraBlockBody (..))
import Cardano.Ledger.Mary.Era (MaryEra)
import Cardano.Ledger.Mary.Tx ()
import Cardano.Ledger.Shelley.BlockBody (
  ShelleyBlockBody,
  mkBasicBlockBodyShelley,
  shelleyBlockBodyHash,
  txSeqBlockBodyShelleyL,
 )
import qualified Data.ByteString as BS

instance EraBlockBody MaryEra where
  type BlockBody MaryEra = ShelleyBlockBody MaryEra
  mkBasicBlockBody = mkBasicBlockBodyShelley
  txSeqBlockBodyL = txSeqBlockBodyShelleyL
  hashBlockBody = shelleyBlockBodyHash
  blockBodySize (ProtVer v _) = BS.length . serialize' v . encCBORGroup
