{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Cardano.Ledger.Conway.BlockBody where

import Cardano.Ledger.Alonzo.BlockBody
import Cardano.Ledger.BaseTypes (ProtVer (..))
import Cardano.Ledger.Binary (EncCBORGroup (..), serialize')
import Cardano.Ledger.Conway.Era
import Cardano.Ledger.Conway.Tx ()
import Cardano.Ledger.Core
import qualified Data.ByteString as BS

instance EraBlockBody ConwayEra where
  type BlockBody ConwayEra = AlonzoBlockBody ConwayEra
  mkBasicBlockBody = mkBasicBlockBodyAlonzo
  txSeqBlockBodyL = txSeqBlockBodyAlonzoL
  hashBlockBody = alonzoBlockBodyHash
  numSegComponents = 4
  blockBodySize (ProtVer v _) = BS.length . serialize' v . encCBORGroup
