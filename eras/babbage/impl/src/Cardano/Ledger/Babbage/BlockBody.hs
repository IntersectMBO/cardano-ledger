{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Cardano.Ledger.Babbage.BlockBody where

import Cardano.Ledger.Alonzo.BlockBody
import Cardano.Ledger.Babbage.Era
import Cardano.Ledger.Babbage.Tx ()
import Cardano.Ledger.BaseTypes (ProtVer (..))
import Cardano.Ledger.Binary (EncCBORGroup (..), serialize')
import Cardano.Ledger.Core
import qualified Data.ByteString as BS

instance EraBlockBody BabbageEra where
  type BlockBody BabbageEra = AlonzoBlockBody BabbageEra
  mkBasicBlockBody = mkBasicBlockBodyAlonzo
  txSeqBlockBodyL = txSeqBlockBodyAlonzoL
  hashBlockBody = alonzoBlockBodyHash
  numSegComponents = 4
  blockBodySize (ProtVer v _) = BS.length . serialize' v . encCBORGroup
