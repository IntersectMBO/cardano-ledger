{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Cardano.Ledger.Babbage.BlockBody where

import Cardano.Ledger.Alonzo.BlockBody
import Cardano.Ledger.Babbage.Era
import Cardano.Ledger.Babbage.Tx ()
import Cardano.Ledger.BaseTypes (ProtVer (..))
import Cardano.Ledger.Binary (
  Annotator,
  DecCBOR (decCBOR),
  EncCBOR (..),
  EncCBORGroup (..),
  decodeRecordNamed,
  encodeListLen,
  serialize',
  toPlainEncoding,
 )
import qualified Cardano.Ledger.Binary.Plain as Plain
import Cardano.Ledger.Block (Block (..))
import Cardano.Ledger.Core
import qualified Data.ByteString as BS
import Data.Typeable (Typeable)

instance EraBlockBody BabbageEra where
  type BlockBody BabbageEra = AlonzoBlockBody BabbageEra
  mkBasicBlockBody = mkBasicBlockBodyAlonzo
  txSeqBlockBodyL = txSeqBlockBodyAlonzoL
  hashBlockBody = alonzoBlockBodyHash
  blockBodySize (ProtVer v _) = BS.length . serialize' v . encCBORGroup

instance EncCBOR h => EncCBOR (Block h BabbageEra) where
  encCBOR (Block h txns) =
    encodeListLen 5 <> encCBOR h <> encCBORGroup txns

instance (EncCBOR h, Typeable h) => Plain.ToCBOR (Block h BabbageEra) where
  toCBOR = toPlainEncoding (eraProtVerLow @BabbageEra) . encCBOR

instance
  (DecCBOR (Annotator h), Typeable h) =>
  DecCBOR (Annotator (Block h BabbageEra))
  where
  decCBOR =
    decodeRecordNamed "Block" (const 5) $ do
      header <- decCBOR
      txns <- decCBOR
      pure $ Block <$> header <*> txns
