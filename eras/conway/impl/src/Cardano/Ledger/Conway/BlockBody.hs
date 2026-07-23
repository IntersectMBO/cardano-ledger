{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Cardano.Ledger.Conway.BlockBody where

import Cardano.Ledger.Alonzo.BlockBody
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
import Cardano.Ledger.Conway.Era
import Cardano.Ledger.Conway.Tx ()
import Cardano.Ledger.Core
import qualified Data.ByteString as BS
import Data.Typeable (Typeable)

instance EraBlockBody ConwayEra where
  type BlockBody ConwayEra = AlonzoBlockBody ConwayEra
  mkBasicBlockBody = mkBasicBlockBodyAlonzo
  txSeqBlockBodyL = txSeqBlockBodyAlonzoL
  hashBlockBody = alonzoBlockBodyHash
  blockBodySize (ProtVer v _) = BS.length . serialize' v . encCBORGroup

instance EncCBOR h => EncCBOR (Block h ConwayEra) where
  encCBOR (Block h txns) =
    encodeListLen 5 <> encCBOR h <> encCBORGroup txns

instance (EncCBOR h, Typeable h) => Plain.ToCBOR (Block h ConwayEra) where
  toCBOR = toPlainEncoding (eraProtVerLow @ConwayEra) . encCBOR

instance
  (DecCBOR (Annotator h), Typeable h) =>
  DecCBOR (Annotator (Block h ConwayEra))
  where
  decCBOR =
    decodeRecordNamed "Block" (const 5) $ do
      header <- decCBOR
      txns <- decCBOR
      pure $ Block <$> header <*> txns
