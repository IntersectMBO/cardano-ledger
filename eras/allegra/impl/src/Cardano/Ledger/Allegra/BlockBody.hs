{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Cardano.Ledger.Allegra.BlockBody () where

import Cardano.Ledger.Allegra.Era (AllegraEra)
import Cardano.Ledger.Allegra.Tx ()
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
import Cardano.Ledger.Core (EraBlockBody (..), eraProtVerLow)
import Cardano.Ledger.Shelley.BlockBody (
  ShelleyBlockBody,
  mkBasicBlockBodyShelley,
  shelleyBlockBodyHash,
  txSeqBlockBodyShelleyL,
 )
import qualified Data.ByteString as BS
import Data.Typeable (Typeable)

instance EraBlockBody AllegraEra where
  type BlockBody AllegraEra = ShelleyBlockBody AllegraEra
  mkBasicBlockBody = mkBasicBlockBodyShelley
  txSeqBlockBodyL = txSeqBlockBodyShelleyL
  hashBlockBody = shelleyBlockBodyHash
  blockBodySize (ProtVer v _) = BS.length . serialize' v . encCBORGroup

instance EncCBOR h => EncCBOR (Block h AllegraEra) where
  encCBOR (Block h txns) =
    encodeListLen 4 <> encCBOR h <> encCBORGroup txns

instance (EncCBOR h, Typeable h) => Plain.ToCBOR (Block h AllegraEra) where
  toCBOR = toPlainEncoding (eraProtVerLow @AllegraEra) . encCBOR

instance
  (DecCBOR (Annotator h), Typeable h) =>
  DecCBOR (Annotator (Block h AllegraEra))
  where
  decCBOR =
    decodeRecordNamed "Block" (const 4) $ do
      header <- decCBOR
      txns <- decCBOR
      pure $ Block <$> header <*> txns
