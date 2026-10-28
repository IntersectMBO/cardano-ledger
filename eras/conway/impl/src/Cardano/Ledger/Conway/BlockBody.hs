{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
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
import Cardano.Ledger.Block (Block (..), PraosEraBlockHeader)
import Cardano.Ledger.Conway.Era
import Cardano.Ledger.Conway.Tx ()
import Cardano.Ledger.Core
import Cardano.Ledger.Plutus.ExUnits (ExUnits)
import Control.DeepSeq (NFData)
import qualified Data.ByteString as BS
import Data.Typeable (Typeable)
import Data.Word (Word32)
import GHC.Generics (Generic)

data ConwayBlockBodyLimits era = ConwayBlockBodyLimits
  { cbblTxsSize :: !Word32
  , cbblExUnits :: !ExUnits
  , cbblRefScriptsSize :: !Int
  }
  deriving stock (Eq, Show, Generic)

instance NFData (ConwayBlockBodyLimits era)

instance Semigroup (ConwayBlockBodyLimits era) where
  ConwayBlockBodyLimits s1 ex1 rss1 <> ConwayBlockBodyLimits s2 ex2 rss2 =
    ConwayBlockBodyLimits (s1 + s2) (ex1 <> ex2) (rss1 + rss2)

instance Monoid (ConwayBlockBodyLimits era) where
  mempty = ConwayBlockBodyLimits 0 mempty 0

instance EraBlockBody ConwayEra where
  type BlockBody ConwayEra = AlonzoBlockBody ConwayEra
  type BlockBodyLimits ConwayEra = ConwayBlockBodyLimits ConwayEra
  type ProtocolEraBlockHeader h ConwayEra = PraosEraBlockHeader h ConwayEra
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
