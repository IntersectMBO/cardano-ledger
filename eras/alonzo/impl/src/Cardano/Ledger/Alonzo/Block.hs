{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Cardano.Ledger.Alonzo.Block () where

import Cardano.Ledger.Alonzo.BlockBody ()
import Cardano.Ledger.Alonzo.Era
import Cardano.Ledger.Alonzo.Tx ()
import Cardano.Ledger.Binary (
  Annotator,
  DecCBOR (decCBOR),
  EncCBOR (..),
  EncCBORGroup (..),
  decodeRecordNamed,
  encodeListLen,
  toPlainEncoding,
 )
import qualified Cardano.Ledger.Binary.Plain as Plain
import Cardano.Ledger.Block (Block (..))
import Cardano.Ledger.Core
import Data.Typeable (Typeable)

instance EncCBOR h => EncCBOR (Block h AlonzoEra) where
  encCBOR (Block h txns) =
    encodeListLen 5 <> encCBOR h <> encCBORGroup txns

instance (EncCBOR h, Typeable h) => Plain.ToCBOR (Block h AlonzoEra) where
  toCBOR = toPlainEncoding (eraProtVerLow @AlonzoEra) . encCBOR

instance
  (DecCBOR (Annotator h), Typeable h) =>
  DecCBOR (Annotator (Block h AlonzoEra))
  where
  decCBOR =
    decodeRecordNamed "Block" (const 5) $ do
      header <- decCBOR
      txns <- decCBOR
      pure $ Block <$> header <*> txns
