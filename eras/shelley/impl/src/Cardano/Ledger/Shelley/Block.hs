{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Cardano.Ledger.Shelley.Block () where

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
import Cardano.Ledger.Shelley.BlockBody ()
import Cardano.Ledger.Shelley.Era
import Cardano.Ledger.Shelley.Tx ()
import Data.Typeable (Typeable)

instance EncCBOR h => EncCBOR (Block h ShelleyEra) where
  encCBOR (Block h txns) =
    encodeListLen 4 <> encCBOR h <> encCBORGroup txns

instance (EncCBOR h, Typeable h) => Plain.ToCBOR (Block h ShelleyEra) where
  toCBOR = toPlainEncoding (eraProtVerLow @ShelleyEra) . encCBOR

instance
  (DecCBOR (Annotator h), Typeable h) =>
  DecCBOR (Annotator (Block h ShelleyEra))
  where
  decCBOR =
    decodeRecordNamed "Block" (const 4) $ do
      header <- decCBOR
      txns <- decCBOR
      pure $ Block <$> header <*> txns
