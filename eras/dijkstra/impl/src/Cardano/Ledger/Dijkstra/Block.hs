{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Cardano.Ledger.Dijkstra.Block () where

import Cardano.Ledger.Binary (
  Annotator,
  DecCBOR (decCBOR),
  EncCBOR (..),
  decodeRecordNamed,
  encodeListLen,
  toPlainEncoding,
 )
import qualified Cardano.Ledger.Binary.Plain as Plain
import Cardano.Ledger.Block (Block (..))
import Cardano.Ledger.Core (EraBlockBody, eraProtVerLow)
import Cardano.Ledger.Dijkstra.BlockBody.Internal ()
import Cardano.Ledger.Dijkstra.Era (DijkstraEra)
import Data.Typeable (Typeable)

instance EncCBOR h => EncCBOR (Block h DijkstraEra) where
  encCBOR (Block h body) =
    encodeListLen 2 <> encCBOR h <> encCBOR body

instance (EncCBOR h, Typeable h) => Plain.ToCBOR (Block h DijkstraEra) where
  toCBOR = toPlainEncoding (eraProtVerLow @DijkstraEra) . encCBOR

instance
  (DecCBOR (Annotator h), Typeable h, EraBlockBody DijkstraEra) =>
  DecCBOR (Annotator (Block h DijkstraEra))
  where
  decCBOR =
    decodeRecordNamed "Block" (const 2) $ do
      header <- decCBOR
      body <- decCBOR
      pure $ Block <$> header <*> body
