{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Test.Cardano.Ledger.Conway.Binary.Annotator (
  module Test.Cardano.Ledger.Babbage.Binary.Annotator,
) where

import Cardano.Ledger.Binary
import Cardano.Ledger.Conway.Core
import Cardano.Ledger.Conway.TxBody.Internal
import Test.Cardano.Ledger.Babbage.Binary.Annotator

instance
  ( DecCBOR (TxOut era)
  , EraTxCert era
  , EraPParams era
  ) =>
  DecCBOR (Annotator (ConwayTxBodyRaw era))
  where
  decCBOR = pure <$> decCBOR

deriving via
  Mem (ConwayTxBodyRaw era)
  instance
    ( DecCBOR (TxOut era)
    , EraTxCert era
    , EraPParams era
    ) =>
    DecCBOR (Annotator (ConwayTxBody era))
