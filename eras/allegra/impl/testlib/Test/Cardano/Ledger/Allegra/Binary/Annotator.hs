{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralisedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Test.Cardano.Ledger.Allegra.Binary.Annotator (
  module Test.Cardano.Ledger.Shelley.Binary.Annotator,
) where

import Cardano.Ledger.Allegra (AllegraEra, Tx (..))
import Cardano.Ledger.Allegra.Scripts
import Cardano.Ledger.Allegra.TxAuxData
import Cardano.Ledger.Allegra.TxBody
import Cardano.Ledger.Binary
import Cardano.Ledger.Core
import Cardano.Ledger.MemoBytes (decodeMemoized)
import qualified Data.Sequence.Strict as StrictSeq
import Test.Cardano.Ledger.Allegra.Arbitrary ()
import Test.Cardano.Ledger.Shelley.Binary.Annotator

deriving newtype instance DecCBOR (TxBody TopTx AllegraEra)

instance
  ( Era era
  , AllegraEraScript era
  , DecCBOR (NativeScript era)
  ) =>
  DecCBOR (AllegraTxAuxDataRaw era)
  where
  decCBOR =
    peekTokenType >>= \case
      TypeMapLen -> decodeFromMap
      TypeMapLen64 -> decodeFromMap
      TypeMapLenIndef -> decodeFromMap
      TypeListLen -> decodeFromList
      TypeListLen64 -> decodeFromList
      TypeListLenIndef -> decodeFromList
      _ -> fail "Failed to decode AuxiliaryDataRaw"
    where
      decodeFromMap = do
        metadata <- decCBOR
        pure $ AllegraTxAuxDataRaw metadata StrictSeq.empty
      decodeFromList =
        decodeRecordNamed "AllegraTxAuxDataRaw" (const 2) $
          AllegraTxAuxDataRaw <$> decCBOR <*> decCBOR

deriving newtype instance
  (AllegraEraScript era, DecCBOR (NativeScript era)) => DecCBOR (AllegraTxAuxData era)

instance Era era => DecCBOR (TimelockRaw era) where
  decCBOR = decodeRecordSum "TimelockRaw" $ \case
    0 -> do
      keyHash <- decCBOR
      pure (2, TimelockSignature keyHash)
    1 -> do
      timelocks <- decCBOR
      pure (2, TimelockAllOf timelocks)
    2 -> do
      timelocks <- decCBOR
      pure (2, TimelockAnyOf timelocks)
    3 -> do
      requiredCount <- decCBOR
      timelocks <- decCBOR
      pure (3, TimelockMOf requiredCount timelocks)
    4 -> do
      slot <- decCBOR
      pure (2, TimelockTimeStart slot)
    5 -> do
      slot <- decCBOR
      pure (2, TimelockTimeExpire slot)
    k -> invalidKey k

instance Era era => DecCBOR (Timelock era) where
  decCBOR = MkTimelock <$> decodeMemoized decCBOR

deriving newtype instance DecCBOR (Tx TopTx AllegraEra)
