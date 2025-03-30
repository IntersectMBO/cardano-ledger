{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
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

import Cardano.Ledger.Allegra.Scripts
import Cardano.Ledger.Allegra.TxAuxData
import Cardano.Ledger.Allegra.TxBody.Internal
import Cardano.Ledger.Binary
import Cardano.Ledger.Binary.Coders
import Cardano.Ledger.Core
import qualified Data.Sequence.Strict as StrictSeq
import Test.Cardano.Ledger.Allegra.Arbitrary ()
import Test.Cardano.Ledger.Core.Binary.Annotator
import Test.Cardano.Ledger.Shelley.Binary.Annotator

instance Era era => DecCBOR (Annotator (AllegraTxAuxDataRaw era)) where
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
      decodeFromMap =
        decode
          ( Ann (Emit AllegraTxAuxDataRaw)
              <*! Ann From
              <*! Ann (Emit StrictSeq.empty)
          )
      decodeFromList =
        decode
          ( Ann (RecD AllegraTxAuxDataRaw)
              <*! Ann From
              <*! D (sequence <$> decCBOR)
          )

deriving via
  (Mem (AllegraTxAuxDataRaw era))
  instance
    Era era => DecCBOR (Annotator (AllegraTxAuxData era))

instance Era era => DecCBOR (Annotator (TimelockRaw era)) where
  decCBOR = decode (Summands "TimelockRaw" decRaw)
    where
      decRaw :: Word -> Decode 'Open (Annotator (TimelockRaw era))
      decRaw 0 = Ann (SumD TimelockSignature <! From)
      decRaw 1 = Ann (SumD TimelockAllOf) <*! D (sequence <$> decCBOR)
      decRaw 2 = Ann (SumD TimelockAnyOf) <*! D (sequence <$> decCBOR)
      decRaw 3 = Ann (SumD TimelockMOf) <*! Ann From <*! D (sequence <$> decCBOR)
      decRaw 4 = Ann (SumD TimelockTimeStart <! From)
      decRaw 5 = Ann (SumD TimelockTimeExpire <! From)
      decRaw n = Invalid n

instance Era era => DecCBOR (Annotator (Timelock era)) where
  decCBOR = fmap MkTimelock <$> decCBOR

instance
  (DecCBOR m, Monoid m, AllegraEraTxBody era) =>
  DecCBOR (Annotator (AllegraTxBodyRaw m era))
  where
  decCBOR = pure <$> decCBOR

deriving via
  Mem (AllegraTxBodyRaw () era)
  instance
    AllegraEraTxBody era => DecCBOR (Annotator (AllegraTxBody era))
