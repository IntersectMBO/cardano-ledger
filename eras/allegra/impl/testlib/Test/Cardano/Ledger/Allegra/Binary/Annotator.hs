{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Test.Cardano.Ledger.Allegra.Binary.Annotator () where

import Cardano.Ledger.Allegra.Scripts
import Cardano.Ledger.Allegra.TxAuxData
import Cardano.Ledger.Allegra.TxBody.Internal
import Cardano.Ledger.Binary
import Cardano.Ledger.Binary.Coders
import Cardano.Ledger.Core
import qualified Data.Sequence.Strict as StrictSeq
import Test.Cardano.Ledger.Allegra.Arbitrary ()
import Test.Cardano.Ledger.Core.Binary.Annotator

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

-- This instance allows us to derive instance DecCBOR (Annotator (Timelock era)).
-- Since Timelock is a newtype around (Memo (Timelock era)).
instance Era era => DecCBOR (Annotator (TimelockRaw era)) where
  decCBOR = decode (Summands "TimelockRaw" decRaw)
    where
      decRaw :: Word -> Decode 'Open (Annotator (TimelockRaw era))
      decRaw 0 = Ann (SumD Signature <! From)
      decRaw 1 = Ann (SumD AllOf) <*! D (sequence <$> decCBOR)
      decRaw 2 = Ann (SumD AnyOf) <*! D (sequence <$> decCBOR)
      decRaw 3 = Ann (SumD MOfN) <*! Ann From <*! D (sequence <$> decCBOR)
      decRaw 4 = Ann (SumD TimeStart <! From)
      decRaw 5 = Ann (SumD TimeExpire <! From)
      decRaw n = Invalid n

instance Era era => DecCBOR (Annotator (Timelock era)) where
  decCBOR = fmap TimelockConstr <$> decCBOR

instance AllegraEraTxBody era => DecCBOR (Annotator (AllegraTxBodyRaw () era)) where
  decCBOR = pure <$> decCBOR

deriving via
  Mem (AllegraTxBodyRaw () era)
  instance
    AllegraEraTxBody era => DecCBOR (Annotator (AllegraTxBody era))
