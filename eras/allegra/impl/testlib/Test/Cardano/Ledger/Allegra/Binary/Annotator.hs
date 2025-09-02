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
import Cardano.Ledger.Binary.Coders
import Cardano.Ledger.Core
import Cardano.Ledger.MemoBytes (decodeMemoized)
import qualified Data.Sequence.Strict as StrictSeq
import Test.Cardano.Ledger.Allegra.Arbitrary ()
import Test.Cardano.Ledger.Shelley.Binary.Annotator

deriving newtype instance DecCBOR (TxBody AllegraEra)

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
      decodeFromMap =
        decode
          ( Emit AllegraTxAuxDataRaw
              <! From
              <! Emit StrictSeq.empty
          )
      decodeFromList =
        decode
          ( RecD AllegraTxAuxDataRaw
              <! From
              <! From
          )

deriving newtype instance
  (AllegraEraScript era, DecCBOR (NativeScript era)) => DecCBOR (AllegraTxAuxData era)

instance Era era => DecCBOR (TimelockRaw era) where
  decCBOR = decode $ Summands "TimelockRaw" $ \case
    0 -> SumD TimelockSignature <! From
    1 -> SumD TimelockAllOf <! From
    2 -> SumD TimelockAnyOf <! From
    3 -> SumD TimelockMOf <! From <! From
    4 -> SumD TimelockTimeStart <! From
    5 -> SumD TimelockTimeExpire <! From
    n -> Invalid n

instance Era era => DecCBOR (Timelock era) where
  decCBOR = MkTimelock <$> decodeMemoized decCBOR

deriving newtype instance DecCBOR (Tx AllegraEra)
