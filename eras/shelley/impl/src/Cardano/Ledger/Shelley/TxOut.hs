{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Cardano.Ledger.Shelley.TxOut (
  ShelleyTxOut (ShelleyTxOut, TxOutCompact),

  -- * Helpers
  addrEitherShelleyTxOutL,
  valueEitherShelleyTxOutL,
) where

import Cardano.Ledger.Address (Addr (..), CompactAddr, compactAddr, decompactAddr)
import Cardano.Ledger.BaseTypes (KeyValuePairs (..), ToKeyValuePairs (..))
import Cardano.Ledger.Binary (
  DecCBOR (..),
  DecShareCBOR (..),
  EncCBOR (..),
  FromCBOR (..),
  Interns (..),
  ToCBOR (..),
  TokenType (..),
  decodeMemPack,
  decodeRecordNamed,
  encodeListLen,
  peekTokenType,
 )
import Cardano.Ledger.Compactible (Compactible (CompactForm, fromCompact, toCompact))
import Cardano.Ledger.Core
import Cardano.Ledger.Credential (Credential)
import Cardano.Ledger.Shelley.Era (ShelleyEra)
import Cardano.Ledger.Shelley.PParams ()
import Cardano.Ledger.Val (Val)
import Control.DeepSeq (NFData (rnf))
import Data.Aeson (ToJSON (..), (.=))
import Data.Maybe (fromMaybe)
import Data.MemPack
import GHC.Generics (Generic)
import GHC.Stack (HasCallStack)
import Lens.Micro
import NoThunks.Class (InspectHeapNamed (..), NoThunks (..))

data ShelleyTxOut era = TxOutCompact
  { txOutCompactAddr :: {-# UNPACK #-} !CompactAddr
  , txOutCompactValue :: !(CompactForm (Value era))
  }
  deriving (Generic)

-- | This instance uses a zero Tag for forward compatibility in binary representation with TxOut
-- instances for future eras
instance (Era era, MemPack (CompactForm (Value era))) => MemPack (ShelleyTxOut era) where
  packedByteCount = \case
    TxOutCompact cAddr cValue ->
      packedTagByteCount + packedByteCount cAddr + packedByteCount cValue
  {-# INLINE packedByteCount #-}
  packM = \case
    TxOutCompact cAddr cValue ->
      packTagM 0 >> packM cAddr >> packM cValue
  {-# INLINE packM #-}
  unpackM =
    unpackTagM >>= \case
      0 -> TxOutCompact <$> unpackM <*> unpackM
      n -> unknownTagM @(ShelleyTxOut era) n
  {-# INLINE unpackM #-}

instance EraTxOut ShelleyEra where
  type TxOut ShelleyEra = ShelleyTxOut ShelleyEra

  mkBasicTxOut = ShelleyTxOut

  -- Calling this partial function will result in compilation error, since ByronEra has
  -- no instance for EraTxOut type class.
  upgradeTxOut = error "It is not possible to translate Byron TxOut with 'upgradeTxOut'"

  addrEitherTxOutL = addrEitherShelleyTxOutL
  {-# INLINE addrEitherTxOutL #-}

  valueEitherTxOutL = valueEitherShelleyTxOutL
  {-# INLINE valueEitherTxOutL #-}

  getMinCoinTxOut pp _ = pp ^. ppMinUTxOValueL

addrEitherShelleyTxOutL :: Lens' (ShelleyTxOut era) (Either Addr CompactAddr)
addrEitherShelleyTxOutL =
  lens
    (Right . txOutCompactAddr)
    ( \txOut -> \case
        Left addr -> txOut {txOutCompactAddr = compactAddr addr}
        Right cAddr -> txOut {txOutCompactAddr = cAddr}
    )
{-# INLINE addrEitherShelleyTxOutL #-}

valueEitherShelleyTxOutL ::
  Val (Value era) => Lens' (ShelleyTxOut era) (Either (Value era) (CompactForm (Value era)))
valueEitherShelleyTxOutL =
  lens
    (Right . txOutCompactValue)
    ( \txOut -> \case
        Left value ->
          txOut
            { txOutCompactValue =
                fromMaybe (error $ "Illegal value in TxOut: " <> show value) $ toCompact value
            }
        Right cValue -> txOut {txOutCompactValue = cValue}
    )
{-# INLINE valueEitherShelleyTxOutL #-}

instance (Era era, Val (Value era)) => Show (ShelleyTxOut era) where
  show = show . viewCompactTxOut -- FIXME: showing TxOut as a tuple is just sad

deriving instance Eq (CompactForm (Value era)) => Eq (ShelleyTxOut era)

instance NFData (ShelleyTxOut era) where
  rnf = (`seq` ())

deriving via InspectHeapNamed "TxOut" (ShelleyTxOut era) instance NoThunks (ShelleyTxOut era)

pattern ShelleyTxOut ::
  (HasCallStack, Era era, Val (Value era)) =>
  Addr ->
  Value era ->
  ShelleyTxOut era
pattern ShelleyTxOut addr vl <-
  (viewCompactTxOut -> (addr, vl))
  where
    ShelleyTxOut addr vl =
      TxOutCompact
        (compactAddr addr)
        (fromMaybe (error $ "Illegal value in TxOut: " <> show vl) $ toCompact vl)

{-# COMPLETE ShelleyTxOut #-}

viewCompactTxOut :: Val (Value era) => ShelleyTxOut era -> (Addr, Value era)
viewCompactTxOut TxOutCompact {txOutCompactAddr, txOutCompactValue} =
  (decompactAddr txOutCompactAddr, fromCompact txOutCompactValue)

instance (Era era, EncCBOR (CompactForm (Value era))) => EncCBOR (ShelleyTxOut era) where
  encCBOR (TxOutCompact addr coin) =
    encodeListLen 2
      <> encCBOR addr
      <> encCBOR coin

instance (Era era, DecCBOR (CompactForm (Value era))) => DecCBOR (ShelleyTxOut era) where
  decCBOR =
    decodeRecordNamed "ShelleyTxOut" (const 2) $ do
      cAddr <- decCBOR
      TxOutCompact cAddr <$> decCBOR

instance
  ( Era era
  , MemPack (CompactForm (Value era))
  , DecCBOR (CompactForm (Value era))
  ) =>
  DecShareCBOR (ShelleyTxOut era)
  where
  type Share (ShelleyTxOut era) = Interns (Credential Staking)
  decShareCBOR _ = do
    peekTokenType >>= \case
      TypeBytes -> decodeMemPack
      TypeBytesIndef -> decodeMemPack
      _ -> decCBOR

instance (Era era, EncCBOR (CompactForm (Value era))) => ToCBOR (ShelleyTxOut era) where
  toCBOR = toEraCBOR @era

instance (Era era, DecCBOR (CompactForm (Value era))) => FromCBOR (ShelleyTxOut era) where
  fromCBOR = fromEraCBOR @era

deriving via
  KeyValuePairs (ShelleyTxOut era)
  instance
    (Era era, Val (Value era)) => ToJSON (ShelleyTxOut era)

instance (Era era, Val (Value era)) => ToKeyValuePairs (ShelleyTxOut era) where
  toKeyValuePairs (ShelleyTxOut !addr !amount) =
    [ "address" .= addr
    , "amount" .= amount
    ]
