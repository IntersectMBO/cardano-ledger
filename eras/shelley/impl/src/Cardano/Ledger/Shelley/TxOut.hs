{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DataKinds #-}
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

import qualified Cardano.Crypto.Hash as HS
import Cardano.HeapWords (HeapWords (..))
import Cardano.Ledger.Address (Addr (..), CompactAddr, compactAddr, decompactAddr)
import Cardano.Ledger.Binary (
  DecCBOR (..),
  DecShareCBOR (..),
  EncCBOR (..),
  FromCBOR (..),
  Interns (..),
  ToCBOR (..),
  decodeRecordNamed,
  encodeListLen,
 )
import Cardano.Ledger.Compactible (Compactible (CompactForm, fromCompact, toCompact))
import Cardano.Ledger.Core
import Cardano.Ledger.Credential (Credential)
import Cardano.Ledger.Crypto (Crypto (ADDRHASH), StandardCrypto)
import Cardano.Ledger.Keys (KeyRole (..))
import Cardano.Ledger.Shelley.Era (ShelleyEra)
import Cardano.Ledger.Shelley.PParams ()
import Cardano.Ledger.Val (Val)
import Data.Aeson (KeyValue, ToJSON (..), object, pairs, (.=))

import Control.DeepSeq (NFData (rnf))
import Data.ByteString.Short (ShortByteString, pack)
import Data.Maybe (fromMaybe)
import Data.Proxy (Proxy (..))
import Data.Word (Word8)
import GHC.Stack (HasCallStack)
import Lens.Micro
import NoThunks.Class (InspectHeapNamed (..), NoThunks (..))

data ShelleyTxOut era = TxOutCompact
  { txOutCompactAddr :: {-# UNPACK #-} !(CompactAddr (EraCrypto era))
  , txOutCompactValue :: !(CompactForm (Value era))
  }

instance Crypto crypto => EraTxOut (ShelleyEra crypto) where
  {-# SPECIALIZE instance EraTxOut (ShelleyEra StandardCrypto) #-}

  type TxOut (ShelleyEra crypto) = ShelleyTxOut (ShelleyEra crypto)

  mkBasicTxOut = ShelleyTxOut

  -- Calling this partial function will result in compilation error, since ByronEra has
  -- no instance for EraTxOut type class.
  upgradeTxOut = error "It is not possible to translate Byron TxOut with 'upgradeTxOut'"

  addrEitherTxOutL = addrEitherShelleyTxOutL
  {-# INLINE addrEitherTxOutL #-}

  valueEitherTxOutL = valueEitherShelleyTxOutL
  {-# INLINE valueEitherTxOutL #-}

  getMinCoinTxOut pp _ = pp ^. ppMinUTxOValueL

addrEitherShelleyTxOutL ::
  Lens' (ShelleyTxOut era) (Either (Addr (EraCrypto era)) (CompactAddr (EraCrypto era)))
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

-- assume Shelley+ type address : payment addr, staking addr (same length as payment), plus 1 word overhead
instance (Era era, HeapWords (CompactForm (Value era))) => HeapWords (ShelleyTxOut era) where
  heapWords (TxOutCompact _ vl) =
    3
      + heapWords (packedADDRHASH (Proxy :: Proxy era))
      + heapWords vl

instance (Era era, Val (Value era)) => Show (ShelleyTxOut era) where
  show = show . viewCompactTxOut -- FIXME: showing TxOut as a tuple is just sad

deriving instance Eq (CompactForm (Value era)) => Eq (ShelleyTxOut era)

instance NFData (ShelleyTxOut era) where
  rnf = (`seq` ())

deriving via InspectHeapNamed "TxOut" (ShelleyTxOut era) instance NoThunks (ShelleyTxOut era)

pattern ShelleyTxOut ::
  (HasCallStack, Era era, Val (Value era)) =>
  Addr (EraCrypto era) ->
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

viewCompactTxOut ::
  (Era era, Val (Value era)) => ShelleyTxOut era -> (Addr (EraCrypto era), Value era)
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

instance (Era era, DecCBOR (CompactForm (Value era))) => DecShareCBOR (ShelleyTxOut era) where
  type Share (ShelleyTxOut era) = Interns (Credential 'Staking (EraCrypto era))
  decShareCBOR _ = decCBOR

instance (Era era, EncCBOR (CompactForm (Value era))) => ToCBOR (ShelleyTxOut era) where
  toCBOR = toEraCBOR @era

instance (Era era, DecCBOR (CompactForm (Value era))) => FromCBOR (ShelleyTxOut era) where
  fromCBOR = fromEraCBOR @era

instance (Era era, Val (Value era)) => ToJSON (ShelleyTxOut era) where
  toJSON = object . toTxOutPair
  toEncoding = pairs . mconcat . toTxOutPair

toTxOutPair :: (KeyValue e a, Era era, Val (Value era)) => ShelleyTxOut era -> [a]
toTxOutPair (ShelleyTxOut !addr !amount) =
  [ "address" .= addr
  , "amount" .= amount
  ]

-- a ShortByteString of the same length as the ADDRHASH
-- used to calculate heapWords
packedADDRHASH :: forall proxy era. Crypto (EraCrypto era) => proxy era -> ShortByteString
packedADDRHASH _ =
  pack $
    replicate
      (fromIntegral (1 + 2 * HS.sizeHash (Proxy :: Proxy (ADDRHASH (EraCrypto era)))))
      (1 :: Word8)
