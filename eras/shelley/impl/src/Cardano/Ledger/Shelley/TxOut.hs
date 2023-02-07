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
  FromCBOR (..),
  FromSharedCBOR (..),
  Interns (..),
  ToCBOR (..),
  decodeRecordNamed,
  encodeListLen,
  fromNotSharedCBOR,
 )
import Cardano.Ledger.Compactible (Compactible (CompactForm, fromCompact, toCompact))
import Cardano.Ledger.Core (Era (EraCrypto), EraTxOut (..), Value, ppMinUTxOValueL)
import Cardano.Ledger.Credential (Credential)
import Cardano.Ledger.Crypto (Crypto (ADDRHASH), StandardCrypto)
import Cardano.Ledger.Keys (KeyRole (..))
import Cardano.Ledger.Shelley.Era (ShelleyEra)
import Cardano.Ledger.Shelley.PParams ()
import Cardano.Ledger.TreeDiff (Expr (App), ToExpr (toExpr))

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
  (Show (Value era), Compactible (Value era)) =>
  Lens' (ShelleyTxOut era) (Either (Value era) (CompactForm (Value era)))
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

instance (Era era, Compactible (Value era), Show (Value era)) => Show (ShelleyTxOut era) where
  show = show . viewCompactTxOut -- FIXME: showing TxOut as a tuple is just sad

deriving instance Eq (CompactForm (Value era)) => Eq (ShelleyTxOut era)

instance NFData (ShelleyTxOut era) where
  rnf = (`seq` ())

deriving via InspectHeapNamed "TxOut" (ShelleyTxOut era) instance NoThunks (ShelleyTxOut era)

pattern ShelleyTxOut ::
  (HasCallStack, EraTxOut era) =>
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

viewCompactTxOut :: (Era era, Compactible (Value era)) => ShelleyTxOut era -> (Addr (EraCrypto era), Value era)
viewCompactTxOut TxOutCompact {txOutCompactAddr, txOutCompactValue} =
  (decompactAddr txOutCompactAddr, fromCompact txOutCompactValue)

instance (Era era, ToCBOR (CompactForm (Value era))) => ToCBOR (ShelleyTxOut era) where
  toCBOR (TxOutCompact addr coin) =
    encodeListLen 2
      <> toCBOR addr
      <> toCBOR coin

instance
  (Era era, FromCBOR (CompactForm (Value era))) =>
  FromCBOR (ShelleyTxOut era)
  where
  fromCBOR = fromNotSharedCBOR

-- This instance does not do any sharing and is isomorphic to FromCBOR
-- use the weakest constraint necessary
instance
  (Era era, FromCBOR (CompactForm (Value era))) =>
  FromSharedCBOR (ShelleyTxOut era)
  where
  type Share (ShelleyTxOut era) = Interns (Credential 'Staking (EraCrypto era))
  fromSharedCBOR _ =
    decodeRecordNamed "ShelleyTxOut" (const 2) $ do
      cAddr <- fromCBOR
      TxOutCompact cAddr <$> fromCBOR

-- a ShortByteString of the same length as the ADDRHASH
-- used to calculate heapWords
packedADDRHASH :: forall proxy era. (Crypto (EraCrypto era)) => proxy era -> ShortByteString
packedADDRHASH _ =
  pack $
    replicate
      (fromIntegral (1 + 2 * HS.sizeHash (Proxy :: Proxy (ADDRHASH (EraCrypto era)))))
      (1 :: Word8)

-- ============================================================

instance (EraTxOut era, ToExpr (Value era)) => ToExpr (ShelleyTxOut era) where
  toExpr (ShelleyTxOut x y) = App "ShelleyTxOut" [toExpr x, toExpr y]
