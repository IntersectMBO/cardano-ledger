{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Cardano.Ledger.Allegra.Scripts (
  Timelock (
    RequireSignature,
    RequireAllOf,
    RequireAnyOf,
    RequireMOf,
    RequireTimeExpire,
    RequireTimeStart
  ),
  pattern TimelockConstr,
  inInterval,
  showTimelock,
  evalTimelock,
  eqTimelockRaw,
  ValidityInterval (..),
  encodeVI,
  decodeVI,
  -- translate,
  translateTimelock,
)
where

import Cardano.Crypto.Hash.Class (HashAlgorithm)
import Cardano.Ledger.Allegra.Era (AllegraEra)
import Cardano.Ledger.BaseTypes (StrictMaybe (SJust, SNothing))
import Cardano.Ledger.Binary (
  Annotator (..),
  EncCBOR (..),
  FromCBOR (fromCBOR),
  ToCBOR (toCBOR),
  encodeStrictSeq,
  fromPlainEncoding,
 )
import Cardano.Ledger.Binary.Coders (
  Decode (..),
  Density (..),
  Encode (..),
  Wrapped (..),
  decode,
  encode,
  (!>),
  (<!),
  (<*!),
 )
import Cardano.Ledger.Core
import Cardano.Ledger.Crypto (Crypto, HASH)
import Cardano.Ledger.Keys (KeyHash (..), KeyRole (Witness))
import Cardano.Ledger.MemoBytes (
  Mem,
  MemoBytes (Memo),
  Memoized (..),
  getMemoRawType,
  mkMemoBytes,
  mkMemoized,
 )
import Cardano.Ledger.SafeHash (SafeToHash)
import Cardano.Ledger.Shelley.Scripts (nativeMultiSigTag)
import Cardano.Slotting.Slot (SlotNo (..))
import Control.DeepSeq (NFData (..))
import Data.ByteString.Lazy (fromStrict)
import Data.ByteString.Short (fromShort)
import Data.Sequence.Strict (StrictSeq (Empty, (:<|)))
import Data.Set (Set, member)
import GHC.Generics (Generic)
import NoThunks.Class (NoThunks (..))

-- | ValidityInterval is a half open interval. Closed on the bottom, open on the top.
--   A SNothing on the bottom is negative infinity, and a SNothing on the top is positive infinity
data ValidityInterval = ValidityInterval
  { invalidBefore :: !(StrictMaybe SlotNo)
  , invalidHereafter :: !(StrictMaybe SlotNo)
  }
  deriving (Ord, Eq, Generic, Show, NoThunks, NFData)

encodeVI :: ValidityInterval -> Encode ('Closed 'Dense) ValidityInterval
encodeVI (ValidityInterval f t) = Rec ValidityInterval !> To f !> To t

instance ToCBOR ValidityInterval where
  toCBOR vi = encode (encodeVI vi)

decodeVI :: Decode ('Closed 'Dense) ValidityInterval
decodeVI = RecD ValidityInterval <! From <! From

instance FromCBOR ValidityInterval where
  fromCBOR = decode decodeVI

-- ==================================================================

data TimelockRaw era
  = Signature !(KeyHash 'Witness (EraCrypto era))
  | AllOf !(StrictSeq (Timelock era)) -- NOTE that Timelock and
  | AnyOf !(StrictSeq (Timelock era)) -- TimelockRaw are mutually recursive.
  | MOfN !Int !(StrictSeq (Timelock era))
  | -- Note that the Int may be negative in which case (MOfN (-2) [..]) is always True
    TimeStart !SlotNo -- The start time
  | TimeExpire !SlotNo -- The time it expires
  deriving (Eq, Generic, NFData)

deriving instance Era era => NoThunks (TimelockRaw era)

deriving instance HashAlgorithm (HASH (EraCrypto era)) => Show (TimelockRaw era)

-- | This function deconstructs and then reconstructs the timelock script
-- to prove the compiler that we can arbirarily switch out the eras as long
-- as the cryptos for both eras are the same.
translateTimelock ::
  forall era1 era2.
  ( Era era1
  , Era era2
  , EraCrypto era1 ~ EraCrypto era2
  ) =>
  Timelock era1 ->
  Timelock era2
translateTimelock (TimelockConstr (Memo tl bs)) =
  let rewrap rtl = TimelockConstr $ mkMemoBytes rtl (fromStrict $ fromShort bs)
   in case tl of
        Signature s -> rewrap $ Signature s
        AllOf l -> rewrap . AllOf $ translateTimelock <$> l
        AnyOf l -> rewrap . AnyOf $ translateTimelock <$> l
        MOfN n l -> rewrap $ MOfN n (translateTimelock <$> l)
        TimeStart x -> rewrap $ TimeStart x
        TimeExpire x -> rewrap $ TimeExpire x

-- These coding choices are chosen so that a MultiSig script
-- can be deserialised as a Timelock script

instance Era era => ToCBOR (TimelockRaw era) where
  toCBOR =
    encode . \case
      Signature hash -> Sum Signature 0 !> To hash
      AllOf xs -> Sum AllOf 1 !> E (encodeStrictSeq (fromPlainEncoding . encCBOR)) xs
      AnyOf xs -> Sum AnyOf 2 !> E (encodeStrictSeq (fromPlainEncoding . encCBOR)) xs
      MOfN m xs -> Sum MOfN 3 !> To m !> E (encodeStrictSeq (fromPlainEncoding . encCBOR)) xs
      TimeStart m -> Sum TimeStart 4 !> To m
      TimeExpire m -> Sum TimeExpire 5 !> To m

-- This instance allows us to derive instance FromCBOR (Annotator (Timelock crypto)).
-- Since Timelock is a newtype around (Memo (Timelock crypto)).

instance Era era => FromCBOR (Annotator (TimelockRaw era)) where
  fromCBOR = decode (Summands "TimelockRaw" decRaw)
    where
      decRaw :: Word -> Decode 'Open (Annotator (TimelockRaw era))
      decRaw 0 = Ann (SumD Signature <! From)
      decRaw 1 = Ann (SumD AllOf) <*! D (sequence <$> fromCBOR)
      decRaw 2 = Ann (SumD AnyOf) <*! D (sequence <$> fromCBOR)
      decRaw 3 = Ann (SumD MOfN) <*! Ann From <*! D (sequence <$> fromCBOR)
      decRaw 4 = Ann (SumD TimeStart <! From)
      decRaw 5 = Ann (SumD TimeExpire <! From)
      decRaw n = Invalid n

-- =================================================================
-- Native Scripts are Memoized TimelockRaw.
-- The patterns give the appearence that the mutual recursion is not present.
-- They rely on memoBytes, and TimelockRaw to memoize each constructor of Timelock
-- =================================================================

newtype Timelock era = TimelockConstr (MemoBytes TimelockRaw era)
  deriving (Eq, Generic)
  deriving newtype (EncCBOR, NoThunks, NFData, SafeToHash)

instance Era era => ToCBOR (Timelock era)

instance Memoized Timelock where
  type RawType Timelock = TimelockRaw

deriving instance HashAlgorithm (HASH (EraCrypto era)) => Show (Timelock era)

type instance SomeScript 'PhaseOne (AllegraEra c) = Timelock (AllegraEra c)

-- | Since Timelock scripts are a strictly backwards compatible extension of
-- MultiSig scripts, we can use the same 'scriptPrefixTag' tag here as we did
-- for the ValidateScript instance in MultiSig
instance Crypto c => EraScript (AllegraEra c) where
  type Script (AllegraEra c) = Timelock (AllegraEra c)
  scriptPrefixTag _script = nativeMultiSigTag -- "\x00"
  phaseScript PhaseOneRep timelock = Just (Phase1Script timelock)
  phaseScript PhaseTwoRep _ = Nothing

deriving via
  Mem TimelockRaw era
  instance
    Era era => FromCBOR (Annotator (Timelock era))

pattern RequireSignature :: Era era => KeyHash 'Witness (EraCrypto era) -> Timelock era
pattern RequireSignature akh <- (getMemoRawType -> Signature akh)
  where
    RequireSignature akh = mkMemoized (Signature akh)

pattern RequireAllOf :: Era era => StrictSeq (Timelock era) -> Timelock era
pattern RequireAllOf ms <- (getMemoRawType -> AllOf ms)
  where
    RequireAllOf ms = mkMemoized (AllOf ms)

pattern RequireAnyOf :: Era era => StrictSeq (Timelock era) -> Timelock era
pattern RequireAnyOf ms <- (getMemoRawType -> AnyOf ms)
  where
    RequireAnyOf ms = mkMemoized (AnyOf ms)

pattern RequireMOf :: Era era => Int -> StrictSeq (Timelock era) -> Timelock era
pattern RequireMOf n ms <- (getMemoRawType -> MOfN n ms)
  where
    RequireMOf n ms = mkMemoized (MOfN n ms)

pattern RequireTimeExpire :: Era era => SlotNo -> Timelock era
pattern RequireTimeExpire mslot <- (getMemoRawType -> TimeExpire mslot)
  where
    RequireTimeExpire mslot = mkMemoized (TimeExpire mslot)

pattern RequireTimeStart :: Era era => SlotNo -> Timelock era
pattern RequireTimeStart mslot <- (getMemoRawType -> TimeStart mslot)
  where
    RequireTimeStart mslot = mkMemoized (TimeStart mslot)

{-# COMPLETE
  RequireSignature
  , RequireAllOf
  , RequireAnyOf
  , RequireMOf
  , RequireTimeExpire
  , RequireTimeStart
  #-}

-- =================================================================
-- Evaluating and validating a Timelock

-- | less-than-equal comparison, where Nothing is negative infinity
lteNegInfty :: SlotNo -> StrictMaybe SlotNo -> Bool
lteNegInfty _ SNothing = False -- i > -∞
lteNegInfty i (SJust j) = i <= j

-- | less-than-equal comparison, where Nothing is positive infinity
ltePosInfty :: StrictMaybe SlotNo -> SlotNo -> Bool
ltePosInfty SNothing _ = False -- ∞ > j
ltePosInfty (SJust i) j = i <= j

evalTimelock ::
  Era era =>
  Set (KeyHash 'Witness (EraCrypto era)) ->
  ValidityInterval ->
  Timelock era ->
  Bool
evalTimelock _vhks (ValidityInterval txStart _) (RequireTimeStart lockStart) =
  lockStart `lteNegInfty` txStart
evalTimelock _vhks (ValidityInterval _ txExp) (RequireTimeExpire lockExp) =
  txExp `ltePosInfty` lockExp
evalTimelock vhks _vi (RequireSignature hash) = member hash vhks
evalTimelock vhks vi (RequireAllOf xs) =
  all (evalTimelock vhks vi) xs
evalTimelock vhks vi (RequireAnyOf xs) =
  any (evalTimelock vhks vi) xs
evalTimelock vhks vi (RequireMOf m xs) =
  m <= sum (fmap (\x -> if evalTimelock vhks vi x then 1 else 0) xs)

-- =========================================================
-- Operations on Timelock scripts

-- | Test if a slot is in the Validity interval. Recall that a ValidityInterval
--   is a half Open interval, that is why we use (slot < top)
inInterval :: SlotNo -> ValidityInterval -> Bool
inInterval _slot (ValidityInterval SNothing SNothing) = True
inInterval slot (ValidityInterval SNothing (SJust top)) = slot < top
inInterval slot (ValidityInterval (SJust bottom) SNothing) = bottom <= slot
inInterval slot (ValidityInterval (SJust bottom) (SJust top)) =
  bottom <= slot && slot < top

showTimelock :: Era era => Timelock era -> String
showTimelock (RequireTimeStart (SlotNo i)) = "(Start >= " ++ show i ++ ")"
showTimelock (RequireTimeExpire (SlotNo i)) = "(Expire < " ++ show i ++ ")"
showTimelock (RequireAllOf xs) = "(AllOf " ++ foldl accum ")" xs
  where
    accum ans x = showTimelock x ++ " " ++ ans
showTimelock (RequireAnyOf xs) = "(AnyOf " ++ foldl accum ")" xs
  where
    accum ans x = showTimelock x ++ " " ++ ans
showTimelock (RequireMOf m xs) = "(MOf " ++ show m ++ " " ++ foldl accum ")" xs
  where
    accum ans x = showTimelock x ++ " " ++ ans
showTimelock (RequireSignature hash) = "(Signature " ++ show hash ++ ")"

-- | Check the equality of two underlying types, while ignoring their binary
-- representation, which `Eq` instance normally does. This is used for testing.
eqTimelockRaw :: Timelock era -> Timelock era -> Bool
eqTimelockRaw t1 t2 = go (getMemoRawType t1) (getMemoRawType t2)
  where
    seqEq Empty Empty = True
    seqEq (x :<| xs) (y :<| ys) = eqTimelockRaw x y && seqEq xs ys
    seqEq _ _ = False
    go (Signature kh1) (Signature kh2) = kh1 == kh2
    go (AllOf ts1) (AllOf ts2) = seqEq ts1 ts2
    go (AnyOf ts1) (AnyOf ts2) = seqEq ts1 ts2
    go (MOfN n1 ts1) (MOfN n2 ts2) = n1 == n2 && seqEq ts1 ts2
    go (TimeStart sn1) (TimeStart sn2) = sn1 == sn2
    go (TimeExpire sn1) (TimeExpire sn2) = sn1 == sn2
    go _ _ = False
