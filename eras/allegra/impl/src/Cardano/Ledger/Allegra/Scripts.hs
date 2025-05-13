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
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE UndecidableSuperClasses #-}
{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Cardano.Ledger.Allegra.Scripts (
  AllegraEraScript (..),
  mkRequireSignatureTimelock,
  getRequireSignatureTimelock,
  mkRequireAllOfTimelock,
  getRequireAllOfTimelock,
  mkRequireAnyOfTimelock,
  getRequireAnyOfTimelock,
  mkRequireMOfTimelock,
  getRequireMOfTimelock,
  mkTimeStartTimelock,
  getTimeStartTimelock,
  mkTimeExpireTimelock,
  getTimeExpireTimelock,
  Timelock (MkTimelock, TimelockConstr),
  pattern RequireTimeExpire,
  pattern RequireTimeStart,
  TimelockRaw (..),
  inInterval,
  showTimelock,
  evalTimelock,
  eqTimelockRaw,
  ValidityInterval (..),
  encodeVI,
  decodeVI,
  -- translate,
  translateTimelock,
) where

import Cardano.Ledger.Allegra.Era (AllegraEra)
import Cardano.Ledger.BaseTypes (StrictMaybe (SJust, SNothing))
import Cardano.Ledger.Binary (
  DecCBOR (decCBOR),
  EncCBOR (encCBOR),
  ToCBOR (..),
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
 )
import Cardano.Ledger.Core
import Cardano.Ledger.MemoBytes (
  EqRaw (..),
  MemoBytes (Memo),
  Memoized (..),
  byteCountMemoBytes,
  decodeMemoized,
  getMemoRawType,
  mkMemoizedEra,
  packMemoBytesM,
  unpackMemoBytesM,
 )
import Cardano.Ledger.MemoBytes.Internal (mkMemoBytes)
import Cardano.Ledger.Shelley.Scripts (
  ShelleyEraScript (..),
  nativeMultiSigTag,
  pattern RequireAllOf,
  pattern RequireAnyOf,
  pattern RequireMOf,
  pattern RequireSignature,
 )
import Cardano.Slotting.Slot (SlotNo (..))
import Control.DeepSeq (NFData (..))
import Data.Aeson (ToJSON (..), (.=))
import qualified Data.Aeson as Aeson
import Data.ByteString.Lazy (fromStrict)
import Data.ByteString.Short (fromShort)
import Data.Foldable as F (foldl')
import Data.MemPack
import Data.Sequence.Strict as Seq (StrictSeq (Empty, (:<|)))
import qualified Data.Sequence.Strict as SSeq
import qualified Data.Set as Set (Set, member)
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

instance EncCBOR ValidityInterval where
  encCBOR vi = encode (encodeVI vi)

decodeVI :: Decode ('Closed 'Dense) ValidityInterval
decodeVI = RecD ValidityInterval <! From <! From

instance DecCBOR ValidityInterval where
  decCBOR = decode decodeVI

instance ToJSON ValidityInterval where
  toJSON vi =
    Aeson.object $
      [ k .= v
      | (k, SJust v) <-
          [ ("invalidBefore", invalidBefore vi)
          , ("invalidHereafter", invalidHereafter vi)
          ]
      ]

-- ==================================================================

data TimelockRaw era
  = TimelockSignature !(KeyHash 'Witness)
  | TimelockAllOf !(StrictSeq (Timelock era)) -- NOTE that Timelock and
  | TimelockAnyOf !(StrictSeq (Timelock era)) -- TimelockRaw are mutually recursive.
  | TimelockMOf !Int !(StrictSeq (Timelock era))
  | -- Note that the Int may be negative in which case (TimelockMOf (-2) [..]) is always True
    TimelockTimeStart !SlotNo -- The start time
  | TimelockTimeExpire !SlotNo -- The time it expires
  deriving (Eq, Generic, NFData)

class ShelleyEraScript era => AllegraEraScript era where
  mkTimeStart :: SlotNo -> NativeScript era
  getTimeStart :: NativeScript era -> Maybe SlotNo

  mkTimeExpire :: SlotNo -> NativeScript era
  getTimeExpire :: NativeScript era -> Maybe SlotNo

deriving instance Era era => NoThunks (TimelockRaw era)

deriving instance Show (TimelockRaw era)

-- | This function deconstructs and then reconstructs the timelock script
-- to prove the compiler that we can arbirarily switch out the eras as long
-- as the cryptos for both eras are the same.
translateTimelock ::
  forall era1 era2.
  ( Era era1
  , Era era2
  ) =>
  Timelock era1 ->
  Timelock era2
translateTimelock (MkTimelock (Memo tl bs)) =
  let rewrap rtl = MkTimelock $ mkMemoBytes rtl (fromStrict $ fromShort bs)
   in case tl of
        TimelockSignature s -> rewrap $ TimelockSignature s
        TimelockAllOf l -> rewrap . TimelockAllOf $ translateTimelock <$> l
        TimelockAnyOf l -> rewrap . TimelockAnyOf $ translateTimelock <$> l
        TimelockMOf n l -> rewrap $ TimelockMOf n (translateTimelock <$> l)
        TimelockTimeStart x -> rewrap $ TimelockTimeStart x
        TimelockTimeExpire x -> rewrap $ TimelockTimeExpire x

-- These coding choices are chosen so that a MultiSig script
-- can be deserialised as a Timelock script

instance Era era => EncCBOR (TimelockRaw era) where
  encCBOR =
    encode . \case
      TimelockSignature hash -> Sum TimelockSignature 0 !> To hash
      TimelockAllOf xs -> Sum TimelockAllOf 1 !> To xs
      TimelockAnyOf xs -> Sum TimelockAnyOf 2 !> To xs
      TimelockMOf m xs -> Sum TimelockMOf 3 !> To m !> To xs
      TimelockTimeStart m -> Sum TimelockTimeStart 4 !> To m
      TimelockTimeExpire m -> Sum TimelockTimeExpire 5 !> To m

instance Era era => DecCBOR (TimelockRaw era) where
  decCBOR = decode $ Summands "TimelockRaw" $ \case
    0 -> SumD TimelockSignature <! From
    1 -> SumD TimelockAllOf <! From
    2 -> SumD TimelockAnyOf <! From
    3 -> SumD TimelockMOf <! From <! From
    4 -> SumD TimelockTimeStart <! From
    5 -> SumD TimelockTimeExpire <! From
    n -> Invalid n

-- =================================================================
-- Native Scripts are Memoized TimelockRaw.
-- The patterns give the appearence that the mutual recursion is not present.
-- They rely on memoBytes, and TimelockRaw to memoize each constructor of Timelock
-- =================================================================

newtype Timelock era = MkTimelock (MemoBytes (TimelockRaw era))
  deriving (Eq, Generic)
  deriving newtype (ToCBOR, NFData, SafeToHash)

pattern TimelockConstr :: MemoBytes (TimelockRaw era) -> Timelock era
pattern TimelockConstr timelockRaw = MkTimelock timelockRaw

{-# COMPLETE TimelockConstr #-}

{-# DEPRECATED TimelockConstr "In favor of more consistently name `MkTimelock`" #-}

instance Era era => MemPack (Timelock era) where
  packedByteCount (MkTimelock mb) = byteCountMemoBytes mb
  packM (MkTimelock mb) = packMemoBytesM mb
  unpackM = MkTimelock <$> unpackMemoBytesM (eraProtVerLow @era)

instance Era era => NoThunks (Timelock era)

instance Era era => EncCBOR (Timelock era)

instance Era era => DecCBOR (Timelock era) where
  decCBOR = MkTimelock <$> decodeMemoized decCBOR

instance Memoized (Timelock era) where
  type RawType (Timelock era) = TimelockRaw era

deriving instance Show (Timelock era)

instance EqRaw (Timelock era) where
  eqRaw = eqTimelockRaw

-- | Since Timelock scripts are a strictly backwards compatible extension of
-- MultiSig scripts, we can use the same 'scriptPrefixTag' tag here as we did
-- for the ValidateScript instance in MultiSig
instance EraScript AllegraEra where
  type Script AllegraEra = Timelock AllegraEra
  type NativeScript AllegraEra = Timelock AllegraEra

  upgradeScript = \case
    RequireSignature keyHash -> RequireSignature keyHash
    RequireAllOf sigs -> RequireAllOf $ upgradeScript <$> sigs
    RequireAnyOf sigs -> RequireAnyOf $ upgradeScript <$> sigs
    RequireMOf n sigs -> RequireMOf n $ upgradeScript <$> sigs
    _ -> error "Impossible: All NativeScripts should have been accounted for"

  scriptPrefixTag _script = nativeMultiSigTag -- "\x00"

  getNativeScript = Just

  fromNativeScript = id

instance ShelleyEraScript AllegraEra where
  mkRequireSignature = mkRequireSignatureTimelock
  getRequireSignature = getRequireSignatureTimelock

  mkRequireAllOf = mkRequireAllOfTimelock
  getRequireAllOf = getRequireAllOfTimelock

  mkRequireAnyOf = mkRequireAnyOfTimelock
  getRequireAnyOf = getRequireAnyOfTimelock

  mkRequireMOf = mkRequireMOfTimelock
  getRequireMOf = getRequireMOfTimelock

instance AllegraEraScript AllegraEra where
  mkTimeStart = mkTimeStartTimelock
  getTimeStart = getTimeStartTimelock

  mkTimeExpire = mkTimeExpireTimelock
  getTimeExpire = getTimeExpireTimelock

pattern RequireTimeExpire :: AllegraEraScript era => SlotNo -> NativeScript era
pattern RequireTimeExpire mslot <- (getTimeExpire -> Just mslot)
  where
    RequireTimeExpire mslot = mkTimeExpire mslot

pattern RequireTimeStart :: AllegraEraScript era => SlotNo -> NativeScript era
pattern RequireTimeStart mslot <- (getTimeStart -> Just mslot)
  where
    RequireTimeStart mslot = mkTimeStart mslot

{-# COMPLETE
  RequireSignature
  , RequireAllOf
  , RequireAnyOf
  , RequireMOf
  , RequireTimeExpire
  , RequireTimeStart
  #-}

mkRequireSignatureTimelock :: forall era. Era era => KeyHash 'Witness -> Timelock era
mkRequireSignatureTimelock = mkMemoizedEra @era . TimelockSignature

getRequireSignatureTimelock :: Timelock era -> Maybe (KeyHash 'Witness)
getRequireSignatureTimelock (MkTimelock (Memo (TimelockSignature kh) _)) = Just kh
getRequireSignatureTimelock _ = Nothing

mkRequireAllOfTimelock :: forall era. Era era => StrictSeq (Timelock era) -> Timelock era
mkRequireAllOfTimelock = mkMemoizedEra @era . TimelockAllOf

getRequireAllOfTimelock :: Timelock era -> Maybe (StrictSeq (Timelock era))
getRequireAllOfTimelock (MkTimelock (Memo (TimelockAllOf ms) _)) = Just ms
getRequireAllOfTimelock _ = Nothing

mkRequireAnyOfTimelock :: forall era. Era era => StrictSeq (Timelock era) -> Timelock era
mkRequireAnyOfTimelock = mkMemoizedEra @era . TimelockAnyOf

getRequireAnyOfTimelock :: Timelock era -> Maybe (StrictSeq (Timelock era))
getRequireAnyOfTimelock (MkTimelock (Memo (TimelockAnyOf ms) _)) = Just ms
getRequireAnyOfTimelock _ = Nothing

mkRequireMOfTimelock :: forall era. Era era => Int -> StrictSeq (Timelock era) -> Timelock era
mkRequireMOfTimelock n = mkMemoizedEra @era . TimelockMOf n

getRequireMOfTimelock :: Timelock era -> Maybe (Int, StrictSeq (Timelock era))
getRequireMOfTimelock (MkTimelock (Memo (TimelockMOf n ms) _)) = Just (n, ms)
getRequireMOfTimelock _ = Nothing

mkTimeStartTimelock :: forall era. Era era => SlotNo -> Timelock era
mkTimeStartTimelock = mkMemoizedEra @era . TimelockTimeStart

getTimeStartTimelock :: Timelock era -> Maybe SlotNo
getTimeStartTimelock (MkTimelock (Memo (TimelockTimeStart mslot) _)) = Just mslot
getTimeStartTimelock _ = Nothing

mkTimeExpireTimelock :: forall era. Era era => SlotNo -> Timelock era
mkTimeExpireTimelock = mkMemoizedEra @era . TimelockTimeExpire

getTimeExpireTimelock :: Timelock era -> Maybe SlotNo
getTimeExpireTimelock (MkTimelock (Memo (TimelockTimeExpire mslot) _)) = Just mslot
getTimeExpireTimelock _ = Nothing

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
  AllegraEraScript era =>
  Set.Set (KeyHash 'Witness) ->
  ValidityInterval ->
  NativeScript era ->
  Bool
evalTimelock vhks (ValidityInterval txStart txExp) = go
  where
    -- The important part of this validator is that it will stop as soon as it reaches the
    -- required number of valid scripts
    isValidMOf n SSeq.Empty = n <= 0
    isValidMOf n (ts SSeq.:<| tss) =
      n <= 0 || if go ts then isValidMOf (n - 1) tss else isValidMOf n tss
    go = \case
      RequireTimeStart lockStart -> lockStart `lteNegInfty` txStart
      RequireTimeExpire lockExp -> txExp `ltePosInfty` lockExp
      RequireSignature hash -> hash `Set.member` vhks
      RequireAllOf xs -> all go xs
      RequireAnyOf xs -> any go xs
      RequireMOf m xs -> isValidMOf m xs

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

showTimelock :: AllegraEraScript era => NativeScript era -> String
showTimelock (RequireTimeStart (SlotNo i)) = "(Start >= " ++ show i ++ ")"
showTimelock (RequireTimeExpire (SlotNo i)) = "(Expire < " ++ show i ++ ")"
showTimelock (RequireAllOf xs) = "(AllOf " ++ F.foldl' accum ")" xs
  where
    accum ans x = showTimelock x ++ " " ++ ans
showTimelock (RequireAnyOf xs) = "(AnyOf " ++ F.foldl' accum ")" xs
  where
    accum ans x = showTimelock x ++ " " ++ ans
showTimelock (RequireMOf m xs) = "(MOf " ++ show m ++ " " ++ F.foldl' accum ")" xs
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
    go (TimelockSignature kh1) (TimelockSignature kh2) = kh1 == kh2
    go (TimelockAllOf ts1) (TimelockAllOf ts2) = seqEq ts1 ts2
    go (TimelockAnyOf ts1) (TimelockAnyOf ts2) = seqEq ts1 ts2
    go (TimelockMOf n1 ts1) (TimelockMOf n2 ts2) = n1 == n2 && seqEq ts1 ts2
    go (TimelockTimeStart sn1) (TimelockTimeStart sn2) = sn1 == sn2
    go (TimelockTimeExpire sn1) (TimelockTimeExpire sn2) = sn1 == sn2
    go _ _ = False
