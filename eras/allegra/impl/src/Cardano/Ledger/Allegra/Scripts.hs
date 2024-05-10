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
{-# LANGUAGE TypeOperators #-}
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
  Timelock,
  pattern RequireTimeExpire,
  pattern RequireTimeStart,
  TimelockRaw,
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
  (<*!),
 )
import Cardano.Ledger.Core
import Cardano.Ledger.Crypto (Crypto, HASH, StandardCrypto)
import Cardano.Ledger.Keys (KeyHash (..), KeyRole (Witness))
import Cardano.Ledger.MemoBytes (
  EqRaw (..),
  Mem,
  MemoBytes (Memo),
  Memoized (..),
  getMemoRawType,
  mkMemoBytes,
  mkMemoized,
 )
import Cardano.Ledger.SafeHash (SafeToHash)
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
  = Signature !(KeyHash 'Witness (EraCrypto era))
  | AllOf !(StrictSeq (Timelock era)) -- NOTE that Timelock and
  | AnyOf !(StrictSeq (Timelock era)) -- TimelockRaw are mutually recursive.
  | MOfN !Int !(StrictSeq (Timelock era))
  | -- Note that the Int may be negative in which case (MOfN (-2) [..]) is always True
    TimeStart !SlotNo -- The start time
  | TimeExpire !SlotNo -- The time it expires
  deriving (Eq, Generic, NFData)

class ShelleyEraScript era => AllegraEraScript era where
  mkTimeStart :: SlotNo -> NativeScript era
  getTimeStart :: NativeScript era -> Maybe SlotNo

  mkTimeExpire :: SlotNo -> NativeScript era
  getTimeExpire :: NativeScript era -> Maybe SlotNo

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

instance Era era => EncCBOR (TimelockRaw era) where
  encCBOR =
    encode . \case
      Signature hash -> Sum Signature 0 !> To hash
      AllOf xs -> Sum AllOf 1 !> To xs
      AnyOf xs -> Sum AnyOf 2 !> To xs
      MOfN m xs -> Sum MOfN 3 !> To m !> To xs
      TimeStart m -> Sum TimeStart 4 !> To m
      TimeExpire m -> Sum TimeExpire 5 !> To m

-- This instance allows us to derive instance DecCBOR (Annotator (Timelock crypto)).
-- Since Timelock is a newtype around (Memo (Timelock crypto)).

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

-- =================================================================
-- Native Scripts are Memoized TimelockRaw.
-- The patterns give the appearence that the mutual recursion is not present.
-- They rely on memoBytes, and TimelockRaw to memoize each constructor of Timelock
-- =================================================================

newtype Timelock era = TimelockConstr (MemoBytes TimelockRaw era)
  deriving (Eq, Generic)
  deriving newtype (ToCBOR, NoThunks, NFData, SafeToHash)

instance Era era => EncCBOR (Timelock era)

instance Memoized Timelock where
  type RawType Timelock = TimelockRaw

deriving instance HashAlgorithm (HASH (EraCrypto era)) => Show (Timelock era)

instance EqRaw (Timelock era) where
  eqRaw = eqTimelockRaw

deriving via
  Mem TimelockRaw era
  instance
    Era era => DecCBOR (Annotator (Timelock era))

-- | Since Timelock scripts are a strictly backwards compatible extension of
-- MultiSig scripts, we can use the same 'scriptPrefixTag' tag here as we did
-- for the ValidateScript instance in MultiSig
instance Crypto c => EraScript (AllegraEra c) where
  type Script (AllegraEra c) = Timelock (AllegraEra c)
  type NativeScript (AllegraEra c) = Timelock (AllegraEra c)

  upgradeScript = \case
    RequireSignature keyHash -> RequireSignature keyHash
    RequireAllOf sigs -> RequireAllOf $ upgradeScript <$> sigs
    RequireAnyOf sigs -> RequireAnyOf $ upgradeScript <$> sigs
    RequireMOf n sigs -> RequireMOf n $ upgradeScript <$> sigs
    _ -> error "Impossible: All NativeScripts should have been accounted for"

  scriptPrefixTag _script = nativeMultiSigTag -- "\x00"

  getNativeScript = Just

  fromNativeScript = id

instance Crypto c => ShelleyEraScript (AllegraEra c) where
  {-# SPECIALIZE instance ShelleyEraScript (AllegraEra StandardCrypto) #-}

  mkRequireSignature = mkRequireSignatureTimelock
  getRequireSignature = getRequireSignatureTimelock

  mkRequireAllOf = mkRequireAllOfTimelock
  getRequireAllOf = getRequireAllOfTimelock

  mkRequireAnyOf = mkRequireAnyOfTimelock
  getRequireAnyOf = getRequireAnyOfTimelock

  mkRequireMOf = mkRequireMOfTimelock
  getRequireMOf = getRequireMOfTimelock

instance Crypto c => AllegraEraScript (AllegraEra c) where
  {-# SPECIALIZE instance AllegraEraScript (AllegraEra StandardCrypto) #-}

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

mkRequireSignatureTimelock :: Era era => KeyHash 'Witness (EraCrypto era) -> Timelock era
mkRequireSignatureTimelock = mkMemoized . Signature
getRequireSignatureTimelock :: Era era => Timelock era -> Maybe (KeyHash 'Witness (EraCrypto era))
getRequireSignatureTimelock (TimelockConstr (Memo (Signature kh) _)) = Just kh
getRequireSignatureTimelock _ = Nothing

mkRequireAllOfTimelock :: Era era => StrictSeq (Timelock era) -> Timelock era
mkRequireAllOfTimelock = mkMemoized . AllOf
getRequireAllOfTimelock :: Era era => Timelock era -> Maybe (StrictSeq (Timelock era))
getRequireAllOfTimelock (TimelockConstr (Memo (AllOf ms) _)) = Just ms
getRequireAllOfTimelock _ = Nothing

mkRequireAnyOfTimelock :: Era era => StrictSeq (Timelock era) -> Timelock era
mkRequireAnyOfTimelock = mkMemoized . AnyOf
getRequireAnyOfTimelock :: Era era => Timelock era -> Maybe (StrictSeq (Timelock era))
getRequireAnyOfTimelock (TimelockConstr (Memo (AnyOf ms) _)) = Just ms
getRequireAnyOfTimelock _ = Nothing

mkRequireMOfTimelock :: Era era => Int -> StrictSeq (Timelock era) -> Timelock era
mkRequireMOfTimelock n = mkMemoized . MOfN n
getRequireMOfTimelock :: Era era => Timelock era -> Maybe (Int, (StrictSeq (Timelock era)))
getRequireMOfTimelock (TimelockConstr (Memo (MOfN n ms) _)) = Just (n, ms)
getRequireMOfTimelock _ = Nothing

mkTimeStartTimelock :: Era era => SlotNo -> Timelock era
mkTimeStartTimelock = mkMemoized . TimeStart
getTimeStartTimelock :: Era era => Timelock era -> Maybe SlotNo
getTimeStartTimelock (TimelockConstr (Memo (TimeStart mslot) _)) = Just mslot
getTimeStartTimelock _ = Nothing

mkTimeExpireTimelock :: Era era => SlotNo -> Timelock era
mkTimeExpireTimelock = mkMemoized . TimeExpire
getTimeExpireTimelock :: Era era => Timelock era -> Maybe SlotNo
getTimeExpireTimelock (TimelockConstr (Memo (TimeExpire mslot) _)) = Just mslot
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
  Set.Set (KeyHash 'Witness (EraCrypto era)) ->
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
