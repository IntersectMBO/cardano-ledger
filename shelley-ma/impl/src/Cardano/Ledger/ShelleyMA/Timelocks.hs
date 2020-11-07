{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternGuards #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Cardano.Ledger.ShelleyMA.Timelocks
  ( Timelock (RequireSignature, RequireAllOf, RequireAnyOf, RequireMOf, RequireTimeExpire, RequireTimeStart),
    pattern Timelock,
    inInterval,
    hashTimelockScript,
    showTimelock,
    validateTimelock,
    ValidityInterval (..),
    encodeVI,
    decodeVI,
    translate,
  )
where

import Cardano.Binary
  ( Annotator (..),
    FromCBOR (fromCBOR),
    FullByteString (Full),
    ToCBOR (toCBOR),
    serialize',
  )
import qualified Cardano.Crypto.Hash as Hash
import qualified Cardano.Ledger.Core as Core
import Cardano.Ledger.Era
import qualified Cardano.Ledger.Shelley as Shelley
import Cardano.Slotting.Slot (SlotNo (..))
import Codec.CBOR.Read (deserialiseFromBytes)
import Control.DeepSeq (NFData (..))
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as Lazy
import Data.ByteString.Short (fromShort)
import Data.Coders
  ( Decode (..),
    Density (..),
    Encode (..),
    Wrapped (..),
    decode,
    decodeNullMaybe,
    encode,
    encodeNullMaybe,
    (!>),
    (<!),
    (<*!),
  )
import Data.MemoBytes
  ( Mem,
    MemoBytes (..),
    memoBytes,
  )
import Data.Sequence.Strict (StrictSeq)
import Data.Set (Set, member)
import qualified Data.Set as Set
import Data.Typeable
import GHC.Generics (Generic)
import GHC.Records
import NoThunks.Class (NoThunks (..))
import Shelley.Spec.Ledger.BaseTypes (StrictMaybe (SJust, SNothing), maybeToStrictMaybe, strictMaybeToMaybe)
import Shelley.Spec.Ledger.Keys (KeyHash (..), KeyRole (Witness))
import Shelley.Spec.Ledger.Scripts (MultiSig, ScriptHash (..), getMultiSigBytes)
import Shelley.Spec.Ledger.Serialization
  ( decodeStrictSeq,
    encodeFoldable,
  )
import Shelley.Spec.Ledger.Tx
  ( Tx (..),
    WitnessSetHKD (..),
    addrWits',
  )
import Shelley.Spec.Ledger.TxBody
  ( witKeyHash,
  )

-- =================================================================
-- We translate a MultiSig by deserializing its bytes as a Timelock
-- If this succeeds (and it should, we designed Timelock to have
-- that property), then both version should have the same bytes,
-- because we are using FromCBOR(Annotator Timelock) instance.

translate :: Era e1 => MultiSig (PreviousEra e1) -> Timelock e1
translate multi =
  let bytes = Lazy.fromStrict (fromShort (getMultiSigBytes multi))
   in case deserialiseFromBytes fromCBOR bytes of
        Left err -> error ("Translating MultiSig script to Timelock script fails\n" ++ show err)
        Right (left, Annotator f) | left == Lazy.empty -> f (Full bytes)
        Right (left, _) -> error ("Translating MultiSig script to Timelock script does not consume all they bytes: " ++ show left)

-- ================================================================
-- An pair of optional SlotNo.

-- | ValidityInterval is a half open interval. Closed on the bottom, Open on the top.
--   A SNothing on the bottom is negative infinity, and a SNothing on the top is positive infinity
data ValidityInterval = ValidityInterval
  { validFrom :: !(StrictMaybe SlotNo),
    validTo :: !(StrictMaybe SlotNo)
  }
  deriving (Ord, Eq, Generic, Show, NoThunks, NFData)

encodeVI :: ValidityInterval -> Encode ( 'Closed 'Dense) ValidityInterval
encodeVI (ValidityInterval f t) = Rec ValidityInterval !> To f !> To t

instance ToCBOR ValidityInterval where
  toCBOR vi = encode (encodeVI vi)

decodeVI :: Decode ( 'Closed 'Dense) ValidityInterval
decodeVI = RecD ValidityInterval <! From <! From

instance FromCBOR ValidityInterval where
  fromCBOR = decode decodeVI

-- ==================================================================

data TimelockRaw era
  = Signature !(KeyHash 'Witness (Crypto era))
  | AllOf !(StrictSeq (Timelock era)) -- NOTE that Timelock and
  | AnyOf !(StrictSeq (Timelock era)) -- TimelockRaw are mutually recursive.
  | MOfN !Int !(StrictSeq (Timelock era)) -- Note that the Int may be negative in which case (MOfN -2 [..]) is always True
  | TimeStart !(StrictMaybe SlotNo) -- The start time
  | TimeExpire !(StrictMaybe SlotNo) -- The time it expires
  deriving (Eq, Show, Ord, Generic)

deriving instance Typeable era => NoThunks (TimelockRaw era)

-- These coding choices are chosen so that a MultiSig script
-- can be deserialised as a Timelock script

encRaw :: Era era => TimelockRaw era -> Encode 'Open (TimelockRaw era)
encRaw (Signature hash) = Sum Signature 0 !> To hash
encRaw (AllOf xs) = Sum AllOf 1 !> E encodeFoldable xs
encRaw (AnyOf xs) = Sum AnyOf 2 !> E encodeFoldable xs
encRaw (MOfN m xs) = Sum MOfN 3 !> To m !> E encodeFoldable xs
encRaw (TimeStart m) = Sum TimeStart 4 !> E (encodeNullMaybe toCBOR . strictMaybeToMaybe) m
encRaw (TimeExpire m) = Sum TimeExpire 5 !> E (encodeNullMaybe toCBOR . strictMaybeToMaybe) m

decRaw :: Era era => Word -> Decode 'Open (Annotator (TimelockRaw era))
decRaw 0 = Ann (SumD Signature <! From)
decRaw 1 = Ann (SumD AllOf) <*! D (sequence <$> decodeStrictSeq fromCBOR)
decRaw 2 = Ann (SumD AnyOf) <*! D (sequence <$> decodeStrictSeq fromCBOR)
decRaw 3 = Ann (SumD MOfN) <*! Ann From <*! D (sequence <$> decodeStrictSeq fromCBOR)
decRaw 4 = Ann (SumD TimeStart <! D (maybeToStrictMaybe <$> decodeNullMaybe fromCBOR))
decRaw 5 = Ann (SumD TimeExpire <! D (maybeToStrictMaybe <$> decodeNullMaybe fromCBOR))
decRaw n = Invalid n

-- This instance allows us to derive instance FromCBOR(Annotator (Timelock era)).
-- Since Timelock is a newtype around (Memo (Timelock era)).

instance Era era => FromCBOR (Annotator (TimelockRaw era)) where
  fromCBOR = decode (Summands "TimelockRaw" decRaw)

-- =================================================================
-- Native Scripts are Memoized TimelockRaw.
-- The patterns give the appearence that the mutual recursion is not present.
-- They rely on memoBytes, and TimelockRaw to memoize each constructor of Timelock
-- =================================================================

newtype Timelock era = Timelock (MemoBytes (TimelockRaw era))
  deriving (Eq, Ord, Show, Generic)
  deriving newtype (ToCBOR, NoThunks)

deriving via
  (Mem (TimelockRaw era))
  instance
    (Era era) => FromCBOR (Annotator (Timelock era))

pattern RequireSignature :: Era era => KeyHash 'Witness (Crypto era) -> Timelock era
pattern RequireSignature akh <-
  Timelock (Memo (Signature akh) _)
  where
    RequireSignature akh =
      Timelock $ memoBytes (encRaw (Signature akh))

pattern RequireAllOf :: Era era => StrictSeq (Timelock era) -> Timelock era
pattern RequireAllOf ms <-
  Timelock (Memo (AllOf ms) _)
  where
    RequireAllOf ms =
      Timelock $ memoBytes (encRaw (AllOf ms))

pattern RequireAnyOf :: Era era => StrictSeq (Timelock era) -> Timelock era
pattern RequireAnyOf ms <-
  Timelock (Memo (AnyOf ms) _)
  where
    RequireAnyOf ms =
      Timelock $ memoBytes (encRaw (AnyOf ms))

pattern RequireMOf :: Era era => Int -> StrictSeq (Timelock era) -> Timelock era
pattern RequireMOf n ms <-
  Timelock (Memo (MOfN n ms) _)
  where
    RequireMOf n ms =
      Timelock $ memoBytes (encRaw (MOfN n ms))

pattern RequireTimeExpire :: Era era => StrictMaybe (SlotNo) -> Timelock era
pattern RequireTimeExpire mslot <-
  Timelock (Memo (TimeExpire mslot) _)
  where
    RequireTimeExpire mslot =
      Timelock $ memoBytes (encRaw (TimeExpire mslot))

pattern RequireTimeStart :: Era era => StrictMaybe (SlotNo) -> Timelock era
pattern RequireTimeStart mslot <-
  Timelock (Memo (TimeStart mslot) _)
  where
    RequireTimeStart mslot =
      Timelock $ memoBytes (encRaw (TimeStart mslot))

{-# COMPLETE RequireSignature, RequireAllOf, RequireAnyOf, RequireMOf, RequireTimeExpire, RequireTimeStart #-}

-- =================================================================
-- Evaluating and validating a Timelock

-- PLEASE SOMEONE VERIFY I AM USING atOrAfter and strictlyBefore RIGHT

atOrAfter :: StrictMaybe SlotNo -> StrictMaybe SlotNo -> Bool
atOrAfter SNothing SNothing = True
atOrAfter SNothing (SJust _) = True
atOrAfter (SJust _) SNothing = False
atOrAfter (SJust i) (SJust j) = i <= j

strictlyBefore :: StrictMaybe SlotNo -> StrictMaybe SlotNo -> Bool
strictlyBefore SNothing SNothing = True
strictlyBefore SNothing (SJust _) = False
strictlyBefore (SJust _) SNothing = True
strictlyBefore (SJust i) (SJust j) = i < j

evalTimelock ::
  Era era =>
  Set (KeyHash 'Witness (Crypto era)) ->
  ValidityInterval ->
  Timelock era ->
  Bool
evalTimelock _vhks (ValidityInterval mstart _) (RequireTimeStart mslot) =
  atOrAfter mstart mslot
evalTimelock _vhks (ValidityInterval _ mexpire) (RequireTimeExpire mslot) =
  strictlyBefore mslot mexpire
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

-- =======================================================
-- Validating timelock scripts
-- We Assume that TxBody has field "vldt" that extracts a ValidityInterval
-- We still need to correctly compute the witness set for Core.TxBody as well.

evalFPS ::
  forall era.
  ( Era era,
    HasField "vldt" (Core.TxBody era) ValidityInterval
  ) =>
  Timelock era ->
  Set (KeyHash 'Witness (Crypto era)) ->
  Core.TxBody era ->
  Bool
evalFPS timelock vhks txb = evalTimelock vhks (getField @"vldt" txb) timelock

validateTimelock ::
  (Shelley.TxBodyConstraints era, HasField "vldt" (Core.TxBody era) ValidityInterval) =>
  Timelock era ->
  Tx era ->
  Bool
validateTimelock lock tx = evalFPS lock vhks (_body tx)
  where
    -- THIS IS JUST A STUB. WHO KNOWS IF
    witsSet = _witnessSet tx -- IT COMPUTES THE RIGHT WITNESS SET.
    vhks = Set.map witKeyHash (addrWits' witsSet)

-- | Magic number representing the tag of the native Timelock script
-- language. For each script language included, a new tag is chosen and the tag
-- is included in the script hash for a script.
nativeTimelockTag :: BS.ByteString
nativeTimelockTag = "\01"

-- | Hashes native timelock script.
hashTimelockScript ::
  Era era =>
  Timelock era ->
  ScriptHash era
hashTimelockScript =
  ScriptHash
    . Hash.castHash
    . Hash.hashWith (\x -> nativeTimelockTag <> serialize' x)

showTimelock :: Era era => Timelock era -> String
showTimelock (RequireTimeStart SNothing) = "(Start -inf)"
showTimelock (RequireTimeStart (SJust (SlotNo i))) = "(Start >= " ++ show i ++ ")"
showTimelock (RequireTimeExpire SNothing) = "(Expire +inf)"
showTimelock (RequireTimeExpire (SJust (SlotNo i))) = "(Expire < " ++ show i ++ ")"
showTimelock (RequireAllOf xs) = "(AllOf " ++ foldl accum ")" xs where accum ans x = showTimelock x ++ " " ++ ans
showTimelock (RequireAnyOf xs) = "(AnyOf " ++ foldl accum ")" xs where accum ans x = showTimelock x ++ " " ++ ans
showTimelock (RequireMOf m xs) = "(MOf " ++ show m ++ " " ++ foldl accum ")" xs where accum ans x = showTimelock x ++ " " ++ ans
showTimelock (RequireSignature hash) = "(Signature " ++ show hash ++ ")"

-- ===============================================================
