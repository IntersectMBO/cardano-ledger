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
  ( Timelock (Interval, Multi, TimelockAnd, TimelockOr),
    inInterval,
    hashTimelockScript,
    showTimelock,
    validateTimelock,
    ValidityInterval (..),
    encodeVI,
    decodeVI,
  )
where

import Cardano.Binary
  ( Annotator (..),
    FromCBOR (fromCBOR),
    ToCBOR (toCBOR),
    serialize',
  )
import qualified Cardano.Crypto.Hash as Hash
import qualified Cardano.Ledger.Core as Core
import Cardano.Ledger.Era
import qualified Cardano.Ledger.Shelley as Shelley
import Cardano.Slotting.Slot (SlotNo (..))
import qualified Data.ByteString as BS
import Data.Coders (Decode (..), Density (..), Encode (..), Wrapped (..), decode, encode, (!>), (<!), (<*!))
import Data.MemoBytes
  ( Mem,
    MemoBytes (..),
    memoBytes,
  )
import Data.Sequence.Strict (StrictSeq)
import Data.Set (Set)
import qualified Data.Set as Set
import GHC.Generics (Generic)
import GHC.Records
import NoThunks.Class (NoThunks (..))
import Shelley.Spec.Ledger.BaseTypes (StrictMaybe (SJust, SNothing))
import Shelley.Spec.Ledger.Keys (KeyHash (..), KeyRole (Witness))
import Shelley.Spec.Ledger.Scripts
  ( MultiSig,
    ScriptHash (..),
  )
import Shelley.Spec.Ledger.Serialization
  ( decodeStrictSeq,
    encodeFoldable,
  )
import Shelley.Spec.Ledger.Tx
  ( Tx (..),
    WitnessSetHKD (..),
    addrWits',
    evalNativeMultiSigScript,
  )
import Shelley.Spec.Ledger.TxBody
  ( witKeyHash,
  )

-- ================================================================
-- An pair of optional SlotNo.

data ValidityInterval = ValidityInterval
  { validFrom :: !(StrictMaybe SlotNo),
    validTo :: !(StrictMaybe SlotNo)
  }
  deriving (Ord, Eq, Generic, Show, NoThunks)

encodeVI :: ValidityInterval -> Encode ( 'Closed 'Dense) ValidityInterval
encodeVI (ValidityInterval f t) = Rec ValidityInterval !> To f !> To t

instance ToCBOR ValidityInterval where
  toCBOR vi = encode (encodeVI vi)

decodeVI :: Decode ( 'Closed 'Dense) ValidityInterval
decodeVI = RecD ValidityInterval <! From <! From

instance FromCBOR ValidityInterval where
  fromCBOR = decode decodeVI

-- ================================================================
-- Timelock' and Timelock are mutually recursive. Timelock adds a
-- convenient place to hang the memoized serialized bytes by
-- being a newtype over MemoBytes. We need to supply two things.
-- 1) A FromCBOR (Annotator (Timelock' era))  instance
-- 2) Serializer code (replaces ToCBOR instances) in the Patterns for Timelock

data Timelock' era
  = Interval' !ValidityInterval
  | Multi' !(MultiSig era)
  | TimelockAnd' !(StrictSeq (Timelock era)) -- Note the hidden recursion of Timelock', through Timelock.
  | TimelockOr' !(StrictSeq (Timelock era))
  deriving (Show, Eq, Ord, Generic, NoThunks)

decTimelock' :: Era era => Word -> Decode 'Open (Annotator (Timelock' era))
decTimelock' 0 = Ann (SumD Interval' <! From)
decTimelock' 1 = Ann (SumD Multi') <*! From
decTimelock' 2 = Ann (SumD TimelockAnd') <*! D (sequence <$> decodeStrictSeq fromCBOR)
decTimelock' 3 = Ann (SumD TimelockOr') <*! D (sequence <$> decodeStrictSeq fromCBOR)
decTimelock' k = Invalid k

instance Era era => FromCBOR (Annotator (Timelock' era)) where
  fromCBOR = decode (Summands "Annotator(Timelock' era)" decTimelock')

{- The decTimelok' function replaces things like this:
instance
  Era era =>
  FromCBOR (Annotator (Timelock' era))
  where
  fromCBOR = decodeRecordSum "Timelock" $
    \case
      0 -> do
        interval <- fromCBOR -- this: fromCBOR :: (Decoder s ValidityInterval)
        pure $ (2, pure (Interval' interval)) -- Note the pure lifts from T to (Annotator T)
        -- Possible because ValidityInterval has no memoized
        -- structures that remember their own bytes.
      1 -> do
        multi <- fromCBOR
        pure $ (2, Multi' <$> multi)
      2 -> do
        timelks <- sequence <$> decodeStrictSeq fromCBOR
        pure (2, TimelockAnd' <$> timelks)
      3 -> do
        timelks <- sequence <$> decodeStrictSeq fromCBOR
        pure (2, TimelockOr' <$> timelks)
      k -> invalidKey k
-}

-- ==============================================================================
-- Now all the problematic Timelock instances are derived. No thinking required

newtype Timelock era = Timelock (MemoBytes (Timelock' era))
  deriving (Eq, Ord, Show, Generic)
  deriving newtype (ToCBOR, NoThunks)

deriving via
  (Mem (Timelock' era))
  instance
    (Era era) =>
    FromCBOR (Annotator (Timelock era))

-- ==========================================================================
-- The patterns give the appearence that the mutual recursion is not present.
-- They rely on memoBytes, and (Symbolic Timelock') to memoize each constructor of Timelock'

pattern Interval ::
  Era era =>
  ValidityInterval ->
  Timelock era
pattern Interval valid <-
  Timelock (Memo (Interval' valid) _)
  where
    Interval valid = Timelock $ memoBytes $ Sum Interval' 0 !> To valid

pattern Multi :: Era era => MultiSig era -> Timelock era
pattern Multi m <-
  Timelock (Memo (Multi' m) _)
  where
    Multi m = Timelock $ memoBytes $ (Sum Multi' 1) !> To m

pattern TimelockAnd :: Era era => StrictSeq (Timelock era) -> Timelock era
pattern TimelockAnd ms <-
  Timelock (Memo (TimelockAnd' ms) _)
  where
    TimelockAnd ms =
      Timelock $ memoBytes $ Sum TimelockAnd' 2 !> E encodeFoldable ms

pattern TimelockOr :: Era era => StrictSeq (Timelock era) -> Timelock era
pattern TimelockOr ms <-
  Timelock (Memo (TimelockOr' ms) _)
  where
    TimelockOr ms =
      Timelock $ memoBytes (Sum TimelockOr' 3 !> E encodeFoldable ms)

{-# COMPLETE Interval, Multi, TimelockAnd, TimelockOr #-}

-- =========================================================
-- Operations on Timelock scripts

inInterval :: SlotNo -> ValidityInterval -> Bool
inInterval _slot (ValidityInterval SNothing SNothing) = True
inInterval slot (ValidityInterval SNothing (SJust i_f)) = slot <= i_f
inInterval slot (ValidityInterval (SJust i_s) SNothing) = i_s <= slot
inInterval slot (ValidityInterval (SJust i_s) (SJust i_f)) =
  i_s <= slot && slot <= i_f

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
evalFPS (TimelockAnd locks) vhks txb = all (\lock -> evalFPS lock vhks txb) locks
evalFPS (TimelockOr locks) vhks txb = any (\lock -> evalFPS lock vhks txb) locks
evalFPS (Multi msig) vhks _tx = evalNativeMultiSigScript msig vhks
evalFPS (Interval (ValidityInterval timeS timeF)) _vhks txb =
  let (ValidityInterval bodyS bodyF) = getField @"vldt" txb -- THIS IS A STUB
   in case (timeS, timeF) of
        (SNothing, SNothing) -> True
        (SNothing, SJust i_f) | SJust i'_f <- bodyF -> i'_f <= i_f
        (SJust i_s, SNothing) | SJust i'_s <- bodyS -> i_s <= i'_s
        (SJust i_s, SJust i_f)
          | SJust i'_s <- bodyS,
            SJust i'_f <- bodyF ->
            i_s <= i'_s && i'_f <= i_f
        _ -> False

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

{-
-- At some point we will need a class, analogous to  MultiSignatureScript
-- which relates scripts and their validators.
instance
  ( Era era,
    HasField "vldt" (Core.TxBody era) ValidityInterval,
    Shelley.TxBodyConstraints era
  ) =>
  MultiSignatureScript (Timelock era) era
  where
  validateScript = validateTimelock
  hashScript = hashTimelockScript
-}

showTimelock :: Era era => Timelock era -> String
showTimelock (Interval (ValidityInterval SNothing SNothing)) = "(Interval -inf .. +inf)"
showTimelock (Interval (ValidityInterval (SJust (SlotNo x)) SNothing)) = "(Interval " ++ show x ++ " .. +inf)"
showTimelock (Interval (ValidityInterval SNothing (SJust (SlotNo x)))) = "(Interval -inf .. " ++ show x ++ ")"
showTimelock (Interval (ValidityInterval (SJust (SlotNo y)) (SJust (SlotNo x)))) = "(Interval " ++ show y ++ " .. " ++ show x ++ ")"
showTimelock (TimelockAnd xs) = "(TimelockAnd " ++ foldl accum ")" xs where accum ans x = showTimelock x ++ " " ++ ans
showTimelock (TimelockOr xs) = "(TimelockOr " ++ foldl accum ")" xs where accum ans x = showTimelock x ++ " " ++ ans
showTimelock (Multi x) = show x
