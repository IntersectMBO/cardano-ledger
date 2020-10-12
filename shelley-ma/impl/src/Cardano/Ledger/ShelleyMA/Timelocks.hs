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
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE UndecidableInstances #-}

module Cardano.Ledger.ShelleyMA.Timelocks
  ( Timelock (Interval, Multi, TimelockAnd, TimelockOr),
    ininterval,
    hashTimelockScript,
  )
where

import Cardano.Binary
  ( Annotator (..),
    FromCBOR (fromCBOR),
    ToCBOR,
    serialize',
  )
import qualified Cardano.Crypto.Hash as Hash
import qualified Cardano.Ledger.Core as Core
import Cardano.Ledger.Era
import qualified Cardano.Ledger.Shelley as Shelley
import Cardano.Slotting.Slot (SlotNo (..))
import qualified Data.ByteString as BS
import Data.Sequence.Strict (StrictSeq)
import Data.Set (Set)
import qualified Data.Set as Set
import GHC.Generics (Generic)
import NoThunks.Class (NoThunks (..))
import Shelley.Spec.Ledger.BaseTypes (StrictMaybe (SJust, SNothing), invalidKey)
import Shelley.Spec.Ledger.Keys (KeyHash (..), KeyRole (Witness))
import Shelley.Spec.Ledger.MemoBytes
  ( Mem,
    MemoBytes (..),
    Symbolic (..),
    memoBytes,
    (<#>),
    (<@>),
  )
import Shelley.Spec.Ledger.Scripts
  ( MultiSig,
    ScriptHash (..),
  )
import Shelley.Spec.Ledger.Serialization
  ( decodeRecordSum,
    decodeStrictSeq,
    encodeFoldable,
  )
import Shelley.Spec.Ledger.Tx
  ( MultiSignatureScript (..),
    Tx (..),
    WitnessSetHKD (..),
    addrWits',
    evalNativeMultiSigScript,
  )
import Shelley.Spec.Ledger.TxBody
  ( witKeyHash,
  )

-- ================================================================
-- Timelock' and Timelock are mutually recursive. Timelock adds a
-- convenient place to hang the memoized serialized bytes by
-- being a newtype over MemoBytes. We need to supply two things.
-- 1) A FromCBOR (Annotator (Timelock' era))  instance
-- 2) Serializer code (replaces ToCBOR instances) in the Patterns for Timelock

data Timelock' era
  = Interval' !(StrictMaybe SlotNo) !(StrictMaybe SlotNo)
  | Multi' !(MultiSig era)
  | TimelockAnd' !(StrictSeq (Timelock era)) -- Note the hidden recursion of Timelock', through Timelock.
  | TimelockOr' !(StrictSeq (Timelock era))
  deriving (Show, Eq, Ord, Generic, NoThunks)

instance
  Era era =>
  FromCBOR (Annotator (Timelock' era))
  where
  fromCBOR = decodeRecordSum "Timelock" $
    \case
      0 -> do
        left <- fromCBOR -- this: fromCBOR :: (Decoder s (StrictMaybe SlotNo))
        right <- fromCBOR
        pure $ (2, pure (Interval' left right)) -- Note the pure lifts from T to (Annotator T)
        -- Possible because intervalpair has no memoized
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
  StrictMaybe SlotNo ->
  StrictMaybe SlotNo ->
  Timelock era
pattern Interval left right <-
  Timelock (Memo (Interval' left right) _)
  where
    Interval left right =
      Timelock (memoBytes (Con Interval' 0 <@> left <@> right))

pattern Multi :: Era era => MultiSig era -> Timelock era
pattern Multi m <-
  Timelock (Memo (Multi' m) _)
  where
    Multi m = Timelock (memoBytes (Con Multi' 1 <@> m))

pattern TimelockAnd :: Era era => StrictSeq (Timelock era) -> Timelock era
pattern TimelockAnd ms <-
  Timelock (Memo (TimelockAnd' ms) _)
  where
    TimelockAnd ms =
      Timelock
        ( memoBytes
            ( Con TimelockAnd' 2
                <#> (encodeFoldable, ms)
            )
        )

pattern TimelockOr :: Era era => StrictSeq (Timelock era) -> Timelock era
pattern TimelockOr ms <-
  Timelock (Memo (TimelockOr' ms) _)
  where
    TimelockOr ms =
      Timelock
        ( memoBytes
            ( Con TimelockOr' 3
                <#> (encodeFoldable, ms)
            )
        )

{-# COMPLETE Interval, Multi, TimelockAnd, TimelockOr #-}

-- =========================================================
-- Operations on Timelock scripts

ininterval :: SlotNo -> (StrictMaybe SlotNo, StrictMaybe SlotNo) -> Bool
ininterval _slot (SNothing, SNothing) = True
ininterval slot (SNothing, SJust i_f) = slot <= i_f
ininterval slot (SJust i_s, SNothing) = i_s <= slot
ininterval slot (SJust i_s, SJust i_f) = i_s <= slot && slot <= i_f

-- =======================================================
-- Validating timelock scripts
-- The following functions are stubs, since (TxBody era) does not currently
-- contain a validity interval. At somepoint txvld :: TxBody era -> ValidityInterval
-- will provide this operation. We will also need to correctly compute the witness
-- set for this new (TxBody era) as well.

type ValidityInterval = (StrictMaybe SlotNo, StrictMaybe SlotNo)

txvld :: Core.TxBody era -> ValidityInterval
txvld _ = (SNothing, SNothing)

evalFPS ::
  forall era.
  Era era =>
  Timelock era ->
  Set (KeyHash 'Witness era) ->
  Core.TxBody era ->
  Bool
evalFPS (TimelockAnd locks) vhks txb = all (\lock -> evalFPS lock vhks txb) locks
evalFPS (TimelockOr locks) vhks txb = any (\lock -> evalFPS lock vhks txb) locks
evalFPS (Multi msig) vhks _tx = evalNativeMultiSigScript msig vhks
evalFPS (Interval timeS timeF) _vhks txb =
  let (bodyS, bodyF) = txvld @era txb -- THIS IS A STUB
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
  Shelley.TxBodyConstraints era => Timelock era -> Tx era -> Bool
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

instance
  (Era era, Shelley.TxBodyConstraints era) =>
  MultiSignatureScript (Timelock era) era
  where
  validateScript = validateTimelock
  hashScript = hashTimelockScript
