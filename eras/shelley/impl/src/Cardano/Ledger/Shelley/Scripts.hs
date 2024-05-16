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
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE UndecidableSuperClasses #-}
{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Cardano.Ledger.Shelley.Scripts (
  MultiSig,
  ShelleyEraScript (..),
  pattern RequireSignature,
  pattern RequireAllOf,
  pattern RequireAnyOf,
  pattern RequireMOf,
  evalMultiSig,
  validateMultiSig,
  ScriptHash (..),
  nativeMultiSigTag,
  eqMultiSigRaw,
  MultiSigRaw,
)
where

import Cardano.Crypto.Hash.Class (HashAlgorithm)
import Cardano.Ledger.BaseTypes (invalidKey)
import Cardano.Ledger.Binary (
  Annotator (..),
  DecCBOR (decCBOR),
  EncCBOR (..),
  ToCBOR,
  decodeRecordSum,
 )
import Cardano.Ledger.Binary.Coders (Encode (..), (!>))
import Cardano.Ledger.Core
import Cardano.Ledger.Crypto (Crypto, HASH, StandardCrypto)
import Cardano.Ledger.Keys (KeyHash (..), KeyRole (Witness))
import Cardano.Ledger.Keys.WitVKey (witVKeyHash)
import Cardano.Ledger.MemoBytes (
  EqRaw (..),
  Mem,
  MemoBytes,
  Memoized (..),
  getMemoRawType,
  memoBytes,
  pattern Memo,
 )
import Cardano.Ledger.SafeHash (SafeToHash (..))
import Cardano.Ledger.Shelley.Era
import Control.DeepSeq (NFData)
import qualified Data.ByteString as BS
import Data.Sequence.Strict (StrictSeq (..))
import qualified Data.Sequence.Strict as StrictSeq
import qualified Data.Set as Set
import GHC.Generics (Generic)
import Lens.Micro
import NoThunks.Class (NoThunks (..))

-- | A simple language for expressing conditions under which it is valid to
-- withdraw from a normal UTxO payment address or to use a stake address.
--
-- The use case is for expressing multi-signature payment addresses and
-- multi-signature stake addresses. These can be combined arbitrarily using
-- logical operations:
--
-- * multi-way \"and\";
-- * multi-way \"or\";
-- * multi-way \"N of M\".
--
-- This makes it easy to express multi-signature addresses, and provides an
-- extension point to express other validity conditions, e.g., as needed for
-- locking funds used with lightning.
data MultiSigRaw era
  = -- | Require the redeeming transaction be witnessed by the spending key
    --   corresponding to the given verification key hash.
    RequireSignature' !(KeyHash 'Witness (EraCrypto era))
  | -- | Require all the sub-terms to be satisfied.
    RequireAllOf' !(StrictSeq (MultiSig era))
  | -- | Require any one of the sub-terms to be satisfied.
    RequireAnyOf' !(StrictSeq (MultiSig era))
  | -- | Require M of the given sub-terms to be satisfied.
    RequireMOf' !Int !(StrictSeq (MultiSig era))
  deriving (Eq, Generic)
  deriving anyclass (NoThunks)

class EraScript era => ShelleyEraScript era where
  mkRequireSignature :: KeyHash 'Witness (EraCrypto era) -> NativeScript era
  getRequireSignature :: NativeScript era -> Maybe (KeyHash 'Witness (EraCrypto era))

  mkRequireAllOf :: StrictSeq (NativeScript era) -> NativeScript era
  getRequireAllOf :: NativeScript era -> Maybe (StrictSeq (NativeScript era))

  mkRequireAnyOf :: StrictSeq (NativeScript era) -> NativeScript era
  getRequireAnyOf :: NativeScript era -> Maybe (StrictSeq (NativeScript era))

  mkRequireMOf :: Int -> StrictSeq (NativeScript era) -> NativeScript era
  getRequireMOf :: NativeScript era -> Maybe (Int, StrictSeq (NativeScript era))

deriving instance HashAlgorithm (HASH (EraCrypto era)) => Show (MultiSigRaw era)

instance NFData (MultiSigRaw era)

newtype MultiSig era = MultiSigConstr (MemoBytes MultiSigRaw era)
  deriving (Eq, Generic)
  deriving newtype (ToCBOR, NoThunks, SafeToHash)

instance Memoized MultiSig where
  type RawType MultiSig = MultiSigRaw

deriving instance HashAlgorithm (HASH (EraCrypto era)) => Show (MultiSig era)

-- | Magic number "memorialized" in the ValidateScript class under the method:
--   scriptPrefixTag:: Core.Script era -> Bs.ByteString, for the Shelley Era.
nativeMultiSigTag :: BS.ByteString
nativeMultiSigTag = "\00"

instance Crypto c => EraScript (ShelleyEra c) where
  type Script (ShelleyEra c) = MultiSig (ShelleyEra c)
  type NativeScript (ShelleyEra c) = MultiSig (ShelleyEra c)

  -- Calling this partial function will result in compilation error, since ByronEra has
  -- no instance for EraScript type class.
  upgradeScript = error "It is not possible to translate a script with 'upgradeScript' from Byron era"

  getNativeScript = Just

  fromNativeScript = id

  -- In the ShelleyEra there is only one kind of Script and its tag is "\x00"
  scriptPrefixTag _script = nativeMultiSigTag

instance Crypto c => ShelleyEraScript (ShelleyEra c) where
  {-# SPECIALIZE instance ShelleyEraScript (ShelleyEra StandardCrypto) #-}

  mkRequireSignature kh =
    MultiSigConstr $ memoBytes (Sum RequireSignature' 0 !> To kh)
  getRequireSignature (MultiSigConstr (Memo (RequireSignature' kh) _)) = Just kh
  getRequireSignature _ = Nothing

  mkRequireAllOf ms =
    MultiSigConstr $ memoBytes (Sum RequireAllOf' 1 !> To ms)
  getRequireAllOf (MultiSigConstr (Memo (RequireAllOf' ms) _)) = Just ms
  getRequireAllOf _ = Nothing

  mkRequireAnyOf ms =
    MultiSigConstr $ memoBytes (Sum RequireAnyOf' 2 !> To ms)
  getRequireAnyOf (MultiSigConstr (Memo (RequireAnyOf' ms) _)) = Just ms
  getRequireAnyOf _ = Nothing

  mkRequireMOf n ms =
    MultiSigConstr $ memoBytes (Sum RequireMOf' 3 !> To n !> To ms)
  getRequireMOf (MultiSigConstr (Memo (RequireMOf' n ms) _)) = Just (n, ms)
  getRequireMOf _ = Nothing

deriving newtype instance NFData (MultiSig era)

deriving via
  Mem MultiSigRaw era
  instance
    Era era => DecCBOR (Annotator (MultiSig era))

instance EqRaw (MultiSig era) where
  eqRaw = eqMultiSigRaw

pattern RequireSignature ::
  ShelleyEraScript era => KeyHash 'Witness (EraCrypto era) -> NativeScript era
pattern RequireSignature akh <- (getRequireSignature -> Just akh)
  where
    RequireSignature akh = mkRequireSignature akh

pattern RequireAllOf :: ShelleyEraScript era => StrictSeq (NativeScript era) -> NativeScript era
pattern RequireAllOf ms <- (getRequireAllOf -> Just ms)
  where
    RequireAllOf ms = mkRequireAllOf ms

pattern RequireAnyOf :: ShelleyEraScript era => StrictSeq (NativeScript era) -> NativeScript era
pattern RequireAnyOf ms <- (getRequireAnyOf -> Just ms)
  where
    RequireAnyOf ms = mkRequireAnyOf ms

pattern RequireMOf ::
  ShelleyEraScript era => Int -> StrictSeq (NativeScript era) -> NativeScript era
pattern RequireMOf n ms <- (getRequireMOf -> Just (n, ms))
  where
    RequireMOf n ms = mkRequireMOf n ms

-- | Encodes memoized bytes created upon construction.
instance Era era => EncCBOR (MultiSig era)

instance Era era => DecCBOR (Annotator (MultiSigRaw era)) where
  decCBOR = decodeRecordSum "MultiSig" $
    \case
      0 -> (,) 2 . pure . RequireSignature' . KeyHash <$> decCBOR
      1 -> do
        multiSigs <- sequence <$> decCBOR
        pure (2, RequireAllOf' <$> multiSigs)
      2 -> do
        multiSigs <- sequence <$> decCBOR
        pure (2, RequireAnyOf' <$> multiSigs)
      3 -> do
        m <- decCBOR
        multiSigs <- sequence <$> decCBOR
        pure (3, RequireMOf' m <$> multiSigs)
      k -> invalidKey k

-- | Check the equality of two underlying types, while ignoring their binary
-- representation, which `Eq` instance normally does. This is used for testing.
eqMultiSigRaw :: MultiSig era -> MultiSig era -> Bool
eqMultiSigRaw t1 t2 = go (getMemoRawType t1) (getMemoRawType t2)
  where
    seqEq Empty Empty = True
    seqEq (x :<| xs) (y :<| ys) = eqMultiSigRaw x y && seqEq xs ys
    seqEq _ _ = False
    go (RequireSignature' kh1) (RequireSignature' kh2) = kh1 == kh2
    go (RequireAllOf' ts1) (RequireAllOf' ts2) = seqEq ts1 ts2
    go (RequireAnyOf' ts1) (RequireAnyOf' ts2) = seqEq ts1 ts2
    go (RequireMOf' n1 ts1) (RequireMOf' n2 ts2) = n1 == n2 && seqEq ts1 ts2
    go _ _ = False

-- | Script evaluator for native multi-signature scheme. 'vhks' is the set of
-- key hashes that signed the transaction to be validated.
evalMultiSig ::
  (ShelleyEraScript era, NativeScript era ~ MultiSig era) =>
  Set.Set (KeyHash 'Witness (EraCrypto era)) ->
  NativeScript era ->
  Bool
evalMultiSig vhks = go
  where
    -- The important part of this validator is that it will stop as soon as it reaches the
    -- required number of valid scripts
    isValidMOf n StrictSeq.Empty = n <= 0
    isValidMOf n (msig StrictSeq.:<| msigs) =
      n <= 0 || if go msig then isValidMOf (n - 1) msigs else isValidMOf n msigs
    go = \case
      RequireSignature hk -> Set.member hk vhks
      RequireAllOf msigs -> all go msigs
      RequireAnyOf msigs -> any go msigs
      RequireMOf m msigs -> isValidMOf m msigs
      _ -> error "Impossible: All NativeScripts should have been accounted for"

-- | Script validator for native multi-signature scheme.
validateMultiSig ::
  (ShelleyEraScript era, EraTx era, NativeScript era ~ MultiSig era) =>
  Tx era ->
  NativeScript era ->
  Bool
validateMultiSig tx =
  evalMultiSig $ Set.map witVKeyHash (tx ^. witsTxL . addrTxWitsL)
{-# INLINE validateMultiSig #-}
