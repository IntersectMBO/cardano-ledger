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
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Cardano.Ledger.Shelley.Scripts (
  MultiSig (
    RequireAllOf,
    RequireAnyOf,
    RequireSignature,
    RequireMOf
  ),
  ScriptHash (..),
  nativeMultiSigTag,
)
where

import Cardano.Crypto.Hash.Class (HashAlgorithm)
import Cardano.Ledger.BaseTypes (invalidKey)
import Cardano.Ledger.Binary (
  Annotator (..),
  FromCBOR (fromCBOR),
  EncCBOR,
  decodeRecordSum,
 )
import Cardano.Ledger.Binary.Coders (Encode (..), (!>))
import Cardano.Ledger.Core
import Cardano.Ledger.Crypto (HASH)
import qualified Cardano.Ledger.Crypto as CC (Crypto)
import Cardano.Ledger.Keys (KeyHash (..), KeyRole (Witness))
import Cardano.Ledger.MemoBytes (
  Mem,
  MemoBytes,
  memoBytes,
  pattern Memo,
 )
import Cardano.Ledger.SafeHash (SafeToHash (..))
import Cardano.Ledger.Shelley.Era
import Control.DeepSeq (NFData)
import qualified Data.ByteString as BS
import GHC.Generics (Generic)
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
    RequireAllOf' ![MultiSig era]
  | -- | Require any one of the sub-terms to be satisfied.
    RequireAnyOf' ![MultiSig era]
  | -- | Require M of the given sub-terms to be satisfied.
    RequireMOf' !Int ![MultiSig era]
  deriving (Eq, Generic)
  deriving anyclass (NoThunks)

deriving instance HashAlgorithm (HASH (EraCrypto era)) => Show (MultiSigRaw era)

instance NFData (MultiSigRaw era)

newtype MultiSig era = MultiSigConstr (MemoBytes MultiSigRaw era)
  deriving (Eq, Generic)
  deriving newtype (EncCBOR, NoThunks, SafeToHash)

deriving instance HashAlgorithm (HASH (EraCrypto era)) => Show (MultiSig era)

-- | Magic number "memorialized" in the ValidateScript class under the method:
--   scriptPrefixTag:: Core.Script era -> Bs.ByteString, for the Shelley Era.
nativeMultiSigTag :: BS.ByteString
nativeMultiSigTag = "\00"

type instance SomeScript 'PhaseOne (ShelleyEra c) = MultiSig (ShelleyEra c)

instance CC.Crypto c => EraScript (ShelleyEra c) where
  type Script (ShelleyEra c) = MultiSig (ShelleyEra c)

  -- In the ShelleyEra there is only one kind of Script and its tag is "\x00"
  scriptPrefixTag _script = nativeMultiSigTag
  phaseScript PhaseOneRep multisig = Just (Phase1Script multisig)
  phaseScript PhaseTwoRep _ = Nothing

deriving newtype instance NFData (MultiSig era)

deriving via
  Mem MultiSigRaw era
  instance
    Era era => FromCBOR (Annotator (MultiSig era))

pattern RequireSignature :: Era era => KeyHash 'Witness (EraCrypto era) -> MultiSig era
pattern RequireSignature akh <-
  MultiSigConstr (Memo (RequireSignature' akh) _)
  where
    RequireSignature akh =
      MultiSigConstr $ memoBytes (Sum RequireSignature' 0 !> To akh)

pattern RequireAllOf :: Era era => [MultiSig era] -> MultiSig era
pattern RequireAllOf ms <-
  MultiSigConstr (Memo (RequireAllOf' ms) _)
  where
    RequireAllOf ms =
      MultiSigConstr $ memoBytes (Sum RequireAllOf' 1 !> Enc ms)

pattern RequireAnyOf :: Era era => [MultiSig era] -> MultiSig era
pattern RequireAnyOf ms <-
  MultiSigConstr (Memo (RequireAnyOf' ms) _)
  where
    RequireAnyOf ms =
      MultiSigConstr $ memoBytes (Sum RequireAnyOf' 2 !> Enc ms)

pattern RequireMOf :: Era era => Int -> [MultiSig era] -> MultiSig era
pattern RequireMOf n ms <-
  MultiSigConstr (Memo (RequireMOf' n ms) _)
  where
    RequireMOf n ms =
      MultiSigConstr $ memoBytes (Sum RequireMOf' 3 !> To n !> Enc ms)

{-# COMPLETE RequireSignature, RequireAllOf, RequireAnyOf, RequireMOf #-}

instance
  (Era era) =>
  FromCBOR (Annotator (MultiSigRaw era))
  where
  fromCBOR = decodeRecordSum "MultiSig" $
    \case
      0 -> (,) 2 . pure . RequireSignature' . KeyHash <$> fromCBOR
      1 -> do
        multiSigs <- sequence <$> fromCBOR
        pure (2, RequireAllOf' <$> multiSigs)
      2 -> do
        multiSigs <- sequence <$> fromCBOR
        pure (2, RequireAnyOf' <$> multiSigs)
      3 -> do
        m <- fromCBOR
        multiSigs <- sequence <$> fromCBOR
        pure (3, RequireMOf' m <$> multiSigs)
      k -> invalidKey k
