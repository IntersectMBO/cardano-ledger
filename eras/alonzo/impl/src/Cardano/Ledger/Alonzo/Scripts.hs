{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Cardano.Ledger.Alonzo.Scripts (
  Tag (..),
  PlutusBinary (..),
  AlonzoScript (TimelockScript, PlutusScript),
  Script,
  isPlutusScript,
  validScript,
  transProtocolVersion,
  eqAlonzoScriptRaw,
  translateAlonzoScript,

  -- * Re-exports
  module Cardano.Ledger.Plutus.CostModels,
  module Cardano.Ledger.Plutus.ExUnits,
)
where

import Cardano.Ledger.Allegra.Scripts (Timelock, eqTimelockRaw, translateTimelock)
import Cardano.Ledger.Alonzo.Era (AlonzoEra)
import Cardano.Ledger.BaseTypes (ProtVer (..))
import Cardano.Ledger.Binary (
  Annotator,
  DecCBOR (decCBOR),
  DecoderError (..),
  EncCBOR (encCBOR),
  ToCBOR (toCBOR),
  cborError,
 )
import Cardano.Ledger.Binary.Coders (
  Decode (Ann, D, From, Invalid, SumD, Summands),
  Encode (Sum, To),
  Wrapped (..),
  decode,
  encode,
  (!>),
  (<*!),
 )
import Cardano.Ledger.Binary.Plain (serializeAsHexText)
import Cardano.Ledger.Core
import Cardano.Ledger.Crypto (Crypto)
import Cardano.Ledger.MemoBytes (EqRaw (..))
import Cardano.Ledger.Plutus.CostModels
import Cardano.Ledger.Plutus.ExUnits
import Cardano.Ledger.Plutus.Language (
  Language (..),
  Plutus (..),
  PlutusBinary (..),
  guardPlutus,
 )
import Cardano.Ledger.Plutus.TxInfo (transProtocolVersion)
import Cardano.Ledger.SafeHash (SafeToHash (..))
import Cardano.Ledger.Shelley.Scripts (nativeMultiSigTag)
import Control.DeepSeq (NFData (..), deepseq, rwhnf)
import Data.Aeson (ToJSON (..), Value (String))
import Data.Either (isRight)
import Data.Word (Word8)
import GHC.Generics (Generic)
import NoThunks.Class (NoThunks (..))
import qualified PlutusLedgerApi.V1 as PV1 (
  ScriptDecodeError,
  ScriptForEvaluation,
  deserialiseScript,
 )
import qualified PlutusLedgerApi.V2 as PV2 (deserialiseScript)
import qualified PlutusLedgerApi.V3 as PV3 (deserialiseScript)

-- | Marker indicating the part of a transaction for which this script is acting
-- as a validator.
data Tag
  = -- | Validates spending a script-locked UTxO
    Spend
  | -- | Validates minting new tokens
    Mint
  | -- | Validates certificate transactions
    Cert
  | -- | Validates withdrawal from a reward account
    Rewrd
  deriving (Eq, Generic, Ord, Show, Enum, Bounded)

instance NoThunks Tag

instance NFData Tag where
  rnf = rwhnf

-- =======================================================

-- | Scripts in the Alonzo Era, Either a Timelock script or a Plutus script.
data AlonzoScript era
  = TimelockScript !(Timelock era)
  | PlutusScript !Plutus
  deriving (Eq, Generic, NoThunks)

translateAlonzoScript ::
  (Era era1, Era era2, EraCrypto era1 ~ EraCrypto era2) =>
  AlonzoScript era1 ->
  AlonzoScript era2
translateAlonzoScript = \case
  TimelockScript ts -> TimelockScript $ translateTimelock ts
  PlutusScript ps -> PlutusScript ps

instance (EraScript era, Script era ~ AlonzoScript era) => Show (AlonzoScript era) where
  show (TimelockScript x) = "TimelockScript " ++ show x
  show s@(PlutusScript plutus) =
    "PlutusScript " ++ show (plutusLanguage plutus) ++ " " ++ show (hashScript @era s)

instance NFData (AlonzoScript era)

-- | Both constructors know their original bytes
instance SafeToHash (AlonzoScript era) where
  originalBytes (TimelockScript t) = originalBytes t
  originalBytes (PlutusScript (Plutus _ binaryPlutus)) = originalBytes binaryPlutus

isPlutusScript :: EraScript era => Script era -> Bool
isPlutusScript = not . isNativeScript

instance Crypto c => EraScript (AlonzoEra c) where
  type Script (AlonzoEra c) = AlonzoScript (AlonzoEra c)
  type NativeScript (AlonzoEra c) = Timelock (AlonzoEra c)

  upgradeScript = TimelockScript . translateTimelock

  scriptPrefixTag script =
    case script of
      TimelockScript _ -> nativeMultiSigTag -- "\x00"
      PlutusScript (Plutus PlutusV1 _) -> "\x01"
      PlutusScript (Plutus PlutusV2 _) -> "\x02"
      PlutusScript (Plutus PlutusV3 _) -> "\x03"

  getNativeScript = \case
    TimelockScript ts -> Just ts
    _ -> Nothing

instance EqRaw (AlonzoScript era) where
  eqRaw = eqAlonzoScriptRaw

instance Era era => ToJSON (AlonzoScript era) where
  toJSON = String . serializeAsHexText

--------------------------------------------------------------------------------
-- Serialisation
--------------------------------------------------------------------------------

tagToWord8 :: Tag -> Word8
tagToWord8 = toEnum . fromEnum

word8ToTag :: Word8 -> Maybe Tag
word8ToTag e
  | fromEnum e > fromEnum (Prelude.maxBound :: Tag) = Nothing
  | fromEnum e < fromEnum (minBound :: Tag) = Nothing
  | otherwise = Just $ toEnum (fromEnum e)

instance EncCBOR Tag where
  encCBOR = encCBOR . tagToWord8

instance DecCBOR Tag where
  decCBOR =
    word8ToTag <$> decCBOR >>= \case
      Nothing -> cborError $ DecoderErrorCustom "Tag" "Unknown redeemer tag"
      Just n -> pure n
  {-# INLINE decCBOR #-}

instance Era era => EncCBOR (AlonzoScript era)

instance Era era => ToCBOR (AlonzoScript era) where
  toCBOR = toEraCBOR @era . encode . encodeScript

encodeScript :: Era era => AlonzoScript era -> Encode 'Open (AlonzoScript era)
encodeScript = \case
  TimelockScript i -> Sum TimelockScript 0 !> To i
  PlutusScript (Plutus PlutusV1 s) -> Sum (PlutusScript . Plutus PlutusV1) 1 !> To s
  PlutusScript (Plutus PlutusV2 s) -> Sum (PlutusScript . Plutus PlutusV2) 2 !> To s
  PlutusScript (Plutus PlutusV3 s) -> Sum (PlutusScript . Plutus PlutusV3) 3 !> To s

instance Era era => DecCBOR (Annotator (AlonzoScript era)) where
  decCBOR = decode (Summands "Alonzo Script" decodeScript)
    where
      decodeAnnPlutus lang =
        Ann (SumD $ PlutusScript . Plutus lang) <*! Ann (D (guardPlutus lang >> decCBOR))
      {-# INLINE decodeAnnPlutus #-}
      decodeScript :: Word -> Decode 'Open (Annotator (AlonzoScript era))
      decodeScript = \case
        0 -> Ann (SumD TimelockScript) <*! From
        1 -> decodeAnnPlutus PlutusV1
        2 -> decodeAnnPlutus PlutusV2
        3 -> decodeAnnPlutus PlutusV3
        n -> Invalid n
      {-# INLINE decodeScript #-}
  {-# INLINE decCBOR #-}

-- | Test that every Alonzo script represents a real Script.
--     Run deepseq to see that there are no infinite computations and that
--     every Plutus Script unflattens into a real PV1.Script
validScript :: ProtVer -> AlonzoScript era -> Bool
validScript pv script =
  case script of
    TimelockScript sc -> deepseq sc True
    PlutusScript (Plutus lang (PlutusBinary bytes)) ->
      let deserialiseScript =
            case lang of
              PlutusV1 -> PV1.deserialiseScript
              PlutusV2 -> PV2.deserialiseScript
              PlutusV3 -> PV3.deserialiseScript
          eWellFormed :: Either PV1.ScriptDecodeError PV1.ScriptForEvaluation
          eWellFormed = deserialiseScript (transProtocolVersion pv) bytes
       in isRight eWellFormed

-- | Check the equality of two underlying types, while ignoring their binary
-- representation, which `Eq` instance normally does. This is used for testing.
eqAlonzoScriptRaw :: AlonzoScript era -> AlonzoScript era -> Bool
eqAlonzoScriptRaw (TimelockScript t1) (TimelockScript t2) = eqTimelockRaw t1 t2
eqAlonzoScriptRaw (PlutusScript ps1) (PlutusScript ps2) = ps1 == ps2
eqAlonzoScriptRaw _ _ = False
