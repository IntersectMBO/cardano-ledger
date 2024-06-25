{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE UndecidableSuperClasses #-}

module Cardano.Ledger.Plutus.Evaluate (
  PlutusWithContext (..),
  ScriptFailure (..),
  ScriptResult (..),
  scriptPass,
  scriptFail,
  PlutusDatums (..),
  PlutusDebugInfo (..),
  debugPlutus,
  runPlutusScript,
  runPlutusScriptWithLogs,
  evaluatePlutusWithContext,
  explainPlutusEvaluationError,
)
where

import Cardano.Ledger.Binary (
  DecCBOR (..),
  EncCBOR (..),
  FromCBOR (..),
  ToCBOR (..),
  Version,
  toPlainDecoder,
  toPlainEncoding,
 )
import qualified Cardano.Ledger.Binary.Plain as Plain
import Cardano.Ledger.Crypto (Crypto)
import Cardano.Ledger.Hashes (ScriptHash)
import Cardano.Ledger.Plutus.CostModels (
  CostModel,
  decodeCostModelFailHard,
  encodeCostModel,
  getEvaluationContext,
 )
import Cardano.Ledger.Plutus.ExUnits (ExUnits)
import Cardano.Ledger.Plutus.Language (
  Language (..),
  Plutus (..),
  PlutusLanguage (..),
  PlutusRunnable (..),
  decodeWithPlutus,
  hashPlutusScript,
  plutusFromRunnable,
  plutusLanguage,
 )
import Cardano.Ledger.Plutus.TxInfo
import Control.DeepSeq (NFData (..))
import Control.Monad (unless)
import qualified Data.ByteString.Base64 as B64
import qualified Data.ByteString.UTF8 as BSU
import Data.List.NonEmpty (NonEmpty (..))
import Data.Text (Text, pack)
import GHC.Generics (Generic)
import PlutusLedgerApi.Common as P (EvaluationError (CodecError), ExBudget, VerboseMode)
import qualified PlutusLedgerApi.V1 as PV1
import PlutusLedgerApi.V1.Contexts ()
import qualified PlutusLedgerApi.V2 as PV2
import qualified PlutusLedgerApi.V3 as PV3
import qualified PlutusLedgerApi.V4 as PV4
import Prettyprinter (Pretty (..))

-- | This type contains all that is necessary from Ledger to evaluate a plutus script.
data PlutusWithContext c where
  PlutusWithContext ::
    PlutusLanguage l =>
    { pwcProtocolVersion :: !Version
    -- ^ Mayjor protocol version that is necessary for [de]serialization
    , pwcScript :: !(Either (Plutus l) (PlutusRunnable l))
    -- ^ Actual plutus script that will be evaluated. Script is allowed to be in two forms:
    -- serialized and deserialized. This is necesary for implementing the opptimization
    -- that preserves deserialized `PlutusRunnable` after verifying wellformedness of
    -- plutus scripts during transaction validation (yet to be implemented).
    , pwcScriptHash :: !(ScriptHash c)
    , pwcDatums :: !PlutusDatums
    -- ^ All of the arguments to the Plutus scripts, including the redeemer and the
    -- Plutus context that was obtained from the transaction translation
    , pwcExUnits :: !ExUnits
    -- ^ Limit on the execution units
    , pwcCostModel :: !CostModel
    -- ^ `CostModel` to be used during script evaluation. It must match the language
    -- version in the `pwcScript`
    } ->
    PlutusWithContext c

deriving instance Show (PlutusWithContext c)

instance NFData (PlutusWithContext c) where
  rnf PlutusWithContext {..} =
    rnf pwcProtocolVersion `seq`
      rnf pwcScript `seq`
        rnf pwcScriptHash `seq`
          rnf pwcDatums `seq`
            rnf pwcExUnits `seq`
              rnf pwcCostModel

instance Eq (PlutusWithContext c) where
  pwc1@(PlutusWithContext {pwcScript = s1}) == pwc2@(PlutusWithContext {pwcScript = s2}) =
    pwcProtocolVersion pwc1 == pwcProtocolVersion pwc2
      && pwcScriptHash pwc1 == pwcScriptHash pwc2
      && pwcDatums pwc1 == pwcDatums pwc2
      && pwcExUnits pwc1 == pwcExUnits pwc2
      && pwcCostModel pwc1 == pwcCostModel pwc2
      && eqScripts s1 s2
    where
      eqScripts (Left p1) (Left p2) =
        plutusLanguage p1 == plutusLanguage p2 && plutusBinary p1 == plutusBinary p2
      eqScripts (Right p1) (Right p2) =
        plutusLanguage p1 == plutusLanguage p2 && plutusRunnable p1 == plutusRunnable p2
      eqScripts _ _ = False

data ScriptFailure c = ScriptFailure
  { scriptFailureMessage :: Text
  , scriptFailurePlutus :: PlutusWithContext c
  }
  deriving (Show, Generic)

data ScriptResult c
  = Passes [PlutusWithContext c]
  | Fails [PlutusWithContext c] (NonEmpty (ScriptFailure c))
  deriving (Generic)

scriptPass :: PlutusWithContext c -> ScriptResult c
scriptPass pwc = Passes [pwc]

scriptFail :: ScriptFailure c -> ScriptResult c
scriptFail sf = Fails [] (pure sf)

withRunnablePlutusWithContext ::
  PlutusWithContext c ->
  -- | Handle the decoder failure
  (P.EvaluationError -> a) ->
  (forall l. PlutusLanguage l => PlutusRunnable l -> a) ->
  a
withRunnablePlutusWithContext PlutusWithContext {pwcProtocolVersion, pwcScript} onError f =
  case pwcScript of
    Right pr -> f pr
    Left plutus ->
      case decodePlutusRunnable pwcProtocolVersion plutus of
        Right pr -> f pr
        Left err -> onError (P.CodecError err)

instance Semigroup (ScriptResult c) where
  Passes ps <> Passes qs = Passes (ps <> qs)
  Passes ps <> Fails qs xs = Fails (ps <> qs) xs
  Fails ps xs <> Passes qs = Fails (ps <> qs) xs
  Fails ps xs <> Fails qs ys = Fails (ps <> qs) (xs <> ys)

instance Monoid (ScriptResult c) where
  mempty = Passes mempty

newtype PlutusDatums = PlutusDatums {unPlutusDatums :: [PV1.Data]}
  deriving (Eq, Show, Generic)

instance NFData PlutusDatums

instance EncCBOR PlutusDatums where
  encCBOR (PlutusDatums d) = encCBOR d

instance DecCBOR PlutusDatums where
  decCBOR = PlutusDatums <$> decCBOR

instance Crypto c => ToCBOR (PlutusWithContext c) where
  toCBOR (PlutusWithContext {..}) =
    Plain.encodeListLen 6
      <> toCBOR pwcProtocolVersion
      <> toPlainEncoding pwcProtocolVersion (either encCBOR encCBOR pwcScript)
      <> toPlainEncoding pwcProtocolVersion (encCBOR pwcScriptHash)
      <> toPlainEncoding pwcProtocolVersion (encCBOR pwcDatums)
      <> toPlainEncoding pwcProtocolVersion (encCBOR pwcExUnits)
      <> toPlainEncoding pwcProtocolVersion (encodeCostModel pwcCostModel)

instance Crypto c => FromCBOR (PlutusWithContext c) where
  fromCBOR = Plain.decodeRecordNamed "PlutusWithContext" (const 6) $ do
    pwcProtocolVersion <- fromCBOR
    toPlainDecoder pwcProtocolVersion $ decodeWithPlutus $ \plutus -> do
      let lang = plutusLanguage plutus
          pwcScript = Left plutus
          scriptHash = hashPlutusScript plutus
      pwcScriptHash <- decCBOR
      unless (pwcScriptHash == scriptHash) $
        fail $
          "ScriptHash mismatch. Encoded: "
            <> show pwcScriptHash
            <> " doesn't match the actual: "
            <> show scriptHash
      pwcDatums <- decCBOR
      pwcExUnits <- decCBOR
      pwcCostModel <- decodeCostModelFailHard lang
      pure PlutusWithContext {..}

data PlutusDebugInfo c
  = DebugBadHex String
  | DebugCannotDecode String
  | DebugSuccess [Text] PV1.ExBudget
  | DebugFailure [Text] P.EvaluationError (PlutusWithContext c)
  deriving (Show)

debugPlutus :: Crypto c => String -> PlutusDebugInfo c
debugPlutus db =
  case B64.decode (BSU.fromString db) of
    Left e -> DebugBadHex (show e)
    Right bs ->
      case Plain.decodeFull' bs of
        Left e -> DebugCannotDecode $ show e
        Right pwc@(PlutusWithContext {..}) ->
          let cm = getEvaluationContext pwcCostModel
              eu = transExUnits pwcExUnits
              PlutusDatums d = pwcDatums
              onDecoderError err = DebugFailure [] err pwc
              toDebugInfo = \case
                (logs, Left err) -> DebugFailure logs err pwc
                (logs, Right ex) -> DebugSuccess logs ex
           in withRunnablePlutusWithContext pwc onDecoderError $ \plutusRunnable ->
                toDebugInfo $
                  evaluatePlutusRunnable pwcProtocolVersion PV1.Verbose cm eu plutusRunnable d

runPlutusScript :: PlutusWithContext c -> ScriptResult c
runPlutusScript = snd . runPlutusScriptWithLogs

runPlutusScriptWithLogs ::
  PlutusWithContext c ->
  ([Text], ScriptResult c)
runPlutusScriptWithLogs pwc = toScriptResult <$> evaluatePlutusWithContext PV1.Quiet pwc
  where
    toScriptResult = \case
      Left evalError -> explainPlutusEvaluationError pwc evalError
      Right _ -> scriptPass pwc

evaluatePlutusWithContext ::
  P.VerboseMode ->
  PlutusWithContext c ->
  ([Text], Either P.EvaluationError P.ExBudget)
evaluatePlutusWithContext mode pwc@PlutusWithContext {..} =
  withRunnablePlutusWithContext pwc (([],) . Left) $ \plutusRunnable ->
    evaluatePlutusRunnable
      pwcProtocolVersion
      mode
      (getEvaluationContext pwcCostModel)
      (transExUnits pwcExUnits)
      plutusRunnable
      (unPlutusDatums pwcDatums)

-- | Explain why a script might fail. Scripts come in two flavors:
--
-- (1) with 3 data arguments [data,redeemer,context]
--
-- (2) with 2 data arguments [redeemer,context].
--
-- It pays to decode the context data into a real context because that provides
-- way more information. But there is no guarantee the context data really can
-- be decoded.
explainPlutusEvaluationError ::
  PlutusWithContext c ->
  P.EvaluationError ->
  ScriptResult c
explainPlutusEvaluationError pwc@PlutusWithContext {pwcProtocolVersion, pwcScript, pwcDatums} e =
  let lang = either plutusLanguage plutusLanguage pwcScript
      Plutus binaryScript = either id plutusFromRunnable pwcScript
      firstLines =
        [ "The " ++ show lang ++ " script failed:"
        , "Base64-encoded script bytes:"
        ]
      shLine = "The script hash is:" ++ show (pwcScriptHash pwc)
      pvLine = "The protocol version is: " ++ show pwcProtocolVersion
      plutusError = "The plutus evaluation error is: " ++ show e

      getCtxAsString :: PV1.Data -> Language -> Maybe String
      getCtxAsString d = \case
        PlutusV1 -> show . pretty <$> (PV1.fromData d :: Maybe PV1.ScriptContext)
        PlutusV2 -> show . pretty <$> (PV2.fromData d :: Maybe PV2.ScriptContext)
        PlutusV3 -> show . pretty <$> (PV3.fromData d :: Maybe PV3.ScriptContext)
        PlutusV4 -> show . pretty <$> (PV4.fromData d :: Maybe PV4.ScriptContext)

      ctxMessage info =
        case getCtxAsString info lang of
          Nothing ->
            concat
              [ "The third data argument does not translate to a "
              , show lang
              , " script context\n"
              , show info
              ]
          Just ctx -> "The script context is:\n" ++ ctx
      dataLines =
        case unPlutusDatums pwcDatums of
          [dat, redeemer, info] ->
            [ "The datum is: " ++ show dat
            , "The redeemer is: " ++ show redeemer
            , ctxMessage info
            ]
          [redeemer, info] ->
            [ "The redeemer is: " ++ show redeemer
            , ctxMessage info
            ]
          ds ->
            [ "Received an unexpected number of Data"
            , "The data is:\n" ++ show ds
            ]
      line = pack . unlines $ "" : firstLines ++ show binaryScript : shLine : plutusError : pvLine : dataLines
   in scriptFail $ ScriptFailure line pwc
