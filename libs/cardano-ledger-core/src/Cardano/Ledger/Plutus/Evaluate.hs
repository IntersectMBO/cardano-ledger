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
  PlutusDebugInfo (..),
  PlutusDebugOverrides (..),
  defaultPlutusDebugOverrides,
  debugPlutus,
  debugPlutusUnbounded,
  runPlutusScript,
  runPlutusScriptWithLogs,
  evaluatePlutusWithContext,
  explainPlutusEvaluationError,
) where

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
import Cardano.Ledger.Hashes (ScriptHash)
import Cardano.Ledger.Plutus.CostModels (
  CostModel,
  decodeCostModel,
  encodeCostModel,
  getCostModelLanguage,
  getCostModelParams,
  getEvaluationContext,
  mkCostModel,
 )
import Cardano.Ledger.Plutus.ExUnits (ExUnits (..))
import Cardano.Ledger.Plutus.Language (
  Language,
  Plutus (..),
  PlutusBinary (..),
  PlutusLanguage (..),
  PlutusRunnable (..),
  decodeWithPlutus,
  hashPlutusScript,
  plutusFromRunnable,
  plutusLanguage,
  withSamePlutusLanguage,
 )
import Cardano.Ledger.Plutus.TxInfo
import Codec.Extras.SerialiseViaFlat (DeserialiseFailureInfo (..), DeserialiseFailureReason (..))
import Control.DeepSeq (NFData (..), deepseq, ($!!))
import Control.Monad (unless)
import Data.ByteString (ByteString)
import qualified Data.ByteString.Base16 as B16
import qualified Data.ByteString.Base64 as B64
import qualified Data.ByteString.Char8 as BSC
import qualified Data.ByteString.Short as SBS
import qualified Data.ByteString.UTF8 as BSU
import Data.Either (fromRight)
import Data.Functor ((<&>))
import Data.Int (Int64)
import Data.List.NonEmpty (NonEmpty (..))
import Data.Maybe (fromMaybe)
import Data.Text (Text, pack)
import GHC.Generics (Generic)
import GHC.Stack (HasCallStack)
import Numeric.Natural (Natural)
import qualified PlutusLedgerApi.Common as P (
  EvaluationError (..),
  ExBudget,
  ScriptDecodeError (..),
  VerboseMode (..),
 )
import Prettyprinter (Pretty (..))
import System.Timeout (timeout)

-- | This type contains all that is necessary from Ledger to evaluate a plutus script.
data PlutusWithContext where
  PlutusWithContext ::
    PlutusLanguage l =>
    { pwcProtocolVersion :: !Version
    -- ^ Major protocol version that is necessary for [de]serialization
    , pwcScript :: !(Either (Plutus l) (PlutusRunnable l))
    -- ^ Actual plutus script that will be evaluated. Script is allowed to be in two forms:
    -- serialized and deserialized. This is necesary for implementing the opptimization
    -- that preserves deserialized `PlutusRunnable` after verifying wellformedness of
    -- plutus scripts during transaction validation (yet to be implemented).
    , pwcScriptHash :: !ScriptHash
    -- ^ Hash of the above script as it would appear on-chain. In other words it is not
    -- just a hash of the script contents. (See `Cardano.Ledger.Core.hashScript` for more info)
    , pwcArgs :: !(PlutusArgs l)
    -- ^ All of the arguments to the Plutus scripts, including the redeemer and the
    -- Plutus context that was obtained from the transaction translation
    , pwcExUnits :: !ExUnits
    -- ^ Limit on the execution units
    , pwcCostModel :: !CostModel
    -- ^ `CostModel` to be used during script evaluation. It must match the language
    -- version in the `pwcScript`
    } ->
    PlutusWithContext

deriving instance Show PlutusWithContext

instance NFData PlutusWithContext where
  rnf PlutusWithContext {..} =
    rnf pwcProtocolVersion `seq`
      rnf pwcScript `seq`
        rnf pwcScriptHash `seq`
          rnf pwcArgs `seq`
            rnf pwcExUnits `seq`
              rnf pwcCostModel

instance Eq PlutusWithContext where
  pwc1@(PlutusWithContext {pwcScript = s1, pwcArgs = args1})
    == pwc2@(PlutusWithContext {pwcScript = s2, pwcArgs = args2}) =
      pwcProtocolVersion pwc1 == pwcProtocolVersion pwc2
        && pwcScriptHash pwc1 == pwcScriptHash pwc2
        && fromMaybe False (withSamePlutusLanguage args1 args2 (==))
        && pwcExUnits pwc1 == pwcExUnits pwc2
        && pwcCostModel pwc1 == pwcCostModel pwc2
        && eqScripts s1 s2
      where
        eqScripts (Left p1) (Left p2) =
          plutusLanguage p1 == plutusLanguage p2 && plutusBinary p1 == plutusBinary p2
        eqScripts (Right p1) (Right p2) =
          plutusLanguage p1 == plutusLanguage p2 && plutusRunnable p1 == plutusRunnable p2
        eqScripts _ _ = False

data ScriptFailure = ScriptFailure
  { scriptFailureMessage :: Text
  , scriptFailurePlutus :: PlutusWithContext
  }
  deriving (Show, Generic)

data ScriptResult
  = Passes [PlutusWithContext]
  | Fails [PlutusWithContext] (NonEmpty ScriptFailure)
  deriving (Generic)

scriptPass :: PlutusWithContext -> ScriptResult
scriptPass pwc = Passes [pwc]

scriptFail :: ScriptFailure -> ScriptResult
scriptFail sf = Fails [] (pure sf)

withRunnablePlutusWithContext ::
  PlutusWithContext ->
  -- | Handle the decoder failure
  (P.EvaluationError -> a) ->
  (forall l. PlutusLanguage l => PlutusRunnable l -> PlutusArgs l -> a) ->
  a
withRunnablePlutusWithContext PlutusWithContext {pwcProtocolVersion, pwcScript, pwcArgs} onError f =
  case pwcScript of
    Right pr -> f pr pwcArgs
    Left plutus ->
      case decodePlutusRunnable pwcProtocolVersion plutus of
        Right pr -> f pr pwcArgs
        Left err -> onError (P.CodecError err)

instance Semigroup ScriptResult where
  Passes ps <> Passes qs = Passes (ps <> qs)
  Passes ps <> Fails qs xs = Fails (ps <> qs) xs
  Fails ps xs <> Passes qs = Fails (ps <> qs) xs
  Fails ps xs <> Fails qs ys = Fails (ps <> qs) (xs <> ys)

instance Monoid ScriptResult where
  mempty = Passes mempty

instance ToCBOR PlutusWithContext where
  toCBOR (PlutusWithContext {..}) =
    Plain.encodeListLen 6
      <> toCBOR pwcProtocolVersion
      <> toPlainEncoding pwcProtocolVersion (either encCBOR encCBOR pwcScript)
      <> toPlainEncoding pwcProtocolVersion (encCBOR pwcScriptHash)
      <> toPlainEncoding pwcProtocolVersion (encCBOR pwcArgs)
      <> toPlainEncoding pwcProtocolVersion (encCBOR pwcExUnits)
      <> toPlainEncoding pwcProtocolVersion (encodeCostModel pwcCostModel)

instance FromCBOR PlutusWithContext where
  fromCBOR = Plain.decodeRecordNamed "PlutusWithContext" (const 6) $ do
    pwcProtocolVersion <- fromCBOR
    toPlainDecoder Nothing pwcProtocolVersion $ decodeWithPlutus $ \plutus -> do
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
      pwcArgs <- decCBOR
      pwcExUnits <- decCBOR
      pwcCostModel <- decodeCostModel lang
      pure PlutusWithContext {..}

data PlutusDebugInfo
  = DebugBadHex String
  | DebugCannotDecode String
  | DebugSuccess
      -- | Execution logs from the plutus interpreter
      [Text]
      -- | Execution budget that was consumed. It will always be less or equal to what was
      -- supplied during execution.
      P.ExBudget
  | DebugFailure
      -- | Execution logs from the plutus interpreter
      [Text]
      -- | Evaluation error from Plutus interpreter
      P.EvaluationError
      -- | Everything that is needed in order to run the script
      PlutusWithContext
      -- | Expected execution budget. This value is Nothing when the supplied script can't
      -- be executed within 5 second limit or there is a problem with decoding plutus script
      -- itself.
      (Maybe P.ExBudget)
  | -- | Script did not terminate within the imposed limit
    DebugTimedOut
      -- | Wall clock limit in microseconds that was imposed on the script execution.
      Int
  deriving (Show)

instance NFData PlutusDebugInfo where
  rnf = \case
    DebugBadHex str -> rnf str
    DebugCannotDecode str -> rnf str
    DebugSuccess logs exBudget -> logs `deepseq` rnf exBudget
    DebugFailure logs evalError pwc mExBudget ->
      let
        -- TODO: Upstream `NFData` instance for `EvaluationError`
        seqEvalError = \case
          P.CekError exc -> deepseq exc
          P.DeBruijnError err -> deepseq err
          P.CodecError err -> deepseqCodecError err
          P.CostModelParameterMismatch -> id
          P.InvalidReturnValue -> id
        -- TODO: Upstream `NFData` instance for `CodecError` and `MajorProtocolVersion`
        deepseqCodecError = \case
          P.CBORDeserialiseError failureInfo -> deepseqDeserialiseFailureInfo failureInfo
          P.RemainderError bsl -> deepseq bsl
          P.LedgerLanguageNotAvailableError pll ipv tpv -> pll `deepseq` ipv `seq` (tpv `seq`)
          P.PlutusCoreLanguageNotAvailableError pll ipv tpv -> pll `deepseq` ipv `seq` (tpv `seq`)
        -- TODO: Upstream `NFData` instance for `DeserialiseFailureInfo`
        deepseqDeserialiseFailureInfo = \case
          DeserialiseFailureInfo bo reason ->
            bo `deepseq`
              ( -- TODO: Upstream `NFData` instance for `DeserialiseFailureReason`
                case reason of
                  EndOfInput -> id
                  ExpectedBytes -> id
                  OtherReason str -> deepseq str
              )
       in
        logs `deepseq` evalError `seqEvalError` pwc `deepseq` rnf mExBudget
    DebugTimedOut t -> rnf t

-- | Various overrides that can be supplied to `plutusDebug` and `plutusDebugUnbouded`
data PlutusDebugOverrides = PlutusDebugOverrides
  { pdoScript :: !(Maybe ByteString)
  -- ^ Hex encoded version of the script
  , pdoProtocolVersion :: !(Maybe Version)
  -- ^ Protocol version to be used for decoding and exection
  , pdoLanguage :: !(Maybe Language)
  -- ^ Plutus ledger language version
  , pdoCostModelValues :: !(Maybe [Int64])
  -- ^ Cost model to be used for deciding execution units
  , pdoExUnitsMem :: !(Maybe Natural)
  -- ^ Memory execution units to be used for execution
  , pdoExUnitsSteps :: !(Maybe Natural)
  -- ^ CPU execution units to be used for execution
  , pdoExUnitsEnforced :: !Bool
  -- ^ Setting this flag to True will disable reporting expected execution units upon a failure,
  -- which would protect against a potentially unbounded script execution.
  }
  deriving (Show)

defaultPlutusDebugOverrides :: PlutusDebugOverrides
defaultPlutusDebugOverrides =
  PlutusDebugOverrides
    { pdoScript = Nothing
    , pdoProtocolVersion = Nothing
    , pdoLanguage = Nothing
    , pdoCostModelValues = Nothing
    , pdoExUnitsMem = Nothing
    , pdoExUnitsSteps = Nothing
    , pdoExUnitsEnforced = False
    }

-- TODO: Add support for overriding arguments.
overrideContext :: HasCallStack => PlutusWithContext -> PlutusDebugOverrides -> PlutusWithContext
overrideContext PlutusWithContext {..} PlutusDebugOverrides {..} =
  -- NOTE: due to GADTs, we can't do a record update here and need to
  -- copy all the fields. Otherwise GHC will greet us with
  -- `Record update for insufficiently polymorphic field...` error
  PlutusWithContext
    { pwcProtocolVersion = fromMaybe pwcProtocolVersion pdoProtocolVersion
    , pwcScript = overrideScript
    , pwcExUnits = overrideExUnits
    , pwcCostModel = overrideCostModel
    , pwcScriptHash = overrideSriptHash
    , ..
    }
  where
    overrideExUnits =
      ExUnits
        (fromMaybe (exUnitsMem pwcExUnits) pdoExUnitsMem)
        (fromMaybe (exUnitsSteps pwcExUnits) pdoExUnitsSteps)
    overrideCostModel =
      fromRight pwcCostModel $
        mkCostModel
          (fromMaybe (getCostModelLanguage pwcCostModel) pdoLanguage)
          (fromMaybe (getCostModelParams pwcCostModel) pdoCostModelValues)
    (overrideSriptHash, overrideScript) =
      case pdoScript of
        Nothing -> (pwcScriptHash, pwcScript)
        Just hexScript ->
          case Plutus . PlutusBinary . SBS.toShort <$> B16.decode (BSC.filter (/= '\n') hexScript) of
            Left err -> error $ "Failed hex decoding of the custom script: " <> err
            Right script -> (hashPlutusScript script, Left script)

-- | Execute a hex encoded script with the context that was produced within the ledger predicate
-- failure. Using `PlutusDebugOverrides` it is possible to override any part of the execution.
debugPlutus :: HasCallStack => String -> Int -> PlutusDebugOverrides -> IO PlutusDebugInfo
debugPlutus scriptsWithContext limit opts =
  timeout limit (pure $!! debugPlutusUnbounded scriptsWithContext opts)
    <&> \case
      Nothing -> DebugTimedOut limit
      Just res -> res

-- | This is just like `debugPlutus`, except it is pure and if a supplied script contains an
-- infinite loop or a very expensive computation, it might not terminate within a reasonable
-- timeframe.
debugPlutusUnbounded :: HasCallStack => String -> PlutusDebugOverrides -> PlutusDebugInfo
debugPlutusUnbounded scriptsWithContext opts =
  case B64.decode (BSU.fromString scriptsWithContext) of
    Left e -> DebugBadHex e
    Right bs ->
      case Plain.decodeFull' bs of
        Left e -> DebugCannotDecode $ show e
        Right pwcOriginal ->
          let pwc = overrideContext pwcOriginal opts
              cm = getEvaluationContext $ pwcCostModel pwc
              eu = transExUnits $ pwcExUnits pwc
              onDecoderError err = DebugFailure [] err pwc Nothing
           in withRunnablePlutusWithContext pwc onDecoderError $ \plutusRunnable args ->
                let toDebugInfo logs = \case
                      Left err@(P.CodecError {}) -> DebugFailure logs err pwc Nothing
                      Left err | pdoExUnitsEnforced opts -> DebugFailure logs err pwc Nothing
                      Left err ->
                        let res =
                              evaluatePlutusRunnableBudget (pwcProtocolVersion pwc) P.Verbose cm plutusRunnable args
                            mExpectedExUnits =
                              case snd res of
                                Left {} -> Nothing
                                Right exUnits -> Just exUnits
                         in DebugFailure logs err pwc mExpectedExUnits
                      Right ex -> DebugSuccess logs ex
                 in uncurry toDebugInfo $
                      evaluatePlutusRunnable (pwcProtocolVersion pwc) P.Verbose cm eu plutusRunnable args

runPlutusScript :: PlutusWithContext -> ScriptResult
runPlutusScript = snd . runPlutusScriptWithLogs

runPlutusScriptWithLogs ::
  PlutusWithContext ->
  ([Text], ScriptResult)
runPlutusScriptWithLogs pwc = toScriptResult <$> evaluatePlutusWithContext P.Quiet pwc
  where
    toScriptResult = \case
      Left evalError -> explainPlutusEvaluationError pwc evalError
      Right _ -> scriptPass pwc

evaluatePlutusWithContext ::
  P.VerboseMode ->
  PlutusWithContext ->
  ([Text], Either P.EvaluationError P.ExBudget)
evaluatePlutusWithContext mode pwc@PlutusWithContext {..} =
  withRunnablePlutusWithContext pwc (([],) . Left) $
    evaluatePlutusRunnable
      pwcProtocolVersion
      mode
      (getEvaluationContext pwcCostModel)
      (transExUnits pwcExUnits)

-- | Explain why a script might fail. Scripts come in three flavors:
--
-- (1) with 3 arguments @[data,redeemer,context]@ for `PlutusV1` and `PlustuV2`
--
-- (2) with 2 arguments @[redeemer,context]@ for `PlutusV1` and `PlustuV2`
--
-- (3) with 1 argument @context@ for `PlutusV3` onwards
explainPlutusEvaluationError ::
  PlutusWithContext ->
  P.EvaluationError ->
  ScriptResult
explainPlutusEvaluationError pwc@PlutusWithContext {pwcProtocolVersion, pwcScript, pwcArgs} e =
  let lang = either plutusLanguage plutusLanguage pwcScript
      Plutus binaryScript = either id plutusFromRunnable pwcScript
      firstLines =
        [ "The " ++ show lang ++ " script failed:"
        , "Base64-encoded script bytes:"
        ]
      shLine = "The script hash is:" ++ show (pwcScriptHash pwc)
      pvLine = "The protocol version is: " ++ show pwcProtocolVersion
      plutusError = "The plutus evaluation error is: " ++ show e

      dataLines = show (pretty pwcArgs)
      line =
        pack . unlines $ "" : firstLines ++ [show binaryScript, shLine, plutusError, pvLine, dataLines]
   in scriptFail $ ScriptFailure line pwc
