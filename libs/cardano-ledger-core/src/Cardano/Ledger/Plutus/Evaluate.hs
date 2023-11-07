{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE UndecidableSuperClasses #-}

module Cardano.Ledger.Plutus.Evaluate (
  ScriptFailure (..),
  ScriptResult (..),
  scriptPass,
  scriptFail,
  PlutusDebugLang (..),
  PlutusDebug (..),
  PlutusData (..),
  PlutusError (..),
  PlutusDebugInfo (..),
  EraPlutusContext (..),
  PlutusWithContext (..),
  PlutusTxCert (..),
  unTxCertV1,
  unTxCertV2,
  unTxCertV3,
  debugPlutus,
  runPlutusScript,
  runPlutusScriptWithLogs,
  deserialiseAndEvaluateScript,
  explainPlutusEvaluationError,
)
where

import Cardano.Ledger.BaseTypes (ProtVer (..))
import Cardano.Ledger.Binary (DecCBOR (..), EncCBOR (..), Version, decodeFull')
import Cardano.Ledger.Binary.Coders (Encode (..), encode, (!>))
import Cardano.Ledger.Binary.Decoding (decodeRecordSum)
import Cardano.Ledger.Core
import Cardano.Ledger.Plutus.CostModels (
  CostModel,
  decodeCostModelFailHard,
  encodeCostModel,
  getEvaluationContext,
 )
import Cardano.Ledger.Plutus.Data (Data (..), getPlutusData)
import Cardano.Ledger.Plutus.ExUnits (ExUnits)
import Cardano.Ledger.Plutus.Language (
  BinaryPlutus (..),
  IsLanguage (..),
  Language (..),
  Plutus (..),
  SLanguage (..),
  fromSLanguage,
  withSLanguage,
 )
import Cardano.Ledger.Plutus.TxInfo
import Control.Monad (when)
import Control.Monad.Trans.Fail
import qualified Data.ByteString.Base64 as B64
import qualified Data.ByteString.UTF8 as BSU
import qualified Data.Foldable as F (asum)
import Data.List.NonEmpty (NonEmpty (..))
import Data.Text (Text, pack)
import Data.Typeable (Proxy (..))
import GHC.Generics (Generic)
import qualified PlutusLedgerApi.V1 as PV1
import PlutusLedgerApi.V1.Contexts ()
import qualified PlutusLedgerApi.V2 as PV2
import qualified PlutusLedgerApi.V3 as PV3
import Prettyprinter (Pretty (..))

data PlutusWithContext era = PlutusWithContext
  { pwcScript :: !Plutus
  , pwcDatums :: ![Data era]
  , pwcExUnits :: !ExUnits
  , pwcCostModel :: !CostModel
  }
  deriving (Eq)

deriving instance Era era => Show (PlutusWithContext era)

data ScriptFailure = PlutusSF Text PlutusDebug
  deriving (Show, Generic)

data ScriptResult
  = Passes [PlutusDebug]
  | Fails [PlutusDebug] (NonEmpty ScriptFailure)
  deriving (Generic)

scriptPass :: PlutusDebug -> ScriptResult
scriptPass pd = Passes [pd]

scriptFail :: ScriptFailure -> ScriptResult
scriptFail pd = Fails [] (pure pd)

instance Semigroup ScriptResult where
  (Passes ps) <> (Passes qs) = Passes (ps <> qs)
  (Passes ps) <> (Fails qs xs) = Fails (ps <> qs) xs
  (Fails ps xs) <> (Passes qs) = Fails (ps <> qs) xs
  (Fails ps xs) <> (Fails qs ys) = Fails (ps <> qs) (xs <> ys)

instance Monoid ScriptResult where
  mempty = Passes mempty

newtype PlutusData = PlutusData {unPlutusData :: [PV1.Data]}
  deriving (Eq)

instance EncCBOR PlutusData where
  encCBOR (PlutusData d) = encCBOR d

instance DecCBOR PlutusData where
  decCBOR = PlutusData <$> decCBOR

data PlutusDebugLang (l :: Language) where
  PlutusDebugLang ::
    { pdSLanguage :: SLanguage l
    , pdCostModel :: CostModel
    , pdExUnits :: ExUnits
    , pdPlutusScript :: BinaryPlutus
    , pdPlutusData :: PlutusData
    , pdProtVer :: ProtVer
    } ->
    PlutusDebugLang l

-- | There is dummy Show instance for PlutusDebugLang intentionally, because it is too
-- expensive and it will be too tempting to use it incorrectly. If needed for
-- testing use 'StandaloneDeriving', otherwise define an efficient way to display
-- this info.
instance Show (PlutusDebugLang l) where
  show _ = "PlutusDebug Omitted"

deriving instance Eq (SLanguage l) => Eq (PlutusDebugLang l)

deriving instance Generic (PlutusDebugLang l)

instance (IsLanguage l, EncCBOR (SLanguage l)) => EncCBOR (PlutusDebugLang l) where
  encCBOR (PlutusDebugLang slang costModel exUnits sbs pData protVer) =
    encode $
      Sum (PlutusDebugLang slang) (fromIntegral (fromEnum (fromSLanguage slang)))
        !> E encodeCostModel costModel
        !> To exUnits
        !> To sbs
        !> To pData
        !> To protVer

instance IsLanguage l => DecCBOR (PlutusDebugLang l) where
  decCBOR = decodeRecordSum "PlutusDebugLang" $ \tag -> do
    let slang = isLanguage @l
        lang = fromSLanguage slang
    when (fromEnum lang /= fromIntegral tag) $ fail $ "Unexpected language: " <> show tag
    costModel <- decodeCostModelFailHard lang
    exUnits <- decCBOR
    sbs <- decCBOR
    pData <- decCBOR
    protVer <- decCBOR
    pure (6, PlutusDebugLang slang costModel exUnits sbs pData protVer)

data PlutusDebug where
  PlutusDebug :: IsLanguage l => PlutusDebugLang l -> PlutusDebug

deriving instance Show PlutusDebug

instance EncCBOR PlutusDebug where
  encCBOR (PlutusDebug pdbg) = encCBOR pdbg

data PlutusError
  = PlutusErrorV1 PV1.EvaluationError
  | PlutusErrorV2 PV2.EvaluationError
  | PlutusErrorV3 PV3.EvaluationError
  deriving (Show)

data PlutusDebugInfo
  = DebugSuccess PV1.ExBudget -- NOTE: PV1.ExBudget == PV2.ExBudget, hence this works
  | DebugCannotDecode String
  | DebugInfo [Text] PlutusError PlutusDebug
  | DebugBadHex String
  deriving (Show)

debugPlutus ::
  Version ->
  String ->
  PlutusDebugInfo
debugPlutus version db =
  case B64.decode (BSU.fromString db) of
    Left e -> DebugBadHex (show e)
    Right bs ->
      let plutusDebugLangDecoder ::
            forall l. IsLanguage l => Proxy l -> Fail String PlutusDebug
          plutusDebugLangDecoder _ =
            FailT $
              pure $
                either (Left . pure . show) (Right . PlutusDebug) $
                  decodeFull' @(PlutusDebugLang l) version bs
          plutusDebugDecoder =
            F.asum
              [ plutusDebugLangDecoder (Proxy @'PlutusV1)
              , plutusDebugLangDecoder (Proxy @'PlutusV2)
              , plutusDebugLangDecoder (Proxy @'PlutusV3)
              ]
       in case runFail plutusDebugDecoder of
            Left e -> DebugCannotDecode e
            Right pd@(PlutusDebug (PlutusDebugLang sl costModel exUnits binaryScript pData protVer)) ->
              let pv = transProtocolVersion protVer
                  v = PV1.Verbose
                  cm = getEvaluationContext costModel
                  eu = transExUnits exUnits
                  BinaryPlutus script = binaryScript
                  PlutusData d = pData
               in case sl of
                    SPlutusV1 ->
                      case deserialiseAndEvaluateScript PlutusV1 pv v cm eu script d of
                        (logs, Left e) -> DebugInfo logs (PlutusErrorV1 e) pd
                        (_, Right ex) -> DebugSuccess ex
                    SPlutusV2 ->
                      case deserialiseAndEvaluateScript PlutusV2 pv v cm eu script d of
                        (logs, Left e) -> DebugInfo logs (PlutusErrorV2 e) pd
                        (_, Right ex) -> DebugSuccess ex
                    SPlutusV3 ->
                      case deserialiseAndEvaluateScript PlutusV3 pv v cm eu script d of
                        (logs, Left e) -> DebugInfo logs (PlutusErrorV3 e) pd
                        (_, Right ex) -> DebugSuccess ex

-- TODO: this function is supposed to exist temporarily to cope with the breaking changes
-- in https://github.com/input-output-hk/plutus/pull/5538, until script deserialisation
-- is properly fixed in the ledger.
deserialiseAndEvaluateScript ::
  Language ->
  PV1.MajorProtocolVersion ->
  PV1.VerboseMode ->
  PV1.EvaluationContext ->
  PV1.ExBudget ->
  PV1.SerialisedScript ->
  [PV1.Data] ->
  (PV1.LogOutput, Either PV1.EvaluationError PV1.ExBudget)
deserialiseAndEvaluateScript lang pv v ctx budget ss = case deserialise pv ss of
  Right script -> eval pv v ctx budget script
  Left err -> const (mempty, Left (PV1.CodecError err))
  where
    (deserialise, eval) = case lang of
      PlutusV1 -> (PV1.deserialiseScript, PV1.evaluateScriptRestricting)
      PlutusV2 -> (PV2.deserialiseScript, PV2.evaluateScriptRestricting)
      PlutusV3 -> (PV3.deserialiseScript, PV3.evaluateScriptRestricting)

-- The runPLCScript in the Specification has a slightly different type
-- than the one in the implementation below. Made necessary by the the type
-- of PV1.evaluateScriptRestricting which is the interface to Plutus, and in the impementation
-- we try to track why a script failed (if it does) by the [String] in the Fails constructor of
-- ScriptResut.

runPlutusScript ::
  forall era.
  ProtVer ->
  PlutusWithContext era ->
  ScriptResult
runPlutusScript pv pwc = snd (runPlutusScriptWithLogs pv pwc)

runPlutusScriptWithLogs ::
  forall era.
  ProtVer ->
  PlutusWithContext era ->
  ([Text], ScriptResult)
runPlutusScriptWithLogs pv pwc@PlutusWithContext {pwcScript, pwcDatums, pwcExUnits, pwcCostModel} =
  ( logs
  , case interpretedScript of
      Left evalError -> explainPlutusEvaluationError pv pwc evalError
      Right _ ->
        withSLanguage lang $ \slang ->
          scriptPass $
            PlutusDebug $
              PlutusDebugLang
                slang
                pwcCostModel
                pwcExUnits
                scriptBytes
                (PlutusData (map getPlutusData pwcDatums))
                pv
  )
  where
    Plutus lang scriptBytes = pwcScript
    (logs, interpretedScript) =
      plutusInterpreter
        lang
        PV1.Quiet
        (getEvaluationContext pwcCostModel)
        (transExUnits pwcExUnits)
        (unBinaryPlutus scriptBytes)
        (map getPlutusData pwcDatums)
    plutusPV = transProtocolVersion pv
    plutusInterpreter PlutusV1 = deserialiseAndEvaluateScript PlutusV1 plutusPV
    plutusInterpreter PlutusV2 = deserialiseAndEvaluateScript PlutusV2 plutusPV
    plutusInterpreter PlutusV3 = deserialiseAndEvaluateScript PlutusV3 plutusPV -- TODO: Make class to unify all plutus versioned operations

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
  forall era.
  ProtVer ->
  PlutusWithContext era ->
  PV1.EvaluationError ->
  ScriptResult
explainPlutusEvaluationError pv PlutusWithContext {pwcScript, pwcDatums, pwcExUnits, pwcCostModel} e =
  let Plutus lang binaryScript = pwcScript
      firstLine = "The " ++ show lang ++ " script failed:"
      pvLine = "The protocol version is: " ++ show pv
      plutusError = "The plutus evaluation error is: " ++ show e

      getCtxAsString :: Language -> PV1.Data -> Maybe String
      getCtxAsString PlutusV1 d = show . pretty <$> (PV1.fromData d :: Maybe PV1.ScriptContext)
      getCtxAsString PlutusV2 d = show . pretty <$> (PV2.fromData d :: Maybe PV2.ScriptContext)
      getCtxAsString PlutusV3 d = show . pretty <$> (PV3.fromData d :: Maybe PV3.ScriptContext)

      ctxMessage info =
        case getCtxAsString lang info of
          Nothing ->
            concat
              [ "The third data argument does not translate to a "
              , show lang
              , " script context\n"
              , show info
              ]
          Just ctx -> "The script context is:\n" ++ ctx
      datums = map getPlutusData pwcDatums
      dataLines =
        case datums of
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
      line = pack . unlines $ "" : firstLine : show binaryScript : plutusError : pvLine : dataLines

      plutusDebug =
        withSLanguage lang $ \slang ->
          PlutusDebug $
            PlutusDebugLang
              { pdSLanguage = slang
              , pdCostModel = pwcCostModel
              , pdExUnits = pwcExUnits
              , pdPlutusScript = binaryScript
              , pdPlutusData = PlutusData datums
              , pdProtVer = pv
              }
   in scriptFail $ PlutusSF line plutusDebug
