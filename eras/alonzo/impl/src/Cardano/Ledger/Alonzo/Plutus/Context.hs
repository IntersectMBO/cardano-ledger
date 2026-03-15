{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilyDependencies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE UndecidableSuperClasses #-}
-- Recursive definition constraints of `EraPlutusContext` and `EraPlutusTxInfo` lead to a wrongful
-- redundant constraint warning in the definition of `lookupTxInfoResult`.
--
-- Also `mkSupportedPlutusScript` has a constraint that is not required by the type system, but is
-- necessary for the safety of the function.
{-# OPTIONS_GHC -Wno-redundant-constraints #-}

module Cardano.Ledger.Alonzo.Plutus.Context (
  CollectError (..),
  LedgerTxInfo (..),
  EraPlutusTxInfo (..),
  PlutusTxInfoResult (..),
  mkPlutusTxInfoFromResult,
  toPlutusTxInfoForPurpose,
  EraPlutusContext (..),
  lookupTxInfoResultImpossible,
  SupportedLanguage (..),
  mkSupportedLanguageM,
  supportedLanguages,
  mkSupportedPlutusScript,
  mkSupportedBinaryPlutusScript,

  -- * Language dependent translation
  PlutusTxInfo,
  PlutusTxCert,
  PlutusScriptPurpose,
  PlutusScriptContext,
  PlutusTxInInfo,
) where

import Cardano.Ledger.Alonzo.Era (AlonzoEra)
import Cardano.Ledger.Alonzo.Scripts (
  AlonzoEraScript (eraMaxLanguage, mkPlutusScript),
  AsItem (..),
  AsIxItem (..),
  AsPurpose,
  PlutusPurpose,
  PlutusScript (..),
 )
import Cardano.Ledger.BaseTypes (ProtVer (..), kindObject)
import Cardano.Ledger.Binary (DecCBOR (..), EncCBOR (..))
import Cardano.Ledger.Binary.Coders
import Cardano.Ledger.Core
import Cardano.Ledger.Plutus (
  CostModel,
  Data,
  ExUnits,
  Language (..),
  Plutus (..),
  PlutusArgs,
  PlutusBinary,
  PlutusLanguage,
  PlutusScriptContext,
  PlutusWithContext (..),
  SLanguage (..),
  asSLanguage,
  plutusLanguage,
 )
import Cardano.Ledger.State (UTxO (..))
import Cardano.Ledger.TxIn (TxId, TxIn)
import Cardano.Slotting.EpochInfo (EpochInfo)
import Cardano.Slotting.Time (SystemStart)
import Control.DeepSeq (NFData)
import Control.Monad (join)
import Control.Monad.Trans.Fail.String (errorFail)
import Data.Aeson (ToJSON (..), (.=), pattern String)
import Data.Kind (Type)
import Data.List.NonEmpty (NonEmpty, nonEmpty)
import Data.Map.Strict (Map)
import Data.Text (Text)
import GHC.Generics
import GHC.Stack
import qualified PlutusLedgerApi.V1 as PV1
import qualified PlutusLedgerApi.V2 as PV2
import qualified PlutusLedgerApi.V3 as PV3

-- | All information that is necessary from the ledger to construct Plutus' TxInfo.
data LedgerTxInfo era where
  LedgerTxInfo ::
    { ltiProtVer :: !ProtVer
    , ltiEpochInfo :: !(EpochInfo (Either Text))
    , ltiSystemStart :: !SystemStart
    , ltiUTxO :: !(UTxO era)
    , ltiTx :: !(Tx level era)
    , ltiMemoizedSubTransactions :: Map TxId (TxInfoResult era)
    -- ^ This is a tricky field that is only used starting with Dijkstra era and only by top level
    -- transactions. It is always safe to leave it as `mempty` upon construction, even for Dijkstra
    } ->
    LedgerTxInfo era

class
  ( PlutusLanguage l
  , EraPlutusContext era
  , EraTxLevel era
  , Eq (PlutusTxInfo l)
  , Show (PlutusTxInfo l)
  ) =>
  EraPlutusTxInfo (l :: Language) era
  where
  toPlutusTxCert ::
    proxy l ->
    ProtVer ->
    TxCert era ->
    Either (ContextError era) (PlutusTxCert l)

  toPlutusScriptPurpose ::
    proxy l ->
    ProtVer ->
    PlutusPurpose AsIxItem era ->
    Either (ContextError era) (PlutusScriptPurpose l)

  toPlutusTxInfo ::
    proxy l ->
    LedgerTxInfo era ->
    PlutusTxInfoResult l era

  toPlutusArgs ::
    proxy l ->
    ProtVer ->
    PlutusTxInfo l ->
    PlutusPurpose AsIxItem era ->
    Maybe (Data era) ->
    Data era ->
    Either (ContextError era) (PlutusArgs l)

  toPlutusTxInInfo ::
    proxy l ->
    UTxO era ->
    TxIn ->
    Either (ContextError era) (PlutusTxInInfo era l)

-- | This is the helper type that captures translation of `Tx` to `PlutusTxInfo`.
--
-- It is important to note that `TxInfo` is always the same per Plutus version for each `Tx`. This
-- invariant allows us to avoid duplicate computation by memoizing all possible `PlutusTxInfo`s per
-- transaction. Starting with Dijkstra era there is a slight complication introduced to this
-- invariant where top level transaction has a different `PlutusTxInfo` for "Guarding" purpose, when
-- compared to all other purposes. That is the reason why result is somewhat strange, namely a
-- function from `PlutusPurpose` to `PlutusTxInfo`. It is also done this way, instead of adding
-- `ScriptPurpose` as an argument to `toPlutusTxInfo` to preserve capability of memoization, hence
-- nested `Either`
newtype PlutusTxInfoResult l era
  = PlutusTxInfoResult
  { unPlutusTxInfoResult ::
      Either
        (ContextError era)
        ( PlutusPurpose AsPurpose era ->
          Either (ContextError era) (PlutusTxInfo l)
        )
  }

-- | Given the prepared `PlutusTxInfoResult` and the purpose this function allows constructing the `PlutusTxInfo`, while memoizing the computation from  `PlutusTxInfoResult` for its subsequent uses.
mkPlutusTxInfoFromResult ::
  PlutusPurpose AsPurpose era ->
  PlutusTxInfoResult l era ->
  Either (ContextError era) (PlutusTxInfo l)
mkPlutusTxInfoFromResult sp (PlutusTxInfoResult txInfoResult) =
  join $ ($ sp) <$> txInfoResult

-- | This is what `toPlutusTxInfo` would be without the intermediate `PlutusTxInfoResult`.
--
-- /Note/ - Using this function totally drops any memoization of `TxInfo`, as such use it only for
-- testing or tooling that doesn't care about performance.
toPlutusTxInfoForPurpose ::
  EraPlutusTxInfo l era =>
  proxy l ->
  LedgerTxInfo era ->
  PlutusPurpose AsPurpose era ->
  Either (ContextError era) (PlutusTxInfo l)
toPlutusTxInfoForPurpose proxy lti sp =
  mkPlutusTxInfoFromResult sp $ toPlutusTxInfo proxy lti

class
  ( AlonzoEraScript era
  , Eq (ContextError era)
  , Show (ContextError era)
  , NFData (ContextError era)
  , EncCBOR (ContextError era)
  , DecCBOR (ContextError era)
  , ToJSON (ContextError era)
  ) =>
  EraPlutusContext era
  where
  type ContextError era = (r :: Type) | r -> era

  -- | This data type family is used to memoize the results of `toPlutusTxInfo`, so the outcome can
  -- be shared between execution of different scripts with the same language version.
  data TxInfoResult era :: Type

  mkSupportedLanguage :: Language -> Maybe (SupportedLanguage era)

  -- | Construct `PlutusTxInfo` for all supported languages in this era.
  mkTxInfoResult :: LedgerTxInfo era -> TxInfoResult era

  -- | `TxInfo` for the same language can be shared between executions of every script of the same
  -- version in a single transaction.
  --
  -- /Note/ - The `EraPlutusTxInfo` is here only to enforce this function is not called with an
  -- unsupported plutus language version.
  lookupTxInfoResult ::
    EraPlutusTxInfo l era =>
    SLanguage l ->
    TxInfoResult era ->
    PlutusTxInfoResult l era

  mkPlutusWithContext ::
    PlutusScript era ->
    ScriptHash ->
    PlutusPurpose AsIxItem era ->
    LedgerTxInfo era ->
    TxInfoResult era ->
    (Data era, ExUnits) ->
    CostModel ->
    Either (ContextError era) PlutusWithContext

-- | Helper function to use when implementing `lookupTxInfoResult` for plutus languages that are not
-- supported by the era.
lookupTxInfoResultImpossible ::
  (HasCallStack, EraPlutusTxInfo l era) => SLanguage l -> PlutusTxInfoResult l era
lookupTxInfoResultImpossible slang =
  error $ "Impossible: Attempt to lookup TxInfoResult for an unsupported language: " <> show slang

-- =============================================
-- Type families that specify Plutus types that are different from one version to another

type family PlutusTxCert (l :: Language) where
  PlutusTxCert 'PlutusV1 = PV1.DCert
  PlutusTxCert 'PlutusV2 = PV2.DCert
  PlutusTxCert 'PlutusV3 = PV3.TxCert
  PlutusTxCert 'PlutusV4 = PV3.TxCert

type family PlutusScriptPurpose (l :: Language) where
  PlutusScriptPurpose 'PlutusV1 = PV1.ScriptPurpose
  PlutusScriptPurpose 'PlutusV2 = PV2.ScriptPurpose
  PlutusScriptPurpose 'PlutusV3 = PV3.ScriptPurpose
  PlutusScriptPurpose 'PlutusV4 = PV3.ScriptPurpose

type family PlutusTxInfo (l :: Language) where
  PlutusTxInfo 'PlutusV1 = PV1.TxInfo
  PlutusTxInfo 'PlutusV2 = PV2.TxInfo
  PlutusTxInfo 'PlutusV3 = PV3.TxInfo
  PlutusTxInfo 'PlutusV4 = PV3.TxInfo

type family PlutusTxInInfo era (l :: Language) where
  -- This special case is here because Alonzo does not have a ContextError
  -- for the case where it encounters a Byron address in a TxIn
  PlutusTxInInfo AlonzoEra PlutusV1 = Maybe PV1.TxInInfo
  PlutusTxInInfo _ 'PlutusV1 = PV1.TxInInfo
  PlutusTxInInfo _ 'PlutusV2 = PV2.TxInInfo
  PlutusTxInInfo _ 'PlutusV3 = PV3.TxInInfo
  PlutusTxInInfo _ 'PlutusV4 = PV3.TxInInfo

-- | This is just like `mkPlutusScript`, except it is guaranteed to be total through the enforcement
-- of support by the type system and `EraPlutusTxInfo` type class instances for supported plutus
-- versions.
mkSupportedPlutusScript ::
  forall l era.
  (HasCallStack, EraPlutusTxInfo l era) =>
  Plutus l ->
  PlutusScript era
mkSupportedPlutusScript plutus =
  case mkPlutusScript plutus of
    Nothing ->
      error $
        "Impossible: "
          ++ show plutus
          ++ " language version should be supported by the "
          ++ eraName @era
    Just plutusScript -> plutusScript

-- | This is just like `mkBinaryPlutusScript`, except it is guaranteed to be total through the enforcement
-- of support by the type system and `EraPlutusTxInfo` type class instances (via calling `mkSupportedPlutusScript) for supported plutus
-- versions.
mkSupportedBinaryPlutusScript ::
  forall era.
  (HasCallStack, AlonzoEraScript era) =>
  SupportedLanguage era ->
  PlutusBinary ->
  PlutusScript era
mkSupportedBinaryPlutusScript supportedLanguage plutus =
  case supportedLanguage of
    SupportedLanguage sLang ->
      mkSupportedPlutusScript (asSLanguage sLang (Plutus plutus))

data SupportedLanguage era where
  SupportedLanguage :: EraPlutusTxInfo l era => SLanguage l -> SupportedLanguage era

instance Show (SupportedLanguage era) where
  show (SupportedLanguage sLang) = "(SupportedLanguage (" ++ show sLang ++ "))"

instance Eq (SupportedLanguage era) where
  SupportedLanguage sLang1 == SupportedLanguage sLang2 =
    plutusLanguage sLang1 == plutusLanguage sLang2

instance Ord (SupportedLanguage era) where
  compare (SupportedLanguage sLang1) (SupportedLanguage sLang2) =
    compare (plutusLanguage sLang1) (plutusLanguage sLang2)

instance Era era => EncCBOR (SupportedLanguage era) where
  encCBOR (SupportedLanguage sLang) = encCBOR sLang

instance EraPlutusContext era => DecCBOR (SupportedLanguage era) where
  decCBOR = decCBOR >>= mkSupportedLanguageM

supportedLanguages ::
  forall era.
  (HasCallStack, EraPlutusContext era) =>
  NonEmpty (SupportedLanguage era)
supportedLanguages =
  let langs =
        [ errorFail (mkSupportedLanguageM lang)
        | lang <- [minBound .. eraMaxLanguage @era]
        ]
   in case nonEmpty langs of
        Nothing -> error "Impossible: there are no supported languages"
        Just neLangs -> neLangs

mkSupportedLanguageM ::
  forall era m.
  (EraPlutusContext era, MonadFail m) =>
  Language ->
  m (SupportedLanguage era)
mkSupportedLanguageM lang =
  case mkSupportedLanguage lang of
    Nothing -> fail $ show lang ++ " language is not supported in " ++ eraName @era
    Just supportedLanguage -> pure supportedLanguage

-- | When collecting inputs for two phase scripts, these are the that things can go wrong.
data CollectError era
  = NoRedeemer !(PlutusPurpose AsItem era)
  | NoWitness !ScriptHash
  | NoCostModel !Language
  | BadTranslation !(ContextError era)
  deriving (Generic)

deriving instance
  (AlonzoEraScript era, Eq (ContextError era)) =>
  Eq (CollectError era)

deriving instance
  (AlonzoEraScript era, Show (ContextError era)) =>
  Show (CollectError era)

instance
  (AlonzoEraScript era, NFData (ContextError era)) =>
  NFData (CollectError era)

instance (AlonzoEraScript era, EncCBOR (ContextError era)) => EncCBOR (CollectError era) where
  encCBOR (NoRedeemer x) = encode $ Sum NoRedeemer 0 !> To x
  encCBOR (NoWitness x) = encode $ Sum (NoWitness @era) 1 !> To x
  encCBOR (NoCostModel x) = encode $ Sum NoCostModel 2 !> To x
  encCBOR (BadTranslation x) = encode $ Sum (BadTranslation @era) 3 !> To x

instance (AlonzoEraScript era, DecCBOR (ContextError era)) => DecCBOR (CollectError era) where
  decCBOR = decode (Summands "CollectError" dec)
    where
      dec 0 = SumD NoRedeemer <! From
      dec 1 = SumD NoWitness <! From
      dec 2 = SumD NoCostModel <! From
      dec 3 = SumD BadTranslation <! From
      dec n = Invalid n

instance
  ( Era era
  , ToJSON (PlutusPurpose AsItem era)
  , ToJSON (ContextError era)
  ) =>
  ToJSON (CollectError era)
  where
  toJSON = \case
    NoRedeemer sPurpose ->
      kindObject "CollectError" $
        [ "error" .= String "NoRedeemer"
        , "plutusPurpose" .= toJSON sPurpose
        ]
    NoWitness sHash ->
      kindObject "CollectError" $
        [ "error" .= String "NoWitness"
        , "scriptHash" .= toJSON sHash
        ]
    NoCostModel lang ->
      kindObject "CollectError" $
        [ "error" .= String "NoCostModel"
        , "language" .= toJSON lang
        ]
    BadTranslation err ->
      kindObject "BadTranslation" ["error" .= toJSON err]
