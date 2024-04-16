{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module Cardano.Ledger.Alonzo.Plutus.Evaluate (
  evalPlutusScripts,
  evalPlutusScriptsWithLogs,
  CollectError (..),
  collectPlutusScriptsWithContext,
  lookupPlutusScript,
)
where

import Cardano.Ledger.Alonzo.Core
import Cardano.Ledger.Alonzo.Plutus.Context (EraPlutusContext (..))
import Cardano.Ledger.Alonzo.Scripts (plutusScriptLanguage, toAsItem, toAsIx)
import Cardano.Ledger.Alonzo.TxWits (lookupRedeemer)
import Cardano.Ledger.Alonzo.UTxO (AlonzoEraUTxO (getSpendingDatum), AlonzoScriptsNeeded (..))
import Cardano.Ledger.BaseTypes (ProtVer (pvMajor), kindObject, natVersion)
import Cardano.Ledger.Binary (DecCBOR (..), EncCBOR (..))
import Cardano.Ledger.Binary.Coders
import Cardano.Ledger.Plutus.CostModels (costModelsValid)
import Cardano.Ledger.Plutus.Data (fromMaybeData, getPlutusData)
import Cardano.Ledger.Plutus.Evaluate (
  PlutusDatums (..),
  PlutusWithContext (..),
  ScriptResult (..),
  runPlutusScriptWithLogs,
 )
import Cardano.Ledger.Plutus.Language (Language (..))
import Cardano.Ledger.UTxO (EraUTxO (..), ScriptsProvided (..), UTxO (..))
import Cardano.Slotting.EpochInfo (EpochInfo)
import Cardano.Slotting.Time (SystemStart)
import Control.DeepSeq (NFData)
import Control.Monad (guard)
import Data.Aeson (ToJSON (..), (.=), pattern String)
import Data.List (intercalate)
import qualified Data.Map.Strict as Map
import Data.Maybe (mapMaybe)
import qualified Data.Set as Set
import Data.Text (Text)
import Debug.Trace (traceEvent)
import GHC.Generics
import Lens.Micro
import NoThunks.Class (NoThunks)

-- ===============================================================
-- From the specification, Figure 7 "Scripts and their Arguments"
-- ===============================================================

-- | When collecting inputs for two phase scripts, 3 things can go wrong.
data CollectError era
  = NoRedeemer !(PlutusPurpose AsItem era)
  | NoWitness !(ScriptHash (EraCrypto era))
  | NoCostModel !Language
  | BadTranslation !(ContextError era)
  deriving (Generic)

deriving instance
  (AlonzoEraScript era, Eq (ContextError era)) =>
  Eq (CollectError era)

deriving instance
  (AlonzoEraScript era, Show (ContextError era)) =>
  Show (CollectError era)

deriving instance
  (AlonzoEraScript era, NoThunks (ContextError era)) =>
  NoThunks (CollectError era)

deriving instance
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

-- | Given a script hash and a Map of available scripts, find the PlutusScript. Returns
-- Nothing when script is missing or it is not a PlutusScript
lookupPlutusScript ::
  AlonzoEraScript era =>
  Map.Map (ScriptHash (EraCrypto era)) (Script era) ->
  ScriptHash (EraCrypto era) ->
  Maybe (PlutusScript era)
lookupPlutusScript scriptsAvailable scriptHash = do
  script <- scriptHash `Map.lookup` scriptsAvailable
  toPlutusScript script

collectPlutusScriptsWithContext ::
  forall era.
  ( AlonzoEraTxBody era
  , AlonzoEraTxWits era
  , AlonzoEraUTxO era
  , ScriptsNeeded era ~ AlonzoScriptsNeeded era
  , EraPlutusContext era
  ) =>
  EpochInfo (Either Text) ->
  SystemStart ->
  PParams era ->
  Tx era ->
  UTxO era ->
  Either [CollectError era] [PlutusWithContext (EraCrypto era)]
collectPlutusScriptsWithContext epochInfo sysStart pp tx utxo =
  -- TODO: remove this whole complicated check when we get into Conway. It is much simpler
  -- to fail on a CostModel lookup in the `apply` function (already implemented).
  let missingCostModels = Set.filter (`Map.notMember` costModels) usedLanguages
   in case guard (protVerMajor < natVersion @9) >> Set.lookupMin missingCostModels of
        Just l -> Left [NoCostModel l]
        Nothing ->
          merge
            apply
            (map getScriptWithRedeemer neededPlutusScripts)
            (Right [])
  where
    -- Check on a protocol version to preserve failure mode (a single NoCostModel failure
    -- for languages with missing cost models) until we are in Conway era. After we hard
    -- fork into Conway it will be safe to remove this check together with the
    -- `missingCostModels` lookup
    --
    -- We also need to pass major protocol version to the script for script evaluation
    protVerMajor = pvMajor (pp ^. ppProtocolVersionL)
    costModels = costModelsValid $ pp ^. ppCostModelsL

    ScriptsProvided scriptsProvided = getScriptsProvided utxo tx
    AlonzoScriptsNeeded scriptsNeeded = getScriptsNeeded utxo (tx ^. bodyTxL)
    neededPlutusScripts =
      mapMaybe (\(sp, sh) -> (,) (sh, sp) <$> lookupPlutusScript scriptsProvided sh) scriptsNeeded
    usedLanguages = Set.fromList $ map (plutusScriptLanguage . snd) neededPlutusScripts

    getScriptWithRedeemer ((plutusScriptHash, plutusPurpose), plutusScript) =
      let redeemerIndex = hoistPlutusPurpose toAsIx plutusPurpose
       in case lookupRedeemer redeemerIndex $ tx ^. witsTxL . rdmrsTxWitsL of
            Just (d, exUnits) -> Right (plutusScript, plutusPurpose, d, exUnits, plutusScriptHash)
            Nothing -> Left (NoRedeemer (hoistPlutusPurpose toAsItem plutusPurpose))
    apply (plutusScript, plutusPurpose, d, exUnits, plutusScriptHash) = do
      let lang = plutusScriptLanguage plutusScript
      costModel <- maybe (Left (NoCostModel lang)) Right $ Map.lookup lang costModels
      case mkPlutusScriptContext plutusScript plutusPurpose pp epochInfo sysStart utxo tx of
        Right scriptContext ->
          let spendingDatum = getSpendingDatum utxo tx $ hoistPlutusPurpose toAsItem plutusPurpose
              datums
                | protVerMajor < natVersion @9 = maybe id (:) spendingDatum [d, scriptContext]
                | otherwise = [fromMaybeData spendingDatum, d, scriptContext]
           in Right $
                withPlutusScript plutusScript $ \plutus ->
                  PlutusWithContext
                    { pwcProtocolVersion = protVerMajor
                    , pwcScript = Left plutus
                    , pwcScriptHash = plutusScriptHash
                    , pwcDatums = PlutusDatums (getPlutusData <$> datums)
                    , pwcExUnits = exUnits
                    , pwcCostModel = costModel
                    }
        Left te -> Left $ BadTranslation te

-- | Merge two lists (the first of which may have failures, i.e. (Left _)), collect all the failures
--   but if there are none, use 'f' to construct a success.
merge :: forall t b a. (t -> Either a b) -> [Either a t] -> Either [a] [b] -> Either [a] [b]
merge _f [] answer = answer
merge f (x : xs) zs = merge f xs (gg x zs)
  where
    gg :: Either a t -> Either [a] [b] -> Either [a] [b]
    gg (Right t) (Right cs) =
      case f t of
        Right c -> Right $ c : cs
        Left e -> Left [e]
    gg (Left a) (Right _) = Left [a]
    gg (Right _) (Left cs) = Left cs
    gg (Left a) (Left cs) = Left (a : cs)

-- | Evaluate a list of Plutus scripts. All scripts in the list must evaluate to `True`.
evalPlutusScripts ::
  EraTx era =>
  Tx era ->
  [PlutusWithContext (EraCrypto era)] ->
  ScriptResult (EraCrypto era)
evalPlutusScripts tx pwcs = snd $ evalPlutusScriptsWithLogs tx pwcs

evalPlutusScriptsWithLogs ::
  EraTx era =>
  Tx era ->
  [PlutusWithContext (EraCrypto era)] ->
  ([Text], ScriptResult (EraCrypto era))
evalPlutusScriptsWithLogs _tx [] = mempty
evalPlutusScriptsWithLogs tx (plutusWithContext : rest) =
  let beginMsg =
        intercalate
          ","
          [ "[LEDGER][PLUTUS_SCRIPT]"
          , "BEGIN"
          ]
      !res = traceEvent beginMsg $ runPlutusScriptWithLogs plutusWithContext
      endMsg =
        intercalate
          ","
          [ "[LEDGER][PLUTUS_SCRIPT]"
          , "END"
          ]
   in traceEvent endMsg res <> evalPlutusScriptsWithLogs tx rest
