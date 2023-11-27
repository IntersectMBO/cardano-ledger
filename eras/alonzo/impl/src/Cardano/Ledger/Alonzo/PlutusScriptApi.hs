{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module Cardano.Ledger.Alonzo.PlutusScriptApi (
  -- Figure 8
  getSpendingTxIn,
  getDatumAlonzo,
  evalScripts,
  evalScriptsWithLogs,
  evalPlutusScripts,
  evalPlutusScriptsWithLogs,
  -- Figure 12
  scriptsNeeded,
  scriptsNeededFromBody,
  language,
  CollectError (..),
  collectTwoPhaseScriptInputs,
  collectPlutusScriptsWithContext,
  knownToNotBe1Phase,
)
where

import Cardano.Ledger.Alonzo.Core hiding (TranslationError)
import Cardano.Ledger.Alonzo.Plutus.TxInfo (
  EraPlutusContext,
  ExtendedUTxO (..),
  PlutusWithContext (..),
  ScriptResult (..),
  TranslationError (..),
  runPlutusScriptWithLogs,
  valContext,
 )
import Cardano.Ledger.Alonzo.Scripts (AlonzoScript (..), CostModel, CostModels (..), ExUnits (..))
import Cardano.Ledger.Alonzo.Tx (ScriptPurpose (..), indexedRdmrs)
import Cardano.Ledger.Alonzo.TxWits (AlonzoEraTxWits (..))
import Cardano.Ledger.Alonzo.UTxO (
  AlonzoEraUTxO (getSpendingDatum),
  AlonzoScriptsNeeded (..),
  getAlonzoSpendingDatum,
  getAlonzoSpendingTxIn,
 )
import Cardano.Ledger.BaseTypes (ProtVer (pvMajor), natVersion)
import Cardano.Ledger.Binary (DecCBOR (..), EncCBOR (..))
import Cardano.Ledger.Binary.Coders
import Cardano.Ledger.Plutus.Data (Data)
import Cardano.Ledger.Plutus.Language (BinaryPlutus (..), Language (..), Plutus (..))
import Cardano.Ledger.TxIn (TxIn (..))
import Cardano.Ledger.UTxO (EraUTxO (..), ScriptsProvided (..), UTxO (..))
import Cardano.Slotting.EpochInfo (EpochInfo)
import Cardano.Slotting.Time (SystemStart)
import Control.Monad (guard)
import Data.ByteString.Short (ShortByteString)
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
-- From the specification, Figure 8 "Scripts and their Arguments"
-- ===============================================================

-- | Only the Spending ScriptPurpose contains TxIn
getSpendingTxIn :: ScriptPurpose era -> Maybe (TxIn (EraCrypto era))
getSpendingTxIn = getAlonzoSpendingTxIn
{-# DEPRECATED getSpendingTxIn "In favor of `getAlonzoSpendingTxIn`" #-}

-- | Get the Data associated with a ScriptPurpose. Only the Spending
--   ScriptPurpose contains Data. Nothing is returned for the other kinds.
getDatumAlonzo ::
  (AlonzoEraTxWits era, AlonzoEraTxOut era, EraTx era) =>
  Tx era ->
  UTxO era ->
  ScriptPurpose era ->
  Maybe (Data era)
getDatumAlonzo tx utxo = getAlonzoSpendingDatum utxo tx
{-# DEPRECATED getDatumAlonzo "In favor of `getAlonzoSpendingDatum`" #-}

-- ========================================================================

-- | When collecting inputs for two phase scripts, 3 things can go wrong.
data CollectError era
  = NoRedeemer !(ScriptPurpose era)
  | NoWitness !(ScriptHash (EraCrypto era))
  | NoCostModel !Language
  | BadTranslation !(TranslationError (EraCrypto era))
  deriving (Generic)

deriving instance (Era era, Eq (TxCert era)) => Eq (CollectError era)
deriving instance (Era era, Show (TxCert era)) => Show (CollectError era)
deriving instance (Era era, NoThunks (TxCert era)) => NoThunks (CollectError era)

instance EraTxCert era => EncCBOR (CollectError era) where
  encCBOR (NoRedeemer x) = encode $ Sum NoRedeemer 0 !> To x
  encCBOR (NoWitness x) = encode $ Sum (NoWitness @era) 1 !> To x
  encCBOR (NoCostModel x) = encode $ Sum NoCostModel 2 !> To x
  encCBOR (BadTranslation x) = encode $ Sum (BadTranslation @era) 3 !> To x

instance (Era era, DecCBOR (TxCert era)) => DecCBOR (CollectError era) where
  decCBOR = decode (Summands "CollectError" dec)
    where
      dec 0 = SumD NoRedeemer <! From
      dec 1 = SumD NoWitness <! From
      dec 2 = SumD NoCostModel <! From
      dec 3 = SumD BadTranslation <! From
      dec n = Invalid n

-- Given a script purpose and a script hash, determine if it is *not*
-- a simple 1-phase script by looking up the script hash in a mapping
-- of script hashes to labeled scripts.
-- If the script is determined to not be a 1-phase script, we return
-- a triple: the script purpose, the language, and the script bytes.
--
-- The formal spec achieves the same filtering as knownToNotBe1Phase
-- by use of the (partial) language function, which is not defined on 1-phase scripts.
knownToNotBe1Phase ::
  Map.Map (ScriptHash (EraCrypto era)) (AlonzoScript era) ->
  (ScriptPurpose era, ScriptHash (EraCrypto era)) ->
  Maybe (ScriptPurpose era, Plutus)
knownToNotBe1Phase scriptsAvailable (sp, sh) = do
  PlutusScript plutus <- sh `Map.lookup` scriptsAvailable
  Just (sp, plutus)

-- | Collect the inputs for twophase scripts. If any script can't find ist data return
--     a list of CollectError, if all goes well return a list of quadruples with the inputs.
--     Previous PredicateFailure tests should ensure we find Data for every script, BUT
--     the consequences of not finding Data means scripts can get dropped, so things
--     might validate that shouldn't. So we double check that every Script has its Data, and
--     if that is not the case, a PredicateFailure is raised in the Utxos rule.
collectTwoPhaseScriptInputs ::
  forall era.
  ( MaryEraTxBody era
  , AlonzoEraTxWits era
  , AlonzoEraUTxO era
  , ScriptsNeeded era ~ AlonzoScriptsNeeded era
  , ExtendedUTxO era
  , Script era ~ AlonzoScript era
  , AlonzoEraPParams era
  , EraPlutusContext 'PlutusV1 era
  ) =>
  EpochInfo (Either Text) ->
  SystemStart ->
  PParams era ->
  Tx era ->
  UTxO era ->
  Either [CollectError era] [(ShortByteString, Language, [Data era], ExUnits, CostModel)]
collectTwoPhaseScriptInputs ei sysS pp tx utxo =
  map unwrap <$> collectPlutusScriptsWithContext ei sysS pp tx utxo
  where
    unwrap (PlutusWithContext (Plutus lang (BinaryPlutus scriptBytes)) args exUnits costModel) =
      (scriptBytes, lang, args, exUnits, costModel)
{-# DEPRECATED collectTwoPhaseScriptInputs "In favor of `collectPlutusScriptsWithContext`" #-}

collectPlutusScriptsWithContext ::
  forall era.
  ( MaryEraTxBody era
  , AlonzoEraTxWits era
  , AlonzoEraUTxO era
  , ScriptsNeeded era ~ AlonzoScriptsNeeded era
  , ExtendedUTxO era
  , Script era ~ AlonzoScript era
  , AlonzoEraPParams era
  , EraPlutusContext 'PlutusV1 era
  ) =>
  EpochInfo (Either Text) ->
  SystemStart ->
  PParams era ->
  Tx era ->
  UTxO era ->
  Either [CollectError era] [PlutusWithContext era]
collectPlutusScriptsWithContext epochInfo sysStart pp tx utxo =
  -- TODO: remove this whole complicated check when we get into Conway. It is much simpler
  -- to fail on a CostModel lookup in the `apply` function (already implemented).
  let usedLanguages = Set.fromList [lang | (_, Plutus lang _) <- neededAndConfirmedToBePlutus]
      missingCostModels = Set.filter (`Map.notMember` costModels) usedLanguages
      -- Check on a protcol version is to preserve failure mode (a single NoCostModel
      -- failure for languages with missing cost models) until we are in Conway era. After
      -- we hard fork into Conway it will be safe to remove this check together with the
      -- `missingCostModels` lookup
      protVerMajor = pvMajor (pp ^. ppProtocolVersionL)
   in case guard (protVerMajor < natVersion @9) >> Set.lookupMin missingCostModels of
        Just l -> Left [NoCostModel l]
        Nothing ->
          merge
            apply
            (map getScriptWithRedeemer neededAndConfirmedToBePlutus)
            (Right [])
  where
    costModels = costModelsValid $ pp ^. ppCostModelsL
    ScriptsProvided scriptsProvided = getScriptsProvided utxo tx
    AlonzoScriptsNeeded scriptsNeeded' = getScriptsNeeded utxo (tx ^. bodyTxL)
    neededAndConfirmedToBePlutus =
      mapMaybe (knownToNotBe1Phase scriptsProvided) scriptsNeeded'
    getScriptWithRedeemer (sp, script) =
      case indexedRdmrs tx sp of
        Just (d, eu) -> Right (script, sp, d, eu)
        Nothing -> Left (NoRedeemer sp)
    apply (script@(Plutus lang _), sp, d, eu) = do
      cm <- maybe (Left (NoCostModel lang)) Right $ Map.lookup lang costModels
      case txInfo pp lang epochInfo sysStart utxo tx of
        Right inf ->
          let datums = maybe id (:) (getSpendingDatum utxo tx sp) [d, valContext inf sp]
           in Right $
                PlutusWithContext
                  { pwcScript = script
                  , pwcDatums = datums
                  , pwcExUnits = eu
                  , pwcCostModel = cm
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

language :: AlonzoScript era -> Maybe Language
language (PlutusScript (Plutus lang _)) = Just lang
language (TimelockScript _) = Nothing

-- | evaluate a list of scripts, All scripts in the list must be True.
--   There are two kinds of scripts, evaluate each kind using the
--   appropriate mechanism.
evalScripts ::
  forall era.
  (EraTx era, Script era ~ AlonzoScript era) =>
  ProtVer ->
  Tx era ->
  [(ShortByteString, Language, [Data era], ExUnits, CostModel)] ->
  ScriptResult
evalScripts pv tx scripts = snd $ evalScriptsWithLogs pv tx scripts

evalScriptsWithLogs ::
  forall era.
  (EraTx era, Script era ~ AlonzoScript era) =>
  ProtVer ->
  Tx era ->
  [(ShortByteString, Language, [Data era], ExUnits, CostModel)] ->
  ([Text], ScriptResult)
evalScriptsWithLogs pv tx scripts =
  evalPlutusScriptsWithLogs pv tx scripts'
  where
    scripts' =
      [ PlutusWithContext (Plutus lang (BinaryPlutus pscript)) ds units cost
      | (pscript, lang, ds, units, cost) <- scripts
      ]
{-# DEPRECATED evalScripts "In favor of `evalPlutusScripts`" #-}

-- | Evaluate a list of Plutus scripts. All scripts in the list must evaluate to `True`.
evalPlutusScripts ::
  (EraTx era, Script era ~ AlonzoScript era) =>
  ProtVer ->
  Tx era ->
  [PlutusWithContext era] ->
  ScriptResult
evalPlutusScripts pv tx pwcs = snd $ evalPlutusScriptsWithLogs pv tx pwcs

evalPlutusScriptsWithLogs ::
  (EraTx era, Script era ~ AlonzoScript era) =>
  ProtVer ->
  Tx era ->
  [PlutusWithContext era] ->
  ([Text], ScriptResult)
evalPlutusScriptsWithLogs _pv _tx [] = mempty
evalPlutusScriptsWithLogs pv tx (plutusWithContext : rest) =
  let beginMsg =
        intercalate
          ","
          [ "[LEDGER][PLUTUS_SCRIPT]"
          , "BEGIN"
          ]
      !res = traceEvent beginMsg $ runPlutusScriptWithLogs pv plutusWithContext
      endMsg =
        intercalate
          ","
          [ "[LEDGER][PLUTUS_SCRIPT]"
          , "END"
          ]
   in traceEvent endMsg res <> evalPlutusScriptsWithLogs pv tx rest

-- Collect information (purpose and ScriptHash) about all the
-- Credentials that refer to scripts, that might be run in a Tx.
-- THE SPEC CALLS FOR A SET, BUT THAT NEEDS A BUNCH OF ORD INSTANCES (TxCert)
-- See additional comments about 'scriptsNeededFromBody' below.
scriptsNeeded ::
  ( EraUTxO era
  , ScriptsNeeded era ~ [(ScriptPurpose (EraCrypto era), ScriptHash (EraCrypto era))]
  ) =>
  UTxO era ->
  Tx era ->
  [(ScriptPurpose (EraCrypto era), ScriptHash (EraCrypto era))]
scriptsNeeded utxo tx = scriptsNeededFromBody utxo (tx ^. bodyTxL)
{-# DEPRECATED scriptsNeeded "In favor of `getScritpsNeeded`" #-}

scriptsNeededFromBody ::
  forall era.
  ( EraUTxO era
  , ScriptsNeeded era ~ [(ScriptPurpose (EraCrypto era), ScriptHash (EraCrypto era))]
  ) =>
  UTxO era ->
  TxBody era ->
  [(ScriptPurpose (EraCrypto era), ScriptHash (EraCrypto era))]
scriptsNeededFromBody = getScriptsNeeded
{-# DEPRECATED scriptsNeededFromBody "In favor of `getScritpsNeeded`" #-}
