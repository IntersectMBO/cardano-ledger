{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

module Cardano.Ledger.Alonzo.PlutusScriptApi
  ( -- Figure 8
    getSpendingTxIn,
    getDatumAlonzo,
    evalScripts,
    -- Figure 12
    scriptsNeeded,
    scriptsNeededFromBody,
    language,
    CollectError (..),
    collectTwoPhaseScriptInputs,
    knownToNotBe1Phase,
  )
where

import Cardano.Ledger.Alonzo.Core (MaryEraTxBody (..))
import Cardano.Ledger.Alonzo.Data (getPlutusData)
import Cardano.Ledger.Alonzo.Language (Language (..))
import Cardano.Ledger.Alonzo.PParams
import Cardano.Ledger.Alonzo.Scripts (AlonzoScript (..), CostModel, CostModels (..), ExUnits (..))
import Cardano.Ledger.Alonzo.Tx (Data, ScriptPurpose (..), indexedRdmrs, txdats')
import Cardano.Ledger.Alonzo.TxBody (AlonzoEraTxOut (..))
import Cardano.Ledger.Alonzo.TxInfo
  ( ExtendedUTxO (..),
    ScriptResult (..),
    TranslationError (..),
    runPLCScript,
    valContext,
  )
import Cardano.Ledger.Alonzo.TxWits (AlonzoEraTxWits (..), AlonzoTxWits, unTxDats)
import Cardano.Ledger.Alonzo.UTxO (AlonzoScriptsNeeded (..))
import Cardano.Ledger.BaseTypes (ProtVer, StrictMaybe (..))
import Cardano.Ledger.Binary (FromCBOR (..), ToCBOR (..))
import Cardano.Ledger.Binary.Coders
import Cardano.Ledger.Core hiding (TranslationError)
import Cardano.Ledger.Crypto (Crypto)
import Cardano.Ledger.TxIn (TxIn (..))
import Cardano.Ledger.UTxO (EraUTxO (..), UTxO (..))
import Cardano.Slotting.EpochInfo (EpochInfo)
import Cardano.Slotting.Time (SystemStart)
import Data.ByteString.Short (ShortByteString)
import Data.List (intercalate)
import qualified Data.Map.Strict as Map
import Data.Maybe (mapMaybe)
import Data.Proxy (Proxy (..))
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
getSpendingTxIn :: ScriptPurpose c -> Maybe (TxIn c)
getSpendingTxIn = \case
  Spending txin -> Just txin
  Minting _policyid -> Nothing
  Rewarding _rewaccnt -> Nothing
  Certifying _dcert -> Nothing

-- | Get the Data associated with a ScriptPurpose. Only the Spending
--   ScriptPurpose contains Data. The null list is returned for the other kinds.
getDatumAlonzo ::
  (AlonzoEraTxOut era, EraTx era, TxWits era ~ AlonzoTxWits era) =>
  Tx era ->
  UTxO era ->
  ScriptPurpose (EraCrypto era) ->
  Maybe (Data era)
getDatumAlonzo tx (UTxO m) sp = do
  txIn <- getSpendingTxIn sp
  txOut <- Map.lookup txIn m
  SJust hash <- Just $ txOut ^. dataHashTxOutL
  Map.lookup hash (unTxDats $ txdats' (tx ^. witsTxL))

-- ========================================================================

-- | When collecting inputs for twophase scripts, 3 things can go wrong.
data CollectError c
  = NoRedeemer !(ScriptPurpose c)
  | NoWitness !(ScriptHash c)
  | NoCostModel !Language
  | BadTranslation !(TranslationError c)
  deriving (Eq, Show, Generic, NoThunks)

instance (Crypto c) => ToCBOR (CollectError c) where
  toCBOR (NoRedeemer x) = encode $ Sum NoRedeemer 0 !> To x
  toCBOR (NoWitness x) = encode $ Sum NoWitness 1 !> To x
  toCBOR (NoCostModel x) = encode $ Sum NoCostModel 2 !> To x
  toCBOR (BadTranslation x) = encode $ Sum BadTranslation 3 !> To x

instance (Crypto c) => FromCBOR (CollectError c) where
  fromCBOR = decode (Summands "CollectError" dec)
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
  (ScriptPurpose (EraCrypto era), ScriptHash (EraCrypto era)) ->
  Maybe (ScriptPurpose (EraCrypto era), Language, ShortByteString)
knownToNotBe1Phase scriptsAvailable (sp, sh) = do
  PlutusScript lang script <- sh `Map.lookup` scriptsAvailable
  Just (sp, lang, script)

-- | Collect the inputs for twophase scripts. If any script can't find ist data return
--     a list of CollectError, if all goes well return a list of quadruples with the inputs.
--     Previous PredicateFailure tests should ensure we find Data for every script, BUT
--     the consequences of not finding Data means scripts can get dropped, so things
--     might validate that shouldn't. So we double check that every Script has its Data, and
--     if that is not the case, a PredicateFailure is raised in the Utxos rule.
collectTwoPhaseScriptInputs ::
  forall era.
  ( EraTx era,
    MaryEraTxBody era,
    AlonzoEraTxWits era,
    EraUTxO era,
    ScriptsNeeded era ~ AlonzoScriptsNeeded era,
    ExtendedUTxO era,
    Script era ~ AlonzoScript era,
    AlonzoEraPParams era
  ) =>
  EpochInfo (Either Text) ->
  SystemStart ->
  PParams era ->
  Tx era ->
  UTxO era ->
  Either [CollectError (EraCrypto era)] [(ShortByteString, Language, [Data era], ExUnits, CostModel)]
collectTwoPhaseScriptInputs ei sysS pp tx utxo =
  let usedLanguages = Set.fromList [lang | (_, lang, _) <- neededAndConfirmedToBePlutus]
      costModels = unCostModels $ pp ^. ppCostModelsL
      missingCMs = Set.filter (`Map.notMember` costModels) usedLanguages
   in case Set.lookupMin missingCMs of
        Just l -> Left [NoCostModel l]
        Nothing ->
          merge
            (apply costModels)
            (map redeemer neededAndConfirmedToBePlutus)
            (map getscript neededAndConfirmedToBePlutus)
            (Right [])
  where
    scriptsAvailable = txscripts utxo tx
    txinfo lang = txInfo pp lang ei sysS utxo tx
    AlonzoScriptsNeeded scriptsNeeded' = getScriptsNeeded utxo (tx ^. bodyTxL)
    neededAndConfirmedToBePlutus =
      mapMaybe (knownToNotBe1Phase scriptsAvailable) scriptsNeeded'
    redeemer (sp, lang, _) =
      case indexedRdmrs tx sp of
        Just (d, eu) -> Right (lang, sp, d, eu)
        Nothing -> Left (NoRedeemer sp)
    getscript (_, _, script) = script
    apply costs (lang, sp, d, eu) script =
      case txinfo lang of
        Right inf ->
          let datums = maybe id (:) (getDatum tx utxo sp) [d, valContext inf sp]
           in Right (script, lang, datums, eu, costs Map.! lang)
        Left te -> Left $ BadTranslation te

-- | Merge two lists (the first of which may have failures, i.e. (Left _)), collect all the failures
--   but if there are none, use 'f' to construct a success.
merge :: forall t1 t2 a1 a2. (t1 -> t2 -> Either a2 a1) -> [Either a2 t1] -> [t2] -> Either [a2] [a1] -> Either [a2] [a1]
merge _f [] [] answer = answer
merge _f [] (_ : _) answer = answer
merge _f (_ : _) [] answer = answer
merge f (x : xs) (y : ys) zs = merge f xs ys (gg x y zs)
  where
    gg :: Either a2 t1 -> t2 -> Either [a2] [a1] -> Either [a2] [a1]
    gg (Right a) b (Right cs) =
      case f a b of
        Right c -> Right $ c : cs
        Left e -> Left [e]
    gg (Left a) _ (Right _) = Left [a]
    gg (Right _) _ (Left cs) = Left cs
    gg (Left a) _ (Left cs) = Left (a : cs)

language :: AlonzoScript era -> Maybe Language
language (PlutusScript lang _) = Just lang
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
evalScripts _pv _tx [] = mempty
evalScripts pv tx ((pscript, lang, ds, units, cost) : rest) =
  let beginMsg =
        intercalate
          ","
          [ "[LEDGER][PLUTUS_SCRIPT]",
            "BEGIN"
          ]
      !res = traceEvent beginMsg $ runPLCScript (Proxy @era) pv lang cost pscript units (map getPlutusData ds)
      endMsg =
        intercalate
          ","
          [ "[LEDGER][PLUTUS_SCRIPT]",
            "END"
          ]
   in traceEvent endMsg res <> evalScripts pv tx rest

-- Collect information (purpose and ScriptHash) about all the
-- Credentials that refer to scripts, that might be run in a Tx.
-- THE SPEC CALLS FOR A SET, BUT THAT NEEDS A BUNCH OF ORD INSTANCES (DCert)
-- See additional comments about 'scriptsNeededFromBody' below.
scriptsNeeded ::
  ( EraTx era,
    EraUTxO era,
    ScriptsNeeded era ~ [(ScriptPurpose (EraCrypto era), ScriptHash (EraCrypto era))]
  ) =>
  UTxO era ->
  Tx era ->
  [(ScriptPurpose (EraCrypto era), ScriptHash (EraCrypto era))]
scriptsNeeded utxo tx = scriptsNeededFromBody utxo (tx ^. bodyTxL)
{-# DEPRECATED scriptsNeeded "In favor of `getScritpsNeeded`" #-}

scriptsNeededFromBody ::
  forall era.
  ( EraUTxO era,
    ScriptsNeeded era ~ [(ScriptPurpose (EraCrypto era), ScriptHash (EraCrypto era))]
  ) =>
  UTxO era ->
  TxBody era ->
  [(ScriptPurpose (EraCrypto era), ScriptHash (EraCrypto era))]
scriptsNeededFromBody = getScriptsNeeded
{-# DEPRECATED scriptsNeededFromBody "In favor of `getScritpsNeeded`" #-}
