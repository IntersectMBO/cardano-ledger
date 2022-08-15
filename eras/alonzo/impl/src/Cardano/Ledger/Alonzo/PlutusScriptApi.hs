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

import Cardano.Binary (FromCBOR (..), ToCBOR (..))
import Cardano.Ledger.Alonzo.Data (getPlutusData)
import Cardano.Ledger.Alonzo.Language (Language (..))
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
import Cardano.Ledger.Alonzo.TxWitness (TxWitness, unTxDats)
import Cardano.Ledger.BaseTypes (ProtVer, StrictMaybe (..))
import Cardano.Ledger.Core hiding (TranslationError)
import Cardano.Ledger.Credential (Credential (ScriptHashObj))
import qualified Cardano.Ledger.Crypto as CC (Crypto)
import Cardano.Ledger.Mary.Value (PolicyID (..))
import Cardano.Ledger.Shelley.Delegation.Certificates (DCert (..))
import Cardano.Ledger.Shelley.TxBody
  ( DelegCert (..),
    Delegation (..),
    ShelleyEraTxBody (..),
    Wdrl (..),
    getRwdCred,
  )
import Cardano.Ledger.Shelley.UTxO (UTxO (..), getScriptHash, scriptCred)
import Cardano.Ledger.TxIn (TxIn (..))
import Cardano.Slotting.EpochInfo (EpochInfo)
import Cardano.Slotting.Time (SystemStart)
import Data.ByteString.Short (ShortByteString)
import Data.Coders
import Data.Foldable (foldl')
import Data.List (intercalate)
import qualified Data.Map.Strict as Map
import Data.Maybe (mapMaybe)
import Data.Proxy (Proxy (..))
import qualified Data.Set as Set
import Data.Text (Text)
import Debug.Trace (traceEvent)
import GHC.Generics
import GHC.Records (HasField (..))
import Lens.Micro
import Lens.Micro.Extras (view)
import NoThunks.Class (NoThunks)

-- ===============================================================
-- From the specification, Figure 8 "Scripts and their Arguments"
-- ===============================================================

-- | Only the Spending ScriptPurpose contains TxIn
getSpendingTxIn :: ScriptPurpose crypto -> Maybe (TxIn crypto)
getSpendingTxIn = \case
  Spending txin -> Just txin
  Minting _policyid -> Nothing
  Rewarding _rewaccnt -> Nothing
  Certifying _dcert -> Nothing

-- | Get the Data associated with a ScriptPurpose. Only the Spending
--   ScriptPurpose contains Data. The null list is returned for the other kinds.
getDatumAlonzo ::
  (AlonzoEraTxOut era, EraTx era, TxWits era ~ TxWitness era) =>
  Tx era ->
  UTxO era ->
  ScriptPurpose (Crypto era) ->
  Maybe (Data era)
getDatumAlonzo tx (UTxO m) sp = do
  txIn <- getSpendingTxIn sp
  txOut <- Map.lookup txIn m
  SJust hash <- Just $ txOut ^. dataHashTxOutL
  Map.lookup hash (unTxDats $ txdats' (tx ^. witsTxL))

-- ========================================================================

-- | When collecting inputs for twophase scripts, 3 things can go wrong.
data CollectError crypto
  = NoRedeemer !(ScriptPurpose crypto)
  | NoWitness !(ScriptHash crypto)
  | NoCostModel !Language
  | BadTranslation !(TranslationError crypto)
  deriving (Eq, Show, Generic, NoThunks)

instance (CC.Crypto crypto) => ToCBOR (CollectError crypto) where
  toCBOR (NoRedeemer x) = encode $ Sum NoRedeemer 0 !> To x
  toCBOR (NoWitness x) = encode $ Sum NoWitness 1 !> To x
  toCBOR (NoCostModel x) = encode $ Sum NoCostModel 2 !> To x
  toCBOR (BadTranslation x) = encode $ Sum BadTranslation 3 !> To x

instance (CC.Crypto crypto) => FromCBOR (CollectError crypto) where
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
  Map.Map (ScriptHash (Crypto era)) (AlonzoScript era) ->
  (ScriptPurpose (Crypto era), ScriptHash (Crypto era)) ->
  Maybe (ScriptPurpose (Crypto era), Language, ShortByteString)
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
    ShelleyEraTxBody era,
    ExtendedUTxO era,
    Script era ~ AlonzoScript era,
    TxWits era ~ TxWitness era,
    HasField "_costmdls" (PParams era) CostModels
  ) =>
  EpochInfo (Either Text) ->
  SystemStart ->
  PParams era ->
  Tx era ->
  UTxO era ->
  Either [CollectError (Crypto era)] [(ShortByteString, Language, [Data era], ExUnits, CostModel)]
collectTwoPhaseScriptInputs ei sysS pp tx utxo =
  let usedLanguages = Set.fromList [lang | (_, lang, _) <- neededAndConfirmedToBePlutus]
      costModels = unCostModels $ getField @"_costmdls" pp
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
    neededAndConfirmedToBePlutus =
      mapMaybe (knownToNotBe1Phase scriptsAvailable) $ scriptsNeeded utxo tx
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
  forall era.
  (EraTx era, ShelleyEraTxBody era) =>
  UTxO era ->
  Tx era ->
  [(ScriptPurpose (Crypto era), ScriptHash (Crypto era))]
scriptsNeeded utxo tx = scriptsNeededFromBody utxo (tx ^. bodyTxL)

-- We only find certificate witnesses in Delegating and Deregistration DCerts
-- that have ScriptHashObj credentials.
addOnlyCwitness ::
  [(ScriptPurpose crypto, ScriptHash crypto)] ->
  DCert crypto ->
  [(ScriptPurpose crypto, ScriptHash crypto)]
addOnlyCwitness !ans (DCertDeleg c@(DeRegKey (ScriptHashObj hk))) =
  (Certifying $ DCertDeleg c, hk) : ans
addOnlyCwitness !ans (DCertDeleg c@(Delegate (Delegation (ScriptHashObj hk) _dpool))) =
  (Certifying $ DCertDeleg c, hk) : ans
addOnlyCwitness !ans _ = ans

-- |
-- Uses of inputs in ‘txscripts’ and ‘neededScripts’
-- There are currently 3 sets of inputs (spending, collateral, reference). A particular TxInput
-- can appear in more than one of the sets. Even in all three at the same, but that may not be
-- a really useful case. Inputs are where you find scripts with the 'Spending' purpose.
--
-- 1) Collateral inputs are only spent if phase two fails. Their corresponding TxOut can only have
--    Key (not Script) Pay credentials, so ‘neededScripts’ does not look there.
-- 2) Reference inputs are not spent in the current Tx, unless that same input also appears in one
--    of the other sets. If that is not the case, their credentials are never needed, so anyone can
--    access the inline datums and scripts in their corresponding TxOut, without needing any
--    authorizing credentials. So ‘neededScripts’ does not look there.
-- 3) Spending inputs are always spent. So their Pay credentials are always needed.
--
-- Collect information (purpose and ScriptHash) about all the Credentials that refer to scripts
-- that will be needed to run in a TxBody in the Utxow rule. Note there may be credentials that
-- cannot be run, so are not collected. In Babbage, reference inputs, fit that description.
-- Purposes include
-- 1) Spending (payment script credentials, but NOT staking scripts) in the Addr of a TxOut, pointed
--    to by some input that needs authorization. Be sure (getField @"inputs" txb) gets all such inputs.
--    In some Eras there may be multiple sets of inputs, which ones should be included? Currently that
--    is only the spending inputs. Because collateral inputs can only have key-locked credentials,
--    and reference inputs are never authorized. That might not always be the case.
-- 2) Rewarding (Withdrawals),
-- 3) Minting (minted field), and
-- 4) Certifying (Delegating) scripts.
--
-- 'scriptsNeeded' is an aggregation of the needed Credentials referring to Scripts used in Utxow rule.
-- The flip side of 'scriptsNeeded' (which collects script hashes) is 'txscripts' which finds the
-- actual scripts. We maintain an invariant that every script credential refers to some actual script.
-- This is tested in the test function 'validateMissingScripts' in the Utxow rule.
scriptsNeededFromBody ::
  forall era.
  (ShelleyEraTxBody era) =>
  UTxO era ->
  TxBody era ->
  [(ScriptPurpose (Crypto era), ScriptHash (Crypto era))]
scriptsNeededFromBody (UTxO u) txBody = spend ++ reward ++ cert ++ minted
  where
    collect :: TxIn (Crypto era) -> Maybe (ScriptPurpose (Crypto era), ScriptHash (Crypto era))
    collect !i = do
      addr <- view addrTxOutL <$> Map.lookup i u
      hash <- getScriptHash addr
      return (Spending i, hash)

    !spend = mapMaybe collect (Set.toList $ txBody ^. inputsTxBodyL)

    !reward = mapMaybe fromRwd (Map.keys withdrawals)
      where
        withdrawals = unWdrl $ txBody ^. wdrlsTxBodyL
        fromRwd accnt = do
          hash <- scriptCred $ getRwdCred accnt
          return (Rewarding accnt, hash)

    !cert = foldl' addOnlyCwitness [] (txBody ^. certsTxBodyL)

    !minted = map (\hash -> (Minting (PolicyID hash), hash)) $ Set.toList $ txBody ^. mintedTxBodyF
