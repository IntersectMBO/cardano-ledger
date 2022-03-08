{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

module Cardano.Ledger.Alonzo.PlutusScriptApi
  ( -- Figure 8
    getData,
    evalScripts,
    -- Figure 12
    scriptsNeeded,
    scriptsNeededFromBody,
    language,
    CollectError (..),
    collectTwoPhaseScriptInputs,
  )
where

import Cardano.Binary (FromCBOR (..), ToCBOR (..))
import Cardano.Ledger.Alonzo.Data (getPlutusData)
import Cardano.Ledger.Alonzo.Language (Language (..))
import Cardano.Ledger.Alonzo.Scripts (CostModel (..), ExUnits (..))
import qualified Cardano.Ledger.Alonzo.Scripts as AlonzoScript (Script (..))
import Cardano.Ledger.Alonzo.Tx
  ( Data,
    DataHash,
    ScriptPurpose (..),
    indexedRdmrs,
    txdats',
  )
import Cardano.Ledger.Alonzo.TxInfo
  ( ExtendedUTxO (..),
    FailureDescription (..),
    ScriptResult (..),
    TranslationError (..),
    andResult,
    runPLCScript,
    valContext,
  )
import Cardano.Ledger.Alonzo.TxWitness (TxWitness (txwitsVKey'), unTxDats)
import Cardano.Ledger.BaseTypes (StrictMaybe (..))
import qualified Cardano.Ledger.Core as Core
import Cardano.Ledger.Credential (Credential (ScriptHashObj))
import qualified Cardano.Ledger.Crypto as CC (Crypto)
import Cardano.Ledger.Era (Era (..))
import Cardano.Ledger.Mary.Value (PolicyID (..))
import Cardano.Ledger.Shelley.Delegation.Certificates (DCert (..))
import Cardano.Ledger.Shelley.Scripts (ScriptHash (..))
import Cardano.Ledger.Shelley.TxBody
  ( DelegCert (..),
    Delegation (..),
    Wdrl (..),
    getRwdCred,
    witKeyHash,
  )
import Cardano.Ledger.Shelley.UTxO (UTxO (..), getScriptHash, scriptCred)
import Cardano.Ledger.ShelleyMA.Timelocks (evalTimelock)
import Cardano.Ledger.ShelleyMA.TxBody (ValidityInterval)
import Cardano.Ledger.TxIn (TxIn (..))
import Cardano.Slotting.EpochInfo (EpochInfo)
import Cardano.Slotting.Time (SystemStart)
import Data.Coders
import qualified Data.Compact.SplitMap as SplitMap
import Data.Foldable (foldl')
import Data.Functor.Identity (Identity, runIdentity)
import Data.List (intercalate)
import qualified Data.Map as Map
import Data.Maybe (mapMaybe)
import Data.Proxy (Proxy (..))
import Data.Sequence.Strict (StrictSeq)
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Text (pack)
import Debug.Trace (traceEvent)
import GHC.Generics
import GHC.Records (HasField (..))
import NoThunks.Class (NoThunks)

-- ===============================================================
-- From the specification, Figure 8 "Scripts and their Arguments"
-- ===============================================================

-- | Get the Data associated with a ScriptPurpose. Only the Spending
--   ScriptPurpose contains Data. The null list is returned for the other kinds.
getData ::
  forall era tx.
  ( HasField "datahash" (Core.TxOut era) (StrictMaybe (DataHash (Crypto era))),
    HasField "wits" tx (TxWitness era)
  ) =>
  tx ->
  UTxO era ->
  ScriptPurpose (Crypto era) ->
  [Data era]
getData tx (UTxO m) sp = case sp of
  Minting _policyid -> []
  Rewarding _rewaccnt -> []
  Certifying _dcert -> []
  Spending txin ->
    -- Only the Spending ScriptPurpose contains Data
    case SplitMap.lookup txin m of
      Nothing -> []
      Just txout ->
        case getField @"datahash" txout of
          SNothing -> []
          SJust hash ->
            case Map.lookup hash (unTxDats $ txdats' (getField @"wits" tx)) of
              Nothing -> []
              Just d -> [d]

-- ========================================================================

-- | When collecting inputs for twophase scripts, 3 things can go wrong.
data CollectError crypto
  = NoRedeemer !(ScriptPurpose crypto)
  | NoWitness !(ScriptHash crypto)
  | NoCostModel !Language
  | BadTranslation !TranslationError
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

-- | Collect the inputs for twophase scripts. If any script can't find ist data return
--     a list of CollectError, if all goes well return a list of quadruples with the inputs.
--     Previous PredicateFailure tests should ensure we find Data for every script, BUT
--     the consequences of not finding Data means scripts can get dropped, so things
--     might validate that shouldn't. So we double check that every Script has its Data, and
--     if that is not the case, a PredicateFailure is raised in the Utxos rule.
--
--   NOTE that 'runIdentity $ txInfo ei sysS utxo tx' will fail when the validity interval
--     of the transaction 'tx' is beyond the time horizon, ie when
--     'epochInfoSlotToUTCTime ei sysS validityInterval' returns Left.
--     Therefore collectTwoPhaseScriptInputs must only be called after checking
--     that the transaction is within the time horizon.
collectTwoPhaseScriptInputs ::
  forall era.
  ( Era era,
    ExtendedUTxO era,
    Core.Script era ~ AlonzoScript.Script era,
    HasField "datahash" (Core.TxOut era) (StrictMaybe (DataHash (Crypto era))),
    HasField "_costmdls" (Core.PParams era) (Map.Map Language CostModel),
    HasField "wdrls" (Core.TxBody era) (Wdrl (Crypto era)),
    HasField "certs" (Core.TxBody era) (StrictSeq (DCert (Crypto era))),
    HasField "inputs" (Core.TxBody era) (Set (TxIn (Crypto era))),
    HasField "wits" (Core.Tx era) (TxWitness era)
  ) =>
  EpochInfo Identity ->
  SystemStart ->
  Core.PParams era ->
  Core.Tx era ->
  UTxO era ->
  Either [CollectError (Crypto era)] [(AlonzoScript.Script era, [Data era], ExUnits, CostModel)]
collectTwoPhaseScriptInputs ei sysS pp tx utxo =
  let usedLanguages = [lang | (AlonzoScript.PlutusScript lang _) <- Map.elems scriptsUsed]
      costModels = getField @"_costmdls" pp
      missingCMs = [lang | lang <- usedLanguages, lang `Map.notMember` costModels]
   in case missingCMs of
        l : _ -> Left [NoCostModel l]
        _ -> merge (apply costModels) (map redeemer needed) (map getscript needed) (Right [])
  where
    scriptsUsed = txscripts utxo tx
    txinfo lang = runIdentity $ txInfo pp lang ei sysS utxo tx
    needed = filter knownToNotBe1Phase $ scriptsNeeded utxo tx
    -- The formal spec achieves the same filtering as knownToNotBe1Phase
    -- by use of the (partial) language function, which is not defined
    -- on 1-phase scripts.
    knownToNotBe1Phase (_, sh) =
      case sh `Map.lookup` scriptsUsed of
        Just (AlonzoScript.PlutusScript _ _) -> True
        Just (AlonzoScript.TimelockScript _) -> False
        Nothing -> True
    redeemer (sp, _) =
      case indexedRdmrs tx sp of
        Just (d, eu) -> Right (sp, d, eu)
        Nothing -> Left (NoRedeemer sp)
    getscript (_, hash) =
      case hash `Map.lookup` scriptsUsed of
        Just script -> Right script
        Nothing -> Left (NoWitness hash)
    apply costs (sp, d, eu) script@(AlonzoScript.PlutusScript lang _) =
      case txinfo lang of
        Right inf -> Right (script, getData tx utxo sp ++ (d : [valContext inf sp]), eu, costs Map.! lang)
        Left te -> Left $ BadTranslation te
    apply _ (_, _, eu) script = Right (script, [], eu, CostModel mempty)

-- | Merge two lists (either of which may have failures, i.e. (Left _)), collect all the failures
--   but if there are none, use 'f' to construct a success.
merge :: forall t1 t2 a1 a2. (t1 -> t2 -> Either a2 a1) -> [Either a2 t1] -> [Either a2 t2] -> Either [a2] [a1] -> Either [a2] [a1]
merge _f [] [] answer = answer
merge _f [] (_ : _) answer = answer
merge _f (_ : _) [] answer = answer
merge f (x : xs) (y : ys) zs = merge f xs ys (gg x y zs)
  where
    gg :: Either a2 t1 -> Either a2 t2 -> Either [a2] [a1] -> Either [a2] [a1]
    gg (Right a) (Right b) (Right cs) =
      case f a b of
        Right c -> Right $ c : cs
        Left e -> Left [e]
    gg (Left a) (Right _) (Right _) = Left [a]
    gg (Right _) (Left b) (Right _) = Left [b]
    gg (Left a) (Left b) (Right _) = Left [a, b]
    gg (Right _) (Right _) (Left cs) = Left cs
    gg (Right _) (Left b) (Left cs) = Left (b : cs)
    gg (Left a) (Right _) (Left cs) = Left (a : cs)
    gg (Left a) (Left b) (Left cs) = Left (a : b : cs)

language :: AlonzoScript.Script era -> Maybe Language
language (AlonzoScript.PlutusScript lang _) = Just lang
language (AlonzoScript.TimelockScript _) = Nothing

-- | evaluate a list of scripts, All scripts in the list must be True.
--   There are two kinds of scripts, evaluate each kind using the
--   appropriate mechanism.
evalScripts ::
  forall era tx.
  ( Era era,
    Show (AlonzoScript.Script era),
    HasField "body" tx (Core.TxBody era),
    HasField "wits" tx (TxWitness era),
    HasField "vldt" (Core.TxBody era) ValidityInterval
  ) =>
  tx ->
  [(AlonzoScript.Script era, [Data era], ExUnits, CostModel)] ->
  ScriptResult
evalScripts _tx [] = Passes
evalScripts tx ((AlonzoScript.TimelockScript timelock, _, _, _) : rest) =
  lift (evalTimelock vhks (getField @"vldt" (getField @"body" tx)) timelock)
    `andResult` evalScripts tx rest
  where
    vhks = Set.map witKeyHash (txwitsVKey' (getField @"wits" tx))
    lift True = Passes
    lift False = Fails [OnePhaseFailure . pack . show $ timelock]
evalScripts tx ((AlonzoScript.PlutusScript lang pscript, ds, units, cost) : rest) =
  let beginMsg =
        intercalate
          ","
          [ "[LEDGER][PLUTUS_SCRIPT]",
            "BEGIN"
          ]
      !res = traceEvent beginMsg $ runPLCScript (Proxy @era) lang cost pscript units (map getPlutusData ds)
      endMsg =
        intercalate
          ","
          [ "[LEDGER][PLUTUS_SCRIPT]",
            "END",
            "res = " <> show res
          ]
   in (traceEvent endMsg res) `andResult` evalScripts tx rest

-- Collect information (purpose and hash) about all the scripts in a Tx.
-- THE SPEC CALLS FOR A SET, BUT THAT NEEDS A BUNCH OF ORD INSTANCES (DCert)
scriptsNeeded ::
  forall era tx.
  ( Era era,
    HasField "inputs" (Core.TxBody era) (Set (TxIn (Crypto era))),
    HasField "wdrls" (Core.TxBody era) (Wdrl (Crypto era)),
    HasField "certs" (Core.TxBody era) (StrictSeq (DCert (Crypto era))),
    HasField "body" tx (Core.TxBody era)
  ) =>
  UTxO era ->
  tx ->
  [(ScriptPurpose (Crypto era), ScriptHash (Crypto era))]
scriptsNeeded utxo tx = scriptsNeededFromBody utxo (getField @"body" tx)

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

scriptsNeededFromBody ::
  forall era.
  ( Era era,
    HasField "inputs" (Core.TxBody era) (Set (TxIn (Crypto era))),
    HasField "wdrls" (Core.TxBody era) (Wdrl (Crypto era)),
    HasField "certs" (Core.TxBody era) (StrictSeq (DCert (Crypto era)))
  ) =>
  UTxO era ->
  Core.TxBody era ->
  [(ScriptPurpose (Crypto era), ScriptHash (Crypto era))]
scriptsNeededFromBody (UTxO u) txb = spend ++ reward ++ cert ++ minted
  where
    !spend = mapMaybe collect (Set.toList $ getField @"inputs" txb)
      where
        collect :: TxIn (Crypto era) -> Maybe (ScriptPurpose (Crypto era), ScriptHash (Crypto era))
        collect !i = do
          addr <- getTxOutAddr <$> SplitMap.lookup i u
          hash <- getScriptHash addr
          return (Spending i, hash)

    !reward = mapMaybe fromRwd (Map.keys withdrawals)
      where
        withdrawals = unWdrl $ getField @"wdrls" txb
        fromRwd accnt = do
          hash <- scriptCred $ getRwdCred accnt
          return (Rewarding accnt, hash)

    !cert = foldl' addOnlyCwitness [] (getField @"certs" txb)

    !minted = map (\hash -> (Minting (PolicyID hash), hash)) $ Set.toList $ getField @"minted" txb
