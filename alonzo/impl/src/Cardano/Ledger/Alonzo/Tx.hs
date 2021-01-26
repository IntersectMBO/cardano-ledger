{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

-- | This module exports implementations of many of the functions outlined in the Alonzo specification.
--     The link to source of the specification
--       https://github.com/input-output-hk/cardano-ledger-specs/tree/master/alonzo/formal-spec
--     The most recent version of the document can be found here:
--       https://hydra.iohk.io/job/Cardano/cardano-ledger-specs/specs.alonzo-ledger/latest/download-by-type/doc-pdf/alonzo-changes
--     The functions can be found in Figures in that document, and sections of this code refer to those figures.
module Cardano.Ledger.Alonzo.Tx
  ( Indexable (..),
    -- Figure 1
    CostModel,
    PPHash,
    hashLanguagePP,
    -- Figure 2
    ScriptData,
    ScriptDataHash,
    Data,
    DataHash,
    IsValidating (..),
    hashData,
    language,
    plutusLanguage,
    timelockLanguage,
    nonNativeLanguages,
    hashScriptData,
    getCoin,
    -- Figure 3
    Tx (Tx, body, wits, isValidating, auxiliaryData),
    TxBody (..),
    -- Figure 4
    ScriptPurpose (..),
    --  Figure 5
    getValidatorHash,
    txscriptfee,
    txbody,
    minfee,
    isNonNativeScriptAddress,
    feesOK,
    -- Figure 6
    txrdmrs,
    rdptr,
    getMapFromValue,
    indexedRdmrs,
    -- Figure 7
    valContext,
    runPLCScript,
    -- Figure 8
    getData,
    collectNNScriptInputs,
    evalScripts,
    -- Figure 12
    scriptsNeeded,
    checkScriptData,
    hashSD,
  )
where

import Cardano.Binary (FromCBOR (..), ToCBOR (..))
import Cardano.Ledger.Alonzo.Data (Data, DataHash, hashData)
import Cardano.Ledger.Alonzo.PParams (PPHash, PParams, PParams' (..), hashLanguagePP)
import Cardano.Ledger.Alonzo.Scripts (CostModel, ExUnits (..), Language (..), Prices (..))
import qualified Cardano.Ledger.Alonzo.Scripts as AlonzoScript (Script (..), Tag (..))
import Cardano.Ledger.Alonzo.TxBody
  ( AlonzoBody,
    TxBody (..),
    TxOut (..),
  )
import Cardano.Ledger.Alonzo.TxWitness
  ( RdmrPtr (..),
    ScriptData (..),
    ScriptDataHash (..),
    TxWitness (..),
    hashSD,
    witsData,
    witsRdmr,
    witsScript,
  )
import Cardano.Ledger.Compactible
import qualified Cardano.Ledger.Core as Core
import Cardano.Ledger.Era (Crypto, Era)
import Cardano.Ledger.Mary.Value (AssetName, PolicyID (..), Value (..))
import Cardano.Ledger.Shelley.Constraints
import Cardano.Ledger.Val (DecodeMint, DecodeNonNegative, Val (coin, (<+>), (<×>)))
import Control.SetAlgebra (eval, (◁))
import qualified Data.ByteString.Short as SBS (length)
import Data.Coders
import Data.List (foldl')
import qualified Data.Map as Map
import Data.Maybe (isJust, maybeToList)
import Data.MemoBytes (Mem, MemoBytes (Memo), memoBytes)
import Data.Sequence.Strict (StrictSeq)
import qualified Data.Sequence.Strict as StrictSeq
import Data.Set (Set)
import qualified Data.Set as Set
  ( elemAt,
    empty,
    findIndex,
    insert,
    map,
    null,
    union,
  )
import Data.Text.Encoding (encodeUtf8)
import Data.Typeable (Typeable)
import Data.Word (Word64)
import GHC.Generics (Generic)
import GHC.Records (HasField (..))
import NoThunks.Class (NoThunks)
import Shelley.Spec.Ledger.Address (Addr (..), RewardAcnt, getRwdCred)
import Shelley.Spec.Ledger.BaseTypes (StrictMaybe, maybeToStrictMaybe, strictMaybeToMaybe)
import Shelley.Spec.Ledger.Coin (Coin (..))
import Shelley.Spec.Ledger.Credential (Credential (ScriptHashObj))
import Shelley.Spec.Ledger.Delegation.Certificates (DCert (..))
import Shelley.Spec.Ledger.Scripts (ScriptHash)
import Shelley.Spec.Ledger.Tx (ValidateScript (isNativeScript))
import Shelley.Spec.Ledger.TxBody (DelegCert (..), Delegation (..), TxIn (..), Wdrl (..), unWdrl)
import Shelley.Spec.Ledger.UTxO (UTxO (..), balance)

-- ===================================================

-- | Tag indicating whether non-native scripts in this transaction are expected
-- to validate. This is added by the block creator when constructing the block.
newtype IsValidating = IsValidating Bool
  deriving (Eq, NoThunks, Show)

data TxRaw era = TxRaw
  { _body :: !(TxBody era),
    _wits :: !(TxWitness era),
    _isValidating :: !IsValidating,
    _auxiliaryData :: !(StrictMaybe (Core.AuxiliaryData era))
  }
  deriving (Generic, Typeable)

deriving instance
  ( Era era,
    Eq (Core.AuxiliaryData era),
    Eq (Core.Script era),
    Eq (Core.Value era),
    Compactible (Core.Value era)
  ) =>
  Eq (TxRaw era)

deriving instance
  ( Era era,
    Compactible (Core.Value era),
    Show (Core.AuxiliaryData era),
    Show (Core.Script era),
    Show (Core.Value era)
  ) =>
  Show (TxRaw era)

instance
  ( Era era,
    NoThunks (Core.AuxiliaryData era),
    NoThunks (Core.Script era),
    NoThunks (Core.Value era)
  ) =>
  NoThunks (TxRaw era)

newtype Tx era = TxConstr (MemoBytes (TxRaw era))
  deriving (ToCBOR)

deriving newtype instance
  ( Era era,
    Eq (Core.AuxiliaryData era),
    Eq (Core.Script era),
    Eq (Core.Value era),
    Compactible (Core.Value era)
  ) =>
  Eq (Tx era)

deriving newtype instance
  ( Era era,
    Compactible (Core.Value era),
    Show (Core.AuxiliaryData era),
    Show (Core.Script era),
    Show (Core.Value era)
  ) =>
  Show (Tx era)

deriving newtype instance
  ( Era era,
    NoThunks (Core.AuxiliaryData era),
    NoThunks (Core.Script era),
    NoThunks (Core.Value era)
  ) =>
  NoThunks (Tx era)

pattern Tx ::
  (Era era, ToCBOR (Core.AuxiliaryData era)) =>
  TxBody era ->
  TxWitness era ->
  IsValidating ->
  StrictMaybe (Core.AuxiliaryData era) ->
  Tx era
pattern Tx {body, wits, isValidating, auxiliaryData} <-
  TxConstr
    ( Memo
        TxRaw
          { _body = body,
            _wits = wits,
            _isValidating = isValidating,
            _auxiliaryData = auxiliaryData
          }
        _
      )
  where
    Tx b w v a = TxConstr $ memoBytes (encodeTxRaw $ TxRaw b w v a)

--------------------------------------------------------------------------------
-- Serialisation
--------------------------------------------------------------------------------

deriving newtype instance FromCBOR IsValidating

deriving newtype instance ToCBOR IsValidating

encodeTxRaw ::
  (Era era, ToCBOR (Core.AuxiliaryData era)) =>
  TxRaw era ->
  Encode ('Closed 'Dense) (TxRaw era)
encodeTxRaw TxRaw {_body, _wits, _isValidating, _auxiliaryData} =
  Rec TxRaw
    !> To _body
    !> To _wits
    !> To _isValidating
    !> E (encodeNullMaybe toCBOR . strictMaybeToMaybe) _auxiliaryData

instance
  ( Era era,
    FromCBOR (Annotator (Core.Script era)),
    FromCBOR (Annotator (Core.AuxiliaryData era)),
    ToCBOR (Core.Script era),
    Typeable (Core.Script era),
    Typeable (Core.AuxiliaryData era),
    Compactible (Core.Value era),
    DecodeNonNegative (Core.Value era),
    DecodeMint (Core.Value era),
    Show (Core.Value era),
    Val (Core.Value era)
  ) =>
  FromCBOR (Annotator (TxRaw era))
  where
  fromCBOR =
    decode $
      Ann (RecD TxRaw)
        <*! From
        <*! From
        <*! Ann From
        <*! D
          ( sequence . maybeToStrictMaybe
              <$> decodeNullMaybe fromCBOR
          )

deriving via
  Mem (TxRaw era)
  instance
    ( Era era,
      FromCBOR (Annotator (Core.Script era)),
      FromCBOR (Annotator (Core.AuxiliaryData era)),
      ToCBOR (Core.Script era),
      Typeable (Core.Script era),
      Typeable (Core.AuxiliaryData era),
      Compactible (Core.Value era),
      DecodeNonNegative (Core.Value era),
      DecodeMint (Core.Value era),
      Show (Core.Value era),
      Val (Core.Value era)
    ) =>
    FromCBOR (Annotator (Tx era))

-- =========================================================
-- Figure 2: Definitions for Transactions

-- For now, the only Non-Native Scriting language is Plutus
-- We might add new languages in the futures.

nonNativeLanguages :: Set Language
nonNativeLanguages = Set.insert plutusLanguage Set.empty

plutusLanguage :: Language
plutusLanguage = Language (encodeUtf8 "Plutus")

timelockLanguage :: Language
timelockLanguage = Language (encodeUtf8 "Timelock")

language :: AlonzoScript.Script era -> Language
language (AlonzoScript.NativeScript _) = timelockLanguage
language (AlonzoScript.PlutusScript) = plutusLanguage

getCoin :: UsesValue era => TxOut era -> Coin
getCoin (TxOut _ v _) = coin v

{-
-- TODO fix this.  A ScriptDataHash is a hash of a Virtual triple.
-- See the selector function sdHash in TxWitness
-- In figure 14 we use this selector in the precondition
-- sdHash txb ==  hashScriptData pp ( languages txw ) ( txrdmrs txw )
-- We have two functions that produce a ScriptDataHash
-- 1) hashSD :: TxWitness -> Maybe(ScriptDataHash)  (Figure 12)
-- 2) hashScriptData :: PParams era -> Set Language ->
--       Map.Map RdmrPtr (Data era) -> Maybe (ScriptDataHash era) (Figure 2)
-- These two functions are not computing the same thing at all.

hashSD ::
  (Era era, ToCBOR (Core.Script era)) =>
  TxWitness era ->
  Maybe (ScriptDataHash (Crypto era))
hashSD (w@(TxWitnessConstr (Memo (TxWitnessRaw _ _ scriptdata) _))) =
  if (Map.null (witsScript w) && Map.null (witsData w) && Map.null (witsRdmr w))
    then Nothing
    else Just (ScriptDataHash (hashAnnotated scriptdata))
-}

hashScriptData ::
  Era era =>
  PParams era ->
  Set Language ->
  Map.Map RdmrPtr (Data era) ->
  Maybe (ScriptDataHash era)
hashScriptData pp langs rdmrs =
  if Map.null rdmrs && Set.null langs
    then Nothing
    else
      let _newset = Set.map (hashLanguagePP pp) langs
       in undefined -- hash(rdmrs,_newset)

-- ===============================================================
-- From the specification, Figure 5 "Functions related to fees"
-- ===============================================================

isNonNativeScriptAddress ::
  forall era.
  ValidateScript era =>
  Tx era ->
  Addr (Crypto era) ->
  Bool
isNonNativeScriptAddress (TxConstr (Memo (TxRaw {_wits = w}) _)) addr =
  case getValidatorHash addr of
    Nothing -> False
    Just hash ->
      case Map.lookup hash (txscripts w) of
        Nothing -> False
        Just scr -> not (isNativeScript @era scr)

feesOK ::
  forall era.
  ( UsesValue era,
    AlonzoBody era,
    UsesTxOut era,
    ValidateScript era
  ) =>
  PParams era ->
  Tx era ->
  UTxO era ->
  Bool
feesOK pp tx (UTxO m) =
  (bal >= txfee txb)
    && (all (\txout -> not (isNonNativeScriptAddress tx (getField @"address" txout))) utxoFees)
    && (minfee pp tx <= txfee txb)
  where
    txb = txbody tx
    fees = txinputs_fee txb
    utxoFees = eval (fees ◁ m) -- compute the domain restriction to those inputs where fees are paid
    bal = coin (balance @era (UTxO utxoFees))

-- | The keys of all the inputs of the TxBody (both the inputs for fees, and the normal inputs).
txins :: AlonzoBody era => TxBody era -> Set (TxIn (Crypto era))
txins (TxBody {txinputs = is, txinputs_fee = fs}) = Set.union is fs

txscriptfee :: Prices -> ExUnits -> Coin
txscriptfee (Prices pr_mem pr_steps) (ExUnits mem steps) =
  (mem <×> pr_mem) <+> (steps <×> pr_steps)

-- | txsize computes the length of the serialised bytes
txsize :: Tx era -> Integer
txsize (TxConstr (Memo _ bytes)) = fromIntegral (SBS.length bytes)

minfee :: AlonzoBody era => PParams era -> Tx era -> Coin
minfee pp tx =
  ((txsize tx) <×> (a pp))
    <+> (b pp)
    <+> (txscriptfee (_prices pp) (exunits (txbody tx)))
  where
    a protparam = Coin (fromIntegral (_minfeeA protparam))
    b protparam = Coin (fromIntegral (_minfeeB protparam))

-- The specification uses "validatorHash" to extract ScriptHash from
-- an Addr. But not every Addr has a ScriptHash. In particular KeyHashObj
-- do not. So we use getValidatorHash which returns a Maybe type.

getValidatorHash :: Addr crypto -> Maybe (ScriptHash crypto)
getValidatorHash (Addr _network (ScriptHashObj hash) _ref) = Just hash
getValidatorHash _ = Nothing

txbody :: Tx era -> TxBody era
txbody (TxConstr (Memo (TxRaw {_body = b}) _)) = b

-- ===============================================================
-- Operations on scripts from specification
-- Figure 6:Indexing script and data objects
-- ===============================================================

data ScriptPurpose crypto
  = Minting !(PolicyID crypto)
  | Spending !(TxIn crypto)
  | Rewarding !(RewardAcnt crypto) -- Not sure if this is the right type.
  | Certifying !(DCert crypto)
  deriving (Eq)

class Indexable elem container where
  indexOf :: elem -> container -> Word64
  atIndex :: Word64 -> container -> elem

instance Ord k => Indexable k (Set k) where
  indexOf n set = fromIntegral $ Set.findIndex n set
  atIndex i set = Set.elemAt (fromIntegral i) set

instance Eq k => Indexable k (StrictSeq k) where
  indexOf n seqx = case StrictSeq.findIndexL (== n) seqx of
    Just m -> fromIntegral m
    Nothing -> error ("Not found in StrictSeq")
  atIndex i seqx = case StrictSeq.lookup (fromIntegral i) seqx of
    Just element -> element
    Nothing -> error ("No elem at index " ++ show i)

instance Ord k => Indexable k (Map.Map k v) where
  indexOf n mp = fromIntegral $ Map.findIndex n mp
  atIndex i mp = fst (Map.elemAt (fromIntegral i) mp) -- If one needs the value, on can use Map.Lookup

rdptr ::
  AlonzoBody era =>
  TxBody era ->
  ScriptPurpose (Crypto era) ->
  RdmrPtr
rdptr txb (Minting pid) = RdmrPtr AlonzoScript.Mint (indexOf pid (getMapFromValue (mint txb)))
rdptr txb (Spending txin) = RdmrPtr AlonzoScript.Spend (indexOf txin (txinputs txb))
rdptr txb (Rewarding racnt) = RdmrPtr AlonzoScript.Rewrd (indexOf racnt (unWdrl (txwdrls txb)))
rdptr txb (Certifying d) = RdmrPtr AlonzoScript.Cert (indexOf d (txcerts txb))

getMapFromValue :: Value crypto -> Map.Map (PolicyID crypto) (Map.Map AssetName Integer)
getMapFromValue (Value _ m) = m

txrdmrs ::
  (Era era, ToCBOR (Core.Script era)) =>
  TxWitness era ->
  Map.Map RdmrPtr (Data era, ExUnits)
txrdmrs txw = witsRdmr txw

indexedRdmrs ::
  ( Era era,
    ToCBOR (Core.AuxiliaryData era),
    ToCBOR (Core.Script era),
    Compactible (Core.Value era)
  ) =>
  Tx era ->
  ScriptPurpose (Crypto era) ->
  Maybe (Data era, ExUnits)
indexedRdmrs tx sp = Map.lookup policyid (txrdmrs . txwits $ tx)
  where
    policyid = rdptr (body tx) sp

-- ===============================================================
-- From the specification, Figure 7 "Script Validation, cont."
-- ===============================================================

-- | valContext collects info from the Tx and the UTxO and translates it into
--   a 'Data', which the Plutus language knows how to interpret.
valContext :: UTxO era -> Tx era -> ScriptPurpose (Crypto era) -> [Data era]
valContext _utxo _tx _sp = []

--TODO FIX THIS, when defined will always return singleton list
-- see also: collectNNScriptInputs    where it is called

-- TODO  Specification says CostMod, not CostModel
runPLCScript ::
  CostModel ->
  AlonzoScript.Script era ->
  [Data era] ->
  ExUnits ->
  (IsValidating, ExUnits)
runPLCScript _cost _script _data _exunits = (IsValidating True, ExUnits 0 0) -- TODO FIX THIS

-- ===============================================================
-- From the specification, Figure 8 "Scripts and their Arguments"
-- ===============================================================

getData ::
  forall era.
  ( ToCBOR (Core.AuxiliaryData era),
    ToCBOR (Core.Script era),
    UsesTxOut era,
    HasField "datahash" (Core.TxOut era) (Maybe (DataHash (Crypto era)))
  ) =>
  Tx era ->
  UTxO era ->
  ScriptPurpose (Crypto era) ->
  [Data era]
getData tx (UTxO m) sp = case sp of
  Minting _policyid -> []
  Rewarding _rewaccnt -> []
  Certifying _dcert -> []
  Spending txin ->
    -- Only the Spending ScriptPurpose contains Data
    case Map.lookup txin m of
      Nothing -> []
      Just txout ->
        case getField @"datahash" txout of
          Nothing -> []
          Just hash ->
            case Map.lookup hash (witsData (wits tx)) of
              Nothing -> []
              Just d -> [d]

collectNNScriptInputs ::
  ( UsesTxOut era,
    ToCBOR (Core.Script era),
    Compactible (Core.Value era),
    ToCBOR (Core.AuxiliaryData era),
    Core.Script era ~ AlonzoScript.Script era,
    HasField "datahash" (Core.TxOut era) (Maybe (DataHash (Crypto era)))
  ) =>
  PParams era ->
  Tx era ->
  UTxO era ->
  [(AlonzoScript.Script era, [Data era], ExUnits, CostModel)]
collectNNScriptInputs _pp tx utxo =
  [ (script, (d : (valContext utxo tx sp ++ getData tx utxo sp)), eu, cost)
    | (sp, scripthash) <- scriptsNeeded utxo tx, -- TODO, IN specification ORDER IS WRONG
      (d, eu) <- maybeToList (indexedRdmrs tx sp),
      script <- maybeToList (Map.lookup scripthash (txscripts (txwits tx))),
      cost <- maybeToList (Map.lookup (language script) (_costmdls _pp))
  ]

evalScripts :: (AlonzoScript.Script era, [Data era], ExUnits, CostModel) -> Bool
evalScripts (AlonzoScript.NativeScript _timelock, _, _, _) = True
evalScripts (AlonzoScript.PlutusScript, ds, units, cost) = b
  where
    (IsValidating b, _exunits) = runPLCScript cost AlonzoScript.PlutusScript ds units

-- ===================================================================
-- From Specification, Figure 12 "UTXOW helper functions"

-- THE SPEC CALLS FOR A SET, BUT THAT NEEDS A BUNCH OF ORD INSTANCES (DCert)
scriptsNeeded ::
  forall era.
  ( UsesTxOut era,
    AlonzoBody era
  ) =>
  UTxO era ->
  Tx era ->
  [(ScriptPurpose (Crypto era), ScriptHash (Crypto era))]
scriptsNeeded (UTxO utxomap) tx = spend ++ reward ++ cert ++ minted
  where
    txb = txbody tx
    !spend = foldl' accum [] (txins txb)
      where
        accum !ans !i =
          case Map.lookup i utxomap of
            Nothing -> ans
            Just txout ->
              case getValidatorHash (getField @"address" txout) of
                Nothing -> ans
                Just hash -> (Spending i, hash) : ans

    !reward = foldl' accum [] (Map.keys m2)
      where
        (Wdrl m2) = txwdrls txb
        accum !ans !accnt = case getRwdCred accnt of -- TODO  IS THIS RIGHT?
          (ScriptHashObj hash) -> (Rewarding accnt, hash) : ans
          _ -> ans

    !cert = foldl addOnlyCwitness [] (txcerts txb)

    !minted = map (\(pid@(PolicyID hash)) -> (Minting pid, hash)) (Map.keys m3)
      where
        m3 = getMapFromValue (mint txb)

-- We only find certificate witnesses in Delegating and Deregistration DCerts
-- that have ScriptHashObj credentials.
addOnlyCwitness ::
  [(ScriptPurpose crypto, ScriptHash crypto)] ->
  DCert crypto ->
  [(ScriptPurpose crypto, ScriptHash crypto)]
addOnlyCwitness !ans !(DCertDeleg (c@(DeRegKey (ScriptHashObj hk)))) =
  (Certifying $ DCertDeleg c, hk) : ans
addOnlyCwitness !ans !(DCertDeleg (c@(Delegate (Delegation (ScriptHashObj hk) _dpool)))) =
  (Certifying $ DCertDeleg c, hk) : ans
addOnlyCwitness !ans _ = ans

checkScriptData ::
  forall era.
  ( ToCBOR (Core.AuxiliaryData era),
    ValidateScript era,
    Compactible (Core.Value era),
    UsesTxOut era,
    HasField "datahash" (Core.TxOut era) (Maybe (DataHash (Crypto era)))
  ) =>
  Tx era ->
  UTxO era ->
  (ScriptPurpose (Crypto era), ScriptHash (Crypto era)) ->
  Bool
checkScriptData tx utxo (sp, _h) = any ok scripts
  where
    scripts = txscripts (txwits tx)
    isSpending (Spending _) = True
    isSpending _ = False
    ok s =
      (isNativeScript @era s)
        || ( isJust (indexedRdmrs tx sp)
               && (not (isSpending sp) || not (null (getData tx utxo sp)))
           )

-- The function hashSD, specified in Figure 12
-- hashSD :: TxWitness era -> Maybe (ScriptDataHash (Crypto era))
-- is defined in Cardano.Ledger.Alonzo.TxWitness

-- languages:: TxWitness era -> Set Language  -- TODO

txscripts ::
  (Era era, ToCBOR (Core.Script era)) =>
  TxWitness era ->
  Map.Map (ScriptHash (Crypto era)) (Core.Script era)
txscripts x = witsScript x

txwits :: (Era era, ToCBOR (Core.AuxiliaryData era)) => Tx era -> TxWitness era
txwits x = wits x
