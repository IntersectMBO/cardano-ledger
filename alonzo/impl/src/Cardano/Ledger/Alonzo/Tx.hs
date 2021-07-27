{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
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
    getLanguageView,
    -- Figure 2
    Data,
    DataHash,
    IsValidating (..),
    hashData,
    nonNativeLanguages,
    hashWitnessPPData,
    getCoin,
    EraIndependentWitnessPPData,
    WitnessPPData (WitnessPPData),
    WitnessPPDataHash,
    -- Figure 3
    ValidatedTx (ValidatedTx, body, wits, isValidating, auxiliaryData),
    txdats',
    txscripts',
    TxBody (..),
    -- Figure 4
    ScriptPurpose (..),
    totExUnits,
    --  Figure 5
    getValidatorHash,
    minfee,
    isTwoPhaseScriptAddress,
    Shelley.txouts,
    -- Figure 6
    txrdmrs,
    rdptr,
    getMapFromValue,
    indexedRdmrs,
    -- Pretty
    ppIsValidating,
    ppTx,
    -- Segwit
    segwitTx,
    -- Other
    toCBORForSizeComputation,
    toCBORForMempoolSubmission,
  )
where

import Cardano.Binary
  ( FromCBOR (..),
    ToCBOR (toCBOR),
    encodeListLen,
    serializeEncoding,
    serializeEncoding',
  )
import Cardano.Ledger.Address (Addr (..), RewardAcnt (..))
import Cardano.Ledger.Alonzo.Data (Data, DataHash, hashData)
import Cardano.Ledger.Alonzo.Language (Language (..), nonNativeLanguages)
import Cardano.Ledger.Alonzo.PParams
  ( LangDepView (..),
    PParams,
    encodeLangViews,
    getLanguageView,
  )
import Cardano.Ledger.Alonzo.Scripts
  ( CostModel,
    ExUnits (..),
    Prices,
    Script,
    Tag (..),
    txscriptfee,
  )
import Cardano.Ledger.Alonzo.TxBody
  ( EraIndependentWitnessPPData,
    TxBody (..),
    TxOut (..),
    WitnessPPDataHash,
  )
import Cardano.Ledger.Alonzo.TxWitness
  ( RdmrPtr (..),
    Redeemers (..),
    TxDats (..),
    TxWitness (..),
    nullDats,
    nullRedeemers,
    ppTxWitness,
    txrdmrs,
    unRedeemers,
    unTxDats,
  )
import Cardano.Ledger.Coin (Coin (..))
import Cardano.Ledger.Compactible
import qualified Cardano.Ledger.Core as Core
import Cardano.Ledger.Credential (Credential (ScriptHashObj))
import qualified Cardano.Ledger.Crypto as CC
import Cardano.Ledger.Era (Crypto, Era, ValidateScript (isNativeScript))
import Cardano.Ledger.Keys (KeyRole (Witness))
import Cardano.Ledger.Mary.Value (AssetName, PolicyID (..), Value (..))
import Cardano.Ledger.Pretty
  ( PDoc,
    PrettyA (..),
    ppRecord,
    ppStrictMaybe,
    ppString,
  )
import Cardano.Ledger.SafeHash
  ( HashAnnotated,
    SafeToHash (..),
    hashAnnotated,
  )
import Cardano.Ledger.Val (Val (coin, (<+>), (<×>)))
import Control.DeepSeq (NFData (..))
import qualified Data.ByteString.Lazy as LBS
import Data.Coders
import Data.Foldable (fold)
import qualified Data.Map as Map
import Data.Maybe.Strict
  ( StrictMaybe (..),
    maybeToStrictMaybe,
    strictMaybeToMaybe,
  )
import Data.Sequence.Strict (StrictSeq)
import qualified Data.Sequence.Strict as StrictSeq
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Typeable (Typeable)
import Data.Word (Word64)
import GHC.Generics (Generic)
import GHC.Records (HasField (..))
import NoThunks.Class (NoThunks)
import Numeric.Natural (Natural)
import Shelley.Spec.Ledger.Address.Bootstrap (BootstrapWitness)
import Shelley.Spec.Ledger.Delegation.Certificates (DCert (..))
import Shelley.Spec.Ledger.Scripts (ScriptHash)
import Shelley.Spec.Ledger.TxBody (TxIn (..), Wdrl (..), WitVKey, unWdrl)
import qualified Shelley.Spec.Ledger.UTxO as Shelley

-- ===================================================

-- | Tag indicating whether non-native scripts in this transaction are expected
-- to validate. This is added by the block creator when constructing the block.
newtype IsValidating = IsValidating Bool
  deriving (Eq, Show, Generic)
  deriving newtype (NoThunks)

data ValidatedTx era = ValidatedTx
  { body :: !(Core.TxBody era),
    wits :: !(TxWitness era),
    isValidating :: !IsValidating,
    auxiliaryData :: !(StrictMaybe (Core.AuxiliaryData era))
  }
  deriving (Generic, Typeable)

deriving instance
  ( Era era,
    Eq (Core.AuxiliaryData era),
    Eq (Core.Script era),
    Eq (Core.TxBody era),
    Eq (Core.Value era),
    Eq (Core.PParamsDelta era),
    Compactible (Core.Value era)
  ) =>
  Eq (ValidatedTx era)

deriving instance
  ( Era era,
    Compactible (Core.Value era),
    Show (Core.AuxiliaryData era),
    Show (Core.Script era),
    Show (Core.TxBody era),
    Show (Core.Value era),
    Show (Core.PParamsDelta era)
  ) =>
  Show (ValidatedTx era)

instance
  ( Era era,
    NoThunks (Core.AuxiliaryData era),
    NoThunks (Core.Script era),
    NoThunks (Core.TxBody era),
    NoThunks (Core.Value era),
    NoThunks (Core.PParamsDelta era)
  ) =>
  NoThunks (ValidatedTx era)

-- ===================================
-- WellFormed instances

instance
  c ~ Crypto era =>
  HasField "addrWits" (ValidatedTx era) (Set (WitVKey 'Witness c))
  where
  getField = txwitsVKey' . wits

instance
  (c ~ Crypto era, script ~ Core.Script era) =>
  HasField "scriptWits" (ValidatedTx era) (Map.Map (ScriptHash c) script)
  where
  getField = txscripts' . wits

instance
  c ~ Crypto era =>
  HasField "bootWits" (ValidatedTx era) (Set (BootstrapWitness c))
  where
  getField = txwitsBoot' . wits

instance
  c ~ Crypto era =>
  HasField "txdatahash" (ValidatedTx era) (Map.Map (DataHash c) (Data era))
  where
  getField = unTxDats . txdats' . wits

-- =========================================================
-- Figure 2: Definitions for Transactions

getCoin :: (Era era) => TxOut era -> Coin
getCoin txout = coin (getField @"value" txout)

-- ========================================================================
-- A WitnessPPDataHash is the hash of two things. The first part comes from
-- the witnesses and the second comes from the Protocol Parameters (PParams).
-- In order to hash 2 things we make a newtype WitnessPPData which will be
-- a MemoBytes of these two things (WitnessPPDataRaw), so that we can hash it.

data WitnessPPData era
  = WitnessPPData
      !(Redeemers era) -- From the witnesses
      !(TxDats era)
      !(Set LangDepView) -- From the Porotocl parameters
  deriving (Show, Eq, Generic, Typeable)

deriving instance Typeable era => NoThunks (WitnessPPData era)

-- WitnessPPData is not transmitted over the network. The bytes are independently
-- reconstructed by all nodes. There are no original bytes to preserve.
-- Instead, we must use a reproducable serialization
instance Era era => SafeToHash (WitnessPPData era) where
  originalBytes (WitnessPPData m d l) =
    -- TODO: double check that canonical encodings are used for the langDepView (l)
    let dBytes = if nullDats d then mempty else originalBytes d
        lBytes = serializeEncoding' (encodeLangViews l)
     in originalBytes m <> dBytes <> lBytes

instance (Era era, c ~ Crypto era) => HashAnnotated (WitnessPPData era) EraIndependentWitnessPPData c

hashWitnessPPData ::
  forall era.
  Era era =>
  PParams era ->
  Set Language ->
  Redeemers era ->
  TxDats era ->
  StrictMaybe (WitnessPPDataHash (Crypto era))
hashWitnessPPData pp langs rdmrs dats =
  if nullRedeemers rdmrs && Set.null langs && nullDats dats
    then SNothing
    else
      let newset = Set.map (getLanguageView pp) langs
       in SJust (hashAnnotated (WitnessPPData rdmrs dats newset))

-- ===============================================================
-- From the specification, Figure 5 "Functions related to fees"
-- ===============================================================

isTwoPhaseScriptAddress ::
  forall era.
  (ValidateScript era) =>
  ValidatedTx era ->
  Addr (Crypto era) ->
  Bool
isTwoPhaseScriptAddress tx addr =
  case getValidatorHash addr of
    Nothing -> False
    Just hash ->
      case Map.lookup hash (getField @"scriptWits" tx) of
        Nothing -> False
        Just scr -> not (isNativeScript @era scr)

-- | txsize computes the length of the serialised bytes
instance
  ( Typeable era,
    ToCBOR (Core.TxBody era),
    ToCBOR (Core.AuxiliaryData era)
  ) =>
  HasField "txsize" (ValidatedTx era) Integer
  where
  getField tx =
    fromIntegral . LBS.length . serializeEncoding $
      toCBORForSizeComputation tx

-- This ensures that the size of transactions from Mary is unchanged.
-- The individual components all store their bytes; the only work we do in this function is concatenating
toCBORForSizeComputation ::
  ( Typeable era,
    ToCBOR (Core.TxBody era),
    ToCBOR (Core.AuxiliaryData era)
  ) =>
  ValidatedTx era ->
  Encoding
toCBORForSizeComputation (ValidatedTx {body, wits, auxiliaryData}) =
  encodeListLen 3
    <> toCBOR body
    <> toCBOR wits
    <> encodeNullMaybe toCBOR (strictMaybeToMaybe auxiliaryData)

minfee ::
  ( HasField "_minfeeA" (Core.PParams era) Natural,
    HasField "_minfeeB" (Core.PParams era) Natural,
    HasField "_prices" (Core.PParams era) Prices,
    HasField "wits" (tx era) (Core.Witnesses era),
    HasField "txrdmrs" (Core.Witnesses era) (Redeemers era),
    HasField "txsize" (tx era) Integer
  ) =>
  Core.PParams era ->
  tx era ->
  Coin
minfee pp tx =
  (getField @"txsize" tx <×> a pp)
    <+> b pp
    <+> txscriptfee (getField @"_prices" pp) allExunits
  where
    a protparam = Coin (fromIntegral (getField @"_minfeeA" protparam))
    b protparam = Coin (fromIntegral (getField @"_minfeeB" protparam))
    allExunits = totExUnits tx

totExUnits ::
  ( HasField "wits" (tx era) (Core.Witnesses era),
    HasField "txrdmrs" (Core.Witnesses era) (Redeemers era)
  ) =>
  tx era ->
  ExUnits
totExUnits = fold . snd . unzip . Map.elems . unRedeemers . getField @"txrdmrs" . getField @"wits"

-- The specification uses "validatorHash" to extract ScriptHash from
-- an Addr. But not every Addr has a ScriptHash. In particular KeyHashObj
-- do not. So we use getValidatorHash which returns a Maybe type.

getValidatorHash :: Addr crypto -> Maybe (ScriptHash crypto)
getValidatorHash (Addr _network (ScriptHashObj hash) _ref) = Just hash
getValidatorHash _ = Nothing

-- ===============================================================
-- Operations on scripts from specification
-- Figure 6:Indexing script and data objects
-- ===============================================================

data ScriptPurpose crypto
  = Minting !(PolicyID crypto)
  | Spending !(TxIn crypto)
  | Rewarding !(RewardAcnt crypto) -- Not sure if this is the right type.
  | Certifying !(DCert crypto)
  deriving (Eq, Show, Generic, NoThunks, NFData)

instance (Typeable c, CC.Crypto c) => ToCBOR (ScriptPurpose c) where
  toCBOR (Minting x) = encode (Sum Minting 0 !> To x)
  toCBOR (Spending x) = encode (Sum Spending 1 !> To x)
  toCBOR (Rewarding x) = encode (Sum Rewarding 2 !> To x)
  toCBOR (Certifying x) = encode (Sum Certifying 3 !> To x)

instance (Typeable c, CC.Crypto c) => FromCBOR (ScriptPurpose c) where
  fromCBOR = decode (Summands "ScriptPurpose" dec)
    where
      dec 0 = SumD Minting <! From
      dec 1 = SumD Spending <! From
      dec 2 = SumD Rewarding <! From
      dec 3 = SumD Certifying <! From
      dec n = Invalid n

-- =======================================

class Indexable elem container where
  indexOf :: elem -> container -> StrictMaybe Word64

instance Ord k => Indexable k (Set k) where
  indexOf n set = case Set.lookupIndex n set of
    Just x -> SJust (fromIntegral x)
    Nothing -> SNothing

instance Eq k => Indexable k (StrictSeq k) where
  indexOf n seqx = case StrictSeq.findIndexL (== n) seqx of
    Just m -> SJust (fromIntegral m)
    Nothing -> SNothing

instance Ord k => Indexable k (Map.Map k v) where
  indexOf n mp = case Map.lookupIndex n mp of
    Just x -> SJust (fromIntegral x)
    Nothing -> SNothing

rdptr ::
  forall era.
  ( HasField "inputs" (Core.TxBody era) (Set (TxIn (Crypto era))),
    HasField "wdrls" (Core.TxBody era) (Wdrl (Crypto era)),
    HasField "certs" (Core.TxBody era) (StrictSeq (DCert (Crypto era))),
    HasField "minted" (Core.TxBody era) (Set (ScriptHash (Crypto era)))
  ) =>
  Core.TxBody era ->
  ScriptPurpose (Crypto era) ->
  StrictMaybe RdmrPtr
rdptr txb (Minting (PolicyID hash)) = RdmrPtr Mint <$> (indexOf hash ((getField @"minted" txb) :: Set (ScriptHash (Crypto era))))
rdptr txb (Spending txin) = RdmrPtr Spend <$> (indexOf txin (getField @"inputs" txb))
rdptr txb (Rewarding racnt) = RdmrPtr Rewrd <$> (indexOf racnt (unWdrl (getField @"wdrls" txb)))
rdptr txb (Certifying d) = RdmrPtr Cert <$> (indexOf d (getField @"certs" txb))

getMapFromValue :: Value crypto -> Map.Map (PolicyID crypto) (Map.Map AssetName Integer)
getMapFromValue (Value _ m) = m

-- | Find the Data and ExUnits assigned to a script.
indexedRdmrs ::
  forall era tx.
  ( Era era,
    HasField "inputs" (Core.TxBody era) (Set (TxIn (Crypto era))),
    HasField "wdrls" (Core.TxBody era) (Wdrl (Crypto era)),
    HasField "certs" (Core.TxBody era) (StrictSeq (DCert (Crypto era))),
    HasField "wits" tx (TxWitness era),
    HasField "body" tx (Core.TxBody era)
  ) =>
  tx ->
  ScriptPurpose (Crypto era) ->
  Maybe (Data era, ExUnits)
indexedRdmrs tx sp = case rdptr @era (getField @"body" tx) sp of
  SNothing -> Nothing
  SJust policyid -> Map.lookup policyid rdmrs
    where
      rdmrs = unRedeemers $ txrdmrs' . getField @"wits" $ tx

-- =======================================================

ppIsValidating :: IsValidating -> PDoc
ppIsValidating (IsValidating True) = ppString "True"
ppIsValidating (IsValidating False) = ppString "False"

instance PrettyA IsValidating where prettyA = ppIsValidating

ppTx ::
  ( Era era,
    PrettyA (Core.Script era),
    PrettyA (Core.TxBody era),
    PrettyA (Core.AuxiliaryData era)
  ) =>
  ValidatedTx era ->
  PDoc
ppTx (ValidatedTx b w iv aux) =
  ppRecord
    "Tx"
    [ ("body", prettyA b),
      ("wits", ppTxWitness w),
      ("isValidating", ppIsValidating iv),
      ("auxiliaryData", ppStrictMaybe prettyA aux)
    ]

instance
  ( Era era,
    PrettyA (Core.Script era),
    PrettyA (Core.TxBody era),
    PrettyA (Core.AuxiliaryData era)
  ) =>
  PrettyA (ValidatedTx era)
  where
  prettyA = ppTx

--------------------------------------------------------------------------------
-- Serialisation
--------------------------------------------------------------------------------

deriving newtype instance FromCBOR IsValidating

deriving newtype instance ToCBOR IsValidating

segwitTx ::
  Annotator (Core.TxBody era) ->
  Annotator (TxWitness era) ->
  IsValidating ->
  Maybe (Annotator (Core.AuxiliaryData era)) ->
  Annotator (ValidatedTx era)
segwitTx
  bodyAnn
  witsAnn
  isval
  metaAnn = Annotator $ \bytes ->
    let bodyb = runAnnotator bodyAnn bytes
        witnessSet = runAnnotator witsAnn bytes
        metadata = flip runAnnotator bytes <$> metaAnn
     in ValidatedTx
          bodyb
          witnessSet
          isval
          (maybeToStrictMaybe metadata)

--------------------------------------------------------------------------------
-- Mempool Serialisation
--
-- We do not store the Tx bytes for the following reasons:
-- - A Tx serialised in this way never forms part of any hashed structure, hence
--   we do not worry about the serialisation changing and thus seeing a new
--   hash.
-- - The three principal components of this Tx already store their own bytes;
--   here we simply concatenate them. The final component, `IsValidating`, is
--   just a flag and very cheap to serialise.
--------------------------------------------------------------------------------

-- | Encode to CBOR for the purposes of transmission from node to node, or from
-- wallet to node.
--
-- Note that this serialisation is neither the serialisation used on-chain
-- (where Txs are deconstructed using segwit), nor the serialisation used for
-- computing the transaction size (which omits the `IsValidating` field for
-- compatibility with Mary - see 'toCBORForSizeComputation').
toCBORForMempoolSubmission ::
  ( Typeable era,
    ToCBOR (Core.TxBody era),
    ToCBOR (Core.AuxiliaryData era)
  ) =>
  ValidatedTx era ->
  Encoding
toCBORForMempoolSubmission
  ValidatedTx {body, wits, auxiliaryData, isValidating} =
    encode $
      Rec ValidatedTx
        !> To body
        !> To wits
        !> To isValidating
        !> E (encodeNullMaybe toCBOR . strictMaybeToMaybe) auxiliaryData

instance
  ( Typeable era,
    ToCBOR (Core.TxBody era),
    ToCBOR (Core.AuxiliaryData era)
  ) =>
  ToCBOR (ValidatedTx era)
  where
  toCBOR = toCBORForMempoolSubmission

instance
  ( Era era,
    FromCBOR (Annotator (Core.TxBody era)),
    FromCBOR (Annotator (Core.AuxiliaryData era)),
    FromCBOR (Annotator (Core.Witnesses era)),
    ValidateScript era,
    Core.Script era ~ Script era
  ) =>
  FromCBOR (Annotator (ValidatedTx era))
  where
  fromCBOR =
    decode $
      Ann (RecD ValidatedTx)
        <*! From
        <*! From
        <*! Ann From
        <*! D
          ( sequence . maybeToStrictMaybe
              <$> decodeNullMaybe fromCBOR
          )
