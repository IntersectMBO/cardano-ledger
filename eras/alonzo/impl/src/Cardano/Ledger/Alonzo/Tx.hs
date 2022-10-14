{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

-- | This module exports implementations of many of the functions outlined in the Alonzo specification.
--     The link to source of the specification
--       https://github.com/input-output-hk/cardano-ledger/tree/master/eras/alonzo/formal-spec
--     The most recent version of the document can be found here:
--       https://hydra.iohk.io/job/Cardano/cardano-ledger/specs.alonzo-ledger/latest/download-by-type/doc-pdf/alonzo-changes
--     The functions can be found in Figures in that document, and sections of this code refer to those figures.
module Cardano.Ledger.Alonzo.Tx
  ( -- Figure 1
    CostModel,
    getLanguageView,
    -- Figure 2
    Data,
    DataHash,
    IsValid (..),
    hashData,
    nonNativeLanguages,
    hashScriptIntegrity,
    getCoin,
    EraIndependentScriptIntegrity,
    ScriptIntegrity (ScriptIntegrity),
    ScriptIntegrityHash,
    -- Figure 3
    AlonzoTx (AlonzoTx, body, wits, isValid, auxiliaryData),
    ValidatedTx,
    AlonzoEraTx (..),
    mkBasicAlonzoTx,
    bodyAlonzoTxL,
    witsAlonzoTxL,
    auxDataAlonzoTxL,
    sizeAlonzoTxF,
    isValidAlonzoTxL,
    txdats',
    txscripts',
    txrdmrs,
    TxBody,
    AlonzoTxBody (..),
    -- Figure 4
    totExUnits,
    isTwoPhaseScriptAddress,
    alonzoMinFeeTx,
    minfee,
    --  Figure 5
    Indexable (..), -- indexOf
    ScriptPurpose (..),
    isTwoPhaseScriptAddressFromMap,
    Shelley.txouts,
    indexedRdmrs,
    rdptr,
    -- Figure 6
    rdptrInv,
    getMapFromValue,
    -- Segwit
    alonzoSegwitTx,
    -- Other
    toCBORForSizeComputation,
    toCBORForMempoolSubmission,
  )
where

import Cardano.Crypto.Hash.Class (HashAlgorithm)
import Cardano.Ledger.Address (Addr (..), RewardAcnt (..))
import Cardano.Ledger.Alonzo.Data (Data, hashData)
import Cardano.Ledger.Alonzo.Era (AlonzoEra)
import Cardano.Ledger.Alonzo.PParams
  ( AlonzoPParamsHKD (..),
    LangDepView (..),
    encodeLangViews,
    getLanguageView,
  )
import Cardano.Ledger.Alonzo.Scripts
  ( CostModel,
    ExUnits (..),
    Prices,
    Tag (..),
    txscriptfee,
  )
import Cardano.Ledger.Alonzo.TxBody
  ( AlonzoEraTxBody (..),
    AlonzoTxBody (..),
    ScriptIntegrityHash,
    ShelleyMAEraTxBody (..),
    TxBody,
  )
import Cardano.Ledger.Alonzo.TxWits
  ( AlonzoEraTxWits (..),
    AlonzoTxWits (..),
    RdmrPtr (..),
    Redeemers (..),
    TxDats (..),
    nullDats,
    nullRedeemers,
    txrdmrs,
    unRedeemers,
  )
import Cardano.Ledger.Binary
  ( Annotator (..),
    Encoding,
    FromCBOR (..),
    ToCBOR (toCBOR),
    decodeNullMaybe,
    encodeListLen,
    encodeNullMaybe,
    serializeEncoding,
    serializeEncoding',
  )
import Cardano.Ledger.Binary.Coders
import Cardano.Ledger.Coin (Coin (..))
import Cardano.Ledger.Core hiding (TxBody)
import qualified Cardano.Ledger.Core as Core
import Cardano.Ledger.Crypto (HASH)
import qualified Cardano.Ledger.Crypto as CC
import Cardano.Ledger.Language (nonNativeLanguages)
import Cardano.Ledger.Mary.Value (AssetName, MaryValue (..), MultiAsset (..), PolicyID (..))
import Cardano.Ledger.SafeHash (HashAnnotated, SafeToHash (..), hashAnnotated)
import Cardano.Ledger.Shelley.Delegation.Certificates (DCert (..))
import Cardano.Ledger.Shelley.TxBody (ShelleyEraTxBody (..), Wdrl (..), unWdrl)
import Cardano.Ledger.ShelleyMA.Tx (validateTimelock)
import Cardano.Ledger.TxIn (TxIn (..))
import qualified Cardano.Ledger.UTxO as Shelley
import Cardano.Ledger.Val (Val ((<+>), (<×>)))
import Control.DeepSeq (NFData (..))
import qualified Data.ByteString.Lazy as LBS
import qualified Data.Map.Strict as Map
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
import Lens.Micro hiding (set)
import NoThunks.Class (NoThunks)
import Numeric.Natural (Natural)

-- ===================================================

-- | Tag indicating whether non-native scripts in this transaction are expected
-- to validate. This is added by the block creator when constructing the block.
newtype IsValid = IsValid Bool
  deriving (Eq, Show, Generic)
  deriving newtype (NoThunks, NFData)

data AlonzoTx era = AlonzoTx
  { body :: !(Core.TxBody era),
    wits :: !(Core.TxWits era),
    isValid :: !IsValid,
    auxiliaryData :: !(StrictMaybe (TxAuxData era))
  }
  deriving (Generic)

{-# DEPRECATED ValidatedTx "Use `AlonzoTx` instead" #-}

type ValidatedTx era = AlonzoTx era

instance CC.Crypto c => EraTx (AlonzoEra c) where
  {-# SPECIALIZE instance EraTx (AlonzoEra CC.StandardCrypto) #-}

  type Tx (AlonzoEra c) = AlonzoTx (AlonzoEra c)

  mkBasicTx = mkBasicAlonzoTx

  bodyTxL = bodyAlonzoTxL
  {-# INLINE bodyTxL #-}

  witsTxL = witsAlonzoTxL
  {-# INLINE witsTxL #-}

  auxDataTxL = auxDataAlonzoTxL
  {-# INLINE auxDataTxL #-}

  sizeTxF = sizeAlonzoTxF
  {-# INLINE sizeTxF #-}

  validateScript (Phase1Script script) tx = validateTimelock @(AlonzoEra c) script tx
  {-# INLINE validateScript #-}

  getMinFeeTx = alonzoMinFeeTx

class (EraTx era, AlonzoEraTxBody era, AlonzoEraTxWits era) => AlonzoEraTx era where
  isValidTxL :: Lens' (Core.Tx era) IsValid

instance CC.Crypto c => AlonzoEraTx (AlonzoEra c) where
  {-# SPECIALIZE instance AlonzoEraTx (AlonzoEra CC.StandardCrypto) #-}

  isValidTxL = isValidAlonzoTxL
  {-# INLINE isValidTxL #-}

mkBasicAlonzoTx :: Monoid (TxWits era) => Core.TxBody era -> AlonzoTx era
mkBasicAlonzoTx txBody = AlonzoTx txBody mempty (IsValid True) SNothing

-- | `Core.TxBody` setter and getter for `AlonzoTx`.
bodyAlonzoTxL :: Lens' (AlonzoTx era) (Core.TxBody era)
bodyAlonzoTxL = lens body (\tx txBody -> tx {body = txBody})
{-# INLINEABLE bodyAlonzoTxL #-}

-- | `TxWits` setter and getter for `AlonzoTx`.
witsAlonzoTxL :: Lens' (AlonzoTx era) (TxWits era)
witsAlonzoTxL = lens wits (\tx txWits -> tx {wits = txWits})
{-# INLINEABLE witsAlonzoTxL #-}

-- | `TxAuxData` setter and getter for `AlonzoTx`.
auxDataAlonzoTxL :: Lens' (AlonzoTx era) (StrictMaybe (TxAuxData era))
auxDataAlonzoTxL = lens auxiliaryData (\tx txTxAuxData -> tx {auxiliaryData = txTxAuxData})
{-# INLINEABLE auxDataAlonzoTxL #-}

-- | txsize computes the length of the serialised bytes
sizeAlonzoTxF :: forall era. EraTx era => SimpleGetter (AlonzoTx era) Integer
sizeAlonzoTxF =
  to $
    fromIntegral
      . LBS.length
      . serializeEncoding (eraProtVerLow @era)
      . toCBORForSizeComputation
{-# INLINEABLE sizeAlonzoTxF #-}

isValidAlonzoTxL :: Lens' (AlonzoTx era) IsValid
isValidAlonzoTxL = lens isValid (\tx valid -> tx {isValid = valid})
{-# INLINEABLE isValidAlonzoTxL #-}

deriving instance
  (Era era, Eq (Core.TxBody era), Eq (TxWits era), Eq (TxAuxData era)) => Eq (AlonzoTx era)

deriving instance
  (Era era, Show (Core.TxBody era), Show (TxAuxData era), Show (Script era), Show (TxWits era)) =>
  Show (AlonzoTx era)

instance
  ( Era era,
    NoThunks (TxWits era),
    NoThunks (TxAuxData era),
    NoThunks (Core.TxBody era)
  ) =>
  NoThunks (AlonzoTx era)

instance
  ( Era era,
    NFData (TxWits era),
    NFData (TxAuxData era),
    NFData (Core.TxBody era)
  ) =>
  NFData (AlonzoTx era)

-- =========================================================
-- Figure 2: Definitions for Transactions

getCoin :: EraTxOut era => TxOut era -> Coin
getCoin txOut = txOut ^. coinTxOutL
{-# DEPRECATED getCoin "In favor of `coinTxOutL`" #-}

-- | A ScriptIntegrityHash is the hash of three things.  The first two come
-- from the witnesses and the last comes from the Protocol Parameters.
data ScriptIntegrity era
  = ScriptIntegrity
      !(Redeemers era) -- From the witnesses
      !(TxDats era)
      !(Set LangDepView) -- From the Protocol parameters
  deriving (Eq, Generic, Typeable)

deriving instance HashAlgorithm (HASH (EraCrypto era)) => Show (ScriptIntegrity era)

deriving instance Typeable era => NoThunks (ScriptIntegrity era)

-- ScriptIntegrity is not transmitted over the network. The bytes are independently
-- reconstructed by all nodes. There are no original bytes to preserve.
-- Instead, we must use a reproducable serialization
instance Era era => SafeToHash (ScriptIntegrity era) where
  originalBytes (ScriptIntegrity m d l) =
    let dBytes = if nullDats d then mempty else originalBytes d
        lBytes = serializeEncoding' (eraProtVerLow @era) (encodeLangViews l)
     in originalBytes m <> dBytes <> lBytes

instance
  (Era era, c ~ EraCrypto era) =>
  HashAnnotated (ScriptIntegrity era) EraIndependentScriptIntegrity c

hashScriptIntegrity ::
  forall era.
  Era era =>
  Set LangDepView ->
  Redeemers era ->
  TxDats era ->
  StrictMaybe (ScriptIntegrityHash (EraCrypto era))
hashScriptIntegrity langViews rdmrs dats =
  if nullRedeemers rdmrs && Set.null langViews && nullDats dats
    then SNothing
    else SJust (hashAnnotated (ScriptIntegrity rdmrs dats langViews))

-- ===============================================================
-- From the specification, Figure 4 "Functions related to fees"
-- ===============================================================

isTwoPhaseScriptAddress ::
  forall era.
  (EraTx era, TxWits era ~ AlonzoTxWits era) =>
  AlonzoTx era ->
  Addr (EraCrypto era) ->
  Bool
isTwoPhaseScriptAddress tx =
  isTwoPhaseScriptAddressFromMap @era (wits tx ^. scriptTxWitsL)

-- | This ensures that the size of transactions from Mary is unchanged.
-- The individual components all store their bytes; the only work we do in this
-- function is concatenating
toCBORForSizeComputation ::
  ( ToCBOR (Core.TxBody era),
    ToCBOR (TxWits era),
    ToCBOR (TxAuxData era)
  ) =>
  AlonzoTx era ->
  Encoding
toCBORForSizeComputation AlonzoTx {body, wits, auxiliaryData} =
  encodeListLen 3
    <> toCBOR body
    <> toCBOR wits
    <> encodeNullMaybe toCBOR (strictMaybeToMaybe auxiliaryData)

alonzoMinFeeTx ::
  ( EraTx era,
    AlonzoEraTxWits era,
    HasField "_minfeeA" (PParams era) Natural,
    HasField "_minfeeB" (PParams era) Natural,
    HasField "_prices" (PParams era) Prices
  ) =>
  PParams era ->
  Core.Tx era ->
  Coin
alonzoMinFeeTx pp tx =
  (tx ^. sizeTxF <×> a pp)
    <+> b pp
    <+> txscriptfee (getField @"_prices" pp) allExunits
  where
    a protparam = Coin (fromIntegral (getField @"_minfeeA" protparam))
    b protparam = Coin (fromIntegral (getField @"_minfeeB" protparam))
    allExunits = totExUnits tx

minfee ::
  ( EraTx era,
    AlonzoEraTxWits era,
    HasField "_minfeeA" (PParams era) Natural,
    HasField "_minfeeB" (PParams era) Natural,
    HasField "_prices" (PParams era) Prices
  ) =>
  PParams era ->
  Core.Tx era ->
  Coin
minfee = alonzoMinFeeTx
{-# DEPRECATED minfee "In favor of `getMinFeeTx`" #-}

totExUnits ::
  (EraTx era, AlonzoEraTxWits era) =>
  Tx era ->
  ExUnits
totExUnits tx =
  foldMap snd . Map.elems . unRedeemers $ tx ^. witsTxL . rdmrsTxWitsL

-- ===============================================================
-- Operations on scripts from specification
-- Figure 6:Indexing script and data objects
-- ===============================================================

data ScriptPurpose c
  = Minting !(PolicyID c)
  | Spending !(TxIn c)
  | Rewarding !(RewardAcnt c) -- Not sure if this is the right type.
  | Certifying !(DCert c)
  deriving (Eq, Show, Generic, NoThunks, NFData)

instance (CC.Crypto c) => ToCBOR (ScriptPurpose c) where
  toCBOR (Minting x) = encode (Sum Minting 0 !> To x)
  toCBOR (Spending x) = encode (Sum Spending 1 !> To x)
  toCBOR (Rewarding x) = encode (Sum Rewarding 2 !> To x)
  toCBOR (Certifying x) = encode (Sum Certifying 3 !> To x)

instance (CC.Crypto c) => FromCBOR (ScriptPurpose c) where
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
  fromIndex :: Word64 -> container -> StrictMaybe elem

instance Ord k => Indexable k (Set k) where
  indexOf n set = case Set.lookupIndex n set of
    Just x -> SJust (fromIntegral x)
    Nothing -> SNothing
  fromIndex i set =
    if fromIntegral i < Set.size set
      then SJust $ Set.elemAt (fromIntegral i) set
      else SNothing

instance Eq k => Indexable k (StrictSeq k) where
  indexOf n seqx = case StrictSeq.findIndexL (== n) seqx of
    Just m -> SJust (fromIntegral m)
    Nothing -> SNothing
  fromIndex i seqx = maybeToStrictMaybe $ StrictSeq.lookup (fromIntegral i) seqx

instance Ord k => Indexable k (Map.Map k v) where
  indexOf n mp = case Map.lookupIndex n mp of
    Just x -> SJust (fromIntegral x)
    Nothing -> SNothing
  fromIndex i mp =
    if fromIntegral i < Map.size mp
      then SJust . fst $ Map.elemAt (fromIntegral i) mp
      else SNothing

rdptr ::
  forall era.
  ShelleyMAEraTxBody era =>
  Core.TxBody era ->
  ScriptPurpose (EraCrypto era) ->
  StrictMaybe RdmrPtr
rdptr txBody (Minting (PolicyID hash)) =
  RdmrPtr Mint <$> indexOf hash (txBody ^. mintedTxBodyF :: Set (ScriptHash (EraCrypto era)))
rdptr txBody (Spending txin) = RdmrPtr Spend <$> indexOf txin (txBody ^. inputsTxBodyL)
rdptr txBody (Rewarding racnt) = RdmrPtr Rewrd <$> indexOf racnt (unWdrl (txBody ^. wdrlsTxBodyL))
rdptr txBody (Certifying d) = RdmrPtr Cert <$> indexOf d (txBody ^. certsTxBodyL)

rdptrInv ::
  forall era.
  ShelleyMAEraTxBody era =>
  Core.TxBody era ->
  RdmrPtr ->
  StrictMaybe (ScriptPurpose (EraCrypto era))
rdptrInv txBody (RdmrPtr Mint idx) =
  Minting . PolicyID <$> fromIndex idx (txBody ^. mintedTxBodyF)
rdptrInv txBody (RdmrPtr Spend idx) =
  Spending <$> fromIndex idx (txBody ^. inputsTxBodyL)
rdptrInv txBody (RdmrPtr Rewrd idx) =
  Rewarding <$> fromIndex idx (unWdrl (txBody ^. wdrlsTxBodyL))
rdptrInv txBody (RdmrPtr Cert idx) =
  Certifying <$> fromIndex idx (txBody ^. certsTxBodyL)

{-# DEPRECATED getMapFromValue "No longer used" #-}
getMapFromValue :: MaryValue c -> Map.Map (PolicyID c) (Map.Map AssetName Integer)
getMapFromValue (MaryValue _ (MultiAsset m)) = m

-- | Find the Data and ExUnits assigned to a script.
indexedRdmrs ::
  forall era.
  (ShelleyMAEraTxBody era, AlonzoEraTxWits era, EraTx era) =>
  Tx era ->
  ScriptPurpose (EraCrypto era) ->
  Maybe (Data era, ExUnits)
indexedRdmrs tx sp = case rdptr @era (tx ^. bodyTxL) sp of
  SNothing -> Nothing
  SJust rPtr -> Map.lookup rPtr rdmrs
    where
      rdmrs = unRedeemers (tx ^. witsTxL . rdmrsTxWitsL)

--------------------------------------------------------------------------------
-- Serialisation
--------------------------------------------------------------------------------

deriving newtype instance FromCBOR IsValid

deriving newtype instance ToCBOR IsValid

-- | Construct an annotated Alonzo style transaction.
alonzoSegwitTx ::
  AlonzoEraTx era =>
  Annotator (Core.TxBody era) ->
  Annotator (TxWits era) ->
  IsValid ->
  Maybe (Annotator (TxAuxData era)) ->
  Annotator (Tx era)
alonzoSegwitTx txBodyAnn txWitsAnn isValid auxDataAnn = Annotator $ \bytes ->
  let txBody = runAnnotator txBodyAnn bytes
      txWits = runAnnotator txWitsAnn bytes
      txAuxData = maybeToStrictMaybe (flip runAnnotator bytes <$> auxDataAnn)
   in mkBasicTx txBody
        & witsTxL .~ txWits
        & auxDataTxL .~ txAuxData
        & isValidTxL .~ isValid

--------------------------------------------------------------------------------
-- Mempool Serialisation
--
-- We do not store the Tx bytes for the following reasons:
-- - A Tx serialised in this way never forms part of any hashed structure, hence
--   we do not worry about the serialisation changing and thus seeing a new
--   hash.
-- - The three principal components of this Tx already store their own bytes;
--   here we simply concatenate them. The final component, `IsValid`, is
--   just a flag and very cheap to serialise.
--------------------------------------------------------------------------------

-- | Encode to CBOR for the purposes of transmission from node to node, or from
-- wallet to node.
--
-- Note that this serialisation is neither the serialisation used on-chain
-- (where Txs are deconstructed using segwit), nor the serialisation used for
-- computing the transaction size (which omits the `IsValid` field for
-- compatibility with Mary - see 'toCBORForSizeComputation').
toCBORForMempoolSubmission ::
  ( ToCBOR (Core.TxBody era),
    ToCBOR (TxWits era),
    ToCBOR (TxAuxData era)
  ) =>
  AlonzoTx era ->
  Encoding
toCBORForMempoolSubmission
  AlonzoTx {body, wits, auxiliaryData, isValid} =
    encode $
      Rec AlonzoTx
        !> To body
        !> To wits
        !> To isValid
        !> E (encodeNullMaybe toCBOR . strictMaybeToMaybe) auxiliaryData

instance
  ( Era era,
    ToCBOR (Core.TxBody era),
    ToCBOR (TxAuxData era),
    ToCBOR (TxWits era)
  ) =>
  ToCBOR (AlonzoTx era)
  where
  toCBOR = toCBORForMempoolSubmission

instance
  ( Typeable era,
    FromCBOR (Annotator (Core.TxBody era)),
    FromCBOR (Annotator (Core.TxWits era)),
    FromCBOR (Annotator (TxAuxData era))
  ) =>
  FromCBOR (Annotator (AlonzoTx era))
  where
  fromCBOR =
    decode $
      Ann (RecD AlonzoTx)
        <*! From
        <*! From
        <*! Ann From
        <*! D
          ( sequence . maybeToStrictMaybe
              <$> decodeNullMaybe fromCBOR
          )

-- =======================================================================
-- Some generic functions that compute over Tx. We try to be abstract over
-- things that might differ from Era to Era like
--    1) TxOut will have additional fields
--    2) Scripts might appear in places other than the witness set. So
--       we need such a 'witness' we pass it as a parameter and each call site
--       can use a different method to compute it in the current Era.

-- | Compute if an Addr has the hash of a TwoPhaseScript, we can tell
--   what kind of Script from the Hash, by looking it up in the Map
isTwoPhaseScriptAddressFromMap ::
  forall era.
  EraScript era =>
  Map.Map (ScriptHash (EraCrypto era)) (Script era) ->
  Addr (EraCrypto era) ->
  Bool
isTwoPhaseScriptAddressFromMap hashScriptMap addr =
  case Shelley.getScriptHash @(EraCrypto era) addr of
    Nothing -> False
    Just hash -> any ok hashScriptMap
      where
        ok script = hashScript @era script == hash && not (isNativeScript @era script)
