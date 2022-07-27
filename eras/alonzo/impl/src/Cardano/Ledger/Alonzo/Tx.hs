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
    minfee,
    --  Figure 5
    Indexable (..), -- indexOf
    ScriptPurpose (..),
    isTwoPhaseScriptAddressFromMap,
    alonzoInputHashes,
    Shelley.txouts,
    indexedRdmrs,
    rdptr,
    -- Figure 6
    rdptrInv,
    getMapFromValue,
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
import Cardano.Crypto.DSIGN.Class (SigDSIGN, VerKeyDSIGN)
import Cardano.Ledger.Address (Addr (..), RewardAcnt (..))
import Cardano.Ledger.Alonzo.Data (Data, hashData)
import Cardano.Ledger.Alonzo.Era
import Cardano.Ledger.Alonzo.Language (nonNativeLanguages)
import Cardano.Ledger.Alonzo.PParams
  ( LangDepView (..),
    encodeLangViews,
    getLanguageView,
  )
import Cardano.Ledger.Alonzo.Scripts
  ( AlonzoScript (..),
    CostModel,
    ExUnits (..),
    Prices,
    Tag (..),
    txscriptfee,
  )
import Cardano.Ledger.Alonzo.TxBody
  ( AlonzoEraTxBody (..),
    AlonzoTxBody (..),
    AlonzoTxOut (..),
    ScriptIntegrityHash,
    TxBody,
  )
import Cardano.Ledger.Alonzo.TxWitness
  ( AlonzoEraWitnesses (..),
    RdmrPtr (..),
    Redeemers (..),
    TxDats (..),
    TxWitness (..),
    nullDats,
    nullRedeemers,
    txrdmrs,
    unRedeemers,
  )
import Cardano.Ledger.Coin (Coin (..))
import Cardano.Ledger.Core hiding (TxBody)
import qualified Cardano.Ledger.Core as Core
import qualified Cardano.Ledger.Crypto as CC
import Cardano.Ledger.Mary.Value (AssetName, MaryValue (..), MultiAsset (..), PolicyID (..))
import Cardano.Ledger.SafeHash
  ( HashAnnotated,
    SafeToHash (..),
    hashAnnotated,
  )
import Cardano.Ledger.Shelley.Delegation.Certificates (DCert (..))
import Cardano.Ledger.Shelley.TxBody (ShelleyEraTxBody (..), Wdrl (..), unWdrl)
import Cardano.Ledger.Shelley.UTxO (UTxO (..))
import qualified Cardano.Ledger.Shelley.UTxO as Shelley
import Cardano.Ledger.ShelleyMA.Tx (validateTimelock)
import Cardano.Ledger.TxIn (TxIn (..))
import Cardano.Ledger.Val (Val ((<+>), (<×>)))
import Control.DeepSeq (NFData (..))
import Control.SetAlgebra (eval, (◁))
import qualified Data.ByteString.Lazy as LBS
import Data.Coders hiding (to)
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
    wits :: !(TxWitness era),
    isValid :: !IsValid,
    auxiliaryData :: !(StrictMaybe (AuxiliaryData era))
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

class (EraTx era, AlonzoEraTxBody era, AlonzoEraWitnesses era) => AlonzoEraTx era where
  isValidTxL :: Lens' (Core.Tx era) IsValid

instance CC.Crypto c => AlonzoEraTx (AlonzoEra c) where
  {-# SPECIALIZE instance AlonzoEraTx (AlonzoEra CC.StandardCrypto) #-}

  isValidTxL = isValidAlonzoTxL
  {-# INLINE isValidTxL #-}

mkBasicAlonzoTx :: (Era era, Script era ~ AlonzoScript era) => Core.TxBody era -> AlonzoTx era
mkBasicAlonzoTx txBody = AlonzoTx txBody mempty (IsValid True) SNothing

-- | `Core.TxBody` setter and getter for `AlonzoTx`.
bodyAlonzoTxL :: Lens' (AlonzoTx era) (Core.TxBody era)
bodyAlonzoTxL = lens body (\tx txBody -> tx {body = txBody})
{-# INLINE bodyAlonzoTxL #-}

-- | `Witnesses` setter and getter for `AlonzoTx`.
witsAlonzoTxL :: Lens' (AlonzoTx era) (TxWitness era)
witsAlonzoTxL = lens wits (\tx txWits -> tx {wits = txWits})
{-# INLINE witsAlonzoTxL #-}

-- | `AuxiliaryData` setter and getter for `AlonzoTx`.
auxDataAlonzoTxL :: Lens' (AlonzoTx era) (StrictMaybe (AuxiliaryData era))
auxDataAlonzoTxL = lens auxiliaryData (\tx txAuxiliaryData -> tx {auxiliaryData = txAuxiliaryData})
{-# INLINE auxDataAlonzoTxL #-}

-- | txsize computes the length of the serialised bytes
sizeAlonzoTxF :: EraTx era => SimpleGetter (AlonzoTx era) Integer
sizeAlonzoTxF = to (fromIntegral . LBS.length . serializeEncoding . toCBORForSizeComputation)
{-# INLINE sizeAlonzoTxF #-}

isValidAlonzoTxL :: Lens' (AlonzoTx era) IsValid
isValidAlonzoTxL = lens isValid (\tx valid -> tx {isValid = valid})
{-# INLINE isValidAlonzoTxL #-}

deriving instance
  (Era era, Eq (Core.TxBody era), Eq (AuxiliaryData era)) =>
  Eq (AlonzoTx era)

deriving instance
  (Era era, Show (Core.TxBody era), Show (AuxiliaryData era), Show (Script era)) =>
  Show (AlonzoTx era)

instance
  ( Era era,
    NoThunks (AuxiliaryData era),
    NoThunks (Script era),
    NoThunks (Core.TxBody era),
    NoThunks (Value era),
    NoThunks (PParamsUpdate era)
  ) =>
  NoThunks (AlonzoTx era)

instance
  ( Era era,
    Script era ~ AlonzoScript era,
    crypto ~ Crypto era,
    NFData (AuxiliaryData era),
    NFData (Script era),
    NFData (Core.TxBody era),
    NFData (Value era),
    NFData (PParamsUpdate era),
    NFData (TxDats era),
    NFData (Redeemers era),
    NFData (VerKeyDSIGN (CC.DSIGN crypto)),
    NFData (SigDSIGN (CC.DSIGN crypto))
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
      !(Set LangDepView) -- From the Porotocl parameters
  deriving (Show, Eq, Generic, Typeable)

deriving instance Typeable era => NoThunks (ScriptIntegrity era)

-- ScriptIntegrity is not transmitted over the network. The bytes are independently
-- reconstructed by all nodes. There are no original bytes to preserve.
-- Instead, we must use a reproducable serialization
instance Era era => SafeToHash (ScriptIntegrity era) where
  originalBytes (ScriptIntegrity m d l) =
    let dBytes = if nullDats d then mempty else originalBytes d
        lBytes = serializeEncoding' (encodeLangViews l)
     in originalBytes m <> dBytes <> lBytes

instance
  (Era era, c ~ Crypto era) =>
  HashAnnotated (ScriptIntegrity era) EraIndependentScriptIntegrity c

hashScriptIntegrity ::
  forall era.
  Era era =>
  Set LangDepView ->
  Redeemers era ->
  TxDats era ->
  StrictMaybe (ScriptIntegrityHash (Crypto era))
hashScriptIntegrity langViews rdmrs dats =
  if nullRedeemers rdmrs && Set.null langViews && nullDats dats
    then SNothing
    else SJust (hashAnnotated (ScriptIntegrity rdmrs dats langViews))

-- ===============================================================
-- From the specification, Figure 4 "Functions related to fees"
-- ===============================================================

isTwoPhaseScriptAddress ::
  forall era.
  (EraTx era, Witnesses era ~ TxWitness era) =>
  AlonzoTx era ->
  Addr (Crypto era) ->
  Bool
isTwoPhaseScriptAddress tx =
  isTwoPhaseScriptAddressFromMap @era (wits tx ^. scriptWitsL)

-- | This ensures that the size of transactions from Mary is unchanged.
-- The individual components all store their bytes; the only work we do in this
-- function is concatenating
toCBORForSizeComputation ::
  ( Typeable era,
    ToCBOR (Core.TxBody era),
    ToCBOR (AuxiliaryData era)
  ) =>
  AlonzoTx era ->
  Encoding
toCBORForSizeComputation AlonzoTx {body, wits, auxiliaryData} =
  encodeListLen 3
    <> toCBOR body
    <> toCBOR wits
    <> encodeNullMaybe toCBOR (strictMaybeToMaybe auxiliaryData)

minfee ::
  ( EraTx era,
    AlonzoEraWitnesses era,
    HasField "_minfeeA" (PParams era) Natural,
    HasField "_minfeeB" (PParams era) Natural,
    HasField "_prices" (PParams era) Prices
  ) =>
  PParams era ->
  Core.Tx era ->
  Coin
minfee pp tx =
  (tx ^. sizeTxF <×> a pp)
    <+> b pp
    <+> txscriptfee (getField @"_prices" pp) allExunits
  where
    a protparam = Coin (fromIntegral (getField @"_minfeeA" protparam))
    b protparam = Coin (fromIntegral (getField @"_minfeeB" protparam))
    allExunits = totExUnits tx

totExUnits ::
  (EraTx era, AlonzoEraWitnesses era) =>
  Tx era ->
  ExUnits
totExUnits tx =
  foldMap snd . Map.elems . unRedeemers $ tx ^. witsTxL . rdmrsWitsL

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
  ShelleyEraTxBody era =>
  Core.TxBody era ->
  ScriptPurpose (Crypto era) ->
  StrictMaybe RdmrPtr
rdptr txBody (Minting (PolicyID hash)) =
  RdmrPtr Mint <$> indexOf hash (txBody ^. mintedTxBodyF :: Set (ScriptHash (Crypto era)))
rdptr txBody (Spending txin) = RdmrPtr Spend <$> indexOf txin (txBody ^. inputsTxBodyL)
rdptr txBody (Rewarding racnt) = RdmrPtr Rewrd <$> indexOf racnt (unWdrl (txBody ^. wdrlsTxBodyL))
rdptr txBody (Certifying d) = RdmrPtr Cert <$> indexOf d (txBody ^. certsTxBodyL)

rdptrInv ::
  forall era.
  ShelleyEraTxBody era =>
  Core.TxBody era ->
  RdmrPtr ->
  StrictMaybe (ScriptPurpose (Crypto era))
rdptrInv txBody (RdmrPtr Mint idx) =
  Minting . PolicyID <$> fromIndex idx (txBody ^. mintedTxBodyF)
rdptrInv txBody (RdmrPtr Spend idx) =
  Spending <$> fromIndex idx (txBody ^. inputsTxBodyL)
rdptrInv txBody (RdmrPtr Rewrd idx) =
  Rewarding <$> fromIndex idx (unWdrl (txBody ^. wdrlsTxBodyL))
rdptrInv txBody (RdmrPtr Cert idx) =
  Certifying <$> fromIndex idx (txBody ^. certsTxBodyL)

{-# DEPRECATED getMapFromValue "No longer used" #-}
getMapFromValue :: MaryValue crypto -> Map.Map (PolicyID crypto) (Map.Map AssetName Integer)
getMapFromValue (MaryValue _ (MultiAsset m)) = m

-- | Find the Data and ExUnits assigned to a script.
indexedRdmrs ::
  forall era.
  (ShelleyEraTxBody era, EraTx era, Witnesses era ~ TxWitness era) =>
  Tx era ->
  ScriptPurpose (Crypto era) ->
  Maybe (Data era, ExUnits)
indexedRdmrs tx sp = case rdptr @era (tx ^. bodyTxL) sp of
  SNothing -> Nothing
  SJust rPtr -> Map.lookup rPtr rdmrs
    where
      rdmrs = unRedeemers $ txrdmrs' (tx ^. witsTxL)

--------------------------------------------------------------------------------
-- Serialisation
--------------------------------------------------------------------------------

deriving newtype instance FromCBOR IsValid

deriving newtype instance ToCBOR IsValid

segwitTx ::
  Annotator (Core.TxBody era) ->
  Annotator (TxWitness era) ->
  IsValid ->
  Maybe (Annotator (AuxiliaryData era)) ->
  Annotator (AlonzoTx era)
segwitTx
  bodyAnn
  witsAnn
  isval
  metaAnn = Annotator $ \bytes ->
    let bodyb = runAnnotator bodyAnn bytes
        witnessSet = runAnnotator witsAnn bytes
        metadata = flip runAnnotator bytes <$> metaAnn
     in AlonzoTx
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
  ( Typeable era,
    ToCBOR (Core.TxBody era),
    ToCBOR (AuxiliaryData era)
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
  (Era era, ToCBOR (Core.TxBody era), ToCBOR (AuxiliaryData era)) =>
  ToCBOR (AlonzoTx era)
  where
  toCBOR = toCBORForMempoolSubmission

instance
  ( EraScript era,
    Script era ~ AlonzoScript era,
    FromCBOR (Annotator (Core.TxBody era)),
    FromCBOR (Annotator (AuxiliaryData era))
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
  Map.Map (ScriptHash (Crypto era)) (Script era) ->
  Addr (Crypto era) ->
  Bool
isTwoPhaseScriptAddressFromMap hashScriptMap addr =
  case Shelley.getScriptHash @(Crypto era) addr of
    Nothing -> False
    Just hash -> any ok hashScriptMap
      where
        ok script = hashScript @era script == hash && not (isNativeScript @era script)

alonzoInputHashes ::
  forall era.
  ( EraTxBody era,
    EraScript era,
    TxOut era ~ AlonzoTxOut era
  ) =>
  Map.Map (ScriptHash (Crypto era)) (Script era) ->
  AlonzoTx era ->
  UTxO era ->
  (Set (DataHash (Crypto era)), Set (TxIn (Crypto era)))
alonzoInputHashes hashScriptMap tx (UTxO mp) = Map.foldlWithKey' accum (Set.empty, Set.empty) smallUtxo
  where
    spendInputs :: Set (TxIn (Crypto era))
    spendInputs = body tx ^. inputsTxBodyL
    smallUtxo = eval (spendInputs ◁ mp)
    accum ans@(hashSet, inputSet) txin txout =
      case txout of
        (AlonzoTxOut addr _ SNothing) ->
          if isTwoPhaseScriptAddressFromMap @era hashScriptMap addr
            then (hashSet, Set.insert txin inputSet)
            else ans
        (AlonzoTxOut addr _ (SJust dhash)) ->
          if isTwoPhaseScriptAddressFromMap @era hashScriptMap addr
            then (Set.insert dhash hashSet, inputSet)
            else ans
