{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE UndecidableSuperClasses #-}
{-# OPTIONS_GHC -Wno-orphans #-}

-- | This module exports implementations of many of the functions outlined in the Alonzo specification.
--     The link to source of the specification
--       https://github.com/intersectmbo/cardano-ledger/tree/master/eras/alonzo/formal-spec
--     The most recent version of the document can be found here:
--       https://github.com/intersectmbo/cardano-ledger/releases/latest/download/alonzo-ledger.pdf
--     The functions can be found in Figures in that document, and sections of this code refer to those figures.
module Cardano.Ledger.Alonzo.Tx (
  -- Figure 1
  CostModel,
  getLanguageView,
  -- Figure 2
  Data,
  DataHash,
  IsValid (..),
  hashData,
  nonNativeLanguages,
  hashScriptIntegrity,
  EraIndependentScriptIntegrity,
  ScriptIntegrity (ScriptIntegrity),
  ScriptIntegrityHash,
  -- Figure 3
  AlonzoTx (AlonzoTx, atBody, atWits, atIsValid, atAuxData),
  Tx (..),
  AlonzoEraTx (..),
  mkBasicAlonzoTx,
  bodyAlonzoTxL,
  witsAlonzoTxL,
  auxDataAlonzoTxL,
  sizeAlonzoTxF,
  isValidAlonzoTxL,
  txrdmrs,
  TxBody (AlonzoTxBody),
  -- Figure 4
  totExUnits,
  alonzoMinFeeTx,
  --  Figure 5
  Shelley.txouts,
  -- Other
  toCBORForSizeComputation,
  toCBORForMempoolSubmission,
  alonzoTxEqRaw,
  mkScriptIntegrity,
) where

import Cardano.Ledger.Allegra.Tx (validateTimelock)
import Cardano.Ledger.Alonzo.Era (AlonzoEra)
import Cardano.Ledger.Alonzo.PParams (
  AlonzoEraPParams,
  LangDepView (..),
  encodeLangViews,
  getLanguageView,
  ppPricesL,
 )
import Cardano.Ledger.Alonzo.Scripts (
  AlonzoEraScript (..),
  CostModel,
  ExUnits (..),
  plutusScriptLanguage,
  txscriptfee,
 )
import Cardano.Ledger.Alonzo.TxBody (
  AlonzoEraTxBody (..),
  ScriptIntegrityHash,
  TxBody (AlonzoTxBody),
 )
import Cardano.Ledger.Alonzo.TxWits (
  AlonzoEraTxWits (..),
  AlonzoTxWits (..),
  Redeemers (..),
  TxDats (..),
  txrdmrs,
  unRedeemersL,
  unTxDatsL,
 )
import Cardano.Ledger.BaseTypes (integralToBounded)
import Cardano.Ledger.Binary (
  Annotator,
  DecCBOR (..),
  EncCBOR (encCBOR),
  Encoding,
  ToCBOR (..),
  decodeNullStrictMaybe,
  encodeListLen,
  encodeNullStrictMaybe,
  serialize,
  serialize',
 )
import Cardano.Ledger.Binary.Coders
import Cardano.Ledger.Coin (Coin (..))
import Cardano.Ledger.Core
import Cardano.Ledger.Mary (Tx (..))
import Cardano.Ledger.MemoBytes (EqRaw (..))
import Cardano.Ledger.Plutus.Data (Data, hashData)
import Cardano.Ledger.Plutus.Language (nonNativeLanguages)
import Cardano.Ledger.Shelley.Tx (shelleyTxEqRaw)
import Cardano.Ledger.State (EraUTxO, ScriptsProvided (..))
import qualified Cardano.Ledger.State as Shelley
import Cardano.Ledger.Val (Val ((<+>), (<×>)))
import Control.DeepSeq (NFData (..), deepseq)
import Control.Monad.Trans.Fail.String (errorFail)
import Data.Aeson (ToJSON (..))
import qualified Data.ByteString.Lazy as LBS
import Data.Int (Int64)
import qualified Data.Map.Strict as Map
import Data.Maybe (mapMaybe)
import Data.Maybe.Strict (StrictMaybe (..))
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Typeable (Typeable)
import Data.Word (Word32)
import GHC.Generics (Generic)
import GHC.Stack (HasCallStack)
import Lens.Micro hiding (set)
import NoThunks.Class (InspectHeap (..), NoThunks)

-- ===================================================

-- | Tag indicating whether non-native scripts in this transaction are expected
-- to validate. This is added by the block creator when constructing the block.
newtype IsValid = IsValid Bool
  deriving (Eq, Show, Generic)
  deriving newtype (NoThunks, NFData, ToCBOR, EncCBOR, DecCBOR, ToJSON)

data AlonzoTx l era where
  AlonzoTx ::
    { atBody :: !(TxBody TopTx era)
    , atWits :: !(TxWits era)
    , atIsValid :: !IsValid
    , atAuxData :: !(StrictMaybe (TxAuxData era))
    } ->
    AlonzoTx TopTx era

instance HasEraTxLevel Tx AlonzoEra where
  toSTxLevel (MkAlonzoTx AlonzoTx {}) = STopTxOnly @AlonzoEra

instance EraTx AlonzoEra where
  newtype Tx l AlonzoEra = MkAlonzoTx {unAlonzoTx :: AlonzoTx l AlonzoEra}
    deriving newtype (Eq, NFData, EncCBOR, ToCBOR, NoThunks, Show)
    deriving (Generic)

  mkBasicTx = MkAlonzoTx . mkBasicAlonzoTx

  bodyTxL = alonzoTxL . bodyAlonzoTxL
  {-# INLINE bodyTxL #-}

  witsTxL = alonzoTxL . witsAlonzoTxL
  {-# INLINE witsTxL #-}

  auxDataTxL = alonzoTxL . auxDataAlonzoTxL
  {-# INLINE auxDataTxL #-}

  sizeTxF = alonzoTxL . sizeAlonzoTxF
  {-# INLINE sizeTxF #-}

  validateNativeScript = validateTimelock
  {-# INLINE validateNativeScript #-}

  getMinFeeTx pp tx _ = alonzoMinFeeTx pp tx
  {-# INLINE getMinFeeTx #-}

alonzoTxEqRaw ::
  ( AlonzoEraTx era
  , STxLevel l era ~ STxTopLevel l era
  ) =>
  Tx l era -> Tx l era -> Bool
alonzoTxEqRaw tx1 tx2 =
  withTopTxLevelOnly tx1 $ \tx1' ->
    withTopTxLevelOnly tx2 $ \tx2' ->
      shelleyTxEqRaw tx1 tx2 && (tx1' ^. isValidTxL == tx2' ^. isValidTxL)

instance EqRaw (Tx l AlonzoEra) where
  eqRaw = alonzoTxEqRaw

alonzoTxL :: Lens' (Tx l AlonzoEra) (AlonzoTx l AlonzoEra)
alonzoTxL = lens unAlonzoTx $ const MkAlonzoTx

class
  (EraTx era, AlonzoEraTxBody era, AlonzoEraTxWits era, AlonzoEraScript era) =>
  AlonzoEraTx era
  where
  isValidTxL :: Lens' (Tx TopTx era) IsValid

instance Typeable l => DecCBOR (Annotator (Tx l AlonzoEra)) where
  decCBOR = fmap MkAlonzoTx <$> decCBOR

instance AlonzoEraTx AlonzoEra where
  isValidTxL = alonzoTxL . isValidAlonzoTxL
  {-# INLINE isValidTxL #-}

mkBasicAlonzoTx ::
  ( EraTx era
  , STxLevel l era ~ STxTopLevel l era
  ) =>
  TxBody l era -> AlonzoTx l era
mkBasicAlonzoTx txBody =
  case toSTxLevel txBody of
    STopTxOnly ->
      AlonzoTx txBody mempty (IsValid True) SNothing

-- | `TxBody` setter and getter for `AlonzoTx`.
bodyAlonzoTxL :: Lens' (AlonzoTx l era) (TxBody l era)
bodyAlonzoTxL =
  lens (\AlonzoTx {atBody} -> atBody) $ \tx txBody ->
    case tx of
      AlonzoTx {} -> tx {atBody = txBody}
{-# INLINEABLE bodyAlonzoTxL #-}

-- | `TxWits` setter and getter for `AlonzoTx`.
witsAlonzoTxL :: Lens' (AlonzoTx l era) (TxWits era)
witsAlonzoTxL =
  lens (\AlonzoTx {atWits} -> atWits) $ \tx txWits ->
    case tx of
      AlonzoTx {} -> tx {atWits = txWits}
{-# INLINEABLE witsAlonzoTxL #-}

-- | `TxAuxData` setter and getter for `AlonzoTx`.
auxDataAlonzoTxL :: Lens' (AlonzoTx l era) (StrictMaybe (TxAuxData era))
auxDataAlonzoTxL =
  lens (\AlonzoTx {atAuxData} -> atAuxData) $ \tx txAuxData ->
    case tx of
      AlonzoTx {} -> tx {atAuxData = txAuxData}
{-# INLINEABLE auxDataAlonzoTxL #-}

-- | txsize computes the length of the serialised bytes (for estimations)
sizeAlonzoTxF :: forall era l. (HasCallStack, EraTx era) => SimpleGetter (AlonzoTx l era) Word32
sizeAlonzoTxF =
  to $
    errorFail
      . integralToBounded @Int64 @Word32
      . LBS.length
      . serialize (eraProtVerLow @era)
      . toCBORForSizeComputation
{-# INLINEABLE sizeAlonzoTxF #-}

isValidAlonzoTxL :: Lens' (AlonzoTx l era) IsValid
isValidAlonzoTxL =
  lens (\AlonzoTx {atIsValid} -> atIsValid) $ \tx txIsValid ->
    case tx of
      AlonzoTx {} -> tx {atIsValid = txIsValid}
{-# INLINEABLE isValidAlonzoTxL #-}

deriving instance
  (Era era, Eq (TxBody l era), Eq (TxWits era), Eq (TxAuxData era)) => Eq (AlonzoTx l era)

deriving instance
  (Era era, Show (TxBody l era), Show (TxAuxData era), Show (Script era), Show (TxWits era)) =>
  Show (AlonzoTx l era)

deriving via
  InspectHeap (AlonzoTx l era)
  instance
    (Typeable era, Typeable l) => NoThunks (AlonzoTx l era)

instance
  ( Era era
  , NFData (TxWits era)
  , NFData (TxAuxData era)
  , NFData (TxBody l era)
  ) =>
  NFData (AlonzoTx l era)
  where
  rnf AlonzoTx {..} =
    atBody `deepseq`
      atWits `deepseq`
        atAuxData `deepseq`
          rnf atIsValid

-- | A ScriptIntegrityHash is the hash of three things.  The first two come
-- from the witnesses and the last comes from the Protocol Parameters.
data ScriptIntegrity era
  = ScriptIntegrity
      !(Redeemers era) -- From the witnesses
      !(TxDats era)
      !(Set LangDepView) -- From the Protocol parameters
  deriving (Eq, Generic)

deriving instance AlonzoEraScript era => Show (ScriptIntegrity era)

deriving instance AlonzoEraScript era => NoThunks (ScriptIntegrity era)

-- ScriptIntegrity is not transmitted over the network. The bytes are independently
-- reconstructed by all nodes. There are no original bytes to preserve.
-- Instead, we must use a reproducable serialization
instance Era era => SafeToHash (ScriptIntegrity era) where
  originalBytes (ScriptIntegrity m d l) =
    let dBytes = if null (d ^. unTxDatsL) then mempty else originalBytes d
        lBytes = serialize' (eraProtVerLow @era) (encodeLangViews l)
     in originalBytes m <> dBytes <> lBytes

instance
  Era era =>
  HashAnnotated (ScriptIntegrity era) EraIndependentScriptIntegrity

hashScriptIntegrity :: Era era => ScriptIntegrity era -> ScriptIntegrityHash
hashScriptIntegrity = hashAnnotated

mkScriptIntegrity ::
  ( AlonzoEraPParams era
  , AlonzoEraTxWits era
  , EraUTxO era
  ) =>
  PParams era ->
  Tx l era ->
  ScriptsProvided era ->
  Set ScriptHash ->
  StrictMaybe (ScriptIntegrity era)
mkScriptIntegrity pp tx (ScriptsProvided scriptsProvided) scriptsNeeded
  | null (txRedeemers ^. unRedeemersL)
  , null langViews
  , null (txDats ^. unTxDatsL) =
      SNothing
  | otherwise = SJust $ ScriptIntegrity txRedeemers txDats langViews
  where
    scriptsUsed = Map.elems $ Map.restrictKeys scriptsProvided scriptsNeeded
    langs = Set.fromList $ plutusScriptLanguage <$> mapMaybe toPlutusScript scriptsUsed
    langViews = Set.map (getLanguageView pp) langs
    txWits = tx ^. witsTxL
    txRedeemers = txWits ^. rdmrsTxWitsL
    txDats = txWits ^. datsTxWitsL

-- ===============================================================
-- From the specification, Figure 4 "Functions related to fees"
-- ===============================================================

-- | This ensures that the size of transactions from Mary is unchanged.
-- The individual components all store their bytes; the only work we do in this
-- function is concatenating
toCBORForSizeComputation ::
  ( EncCBOR (TxBody l era)
  , EncCBOR (TxWits era)
  , EncCBOR (TxAuxData era)
  ) =>
  AlonzoTx l era ->
  Encoding
toCBORForSizeComputation AlonzoTx {atBody, atWits, atAuxData} =
  encodeListLen 3
    <> encCBOR atBody
    <> encCBOR atWits
    <> encodeNullStrictMaybe encCBOR atAuxData

alonzoMinFeeTx ::
  ( EraTx era
  , AlonzoEraTxWits era
  , AlonzoEraPParams era
  ) =>
  PParams era ->
  Tx l era ->
  Coin
alonzoMinFeeTx pp tx =
  (tx ^. sizeTxF <×> unCoinPerByte (pp ^. ppMinFeeFactorL))
    <+> (pp ^. ppMinFeeBL)
    <+> txscriptfee (pp ^. ppPricesL) allExunits
  where
    allExunits = totExUnits tx

totExUnits ::
  (EraTx era, AlonzoEraTxWits era) =>
  Tx l era ->
  ExUnits
totExUnits tx = foldMap snd $ tx ^. witsTxL . rdmrsTxWitsL . unRedeemersL

--------------------------------------------------------------------------------
-- Serialisation
--------------------------------------------------------------------------------

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
  ( EncCBOR (TxBody l era)
  , EncCBOR (TxWits era)
  , EncCBOR (TxAuxData era)
  ) =>
  AlonzoTx l era ->
  Encoding
toCBORForMempoolSubmission
  AlonzoTx {atBody, atWits, atAuxData, atIsValid} =
    encode $
      Rec AlonzoTx
        !> To atBody
        !> To atWits
        !> To atIsValid
        !> E (encodeNullStrictMaybe encCBOR) atAuxData

instance
  ( Era era
  , EncCBOR (TxBody l era)
  , EncCBOR (TxAuxData era)
  , EncCBOR (TxWits era)
  ) =>
  EncCBOR (AlonzoTx l era)
  where
  encCBOR = toCBORForMempoolSubmission

instance
  ( Era era
  , EncCBOR (TxBody l era)
  , EncCBOR (TxAuxData era)
  , EncCBOR (TxWits era)
  , Typeable l
  ) =>
  ToCBOR (AlonzoTx l era)
  where
  toCBOR = toEraCBOR @era

instance
  ( Typeable l
  , Era era
  , Typeable (TxBody l era)
  , Typeable (TxWits era)
  , Typeable (TxAuxData era)
  , DecCBOR (Annotator (TxBody l era))
  , DecCBOR (Annotator (TxWits era))
  , DecCBOR (Annotator (TxAuxData era))
  ) =>
  DecCBOR (Annotator (AlonzoTx l era))
  where
  decCBOR =
    withSTxTopLevelM @l @era $ \case
      STopTxOnly ->
        decode $
          Ann (RecD AlonzoTx)
            <*! From
            <*! From
            <*! Ann From
            <*! D (sequence <$> decodeNullStrictMaybe decCBOR)
  {-# INLINE decCBOR #-}
