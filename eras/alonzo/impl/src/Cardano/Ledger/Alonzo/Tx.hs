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
  txdats',
  txscripts',
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
  decodeNullMaybe,
  encodeListLen,
  encodeNullMaybe,
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
import Control.DeepSeq (NFData (..))
import Control.Monad.Trans.Fail.String (errorFail)
import Data.Aeson (ToJSON (..))
import qualified Data.ByteString.Lazy as LBS
import Data.Int (Int64)
import qualified Data.Map.Strict as Map
import Data.Maybe (mapMaybe)
import Data.Maybe.Strict (
  StrictMaybe (..),
  maybeToStrictMaybe,
  strictMaybeToMaybe,
 )
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Typeable (Typeable)
import Data.Word (Word32)
import GHC.Generics (Generic)
import GHC.Stack (HasCallStack)
import Lens.Micro hiding (set)
import NoThunks.Class (NoThunks)

-- ===================================================

-- | Tag indicating whether non-native scripts in this transaction are expected
-- to validate. This is added by the block creator when constructing the block.
newtype IsValid = IsValid Bool
  deriving (Eq, Show, Generic)
  deriving newtype (NoThunks, NFData, ToCBOR, EncCBOR, DecCBOR, ToJSON)

data AlonzoTx era = AlonzoTx
  { atBody :: !(TxBody era)
  , atWits :: !(TxWits era)
  , atIsValid :: !IsValid
  , atAuxData :: !(StrictMaybe (TxAuxData era))
  }
  deriving (Generic)

instance EraTx AlonzoEra where
  newtype Tx AlonzoEra = MkAlonzoTx {unAlonzoTx :: AlonzoTx AlonzoEra}
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

alonzoTxEqRaw :: AlonzoEraTx era => Tx era -> Tx era -> Bool
alonzoTxEqRaw tx1 tx2 =
  shelleyTxEqRaw tx1 tx2 && (tx1 ^. isValidTxL == tx2 ^. isValidTxL)

instance EqRaw (Tx AlonzoEra) where
  eqRaw = alonzoTxEqRaw

alonzoTxL :: Lens' (Tx AlonzoEra) (AlonzoTx AlonzoEra)
alonzoTxL = lens unAlonzoTx $ const MkAlonzoTx

class
  (EraTx era, AlonzoEraTxBody era, AlonzoEraTxWits era, AlonzoEraScript era) =>
  AlonzoEraTx era
  where
  isValidTxL :: Lens' (Tx era) IsValid

instance DecCBOR (Annotator (Tx AlonzoEra)) where
  decCBOR = fmap MkAlonzoTx <$> decCBOR

instance AlonzoEraTx AlonzoEra where
  isValidTxL = alonzoTxL . isValidAlonzoTxL
  {-# INLINE isValidTxL #-}

mkBasicAlonzoTx :: Monoid (TxWits era) => TxBody era -> AlonzoTx era
mkBasicAlonzoTx txBody = AlonzoTx txBody mempty (IsValid True) SNothing

-- | `TxBody` setter and getter for `AlonzoTx`.
bodyAlonzoTxL :: Lens' (AlonzoTx era) (TxBody era)
bodyAlonzoTxL = lens atBody (\tx txBody -> tx {atBody = txBody})
{-# INLINEABLE bodyAlonzoTxL #-}

-- | `TxWits` setter and getter for `AlonzoTx`.
witsAlonzoTxL :: Lens' (AlonzoTx era) (TxWits era)
witsAlonzoTxL = lens atWits (\tx txWits -> tx {atWits = txWits})
{-# INLINEABLE witsAlonzoTxL #-}

-- | `TxAuxData` setter and getter for `AlonzoTx`.
auxDataAlonzoTxL :: Lens' (AlonzoTx era) (StrictMaybe (TxAuxData era))
auxDataAlonzoTxL = lens atAuxData (\tx txTxAuxData -> tx {atAuxData = txTxAuxData})
{-# INLINEABLE auxDataAlonzoTxL #-}

-- | txsize computes the length of the serialised bytes (for estimations)
sizeAlonzoTxF :: forall era. (HasCallStack, EraTx era) => SimpleGetter (AlonzoTx era) Word32
sizeAlonzoTxF =
  to $
    errorFail
      . integralToBounded @Int64 @Word32
      . LBS.length
      . serialize (eraProtVerLow @era)
      . toCBORForSizeComputation
{-# INLINEABLE sizeAlonzoTxF #-}

isValidAlonzoTxL :: Lens' (AlonzoTx era) IsValid
isValidAlonzoTxL = lens atIsValid (\tx valid -> tx {atIsValid = valid})
{-# INLINEABLE isValidAlonzoTxL #-}

deriving instance
  (Era era, Eq (TxBody era), Eq (TxWits era), Eq (TxAuxData era)) => Eq (AlonzoTx era)

deriving instance
  (Era era, Show (TxBody era), Show (TxAuxData era), Show (Script era), Show (TxWits era)) =>
  Show (AlonzoTx era)

instance
  ( Era era
  , NoThunks (TxWits era)
  , NoThunks (TxAuxData era)
  , NoThunks (TxBody era)
  ) =>
  NoThunks (AlonzoTx era)

instance
  ( Era era
  , NFData (TxWits era)
  , NFData (TxAuxData era)
  , NFData (TxBody era)
  ) =>
  NFData (AlonzoTx era)

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
  Tx era ->
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
  ( EncCBOR (TxBody era)
  , EncCBOR (TxWits era)
  , EncCBOR (TxAuxData era)
  ) =>
  AlonzoTx era ->
  Encoding
toCBORForSizeComputation AlonzoTx {atBody, atWits, atAuxData} =
  encodeListLen 3
    <> encCBOR atBody
    <> encCBOR atWits
    <> encodeNullMaybe encCBOR (strictMaybeToMaybe atAuxData)

alonzoMinFeeTx ::
  ( EraTx era
  , AlonzoEraTxWits era
  , AlonzoEraPParams era
  ) =>
  PParams era ->
  Tx era ->
  Coin
alonzoMinFeeTx pp tx =
  (tx ^. sizeTxF <×> pp ^. ppMinFeeAL)
    <+> (pp ^. ppMinFeeBL)
    <+> txscriptfee (pp ^. ppPricesL) allExunits
  where
    allExunits = totExUnits tx

totExUnits ::
  (EraTx era, AlonzoEraTxWits era) =>
  Tx era ->
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
  ( EncCBOR (TxBody era)
  , EncCBOR (TxWits era)
  , EncCBOR (TxAuxData era)
  ) =>
  AlonzoTx era ->
  Encoding
toCBORForMempoolSubmission
  AlonzoTx {atBody, atWits, atAuxData, atIsValid} =
    encode $
      Rec AlonzoTx
        !> To atBody
        !> To atWits
        !> To atIsValid
        !> E (encodeNullMaybe encCBOR . strictMaybeToMaybe) atAuxData

instance
  ( Era era
  , EncCBOR (TxBody era)
  , EncCBOR (TxAuxData era)
  , EncCBOR (TxWits era)
  ) =>
  EncCBOR (AlonzoTx era)
  where
  encCBOR = toCBORForMempoolSubmission

instance
  ( Era era
  , EncCBOR (TxBody era)
  , EncCBOR (TxAuxData era)
  , EncCBOR (TxWits era)
  ) =>
  ToCBOR (AlonzoTx era)
  where
  toCBOR = toEraCBOR @era

instance
  ( Typeable era
  , Typeable (TxBody era)
  , Typeable (TxWits era)
  , Typeable (TxAuxData era)
  , DecCBOR (Annotator (TxBody era))
  , DecCBOR (Annotator (TxWits era))
  , DecCBOR (Annotator (TxAuxData era))
  ) =>
  DecCBOR (Annotator (AlonzoTx era))
  where
  decCBOR =
    decode $
      Ann (RecD AlonzoTx)
        <*! From
        <*! From
        <*! Ann From
        <*! D
          ( sequence . maybeToStrictMaybe
              <$> decodeNullMaybe decCBOR
          )
  {-# INLINE decCBOR #-}
