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
{-# LANGUAGE PatternSynonyms #-}
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
  AlonzoTx (AlonzoTx, body, wits, isValid, auxiliaryData),
  AlonzoEraTx (..),
  mkBasicAlonzoTx,
  bodyAlonzoTxL,
  witsAlonzoTxL,
  auxDataAlonzoTxL,
  sizeAlonzoTxF,
  wireSizeAlonzoTxF,
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
  alonzoEqTxRaw,
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
  AlonzoEraScript,
  CostModel,
  ExUnits (..),
  txscriptfee,
 )
import Cardano.Ledger.Alonzo.TxBody (
  AlonzoEraTxBody (..),
  AlonzoTxBodyUpgradeError,
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
import Cardano.Ledger.Binary (
  DecCBOR (..),
  EncCBOR (encCBOR),
  Encoding,
  ToCBOR (..),
  decodeNullStrictMaybe,
  encodeListLen,
  encodeNullMaybe,
  serialize,
  serialize',
 )
import Cardano.Ledger.Binary.Coders
import Cardano.Ledger.Coin (Coin (..))
import Cardano.Ledger.Core
import Cardano.Ledger.MemoBytes (EqRaw (..))
import Cardano.Ledger.Plutus.Data (Data, hashData)
import Cardano.Ledger.Plutus.Language (nonNativeLanguages)
import Cardano.Ledger.Shelley.Tx (ShelleyTx (ShelleyTx), shelleyEqTxRaw)
import qualified Cardano.Ledger.State as Shelley
import Cardano.Ledger.Val (Val ((<+>), (<×>)))
import Control.Arrow (left)
import Control.DeepSeq (NFData (..))
import Data.Aeson (ToJSON (..))
import qualified Data.ByteString.Lazy as LBS
import Data.Maybe.Strict (
  StrictMaybe (..),
  strictMaybeToMaybe,
 )
import Data.Set (Set)
import Data.Typeable (Typeable)
import Data.Word (Word32)
import GHC.Generics (Generic)
import Lens.Micro hiding (set)
import NoThunks.Class (NoThunks)

-- ===================================================

-- | Tag indicating whether non-native scripts in this transaction are expected
-- to validate. This is added by the block creator when constructing the block.
newtype IsValid = IsValid Bool
  deriving (Eq, Show, Generic)
  deriving newtype (NoThunks, NFData, ToCBOR, EncCBOR, DecCBOR, ToJSON)

data AlonzoTx era = AlonzoTx
  { body :: !(TxBody era)
  , wits :: !(TxWits era)
  , isValid :: !IsValid
  , auxiliaryData :: !(StrictMaybe (TxAuxData era))
  }
  deriving (Generic)

newtype AlonzoTxUpgradeError = ATUEBodyUpgradeError AlonzoTxBodyUpgradeError
  deriving (Show)

instance EraTx AlonzoEra where
  type Tx AlonzoEra = AlonzoTx AlonzoEra
  type TxUpgradeError AlonzoEra = AlonzoTxUpgradeError

  mkBasicTx = mkBasicAlonzoTx

  bodyTxL = bodyAlonzoTxL
  {-# INLINE bodyTxL #-}

  witsTxL = witsAlonzoTxL
  {-# INLINE witsTxL #-}

  auxDataTxL = auxDataAlonzoTxL
  {-# INLINE auxDataTxL #-}

  sizeTxF = sizeAlonzoTxF
  {-# INLINE sizeTxF #-}

  wireSizeTxF = wireSizeAlonzoTxF
  {-# INLINE wireSizeTxF #-}

  validateNativeScript = validateTimelock
  {-# INLINE validateNativeScript #-}

  getMinFeeTx pp tx _ = alonzoMinFeeTx pp tx
  {-# INLINE getMinFeeTx #-}

  upgradeTx (ShelleyTx body wits aux) =
    AlonzoTx
      <$> left ATUEBodyUpgradeError (upgradeTxBody body)
      <*> pure (upgradeTxWits wits)
      <*> pure (IsValid True)
      <*> pure (fmap upgradeTxAuxData aux)

instance (Tx era ~ AlonzoTx era, AlonzoEraTx era) => EqRaw (AlonzoTx era) where
  eqRaw = alonzoEqTxRaw

class
  (EraTx era, AlonzoEraTxBody era, AlonzoEraTxWits era, AlonzoEraScript era) =>
  AlonzoEraTx era
  where
  isValidTxL :: Lens' (Tx era) IsValid

instance AlonzoEraTx AlonzoEra where
  isValidTxL = isValidAlonzoTxL
  {-# INLINE isValidTxL #-}

mkBasicAlonzoTx :: Monoid (TxWits era) => TxBody era -> AlonzoTx era
mkBasicAlonzoTx txBody = AlonzoTx txBody mempty (IsValid True) SNothing

-- | `TxBody` setter and getter for `AlonzoTx`.
bodyAlonzoTxL :: Lens' (AlonzoTx era) (TxBody era)
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

-- | txsize computes the length of the serialised bytes (for estimations)
sizeAlonzoTxF :: forall era. EraTx era => SimpleGetter (AlonzoTx era) Integer
sizeAlonzoTxF =
  to $
    fromIntegral
      . LBS.length
      . serialize (eraProtVerLow @era)
      . toCBORForSizeComputation
{-# INLINEABLE sizeAlonzoTxF #-}

-- | txsize computes the length of the serialised bytes (actual size)
wireSizeAlonzoTxF :: forall era. EraTx era => SimpleGetter (AlonzoTx era) Word32
wireSizeAlonzoTxF =
  to $
    checkedFromIntegral
      . LBS.length
      . serialize (eraProtVerLow @era)
      . encCBOR
  where
    checkedFromIntegral n =
      if n <= fromIntegral (maxBound :: Word32)
        then fromIntegral n
        else error $ "Impossible: Size of the transaction is too big: " ++ show n
{-# INLINEABLE wireSizeAlonzoTxF #-}

isValidAlonzoTxL :: Lens' (AlonzoTx era) IsValid
isValidAlonzoTxL = lens isValid (\tx valid -> tx {isValid = valid})
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

hashScriptIntegrity ::
  forall era.
  AlonzoEraScript era =>
  Set LangDepView ->
  Redeemers era ->
  TxDats era ->
  StrictMaybe ScriptIntegrityHash
hashScriptIntegrity langViews rdmrs dats =
  if null (rdmrs ^. unRedeemersL) && null langViews && null (dats ^. unTxDatsL)
    then SNothing
    else SJust (hashAnnotated (ScriptIntegrity rdmrs dats langViews))

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
toCBORForSizeComputation AlonzoTx {body, wits, auxiliaryData} =
  encodeListLen 3
    <> encCBOR body
    <> encCBOR wits
    <> encodeNullMaybe encCBOR (strictMaybeToMaybe auxiliaryData)

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
  AlonzoTx {body, wits, auxiliaryData, isValid} =
    encode $
      Rec AlonzoTx
        !> To body
        !> To wits
        !> To isValid
        !> E (encodeNullMaybe encCBOR . strictMaybeToMaybe) auxiliaryData

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
  , DecCBOR (TxBody era)
  , DecCBOR (TxWits era)
  , DecCBOR (TxAuxData era)
  ) =>
  DecCBOR (AlonzoTx era)
  where
  decCBOR =
    decode $
      RecD AlonzoTx
        <! From
        <! From
        <! From
        <! D (decodeNullStrictMaybe decCBOR)
  {-# INLINE decCBOR #-}

alonzoEqTxRaw :: AlonzoEraTx era => Tx era -> Tx era -> Bool
alonzoEqTxRaw tx1 tx2 =
  shelleyEqTxRaw tx1 tx2 && (tx1 ^. isValidTxL == tx2 ^. isValidTxL)
