{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Cardano.Ledger.Alonzo.Tx
  ( IsValidating (..),
    Tx (Tx, body, wits, isValidating, auxiliaryData),
    ScriptPurpose (..),
    Indexable (..),
    txrdmrs,
    rdptr,
    getMapFromValue,
    indexedRdmrs,
  )
where

import Cardano.Binary (FromCBOR (..), ToCBOR (..))
import Cardano.Ledger.Alonzo.Data (Data)
import qualified Cardano.Ledger.Alonzo.Scripts as AlonzoScript (Tag (..))
import Cardano.Ledger.Alonzo.TxBody (AlonzoBody, TxBody (..), TxIn)
import Cardano.Ledger.Alonzo.TxWitness (RdmrPtr (..), TxWitness (..))
import Cardano.Ledger.Compactible
import qualified Cardano.Ledger.Core as Core
import Cardano.Ledger.Era (Crypto, Era)
import Cardano.Ledger.Mary.Value (AssetName, PolicyID, Value (..))
import Cardano.Ledger.Val (DecodeMint, DecodeNonNegative, Val)
import Data.Coders
import qualified Data.Map as Map
import Data.MemoBytes (Mem, MemoBytes (Memo), memoBytes)
import Data.Sequence.Strict (StrictSeq)
import qualified Data.Sequence.Strict as StrictSeq
import qualified Data.Set as Set
import Data.Typeable (Typeable)
import Data.Word (Word64)
import GHC.Generics (Generic)
import NoThunks.Class (NoThunks)
import Shelley.Spec.Ledger.Address (RewardAcnt)
import Shelley.Spec.Ledger.BaseTypes (StrictMaybe, maybeToStrictMaybe, strictMaybeToMaybe)
import Shelley.Spec.Ledger.Delegation.Certificates (DCert)
import Shelley.Spec.Ledger.TxBody (Wdrl (..), unWdrl)

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
    Eq (CompactForm (Core.Value era))
  ) =>
  Eq (TxRaw era)

deriving instance
  ( Era era,
    Compactible (Core.Value era),
    Show (Core.AuxiliaryData era),
    Show (Core.Script era),
    Show (Core.Value era),
    Show (CompactForm (Core.Value era))
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
    Eq (CompactForm (Core.Value era))
  ) =>
  Eq (Tx era)

deriving newtype instance
  ( Era era,
    Compactible (Core.Value era),
    Show (Core.AuxiliaryData era),
    Show (Core.Script era),
    Show (Core.Value era),
    Show (CompactForm (Core.Value era))
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

-- ===========================================
-- Operations on scripts from specification
-- Figure 6:Indexing script and data objects

data ScriptPurpose crypto
  = Minting !(PolicyID crypto)
  | Spending !(TxIn crypto)
  | Rewarding !(RewardAcnt crypto) -- Not sure if this is the right type.
  | Certifying !(DCert crypto)

class Indexable elem container where
  indexOf :: elem -> container -> Word64
  atIndex :: Word64 -> container -> elem

instance Ord k => Indexable k (Set.Set k) where
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
rdptr txbody (Minting pid) = RdmrPtr AlonzoScript.Mint (indexOf pid (getMapFromValue (mint txbody)))
rdptr txbody (Spending txin) = RdmrPtr AlonzoScript.Spend (indexOf txin (inputs txbody))
rdptr txbody (Rewarding racnt) = RdmrPtr AlonzoScript.Rewrd (indexOf racnt (unWdrl (wdrls txbody)))
rdptr txbody (Certifying d) = RdmrPtr AlonzoScript.Cert (indexOf d (certs txbody))

getMapFromValue :: Value crypto -> Map.Map (PolicyID crypto) (Map.Map AssetName Integer)
getMapFromValue (Value _ m) = m

txrdmrs ::
  (Era era, ToCBOR (Core.Script era)) =>
  TxWitness era ->
  Map.Map RdmrPtr (Data era)
txrdmrs (TxWitness {witsRdmr = m}) = m

indexedRdmrs ::
  ( Era era,
    ToCBOR (Core.AuxiliaryData era),
    ToCBOR (Core.Script era),
    ToCBOR (CompactForm (Core.Value era))
  ) =>
  Tx era ->
  ScriptPurpose (Crypto era) ->
  Maybe (Data era)
indexedRdmrs tx sp = Map.lookup policyid (txrdmrs . wits $ tx)
  where
    policyid = rdptr (body tx) sp
