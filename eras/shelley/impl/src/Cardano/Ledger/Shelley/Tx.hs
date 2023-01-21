{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
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
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Cardano.Ledger.Shelley.Tx (
  -- * Transaction
  ShelleyTx (
    ShelleyTx,
    body,
    wits,
    auxiliaryData
  ),
  bodyShelleyTxL,
  witsShelleyTxL,
  auxDataShelleyTxL,
  sizeShelleyTxF,
  segwitTx,
  mkBasicShelleyTx,
  txwitsScript,
  extractKeyHashWitnessSet,
  evalNativeMultiSigScript,
  hashMultiSigScript,
  nativeMultiSigTag,
  validateNativeMultiSigScript,
  minfee,
  shelleyMinFeeTx,
  witsFromTxWitnesses,

  -- * Re-exports
  ShelleyTxBody (..),
  ShelleyTxOut (..),
  TxIn (..),
  TxId (..),
)
where

import Cardano.Ledger.Binary (
  Annotator (..),
  EncCBOR (encCBOR),
  FromCBOR (fromCBOR),
  ToCBOR (toCBOR),
  decodeNullMaybe,
  encodeListLen,
  encodeNull,
  encodeNullMaybe,
  fromPlainEncoding,
  runAnnotator,
  serializeEncoding,
 )
import Cardano.Ledger.Binary.Coders
import qualified Cardano.Ledger.Binary.Plain as Plain
import Cardano.Ledger.Coin (Coin)
import Cardano.Ledger.Credential (Credential (..))
import Cardano.Ledger.Crypto (Crypto, StandardCrypto)
import Cardano.Ledger.Keys (HasKeyRole (coerceKeyRole), KeyHash, KeyRole (Witness), asWitness)
import Cardano.Ledger.Keys.Bootstrap (bootstrapWitKeyHash)
import Cardano.Ledger.Keys.WitVKey (witVKeyHash)
import Cardano.Ledger.MemoBytes (Mem, MemoBytes, memoBytes, mkMemoBytes, pattern Memo)
import Cardano.Ledger.SafeHash (SafeToHash (..))
import Cardano.Ledger.Shelley.Core
import Cardano.Ledger.Shelley.Era (ShelleyEra)
import Cardano.Ledger.Shelley.Scripts (MultiSig (..), nativeMultiSigTag)
import Cardano.Ledger.Shelley.TxAuxData ()
import Cardano.Ledger.Shelley.TxBody (ShelleyTxBody (..), ShelleyTxOut (..))
import Cardano.Ledger.Shelley.TxWits ()
import Cardano.Ledger.TxIn (TxId (..), TxIn (..))
import Cardano.Ledger.Val ((<+>), (<×>))
import Control.DeepSeq (NFData)
import qualified Data.ByteString.Lazy as LBS
import qualified Data.ByteString.Short as SBS
import Data.Map.Strict (Map)
import Data.Maybe.Strict (
  StrictMaybe (..),
  maybeToStrictMaybe,
  strictMaybeToMaybe,
 )
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Typeable (Typeable)
import GHC.Generics (Generic)
import Lens.Micro (Lens', SimpleGetter, lens, to, (^.))
import NoThunks.Class (NoThunks (..))

-- ========================================================

data ShelleyTxRaw era = ShelleyTxRaw
  { strBody :: !(TxBody era)
  , strWits :: !(TxWits era)
  , strAuxiliaryData :: !(StrictMaybe (TxAuxData era))
  }
  deriving (Generic, Typeable)

instance
  ( NFData (TxBody era)
  , NFData (TxWits era)
  , NFData (TxAuxData era)
  ) =>
  NFData (ShelleyTxRaw era)

deriving instance
  ( Era era
  , Eq (TxBody era)
  , Eq (TxWits era)
  , Eq (TxAuxData era)
  ) =>
  Eq (ShelleyTxRaw era)

deriving instance
  ( Era era
  , Show (TxBody era)
  , Show (TxWits era)
  , Show (TxAuxData era)
  ) =>
  Show (ShelleyTxRaw era)

instance
  ( Era era
  , NoThunks (TxAuxData era)
  , NoThunks (TxBody era)
  , NoThunks (TxWits era)
  ) =>
  NoThunks (ShelleyTxRaw era)

newtype ShelleyTx era = TxConstr (MemoBytes ShelleyTxRaw era)
  deriving newtype (SafeToHash, EncCBOR)

-- | `TxBody` setter and getter for `ShelleyTx`. The setter does update
-- memoized binary representation.
bodyShelleyTxL :: EraTx era => Lens' (ShelleyTx era) (TxBody era)
bodyShelleyTxL =
  lens
    (\(TxConstr (Memo tx _)) -> strBody tx)
    (\(TxConstr (Memo tx _)) txBody -> TxConstr $ memoBytes $ encodeShelleyTxRaw $ tx {strBody = txBody})
{-# INLINEABLE bodyShelleyTxL #-}

-- | `TxWits` setter and getter for `ShelleyTx`. The setter does update
-- memoized binary representation.
witsShelleyTxL :: EraTx era => Lens' (ShelleyTx era) (TxWits era)
witsShelleyTxL =
  lens
    (\(TxConstr (Memo tx _)) -> strWits tx)
    (\(TxConstr (Memo tx _)) txWits -> TxConstr $ memoBytes $ encodeShelleyTxRaw $ tx {strWits = txWits})
{-# INLINEABLE witsShelleyTxL #-}

-- | `TxAuxData` setter and getter for `ShelleyTx`. The setter does update
-- memoized binary representation.
auxDataShelleyTxL :: EraTx era => Lens' (ShelleyTx era) (StrictMaybe (TxAuxData era))
auxDataShelleyTxL =
  lens
    (\(TxConstr (Memo tx _)) -> strAuxiliaryData tx)
    (\(TxConstr (Memo tx _)) auxData -> mkShelleyTx $ tx {strAuxiliaryData = auxData})
{-# INLINEABLE auxDataShelleyTxL #-}

-- | Size getter for `ShelleyTx`.
sizeShelleyTxF :: Era era => SimpleGetter (ShelleyTx era) Integer
sizeShelleyTxF = to (\(TxConstr (Memo _ bytes)) -> fromIntegral $ SBS.length bytes)
{-# INLINEABLE sizeShelleyTxF #-}

mkShelleyTx :: EraTx era => ShelleyTxRaw era -> ShelleyTx era
mkShelleyTx = TxConstr . memoBytes . encodeShelleyTxRaw
{-# INLINEABLE mkShelleyTx #-}

mkBasicShelleyTx :: EraTx era => TxBody era -> ShelleyTx era
mkBasicShelleyTx txBody =
  mkShelleyTx $
    ShelleyTxRaw
      { strBody = txBody
      , strWits = mkBasicTxWits
      , strAuxiliaryData = SNothing
      }

instance Crypto c => EraTx (ShelleyEra c) where
  {-# SPECIALIZE instance EraTx (ShelleyEra StandardCrypto) #-}

  type Tx (ShelleyEra c) = ShelleyTx (ShelleyEra c)

  mkBasicTx = mkBasicShelleyTx

  bodyTxL = bodyShelleyTxL
  {-# INLINE bodyTxL #-}

  witsTxL = witsShelleyTxL
  {-# INLINE witsTxL #-}

  auxDataTxL = auxDataShelleyTxL
  {-# INLINE auxDataTxL #-}

  sizeTxF = sizeShelleyTxF
  {-# INLINE sizeTxF #-}

  validateScript (Phase1Script multisig) tx = validateNativeMultiSigScript multisig tx
  {-# INLINE validateScript #-}

  getMinFeeTx = shelleyMinFeeTx

deriving newtype instance
  ( NFData (TxBody era)
  , NFData (TxWits era)
  , NFData (TxAuxData era)
  ) =>
  NFData (ShelleyTx era)

deriving newtype instance
  ( Era era
  , Eq (TxBody era)
  , Eq (TxWits era)
  , Eq (TxAuxData era)
  ) =>
  Eq (ShelleyTx era)

deriving newtype instance
  (Era era, Show (TxBody era), Show (TxWits era), Show (TxAuxData era)) =>
  Show (ShelleyTx era)

deriving newtype instance
  ( Era era
  , NoThunks (TxAuxData era)
  , NoThunks (TxBody era)
  , NoThunks (TxWits era)
  ) =>
  NoThunks (ShelleyTx era)

pattern ShelleyTx ::
  EraTx era =>
  TxBody era ->
  TxWits era ->
  StrictMaybe (TxAuxData era) ->
  ShelleyTx era
pattern ShelleyTx {body, wits, auxiliaryData} <-
  TxConstr
    ( Memo
        ShelleyTxRaw
          { strBody = body
          , strWits = wits
          , strAuxiliaryData = auxiliaryData
          }
        _
      )
  where
    ShelleyTx b w a = mkShelleyTx $ ShelleyTxRaw b w a

{-# COMPLETE ShelleyTx #-}

--------------------------------------------------------------------------------
-- Serialisation
--------------------------------------------------------------------------------

encodeShelleyTxRaw ::
  (EncCBOR (TxWits era), EncCBOR (TxBody era), EncCBOR (TxAuxData era)) =>
  ShelleyTxRaw era ->
  Encode ('Closed 'Dense) (ShelleyTxRaw era)
encodeShelleyTxRaw ShelleyTxRaw {strBody, strWits, strAuxiliaryData} =
  Rec ShelleyTxRaw
    !> Enc strBody
    !> Enc strWits
    !> E (encodeNullMaybe (fromPlainEncoding . encCBOR) . strictMaybeToMaybe) strAuxiliaryData

instance
  (Era era, EncCBOR (TxWits era), EncCBOR (TxBody era), EncCBOR (TxAuxData era)) =>
  ToCBOR (ShelleyTxRaw era)
  where
  toCBOR = encode . encodeShelleyTxRaw

instance
  (Era era, EncCBOR (TxWits era), EncCBOR (TxBody era), EncCBOR (TxAuxData era)) =>
  ToCBOR (ShelleyTx era)
  where
  toCBOR = fromPlainEncoding . encCBOR

instance
  ( Era era
  , FromCBOR (Annotator (TxBody era))
  , FromCBOR (Annotator (TxWits era))
  , FromCBOR (Annotator (TxAuxData era))
  ) =>
  FromCBOR (Annotator (ShelleyTxRaw era))
  where
  fromCBOR =
    decode $
      Ann (RecD ShelleyTxRaw)
        <*! From
        <*! From
        <*! D
          ( sequence . maybeToStrictMaybe
              <$> decodeNullMaybe fromCBOR
          )

deriving via
  Mem ShelleyTxRaw era
  instance
    EraTx era => FromCBOR (Annotator (ShelleyTx era))

-- | Construct a Tx containing the explicit serialised bytes.
--
--   This function is marked as unsafe since it makes no guarantee that the
--   represented bytes are indeed the correct serialisation of the transaction.
--   Thus, when calling this function, the caller is responsible for making this
--   guarantee.
--
--   The only intended use case for this is for segregated witness.
unsafeConstructTxWithBytes ::
  Era era =>
  TxBody era ->
  TxWits era ->
  StrictMaybe (TxAuxData era) ->
  LBS.ByteString ->
  ShelleyTx era
unsafeConstructTxWithBytes b w a bytes = TxConstr (mkMemoBytes (ShelleyTxRaw b w a) bytes)

--------------------------------------------------------------------------------
-- Segregated witness
--------------------------------------------------------------------------------

segwitTx ::
  forall era.
  EraTx era =>
  Annotator (TxBody era) ->
  Annotator (TxWits era) ->
  Maybe (Annotator (TxAuxData era)) ->
  Annotator (ShelleyTx era)
segwitTx
  bodyAnn
  witsAnn
  metaAnn = Annotator $ \bytes ->
    let version = eraProtVerLow @era
        body' = runAnnotator bodyAnn bytes
        witnessSet = runAnnotator witsAnn bytes
        metadata = flip runAnnotator bytes <$> metaAnn
        wrappedMetadataBytes = case metadata of
          Nothing -> serializeEncoding version encodeNull
          Just b -> Plain.serialize b
        fullBytes =
          serializeEncoding version (encodeListLen 3)
            <> Plain.serialize body'
            <> Plain.serialize witnessSet
            <> wrappedMetadataBytes
     in unsafeConstructTxWithBytes
          body'
          witnessSet
          (maybeToStrictMaybe metadata)
          fullBytes

-- ===============================================================

-- | Hashes native multi-signature script.
hashMultiSigScript ::
  forall era.
  ( EraScript era
  , Script era ~ MultiSig (EraCrypto era)
  ) =>
  MultiSig (EraCrypto era) ->
  ScriptHash (EraCrypto era)
hashMultiSigScript = hashScript @era

-- ========================================

-- | Script evaluator for native multi-signature scheme. 'vhks' is the set of
-- key hashes that signed the transaction to be validated.
evalNativeMultiSigScript ::
  Era era =>
  MultiSig era ->
  Set (KeyHash 'Witness (EraCrypto era)) ->
  Bool
evalNativeMultiSigScript (RequireSignature hk) vhks = Set.member hk vhks
evalNativeMultiSigScript (RequireAllOf msigs) vhks =
  all (`evalNativeMultiSigScript` vhks) msigs
evalNativeMultiSigScript (RequireAnyOf msigs) vhks =
  any (`evalNativeMultiSigScript` vhks) msigs
evalNativeMultiSigScript (RequireMOf m msigs) vhks =
  m <= sum [if evalNativeMultiSigScript msig vhks then 1 else 0 | msig <- msigs]

-- | Script validator for native multi-signature scheme.
validateNativeMultiSigScript ::
  EraTx era =>
  MultiSig era ->
  Tx era ->
  Bool
validateNativeMultiSigScript msig tx =
  evalNativeMultiSigScript msig (coerceKeyRole `Set.map` vhks)
  where
    vhks = Set.map witVKeyHash (tx ^. witsTxL . addrTxWitsL)

-- | Multi-signature script witness accessor function for Transactions
txwitsScript ::
  EraTx era =>
  Tx era ->
  Map (ScriptHash (EraCrypto era)) (Script era)
txwitsScript tx = tx ^. witsTxL . scriptTxWitsL

extractKeyHashWitnessSet ::
  forall (r :: KeyRole) c.
  [Credential r c] ->
  Set (KeyHash 'Witness c)
extractKeyHashWitnessSet = foldr accum Set.empty
  where
    accum (KeyHashObj hk) ans = Set.insert (asWitness hk) ans
    accum _other ans = ans

-- | Minimum fee calculation
shelleyMinFeeTx :: EraTx era => PParams era -> Tx era -> Coin
shelleyMinFeeTx pp tx =
  (tx ^. sizeTxF <×> pp ^. ppMinFeeAL) <+> pp ^. ppMinFeeBL

minfee :: EraTx era => PParams era -> Tx era -> Coin
minfee = shelleyMinFeeTx
{-# DEPRECATED minfee "In favor of `getMinFeeTx`" #-}

-- | Extract the witness hashes from the Transaction.
witsFromTxWitnesses ::
  EraTx era =>
  Tx era ->
  Set (KeyHash 'Witness (EraCrypto era))
witsFromTxWitnesses tx =
  Set.map witVKeyHash (tx ^. witsTxL . addrTxWitsL)
    `Set.union` Set.map bootstrapWitKeyHash (tx ^. witsTxL . bootAddrTxWitsL)
