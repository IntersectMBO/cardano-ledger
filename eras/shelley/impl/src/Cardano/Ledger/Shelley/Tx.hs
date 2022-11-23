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

module Cardano.Ledger.Shelley.Tx
  ( -- * Transaction
    Tx,
    ShelleyTx
      ( ShelleyTx,
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
    TxBody,
    ShelleyTxBody (..),
    TxOut,
    ShelleyTxOut (..),
    TxIn (..),
    TxId (..),
  )
where

import Cardano.Ledger.Binary
  ( Annotator (..),
    FromCBOR (fromCBOR),
    ToCBOR (toCBOR),
    decodeNullMaybe,
    encodeListLen,
    encodeNull,
    encodeNullMaybe,
    runAnnotator,
    serialize,
    serializeEncoding,
  )
import Cardano.Ledger.Binary.Coders
import Cardano.Ledger.Coin (Coin (Coin))
import Cardano.Ledger.Core hiding (Tx, TxBody, TxOut)
import qualified Cardano.Ledger.Core as Core
import Cardano.Ledger.Credential (Credential (..))
import qualified Cardano.Ledger.Crypto as CC (Crypto, StandardCrypto)
import Cardano.Ledger.Keys (HasKeyRole (coerceKeyRole), KeyHash, KeyRole (Witness), asWitness)
import Cardano.Ledger.Keys.Bootstrap (bootstrapWitKeyHash)
import Cardano.Ledger.Keys.WitVKey (witVKeyHash)
import Cardano.Ledger.MemoBytes (Mem, MemoBytes, memoBytes, mkMemoBytes, pattern Memo)
import Cardano.Ledger.SafeHash (SafeToHash (..))
import Cardano.Ledger.Shelley.Era (ShelleyEra)
import Cardano.Ledger.Shelley.Metadata ()
import Cardano.Ledger.Shelley.PParams (ShelleyPParamsHKD (..))
import Cardano.Ledger.Shelley.Scripts (MultiSig (..), nativeMultiSigTag)
import Cardano.Ledger.Shelley.TxBody (ShelleyTxBody (..), ShelleyTxOut (..), TxBody, TxOut)
import Cardano.Ledger.Shelley.TxWits ()
import Cardano.Ledger.TxIn (TxId (..), TxIn (..))
import Control.DeepSeq (NFData)
import qualified Data.ByteString.Lazy as LBS
import qualified Data.ByteString.Short as SBS
import Data.Map.Strict (Map)
import Data.Maybe.Strict
  ( StrictMaybe (..),
    maybeToStrictMaybe,
    strictMaybeToMaybe,
  )
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Typeable (Typeable)
import GHC.Generics (Generic)
import GHC.Records
import Lens.Micro (Lens', SimpleGetter, lens, to, (^.))
import NoThunks.Class (NoThunks (..))
import Numeric.Natural (Natural)

-- ========================================================

data TxRaw era = TxRaw
  { trBody :: !(Core.TxBody era),
    trWits :: !(TxWits era),
    trAuxiliaryData :: !(StrictMaybe (TxAuxData era))
  }
  deriving (Generic, Typeable)

instance
  ( NFData (Core.TxBody era),
    NFData (TxWits era),
    NFData (TxAuxData era)
  ) =>
  NFData (TxRaw era)

deriving instance
  ( Era era,
    Eq (Core.TxBody era),
    Eq (TxWits era),
    Eq (TxAuxData era)
  ) =>
  Eq (TxRaw era)

deriving instance
  ( Era era,
    Show (Core.TxBody era),
    Show (TxWits era),
    Show (TxAuxData era)
  ) =>
  Show (TxRaw era)

instance
  ( Era era,
    NoThunks (TxAuxData era),
    NoThunks (Core.TxBody era),
    NoThunks (TxWits era)
  ) =>
  NoThunks (TxRaw era)

newtype ShelleyTx era = TxConstr (MemoBytes TxRaw era)
  deriving newtype (SafeToHash, ToCBOR)

type Tx era = ShelleyTx era

{-# DEPRECATED Tx "Use `ShelleyTx` instead" #-}

-- | `Core.TxBody` setter and getter for `ShelleyTx`. The setter does update
-- memoized binary representation.
bodyShelleyTxL :: EraTx era => Lens' (ShelleyTx era) (Core.TxBody era)
bodyShelleyTxL =
  lens
    (\(TxConstr (Memo tx _)) -> trBody tx)
    (\(TxConstr (Memo tx _)) txBody -> TxConstr $ memoBytes $ encodeTxRaw $ tx {trBody = txBody})
{-# INLINEABLE bodyShelleyTxL #-}

-- | `TxWits` setter and getter for `ShelleyTx`. The setter does update
-- memoized binary representation.
witsShelleyTxL :: EraTx era => Lens' (ShelleyTx era) (TxWits era)
witsShelleyTxL =
  lens
    (\(TxConstr (Memo tx _)) -> trWits tx)
    (\(TxConstr (Memo tx _)) txWits -> TxConstr $ memoBytes $ encodeTxRaw $ tx {trWits = txWits})
{-# INLINEABLE witsShelleyTxL #-}

-- | `TxAuxData` setter and getter for `ShelleyTx`. The setter does update
-- memoized binary representation.
auxDataShelleyTxL :: EraTx era => Lens' (ShelleyTx era) (StrictMaybe (TxAuxData era))
auxDataShelleyTxL =
  lens
    (\(TxConstr (Memo tx _)) -> trAuxiliaryData tx)
    (\(TxConstr (Memo tx _)) auxData -> mkShelleyTx $ tx {trAuxiliaryData = auxData})
{-# INLINEABLE auxDataShelleyTxL #-}

-- | Size getter for `ShelleyTx`.
sizeShelleyTxF :: Era era => SimpleGetter (Tx era) Integer
sizeShelleyTxF = to (\(TxConstr (Memo _ bytes)) -> fromIntegral $ SBS.length bytes)
{-# INLINEABLE sizeShelleyTxF #-}

mkShelleyTx :: EraTx era => TxRaw era -> ShelleyTx era
mkShelleyTx = TxConstr . memoBytes . encodeTxRaw
{-# INLINEABLE mkShelleyTx #-}

mkBasicShelleyTx :: EraTx era => Core.TxBody era -> ShelleyTx era
mkBasicShelleyTx txBody =
  mkShelleyTx $
    TxRaw
      { trBody = txBody,
        trWits = mkBasicTxWits,
        trAuxiliaryData = SNothing
      }

instance CC.Crypto c => EraTx (ShelleyEra c) where
  {-# SPECIALIZE instance EraTx (ShelleyEra CC.StandardCrypto) #-}

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
  ( NFData (Core.TxBody era),
    NFData (TxWits era),
    NFData (TxAuxData era)
  ) =>
  NFData (ShelleyTx era)

deriving newtype instance Eq (ShelleyTx era)

deriving newtype instance
  (Era era, Show (Core.TxBody era), Show (TxWits era), Show (TxAuxData era)) =>
  Show (ShelleyTx era)

deriving newtype instance
  ( Era era,
    NoThunks (TxAuxData era),
    NoThunks (Core.TxBody era),
    NoThunks (TxWits era)
  ) =>
  NoThunks (ShelleyTx era)

pattern ShelleyTx ::
  EraTx era =>
  Core.TxBody era ->
  TxWits era ->
  StrictMaybe (TxAuxData era) ->
  ShelleyTx era
pattern ShelleyTx {body, wits, auxiliaryData} <-
  TxConstr
    ( Memo
        TxRaw
          { trBody = body,
            trWits = wits,
            trAuxiliaryData = auxiliaryData
          }
        _
      )
  where
    ShelleyTx b w a = mkShelleyTx $ TxRaw b w a

{-# COMPLETE ShelleyTx #-}

--------------------------------------------------------------------------------
-- Serialisation
--------------------------------------------------------------------------------

encodeTxRaw :: EraTx era => TxRaw era -> Encode ('Closed 'Dense) (TxRaw era)
encodeTxRaw TxRaw {trBody, trWits, trAuxiliaryData} =
  Rec TxRaw
    !> To trBody
    !> To trWits
    !> E (encodeNullMaybe toCBOR . strictMaybeToMaybe) trAuxiliaryData

instance
  ( Era era,
    FromCBOR (Annotator (Core.TxBody era)),
    FromCBOR (Annotator (TxWits era)),
    FromCBOR (Annotator (TxAuxData era))
  ) =>
  FromCBOR (Annotator (TxRaw era))
  where
  fromCBOR =
    decode $
      Ann (RecD TxRaw)
        <*! From
        <*! From
        <*! D
          ( sequence . maybeToStrictMaybe
              <$> decodeNullMaybe fromCBOR
          )

deriving via Mem TxRaw era instance EraTx era => FromCBOR (Annotator (Tx era))

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
  Core.TxBody era ->
  TxWits era ->
  StrictMaybe (TxAuxData era) ->
  LBS.ByteString ->
  Tx era
unsafeConstructTxWithBytes b w a bytes = TxConstr (mkMemoBytes (TxRaw b w a) bytes)

--------------------------------------------------------------------------------
-- Segregated witness
--------------------------------------------------------------------------------

segwitTx ::
  forall era.
  EraTx era =>
  Annotator (Core.TxBody era) ->
  Annotator (TxWits era) ->
  Maybe (Annotator (TxAuxData era)) ->
  Annotator (Tx era)
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
          Just b -> serialize version b
        fullBytes =
          serializeEncoding version (encodeListLen 3)
            <> serialize version body'
            <> serialize version witnessSet
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
  ( EraScript era,
    Script era ~ MultiSig (EraCrypto era)
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
  Core.Tx era ->
  Bool
validateNativeMultiSigScript msig tx =
  evalNativeMultiSigScript msig (coerceKeyRole `Set.map` vhks)
  where
    vhks = Set.map witVKeyHash (tx ^. witsTxL . addrTxWitsL)

-- | Multi-signature script witness accessor function for Transactions
txwitsScript ::
  EraTx era =>
  Core.Tx era ->
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
shelleyMinFeeTx ::
  ( EraTx era,
    HasField "_minfeeA" (Core.PParams era) Natural,
    HasField "_minfeeB" (Core.PParams era) Natural
  ) =>
  Core.PParams era ->
  Core.Tx era ->
  Coin
shelleyMinFeeTx pp tx =
  Coin $
    fromIntegral (getField @"_minfeeA" pp)
      * tx ^. sizeTxF
      + fromIntegral (getField @"_minfeeB" pp)

minfee ::
  ( EraTx era,
    HasField "_minfeeA" (Core.PParams era) Natural,
    HasField "_minfeeB" (Core.PParams era) Natural
  ) =>
  Core.PParams era ->
  Core.Tx era ->
  Coin
minfee = shelleyMinFeeTx
{-# DEPRECATED minfee "In favor of `getMinFeeTx`" #-}

-- | Extract the witness hashes from the Transaction.
witsFromTxWitnesses ::
  EraTx era =>
  Core.Tx era ->
  Set (KeyHash 'Witness (EraCrypto era))
witsFromTxWitnesses tx =
  Set.map witVKeyHash (tx ^. witsTxL . addrTxWitsL)
    `Set.union` Set.map bootstrapWitKeyHash (tx ^. witsTxL . bootAddrTxWitsL)
