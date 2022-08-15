{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
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
    decodeWits,
    segwitTx,

    -- * TxWits
    ShelleyWitnesses,
    WitnessSet,
    WitnessSetHKD
      ( ShelleyWitnesses,
        WitnessSet,
        addrWits,
        bootWits,
        scriptWits,
        txWitsBytes
      ),
    mkBasicShelleyTx,
    scriptShelleyWitsL,
    addrShelleyWitsL,
    bootAddrShelleyWitsL,
    txwitsScript,
    extractKeyHashWitnessSet,
    addrWits',
    evalNativeMultiSigScript,
    hashMultiSigScript,
    nativeMultiSigTag,
    validateNativeMultiSigScript,
    prettyWitnessSetParts,
    minfee,
    witsFromTxWitnesses,

    -- * Re-exports
    TxBody,
    ShelleyTxBody (..),
    TxOut,
    ShelleyTxOut (..),
    TxIn (..),
    TxId (..),
    WitVKey (..),
  )
where

import Cardano.Binary
  ( FromCBOR (fromCBOR),
    ToCBOR (toCBOR),
    decodeWord,
    encodeListLen,
    encodeMapLen,
    encodeNull,
    encodePreEncoded,
    encodeWord,
    serialize,
    serializeEncoding,
    withSlice,
  )
import Cardano.Ledger.Coin (Coin (Coin))
import Cardano.Ledger.Core hiding (Tx, TxBody, TxOut)
import qualified Cardano.Ledger.Core as Core
import Cardano.Ledger.Credential (Credential (..))
import qualified Cardano.Ledger.Crypto as CC (Crypto)
import Cardano.Ledger.HKD (HKD)
import Cardano.Ledger.Keys (HasKeyRole (coerceKeyRole), KeyHash, KeyRole (Witness), asWitness)
import Cardano.Ledger.Keys.Bootstrap (BootstrapWitness, bootstrapWitKeyHash)
import Cardano.Ledger.Keys.WitVKey (WitVKey (..), witVKeyHash)
import Cardano.Ledger.SafeHash (SafeToHash (..))
import Cardano.Ledger.Shelley.Era (ShelleyEra)
import Cardano.Ledger.Shelley.Metadata ()
import Cardano.Ledger.Shelley.Scripts (MultiSig (..), nativeMultiSigTag)
import Cardano.Ledger.Shelley.TxBody (ShelleyTxBody (..), ShelleyTxOut (..), TxBody, TxOut)
import Cardano.Ledger.TxIn (TxId (..), TxIn (..))
import Control.DeepSeq (NFData)
import qualified Data.ByteString.Lazy as BSL
import qualified Data.ByteString.Short as SBS
import Data.Coders hiding (to)
import Data.Foldable (fold)
import Data.Functor.Identity (Identity)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe (catMaybes)
import Data.Maybe.Strict
  ( StrictMaybe (..),
    maybeToStrictMaybe,
    strictMaybeToMaybe,
  )
import Data.MemoBytes (Mem, MemoBytes (Memo), memoBytes)
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Typeable (Typeable)
import GHC.Generics (Generic)
import GHC.Records
import Lens.Micro (Lens', SimpleGetter, lens, to, (^.))
import NoThunks.Class (AllowThunksIn (..), NoThunks (..))
import Numeric.Natural (Natural)

-- ========================================================

data TxRaw era = TxRaw
  { _body :: !(Core.TxBody era),
    _wits :: !(TxWits era),
    _auxiliaryData :: !(StrictMaybe (AuxiliaryData era))
  }
  deriving (Generic, Typeable)

instance
  ( NFData (Core.TxBody era),
    NFData (TxWits era),
    NFData (AuxiliaryData era)
  ) =>
  NFData (TxRaw era)

deriving instance EraTx era => Eq (TxRaw era)

deriving instance EraTx era => Show (TxRaw era)

instance
  ( Era era,
    NoThunks (AuxiliaryData era),
    NoThunks (Core.TxBody era),
    NoThunks (TxWits era)
  ) =>
  NoThunks (TxRaw era)

newtype ShelleyTx era = TxConstr (MemoBytes (TxRaw era))
  deriving newtype (SafeToHash, ToCBOR)

type Tx era = ShelleyTx era

{-# DEPRECATED Tx "Use `ShelleyTx` instead" #-}

-- | `Core.TxBody` setter and getter for `ShelleyTx`. The setter does update
-- memoized binary representation.
bodyShelleyTxL :: EraTx era => Lens' (ShelleyTx era) (Core.TxBody era)
bodyShelleyTxL =
  lens
    (\(TxConstr (Memo tx _)) -> _body tx)
    (\(TxConstr (Memo tx _)) txBody -> TxConstr $ memoBytes $ encodeTxRaw $ tx {_body = txBody})

-- | `TxWits` setter and getter for `ShelleyTx`. The setter does update
-- memoized binary representation.
witsShelleyTxL :: EraTx era => Lens' (ShelleyTx era) (TxWits era)
witsShelleyTxL =
  lens
    (\(TxConstr (Memo tx _)) -> _wits tx)
    (\(TxConstr (Memo tx _)) txWits -> TxConstr $ memoBytes $ encodeTxRaw $ tx {_wits = txWits})

-- | `AuxiliaryData` setter and getter for `ShelleyTx`. The setter does update
-- memoized binary representation.
auxDataShelleyTxL :: EraTx era => Lens' (ShelleyTx era) (StrictMaybe (AuxiliaryData era))
auxDataShelleyTxL =
  lens
    (\(TxConstr (Memo tx _)) -> _auxiliaryData tx)
    (\(TxConstr (Memo tx _)) auxData -> mkShelleyTx $ tx {_auxiliaryData = auxData})

-- | Size getter for `ShelleyTx`.
sizeShelleyTxF :: SimpleGetter (Tx era) Integer
sizeShelleyTxF = to (\(TxConstr (Memo _ bytes)) -> fromIntegral $ SBS.length bytes)

mkShelleyTx :: EraTx era => TxRaw era -> ShelleyTx era
mkShelleyTx = TxConstr . memoBytes . encodeTxRaw

mkBasicShelleyTx :: EraTx era => Core.TxBody era -> ShelleyTx era
mkBasicShelleyTx txBody =
  mkShelleyTx $
    TxRaw
      { _body = txBody,
        _wits = mkBasicWits,
        _auxiliaryData = SNothing
      }

instance CC.Crypto crypto => EraTx (ShelleyEra crypto) where
  type Tx (ShelleyEra crypto) = ShelleyTx (ShelleyEra crypto)

  mkBasicTx = mkBasicShelleyTx

  bodyTxL = bodyShelleyTxL

  witsTxL = witsShelleyTxL

  auxDataTxL = auxDataShelleyTxL

  sizeTxF = sizeShelleyTxF

  validateScript (Phase1Script multisig) tx = validateNativeMultiSigScript multisig tx

deriving newtype instance
  ( NFData (Core.TxBody era),
    NFData (TxWits era),
    NFData (AuxiliaryData era)
  ) =>
  NFData (ShelleyTx era)

deriving newtype instance Eq (Tx era)

deriving newtype instance EraTx era => Show (ShelleyTx era)

deriving newtype instance
  ( Era era,
    NoThunks (AuxiliaryData era),
    NoThunks (Core.TxBody era),
    NoThunks (TxWits era)
  ) =>
  NoThunks (ShelleyTx era)

pattern ShelleyTx ::
  EraTx era =>
  Core.TxBody era ->
  TxWits era ->
  StrictMaybe (AuxiliaryData era) ->
  ShelleyTx era
pattern ShelleyTx {body, wits, auxiliaryData} <-
  TxConstr
    ( Memo
        TxRaw
          { _body = body,
            _wits = wits,
            _auxiliaryData = auxiliaryData
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
encodeTxRaw TxRaw {_body, _wits, _auxiliaryData} =
  Rec TxRaw
    !> To _body
    !> To _wits
    !> E (encodeNullMaybe toCBOR . strictMaybeToMaybe) _auxiliaryData

instance EraTx era => FromCBOR (Annotator (TxRaw era)) where
  fromCBOR =
    decode $
      Ann (RecD TxRaw)
        <*! From
        <*! From
        <*! D
          ( sequence . maybeToStrictMaybe
              <$> decodeNullMaybe fromCBOR
          )

deriving via Mem (TxRaw era) instance EraTx era => FromCBOR (Annotator (Tx era))

-- | Construct a Tx containing the explicit serialised bytes.
--
--   This function is marked as unsafe since it makes no guarantee that the
--   represented bytes are indeed the correct serialisation of the transaction.
--   Thus, when calling this function, the caller is responsible for making this
--   guarantee.
--
--   The only intended use case for this is for segregated witness.
unsafeConstructTxWithBytes ::
  Core.TxBody era ->
  TxWits era ->
  StrictMaybe (AuxiliaryData era) ->
  SBS.ShortByteString ->
  Tx era
unsafeConstructTxWithBytes b w a bytes = TxConstr (Memo (TxRaw b w a) bytes)

--------------------------------------------------------------------------------
-- Witnessing
--------------------------------------------------------------------------------

data WitnessSetHKD f era = WitnessSet'
  { addrWits' :: !(HKD f (Set (WitVKey 'Witness (Crypto era)))),
    scriptWits' :: !(HKD f (Map (ScriptHash (Crypto era)) (Script era))),
    bootWits' :: !(HKD f (Set (BootstrapWitness (Crypto era)))),
    txWitsBytes :: BSL.ByteString
  }

deriving instance EraScript era => Show (WitnessSetHKD Identity era)

deriving instance EraScript era => Eq (WitnessSetHKD Identity era)

deriving instance Era era => Generic (WitnessSetHKD Identity era)

instance
  ( Era era,
    NFData (Script era),
    NFData (WitVKey 'Witness (Crypto era)),
    NFData (BootstrapWitness (Crypto era))
  ) =>
  NFData (WitnessSetHKD Identity era)

deriving via
  AllowThunksIn
    '[ "txWitsBytes"
     ]
    (WitnessSetHKD Identity era)
  instance
    (Era era, NoThunks (Script era)) =>
    (NoThunks (WitnessSetHKD Identity era))

type WitnessSet = WitnessSetHKD Identity

type ShelleyWitnesses = WitnessSetHKD Identity

-- | Script witness setter and getter for `ShelleyWitnesses`. The
-- setter does update memoized binary representation.
scriptShelleyWitsL ::
  EraTxWits era => Lens' (ShelleyWitnesses era) (Map (ScriptHash (Crypto era)) (Script era))
scriptShelleyWitsL = lens scriptWits' (\w s -> w {scriptWits = s})

-- | Addresses witness setter and getter for `ShelleyWitnesses`. The
-- setter does update memoized binary representation.
addrShelleyWitsL ::
  EraTxWits era => Lens' (ShelleyWitnesses era) (Set (WitVKey 'Witness (Crypto era)))
addrShelleyWitsL = lens addrWits' (\w s -> w {addrWits = s})

-- | Bootstrap Addresses witness setter and getter for `ShelleyWitnesses`. The
-- setter does update memoized binary representation.
bootAddrShelleyWitsL ::
  EraTxWits era => Lens' (ShelleyWitnesses era) (Set (BootstrapWitness (Crypto era)))
bootAddrShelleyWitsL = lens bootWits' (\w s -> w {bootWits = s})

instance CC.Crypto crypto => EraTxWits (ShelleyEra crypto) where
  type TxWits (ShelleyEra crypto) = ShelleyWitnesses (ShelleyEra crypto)

  mkBasicWits = mempty

  scriptWitsL = scriptShelleyWitsL

  addrWitsL = addrShelleyWitsL

  bootAddrWitsL = bootAddrShelleyWitsL

instance Era era => ToCBOR (WitnessSetHKD Identity era) where
  toCBOR = encodePreEncoded . BSL.toStrict . txWitsBytes

instance EraScript era => Semigroup (WitnessSetHKD Identity era) where
  (WitnessSet' a b c _) <> y | Set.null a && Map.null b && Set.null c = y
  y <> (WitnessSet' a b c _) | Set.null a && Map.null b && Set.null c = y
  (WitnessSet a b c) <> (WitnessSet a' b' c') =
    WitnessSet (a <> a') (b <> b') (c <> c')

instance EraScript era => Monoid (WitnessSetHKD Identity era) where
  mempty = WitnessSet mempty mempty mempty

pattern ShelleyWitnesses ::
  EraScript era =>
  Set (WitVKey 'Witness (Crypto era)) ->
  Map (ScriptHash (Crypto era)) (Script era) ->
  Set (BootstrapWitness (Crypto era)) ->
  WitnessSet era
pattern ShelleyWitnesses {addrWits, scriptWits, bootWits} <-
  WitnessSet' addrWits scriptWits bootWits _
  where
    ShelleyWitnesses awits scriptWitMap bootstrapWits =
      let encodeMapElement ix enc x =
            if null x then Nothing else Just (encodeWord ix <> enc x)
          l =
            catMaybes
              [ encodeMapElement 0 encodeFoldable awits,
                encodeMapElement 1 encodeFoldable scriptWitMap,
                encodeMapElement 2 encodeFoldable bootstrapWits
              ]
          n = fromIntegral $ length l
          witsBytes = serializeEncoding $ encodeMapLen n <> fold l
       in WitnessSet'
            { addrWits' = awits,
              scriptWits' = scriptWitMap,
              bootWits' = bootstrapWits,
              txWitsBytes = witsBytes
            }

{-# COMPLETE ShelleyWitnesses #-}

pattern WitnessSet ::
  EraScript era =>
  Set (WitVKey 'Witness (Crypto era)) ->
  Map (ScriptHash (Crypto era)) (Script era) ->
  Set (BootstrapWitness (Crypto era)) ->
  WitnessSet era
pattern WitnessSet a s b = ShelleyWitnesses a s b

{-# COMPLETE WitnessSet #-}

instance SafeToHash (WitnessSetHKD Identity era) where
  originalBytes = BSL.toStrict . txWitsBytes

-- | Exports the relevant parts from a (WintessSetHKD Identity era) for
--     use by the pretty printer without all the horrible constraints.
--     Uses the non-exported WitnessSet' constructor.
prettyWitnessSetParts ::
  WitnessSetHKD Identity era ->
  ( Set (WitVKey 'Witness (Crypto era)),
    Map (ScriptHash (Crypto era)) (Core.Script era),
    Set (BootstrapWitness (Crypto era))
  )
prettyWitnessSetParts (WitnessSet' a b c _) = (a, b, c)

--------------------------------------------------------------------------------
-- Segregated witness
--------------------------------------------------------------------------------

segwitTx ::
  EraTx era =>
  Annotator (Core.TxBody era) ->
  Annotator (TxWits era) ->
  Maybe (Annotator (AuxiliaryData era)) ->
  Annotator (Tx era)
segwitTx
  bodyAnn
  witsAnn
  metaAnn = Annotator $ \bytes ->
    let body' = runAnnotator bodyAnn bytes
        witnessSet = runAnnotator witsAnn bytes
        metadata = flip runAnnotator bytes <$> metaAnn
        wrappedMetadataBytes = case metadata of
          Nothing -> serializeEncoding encodeNull
          Just b -> serialize b
        fullBytes =
          serializeEncoding (encodeListLen 3)
            <> serialize body'
            <> serialize witnessSet
            <> wrappedMetadataBytes
     in unsafeConstructTxWithBytes
          body'
          witnessSet
          (maybeToStrictMaybe metadata)
          (SBS.toShort . BSL.toStrict $ fullBytes)

instance EraScript era => FromCBOR (Annotator (WitnessSetHKD Identity era)) where
  fromCBOR = decodeWits

-- | This type is only used to preserve the old buggy behavior where signature
-- was ignored in the `Ord` instance for `WitVKey`s.
newtype IgnoreSigOrd kr crypto = IgnoreSigOrd {unIgnoreSigOrd :: WitVKey kr crypto}
  deriving (Eq)

instance (Typeable kr, CC.Crypto crypto) => Ord (IgnoreSigOrd kr crypto) where
  compare (IgnoreSigOrd w1) (IgnoreSigOrd w2) = compare (witVKeyHash w1) (witVKeyHash w2)

decodeWits :: forall era s. EraScript era => Decoder s (Annotator (WitnessSet era))
decodeWits = do
  (mapParts, annBytes) <-
    withSlice $
      decodeMapContents $
        decodeWord >>= \case
          0 ->
            decodeList fromCBOR >>= \x ->
              pure
                ( \ws ->
                    ws
                      { addrWits' =
                          Set.map unIgnoreSigOrd . Set.fromList . fmap IgnoreSigOrd <$> sequence x
                      }
                )
          1 ->
            decodeList fromCBOR >>= \x ->
              pure (\ws -> ws {scriptWits' = keyBy (hashScript @era) <$> sequence x})
          2 ->
            decodeList fromCBOR >>= \x ->
              pure (\ws -> ws {bootWits' = Set.fromList <$> sequence x})
          k -> invalidKey k
  let witSet = foldr ($) emptyWitnessSetHKD mapParts
      emptyWitnessSetHKD :: WitnessSetHKD Annotator era
      emptyWitnessSetHKD =
        WitnessSet'
          { addrWits' = pure mempty,
            scriptWits' = pure mempty,
            bootWits' = pure mempty,
            txWitsBytes = mempty
          }
  pure $
    WitnessSet'
      <$> addrWits' witSet
      <*> scriptWits' witSet
      <*> bootWits' witSet
      <*> annBytes

keyBy :: Ord k => (a -> k) -> [a] -> Map k a
keyBy f xs = Map.fromList $ (\x -> (f x, x)) <$> xs

-- ===============================================================

-- | Hashes native multi-signature script.
hashMultiSigScript ::
  forall era.
  ( EraScript era,
    Script era ~ MultiSig (Crypto era)
  ) =>
  MultiSig (Crypto era) ->
  ScriptHash (Crypto era)
hashMultiSigScript = hashScript @era

-- ========================================

-- | Script evaluator for native multi-signature scheme. 'vhks' is the set of
-- key hashes that signed the transaction to be validated.
evalNativeMultiSigScript ::
  CC.Crypto crypto =>
  MultiSig crypto ->
  Set (KeyHash 'Witness crypto) ->
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
  MultiSig (Crypto era) ->
  Core.Tx era ->
  Bool
validateNativeMultiSigScript msig tx =
  evalNativeMultiSigScript msig (coerceKeyRole `Set.map` vhks)
  where
    vhks = Set.map witVKeyHash (tx ^. witsTxL . addrWitsL)

-- | Multi-signature script witness accessor function for Transactions
txwitsScript ::
  EraTx era =>
  Core.Tx era ->
  Map (ScriptHash (Crypto era)) (Script era)
txwitsScript tx = tx ^. witsTxL . scriptWitsL

extractKeyHashWitnessSet ::
  forall (r :: KeyRole) crypto.
  [Credential r crypto] ->
  Set (KeyHash 'Witness crypto)
extractKeyHashWitnessSet = foldr accum Set.empty
  where
    accum (KeyHashObj hk) ans = Set.insert (asWitness hk) ans
    accum _other ans = ans

-- | Minimum fee calculation
minfee ::
  ( EraTx era,
    HasField "_minfeeA" pp Natural,
    HasField "_minfeeB" pp Natural
  ) =>
  pp ->
  Core.Tx era ->
  Coin
minfee pp tx =
  Coin $
    fromIntegral (getField @"_minfeeA" pp)
      * tx ^. sizeTxF + fromIntegral (getField @"_minfeeB" pp)

-- | Extract the witness hashes from the Transaction.
witsFromTxWitnesses ::
  EraTx era =>
  Core.Tx era ->
  Set (KeyHash 'Witness (Crypto era))
witsFromTxWitnesses tx =
  Set.map witVKeyHash (tx ^. witsTxL . addrWitsL)
    `Set.union` Set.map bootstrapWitKeyHash (tx ^. witsTxL . bootAddrWitsL)
