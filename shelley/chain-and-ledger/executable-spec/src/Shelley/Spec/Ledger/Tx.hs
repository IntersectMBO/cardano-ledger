{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}

module Shelley.Spec.Ledger.Tx
  ( -- transaction
    Tx
      ( Tx,
        _body,
        _witnessSet,
        _metadata,
        txFullBytes
      ),
    TxBody (..),
    TxOut (..),
    TxIn (..),
    TxId (..),
    decodeWits,
    segwitTx,
    -- witness data
    WitnessSet,
    WitnessSetHKD
      ( WitnessSet,
        addrWits,
        bootWits,
        msigWits,
        txWitsBytes
      ),
    WitVKey (..),
    MultiSignatureScript,
    validateScript,
    hashScript,
    txwitsScript,
    extractKeyHash,
    extractKeyHashWitnessSet,
    extractScriptHash,
    getKeyCombinations,
    getKeyCombination,
  )
where

import Cardano.Binary
  ( Annotator (..),
    Decoder,
    FromCBOR (fromCBOR),
    ToCBOR (toCBOR),
    annotatorSlice,
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
import Cardano.Prelude
  ( AllowThunksIn (..),
    LByteString,
    NoUnexpectedThunks (..),
    UseIsNormalFormNamed (..),
    catMaybes,
  )
import qualified Data.ByteString.Lazy as BSL
import Data.Foldable (fold)
import Data.Functor.Identity (Identity)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe (mapMaybe)
import Data.Set (Set)
import qualified Data.Set as Set
import GHC.Generics (Generic)
import Shelley.Spec.Ledger.Address.Bootstrap (BootstrapWitness)
import Shelley.Spec.Ledger.BaseTypes
  ( StrictMaybe,
    invalidKey,
    maybeToStrictMaybe,
    strictMaybeToMaybe,
  )
import Shelley.Spec.Ledger.Credential (Credential (..))
import Shelley.Spec.Ledger.Crypto
import Shelley.Spec.Ledger.Hashing (HashAnnotated (..))
import Shelley.Spec.Ledger.Keys
import Shelley.Spec.Ledger.MetaData (MetaData)
import Shelley.Spec.Ledger.Scripts
import Shelley.Spec.Ledger.Serialization
  ( decodeList,
    decodeMapContents,
    decodeNullMaybe,
    decodeRecordNamed,
    encodeFoldable,
    encodeNullMaybe,
  )
import Shelley.Spec.Ledger.TxData
  ( TxBody (..),
    TxId (..),
    TxIn (..),
    TxOut (..),
    WitVKey (..),
    witKeyHash,
  )
import Shelley.Spec.Ledger.Value

-- | Higher Kinded Data
type family HKD f a where
  HKD Identity a = a
  HKD f a = f a

data WitnessSetHKD f crypto v = WitnessSet'
  { addrWits' :: !(HKD f (Set (WitVKey crypto v 'Witness))),
    msigWits' :: !(HKD f (Map (ScriptHash crypto) (MultiSig crypto))),
    bootWits' :: !(HKD f (Set (BootstrapWitness crypto v))),
    txWitsBytes :: LByteString
  }

deriving instance Crypto crypto => Show (WitnessSetHKD Identity crypto v)

deriving instance Crypto crypto => Eq (WitnessSetHKD Identity crypto v)

deriving via UseIsNormalFormNamed "WitnessSetHKD" (WitnessSetHKD Identity crypto v) instance Crypto crypto => NoUnexpectedThunks (WitnessSetHKD Identity crypto v)

type WitnessSet = WitnessSetHKD Identity

instance CV crypto v => ToCBOR (WitnessSetHKD Identity crypto v) where
  toCBOR = encodePreEncoded . BSL.toStrict . txWitsBytes

instance CV crypto v => Semigroup (WitnessSetHKD Identity crypto v) where
  (WitnessSet a b c) <> (WitnessSet a' b' c') =
    WitnessSet (a <> a') (b <> b') (c <> c')

instance CV crypto v => Monoid (WitnessSetHKD Identity crypto v) where
  mempty = WitnessSet mempty mempty mempty

pattern WitnessSet ::
  CV crypto v =>
  Set (WitVKey crypto v 'Witness) ->
  Map (ScriptHash crypto) (MultiSig crypto) ->
  Set (BootstrapWitness crypto v) ->
  WitnessSet crypto v
pattern WitnessSet {addrWits, msigWits, bootWits} <-
  WitnessSet' addrWits msigWits bootWits _
  where
    WitnessSet awits witnessMSigMap bootstrapWits =
      let encodeMapElement ix enc x =
            if null x then Nothing else Just (encodeWord ix <> enc x)
          l =
            catMaybes $
              [ encodeMapElement 0 encodeFoldable awits,
                encodeMapElement 1 encodeFoldable witnessMSigMap,
                encodeMapElement 2 encodeFoldable bootstrapWits
              ]
          n = fromIntegral $ length l
          witsBytes = serializeEncoding $ encodeMapLen n <> fold l
       in WitnessSet'
            { addrWits' = awits,
              msigWits' = witnessMSigMap,
              bootWits' = bootstrapWits,
              txWitsBytes = witsBytes
            }

{-# COMPLETE WitnessSet #-}

-- | A fully formed transaction.
data Tx crypto v = Tx'
  { _body' :: !(TxBody crypto v),
    _witnessSet' :: !(WitnessSet crypto v),
    _metadata' :: !(StrictMaybe MetaData),
    txFullBytes :: LByteString
  }
  deriving (Show, Generic)
  deriving
    (NoUnexpectedThunks)
    via AllowThunksIn
          '[ "txFullBytes"
           ]
          (Tx crypto v)

deriving instance (Crypto crypto, Eq v) => Eq (Tx crypto v)

pattern Tx ::
  CV crypto v =>
  TxBody crypto v ->
  WitnessSet crypto v ->
  StrictMaybe MetaData ->
  Tx crypto v
pattern Tx {_body, _witnessSet, _metadata} <-
  Tx' _body _witnessSet _metadata _
  where
    Tx body witnessSet metadata =
      let bodyBytes = serialize body
          wrappedMetadataBytes =
            serializeEncoding $
              encodeNullMaybe toCBOR (strictMaybeToMaybe metadata)
          fullBytes =
            (serializeEncoding $ encodeListLen 3)
              <> bodyBytes
              <> serialize witnessSet
              <> wrappedMetadataBytes
       in Tx'
            { _body' = body,
              _witnessSet' = witnessSet,
              _metadata' = metadata,
              txFullBytes = fullBytes
            }

{-# COMPLETE Tx #-}

instance CV c v => HashAnnotated (Tx c v) c

segwitTx ::
  CV crypto v =>
  Annotator (TxBody crypto v) ->
  Annotator (WitnessSet crypto v) ->
  Maybe (Annotator MetaData) ->
  Annotator (Tx crypto v)
segwitTx
  bodyAnn
  witsAnn
  metaAnn = Annotator $ \bytes ->
    let body = runAnnotator bodyAnn bytes
        witnessSet = runAnnotator witsAnn bytes
        metadata = flip runAnnotator bytes <$> metaAnn
        wrappedMetadataBytes = case metadata of
          Nothing -> serializeEncoding encodeNull
          Just b -> serialize b
        fullBytes =
          (serializeEncoding $ encodeListLen 3)
            <> serialize body
            <> serialize witnessSet
            <> wrappedMetadataBytes
     in Tx'
          { _body' = body,
            _witnessSet' = witnessSet,
            _metadata' = maybeToStrictMaybe metadata,
            txFullBytes = fullBytes
          }

decodeWits :: forall crypto s v. (CV crypto v) => Decoder s (Annotator (WitnessSet crypto v))
decodeWits = do
  (mapParts, annBytes) <-
    withSlice $
      decodeMapContents $
        decodeWord >>= \case
          0 ->
            decodeList fromCBOR >>= \x ->
              pure (\ws -> ws {addrWits' = Set.fromList <$> sequence x})
          1 ->
            decodeList fromCBOR >>= \x ->
              pure (\ws -> ws {msigWits' = keyBy hashScript <$> sequence x})
          2 ->
            decodeList fromCBOR >>= \x ->
              pure (\ws -> ws {bootWits' = Set.fromList <$> sequence x})
          k -> invalidKey k
  let witSet = foldr ($) emptyWitnessSetHKD mapParts
      emptyWitnessSetHKD :: WitnessSetHKD Annotator crypto v
      emptyWitnessSetHKD =
        WitnessSet'
          { addrWits' = pure mempty,
            msigWits' = pure mempty,
            bootWits' = pure mempty,
            txWitsBytes = mempty
          }
  pure $
    WitnessSet'
      <$> addrWits' witSet
      <*> msigWits' witSet
      <*> bootWits' witSet
      <*> annBytes

keyBy :: Ord k => (a -> k) -> [a] -> Map k a
keyBy f xs = Map.fromList $ (\x -> (f x, x)) <$> xs

instance
  (CV crypto v) =>
  ToCBOR (Tx crypto v)
  where
  toCBOR tx = encodePreEncoded . BSL.toStrict $ txFullBytes tx

instance CV crypto v => FromCBOR (Annotator (Tx crypto v)) where
  fromCBOR = annotatorSlice $
    decodeRecordNamed "Tx" (const 3) $ do
      body <- fromCBOR
      wits <- decodeWits
      meta <- (decodeNullMaybe fromCBOR :: Decoder s (Maybe (Annotator MetaData)))
      pure $
        Annotator $ \fullBytes bytes ->
          Tx'
            { _body' = runAnnotator body fullBytes,
              _witnessSet' = runAnnotator wits fullBytes,
              _metadata' = (maybeToStrictMaybe $ flip runAnnotator fullBytes <$> meta),
              txFullBytes = bytes
            }

-- | Typeclass for multis-signature script data types. Allows for script
-- validation and hashing.
class
  (ToCBOR a, Crypto crypto) =>
  MultiSignatureScript a crypto
  where
  validateScript :: forall v. (CV crypto v) => a -> Tx crypto v -> Bool
  hashScript :: a -> ScriptHash crypto

-- | instance of MultiSignatureScript type class
instance
  (Crypto crypto) =>
  MultiSignatureScript (MultiSig crypto) crypto
  where
  validateScript = validateNativeMultiSigScript
  hashScript = hashMultiSigScript

-- | Script evaluator for native multi-signature scheme. 'vhks' is the set of
-- key hashes that signed the transaction to be validated.
evalNativeMultiSigScript ::
  Crypto crypto =>
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
  CV crypto v =>
  MultiSig crypto ->
  Tx crypto v ->
  Bool
validateNativeMultiSigScript msig tx =
  evalNativeMultiSigScript msig (coerceKeyRole `Set.map` vhks)
  where
    witsSet = _witnessSet tx
    vhks = Set.map witKeyHash (addrWits' witsSet)

-- | Multi-signature script witness accessor function for Transactions
txwitsScript ::
  CV crypto v =>
  Tx crypto v ->
  Map (ScriptHash crypto) (MultiSig crypto)
txwitsScript = msigWits . _witnessSet

extractKeyHash ::
  [Credential kr crypto] ->
  [KeyHash kr crypto]
extractKeyHash =
  mapMaybe
    ( \case
        KeyHashObj hk -> Just hk
        _ -> Nothing
    )

extractKeyHashWitnessSet ::
  forall (r :: KeyRole) crypto.
  [Credential r crypto] ->
  Set (KeyHash 'Witness crypto)
extractKeyHashWitnessSet credentials = foldr accum Set.empty credentials
  where
    accum (KeyHashObj hk) ans = Set.insert (asWitness hk) ans
    accum _other ans = ans

extractScriptHash ::
  [Credential 'Payment crypto] ->
  [ScriptHash crypto]
extractScriptHash =
  mapMaybe
    ( \case
        ScriptHashObj hk -> Just hk
        _ -> Nothing
    )
