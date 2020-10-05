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
    extractKeyHashWitnessSet,
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
import Cardano.Ledger.Era
import qualified Data.ByteString.Lazy as BSL
import Data.Foldable (fold)
import Data.Functor.Identity (Identity)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe (catMaybes)
import Data.Set (Set)
import qualified Data.Set as Set
import GHC.Generics (Generic)
import NoThunks.Class (AllowThunksIn (..), NoThunks (..))
import Shelley.Spec.Ledger.Address.Bootstrap (BootstrapWitness)
import Shelley.Spec.Ledger.BaseTypes
  ( StrictMaybe,
    invalidKey,
    maybeToStrictMaybe,
    strictMaybeToMaybe,
  )
import Shelley.Spec.Ledger.Credential (Credential (..))
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
import Shelley.Spec.Ledger.TxBody
  ( TxBody (..),
    TxId (..),
    TxIn (..),
    TxOut (..),
    WitVKey (..),
    witKeyHash,
  )

-- | Higher Kinded Data
type family HKD f a where
  HKD Identity a = a
  HKD f a = f a

data WitnessSetHKD f era = WitnessSet'
  { addrWits' :: !(HKD f (Set (WitVKey era 'Witness))),
    msigWits' :: !(HKD f (Map (ScriptHash era) (MultiSig era))),
    bootWits' :: !(HKD f (Set (BootstrapWitness era))),
    txWitsBytes :: BSL.ByteString
  }

deriving instance Era era => Show (WitnessSetHKD Identity era)

deriving instance Era era => Eq (WitnessSetHKD Identity era)

deriving instance Era era => Generic (WitnessSetHKD Identity era)

deriving via
  AllowThunksIn
    '[ "txWitsBytes"
     ]
    (WitnessSetHKD Identity era)
  instance
    Era era => (NoThunks (WitnessSetHKD Identity era))

type WitnessSet = WitnessSetHKD Identity

instance Era era => ToCBOR (WitnessSetHKD Identity era) where
  toCBOR = encodePreEncoded . BSL.toStrict . txWitsBytes

instance Era era => Semigroup (WitnessSetHKD Identity era) where
  (WitnessSet a b c) <> (WitnessSet a' b' c') =
    WitnessSet (a <> a') (b <> b') (c <> c')

instance Era era => Monoid (WitnessSetHKD Identity era) where
  mempty = WitnessSet mempty mempty mempty

pattern WitnessSet ::
  Era era =>
  Set (WitVKey era 'Witness) ->
  Map (ScriptHash era) (MultiSig era) ->
  Set (BootstrapWitness era) ->
  WitnessSet era
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
data Tx era = Tx'
  { _body' :: !(TxBody era),
    _witnessSet' :: !(WitnessSet era),
    _metadata' :: !(StrictMaybe MetaData),
    txFullBytes :: BSL.ByteString
  }
  deriving (Show, Eq, Generic)
  deriving
    (NoThunks)
    via AllowThunksIn
          '[ "txFullBytes"
           ]
          (Tx era)

pattern Tx ::
  Era era =>
  TxBody era ->
  WitnessSet era ->
  StrictMaybe MetaData ->
  Tx era
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

instance Era era => HashAnnotated (Tx era) era

segwitTx ::
  Era era =>
  Annotator (TxBody era) ->
  Annotator (WitnessSet era) ->
  Maybe (Annotator MetaData) ->
  Annotator (Tx era)
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

decodeWits :: forall era s. Era era => Decoder s (Annotator (WitnessSet era))
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
      emptyWitnessSetHKD :: WitnessSetHKD Annotator era
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
  (Era era) =>
  ToCBOR (Tx era)
  where
  toCBOR tx = encodePreEncoded . BSL.toStrict $ txFullBytes tx

instance Era era => FromCBOR (Annotator (Tx era)) where
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
  (Era era, ToCBOR a) =>
  MultiSignatureScript a era
  where
  validateScript :: a -> Tx era -> Bool
  hashScript :: a -> ScriptHash era

-- | instance of MultiSignatureScript type class
instance
  Era era =>
  MultiSignatureScript (MultiSig era) era
  where
  validateScript = validateNativeMultiSigScript
  hashScript = hashMultiSigScript

-- | Script evaluator for native multi-signature scheme. 'vhks' is the set of
-- key hashes that signed the transaction to be validated.
evalNativeMultiSigScript ::
  Era era =>
  MultiSig era ->
  Set (KeyHash 'Witness era) ->
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
  (Era era) =>
  MultiSig era ->
  Tx era ->
  Bool
validateNativeMultiSigScript msig tx =
  evalNativeMultiSigScript msig (coerceKeyRole `Set.map` vhks)
  where
    witsSet = _witnessSet tx
    vhks = Set.map witKeyHash (addrWits' witsSet)

-- | Multi-signature script witness accessor function for Transactions
txwitsScript ::
  Era era =>
  Tx era ->
  Map (ScriptHash era) (MultiSig era)
txwitsScript = msigWits . _witnessSet

extractKeyHashWitnessSet ::
  forall (r :: KeyRole) era.
  [Credential r era] ->
  Set (KeyHash 'Witness era)
extractKeyHashWitnessSet credentials = foldr accum Set.empty credentials
  where
    accum (KeyHashObj hk) ans = Set.insert (asWitness hk) ans
    accum _other ans = ans
