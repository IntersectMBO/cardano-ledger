{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleContexts #-}
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
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Shelley.Spec.Ledger.Tx
  ( -- transaction
    Tx
      ( Tx,
        body,
        wits,
        auxiliaryData
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
        scriptWits,
        txWitsBytes
      ),
    WitVKey (..),
    ValidateScript (..), -- reexported from Cardano.Ledger.Era
    txwitsScript,
    extractKeyHashWitnessSet,
    addrWits',
    evalNativeMultiSigScript,
    hashMultiSigScript,
    validateNativeMultiSigScript,
    TransTx,
    TransWitnessSet,
    prettyWitnessSetParts,
  )
where

import Cardano.Binary
  ( Annotator (..),
    Decoder,
    FromCBOR (fromCBOR),
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
import Cardano.Ledger.BaseTypes
  ( invalidKey,
    maybeToStrictMaybe,
  )
import qualified Cardano.Ledger.Core as Core
import Cardano.Ledger.Credential (Credential (..))
import qualified Cardano.Ledger.Crypto as CC (Crypto)
import Cardano.Ledger.Era
import Cardano.Ledger.Keys
import Cardano.Ledger.SafeHash (SafeToHash (..))
import Cardano.Ledger.Serialization
  ( decodeList,
    decodeMapContents,
    encodeFoldable,
  )
import Cardano.Ledger.Tx
import qualified Data.ByteString.Lazy as BSL
import qualified Data.ByteString.Short as SBS
import Data.Constraint (Constraint)
import Data.Foldable (fold)
import Data.Functor.Identity (Identity)
import Data.Kind (Type)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe (catMaybes)
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Typeable
import GHC.Generics (Generic)
import GHC.Records (HasField (..))
import NoThunks.Class (AllowThunksIn (..), NoThunks (..))
import Shelley.Spec.Ledger.Address.Bootstrap (BootstrapWitness)
import Shelley.Spec.Ledger.Scripts
import Shelley.Spec.Ledger.TxBody
  ( TxBody (..),
    TxId (..),
    TxIn (..),
    TxOut (..),
    WitVKey (..),
    witKeyHash,
  )

-- ========================================================

-- | Higher Kinded Data
type family HKD f a where
  HKD Identity a = a
  HKD f a = f a

data WitnessSetHKD f era = WitnessSet'
  { addrWits' :: !(HKD f (Set (WitVKey 'Witness (Crypto era)))),
    scriptWits' :: !(HKD f (Map (ScriptHash (Crypto era)) (Core.Script era))),
    bootWits' :: !(HKD f (Set (BootstrapWitness (Crypto era)))),
    txWitsBytes :: BSL.ByteString
  }

type TransWitnessSet (c :: Type -> Constraint) era = c (Core.Script era)

deriving instance
  (Era era, TransWitnessSet Show era) =>
  Show (WitnessSetHKD Identity era)

deriving instance
  (Era era, TransWitnessSet Eq era) =>
  Eq (WitnessSetHKD Identity era)

deriving instance Era era => Generic (WitnessSetHKD Identity era)

deriving via
  AllowThunksIn
    '[ "txWitsBytes"
     ]
    (WitnessSetHKD Identity era)
  instance
    (Era era, TransWitnessSet NoThunks era) =>
    (NoThunks (WitnessSetHKD Identity era))

type WitnessSet = WitnessSetHKD Identity

instance Era era => ToCBOR (WitnessSetHKD Identity era) where
  toCBOR = encodePreEncoded . BSL.toStrict . txWitsBytes

instance
  (Era era, Core.AnnotatedData (Core.Script era)) =>
  Semigroup (WitnessSetHKD Identity era)
  where
  (WitnessSet' a b c _) <> y | Set.null a && Map.null b && Set.null c = y
  y <> (WitnessSet' a b c _) | Set.null a && Map.null b && Set.null c = y
  (WitnessSet a b c) <> (WitnessSet a' b' c') =
    WitnessSet (a <> a') (b <> b') (c <> c')

instance
  (Era era, Core.AnnotatedData (Core.Script era)) =>
  Monoid (WitnessSetHKD Identity era)
  where
  mempty = WitnessSet mempty mempty mempty

pattern WitnessSet ::
  (Era era, Core.AnnotatedData (Core.Script era)) =>
  Set (WitVKey 'Witness (Crypto era)) ->
  Map (ScriptHash (Crypto era)) (Core.Script era) ->
  Set (BootstrapWitness (Crypto era)) ->
  WitnessSet era
pattern WitnessSet {addrWits, scriptWits, bootWits} <-
  WitnessSet' addrWits scriptWits bootWits _
  where
    WitnessSet awits scriptWitMap bootstrapWits =
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

type TransTx (c :: Type -> Constraint) era =
  (Era era, c (Core.Script era), c (Core.TxBody era), c (Core.AuxiliaryData era))

instance
  (c ~ Crypto era, Core.Witnesses era ~ WitnessSet era) =>
  HasField "addrWits" (Tx era) (Set (WitVKey 'Witness c))
  where
  getField = addrWits' . getField @"wits"

instance
  (c ~ Crypto era, Core.Witnesses era ~ WitnessSet era) =>
  HasField "addrWits" (WitnessSet era) (Set (WitVKey 'Witness c))
  where
  getField = addrWits'

instance
  ( c ~ Crypto era,
    script ~ Core.Script era,
    Core.Witnesses era ~ WitnessSet era
  ) =>
  HasField "scriptWits" (Tx era) (Map (ScriptHash c) script)
  where
  getField = scriptWits' . getField @"wits"

instance
  ( c ~ Crypto era,
    script ~ Core.Script era,
    Core.Witnesses era ~ WitnessSet era
  ) =>
  HasField "scriptWits" (WitnessSet era) (Map (ScriptHash c) script)
  where
  getField = scriptWits'

instance
  (c ~ Crypto era, Core.Witnesses era ~ WitnessSet era) =>
  HasField "bootWits" (Tx era) (Set (BootstrapWitness c))
  where
  getField = bootWits' . getField @"wits"

-- =====================================

segwitTx ::
  ( ToCBOR (Core.TxBody era),
    ToCBOR (Core.Witnesses era),
    ToCBOR (Core.AuxiliaryData era)
  ) =>
  Annotator (Core.TxBody era) ->
  Annotator (Core.Witnesses era) ->
  Maybe (Annotator (Core.AuxiliaryData era)) ->
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
          (serializeEncoding $ encodeListLen 3)
            <> serialize body'
            <> serialize witnessSet
            <> wrappedMetadataBytes
     in unsafeConstructTxWithBytes
          body'
          witnessSet
          (maybeToStrictMaybe metadata)
          (SBS.toShort . BSL.toStrict $ fullBytes)

instance
  ( Typeable era,
    FromCBOR (Annotator (Core.Script era)),
    ValidateScript era
  ) =>
  FromCBOR (Annotator (WitnessSetHKD Identity era))
  where
  fromCBOR = decodeWits

decodeWits ::
  forall era s.
  ( FromCBOR (Annotator (Core.Script era)),
    ValidateScript era
  ) =>
  Decoder s (Annotator (WitnessSet era))
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
  ( ValidateScript era,
    Core.Script era ~ MultiSig (Crypto era)
  ) =>
  MultiSig (Crypto era) ->
  ScriptHash (Crypto era)
hashMultiSigScript x = hashScript @era x

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
  (TransTx ToCBOR era, Core.Witnesses era ~ WitnessSet era) =>
  MultiSig (Crypto era) ->
  Tx era ->
  Bool
validateNativeMultiSigScript msig tx =
  evalNativeMultiSigScript msig (coerceKeyRole `Set.map` vhks)
  where
    vhks = Set.map witKeyHash (getField @"addrWits" tx)

-- | Multi-signature script witness accessor function for Transactions
txwitsScript ::
  Core.Witnesses era ~ WitnessSet era =>
  Tx era ->
  Map (ScriptHash (Crypto era)) (Core.Script era)
txwitsScript = getField @"scriptWits"

extractKeyHashWitnessSet ::
  forall (r :: KeyRole) crypto.
  [Credential r crypto] ->
  Set (KeyHash 'Witness crypto)
extractKeyHashWitnessSet credentials = foldr accum Set.empty credentials
  where
    accum (KeyHashObj hk) ans = Set.insert (asWitness hk) ans
    accum _other ans = ans
