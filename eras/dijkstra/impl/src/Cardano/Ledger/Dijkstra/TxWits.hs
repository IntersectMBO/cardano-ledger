{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE UndecidableSuperClasses #-}
{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_GHC -Wno-orphans #-}

-- | Dijkstra transaction witnesses: the Alonzo witness set extended with
-- pool-vote witnesses (key 9), which authorize SPO governance votes with the
-- pool's registered Leios voting key (CIP-0164) instead of the pool cold key.
module Cardano.Ledger.Dijkstra.TxWits (
  PoolVoteWitness (..),
  poolVoteSignContext,
  DijkstraTxWits (
    MkDijkstraTxWits,
    DijkstraTxWits,
    dtwVKeyWits,
    dtwBootWits,
    dtwScriptWits,
    dtwDats,
    dtwRdmrs,
    dtwPoolVoteWits
  ),
  DijkstraTxWitsRaw (..),
  DijkstraEraTxWits (..),
  emptyDijkstraTxWitsRaw,
  addScriptsDijkstraTxWitsRaw,
  addrDijkstraTxWitsL,
  bootAddrDijkstraTxWitsL,
  scriptDijkstraTxWitsL,
  datsDijkstraTxWitsL,
  rdmrsDijkstraTxWitsL,
  poolVoteDijkstraTxWitsL,
) where

import Cardano.Base.Typeable (TypeName (TypeName))
import Cardano.Crypto.DSIGN (SigDSIGN)
import Cardano.Crypto.DSIGN.BLS12381.Internal (
  BLS12381MinSigDSIGN,
  BLS12381SignContext (..),
  minSigPoPDST,
 )
import Cardano.Ledger.Alonzo.Scripts (AlonzoEraScript (..), fromPlutusScript, toPlutusSLanguage)
import Cardano.Ledger.Alonzo.TxWits (
  AlonzoEraTxWits (..),
  Redeemers (..),
  TxDats (..),
  alonzoPlutusScriptDecoder,
  asHashedScriptPair,
  unRedeemers,
  unTxDats,
 )
import Cardano.Ledger.Binary (
  Annotator (..),
  DecCBOR (..),
  Decoder,
  EncCBOR (..),
  ToCBOR (..),
  decodeAccA,
  decodeFixedSized,
  decodeNonEmptySetLikeEnforceNoDuplicates,
  decodeNonEmptySetLikeEnforceNoDuplicatesAnn,
  decodeSparseKeyed,
  encodeFixedSized,
  encodeListLen,
  encodeTag,
  setTag,
 )
import Cardano.Ledger.Binary.Coders
import qualified Cardano.Ledger.Binary.Plain as Plain
import Cardano.Ledger.Core
import Cardano.Ledger.Dijkstra.Era (DijkstraEra)
import Cardano.Ledger.Dijkstra.Scripts ()
import Cardano.Ledger.Keys (BootstrapWitness, WitVKey)
import Cardano.Ledger.MemoBytes (
  EqRaw (..),
  Mem,
  MemoBytes,
  Memoized (..),
  eqRawType,
  getMemoRawType,
  lensMemoRawType,
  mkMemoizedEra,
 )
import Cardano.Ledger.Plutus.Language (
  Language (..),
  Plutus (..),
  PlutusLanguage,
  SLanguage (..),
  plutusBinary,
 )
import Cardano.Ledger.Shelley.TxWits (shelleyEqTxWitsRaw)
import Control.DeepSeq (NFData)
import Control.Monad ((>=>))
import Control.Monad.Trans.Fail (runFail)
import Data.Aeson (FromJSON (..), ToJSON (..), object, withObject, (.:), (.=))
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe (mapMaybe)
import Data.Set (Set)
import qualified Data.Set as Set
import GHC.Generics (Generic)
import Lens.Micro
import NoThunks.Class (NoThunks)

-- =====================================================
-- Pool-vote witnesses

-- | Witness authorizing all SPO votes of one pool in a transaction. A tagged
-- sum so future authorization schemes extend it with new constructors rather
-- than new witness-set fields; tag 0 is a BLS signature by the pool's
-- registered Leios voting key over the transaction body hash under
-- 'poolVoteSignContext'.
data PoolVoteWitness
  = BlsPoolVoteWitness !(SigDSIGN BLS12381MinSigDSIGN)
  deriving (Show, Generic, Eq, NoThunks, NFData)

-- | Standard min-sig PoP-scheme DST (which must stay untouched — it separates
-- proofs-of-possession from signatures) with a message augmentation that
-- domain-separates pool governance votes from Leios votes (CIP-0164).
poolVoteSignContext :: BLS12381SignContext
poolVoteSignContext = minSigPoPDST {blsSignContextAug = Just "cardano-pool-vote"}

instance EncCBOR PoolVoteWitness where
  encCBOR (BlsPoolVoteWitness sig) =
    encodeListLen 2 <> encCBOR (0 :: Word) <> encodeFixedSized sig

instance DecCBOR PoolVoteWitness where
  decCBOR = decodeRecordSum "PoolVoteWitness" $ \case
    0 -> (\sig -> (2, BlsPoolVoteWitness sig)) <$> decodeFixedSized
    k -> invalidKey k

instance ToJSON PoolVoteWitness where
  toJSON (BlsPoolVoteWitness sig) =
    object ["blsSignature" .= Plain.serializeAsHexText sig]

instance FromJSON PoolVoteWitness where
  parseJSON = withObject "PoolVoteWitness" $ \obj -> do
    sigHex <- obj .: "blsSignature"
    either (fail . show) (pure . BlsPoolVoteWitness) $ Plain.decodeFullFromHexText sigHex

-- =====================================================
-- DijkstraTxWits: the Alonzo witness set plus pool-vote witnesses

-- | Internal 'DijkstraTxWits' type, lacking serialised bytes.
data DijkstraTxWitsRaw era = DijkstraTxWitsRaw
  { dtwrAddrTxWits :: !(Set (WitVKey Witness))
  , dtwrBootAddrTxWits :: !(Set BootstrapWitness)
  , dtwrScriptTxWits :: !(Map ScriptHash (Script era))
  , dtwrDatsTxWits :: !(TxDats era)
  , dtwrRdmrsTxWits :: !(Redeemers era)
  , dtwrPoolVoteTxWits :: !(Map (KeyHash StakePool) PoolVoteWitness)
  }
  deriving (Generic)

instance
  ( Era era
  , NFData (Script era)
  , NFData (TxDats era)
  , NFData (Redeemers era)
  ) =>
  NFData (DijkstraTxWitsRaw era)

newtype DijkstraTxWits era = MkDijkstraTxWits (MemoBytes (DijkstraTxWitsRaw era))
  deriving newtype (SafeToHash, ToCBOR)
  deriving (Generic)

instance Memoized (DijkstraTxWits era) where
  type RawType (DijkstraTxWits era) = DijkstraTxWitsRaw era

instance AlonzoEraScript era => Semigroup (DijkstraTxWits era) where
  (<>) x y | isEmptyTxWitness x = y
  (<>) x y | isEmptyTxWitness y = x
  (<>)
    (getMemoRawType -> DijkstraTxWitsRaw a b c d (Redeemers e) f)
    (getMemoRawType -> DijkstraTxWitsRaw u v w x (Redeemers y) z) =
      DijkstraTxWits (a <> u) (b <> v) (c <> w) (d <> x) (Redeemers (e <> y)) (f <> z)

instance AlonzoEraScript era => Monoid (DijkstraTxWits era) where
  mempty = DijkstraTxWits mempty mempty mempty mempty (Redeemers mempty) mempty

deriving newtype instance
  ( Era era
  , NFData (Script era)
  , NFData (TxDats era)
  , NFData (Redeemers era)
  ) =>
  NFData (DijkstraTxWits era)

isEmptyTxWitness :: AlonzoEraScript era => DijkstraTxWits era -> Bool
isEmptyTxWitness (getMemoRawType -> DijkstraTxWitsRaw a b c d (Redeemers e) f) =
  Set.null a && Set.null b && Map.null c && Map.null (unTxDats d) && Map.null e && Map.null f

emptyDijkstraTxWitsRaw :: AlonzoEraScript era => DijkstraTxWitsRaw era
emptyDijkstraTxWitsRaw =
  DijkstraTxWitsRaw mempty mempty mempty mempty (Redeemers mempty) mempty

deriving stock instance AlonzoEraScript era => Eq (DijkstraTxWitsRaw era)

deriving stock instance AlonzoEraScript era => Show (DijkstraTxWitsRaw era)

instance AlonzoEraScript era => NoThunks (DijkstraTxWitsRaw era)

deriving newtype instance AlonzoEraScript era => Eq (DijkstraTxWits era)

deriving newtype instance AlonzoEraScript era => Show (DijkstraTxWits era)

deriving newtype instance AlonzoEraScript era => NoThunks (DijkstraTxWits era)

pattern DijkstraTxWits ::
  forall era.
  AlonzoEraScript era =>
  Set (WitVKey Witness) ->
  Set BootstrapWitness ->
  Map ScriptHash (Script era) ->
  TxDats era ->
  Redeemers era ->
  Map (KeyHash StakePool) PoolVoteWitness ->
  DijkstraTxWits era
pattern DijkstraTxWits {dtwVKeyWits, dtwBootWits, dtwScriptWits, dtwDats, dtwRdmrs, dtwPoolVoteWits} <-
  ( getMemoRawType ->
      DijkstraTxWitsRaw dtwVKeyWits dtwBootWits dtwScriptWits dtwDats dtwRdmrs dtwPoolVoteWits
    )
  where
    DijkstraTxWits vkeys' boots' scripts' dats' rdmrs' poolVotes' =
      mkMemoizedEra @era $ DijkstraTxWitsRaw vkeys' boots' scripts' dats' rdmrs' poolVotes'

{-# COMPLETE DijkstraTxWits #-}

-- =====================================================
-- Accessors

addrDijkstraTxWitsL ::
  forall era. AlonzoEraScript era => Lens' (DijkstraTxWits era) (Set (WitVKey Witness))
addrDijkstraTxWitsL =
  lensMemoRawType @era dtwrAddrTxWits $
    \witsRaw addrWits -> witsRaw {dtwrAddrTxWits = addrWits}
{-# INLINEABLE addrDijkstraTxWitsL #-}

bootAddrDijkstraTxWitsL ::
  forall era. AlonzoEraScript era => Lens' (DijkstraTxWits era) (Set BootstrapWitness)
bootAddrDijkstraTxWitsL =
  lensMemoRawType @era dtwrBootAddrTxWits $
    \witsRaw bootAddrWits -> witsRaw {dtwrBootAddrTxWits = bootAddrWits}
{-# INLINEABLE bootAddrDijkstraTxWitsL #-}

scriptDijkstraTxWitsL ::
  forall era. AlonzoEraScript era => Lens' (DijkstraTxWits era) (Map ScriptHash (Script era))
scriptDijkstraTxWitsL =
  lensMemoRawType @era dtwrScriptTxWits $
    \witsRaw scriptWits -> witsRaw {dtwrScriptTxWits = scriptWits}
{-# INLINEABLE scriptDijkstraTxWitsL #-}

datsDijkstraTxWitsL ::
  forall era. AlonzoEraScript era => Lens' (DijkstraTxWits era) (TxDats era)
datsDijkstraTxWitsL =
  lensMemoRawType @era dtwrDatsTxWits $
    \witsRaw datsWits -> witsRaw {dtwrDatsTxWits = datsWits}
{-# INLINEABLE datsDijkstraTxWitsL #-}

rdmrsDijkstraTxWitsL ::
  forall era. AlonzoEraScript era => Lens' (DijkstraTxWits era) (Redeemers era)
rdmrsDijkstraTxWitsL =
  lensMemoRawType @era dtwrRdmrsTxWits $
    \witsRaw rdmrsWits -> witsRaw {dtwrRdmrsTxWits = rdmrsWits}
{-# INLINEABLE rdmrsDijkstraTxWitsL #-}

poolVoteDijkstraTxWitsL ::
  forall era.
  AlonzoEraScript era =>
  Lens' (DijkstraTxWits era) (Map (KeyHash StakePool) PoolVoteWitness)
poolVoteDijkstraTxWitsL =
  lensMemoRawType @era dtwrPoolVoteTxWits $
    \witsRaw poolVoteWits -> witsRaw {dtwrPoolVoteTxWits = poolVoteWits}
{-# INLINEABLE poolVoteDijkstraTxWitsL #-}

instance EraTxWits DijkstraEra where
  type TxWits DijkstraEra = DijkstraTxWits DijkstraEra

  mkBasicTxWits = mempty

  addrTxWitsL = addrDijkstraTxWitsL
  {-# INLINE addrTxWitsL #-}

  bootAddrTxWitsL = bootAddrDijkstraTxWitsL
  {-# INLINE bootAddrTxWitsL #-}

  scriptTxWitsL = scriptDijkstraTxWitsL
  {-# INLINE scriptTxWitsL #-}

instance AlonzoEraTxWits DijkstraEra where
  datsTxWitsL = datsDijkstraTxWitsL
  {-# INLINE datsTxWitsL #-}

  rdmrsTxWitsL = rdmrsDijkstraTxWitsL
  {-# INLINE rdmrsTxWitsL #-}

class AlonzoEraTxWits era => DijkstraEraTxWits era where
  poolVoteTxWitsL :: Lens' (TxWits era) (Map (KeyHash StakePool) PoolVoteWitness)

instance DijkstraEraTxWits DijkstraEra where
  poolVoteTxWitsL = poolVoteDijkstraTxWitsL
  {-# INLINE poolVoteTxWitsL #-}

instance
  (TxWits era ~ DijkstraTxWits era, DijkstraEraTxWits era) =>
  EqRaw (DijkstraTxWits era)
  where
  eqRaw txWits1 txWits2 =
    shelleyEqTxWitsRaw txWits1 txWits2
      && eqRawType (txWits1 ^. datsTxWitsL) (txWits2 ^. datsTxWitsL)
      && eqRawType (txWits1 ^. rdmrsTxWitsL) (txWits2 ^. rdmrsTxWitsL)
      && txWits1 ^. poolVoteTxWitsL == txWits2 ^. poolVoteTxWitsL

--------------------------------------------------------------------------------
-- Serialisation
--------------------------------------------------------------------------------

-- | Encodes memoized bytes created upon construction.
instance Era era => EncCBOR (DijkstraTxWits era)

instance AlonzoEraScript era => EncCBOR (DijkstraTxWitsRaw era) where
  encCBOR (DijkstraTxWitsRaw vkeys boots scripts dats rdmrs poolVotes) =
    encode $
      Keyed
        ( \a b c d e f g h i ->
            let ps = toScript @'PlutusV1 d <> toScript @'PlutusV2 e <> toScript @'PlutusV3 f
             in DijkstraTxWitsRaw a b (c <> ps) g h i
        )
        !> Omit null (Key 0 $ To vkeys)
        !> Omit null (Key 2 $ To boots)
        !> Omit
          null
          ( Key 1 $
              E
                (encodeWithSetTag . mapMaybe getNativeScript . Map.elems)
                (Map.filter isNativeScript scripts)
          )
        !> Omit null (Key 3 $ encodePlutus SPlutusV1)
        !> Omit null (Key 6 $ encodePlutus SPlutusV2)
        !> Omit null (Key 7 $ encodePlutus SPlutusV3)
        !> Omit (null . unTxDats) (Key 4 $ To dats)
        !> Omit (null . unRedeemers) (Key 5 $ To rdmrs)
        -- Key 8 is left unallocated for plutus_v4_script.
        !> Omit null (Key 9 $ To poolVotes)
    where
      encodeWithSetTag xs = encodeTag setTag <> encCBOR xs
      encodePlutus ::
        PlutusLanguage l =>
        SLanguage l ->
        Encode (Closed Dense) (Map.Map ScriptHash (Plutus l))
      encodePlutus slang =
        E
          (encodeWithSetTag . encCBOR . map plutusBinary . Map.elems)
          (Map.mapMaybe (toPlutusScript >=> toPlutusSLanguage slang) scripts)
      toScript ::
        forall l h. PlutusLanguage l => Map.Map h (Plutus l) -> Map.Map h (Script era)
      toScript ps =
        case runFail $ traverse (fmap fromPlutusScript . mkPlutusScript) ps of
          Left e -> error $ "Impossible: Re-constructing unsupported language: " <> e
          Right plutusScripts -> plutusScripts

-- Dijkstra exists from protocol version 12 onwards, so unlike 'AlonzoTxWits'
-- the decoder does not need any pre-12 compatibility paths.
instance
  (AlonzoEraScript era, DecCBOR (Annotator (NativeScript era))) =>
  DecCBOR (Annotator (DijkstraTxWitsRaw era))
  where
  decCBOR = decodeSparseKeyed TypeName [] (pure emptyDijkstraTxWitsRaw) decoderByKey
    where
      addrWitsSetDecoder :: (Ord a, DecCBOR a) => Decoder s (Set a)
      addrWitsSetDecoder =
        decodeNonEmptySetLikeEnforceNoDuplicates Set.insert (\s -> (Set.size s, s)) decCBOR
      {-# INLINE addrWitsSetDecoder #-}

      nativeScriptsDecoder :: Decoder s (Annotator (Map ScriptHash (Script era)))
      nativeScriptsDecoder =
        decodeNonEmptySetLikeEnforceNoDuplicatesAnn
          (\x m -> let (k, v) = asHashedScriptPair @era (fromNativeScript x) in Map.insert k v m)
          (\m -> (Map.size m, m))
      {-# INLINE nativeScriptsDecoder #-}

      decoderByKey ::
        Annotator (DijkstraTxWitsRaw era) ->
        Word ->
        Maybe (Decoder s (Annotator (DijkstraTxWitsRaw era)))
      decoderByKey acc = \case
        0 -> Just $ decodeAccA acc (\x w -> w {dtwrAddrTxWits = x}) (pure <$> addrWitsSetDecoder)
        1 -> Just $ decodeAccA acc addScriptsDijkstraTxWitsRaw nativeScriptsDecoder
        2 -> Just $ decodeAccA acc (\x w -> w {dtwrBootAddrTxWits = x}) (pure <$> addrWitsSetDecoder)
        3 -> Just $ decodeAccA acc addScriptsDijkstraTxWitsRaw (pure <$> alonzoPlutusScriptDecoder SPlutusV1)
        4 -> Just $ decodeAccA acc (\x w -> w {dtwrDatsTxWits = x}) decCBOR
        5 -> Just $ decodeAccA acc (\x w -> w {dtwrRdmrsTxWits = x}) decCBOR
        6 -> Just $ decodeAccA acc addScriptsDijkstraTxWitsRaw (pure <$> alonzoPlutusScriptDecoder SPlutusV2)
        7 -> Just $ decodeAccA acc addScriptsDijkstraTxWitsRaw (pure <$> alonzoPlutusScriptDecoder SPlutusV3)
        9 -> Just $ decodeAccA acc (\x w -> w {dtwrPoolVoteTxWits = x}) (pure <$> decCBOR)
        _ -> Nothing
      {-# INLINE decoderByKey #-}
  {-# INLINE decCBOR #-}

deriving via
  Mem (DijkstraTxWitsRaw era)
  instance
    ( AlonzoEraScript era
    , DecCBOR (Annotator (NativeScript era))
    ) =>
    DecCBOR (Annotator (DijkstraTxWits era))

addScriptsDijkstraTxWitsRaw ::
  Map ScriptHash (Script era) ->
  DijkstraTxWitsRaw era ->
  DijkstraTxWitsRaw era
addScriptsDijkstraTxWitsRaw scriptWitnesses txWits =
  txWits
    { dtwrScriptTxWits = scriptWitnesses <> dtwrScriptTxWits txWits
    }
{-# INLINE addScriptsDijkstraTxWitsRaw #-}

instance
  ( AlonzoEraScript era
  , ToJSON (Script era)
  , ToJSON (TxDats era)
  , ToJSON (Redeemers era)
  ) =>
  ToJSON (DijkstraTxWits era)
  where
  toJSON (DijkstraTxWits vkeys boots scripts dats rdmrs poolVotes) =
    object
      [ "addrWits" .= Set.toList vkeys
      , "bootWits" .= Set.toList boots
      , "scriptWits" .= scripts
      , "datums" .= dats
      , "redeemers" .= rdmrs
      , "poolVoteWits" .= poolVotes
      ]

instance
  ( AlonzoEraScript era
  , FromJSON (Script era)
  , FromJSON (TxDats era)
  , FromJSON (Redeemers era)
  ) =>
  FromJSON (DijkstraTxWits era)
  where
  parseJSON = withObject "DijkstraTxWits" $ \o ->
    DijkstraTxWits
      <$> (Set.fromList <$> o .: "addrWits")
      <*> (Set.fromList <$> o .: "bootWits")
      <*> o .: "scriptWits"
      <*> o .: "datums"
      <*> o .: "redeemers"
      <*> o .: "poolVoteWits"
