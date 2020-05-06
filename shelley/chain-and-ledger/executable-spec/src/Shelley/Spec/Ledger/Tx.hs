{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}


module Shelley.Spec.Ledger.Tx
  ( -- transaction
    Tx(Tx
      , _body
      , _witnessVKeySet
      , _witnessMSigMap
      , _metadata
      , txWitsBytes
      )
  , TxBody(..)
  , TxOut(..)
  , TxIn(..)
  , TxId(..)
  , Wits(..)
  , decodeWits
  , segwitTx
    -- witness data
  , WitVKey(..)
  , MultiSignatureScript
  , validateScript
  , hashScript
  , txwitsScript
  , extractKeyHash
  , extractScriptHash
  , getKeyCombinations
  , getKeyCombination
  )
where


import           Shelley.Spec.Ledger.BaseTypes (StrictMaybe, invalidKey, maybeToStrictMaybe,
                     strictMaybeToMaybe)
import           Shelley.Spec.Ledger.Keys

import           Cardano.Binary (Annotator (..), Decoder, FromCBOR (fromCBOR), ToCBOR (toCBOR),
                     annotatorSlice, decodeWord, encodeListLen, encodeMapLen, encodeNull,
                     encodePreEncoded, encodeWord, serialize, serializeEncoding, withSlice)
import           Cardano.Prelude (AllowThunksIn (..), LByteString, NoUnexpectedThunks (..),
                     catMaybes)
import qualified Data.ByteString.Lazy as BSL
import           Data.Foldable (fold)
import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import           Data.Maybe (mapMaybe)
import           Data.Set (Set)
import qualified Data.Set as Set
import           GHC.Generics (Generic)
import           Shelley.Spec.Ledger.Crypto
import           Shelley.Spec.Ledger.MetaData (MetaData)

import           Shelley.Spec.Ledger.Credential (Credential (..))
import           Shelley.Spec.Ledger.Scripts
import           Shelley.Spec.Ledger.Serialization (decodeList, decodeMapContents, decodeNullMaybe,
                     decodeRecordNamed, encodeFoldable, encodeNullMaybe)
import           Shelley.Spec.Ledger.TxData (TxBody (..), TxId (..), TxIn (..), TxOut (..),
                     WitVKey (..), witKeyHash)

-- |A fully formed transaction.
data Tx crypto
  = Tx'
      { _body'           :: !(TxBody crypto)
      , _witnessVKeySet' :: !(Set (WitVKey crypto))
      , _witnessMSigMap' :: !(Map (ScriptHash crypto) (MultiSig crypto))
      , _metadata'       :: !(StrictMaybe MetaData)
      , txWitsBytes      :: LByteString
      , txFullBytes      :: LByteString
      } deriving (Show, Eq, Generic)
        deriving NoUnexpectedThunks via
          AllowThunksIn
            '[ "txWitsBytes"
            , "txFullBytes"
            ] (Tx crypto)

pattern Tx :: Crypto crypto
  => TxBody crypto
  -> Set (WitVKey crypto)
  -> Map (ScriptHash crypto) (MultiSig crypto)
  -> StrictMaybe MetaData
  -> Tx crypto
pattern Tx { _body, _witnessVKeySet, _witnessMSigMap, _metadata } <-
  Tx' _body _witnessVKeySet _witnessMSigMap _metadata _ _
  where
  Tx body witnessVKeySet witnessMSigMap metadata =
    let encodeMapElement ix enc x =
          if null x then Nothing else Just (encodeWord ix <> enc x)
        l = catMaybes $
              [ encodeMapElement 0 encodeFoldable witnessVKeySet
              , encodeMapElement 1 encodeFoldable witnessMSigMap
              ]
        n = fromIntegral $ length l
        bodyBytes = serialize body
        witsBytes = serializeEncoding $ encodeMapLen n <> fold l
        wrappedMetadataBytes = serializeEncoding $
          encodeNullMaybe toCBOR (strictMaybeToMaybe metadata)
        fullBytes = (serializeEncoding $ encodeListLen 3)
          <> bodyBytes <> witsBytes <> wrappedMetadataBytes
     in Tx'
        { _body'           = body
        , _witnessVKeySet' = witnessVKeySet
        , _witnessMSigMap' = witnessMSigMap
        , _metadata'       = metadata
        , txWitsBytes      = witsBytes
        , txFullBytes      = fullBytes
        }

{-# COMPLETE Tx #-}

segwitTx
  :: Crypto crypto
  => Annotator (TxBody crypto)
  -> (Annotator (Wits crypto), Annotator LByteString)
  -> Maybe (Annotator MetaData)
  -> Annotator (Tx crypto)
segwitTx
  bodyAnn
  (wits, witsAnn)
  metaAnn
  = Annotator $ \bytes ->
      let body = runAnnotator bodyAnn bytes
          Wits witnessVKeySet witnessMSigMap = runAnnotator wits bytes
          witsBytes = runAnnotator witsAnn bytes
          metadata = flip runAnnotator bytes <$> metaAnn
          wrappedMetadataBytes = case metadata of
            Nothing -> serializeEncoding encodeNull
            Just b -> serialize b
          fullBytes = (serializeEncoding $ encodeListLen 3)
            <> serialize body <> witsBytes <> wrappedMetadataBytes
       in Tx'
          { _body'           = body
          , _witnessVKeySet' = witnessVKeySet
          , _witnessMSigMap' = witnessMSigMap
          , _metadata'       = maybeToStrictMaybe metadata
          , txWitsBytes      = witsBytes
          , txFullBytes      = fullBytes
          }

type MultiSigMap crypto = Map (ScriptHash crypto) (MultiSig crypto)
data Wits crypto = Wits
  (Set (WitVKey crypto))
  (MultiSigMap crypto)

decodeWits :: Crypto crypto => Decoder s (Annotator (Wits crypto))
decodeWits = do
  mapParts <- decodeMapContents $
    decodeWord >>= \case
      0 -> decodeList fromCBOR >>= \x -> pure (\(_,b) -> (x,b))
      1 -> decodeList fromCBOR >>= \x -> pure (\(a,_) -> (a,x))
      k -> invalidKey k
  let (witsVKeys, witsScripts) = foldr ($) ([], []) mapParts
      keysSet = Set.fromList <$> sequence witsVKeys
      multiSigMap = keyBy hashScript <$> sequence witsScripts
  pure $ Wits <$> keysSet <*> multiSigMap

keyBy :: Ord k => (a -> k) -> [a] -> Map k a
keyBy f xs = Map.fromList $ (\x -> (f x, x)) <$> xs

instance
  (Crypto crypto)
  => ToCBOR (Tx crypto)
 where
  toCBOR tx = encodePreEncoded . BSL.toStrict $ txFullBytes tx

instance Crypto crypto => FromCBOR (Annotator (Tx crypto)) where
  fromCBOR = annotatorSlice $ decodeRecordNamed "Tx" (const 3) $ do
    body <- fromCBOR
    (wits, witsAnn) <- withSlice decodeWits
    meta <- decodeNullMaybe fromCBOR
    pure $ Annotator $ \fullbytes bytes ->
      let (Wits witsVKeys witsScripts) = runAnnotator wits fullbytes
       in Tx' (runAnnotator body fullbytes)
              witsVKeys
              witsScripts
              (maybeToStrictMaybe $ flip runAnnotator fullbytes <$> meta)
              (runAnnotator witsAnn fullbytes)
              bytes

-- | Typeclass for multis-signature script data types. Allows for script
-- validation and hashing.
class (Crypto crypto, ToCBOR a) =>
  MultiSignatureScript a crypto where
  validateScript :: a -> Tx crypto -> Bool
  hashScript :: a -> ScriptHash crypto

-- | instance of MultiSignatureScript type class
instance Crypto crypto =>
  MultiSignatureScript (MultiSig crypto) crypto where
  validateScript = validateNativeMultiSigScript
  hashScript = \x -> hashAnyScript (MultiSigScript x)

-- | Script evaluator for native multi-signature scheme. 'vhks' is the set of
-- key hashes that signed the transaction to be validated.
evalNativeMultiSigScript
  :: Crypto crypto
  => MultiSig crypto
  -> Set (KeyHash 'Witness crypto)
  -> Bool
evalNativeMultiSigScript (RequireSignature hk) vhks = Set.member hk vhks
evalNativeMultiSigScript (RequireAllOf msigs) vhks =
  all (`evalNativeMultiSigScript` vhks) msigs
evalNativeMultiSigScript (RequireAnyOf msigs) vhks =
  any (`evalNativeMultiSigScript` vhks) msigs
evalNativeMultiSigScript (RequireMOf m msigs) vhks =
  m <= sum [if evalNativeMultiSigScript msig vhks then 1 else 0 | msig <- msigs]

-- | Script validator for native multi-signature scheme.
validateNativeMultiSigScript
  :: (Crypto crypto)
  => MultiSig crypto
  -> Tx crypto
  -> Bool
validateNativeMultiSigScript msig tx =
  evalNativeMultiSigScript msig (coerceKeyRole `Set.map` vhks)
  where witsSet = _witnessVKeySet tx
        vhks    = Set.map witKeyHash witsSet


-- | Multi-signature script witness accessor function for Transactions
txwitsScript
  :: Crypto crypto => Tx crypto
  -> Map (ScriptHash crypto) (MultiSig crypto)
txwitsScript = _witnessMSigMap

extractKeyHash
  :: [Credential kr crypto]
  -> [KeyHash kr crypto]
extractKeyHash =
  mapMaybe (\case
                KeyHashObj hk -> Just hk
                _ -> Nothing)

extractScriptHash
  :: [Credential 'Payment crypto]
  -> [ScriptHash crypto]
extractScriptHash =
  mapMaybe (\case
                ScriptHashObj hk -> Just hk
                _ -> Nothing)
