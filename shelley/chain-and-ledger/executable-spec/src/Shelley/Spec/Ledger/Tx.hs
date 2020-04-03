{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell #-}


module Shelley.Spec.Ledger.Tx
  ( -- transaction
    Tx(..)
  , TxBody(..)
  , TxOut(..)
  , TxIn(..)
  , TxId(..)
    -- witness data
  , WitVKey(..)
  , MultiSignatureScript
  , validateScript
  , hashScript
  , txwitsScript
  , extractKeyHash
  , extractScriptHash
  , extractGenKeyHash
  , getKeyCombinations
  , getKeyCombination
  , txToCBORWits
  , cborWitsToTx
  )
where


import           Shelley.Spec.Ledger.BaseTypes (invalidKey)
import           Shelley.Spec.Ledger.Keys (AnyKeyHash, GenKeyHash, undiscriminateKeyHash)

import           Cardano.Binary (FromCBOR (fromCBOR), ToCBOR (toCBOR), decodeListLenOf, decodeWord,
                     encodeListLen, encodeMapLen, encodeWord)
import           Cardano.Prelude (NoUnexpectedThunks (..), catMaybes)
import           Data.Foldable (fold, toList)
import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import           Data.Maybe (mapMaybe)
import qualified Data.Sequence as Seq
import           Data.Set (Set)
import qualified Data.Set as Set
import           GHC.Generics (Generic)
import           Shelley.Spec.Ledger.Crypto
import           Shelley.Spec.Ledger.MetaData (MetaData)

import           Shelley.Spec.Ledger.Scripts
import           Shelley.Spec.Ledger.Serialization (CborSeq (..), decodeMapContents)
import           Shelley.Spec.Ledger.TxData (Credential (..), TxBody (..), TxId (..), TxIn (..),
                     TxOut (..), WitVKey (..), witKeyHash)

-- |A fully formed transaction.
data Tx crypto
  = Tx
      { _body           :: !(TxBody crypto)
      , _witnessVKeySet :: !(Set (WitVKey crypto))
      , _witnessMSigMap ::
          Map (ScriptHash crypto) (MultiSig crypto)
      , _metadata       :: Maybe MetaData
      } deriving (Show, Eq, Generic)

instance Crypto crypto => NoUnexpectedThunks (Tx crypto)

txToCBORWits
  :: Tx crypto
  -> CBORWits crypto
txToCBORWits tx = CBORWits
  { _cborWitsVKeys   = (CborSeq . Seq.fromList . Set.toList . _witnessVKeySet) tx
  , _cborWitsScripts = (CborSeq . Seq.fromList . Map.elems . _witnessMSigMap) tx
  }

cborWitsToTx
  :: (Crypto crypto)
  => TxBody crypto
  -> CBORWits crypto
  -> Maybe MetaData
  -> Tx crypto
cborWitsToTx b ws md = Tx
  { _body = b
  , _witnessVKeySet =
      (Set.fromList . toList . unwrapCborSeq . _cborWitsVKeys) ws
  , _witnessMSigMap =
      Map.fromList $
        fmap
          (\x -> (hashScript x, x))
          ((toList . unwrapCborSeq . _cborWitsScripts) ws)
  , _metadata = md
  }

data CBORWits crypto
  = CBORWits
      { _cborWitsVKeys   :: CborSeq (WitVKey crypto)
      , _cborWitsScripts :: CborSeq (MultiSig crypto)
      } deriving (Generic)

instance (Crypto crypto) =>
  ToCBOR (CBORWits crypto) where
  toCBOR ws =
    let l = catMaybes $
              [ encodeMapElement 0 $ _cborWitsVKeys ws
              , encodeMapElement 1 $ _cborWitsScripts ws
              ]
        n = fromIntegral $ length l
    in encodeMapLen n <> fold l
    where
      encodeMapElement ix x = if null x then Nothing else Just (encodeWord ix <> toCBOR x)

instance (Crypto crypto) =>
  FromCBOR (CBORWits crypto) where
  fromCBOR = do
    mapParts <- decodeMapContents $
      decodeWord >>= \case
        0 -> fromCBOR >>= \x -> pure (\w -> w { _cborWitsVKeys  = x })
        1 -> fromCBOR >>= \x -> pure (\w -> w { _cborWitsScripts  = x })
        k -> invalidKey k
    pure $ foldr ($) start mapParts
    where
      start = CBORWits
         { _cborWitsVKeys   = CborSeq Seq.empty
         , _cborWitsScripts = CborSeq Seq.empty
         }

instance
  (Crypto crypto)
  => ToCBOR (Tx crypto)
 where
  toCBOR tx =
    encodeListLen 3
      <> toCBOR (_body tx)
      <> toCBOR (txToCBORWits tx)
      <> toCBOR (_metadata tx)

instance Crypto crypto => FromCBOR (Tx crypto) where
  fromCBOR = decodeListLenOf 3 >>
    cborWitsToTx <$> fromCBOR <*> fromCBOR <*> fromCBOR

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
  :: MultiSig crypto
  -> Set (AnyKeyHash crypto)
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
  evalNativeMultiSigScript msig vhks
  where witsSet = _witnessVKeySet tx
        vhks    = Set.map witKeyHash witsSet


-- | Multi-signature script witness accessor function for Transactions
txwitsScript
  :: Tx crypto
  -> Map (ScriptHash crypto) (MultiSig crypto)
txwitsScript = _witnessMSigMap

extractKeyHash
  :: [Credential crypto]
  -> [AnyKeyHash crypto]
extractKeyHash =
  mapMaybe (\case
                KeyHashObj hk -> Just $ undiscriminateKeyHash hk
                _ -> Nothing)

extractScriptHash
  :: [Credential crypto]
  -> [ScriptHash crypto]
extractScriptHash =
  mapMaybe (\case
                ScriptHashObj hk -> Just hk
                _ -> Nothing)

extractGenKeyHash
  :: [GenKeyHash crypto]
  -> [AnyKeyHash crypto]
extractGenKeyHash = map undiscriminateKeyHash
