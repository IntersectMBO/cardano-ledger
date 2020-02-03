{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell #-}


module Tx
  ( -- transaction
    Tx(..)
  , TxBody(..)
  , TxOut(..)
  , TxIn(..)
  , TxId(..)
  , Value
  , makeAdaValue
  , txUpdate
  , txinputs
  , outputs
  , certs
  , wdrls
  , txfee
  , ttl
  , body
  , metadata
  , witnessVKeySet
  , unsignedData
  , txinputs_vf
--  , witnessMSigMap
    -- witness data
  , WitVKey(..)
  , MultiSignatureScript
  , validateScript
  , hashScript
--  , txwitsScript
  , extractKeyHash
  , extractScriptHash
  , extractGenKeyHash
  , getKeyCombinations
  , getKeyCombination
  , txToCBORWits
  , cborWitsToTx
  , getrefs
  , addrTxOut
  , extractGenKeyHash
  , getKeyCombinations
  , getKeyCombination
  , adaID
  , adaToken
  )
where

import           Numeric.Natural (Natural)

import           BaseTypes (invalidKey)
import           Keys (AnyKeyHash, GenKeyHash, undiscriminateKeyHash)

import           Cardano.Binary (FromCBOR (fromCBOR), ToCBOR (toCBOR), decodeListLenOf, decodeWord,
                     encodeListLen, encodeMapLen, encodeWord, encodeWord8)
import           Cardano.Crypto.Hash (hashWithSerialiser)
import           Cardano.Ledger.Shelley.Crypto
import           Cardano.Prelude (NoUnexpectedThunks (..), catMaybes)
import           Data.Foldable (fold, toList)
import qualified Data.List as List (concat, concatMap, permutations)
import           Data.Map.Strict (Map, singleton)
import qualified Data.Map.Strict as Map
import           Data.Maybe (mapMaybe)
import qualified Data.Sequence as Seq
import           Data.Set (Set, filter)
import qualified Data.Set as Set
import           GHC.Generics (Generic)
import           Lens.Micro.TH (makeLenses)
import           MetaData (MetaData)

import           Serialization (CborSeq (..), mapHelper)
import           TxData (Credential (..), MultiSig (..), Script (..), ScriptHash (..), TxBody (..),
                     TxId (..), TxIn (..), TxOut (..), WitVKey (..), certs, inputs,
                     nativeMultiSigTag, outputs, ttl, txUpdate, txfee, wdrls, witKeyHash)
import           TxData (Credential (..), StakeCredential, Tx (..),
                     TxBody (..), TxId (..), TxIn (..), TxInTx (..), TxOut (..), WitVKey (..), UnsignedData (..),
import           TxData (Credential (..), MultiSig (..), ScriptHash (..), StakeCredential, Tx (..),
                     TxBody (..), TxId (..), TxIn (..), TxInTx (..), TxOut (..), WitVKey (..), UnsignedData (..),
                     Addr, body, certs,
                     txinputs, outputs, ttl, txUpdate, txfee, wdrls, witKeyHash, unsignedData,
                     witnessVKeySet, txlst, forged, txexunits, hashPP, txvlds, txdats,
                     txvaltag)
import           Scripts
import           CostModel
import           Data.Word (Word8)
--import           Data.ByteString.Internal (unpackBytes)
import           Data.ByteString.Char8 (ByteString, pack)

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
    mapParts <- mapHelper $
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


-- | Script evaluator for native multi-signature scheme. 'vhks' is the set of
-- key hashes that signed the transaction to be validated.
evalNativeMultiSigScript :: MultiSig crypto
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
validateNativeMultiSigScript :: (Crypto crypto)
  => MultiSig crypto
  -> Tx crypto
  -> Bool
validateNativeMultiSigScript msig tx =
  evalNativeMultiSigScript msig vhks
  where vhks    = Set.map witKeyHash (_witnessVKeySet (_unsignedData tx))

-- | Hashes native multi-signature script, appending the 'nativeMultiSigTag' in
-- front and then calling the script CBOR function.
hashAnyScript
  :: Crypto crypto
  => Script crypto
  -> ScriptHash crypto
hashAnyScript (MultiSigScript msig) =
  ScriptHash $ hashWithSerialiser (\x -> encodeWord8 nativeMultiSigTag
                                          <> toCBOR x) (MultiSigScript msig)


-- | Hashes plutus script, appending the 'plutusTag' in
-- front and then calling the script CBOR function.
hashPLCScript :: Crypto crypto
  => ScriptPLC crypto
  -> ScriptHash crypto
hashPLCScript plc =
  ScriptHashPLC $ hashWithSerialiser (\x -> encodeWord8 plutusTag
                                          <> toCBOR x) plc


-- | native currency (Ada) currencyID
adaID :: (Crypto crypto) => ScriptHash crypto
adaID = hashPLCScript (ScriptPLC 1)

adaToken :: ByteString
adaToken =  pack "Ada"

-- | returns a Value representing the given amount of Ada
makeAdaValue :: Crypto crypto => Coin -> Value crypto
makeAdaValue (Coin c) = Value (singleton adaID (singleton adaToken (Quantity c)))


-- | 0 Ada
makeAdaValue :: Crypto crypto => Value crypto
makeAdaValue = Value (singleton (hashPLCScript (ScriptPLC 1)) (singleton adaToken (Quantity 0)))

-- | Get one possible combination of keys for multi signature script
getKeyCombination :: MultiSig crypto -> [AnyKeyHash crypto]

getKeyCombination (RequireSignature hk) = [hk]
getKeyCombination (RequireAllOf msigs) =
  List.concatMap getKeyCombination msigs
getKeyCombination (RequireAnyOf msigs) =
  case msigs of
    []  -> []
    x:_ -> getKeyCombination x
getKeyCombination (RequireMOf m msigs) =
  List.concatMap getKeyCombination (take m msigs)


-- | Get all valid combinations of keys for given multi signature. This is
-- mainly useful for testing.
getKeyCombinations :: MultiSig crypto -> [[AnyKeyHash crypto]]

getKeyCombinations (RequireSignature hk) = [[hk]]

getKeyCombinations (RequireAllOf msigs) = [List.concat $
  List.concatMap getKeyCombinations msigs]

getKeyCombinations (RequireAnyOf msigs) = List.concatMap getKeyCombinations msigs

getKeyCombinations (RequireMOf m msigs) =
  let perms = map (take m) $ List.permutations msigs in
    map (concat . List.concatMap getKeyCombinations) perms


-- | Magic number representing the tag of the native multi-signature script
-- language. For each script language included, a new tag is chosen and the tag
-- is included in the script hash for a script.
nativeMultiSigTag :: Word8
nativeMultiSigTag = 0

-- | Magic number representing the tag of the Plutus script
-- language. For each script language included, a new tag is chosen and the tag
-- is included in the script hash for a script.
plutusTag :: Word8
plutusTag = 1

instance Crypto crypto =>
  MultiSignatureScript (MultiSig crypto) crypto where
  validateScript = validateNativeMultiSigScript
  hashScript = \x -> hashAnyScript (MultiSigScript x)

-- -- | Multi-signature script witness accessor function for Transactions
txwitsScript
  :: Tx crypto
  -> Map (ScriptHashMSig crypto) (MultiSig crypto)
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

-- | make validation data to pass to Plutus validator
validationData :: UTxO -> Tx -> CurItem -> Data
validationData _ _ _ = Data 1

makeLenses ''Tx

-- accessors of data in TxIn and TxOut
getref :: TxInTx crypto -> TxIn crypto
getref (TxIn  txid ix _) = (txid,ix)

-- | access only the output reference part of a TxInTx
getrefs :: (Set (TxInTx crypto)) -> Set (TxIn crypto)
getrefs = Set.map getref

-- | an input is a fee
inputisfee                        :: (TxInTx crypto) -> Bool
inputisfee (TxInTx _ _ (IsFee Yes)) = True
inputisfee _                        = False

-- | just the for-fee-payment inputs
txinputs_vf    :: TxBody crypto -> Set (TxIn crypto)
txinputs_vf tx =  getrefs $ Data.Set.filter inputisfee (_txinputs tx)

-- | return the address in an output
addrTxOut :: TxOut crypto -> Addr crypto
addrTxOut (TxOutVK  a _  ) = a
addrTxOut (TxOutScr a _ _) = a


-- | temporary validator always returns true and same amount of resources
valPLCupTo :: CostMod -> ScriptPLC crypto -> ([Data crypto], ExUnits)
  -> (IsValidating, ExUnits)
valPLCupTo _ _ _ = (IsValidating Yes, ExUnits 0 0)
