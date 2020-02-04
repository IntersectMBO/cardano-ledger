{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}


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

import           Keys (AnyKeyHash, GenKeyHash, undiscriminateKeyHash)

import           Cardano.Binary (ToCBOR (toCBOR), encodeWord8)
import           Cardano.Crypto.Hash (hashWithSerialiser)
import           Cardano.Ledger.Shelley.Crypto
import qualified Data.List as List (concat, concatMap, permutations)
import           Data.Map.Strict (Map, singleton)
import           Data.Maybe (mapMaybe)
import           Data.Set (Set, filter)
import qualified Data.Set as Set
import           Data.Word (Word8)
import           Data.ByteString.Char8 (ByteString, pack)

import           TxData (Credential (..), MultiSig (..), ScriptHash (..), StakeCredential, Tx (..),
                     TxBody (..), TxId (..), TxIn (..), TxInTx (..), TxOut (..), WitVKey (..), UnsignedData (..),
                     Addr, body, certs,
                     txinputs, outputs, ttl, txUpdate, txfee, wdrls, witKeyHash, unsignedData,
                     witnessVKeySet, txlst, forged, txexunits, hashPP, txvlds, txdats,
                     txvaltag)
import           Scripts
import           CostModel

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
hashNativeMultiSigScript :: Crypto crypto
  => MultiSig crypto
  -> ScriptHash crypto
hashNativeMultiSigScript msig =
  ScriptHashMSig $ hashWithSerialiser (\x -> encodeWord8 nativeMultiSigTag
                                          <> toCBOR x) msig


-- | Hashes plutus script, appending the 'plutusTag' in
-- front and then calling the script CBOR function.
hashPLCScript :: Crypto crypto
  => ScriptPLC crypto
  -> ScriptHash crypto
hashPLCScript (ScriptPLC pv plc) =
  ScriptHashPlutus (hashWithSerialiser (\x -> encodeWord8 plutusTag
                                          <> toCBOR x) plc)


-- | native currency (Ada) currencyID
adaID :: (Crypto crypto) => ScriptHash crypto
adaID = hashPLCScript (ScriptPLC 1)

adaToken :: ByteString
adaToken =  pack "Ada"

-- | returns a Value representing the given amount of Ada
toValue :: Crypto crypto => Coin -> Value crypto
toValue (Coin c) = Value (singleton adaID (singleton adaToken (Quantity c)))


-- | 0 Ada
zeroValue :: Crypto crypto => Value crypto
zeroValue = Value (singleton (hashPLCScript (ScriptPLC (0,0,0) 1)) (singleton adaToken (Quantity 0)))

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
  hashScript = hashNativeMultiSigScript

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
