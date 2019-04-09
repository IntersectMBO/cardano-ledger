{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DeriveAnyClass             #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE DerivingStrategies         #-}

module Cardano.Chain.Txp.UTxO
  ( UTxO(..)
  , UTxOError
  , fromList
  , member
  , lookupAddress
  , union
  , balance
  , (<|)
  , (</|)
  , txOutputUTxO
  , isRedeemUTxO
  )
where

import Cardano.Prelude

import qualified Data.List.NonEmpty as NE
import qualified Data.Map.Strict as M
import qualified Data.Set as S

import Cardano.Chain.Common
  (Address, Lovelace, LovelaceError, isRedeemAddress, sumLovelace)
import Cardano.Chain.Txp.Tx (Tx(..), TxId, TxIn(..), TxOut(..))
import Cardano.Chain.Txp.Compact
  ( CompactTxIn
  , CompactTxOut
  , fromCompactTxOut
  , toCompactTxIn
  , toCompactTxOut
  )
import Cardano.Crypto (hash)


newtype UTxO = UTxO
  { unUTxO :: Map CompactTxIn CompactTxOut
  } deriving (Eq, Show, Generic)
    deriving newtype HeapWords
    deriving anyclass NFData

data UTxOError
  = UTxOMissingInput TxIn
  | UTxOOverlappingUnion
  deriving (Eq, Show)

fromList :: [(TxIn, TxOut)] -> UTxO
fromList = UTxO . M.fromList . toCompactTxInTxOutList
 where
  toCompactTxInTxOutList :: [(TxIn, TxOut)] -> [(CompactTxIn, CompactTxOut)]
  toCompactTxInTxOutList = map (bimap toCompactTxIn toCompactTxOut)

member :: TxIn -> UTxO -> Bool
member txIn = M.member (toCompactTxIn txIn) . unUTxO

lookupAddress :: TxIn -> UTxO -> Either UTxOError Address
lookupAddress txIn =
  maybe (Left $ UTxOMissingInput txIn) (Right . txOutAddress . fromCompactTxOut)
    . M.lookup (toCompactTxIn txIn)
    . unUTxO

union :: MonadError UTxOError m => UTxO -> UTxO -> m UTxO
union (UTxO m) (UTxO m') = do
  let m'' = M.union m m'
  (M.size m'' == M.size m + M.size m') `orThrowError` UTxOOverlappingUnion
  pure $ UTxO m''

balance :: UTxO -> Either LovelaceError Lovelace
balance = sumLovelace . fmap compactTxOutValue . M.elems . unUTxO
 where
  compactTxOutValue :: CompactTxOut -> Lovelace
  compactTxOutValue = txOutValue . fromCompactTxOut

(<|) :: Set TxIn -> UTxO -> UTxO
(<|) inputs = UTxO . flip M.restrictKeys compactInputs . unUTxO
 where
  compactInputs = S.map toCompactTxIn inputs

(</|) :: Set TxIn -> UTxO -> UTxO
(</|) inputs = UTxO . flip M.withoutKeys compactInputs . unUTxO
 where
  compactInputs = S.map toCompactTxIn inputs

txOutputUTxO :: Tx -> UTxO
txOutputUTxO tx = UTxO $ M.fromList
  [ (toCompactTxIn (TxInUtxo (txId tx) ix), (toCompactTxOut txOut))
    | (ix, txOut) <- indexedOutputs ]
 where
  indexedOutputs :: [(Word32, TxOut)]
  indexedOutputs = zip [0 ..] (NE.toList $ txOutputs tx)

  txId :: Tx -> TxId
  txId = hash

isRedeemUTxO :: UTxO -> Bool
isRedeemUTxO =
  all (isRedeemAddress . txOutAddress . fromCompactTxOut)
    . M.elems
    . unUTxO
