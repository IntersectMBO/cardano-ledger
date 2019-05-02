{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DeriveAnyClass             #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE DerivingStrategies         #-}

module Cardano.Chain.UTxO.UTxO
  ( UTxO(..)
  , UTxOError
  , empty
  , fromList
  , fromBalances
  , member
  , lookup
  , lookupAddress
  , union
  , balance
  , (<|)
  , (</|)
  , txOutputUTxO
  , isRedeemUTxO
  )
where

import Cardano.Prelude hiding (empty)

import Data.Coerce
import qualified Data.List.NonEmpty as NE
import qualified Data.Map.Strict as M
import qualified Data.Set as S

import Cardano.Chain.Common
  (Address, Lovelace, LovelaceError, isRedeemAddress, sumLovelace)
import Cardano.Chain.UTxO.Tx (Tx(..), TxId, TxIn(..), TxOut(..))
import Cardano.Chain.UTxO.Compact
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

empty :: UTxO
empty = UTxO mempty

fromList :: [(TxIn, TxOut)] -> UTxO
fromList = UTxO . M.fromList . toCompactTxInTxOutList
 where
  toCompactTxInTxOutList :: [(TxIn, TxOut)] -> [(CompactTxIn, CompactTxOut)]
  toCompactTxInTxOutList = map (bimap toCompactTxIn toCompactTxOut)

-- | Create a 'UTxO' from a list of initial balances
fromBalances :: [(Address, Lovelace)] -> UTxO
fromBalances = fromList . fmap utxoEntry
 where
  utxoEntry :: (Address, Lovelace) -> (TxIn, TxOut)
  utxoEntry (addr, lovelace) =
    (TxInUtxo (coerce $ hash addr) 0, TxOut addr lovelace)

member :: TxIn -> UTxO -> Bool
member txIn = M.member (toCompactTxIn txIn) . unUTxO

lookup :: TxIn -> UTxO -> Maybe TxOut
lookup txIn = fmap fromCompactTxOut . M.lookup (toCompactTxIn txIn) . unUTxO

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
