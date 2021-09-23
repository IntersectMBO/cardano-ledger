{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -fno-warn-missed-specialisations #-}

module Cardano.Chain.UTxO.UTxO
  ( UTxO (..),
    UTxOError (..),
    empty,
    fromList,
    fromBalances,
    fromTxOut,
    toList,
    member,
    lookup,
    lookupCompact,
    lookupAddress,
    union,
    concat,
    balance,
    (<|),
    (</|),
    txOutputUTxO,
    isRedeemUTxO,
  )
where

import Cardano.Binary
  ( DecoderError (..),
    FromCBOR (..),
    ToCBOR (..),
    decodeListLen,
    decodeWord8,
    encodeListLen,
    matchSize,
  )
import Cardano.Chain.Common
  ( Address,
    Lovelace,
    LovelaceError,
    isRedeemAddress,
    sumLovelace,
  )
import Cardano.Chain.UTxO.Compact
  ( CompactTxIn,
    CompactTxOut,
    fromCompactTxIn,
    fromCompactTxOut,
    toCompactTxIn,
    toCompactTxOut,
  )
import Cardano.Chain.UTxO.Tx (Tx (..), TxId, TxIn (..), TxOut (..))
import Cardano.Crypto (serializeCborHash)
import Cardano.Prelude hiding (concat, empty, toList)
import Data.Coerce
import qualified Data.List.NonEmpty as NE
import qualified Data.Map.Strict as M
import qualified Data.Set as S
import NoThunks.Class (NoThunks (..))

newtype UTxO = UTxO
  { unUTxO :: Map CompactTxIn CompactTxOut
  }
  deriving (Eq, Show, Generic)
  deriving newtype (HeapWords, FromCBOR, ToCBOR)
  deriving anyclass (NFData, NoThunks)

data UTxOError
  = UTxOMissingInput TxIn
  | UTxOOverlappingUnion
  deriving (Eq, Show)

instance ToCBOR UTxOError where
  toCBOR = \case
    UTxOMissingInput txIn ->
      encodeListLen 2 <> toCBOR @Word8 0 <> toCBOR txIn
    UTxOOverlappingUnion ->
      encodeListLen 1 <> toCBOR @Word8 1

instance FromCBOR UTxOError where
  fromCBOR = do
    len <- decodeListLen
    tag <- decodeWord8
    case tag of
      0 -> matchSize "UTxOError" 2 len >> UTxOMissingInput <$> fromCBOR
      1 -> matchSize "UTxOError" 1 len $> UTxOOverlappingUnion
      _ -> cborError $ DecoderErrorUnknownTag "UTxOError" tag

empty :: UTxO
empty = UTxO mempty

fromList :: [(TxIn, TxOut)] -> UTxO
fromList = UTxO . M.fromList . toCompactTxInTxOutList
  where
    toCompactTxInTxOutList :: [(TxIn, TxOut)] -> [(CompactTxIn, CompactTxOut)]
    toCompactTxInTxOutList = map (bimap toCompactTxIn toCompactTxOut)

-- | Create a 'UTxO' from a list of initial balances
fromBalances :: [(Address, Lovelace)] -> UTxO
fromBalances =
  fromRight (panic "fromBalances: duplicate Address in initial balances")
    . concat
    . fmap (fromTxOut . uncurry TxOut)

-- | Construct a UTxO from a TxOut. This UTxO is a singleton with a TxIn that
-- references an address constructed by hashing the TxOut address. This means
-- it is not guaranteed (or likely) to be a real address.
fromTxOut :: TxOut -> UTxO
fromTxOut out = fromList [(TxInUtxo (coerce . serializeCborHash $ txOutAddress out) 0, out)]

toList :: UTxO -> [(TxIn, TxOut)]
toList = fmap (bimap fromCompactTxIn fromCompactTxOut) . M.toList . unUTxO

member :: TxIn -> UTxO -> Bool
member txIn = M.member (toCompactTxIn txIn) . unUTxO

lookup :: TxIn -> UTxO -> Maybe TxOut
lookup txIn = fmap fromCompactTxOut . M.lookup (toCompactTxIn txIn) . unUTxO

lookupCompact :: CompactTxIn -> UTxO -> Maybe CompactTxOut
lookupCompact txIn = M.lookup txIn . unUTxO

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

concat :: MonadError UTxOError m => [UTxO] -> m UTxO
concat = foldM union empty

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
txOutputUTxO tx =
  UTxO $
    M.fromList
      [ (toCompactTxIn (TxInUtxo (txId tx) ix), (toCompactTxOut txOut))
        | (ix, txOut) <- indexedOutputs
      ]
  where
    indexedOutputs :: [(Word32, TxOut)]
    indexedOutputs = zip [0 ..] (NE.toList $ txOutputs tx)

    txId :: Tx -> TxId
    txId = serializeCborHash

isRedeemUTxO :: UTxO -> Bool
isRedeemUTxO =
  all (isRedeemAddress . txOutAddress . fromCompactTxOut)
    . M.elems
    . unUTxO
