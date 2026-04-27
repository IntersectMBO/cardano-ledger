{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE UndecidableSuperClasses #-}
{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-pattern-binds #-}
{-# OPTIONS_HADDOCK not-home #-}

-- | Provides BlockBody internals
--
-- = Warning
--
-- This module is considered __internal__.
--
-- The contents of this module may change __in any way whatsoever__
-- and __without any warning__ between minor versions of this package.
module Cardano.Ledger.Dijkstra.BlockBody.Internal (
  DijkstraBlockBody (DijkstraBlockBody, MkDijkstraBlockBody),
  DijkstraBlockBodyRaw (..),
  alignedValidFlags,
  mkBasicBlockBodyDijkstra,
  DijkstraEraBlockBody (..),
  PerasCert (..),
  PerasKey (..),
  validatePerasCert,
) where

import Cardano.Ledger.Alonzo.Tx (AlonzoEraTx (..), IsValid (..))
import Cardano.Ledger.BaseTypes (Nonce, ProtVer (..))
import Cardano.Ledger.Binary (
  Annotator (..),
  DecCBOR (..),
  EncCBOR,
  decodeNullStrictMaybe,
  decodeRecordNamed,
  encCBOR,
  encodeListLen,
  encodeNullStrictMaybe,
  serialize',
 )
import Cardano.Ledger.Core
import Cardano.Ledger.Dijkstra.Era
import Cardano.Ledger.Dijkstra.Tx ()
import Cardano.Ledger.MemoBytes (
  Mem,
  MemoBytes,
  MemoHashIndex,
  Memoized (..),
  getMemoBytesHash,
  getMemoRawType,
  lensMemoRawType,
  mkMemoized,
  mkMemoizedEra,
 )
import Control.DeepSeq (NFData)
import Control.Monad (forM_, unless)
import qualified Data.ByteString as BS
import qualified Data.IntSet as IntSet
import Data.Maybe.Strict (StrictMaybe (..))
import qualified Data.Sequence as Seq
import Data.Sequence.Strict (StrictSeq)
import qualified Data.Sequence.Strict as StrictSeq
import Data.Typeable (Typeable)
import Data.Word (Word16)
import GHC.Generics (Generic)
import Lens.Micro
import NoThunks.Class (NoThunks)

-- =================================================

-- $BlockBody
--
-- * BlockBody
--
-- BlockBody provides an alternate way of formatting transactions in a block, in
-- order to support segregated witnessing.

data DijkstraBlockBodyRaw era = DijkstraBlockBodyRaw
  { dbbrTxs :: !(StrictSeq (Tx TopTx era))
  , dbbrPerasCert :: !(StrictMaybe PerasCert)
  -- ^ Optional Peras certificate
  }
  deriving (Generic)

instance (NFData (Tx TopTx era), NFData PerasCert) => NFData (DijkstraBlockBodyRaw era)

type instance MemoHashIndex (DijkstraBlockBodyRaw era) = EraIndependentBlockBody

instance EraBlockBody DijkstraEra where
  type BlockBody DijkstraEra = DijkstraBlockBody DijkstraEra
  mkBasicBlockBody = mkBasicBlockBodyDijkstra
  txSeqBlockBodyL = lensMemoRawType @DijkstraEra dbbrTxs (\bb p -> bb {dbbrTxs = p})
  hashBlockBody (MkDijkstraBlockBody m) = extractHash $ getMemoBytesHash m
  numSegComponents = 1
  blockBodySize (ProtVer v _) = BS.length . serialize' v . encCBOR

mkBasicBlockBodyDijkstra :: forall era. AlonzoEraTx era => DijkstraBlockBody era
mkBasicBlockBodyDijkstra = mkMemoized (eraProtVerLow @era) $ DijkstraBlockBodyRaw mempty SNothing
{-# INLINEABLE mkBasicBlockBodyDijkstra #-}

-- | Dijkstra-specific extensions to 'EraBlockBody'
class EraBlockBody era => DijkstraEraBlockBody era where
  perasCertBlockBodyL :: Lens' (BlockBody era) (StrictMaybe PerasCert)
  -- ^ Lens to access the optional Peras certificate in the block body

instance DijkstraEraBlockBody DijkstraEra where
  perasCertBlockBodyL = lensMemoRawType @DijkstraEra dbbrPerasCert (\bb c -> bb {dbbrPerasCert = c})

deriving instance (Typeable era, NoThunks (Tx TopTx era)) => NoThunks (DijkstraBlockBodyRaw era)

deriving stock instance Show (Tx TopTx era) => Show (DijkstraBlockBodyRaw era)

deriving stock instance Eq (Tx TopTx era) => Eq (DijkstraBlockBodyRaw era)

newtype DijkstraBlockBody era = MkDijkstraBlockBody (MemoBytes (DijkstraBlockBodyRaw era))
  deriving (Generic)

deriving instance Eq (Tx TopTx era) => Eq (DijkstraBlockBody era)

deriving instance Show (Tx TopTx era) => Show (DijkstraBlockBody era)

deriving newtype instance
  (NFData (Tx TopTx era), NFData PerasCert) => NFData (DijkstraBlockBody era)

deriving newtype instance EncCBOR (DijkstraBlockBody era)

instance Memoized (DijkstraBlockBody era) where
  type RawType (DijkstraBlockBody era) = DijkstraBlockBodyRaw era

pattern DijkstraBlockBody ::
  AlonzoEraTx era =>
  StrictSeq (Tx TopTx era) ->
  StrictMaybe PerasCert ->
  DijkstraBlockBody era
pattern DijkstraBlockBody txs perasCert <- (getMemoRawType -> DijkstraBlockBodyRaw txs perasCert)
  where
    DijkstraBlockBody txs perasCert =
      mkMemoizedEra @DijkstraEra $
        DijkstraBlockBodyRaw txs perasCert

{-# COMPLETE DijkstraBlockBody #-}

--------------------------------------------------------------------------------
-- Serialisation and hashing
--------------------------------------------------------------------------------

instance
  ( AlonzoEraTx era
  , EncCBOR (Tx TopTx era)
  ) =>
  EncCBOR (DijkstraBlockBodyRaw era)
  where
  encCBOR (DijkstraBlockBodyRaw txs perasCert) =
    encodeListLen 3
      <> encCBOR invalidIndices
      <> encCBOR txs
      <> encodeNullStrictMaybe encCBOR perasCert
    where
      invalidIndices =
        StrictSeq.findIndicesL (\tx -> tx ^. isValidTxL == IsValid False) txs

instance
  ( AlonzoEraTx era
  , DecCBOR (Annotator (TxAuxData era))
  , DecCBOR (Annotator (TxBody TopTx era))
  , DecCBOR (Annotator (TxWits era))
  ) =>
  DecCBOR (Annotator (DijkstraBlockBodyRaw era))
  where
  decCBOR = decodeRecordNamed "DijkstraBlockBodyRaw" (const 3) $ do
    invalidTxs :: [Word16] <- decCBOR
    txs <- decCBOR
    perasCert <- decodeNullStrictMaybe decCBOR
    let txsLength = Seq.length txs
        inRange x = 0 <= x && x < txsLength
    forM_ invalidTxs $ \i ->
      unless (inRange $ fromIntegral @Word16 @Int i) . fail $
        "index is out of range: " <> show i
    let
      setValidityFlag tx isValid = set isValidTxL isValid <$> tx
      validityFlags = alignedValidFlags txsLength (fromIntegral <$> invalidTxs)
      txsWithIsValid = Seq.zipWith setValidityFlag txs validityFlags
    pure $
      DijkstraBlockBodyRaw
        <$> sequenceA (StrictSeq.forceToStrict txsWithIsValid)
        <*> pure perasCert

deriving via
  Mem (DijkstraBlockBodyRaw era)
  instance
    ( AlonzoEraTx era
    , DecCBOR (Annotator (TxAuxData era))
    , DecCBOR (Annotator (TxBody TopTx era))
    , DecCBOR (Annotator (TxWits era))
    ) =>
    DecCBOR (Annotator (DijkstraBlockBody era))

--------------------------------------------------------------------------------
-- Internal utility functions
--------------------------------------------------------------------------------

-- | Given the number of transactions, and the set of indices for which these
-- transactions do not validate, create an aligned sequence of `IsValid`
-- flags.
alignedValidFlags :: Int -> [Int] -> Seq.Seq IsValid
alignedValidFlags n idxs =
  Seq.fromFunction n $ \i ->
    IsValid (i `IntSet.notMember` invalidSet)
  where
    invalidSet = IntSet.fromList idxs

-- | Placeholder for Peras certificates
--
-- NOTE: The real type will be brought from 'cardano-base' once it's ready.
data PerasCert = PerasCert
  deriving (Eq, Show, Generic, NoThunks)

instance NFData PerasCert

instance EncCBOR PerasCert where
  encCBOR PerasCert = encCBOR BS.empty

instance DecCBOR PerasCert where
  decCBOR = do
    (_ :: BS.ByteString) <- decCBOR
    pure PerasCert

-- | Placeholder for Peras public keys
--
-- NOTE: The real type will be brought from 'cardano-base' once it's ready.
data PerasKey = PerasKey
  deriving (Eq, Show, Generic, NoThunks)

-- | Mocked-up Peras certificate validation routine
--
-- NOTE: this function will be replaced with the real implementation from
-- 'cardano-base' once it's ready.
validatePerasCert :: Nonce -> PerasKey -> PerasCert -> Bool
validatePerasCert _ _ _ = True
