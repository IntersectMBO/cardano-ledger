{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
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
  mkBasicBlockBodyDijkstra,
  DijkstraEraBlockBody (..),
  PerasCert (..),
  PerasKey (..),
  validatePerasCert,
) where

import Cardano.Crypto.Leios (LeiosCert)
import Cardano.Ledger.Alonzo.Tx (AlonzoEraTx (..))
import Cardano.Ledger.BaseTypes (Nonce, ProtVer (..))
import Cardano.Ledger.Binary (
  Annotator (..),
  DecCBOR (..),
  EncCBOR,
  decodeNullStrictMaybe,
  decodeRecordNamed,
  decodeSeq,
  encCBOR,
  encodeFoldableEncoder,
  encodeListLen,
  encodeNullStrictMaybe,
  serialize',
 )
import Cardano.Ledger.Core
import Cardano.Ledger.Dijkstra.Era
import Cardano.Ledger.Dijkstra.Tx (
  DijkstraTx,
  Tx (..),
  decodeDijkstraTopTxInBlock,
  toCBORForBlockInclusion,
 )
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
import Cardano.Ledger.Orphans ()
import Control.DeepSeq (NFData)
import Data.Array.Byte (ByteArray)
import qualified Data.ByteString as BS
import Data.Coerce (Coercible, coerce)
import Data.Maybe.Strict (StrictMaybe (..))
import Data.Sequence.Strict (StrictSeq)
import qualified Data.Sequence.Strict as StrictSeq
import Data.Typeable (Typeable)
import GHC.Generics (Generic)
import Lens.Micro
import NoThunks.Class (NoThunks)

-- =================================================

-- $BlockBody
--
-- * BlockBody
--
-- Unlike in the previous eras, transactions in a Dijkstra block body are not
-- deconstructed into segregated components. Each transaction is serialized
-- whole, as @[transaction_body, transaction_witness_set, auxiliary_data\/ nil,
-- is_valid]@, with the block-producer-supplied `IsPhase2Valid` flag as the
-- trailing element.

data DijkstraBlockBodyRaw era = DijkstraBlockBodyRaw
  { dbbrTxs :: !(StrictSeq (Tx TopTx era))
  , dbbrLeiosCert :: !(StrictMaybe LeiosCert)
  -- ^ Optional Leios certificate
  , dbbrPerasCert :: !(StrictMaybe PerasCert)
  -- ^ Optional Peras certificate
  }
  deriving (Generic)

instance
  (NFData (Tx TopTx era), NFData LeiosCert, NFData PerasCert) =>
  NFData (DijkstraBlockBodyRaw era)

type instance MemoHashIndex (DijkstraBlockBodyRaw era) = EraIndependentBlockBody

instance EraBlockBody DijkstraEra where
  type BlockBody DijkstraEra = DijkstraBlockBody DijkstraEra
  mkBasicBlockBody = mkBasicBlockBodyDijkstra
  txSeqBlockBodyL = lensMemoRawType @DijkstraEra dbbrTxs (\bb p -> bb {dbbrTxs = p})
  hashBlockBody (MkDijkstraBlockBody m) = extractHash $ getMemoBytesHash m
  blockBodySize (ProtVer v _) = BS.length . serialize' v . encCBOR

mkBasicBlockBodyDijkstra :: forall era. AlonzoEraTx era => DijkstraBlockBody era
mkBasicBlockBodyDijkstra =
  mkMemoized (eraProtVerLow @era) $
    DijkstraBlockBodyRaw mempty SNothing SNothing
{-# INLINEABLE mkBasicBlockBodyDijkstra #-}

-- | Dijkstra-specific extensions to 'EraBlockBody'
class EraBlockBody era => DijkstraEraBlockBody era where
  leiosCertBlockBodyL :: Lens' (BlockBody era) (StrictMaybe LeiosCert)
  -- ^ Lens to access the optional Leios certificate in the block body

  perasCertBlockBodyL :: Lens' (BlockBody era) (StrictMaybe PerasCert)
  -- ^ Lens to access the optional Peras certificate in the block body

instance DijkstraEraBlockBody DijkstraEra where
  leiosCertBlockBodyL = lensMemoRawType @DijkstraEra dbbrLeiosCert (\bb c -> bb {dbbrLeiosCert = c})

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
  StrictMaybe LeiosCert ->
  StrictMaybe PerasCert ->
  DijkstraBlockBody era
pattern DijkstraBlockBody txs mbLeiosCert mbPerasCert <-
  ( getMemoRawType ->
      DijkstraBlockBodyRaw txs mbLeiosCert mbPerasCert
    )
  where
    DijkstraBlockBody txs leiosCert perasCert =
      mkMemoizedEra @DijkstraEra $
        DijkstraBlockBodyRaw txs leiosCert perasCert

{-# COMPLETE DijkstraBlockBody #-}

--------------------------------------------------------------------------------
-- Serialisation and hashing
--------------------------------------------------------------------------------

instance AlonzoEraTx era => EncCBOR (DijkstraBlockBodyRaw era) where
  encCBOR (DijkstraBlockBodyRaw txs mbLeiosCert mbPerasCert) =
    encodeListLen 3
      <> encodeFoldableEncoder toCBORForBlockInclusion txs
      <> encodeNullStrictMaybe encCBOR mbLeiosCert
      <> encodeNullStrictMaybe encCBOR mbPerasCert

instance
  ( AlonzoEraTx era
  , DecCBOR (Annotator (TxAuxData era))
  , DecCBOR (Annotator (TxBody TopTx era))
  , DecCBOR (Annotator (TxWits era))
  , Coercible (DijkstraTx TopTx era) (Tx TopTx era)
  ) =>
  DecCBOR (Annotator (DijkstraBlockBodyRaw era))
  where
  decCBOR = decodeRecordNamed "DijkstraBlockBodyRaw" (const 3) $ do
    txs <- decodeSeq (decodeDijkstraTopTxInBlock @era)
    mbLeiosCert <- decodeNullStrictMaybe decCBOR
    mbPerasCert <- decodeNullStrictMaybe decCBOR
    pure $
      DijkstraBlockBodyRaw
        <$> sequenceA (StrictSeq.forceToStrict (coerce txs))
        <*> pure mbLeiosCert
        <*> pure mbPerasCert

deriving via
  Mem (DijkstraBlockBodyRaw era)
  instance
    ( AlonzoEraTx era
    , Coercible (DijkstraTx TopTx era) (Tx TopTx era)
    , DecCBOR (Annotator (TxAuxData era))
    , DecCBOR (Annotator (TxBody TopTx era))
    , DecCBOR (Annotator (TxWits era))
    ) =>
    DecCBOR (Annotator (DijkstraBlockBody era))

-- | Placeholder for Peras certificates
--
-- NOTE: The real type will be brought from 'cardano-base' once it's ready.
newtype PerasCert = PerasCert ByteArray
  deriving (Eq, Ord, Show, Generic)
  deriving newtype (EncCBOR, DecCBOR)

instance NoThunks PerasCert

instance NFData PerasCert

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
