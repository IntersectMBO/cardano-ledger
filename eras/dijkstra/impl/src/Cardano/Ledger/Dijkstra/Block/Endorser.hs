{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilyDependencies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE UndecidableSuperClasses #-}

-- | Endorser blocks and references
module Cardano.Ledger.Dijkstra.Block.Endorser (
  EbReferences,
  mkEbReferences,
  getEbReferences,
  EbTransactions (..),
  DijkstraEndorserBlock (..),
  EraEndorserBlock (..),
) where

import Cardano.Ledger.Binary (Annotator, DecCBOR (..), EncCBOR, ToCBOR, Version)
import Cardano.Ledger.Core (EraTx, TopTx, Tx)
import Cardano.Ledger.Dijkstra.Era (DijkstraEra)
import Cardano.Ledger.Dijkstra.Tx ()
import Cardano.Ledger.Hashes (
  EraIndependentEbReferences,
  EraIndependentTx,
  HashAnnotated,
  SafeHash,
  SafeToHash,
  hashAnnotated,
 )
import Cardano.Ledger.MemoBytes (
  EqRaw,
  Mem,
  MemoBytes,
  MemoHashIndex,
  Memoized (..),
  decodeMemoized,
  getMemoRawType,
  getMemoSafeHash,
  mkMemoized,
 )
import Control.DeepSeq (NFData)
import Control.Monad (when)
import Data.Kind (Type)
import Data.Sequence.Strict (StrictSeq)
import Data.SimpleOMap.Strict (OMap)
import Data.Typeable (Typeable)
import Data.Word (Word32)
import GHC.Generics (Generic)
import Lens.Micro (Lens', lens)
import NoThunks.Class (NoThunks)

newtype EbReferencesRaw = EbReferencesRaw
  { unEbReferencesRaw :: OMap (SafeHash EraIndependentTx) Word32
  }
  deriving newtype (Eq, Show, NFData, NoThunks, EncCBOR)

instance DecCBOR EbReferencesRaw where
  decCBOR = do
    refs <- decCBOR
    EbReferencesRaw refs <$ when (null refs) (fail "Empty EbReferences are not allowed")

instance DecCBOR (Annotator EbReferencesRaw) where
  decCBOR = pure <$> decCBOR

type instance MemoHashIndex EbReferencesRaw = EraIndependentEbReferences

newtype EbReferences = MkEbReferences (MemoBytes EbReferencesRaw)
  deriving stock (Generic)
  deriving newtype (Eq, Show, NFData, NoThunks, EncCBOR, ToCBOR, SafeToHash)

instance Memoized EbReferences where
  type RawType EbReferences = EbReferencesRaw

instance DecCBOR EbReferences where
  decCBOR = MkEbReferences <$> decodeMemoized decCBOR

deriving via Mem EbReferencesRaw instance DecCBOR (Annotator EbReferences)

instance HashAnnotated EbReferences EraIndependentEbReferences where
  hashAnnotated = getMemoSafeHash

instance EqRaw EbReferences

mkEbReferences :: Version -> OMap (SafeHash EraIndependentTx) Word32 -> EbReferences
mkEbReferences v = mkMemoized v . EbReferencesRaw

getEbReferences :: EbReferences -> OMap (SafeHash EraIndependentTx) Word32
getEbReferences = unEbReferencesRaw . getMemoRawType

newtype EbTransactions era = EbTransactions
  { unEbTransactions :: StrictSeq (Tx TopTx era)
  }
  deriving stock (Generic)

deriving stock instance Eq (Tx TopTx era) => Eq (EbTransactions era)

deriving stock instance Show (Tx TopTx era) => Show (EbTransactions era)

deriving newtype instance NFData (Tx TopTx era) => NFData (EbTransactions era)

deriving newtype instance NoThunks (Tx TopTx era) => NoThunks (EbTransactions era)

data DijkstraEndorserBlock era = DijkstraEndorserBlock
  { debReferences :: !EbReferences
  , debTransactions :: !(EbTransactions era)
  }
  deriving stock (Generic)

deriving stock instance Eq (Tx TopTx era) => Eq (DijkstraEndorserBlock era)

deriving stock instance Show (Tx TopTx era) => Show (DijkstraEndorserBlock era)

deriving anyclass instance NFData (Tx TopTx era) => NFData (DijkstraEndorserBlock era)

deriving anyclass instance
  (Typeable era, NoThunks (Tx TopTx era)) => NoThunks (DijkstraEndorserBlock era)

class EraTx era => EraEndorserBlock era where
  type EndorserBlock era = (r :: Type) | r -> era

  referencesEndorserBlockL :: Lens' (EndorserBlock era) EbReferences
  transactionsEndorserBlockL :: Lens' (EndorserBlock era) (EbTransactions era)

instance EraEndorserBlock DijkstraEra where
  type EndorserBlock DijkstraEra = DijkstraEndorserBlock DijkstraEra
  referencesEndorserBlockL = lens debReferences $ \b r -> b {debReferences = r}
  transactionsEndorserBlockL = lens debTransactions $ \b t -> b {debTransactions = t}
