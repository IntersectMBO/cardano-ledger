{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilyDependencies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE UndecidableSuperClasses #-}
{-# LANGUAGE ViewPatterns #-}

-- | Endorser blocks and references
module Cardano.Ledger.Dijkstra.Block.Endorser (
  DijkstraEbReferences (MkDijkstraEbReferences),
  EbTransactions (..),
  DijkstraEndorserBlock (..),
  EraEndorserBlock (..),
  pattern EbReferences,
) where

import Cardano.Ledger.Binary (Annotator, DecCBOR (..), EncCBOR, ToCBOR)
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
  getMemoRawType,
  getMemoSafeHash,
  mkMemoizedEra,
 )
import Control.DeepSeq (NFData)
import Control.Monad (when)
import Data.Kind (Type)
import Data.Sequence.Strict (StrictSeq)
import Data.SimpleOMap.Strict (OMap)
import Data.Typeable (Typeable)
import Data.Word (Word32)
import GHC.Generics (Generic)
import Lens.Micro (Lens', SimpleGetter, lens, to, (^.))
import NoThunks.Class (NoThunks)

newtype DijkstraEbReferencesRaw era = DijkstraEbReferencesRaw
  { unDijkstraEbReferencesRaw :: OMap (SafeHash EraIndependentTx) Word32
  }
  deriving newtype (Eq, Show, NFData, NoThunks, EncCBOR)

instance Typeable era => DecCBOR (DijkstraEbReferencesRaw era) where
  decCBOR = do
    refs <- decCBOR
    DijkstraEbReferencesRaw refs <$ when (null refs) (fail "Empty EbReferences are not allowed")

instance Typeable era => DecCBOR (Annotator (DijkstraEbReferencesRaw era)) where
  decCBOR = pure <$> decCBOR

type instance MemoHashIndex (DijkstraEbReferencesRaw era) = EraIndependentEbReferences

newtype DijkstraEbReferences era = MkDijkstraEbReferences (MemoBytes (DijkstraEbReferencesRaw era))
  deriving stock (Generic)
  deriving newtype (Eq, Show, NFData, EncCBOR, SafeToHash)

deriving newtype instance Typeable era => NoThunks (DijkstraEbReferences era)

deriving newtype instance Typeable era => ToCBOR (DijkstraEbReferences era)

instance Memoized (DijkstraEbReferences era) where
  type RawType (DijkstraEbReferences era) = DijkstraEbReferencesRaw era

deriving via
  Mem (DijkstraEbReferencesRaw era)
  instance
    Typeable era => DecCBOR (Annotator (DijkstraEbReferences era))

instance HashAnnotated (DijkstraEbReferences era) EraIndependentEbReferences where
  hashAnnotated = getMemoSafeHash

instance EqRaw (DijkstraEbReferences era)

newtype EbTransactions era = EbTransactions
  { unEbTransactions :: StrictSeq (Tx TopTx era)
  }
  deriving stock (Generic)

deriving stock instance Eq (Tx TopTx era) => Eq (EbTransactions era)

deriving stock instance Show (Tx TopTx era) => Show (EbTransactions era)

deriving newtype instance NFData (Tx TopTx era) => NFData (EbTransactions era)

deriving newtype instance NoThunks (Tx TopTx era) => NoThunks (EbTransactions era)

data DijkstraEndorserBlock era = DijkstraEndorserBlock
  { debReferences :: !(EbReferences era)
  , debTransactions :: !(EbTransactions era)
  }
  deriving stock (Generic)

deriving stock instance
  (Eq (EbReferences era), Eq (Tx TopTx era)) => Eq (DijkstraEndorserBlock era)

deriving stock instance
  (Show (EbReferences era), Show (Tx TopTx era)) => Show (DijkstraEndorserBlock era)

deriving anyclass instance
  (NFData (EbReferences era), NFData (Tx TopTx era)) => NFData (DijkstraEndorserBlock era)

deriving anyclass instance
  (Typeable era, NoThunks (EbReferences era), NoThunks (Tx TopTx era)) =>
  NoThunks (DijkstraEndorserBlock era)

class
  ( EraTx era
  , Eq (EbReferences era)
  , Show (EbReferences era)
  , NFData (EbReferences era)
  , NoThunks (EbReferences era)
  , EqRaw (EbReferences era)
  , EncCBOR (EbReferences era)
  , ToCBOR (EbReferences era)
  , DecCBOR (Annotator (EbReferences era))
  , HashAnnotated (EbReferences era) EraIndependentEbReferences
  ) =>
  EraEndorserBlock era
  where
  type EbReferences era = (r :: Type) | r -> era
  type EndorserBlock era = (r :: Type) | r -> era

  mkBasicEndorserBlock :: EndorserBlock era
  mkEbReferences :: OMap (SafeHash EraIndependentTx) Word32 -> EbReferences era
  txRefsEbReferencesG :: SimpleGetter (EbReferences era) (OMap (SafeHash EraIndependentTx) Word32)

  referencesEndorserBlockL :: Lens' (EndorserBlock era) (EbReferences era)
  transactionsEndorserBlockL :: Lens' (EndorserBlock era) (EbTransactions era)

pattern EbReferences ::
  EraEndorserBlock era => OMap (SafeHash EraIndependentTx) Word32 -> EbReferences era
pattern EbReferences refs <- ((^. txRefsEbReferencesG) -> refs)
  where
    EbReferences refs = mkEbReferences refs

{-# COMPLETE EbReferences #-}

mkBasicEndorserBlockDijkstra :: EraEndorserBlock era => DijkstraEndorserBlock era
mkBasicEndorserBlockDijkstra =
  DijkstraEndorserBlock (mkEbReferences mempty) $ EbTransactions mempty

instance EraEndorserBlock DijkstraEra where
  type EbReferences DijkstraEra = DijkstraEbReferences DijkstraEra
  type EndorserBlock DijkstraEra = DijkstraEndorserBlock DijkstraEra
  mkBasicEndorserBlock = mkBasicEndorserBlockDijkstra
  mkEbReferences = mkMemoizedEra @DijkstraEra . DijkstraEbReferencesRaw
  txRefsEbReferencesG = to (unDijkstraEbReferencesRaw . getMemoRawType)
  referencesEndorserBlockL = lens debReferences $ \b r -> b {debReferences = r}
  transactionsEndorserBlockL = lens debTransactions $ \b t -> b {debTransactions = t}
