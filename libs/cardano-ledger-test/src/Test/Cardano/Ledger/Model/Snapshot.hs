{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveTraversable #-}

module Test.Cardano.Ledger.Model.Snapshot where

import Control.Applicative (liftA2)
import Control.DeepSeq (NFData)
import Control.Lens
import GHC.Generics (Generic)

data SnapshotQueue a = SnapshotQueue
  { _snapshotQueue_mark :: a,
    _snapshotQueue_set :: a,
    _snapshotQueue_go :: a
  }
  deriving (Eq, Ord, Show, Functor, Foldable, Traversable, Generic)

instance NFData a => NFData (SnapshotQueue a)

snapshotQueue_mark :: Lens' (SnapshotQueue a) a
snapshotQueue_mark = lens _snapshotQueue_mark (\s b -> s {_snapshotQueue_mark = b})
{-# INLINE snapshotQueue_mark #-}

snapshotQueue_set :: Lens' (SnapshotQueue a) a
snapshotQueue_set = lens _snapshotQueue_set (\s b -> s {_snapshotQueue_set = b})
{-# INLINE snapshotQueue_set #-}

snapshotQueue_go :: Lens' (SnapshotQueue a) a
snapshotQueue_go = lens _snapshotQueue_go (\s b -> s {_snapshotQueue_go = b})
{-# INLINE snapshotQueue_go #-}

shiftSnapshotQueue :: SnapshotQueue a -> (a, SnapshotQueue a)
shiftSnapshotQueue (SnapshotQueue markX setX goX) = (goX, SnapshotQueue markX markX setX)

instance Applicative SnapshotQueue where
  pure x = SnapshotQueue x x x
  SnapshotQueue markF setF goF <*> SnapshotQueue markX setX goX =
    SnapshotQueue (markF markX) (setF setX) (goF goX)

instance Monad SnapshotQueue where
  SnapshotQueue markX setX goX >>= cont =
    SnapshotQueue
      (_snapshotQueue_mark $ cont markX)
      (_snapshotQueue_set $ cont setX)
      (_snapshotQueue_go $ cont goX)

instance Semigroup a => Semigroup (SnapshotQueue a) where (<>) = liftA2 (<>)

instance Monoid a => Monoid (SnapshotQueue a) where mempty = pure mempty
