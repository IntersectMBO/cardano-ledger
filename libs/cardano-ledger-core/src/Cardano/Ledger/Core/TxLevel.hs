{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeData #-}
{-# LANGUAGE TypeFamilyDependencies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableSuperClasses #-}

module Cardano.Ledger.Core.TxLevel (
  TxLevel (..),
  STxTopLevel (..),
  withSTxTopLevelM,
  STxBothLevels (..),
  withSTxBothLevels,
  EraTxLevel (..),
  HasEraTxLevel (..),
  asSTxTopLevel,
  mkSTxTopLevelM,
  withTopTxLevelOnly,
  asSTxBothLevels,
  mkSTxBothLevelsM,
  withBothTxLevels,
) where

import Cardano.Ledger.Core.Era (Era (..))
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Fail.String
import Data.Functor.Identity (Identity (..), runIdentity)
import Data.Kind (Type)
import Data.Typeable
import GHC.Stack

type data TxLevel = TopTx | SubTx

data STxTopLevel (l :: TxLevel) era where
  STopTxOnly :: STxTopLevel TopTx era

withSTxTopLevelM ::
  forall l era a m. (Typeable l, Era era, MonadFail m) => (STxTopLevel l era -> m a) -> m a
withSTxTopLevelM f =
  case eqT @l @TopTx of
    Just Refl -> f STopTxOnly
    Nothing -> fail $ "SubTx level is not supported in the " <> eraName @era <> " era"

data STxBothLevels (l :: TxLevel) era where
  STopTx :: STxBothLevels TopTx era
  SSubTx :: STxBothLevels SubTx era

withSTxBothLevels :: forall l era a. (Typeable l, HasCallStack) => (STxBothLevels l era -> a) -> a
withSTxBothLevels f =
  case eqT @l @TopTx of
    Just Refl -> f STopTx
    Nothing -> case eqT @l @SubTx of
      Just Refl -> f SSubTx
      Nothing -> error $ "Impossible: Unrecognized TxLevel: " <> show (typeRep (Proxy @l))

class Era era => EraTxLevel era where
  -- | Supported transaction level as a singleton. One of these two should be used:
  --
  -- * `STxTopLevel` - for eras up to and including Conway, that do not support nested transactions.
  -- * `STxBothLevels` - for Dijkstra onwards that do support nested transactions.
  type STxLevel (l :: TxLevel) era = (r :: Type) | r -> era

  type STxLevel l era = STxBothLevels l era

-- | Type class for data families that have different definition depending on the level. Currently
-- it is only `Cardano.Ledger.Core.Tx` and `Cardano.Ledger.Core.TxBody` that have this distinction.
class EraTxLevel era => HasEraTxLevel (t :: TxLevel -> Type -> Type) era where
  toSTxLevel :: t l era -> STxLevel l era

mkSTxTopLevelM ::
  forall (l :: TxLevel) t m era.
  (Typeable l, Monad m, HasEraTxLevel t era, STxLevel l era ~ STxTopLevel l era) =>
  m (t TopTx era) -> m (t l era)
mkSTxTopLevelM mkTopTx = do
  fmap (either error id) $ runFailT $ withSTxTopLevelM @l @era $ \level ->
    case level of
      STopTxOnly -> do
        res <- lift mkTopTx
        -- Here we tell the compiler that we only expect top level transactions in this function and
        -- any attempt to construct a sub transaction level will result in a compiler error,
        -- instead of a trigger of `fail` in `MonadFail`, as `withSTxTopLevelM` would normally do.
        let _level = asTypeOf (toSTxLevel res) level
        pure res

asSTxTopLevel ::
  forall (l :: TxLevel) t era.
  (Typeable l, HasEraTxLevel t era, STxLevel l era ~ STxTopLevel l era) =>
  t TopTx era -> t l era
asSTxTopLevel = runIdentity . mkSTxTopLevelM . pure

withTopTxLevelOnly ::
  (HasEraTxLevel t era, STxLevel l era ~ STxTopLevel l era) =>
  t l era -> (t TopTx era -> a) -> a
withTopTxLevelOnly t f =
  case toSTxLevel t of
    STopTxOnly -> f t

mkSTxBothLevelsM ::
  forall (l :: TxLevel) t m era.
  (Typeable l, Monad m, HasEraTxLevel t era, STxLevel l era ~ STxBothLevels l era) =>
  m (t TopTx era) -> m (t SubTx era) -> m (t l era)
mkSTxBothLevelsM mkTopTx mkSubTx =
  withSTxBothLevels @l $ \level -> do
    res <- case level of
      STopTx -> mkTopTx
      SSubTx -> mkSubTx
    -- Tell the compiler that we expect only `STxBothLevels` in this action
    let _level = asTypeOf (toSTxLevel res) level
    pure res

asSTxBothLevels ::
  forall (l :: TxLevel) t era.
  (Typeable l, HasEraTxLevel t era, STxLevel l era ~ STxBothLevels l era) =>
  t TopTx era -> t SubTx era -> t l era
asSTxBothLevels mkTopTx mkSubTx = runIdentity $ mkSTxBothLevelsM (pure mkTopTx) (pure mkSubTx)

withBothTxLevels ::
  (HasEraTxLevel t era, STxLevel l era ~ STxBothLevels l era) =>
  t l era -> (t TopTx era -> a) -> (t SubTx era -> a) -> a
withBothTxLevels t fTop fSub =
  case toSTxLevel t of
    STopTx -> fTop t
    SSubTx -> fSub t
