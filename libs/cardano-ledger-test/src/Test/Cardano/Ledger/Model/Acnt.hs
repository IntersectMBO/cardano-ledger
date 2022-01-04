{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeSynonymInstances #-}

module Test.Cardano.Ledger.Model.Acnt where

import Cardano.Ledger.Coin (Coin)
import Cardano.Ledger.Shelley.TxBody (MIRPot (..))
import Control.Applicative (liftA2)
import Control.DeepSeq (NFData)
import Control.Lens
  ( Lens',
    view,
  )
import Control.Monad.Reader.Class (MonadReader (..))
import Data.Group (Abelian, Group (..))
import GHC.Generics (Generic)
import Test.Cardano.Ledger.Model.BaseTypes
  ( PreservedAda (..),
  )

-- | fig 35
-- TODO: swap out for LedgerState.AccountState
data ModelAcntF a = ModelAcnt
  { _modelAcnt_treasury :: !a,
    _modelAcnt_reserves :: !a
  }
  deriving (Eq, Show, Generic, Functor, Foldable, Traversable)

instance Applicative ModelAcntF where
  pure x = ModelAcnt x x
  ModelAcnt f g <*> ModelAcnt x y = ModelAcnt (f x) (g y)

instance Monad ModelAcntF where
  ModelAcnt t r >>= k = ModelAcnt (_modelAcnt_treasury $ k t) (_modelAcnt_reserves $ k r)

instance MonadReader MIRPot ModelAcntF where
  ask = ModelAcnt TreasuryMIR ReservesMIR
  local f xs = ModelAcnt (g TreasuryMIR) (g ReservesMIR)
    where
      g = lookupModelAcnt xs . f
  reader f = ModelAcnt (f TreasuryMIR) (f ReservesMIR)

-- ModelAcntF is representable, with representing object MIRPot
lookupModelAcnt :: ModelAcntF a -> MIRPot -> a
lookupModelAcnt = flip $ view . modelAcntPot
{-# INLINE lookupModelAcnt #-}

type ModelAcnt = ModelAcntF Coin

instance NFData a => NFData (ModelAcntF a)

instance Semigroup w => Semigroup (ModelAcntF w) where
  (<>) = liftA2 (<>)

instance Monoid w => Monoid (ModelAcntF w) where
  mempty = pure mempty

instance Group g => Group (ModelAcntF g) where
  invert = fmap invert
  (~~) = liftA2 (~~)
  pow xs n = fmap (`pow` n) xs

instance Abelian g => Abelian (ModelAcntF g)

modelAcnt_treasury :: Lens' (ModelAcntF a) a
modelAcnt_treasury a2fb s = (\b -> s {_modelAcnt_treasury = b}) <$> a2fb (_modelAcnt_treasury s)
{-# INLINE modelAcnt_treasury #-}

modelAcnt_reserves :: Lens' (ModelAcntF a) a
modelAcnt_reserves a2fb s = (\b -> s {_modelAcnt_reserves = b}) <$> a2fb (_modelAcnt_reserves s)
{-# INLINE modelAcnt_reserves #-}

modelAcntPot :: MIRPot -> Lens' (ModelAcntF a) a
modelAcntPot = \case
  TreasuryMIR -> modelAcnt_treasury
  ReservesMIR -> modelAcnt_reserves
{-# INLINE modelAcntPot #-}

instance PreservedAda ModelAcnt where
  totalPreservedAda = _modelAcnt_treasury <> _modelAcnt_reserves
