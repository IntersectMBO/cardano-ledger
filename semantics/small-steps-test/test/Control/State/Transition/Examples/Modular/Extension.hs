{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralisedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ViewPatterns #-}

module Control.State.Transition.Examples.Modular.Extension where

import Control.State.Transition
import qualified Control.State.Transition.Examples.Modular.Base as Base
import qualified Control.State.Transition.Examples.Modular.Base.Types as BT
import Data.Group (Group, invert)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Monoid (Sum (..))

data Era_1

-------------------------------------------------------------------------------
-- New types for the era
-------------------------------------------------------------------------------

newtype Quantity = Quantity Int
  deriving newtype (Eq, Ord, Show)
  deriving
    (Group, Semigroup, Monoid)
    via Sum Int

newtype Token = Token String
  deriving newtype (Eq, Ord, Show)

newtype Coin = Coin {unCoin :: Map Token Quantity}
  deriving newtype (Eq, Monoid, Ord, Semigroup, Show)

instance Group Coin where
  invert (Coin m) = Coin $! Map.map invert m

-------------------------------------------------------------------------------
-- Era instances
-------------------------------------------------------------------------------

type instance BT.Coin Era_1 = Coin

type instance BT.AccountName Era_1 = Base.AccountName

type instance BT.Account Era_1 = Base.Account Era_1

type instance BT.Tx Era_1 = Base.Tx Era_1

type instance BT.Admin Era_1 = Base.Admin Era_1

type instance BT.Op Era_1 = Base.Op Era_1

type instance BT.Accounting Era_1 = Base.Accounting Era_1

type instance BT.BlockId Era_1 = Base.BlockId

type instance BT.Block Era_1 = Base.Block Era_1

type instance BT.ChainState Era_1 = Base.ChainState Era_1

-------------------------------------------------------------------------------
-- STS Plumbing
-------------------------------------------------------------------------------

{-
newtype Clone a = Clone a

instance STS a => STS (Clone a) where
  type Environment (Clone a) = Environment a
  type State (Clone a) = State a
  type Signal (Clone a) = Signal a

  initialRules = initialRules @a
  transitionRules = transitionRules @a

deriving via (Clone (Base.ADMIN))
-}

data TX

deriving stock instance Eq (Base.TXPredicateFailure Era_1)

deriving stock instance Show (Base.TXPredicateFailure Era_1)

instance STS (TX) where
  type Environment (TX) = ()
  type State (TX) = BT.Accounting Era_1
  type Signal (TX) = BT.Tx Era_1

  type
    PredicateFailure TX =
      Base.TXPredicateFailure Era_1

  initialRules = []

  transitionRules = [processTx]

-- | This is the thing we actually want to override
processTx :: TransitionRule TX
processTx = undefined

data ADMIN

deriving stock instance Eq (Base.AdminPredicateFailure Era_1)

deriving stock instance Show (Base.AdminPredicateFailure Era_1)

instance STS ADMIN where
  type Environment (ADMIN) = ()
  type State (ADMIN) = (BT.Accounting Era_1)
  type Signal (ADMIN) = Base.Admin Era_1

  type PredicateFailure (ADMIN) = Base.AdminPredicateFailure Era_1

  initialRules = []
  transitionRules = [Base.processAdmin @ADMIN @Era_1]

data OPS

deriving stock instance Eq (Base.OPSPredicateFailure Era_1)

deriving stock instance Show (Base.OPSPredicateFailure Era_1)
instance STS OPS where
  type Environment (OPS) = ()
  type State (OPS) = (BT.Accounting Era_1)
  type Signal (OPS) = [BT.Op Era_1]

  type
    PredicateFailure OPS =
      Base.OPSPredicateFailure Era_1

  initialRules = []
  transitionRules =
    [Base.processOps @(OPS) @(TX) @ADMIN @Era_1]

instance Embed TX OPS where
  wrapFailed = Base.TXFailure

instance Embed (ADMIN) (OPS) where
  wrapFailed = Base.ADMINFailure
