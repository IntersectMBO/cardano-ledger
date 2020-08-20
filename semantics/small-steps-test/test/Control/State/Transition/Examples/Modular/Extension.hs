{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralisedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ViewPatterns #-}

module Control.State.Transition.Examples.Modular.Extension where

import Control.State.Transition
import qualified Control.State.Transition.Examples.Modular.Base as Base
import qualified Control.State.Transition.Examples.Modular.Base.Types as BT
import Data.Group (Group, invert)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe (isJust)
import Data.Monoid (Sum (..))
import Optics

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
  deriving newtype (Eq, Ord, Show)

-------------------------------------------------------------------------------
-- Era instances
-------------------------------------------------------------------------------

type instance BT.Coin Era_1 = Coin
type instance BT.AccountName Era_1 = Base.AccountName
