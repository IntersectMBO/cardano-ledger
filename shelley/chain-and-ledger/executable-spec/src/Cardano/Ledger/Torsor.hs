{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}

module Cardano.Ledger.Torsor where

import Data.Kind (Type)
import NoThunks.Class (NoThunks)

class (Eq (Delta a), Show (Delta a), NoThunks (Delta a)) => Torsor a where
  type Delta a :: Type
  addDelta :: a -> Delta a -> a
  toDelta :: a -> Delta a
