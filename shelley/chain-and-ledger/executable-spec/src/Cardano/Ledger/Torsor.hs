{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}

module Cardano.Ledger.Torsor where

import Data.Kind (Type)

class Torsor a where
  type Delta a :: Type
  addDelta :: a -> Delta a -> a
  toDelta :: a -> Delta a
