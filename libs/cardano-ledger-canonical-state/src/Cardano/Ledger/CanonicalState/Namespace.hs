{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE TypeFamilies #-}

module Cardano.Ledger.CanonicalState.Namespace (NamespaceEra, Era) where

import Cardano.Ledger.Core (Era)
import Data.Kind (Type)
import GHC.TypeLits (Symbol)

type family NamespaceEra (era :: Symbol) :: Type
