{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Test.Cardano.Ledger.Conformance.Orphans.Core where

import Cardano.Ledger.Hashes (standardAddrHashSize)
import Data.Default (Default)
import MAlonzo.Code.Ledger.Core.Foreign.API as Agda
import Test.Cardano.Ledger.Common (NFData, ToExpr)
import Test.Cardano.Ledger.Conformance.Utils
import Test.Cardano.Ledger.Conway.TreeDiff (Expr (..), ToExpr (..))

deriving instance Ord Credential

deriving instance Ord RewardAddress

instance (NFData k, NFData v) => NFData (HSMap k v)

instance NFData a => NFData (HSSet a)

instance NFData RewardAddress

instance NFData BaseAddr

instance NFData BootstrapAddr

instance NFData Credential

instance NFData HSVKey

instance ToExpr a => ToExpr (HSSet a)

instance ToExpr Credential where
  toExpr (KeyHashObj h) =
    App
      "KeyHashObj"
      [ agdaHashToExpr standardAddrHashSize h
      , toExpr h
      ]
  toExpr (ScriptObj h) =
    App
      "ScriptObj"
      [ agdaHashToExpr standardAddrHashSize h
      , toExpr h
      ]

instance (ToExpr k, ToExpr v) => ToExpr (HSMap k v)

instance ToExpr RewardAddress

instance ToExpr BaseAddr

instance ToExpr BootstrapAddr

instance ToExpr HSVKey

instance Default (HSMap k v)

deriving instance Semigroup (HSMap k v)

deriving instance Monoid (HSMap k v)
