{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Cardano.Ledger.Pretty.Generic 
  ( GPrettyA (..)
  , module Cardano.Ledger.Pretty.Core
  ) where

import Data.Text (Text, pack)
import GHC.Generics (type (:*:) (..), Meta (..), M1 (..), U1 (..), K1 (..), type (:+:) (..))
import GHC.TypeLits (KnownSymbol, symbolVal)
import Data.Data (Proxy(..))
import Cardano.Ledger.Pretty.Core

data GFields
  = NamedFields [(Text, PDoc)]
  | UnnamedFields [PDoc]

instance Semigroup GFields where
  NamedFields x <> NamedFields y = NamedFields $ x <> y
  UnnamedFields x <> UnnamedFields y = UnnamedFields $ x <> y
  _ <> _ = error "Encountered both named fields and unnamed fields in a constructor"

class GFieldsPrettyA t where
  genericFieldsPrettyA :: t -> GFields

instance
  ( GFieldsPrettyA (l p)
  , GFieldsPrettyA (r p)
  ) =>
  GFieldsPrettyA ((l :*: r) p)
  where
  genericFieldsPrettyA (l :*: r) = genericFieldsPrettyA l <> genericFieldsPrettyA r

instance
  GPrettyA (f p) =>
  GFieldsPrettyA (M1 i ('MetaSel 'Nothing su ss ds) f p)
  where
  genericFieldsPrettyA (M1 fp) = UnnamedFields [genericPrettyA fp]

instance
  ( GPrettyA (f p)
  , KnownSymbol n
  ) =>
  GFieldsPrettyA (M1 i ('MetaSel ('Just n) su ss ds) f p)
  where
  genericFieldsPrettyA (M1 fp) =
    NamedFields
      [(pack . symbolVal $ Proxy @n, genericPrettyA fp)]

instance GFieldsPrettyA (U1 r) where
  genericFieldsPrettyA U1 = UnnamedFields []

instance
  ( GPrettyA (f p)
  ) =>
  GPrettyA (M1 i ('MetaData n m p' nt) f p)
  where
  genericPrettyA (M1 fp) = genericPrettyA fp

instance
  ( KnownSymbol n
  , GFieldsPrettyA (f p)
  ) =>
  GPrettyA (M1 i ('MetaCons n fty s) f p)
  where
  genericPrettyA (M1 fp) = case genericFieldsPrettyA fp of
    NamedFields fields ->
      ppRecord
        (pack . symbolVal $ Proxy @n)
        fields
    UnnamedFields fields ->
      ppSexp
        (pack . symbolVal $ Proxy @n)
        fields

instance PrettyA c => GPrettyA (K1 i c p) where
  genericPrettyA (K1 v) = prettyA v

instance
  ( GPrettyA (l p)
  , GPrettyA (r p)
  ) =>
  GPrettyA ((l :+: r) p)
  where
  genericPrettyA (L1 x) = genericPrettyA x
  genericPrettyA (R1 x) = genericPrettyA x
