-- | MemoBytes is an abstraction for a data type that encodes its own serialization.
--   The idea is to use a newtype around a MemoBytes applied to a non-memoizing type.
--   For example:   newtype Foo = Foo (`MemoBytes` NonMemoizingFoo)
--   This way all the instances for @Foo (`Eq`, `Show`, `EncCBOR`, `DecCBOR`, `NoThunks`, Generic`)@
--   can be derived for free. MemoBytes plays an important role in the 'SafeToHash' class
--   introduced in the module 'Cardano.Ledger.SafeHash'
module Cardano.Ledger.MemoBytes (
  MemoBytes (Memo),
  MemoHashIndex,
  Mem,
  mkMemoBytes,
  getMemoBytesType,
  getMemoBytesHash,
  memoBytes,
  shorten,
  showMemo,
  printMemo,
  shortToLazy,
  contentsEq,

  -- * Memoized
  Memoized (RawType),
  mkMemoized,
  getMemoSafeHash,
  getMemoRawType,
  zipMemoRawType,
  eqRawType,
  getMemoRawBytes,
  lensMemoRawType,
  getterMemoRawType,

  -- * Raw equality
  EqRaw (..),
)
where

import Cardano.Ledger.MemoBytes.Internal
