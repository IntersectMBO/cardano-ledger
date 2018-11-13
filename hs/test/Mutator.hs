module Mutator
    (
      mutateId
    , mutateNat
    , mutateNat'
    , mutateCoin
    , mutateCoin'
    ) where

import Numeric.Natural

import Hedgehog
import qualified Hedgehog.Gen    as Gen

import Generator

import Coin

mutateId :: a -> Gen a
mutateId = pure

mutateNatRange :: Natural -> Natural -> Natural -> Gen Natural
mutateNatRange lower upper _ = genNatural lower upper

mutateNatSmall :: Natural -> Gen Natural
mutateNatSmall n = do
  b <- genBool
  pure $ if b then n + 1 else (if n > 0 then n - 1 else 0)

mutateNat' :: Natural -> Natural -> Natural -> Gen Natural
mutateNat' n lower upper = do
  n' <- Gen.choice [mutateNatRange lower upper n, mutateNatSmall n]
  if n == n' then Gen.discard else pure n'

mutateNat :: Natural -> Natural -> Natural -> Gen Natural
mutateNat lower upper n =
  Gen.choice [mutateId n, mutateNatRange lower upper n, mutateNatSmall n]

mutateCoin :: Natural -> Natural -> Coin -> Gen Coin
mutateCoin lower upper (Coin val) = Coin <$> mutateNat lower upper val

mutateCoin' :: Natural -> Natural -> Coin -> Gen Coin
mutateCoin' lower upper (Coin val) = Coin <$> mutateNat' lower upper val
