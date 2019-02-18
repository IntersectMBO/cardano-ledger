{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE TypeOperators #-}

-- | An approach to computing the abstract size of data using 'TypeRep'.
--
module Data.AbstractSize
  ( HasTypeReps
  , typeReps
  , abstractSize
  , AccountingMap
  , Size
  ) where

import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Sequence (Seq, (<|), (><), empty)
import Data.Typeable (TypeRep, Typeable, typeOf)
import Data.Word (Word64)
import GHC.Generics
  ( (:*:)((:*:))
  , (:+:)(L1, R1)
  , Generic
  , K1(K1)
  , M1(M1)
  , Rep
  , U1(U1)
  , from
  )
import GHC.Natural

-- | @abstractSize m a@ computes the abstract size of @a@, using the accounting
-- map @m@. The map @m@ determines the abstract size of each 'TypeRep'
-- contained in @a@, and this function simply adds all the individual abstract
-- sizes. To be able to extract the type representations ('TypeRep's) inside
-- @a@, we require it to be an instance of 'HasTypeReps'.
--
-- Examples:
--
-- >>> :set -XOverloadedLists
-- >>> abstractSize [(typeOf (undefined:: Char), 10)] 'a'
-- 10
--
-- >>> abstractSize [(typeOf 'x', 10)] "hello"
-- 50
--
-- >>> abstractSize [(typeOf 'x', 10), (typeOf True, 100)] ("hello", False)
-- 150
--
-- >>> abstractSize [(typeOf (undefined :: [Int]), 6), (typeOf (1 :: Int), 1)] ([0, 1, 2, 3] :: [Int])
-- 10
--
-- >>> abstractSize [(typeOf (undefined :: [Int]), 3), (typeOf (1 :: Int), -1)] ([0, 1, 2] :: [Int])
-- 0
--
abstractSize :: HasTypeReps a => AccountingMap -> a -> Size
abstractSize m a = sum $ fmap cost trs
  where
    trs = typeReps a
    cost t = Map.findWithDefault 0 t m

type Size = Int
type AccountingMap = Map TypeRep Size

--------------------------------------------------------------------------------
-- HasTypeReps class
--------------------------------------------------------------------------------

-- | The 'typeReps' function retrieves all the type representations found while
-- traversing the data given as parameter.
--
-- If you custom data type in an instance of 'Generics', a default
-- implementation is provided for you.
--
-- Examples:
--
-- >>> typeReps "a"
-- fromList [[Char],Char]
--
-- >>> typeReps "ab"
-- fromList [[Char],Char,Char]
--
-- >>> typeReps ([] :: [Int])
-- fromList [[Int]]
--
-- >>> :set -XDeriveGeneric
-- >>> data Foo = Foo [Int] (Char, Char) deriving (Generic)
-- >>> instance HasTypeReps Foo
-- >>> typeReps $ Foo [1, 2] ('a', 'b')
-- fromList [Foo,[Int],Int,Int,(Char,Char),Char,Char]
--
class Typeable a => HasTypeReps a where
  typeReps :: a -> Seq TypeRep

  default typeReps :: (Generic a, GHasTypeReps (Rep a)) => a -> Seq TypeRep
  typeReps a = typeOf a <| gTypeReps (from a)

class GHasTypeReps f where
  gTypeReps :: f a -> Seq TypeRep

--------------------------------------------------------------------------------
-- GHasTypeReps instances
--------------------------------------------------------------------------------

-- | No types to report for a constructor without arguments.
instance GHasTypeReps U1 where
  gTypeReps U1 = empty

-- | The types in a product is the concatenation of the types found in the
-- values of the product terms.
instance (GHasTypeReps a, GHasTypeReps b) => GHasTypeReps (a :*: b) where
  gTypeReps (a :*: b) = gTypeReps a >< gTypeReps b

instance (GHasTypeReps a, GHasTypeReps b) => GHasTypeReps (a :+: b) where
  gTypeReps (L1 a) = gTypeReps a
  gTypeReps (R1 b) = gTypeReps b

-- | We do need to do anything for the metadata.
instance (GHasTypeReps a) => GHasTypeReps (M1 i c a) where
    gTypeReps (M1 x) = gTypeReps x

-- | And the only interesting case, get the type of a type constructor
instance (HasTypeReps a) => GHasTypeReps (K1 i a) where
    gTypeReps (K1 x) = typeReps x

--------------------------------------------------------------------------------
-- HasTypeReps instances
--------------------------------------------------------------------------------

instance HasTypeReps a => HasTypeReps [a] where
  typeReps xs = typeOf xs <| foldMap typeReps xs

instance (HasTypeReps a, HasTypeReps b) => HasTypeReps (a, b) where
  typeReps t@(a, b) = typeOf t <| (typeReps a >< typeReps b)

instance HasTypeReps Bool where
  typeReps x = [typeOf x]

instance HasTypeReps Char where
  typeReps x = [typeOf x]

instance HasTypeReps Int where
  typeReps x = [typeOf x]

instance HasTypeReps Double where
  typeReps x = [typeOf x]

instance HasTypeReps Natural where
  typeReps x = [typeOf x]

instance HasTypeReps Word64 where
  typeReps x = [typeOf x]
