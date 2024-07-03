{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Constrained.Examples.Set where

import GHC.Generics

import Data.Set (Set)
import Data.Set qualified as Set

import Constrained
import Constrained.Base ()
import Constrained.Examples.Basic

setPairSpec :: Specification BaseFn (Set Int, Set Int)
setPairSpec = constrained' $ \s s' ->
  forAll s $ \x ->
    forAll s' $ \y ->
      x <=. y

fixedSetSpec :: Specification BaseFn (Set Int)
fixedSetSpec = constrained $ \s ->
  forAll s $ \x ->
    [x <=. lit (i :: Int) | i <- [1 .. 3]]

setOfPairLetSpec :: Specification BaseFn (Set (Int, Int))
setOfPairLetSpec = constrained $ \ps ->
  forAll' ps $ \x y ->
    x <=. y

setSingletonSpec :: Specification BaseFn (Set (Int, Int))
setSingletonSpec = constrained $ \ps ->
  forAll ps $ \p ->
    forAll (singleton_ (fst_ p)) $ \x ->
      x <=. 10

eitherSimpleSetSpec :: Specification BaseFn (Set (Either Int Int))
eitherSimpleSetSpec = constrained $ \ss ->
  forAll ss $ \s ->
    (caseOn s)
      (branch $ \a -> a <=. 0)
      (branch $ \b -> 0 <=. b)

forAllAnySpec :: Specification BaseFn (Set Int)
forAllAnySpec = constrained $ \as ->
  forAll as $ \_ -> True

maybeJustSetSpec :: Specification BaseFn (Set (Maybe Int))
maybeJustSetSpec = constrained $ \ms ->
  forAll ms $ \m ->
    (caseOn m)
      (branch $ \_ -> False)
      (branch $ \y -> 0 <=. y)

notSubsetSpec :: Specification BaseFn (Set Int, Set Int)
notSubsetSpec = constrained' $ \s s' -> not_ $ subset_ s s'

emptyEitherMemberSpec :: Specification BaseFn (Set (Either Int Int))
emptyEitherMemberSpec = constrained $ \s ->
  forAll s $ \x ->
    (caseOn x)
      (branch $ \l -> member_ l mempty)
      (branch $ \r -> member_ r mempty)

emptyEitherSpec :: Specification BaseFn (Set (Either Int Int))
emptyEitherSpec = constrained $ \s ->
  forAll s $ \x ->
    (caseOn x)
      (branch $ \_ -> False)
      (branch $ \_ -> False)

notSubset :: Specification BaseFn (Set Int)
notSubset = constrained $ \s ->
  not_ $ s `subset_` lit (Set.fromList [1, 2, 3])

unionSized :: Specification BaseFn (Set Int)
unionSized = constrained $ \s ->
  10 ==. size_ (s <> lit (Set.fromList [1 .. 8]))

maybeSpec :: Specification BaseFn (Set (Maybe Int))
maybeSpec = constrained $ \ms ->
  forAll ms $ \m ->
    (caseOn m)
      (branch $ \_ -> False)
      (branch $ \y -> 0 <=. y)

eitherSetSpec ::
  Specification BaseFn (Set (Either Int Int), Set (Either Int Int), Set (Either Int Int))
eitherSetSpec = constrained' $ \es as bs ->
  [ assert $ es ==. (as <> bs)
  , forAll as $ \a ->
      (caseOn a)
        (branch $ \a' -> a' <=. 0)
        (branch $ \b' -> 1 <=. b')
  , forAll bs $ \b ->
      (caseOn b)
        (branch $ \_ -> False)
        (branch $ \b' -> 1 <=. b')
  ]

weirdSetPairSpec :: Specification BaseFn ([Int], Set (Either Int Int))
weirdSetPairSpec = constrained' $ \as as' ->
  [ as' `dependsOn` as
  , forAll as $ \a ->
      member_ (left_ a) as'
  , forAll as' $ \a' ->
      (caseOn a')
        (branch $ \x -> elem_ x as)
        (branch $ \_ -> False)
  ]

setPair :: Specification BaseFn (Set (Int, Int))
setPair = constrained $ \s ->
  [ forAll s $ \p ->
      p `satisfies` leqPair
  , assert $ lit (0, 1) `member_` s
  ]

setSpec :: Specification BaseFn (Set Int)
setSpec = constrained $ \ss ->
  forAll ss $ \s ->
    s <=. 10

compositionalSpec :: Specification BaseFn (Set Int)
compositionalSpec = constrained $ \x ->
  [ satisfies x setSpec
  , assert $ 0 `member_` x
  ]

emptySetSpec :: Specification BaseFn (Set Int)
emptySetSpec = constrained $ \s ->
  forAll s $ \x -> member_ x mempty

setSubSize :: Specification BaseFn (Set Int)
setSubSize = constrained $ \s ->
  2 ==. 12 - (sizeOf_ s)

newtype NotASet a = NotASet (Set a)
  deriving (Generic, Show, Eq)
instance Ord a => HasSimpleRep (NotASet a) where
  type SimpleRep (NotASet a) = [a]
  fromSimpleRep = NotASet . Set.fromList
  toSimpleRep (NotASet s) = Set.toList s
instance (Ord a, HasSpec fn a) => HasSpec fn (NotASet a)
instance Ord a => Forallable (NotASet a) a

emptyListSpec :: Specification BaseFn ([Int], NotASet (Either Int Int, Int))
emptyListSpec = constrained' $ \is ls ->
  [ forAll is $ \i -> i <=. 0
  , forAll' ls $ \l _ ->
      caseOn l (branch $ \_ -> False) (branch $ \_ -> False)
  ]

foldSingleCase :: Specification BaseFn Int
foldSingleCase = constrained $ \x ->
  [ assert $ not_ $ member_ x (lit (Set.fromList [10]))
  , letBind (pair_ x $ lit [(10, 20) :: (Int, Int)]) $ \p ->
      match p $ \_ p1 -> forAll p1 $ \p2 ->
        assert (0 <=. snd_ p2)
  ]

complexUnion :: Specification BaseFn (Set Int, Set Int)
complexUnion = constrained' $ \ys zs ->
  [ sizeOf_ ys <=. 10
  , 0 <. sizeOf_ (ys <> zs)
  ]

unionBounded :: Specification BaseFn (Set Int)
unionBounded = constrained $ \xs ->
  [ sizeOf_ (xs <> lit (Set.fromList [1, 2, 3])) <=. 3
  ]
