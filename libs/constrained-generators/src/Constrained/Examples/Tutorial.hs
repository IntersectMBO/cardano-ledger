module Constrained.Examples.Tutorial where

import Constrained

-- The `constrained-generators` library allows us to write
-- constraints that give us random generators, shrinkers, and checkers
-- for data using a small embedded DSL.

-- We can talk about numbers:

specInt :: Specification BaseFn Int
specInt = constrained $ \ i ->
  [ i <. 10
  , 0 <. i
  ]

-- TODO: talk about what's going on here:
--  - what's the type of `constrained`
--  - why a list?
--  - what's the type of `i`?
--  - What's the difference between Specification, Pred, and Term?
--  - What's the `fn` parameter?

-- We get a generator from `genFromSpec_ :: Specification BaseFn a -> Gen a`:
-- λ> sample $ genFromSpec_ specInt
-- 1
-- 5
-- 6
-- 6
-- 8
-- 5
-- 3
-- 1
-- 1
-- 4
-- 8

-- Likewise, `shrinkWithSpec :: Specification BaseFn a -> a -> [a]` gives us
-- a shrinker:
-- λ> shrinkWithSpec specInt 10
-- [5,8,9]
-- λ> shrinkWithSpec specInt 5
-- [3,4]
-- λ> shrinkWithSpec specInt 3
-- [2]
-- λ> shrinkWithSpec specInt 1
-- []

-- And, `conformsToSpec :: a -> Specification BaseFn a -> Bool` gives us a checker:
-- λ> 10 `conformsToSpec` specInt
-- False
-- λ> 5 `conformsToSpec` specInt
-- True

specProd0 :: Specification BaseFn (Int, Int)
specProd0 = constrained $ \ p -> fst_ p <. snd_ p

-- TODO: fix this error message so that it is more clear!

-- λ> sample $ genFromSpec_ specProd0
-- *** Exception: Simplifying:
--   constrained $ \ v0 -> assert $ Less (Fst (ToGeneric v0)) (Snd (ToGeneric v0))
-- optimisePred => assert $ Less (Fst (ToGeneric v0)) (Snd (ToGeneric v0))
-- assert $ Less (Fst (ToGeneric v0)) (Snd (ToGeneric v0))
-- toCtxList with too many holes

-- This gives us the _fundamental restriction_:
--   A variable can not appear twice in the same constraint

-- The fundamental restriction is very important to make the system compositional
-- and modular. We will get back to talking about it in detail when we discuss how to
-- extend the system. However, for now suffice to say that it's a lot easier to solve
-- constraints that look like `2 * x <. 10` than it is to solve constraints
-- like `x <. 10 - x`.
-- TODO: better example here!

-- To overcome the fundamental restrction we can use `match`:
-- match ::
--   forall fn p a.
--   ( HasSpec fn a
--   , IsProductType fn a
--   , IsPred p fn
--   ) =>
--   Term fn a ->
--   FunTy (MapList (Term fn) (ProductAsList a)) p ->
--   Pred fn
-- TODO: show a simplified type for match perhaps?

specProd1 :: Specification BaseFn (Int, Int)
specProd1 = constrained $ \ p ->
  match p $ \ x y ->
    x <. y

-- λ> sample $ genFromSpec_ specProd1
-- (-1,0)
-- (-4,-2)
-- (1,2)
-- (-2,1)
-- (7,8)
-- (-9,-4)
-- (-3,3)
-- (-1,12)
-- (-7,-6)
-- (-11,17)
-- (-53,-14)

-- This pattern of `constrained \ x -> match x f` is very common
-- and has a shorthand in the form of `constrained'`:

specProd2 :: Specification BaseFn (Int, Int)
specProd2 = constrained' $ \ x y -> x <. y

-- TODO:
--  - dependsOn
--    - Explain how solving actually works (show the graph)
--  - Booleans
--    - ifElse (only show simple example here, more complex example once
--      fix has been merged)
--  - Sum types
--  - Sets and Maps
--  - exists
--  - working with your own types
--  - reify
--  - monitor
--  - Extending the function universe
