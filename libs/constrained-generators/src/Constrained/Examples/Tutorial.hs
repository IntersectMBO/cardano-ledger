{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
module Constrained.Examples.Tutorial where

import Data.Set (Set)
import GHC.Generics

import Constrained

-- The `constrained-generators` library allows us to write
-- constraints that give us random generators, shrinkers, and checkers
-- for data using a small embedded DSL.

-- We can talk about numbers:

specInt :: Specification BaseFn Int
specInt = constrained $ \i ->
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

-- We also have support for product types with functions like `fst_`, `snd_`, and `pair_`:

specProd :: Specification BaseFn (Int, Int)
specProd = constrained $ \p ->
  [ fst_ p <. 10
  , snd_ p <. 100
  ]

-- However, product types can also be a bit finicky:

specProd0 :: Specification BaseFn (Int, Int)
specProd0 = constrained $ \p -> fst_ p <. snd_ p

-- λ> sample $ genFromSpec_ specProd0

-- *** Exception: Simplifying:

--   constrained $ \ v0 -> assert $ Less (Fst (ToGeneric v0)) (Snd (ToGeneric v0))
-- optimisePred => assert $ Less (Fst (ToGeneric v0)) (Snd (ToGeneric v0))
-- assert $ Less (Fst (ToGeneric v0)) (Snd (ToGeneric v0))
-- toCtxList with too many holes
-- TODO: fix this error message so that it is more clear!

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
specProd1 = constrained $ \p ->
  match p $ \x y ->
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

-- This pattern of `constrained $ \ p -> match p $ \ x y -> ...` is very common
-- and has a shorthand in the form of `constrained'`:

specProd2 :: Specification BaseFn (Int, Int)
specProd2 = constrained' $ \x y -> x <. y

-- How does generation actually work when we have multiple variables? For example,
-- it is not obvious (to the computer) what the best way of generating values satisfying
-- this constraint is:

solverOrder :: Specification BaseFn (Int, Int)
solverOrder = constrained' $ \x y ->
  [ x <. y
  , y <. 10
  ]

-- For example, if you tried generating a value for `x` first chances are you'd generate
-- something larger than 10, which would make it impossible to generate a valid `y`. However,
-- when we run it we get reasonable values out:

-- sample $ genFromSpec_ solverOrder
-- (-1,0)
-- (0,2)
-- (-4,4)
-- (-7,-3)
-- (-7,3)
-- (-11,-3)
-- (4,8)
-- (-15,-14)
-- (-25,-10)
-- (-23,-6)
-- (-51,-20)

-- But how does the system know to generate `y` first? Unfortunately, there is nothing smart about
-- it. The system simply solves things "right to left" - variables that appear to the right in assertions
-- are solved before variables to the left. If one wants to understand the consequences of this and how it
-- affects the generator the `printPlan` function comes in handy:

-- λ> printPlan solverOrder
-- Simplified spec:
--   constrained $ \ v3 ->
--     let v1 = Fst (ToGeneric v3) in
--     let v0 = Snd (ToGeneric v3) in
--     {assert $ Less v0 10
--      assert $ Less v1 v0}
-- Graph: v0 <- []
--        v1 <- [v0]
--        v3 <- [v0, v1]
-- Linearization:
--   v0 <- assert $ Less v0 10
--   v1 <- assert $ Less v1 v0
--   v3 <-
--     assert $ Equal (Fst (ToGeneric v3)) v1
--     assert $ Equal (Snd (ToGeneric v3)) v0

-- There are three parts to the output:
--  - The "Simplified spec" is the input specification after it has gone through a number of optimization
--    and simplification passes to make it amenable to solving.
--  - The "Graph" tells us what variables depend on what other variables to be solved. In this case `v0` (y)
--    has no dependencies, `v1` (x) is solved after `v0` and `v3` (the actual pair we are generating) is solved
--    last.
--  - Finaly, the "Linearization" tells us _what constraints define what varible_. This is an important aspect of the
--    system: variables are only constrained by assertions that talk about the variable itself and variables that
--    are solved before it. In this case `v0` (y) is defined by `y <. 10`, `v1` (x) by `x <. y` and `v3` by the equalities
--    in the `Let` constructs (we will get back to how they work later on). (TODO: get back to how they work later on)
--
-- As the generator executes this plan it will pick the variables in the order in which they appear in the linearization
-- and generate the corresponding values. For example, an execution trace could go like the following pseudo-trace (the details of how
-- this works are slightly more involved but the basic order of operations is accurate):
--  v0 <- pick from (-∞, 10)
--  v0 = 4
--  v1 <- pick from [4/v0](-∞, v0)
--        -> pick from (-∞, 4)
--  v1 = 2
--  v3 <- pick from [4/v0, 2/v1]{fst == v1, snd == v0}
--        -> pick from {fst == 2, snd == 4}
--  v3 = (2, 4)

-- A consequence of this approach is that it's possible to write constraints that put you in a tricky situation:

tightFit0 :: Specification BaseFn (Int, Int)
tightFit0 = constrained' $ \x y ->
  [ 0 <. x
  , x <. y
  ]

-- λ> sample $ genFromSpec_ tightFit0
-- *** Exception: genFromPreds:
--   let v1 = Fst (ToGeneric v3) in
--   let v0 = Snd (ToGeneric v3) in
--   {assert $ Less v1 v0
--    assert $ Less 0 v1}
-- With graph:
--   v0 <- []
--   v1 <- [v0]
--   v3 <- [v0, v1]
-- With linearization:
--   v0 <-
--   v1 <-
--     assert $ Less v1 v0
--     assert $ Less 0 v1
--   v3 <-
--     assert $ Equal (Fst (ToGeneric v3)) v1
--     assert $ Equal (Snd (ToGeneric v3)) v0
-- [1..-1]
-- TODO: improve this error message

-- The generator fails with output similar to what we saw above and a messge telling us we tried to generate
-- a value from the (empty) interval [1..-1]. Inspecting the output above carefully we see that the graph and the
-- linearization tell us that `v0` (y) is completely unconstrained. The consequence of this is that when we get to the
-- point of trying to generate `v1` (x) we've already picked a value (-1) for `v0` that makes it impossible to satisfy
-- the constraints on `v1`.

-- The solution to this issue is to introduce `dependsOn`, which lets us override the dependency order in constraints:

tightFit1 :: Specification BaseFn (Int, Int)
tightFit1 = constrained' $ \x y ->
  [ assert $ 0 <. x
  , assert $ x <. y
  , y `dependsOn` x
  ]

-- λ> printPlan tightFit1
-- Simplified spec:
--   constrained $ \ v3 ->
--     let v1 = Fst (ToGeneric v3) in
--     let v0 = Snd (ToGeneric v3) in
--     {v0 <- v1
--      assert $ Less v1 v0
--      assert $ Less 0 v1}
-- Graph:
--   v0 <- [v1]
--   v1 <- []
--   v3 <- [v0, v1]
-- Linearization:
--   v1 <- assert $ Less 0 v1
--   v0 <- assert $ Less v1 v0
--   v3 <-
--     assert $ Equal (Fst (ToGeneric v3)) v1
--     assert $ Equal (Snd (ToGeneric v3)) v0

-- This gives us more balanced constraints that solve `v1` before they solve `v0`!

-- TODO:
--  - Booleans
--    - ifElse (only show simple example here, more complex example once
--      fix has been merged)

-- We can combine `ifElse` and `dependsOn` to write a nice example saying
-- that a PVP version pair `q` can follow a pair `p`.

-- Because we will need to re-use this multiple times we start by defining a valid
-- PVP constraint as any constraint that has non-negative major and minor version number.
validPVPVersion :: Specification BaseFn (Int, Int)
validPVPVersion = constrained' $ \ ma mi -> [ 0 <=. ma, 0 <=. mi ]

-- Now we are ready to define the constraints for valid PVP succession. Note here that
-- we use the `satisfies :: Term fn a -> Specification BaseFn a -> Pred fn` combinator
-- to re-use the `validPVPVersion` constraint.

canFollowExample :: Specification BaseFn ((Int, Int), (Int, Int))
canFollowExample = constrained' $ \p q ->
  [ match p $ \ma mi ->
      match q $ \ma' mi' ->
        [ ifElse
            (ma' ==. ma)
            (mi' ==. mi + 1)
            (mi' ==. 0)
        , -- Note how these two constraints imply a cycle:
          --  ma' <- ma <- ma'
          assert $ ma' <=. ma + 1
        , assert $ ma <=. ma'
        , -- We break that cycle by specifying a concrete order
          -- Another option would be to define `>=.` but that doesn't
          -- exist right now and we will get to extending the language
          -- later on!
          ma' `dependsOn` ma
        ]
  , p `satisfies` validPVPVersion
  , q `satisfies` validPVPVersion
  ]

-- λ> sample $ genFromSpec_ canFollowExample
-- ((0,0),(0,1))
-- ((1,0),(1,1))
-- ((4,2),(4,3))
-- ((12,1),(12,2))
-- ((11,16),(11,17))
-- ((20,7),(21,0))
-- ((18,12),(18,13))
-- ((6,18),(7,0))
-- ((29,24),(30,0))
-- ((23,21),(23,22))
-- ((26,14),(26,15))

-- TODO:
--  - Sum types

-- TODO:
--  - start off with a simpler example for sets (union and disjont?)

-- We can also quantify over constraints using `
forAllFollow0 :: Specification BaseFn ((Int, Int), Set (Int, Int))
forAllFollow0 = constrained' $ \ p qs ->
  [ forAll qs $ \ q -> pair_ p q `satisfies` canFollowExample
  ]

-- λ> sample $ genFromSpec_ forAllFollow0
-- ((0,0),fromList [])
-- ((1,-1),fromList [])
-- ((2,3),fromList [(2,4),(3,0)])
-- ((4,2),fromList [(4,3),(5,0)])
-- ((-2,6),fromList [])
-- ((10,-9),fromList [])
-- ((-1,-8),fromList [])
-- ((-8,-1),fromList [])
-- ((1,4),fromList [(1,5),(2,0)])
-- ((-17,-5),fromList [])
-- ((-2,12),fromList [])

-- How come the sets are so small? Note that we sometimes still generate
-- negative values for the components of `p`. But we said in the `canFollowExample`
-- that `p` needs to be a valid PVP version. However, the constraints only say that
-- it needs to be a valid PVP version _if `qs` is non-empty!_. This is easily fixed
-- by specifying that `p` is _always_ a valid PVP version!

forAllFollow :: Specification BaseFn ((Int, Int), Set (Int, Int))
forAllFollow = constrained' $ \ p qs ->
  [ forAll qs $ \ q -> pair_ p q `satisfies` canFollowExample
  , p `satisfies` validPVPVersion
  ]

-- λ> sample $ genFromSpec_ forAllFollow
-- ((0,0),fromList [])
-- ((0,1),fromList [])
-- ((1,5),fromList [(1,6),(2,0)])
-- ((8,10),fromList [(8,11)])
-- ((12,15),fromList [(12,16)])
-- ((6,16),fromList [])
-- ((4,11),fromList [(4,12)])
-- ((10,21),fromList [(10,22),(11,0)])
-- ((28,2),fromList [(28,3),(29,0)])
-- ((20,3),fromList [(20,4),(21,0)])
-- ((16,29),fromList [(16,30),(17,0)])

-- TODO:
--  - exists

-- You can work with your own types relatively easily. If they are `Generic`
-- you even get all the machinery of sum and product types for free!

data FooBarBaz = Foo Int Int | Bar Bool | Baz deriving (Eq, Show, Generic)

-- All you need to do is introduce instances for `HasSimpleRep` and `HasSpec`:

instance HasSimpleRep FooBarBaz
instance BaseUniverse fn => HasSpec fn FooBarBaz

fooBarBaz :: Specification BaseFn FooBarBaz
fooBarBaz = constrained $ \ fbb ->
  caseOn fbb
    (branch $ \ i j -> i <. j)
    (branch $ \ b -> not_ b)
    (branch $ \ _ -> False)

-- λ> sample $ genFromSpec_ fooBarBaz
-- Foo (-1) 0
-- Bar False
-- Foo (-9) (-3)
-- Bar False
-- Foo 1 3
-- Foo (-20) (-8)
-- Foo (-35) (-11)
-- Bar False
-- Foo (-8) 5
-- Bar False
-- Foo (-4) 7

-- TODO:
--  - reify
--  - monitor
--  - Extending the function universe
--  - More complicated work with your own types (e.g. treating a sum type as a product type like we do with some types that have patterns
--  that unify multiple constructors).
