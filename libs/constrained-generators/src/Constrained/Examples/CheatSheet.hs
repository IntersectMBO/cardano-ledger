{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ViewPatterns #-}

module Constrained.Examples.CheatSheet where

import Constrained.API
import Constrained.Properties (forAllSpec)
import Data.Set (Set)
import Data.Set qualified as Set
import GHC.Generics
import Test.QuickCheck (Property, label)

-- The `constrained-generators` library allows us to write
-- constraints that give us random generators, shrinkers, and checkers
-- for data using a small embedded DSL, which defines a limited first order logic.
--
-- Every first order logic has 4 parts, as does our DSL.
-- 1) Terms :  e.g. x, 5, (member_ x set) (x ==. y)
--    Implemented as (Term a). We have variables like 'x', and constants like '5'.
--    'member_' and '==.' are function symbols, and build Terms from other terms.
--    By convention, a name followed by '_' or an infix operator followed by '.' are function symbols.
-- 2) Predicates (over terms). Predicates commonly used are
--        TruePred,
--        FalsePred (pure "explain"),
--        assert $ termWithTypeBool,
--    Some more unusual predicates are described below.
--    Implemented as type (Pred fn)
-- 3) Combinators (combining predicates). In general, And, Or, Not, Implies, True, False
--    But in the DSL, we are limited to
--      'And' using Block :: [Pred] -> Pred
--      'Not' using the function symbol not_ :: Term Bool -> Term Bool
--            for example:  assert $ not_ (x ==. y)
--      limited form of 'Or' using
--         chooseSpec :: (Int, Specification a)- > (Int, Specification a) -> Specification a
-- 4) Quantifiers (applying constraints to many things) :
--    forAll: Term t -> (Term a -> p) -> Pred fn
--    exists: ((forall b. Term b -> b) -> GE a) -> (Term a -> p) -> Pred fn
--    These are explained in detail below

-- In case you are interested, here is a list of supported function symbols (note the use of the '_' and '.' convention)
-- disjoint_,  dom_,  elem_,  length_,  member_,  not_,  rng_,  singleton_,  sizeOf_,  subset_,  sum_,  (/=.),
-- (<.),  (<=.),  (==.),  (>.),  (>=.), fromList_, null_, union_
-- You may also use the methods of Num (+) (-) (*), since there is a (Num (Term fn)) instance.

-- The first order logic DSL is used to build Specifications
-- A specifcation with type (Specification x) has two uses
-- 1) To generate a random values of type 'x', subject to the constraints in the specifications definition.
--    This is implemented by   genFromSpec :: Specification x -> Gen x (Gen is the QuickCheck Gen)
-- 2) To test if a value of type 'x' meets all of the constraints given in the specifications definition.
--     This is implemented by  conformsToSpec :: HasSpec a => a -> Specification a -> Bool

-- Lets get started. We can talk about numbers:

specInt :: Specification Int
specInt = constrained $ \i ->
  [ assert $ i <. 10
  , assert $ 0 <. i
  ]

-- What's going on here? In short:
--    `constrained :: (HasSpec a, IsPred p fn) => (Term a -> p) -> Specification a`
--    Introduces the variable `i` over which we can write constraints of type `p` over something
--    of type `a` to produce a `Specifcation a` using a list of
--    `assert :: Term Bool -> Pred  with `Term -level versions (function symbols) of familiar functions like
--    `(<.) :: OrdLike a => Term a -> Term a -> Term Bool`, `null_ :: Term [a] -> Term Bool`,
--    `rng_ :: (HasSpec k, HasSpec v, Ord k) => Term (Map k v) -> Term (Set k)` etc.
-- We get a generator from `genFromSpec :: Specification BaseFn a -> Gen a`:
-- λ> sample $ genFromSpec specInt
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

-- Note that the type of `constrained` says the binding function of type `Term a -> p` doesn't
-- have to produce a `Pred  (which is the return type of `assert`), but can produce something of type `p`
-- that satisfies `IsPred p`. This basically just means something that can be readily turned into a
-- `Pred`, like e.g. `Pred`, `Bool`, `Term Bool`, `[p]` for `IsPred p`. Consequently, we could
-- have written `specInt` as:

specInt' :: Specification Int
specInt' = constrained $ \i ->
  [ i <. 10
  , 0 <. i
  ]

-- However, beware that when we start mixing `Term Bool` and `Pred` in these lists we can end
-- up getting some inscrutable error messages. So, if a call to `constrained` or another function that
-- has `IsPred` as a constraint, starts giving you strange error messages, double check that you have
-- used `assert` instead of raw `Term Bool` everywhere relevant.

-- We also have support for product types with functions like `fst_`, `snd_`, and `pair_`:

specProd :: Specification (Int, Int)
specProd = constrained $ \p ->
  [ fst_ p <. 10
  , snd_ p <. 100
  ]

-- However, product types can also be a bit finicky:

specProd0 :: Specification (Int, Int)
specProd0 = constrained $ \p -> assert $ fst_ p <. snd_ p

-- λ> sample $ genFromSpec specProd0

-- *** Exception: Simplifying:

--   constrained $ \ v0 -> assert $ Less (Fst (ToGeneric v0)) (Snd (ToGeneric v0))
-- optimisePred => assert $ Less (Fst (ToGeneric v0)) (Snd (ToGeneric v0))
-- assert $ Less (Fst (ToGeneric v0)) (Snd (ToGeneric v0))
-- Can't build a single-hole context for variable v0 in term Less (Fst (ToGeneric v0)) (Snd (ToGeneric v0))

-- This gives us the _fundamental restriction_:
--   A variable can not appear twice in the same constraint

-- The fundamental restriction is very important to make the system compositional
-- and modular. We will get back to talking about it in detail when we discuss how to
-- extend the system. However, for now suffice to say that it's a lot easier to solve
-- constraints that look like `2 * x <. 10` than it is to solve constraints
-- like `x <. 10 - x` (i.e. ones that mention the same variable more than once).

-- To overcome the fundamental restriction we can use `match`:
-- match ::
--   forall p a.
--   ( HasSpec a
--   , IsProductType a
--   , IsPred p fn
--   ) =>
--   Term a ->
--   FunTy (MapList (Term fn) (ProductAsList a)) p ->
--   Pred fn

specProd1 :: Specification (Int, Int)
specProd1 = constrained $ \p ->
  match p $ \x y ->
    x <. y

-- λ> sample $ genFromSpec specProd1
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

-- Bringing variables into scope.
-- 'constrained' and 'match' are the ways we bring variable into scope, And they are often nested.
-- Consider writing a specification for pair of nested pairs: Specification ((Int,Int),(Int,Int))
-- How do we name the four different Int's ?

nested :: Specification ((Int, Int), (Int, Int))
nested =
  constrained $ \pp ->
    match pp $ \p1 p2 ->
      match p1 $ \x1 y1 ->
        match p2 $ \x2 y2 ->
          [x1 <=. y1, y1 <=. x2, x2 <=. y2]

-- ghci> sample $ genFromSpec nested
-- ((0,0),(0,0))
-- ((-9,-5),(-1,0))
-- ((-12,-10),(-5,-2))
-- ((-8,-4),(-3,-2))
-- ((-33,-18),(-15,-6))
-- ((-21,-12),(-1,3))
-- ((-36,-12),(1,9))
-- ((-64,-37),(-30,-4))
-- ((-53,-37),(-33,-10))
-- ((-49,-15),(-6,8))
-- ((-72,-34),(-26,-19))

-- A good rule of thumb when starting a new specification is to think about how you would
-- use 'constrained' and 'match' to bring variables, naming each of the parts that you want
-- to constrain, into scope.

-- Let's look under the hood of `match`, it introduces two auxilliary variables `v0` and `v1`
-- that circumvents the fundamental restriction by allowing us to generate values for `v1` and
-- `v0` before we generate a value for `v3`.

-- λ> simplifySpec specProd1
-- constrained $ \ v3 ->
--   let v1 = Fst (ToGeneric v3) in
--   let v0 = Snd (ToGeneric v3) in
--   assert $ Less v1 v0

-- This pattern of `constrained $ \ p -> match p $ \ x y -> ...` is very common
-- and has a shorthand in the form of `constrained'`:

specProd2 :: Specification (Int, Int)
specProd2 = constrained' $ \x y -> x <. y

-- How does generation actually work when we have multiple variables? For example,
-- it is not obvious (to the computer) what the best way of generating values satisfying
-- this constraint is:

solverOrder :: Specification (Int, Int)
solverOrder = constrained' $ \x y ->
  [ x <. y
  , y <. 10
  ]

-- For example, if you tried generating a value for `x` first chances are you'd generate
-- something larger than 10, which would make it impossible to generate a valid `y`. However,
-- when we run it we get reasonable values out:

-- sample $ genFromSpec solverOrder
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
--   constrained $ \ v_3 ->
--     let v_1 = Fst (ToGeneric v_3) in
--     let v_0 = Snd (ToGeneric v_3) in
--     {assert $ Less v_0 10
--      assert $ Less v_1 v_0}
-- SolverPlan
--   Dependencies:
--     v_0 <- []
--     v_1 <- [v_0]
--     v_3 <- [v_0, v_1]
--   Linearization:
--     v_0 <- TypeSpec [..9] []
--     v_1 <- assert $ Less v_1 v_0
--     v_3 <-
--       assert $ Equal (Fst (ToGeneric v_3)) v_1
--       assert $ Equal (Snd (ToGeneric v_3)) v_0

-- There are three parts to the output:
--  - The "Simplified spec" is the input specification after it has gone through a number of optimization
--    and simplification passes to make it amenable to solving.
--  - The "Dependencies" tells us what variables depend on what other variables to be solved. In this case `v0` (y)
--    has no dependencies, `v1` (x) is solved after `v0` and `v3` (the actual pair we are generating) is solved
--    last.
--  - Finaly, the "Linearization" tells us _what constraints define what varible_. This is an important aspect of the
--    system: variables are only constrained by assertions that talk about the variable itself and variables that
--    are solved before it. In this case `v0` (y) is defined by `y <. 10`, `v1` (x) by `x <. y` and `v3` by the equalities
--    in the `Let` constructs.
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

-- As an aside, the frustrating thing about making sense of the output of `printPlan` is the `v0`, `v1`, etc. naming.
-- To introduce proper names we can use the `var` quasi-quoter:

solverOrder' :: Specification (Int, Int)
solverOrder' = constrained' $ \ [var|x|] [var|y|] ->
  [ x <. y
  , y <. 10
  ]

-- Now we get more reasonable looking oputput from `printPlan`:
-- λ> printPlan solverOrder'
-- Simplified spec:
--   constrained $ \ v_3 ->
--     let x_1 = Fst (ToGeneric v_3) in
--     let y_0 = Snd (ToGeneric v_3) in
--     {assert $ Less y_0 10
--      assert $ Less x_1 y_0}
-- SolverPlan
--   Dependencies:
--     y_0 <- []
--     x_1 <- [y_0]
--     v_3 <- [y_0, x_1]
--   Linearization:
--     y_0 <- TypeSpec [..9] []
--     x_1 <- assert $ Less x_1 y_0
--     v_3 <-
--       assert $ Equal (Fst (ToGeneric v_3)) x_1
--       assert $ Equal (Snd (ToGeneric v_3)) y_0

-- A consequence of the default dependency order approach is that it's possible
-- to write constraints that put you in a tricky situation:

tightFit0 :: Specification (Int, Int)
tightFit0 = constrained' $ \x y ->
  [ 0 <. x
  , x <. y
  ]

-- λ> sample $ genFromSpec tightFit0

-- *** Exception: genFromPreds:

--   let v_1 = Fst (ToGeneric v_3) in
--   let v_0 = Snd (ToGeneric v_3) in
--   {assert $ Less v_1 v_0
--    assert $ Less 0 v_1}
-- SolverPlan
--   Dependencies:
--     v_0 <- []
--     v_1 <- [v_0]
--     v_3 <- [v_0, v_1]
--   Linearization:
--     v_0 <-
--     v_1 <-
--       TypeSpec [1..] []
--       ---
--       assert $ Less v_1 v_0
--     v_3 <-
--       assert $ Equal (Fst (ToGeneric v_3)) v_1
--       assert $ Equal (Snd (ToGeneric v_3)) v_0
-- Stepping the plan:
--   SolverPlan
--     Dependencies:
--       v_1 <- []
--       v_3 <- [v_1]
--     Linearization:
--       v_1 <- ErrorSpec [1..-1]
--       v_3 <-
--         TypeSpec (Cartesian TrueSpec (MemberSpec [0])) []
--         ---
--         assert $ Equal (Fst (ToGeneric v_3)) v_1
--   Env {unEnv = fromList [(v_0,EnvValue 0)]}
-- genFromSpecT ErrorSpec{} with explanation:
-- [1..-1]

-- The generator fails with output similar to what we saw above and a message telling us we tried to generate
-- a value from the (empty) interval [1..-1]. Inspecting the output above carefully we see that the graph and the
-- linearization tell us that `v0` (y) is completely unconstrained. The consequence of this is that when we get to the
-- point of trying to generate `v1` (x) we've already picked a value (-1) for `v0` that makes it impossible to satisfy
-- the constraints on `v1` and its constraints have specialized away to an error spec.

-- The solution to this issue is to introduce `dependsOn`, which lets us override the dependency order in constraints:

tightFit1 :: Specification (Int, Int)
tightFit1 = constrained' $ \x y ->
  [ assert $ 0 <. x
  , assert $ x <. y
  , y `dependsOn` x
  ]

-- λ> printPlan tightFit1
-- Simplified spec:
--   constrained $ \ v_3 ->
--     let v_1 = Fst (ToGeneric v_3) in
--     let v_0 = Snd (ToGeneric v_3) in
--     {v_0 <- v_1
--      assert $ Less v_1 v_0
--      assert $ Less 0 v_1}
-- SolverPlan
--   Dependencies:
--     v_0 <- [v_1]
--     v_1 <- []
--     v_3 <- [v_0, v_1]
--   Linearization:
--     v_1 <- TypeSpec [1..] []
--     v_0 <- assert $ Less v_1 v_0
--     v_3 <-
--       assert $ Equal (Fst (ToGeneric v_3)) v_1
--       assert $ Equal (Snd (ToGeneric v_3)) v_0

-- This gives us more balanced constraints that solve `v1` before they solve `v0`!
-- Consequently, this constraint generates reasonable values:

-- λ> sample $ genFromSpec tightFit1
-- (1,2)
-- (2,3)
-- (9,15)
-- (4,10)
-- (12,27)
-- (15,21)
-- (10,30)
-- (23,51)
-- (7,34)
-- (21,46)
-- (28,49)

-- We also support booleans with `ifElse :: Term Bool -> Pred -> Pred -> Pred`
-- where the branches of the `ifElse` depend on the scrutinee.

booleanExample :: Specification (Int, Int)
booleanExample = constrained' $ \x y ->
  ifElse
    (0 <. x)
    (y ==. 10)
    (y ==. 20)

-- sample $ genFromSpec booleanExample
-- (0,20)
-- (2,10)
-- (4,10)
-- (1,10)
-- (-2,20)
-- (3,10)
-- (7,10)
-- (-8,20)
-- (-5,20)
-- (-2,20)
-- (-19,20)

-- We can combine `ifElse` and `dependsOn` to write a nice example saying
-- that a PVP version pair `q` can follow a pair `p`.

-- Because we will need to re-use this multiple times we start by defining a valid
-- PVP constraint as any constraint that has non-negative major and minor version number.
validPVPVersion :: Specification (Int, Int)
validPVPVersion = constrained' $ \ma mi -> [0 <=. ma, 0 <=. mi]

-- Now we are ready to define the constraints for valid PVP succession. Note here that
-- we use the `satisfies :: Term a -> Specification BaseFn a -> Pred` combinator
-- to re-use the `validPVPVersion` constraint.

canFollowExample :: Specification ((Int, Int), (Int, Int))
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

-- λ> sample $ genFromSpec canFollowExample
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

-- We have native support for sum types using `caseOn` and `branch`:

sumExample :: Specification (Either Int Bool)
sumExample = constrained $ \e ->
  (caseOn e)
    (branch $ \i -> i <. 0)
    (branch $ \b -> not_ b)

-- Furthermore, cases are solved _inside-out_ by default:

sumExampleTwo :: Specification (Int, Either Int Bool)
sumExampleTwo = constrained' $ \i e ->
  [ caseOn
      e
      (branch $ \j -> i <. j)
      (branch $ \b -> not_ b)
  , assert $ 20 <. i
  ]

-- We can work with sets with operations like `subset_`, `union_` (or `<>`), `disjoint_`, and `singleton_`:

setExample :: Specification (Set Int, Set Int, Set Int)
setExample = constrained' $ \xs ys zs ->
  [ xs `subset_` (ys <> zs)
  , sizeOf_ ys <=. 10
  ]

-- We can also quantify over things like sets with `forAll`:

forAllFollow0 :: Specification ((Int, Int), Set (Int, Int))
forAllFollow0 = constrained' $ \p qs ->
  [ forAll qs $ \q -> pair_ p q `satisfies` canFollowExample
  ]

-- λ> sample $ genFromSpec forAllFollow0
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

forAllFollow :: Specification ((Int, Int), Set (Int, Int))
forAllFollow = constrained' $ \p qs ->
  [ forAll qs $ \q -> pair_ p q `satisfies` canFollowExample
  , p `satisfies` validPVPVersion
  ]

-- λ> sample $ genFromSpec forAllFollow
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

-- We also have existential quantification in the language. The first argument to
-- `exists` tells you how to reconstruct the value from known values.

existentials :: Specification (Set Int, Set Int)
existentials = constrained' $ \xs ys ->
  exists (\eval -> pure $ Set.intersection (eval xs) (eval ys)) $ \zs ->
    [ assert $ not_ $ null_ zs
    , assert $ zs `subset_` xs
    , assert $ zs `subset_` ys
    , xs `dependsOn` zs
    , ys `dependsOn` zs
    ]

-- You can work with your own types relatively easily. If they are `Generic`
-- you even get all the machinery of sum and product types for free!

data FooBarBaz = Foo Int Int | Bar Bool | Baz deriving (Eq, Show, Generic)

-- All you need to do is introduce instances for `HasSimpleRep` and `HasSpec`:

instance HasSimpleRep FooBarBaz

instance HasSpec FooBarBaz

fooBarBaz :: Specification FooBarBaz
fooBarBaz = constrained $ \fbb ->
  caseOn
    fbb
    (branch $ \i j -> i <. j)
    (branch $ \b -> not_ b)
    (branch $ \_ -> False)

-- λ> sample $ genFromSpec fooBarBaz
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

-- Some functions don't exist on the term level. In this case we can use
-- `reifies :: (HasSpec a, HasSpec b) => Term b -> Term a -> (a -> b) -> Pred`
-- to introduce a one-way evaluation of a Haskell function:

reifyExample :: Specification (Int, Int)
reifyExample = constrained' $ \ [var|a|] [var|b|] ->
  reifies b a $ \x -> mod x 10

-- Here we introduce two variables `a` and `b` without any immediate dependency and we say that
-- `b` reifies `a` via the haskell function `\x -> mod x 10`. The best way to understand what this
-- cryptic code means is to imagine there was a `mod_` function, in that case this code would be equivalent
-- to:

reifyExample' :: Specification (Int, Int)
reifyExample' = constrained' $ \a b ->
  [ assert $ b ==. mod_ a 10
  , b `dependsOn` a
  ]
  where
    mod_ :: Term Int -> Term Int -> Term Int
    mod_ = error "This doesn't exist"

-- When we look at the plan we get from `reifyExample` we get what we'd expect:
-- λ> printPlan reifyExample
-- Simplified spec:
--   constrained $ \ v_3 ->
--     let v_1 = Fst (ToGeneric v_3) in
--     let v_0 = Snd (ToGeneric v_3) in reifies v_0 v_1
-- SolverPlan
--   Dependencies:
--     v_0 <- [v_1]
--     v_1 <- []
--     v_3 <- [v_0, v_1]
--   Linearization:
--     v_1 <-
--     v_0 <- reifies v_0 v_1
--     v_3 <-
--       assert $ Equal (Fst (ToGeneric v_3)) v_1
--       assert $ Equal (Snd (ToGeneric v_3)) v_0

-- Sometimes it is convenient to introduce an auxilliary variable to represent the result of applying the
-- haskell-level function to the term, for this purpose we have
-- `reify :: (HasSpec a, HasSpec b, IsPred p fn) => Term a -> (a -> b) -> (Term b -> p) -> Pred`.

-- We have tools to control the distribution of test cases and monitor those distributions. Using `branchW` we can
-- attach weights to branches in a `caseOn` and using `monitor :: ((forall. Term a -> a) -> Property -> Property) -> Pred`
-- we can use the normal QuickCheck functions for monitoring distributions of generators to see the effects of this.

monitorExample :: Specification (Either Int Int)
monitorExample = constrained $ \e ->
  caseOn
    e
    (branchW 1 $ \_ -> monitor $ \_ -> label "Left")
    (branchW 2 $ \_ -> monitor $ \_ -> label "Right")

-- The `forAllSpec :: (Testable p, HasSpec a) => Specification a -> (a -> p) -> Property` we
-- automatically get the monitoring from the spec in our property:

prop_monitoring :: Property
prop_monitoring = forAllSpec monitorExample $ \_ -> True

-- λ> quickCheck $ prop_monitoring
-- +++ OK, passed 100 tests:
-- 64% Right
-- 36% Left

-- Other tools for controlling distributions of specifications are available too, for example
-- `chooseSpec :: HasSpec a => (Int, Specification a) -> (Int, Specification a) -> Specification a`,
-- the definition of which constitutes a useful object of study to better understand how to use the compositional
-- nature of the system to build powerful features.

chooseSpecExample :: Specification Int
chooseSpecExample =
  chooseSpec
    (1, constrained $ \i -> i <. 0)
    (2, constrained $ \i -> 0 <. i)

prop_chooseSpec :: Property
prop_chooseSpec = forAllSpec chooseSpecExample $ \i ->
  label (show $ signum i) True

-- λ> quickCheck prop_chooseSpec
-- +++ OK, passed 100 tests:
-- 67% 1
-- 33% -1
