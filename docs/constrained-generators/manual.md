# Constrained Generators Manual


The markdown source of this file can be obtained
[here](https://github.com/IntersectMBO/cardano-ledger/blob/ts-constrained-manual/docs/constrained-generators/manual.md)


All the examples in this file can be obtained 
[here](https://github.com/IntersectMBO/cardano-ledger/blob/ts-constrained-manual/docs/constrained-generators/ManualExamples.hs)

<!-- markdown-toc start - Don't edit this section. Run M-x markdown-toc-refresh-toc -->
**Table of Contents**

- [Constrained Generators Manual](#constrained-generators-manual)
    - [Constrained Generators is a First-Order Logic](#constrained-generators-is-a-first-order-logic)
    - [Design Goals of the Library](#design-goals-of-the-library)
    - [HasSpec instances](#hasspec-instances)
    - [Building logic specifications using Haskell functions](#building-logic-specifications-using-haskell-functions)
    - [Another example using conjunction and simple arithmetic](#another-example-using-conjunction-and-simple-arithmetic)
    - [Function Symbols](#function-symbols)
    - [Predefined HasSpec instances and their function symbols.](#predefined-hasspec-instances-and-their-function-symbols)
        - [Function symbols for numeric types](#function-symbols-for-numeric-types)
        - [` Function symbols for Bool`](#-function-symbols-for-bool)
        - [Function symbols for List](#function-symbols-for-list)
        - [Function symbols for Set](#function-symbols-for-set)
        - [Function symbols for Map](#function-symbols-for-map)
    - [Generating from and checking against specifications](#generating-from-and-checking-against-specifications)
    - [How we solve the constraints](#how-we-solve-the-constraints)
        - [How to pick the variable order](#how-to-pick-the-variable-order)
        - [The total definition requirement ](#the-total-definition-requirement)
        - [Using Match to introduce new variables for subcomponents](#using-match-to-introduce-new-variables-for-subcomponents)
    - [Overloaded types in the library](#overloaded-types-in-the-library)
- [Library functions to build Term, Pred, Specification](#library-functions-to-build-term-pred-specification)
    - [From Term to Pred](#from-term-to-pred)
    - [For all elements in a container type (List, Set, Map)](#for-all-elements-in-a-container-type-list-set-map)
    - [Reification](#reification)
    - [Disjunction, choosing between multiple things with the same type](#disjunction-choosing-between-multiple-things-with-the-same-type)
    - [Primed library functions which are compositions with match](#primed-library-functions-which-are-compositions-with-match)
    - [Constructors and Selectors](#constructors-and-selectors)
    - [Naming introduced lambda bound Term variables](#naming-introduced-lambda-bound-term-variables)
    - [Existential quantifiers](#existential-quantifiers)
    - [Conditionals](#conditionals)
    - [Explanations](#explanations)
    - [Operations to define and use Specifications](#operations-to-define-and-use-specifications)
    - [Utility functions](#utility-functions)
    - [Escape Hatch to QuickCheck Gen monad](#escape-hatch-to-quickcheck-gen-monad)
- [Strategy for constraining a large type with many nested sub-components.](#strategy-for-constraining-a-large-type-with-many-nested-sub-components)
- [Writing HasSpec instances by hand.](#writing-hasspec-instances-by-hand)
    - [Strategy 1 using GHC.Generics](#strategy-1-using-ghcgenerics)
    - [Strategy 2 writing your own SimpleRep instance](#strategy-2-writing-your-own-simplerep-instance)
    - [Strategy 3 defining the SimpleRep instance in terms of another type with a SimpleRep instance](#strategy-3-defining-the-simplerep-instance-in-terms-of-another-type-with-a-simplerep-instance)
    - [Strategy 4, bypassing SimpleRep, and write the HasSpec instance by Hand](#strategy-4-bypassing-simplerep-and-write-the-hasspec-instance-by-hand)
- [A look into the internals of the system.](#a-look-into-the-internals-of-the-system)

<!-- markdown-toc end -->


## Constrained Generators is a First-Order Logic


A First-order typed logic has 4 components

1. **Terms** consisting of 
   - Variables: `x`, `y` . 
   - Constants: `5`, `"abc"`, `True` . 
   - Applications: `elem_ "abc" xs`.  Applications apply a function symbol (i.e. `elem_`) to a list of Terms
2. **Predicates**   (Assert (x ==. 5)). Predicates are the assertions of boolean typed terms.
3. **Connectives**  (And, Or, Not, =>, ...).  Connectives make complicated Predicates out of simpler ones.
4. **Quantifiers**  (Forall, Exists)

The **Constrained generators** system allows programmers to write Haskell programs with type `(Specification T)` that denotes a set of random values for the type `T`, that are subject to a set of constraints. This supports property based testing where a completely random set of values may not be useful.


## Design Goals of the Library

The system was designed to have three important properties

1.  A Specification determines a `QuickCheck` generator for generating random values subject to constraints.
2.  A Specification determines a check that a particular value meets all the specifications constraints.
3. If a Specification is over constrained, the system tries to explain where the ambiguity occurs.

The first part is implemented by the function `genFromSpec`

```
genFromSpec :: HasSpec a => Specification a -> QuickCheck.Gen a
```

The second is implemented by the function `conformsToSpec`

```
conformsToSpec :: HasSpec a => a -> Specification a -> Bool
```

The third is embodied by the error messages when solving a constraint fails


The **Constrained Generators** system is implemented as an embeded domain specific language in Haskell. 
The Terms, Predicates, Connectives, and Quantifiers of the first order logic are embedded into three
Haskell datatypes (`Specification t`, `Term t`, and `Pred`). There is a rich (extendable) library
of Haskell functions that can be used to define and construct values of these types.  The library is
implemented in a such a way, that the four parts of the logic are defined in ways that are similar 
to Haskell expressions with type Bool. 

Let us look at a simple example, and study how this is done. Below is a Haskell declaration that defines a 
specification of a pair of Int (`p`) , subject to the constraint that the first component (`x`)
is less than or equal to the second component (`y`) plus 2

```
leqPair :: Specification (Int, Int)
leqPair = constrained $ \ p ->
   match p $ \ x y ->
     assert (x <=. (y + lit 2))
```	

The library uses Haskell lambda expressions to introduce variables in the Term language of the system,
and Haskell functions to build Terms and Predicates. The Haskell function `lit` takes Haskell values 
and turns them into constants in the Term language. The types of the Haskell functions used in the 
above definitions are

```
constrained :: HasSpec a => (Term a -> Pred) -> Specification a

match :: (HasSpec a, HasSpec b) => Term (a,b) -> (Term a -> Term b -> Pred) -> Pred

lit :: HasSpec a -> a -> Term a

assert :: Term Bool -> Pred

(<=.) :: OrdLike a => Term a -> Term a -> Term Bool
```

The Haskell Constraint `HasSpec a` states that the type `a` has been admitted to the system as one of the types 
that can be subject to constraints. The system comes with a large set of `HasSpec` instances, including ones for:

1. Bool
2. Tuples
3. Sums
4. Numeric types (Int,Integer, Natural, Int8, Int16, Int32, Int64, Word8, Word16, Word32, Word64)
5. Lists
6. Maps
7. Sets
8. Trees
9. Maybe
10. Either
11. ()

## HasSpec instances

`HasSpec` instances can always be added to admit more types. Any type with a `GHC.Generics(Generic)` instance can be 
given a default instance by using its Sum-of-Products generic definition. In the Cardano Ledger System 
over 200 types have been given `HasSpec` instances, either by using the `GHC.Generics` path, or by writing the instances by hand.

## Building logic specifications using Haskell functions

Note that `constrained` and `match` take functions, which abstract over terms, and return `Specification` and `Pred`. 
Using the library functions, variables in the Term language are always introduced using Haskell lambda abstractions. And the library
functions combine these into Terms, Preds, and Specifications.

## Another example using conjunction and simple arithmetic

Suppose we want to put more than one simple condition on the pair of Ints. We would do that using the connective `And` that converts a `[Pred]` into a `Pred`

```
sumPair :: Specification (Int, Int)
sumPair = constrained $ \ p ->
  match p $ \ x y ->
    And [ assert $ x <=. y
        , assert $ y <=. 20 
        , assert $ x + y ==. 25 ]
```	

This example also re-illustrates that `(Term Int)` has a (partial) Num instance, and that we can constrain
multiple (different) variables using simple `Num` methods (`(+)`, `(-)`,  and `negate`). Note also 
the operator: `(==.) :: (Eq n, HasSpec n) => Term n -> Term n -> Term Bool` 

## Function Symbols

Note that `(<=.)` , and `(==.)` are two of the function symbols in the first order logic. They obey a 
useful naming convention. Infix function symbols corresponding to Haskell infix operators have 
corresponding infix operators,  lifting Haskell infix functions with type `(a -> b -> c)`, to library infix 
functions which have analogous types `(Term a -> Term b -> Term c)`
and are named using the convention that we add the dot `(.) to the end of the Haskell operator.

A similar naming convention holds for prefix function symbols, except instead of adding a
dot to the end of the Haskell name, we add an underscore `(_) to the end of the Haskell prefix functions's
name. Some examples follow.

```
(fst :: (a,b) -> a)` **to**
(fst_ :: (HasSpec a,HasSpec b) => Term (a,b) -> Term a)
```
```
(snd :: (a,b) -> b) **to**
(snd_ :: (HasSpec a,HasSpec b) => Term (a,b) -> Term b)
```

```
(not :: Bool -> Bool) **to**
(not_ :: Term Bool -> Term Bool)
```

```
(member :: a -> Set a -> Bool) **to**
(member_ :: HasSpec a => Term a -> Term (Set a) -> Term Bool)
```


## Predefined HasSpec instances and their function symbols.

In order to write specification for a particular type, that type must have a `HasSpec` instance. 
A type with a `HasSpec` instance might have a number of Function Symbols that operate on that type.
There are a number of types that have predefined `HasSpec` instances. As a reference, we list them 
here along with the type of their function symbols.

### Function symbols for numeric types

`(Int, Integer, Natural, Int8, Int16, Int32, Int64, Word8, Word16, Word32, Word64)`

The function symbols of numeric types are:

 1. `(<=.) :: OrdLike a => Term a -> Term a -> Term Bool`
 2. `(<.) :: OrdLike a => Term a -> Term a -> Term Bool`
 3. `(>=.) :: OrdLike a => Term a -> Term a -> Term Bool`
 4. `(>.) :: OrdLike a => Term a -> Term a -> Term Bool`
 5. `(==.) :: HasSpec a => Term a -> Term a -> Term Bool`
 6.  A partial Num instance for (Term n) where n is a Numeric type. Operators `(+)`, `(-)`, `(*)`
 
### ` Function symbols for Bool`

The function symbols of `Bool` are:

  1.  `or_ :: Term Bool -> Term Bool -> Term Bool`
  2.  `not_ :: Term Bool -> Term Bool`
 
###  Function symbols for List

`HaSpec a => HasSpec [a]`

The function symbols of `[a]` are:

  1.  `foldMap_ :: (Sized [a], Foldy b, HasSpec a) => (Term a -> Term b) -> Term [a] -> Term b`
  2.  `singletonList_ :: (Sized [a], HasSpec a) => Term a -> Term [a]`
  3.  `append_  :: (Sized [a], HasSpec a) => Term [a] -> Term [a] -> Term [a]`
  
###  Function symbols for Set

`HasSpec a => HasSpec (Set a)`

The function symbols of `(Set a)` are:

  1.  `singleton_ :: (Ord a, HasSpec a) => Term a -> Term (Set a)`
  2.  `union_ :: (Ord a, HasSpec a) => Term (Set a) -> Term (Set a) -> Term (Set a)`
  3.  `subset_ :: (Ord a, HasSpec a) => Term (Set a) -> Term (Set a) -> Term Bool`
  4.  `member_ :: (Ord a, HasSpec a) => Term a -> Term (Set a) -> Term Bool`
  5.  `disjoint_ :: (Ord a, HasSpec a) => Term (Set a) -> Term (Set a) -> Term Bool`
  6.  `fromList_ :: (Ord a, HasSpec a) => Term [a] -> Term (Set a)`

###  Function symbols for Map

`(HasSpec k, HasSpec v) => HasSpec (Mapk k v)`

The function symbols of `(Map k v)` are:

1.  `dom_ :: Ord k => Term(Map k v) -> Term (Set k)`
2.  `rng_ :: Ord k => Term (Map k v) -> Term [v]`
3.  `lookup_ :: Ord k => Term k -> Term (Map k v) -> Term (Maybe v)`

## Generating from and checking against specifications

Once we have written a `Specification` what can we do with it? Specifications have two interpretations.

1.  We can interpret it as a generator of values the meet all the constraints inside the specification.
2.  We can interpret it as a function that checks if a given value meets all the constraints inside the specification.


The first interpretation of the specification is the function `genFromSpec`

```
-- Generate a value from the spec in the QuickCheck monad 'Gen'
genFromSpec:: (HasCallStack, HasSpec a) => Specification a -> QuickCheck.Gen a
```

This function is very useful when writing QuickCheck properties. With it we can write 
`Gen` instances that are not completely random, but instead meet a set of constraints.

Consider a system of 4 variables (w,x,y,z) where we want to test the QuickCheck *implication*  property
`(w < x && x < y && y < z) ==> property (w < z)`
We might write a QuickCheck property like this

```
prop1 :: Gen Property
prop1 = do
   (w,x,y,z) <- arbitrary :: Gen (Int,Int,Int,Int)
   pure $ (w < x && x < y && y < z) ==> property (w < z)
```

The problem with this is that the probability that the precedant `(w < x && x < y && y < z)` is True, for random
`w`, `x`, `y`, and `z`, is pretty low, so the property will pass vacuously most of the time, making a poor test.
We can observe this by

```
ghci> quickCheck prop1
*** Gave up! Passed only 29 tests; 1000 discarded tests.
```

A vacuous pass, becomes a QuickCheck `discard`, so we cannot find 100 successful passes.
We can do a better job by constraining the precedant using `genFromSpec`

```
spec1 :: Specification (Int,Int,Int,Int) 
spec1 = constrained' $ \ w x y z -> [w <. x, x <. y, y <. z]

prop2:: Gen Property
prop2 = do
   (w,x,y,z) <- genFromSpec spec1
   pure $ (w < x && x < y && y < z) ==> property (w < z)
```

Now the test passes.

```
ghci> quickCheck prop2
+++ OK, passed 100 tests.
```

Now this isn't a very good test either, since the precedant is alway true. A better solution would be to
generate a mix, where the precedent is True most of the time, but sometimes False.

```
prop3:: Gen Property
prop3 = do
   (w,x,y,z) <- frequency [(9,genFromSpec spec1),(1,arbitrary)]
   pure $ (w < x && x < y && y < z) ==> property (w < z)   
```

Observe the result:

```
ghci> quickCheck prop3
+++ OK, passed 100 tests; 7 discarded.
```
This makes it possible to write conditional 'implication' properties that have a high probability of
not being vacuosly true. 


The second interpretation of the specification is as a constraint checker, implemented as the function.

```
conformsToSpec :: HasSpec a => a -> Specification a -> Bool
```

## How we solve the constraints

The strategy for generating things from `Pred`s is relatively straightforward
and relies on one key fact: any constraint that has only one free variable `x`
and where `x` occurs only once can be turned into a `Specification` for `x`.

We say that such constraints _define_ `x` and given a set of constraints `ps`
and a variable `x` we can split `ps` into the constraints that define `x` and
any constraints that don't. We can then generate a value from `x` by computing
a spec for each defining constraint in `ps` and using the `Semigroup` structure
of `Specification`s to combine them and give them to `genFromSpecT`. Once we obtain a
value for `x` we can substitute this value in all other constraints and pick
another variable to solve.

For example, given the following constraints on integers `x` and `y`

```
  x <. 10
  3 <=. x
  y <. x
```

we see that `x <. 10` and `3 <= x.` are defining constraints for `x` and there
are no defining constraints for `y`. We compute a `Specification` for `x` for each
constraint, in this case `x <. 10` turns into something like `(-∞,10)` and
`3 <=. x` turns into `[3, ∞)`. We combine the specs to form `[3, 10)` from which we
can generate a value, e.g. 4 (chosen by fair dice roll). We then substitute
`[x := 4]` in the remaining constraints and obtain `y <. 4`, giving us a defining
constraint for `y`.

### How to pick the variable order

At this point it should be relatively clear that the order we pick for the
variables matters a great deal. If we choose to generate `y` before `x` in our
example we will have no defining constraints for `y` and so we pick a value for
it freely. But that renders `x` unsolvable if `y > 9` - which will result in
the generator failing to generate a value (one could consider backtracking, but
that is very computationally expensive so _relying_ on it would probably not be
wise).

Computing a good choice of variable order that leaves the least room for error
is obviously undecidable and difficult and we choose instead an explicit
syntax-directed variable order. Specifically, variable dependency in terms is
_left-to-right_, meaning that the variables in `x + y <. z` will be solved in
the order `z -> y -> x`. On top of that there is a constraint `dependsOn y x`
that allows you to overwrite the order of two variables. Consequently, the
following constraints will be solved in the order `z -> x -> y`:

```
  x + y <. z
  y `dependsOn` x
```

A consequence of this is that it is possible to form dependency loops by
specifying multiple constraints, e.g. in:

```
  x <. y
  y <. x + 10
```

However, this situation can be addressed by the introduction of `dependsOn` to
settle the order.  It is worth noting that the choice of order in `dependsOn`
is important as it affects the solvability of the constraints (as we saw
above). We leave the choice of `dependsOn` in the example below as an exercise
for the reader.

```
  x <. y
  y <. x + 10
  0 <. x
  ? `dependsOn` ?
```

### The total definition requirement 

For the sake of efficiency we require that all constraints are dispatched as
defining constraints for a variable before we begin solving. We call this the
total definition requirement. This requirement is necessary because a set of
constraints with left over constraints are unlikely to be solvable.

Consider the following example for `p :: (Int, Int)`

```
fst_ p <. snd_ p
```

in which there is no defining constraint for `p`, which would lead us to
compute the spec `mempty` for `p` during solving - meaning we would pick an
arbitrary `p` that is irrespective of the constraints. This is problematic as
the probability of picking `p = (x, y)` such that `x <. y` is roughly `1/2`, as
you add more constraints things get much worse.

The principal problem above is that information that is present in the
constraints is lost, which would force us to rely on a `suchThat` approach to
generation - which will become very slow as constraint systems grow.

### Using Match to introduce new variables for subcomponents

A solution to the total definition requirement is to *introduce more variables*.
We can rewrite the problematic `fst p <. snd p` example below as:

```
fst_ p ==. x
snd_ p ==. y
x <. y
```

The dependency graph for these constraints will be the following:

```
x `dependsOn` y
p `dependsOn` x
```

This configuration is solvable, one picks `y` first, then picks `x <. y`
and finally constructs `p = (x, y)`.

Note that (1) we introduced more variables than were initially in the
constraints - these need to be bound somewhere - and (2) the order of
`fst p = x` is important, `p` depends on `x`,  and not the other way
around.

To do both of these things at the same time we use the `match`  construct
to the language:

```
match :: Term (a,b) -> (Term a -> Term b -> Pred) -> Pred 
```

Since `p` has type `(Term (Int,Int))` we can redo the example

```
fst_ p ==. x
snd_ p ==. y
x <. y
```

using match. This shows how to bring the new variables `x` and `y` into scope..

```
match p $ \ x y -> x <. y
```

## Overloaded types in the library

In previous sections we provided some types for several of the library functions: `constrained`, `match`, 


```
constrained:: HasSpec a => (Term a -> Pred) -> Specification a

match :: (HasSpec a, HasSpec b) =>
         Term (a,b) -> (Term a -> Term b -> Pred) -> Pred
```

It turns out, that these functions would be much more useful with more general types. This also applies to some other
library functions (`reify`, `caseOn`, etc.) we have not yet introduced. The general type of `constrained` is:

```
constrained:: (IsPred p, HasSpec a) => (Term a -> p) -> Specification a
```

This general type allows the function passed to `constrained` to be any function, that given a Term, returns any type that acts like a `Pred`.
The class IsPred is defined as follows, and has 4 instances.

```
class Show p => IsPred p where
  toPred :: p -> Pred
 
instance IsPred Bool      
instance IsPred p => IsPred [p] 
instance IsPred (Term Bool)
instance IsPred Pred 
```

Thus the following would be type-correct calls to constrained.

```
ex1 :: Specification Int
ex1 = constrained $ \ x -> True
-- Any Haskell Boolean value
                           
ex2 :: Specification Int                      
ex2 = constrained $ \ x -> x ==. lit 3
-- Any Haskell term, with type (Term Bool)

ex3 :: Specification Int  
ex3 = constrained $ \ x -> [ x <=. lit 2, x >=. lit 5 ] 
-- A list of things that act like a Pred

ex4 :: Specification Int
ex4 = constrained $ \ x -> assert $ x == lit 9
-- Anything with type Pred
```

The type of `match` is also overloaded. It supports writing specifications over type with sub-components, allowing
each sub-component to be individually constrained.

The `match` library function is used to introduce new `Term` variables for the sub-components of another type.
If `t` has type `T`, and `T` has 3 sub-components, then the `match` function would take a Haskell
lambda expression with three parameters. `(match t $ \ x y z -> ...)`. Its overloaded type is:

```
match
  :: (HasSpec a, IsProductType a, IsPred p) =>
     Term a -> FunTy (MapList Term (ProductAsList a)) p -> Pred
```

The meaning of this is a bit hard to parse: `IsProductType a`. It means the type `a` is isomorphic to a product type.
I.e. isomorphic to  `(t1,t2, ..., tn)` So all tuples would work. So would any type whose constructor had
one or more arguments, So would any type whose HasSpec instance was derived via the GHC.Generics instance. 
So in summary, if the type `a`  has **n** distinct parts, then the constraint (`IsProductType a`) is met, 
and the interpretation of the `FunTy` is a function with **n** parameters.

```
type FunTy (MapList Term (ProductAsList a)) p = t1 -> t2 -> ... -> tn -> p
``` 

# Library functions to build Term, Pred, Specification

## From Term to Pred
1.  `assert :: IsPred p => p -> Pred`

`assert` lifts a `(Term Bool)` to a `Pred`.  by using the `IsPred` class, we can often get around using it, but it becomes necessary when we want to use the `And` operator, and the operands of `And` are a mix of `Pred`, `(Term Bool), and other operations. Here is a very simple use. Further examples illustrate its use in more challenging contexts.

```
ex5 :: Specification [Int]
ex5 = constrained $ \ xs -> assert $ elem_ 7 xs
```

Note that `elem_` is the function symbol corresponding to `Data.List.elem`. 

## For all elements in a container type (List, Set, Map)
1.  `forAll :: (Forallable t a, HasSpec t, HasSpec a, IsPred p) => Term t -> (Term a -> p) -> Pred`

The library function `forAll` is used to impose a constraint on every element of a container type. There are 
three `Forallable` instances in the Base system.

```
class Forallable t e | t -> e where
instance Ord k => Forallable (Map k v) (k, v)
instance Ord a => Forallable (Set a) a
instance Forallable [a] a
```

Here is an example of its use.

```
ex6 :: Specification [Int]
ex6 = constrained $ \ xs -> 
      forAll xs $ \ x -> [ x <=. 10, x >. 1] 
```

We sample this specification using the library function `debugSpec` as follows

```
ghci> debugSpec ex6
constrained $ \ v_1 ->
  forall v_0 in v_1 $
    {assert $ v_0 <=. 10
     assert $ v_0 >. 1}
[6,3,7,4,9,10,5,6,10,5,8,3,6,7,8,5,5,6]
True
```

The library function `debugSpec` prints 3 things

1.  The internal representation of the specification.
2.  A sample solution
3.  The boolean result of testing if the sample solution, actually conforms to the spec.

If the specification fails to find a solution, it prints out an explanation of why it failed.
Most of the time this means the spec was overconstrained, and the explanation attempts to identify
the part of the specifcation that cause this ambiguity.

## Reification
1.  `reifies :: (HasSpec a, HasSpec b) => Term b -> Term a -> (a -> b) -> Pred`
2.  `reify :: (HasSpec a, HasSpec b, IsPred p) => Term a -> (a -> b) -> (Term b -> p) -> Pred`
3.  `assertReified :: (HasSpec Bool, HasSpec a) => Term a -> (a -> Bool) -> Pred`

Reification is used to produce a constrained value that can be obtained as the application of a Haskell function to
the value of another constrained term. This ensures a known relationship between the two. That relationship is exactly
the action that Haskell function computes from the value of the second `Term` to obtain the first. Unfortunately this
doesn't quite make sense as the Haskell function work on values not `Terms`.  Reification makes it possible
to use any Haskell function from `(a -> b)` as if it had type `(Term a -> Term b)`. 
This comes in several flavors, two of which have an internal (hidden)  existential constraint.

The first use: `(reifies b a f)`,  says a term `b` can be obtained from a term `a` using the Haskell function `(f :: a -> b)`.
Here is an example of its use. Internally, it works by forcing the solution of `a` before solving for `b`, applying the `f` to
the solution for `a`, and then constructing a `(Term b)` obtained from this value.

```
ex7 :: Specification (Int,[Int])
ex7 = constrained $ \ pair ->
      match pair $ \ n xs ->
      reifies n xs sum
```

Note that `sum` is a Haskell function, not a function symbol in the system that can be applied to `Term`s.
The system solves for `xs`,
then  applies `sum` to the list obtained by solving `xs`. Then binds the `Term` variable `n` to that literal value.
Here is a sample solution.

```
ghci> debugSpec ex7
constrained $ \ v_3 ->
  [to v_3/v_2]let v_1 = prodFst_ v_2 in
              let v_0 = prodSnd_ v_2 in reifies v_1 v_0
(82,[-24,22,28,19,16,24,-20,1,16])
True
```

The second operation `reify` can be defined using `reifies` by placing an existential constraint on the range of the function.
Here is an example of the use of `reify`

```
ex8 :: Specification ([Int],[Int])
ex8 = constrained $ \ pair ->
      match pair $ \ xs1 xs2 -> 
       [ assert $ sizeOf_ xs1 <=. 5
       , forAll xs1 $ \ x ->  x <=. 10
       , reify xs1 reverse $ \ t -> xs2 ==. t
       ]
```

Here we are saying there exists `t` which can be obtained by `reverse`ing `xs1` and that `xs2` equals `t`
Here is a sample solution.

```
ghci> debugSpec ex8
constrained $ \ v_4 ->
  [to v_4/v_3]let v_2 = prodFst_ v_3 in
              let v_1 = prodSnd_ v_3 in
              {assert $ sizeOf_ v_2 <=. 5
               forall v_0 in v_2 $ assert $ v_0 <=. 10
               exists v_0 in
               {reifies v_0 v_2
                Explain ["reify v_2 somef $"] $ assert $ v_1 ==. v_0}}
([9,-1,-11],[-11,-1,9])
True
```

The third operation `assertReified` can be used to place a boolean constraint on the existential.
Here is an example of its use.

```
ex9 :: Specification Int
ex9 = constrained $ \x ->
  [ assert $ x <=. 10
  , assertReified x (<= 10)
  ]
```

## Disjunction, choosing between multiple things with the same type

1.  `caseOn`, `branch`, `branchW`
2.  `chooseSpec`

Sometimes we want to choose between several different specifications for the same type.  This come in two flavors. 

-  When the choices we want to make arise from different constructors of the same (sum of products) type
-  When we have two logically distinct constraints, either of which will meet our needs.

Let's look at the first. Multiple constuctors from the same type. This uses the `caseOn` library functions and its two
helper functions `branch` (where each constructor is choosen equally) and `branchW` (where we can give weights, to determine the frequency each constructor is choosen). The type of `caseOn`

```
caseOn
  :: Term a
     -> FunTy
          (MapList
             (Weighted Binder) (Constrained.Spec.SumProd.Cases (SimpleRep a)))
          Pred
```

The way to interpret this, is that `caseOn` is a function we apply to a `(Term a)` and *n* different branches, one for each
of the constructors of type `a`. Each branch is either weighted (using `(weight, branchW ...)` or
unweighted using `(branch ...)`, where the `...` is a function with *m* parameters, one for each of the 
subcomponents of that constructor). First we introduce a sum of products type `Three` and use the GHC.Generics
instance to derive the HasSpec instance.

```
data Three = One Int | Two Bool | Three Int deriving (Ord, Eq, Show, Generic)
instance HasSimpleRep Three
instance HasSpec Three
```

Here is an example using the unweighted mechanism.

```
ex10 :: Specification Three
ex10 = constrained $ \ three ->
       caseOn three
          (branch $ \ i -> i ==. 1)          -- One
          (branch $ \ b -> assert (not_ b))  -- Two
          (branch $ \ j -> j ==. 3)          -- Three
```

Note the trailing Haskell comments, they remind us which constructor we are dealing with. The system
expects the branches to be in the same order the constructors are introduced in the `data` defintions for `Three`

Here is another example using the weighted mechanism. A sample solution of this spec can be found in the
section about `monitor` in a later section.

```
ex11 :: Specification Three
ex11 = constrained $ \ three ->
       caseOn three
          (branchW 1 $ \ i -> i <. 0)        -- One, weight 1
          (branchW 2 $ \ b -> assert b)      -- Two, weight 2
          (branchW 3 $ \ j -> j >. 0)        -- Three, weight 3
```

The second way to specify disjunctions is to choose `chooseSpec`, where the two choices use the same
type, but are distinguished logically by the two input specifications. The type of
`chooseSpec` is as follows, where the `Int` determines the frequency of each choice.

```
chooseSpec:: HasSpec a => (Int, Specification a) -> (Int, Specification a) -> Specification a
```

Here is an example.

```
ex12 :: Specification (Int,[Int])
ex12 = chooseSpec 
         (5, constrained $ \ pair ->
             match pair $ \ total xs -> [ total >. lit 10, sum_ xs ==. total , sizeOf_ xs ==. lit 3])
         (3, constrained $ \ pair -> 
             match pair $ \ total xs -> [ total <. lit 10, sum_ xs ==. total, sizeOf_ xs ==. lit 6])
```

Here are two samples, one using the first spec (total > 10 and size == 3), the other using the
second spec (total < 10 and size == 6). We have elided printing the internal representation (using ...)
because it is rather large.

```
ghci> debugSpec ex12
constrained $ \ v_5 -> ...
(41,[15,3,23])
True

ghci> debugSpec ex12
constrained $ \ v_5 -> ...
(9,[3,0,0,2,2,2])
True
```

## Primed library functions which are compositions with match

Some library functions introduce a new term variable using a Haskell lambda-abstraction. If that variable
has sub-components, then we often need to `match` that variable in order to introduce new term
variables for each of those components. The following library functions make that easier, because they are
predefined to compose the unprimed base library functions with `match`

1.  `forAll'`
2.  `constrained'`
3.  `reify'`

Here are three examples using these primed library functions. Each example has two parts. The first part
defined interms of the un-primed function and `match`, and the second defined nterms of the primed librbary function.

The primed version of `forAll` is `forAll'`

```
ex13a :: Specification [(Int,Int)]
ex13a = constrained $ \ xs -> 
      forAll xs $ \ x -> match x $ \ a b -> a ==. negate b 

ex13b :: Specification [(Int,Int)]
ex13b = constrained $ \ xs -> 
      forAll' xs $ \ a b -> a ==. negate b  
``` 

The primed version of `constrained` is `constrained'`

```
ex14a :: Specification (Int,Int,Int)
ex14a = constrained $ \ triple ->
        match triple $ \ a b c -> [ b ==. a + lit 1, c ==. b + lit 1]

ex14b :: Specification (Int,Int,Int)
ex14b = constrained' $ \ a b c -> [ b ==. a + lit 1, c ==. b + lit 1]        
```

The primed version of `reify` is `reify'`


```
ex15a :: Specification (Int,Int,Int)
ex15a = constrained $ \ triple ->
          match triple $ \ x1 x2 x3 -> 
            reify x1 (\ a -> (a+1,a+2)) $ \ t ->  
               match t $ \ b c -> [x2 ==. b, x3 ==. c]

ex15b :: Specification (Int,Int,Int)
ex15b = 
  constrained $ \ triple ->
    match triple $ \ x1 x2 x3-> 
      reify' x1 (\ a -> (a+1,a+2)) $ \ b c -> [x2 ==. b, x3 ==. c]   
```

## Constructors and Selectors
1.  `onCon`
2.  `sel`
4.  `isJust`

In Haskell we can define data types with multiple constructors, and constructors with multiple sub-components.
The library functions `onCon`, `sel`, and `isJust`, allow us to constrain such types in a way less verbose
than using the `caseOn` library function. Consider the following

```
ex16 :: Specification Three
ex16 = constrained $ \ three ->
       caseOn three
          (branchW 1 $ \ i -> i ==. lit 1)       -- One, weight 1
          (branchW 2 $ \ b -> assert (not_ b))   -- Two, weight 2
          (branchW 3 $ \ j -> j ==. 3)           -- Three, weight 3
```

We can express the same constraints using the `OnCon` library function. To use `OnCon` one must
define the `HasSpec` instance for the type using the `GHC.Generics` instance mechanism (see the example of the type `Three` in the *Disjunction* section). This makes sure the
system knows about the constructors and selectors of the type. To use `onCon` one must 
type apply it to a String that names one of the constructors. Like this `(onCon @"One" ...)` . 
This requires that the GHC language directive`{-# LANGUAGE DataKinds #-}` be in the source file.
Then apply it to a Term with the type returned by that constructor, followed by a Haskell function with one parameter
for each subcomponent of that constructor, that returns a Pred. Here is `ex16` redone using three `onCon` predicates.

```
ex17:: Specification Three
ex17 = constrained $ \ three -> 
    [ onCon @"One" three (\ x -> x==. lit 1)
    , onCon @"Two" three (\ x -> not_ x)
    , onCon @"Three" three ( \ x -> x==. lit 3)
    ]
```

The real power of `onCon` is when you only want to constrain one (or a subset) of the constructors of a type,
and the other constructors remain unconstrained. Here we only constrain the constructor `Three` and the constructors
`One` and `Two` remain unconstrained.

```
ex18:: Specification Three
ex18 = constrained $ \ three ->
       onCon @"Three" three ( \ x -> x==. lit 3)
```

Here is another example where we only constrain the constructor `Just` of the maybe type.
```
ex19 :: Specification (Maybe Bool)
ex19 = constrained $ \ mb -> onCon @"Just" mb (\ x -> x==. lit False)
```

Haskell allows the definition of data types with named selectors. Here is an example.

```
data Dimensions 
  where Dimensions :: 
          { length :: Int
          , width :: Int
          , depth :: Int } -> Dimensions
     deriving (Ord, Eq, Show, Generic)
instance HasSimpleRep Dimensions
instance HasSpec Dimensions
```

This introduces Haskell functions with types

```
length :: Dimensions -> Int
width :: Dimensions -> Int
depth :: Dimensions -> Int
```

If we use the `GHC.Generics` path to derive the `HasSimpleRep` and the `HasSpec`
instances, the we can use the `sel` library function to create lifted versions of
the Haskell selector functions like this.

```
width_ :: Term Dimensions -> Term Int
width_ d = sel @1 d
```

This requires the `DataKinds` directive, and importing `GHC.Generics` and `GHC.TypeLits` to work.

```
{-# LANGUAGE DataKinds #-}
import GHC.Generics
import GHC.TypeLits
```

When we type-apply the library function `sel` to a type-level `Natural` number like this `(sel @1)` it
selects the `ith` selector function. The selectors are numbered from `0` to `n-1` . Selection 
can always be expressed using `match` like this:

```
ex20a :: Specification Dimensions
ex20a = constrained $ \ d ->
        match d $ \ l w d -> [ l >. lit 10, w ==. lit 5, d <. lit 20]
```

which can be reexpressed using `sel` as this.

```
ex20b :: Specification Dimensions
ex20b = constrained $ \ d ->
        [sel @0 d >. lit 10
        ,sel @1 d ==. lit 5
        ,sel @2 d <. lit 20]     
```

When we wish to constrain just a subset of the subcomponents, selectors make it possible 
to write more concise `Specification`s.

```
ex21 :: Specification Dimensions
ex21 = constrained $ \ d -> width_ d ==. lit 1
```


## Naming introduced lambda bound Term variables
1.  [var|name|]

When we use a library function that introduces new Term variable using a Haskell lambda expression, the system
gives the Haskell variable a unique Term level name such as `v0` or `v1`  or `v2` etc. When the specification is
over constrained, the system attempts to explain what is wrong, but when doing this, it uses the internal 
Term level names that were assigned to the Haskell variables. This means the error messages are often hard to
understand. For example consider the over constrained specification. It is over constrained because
`left` cannot be simultaneously equal to `right` and `right + lit 1`

```
ex22a :: Specification (Int,Int)
ex22a = constrained $ \ pair -> 
       match pair $ \ left right ->
	      [ left ==. right, left ==. right + lit 1]
```

When attempt to solve this specitication for `pair` we get the following error message

```
debugSpec ex22a
StepPlan for variable: v_1 fails to produce Specification, probably overconstrained. 
Original spec ErrorSpec
  Intersecting: 
    MemberSpec [20]
    MemberSpec [19]
  Empty intersection
```

Note the error message is in terms of the internal Term name `v1`. Which is not very useful.

To make error messages clearer we can name Haskell lambda bound variables using `[var|left|]` instead of just `left`.  In order to do this we must have the following directives in our file.

```
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ViewPatterns #-}
```

Here is the same example using this naming schema.

```
ex22b :: Specification (Int,Int)
ex22b = constrained $ \ [var|pair|] -> 
         match pair $ \ [var|left|] [var|right|] -> [ left ==. right, left ==. right + lit 1]
```

We now get a better error message

```
ghci> debugSpec ex22b
StepPlan for variable: left_1 fails to produce Specification, probably overconstrained. 
Original spec ErrorSpec
  Intersecting: 
    MemberSpec [29]
    MemberSpec [28]
  Empty intersection
```


## Existential quantifiers
1.  `exists`
2.  `unsafeExists`


Sometimes we want to constrain a variable, in terms of another internal or hidden variable.
A classic example is constraining a number to be odd.  A number `x` is odd, if there exists another internal number
`y`, such that, `x` is equal to `(y + y + 1)`


Here is an example.

```
ex22 :: Specification Int
ex22 = constrained $ \ [var|oddx|] ->
         unsafeExists
         (\ [var|y|] -> [Assert $ oddx ==. y + y + 1])
```

Why do we call the library function `unsafeExists` ? It is unsafe because function `conformsToSpec`
will fail to return `True` when called on a generated result. Why?  Because the system does not know how
to find the internal, hidden variable. To solve this the safe function `exists` takes two Haskell
lambda expressions.  The first is a function that tells how to compute the hidden variable from the
values returned by solving the constraints. The second is the normal use of a Haskell 
lambda expression to introduce a Term variable naming the hidden variable. Here is an example.

```
ex23 :: Specification Int
ex23 = ExplainSpec ["odd via (y+y+1)"] $
       constrained $ \ [var|oddx|] ->
         exists
		   -- first lambda, how to compute the hidden value for 'y'
           (\eval -> pure (div (eval oddx - 1) 2))
		   
		   -- second lambda, introduces name for hidden value 'y'
           (\ [var|y|] -> [Assert $ oddx ==. y + y + 1]) 
```

One might ask what is the role of the parameter `eval` in the first lambda expression passed to `exists`.
Recall that we are trying assist the function `conformsToSpec` in determining if a completely known value
conforms to the specification. In this context, every Term variable is known. But in the specification we
only have variables with type `(Term a)`,  what we need are variables with type `a`. The `eval` functional parameter
has type

```
eval :: Term a -> a
```

This, lets us access those values. In the example the Term variable `(oddx :: Term Int)` is in scope, so
`(eval oddx :: Int)`, and since this is only used when calling `conformsToSpec`, we actually have the
value of the `oddx` for `eval` to return. Finally `(div (eval oddx - 1) 2))` tells us how to compute 
the value of the hidden variable. 


## Conditionals
1.  `whenTrue`
2.  `ifElse`

If one has a term `x` with type `(Term Bool)` one could use the value of this term to
define a Specification conditional on this term by using the `(caseOn x ...)` library function.
There are two more concise library function one can use instead.

```
whenTrue :: IsPred p => Term Bool -> p -> Pred
ifElse :: (IsPred p, IsPred q) => Term Bool -> p -> q -> Pred
```

For example consider the data type `Rectangle` where the selector `square` indicates
that the rectangle has equal length width and length.

```
data Rectangle = Rectangle { wid :: Int, len :: Int, square :: Bool} 
    deriving (Show, Eq,Generic)
instance HasSimpleRep Rectangle
instance HasSpec Rectangle
```

We can enforce this in a specification as follows. 

```
ex26 :: Specification Rectangle
ex26 = constrained' $ \ wid len square -> 
 [ assert $ wid >=. lit 0
 , assert $ len >=. lit 0
 , whenTrue square (assert $ wid ==. len)
 ]
 ```

Note that there are no constraints relating `wid` and `len` if the selector `square` is `False`
The library function `ifElse` allows two separate sets of constraints, one when the
`square` is `True` and a completely separate set when `square` is `False`

```
ex27 :: Specification Rectangle
ex27 = constrained' $ \ wid len square -> 
  ifElse square
         (assert $ wid ==. len)
         [ assert $ wid >=. lit 0
         , assert $ len >=. lit 0 ]
```

## Explanations
1.  `assertExplain`
2.  `explanation`
3.  `ExplainSpec`

Explanations allow the writer of a specification to add textual message, that can be used 
if solving a specification fails. These library functions have type

```
explanation   :: NonEmpty String -> Pred -> Pred
assertExplain :: IsPred p => NonEmpty String -> p -> Pred
ExplainSpec   :: [String] -> Specification a -> Specification a
```

Here is a specification with no explanations

```
ex28a :: Specification (Set Int)
ex28a = constrained $ \ s ->
       [ assert $ member_ (lit 5) s
       , forAll s $ \ x -> [ x >. lit 6, x <. lit 20]
       ]
```
This specification is over constrained. Here is what the error message returns.

```
ghci> debugSpec ex28a
Some 'must' items do not conform to 'element' spec: TypeSpec [7..19] []
While combining 2 SetSpecs
  (SetSpec must=[] elem=TypeSpec [7..19] [] size=TrueSpec @(Integer))
  (SetSpec must=[ 5 ] elem=TrueSpec @(Int) size=TrueSpec @(Integer))
```

By adding an `ExplainSpec` like this

```
ex28b :: Specification (Set Int)
ex28b = ExplainSpec ["5 must be in the set"] $
        constrained $ \ s ->
       [ assert $ member_ (lit 5) s
       , forAll s $ \ x -> [ x >. lit 6, x <. lit 20]
       ] 
```

we get a better explanation

```
ghci> debugSpec ex28b
5 must be in the set

Some 'must' items do not conform to 'element' spec: TypeSpec [7..19] []
While combining 2 SetSpecs
  (SetSpec must=[] elem=TypeSpec [7..19] [] size=TrueSpec @(Integer))
  (SetSpec must=[ 5 ] elem=TrueSpec @(Int) size=TrueSpec @(Integer))
```

```
ex28c :: Specification (Set Int)
ex28c = 
        constrained $ \ s -> explanation (pure "5 must be in the set")
       [ assert $ member_ (lit 5) s
       , forAll s $ \ x -> [ x >. lit 6, x <. lit 20]
       ] 
```

Where to add explanations is some what of an art. In many cases explanations are simply
lost. We are looking for examples where explanations get lost so that we can improve the system.

## Operations to define and use Specifications
1.  `satisfies`
2.  `equalSpec`
3.  `notEqualSpec`
4.  `notMemberSpec`
5.  `leqSpec` 
6.  `ltSpec`
7.  `geqSpec`
8.  `gtSpec`
5.  `cardinality`

There are a number of library functions that create specifications directly
and do not use the `constrained` library function. These are particulary useful
in conjunction with the library function `satisfies` which converts a `Specification` into a `Pred`

```
satisfies :: HasSpec a => Term a -> Specification a -> Pred
equalSpec :: a -> Specification a
notEqualSpec :: HasSpec a => a -> Specification a
notMemberSpec :: (HasSpec a, Foldable f) => f a -> Specification a
leqSpec :: OrdLike a => a -> Specification a
ltSpec :: OrdLike a => a -> Specification a
geqSpec :: OrdLike a => a -> Specification a
gtSpec :: OrdLike a => a -> Specification a
cardinality :: (.Number Integer, HasSpec a) => Specification a -> Specification Integer
```

Here is an example of the use of `satisfies` in conjunction with `notMemberSpec`

```
ex29 :: Specification Int
ex29 = constrained $ \ x ->
       [ assert $ x >=. lit 0
       , assert $ x <=. lit 5
       , satisfies x (notMemberSpec [2,3]) -- meaning (x /= 2) and (x /= 3)
```
 
In essence this spec says `x` is either  `0`, `1`, `4`, or `5`, where
`2` and `3` are excluded by the `notMemberSpec`

## Utility functions
1.  `simplifyTerm`
2.  `simplifySpec`
3.  `genFromSpecT`
4.  `genFromSpec`
5.  `genFromSpecWithSeed`
6.  `debugSpec`

These functions are generally useful when writing and debugging specifications.

```
-- | Simplify a Term
simplifyTerm :: Term a -> Term a

-- | Simplify a Spec, sometimes makes big simplications
simplifySpec :: HasSpec a => Specification a -> Specification a

-- Generate a value from the Spec in the internal monad GenT
genFromSpecT
  :: (HasSpec a,MonadGenError m) =>
     Specification a -> GenT m a

-- Generate a value from the spec in the QuickCheck monad 'Gen'
genFromSpec
  :: (GHC.Stack.Types.HasCallStack, HasSpec a) =>
     Specification a -> QuickCheck.Gen a

-- | generate a value from a Spec using a seed (reproduceable)
genFromSpecWithSeed
  :: HasSpec a => Int -> Int -> Specification a -> a

-- | Run a Spec in the IO monad, prints helpful intermediate information
debugSpec :: HasSpec a => Specification a -> IO ()

```


## Escape Hatch to QuickCheck Gen monad
1.  `monitor`

The function `forAllSpec` allows one to turn a  `Specification` into a QuickCheck  `Property`

```
forAllSpec :: 
  (HasSpec a, QuickCheck.Testable p) =>
  Specification a -> (a -> p) -> QuickCheck.Property
```

The librrary function `monitor` allows specification writers to access some of the QuickCheck property
modifiers,  like  `classify`,  `label`,  and  `cover`,  by turning  them into a  `Pred` using `monitor`

```
monitor
  :: ((forall a. Term a -> a) -> QuickCheck.Property -> QuickCheck.Property)
     -> Pred
```

The monitor `Pred` has no effect,  unless the  `Specification` that embeds the `monitor` call,  is lifted
to a  `Property` using  `forAllSpec.  Here is a example.

```
ex30 :: Specification (Int, Int)
ex30 = constrained $ \ [var|p|] ->
  match p $ \ [var|x|] [var|y|] ->
    [ assert $ x /=. 0
    , -- You can use `monitor` to add QuickCheck property modifiers for
      -- monitoring distribution, like classify, label, and cover, to your
      -- specification
      monitor $ \eval ->
        QuickCheck.classify (eval y > 0) "positive y"
          . QuickCheck.classify (eval x > 0) "positive x"
    ]
```
The `monitor` library function use the same programming device as  `reify`,  it accepts a function
(with an functional argument, usually called  `eval`,  that turns a `(Term a)` into a value `a`. 
Thus each successfull generation of the `Specification`  lifts  one or more  `Specification` 
variables  ( `x` and `y` in the example) to a value that is then 
passed to the QuicCheck modifier  (`classify` in the example).  Then `forAllSpec` turns the modifier on.
Here are two examples.  The first just runs the specification using `debugSpec`, since this is not inside a call to
`forAllSpec`,  no  modification is created.

```
ghci> debugSpec ex30
constrained $ \ v_3 ->
  [to p_3/v_2]let v_1 = prodFst_ v_2 in
              let v_0 = prodSnd_ v_2 in
              {assert $ not_ (x_1 ==. 0)
               monitor}
(-12,-15)
True
```

The second example uses `forAllSpec` to create a `Property`,  that passes as long as the generator does not fail.

```
prop31 :: QuickCheck.Property
prop31 = forAllSpec ex30 $ \_ -> True
```	
	
We can now use `QuicCheck.quickCheck` to test the property.	
	 
```
ghci> QuickCheck.quickCheck $ prop31
+++ OK, passed 100 tests:
52% positive x
46% positive y
```

Where the `classify` statistics are reported.

As a final example we redo the weighted branch example `ex11` from the discussion of `caseOn` above.
We modify it by adding a `monitor` predicate. The purpose of this example is two fold.

1.  Illustrate the use of `monitor`
2.  Demonstrate that the weighted `caseOn` changes the frequency at which the branches are choosen. 

```
ex11m :: Specification Three
ex11m = constrained $ \ three ->
       [ caseOn three
          (branchW 1 $ \ i -> i <. 0)        -- One, weight 1
          (branchW 2 $ \ b -> assert b)      -- Two, weight 2
          (branchW 3 $ \ j -> j >. 0)        -- Three, weight 3
       , monitor $ \eval ->
         case (eval three) of
           One _ ->  QuickCheck.classify True "One should be about 1/6"
           Two _ ->  QuickCheck.classify True "Two should be about 2/6"
           Three _ ->  QuickCheck.classify True "Three should be about 3/6"
       ]

propex11 :: QuickCheck.Property
propex11 = forAllSpec ex11m $ \_ -> True   

ex33 = QuickCheck.quickCheck $ propex11
```

If we run `quickCheck` on `propex11` we get the following results. We run it several times to illustrate
that the weights are some what random, but do change the frequency.

```
ghci> ex33
+++ OK, passed 100 tests:
45% Three should be about 3/6
38% Two should be about 2/6
17% One should be about 1/6

ghci> ex33
+++ OK, passed 100 tests:
63% Three should be about 3/6
21% Two should be about 2/6
16% One should be about 1/6

ghci> ex33
+++ OK, passed 100 tests:
52% Three should be about 3/6
36% Two should be about 2/6
12% One should be about 1/6

```

# Strategy for constraining a large type with many nested sub-components.

When writing a `Specification` for a complex type with deeply nested subcomponents, the first thing that we need
to do, is introduce a `Term` variable for each subcomponent that needs to be constrained. A good way to start is
to build a skeleton `Specification` that does nothing more that just bring into scope a variable for each 
(possibly nested) subcomponent.  We build a deeply nested example by building on top of the types from 
previous examples `Three` and  `Rectangle`.    We introduce a new deeply nested type `Nested`

```
data Nested = Nested Three Rectangle [Int]
  deriving (Show,Eq,Generic)
instance HasSimpleRep Nested
instance HasSpec Nested  
```

It is important that we derive `Show`, `Eq` and `Generic`,  as these are needed to automatically derive the 
`HasSimpleRep` and `HasSpec` instances.  Now we write a `skeleton` specification. The process is to
introduce a  `Term` variable for each nested component.  We use the library functions
`constrained`, `match`, `caseOn` and  `forAll`,  each of which takes a Haskell lambda expression,
introducing a Haskell variable that is bound to a `Term` varaible for each component.  The goal in
this stage  is to just introduce a varaible for each subcomponent. So we use `TruePred` which places 
no constraints on each of these subcomponents. We use named variables, and use a comment to label each `branch` 
with the name of the construtor.

```
skeleton :: Specification Nested
skeleton = constrained $ \ [var|nest|] ->
           match nest $ \ [var|three|] [var|rect|] [var|line|] ->
              [ (caseOn three)
                   (branch $ \ i -> TruePred)     -- One
                   (branch $ \ b -> TruePred)     -- Two
                   (branch $ \ j -> TruePred)     -- Three
              , match rect $ \ [var|wid|] [var|len|] [var|square|] -> [ TruePred ]
              , forAll line $ \ [var|point|] -> TruePred
              ]
```

Once we have the `skeleton` compiling, we can replace each `TruePred` with some constraints.
In this stage, we worry just about the constraints,  and which `Pred` to use, and not about how we bring
a variable into scope for each subcomponent.  Experience show that we have way fewer compiler 
errors, using this one step at a time strategy.

# Writing HasSpec instances by hand.

The `HasSpec` constraint tells us that a particular type has been admitted to the system, and that we can now 
write constraints for that type. We have used four different strategies for writing HasSpec instances. We list
them here in increasing difficulty for the programmer, when writing an instance `(HasSpec T)` for some
new datatype `T`.

1.  Use a derived `GHC.Generic` instance for `T` to define a default `SimpleRep` instance. This uses automatic
means to define the `(SimpleRep T)` instance as a Sum-of-Products type.  Then this Sum-of-Products type is used 
to make a `(HasSpec T)` instance, using the default instances on Sum-Of-Products for the methods of `(HasSpec T)`.

2.  Write our own `(SimpleRep T)` instance.  We need to this when the type `T` has some invariants not captured 
by the stuctural description captured by Sum-Of-Products type. This requires a good understanding of the
relationship between the `SimpleRep` and `HasSpec` instances, and some of the internals of the system.

3.  Define the `(SimpleRep T)` instance in terms of another type `S` that already has a HasSpec instance.
This method works by thinking of the `T` as a newtype-like copy of  `S`, and uses the `(match t)` library function
to bring into scope a variable of type `(Term S)`, and to then express the constraints in terms of `S`.
A classic example of this to define `(StrictSeq a)` in terms of `( [a] )`. This is possible because
the abstract properties of sequences and lists are the same.  They only differ in terms of efficiency
on different function symbols. But the system constraints can only express things about the abstract properties, 
not efficiency. 

4.  Bypass the SimpleRep pathway, by defining the `(HasSpec T)` instance directly. This requires
defining the associated type family `TypeSpec` and all the methods of the `HasSpec` class.  This is the method used 
to define instances `(HasSpec [a])`,  `(HasSpec (Set a))` , and  `(HasSpec (Map k ))`. These are all abstract 
types that have many properties not captured by the simple structural properties embedded in the types 
constructor functions.  Most of these properties are captured by relationships between the different
function symbols supported by that type. This is the most difficult pathway, but it can capture complicated
relationships on the type not possible using the other strategies. This requires a deep understanding of 
the type `T`, its function symbols and their relationships, and internals of the system. This is a comlex task, 
but it gives the system much of its power, as it makes the system extendible, to additional complex
types, simply by adding a new `HasSpec` instance.

## Strategy 1 using GHC.Generics

This is the strategy we used in this manual for the types `Three`, `Dimensions`, `Rectangle`,  and `Nested`. We will 
not give another example here, but reiterate the requirement that the new type -must- have `Generic`, `Eq`, and `Show` instances,
so do not forget to put the deriving clause `deriving (Generic,Eq,Show)` in the data definition.

## Strategy 2 writing your own SimpleRep instance

This strategy depends on the provided `(SimpleRep T)` associated type family instance, being an actual Sum-of-Products type.
This requires quite a bit of knowledge about the internals of the system. Let's look closely at the `SimpleRep` class

```
class Typeable (SimpleRep a) => HasSimpleRep a where
  type SimpleRep :: * -> *
  type family SimpleRep a
  toSimpleRep :: a -> SimpleRep a
  fromSimpleRep :: HasSimpleRep a => SimpleRep a -> a
```

What kind of type lends itself to this strategy? 
1.  A type that has internal structure that enforces some internal invariants.
2.  A type that has a -builder- function, that takes simple input, and constructs the internal struture.
3.  A type that has an -accessor- function, that takes the internal structure, and returns the simple input.
4.  A type where the -simple input- has a Sum-of-Products representation.

Often the -builder- function is implemented as a Haskell Pattern.  Here is an example that come from the Cardano Ledger.
A lot of complicated stuff is not fully describe here, but the example gives an overview of how it works.


```
-- NOTE: this is a representation of the `ShelleyTxOut` type. You can't
-- simply use the generics to derive the `SimpleRep` for `ShelleyTxOut`
-- because the type is memoized. So instead we say that the representation
-- is the same as what you would get from using the `ShelleyTxOut` pattern.
type ShelleyTxOutTypes era =
  '[ Addr
   , Value era
   ]
instance (Era era, Val (Value era)) => HasSimpleRep (ShelleyTxOut era) where
  type TheSop (ShelleyTxOut era) = '["ShelleyTxOut" ::: ShelleyTxOutTypes era]
  toSimpleRep (ShelleyTxOut addr val) =
    inject @"ShelleyTxOut" @'["ShelleyTxOut" ::: ShelleyTxOutTypes era]
      addr
      val
  fromSimpleRep rep =
    algebra @'["ShelleyTxOut" ::: ShelleyTxOutTypes era] rep ShelleyTxOut

instance (EraTxOut era, HasSpec (Value era)) => HasSpec (ShelleyTxOut era)
```

## Strategy 3 defining the SimpleRep instance in terms of another type with a SimpleRep instance

The type `Coin` is a defined as

```
newtype Coin = Coin {unCoin :: Integer}
```
The operations on Coin ensure the invariant that one cannot build a `Coin` with a negative value.
We can enforce these invariants in a `SimpleRep` instance by making the `SimpleRep` type be a `Word64`.
`Word64` is one of the numeric types, and has its own `SimpleRep` instance, so it is a good choice.
Here is the `SimpleRep` instance, and the `HasSpec` instance defined using that representation.

```
instance HasSimpleRep Coin where
  type SimpleRep Coin = Word64
  toSimpleRep (Coin i) = case integerToWord64 i of
    Nothing -> error $ "The impossible happened in toSimpleRep for (Coin " ++ show i ++ ")"
    Just w -> w
  fromSimpleRep = word64ToCoin
instance HasSpec Coin
```

To write a `Specification` for `Coin` we simply `match` against it, and then use the operations on the underlying
type `Word64` to constraint it.

```
ex34 :: Specification Coin
ex34 = constrained $ \ coin ->
       match coin $ \ w64 -> [w64 >=. lit 100, w64 <=. lit 200]
```	

Here we use `debugSpec` to get a sample solution

```
ghci> debugSpec ex34
constrained $ \ v_1 ->
  [to v_1/v_0]{assert $ v_0 >=. 100
               assert $ v_0 <=. 200}
Coin {unCoin = 119}
```

## Strategy 4, bypassing SimpleRep, and write the HasSpec instance by Hand

The `HasSpec` class has an associated type family and many methods. Here is a summary of some of it.

```
class (Typeable a, Eq a, Show a, Show (TypeSpec a), Typeable (TypeSpec a)) => HasSpec a where
  -- | The `TypeSpec a` is the type-specific `Specification a`.
  type TypeSpec a
 
  -- the default TypeSpec type, if one is not given. This is how 
  -- SimpleRep is used to automatically make a HasSpec instance.
  type TypeSpec a = TypeSpec (SimpleRep a)

  -- `TypeSpec` behaves sort-of like a monoid with a neutral
  -- element `emptySpec` and a `combineSpec` for combining
  -- two `TypeSpec a`. However, in order to provide flexibilty
  -- `combineSpec` takes two `TypeSpec` and constucts a `Specification`. This
  -- avoids e.g. having to have a separate implementation of `ErrorSpec`
  -- and `MemberSpec` in `TypeSpec`.

  emptySpec :: TypeSpec a
  combineSpec :: TypeSpec a -> TypeSpec a -> Specification a

  -- | Generate a value that satisfies the `TypeSpec`.
  -- The key property for this generator is soundness:
  --  ∀ a ∈ genFromTypeSpec spec. a `conformsTo` spec
  genFromTypeSpec :: (HasCallStack, MonadGenError m) => TypeSpec a -> GenT m a

  -- | Check conformance to the spec.
  conformsTo :: HasCallStack => a -> TypeSpec a -> Bool

  -- | Shrink an `a` with the aide of a `TypeSpec`
  shrinkWithTypeSpec :: TypeSpec a -> a -> [a]

  -- | Convert a spec to predicates:
  -- The key property here is:
  --   ∀ a. a `conformsTo` spec == a `conformsTo` constrained (\t -> toPreds t spec)
  toPreds :: Term a -> TypeSpec a -> Pred

  -- | Compute an upper and lower bound on the number of solutions genFromTypeSpec might return
  cardinalTypeSpec :: TypeSpec a -> Specification Integer
```



# A look into the internals of the system.

1.  Generics
2.  The `GenT` monad
3.  The `Syntax`, `Semantics` and `Logic` classes
4.  Overloaded numeric types
5.  `Size` and `Cardinality`
6. Mutual recursion between `Specification`, `HasSpec`, `Generics`, Bool and Integer `HasSpec` instances
7. Monoid and Semigroup-like behavior of  `Specification` 
8. `Explanations` and the `GenT` monad
