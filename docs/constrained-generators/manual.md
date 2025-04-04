
# Constrained Generators Manual


## Constrained Generators is a First-Order Logic


A First-order typed logic has 4 components

1. **Terms** consisting of 
  - Variables (i.e. x), 
  - Constants (i.e. 5), 
  - Applications (ie. F x 5). Applications, apply a function symbol to a list of arguments: (i.e.  FunctionSymbol term1 .. termN)
2. **Predicates**   (x == 5)
3. **Connectives**  (And, Or, Not, =>, ...)
4. **Quantifiers**  (Forall, Exists)

The **Constrained generators** system allows programmers to write Haskell programs with type `(Specification T)` that denotes a set of random values for the type `T`, that are subject to a set of constraints. This supports property based testing where a completely random set of values may not be useful.

The **Constrained Generators** system is implemented as an embeded domain specific language in Haskell. The Terms, Predicates, Connectives,
and Quantifiers of the first order logic are embedded into three Haskell datatypes (`Specification t`, `Term t`, and `Pred`). There is a rich (extendable) library
of Haskell functions that can be used to define and construct values of these types.  The library is implemented in a such a way, 
that the four parts of the logic are defined in ways that are similar to Haskell expressions with type Bool. 

Let us look at a simple example, and study how this is done. Below is a Haskell declaration that defines a specification of a pair of Int (`p`) , subject to the constraint that the first component (`x`) is less than or equal to the second component (`y`) plus 2

```
leqPair :: Specification (Int, Int)
leqPair = constrained $ \ p ->
   match p $ \ x y ->
     assert (x <=. (y + lit 2))
```	


The library uses Haskell lambda expressions to introduce variables in the Term language of the system, and Haskell functions 
to build Terms and Predicates. The Haskell function `lit` takes Haskell values and turns them into constants 
in the Term language. The types of the Haskell functions used in the above definitions are

```
constrained :: HasSpec a => (Term a -> Pred) -> Specification a

match :: (HasSpec a, HasSpec b) => Term (a,b) -> (Term a -> Term b -> Pred) -> Pred

lit :: HasSpec a -> a -> Term a

assert :: Term Bool -> Pred

(<=.) :: OrdLike a => Term a -> Term a -> Term Bool
```

The Haskell Constraint `(HasSpec a)` states that the type `a` has been admitted to the system as one of the types 
that can be subject to constraints. The system comes with a large set of types that are already admitted, including

1. Bool
2. Tuples
3. Sums
4. Numberic types (Int,Integer, Natural, Int8, Int16, Int32, Int64, Word8, Word16, Word32, Word62)
5. Lists
6. Maps
7. Sets
8. Trees
9. Maybe
10. Either
11. ()

### HasSpec instances

Haspec instances can always be added to admit more types. Any type with a `GHC.Generics(Generic)` instance can be 
given a default instance by using its Sum-of-Products generic definition. In the Cardano Ledger System 
over 200 types have been given HasSpec instances, either by using the GHC.Generics path, or by writing the instances by hand.

### Building logic specifications using Haskell functions

Note that `constrained` and `match` take functions, which abstract over terms, and return `Specification` and `Pred`. 
Using the libray functions, variables in the Term language are always introduced using Haskell lambda abstractions. And the lbrary
functions combine these into Terms, Preds, and Specifications.

### Another example using conjunction and simple arithmetic

Suppose we want to put more than one simple condition on the pair of Ints. We would do that using the connective `And` that converts a `[Pred]` into a `Pred`

```
sumPair :: Specification (Int, Int)
sumPair = constrained $ \ p ->
  match p $ \ x y ->
    And [ assert $ x <=. y
        , assert $ y <=. 20 
        , assert $ x + y ==. 25 ]
```	

This example also re-illustrates that `(Term Int)` has a Num instance, and that we can constrain multiple (different)
variables using simple arithmetic conditions. Note the new operator: `(==.) :: (Eq n, HasSpec n) => Term n -> Term n -> Term Bool` 

### Function Symbols

Note that `(<=.)` , and `(==.)` are two of the function symbols in the first order logic. They obey a 
useful naming convention. Infix function symbols corresponding to Haskell infix operators have 
corresponding infix operators,  lifting Haskell infix functions with type `(a -> b -> c)`, to library infix 
functions which have analogous types `(Term a -> Term b -> Term c)`
and are named using the convention that we add the dot `(.) to the end of the Haskell operator.

A similar naming convention holds for prefix function symbols, except instead of adding a
dot to the end of the Haskell name, we add an underscore `(_) to the end of the Haskell prefix functions's name. Some examples follow.

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

# How we solve the constraints

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
  x < 10
  3 <= x
  y < x
```

we see that `x < 10` and `3 <= x` are defining constraints for `x` and there
are no defining constraints for `y`. We compute a `Specification` for `x` for each
constraint, in this case `x < 10` turns into something like `(-∞,10)` and
`3 <= x` turns into `[3, ∞)`. We combine the specs to form `[3, 10)` from which we
can generate a value, e.g. 4 (chosen by fair dice roll). We then substitute
`[x := 4]` in the remaining constraints and obtain `y < 4`, giving us a defining
constraint for `y`.

## How to pick the variable order

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
_left-to-right_, meaning that the variables in `x + y < z` will be solved in
the order `z -> y -> x`. On top of that there is a constraint `dependsOn y x`
that allows you to overwrite the order of two variables. Consequently, the
following constraints will be solved in the order `z -> x -> y`:

```
  x + y < z
  y `dependsOn` x
```

A consequence of this is that it is possible to form dependency loops by
specifying multiple constraints, e.g. in:

```
  x < y
  y < x + 10
```

However, this situation can be addressed by the introduction of `dependsOn` to
settle the order.  It is worth noting that the choice of order in `dependsOn`
is important as it affects the solvability of the constraints (as we saw
above). We leave the choice of `dependsOn` in the example below as an exercise
for the reader.

```
  x < y
  y < x + 10
  0 < x
  ? `dependsOn` ?
```

## The total definition requirement 

For the sake of efficiency we require that all constraints are dispatched as
defining constraints for a variable before we begin solving. We call this the
total definition requirement. This requirement is necessary because a set of
constraints with left over constraints are unlikely to be solvable.

Consider the following example for `p :: (Int, Int)`

```
fst p < snd p
```

in which there is no defining constraint for `p`, which would lead us to
compute the spec `mempty` for `p` during solving - meaning we would pick an
arbitrary `p` that is irrespective of the constraints. This is problematic as
the probability of picking `p = (x, y)` such that `x < y` is roughly `1/2`, as
you add more constraints things get much worse.

The principal problem above is that information that is present in the
constraints is lost, which would force us to rely on a `suchThat` approach to
generation - which will become very slow as constraint systems grow.

### letBind

A solution to the total definition requirement is to introduce more variables.
We can rewrite the problematic `fst p < snd p` example below as:

```
fst p = x
snd p = y
x < y
```

The dependency graph for these constraints will be the following:

```
x `dependsOn` y
p `dependsOn` x
```

This configuration is solvable, one picks `y` first, then picks `x < y`
and finally constructs `p = (x, y)`.

Note that (1) we introduced more variables than were initially in the
constraints - these need to be bound somewhere - and (2) the order of
`fst p = x` is important - `p` depends on `x` and not the other way
around.

To do both of these things at the same time we introduce the `letBind` construct
to the language:

```
letBind tm $ \ x -> preds
```

Which is semantically equivalent to:

```
exists $ \ x ->
  tm == x
  preds
```

# Overloaded types in the library

In an earlier section we provided some types for several of the library functions: `constrained`, `match`, 


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
instance IsPred Pred            -- Defined in ‘Constrained.Base’ 
```

Thus the following would be type-correct calls to constrained.

```
ex1 :: Specification Int
ex1 = constrained $ \ x -> True
-- Any Haskell Boolean value
                           
ex2 :: Specification Int                      
ex2 = constrained $ \ x -> x ==. lit 3
-- Any Term with type Bool

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

The meaning of this is a bit hard to parse. `IsProductType a`. It means the type `a` is isomorphic to a product type.
I.e. isomorphic to  `(t1,t2, ..., tn)` So all tuples would work. So would any type whose constructor had one or more arguments,
So would any type whose HasSpec instance was derived via the GHC.Generics instance. So in summary, if the type `a`  has **n** distinct parts,
then the constraint (`IsProductType a`) is met, and the interpretation of the `FunTy` is a function with **n** parameters.

```
FunTy (MapList Term (ProductAsList a)) p == t1 -> t2 -> ... -> tn -> p
``` 

# Library functions to build Term, Pred, Specification

### From Term to Pred
1.  `assert`

### Disjunction, choosing between multiple things with the same type

1. `CaseOn`, `branch`, `branchW`
2. `chooseSpec`

### Primed library functions which are compositions with match

1. `forAll'`
2. `constrained'`
3. `reify'`

### Construtors and Selectors
1. `con`
2. `onCon`
3. `isCon`
4. `sel`
5. `onJust`
6 `isJust`

### Operations to define and use Specifications
1. `satisfies`
2. `equalSpec`
3. `notEqualSpec`
4. `notMemberSpec`
5. `leqSpec` 
6. `ltSpec`
7. `geqSpec`
8. `gtSpec`
5. `cardinality`

### Controlling Variable order
1. `dependsOn`


### For all elements in a container type (List, Set, Map)
1. `forAll`

### Existential quantifiers
1. `exists`
2. `unsafeExists`

### Reification
1. `reify`
2. `refifies`
3. `assertRefified`

### Conditionals
1. `whenTrue`
2. `ifElse`

### `Explanantions`
1. `assertExplain`
2. `explanation`
3. `ExplainSpec`

### Escape Hatch to QuickCheck Gen monad
1. `monitor`


### Utility functions
1. `simplifyTerm`
2. `simplifySpec`
3. `genFromSpecT`
4. `genFromSpec`
5. `genFromSpecWithSeed`
6. `debugSpec`

# Predefined HasSpec instances.

In order to write specification for a particular type, that type must have a `HasSpec` instance. 
A type with a `HasSpec` instance might have a number of Function Symbols that operate on that type.
There are a number of types that have predefined `HasSpec` instances. We list them here along with the
type of their function symbols.

## Numeric types

`(Int,Integer, Natural, Int8, Int16, Int32, Int64, Word8, Word16, Word32, Word62)`

 1. `(<=.) :: OrdLike a => Term a -> Term a -> Term Bool`
 2. `(<.) :: OrdLike a => Term a -> Term a -> Term Bool`
 3. `(>=.) :: OrdLike a => Term a -> Term a -> Term Bool`
 4. `(>.) :: OrdLike a => Term a -> Term a -> Term Bool`
 5. `(==.) :: HasSpec a => Term a -> Term a -> Term Bool`
 6.  Num instance for (Term n) where n is a Numeric type. Operators `(+)`, `(-)`, `(*)`
 
## `HasSpec Bool`

  1. `or_ :: Term Bool -> Term Bool -> Term Bool`
  2.  `not_ :: Term Bool -> Term Bool`
 
## `HaSpec a => HasSpec [a]`

  1. `foldMap_ ::(Sized [a],Foldy b,HasSpec a)=> (Term a -> Term b) -> Term [a] -> Term b`
  2. `singletonList_ :: (Sized [a], HasSpec a) => Term a -> Term [a]`
  3. `append_  :: (Sized [a], HasSpec a) => Term [a] -> Term [a] -> Term [a]`
  
## `HasSpec a => HasSpec (Set a)`

  1. `singleton_ :: (Ord a, HasSpec a) => Term a -> Term (Set a)`
  2. `union_ :: (Ord a, HasSpec a) => Term (Set a) -> Term (Set a) -> Term (Set a)`
  3. `subset_ :: (Ord a, HasSpec a) => Term (Set a) -> Term (Set a) -> Term Bool`
  4. `member_ :: (Ord a, HasSpec a) => Term a -> Term (Set a) -> Term Bool`
  5. `disjoint_ :: (Ord a, HasSpec a) => Term (Set a) -> Term (Set a) -> Term Bool`
  6. `fromList_ :: (Ord a, HasSpec a) => Term [a] -> Term (Set a)`




