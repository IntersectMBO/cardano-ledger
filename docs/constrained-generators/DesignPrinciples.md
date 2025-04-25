# Design Principles of the Constrained Generators System

This document describes the design of the internals of the Constrained Generators System.
It attempts to explain how the system works. 

There is another document
*Constrained Generators Manual* that can be found in the file 
"cardano-ledger/docs/constrained-generators/manual.md" in the Cardano Ledger repository.
This explains the user facing API, and teaches users to write specifications. It is
deliberately silent about the internals of the system. 

In this document we explore the principles that make the system work. To do that we deliberately omit some of the advanced
features having to do with explanations, interfacing with advanced features of Test.QuickCheck, existentials, reifications, 
and hints. We expect another document to deal with those features, which add considerable complexity to the code base. 
So we use a much simplified system to describe just the basic principles of adding logical properties to 
a simple typed lamba calculus.

## Constrained Generators is a First-Order Logic

A First-order typed logic (FOTL) has 4 components, where each component uses types to ensure well-formedness.

1. **Terms** consisting of 
   - Variables: `x`, `y` . 
   - Constants: `5`, `"abc"`, `True` . 
   - Applications: `elem_ "abc" xs`, `x ==. y`.  
       - Applications apply a function symbol (i.e. `elem_`) to a list of Terms,
	   - In an infix application the function symbol lies between its two Term arguments .(i.e. `abc ==. y`)
2. **Predicates**   (Assert (x ==. 5)). Predicates are the assertions of boolean typed terms.
3. **Connectives**  (And, Or, Not, =>, ...).  Connectives make more complex Predicates out of simpler ones.
4. **Quantifiers**  (Forall, Exists)

The **Constrained generators** system is a FOTL implemented as an embedded domain specific language in Haskell.
It allows programmers to write Haskell programs with type `Spec T` that denotes a set of random
values for the type `T`, that are subject to a set of constraints expressed as Predicates. This supports property
based testing where a completely random set of values may not be useful.

## Overview of the Design Principles

We list the set of Design Principles here. In the rest of the paper we will go over these in much
more detail. 

1. Every function symbol has `Syntax`, `Semantics`, and `Logic` capabilities. While the concepts of
   `Syntax` and `Semantics` are familiar ones, `Logic` capabilities, include the the notion of property
    propagation. Consider an application term `(f x 8)` with exactly one variable `x` that has some property
	`P`. Propagation works as follows. For every function symbol `f`, we need to be able to do the following.
    Given `(hasProperty (f x 8) P)` find property `Q` such that `(hasProperty x Q)`

2. The FOTL for the Constrained Generators System is embedded in the Haskell datatypes `Term a` and `Pred`

3. Associated with some types `t`, is another type called the `TypeSpec T` . This an associated type family
   of the class `HasSpec T`. If a type has a  `HasSpec T` instance, then we can write solveable
   constraints for a variable of type `T`. A  `TypeSpec T` is a type that encodes type specific information
   on how to generate constrained values for the type `T`.

4. A set of constraints about a single variable of type `t` is collected into a `Spec t`, The type `Spec t`
   is a type with several constructors, one of which embeds the type-specific `TypeSpec t`. The other constructors
   embed non type-specific constraint properties. The type `(Spec t)` is how we encode properties for type `t`.
  
5. The type `Spec t` has a Monoid instance. So two `Spec t` values can be combined `spec1 <> spec2`, which
   describes a new `Spec t` where all the properties of `spec1` and `spec2` hold.

6. We solve a set of constraints one variable at a time. The order in which we solve the variables is called
   a `SolverPlan`. We can generate a random set of values that meet all the constraints, by picking the first variable
   from the plan. Then choosing a random value for that variable that meets the constraints on it. Then
   adding that value to an `Env`. Then the extended `Env` is substituted into the rest of the plan. This now has
   one less variable. We then solve the next variable in the plan, until the plan is empty, in which case the Env, 
   contains a solution for every variable.
   
7. Given a term `t :: T` and 1 or more `Pred` that only mention `t`, compute a `(Spec T)` from the `Pred`s.
   This works by computing a `Spec` for each `Pred`, and then use the `Monoid` operation `(<>)` on `(Spec T)` to combine
   them all into 1 comprehensive  `(Spec T)`. This step is how we relate the type `Pred` to the type `Spec`
   
8. We can extract a random value from a `Spec t`, by using the function `genFromSpec :: Spec t -> QuickCheck.Gen t`

## What is the Constrained Generators System good for? 

Once we have written a `Specification` what can we do with it? Specifications have two interpretations.

1.  We can interpret them as a generator of values the meet all the constraints inside the specification.
2.  We can interpret them  as a function that checks if a given value meets all the constraints inside the specification.


The first interpretation of the specification is the function `genFromSpec`

```
--| Generate a value from the spec in the QuickCheck monad 'Gen'
genFromSpec:: (HasCallStack, HasSpec a) => Specification a -> QuickCheck.Gen a
```

This function is very useful when writing QuickCheck properties. With it we can write 
`Gen` instances that are not completely random, but instead meet a set of constraints.
This is the actual purpose of the system. Construct mostly random values, that meet some constraints
that inter-relate many different objects. 

Consider a system of 4 objects, named by the variables,  _w,x,y,z_ where we want to test the QuickCheck *implication* property

`(w < x && x < y && y < z) ==> property (w < z)` We might write a QuickCheck property like this

```
prop1 :: Gen Property
prop1 = do
   (w,x,y,z) <- arbitrary :: Gen (Int,Int,Int,Int)
   pure $ (w < x && x < y && y < z) ==> property (w < z)
```

The problem with this is that the probability that the condition `(w < x && x < y && y < z)` is True, for random
`w`, `x`, `y`, and `z`, is pretty low, so the property will pass vacuously most of the time, making a poor test.
We can observe this by

```
ghci> quickCheck prop1
*** Gave up! Passed only 29 tests; 1000 discarded tests.
```

A vacuous pass, becomes a QuickCheck `discard`, so we cannot find even 100 successful passes.
We can do a better job by constraining a semi-random set of 4 variables by the the condition  `(w < x && x < y && y < z)`,
then use `genFromSpec` to get the constrained set of random values. We do this in two steps.
First write a specification that expresses the constraint  `(w < x && x < y && y < z)`, then second,
embed the call to `genFromSpec` in a QuickCheck property.

```
spec4 :: Spec (Integer, Integer, Integer, Integer)
spec4 = constrained $ \x ->
  match x $ \a b c d -> And [Assert $ a <=. b, Assert $ b <=. c, Assert $ c <=. d]


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

Now this isn't a very good test either, since the precedent is alway true. A better solution would be to
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
This makes it possible to write conditional 'implication' properties that have a high
probability of not being vacuously true. 

## The FOTL is embedded in the Haskell datatypes `Term a` and `Pred`

Now lets look closely at the specification, identifying the different parts which make it a statement in a *FOTL*

```
spec4 :: Spec (Integer, Integer, Integer, Integer)
spec4 = constrained $ \x ->
  match x $ \a b c d -> And [Assert $ a <=. b, Assert $ b <=. c, Assert $ c <=. d]
```
1. `constrained` says we are writing a `Spec` and the Haskell lambda expression introduces the the variable `x` that 
    names the tuple of 4 integers.
2. `match` says we are decomposing the 4-tuple, and naming each of its components `a`, `b`, `c`, `d`.
3. `And` is a connective, saying we are making 1 big predicate out of 3 smaller ones.
4. `Assert` says we are asserting that something is true. `Assert` makes a predicate out of a Boolean valued term.
5. `a <=. b` is a Boolean valued term, and `(<=.)` an infix function symbol.

Here are the complete definitions for `Term` and `Pred`

```
data Term a where
  -- ^ An application of a function symbol `t` to a list of arguments
  App ::
    forall t dom rng.
    AppRequires t dom rng =>
    t dom rng ->
    List Term dom ->
    Term rng
  -- ^ A literal constant
  Lit :: (Typeable a, Eq a, Show a) => a -> Term a
  -- ^ A variable
  V :: HasSpec a => Var a -> Term a
  
data Pred where
  ElemPred :: forall a. HasSpec a => Bool -> Term a -> NonEmpty a -> Pred
  And :: [Pred] -> Pred
  Exists :: ((forall b. Term b -> b) -> GE a) -> Binder a -> Pred
  ForAll :: (Container t a, HasSpec t, HasSpec a) => Term t -> Binder a -> Pred
  DependsOn :: (HasSpec a, HasSpec b) => Term a -> Term b -> Pred
  Assert :: Term Bool -> Pred
  TruePred :: Pred
  FalsePred :: NonEmpty String -> Pred
  Case :: HasSpec (Either a b) => Term (Either a b) -> Binder a -> Binder b -> Pred
  Let :: Term a -> Binder a -> Pred
  Subst :: HasSpec a => Var a -> Term a -> Pred -> Pred  
```

##  Every function symbol has `Syntax`, `Semantics`, and `Logic` properties.

A function symbol can have three kinds of operations.

1. Syntax. Used, if we want to display the symbol.
2. Semantics. Used, if we want to evaluate a `Term` with that symbol, i.e.  (5 <=. 1)  evalutes to False.
3. Logic. Used, if we want to reason about a `Term` with that symbol, 
      - i.e.  if Term `(x +. 2)` has property `(x +. 2) in range[3..6]`
      - what does that say about variable `x`.  It says that `x in range [1..4]`
      - This logic operation is called propagation.  For any function symbol `f` and any property `p`,
        if `(f x) has p`, then what property `q` makes `x has q` true? Propagation computes properties about
        the variable in an application, from the property about the application itself.
   
Note that if a Term is not inside an `Assert` all we need are the `Syntax` and `Semantic` properties.
Every function symbol inside an `Assert` must have `Logic` properties. Here is how way capture this
in the Constrained Generators System. Constructors of a type with kind `([Type] -> Type -> Type)`
are candidates for function symbols. We make the constuctors real function symbols by
making `Syntax`, `Semantics` and `Logic` instances for that type.

```
-- | Syntactic operations are ones that have to do with the structure and appearence of the type.
class Syntax (t :: [Type] -> Type -> Type) where
  inFix :: forall dom rng. t dom rng -> Bool
  inFix _ = False
  name :: forall dom rng. t dom rng -> String

-- | Semantic operations are ones that give the function symbol, meaning as a function.
--   I.e. how to apply the function to a list of arguments and return a value,
--   or to apply meaning preserving rewrite rules.
class Syntax t => Semantics (t :: [Type] -> Type -> Type) where
  semantics :: forall d r. t d r -> FunTy d r   -- e.g. FunTy '[a,Int] Bool == a -> Int -> Bool
  rewriteRules :: forall ds rng. (TypeList ds, Typeable ds, HasSpec rng, All HasSpec ds) =>
    t ds rng -> List Term ds -> Evidence (Typeable rng, Eq rng, Show rng) -> Maybe (Term rng)
  rewriteRules t l Evidence = Lit <$> (applyFunSym @t (semantics t) l)

-- | Logical operations are one that support reasoning about how a function symbol
--   relates to logical properties, that we call Spec's
class (Typeable t, Syntax t, Semantics t) => Logic (t :: [Type] -> Type -> Type) where
  propagate ::
    (AppRequires t as b, HasSpec a) =>
    t as b -> ListCtx as (HOLE a) -> Spec b -> Spec a
```	

Here is an example of a type whose constructors are candidates for function symbols. Note that
`IntegerSym` has kind `([Type] -> Type -> Type)`

```
data IntegerSym (dom :: [Type]) rng where
  PlusW :: IntegerSym '[Integer, Integer] Integer
  MinusW :: IntegerSym '[Integer, Integer] Integer
  LessOrEqW :: IntegerSym '[Integer, Integer] Bool
  GreaterOrEqW :: IntegerSym '[Integer, Integer] Bool
```

## The function propagateSpec, and nested function symbols.

Consider the term `((size_ x +. Lit 3) <=. Lit 12)` with a bunch of nested function symbols, with just 1 variable `x`
To solve this, we will have to use `propagate` for each of the nested function symbols `size_`, `(+.)`, and `(<=.)`.
We do this by working our way from the outermost function symbol `(<=.)`, to the middle function symbol `(+.)`, to the
innermost function symbol `size_`. This is the role of the function `propagateSpec` 

Given a `Term` with just 1 variable `x`, we can build a context with exactly one `CtxHole`, replacing the variable `x`
Applied to  `((size_ x +. Lit 3) <=. Lit 12)` we get the context

```
CtxApp <=. (CtxApp +. (CtxApp size_ (Unary CtxHole) :<| 3) :<| 12)
```

Working our way from outside in, we first propagate (<=.), then (+.), then (size_). This reduces in several steps.
Note that we have deliberately **bolded** the  **`$`** , to note visually where the last argument to `propagateSpec`
occurs. Note after each step, this last arg becomes composition of `propagate`.

1. `propagateSpec (CtxApp <=. (CtxApp +. (CtxApp size_ (Unary CtxHole) :<| 3) :<| 12))` **`$`** `spec`
2. `propagateSpec (CtxApp +. (CtxApp size_ (Unary CtxHole) :<| 3)))` **`$`** `(propagate <=. (HOLE:<| 12) spec)`
3. `propagateSpec (CtxApp size_ (Unary CtxHole)))` **`$`** `(propagate +. (HOLE:<| 3) (propagate <=. (HOLE :<| 12) spec))`
4. `propagateSpec CtxHole)` **`$`** `(propagate size_ Hole (propagate +. (HOLE:<| 3) (propagate <=. (HOLE :<| 12) spec)))`
5.  **`$`** `propagate size_ Hole (propagate +. (HOLE:<| 3) (propagate <=. (HOLE :<| 12) spec))`

Note the pattern in the code below. The recursize call to 'propagateSpec' is on the pattern variable `ctx` which is the
part of the context pointed to by the arrows (:<|) and (:|>), and this recurive call to `propagateSpec` is
applied to a new spec computed by 'propagate', where the variable `ctx` is replaced by HOLE.
we end up on line 5), with three nested calls to `propagate`. Here is the defintion of `propagateSpec`

```
propagateSpec :: forall v a. HasSpec v => Ctx v a -> Spec a -> Spec v
propagateSpec context spec = case context of
  CtxHole -> spec
  CtxApp f (Unary ctx) | Evidence <- ctxHasSpec ctx -> propagateSpec ctx (propagate f (Unary HOLE) spec)
  CtxApp f (ctx :<| v) | Evidence <- ctxHasSpec ctx -> propagateSpec ctx (propagate f (HOLE :<| v) spec)
  CtxApp f (v :|> ctx) | Evidence <- ctxHasSpec ctx -> propagateSpec ctx (propagate f (v :|> HOLE) spec)
```


## Inductive tuples, The List type, and their operations

Before we can study  `Syntax`, `Semantics` and `Logic` instances for `IntegerSym`, we need to
discuss the datatype `List` and the constraints `TypeList`, `Typeable`, `HasSpec`, `All`, and `AppRequires`

The Constrained Generators System is a **typed** lambda calculus. As such, the datatypes that define it must track the
type of every object. To do this we use the `Typeable` class from the Haskell module `Data.Typeable`. When a value or
function has a `Typeable t` constraint, the Haskell compiler stores a `TypeRep` for `t` in the code. There are ways for
programs to access the type stored in this `TypeRep`.

The data type `List` implements heterogeneous lists. One can think of this as an inductive tuple. For example

```
data List (f :: k -> Type) (as :: [k]) where
  Nil :: List f '[]
  (:>) :: f a -> List f as -> List f (a : as)

args :: List Term '[Int,Bool,String]
args = Lit 5 :> Lit True :> Lit "abc" :> Nil
```
Is an inductive representation of the tuple `(Lit 5, Lit True,Lit "abc") :: (Term Int, Term Bool, Term String)`

`List`s and the constraints listed above make it possible to write Haskell functions that can manipulate
typed `Term t`, and other type indexed datatypes. 

The `TypeList ds` class constraint, says that `ds` has kind `[Type]`
(just like the second type parameter of `(List Term '[Int,Bool,String])`
The methods of the `TypeList` class, along with the
type family `FunTy`, are used to express the type of, and implement functions that manipulate `List`s

```
type family FunTy ts res where
  FunTy '[] a = a
  FunTy (a : as) r = a -> FunTy as r
```

Ie. `FunTy ds Bool`  rewrites to  `Integer -> Bool -> [Char] -> Bool`

```
-- | Higher-order functions for working on `List`s
class TypeList ts where
  uncurryList :: FunTy (MapList f ts) r -> List f ts -> r
  uncurryList_ :: (forall a. f a -> a) -> FunTy ts r -> List f ts -> r
  curryList :: (List f ts -> r) -> FunTy (MapList f ts) r
  curryList_ :: (forall a. a -> f a) -> (List f ts -> r) -> FunTy ts r
  unfoldList :: (forall a as. List f as -> f a) -> List f ts
```
Here are some examples that illustrate the methods of `TypeList`

```
-- | Fold over a (List Term ts) with 'getTermsize' which consumes a Term component for each element of the list
ex1:: Maybe Int
ex1 = uncurryList getTermsize args1
  where args1 :: List Term '[Int,Bool,String]
        args1 = Lit 5 :> Lit True :> Lit "abc" :> Nil
        getTermsize :: Term Int -> Term Bool -> Term String -> Maybe Int
        getTermsize (Lit n) (Lit b) (Lit s) = Just $ if b then n else length s
        getTermsize _ _ _ = Nothing
```

```
-- Fold over a list with two parts 'unLit' and 'getSize'
ex2 :: Int
ex2 = uncurryList_ unLit getsize args2
   where unLit :: forall a. Term a -> a
         unLit (Lit n) = n
         getsize :: Int -> Bool -> String -> Int
         getsize n b s = if b then n else length s
         args2 :: List Term '[Int,Bool,String]
         args2 = Lit 5 :> Lit True :> Lit "abc" :> Nil
```

Construct a function over `Terms` from a function over `(List Term '[a,b,c])`
 
```
ex3 :: Term a -> Term b -> Term c -> String
ex3 = curryList crush
  where crush :: (List Term '[a,b,c] -> String) 
        crush (a :> b :> c :> Nil) = show a ++ show b ++ show c
```

Construct a function `FunTy '[a,b,c] r` from a function over `(List T '[a,b,c] -> r)`
and a function from `(a -> T a)`

``` 
ex4 ::Int -> Bool -> String -> Int
ex4 = curryList_ one totalLength
  where totalLength :: List [] '[Int,Bool,String] -> Int
        totalLength (n :> b :> s :> Nil) = length n + length b + length s
        one :: a -> [a]
        one x = [x]
```


The constraint `All F`  applies constraint `F` to all elements in a type list.

`All :: forall k. (k -> Constraint) -> [k] -> Constraint`

So `All HasSpec '[ a, b, c]` reduces to `(HasSpec a, HasSpec b, HasSpec c)`

## The HasSpec class

The `HasSpec` class is at the heart of the system. It does two things

1. Identifies the operations necessary to generate random values for a type.
2. Provides the interface to QuickCheck (`genFromSpecT`) that supports writing impliication properties. 

```
class HasSpec a where
  -- | The `TypeSpec a` is the type-specific version of `Spec a` for type `a`
  type TypeSpec a

  -- | The `TypeSpec a` that has no constraints.
  emptySpec :: TypeSpec a
  
  -- | A function, akin to `Semigroup(<>)`,  that combines two `TypeSpec`. It is different
  --   in that it returns a general `Spec` rather than a type specific `TypeSpec`
  combineSpec :: TypeSpec a -> TypeSpec a -> Spec a

  -- | Generate a value that satisfies the `TypeSpec`.
  -- The key property for this generator is soundness:
  --  ∀ a ∈ genFromTypeSpec spec. a `conformsTo` spec
  genFromTypeSpec :: (HasCallStack, MonadGenError m) => TypeSpec a -> GenT m a

  -- | Check conformance to the spec.
  conformsTo :: HasCallStack => a -> TypeSpec a -> Bool

  -- | Convert a spec to predicates:
  -- The key property here is:
  --   ∀ a. a `conformsTo` spec == a `conformsTo` constrained (\t -> toPreds t spec)
  toPreds :: Term a -> TypeSpec a -> Pred

  -- | This is used to detect self inconsistencies in a (TypeSpec t)
  --   guardTypeSpec ty --> ErrorSpec message, if ty is inconsistent
  guardTypeSpec :: TypeSpec a -> Spec a
  guardTypeSpec ty = typeSpec ty
```

In a later section we present `HasSpec` instances for several
types: [Bool](#HasSpecBool), [Integer](#HasSpecInteger), [Set a](#HasSpecSet), 
[Pair(a,b)](HasSpecPair) and `(Either a b)`

## The Spec datatype

 ```
  data Spec a where
    -- | There are no contraints at all.
    TrueSpec :: Spec a         
    -- | The `Spec` is inconsistent
    ErrorSpec :: NonEmpty String -> Spec a
	-- | The Spec encodes a FOTL statement, with Predicates encoded in the type `Pred`
    SuspendedSpec :: HasSpec a => Var a -> Pred -> Spec a
	-- | The solution is exactly the elements in the Non empty list
    MemberSpec :: NonEmpty a -> Spec a
	-- | The solution is embedded in the type-specific `TypeSpec a`
    TypeSpec :: HasSpec a => TypeSpec a -> [a] -> Spec a
```

<a id="HasSpecBool"></a>
## HasSpec Bool instance

The `HasSpec Bool` instance is relatively simple, since the type `Bool` has only two elements. 

```
instance HasSpec Bool where
  type TypeSpec Bool = Set Bool

  emptySpec = Set.fromList [False,True]

  combineSpec s s' = typeSpec (Set.union s s')

  genFromTypeSpec set
    | Set.null set = fatalError "genFromTypeSpec @Set where the typeSpec is Set.empty"
    | otherwise = oneofT (map pure (Set.toList set))

  guardTypeSpec s
    | Set.null s = ErrorSpec $ pure "guardTypeSpec @Set where the typeSpec is Set.empty"
    | otherwise = TypeSpec s []

  conformsTo i set = Set.member i set

  toPreds v set = case Set.toList set of
    [] -> FalsePred (pure "toPreds @Set where the typeSpec is Set.empty")
    (x : xs) -> ElemPred True v (x :| xs)
```

1. The `TypeSpec` for `Bool` is just a set of boolean values. There are only 4 distinct values
   in this set. `{}`, `{True}`, `{False}`, and `{False,True}`  Of these four the empty set `{}` is
   not self consistent, so we will have to handle this carefully.
   
2. The method `emptySpec` is the `TypeSpec` which makes no constraints. That means every `Bool` value must
   be in the set. Since `Bool` has only two values, the `emptySpec` must be the set `{False,True}`

3. The method `guardTypeSpec` returns an `ErrorSpec` if the `TypeSpec` is inconsistent. There is only
   one inconsistent set of `Bool`, so return `ErrorSpec` if the set is null.
   
4. The method `consformsTo` just tests if the input `Bool` is in the set of booleans.

5. The method `toPreds` returns `FalsePred` if the `TypeSpec` is the null set, and if not
   returns the `ElemPred` that say each item in the set must be in the list computed from that set.

### Function symbols for Bool

There is only one function symbol for `Bool` , the negation operation on `Bool`, `not`. 

```
data BoolSym (dom :: [Type]) rng where
  NotW :: BoolSym '[Bool] Bool

deriving instance Eq (BoolSym dom rng)

instance Show (BoolSym dom rng) where show = name

instance Syntax BoolSym where
  name NotW = "not_"
  inFix _ = False

instance Semantics BoolSym where
  semantics NotW = not
```

The `Syntax` and `Semantics` instances are trivial, and should need no explanation. In order to
talk about the `Logic` instance we must study the type of the `Logic` method `propagate`.
The type of the method `propagate` is
```
propagate :: (Logic t, AppRequires t as b, HasSpec a) =>
     t as b -> ListCtx as (HOLE a) -> Spec b -> Spec a
```
A `ListCtx` is a `Term` with a single `HOLE` and no variables (i.e. everything else is a literal constant). 
For a unary function there is only one context `(Unary HOLE)` because a unary function has only one input,
it can only have a `HOLE`, no literal constants are possible. 

```
instance Logic BoolSym where
  propagate _ _ TrueSpec = TrueSpec
  propagate _ _ (ErrorSpec msgs) = ErrorSpec msgs
  propagate NotW (Unary HOLE) (SuspendedSpec v ps) =
    constrained $ \v' -> Let (App NotW (v' :> Nil)) (v :-> ps)
  propagate NotW (Unary HOLE) spec =
    caseBoolSpec spec (equalSpec . not)
```
The only interesting case is the last one. The first 3 only rely on the properties that
propagating over `TrueSpec`, `ErrorSpec` or `SuspendedSpec` rely on their special properties.


The last case boils down to the following. if `(not x) conformsTo spec` what does that say about `x`
To make this concrete we will name the hole `x`, consider the 3 cases for `Spec`. 

1. `(not x) conformsTo {False}`       what does that say about `x` ? `x` must be True
2. `(not x) conformsTo {True}`        what does that say about `x` ? `x` must be False
3. `(not x) conformsTo {False,True}` what does that say about `x` ? `x` can be either True or False

The function `caseBoolSpec` formalizes this. We build a list of values that the `spec` conforms to.
There are only two values (True and False), so we try them all. If the list is empty, 
there is no solution so return an `ErrorSpec` , if the list has more than 1 element, and since Bool has
only two elements, the solution is the Spec with no constraints, i.e. mempty.
If the list has exactly 1 element, we apply the continuation to that element, and let the caller
of `caseBoolSpec` decide what to do.

```
caseBoolSpec :: (HasSpec Bool, HasSpec a) => Spec Bool -> (Bool -> Spec a) -> Spec a
caseBoolSpec spec cont = case possibleValues spec of
  [] -> ErrorSpec (NE.fromList ["No possible values in caseBoolSpec"])
  [b] -> cont b
  _ -> mempty
  where
    possibleValues s = filter (flip conformsToSpec s) [True, False]
```

Finally, add the `name` function symbol as function over terms

```
not_ :: Term Bool -> Term Bool
not_ x = App NotW (x :> Nil)
```
<a id="HasSpecInteger"></a>
## HasSpec Integer instance

We intoduce the datatype `Range`. This will be the type specific `TypeSpec` for `Integer`
It has both a `Semigroup` and `Monoid` instance. A `Range` denotes a contiguous interval.
The Range `(Interval Nothing Nothing)` denotes the interval from negative infinity to positive infinity.
A `Range` with a single `Nothing` denotes infinity in only one direction.

```
data Range = Interval (Maybe Integer) (Maybe Integer) deriving (Eq, Show)

unionWithMaybe :: (a -> a -> a) -> Maybe a -> Maybe a -> Maybe a
unionWithMaybe f x y = case (x,y) of
  (Nothing,Nothing) = Nothing
  (Just lo, Just hi) = Just (f lo hi)
  (Nothing , Just hi) = Just hi
  (Just lo, Nothing) = Just lo

instance Semigroup Range where
  Interval ml mu <> Interval ml' mu' =
    Interval
      (unionWithMaybe max ml ml')
      (unionWithMaybe min mu mu')

instance Monoid Range where
  mempty = Interval Nothing Nothing
```

Just a note. If two Ranges are completely disjoint, with no overlap, then
the operation `<>` leads to an inconsistent Range where lower bound is greater than the upper bound.
```
(Range (Just 5) (Just 10)) <> (Range (Just 12) (Just 20)) ---> Range (Just 12) (Just 10)
              \         \                  /        /
               \---------\---------------max       /
                          \               12      /
				 		   \---------------------min
						                          10
```													 
													 						  
The `HasSpec` instance requires an instance for the type family `TypeSpec Integer` and five other methods.

```
instance HasSpec Integer where
  type TypeSpec Integer = Range

  emptySpec = Interval Nothing Nothing

  combineSpec s s' = guardTypeSpec (s <> s')

  guardTypeSpec r@(Interval (Just n) (Just m))
    | n > m = ErrorSpec (pure ("lower bound greater than upper bound\n" ++ show r))
    | otherwise = typeSpec r
  guardTypeSpec range = typeSpec range

  genFromTypeSpec (Interval ml mu) = do
    n <- sizeT
    chooseT =<< constrainInterval (ml <|> lowerBound) (mu <|> upperBound) (fromIntegral n)

  conformsTo i (Interval ml mu) = maybe True (<= i) ml && maybe True (i <=) mu

  toPreds v (Interval ml mu) =
    Foldable.fold $
      [Assert $ Lit l <=. v | l <- maybeToList ml]
        ++ [Assert $ v <=. Lit u | u <- maybeToList mu]
```

1. The method `emptySpec` is the TypeSpec that has no constraints, so we make it the `Range` from -∞ to +∞ .
2. The method `combineSpec` combines two `TypeSpec` so that the result has the constraints of both. We use the 
   `Semigroup` instance on `Range`, which does exactly that.
3. The method `guardTypeSpec` returns an `ErrorSpec` if its input of type `Range` is inconsistent.
   This happens only when a `(Just lo)` bound is greater than a `(Just hi)` bound. Which usually arises
   when we `combineSpec` two disjoint `Ranges` as explained above. If a `Range` has
   one or more `Nothing` it is always consistent.
4. The method `genFromTypeSpec` generates a random value in the `GenT` monad (a richer version of the QuickCheck
   `Gen` monad that has mechanisom for handling errors, which the `Gen` monad does not). We extract the `QuickCheck.Gen`
    size parameter to scale the random values according to magnitude of the bounds stored in the `Range`.
	`sizeT` and `chooseT` are the `GenT` version of the QuickCheck operations `getSize` and `choose`.
5. The method `conformsTo` checks that `i` is in the given `Range`. 
6. The method `toPreds` translates a `Range` into a pair of assertions.
7. The function `constrainInterval` uses the `n` input to choose a random value whose
   size is scaled to the actual bounds in the range.

### Function symbols for Integer

To make the `HasSpec Integer` instance usefull we must introduce function symbols on Integer, and then provide
`Syntax`, `Semantics` and `Logic` instances for those function symbols. Recall that a datatype whose
constructors denote function symbols must have kind `([Type] -> Type -> Type)`.

```
data IntegerSym (dom :: [Type]) (rng :: Type) where
  PlusW :: IntegerSym '[Integer, Integer] Integer
  MinusW :: IntegerSym '[Integer, Integer] Integer
  LessOrEqW :: IntegerSym '[Integer, Integer] Bool
  GreaterOrEqW :: IntegerSym '[Integer, Integer] Bool
```

One we have `(Syntax IntegerSym)`, `(Semantics IntegerSym)` and `(Logic IntegerSym)` instances
we will have four new function symbols. We use the  kinds `(dom :: [Type])` and  `(rng :: Type)` to encode the type
of the function symbol as `(FunTy dom rng)` which for `PlusW` would be `(+) :: Integer -> Integer -> Integer`
The function symbol `name` will be a Haskell function over terms. So for `PlusW` we would have
`(+.) :: Term Integer -> Term Integer -> Term Integer`

```
instance Syntax IntegerSym where
  name PlusW = "+."
  name MinusW = "-."
  name LessOrEqW = "<=."
  name GreaterOrEqW = ">=."
  inFix _ = True

instance Semantics IntegerSym where
  semantics PlusW = (+)
  semantics MinusW = (-)
  semantics LessOrEqW = (<=)
  semantics GreaterOrEqW = (>=)
```

These function symbols have `name`s that align with their Haskell counterparts, but have a trailing `.` in their name.
The `Syntax` and `Sematics` are exactly what you would think. The `Logic` instance method `propagate`
requires some explanation. First some helper functions for building `Spec Integer` without using any
function symbols. They will be useful when we study the `Logic Integer` instance.

```
negateRange :: Range -> Range
negateRange (Interval ml mu) = Interval (negate <$> mu) (negate <$> ml)

minus :: Integer -> Integer -> Integer
minus n x = n - x

geqSpec :: Integer -> Spec Integer
geqSpec n = typeSpec (Interval (Just n) Nothing)

leqSpec :: Integer -> Spec Integer
leqSpec n = typeSpec (Interval Nothing (Just n))

gtSpec :: Integer -> Spec Integer
gtSpec n = typeSpec (Interval (Just (n + 1)) Nothing)

ltSpec :: Integer -> Spec Integer
ltSpec n = typeSpec (Interval Nothing (Just (n - 1)))
```

1. The function `minus` is just `(-)` with its arguments flipped. Partially applied as `(minus n)` this is quite usefull.
2. If we have a range that includes [-1,0,1,2,3] `(Interval (Just (-1)) (Just 3))`, then the negation of that 
   range would include [1,0,-1,-2,-3] `(Interval (Just (-3)) (Just 1))` We can compute that by negating
   and switching the bounds.
   
3. The `(geqSpec 6)` means  `x` such that `x >= 6`. We encode that as `(Interval (Just 6) Nothing)` 
4. The Haskell functions `leqSpec`, `gtSpec`, and `ltSpec` use similar reasoning


```
instance Logic IntegerSym where
  propagate tag ctx spec = case (tag, ctx, spec) of
    (_, _, TrueSpec) -> TrueSpec
    (_, _, ErrorSpec xs) -> ErrorSpec xs
    (f, context, SuspendedSpec v ps) -> 
       constrained $ \v' -> Let (App f (fromListCtx context v')) (v :-> ps)
    (LessOrEqW, HOLE :<| l, bspec) -> 
       caseBoolSpec bspec $ \case True -> leqSpec l; False -> gtSpec l
    (LessOrEqW, l :|> HOLE, bspec) -> 
       caseBoolSpec bspec $ \case True -> geqSpec l; False -> ltSpec l
    (GreaterOrEqW, HOLE :<| x, spec1) -> 
       propagate LessOrEqW (x :|> HOLE) spec1
    (GreaterOrEqW, x :|> HOLE, spec2) -> 
       propagate LessOrEqW (HOLE :<| x) spec2
    (PlusW, HOLE :<| n, TypeSpec (Interval lo hi) bad) -> 
       TypeSpec (Interval ((minus n) <$> lo) ((minus n) <$> hi)) (map (minus n) bad)
    (PlusW, HOLE :<| n, MemberSpec xs) -> 
       MemberSpec (fmap (minus n) xs)
    (PlusW, n :|> HOLE, TypeSpec (Interval lo hi) bad) -> 
       TypeSpec (Interval ((minus n) <$> lo) ((minus n) <$> hi)) (map (minus n) bad)
    (PlusW, n :|> HOLE, MemberSpec xs) -> MemberSpec (fmap (minus n) xs)
    (MinusW, HOLE :<| n, TypeSpec (Interval lo hi) bad) -> 
       TypeSpec (Interval ((+ n) <$> lo) ((+ n) <$> hi)) (map (+ n) bad)
    (MinusW, HOLE :<| n, MemberSpec xs) -> 
       MemberSpec (fmap (+ n) xs)
    (MinusW, n :|> HOLE, TypeSpec (Interval lo hi) bad) -> 
       TypeSpec (negateRange (Interval ((minus n) <$> lo) ((minus n) <$> hi))) (map (minus n) bad)
    (MinusW, n :|> HOLE, MemberSpec xs) -> 
       MemberSpec (fmap (minus n) xs)
```	   

The type of the method `propagate` is
```
propagate :: (Logic t, AppRequires t as b, HasSpec a) =>
     t as b -> ListCtx as (HOLE a) -> Spec b -> Spec a
```

Notice that `t as b` is a function symbol, for example `LessOrEqW :: IntegerSym '[Integer,Integer] Bool`.
A `ListCtx` is a `Term` with a single `HOLE` and no variables (i.e. everything else is a literal constant). 
For a binary function there are exactly two contexts `(HOLE :<| n)` and `(n :|> HOLE)`. Here `n` is a constant,
so for `LessOrEqW` there are two cases to consider, one for each type of
context (`HOLE` on the right, and `HOLE` on the left). Lets consider the case where the `HOLE` is on the left.

Recall the things an the left of the `=` are known, and we are trying to compute the `intSpec` on the right of the `=`.

1. `propagate LessOrEqW (HOLE :<| intConstant) boolSpec = intSpec`
   To make this concrete we will name the hole `x`, and give a concrete value for `intConstant` which is `7`.
   Since `boolSpec` could be either `True` or `False`,  we have two concrete visual representations of the example.
     -  `(x <=. 7) = True`, so what do we know about `x` ? We know `(x <=. 7)`
	 -  `(x <=. 7) = False`, so what do we know about `x` ?  We know `(x > 7)`
   Given a property about a function call, how do we translate that property to one about the variable that is
   an input to the function call.
   
This is the code that implements this case branch above. Note `caseBoolSpec` allows us to consider
the two cases separately.

```  (LessOrEqW, HOLE :<| l, bspec) ->  
          caseBoolSpec bspec $ \case True -> leqSpec l; False -> gtSpec l
```

All the cases follow similar reasoning. In the cases for `PlusW` and `MinusW`, the known `Spec` will have type
`(Spec Integer)`  rather than `(Spec Bool)` because unlike `LessOrEqW` whose range is `Bool`, the range of
`MinusW` is `Integer`.  One other difference is that we must consider that the `(Spec Integer)` can be one of
the two `Spec` constructors `MemberSpec` or `TypeSpec`.  For `LessOrEqW` we didn't need to handle both cases 
since the function `caseBoolSpec` does that analysis internally.  Lets consider the cases for `MinusW` where the
`HOLE` is on the left.

When the output spec is a `TypeSpec` we have something like this. 

```
(MinusW, HOLE :<| n, TypeSpec (Interval lo hi) bad) -> 
       TypeSpec (Interval ((+ n) <$> lo) ((+ n) <$> hi)) (map (+ n) bad)
```
Lets use the same trick we did earlier by naming the hole `x`, an concretizing the constants.
The concrete example is: `(x - 3) conformsTo (TypeSpec (Interval (Just 6) (Just 12)) [7])` 
So the range is bewteen 6 and 12, but cannot be 7. So what does that say about `x`
It says `x` must be bewteen 9(6+3) and 5(12+3), but cannot be 10(7+3). Which correponds to the code above.


When the code is a `MemberSpec` we have something like this
```
(MinusW, HOLE :<| n, MemberSpec xs) ->  MemberSpec (fmap (+ n) xs)
```
With concrete example  `(x - 3) conformsTo MemberSpec [8,13]`  So what does that say about `x`
It says `x conformsTo MemberSpec [11(8+3), 16(13+3)`. Which corresponds to the code.


Finally we define the lifted versions of the function symbols that work on `Term`

```
(<=.) :: Term Integer -> Term Integer -> Term Bool
(<=.) x y = App LessOrEqW (x :> y :> Nil)

(>=.) :: Term Integer -> Term Integer -> Term Bool
(>=.) x y = App GreaterOrEqW (x :> y :> Nil)

(+.) :: Term Integer -> Term Integer -> Term Integer
(+.) x y = App PlusW (x :> y :> Nil)

(-.) :: Term Integer -> Term Integer -> Term Integer
(-.) x y = App MinusW (x :> y :> Nil)


```

<a id="HasSpecSet"></a>
## HasSpec (Set a) instance

The `HasSpec` instance for `Set` is interesting, because

1. It has a rich set of function symbols,
2. It is the first type that we have seen that has a `Container` instance.
3. Introduces a rather complex `TypeSpec` to account for the first two.

It is best to start our explanation with the `Container` class. A type can have a `(Container t e)` instance
if the type `t` stores elements of type `e`, and we can enumerate over the `e` in `t`. The operation
on constainer types is that they support the quantifier `forAll` which states that every element `e` in `t`
meets some constraint.

```
class Container t e | t -> e where
  fromForAllSpec :: (HasSpec t, HasSpec e) => Spec e -> Spec t
  forAllToList :: t -> [e]

forAll :: (Container t a, HasSpec t, HasSpec a) => Term t -> (Term a -> Pred) -> Pred
```

Set supports the function symbols defined by the constructors of the `SetSym` datatype.

```
data SetSym (dom :: [Type]) rng where
  MemberW :: (HasSpec a, Ord a) => SetSym [a, Set a] Bool
  SizeW :: (HasSpec a, Ord a) => SetSym '[Set a] Integer
  SubsetW :: (HasSpec a, Ord a) => SetSym [Set a, Set a] Bool
```

So the type specific `TypeSpec` will have to encode at least 4 distinct properties.
Set membership, set size, subset, and the quantification over its elements.

We will use this type `SetSpec` as the `TypeSpec` for `Set` . Like many `TypeSpecs`,  it
has both `Semigroup` and `Monoid` instances

```
data SetSpec a = SetSpec {setMust :: Set a, setAll :: Spec a, setCount :: Spec Integer}

instance (Ord a, HasSpec a) => Semigroup (SetSpec a) where
  SetSpec must es size <> SetSpec must' es' size' =
    SetSpec (must <> must') (es <> es') (size <> size')

instance (Ord a, HasSpec a) => Monoid (SetSpec a) where
  mempty = SetSpec mempty mempty TrueSpec
```

We will complete the `HasSpec (Set a)` instance description in a number of steps.

1. Describe the `Container` instance
2. Describe the `Syntax` and `Semantics` instances for `SetSym`
3. Describe some helper functions needed to define the `HasSpec` instance
4. Describe the `HasSpec` instance
5. Describe the `Logic` instance of `SetSym`

### `Container` instance for `Set`

```
instance Ord s => Container (Set s) s where
  fromForAllSpec e = typeSpec $ SetSpec mempty e TrueSpec
  forAllToList = Set.toList
```

1. The method `forAllToList` turns all the elemenst in the container into list.
2. The method `fromForAllSpec` lifts a `Spec e` describing the property every
   element the set must have, into a `Spec t` describing the property the whole set should have.
   Due to to some forethought, this is quite easy. We embed `e` in the `setAll` field of `SetSpec`
   and fill the other components with with values that enforce no constraints.

### `Syntax` and `Semantics` instances for `SetSym`

```
deriving instance Eq (SetSym dom rng)

instance Show (SetSym dom rng) where show = name

instance Syntax SetSym where
  name MemberW = "member_"
  name SizeW = "size_"
  name SubsetW = "subset_"
  inFix _ = False

instance Semantics SetSym where
  semantics MemberW = Set.member
  semantics SizeW = setSize
  semantics SubsetW = Set.isSubsetOf

  rewriteRules SubsetW (Lit s :> _ :> Nil) Evidence | null s = Just $ Lit True
  rewriteRules SubsetW (x :> Lit s :> Nil) Evidence | null s = Just $ x ==. Lit Set.empty
  rewriteRules MemberW (t :> Lit s :> Nil) Evidence
    | null s = Just $ Lit False
    | [a] <- Set.toList s = Just $ t ==. Lit a
  rewriteRules t l Evidence = Lit <$> (applyFunSym @SetSym (semantics t) l)
```

Most of this is straight-forward. The only new part is that this is the first `Semantics` instance that supports
`rewriteRules` over the function symbols. Here are the rewrite rules in a more relaxed notation where `{a}`
denote a Set with 1 element `a`, and `{}` denotes the empty set.

- `subset_ (Lit {}) x       --> Lit True`
- `subset_ x (Lit {})       --> x ==. Lit {}`
- `member_ t (Lit {})       --> Lit False`
- `member_ t (Lit {a})      --> t ==. Lit a`
- `member_ (Lit t) (Lit ts) --> Lit (member t ts )`

###  Helper functions needed to define the `HasSpec (Set a)` instance

```
setSize :: Set a -> Integer
setSize = toInteger . Set.size

guardSetSpec :: (HasSpec a, Ord a) => SetSpec a -> Spec (Set a)
guardSetSpec (SetSpec must elemS ((<> geqSpec 0) -> size))
  | Just u <- knownUpperBound size
  , u < 0 =
      ErrorSpec (("guardSetSpec: negative size " ++ show u) :| [])
  | not (all (`conformsToSpec` elemS) must) =
      ErrorSpec (("Some 'must' items do not conform to 'element' spec: " ++ show elemS) :| [])
  | isErrorLike size = ErrorSpec ("guardSetSpec: error in size" :| [])
  | isErrorLike (geqSpec (setSize must) <> size) =
      ErrorSpec $
        ("Must set size " ++ show (setSize must) ++ ", is inconsistent with SetSpec size" ++ show size)
          :| []
  | otherwise = typeSpec (SetSpec must elemS size)

knownUpperBound :: Spec Integer -> Maybe Integer
knownUpperBound TrueSpec = Nothing
knownUpperBound (MemberSpec as) = Just $ maximum as
knownUpperBound ErrorSpec {} = Nothing
knownUpperBound SuspendedSpec {} = Nothing
knownUpperBound (TypeSpec (Interval lo hi) cant) = upper lo hi
  where
    upper _ Nothing = Nothing
    upper Nothing (Just b) = listToMaybe $ [b, b - 1 ..] \\ cant
    upper (Just a) (Just b)
      | a == b = a <$ guard (a `notElem` cant)
      | otherwise = listToMaybe $ [b, b - 1 .. a] \\ cant
```

1. `setSize` computes the size of a Haskell Set as an Integer (not an Int).
2. `knownUpperBound` computes a concrete upperbound from a Spec if one can be found.
3. `guardSetSpec` looks for inconsistencies in a `SetSpec`
      - We can compute an upperbound from the `setCount` field, and that 
	    upperbound is not positive, all set sizes are positive.
	  - Some element that must be in the set, does not meet the `setAll` field `Spec`
	  - The `setCount` field `Spec` is inconsistent

###  `HasSpec` instance for Set

```
instance (Container (Set a) a, Ord a, HasSpec a) => HasSpec (Set a) where
  type TypeSpec (Set a) = SetSpec a

  emptySpec = SetSpec Set.empty TrueSpec TrueSpec

  combineSpec x y = guardSetSpec (x <> y)

  conformsTo s (SetSpec must es size) =
    and
      [ setSize s `conformsToSpec` size
      , must `Set.isSubsetOf` s
      , all (`conformsToSpec` es) s
      ]

  toPreds s (SetSpec m es size) =
    Foldable.fold $
      -- Don't include this if the must set 'm' is empty
      [Assert $ subset_ (Lit m) s | not $ Set.null m]
        ++ [ forAll s (\e -> satisfies e es)
           , satisfies (size_ s) size
           ]

  guardTypeSpec = guardSetSpec

  genFromTypeSpec (SetSpec must e _)
    | any (not . (`conformsToSpec` e)) must =
        genErrorNE
          ( NE.fromList
              [ "Failed to generate set"
              , "Some element in the must set does not conform to the elem specification"
              , "Unconforming elements from the must set:"
              , unlines (map (\x -> "  " ++ show x) (filter (not . (`conformsToSpec` e)) (Set.toList must)))
              , "Element Specifcation"
              , "  " ++ show e
              ]
          )
  -- Special case when elemS is a MemberSpec.
  -- Just union 'must' with enough elements of 'xs' to meet  'szSpec'
  genFromTypeSpec (SetSpec must (MemberSpec xs) szSpec) = do
    let szSpec' = szSpec <> geqSpec (setSize must)
    choices <- pureGen $ shuffle (NE.toList xs \\ Set.toList must)
    size <- fromInteger <$> genFromSpecT szSpec'
    let additions = Set.fromList $ take (size - Set.size must) choices
    pure (Set.union must additions)
  genFromTypeSpec (SetSpec must elemS szSpec) = do
    let szSpec' = szSpec <> geqSpec (setSize must)
    sizecount <-
      explain "Choose a size for the Set to be generated" $
        genFromSpecT szSpec'
    let targetSize = sizecount - setSize must
    explainNE
      ( NE.fromList
          [ "Choose size count = " ++ show sizecount
          , "szSpec' = " ++ show szSpec'
          , "Picking items not in must = " ++ show (Set.toList must)
          , "that also meet the element test: "
          , "  " ++ show elemS
          ]
      )
      $ go 100 targetSize must
    where
      go _ n s | n <= 0 = pure s
      go tries n s = do
        e <-
          explainNE
            ( NE.fromList
                [ "Generate set member at type " ++ showType @a
                , "  number of items starting with  = " ++ show (Set.size must)
                , "  number of items left to pick   = " ++ show n
                , "  number of items already picked = " ++ show (Set.size s)
                ]
            )
            $ withMode Strict
            $ suchThatWithTryT tries (genFromSpecT elemS) (`Set.notMember` s)

        go tries (n - 1) (Set.insert e s)
```

1. The method `TypeSpec` has 3 components
    - `setMust :: Set a`. Elements that must be in the Set. Captures `memberW` function symbol information.
    - `setAll :: Spec a`. Property that must be True about all elements in the set.
          Captures `forAll` information from the `Container` class.
    - `setCount :: Spec Integer`. The size of the set. Captures `sizeW` function symbol information. 
2. The method `emptySpec`.  All three componets are set to `TrueSpec`
3. The method `combineSpec`. Guard the result of using the `Monoid (SetSpec t)` instance to combine the two inputs.
4. The method `conformsTo`. Individually test all three parts of the `SetSpec` against the input.
5. The method `toPreds` . Split the `setSpec` into 3 sets of `Pred`, one for each field, and join them all together
   using `Foldable.fold` that uses the `Monoid(Pred)` instance.
6. The method `guardTypeSpec`. Uses the `guardSetSpec` function described above.
7. The method `genFromSpecT`. This a complicated function that must deal with all tree fields of
   the `SetSpec` simultaneously. We will step through them one at a time.
     - The first clause `(genFromTypeSpec (SetSpec must e _))` tests for inconsistencies
	   between the `must` set and the `setAll` Spec `e` that must hold for every elememt.
     - The second clause `(genFromTypeSpec (SetSpec must (MemberSpec xs) szSpec))` handles
		 the case when the `setAll` Spec is a `(MemberSpec xs)`. This tells us that every element
		 must come from `xs`. We know the `must` set is a subset of `xs` or we wouldn't have gotten past
		 the first clause. We compute a bunch of `choices` from the elements in `xs` but not in `must`.
		 Then we pick a valid size, take the correct number of elements form the `choices`, call it `additions`,
		 and return the union of the `must` and `additions` set.
	 - The third clause `(genFromTypeSpec (SetSpec must elemS szSpec))` must satisfy all 3 sub-Specs with
	   no special information. So we start by picking a satisfying size `sizeCount`. Compute `targetSize` that
	   accounts for the `must` set, Then loop using `(go 100 targetSize must)`. This starts with the set `must`
	   then tries to add `targetSize` additional elements to `must` each of which meets the `setAll` spec `elemS`.
	   For each of the `targetSize` iterations, we get `100` tries to find an element that 
	   meets `elemS` and is not already in the set `s` we have accumulated so far.


### `Logic` instance of `SetSym`

```
instance Logic SetSym where
  propagate tag ctx spec = case (tag, ctx, spec) of
    (_, _, TrueSpec) -> TrueSpec
    (_, _, ErrorSpec es) -> ErrorSpec es
    (f, context, SuspendedSpec v ps) -> constrained $ \v' -> Let (App f (fromListCtx context v')) (v :-> ps)
    (MemberW, HOLE :<| (s :: Set a), spec1) ->
      caseBoolSpec spec1 $ \case
        True -> memberSpecList (Set.toList s) (pure "propagateSpecFun on (Member x s) where s is Set.empty")
        False -> notMemberSpec s
    (MemberW, e :|> HOLE, spec2) ->
      caseBoolSpec spec2 $ \case
        True -> typeSpec $ SetSpec (Set.singleton e) mempty mempty
        False -> typeSpec $ SetSpec mempty (notEqualSpec e) mempty
    (SizeW, Unary HOLE, spec3) -> typeSpec (SetSpec mempty mempty spec3)
    (SubsetW, HOLE :<| big, spec4) -> caseBoolSpec spec4 $ \case
      True -> constrained $ \small ->
        And
          [ Assert $ size_ small <=. Lit (setSize big)
          , forAll small $ \x -> Assert $ member_ x (Lit big)
          ]
      False -> constrained $ \small ->
        exists (\eval -> headGE $ Set.difference big (eval small)) $ \e ->
          And
            [ -- set `DependsOn` e,
              Assert $ not_ $ member_ e (Lit big)
            , Assert $ member_ e small
            ]
    (SubsetW, small :|> HOLE, spec5) -> caseBoolSpec spec5 $ \case
      True -> typeSpec $ SetSpec small TrueSpec mempty
      False -> constrained $ \big ->
        exists (\eval -> headGE $ Set.difference (eval big) small) $ \e ->
          And
            [ -- set `DependsOn` e,
              Assert $ member_ e (Lit small)
            , Assert $ not_ $ member_ e big
            ]
```



<a id="HasSpecPair"></a>
## HasSpec Pair (a,b) instance

Binary Tuples are a simple example of arbitrary Haskell tuples, which suggest how (3-tuples, 4-tuples, etc.) might be be defined.
We will discuss this generalization later. Binary tuples will need 3 function symbols, which will come from 
the `Syntax`, `Semantics` and `Logic` instances of `PairSym` 

```
data PairSym (dom :: [Type]) rng where
  FstW :: PairSym '[(a, b)] a
  SndW :: PairSym '[(a, b)] b
  PairW :: PairSym '[a, b] (a, b)
```

The `HasSpec` instance uses the `PairSpec` datatype as its `TypeSpec` family instance.
Like many `TypeSpec` it has `SemiGroup` and `Monoid` instances.
```
data PairSpec a b = Cartesian (Spec a) (Spec b)

instance (HasSpec a, HasSpec b) => Show (PairSpec a b) where
  show (Cartesian l r) = "(Cartesian " ++ "(" ++ show l ++ ") (" ++ show r ++ "))"

instance (HasSpec a, HasSpec b) => Semigroup (PairSpec a b) where
  (Cartesian x y) <> (Cartesian a b) = Cartesian (x <> a) (y <> b)

instance (HasSpec a, HasSpec b) => Monoid (PairSpec a b) where mempty = Cartesian mempty mempty

guardPair :: forall a b. (HasSpec a, HasSpec b) => Spec a -> Spec b -> Spec (a, b)
guardPair specA specB = handleErrors specA specB (\s t -> typeSpec (Cartesian s t))

-- | If either of the first two arguments are an ErrorSpec, return an ErrorSpec.
--   If Both are error free, then apply the third argument, a continuation, to
--   the error-free inputs. This pattern occurs frequently.
handleErrors :: Spec a -> Spec b -> (Spec a -> Spec b -> Spec c) -> Spec c
handleErrors spec1 spec2 f = case (hasError spec1, hasError spec2) of
  (Just m1, Just m2) -> ErrorSpec (m1 <> m2)
  (Just m1, _) -> ErrorSpec m1
  (_, Just m2) -> ErrorSpec m2
  (Nothing, Nothing) -> f spec1 spec2
```

The `HasSpec` is actually quite simple.

```
instance (HasSpec a, HasSpec b) => HasSpec (a, b) where
  type TypeSpec (a, b) = PairSpec a b

  emptySpec = Cartesian mempty mempty

  combineSpec (Cartesian a b) (Cartesian a' b') = guardPair (a <> a') (b <> b')

  conformsTo (a, b) (Cartesian sa sb) = conformsToSpec a sa && conformsToSpec b sb

  guardTypeSpec (Cartesian x y) = guardPair x y

  genFromTypeSpec (Cartesian sa sb) = (,) <$> genFromSpecT sa <*> genFromSpecT sb

  toPreds x (Cartesian sf ss) = satisfies (fst_ x) sf <> satisfies (snd_ x) ss
```

### Syntax, Semantics, and Logic instances for PairSym

```
deriving instance Eq (PairSym dom rng)

instance Show (PairSym dom rng) where show = name

instance Syntax PairSym where
  name FstW = "fst_"
  name SndW = "snd_"
  name PairW = "pair_"
  inFix _ = False

instance Semantics PairSym where
  semantics FstW = fst
  semantics SndW = snd
  semantics PairW = (,)
  rewriteRules FstW (Pair x _ :> Nil) Evidence = Just x
  rewriteRules SndW (Pair _ y :> Nil) Evidence = Just y
  rewriteRules t l Evidence = Lit <$> applyFunSym @PairSym (semantics t) l
```
The `Syntax` and `Semantics` instances are very simple, except for the `rewriteRules` which tell
how `FstW` and `SndW` project over a `PairW` application. The pattern matching over 
the over the application of the `PairW` application uses the Haskell Pattern synonym `Pair`

```
pattern Pair ::
  forall c. () => forall a b. (c ~ (a, b), HasSpec a, HasSpec b) => Term a -> Term b -> Term c
pattern Pair x y <- App (getWitness -> Just PairW) (x :> y :> Nil)
```


```
instance Logic PairSym where
  propagateTypeSpec FstW (Unary HOLE) ts cant = typeSpec $ Cartesian (TypeSpec ts cant) TrueSpec
  propagateTypeSpec SndW (Unary HOLE) ts cant = typeSpec $ Cartesian TrueSpec (TypeSpec ts cant)
  propagateTypeSpec PairW (a :|> HOLE) sc@(Cartesian sa sb) cant
    | a `conformsToSpec` sa = sb <> foldMap notEqualSpec (sameFst a cant)
    | otherwise =
        ErrorSpec
          ( NE.fromList
              ["propagate (pair_ " ++ show a ++ " HOLE) has conformance failure on a", show (TypeSpec sc cant)]
          )
  propagateTypeSpec PairW (HOLE :<| b) sc@(Cartesian sa sb) cant
    | b `conformsToSpec` sb = sa <> foldMap notEqualSpec (sameSnd b cant)
    | otherwise =
        ErrorSpec
          ( NE.fromList
              ["propagate (pair_ HOLE " ++ show b ++ ") has conformance failure on b", show (TypeSpec sc cant)]
          )

  propagateMemberSpec FstW (Unary HOLE) es = typeSpec $ Cartesian (MemberSpec es) TrueSpec
  propagateMemberSpec SndW (Unary HOLE) es = typeSpec $ Cartesian TrueSpec (MemberSpec es)
  propagateMemberSpec PairW (a :|> HOLE) es =
    case (nub (sameFst a (NE.toList es))) of
      (w : ws) -> MemberSpec (w :| ws)
      [] ->
        ErrorSpec $
          NE.fromList
            [ "propagate (pair_ HOLE " ++ show a ++ ") on (MemberSpec " ++ show (NE.toList es)
            , "Where " ++ show a ++ " does not appear as the fst component of anything in the MemberSpec."
            ]
  propagateMemberSpec PairW (HOLE :<| b) es =
    case (nub (sameSnd b (NE.toList es))) of
      (w : ws) -> MemberSpec (w :| ws)
      [] ->
        ErrorSpec $
          NE.fromList
            [ "propagate (pair_ HOLE " ++ show b ++ ") on (MemberSpec " ++ show (NE.toList es)
            , "Where " ++ show b ++ " does not appear as the snd component of anything in the MemberSpec."
            ]

sameFst :: Eq a1 => a1 -> [(a1, a2)] -> [a2]
sameFst a ps = [b | (a', b) <- ps, a == a']

sameSnd :: Eq a1 => a1 -> [(a2, a1)] -> [a2]
sameSnd b ps = [a | (a, b') <- ps, b == b']
```

## SolverPlans

The function `genFromSpecT` generates a random value from a `Spec`. Of the 5 constructors of `Spec`, the case analysis over
4 of them (`ErrorSpec`, `TrueSpec`, `TypeSpec`, and `MemberSpec`) is straight forward. The 5th, `SuspendedSpec` is more
involved, in that it consists of 1 or more `Pred`, and poossibly multiple variables. Here is a simplified version of `genFromSpecT`
to illustrate that point.

```
genFromSpecT ::
  forall a m. (HasCallStack, HasSpec a, MonadGenError m) => Spec a -> GenT m a
genFromSpecT (simplifySpec -> spec) = case spec of
  MemberSpec as -> pureGen (elements (NE.toList as))  -- Pick from `as`
  TrueSpec -> genFromSpecT (typeSpec $ emptySpec @a)  -- Use the type specific `genFromSpecT` on no constraints
  SuspendedSpec x preds -> do
        env <- genFromPreds mempty preds              -- Somehow solve all the `preds`
        findEnv env x
  TypeSpec s cant -> do                               -- Use the type specific `genFromSpecT, 
    mode <- getMode
    genFromTypeSpec s `suchThatT` (`notElem` cant)
  ErrorSpec e -> genErrorNE e                         -- raise an error.
```

The key to `genFromPreds` is to turn `preds` into a `SolverPlan` which consists of choosing an order
to solve all the variables in `preds`. For each variable, we create a `SolverStep` which includes that variable, 
and the subset of `preds` that mention it. Then solving each `SolverStep` in the order they appear in the plan.

Let's step throught the process on a simple `SuspendedSpec`

```
spec3 :: Spec (Integer, Integer, Integer)
spec3 = constrained $ \ v4 ->
  match v4 $ \ v3 v1 v0 -> And [Assert $ v3 <=. v1, Assert $ v1 <=. v0]
```

In the picture below, each `SolverStep` includes the variable, followed by `<-` and a list of sub predicates, terminated by `---`

```
 SolverPlan
    Linearization:
    v_0 <- 
	  ---
    v_1 <-
      assert $ v_1 <=. v_0
      ---
    v_3 <-
      assert $ v_3 <=. v_1
      ---
    v_2 <-
      assert $ fst_ v_2 ==. v_1
      assert $ snd_ v_2 ==. v_0
      ---
    v_4 <-
      assert $ head_ v_4 ==. v_3
      assert $ tail_ v_4 ==. v_2
      ---
```

The plan orders the variables in this order `[v_0, v_1, v_3, v_2, v_4]` It has the property that in each step, the sub predicates
mention only the variable for that step, and variables that appear in previous steps. The reader should verify that this is true. Solving
the plan, chooses the first variable and a conforming random value, adds it to the environment, and then substituting that value into
the rest of the plan, and then solving that shorter plan, until no variables are left. 
Choosing `v_0` and a conforming value `19`, note how the the `Pred` for `v_1` after substitution
`(assert $ v_1 <=. 19)` simplifies to the  `TypeSpec (Interval Nothing (Just 19)) []`

```
Env {v_0 -> 19}
Step v_1
  SolverPlan
  Linearization:
    v_1 <-
      TypeSpec (Interval Nothing (Just 19)) []
      ---
    v_3 <-
      assert $ v_3 <=. v_1
      ---
    v_2 <-
      TypeSpec (Cartesian (TrueSpec @(Integer)) (MemberSpec [ 19 ])) []
      assert $ fst_ v_2 ==. v_1
      ---
    v_4 <-
      assert $ head_ v_4 ==. v_3
      assert $ tail_ v_4 ==. v_2
      ---
```

Choosing `v_1` and a conforming value `-2`, and subsituting we get

```
Env {v_0 -> 19, v_1 -> -2}
Step v_3
  SolverPlan
  Linearization:
    v_3 <-
      TypeSpec (Interval Nothing (Just (-2))) []
      ---
    v_2 <-
      TypeSpec (Cartesian (MemberSpec [ -2 ]) (MemberSpec [ 19 ])) []
      ---
    v_4 <-
      assert $ head_ v_4 ==. v_3
      assert $ tail_ v_4 ==. v_2
```

Choosing `v_3`, and a conforming value `-29`, and substituting, we get

```
Env { v_0 -> 19, v_1 -> -2, v_3 -> -29 }
Step v_2
  SolverPlan
  Linearization:
    v_2 <-
      TypeSpec (Cartesian (MemberSpec [ -2 ]) (MemberSpec [ 19 ])) []
      ---
    v_4 <-
      TypeSpec (MemberSpec [ -29 ],TrueSpec @((Integer,Integer))) []
      assert $ tail_ v_4 ==. v_2
```	

Choosing `v_2`, note how the there is only one conforming solution `(-2,19)`, after substituting we get

```
Env { v_0 -> 19, v_1 -> -2, v_2 -> (-2,19), v_3 -> -29 }
Step v_4
  SolverPlan
  Linearization:
    v_4 <-
      MemberSpec [ (-29,-2,19) ]
```	

Finally there is only one variable left to choose `v_4`, and one conforming value, with the final environment

```
Env { v_0 -> 19, v_1 -> -2, v_2 -> (-2,19), v_3 -> -29 , v_4 -> (-29,-2,19) }
```

Since the solution to original `Spec`, is `v_4`, the solution is `(-29,-2,19)`, the other
variables are intermediate, and are used only to build the final solution.

```
spec3 = constrained $ \ v_4 -> 
        match v_4 $ \ v_3 v_1 v_0 -> And [Assert $ v_3 <=. v_1, Assert $ v_1 <=. v_0]
```

## Translating `Pred` to `Spec` and the use of `propagate`
