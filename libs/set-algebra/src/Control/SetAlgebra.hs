{-# LANGUAGE GADTs #-}

-- | Operations for manipulating sets and maps using mathematicial operators. Concrete types that can be
--   interpreted as sets and maps are made instances of the 'Basic' class. In order to make sets and maps
--   behave uniformly, an instance @(Basic f)@ implies @f@ is a binary type constructor. For types interpreted as
--   maps, the type
--  @(f k v)@ means that @k@ is the type of the map's key, and @v@ is the type of map's value. For types
--  interpreted as sets, the value is always the unit type: (). The binary GADT 'Sett' links to the
--  usual 'Data.Set' type. Its constructor has the following type @Sett :: Set k -> Sett k ()@, programmers can
--  use similar strategies to interpret other types as sets. Predefined instances of Basic include 'Data.Map',
--  'Data.Set', 'List', and 'Single'. Programmers can add 'Basic' instances for their own types as well.
--
--  A typical set algebra expression (involving range restriction, ('▷'), here) looks like @(eval (x ▷ y))@.
--  Where @x@ and @y@ are program variables or expressions, the operator ('▷') builds an 'Exp' tree, and
-- 'eval' simplifys the tree, and then evaluates the simplfied tree to get the result.
-- Here is the actual type of the range restrict operator.
--
-- @
-- (▷) :: (Ord k, Iter g, Ord v, HasExp s1 (f k v), HasExp s2 (g v ())) => s1 -> s2 -> Exp (f k v)
-- @
--
-- As the type indicates, in order to support simplifcation and evaluation the types of the
-- operands to ('▷') must be instances of several classes. Possible classes include 'Basic',
-- 'HasExp', 'Iter', and 'Embed'.
--
-- 1. @(Basic f)@ meaning @(f k v)@ must be interpreted as a map or set, with two type parameters @k@ and @v@.
-- 2. @(HasExp t (f k v))@  meaning a value of type @t@ can be lifted to an expression of type @(Exp (f k v))@, where @(Basic f)@.
-- 3. @(Iter f)@ meaning the @Basic@ type constructor @f@ supports certain (usually fast) operations, that can be combined.
-- 4. @(Embed concrete f)@ meaning the types @concrete@ and @(f k v)@ form an isomorphism.
--
-- Available operands to create set algebra expressions are 'dom', 'rng', 'dexclude',  '(⋪)', 'drestrict', '(◁)',
-- 'rexclude', '(⋫)', 'rrestrict',  '(▷)',
-- 'unionright',  '(⨃)', 'unionleft','(∪)', 'unionplus',  '(∪+)','singleton', 'setSingleton',
-- 'intersect', '(∩)',  'subset',  '(⊆)', 'keyeq',  '(≍)',
-- '(∈)', '(∉)',  'setdiff', '(➖)' .
--
-- The key abstraction that makes set algebra work is the self typed GADT: @(Exp t)@, that defines a tree that
-- represents a deep embedding of all set algebra expressions representing maps or sets of type @t@.
-- @Exp@ is a typed symbolic representation of queries we may ask. It allows us to introspect a query.
-- The strategy is to
--
-- 1. Define Exp so all queries can be represented.
-- 2. Define smart constructors that "parse" the surface syntax, and build a typed Exp
-- 3. Write an evaluate function:  eval:: Exp t -> t
-- 4. "eval" can introspect the code and apply efficient domain and type specific translations
-- 5. Use the (Iter f) class to evaluate some Exp that can benefit from its efficient nature.
--
-- Basically, if the compiler can infer concrete type for the operands of [operators](Control-SetAlgebra.html#setoperators#) then
-- all the class instances are automatically solved. If you get an error involving a class, then it is most
-- probably the case that the type of the operands cannot be properly inferred.
module Control.SetAlgebra (
  -- * In addition to 'Data.Map.Map' and 'Data.Set.Set', types interpretable as maps and sets.
  -- $MapAndSetTypes
  List,
  Single (..),

  -- * Classes supporting abstract constructors of Set Algebra Expressions. These show up in the types of overloaded functions.
  -- $ClassesForSetAlgebra
  Basic (..),
  Iter (..),
  HasExp (..),
  Embed (..),

  -- * Types implementing a deep embedding of set algebra expressions
  -- $Deep
  BaseRep (..),
  Exp (Base),
  -- Evaluate an abstract Set Algebra Expression to the Set (Map) it represents.
  eval,

  -- * Operators to build maps and sets,  useable as Set Algebra Expressions
  -- $setoperators
  dom,
  rng,
  dexclude,
  drestrict,
  rexclude,
  rrestrict,
  unionleft,
  unionright,
  unionplus,
  singleton,
  setSingleton,
  intersect,
  subset,
  keyeq,
  (◁),
  (⋪),
  (▷),
  (⋫),
  (∈),
  (∉),
  (∪),
  (⨃),
  (∪+),
  (∩),
  (⊆),
  (≍),
  (<|),
  (|>),
  (➖),
  keysEqual,
  setdiff,

  -- * Miscellaneous operators, including smart constructors for 'List', whose constructors are hidden.
  -- $Misc
  materialize,
  fromList,
)
where

import Control.Iterate.BaseTypes (
  BaseRep (..),
  Basic (..),
  Embed (..),
  Iter (..),
  List,
  Single (..),
 )
import Control.Iterate.Exp (
  Exp (..),
  HasExp (..),
  dexclude,
  dom,
  drestrict,
  intersect,
  keyeq,
  rexclude,
  rng,
  rrestrict,
  setSingleton,
  setdiff,
  singleton,
  subset,
  unionleft,
  unionplus,
  unionright,
  (<|),
  (|>),
  (∈),
  (∉),
  (∩),
  (∪),
  (∪+),
  (≍),
  (⊆),
  (⋪),
  (⋫),
  (▷),
  (◁),
  (➖),
  (⨃),
 )
import Control.Iterate.SetAlgebra
import Data.MapExtras (keysEqual)
