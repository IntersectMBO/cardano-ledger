-- | The goal of this module is to document where everything in the
-- `constrained-generators` library is located in order to provide a map of the
-- source code and hopefully explain at least partly how things hang together.
--
-- The goal of this module is _not_ to provide a full overview and explanation
-- of every module, function, type class, constraint, and concept in the
-- codebase. The import lists we give below are _not_ even complete! If you are
-- confused, go look at the documentation and definition pointed to in the
-- import list below, hopefully it will help!
module Everything where

-- First some utility modules that are used throughout the source code but
-- which don't depend on any of the constraint generation stuff.
import Constrained.TypeErrors (
  -- Here is where we find utility type families for expressing conditional
  -- type errors like:
  Computes, AssertComputes, AssertSpineComputes,
  -- And a re-export of GHC.TypeError containing e.g.
  TypeError, ErrorMessage(..)
  )

import Constrained.List (
  -- This module exports a rich interface for talking about dependently typed
  -- lists, or heterogeneous tuples, via the type:
  List, -- :: (k -> Type) -> [k] -> Type
  -- This module contains numerous utility functions:
  toList, mapList, mapListC, mapListC_,
  -- And various type families and constraints for talking about the indexing list:
  TypeList, Append, (:!), -- etc.
  -- As well as a zipper datatype for talking about _contexts_ over a list where there
  -- is a single hole, useful for talking about e.g. evaluation-contexts in function applications:
  ListCtx
  )

import Constrained.Graph (
  -- This module provides a graph implementation with some algorithms that will be used heavily
  -- when we are picking an order to generate random values for all the variables in a constraint:
  Graph(..),
  -- The operations are not called things like `insertNode` and `parents` or things that you'd normally
  -- find in a graph library, rather they make explicit that the graph is meant to talk about dependencies:
  dependency, -- :: Ord node => node -> Set node -> Graph node, inserts a node with a given set of dependencies
              -- in the graph
  noDependencies, -- :: Ord node => Set node -> Graph node
  transitiveClosure, -- Takes the transitive closure of dependencies in the graph
  topsort, findCycle, dependencies, -- etc. etc.
  )

import Constrained.GenT (
  -- This module provides a special-case monad transformer wrapper for `Gen`:
  GenT,
  -- Unlike a "normal" monad transformer wrapper it contains features that let you
  -- distinguish between different types of generation failure via:
  genError, fatalError,
  -- With backtracking of "explanations" to get sensible error messages:
  explain,
  -- Fifferent levels of tolerance for failure via:
  GenMode,
  -- And a bunch of useful functions for doing generation:
  oneofT, frequencyT, chooseT, sizeT, -- etc
  )
