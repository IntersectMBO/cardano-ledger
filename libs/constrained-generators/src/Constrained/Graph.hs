{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}

-- | This module provides a dependency graph implementation.
module Constrained.Graph (
  Graph,
  opGraph,
  nodes,
  deleteNode,
  subtractGraph,
  dependency,
  findCycle,
  dependsOn,
  dependencies,
  noDependencies,
  topsort,
  transitiveClosure,
  transitiveDependencies,
  irreflexiveDependencyOn,
) where

import Control.Monad
import Data.Foldable
import Data.List (sortOn)
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Maybe
import Data.Set (Set)
import Data.Set qualified as Set
import Prettyprinter

-- | A graph with unlabeled edges for keeping track of dependencies
data Graph node = Graph
  { edges :: !(Map node (Set node))
  , opEdges :: !(Map node (Set node))
  }
  deriving (Show)

instance Ord node => Semigroup (Graph node) where
  Graph e o <> Graph e' o' =
    Graph
      (Map.unionWith (<>) e e')
      (Map.unionWith (<>) o o')

instance Ord node => Monoid (Graph node) where
  mempty = Graph mempty mempty

instance Pretty n => Pretty (Graph n) where
  pretty gr =
    fold $
      punctuate
        hardline
        [ nest 4 $ pretty n <> " <- " <> brackets (fillSep (map pretty (Set.toList ns)))
        | (n, ns) <- Map.toList (edges gr)
        ]

-- | Get all the nodes of a graph
nodes :: Graph node -> Set node
nodes (Graph e _) = Map.keysSet e

-- | Delete a node from a graph
deleteNode :: Ord node => node -> Graph node -> Graph node
deleteNode x (Graph e o) = Graph (clean e) (clean o)
  where
    clean = Map.delete x . fmap (Set.delete x)

-- | Invert the graph
opGraph :: Graph node -> Graph node
opGraph (Graph e o) = Graph o e

-- | @subtractGraph g g'@ is the graph @g@ without the dependencies in @g'@
subtractGraph :: Ord node => Graph node -> Graph node -> Graph node
subtractGraph (Graph e o) (Graph e' o') =
  Graph
    (Map.differenceWith del e e')
    (Map.differenceWith del o o')
  where
    del x y = Just $ Set.difference x y

-- | @dependency x xs@ is the graph where @x@ depends on every node in @xs@
-- and there are no other dependencies.
dependency :: Ord node => node -> Set node -> Graph node
dependency x xs =
  Graph
    (Map.singleton x xs)
    ( Map.unionWith
        (<>)
        (Map.singleton x mempty)
        (Map.fromList [(y, Set.singleton x) | y <- Set.toList xs])
    )

-- | Every node in the first set depends on every node in the second set except themselves
irreflexiveDependencyOn :: Ord node => Set node -> Set node -> Graph node
irreflexiveDependencyOn xs ys =
  deps <> noDependencies ys
  where
    deps =
      Graph
        (Map.fromDistinctAscList [(x, Set.delete x ys) | x <- Set.toList xs])
        (Map.fromDistinctAscList [(a, Set.delete a xs) | a <- Set.toList ys])

-- | Get all down-stream dependencies of a node
transitiveDependencies :: Ord node => node -> Graph node -> Set node
transitiveDependencies x (Graph e _) = go (Set.singleton x) x
  where
    go seen y = ys <> foldMap (go $ Set.insert y seen) (Set.difference ys seen)
      where
        ys = fromMaybe mempty (Map.lookup y e)

-- | Take the transitive closure of the graph
transitiveClosure :: Ord node => Graph node -> Graph node
transitiveClosure g = foldMap (\x -> dependency x (transitiveDependencies x g)) (nodes g)

-- | The discrete graph containing all the input nodes without any dependencies
noDependencies :: Ord node => Set node -> Graph node
noDependencies ns = Graph nodeMap nodeMap
  where
    nodeMap = Map.fromList ((,mempty) <$> Set.toList ns)

-- | Topsort the graph, returning either @Right order@ if the graph is a DAG or
-- @Left cycle@  if it is not
topsort :: Ord node => Graph node -> Either [node] [node]
topsort gr@(Graph e _) = go [] e
  where
    go order g
      | null g = pure $ reverse order
      | otherwise = do
          let noDeps = Map.keysSet . Map.filter null $ g
              removeNode n ds = Set.difference ds noDeps <$ guard (not $ n `Set.member` noDeps)
          if not $ null noDeps
            then go (Set.toList noDeps ++ order) (Map.mapMaybeWithKey removeNode g)
            else Left . concat . take 1 . sortOn length . filter (not . null) . map (findCycle gr) $ Map.keys e

-- | Simple DFS cycle finding
-- TODO: tests for this, currently it can produce a stem with a cycle after it
findCycle :: Ord node => Graph node -> node -> [node]
findCycle (Graph e _) node = concat . take 1 $ go mempty node
  where
    go seen n
      | n `Set.member` seen = [[]]
      | otherwise = do
          n' <- neighbours
          (n :) <$> go (Set.insert n seen) n'
      where
        neighbours = maybe [] Set.toList $ Map.lookup n e

-- | Get the dependencies of a node in the graph, `mempty` if the node is not
-- in the graph
dependencies :: Ord node => node -> Graph node -> Set node
dependencies x (Graph e _) = fromMaybe mempty (Map.lookup x e)

-- | Check if a node depends on another in the graph
dependsOn :: Ord node => node -> node -> Graph node -> Bool
dependsOn x y g = y `Set.member` dependencies x g
