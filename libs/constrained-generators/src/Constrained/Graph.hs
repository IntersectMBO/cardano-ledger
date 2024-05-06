{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE ViewPatterns #-}

module Constrained.Graph where

import Control.Monad
import Data.List (sortOn)
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Maybe
import Data.Set (Set)
import Data.Set qualified as Set
import Prettyprinter

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
  pretty gr = vsep [nest 2 $ pretty n <> " <- " <> pretty (Set.toList ns) | (n, ns) <- Map.toList (edges gr)]

nodes :: Graph node -> Set node
nodes (Graph e _) = Map.keysSet e

opGraph :: Graph node -> Graph node
opGraph (Graph e o) = Graph o e

subtractGraph :: Ord node => Graph node -> Graph node -> Graph node
subtractGraph (Graph e o) (Graph e' o') =
  Graph
    (Map.differenceWith del e e')
    (Map.differenceWith del o o')
  where
    del x y = Just $ Set.difference x y

dependency :: Ord node => node -> Set node -> Graph node
dependency x xs =
  Graph
    (Map.singleton x xs)
    ( Map.unionWith
        (<>)
        (Map.singleton x mempty)
        (Map.fromList [(y, Set.singleton x) | y <- Set.toList xs])
    )

irreflexiveDependencyOn :: Ord node => Set node -> Set node -> Graph node
irreflexiveDependencyOn xs ys =
  deps <> noDependencies ys
  where
    deps =
      Graph
        (Map.fromDistinctAscList [(x, Set.delete x ys) | x <- Set.toList xs])
        ( Map.fromDistinctAscList [(a, Set.delete a xs) | a <- Set.toList ys]
        )

transitiveDependencies :: Ord node => node -> Graph node -> Set node
transitiveDependencies x (Graph e _) = go (Set.singleton x) x
  where
    go seen y = ys <> foldMap (go $ Set.insert y seen) (Set.difference ys seen)
      where
        ys = fromMaybe mempty (Map.lookup y e)

transitiveClosure :: Ord node => Graph node -> Graph node
transitiveClosure g = foldMap (\x -> dependency x (transitiveDependencies x g)) (nodes g)

noDependencies :: Ord node => Set node -> Graph node
noDependencies ns = Graph nodeMap nodeMap
  where
    nodeMap = Map.fromList ((,mempty) <$> Set.toList ns)

-- | Topsort the graph, returning a cycle
-- (`Left cycle`) on failure.
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

dependencies :: Ord node => node -> Graph node -> Set node
dependencies x (Graph e _) = fromMaybe mempty (Map.lookup x e)

dependsOn :: Ord node => node -> node -> Graph node -> Bool
dependsOn x y g = y `Set.member` dependencies x g

deleteNode :: Ord node => node -> Graph node -> Graph node
deleteNode x (Graph e o) =
  Graph
    (Map.delete x $ fmap (Set.delete x) e)
    (Map.delete x $ fmap (Set.delete x) o)
