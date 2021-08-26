-- | Defined Message types that stand for changes to a Map.Map
module Data.Messages where

import Cardano.Prelude (HeapWords (..))
import qualified Data.Map.Strict as Map

-- ===================================================================

-- | First order (Symbolic) representaion of functions
data Function v = Identity | Plus v | Compose (Function v) (Function v)
  deriving (Show)

-- | The operations needed to apply a symbolic function
--   This class might need additional methods if the data Function adds new constructors
class Exp t where
  plus :: t -> t -> t

-- | Turn a First order data structure into a function
applyExp :: Exp v => Function v -> v -> v
applyExp Identity x = x
applyExp (Plus n) x = plus n x
applyExp (Compose f g) x = applyExp f (applyExp g x)

-- | Symbolically simpify the composition of Two Function objects
compose :: Exp v => Function v -> Function v -> Function v
compose Identity x = x
compose (p@(Plus _)) Identity = p
compose (Plus n) (Plus m) = Plus (plus n m)
compose (p@(Plus _)) (Compose f g) = compose (compose p f) g
compose (Compose f g) Identity = compose f g
compose (Compose f g) (p@(Plus _)) = compose f (compose g p)
compose (Compose f g) (Compose i j) = compose (compose f g) (compose i j)

instance Exp v => Semigroup (Function v) where
  (<>) = compose

instance Exp v => Monoid (Function v) where
  mempty = Identity

-- =======================================================

data Message v
  = Edit v -- Change the value to v, if it is there, otherwise insert v if it is not
  | Delete
  | Upsert (Function v)

-- Composition of Messages, apply the operator on the right first
merge :: Exp v => Message v -> Message v -> Message v
merge Delete _ = Delete
merge (e@(Edit _)) Delete = e
merge (e@(Edit _)) (Edit _) = e
merge (e@(Edit _)) (Upsert _) = e
merge (Upsert Identity) (e@(Edit _)) = e
merge (Upsert Identity) Delete = Delete
merge (Upsert Identity) (u@(Upsert _)) = u
merge (Upsert (Plus v)) (Edit x) = Edit (plus v x)
merge (Upsert (Plus _)) Delete = Delete
merge (Upsert (Plus v)) (Upsert e) = Upsert (compose (Plus v) e)
merge (Upsert (Compose f g)) x = merge (Upsert f) (merge (Upsert g) x)

-- ==========================================================================

-- Lift Messages from points to collections of points, indexed by keys 'k'
newtype Delta k v = Delta (Map.Map k (Message v))

instance (Show k, Show v) => Show (Delta k v) where
  show (Delta m) = "{" ++ help (Map.toList m)
    where
      help [] = "}"
      help [pair] = show pair ++ "}"
      help (pair : more) = show pair ++ "," ++ help more

-- Apply a set of Messages to update a Map.
applyMessages :: (Ord k, Exp v) => Map.Map k v -> Delta k v -> Map.Map k v
applyMessages m (Delta messages) = Map.foldlWithKey' acc m messages
  where
    acc ans key Delete = Map.delete key ans
    acc ans key (Edit v) = Map.insert key v ans
    acc ans key (Upsert e) = Map.update (Just . applyExp e) key ans

-- ==============================================
-- Instance Monoid for Message and Delta

instance Exp v => Semigroup (Message v) where
  (<>) = merge

instance Exp v => Monoid (Message v) where
  mempty = Upsert Identity
  mappend = (<>)

instance (Ord k, Exp v) => Semigroup (Delta k v) where
  (<>) (Delta x) (Delta y) = Delta (Map.unionWith merge x y)

instance (Ord k, Exp v) => Monoid (Delta k v) where
  mempty = Delta (Map.empty)

-- =======================================
-- Show instances

instance Show v => Show (Message v) where
  show (Edit n) = "(Edit " ++ show n ++ ")"
  show Delete = "Delete"
  show (Upsert x) = "(Upsert " ++ show x ++ ")"

instance Exp Int where plus x y = x + y

instance (HeapWords k, HeapWords v) => HeapWords (Delta k v) where
  heapWords (Delta x) = heapWords x

instance HeapWords v => HeapWords (Message v) where
  heapWords (Edit x) = 2 + heapWords x
  heapWords Delete = 1
  heapWords (Upsert x) = 2 + heapWords x

instance HeapWords v => HeapWords (Function v) where
  heapWords Identity = 1
  heapWords (Plus x) = 2 + heapWords x
  heapWords (Compose x y) = 3 + heapWords x + heapWords y
