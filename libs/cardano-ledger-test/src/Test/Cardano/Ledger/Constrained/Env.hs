{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE StandaloneDeriving #-}

-- | Provides variables (V era t), and mappings of them to objects of type 't'
module Test.Cardano.Ledger.Constrained.Env (
  V (..),
  Field (..),
  AnyF (..),
  vToField,
  fieldToV,
  Env (..),
  Payload (..),
  emptyEnv,
  findVar,
  storeVar,
  findName,
  storeName,
  restrictEnv,
  P (..),
  bulkStore,
  Name (..),
  Access (..),
) where

import Data.List (intercalate)
import qualified Data.List as List
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe (isJust)
import Data.Universe (Shaped (..))
import Lens.Micro
import Test.Cardano.Ledger.Constrained.Monad (Typed (..), failT)
import Test.Cardano.Ledger.Constrained.TypeRep

-- ================================================================
-- V

-- | A proto variable. May or may not contain a Lens (encoded as Access)
data V era t where
  V :: String -> Rep era t -> Access era s t -> V era t

data Access era s t where
  Yes :: Rep era s -> Lens' s t -> Access era s t
  No :: Access era s t

instance Show (V era t) where
  show (V nm rep _) = nm ++ " :: " ++ show rep

-- ===========================================================
-- Name

-- | An existentially quantified (V era t), hiding the 't'
--   Usefull because unlike (V era t), it has both Eq and Ord instances
data Name era where
  Name :: V era t -> Name era

instance Show (Name era) where
  show (Name (V n _ _)) = n

-- | Does not satisfy extensionality
instance Eq (Name era) where
  Name (V n1 rep1 _) == Name (V n2 rep2 _) = n1 == n2 && isJust (testEql rep1 rep2)

instance Ord (Name era) where
  compare v1@(Name (V n1 rep1 _)) v2@(Name (V n2 rep2 _)) =
    if v1 == v2
      then EQ
      else compare n1 n2 <> compareRep rep1 rep2

-- ================================================================
-- Field

-- | Fields are like V, except they expose the type of the Lens
data Field era s t where
  Field :: Eq t => String -> Rep era t -> Access era s t -> Field era s t
  FConst :: Eq t => Rep era t -> t -> Access era s t -> Field era s t

instance Show (Field era s t) where
  show (Field n r _) = intercalate " " ["Field", show n, show r]
  show (FConst r t _) = "FConst " ++ synopsis r t

-- | Hide the type of 't' in a Field.
data AnyF era s where
  AnyF :: (Show t, Eq t) => Field era s t -> AnyF era s

instance Show (AnyF era s) where
  show (AnyF (Field n r _)) = "Field " ++ n ++ " " ++ show r
  show (AnyF (FConst r t _)) = "FConst " ++ synopsis r t

vToField :: Eq t => Rep era s -> V era t -> Typed (Field era s t)
vToField reps (V name rept access@(Yes reps' _)) = case testEql reps reps' of
  Just Refl -> pure $ Field name rept access
  Nothing ->
    failT
      [ "Given rep and lens target do not match: "
      , "rep: " ++ show reps
      , "lens target: " ++ show reps'
      ]
vToField _ (V name rep No) = pure $ Field name rep No

fieldToV :: Field era s t -> Typed (V era t)
fieldToV (Field name rep access) = pure $ V name rep access
fieldToV (FConst _ _ _) = failT ["Cannot convert a FieldConst to a V"]

-- ===================================================
-- Env

data Payload era where
  Payload :: Rep era t -> t -> Access era s t -> Payload era

instance Shaped (V era) (Rep era) where
  shape (V n1 rep _) = Nary 0 [Esc (ListR CharR) n1, shape rep]

-- We are ignoring the Accessfield on purpose

newtype Env era = Env (Map String (Payload era))

instance Show (Env era) where
  show (Env m) = unlines (map f (Map.toList m))
    where
      f (nm, Payload rep t _) = nm ++ " -> " ++ synopsis rep t

emptyEnv :: Env era
emptyEnv = Env Map.empty

findVar :: V era t -> Env era -> Typed t
findVar (V name rep1 _) (Env m) =
  case Map.lookup name m of
    Nothing -> failT ["Cannot find " ++ name ++ " in env"]
    Just (Payload rep2 t _) ->
      case testEql rep1 rep2 of
        Just Refl -> pure t
        Nothing -> failT ["We found: " ++ name ++ ", but the types did not match. " ++ show rep1 ++ " =/= " ++ show rep2]

storeVar :: V era t -> t -> Env era -> Env era
storeVar (V name rep access) t (Env m) = Env (Map.insert name (Payload rep t access) m)

-- | Untyped version of 'findVar'.
findName :: Name era -> Env era -> Maybe (Payload era)
findName (Name (V name _ _)) (Env env) = Map.lookup name env

-- | Untyped version of 'storeVar'.
storeName :: Name era -> Payload era -> Env era -> Env era
storeName (Name (V name _ _)) p (Env env) = Env $ Map.insert name p env

-- | Drop any names that are not in the given list from an environment.
restrictEnv :: [Name era] -> Env era -> Env era
restrictEnv names (Env env) = Env $ Map.filterWithKey (\x _ -> elem x xs) env
  where
    xs = [x | Name (V x _ _) <- names]

-- ============================================
-- Group a bunch of bindings into a list

data P era where
  P :: V era t -> t -> P era

instance Show (P era) where
  show (P (V nm rep _) t) = nm ++ " = " ++ synopsis rep t
  showList xs ans = unlines (ans : (map show xs))

bulkStore :: [P era] -> Env era -> Env era
bulkStore ps env = List.foldl' accum env ps
  where
    accum e (P v t) = storeVar v t e

-- ===================================
