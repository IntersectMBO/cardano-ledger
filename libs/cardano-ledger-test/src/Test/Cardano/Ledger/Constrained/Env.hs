{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE StandaloneDeriving #-}

-- | Provides variables (V era t), and mappings of them to objects of type 't'
module Test.Cardano.Ledger.Constrained.Env (
  V (..),
  W (..),
  AnyW (..),
  vToW,
  wToV,
  Env (..),
  Payload (..),
  emptyEnv,
  findVar,
  storeVar,
  P (..),
  bulkStore,
  Dyn (..),
  Name (..),
  Access (..),
  Field,
) where

import qualified Data.List as List
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe (isJust)
import Data.Universe (Shaped (..))
import Test.Cardano.Ledger.Constrained.Monad (Dyn (..), Typed (..), failT)
import Test.Cardano.Ledger.Constrained.TypeRep

import Cardano.Ledger.Shelley.LedgerState (NewEpochState)
import Data.List (intercalate)
import Lens.Micro

-- ================================================================

data Access era s t where
  Yes :: forall s t era. Rep era s -> Lens' s t -> Access era s t
  No :: Access era s t

type Field era x = Lens' (NewEpochState era) x

data AnyW era s where
  AnyW :: (Show t, Eq t) => W era s t -> AnyW era s

instance Show (AnyW era s) where
  show (AnyW (W n r _)) = "Field " ++ n ++ " " ++ show r
  show (AnyW (WConst t No)) = "Const " ++ show t
  show (AnyW (WConst t (Yes r _))) = "Const " ++ show t ++ " " ++ show r

data V era t where V :: String -> Rep era t -> Access era s t -> V era t


instance Show (V era t) where
  show (V nm rep _) = nm ++ " :: " ++ show rep

data W era s t where
  W :: Eq t => String -> Rep era t -> Access era s t -> W era s t
  WConst :: Eq t => t -> Access era s t -> W era s t

instance Show (W era s t) where
  show (W n r _) = intercalate " " ["W", show n, show r]
  show (WConst _ _) = "WConst"

vToW :: Eq t => Rep era s -> V era t -> Typed (W era s t)
vToW reps (V name rept access@(Yes reps' _)) = case testEql reps reps' of
  Just Refl -> pure $ W name rept access
  Nothing ->
    failT
      [ "Given rep and lens target do not match: "
      , "rep: " ++ show reps
      , "lens target: " ++ show reps'
      ]
vToW _ (V name rep No) = pure $ W name rep No

wToV :: W era s t -> Typed (V era t)
wToV (W name rep access) = pure $ V name rep access
wToV (WConst _ _) = failT ["Cannot convert a WConst to a V"]


data Payload era where
  Payload :: Rep era t -> t -> Access era s t -> Payload era

instance Shaped (V era) (Rep era) where
  shape (V n1 rep _) = Nary 0 [Esc StringR n1, shape rep]

-- We are ignoring the Accessfield on purpose

data Env era = Env (Map String (Payload era))

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

-- ============================================
-- Group a bunch of bindings into a list

data P era where P :: V era t -> t -> P era

instance Show (P era) where
  show (P (V nm rep _) t) = nm ++ " = " ++ synopsis rep t
  showList xs ans = unlines (ans : (map show xs))

bulkStore :: [P era] -> Env era -> Env era
bulkStore ps env = List.foldl' accum env ps
  where
    accum e (P v t) = storeVar v t e

-- ===================================

-- | An existentially quantified (V era t), hiding the 't'
--   Usefull because unlike (V era t), it has both Eq and Ord instances
data Name era where Name :: V era t -> Name era

instance Show (Name era) where
  show (Name (V n _ _)) = n

-- | Does not satisfy extensionality
instance Eq (Name era) where
  Name (V n1 rep1 _) == Name (V n2 rep2 _) = n1 == n2 && isJust (testEql rep1 rep2)

instance Ord (Name era) where
  compare v1@(Name (V n1 rep1 _)) v2@(Name (V n2 rep2 _)) =
    if v1 == v2
      then EQ
      else case compare n1 n2 of
        LT -> LT
        GT -> GT
        EQ -> compareRep rep1 rep2
