{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

-- | Provides variables (V era t), and mappings of them to objects of type 't'
module Test.Cardano.Ledger.Constrained.Env (
  V (V, VRaw),
  pV,
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
  otherFromEnv,
  sameName,
) where

import Cardano.Ledger.Era (Era)
import Data.Hashable
import Data.List (intercalate)
import qualified Data.List as List
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Universe (Shape (..), Shaped (..))
import Lens.Micro
import Test.Cardano.Ledger.Constrained.Monad (Typed (..), failT)
import Test.Cardano.Ledger.Constrained.TypeRep
import Test.Cardano.Ledger.Generic.Proof (BabbageEra, ConwayEra, StandardCrypto)

-- ================================================================
-- V

-- | A proto variable. May or may not contain a Lens (encoded as Access)
data V era t where
  VRaw :: Era era => String -> Rep era t -> Int -> Access era s t -> V era t

data Access era s t where
  Yes :: Rep era s -> Lens' s t -> Access era s t
  No :: Access era s t

instance Show (V era t) where
  show (VRaw nm rep _ _) = nm ++ " :: " ++ show rep

-- | We make hashing of 'V' cheap by memoizing most of the hash computation in in the
--   3rd argument of 'VRaw', we provide this pattern to hide this from every other use.
pattern V :: () => Era era => String -> Rep era t -> Access era s t -> V era t
pattern V s r a <- VRaw s r _ a
  where
    V s r a = VRaw s r (0 `hashWithSalt` s `hashWithSalt` r) a

-- | Construct a V, dsicharging the (Era era) constraint, using the Proof
pV :: Proof era -> String -> Rep era t -> Access era s t -> V era t
pV Shelley = V
pV Allegra = V
pV Mary = V
pV Alonzo = V
pV Babbage = V
pV Conway = V

{-# COMPLETE V #-}

-- ===========================================================
-- Name

-- | An existentially quantified (V era t), hiding the 't'
--   The Hashable instance is inherited from 'V'
data Name era where
  Name :: V era t -> Name era

instance Show (Name era) where
  show (Name (VRaw n _ _ _)) = n

-- | Does not satisfy extensionality
instance Eq (Name era) where
  Name (V n1 rep1 _) == Name (V n2 rep2 _) = case testEql rep1 rep2 of
    Nothing -> False
    Just Refl -> n1 == n2
  {-# INLINE (==) #-}

instance Ord (Name era) where
  {-# SPECIALIZE instance Ord (Name (BabbageEra StandardCrypto)) #-}
  {-# SPECIALIZE instance Ord (Name (ConwayEra StandardCrypto)) #-}
  compare (Name (V n1 rep1 _)) (Name (V n2 rep2 _)) =
    case compare n1 n2 of
      EQ -> cmpIndex rep1 rep2
      other -> other
  {-# INLINE compare #-}

sameName :: V era t -> V era s -> Maybe (t :~: s)
sameName (V x r1 _) (V y r2 _) | x == y = testEql r1 r2
sameName _ _ = Nothing

-- ================================================================
-- Field

-- | Fields are like V, except they expose the type of the Lens
data Field era s t where
  Field :: Era era => String -> Rep era t -> Rep era s -> Lens' s t -> Field era s t
  FConst :: Rep era t -> t -> Rep era s -> Lens' s t -> Field era s t

-- SubField :: String -> Rep era t -> Access era s t -> Field era t r -> Field era s t

instance Show (Field era s t) where
  show (Field n t s _) = intercalate " " ["Field", show n, show s, show t]
  show (FConst r t _ _) = "FConst " ++ synopsis r t

-- | Hide the type of 't' in a Field.
data AnyF era s where
  AnyF :: -- Eq t =>
    Field era s t -> AnyF era s

instance Show (AnyF era s) where
  show (AnyF (Field n r t _)) = "Field " ++ n ++ " " ++ show t ++ " " ++ show r
  show (AnyF (FConst r t _ _)) = "FConst " ++ synopsis r t

vToField :: Era era => Rep era s -> V era t -> Typed (Field era s t)
vToField reps (V name rept (Yes reps' l)) = case testEql reps reps' of
  Just Refl -> pure $ Field name rept reps l
  Nothing ->
    failT
      [ "Given rep and lens target do not match: "
      , "rep: " ++ show reps
      , "lens target: " ++ show reps'
      ]
vToField _ v@(V _ _ No) = failT ["Cannot convert a V with a No access to a Field", show v]

fieldToV :: Era era => Field era s t -> Typed (V era t)
fieldToV (Field name rep repx l) = pure $ V name rep (Yes repx l)
fieldToV (FConst _ _ _ _) = failT ["Cannot convert a FieldConst to a V"]

-- ===================================================
-- Env

data Payload era where
  Payload :: Rep era t -> t -> Access era s t -> Payload era

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
        Nothing ->
          failT
            ["We found: " ++ name ++ ", but the types did not match. " ++ show rep1 ++ " =/= " ++ show rep2]

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

otherFromEnv :: [String] -> Env era -> [String]
otherFromEnv known (Env m) = [n ++ " = " ++ synopsis r t | (n, Payload r t _) <- Map.toList m, not (elem n known)]

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

-- ==========================================================================
--  Hashable instances so we can make HashSets of Name
--  Something like 60% of all time consumed involves manipulating sets of Name
--  If the set is Data.Set, then we need instance Ord (Name era). This is very expensive
--  So instead we use Data.HashSet where we need instance Hashable(Name era). We can
--  make this very cheap by memoizing most of the hash computation in 'V' in the
--  constructor 'VRaw' and hiding 'VRaw' with pattern 'V'

-- | Inheriting the Hashable instancefor types that have Shapes, Works for types whose
--   Shaped instances don't mention 'Esc', Like Evidence, Proof, Rep
--   But we CANNOT use this strategy for (V era t)  since its's Shape instance use Esc
instance Hashable (Shape a) where
  hashWithSalt s (Nullary n) = s `hashWithSalt` (0 :: Int) `hashWithSalt` n
  hashWithSalt s (Nary n xs) = s `hashWithSalt` (1 :: Int) `hashWithSalt` n `hashWithSalt` xs
  hashWithSalt s (Esc _ _) = s `hashWithSalt` (2 :: Int)
  {-# INLINE hashWithSalt #-}

instance Hashable (Proof e) where
  hashWithSalt s x = s `hashWithSalt` (shape x)
  {-# INLINE hashWithSalt #-}

instance Eq (Proof e) where
  x == y = shape x == shape y
  {-# INLINE (==) #-}

instance Hashable (Rep e t) where
  hashWithSalt s r = s `hashWithSalt` typeRepOf r
  {-# INLINE hashWithSalt #-}

instance Eq (Rep e t) where
  x == y = typeRepOf x == typeRepOf y
  {-# INLINE (==) #-}

instance Hashable (V era t) where
  hashWithSalt s (VRaw _ _ h _) = s `hashWithSalt` h
  {-# INLINE hashWithSalt #-}

instance Eq (V era t) where
  (V n1 r1 _) == (V n2 r2 _) = n1 == n2 && r1 == r2
  {-# INLINE (==) #-}

instance Era era => Hashable (Name era) where
  hashWithSalt s (Name (VRaw _ _ h _)) = s `hashWithSalt` h
  {-# INLINE hashWithSalt #-}
