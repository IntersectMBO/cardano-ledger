{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE StandaloneDeriving  #-}
{-# LANGUAGE DerivingVia #-}

-- | Supports the creation of objects with type (Closure name env (a -> b)) which are serializable functions.
module Data.Closure
   ( Named(..),
     Closure(..),
     apply,
     rootName,
     roundtrip,
   )  where

import Data.Kind(Type)
import Cardano.Binary(ToCBOR(..),FromCBOR(..),serialize', decodeFull')
import Data.Coders
import Data.Typeable(Typeable)
import NoThunks.Class(NoThunks,InspectHeapNamed(..))
import Control.DeepSeq (NFData,rnf)

-- ================================================

{- | The type 'name', Names a unique value of type 'v'. Usually 'name'
     is a Unit type, An enumeration with one constructor. When this is the
     case, it creates a 1-to-1 correspondance between a type and a value, also
     a 1-to-1 corresponance between the unique value of type 'name' and the value
-}
class Eq name => Named name v | name -> v where
  value :: name -> v
  name :: name -> String

infixl 0 :$

-- | A Closure is a serializable function. Apply it using 'apply'
data Closure name (env::[Type]) t where
  Close:: Named name (a -> b) => !name -> Closure name '[] (a -> b)
  (:$):: !(Closure name env (a -> b)) -> !a -> Closure name (a ': env) b

deriving via InspectHeapNamed "Closure" (Closure name env t) instance NoThunks (Closure name env t)

-- | Lift a Closure to a real function
apply :: Closure name env (a -> b) -> a -> b
apply (Close nm) a = value nm a
apply (cl :$ x) a = (apply cl x) a

-- | Get the 'name' value of the underlying function in the closure
rootName :: Closure name env x -> String
rootName (Close nm) = name nm
rootName ( cl :$ _ ) = rootName cl

-- =====================================================================================
-- Class instances for Closure come in pairs,
-- one for the empty environment,       P(Closure n '[] t),
-- and one for a non-empty environment, P(Closure n (a ': e) (a->b))

-- ============
-- NFData pair

instance NFData name => NFData (Closure name '[]  x) where
  rnf (Close x) = rnf x

instance (NFData a, NFData (Closure name env (a -> x))) => NFData (Closure name (a ': env) x) where
  rnf (cl :$ x) = seq (rnf cl) (rnf x)

-- ==========
-- Eq pair

instance Eq (Closure name '[]  x) where
  (Close x) == (Close y) = x==y

instance (Eq a, Eq (Closure name env (a -> x))) => Eq (Closure name (a ': env) x) where
  (cl1 :$ x1) == (cl2 :$ x2) = cl1==cl2 && x1 == x2

-- ===========
-- Show pair

instance Show (Closure name '[]  x) where
  show (Close x) = name x

instance (Show a, Show (Closure name env (a -> x))) => Show (Closure name (a ': env) x) where
  show (cl :$ x) = show cl++" :$ "++show x

-- ============
-- ToCBOR pair

instance (ToCBOR name, Typeable x) => ToCBOR (Closure name '[]  x) where
  toCBOR (Close nm) = encode(Sum Close 0 !> To nm)

instance
    ( Typeable env,
      Typeable name,
      Typeable x,
      ToCBOR a,
      ToCBOR (Closure name env (a -> x))
    ) => ToCBOR (Closure name (a ': env) x) where
  toCBOR (cl :$ a) = encode(Sum (:$) 1 !> To cl !> To a)

-- =============
-- FromCBOR pair

instance (Named name (a -> b),Typeable (a->b),FromCBOR name) => FromCBOR (Closure name '[] (a -> b)) where
  fromCBOR = decode(Summands "Close" decClose)
    where decClose 0 = SumD Close <! From
          decClose n = Invalid n

instance
    ( Typeable env,
      Typeable name,
      Typeable x,
      FromCBOR a,
      FromCBOR (Closure name env (a -> x))
    ) => FromCBOR (Closure name (a ': env) x) where
  fromCBOR = decode(Summands ":$" decDollar)
    where decDollar 1 = SumD (:$) <! From <! From
          decDollar n = Invalid n

-- =============================================

-- | useful for demonstrating (pulse x) == (pulse (roundtrip x))
roundtrip :: (ToCBOR t, FromCBOR t) => t -> t
roundtrip t =
  case decodeFull' (serialize' t) of
    Right x -> x
    Left err -> error (show err)
