{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PolyKinds         #-}
{-# LANGUAGE TypeFamilies      #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}

-- | Orphan instances for external types/classes.

module Cardano.Prelude.Orphans
       (
       ) where

import           Cardano.Prelude.Base

import           Data.Tagged (Tagged (Tagged))
import           Data.Typeable (typeRep)
import qualified Formatting as F
import           Formatting.Buildable (Buildable (..))


--------------------------------------------------------------------------------
-- Buildable
--------------------------------------------------------------------------------

-- | This orphan instance is sometimes useful and why not have it?
instance Buildable () where
  build _ = "()"

instance (Typeable s, Buildable a) => Buildable (Tagged s a) where
  build tt@(Tagged v) =
    F.bprint ("Tagged " F.% F.shown F.% " " F.% F.build) ts v
   where
    ts    = typeRep proxy
    proxy = (const Proxy :: Tagged s a -> Proxy s) tt
