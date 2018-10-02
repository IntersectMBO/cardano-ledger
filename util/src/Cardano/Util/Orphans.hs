{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PolyKinds         #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE TypeApplications  #-}
{-# LANGUAGE TypeFamilies      #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}

-- | Orphan instances for external types/classes.

module Cardano.Util.Orphans
       (
       ) where

import           Cardano.Prelude

import qualified Data.Aeson.Options as S (defaultOptions)
import           Data.Aeson.TH (deriveJSON)
import           Data.Tagged (Tagged (Tagged))
import           Data.Time.Units (Attosecond, Day, Femtosecond, Fortnight, Hour,
                     Microsecond, Millisecond, Minute, Nanosecond, Picosecond,
                     Second, Week, toMicroseconds)
import           Data.Typeable (typeRep)
import qualified Formatting as F
import           Formatting.Buildable (Buildable (..))


----------------------------------------------------------------------------
-- Aeson
----------------------------------------------------------------------------

deriveJSON S.defaultOptions ''Millisecond
deriveJSON S.defaultOptions ''Microsecond
deriveJSON S.defaultOptions ''Second

----------------------------------------------------------------------------
-- NFData
----------------------------------------------------------------------------

instance NFData Millisecond where
    rnf ms = rnf (toInteger ms)

instance NFData Microsecond where
    rnf ms = rnf (toInteger ms)

----------------------------------------------------------------------------
-- Buildable
----------------------------------------------------------------------------

instance Buildable Attosecond  where build = build @String . show
instance Buildable Femtosecond where build = build @String . show
instance Buildable Picosecond  where build = build @String . show
instance Buildable Nanosecond  where build = build @String . show
instance Buildable Millisecond where build = build @String . show
instance Buildable Second      where build = build @String . show
instance Buildable Minute      where build = build @String . show
instance Buildable Hour        where build = build @String . show
instance Buildable Day         where build = build @String . show
instance Buildable Week        where build = build @String . show
instance Buildable Fortnight   where build = build @String . show

-- | Special case. We don't want to print greek letter mu in console because
-- it breaks things sometimes.
instance Buildable Microsecond where
    build = build . (++ "mcs") . show . toMicroseconds

-- | This orphan instance is sometimes useful and why not have it?
instance Buildable () where
    build _ = "()"

instance (Typeable s, Buildable a) => Buildable (Tagged s a) where
    build tt@(Tagged v) = F.bprint ("Tagged " F.% F.shown F.% " " F.% F.build) ts v
      where
        ts = typeRep proxy
        proxy = (const Proxy :: Tagged s a -> Proxy s) tt
