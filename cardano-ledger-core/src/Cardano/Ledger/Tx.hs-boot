{-# LANGUAGE RoleAnnotations #-}

-- | To break the cycle between Tx and Core, we introduce a forward-definition
-- of Tx in this boot module.
module Cardano.Ledger.Tx where

type role Tx nominal

data Tx era
