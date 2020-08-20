{-# LANGUAGE TypeFamilies #-}

-- | This module declares the types in the base module which are open to
-- overriding in future modules.
module Control.State.Transition.Examples.Modular.Base.Types where

type family Coin era
type family AccountName era
type family Account era
type family Tx era
type family Admin era
type family Op era
type family Accounting era
type family BlockId era
type family Block era
type family ChainState era
