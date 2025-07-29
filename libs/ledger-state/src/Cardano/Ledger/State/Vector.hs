{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}

module Cardano.Ledger.State.Vector where

import Cardano.Ledger.Coin
import Cardano.Ledger.Credential
import Cardano.Ledger.Keys as Keys
import Cardano.Ledger.State (PoolParams)
import Control.DeepSeq
import Data.Map.Strict as Map

data SnapShotM = SnapShotM
  { ssStake :: !(Map (Credential 'Staking) (CompactForm Coin))
  , ssDelegations :: !(Map (Credential 'Staking) (KeyHash 'StakePool))
  , ssPoolParams :: !(Map (KeyHash 'StakePool) PoolParams)
  }

instance NFData SnapShotM where
  rnf (SnapShotM s d p) = s `deepseq` d `deepseq` rnf p

data SnapShotsM = SnapShotsM
  { ssPstakeMark :: !SnapShotM
  , ssPstakeSet :: !SnapShotM
  , ssPstakeGo :: !SnapShotM
  , ssFeeSS :: !Coin
  }

instance NFData SnapShotsM where
  rnf (SnapShotsM r s g f) = r `deepseq` s `deepseq` g `deepseq` rnf f
