{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

module Cardano.Ledger.State.Vector where

import Cardano.Ledger.Coin
import Cardano.Ledger.Credential
import Cardano.Ledger.Keys as Keys
import Cardano.Ledger.PoolParams
import Control.DeepSeq
import Data.Map.Strict as Map

data SnapShotM = SnapShotM
  { ssStake :: !(Map (Credential 'Staking) (CompactForm Coin))
  , ssDelegations :: !(Map (Credential 'Staking) (KeyHash 'StakePool))
  , ssStakePoolParams :: !(Map (KeyHash 'StakePool) StakePoolParams)
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
