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
import Cardano.Ledger.State.UTxO
import Control.DeepSeq
import Data.Map.Strict as Map

data SnapShotM c = SnapShotM
  { ssStake :: !(Map (Credential 'Staking c) (CompactForm Coin))
  , ssDelegations :: !(Map (Credential 'Staking c) (KeyHash 'StakePool c))
  , ssPoolParams :: !(Map (KeyHash 'StakePool c) (PoolParams c))
  }

instance NFData (SnapShotM C) where
  rnf (SnapShotM s d p) = s `deepseq` d `deepseq` rnf p

data SnapShotsM c = SnapShotsM
  { ssPstakeMark :: !(SnapShotM c)
  , ssPstakeSet :: !(SnapShotM c)
  , ssPstakeGo :: !(SnapShotM c)
  , ssFeeSS :: !Coin
  }

instance NFData (SnapShotsM C) where
  rnf (SnapShotsM r s g f) = r `deepseq` s `deepseq` g `deepseq` rnf f
