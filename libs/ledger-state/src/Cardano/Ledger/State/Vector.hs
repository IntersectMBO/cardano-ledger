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
import Cardano.Ledger.Shelley.TxBody
import Cardano.Ledger.State.UTxO
import Control.DeepSeq
import Data.Map.Strict as Map

data SnapShotM crypto = SnapShotM
  { ssStake :: !(Map (Credential 'Staking crypto) (CompactForm Coin)),
    ssDelegations :: !(Map (Credential 'Staking crypto) (KeyHash 'StakePool crypto)),
    ssPoolParams :: !(Map (KeyHash 'StakePool crypto) (PoolParams crypto))
  }

instance NFData (SnapShotM C) where
  rnf (SnapShotM s d p) = s `deepseq` d `deepseq` rnf p

data SnapShotsM crypto = SnapShotsM
  { ssPstakeMark :: !(SnapShotM crypto),
    ssPstakeSet :: !(SnapShotM crypto),
    ssPstakeGo :: !(SnapShotM crypto),
    ssFeeSS :: !Coin
  }

instance NFData (SnapShotsM C) where
  rnf (SnapShotsM r s g f) = r `deepseq` s `deepseq` g `deepseq` rnf f
