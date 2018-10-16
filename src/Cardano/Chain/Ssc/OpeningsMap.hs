module Cardano.Chain.Ssc.OpeningsMap
       ( OpeningsMap
       ) where

import           Cardano.Prelude

import           Cardano.Chain.Common (StakeholderId)

import           Cardano.Chain.Ssc.Opening

type OpeningsMap = Map StakeholderId Opening
