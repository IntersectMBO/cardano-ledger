module Cardano.Chain.Ssc.OpeningsMap
       ( OpeningsMap
       , dropOpeningsMap
       ) where

import           Cardano.Prelude

import           Cardano.Binary.Class (Dropper, dropBytes, dropMap)
import           Cardano.Chain.Common (StakeholderId)
import           Cardano.Chain.Ssc.Opening (Opening)


type OpeningsMap = Map StakeholderId Opening

dropOpeningsMap :: Dropper s
dropOpeningsMap = dropMap dropBytes dropBytes
