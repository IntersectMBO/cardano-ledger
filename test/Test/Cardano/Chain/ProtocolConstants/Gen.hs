module Test.Cardano.Chain.ProtocolConstants.Gen
       ( genVssMaxTTL
       , genVssMinTTL
       ) where

import           Cardano.Prelude
import           Test.Cardano.Prelude

import           Hedgehog (Gen)

import           Cardano.Chain.ProtocolConstants (VssMaxTTL (..),
                     VssMinTTL (..))

genVssMaxTTL :: Gen VssMaxTTL
genVssMaxTTL = VssMaxTTL <$> genWord32

genVssMinTTL :: Gen VssMinTTL
genVssMinTTL = VssMinTTL <$> genWord32
