module Cardano.Chain.Ssc.SharesMap
       ( SharesMap
       , InnerSharesMap
       , dropInnerSharesMap
       , dropSharesMap
       ) where

import           Cardano.Prelude

import           Cardano.Binary.Class (AsBinary, Dropper, dropBytes, dropList,
                     dropMap)
import           Cardano.Chain.Common (StakeholderId)
import           Cardano.Crypto (DecShare)

-- | Each node generates several 'SharedSeed's, breaks every
-- 'SharedSeed' into 'Share's, and sends those encrypted shares to
-- other nodes (for i-th commitment at i-th element of NonEmpty
-- list). Then those shares are decrypted.
type InnerSharesMap = Map StakeholderId (NonEmpty (AsBinary DecShare))

dropInnerSharesMap :: Dropper s
dropInnerSharesMap = dropMap dropBytes (dropList dropBytes)

-- | In a 'SharesMap', for each node we collect shares which said node
-- has received and decrypted:
--
--   * Outer key = who decrypted the share
--   * Inner key = who created the share
--
-- Let's say that there are participants {A, B, C}. If A has generated a
-- secret and shared it, A's shares will be denoted as Aa, Ab and Ac (sent
-- correspondingly to A itself, B and C). Then node B will decrypt its share
-- and get Ab_dec; same for other nodes and participants. In the end, after
-- the second phase of the protocol completes and we gather everyone's
-- shares, we'll get the following map:
--
-- @
-- { A: {A: Aa_dec, B: Ba_dec, C: Ca_dec}
-- , B: {A: Ab_dec, B: Bb_dec, C: Cb_dec}
-- , C: {A: Ac_dec, B: Bc_dec, C: Cc_dec}
-- }
-- @
--
-- (Here there's only one share per node, but in reality there'll be more.)
type SharesMap = Map StakeholderId InnerSharesMap

dropSharesMap :: Dropper s
dropSharesMap = dropMap dropBytes dropInnerSharesMap
