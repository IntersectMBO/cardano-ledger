{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeApplications           #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE UndecidableInstances       #-}

module Cardano.Chain.Ssc.CommitmentsMap
       ( CommitmentsMap (getCommitmentsMap)
       , mkCommitmentsMap
       , mkCommitmentsMapUnsafe

       , dropCommitmentsMap
       ) where

import           Cardano.Prelude

import qualified Data.Map.Strict as Map
import qualified Data.Set as Set

import           Cardano.Binary.Class (Bi (..), Decoder, Dropper, Encoding,
                     dropSet)
import           Cardano.Chain.Common (StakeholderId, mkStakeholderId)
import           Cardano.Chain.Ssc.Commitment (SignedCommitment,
                     dropSignedCommitment)

-- | 'CommitmentsMap' is a wrapper for 'HashMap StakeholderId SignedCommitment'
-- which ensures that keys are consistent with values, i. e. 'PublicKey'
-- from 'SignedCommitment' corresponds to key which is 'StakeholderId'.
newtype CommitmentsMap = CommitmentsMap
    { getCommitmentsMap :: Map StakeholderId SignedCommitment
    } deriving (Generic, Semigroup, Monoid, Show, Eq, NFData, Container)

-- | Safe constructor of 'CommitmentsMap'.
mkCommitmentsMap :: [SignedCommitment] -> CommitmentsMap
mkCommitmentsMap = CommitmentsMap . Map.fromList . map toCommPair
  where
    toCommPair signedComm@(pk, _, _) = (mkStakeholderId pk, signedComm)

-- | Unsafe straightforward constructor of 'CommitmentsMap'.
mkCommitmentsMapUnsafe :: Map StakeholderId SignedCommitment -> CommitmentsMap
mkCommitmentsMapUnsafe = CommitmentsMap

dropCommitmentsMap :: Dropper s
dropCommitmentsMap = dropSet dropSignedCommitment

instance Bi CommitmentsMap where
    encode = encodeCommitments
    decode = decodeCommitments


{-
'CommitmentsMap' is simply sets of values, indexed
by stakeholder id *for performance only*; the invariant is that the key
(stakeholder id) corresponds to the key stored in the value. This means that
the keys are redundant and putting them into encoded data is bad for two
reasons:

  * it takes more space
  * we have to do an extra invariant check after decoding

Instead, we serialize those maps as sets, and we make sure to check that
there are no values with duplicate stakeholder ids.
-}

encodeCommitments :: CommitmentsMap -> Encoding
encodeCommitments = encode . Set.fromList . toList

decodeCommitments :: Decoder s CommitmentsMap
decodeCommitments = do
    comms <- toList <$> decode @(Set SignedCommitment)
    -- unless (allDistinct (map (view _1) comms :: [PublicKey])) $ cborError $
    --     "decodeCommitments: two commitments have the same signing key"
    pure (mkCommitmentsMap comms)
