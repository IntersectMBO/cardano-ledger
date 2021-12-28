{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-redundant-constraints #-}

-- | The stake distribution, aggregated by stake pool (as opposed to stake credential),
-- plays a primary role in Cardano's proof of stake network.
-- Together with the VRF checks, the stake distribution determines leader election.
-- The leader election is the precisely the part of the ledger that is
-- determined by Ouroboros (Praos and Genesis), our consensus mechanism.
-- See Section 16, "Leader Value Calculation", of the
-- <https://hydra.iohk.io/job/Cardano/cardano-ledger-specs/shelleyLedgerSpec/latest/download-by-type/doc-pdf/ledger-spec formal specification>.
module Cardano.Ledger.PoolDistr where

import Cardano.Binary
  ( FromCBOR (fromCBOR),
    ToCBOR (..),
    encodeListLen,
  )
import qualified Cardano.Ledger.Crypto as CC
import Cardano.Ledger.Keys (Hash, KeyHash, KeyRole (..), VerKeyVRF)
import Cardano.Ledger.Serialization (decodeRecordNamed)
import Control.DeepSeq (NFData)
import Data.Map.Strict (Map)
import Data.Relation (Relation (..))
import GHC.Generics (Generic)
import NoThunks.Class (NoThunks (..))

-- | The 'IndividualPoolStake' contains all the stake controlled
-- by a single stake pool (the combination of owners and delegates)
-- for a given epoch, and also the hash of the stake pool's
-- registered VRF key.
--
-- When a stake pool produces a block, the header contains the
-- full VRF verification key and VRF value for leader election.
-- We check the VRF key against the value in 'IndividualPoolStake'
-- and we check the VRF value using the epoch nonce and
-- the relative stake of the pool as given in 'IndividualPoolStake'.
-- The stake is relative to the total amount of active stake
-- in the network. Stake is active if it is both registered and
-- delegated to a registered stake pool.
data IndividualPoolStake crypto = IndividualPoolStake
  { individualPoolStake :: !Rational,
    individualPoolStakeVrf :: !(Hash crypto (VerKeyVRF crypto))
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (NFData, NoThunks)

instance CC.Crypto crypto => ToCBOR (IndividualPoolStake crypto) where
  toCBOR (IndividualPoolStake stake vrf) =
    mconcat
      [ encodeListLen 2,
        toCBOR stake,
        toCBOR vrf
      ]

instance CC.Crypto crypto => FromCBOR (IndividualPoolStake crypto) where
  fromCBOR =
    decodeRecordNamed "IndividualPoolStake" (const 2) $
      IndividualPoolStake
        <$> fromCBOR
        <*> fromCBOR

-- | A map of stake pool IDs (the hash of the stake pool operator's
-- verification key) to 'IndividualPoolStake'.
newtype PoolDistr crypto = PoolDistr
  { unPoolDistr ::
      Map (KeyHash 'StakePool crypto) (IndividualPoolStake crypto)
  }
  deriving stock (Show, Eq)
  deriving newtype (ToCBOR, FromCBOR, NFData, NoThunks, Relation)
