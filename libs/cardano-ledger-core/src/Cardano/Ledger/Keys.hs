{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# OPTIONS_GHC -Wno-deprecations #-}

module Cardano.Ledger.Keys (
  -- * VKey
  VKey (..),

  -- * Key Roles
  KeyRole (..),
  HasKeyRole (..),
  asWitness,

  -- * Key Hashes
  KeyHash (..),
  hashKey,

  -- * Signature
  DSIGN,
  DSignable,
  signedDSIGN,
  verifySignedDSIGN,

  -- * VRF Key Hashes
  KeyRoleVRF (..),
  VRFVerKeyHash (..),
  toVRFVerKeyHash,
  fromVRFVerKeyHash,

  -- * Genesis delegations
  GenDelegPair (..),
  GenDelegs (..),
  module Cardano.Ledger.Keys.WitVKey,
  module Cardano.Ledger.Keys.Bootstrap,

  -- * To be removed

  -- ** Re-exports from cardano-crypto-class
  decodeSignedDSIGN,
  encodeSignedDSIGN,
  Hash.hashWithSerialiser,
) where

import qualified Cardano.Crypto.Hash as Hash
import Cardano.Ledger.Hashes (
  GenDelegPair (..),
  GenDelegs (..),
  KeyHash (..),
  KeyRoleVRF (..),
  VRFVerKeyHash (..),
  fromVRFVerKeyHash,
  hashKey,
  toVRFVerKeyHash,
 )
import Cardano.Ledger.Keys.Bootstrap
import Cardano.Ledger.Keys.Internal
import Cardano.Ledger.Keys.WitVKey
