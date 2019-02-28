{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies     #-}

-- | Type classes for interfacing with the consensus layer
module Cardano.Spec.Consensus.Block where

import           Control.Lens ((^.))

import qualified Cardano.Spec.Chain.STS.Block as CBM -- Concrete Block Module
import           Ledger.Core
import           Ledger.Delegation
import           Ledger.Signatures


class BlockHeader h where
  -- | Hash of the previous block header, or 'genesisHash' in case of
  -- the first block in a chain.
  bhPrevHash :: h -> Hash -- This is needed in the PBFT rule
  -- | Signature of the block by its issuer.
  bhSig :: h -> Sig VKey
  -- | Block issuer.
  bhIssuer :: h -> VKey
  -- | Slot for which this block is issued
  bhSlot :: h -> Slot


class BlockBody bb where
  -- | Delegation certificates.
  bbCerts :: bb -> [DCert]


class ( BlockHeader (FamBlockHeader b)
      , BlockBody (FamBlockBody b)
      ) => Block b where
  type family FamBlockHeader b :: *
  type family FamBlockBody   b :: *

  -- | Gets the block header
  bHeader :: b -> FamBlockHeader b
  -- | Gets the block body
  bBody   :: b -> FamBlockBody b


instance BlockBody CBM.BlockBody where
  bbCerts (CBM.BlockBody bDCerts) = bDCerts

instance BlockHeader CBM.BlockHeader where
  bhPrevHash h = h ^. CBM.bhPrevHash
  bhSig      h = h ^. CBM.bhSig
  bhIssuer   h = h ^. CBM.bhIssuer
  bhSlot     h = h ^. CBM.bhSlot

instance Block CBM.Block where
  type FamBlockHeader CBM.Block = CBM.BlockHeader
  type FamBlockBody   CBM.Block = CBM.BlockBody

  bHeader b = b ^. CBM.bHeader
  bBody   b = b ^. CBM.bBody
