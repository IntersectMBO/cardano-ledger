{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies     #-}

-- | Type classes for interfacing with the consensus layer
module Cardano.Spec.Consensus.Block where

import           Control.Lens ((^.))

import qualified Cardano.Spec.Chain.STS.Block as CBM -- Concrete Block Module
import           Ledger.Core
import           Ledger.Delegation
import           Ledger.Update (ProtVer, UProp, Vote)
import           Ledger.UTxO (TxWits, TxId)


class BlockHeader h where
  -- | Hash of the previous block header, or 'genesisHash' in case of
  -- the first block in a chain.
  bhPrevHash :: h -> Hash -- This is needed in the PBFT rule
  -- | Signature of the block by its issuer.
  bhSig :: h -> Sig Hash
  -- | Block issuer.
  bhIssuer :: h -> VKey
  -- | Slot for which this block is issued
  bhSlot :: h -> Slot
  -- | UTxO hash
  bhUtxoHash :: h -> Hash
  -- | Delegation hash
  bhDlgHash :: h -> Hash
  -- | Update payload hash
  bhUpdHash :: h -> Hash


class BlockBody bb where
  -- | Delegation certificates.
  bbCerts :: bb -> [DCert]
  -- | UTxO payload
  bbUtxo :: bb -> [TxWits TxId]
  -- | Update proposal payload
  bbUpdProp :: bb -> Maybe UProp
  -- | Update votes payload
  bbUpdVotes :: bb -> [Vote]
  -- | Protocol version
  bbProtVer :: bb -> ProtVer


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
  bbCerts      b = b ^. CBM.bDCerts
  bbUtxo       b = b ^. CBM.bUtxo
  bbUpdProp    b = b ^. CBM.bUpdProp
  bbUpdVotes   b = b ^. CBM.bUpdVotes
  bbProtVer    b = b ^. CBM.bProtVer


instance BlockHeader CBM.BlockHeader where
  bhPrevHash h = h ^. CBM.bhPrevHash
  bhSig      h = h ^. CBM.bhSig
  bhIssuer   h = h ^. CBM.bhIssuer
  bhSlot     h = h ^. CBM.bhSlot
  bhUtxoHash h = h ^. CBM.bhUtxoHash
  bhDlgHash  h = h ^. CBM.bhDlgHash
  bhUpdHash  h = h ^. CBM.bhUpdHash

instance Block CBM.Block where
  type FamBlockHeader CBM.Block = CBM.BlockHeader
  type FamBlockBody   CBM.Block = CBM.BlockBody

  bHeader b = b ^. CBM.bHeader
  bBody   b = b ^. CBM.bBody

-- | Block update payload
bUpdPayload :: Block b => b -> (Maybe UProp, [Vote])
bUpdPayload block = (bbUpdProp (bBody block), bbUpdVotes (bBody block))


-- | Turns a generic block to a concrete 'CBM.Block'
concretiseBlock :: Block b => b -> CBM.Block
concretiseBlock block = CBM.Block
  { CBM._bHeader = CBM.MkBlockHeader
      { CBM._bhPrevHash = bhPrevHash (bHeader block)
      , CBM._bhSlot     = bhSlot     (bHeader block)
      , CBM._bhIssuer   = bhIssuer   (bHeader block)
      , CBM._bhSig      = bhSig      (bHeader block)
      , CBM._bhUtxoHash = bhUtxoHash (bHeader block)
      , CBM._bhDlgHash  = bhDlgHash  (bHeader block)
      , CBM._bhUpdHash  = bhUpdHash  (bHeader block)
      }
  , CBM._bBody = CBM.BlockBody
      { CBM._bDCerts    = bbCerts      (bBody block)
      , CBM._bUtxo      = bbUtxo       (bBody block)
      , CBM._bUpdProp   = bbUpdProp    (bBody block)
      , CBM._bUpdVotes  = bbUpdVotes   (bBody block)
      , CBM._bProtVer   = bbProtVer    (bBody block)
      }
  }
