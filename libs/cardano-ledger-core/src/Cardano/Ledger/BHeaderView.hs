{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Cardano.Ledger.BHeaderView {-# DEPRECATED "Use the {Era|ShelleyEra|DijkstraEra|...}BlockHeader typeclasses instead." #-} where

import Cardano.Ledger.BaseTypes (Nonce, ProtVer)
import Cardano.Ledger.Hashes (EraIndependentBlockBody, HASH, Hash, KeyHash, KeyRole (..))
import Cardano.Ledger.Slot (SlotNo (..))
import Control.DeepSeq (NFData)
import Data.Word (Word32)
import GHC.Generics (Generic)

-- | 'BHeaderView' provides an interface between block headers
-- from different Cardano protocols and packages that should be
-- agnostic of Cardano protocol specific details,
-- such as those in TPraos, Praos, Genesis, etc.
--
-- In particular, the 'BBODY' rule comprises most of the ledger logic
-- and should work independently of the protocol. The values in
-- 'BHeaderView' provide 'BBODY' all the data that it needs from the
-- block headers.
data BHeaderView = BHeaderView
  { bhviewID :: KeyHash BlockIssuer
  -- ^ The block issuer. In the TPraos protocol, this can be a
  --  Genesis delegate, everywhere else it is the stake pool ID.
  , bhviewBSize :: Word32
  -- ^ The purported size (in bytes) of the block body.
  , bhviewHSize :: Int
  -- ^ The purported size (in bytes) of the block header.
  , bhviewBHash :: Hash HASH EraIndependentBlockBody
  -- ^ The purported hash of the block body.
  , bhviewSlot :: SlotNo
  -- ^ The slot for which this block was submitted to the chain.
  , bhviewPrevEpochNonce :: Maybe Nonce
  -- ^ The previous epoch nonce, needed to validate Peras certificates
  -- contained in blocks.
  , bhviewProtVer :: ProtVer
  }
  deriving (Generic)

instance NFData BHeaderView
