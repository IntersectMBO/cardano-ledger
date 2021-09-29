{-# LANGUAGE DataKinds #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Cardano.Ledger.BHeaderView where

import Cardano.Ledger.BaseTypes (BoundedRational (..), UnitInterval)
import Cardano.Ledger.Hashes (EraIndependentBlockBody)
import Cardano.Ledger.Keys (Hash, KeyHash, KeyRole (..))
import Cardano.Ledger.Slot (SlotNo (..), (-*))
import Numeric.Natural (Natural)

-- | 'BHeaderView' provides an interface between block headers
-- from different Cardano protocols and packages that should be
-- agnostic of Cardano protocol specific details,
-- such as those in TPraos, Praos, Genesis, etc.
--
-- In particular, the 'BBODY' rule comprises most of the ledger logic
-- and should work independently of the protocol. The values in
-- 'BHeaderView' provide 'BBODY' all the data that it needs from the
-- block headers.
data BHeaderView crypto = BHeaderView
  { -- | The block issuer. In the TPraos protocol, this can be a
    --  Genesis delegate, everywhere else it is the stake pool ID.
    bhviewID :: KeyHash 'BlockIssuer crypto,
    -- | The purported size (in bytes) of the block body.
    bhviewBSize :: Natural,
    -- | The purported size (in bytes) of the block header.
    bhviewHSize :: Int,
    -- | The purported hash of the block body.
    bhviewBHash :: Hash crypto EraIndependentBlockBody,
    -- | The slot for which this block was submitted to the chain.
    bhviewSlot :: SlotNo
  }

-- | Determine if the given slot is reserved for the overlay schedule.
isOverlaySlot ::
  -- | The first slot of the given epoch.
  SlotNo ->
  -- | The decentralization parameter.
  UnitInterval ->
  -- | The slot to check.
  SlotNo ->
  Bool
isOverlaySlot firstSlotNo dval slot = step s < step (s + 1)
  where
    s = fromIntegral $ slot -* firstSlotNo
    d = unboundRational dval
    step :: Rational -> Integer
    step x = ceiling (x * d)
