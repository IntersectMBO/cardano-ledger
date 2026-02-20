{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Cardano.Ledger.Chain (
  -- | Chain Checks
  ChainChecksPParams (..),
  ChainPredicateFailure (..),
  pparamsToChainChecksPParams,
  chainChecks,
) where

import Cardano.Ledger.BaseTypes (ProtVer (..), Version)
import Cardano.Ledger.Block (Block, EraBlockHeader (..))
import Cardano.Ledger.Core
import Control.Monad (unless)
import Control.Monad.Except (MonadError, throwError)
import Data.Word (Word16, Word32)
import GHC.Generics (Generic)
import Lens.Micro ((^.))
import NoThunks.Class (NoThunks (..))

data ChainChecksPParams = ChainChecksPParams
  { ccMaxBHSize :: Word16
  , ccMaxBBSize :: Word32
  , ccProtocolVersion :: ProtVer
  }
  deriving (Show, Eq, Generic, NoThunks)

pparamsToChainChecksPParams ::
  EraPParams era =>
  PParams era ->
  ChainChecksPParams
pparamsToChainChecksPParams pp =
  ChainChecksPParams
    { ccMaxBHSize = pp ^. ppMaxBHSizeL
    , ccMaxBBSize = pp ^. ppMaxBBSizeL
    , ccProtocolVersion = pp ^. ppProtocolVersionL
    }

data ChainPredicateFailure
  = HeaderSizeTooLargeCHAIN
      Int -- Header Size
      Word16 -- Max Header Size
  | BlockSizeTooLargeCHAIN
      Word32 -- Block Size
      Word32 -- Max Block Size
  | ObsoleteNodeCHAIN
      Version -- protocol version used
      Version -- max protocol version
  deriving (Generic, Show, Eq, Ord)

instance NoThunks ChainPredicateFailure

chainChecks ::
  (MonadError ChainPredicateFailure m, EraBlockHeader h era) =>
  Version ->
  ChainChecksPParams ->
  Block h era ->
  m ()
chainChecks maxpv ccd blk = do
  unless (m <= maxpv) $ throwError (ObsoleteNodeCHAIN m maxpv)
  let bhHSize = blk ^. blockHeaderHSizeL
      bhBSize = blk ^. blockHeaderBSizeL
  unless (bhHSize <= (fromIntegral :: Word16 -> Int) (ccMaxBHSize ccd)) $
    throwError $
      HeaderSizeTooLargeCHAIN bhHSize (ccMaxBHSize ccd)
  unless (bhBSize <= ccMaxBBSize ccd) $
    throwError $
      BlockSizeTooLargeCHAIN bhBSize (ccMaxBBSize ccd)
  where
    ProtVer m _ = ccProtocolVersion ccd
