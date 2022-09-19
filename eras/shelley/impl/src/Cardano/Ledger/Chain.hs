{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Cardano.Ledger.Chain (
  -- | Chain Checks
  ChainChecksPParams (..),
  ChainPredicateFailure (..),
  pparamsToChainChecksPParams,
  chainChecks,
)
where

import Cardano.Ledger.BHeaderView (BHeaderView (..))
import Cardano.Ledger.BaseTypes (ProtVer (..), Version)
import Cardano.Ledger.Core
import Control.Monad (unless)
import Control.Monad.Except (MonadError, throwError)
import GHC.Generics (Generic)
import Lens.Micro ((^.))
import NoThunks.Class (NoThunks (..))
import Numeric.Natural (Natural)

data ChainChecksPParams = ChainChecksPParams
  { ccMaxBHSize :: Natural
  , ccMaxBBSize :: Natural
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
      !Natural -- Header Size
      !Natural -- Max Header Size
  | BlockSizeTooLargeCHAIN
      !Natural -- Block Size
      !Natural -- Max Block Size
  | ObsoleteNodeCHAIN
      !Version -- protocol version used
      !Version -- max protocol version
  deriving (Generic, Show, Eq, Ord)

instance NoThunks ChainPredicateFailure

chainChecks ::
  MonadError ChainPredicateFailure m =>
  Version ->
  ChainChecksPParams ->
  BHeaderView c ->
  m ()
chainChecks maxpv ccd bhv = do
  unless (m <= maxpv) $ throwError (ObsoleteNodeCHAIN m maxpv)
  unless (fromIntegral (bhviewHSize bhv) <= ccMaxBHSize ccd) $
    throwError $
      HeaderSizeTooLargeCHAIN (fromIntegral $ bhviewHSize bhv) (ccMaxBHSize ccd)
  unless (bhviewBSize bhv <= ccMaxBBSize ccd) $
    throwError $
      BlockSizeTooLargeCHAIN (bhviewBSize bhv) (ccMaxBBSize ccd)
  where
    ProtVer m _ = ccProtocolVersion ccd
