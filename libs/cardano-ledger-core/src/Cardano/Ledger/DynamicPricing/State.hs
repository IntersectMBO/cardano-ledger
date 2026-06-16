{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilyDependencies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE UndecidableSuperClasses #-}

-- | The era-indexed pricing state, following the codebase's established
-- pattern for era-varying state (@EraGov\/GovState@, @EraCertState\/CertState@,
-- @EraStake\/InstantStake@): a class bundling all the instances the state
-- must provide, with an injective associated type family. Pre-dynamic-pricing
-- eras instantiate the inert 'NoPricing'; the era that activates the
-- mechanism instantiates 'DynamicPricing'.
module Cardano.Ledger.DynamicPricing.State (
  -- * The era family
  EraPricing (..),

  -- * Pre-dynamic-pricing eras
  NoPricing (..),

  -- * The live pricing state
  DynamicPricing (..),
  initialPricingState,
  recordTx,
  currentPrice,
  addPendingRefund,
  drainPendingRefunds,

  -- * Per-block usage accounting
  InclusionUsage (..),
  emptyInclusionUsage,

  -- * End-of-block repricing (DIVUP)
  reprice,
  endOfBlock,
) where

import Cardano.Ledger.Binary (DecCBOR (..), EncCBOR (..))
import Cardano.Ledger.Binary.Coders (
  Decode (..),
  Encode (..),
  decode,
  encode,
  (!>),
  (<!),
 )
import Cardano.Ledger.Coin (Coin (..))
import Cardano.Ledger.Credential (Credential)
import Cardano.Ledger.DynamicPricing.InclusionStrategy (Inclusion (..))
import Cardano.Ledger.DynamicPricing.Pricing (
  InclusionPrice (..),
  InclusionPrices,
  TxSizeInBytes (..),
  mkInclusionPrices,
  optimistic,
  priceOf,
  urgent,
 )
import Cardano.Ledger.Plutus.ExUnits (ExUnits (..))
import Control.DeepSeq (NFData)
import Data.Aeson (ToJSON (..), object, (.=))
import Data.Default (Default (..))
import Data.Kind (Type)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Typeable (Typeable)
import Data.Word (Word8)
import Cardano.Ledger.Keys (KeyRole (Staking))
import GHC.Generics (Generic)
import NoThunks.Class (NoThunks)

-- | Every era carries a pricing state; its shape is the era's choice.
class
  ( Eq (PricingState era)
  , Show (PricingState era)
  , NFData (PricingState era)
  , NoThunks (PricingState era)
  , EncCBOR (PricingState era)
  , DecCBOR (PricingState era)
  , Default (PricingState era)
  , ToJSON (PricingState era)
  ) =>
  EraPricing era
  where
  type PricingState era = (r :: Type) | r -> era

  -- | The pricing state an era starts with.
  emptyPricing :: PricingState era
  emptyPricing = def

-- | The inert pricing state of the eras that predate dynamic pricing.
data NoPricing era = NoPricing
  deriving (Eq, Ord, Show, Generic)

instance NoThunks (NoPricing era)

instance NFData (NoPricing era)

instance Default (NoPricing era) where
  def = NoPricing

instance ToJSON (NoPricing era) where
  toJSON NoPricing = toJSON ()

instance EncCBOR (NoPricing era) where
  encCBOR NoPricing = encCBOR (0 :: Word8)

instance Typeable era => DecCBOR (NoPricing era) where
  decCBOR =
    decCBOR >>= \case
      (0 :: Word8) -> pure NoPricing
      n -> fail $ "Invalid NoPricing encoding: " <> show n

-- | The live pricing state (spec: @SDPolicy@): the published prices plus the
-- usage counters of the block being processed.
data DynamicPricing era = DynamicPricing
  { publishedPrices :: !InclusionPrices
  -- ^ The price of each inclusion strategy (deck: DP1\/DP2).
  , blockUsage :: !(Map Inclusion InclusionUsage)
  -- ^ Usage of the block currently being processed (spec: the
  -- @totalSize\/totalFees\/totalExUnits@ maps).
  , pendingRefunds :: !(Map (Credential Staking) Coin)
  -- ^ Refunds owed to bidders (the unused headroom of their bids), flushed
  -- into account balances by the LEDGER rule after every transaction
  -- (spec: @feeRewards@).
  }
  deriving (Eq, Show, Generic)

instance NoThunks (DynamicPricing era)

instance NFData (DynamicPricing era)

instance Default (DynamicPricing era) where
  def = initialPricingState

instance ToJSON (DynamicPricing era) where
  toJSON dp =
    object
      [ "urgent" .= unInclusionPrice (urgent (publishedPrices dp))
      , "optimistic" .= unInclusionPrice (optimistic (publishedPrices dp))
      , "blockUsage" .= Map.toList (Map.mapKeys show (blockUsage dp))
      , "pendingRefunds" .= pendingRefunds dp
      ]

instance EncCBOR (DynamicPricing era) where
  encCBOR (DynamicPricing prices usage refunds) =
    encode $
      Rec mkDynamicPricing
        !> To (urgent prices)
        !> To (optimistic prices)
        !> To usage
        !> To refunds

instance Typeable era => DecCBOR (DynamicPricing era) where
  decCBOR = decode $ RecD mkDynamicPricing <! From <! From <! From <! From

mkDynamicPricing ::
  InclusionPrice ->
  InclusionPrice ->
  Map Inclusion InclusionUsage ->
  Map (Credential Staking) Coin ->
  DynamicPricing era
mkDynamicPricing u o = DynamicPricing (unsafePrices u o)

-- Decoding trusts the encoder: the floor invariant held when the state was
-- produced. TODO(prototype): fail the decoder instead of clamping.
unsafePrices :: InclusionPrice -> InclusionPrice -> InclusionPrices
unsafePrices u o =
  case mkInclusionPrices u o of
    Just prices -> prices
    Nothing -> error "DynamicPricing: decoded prices violate the price-discrimination floor"

-- | Resources consumed within the current block by the transactions of one
-- inclusion strategy. Reset at every block boundary by 'endOfBlock'.
data InclusionUsage = InclusionUsage
  { bytesUsed :: !TxSizeInBytes
  , feesCollected :: !Coin
  , exUnitsUsed :: !ExUnits
  }
  deriving (Eq, Show, Generic)

instance NoThunks InclusionUsage

instance NFData InclusionUsage

instance ToJSON InclusionUsage where
  toJSON (InclusionUsage b f e) =
    object ["bytes" .= unTxSizeInBytes b, "fees" .= f, "exUnits" .= e]

instance EncCBOR InclusionUsage where
  encCBOR (InclusionUsage b f e) =
    encode $ Rec InclusionUsage !> To b !> To f !> To e

instance DecCBOR InclusionUsage where
  decCBOR = decode $ RecD InclusionUsage <! From <! From <! From

emptyInclusionUsage :: InclusionUsage
emptyInclusionUsage = InclusionUsage 0 (Coin 0) mempty

instance Semigroup InclusionUsage where
  InclusionUsage b1 f1 e1 <> InclusionUsage b2 f2 e2 =
    InclusionUsage (b1 + b2) (f1 <> f2) (e1 <> e2)

instance Monoid InclusionUsage where
  mempty = emptyInclusionUsage

-- | Starting state. 'Optimistic' opens at today's @minFeeA@ rate
-- (44 lovelace\/byte); 'Urgent' opens at 16× that (the sim's
-- @initialCoefficient@ for the priority controller).
initialPricingState :: DynamicPricing era
initialPricingState =
  DynamicPricing
    { publishedPrices =
        unsafePrices (InclusionPrice (Coin (16 * 44))) (InclusionPrice (Coin 44))
    , blockUsage = Map.empty
    , pendingRefunds = Map.empty
    }

-- | Record a refund owed to a bidder (U3 of the fee split): the unused
-- headroom between the bid and the charged quote.
addPendingRefund :: Credential Staking -> Coin -> DynamicPricing era -> DynamicPricing era
addPendingRefund cred amount ps =
  ps {pendingRefunds = Map.insertWith (<>) cred amount (pendingRefunds ps)}

-- | Hand over all pending refunds (the LEDGER rule credits them to account
-- balances after every transaction; spec: the @feeRewards@ flush).
drainPendingRefunds :: DynamicPricing era -> (Map (Credential Staking) Coin, DynamicPricing era)
drainPendingRefunds ps = (pendingRefunds ps, ps {pendingRefunds = Map.empty})

-- | Account for one transaction delivered under an inclusion strategy
-- (spec: @processTxTiers@).
recordTx :: Inclusion -> TxSizeInBytes -> Coin -> ExUnits -> DynamicPricing era -> DynamicPricing era
recordTx strategy size fee exUnits ps =
  ps
    { blockUsage =
        Map.insertWith (<>) strategy (InclusionUsage size fee exUnits) (blockUsage ps)
    }

-- | The current public price of an inclusion strategy. Total — the protocol
-- always has a price for every strategy.
currentPrice :: Inclusion -> DynamicPricing era -> InclusionPrice
currentPrice strategy = priceOf strategy . publishedPrices

-- | End-of-block repricing (spec: @updateTiers@, still the identity there).
--
-- TODO(prototype): per-strategy EIP-1559 controller from the abstract-sim
-- (@src\/Pricing.hs@):
--
-- @
--   newPrice   = oldPrice × max 0 (1 + adjustment)
--   adjustment = ((utilisation − target) ÷ target) ÷ maxChangeDenominator
-- @
--
-- with target = 0.5, maxChangeDenominator = 8 (so at most ±12.5% per block,
-- rounded deterministically on lovelace), republished through
-- 'mkInclusionPrices' so the price-discrimination floor holds. Open question
-- (Giorgos' deck, slide 12): the utilisation signal for 'Optimistic'.
reprice :: DynamicPricing era -> InclusionPrices
reprice = publishedPrices

-- | Block boundary (spec: the @DIVUP@ rule): apply 'reprice', reset usage.
-- The spec's @sdChecks@ premise (the optimistic usage fits RB limits) lives
-- with the BBODY rule, not here.
endOfBlock :: DynamicPricing era -> DynamicPricing era
endOfBlock ps =
  ps
    { publishedPrices = reprice ps
    , blockUsage = Map.empty
    }
