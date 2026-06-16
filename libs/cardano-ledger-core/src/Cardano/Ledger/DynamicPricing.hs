-- | Domain model for dynamic pricing: transactions purchase one of two
-- inclusion strategies, each with its own protocol-driven price.
--
-- Umbrella module re-exporting the whole vocabulary. The /lanes/ (RB vs EB
-- transport) are infrastructure and deliberately absent from it.
--
-- Spec correspondence (@formal-ledger-specifications\@polina\/dynamic@):
--
-- @
--   Inclusion          \<-\> TierNo (fastTier = 0, slowTier = 1)
--   InclusionPrice     \<-\> TierCoeff \/ PolicyClause.coeffRange
--   InclusionPrices    \<-\> DiversityPolicy
--   Quote (lovelace)   \<-\> the @tierCoeff × minfee@ premise, made first-class
--   declared strategy  \<-\> TxTier (without the declared tierCoeff)
--   feeRefundAccount   \<-\> feeChangeAddr
--   delivered strategy \<-\> actualTier
--   DynamicPricing     \<-\> SDPolicy
--   reprice            \<-\> updateTiers
-- @
module Cardano.Ledger.DynamicPricing (
  module Cardano.Ledger.DynamicPricing.InclusionStrategy,
  module Cardano.Ledger.DynamicPricing.Pricing,
  module Cardano.Ledger.DynamicPricing.State,
) where

import Cardano.Ledger.DynamicPricing.InclusionStrategy
import Cardano.Ledger.DynamicPricing.Pricing
import Cardano.Ledger.DynamicPricing.State
