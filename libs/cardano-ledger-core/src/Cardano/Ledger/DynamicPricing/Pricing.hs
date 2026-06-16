{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

-- | Prices and quotes for the dynamic pricing mechanism.
--
-- Deck notation (Giorgos): DP1 = the 'urgent' price, DP2 = the 'optimistic'
-- price.
--
-- Spec correspondence (@formal-ledger-specifications\@polina\/dynamic@):
--
-- @
--   InclusionPrice    \<-\> TierCoeff \/ PolicyClause.coeffRange
--   InclusionPrices   \<-\> DiversityPolicy
--   Quote (lovelace)  \<-\> the @tierCoeff × minfee@ premise, made first-class
-- @
--
-- Implementation choice (to be validated at the weekly): an 'InclusionPrice'
-- is a per-byte rate in lovelace (mechanism-design doc: \"the per-byte rate
-- is the dynamic part\"), matching the sim's fee formula. The spec instead
-- applies a dimensionless multiplier to the full min fee (incl. script
-- costs) — known divergence, ABSTRACT_SIM findings §2; only 'quoteFor'
-- changes if the spec side wins.
module Cardano.Ledger.DynamicPricing.Pricing (
  -- * Prices
  InclusionPrice (..),
  InclusionPrices (urgent, optimistic),
  mkInclusionPrices,
  priceDiscriminationFloor,
  priceOf,

  -- * Measuring a transaction
  MinimumTxFee (..),
  minimumTxFee,
  TxSizeInBytes (..),
  txSizeInBytes,

  -- * Quotes
  Quote (..),
  quoteFor,
) where

import Cardano.Ledger.Binary (DecCBOR, EncCBOR)
import Cardano.Ledger.Coin (Coin (..))
import Cardano.Ledger.Core (EraTx, PParams, Tx, getMinFeeTx, ppTxFeeFixedL, sizeTxF)
import Control.DeepSeq (NFData)
import Cardano.Ledger.DynamicPricing.InclusionStrategy (Inclusion (..))
import Data.Word (Word32)
import GHC.Generics (Generic)
import Lens.Micro ((^.))
import NoThunks.Class (NoThunks)

-- | An inclusion strategy's public price: the rate, in lovelace per byte of
-- transaction, currently demanded by the protocol. This is the value the
-- end-of-block controllers move (deck: DP1\/DP2).
--
-- Today's protocol equivalent is @minFeeA@ (44 lovelace\/byte on mainnet):
-- 'Optimistic' starts there, 'Urgent' starts at a multiple of it.
newtype InclusionPrice = InclusionPrice {unInclusionPrice :: Coin}
  deriving (Eq, Ord, Show, Generic)
  deriving newtype (EncCBOR, DecCBOR, NFData, NoThunks)

-- | The protocol-mandated minimum fee of a transaction — what the chain
-- charges today regardless of inclusion strategy (size, scripts, the lot).
newtype MinimumTxFee = MinimumTxFee {unMinimumTxFee :: Coin}
  deriving (Eq, Ord, Show, Generic)

-- | A transaction's size, in bytes — the resource an 'InclusionPrice' is
-- charged on.
newtype TxSizeInBytes = TxSizeInBytes {unTxSizeInBytes :: Word32}
  deriving stock (Eq, Ord, Show, Generic)
  deriving newtype (Num, EncCBOR, DecCBOR, NFData, NoThunks)

-- | Measure a transaction (via @sizeTxF@).
txSizeInBytes :: EraTx era => Tx l era -> TxSizeInBytes
txSizeInBytes tx = TxSizeInBytes (tx ^. sizeTxF)

-- | Compute a transaction's 'MinimumTxFee' (via @getMinFeeTx@).
--
-- NOTE(prototype): reference-script size is taken as 0 for now.
minimumTxFee :: EraTx era => PParams era -> Tx l era -> MinimumTxFee
minimumTxFee pp tx = MinimumTxFee (getMinFeeTx pp tx 0)

-- | A quote: the fee, in lovelace, the protocol currently demands for one
-- specific transaction under one specific inclusion strategy. This is what
-- wallets display and what the transaction's @fee@ field (acting as max
-- fee) is checked against: a bid is valid while @quote ≤ maxFee@.
newtype Quote = Quote {unQuote :: Coin}
  deriving (Eq, Ord, Show, Generic)

-- | Price a transaction under an inclusion strategy:
-- @quote = max minimumTxFee (txFeeFixed + rate × txSizeInBytes)@.
--
-- The quote never undercuts the transaction's 'MinimumTxFee': script-heavy
-- transactions keep paying their execution costs in full.
--
-- = An open design question lives here: WHICH resources does urgency reprice?
--
-- A transaction consumes two scarce resources: block bytes (size) and
-- computation (ExUnits). The sim and the spec disagree on which of the two
-- the dynamic premium applies to:
--
-- [Sim (implemented here)] @quote = txFeeFixed + rate × size@, floored at
-- the protocol minimum. Only /block bytes/ are repriced by congestion;
-- computation stays at today's flat ExUnits prices, merely covered by the
-- floor. Consequence: two same-size urgent transactions pay the same
-- premium even if one burns 1000× more CPU — under compute-bound congestion
-- the premium misprices the actual scarcity, and a compute-heavy spammer
-- buys 'Urgent' relatively cheap.
--
-- [Spec (@tierCoeff × minfee ≤ txFee@)] the multiplier scales the /full/
-- minimum fee — constant part, bytes AND script costs. Consequence: the
-- premium tracks total resource consumption, but the fixed part scales too
-- (at 16×, every urgent transaction pays ~2.5₳ of constant fee before its
-- first byte), and \"price per byte\" stops being the published unit, which
-- breaks the mechanism-design doc's framing (\"the per-byte rate is the
-- dynamic part\", @minFeeB@ \"never multiplied\").
--
-- Note the tension with the rest of the mechanism: usage accounting and the
-- spec's @sdChecks@ both track bytes AND ExUnits as congestion signals — if
-- ExUnits feed the controller but are not repriced, demand can push prices
-- that the compute-heavy users causing it do not proportionally pay.
--
-- To settle with Polina\/Will (ABSTRACT_SIM findings §2). Only this function
-- changes either way.
quoteFor ::
  EraTx era =>
  PParams era ->
  Tx l era ->
  InclusionPrice ->
  Quote
quoteFor pp tx (InclusionPrice (Coin rate)) =
  Quote $ max txMinimum sizePriced
  where
    MinimumTxFee txMinimum = minimumTxFee pp tx
    Coin minFeeB = pp ^. ppTxFeeFixedL
    TxSizeInBytes size = txSizeInBytes tx
    sizePriced = Coin $ minFeeB + rate * fromIntegral size

-- | The two dynamic prices: the protocol always publishes exactly one price
-- per inclusion strategy — no more, no less. Total by construction (no
-- lookup can fail, no inconsistent key\/value pairing is representable).
--
-- The raw constructor is not exported: build with 'mkInclusionPrices',
-- which guarantees the price-discrimination invariant.
data InclusionPrices = InclusionPrices
  { urgent :: !InclusionPrice
  -- ^ Deck: DP1.
  , optimistic :: !InclusionPrice
  -- ^ Deck: DP2.
  }
  deriving (Eq, Show, Generic)

instance NoThunks InclusionPrices

instance NFData InclusionPrices

-- | The price-discrimination guarantee (mechanism-design doc): 'Urgent'
-- always costs at least this many times 'Optimistic', whatever the load
-- regime — so the premium service keeps meaning something even when both
-- prices drift.
--
-- Sim default (@multiplierFloor@); eventually a protocol parameter.
priceDiscriminationFloor :: Integer
priceDiscriminationFloor = 16

-- | Publish the prices, enforcing
-- @urgent ≥ priceDiscriminationFloor × optimistic@.
mkInclusionPrices ::
  -- | 'Urgent' price (deck: DP1).
  InclusionPrice ->
  -- | 'Optimistic' price (deck: DP2).
  InclusionPrice ->
  Maybe InclusionPrices
mkInclusionPrices u@(InclusionPrice (Coin p)) o@(InclusionPrice (Coin s))
  | p >= priceDiscriminationFloor * s = Just (InclusionPrices u o)
  | otherwise = Nothing

-- | Read the price of one inclusion strategy. Total — the protocol always
-- quotes every strategy.
priceOf :: Inclusion -> InclusionPrices -> InclusionPrice
priceOf Urgent = urgent
priceOf Optimistic = optimistic
