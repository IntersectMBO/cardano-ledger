module Cardano.Ledger.Api.Tx.Address (
  -- * Address
  Addr (..),
  getNetwork,
  BootstrapAddress (..),
  serialiseAddr,

  -- ** Strict decoders

  -- | Decoders below will only decode addresses that are allowed to be placed on chain
  -- today. Historically there were a few bugs in the decoder which allowed a few
  -- malformed addressed to be placed on chain. If you need backwards compatibility, reach
  -- out for `decodeAddrLenient`.
  decodeAddr,
  decodeAddrEither,
  decodeAddrShort,
  decodeAddrShortEither,

  -- ** Lenient decoders

  -- | These lenient decoders do not fail for addresses with known bugs
  DecAddr (..),
  decodeAddrLenient,
  decodeAddrLenientEither,

  -- * Reward Account
  RewardAccount (..),
  serialiseRewardAccount,
  deserialiseRewardAccount,
)
where

import Cardano.Ledger.Address
import Control.Applicative ((<|>))
import Control.Monad.Trans.Fail (runFail, runFailLast)
import Control.Monad.Trans.State.Strict (evalStateT, get)
import qualified Data.ByteString as BS
import Data.ByteString.Short (ShortByteString)

-- | Same as `decodeAddrShort`, but produces an `Either` result
decodeAddrShortEither ::
  ShortByteString ->
  Either String Addr
decodeAddrShortEither sbs = runFail $ evalStateT (decodeAddrStateT sbs) 0
{-# INLINE decodeAddrShortEither #-}

-- | Same as `decodeAddr`, but works on `ShortByteString`
decodeAddrShort ::
  MonadFail m =>
  ShortByteString ->
  m Addr
decodeAddrShort sbs = evalStateT (decodeAddrStateT sbs) 0
{-# INLINE decodeAddrShort #-}

-- | Decoded Address.
data DecAddr
  = -- | Address was decoded with no problems
    DecAddr Addr
  | -- | Address was decoded, but it contains an invalid `Cardano.Ledger.Credential.Ptr`, which
    -- means that address will be decoded with Ptr that has all values clamped to zero.
    DecAddrBadPtr Addr
  | -- | Address was decoded, but not all of input was consumed
    DecAddrUnconsumed
      Addr
      -- | Left over bytes after consuming the input
      BS.ByteString
  deriving (Eq, Show)

-- | This is a lenient decoder that will disregard known bugs in the address
-- deserialization. This function is intended for clients that need to deal with
-- historical data that has already been placed on chain. If you also require information
-- on what exactly is bad in the address, or you would like to guard only against a
-- specific bug, you should use `decodeAddrLenientEither` instead.
--
-- @since 1.8.0
decodeAddrLenient ::
  MonadFail m =>
  BS.ByteString ->
  m Addr
decodeAddrLenient bs = evalStateT (decodeAddrStateLenientT True True bs) 0

-- | Decode an address and fail only for addresses that could have never been placed on
-- chain, while decoding addresses with information about potential problems in
-- them. Similar to `decodeAddrLenient`, this function is not intended for addresses that
-- will be placed into a new transaction.
--
-- @since 1.8.0
decodeAddrLenientEither ::
  BS.ByteString ->
  Either String DecAddr
decodeAddrLenientEither bs =
  runFailLast $
    (DecAddr <$> decodeAddr bs)
      <|> (DecAddrBadPtr <$> evalStateT (decodeAddrStateLenientT True False bs) 0)
      <|> decodeWithUnconsumed
  where
    decodeWithUnconsumed = flip evalStateT 0 $ do
      addr <- decodeAddrStateLenientT False True bs
      bytesConsumed <- get
      pure $ DecAddrUnconsumed addr (BS.drop bytesConsumed bs)
