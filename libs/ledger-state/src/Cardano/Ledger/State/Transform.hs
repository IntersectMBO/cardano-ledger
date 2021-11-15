{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies #-}

module Cardano.Ledger.State.Transform where

import Cardano.Ledger.Alonzo.Data
import Cardano.Ledger.Alonzo.TxBody as Alonzo
import Cardano.Ledger.Coin
import Cardano.Ledger.Core as Core
import Cardano.Ledger.Credential
import Cardano.Ledger.Keys
import Cardano.Ledger.Shelley.CompactAddr
import Cardano.Ledger.State.UTxO
import Control.DeepSeq
import Data.Map.Strict.Internal
import Data.Word

data TxOut'
  = TxOut'
      {-# UNPACK #-} !(CompactAddr C)
      !(CompactForm (Core.Value CurrentEra))
  | TxOutDH'
      {-# UNPACK #-} !(CompactAddr C)
      !(CompactForm (Core.Value CurrentEra))
      !(DataHash C)
  | TxOutStaking'
      !(Credential 'Staking C)
      {-# UNPACK #-} !Word64 -- Payment Addr
      {-# UNPACK #-} !Word64 -- Payment Addr
      {-# UNPACK #-} !Word64 -- Payment Addr
      {-# UNPACK #-} !Word64 -- Payment Addr (32bits) + ... +  0/1 for Testnet/Mainnet + 0/1 Script/Pubkey
      {-# UNPACK #-} !(CompactForm Coin) -- Ada value
  | TxOutStakingDH'
      !(Credential 'Staking C)
      {-# UNPACK #-} !Word64 -- Payment Addr
      {-# UNPACK #-} !Word64 -- Payment Addr
      {-# UNPACK #-} !Word64 -- Payment Addr
      {-# UNPACK #-} !Word64 -- Payment Addr (32bits) + ... +  0/1 for Testnet/Mainnet + 0/1 Script/Pubkey
      {-# UNPACK #-} !(CompactForm Coin) -- Ada value
      {-# UNPACK #-} !Word64 -- DataHash
      {-# UNPACK #-} !Word64 -- DataHash
      {-# UNPACK #-} !Word64 -- DataHash
      {-# UNPACK #-} !Word64 -- DataHash

instance NFData TxOut' where
  rnf (TxOut' _ _) = ()
  rnf (TxOutDH' _ _ _) = ()
  rnf TxOutStaking' {} = ()
  rnf TxOutStakingDH' {} = ()

toTxOut' :: Alonzo.TxOut CurrentEra -> TxOut'
toTxOut' txOut =
  case txOut of
    Alonzo.TxOutCompact' cAddr cVal -> TxOut' cAddr cVal
    Alonzo.TxOutCompactDH' cAddr cVal dh -> TxOutDH' cAddr cVal dh
    Alonzo.TxOut_AddrHash28_AdaOnly cred a b c d ada ->
      TxOutStaking' cred a b c d ada
    TxOut_AddrHash28_AdaOnly_DataHash32 cred a b c d ada o p q r ->
      TxOutStakingDH' cred a b c d ada o p q r

toOrigTxOut' :: Alonzo.TxOut CurrentEra -> TxOut'
toOrigTxOut' txOut =
  case txOut of
    Alonzo.TxOutCompact cAddr cVal -> TxOut' cAddr cVal
    Alonzo.TxOutCompactDH cAddr cVal dh -> TxOutDH' cAddr cVal dh

-- toTxOutWithSharing' :: Map (StakeCredential C) a -> Alonzo.TxOut CurrentEra -> TxOut' CurrentEra
-- toTxOut' m txOut =
--   case txOut of
--     Alonzo.TxOutCompact cAddr cVal
--       | Just (cAddr', sr) <- restructureAddr cAddr ->
--         TxOut' (Alonzo.TxOutCompact cAddr' cVal) sr
--     Alonzo.TxOutCompactDH cAddr cVal dh
--       | Just (cAddr', sr) <- restructureAddr cAddr ->
--         TxOut' (Alonzo.TxOutCompactDH cAddr' cVal dh) sr
--     _ -> TxOutNoStake' txOut
--   where
--     restructureAddr cAddr =
--       case decompactAddr cAddr of
--         Addr ni pc (StakeRefBase sr) ->
--           Just (compactAddr (Addr ni pc StakeRefNull), intern sr m)
--         _ -> Nothing

-- intern' :: (Show k, Ord k) => k -> Map k a -> k
-- intern' k m =
--   case Map.lookupIndex k m of
--     Nothing -> k
--     Just ix -> fst $ Map.elemAt ix m

intern :: Ord k => k -> Map k a -> k
intern !k m =
  case internMaybe k m of
    Just kx -> kx
    Nothing -> k

interns :: Ord k => k -> [Map k a] -> k
interns !k = go
  where
    go [] = k
    go (m : ms) =
      case internMaybe k m of
        Just kx -> kx
        Nothing -> go ms

internMaybe :: Ord k => k -> Map k a -> Maybe k
internMaybe !k = go
  where
    go Tip = Nothing
    go (Bin _ kx _ l r) =
      case compare k kx of
        LT -> go l
        GT -> go r
        EQ -> Just kx

internVal :: (Eq a, Ord k) => k -> a -> Map k a -> a
internVal !k !a m =
  case internValMaybe k a m of
    Just ax -> ax
    Nothing -> a

internsVal :: (Eq a, Ord k) => k -> a -> [Map k a] -> a
internsVal !k !a = go
  where
    go [] = a
    go (m : ms) =
      case internValMaybe k a m of
        Just ax -> ax
        Nothing -> go ms

internValMaybe :: (Eq a, Ord k) => k -> a -> Map k a -> Maybe a
internValMaybe !k !a = go
  where
    go Tip = Nothing
    go (Bin _ kx ax l r) =
      case compare k kx of
        LT -> go l
        GT -> go r
        EQ
          | a == ax -> Just ax
          | otherwise -> Nothing
