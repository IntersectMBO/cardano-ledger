{-# LANGUAGE BangPatterns #-}

module Cardano.Ledger.State.Transform where

import Control.DeepSeq
import qualified Data.Map.Strict as Map
import Data.Map.Strict.Internal
import Cardano.Ledger.State.UTxO
import Cardano.Ledger.Address
import Cardano.Ledger.Shelley.CompactAddr
import Cardano.Ledger.Alonzo.TxBody as Alonzo
import Cardano.Ledger.Credential
import Debug.Trace

-- data Addr'
--   = AddrKeyIx' !Network !Ix1 !StakeIx
--   | AddrKeyHash' !Network !(Keys.KeyHash 'Shelley.Payment C) !StakeIx
--   | AddrScript' !Network !(Shelley.ScriptHash C) !StakeIx
--   | AddrBoot' !(CompactAddr C)

-- data TxOut'
--   = TxOut' !Addr' !Word64
--   | TxOutMA' !Addr' !Word64 !Word32 !ShortByteString
--   | TxOutDH' !Addr' !Word64 !(DataHash C)
--   | TxOutMADH' !Addr' !Word64 !Word32 !ShortByteString !(DataHash C)


data TxOut'
  = TxOut' !(Alonzo.TxOut CurrentEra) !(StakeCredential C)
  | TxOutNoStake' !(Alonzo.TxOut CurrentEra)

instance NFData TxOut' where
  rnf (TxOut' _ _) = ()
  rnf (TxOutNoStake' _) = ()


-- transTxOut :: TxOut CurrentEra -> TxOut' CurrentEra
-- transTxOut = \case


toTxOut' :: Map (StakeCredential C) a -> Alonzo.TxOut CurrentEra -> TxOut'
toTxOut' m txOut =
  case txOut of
    Alonzo.TxOutCompact cAddr cVal
      | Just (cAddr', sr) <- restructureAddr cAddr ->
        TxOut' (Alonzo.TxOutCompact cAddr' cVal) sr
    Alonzo.TxOutCompactDH cAddr cVal dh
      | Just (cAddr', sr) <- restructureAddr cAddr ->
        TxOut' (Alonzo.TxOutCompactDH cAddr' cVal dh) sr
    _ -> TxOutNoStake' txOut
  where
    restructureAddr cAddr =
      case decompactAddr cAddr of
        Addr ni pc (StakeRefBase sr) ->
          Just (compactAddr (Addr ni pc StakeRefNull), intern' sr m)
        _ -> Nothing

intern' :: (Show k, Ord k) => k -> Map k a -> k
intern' k m =
  case Map.lookupIndex k m of
    Nothing -> trace ("Did not find: " ++ show k) k
    Just ix -> fst $ Map.elemAt ix m

intern :: (Show k, Ord k) => k -> Map k a -> k
intern !k = go
  where
    go Tip = error $ "Did not find: " ++ show k
    go (Bin _ kx _ l r) =
      case compare k kx of
        LT -> go l
        GT -> go r
        EQ -> kx
