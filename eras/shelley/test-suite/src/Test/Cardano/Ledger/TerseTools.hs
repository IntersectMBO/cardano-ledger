{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}

-- | This module suppies tools to tersely describe the differences between 2 values of the same type.
module Test.Cardano.Ledger.TerseTools where

import Cardano.Ledger.Address (Addr (..))
import Cardano.Ledger.Coin (Coin (..), CompactForm (CompactCoin))
import Cardano.Ledger.Core
import Cardano.Ledger.Credential (
  Credential (..),
  Ptr (..),
  StakeReference (..),
 )
import Cardano.Ledger.State (UTxO (..))
import Cardano.Ledger.TxIn (TxId (..), TxIn (..))
import qualified Data.Map.Strict as Map

-- ====================================================

class Terse t where
  terse :: t -> String

data Case a b = OnLeft !a !b | OnRight !a !b | SameKey !a !b !b

instance (Terse a, Terse b) => Show (Case a b) where
  show (OnLeft a b) = "Left " ++ terse (a, b)
  show (OnRight a b) = "Right" ++ terse (a, b)
  show (SameKey a b c) = "Same " ++ terse (a, b, c)

instance (Terse a, Terse b) => Terse (Case a b) where
  terse = show

instance (Terse a, Terse b) => Terse (a, b) where
  terse (a, b) = "(" ++ terse a ++ ", " ++ terse b ++ ")"

instance (Terse a, Terse b, Terse c) => Terse (a, b, c) where
  terse (a, b, c) = "(" ++ terse a ++ ", " ++ terse b ++ ", " ++ terse c ++ ")"

caseKey :: Case p b -> p
caseKey (OnLeft k _) = k
caseKey (OnRight k _) = k
caseKey (SameKey k _ _) = k

-- | we assume the lists are lexigraphically sorted
differences :: (Ord a, Eq b) => [(a, b)] -> [(a, b)] -> [Case a b]
differences [] [] = []
differences xs [] = (map (\(a, b) -> OnLeft a b) xs)
differences [] ys = (map (\(a, b) -> OnRight a b) ys)
differences ((a1, b1) : xs) ((a2, b2) : ys) =
  case compare a1 a2 of
    EQ -> if b1 == b2 then differences xs ys else (SameKey a1 b1 b2) : differences xs ys
    LT -> (OnLeft a1 b1) : differences xs ((a2, b2) : ys)
    GT -> (OnRight a2 b2) : differences ((a1, b1) : xs) ys

mapdiffs :: (Ord a, Eq b) => Map.Map a b -> Map.Map a b -> [Case a b]
mapdiffs mp1 mp2 = differences (Map.toAscList mp1) (Map.toAscList mp2)

terselist :: Terse a => [Char] -> [a] -> [Char]
terselist message xs = "\n" ++ message ++ "\n" ++ unlines (map terse xs)

terselistfilter :: Terse a => [Char] -> (a -> Bool) -> [a] -> [Char]
terselistfilter message p xs = "\n" ++ message ++ "\n" ++ unlines (map terse (filter p xs))

tersemap :: (Terse k, Terse a) => [Char] -> Map.Map k a -> [Char]
tersemap message mp = terselist message (Map.toAscList mp)

tersemapfilter :: (Terse k, Terse a) => [Char] -> (a -> Bool) -> Map.Map k a -> [Char]
tersemapfilter message p mp = terselistfilter message (\(_, a) -> p a) (Map.toAscList mp)

tersemapdiffs :: (Terse a, Terse b, Ord a, Eq b) => String -> Map.Map a b -> Map.Map a b -> [Char]
tersemapdiffs message mp1 mp2 = terselist message (mapdiffs mp1 mp2)

instance Terse Addr where
  terse (Addr _net cred1 (StakeRefBase cred2)) = "Addr (" ++ terse cred1 ++ ") (" ++ terse cred2 ++ ")"
  terse (Addr _net cred (StakeRefPtr ptr)) = "Addr (" ++ terse cred ++ ") (" ++ terse ptr ++ ")"
  terse (Addr _net cred StakeRefNull) = "Addr (" ++ terse cred ++ ") Null"
  terse (AddrBootstrap x) = "BootStrap " ++ show x

instance Terse (Credential r) where
  terse (ScriptHashObj (ScriptHash hash)) = "Script " ++ show hash
  terse (KeyHashObj (KeyHash hash)) = "Key " ++ show hash

instance Terse Ptr where
  terse (Ptr n i j) = "Ptr " ++ show n ++ " " ++ show i ++ " " ++ show j

instance Terse TxId where
  terse (TxId safehash) = show (extractHash safehash)

instance Terse TxIn where
  terse (TxIn txid n) = "In " ++ terse txid ++ " " ++ show n

instance Terse Coin where
  terse (Coin n) = show n

instance Terse (CompactForm Coin) where
  terse (CompactCoin n) = show n

terseutxo :: Terse (TxOut era) => String -> UTxO era -> String
terseutxo message (UTxO mp) = terselist message (Map.toList mp)
