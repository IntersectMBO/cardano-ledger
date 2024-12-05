{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}

-- | abstract the 'Core.Value` type family into a type class that can be
-- interacted with somewhat generically.
module Test.Cardano.Ledger.ValueFromList where

import Cardano.Ledger.Coin
import Cardano.Ledger.Mary.Value as Mary (
  AssetName,
  MaryValue (..),
  MultiAsset (..),
  PolicyID (..),
  insertMultiAsset,
  multiAssetFromList,
 )
import Cardano.Ledger.Val as Val
import Data.Map.Strict as Map

class Val.Val val => ValueFromList val where
  valueFromList :: Integer -> [(PolicyID, AssetName, Integer)] -> val

  insert :: (Integer -> Integer -> Integer) -> PolicyID -> AssetName -> Integer -> val -> val

  gettriples :: val -> (Integer, [(PolicyID, AssetName, Integer)])

instance ValueFromList MaryValue where
  valueFromList c triples = MaryValue (Coin c) (Mary.multiAssetFromList triples)

  insert combine pid an new (MaryValue c ma) = MaryValue c $ Mary.insertMultiAsset combine pid an new ma

  gettriples (MaryValue (Coin c) (MultiAsset m1)) = (c, triples)
    where
      triples =
        [ (policyId, aname, amount)
        | (policyId, m2) <- Map.toList m1
        , (aname, amount) <- Map.toList m2
        ]
