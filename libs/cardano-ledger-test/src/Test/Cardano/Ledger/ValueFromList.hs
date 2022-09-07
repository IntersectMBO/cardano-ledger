{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}

-- | abstract the 'Core.Value` type family into a type class that can be
-- interacted with somewhat generically.
module Test.Cardano.Ledger.ValueFromList where

import qualified Cardano.Ledger.Crypto as C
import Cardano.Ledger.Mary.Value as Mary
  ( AssetName,
    MaryValue (..),
    MultiAsset (..),
    PolicyID (..),
    insert,
    multiAssetFromList,
  )
import Cardano.Ledger.Val as Val
import Data.Map.Strict as Map

class Val.Val val => ValueFromList val c | val -> c where
  valueFromList :: Integer -> [(PolicyID c, AssetName, Integer)] -> val

  insert :: (Integer -> Integer -> Integer) -> PolicyID c -> AssetName -> Integer -> val -> val

  gettriples :: val -> (Integer, [(PolicyID c, AssetName, Integer)])

instance C.Crypto c => ValueFromList (MaryValue c) c where
  valueFromList c triples = MaryValue c (Mary.multiAssetFromList triples)

  insert combine pid an new (MaryValue c ma) = MaryValue c $ Mary.insert combine pid an new ma

  gettriples (MaryValue c (MultiAsset m1)) = (c, triples)
    where
      triples =
        [ (policyId, aname, amount)
          | (policyId, m2) <- Map.toList m1,
            (aname, amount) <- Map.toList m2
        ]
