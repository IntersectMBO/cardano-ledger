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
    valueFromList,
  )
import Cardano.Ledger.Val as Val
import Data.Map.Strict as Map

class Val.Val val => ValueFromList val crypto | val -> crypto where
  valueFromList :: Integer -> [(PolicyID crypto, AssetName, Integer)] -> val

  insert :: (Integer -> Integer -> Integer) -> PolicyID crypto -> AssetName -> Integer -> val -> val

  gettriples :: val -> (Integer, [(PolicyID crypto, AssetName, Integer)])

instance C.Crypto crypto => ValueFromList (MaryValue crypto) crypto where
  valueFromList = Mary.valueFromList

  insert = Mary.insert

  gettriples (MaryValue c (MultiAsset m1)) = (c, triples)
    where
      triples =
        [ (policyId, aname, amount)
          | (policyId, m2) <- Map.toList m1,
            (aname, amount) <- Map.toList m2
        ]
