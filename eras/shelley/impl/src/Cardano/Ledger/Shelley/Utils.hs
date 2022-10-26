module Cardano.Ledger.Shelley.Utils 
  ( Split(..)
  )
where

import Cardano.Ledger.Coin (Coin)

class Split v where
  vsplit :: v -> Integer -> ([v], Coin)

