{-# LANGUAGE OverloadedStrings #-}

module Cardano.Ledger.Conway.Genesis
  ( ConwayGenesis (..),
    extendPPWithGenesis,
  )
where

import Cardano.Ledger.Babbage.Genesis (extendPPWithGenesis)
import Cardano.Ledger.Keys (GenDelegs)
import Data.Aeson (FromJSON (..), withObject, (.:))
import Data.Unit.Strict (forceElemsToWHNF)
import Cardano.Ledger.Crypto (Crypto)

newtype ConwayGenesis crypto = ConwayGenesis (GenDelegs crypto)

instance Crypto crypto => FromJSON (ConwayGenesis crypto) where
  parseJSON = withObject "ConwayGenesis" $ \obj ->
    ConwayGenesis 
      <$> forceElemsToWHNF obj .: "genDelegs"
