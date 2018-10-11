{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}

module Cardano.Chain.Txp.TxOutAux
       ( TxOutAux (..)
       ) where

import           Cardano.Prelude

import           Data.Aeson.TH (defaultOptions, deriveJSON)
import           Formatting (bprint, build, (%))
import qualified Formatting.Buildable as B

import           Cardano.Binary.Class (Bi (..), encodeListLen, enforceSize)
import           Cardano.Chain.Txp.Tx (TxOut)


-- | Transaction output and auxilary data corresponding to it
data TxOutAux = TxOutAux
  { toaOut :: !TxOut
  -- ^ Tx output
  } deriving (Generic, Show, Eq, Ord)

instance B.Buildable TxOutAux where
  build (TxOutAux out) = bprint ("{txout = "%build%"}") out

instance NFData TxOutAux

instance Bi TxOutAux where
  encode toa = encodeListLen 1 <> encode (toaOut toa)
  decode = enforceSize "TxOutAux" 1 >> TxOutAux <$> decode

deriveJSON defaultOptions ''TxOutAux
