{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}

module Cardano.Chain.Txp.TxAux
       ( TxAux (..)
       , txaF
       ) where

import           Cardano.Prelude

import           Data.Aeson.TH (defaultOptions, deriveJSON)
import           Formatting (Format, bprint, build, later, (%))
import qualified Formatting.Buildable as B

import           Cardano.Binary.Class (Bi (..), encodeListLen, enforceSize)
import           Cardano.Chain.Txp.Tx (Tx)
import           Cardano.Chain.Txp.TxWitness (TxWitness)


-- | Transaction + auxiliary data
data TxAux = TxAux
  { taTx      :: !Tx
  , taWitness :: !TxWitness
  } deriving (Generic, Show, Eq)

instance NFData TxAux

-- | Specialized formatter for 'TxAux'
txaF :: Format r (TxAux -> r)
txaF = later $ \(TxAux tx w) ->
  bprint (build % "\n" % "witnesses: " % listJsonIndent 4) tx w

instance B.Buildable TxAux where
  build = bprint txaF

instance Bi TxAux where
  encode ta = encodeListLen 2 <> encode (taTx ta) <> encode (taWitness ta)

  decode = do
    enforceSize "TxAux" 2
    TxAux <$> decode <*> decode

  encodedSizeExpr size pxy = 1 + size (taTx <$> pxy) + size (taWitness <$> pxy)

deriveJSON defaultOptions ''TxAux
