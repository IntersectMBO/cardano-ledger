{-# LANGUAGE DeriveAnyClass     #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts   #-}
{-# LANGUAGE FlexibleInstances  #-}
{-# LANGUAGE OverloadedStrings  #-}

module Cardano.Chain.UTxO.TxAux
  ( TxAux(..)
  , txaF
  )
where

import Cardano.Prelude

import Formatting (Format, bprint, build, later)
import qualified Formatting.Buildable as B

import Cardano.Binary
  ( FromCBORAnnotated(..)
  , ToCBOR(..)
  , encodeListLen
  , enforceSize
  )
import Cardano.Chain.UTxO.Tx (Tx)
import Cardano.Chain.UTxO.TxWitness (TxWitness(..))


-- | Transaction + auxiliary data
data TxAux = TxAux
  { taTx      :: !Tx
  , taWitness :: !TxWitness
  } deriving (Generic, Show, Eq)
    deriving anyclass NFData

-- | Specialized formatter for 'TxAux'
txaF :: Format r (TxAux -> r)
txaF = later $ \ta -> bprint
  (build . "\n" . "witnesses: " . listJsonIndent 4)
  (taTx ta)
  (txInWitnesses $ taWitness ta)

instance B.Buildable TxAux where
  build = bprint txaF

instance ToCBOR TxAux where
  toCBOR ta = encodeListLen 2 <> toCBOR (taTx ta) <> toCBOR (taWitness ta)

  encodedSizeExpr size pxy = 1 + size (taTx <$> pxy) + size (taWitness <$> pxy)

instance FromCBORAnnotated TxAux where
  fromCBORAnnotated' =
    TxAux <$ lift (enforceSize "TxAux" 2)
      <*> fromCBORAnnotated'
      <*> fromCBORAnnotated'
