{-# LANGUAGE DeriveAnyClass     #-}
{-# LANGUAGE DeriveFunctor      #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts   #-}
{-# LANGUAGE FlexibleInstances  #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE TemplateHaskell    #-}
{-# LANGUAGE TypeApplications   #-}

module Cardano.Chain.UTxO.TxAux
  ( TxAux
  , ATxAux(..)
  , mkTxAux
  , taTx
  , taWitness
  , txaF
  )
where

import Cardano.Prelude

import Data.Aeson (FromJSON, ToJSON)
import Formatting (Format, bprint, build, later)
import qualified Formatting.Buildable as B

import Cardano.Binary
  ( Annotated(..)
  , ByteSpan
  , FromCBOR(..)
  , ToCBOR(..)
  , fromCBORAnnotated
  , encodeListLen
  , enforceSize
  )
import Cardano.Chain.UTxO.Tx (Tx)
import Cardano.Chain.UTxO.TxWitness (TxWitness)


-- | Transaction + auxiliary data
type TxAux = ATxAux ()

mkTxAux :: Tx -> TxWitness -> TxAux
mkTxAux tx tw = ATxAux (Annotated tx ()) (Annotated tw ())

data ATxAux a = ATxAux
  { aTaTx      :: !(Annotated Tx a)
  , aTaWitness :: !(Annotated TxWitness a)
  } deriving (Generic, Show, Eq, Functor)
    deriving anyclass NFData

taTx :: ATxAux a -> Tx
taTx = unAnnotated . aTaTx

taWitness :: ATxAux a -> TxWitness
taWitness = unAnnotated . aTaWitness

-- | Specialized formatter for 'TxAux'
txaF :: Format r (TxAux -> r)
txaF = later $ \ta -> bprint
  (build . "\n" . "witnesses: " . listJsonIndent 4)
  (taTx ta)
  (taWitness ta)

instance B.Buildable TxAux where
  build = bprint txaF

instance ToCBOR TxAux where
  toCBOR ta = encodeListLen 2 <> toCBOR (taTx ta) <> toCBOR (taWitness ta)

  encodedSizeExpr size pxy = 1 + size (taTx <$> pxy) + size (taWitness <$> pxy)

instance FromCBOR TxAux where
  fromCBOR = void <$> fromCBOR @(ATxAux ByteSpan)

instance FromCBOR (ATxAux ByteSpan) where
  fromCBOR = do
    enforceSize "TxAux" 2
    ATxAux <$> fromCBORAnnotated <*> fromCBORAnnotated

instance ToJSON (ATxAux ()) where
instance FromJSON (ATxAux ()) where
