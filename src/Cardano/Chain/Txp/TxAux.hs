{-# LANGUAGE DeriveFunctor     #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}

module Cardano.Chain.Txp.TxAux
  ( TxAux
  , ATxAux(..)
  , decodeATxAux
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

import Cardano.Binary.Class
  ( Annotated(..)
  , Bi(..)
  , ByteSpan
  , Decoder
  , decodeAnnotated
  , encodeListLen
  , enforceSize
  )
import Cardano.Chain.Txp.Tx (Tx)
import Cardano.Chain.Txp.TxWitness (TxWitness)


-- | Transaction + auxiliary data
type TxAux = ATxAux ()

mkTxAux :: Tx -> TxWitness -> TxAux
mkTxAux tx tw = ATxAux (Annotated tx ()) (Annotated tw ())

data ATxAux a = ATxAux
  { aTaTx      :: !(Annotated Tx a)
  , aTaWitness :: !(Annotated TxWitness a)
  } deriving (Generic, Show, Eq, Functor)

instance NFData a => NFData (ATxAux a)

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

instance Bi TxAux where
  encode ta = encodeListLen 2 <> encode (taTx ta) <> encode (taWitness ta)

  decode = void <$> decodeATxAux

  encodedSizeExpr size pxy = 1 + size (taTx <$> pxy) + size (taWitness <$> pxy)


decodeATxAux :: Decoder s (ATxAux ByteSpan)
decodeATxAux = do
  enforceSize "TxAux" 2
  ATxAux <$> decodeAnnotated <*> decodeAnnotated



instance ToJSON (ATxAux ()) where
instance FromJSON (ATxAux ()) where
