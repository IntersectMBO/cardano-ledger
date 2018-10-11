{-# LANGUAGE DeriveAnyClass     #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts   #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE TemplateHaskell    #-}

-- | Types used in epoch boundary blocks

module Cardano.Chain.Block.Boundary
       ( BoundaryProof (..)
       , mkBoundaryProof
       , checkBoundaryProof

       , BoundaryConsensusData (..)
       , gcdEpoch
       , gcdDifficulty

       , BoundaryHeaderAttributes

       , BoundaryExtraHeaderData (..)
       , gehAttributes

       , BoundaryBody (..)
       , gbLeaders

       , BoundaryBodyAttributes

       , BoundaryExtraBodyData (..)
       , bebAttributes
       ) where

import           Cardano.Prelude

import           Control.Lens (makeLenses)
import           Control.Monad.Except (MonadError)
import           Formatting (bprint, build, (%))
import qualified Formatting.Buildable as B

import           Cardano.Binary.Class (Bi (..), encodeListLen, enforceSize)
import           Cardano.Chain.Block.Util (checkBodyProof)
import           Cardano.Chain.Common (Attributes, ChainDifficulty, SlotLeaders,
                     areAttributesKnown)
import           Cardano.Chain.Slotting (EpochIndex (..))
import           Cardano.Crypto (Hash, hash)


--------------------------------------------------------------------------------
-- BoundaryProof
--------------------------------------------------------------------------------

-- | Proof of BoundaryBody is just a hash of slot leaders list
newtype BoundaryProof =
  BoundaryProof (Hash SlotLeaders)
  deriving (Eq, Generic, Show)
  deriving anyclass NFData

instance B.Buildable BoundaryProof where
  build (BoundaryProof h) = B.build h

instance Bi BoundaryProof where
  encode (BoundaryProof h) = encode h
  decode = BoundaryProof <$> decode

mkBoundaryProof :: BoundaryBody -> BoundaryProof
mkBoundaryProof = BoundaryProof . hash . _gbLeaders

checkBoundaryProof :: MonadError Text m => BoundaryBody -> BoundaryProof -> m ()
checkBoundaryProof = checkBodyProof mkBoundaryProof


--------------------------------------------------------------------------------
-- BoundaryConsensusData
--------------------------------------------------------------------------------

data BoundaryConsensusData = BoundaryConsensusData
  { _gcdEpoch      :: !EpochIndex
  -- ^ Index of the slot for which this genesis block is relevant
  , _gcdDifficulty :: !ChainDifficulty
  -- ^ Difficulty of the chain ending in this genesis block
  } deriving (Generic, Show, Eq)
    deriving anyclass NFData

instance Bi BoundaryConsensusData where
  encode bc =
    encodeListLen 2 <> encode (_gcdEpoch bc) <> encode (_gcdDifficulty bc)

  decode = do
    enforceSize "BoundaryConsensusData" 2
    BoundaryConsensusData <$> decode <*> decode


--------------------------------------------------------------------------------
-- BoundaryHeaderAttributes
--------------------------------------------------------------------------------

-- | Represents genesis block header attributes
type BoundaryHeaderAttributes = Attributes ()


--------------------------------------------------------------------------------
-- BoundaryExtraHeaderData
--------------------------------------------------------------------------------

-- | Represents genesis block header extra data
newtype BoundaryExtraHeaderData = BoundaryExtraHeaderData
  { _gehAttributes :: BoundaryHeaderAttributes
  } deriving (Eq, Show, Generic)
    deriving anyclass NFData

instance B.Buildable BoundaryExtraHeaderData where
  build (BoundaryExtraHeaderData attrs)
    | areAttributesKnown attrs = "no extra data"
    | otherwise = bprint ("extra data has attributes: " % build) attrs

instance Bi BoundaryExtraHeaderData where
  encode behd = encodeListLen 1 <> encode (_gehAttributes behd)
  decode = do
    enforceSize "BoundaryExtraHeaderData" 1
    BoundaryExtraHeaderData <$> decode


--------------------------------------------------------------------------------
-- BoundaryBody
--------------------------------------------------------------------------------

-- | Body of genesis block consists of slot leaders for epoch associated with
--   this block
newtype BoundaryBody = BoundaryBody
  { _gbLeaders :: SlotLeaders
  } deriving (Generic, Show, Eq)
    deriving anyclass NFData

instance Bi BoundaryBody where
  encode = encode . _gbLeaders
  decode = BoundaryBody <$> decode


--------------------------------------------------------------------------------
-- BoundaryBodyAttributes
--------------------------------------------------------------------------------

-- | Represents genesis block header attributes
type BoundaryBodyAttributes = Attributes ()


--------------------------------------------------------------------------------
-- BoundaryExtraBodyData
--------------------------------------------------------------------------------

-- | Represents genesis block header extra data
newtype BoundaryExtraBodyData = BoundaryExtraBodyData
  { _bebAttributes :: BoundaryBodyAttributes
  } deriving (Eq, Show, Generic)
    deriving anyclass NFData

instance B.Buildable BoundaryExtraBodyData where
  build (BoundaryExtraBodyData attrs)
    | areAttributesKnown attrs = "no extra data"
    | otherwise = bprint ("extra data has attributes: " % build) attrs

instance Bi BoundaryExtraBodyData where
  encode bodyExtra = encodeListLen 1 <> encode (_bebAttributes bodyExtra)
  decode = do
    enforceSize "BoundaryExtraBodyData" 1
    BoundaryExtraBodyData <$> decode


--------------------------------------------------------------------------------
-- BoundaryConsensusData lenses
--------------------------------------------------------------------------------

makeLenses 'BoundaryConsensusData


--------------------------------------------------------------------------------
-- BoundaryBody
--------------------------------------------------------------------------------

makeLenses 'BoundaryBody


--------------------------------------------------------------------------------
-- BoundaryExtra lenses
--------------------------------------------------------------------------------

makeLenses ''BoundaryExtraHeaderData
makeLenses ''BoundaryExtraBodyData
