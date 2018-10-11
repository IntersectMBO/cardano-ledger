{-# LANGUAGE DeriveAnyClass             #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE TemplateHaskell            #-}

-- | Types used inside main blocks

module Cardano.Chain.Block.Main
       ( MainProof (..)
       , mkMainProof
       , checkMainProof

       , MainExtraHeaderData (..)
       , mehBlockVersion
       , mehSoftwareVersion
       , mehAttributes
       , mehEBDataProof
       , verifyMainExtraHeaderData

       , MainBody (..)
       , mbSscPayload
       , mbTxPayload
       , mbDlgPayload
       , mbUpdatePayload
       , mbTxs
       , mbWitnesses
       , verifyMainBody

       , MainExtraBodyData (..)
       , mebAttributes

       , BlockHeaderAttributes
       , BlockBodyAttributes
       ) where

import           Cardano.Prelude

import           Control.Lens (makeLenses)
import           Control.Monad.Except (MonadError)
import           Formatting (bprint, build, builder, (%))
import qualified Formatting.Buildable as B

import           Cardano.Binary.Class (Bi (..), encodeListLen, enforceSize)
import           Cardano.Chain.Block.Util (checkBodyProof)
import           Cardano.Chain.Common.Attributes (Attributes,
                     areAttributesKnown)
import qualified Cardano.Chain.Delegation.Payload as Delegation (Payload,
                     checkPayload)
import           Cardano.Chain.Ssc.Payload (SscPayload, checkSscPayload)
import           Cardano.Chain.Ssc.Proof (SscProof, mkSscProof)
import           Cardano.Chain.Txp.Tx (Tx)
import           Cardano.Chain.Txp.TxPayload (TxPayload, checkTxPayload, txpTxs,
                     txpWitnesses)
import           Cardano.Chain.Txp.TxProof (TxProof, mkTxProof)
import           Cardano.Chain.Txp.TxWitness (TxWitness)
import           Cardano.Chain.Update.BlockVersion (BlockVersion)
import qualified Cardano.Chain.Update.Payload as Update
import qualified Cardano.Chain.Update.Proof as Update
import           Cardano.Chain.Update.SoftwareVersion (SoftwareVersion,
                     checkSoftwareVersion)
import           Cardano.Crypto (Hash, ProtocolMagic, hash)


-- | Proof of everything contained in the payload
data MainProof = MainProof
  { mpTxProof       :: !TxProof
  , mpMpcProof      :: !SscProof
  , mpProxySKsProof :: !(Hash Delegation.Payload)
  , mpUpdateProof   :: !Update.Proof
  } deriving (Eq, Show, Generic, NFData)

instance B.Buildable MainProof where
  build proof = bprint
    ("<MainProof: " % build % ", " % build % ", " % build % ", " % build % ">")
    (mpTxProof proof)
    (mpMpcProof proof)
    (mpProxySKsProof proof)
    (mpUpdateProof proof)

instance Bi MainProof where
  encode bc =
    encodeListLen 4
      <> encode (mpTxProof bc)
      <> encode (mpMpcProof bc)
      <> encode (mpProxySKsProof bc)
      <> encode (mpUpdateProof bc)

  decode = do
    enforceSize "MainProof" 4
    MainProof <$> decode <*> decode <*> decode <*> decode

mkMainProof :: MainBody -> MainProof
mkMainProof body = MainProof
  { mpTxProof       = mkTxProof $ _mbTxPayload body
  , mpMpcProof      = mkSscProof $ _mbSscPayload body
  , mpProxySKsProof = hash $ _mbDlgPayload body
  , mpUpdateProof   = Update.mkProof $ _mbUpdatePayload body
  }

checkMainProof :: MonadError Text m => MainBody -> MainProof -> m ()
checkMainProof = checkBodyProof mkMainProof

-- | Represents main block header attributes: map from 1-byte integer to
--   arbitrary-type value. To be used for extending header with new fields via
--   softfork.
type BlockHeaderAttributes = Attributes ()

-- | Represents main block header extra data
data MainExtraHeaderData = MainExtraHeaderData
  { _mehBlockVersion    :: !BlockVersion
  -- ^ Version of block
  , _mehSoftwareVersion :: !SoftwareVersion
  -- ^ Software version
  , _mehAttributes      :: !BlockHeaderAttributes
  -- ^ Header attributes
  , _mehEBDataProof     :: !(Hash MainExtraBodyData)
  -- ^ Extra body data Hash
  } deriving (Eq, Show, Generic)

instance NFData MainExtraHeaderData

instance B.Buildable MainExtraHeaderData where
  build mehd = bprint
    ("    block: v" % build % "\n" % "    software: " % build % "\n" % builder)
    (_mehBlockVersion mehd)
    (_mehSoftwareVersion mehd)
    formattedExtra
   where
    formattedExtra
      | areAttributesKnown (_mehAttributes mehd) = mempty
      | otherwise = bprint
        ("    attributes: " % build % "\n")
        (_mehAttributes mehd)

instance Bi MainExtraHeaderData where
  encode mehd =
    encodeListLen 4
      <> encode (_mehBlockVersion mehd)
      <> encode (_mehSoftwareVersion mehd)
      <> encode (_mehAttributes mehd)
      <> encode (_mehEBDataProof mehd)

  decode = do
    enforceSize "MainExtraHeaderData" 4
    MainExtraHeaderData <$> decode <*> decode <*> decode <*> decode

verifyMainExtraHeaderData :: MonadError Text m => MainExtraHeaderData -> m ()
verifyMainExtraHeaderData meh = checkSoftwareVersion (_mehSoftwareVersion meh)

-- | 'MainBody' consists of payloads of all block components
data MainBody = MainBody
  { _mbTxPayload     :: !TxPayload
  -- ^ Txp payload
  , _mbSscPayload    :: !SscPayload
  -- ^ Ssc payload
  , _mbDlgPayload    :: !Delegation.Payload
  -- ^ Heavyweight delegation payload (no-ttl certificates)
  , _mbUpdatePayload :: !Update.Payload
  -- ^ Additional update information for the update system
  } deriving (Eq, Show, Generic, NFData, Typeable)

instance Bi MainBody where
  encode bc =
    encodeListLen 4
      <> encode (_mbTxPayload bc)
      <> encode (_mbSscPayload bc)
      <> encode (_mbDlgPayload bc)
      <> encode (_mbUpdatePayload bc)

  decode = do
    enforceSize "MainBody" 4
    MainBody <$> decode <*> decode <*> decode <*> decode

-- | Represents main block body attributes: map from 1-byte integer to
--   arbitrary-type value. To be used for extending block with new fields via
--   softfork.
type BlockBodyAttributes = Attributes ()

-- | Represents main block extra data
newtype MainExtraBodyData = MainExtraBodyData
  { _mebAttributes :: BlockBodyAttributes
  } deriving (Eq, Show, Generic)
    deriving newtype NFData

instance B.Buildable MainExtraBodyData where
  build (MainExtraBodyData attrs)
    | areAttributesKnown attrs = "no extra data"
    | otherwise = bprint ("extra data has attributes: " % build) attrs

instance Bi MainExtraBodyData where
  encode mebd = encodeListLen 1 <> encode (_mebAttributes mebd)
  decode = do
    enforceSize "MainExtraBodyData" 1
    MainExtraBodyData <$> decode

verifyMainBody :: MonadError Text m => ProtocolMagic -> MainBody -> m ()
verifyMainBody pm mb = do
  checkTxPayload (_mbTxPayload mb)
  checkSscPayload pm (_mbSscPayload mb)
  Delegation.checkPayload pm (_mbDlgPayload mb)
  Update.checkPayload pm (_mbUpdatePayload mb)


--------------------------------------------------------------------------------
-- MainBody lenses
--------------------------------------------------------------------------------

makeLenses 'MainBody

-- | Lens for transaction tree in main block body
mbTxs :: Lens' MainBody [Tx]
mbTxs = mbTxPayload . txpTxs

-- | Lens for witness list in main block body
mbWitnesses :: Lens' MainBody [TxWitness]
mbWitnesses = mbTxPayload . txpWitnesses


--------------------------------------------------------------------------------
-- MainExtra lenses
--------------------------------------------------------------------------------

makeLenses ''MainExtraHeaderData
makeLenses ''MainExtraBodyData
