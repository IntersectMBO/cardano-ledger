{-# LANGUAGE DeriveAnyClass     #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE OverloadedStrings  #-}

-- | Data required to rollback a 'Delegation.Payload'

module Cardano.Chain.Delegation.Undo
  ( Undo(..)
  , isRevokePsk
  )
where

import Cardano.Prelude

import Formatting (bprint)
import Formatting.Buildable (Buildable(..))

import Cardano.Binary.Class (Bi(..), encodeListLen, enforceSize)
import Cardano.Chain.Common (StakeholderId)
import Cardano.Chain.Delegation.Certificate (Certificate)
import Cardano.Crypto (ProxySecretKey, isSelfSignedPsk)


-- | Undo for the delegation component
data Undo = Undo
  { duPsks            :: ![Certificate]
    -- ^ PSKs we've modified when applying the block (by deleting or
    --   overwriting). There should be no duplicates, every psk must have a
    --   unique issuer.
  , duPrevEpochPosted :: !(Set StakeholderId)
    -- ^ Set of stakeholders that posted in epoch i. This field should be
    --   present only for genesis block of epoch i+1.
  } deriving (Eq, Show, Generic)
    deriving anyclass NFData

instance Buildable Undo where
  build undo = bprint
    ( "DlgUndo:\n"
    . "  duPsks: " . listJson . "\n"
    . "  duPrevEpochPosted: " . listJson
    )
    (duPsks undo)
    (duPrevEpochPosted undo)

instance Bi Undo where
  encode undo =
    encodeListLen 2 <> encode (duPsks undo) <> encode (duPrevEpochPosted undo)

  decode = do
    enforceSize "Delegation.Undo" 2
    Undo <$> decode <*> decode

-- | Checks if given PSK revokes delegation (issuer == delegate)
isRevokePsk :: ProxySecretKey w -> Bool
isRevokePsk = isSelfSignedPsk
