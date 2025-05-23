{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Cardano.Ledger.Shelley.Translation (
  FromByronTranslationContext (..),
  emptyFromByronTranslationContext,
  toFromByronTranslationContext,
) where

import Cardano.Ledger.Binary (
  DecCBOR (..),
  EncCBOR (..),
  FromCBOR (..),
  ToCBOR (..),
  shelleyProtVer,
  toPlainDecoder,
  toPlainEncoding,
 )
import Cardano.Ledger.Binary.Coders (Decode (..), Encode (..), decode, encode, (!>), (<!))
import Cardano.Ledger.Core (PParams, TranslationContext, emptyPParams)
import Cardano.Ledger.Keys
import Cardano.Ledger.Shelley.Era (ShelleyEra)
import Cardano.Ledger.Shelley.Genesis (ShelleyGenesis (..))
import Data.Map (Map)
import qualified Data.Map as Map
import GHC.Generics (Generic)
import GHC.Word (Word64)
import NoThunks.Class (NoThunks (..))

-- | Required data to translate a Byron ledger into a Shelley ledger.
data FromByronTranslationContext = FromByronTranslationContext
  { fbtcGenDelegs :: !(Map (KeyHash 'Genesis) GenDelegPair)
  , fbtcProtocolParams :: !(PParams ShelleyEra)
  , fbtcMaxLovelaceSupply :: !Word64
  }
  deriving (Eq, Show, Generic)

instance ToCBOR FromByronTranslationContext where
  toCBOR x@(FromByronTranslationContext _ _ _) =
    let FromByronTranslationContext {..} = x
     in toPlainEncoding shelleyProtVer $
          encode $
            Rec FromByronTranslationContext
              !> To fbtcGenDelegs
              !> To fbtcProtocolParams
              !> To fbtcMaxLovelaceSupply

instance FromCBOR FromByronTranslationContext where
  fromCBOR =
    toPlainDecoder Nothing shelleyProtVer $
      decode $
        RecD FromByronTranslationContext
          <! From
          <! From
          <! From

instance EncCBOR FromByronTranslationContext

instance DecCBOR FromByronTranslationContext

-- | Trivial FromByronTranslationContext value, for use in cases where we do not need
-- to translate from Byron to Shelley.
emptyFromByronTranslationContext :: FromByronTranslationContext
emptyFromByronTranslationContext =
  FromByronTranslationContext
    { fbtcGenDelegs = Map.empty
    , fbtcMaxLovelaceSupply = 0
    , fbtcProtocolParams = emptyPParams
    }

toFromByronTranslationContext ::
  ShelleyGenesis ->
  FromByronTranslationContext
toFromByronTranslationContext ShelleyGenesis {sgGenDelegs, sgMaxLovelaceSupply, sgProtocolParams} =
  FromByronTranslationContext
    { fbtcGenDelegs = sgGenDelegs
    , fbtcProtocolParams = sgProtocolParams
    , fbtcMaxLovelaceSupply = sgMaxLovelaceSupply
    }

instance NoThunks FromByronTranslationContext

type instance TranslationContext ShelleyEra = FromByronTranslationContext
