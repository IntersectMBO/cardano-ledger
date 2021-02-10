{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE StandaloneDeriving #-}

-- | This module provides data structures and operations for talking about
--     Non-native Script languages. It is expected that new languages (or new
--     versions of old languages) will be added here.
module Cardano.Ledger.Alonzo.Language where

import Cardano.Binary (FromCBOR (..), ToCBOR (..), decodeWord64)
import Cardano.Ledger.Pretty (PDoc, PrettyA (..), ppString)
import Control.DeepSeq (NFData (..))
import Data.Coders
import qualified Data.Set as Set
import GHC.Generics (Generic)
import NoThunks.Class (NoThunks)

-- ==================================================================
-- Non-Native Script language. This is an Enumerated type.
-- This is expected to be an open type. We will add new Constuctors
-- to this type as additional Non-Native scripting language as are added.
-- We use an enumerated type for two reasons.
-- 1) We can write total functions by case analysis over the constructors
-- 2) We will use DataKinds to make some datatypes  indexed by Language
-- For now, the only Non-Native Scriting language is Plutus
-- We might add new languages in the futures.

data Language = PlutusV1 --    | ADD-NEW-LANGUAGES-HERE
  deriving (Eq, Generic, Show, Ord)

instance NoThunks Language

instance NFData Language

instance ToCBOR Language where
  toCBOR PlutusV1 = toCBOR (0 :: Int)

instance FromCBOR Language where
  fromCBOR = do n <- decodeWord64; case n of { 0 -> pure PlutusV1; m -> invalidKey (fromIntegral m) }

nonNativeLanguages :: Set.Set Language
nonNativeLanguages = Set.insert PlutusV1 Set.empty

-- ==================================

ppLanguage :: Language -> PDoc
ppLanguage PlutusV1 = ppString "PlutusV1"

instance PrettyA Language where prettyA = ppLanguage
