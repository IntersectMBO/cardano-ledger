{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ViewPatterns #-}

module Cardano.Ledger.DRep (
  DRep (DRepCredential, DRepKeyHash, DRepScriptHash, DRepAlwaysAbstain, DRepAlwaysNoConfidence),
  DRepState (..),
  drepExpiryL,
  drepAnchorL,
  drepDepositL,
) where

import Cardano.Ledger.BaseTypes
import Cardano.Ledger.Binary (DecCBOR (..), EncCBOR (..))
import Cardano.Ledger.Binary.Coders (Decode (..), Encode (..), decode, encode, (!>), (<!))
import Cardano.Ledger.Coin (Coin)
import Cardano.Ledger.Credential (Credential (..), credToText, parseCredential)
import Cardano.Ledger.Crypto (Crypto)
import Cardano.Ledger.Hashes (ScriptHash)
import Cardano.Ledger.Keys (KeyHash (..), KeyRole (..))
import Control.DeepSeq (NFData (..))
import Data.Aeson (
  FromJSON (..),
  FromJSONKey (..),
  FromJSONKeyFunction (..),
  KeyValue (..),
  ToJSON (..),
  ToJSONKey (..),
  Value (..),
  object,
  withObject,
  withText,
  (.!=),
  (.:),
  (.:?),
 )
import Data.Aeson.Types (toJSONKeyText)
import Data.Set (Set)
import qualified Data.Text as T
import GHC.Generics (Generic)
import Lens.Micro
import NoThunks.Class (NoThunks (..))

-- =======================================
-- DRep and DRepState

data DRep c
  = DRepKeyHash !(KeyHash 'DRepRole c)
  | DRepScriptHash !(ScriptHash c)
  | DRepAlwaysAbstain
  | DRepAlwaysNoConfidence
  deriving (Show, Eq, Ord, Generic, NoThunks, NFData)

instance Crypto c => EncCBOR (DRep c) where
  encCBOR (DRepKeyHash kh) =
    encode $
      Sum DRepKeyHash 0
        !> To kh
  encCBOR (DRepScriptHash sh) =
    encode $
      Sum DRepScriptHash 1
        !> To sh
  encCBOR DRepAlwaysAbstain =
    encode $
      Sum DRepAlwaysAbstain 2
  encCBOR DRepAlwaysNoConfidence =
    encode $
      Sum DRepAlwaysNoConfidence 3

instance Crypto c => DecCBOR (DRep c) where
  decCBOR = decode $
    Summands "DRep" $ \case
      0 -> SumD DRepKeyHash <! From
      1 -> SumD DRepScriptHash <! From
      2 -> SumD DRepAlwaysAbstain
      3 -> SumD DRepAlwaysNoConfidence
      k -> Invalid k

dRepToCred :: DRep c -> Maybe (Credential 'DRepRole c)
dRepToCred (DRepKeyHash kh) = Just $ KeyHashObj kh
dRepToCred (DRepScriptHash sh) = Just $ ScriptHashObj sh
dRepToCred _ = Nothing

instance Crypto c => ToJSON (DRep c) where
  toJSON = String . dRepToText

instance Crypto c => ToJSONKey (DRep c) where
  toJSONKey = toJSONKeyText dRepToText

dRepToText :: DRep c -> T.Text
dRepToText = \case
  DRepAlwaysAbstain -> "drep-alwaysAbstain"
  DRepAlwaysNoConfidence -> "drep-alwaysNoConfidence"
  DRepCredential cred -> "drep-" <> credToText cred

instance Crypto c => FromJSON (DRep c) where
  parseJSON = withText "DRep" parseDRep

instance Crypto c => FromJSONKey (DRep c) where
  fromJSONKey = FromJSONKeyTextParser parseDRep

parseDRep :: (MonadFail f, Crypto c) => T.Text -> f (DRep c)
parseDRep t = case T.span (/= '-') t of
  ("drep", restWithDash)
    | restWithDash == "-alwaysAbstain" -> pure DRepAlwaysAbstain
    | restWithDash == "-alwaysNoConfidence" -> pure DRepAlwaysNoConfidence
    | ("-", rest) <- T.span (== '-') restWithDash ->
        DRepCredential <$> parseCredential rest
  _ -> fail $ "Invalid DRep: " <> show t

pattern DRepCredential :: Credential 'DRepRole c -> DRep c
pattern DRepCredential c <- (dRepToCred -> Just c)
  where
    DRepCredential c = case c of
      ScriptHashObj sh -> DRepScriptHash sh
      KeyHashObj kh -> DRepKeyHash kh

{-# COMPLETE DRepCredential, DRepAlwaysAbstain, DRepAlwaysNoConfidence :: DRep #-}

data DRepState c = DRepState
  { drepExpiry :: !EpochNo
  , drepAnchor :: !(StrictMaybe (Anchor c))
  , drepDeposit :: !Coin
  , drepDelegs :: !(Set (Credential 'Staking c))
  }
  deriving (Show, Eq, Ord, Generic)

instance NoThunks (DRepState era)

instance Crypto c => NFData (DRepState c)

instance Crypto c => DecCBOR (DRepState c) where
  decCBOR = do
    decode $
      RecD DRepState
        <! From
        <! From
        <! From
        <! From

instance Crypto c => EncCBOR (DRepState c) where
  encCBOR DRepState {..} =
    encode $
      Rec DRepState
        !> To drepExpiry
        !> To drepAnchor
        !> To drepDeposit
        !> To drepDelegs

instance Crypto c => ToJSON (DRepState c) where
  toJSON x@(DRepState _ _ _ _) =
    let DRepState {..} = x
     in toJSON $
          object $
            [ "expiry" .= toJSON drepExpiry
            , "deposit" .= toJSON drepDeposit
            , "delegators" .= toJSON drepDelegs
            ]
              ++ ["anchor" .= toJSON anchor | SJust anchor <- [drepAnchor]]

instance Crypto c => FromJSON (DRepState c) where
  parseJSON = withObject "DRepState" $ \o ->
    DRepState
      <$> o .: "expiry"
      <*> o .:? "anchor" .!= SNothing
      <*> o .: "deposit"
      -- Construction of DRep state with deleagations is intentionally prohibited, since
      -- there is a requirement to retain the invariant of delegations in the UMap
      <*> pure mempty

drepExpiryL :: Lens' (DRepState c) EpochNo
drepExpiryL = lens drepExpiry (\x y -> x {drepExpiry = y})

drepAnchorL :: Lens' (DRepState c) (StrictMaybe (Anchor c))
drepAnchorL = lens drepAnchor (\x y -> x {drepAnchor = y})

drepDepositL :: Lens' (DRepState c) Coin
drepDepositL = lens drepDeposit (\x y -> x {drepDeposit = y})
