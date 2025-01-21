{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ViewPatterns #-}

module Cardano.Ledger.DRep (
  DRep (DRepCredential, DRepKeyHash, DRepScriptHash, DRepAlwaysAbstain, DRepAlwaysNoConfidence),
  DRepState (..),
  drepExpiryL,
  drepAnchorL,
  drepDepositL,
  drepDelegsL,
) where

import Cardano.Ledger.BaseTypes
import Cardano.Ledger.Binary (
  DecCBOR (..),
  DecShareCBOR (..),
  EncCBOR (..),
  Interns,
  decNoShareCBOR,
  interns,
  internsFromSet,
 )
import Cardano.Ledger.Binary.Coders (Decode (..), Encode (..), decode, encode, (!>), (<!))
import Cardano.Ledger.Coin (Coin)
import Cardano.Ledger.Credential (Credential (..), credToText, parseCredential)
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

data DRep
  = DRepKeyHash !(KeyHash 'DRepRole)
  | DRepScriptHash !ScriptHash
  | DRepAlwaysAbstain
  | DRepAlwaysNoConfidence
  deriving (Show, Eq, Ord, Generic, NoThunks, NFData)

instance EncCBOR DRep where
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

instance DecCBOR DRep where
  decCBOR = decode $
    Summands "DRep" $ \case
      0 -> SumD DRepKeyHash <! From
      1 -> SumD DRepScriptHash <! From
      2 -> SumD DRepAlwaysAbstain
      3 -> SumD DRepAlwaysNoConfidence
      k -> Invalid k

instance DecShareCBOR DRep where
  type Share DRep = Interns (Credential 'DRepRole)
  decShareCBOR cd = do
    dRep <- decCBOR
    pure $!
      case dRepToCred dRep of
        Nothing -> dRep
        Just cred -> credToDRep $ interns cd cred

dRepToCred :: DRep -> Maybe (Credential 'DRepRole)
dRepToCred (DRepKeyHash kh) = Just $ KeyHashObj kh
dRepToCred (DRepScriptHash sh) = Just $ ScriptHashObj sh
dRepToCred _ = Nothing

credToDRep :: Credential 'DRepRole -> DRep
credToDRep (KeyHashObj kh) = DRepKeyHash kh
credToDRep (ScriptHashObj sh) = DRepScriptHash sh

instance ToJSON DRep where
  toJSON = String . dRepToText

instance ToJSONKey DRep where
  toJSONKey = toJSONKeyText dRepToText

dRepToText :: DRep -> T.Text
dRepToText = \case
  DRepAlwaysAbstain -> "drep-alwaysAbstain"
  DRepAlwaysNoConfidence -> "drep-alwaysNoConfidence"
  DRepCredential cred -> "drep-" <> credToText cred

instance FromJSON DRep where
  parseJSON = withText "DRep" parseDRep

instance FromJSONKey DRep where
  fromJSONKey = FromJSONKeyTextParser parseDRep

parseDRep :: MonadFail f => T.Text -> f DRep
parseDRep t = case T.span (/= '-') t of
  ("drep", restWithDash)
    | restWithDash == "-alwaysAbstain" -> pure DRepAlwaysAbstain
    | restWithDash == "-alwaysNoConfidence" -> pure DRepAlwaysNoConfidence
    | ("-", rest) <- T.span (== '-') restWithDash ->
        DRepCredential <$> parseCredential rest
  _ -> fail $ "Invalid DRep: " <> show t

pattern DRepCredential :: Credential 'DRepRole -> DRep
pattern DRepCredential c <- (dRepToCred -> Just c)
  where
    DRepCredential c = case c of
      ScriptHashObj sh -> DRepScriptHash sh
      KeyHashObj kh -> DRepKeyHash kh

{-# COMPLETE DRepCredential, DRepAlwaysAbstain, DRepAlwaysNoConfidence :: DRep #-}

data DRepState = DRepState
  { drepExpiry :: !EpochNo
  , drepAnchor :: !(StrictMaybe Anchor)
  , drepDeposit :: !Coin
  , drepDelegs :: !(Set (Credential 'Staking))
  }
  deriving (Show, Eq, Ord, Generic)

instance NoThunks DRepState

instance NFData DRepState

instance DecCBOR DRepState where
  decCBOR = decNoShareCBOR

instance DecShareCBOR DRepState where
  type Share DRepState = Interns (Credential 'Staking)
  getShare = internsFromSet . drepDelegs
  decShareCBOR is = do
    decode $
      RecD DRepState
        <! From
        <! From
        <! From
        <! D (decShareCBOR is)

instance EncCBOR DRepState where
  encCBOR DRepState {..} =
    encode $
      Rec DRepState
        !> To drepExpiry
        !> To drepAnchor
        !> To drepDeposit
        !> To drepDelegs

instance ToJSON DRepState where
  toJSON x@(DRepState _ _ _ _) =
    let DRepState {..} = x
     in toJSON $
          object $
            [ "expiry" .= toJSON drepExpiry
            , "deposit" .= toJSON drepDeposit
            , "delegators" .= toJSON drepDelegs
            ]
              ++ ["anchor" .= toJSON anchor | SJust anchor <- [drepAnchor]]

instance FromJSON DRepState where
  parseJSON = withObject "DRepState" $ \o ->
    DRepState
      <$> o .: "expiry"
      <*> o .:? "anchor" .!= SNothing
      <*> o .: "deposit"
      -- Construction of DRep state with deleagations is intentionally prohibited, since
      -- there is a requirement to retain the invariant of delegations in the UMap
      <*> pure mempty

drepExpiryL :: Lens' DRepState EpochNo
drepExpiryL = lens drepExpiry (\x y -> x {drepExpiry = y})

drepAnchorL :: Lens' DRepState (StrictMaybe Anchor)
drepAnchorL = lens drepAnchor (\x y -> x {drepAnchor = y})

drepDepositL :: Lens' DRepState Coin
drepDepositL = lens drepDeposit (\x y -> x {drepDeposit = y})

drepDelegsL :: Lens' DRepState (Set (Credential 'Staking))
drepDelegsL = lens drepDelegs (\x y -> x {drepDelegs = y})
