{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE UndecidableInstances #-}

module Cardano.Ledger.Babel.Genesis (
  BabelGenesis (..),
  toBabelGenesisPairs,
  cgDelegsL,
)
where

import Cardano.Ledger.Babel.Era (BabelEra)
import Cardano.Ledger.Babel.TxCert (Delegatee)
import Cardano.Ledger.Binary (
  DecCBOR (..),
  EncCBOR (..),
 )
import Cardano.Ledger.Binary.Coders
import Cardano.Ledger.Conway.Governance
import Cardano.Ledger.Conway.PParams (UpgradeConwayPParams, toUpgradeConwayPParamsUpdatePairs)
import Cardano.Ledger.Credential (Credential)
import Cardano.Ledger.Crypto (Crypto)
import Cardano.Ledger.DRep (DRepState)
import Cardano.Ledger.Keys (KeyRole (..))
import Data.Aeson (
  FromJSON (..),
  KeyValue (..),
  ToJSON (..),
  Value (..),
  object,
  pairs,
  withObject,
  (.!=),
  (.:),
  (.:?),
 )
import Data.Functor.Identity (Identity)
import Data.ListMap (ListMap)
import GHC.Generics (Generic)
import Lens.Micro (Lens', lens)
import NoThunks.Class (NoThunks)

data BabelGenesis c = BabelGenesis
  { cgUpgradePParams :: !(UpgradeConwayPParams Identity)
  , cgConstitution :: !(Constitution (BabelEra c))
  , cgCommittee :: !(Committee (BabelEra c))
  , cgDelegs :: ListMap (Credential 'Staking c) (Delegatee c)
  , cgInitialDReps :: ListMap (Credential 'DRepRole c) (DRepState c)
  }
  deriving (Eq, Generic, Show)

cgDelegsL :: Lens' (BabelGenesis c) (ListMap (Credential 'Staking c) (Delegatee c))
cgDelegsL = lens cgDelegs (\x y -> x {cgDelegs = y})

instance Crypto c => NoThunks (BabelGenesis c)

-- | Genesis are always encoded with the version of era they are defined in.
instance Crypto c => DecCBOR (BabelGenesis c) where
  decCBOR = decode $ RecD BabelGenesis <! From <! From <! From <! From <! From

instance Crypto c => EncCBOR (BabelGenesis c) where
  encCBOR (BabelGenesis pparams constitution committee delegs initialDReps) =
    encode $
      Rec (BabelGenesis @c)
        !> To pparams
        !> To constitution
        !> To committee
        !> To delegs
        !> To initialDReps

instance Crypto c => ToJSON (BabelGenesis c) where
  toJSON = object . toBabelGenesisPairs
  toEncoding = pairs . mconcat . toBabelGenesisPairs

instance Crypto c => FromJSON (BabelGenesis c) where
  parseJSON =
    withObject "BabelGenesis" $ \obj -> do
      upgradeProtocolPParams <- parseJSON (Object obj)
      BabelGenesis
        <$> pure upgradeProtocolPParams
        <*> obj .: "constitution"
        <*> obj .: "committee"
        <*> obj .:? "delegs" .!= mempty
        <*> obj .:? "initialDReps" .!= mempty

toBabelGenesisPairs :: (Crypto c, KeyValue e a) => BabelGenesis c -> [a]
toBabelGenesisPairs cg@(BabelGenesis _ _ _ _ _) =
  let BabelGenesis {..} = cg
   in [ "constitution" .= cgConstitution
      , "committee" .= cgCommittee
      ]
        ++ ["delegs" .= cgDelegs | not (null cgDelegs)]
        ++ ["initialDReps" .= cgInitialDReps | not (null cgInitialDReps)]
        ++ toUpgradeConwayPParamsUpdatePairs cgUpgradePParams
