{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Test.Cardano.Ledger.Conformance.SpecTranslate.Core () where

import Cardano.Ledger.Address (Addr (..), BootstrapAddress (..), RewardAccount (..))
import Cardano.Ledger.BaseTypes (
  EpochInterval (..),
  EpochNo (..),
  Network (..),
  ProtVer (..),
  SlotNo (..),
  getVersion,
 )
import Cardano.Ledger.Coin (Coin (..))
import Cardano.Ledger.Conway.Core (Hash, KeyHash (..), ScriptHash (..))
import Cardano.Ledger.Credential (Credential (..), StakeReference (..))
import Cardano.Ledger.Plutus.CostModels (CostModels)
import Cardano.Ledger.Plutus.ExUnits (ExUnits (..), Prices)
import Control.Monad.Except (throwError)
import qualified MAlonzo.Code.Ledger.Foreign.API as Agda
import Test.Cardano.Ledger.Conformance (
  SpecTranslate (..),
  hashToInteger,
 )

instance SpecTranslate ctx StakeReference where
  type SpecRep StakeReference = Maybe Agda.Credential

  toSpecRep (StakeRefBase c) = Just <$> toSpecRep c
  toSpecRep (StakeRefPtr _) = pure Nothing
  toSpecRep StakeRefNull = pure Nothing

instance SpecTranslate ctx BootstrapAddress where
  type SpecRep BootstrapAddress = Agda.BootstrapAddr

  toSpecRep _ = throwError "Cannot translate bootstrap addresses"

instance SpecTranslate ctx Addr where
  type SpecRep Addr = Agda.Addr

  toSpecRep (Addr nw pc sr) =
    Left
      <$> (Agda.BaseAddr <$> toSpecRep nw <*> toSpecRep pc <*> toSpecRep sr)
  toSpecRep (AddrBootstrap ba) = Right <$> toSpecRep ba

instance SpecTranslate ctx (Hash a b) where
  type SpecRep (Hash a b) = Integer

  toSpecRep = pure . hashToInteger

instance SpecTranslate ctx ScriptHash where
  type SpecRep ScriptHash = Integer

  toSpecRep (ScriptHash h) = toSpecRep h

instance SpecTranslate ctx (KeyHash r) where
  type SpecRep (KeyHash r) = Integer

  toSpecRep (KeyHash h) = toSpecRep h

instance SpecTranslate ctx (Credential k) where
  type SpecRep (Credential k) = Agda.Credential

  toSpecRep (KeyHashObj h) = Agda.KeyHashObj <$> toSpecRep h
  toSpecRep (ScriptHashObj h) = Agda.ScriptObj <$> toSpecRep h

instance SpecTranslate ctx Network where
  type SpecRep Network = Integer

  toSpecRep = pure . fromIntegral . fromEnum

deriving instance SpecTranslate ctx Coin

deriving instance SpecTranslate ctx SlotNo

deriving instance SpecTranslate ctx EpochNo

deriving instance SpecTranslate ctx EpochInterval

instance SpecTranslate ctx ProtVer where
  type SpecRep ProtVer = (Integer, Integer)

  toSpecRep (ProtVer ver minor) = pure (getVersion ver, toInteger minor)

instance SpecTranslate ctx CostModels where
  type SpecRep CostModels = ()

  toSpecRep _ = pure ()

instance SpecTranslate ctx Prices where
  type SpecRep Prices = ()

  toSpecRep _ = pure ()

instance SpecTranslate ctx ExUnits where
  type SpecRep ExUnits = Agda.ExUnits

  toSpecRep (ExUnits a b) = pure (toInteger a, toInteger b)

instance SpecTranslate ctx RewardAccount where
  type SpecRep RewardAccount = Agda.RwdAddr

  toSpecRep (RewardAccount n c) = Agda.RwdAddr <$> toSpecRep n <*> toSpecRep c
