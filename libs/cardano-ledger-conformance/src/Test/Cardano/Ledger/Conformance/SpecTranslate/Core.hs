{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Test.Cardano.Ledger.Conformance.SpecTranslate.Core (
  committeeCredentialToStrictMaybe,
  vkeyToInteger,
  vkeyFromInteger,
  signatureToInteger,
  signatureFromInteger,
) where

import Cardano.Crypto.DSIGN (DSIGNAlgorithm (..), SignedDSIGN (..))
import Cardano.Crypto.Util (bytesToNatural, naturalToBytes)
import Cardano.Ledger.Address (
  AccountAddress (..),
  AccountId (..),
  Addr (..),
  BootstrapAddress (..),
 )
import Cardano.Ledger.BaseTypes (
  BlocksMade (..),
  EpochInterval (..),
  EpochNo (..),
  Network (..),
  ProtVer (..),
  SlotNo (..),
  getVersion,
 )
import Cardano.Ledger.Coin (Coin (..))
import Cardano.Ledger.Conway.Core (
  ADDRHASH,
  Hash,
  KeyHash (..),
  KeyRole (..),
  PParams (..),
  PParamsHKD,
  PParamsUpdate (..),
  ScriptHash (..),
 )
import Cardano.Ledger.Conway.State
import Cardano.Ledger.Credential (Credential (..), StakeReference (..))
import Cardano.Ledger.Keys (VKey (..))
import Cardano.Ledger.Keys.WitVKey (WitVKey (..))
import Cardano.Ledger.Plutus.ExUnits (Prices)
import Control.Monad (forM)
import Control.Monad.Except (throwError)
import Data.Functor.Identity (Identity (..))
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe.Strict (StrictMaybe (..))
import GHC.Natural (naturalToInteger)
import qualified MAlonzo.Code.Ledger.Foreign.API as Agda
import Test.Cardano.Ledger.Conformance.SpecTranslate.Base (
  SpecTranslate (..),
 )
import Test.Cardano.Ledger.Conformance.Utils

instance SpecTranslate ctx era StakeReference where
  type SpecRep era StakeReference = Maybe Agda.Credential

  toSpecRep (StakeRefBase c) = Just <$> toSpecRep @ctx @era c
  toSpecRep (StakeRefPtr _) = pure Nothing
  toSpecRep StakeRefNull = pure Nothing

instance SpecTranslate ctx era BootstrapAddress where
  type SpecRep era BootstrapAddress = Agda.BootstrapAddr

  toSpecRep _ = throwError "Cannot translate bootstrap addresses"

instance SpecTranslate ctx era Addr where
  type SpecRep era Addr = Agda.Addr

  toSpecRep (Addr nw pc sr) =
    Left
      <$> (Agda.BaseAddr <$> toSpecRep @ctx @era nw <*> toSpecRep @ctx @era pc <*> toSpecRep @ctx @era sr)
  toSpecRep (AddrBootstrap ba) = Right <$> toSpecRep @ctx @era ba

instance SpecTranslate ctx era (Hash a b) where
  type SpecRep era (Hash a b) = Integer

  toSpecRep = pure . hashToInteger

instance SpecTranslate ctx era ScriptHash where
  type SpecRep era ScriptHash = Integer

  toSpecRep (ScriptHash h) = toSpecRep @ctx @era h

instance SpecTranslate ctx era (KeyHash r) where
  type SpecRep era (KeyHash r) = Integer

  toSpecRep (KeyHash h) = toSpecRep @ctx @era h

instance SpecTranslate ctx era (Credential k) where
  type SpecRep era (Credential k) = Agda.Credential

  toSpecRep (KeyHashObj h) = Agda.KeyHashObj <$> toSpecRep @ctx @era h
  toSpecRep (ScriptHashObj h) = Agda.ScriptObj <$> toSpecRep @ctx @era h

instance SpecTranslate ctx era Network where
  type SpecRep era Network = Integer

  toSpecRep = pure . fromIntegral . fromEnum

deriving instance SpecTranslate ctx era Coin

deriving instance SpecTranslate ctx era SlotNo

deriving instance SpecTranslate ctx era EpochNo

deriving instance SpecTranslate ctx era EpochInterval

instance SpecTranslate ctx era ProtVer where
  type SpecRep era ProtVer = (Integer, Integer)

  toSpecRep (ProtVer ver minor) = pure (getVersion ver, toInteger minor)

instance SpecTranslate ctx era Prices where
  type SpecRep era Prices = ()

  toSpecRep _ = pure ()

instance SpecTranslate ctx era AccountAddress where
  type SpecRep era AccountAddress = Agda.RewardAddress

  toSpecRep (AccountAddress n (AccountId c)) = Agda.RewardAddress <$> toSpecRep @ctx @era n <*> toSpecRep @ctx @era c

instance
  SpecTranslate ctx era (PParamsHKD Identity era') =>
  SpecTranslate ctx era (PParams era')
  where
  type SpecRep era (PParams era') = SpecRep era (PParamsHKD Identity era')

  toSpecRep (PParams x) = toSpecRep @ctx @era x

instance
  SpecTranslate ctx era (PParamsHKD StrictMaybe era') =>
  SpecTranslate ctx era (PParamsUpdate era')
  where
  type SpecRep era (PParamsUpdate era') = SpecRep era (PParamsHKD StrictMaybe era')

  toSpecRep (PParamsUpdate ppu) = toSpecRep @ctx @era ppu

vkeyToInteger :: VKey kd -> Integer
vkeyToInteger = toInteger . bytesToNatural . rawSerialiseVerKeyDSIGN . unVKey

vkeyFromInteger :: Integer -> Maybe (VKey kd)
vkeyFromInteger = fmap VKey . rawDeserialiseVerKeyDSIGN . naturalToBytes 32 . fromInteger

signatureToInteger :: DSIGNAlgorithm v => SigDSIGN v -> Integer
signatureToInteger = toInteger . bytesToNatural . rawSerialiseSigDSIGN

signatureFromInteger :: DSIGNAlgorithm v => Integer -> Maybe (SigDSIGN v)
signatureFromInteger = rawDeserialiseSigDSIGN . naturalToBytes 64 . fromInteger

instance SpecTranslate ctx era (VKey k) where
  type SpecRep era (VKey k) = Agda.HSVKey

  toSpecRep x = do
    let hvkVKey = vkeyToInteger x
    hvkStoredHash <- toSpecRep @ctx @era (hashVerKeyDSIGN @_ @ADDRHASH $ unVKey x)
    pure Agda.MkHSVKey {..}

instance DSIGNAlgorithm v => SpecTranslate ctx era (SignedDSIGN v a) where
  type SpecRep era (SignedDSIGN v a) = Integer

  toSpecRep (SignedDSIGN x) = pure $ signatureToInteger x

instance SpecTranslate ctx era (WitVKey k) where
  type SpecRep era (WitVKey k) = (SpecRep era (VKey k), Integer)

  toSpecRep (WitVKey vk sk) = toSpecRep @ctx @era (vk, sk)

instance SpecTranslate ctx era CommitteeAuthorization where
  type
    SpecRep era CommitteeAuthorization =
      SpecRep era (Maybe (Credential HotCommitteeRole))

  toSpecRep (CommitteeHotCredential c) = toSpecRep @ctx @era $ Just c
  toSpecRep (CommitteeMemberResigned _) =
    toSpecRep @ctx @era $
      Nothing @(Credential HotCommitteeRole)

instance SpecTranslate ctx era (CommitteeState era') where
  type
    SpecRep era (CommitteeState era') =
      SpecRep era (Map (Credential ColdCommitteeRole) CommitteeAuthorization)

  toSpecRep = toSpecRep @ctx @era . csCommitteeCreds

committeeCredentialToStrictMaybe ::
  CommitteeAuthorization ->
  StrictMaybe (Credential HotCommitteeRole)
committeeCredentialToStrictMaybe (CommitteeHotCredential c) = SJust c
committeeCredentialToStrictMaybe (CommitteeMemberResigned _) = SNothing

instance SpecTranslate ctx era IndividualPoolStake where
  type SpecRep era IndividualPoolStake = SpecRep era Coin

  toSpecRep (IndividualPoolStake _ c _) = toSpecRep @ctx @era c

instance SpecTranslate ctx era PoolDistr where
  type SpecRep era PoolDistr = Agda.HSMap (SpecRep era (KeyHash StakePool)) Agda.Coin

  toSpecRep (PoolDistr ps _) = toSpecRep @ctx @era ps

instance SpecTranslate ctx era BlocksMade where
  type SpecRep era BlocksMade = Agda.HSMap Integer Integer

  toSpecRep (BlocksMade m) = do
    xs <- forM (Map.toList m) $ \(k, v) -> do
      k' <- toSpecRep @ctx @era k
      pure (k', naturalToInteger v)
    pure $ Agda.MkHSMap xs
