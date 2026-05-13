{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
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
  TxIx (..),
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
import qualified Data.Map as Map
import Data.Maybe.Strict (StrictMaybe (..))
import GHC.Natural (naturalToInteger)
import qualified MAlonzo.Code.Ledger.Core.Foreign.API as Agda
import Test.Cardano.Ledger.Conformance.SpecTranslate.Base
import Test.Cardano.Ledger.Conformance.Utils

instance SpecTranslate era TxIx where
  type SpecRep era TxIx = Integer

  toSpecRep (TxIx x) = pure $ toInteger x

instance SpecTranslate era StakeReference where
  type SpecRep era StakeReference = Maybe Agda.Credential

  toSpecRep (StakeRefBase c) = Just <$> toSpecRep c
  toSpecRep (StakeRefPtr _) = pure Nothing
  toSpecRep StakeRefNull = pure Nothing

instance SpecTranslate era BootstrapAddress where
  type SpecRep era BootstrapAddress = Agda.BootstrapAddr

  toSpecRep _ = throwError "Cannot translate bootstrap addresses"

instance SpecTranslate era Addr where
  type SpecRep era Addr = Agda.Addr

  toSpecRep (Addr nw pc sr) =
    Left
      <$> (Agda.BaseAddr <$> toSpecRep nw <*> toSpecRep pc <*> toSpecRep sr)
  toSpecRep (AddrBootstrap ba) = Right <$> toSpecRep ba

instance SpecTranslate era (Hash a b) where
  type SpecRep era (Hash a b) = Integer

  toSpecRep = pure . hashToInteger

instance SpecTranslate era ScriptHash where
  type SpecRep era ScriptHash = Integer

  toSpecRep (ScriptHash h) = toSpecRep h

instance SpecTranslate era (KeyHash r) where
  type SpecRep era (KeyHash r) = Integer

  toSpecRep (KeyHash h) = toSpecRep h

instance SpecTranslate era (Credential k) where
  type SpecRep era (Credential k) = Agda.Credential

  toSpecRep (KeyHashObj h) = Agda.KeyHashObj <$> toSpecRep h
  toSpecRep (ScriptHashObj h) = Agda.ScriptObj <$> toSpecRep h

instance SpecTranslate era Network where
  type SpecRep era Network = Integer

  toSpecRep = pure . fromIntegral . fromEnum

deriving instance SpecTranslate era Coin

deriving instance SpecTranslate era SlotNo

deriving instance SpecTranslate era EpochNo

deriving instance SpecTranslate era EpochInterval

instance SpecTranslate era ProtVer where
  type SpecRep era ProtVer = (Integer, Integer)

  toSpecRep (ProtVer ver minor) = pure (getVersion ver, toInteger minor)

instance SpecTranslate era Prices where
  type SpecRep era Prices = ()

  toSpecRep _ = pure ()

instance SpecTranslate era AccountAddress where
  type SpecRep era AccountAddress = Agda.RewardAddress

  toSpecRep (AccountAddress n (AccountId c)) = Agda.RewardAddress <$> toSpecRep n <*> toSpecRep c

instance
  SpecTranslate era (PParamsHKD Identity era) =>
  SpecTranslate era (PParams era)
  where
  type SpecRep era (PParams era) = SpecRep era (PParamsHKD Identity era)
  type SpecContext era (PParams era) = SpecContext era (PParamsHKD Identity era)

  toSpecRep (PParams x) = toSpecRep x

instance
  SpecTranslate era (PParamsHKD StrictMaybe era) =>
  SpecTranslate era (PParamsUpdate era)
  where
  type SpecRep era (PParamsUpdate era) = SpecRep era (PParamsHKD StrictMaybe era)
  type SpecContext era (PParamsUpdate era) = SpecContext era (PParamsHKD StrictMaybe era)

  toSpecRep (PParamsUpdate ppu) = toSpecRep ppu

vkeyToInteger :: VKey kd -> Integer
vkeyToInteger = toInteger . bytesToNatural . rawSerialiseVerKeyDSIGN . unVKey

vkeyFromInteger :: Integer -> Maybe (VKey kd)
vkeyFromInteger = fmap VKey . rawDeserialiseVerKeyDSIGN . naturalToBytes 32 . fromInteger

signatureToInteger :: DSIGNAlgorithm v => SigDSIGN v -> Integer
signatureToInteger = toInteger . bytesToNatural . rawSerialiseSigDSIGN

signatureFromInteger :: DSIGNAlgorithm v => Integer -> Maybe (SigDSIGN v)
signatureFromInteger = rawDeserialiseSigDSIGN . naturalToBytes 64 . fromInteger

instance SpecTranslate era (VKey k) where
  type SpecRep era (VKey k) = Agda.HSVKey

  toSpecRep x = do
    let hvkVKey = vkeyToInteger x
    hvkStoredHash <- toSpecRep (hashVerKeyDSIGN @_ @ADDRHASH $ unVKey x)
    pure Agda.MkHSVKey {..}

instance DSIGNAlgorithm v => SpecTranslate era (SignedDSIGN v a) where
  type SpecRep era (SignedDSIGN v a) = Integer

  toSpecRep (SignedDSIGN x) = pure $ signatureToInteger x

instance SpecTranslate era (WitVKey k) where
  type SpecRep era (WitVKey k) = (SpecRep era (VKey k), Integer)

  toSpecRep (WitVKey vk sk) = toSpecRepTuple (vk, sk)

instance SpecTranslate era CommitteeAuthorization where
  type
    SpecRep era CommitteeAuthorization =
      SpecRep era (Maybe (Credential HotCommitteeRole))

  toSpecRep (CommitteeHotCredential c) = toSpecRep $ Just c
  toSpecRep (CommitteeMemberResigned _) =
    toSpecRep $
      Nothing @(Credential HotCommitteeRole)

instance SpecTranslate era (CommitteeState era) where
  type
    SpecRep era (CommitteeState era) =
      Agda.HSMap (SpecRep era (Credential ColdCommitteeRole)) (SpecRep era CommitteeAuthorization)

  toSpecRep = toSpecRepMap . csCommitteeCreds

committeeCredentialToStrictMaybe ::
  CommitteeAuthorization ->
  StrictMaybe (Credential HotCommitteeRole)
committeeCredentialToStrictMaybe (CommitteeHotCredential c) = SJust c
committeeCredentialToStrictMaybe (CommitteeMemberResigned _) = SNothing

instance SpecTranslate era IndividualPoolStake where
  type SpecRep era IndividualPoolStake = SpecRep era Coin

  toSpecRep (IndividualPoolStake _ c _) = toSpecRep c

instance SpecTranslate era PoolDistr where
  type SpecRep era PoolDistr = Agda.HSMap (SpecRep era (KeyHash StakePool)) Agda.Coin

  toSpecRep (PoolDistr ps _) = toSpecRepMap ps

instance SpecTranslate era BlocksMade where
  type SpecRep era BlocksMade = Agda.HSMap Integer Integer

  toSpecRep (BlocksMade m) = do
    xs <- forM (Map.toList m) $ \(k, v) -> do
      k' <- toSpecRep k
      pure (k', naturalToInteger v)
    pure $ Agda.MkHSMap xs

instance SpecNormalize Agda.BaseAddr

instance SpecNormalize Agda.BootstrapAddr

instance SpecNormalize Agda.Credential
