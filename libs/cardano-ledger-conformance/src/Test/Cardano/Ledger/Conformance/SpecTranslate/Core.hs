{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
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
import qualified MAlonzo.Code.Ledger.Foreign.API as Agda
import Test.Cardano.Ledger.Conformance.SpecTranslate.Base
import Test.Cardano.Ledger.Conformance.Utils

instance SpecTranslate TxIx where
  type SpecRep TxIx = Integer

  toSpecRep (TxIx x) = pure $ toInteger x

instance SpecTranslate StakeReference where
  type SpecRep StakeReference = Maybe Agda.Credential

  toSpecRep (StakeRefBase c) = Just <$> toSpecRep c
  toSpecRep (StakeRefPtr _) = pure Nothing
  toSpecRep StakeRefNull = pure Nothing

instance SpecTranslate BootstrapAddress where
  type SpecRep BootstrapAddress = Agda.BootstrapAddr

  toSpecRep _ = throwError "Cannot translate bootstrap addresses"

instance SpecTranslate Addr where
  type SpecRep Addr = Agda.Addr

  toSpecRep (Addr nw pc sr) =
    Left
      <$> (Agda.BaseAddr <$> toSpecRep nw <*> toSpecRep pc <*> toSpecRep sr)
  toSpecRep (AddrBootstrap ba) = Right <$> toSpecRep ba

instance SpecTranslate (Hash a b) where
  type SpecRep (Hash a b) = Integer

  toSpecRep = pure . hashToInteger

instance SpecTranslate ScriptHash where
  type SpecRep ScriptHash = Integer

  toSpecRep (ScriptHash h) = toSpecRep h

instance SpecTranslate (KeyHash r) where
  type SpecRep (KeyHash r) = Integer

  toSpecRep (KeyHash h) = toSpecRep h

instance SpecTranslate (Credential k) where
  type SpecRep (Credential k) = Agda.Credential

  toSpecRep (KeyHashObj h) = Agda.KeyHashObj <$> toSpecRep h
  toSpecRep (ScriptHashObj h) = Agda.ScriptObj <$> toSpecRep h

instance SpecTranslate Network where
  type SpecRep Network = Integer

  toSpecRep = pure . fromIntegral . fromEnum

deriving instance SpecTranslate Coin

deriving instance SpecTranslate SlotNo

deriving instance SpecTranslate EpochNo

deriving instance SpecTranslate EpochInterval

instance SpecTranslate ProtVer where
  type SpecRep ProtVer = (Integer, Integer)

  toSpecRep (ProtVer ver minor) = pure (getVersion ver, toInteger minor)

instance SpecTranslate Prices where
  type SpecRep Prices = ()

  toSpecRep _ = pure ()

instance SpecTranslate AccountAddress where
  type SpecRep AccountAddress = Agda.RewardAddress

  toSpecRep (AccountAddress n (AccountId c)) = Agda.RewardAddress <$> toSpecRep n <*> toSpecRep c

instance
  SpecTranslate (PParamsHKD Identity era) =>
  SpecTranslate (PParams era)
  where
  type SpecRep (PParams era) = SpecRep (PParamsHKD Identity era)
  type SpecContext (PParams era) = SpecContext (PParamsHKD Identity era)

  toSpecRep (PParams x) = toSpecRep x

instance
  SpecTranslate (PParamsHKD StrictMaybe era) =>
  SpecTranslate (PParamsUpdate era)
  where
  type SpecRep (PParamsUpdate era) = SpecRep (PParamsHKD StrictMaybe era)
  type SpecContext (PParamsUpdate era) = SpecContext (PParamsHKD StrictMaybe era)

  toSpecRep (PParamsUpdate ppu) = toSpecRep ppu

vkeyToInteger :: VKey kd -> Integer
vkeyToInteger = toInteger . bytesToNatural . rawSerialiseVerKeyDSIGN . unVKey

vkeyFromInteger :: Integer -> Maybe (VKey kd)
vkeyFromInteger = fmap VKey . rawDeserialiseVerKeyDSIGN . naturalToBytes 32 . fromInteger

signatureToInteger :: DSIGNAlgorithm v => SigDSIGN v -> Integer
signatureToInteger = toInteger . bytesToNatural . rawSerialiseSigDSIGN

signatureFromInteger :: DSIGNAlgorithm v => Integer -> Maybe (SigDSIGN v)
signatureFromInteger = rawDeserialiseSigDSIGN . naturalToBytes 64 . fromInteger

instance SpecTranslate (VKey k) where
  type SpecRep (VKey k) = Agda.HSVKey

  toSpecRep x = do
    let hvkVKey = vkeyToInteger x
    hvkStoredHash <- toSpecRep (hashVerKeyDSIGN @_ @ADDRHASH $ unVKey x)
    pure Agda.MkHSVKey {..}

instance DSIGNAlgorithm v => SpecTranslate (SignedDSIGN v a) where
  type SpecRep (SignedDSIGN v a) = Integer

  toSpecRep (SignedDSIGN x) = pure $ signatureToInteger x

instance SpecTranslate (WitVKey k) where
  type SpecRep (WitVKey k) = SpecRep (VKey k, Integer)

  toSpecRep (WitVKey vk sk) = withSpecTransM (const ((), ())) $ toSpecRep (vk, sk)

instance SpecTranslate CommitteeAuthorization where
  type
    SpecRep CommitteeAuthorization =
      SpecRep (Maybe (Credential HotCommitteeRole))

  toSpecRep (CommitteeHotCredential c) = toSpecRep $ Just c
  toSpecRep (CommitteeMemberResigned _) =
    toSpecRep $
      Nothing @(Credential HotCommitteeRole)

instance SpecTranslate (CommitteeState era) where
  type
    SpecRep (CommitteeState era) =
      Agda.HSMap (SpecRep (Credential ColdCommitteeRole)) (SpecRep CommitteeAuthorization)

  toSpecRep = withSpecTransM (const ((), ())) . toSpecRep . csCommitteeCreds

committeeCredentialToStrictMaybe ::
  CommitteeAuthorization ->
  StrictMaybe (Credential HotCommitteeRole)
committeeCredentialToStrictMaybe (CommitteeHotCredential c) = SJust c
committeeCredentialToStrictMaybe (CommitteeMemberResigned _) = SNothing

instance SpecTranslate IndividualPoolStake where
  type SpecRep IndividualPoolStake = SpecRep Coin

  toSpecRep (IndividualPoolStake _ c _) = toSpecRep c

instance SpecTranslate PoolDistr where
  type SpecRep PoolDistr = Agda.HSMap (SpecRep (KeyHash StakePool)) Agda.Coin

  toSpecRep (PoolDistr ps _) = withSpecTransM (const ((), ())) $ toSpecRep ps

instance SpecTranslate BlocksMade where
  type SpecRep BlocksMade = Agda.HSMap Integer Integer

  toSpecRep (BlocksMade m) = do
    xs <- forM (Map.toList m) $ \(k, v) -> do
      k' <- toSpecRep k
      pure (k', naturalToInteger v)
    pure $ Agda.MkHSMap xs
