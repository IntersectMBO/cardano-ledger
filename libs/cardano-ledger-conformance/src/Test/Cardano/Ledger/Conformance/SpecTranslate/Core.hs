{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StandaloneDeriving #-}
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
import Cardano.Ledger.Address (Addr (..), BootstrapAddress (..), RewardAccount (..))
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
  DataHash,
  Era,
  Hash,
  KeyHash (..),
  KeyRole (..),
  PParams (..),
  PParamsHKD,
  PParamsUpdate (..),
  SafeHash,
  ScriptHash (..),
  TxAuxDataHash (..),
  extractHash,
  hashAnnotated,
 )
import Cardano.Ledger.Conway.State
import Cardano.Ledger.Credential (Credential (..), StakeReference (..))
import Cardano.Ledger.Keys (VKey (..))
import Cardano.Ledger.Keys.WitVKey (WitVKey (..))
import Cardano.Ledger.Plutus.CostModels (CostModels)
import Cardano.Ledger.Plutus.Data (BinaryData, Data, Datum (..), hashBinaryData)
import Cardano.Ledger.Plutus.ExUnits (ExUnits (..), Prices)
import Cardano.Ledger.TxIn (TxId (..), TxIn (..))
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

instance SpecTranslate ctx TxId where
  type SpecRep TxId = Agda.TxId

  toSpecRep (TxId x) = toSpecRep x

instance SpecTranslate ctx TxIx where
  type SpecRep TxIx = Integer

  toSpecRep (TxIx x) = pure $ toInteger x

instance SpecTranslate ctx TxIn where
  type SpecRep TxIn = Agda.TxIn

  toSpecRep (TxIn txId txIx) = toSpecRep (txId, txIx)

instance SpecTranslate ctx (SafeHash a) where
  type SpecRep (SafeHash a) = Agda.DataHash

  toSpecRep = toSpecRep . extractHash

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

instance
  ( SpecRep DataHash ~ Agda.DataHash
  , Era era
  ) =>
  SpecTranslate ctx (BinaryData era)
  where
  type SpecRep (BinaryData era) = Agda.DataHash

  toSpecRep = toSpecRep . hashBinaryData

instance Era era => SpecTranslate ctx (Datum era) where
  type SpecRep (Datum era) = Maybe (Either Agda.Datum Agda.DataHash)

  toSpecRep NoDatum = pure Nothing
  toSpecRep (Datum d) = Just . Left <$> toSpecRep d
  toSpecRep (DatumHash h) = Just . Right <$> toSpecRep h

instance Era era => SpecTranslate ctx (Data era) where
  type SpecRep (Data era) = Agda.DataHash

  toSpecRep = toSpecRep . hashAnnotated

instance
  SpecTranslate ctx (PParamsHKD Identity era) =>
  SpecTranslate ctx (PParams era)
  where
  type SpecRep (PParams era) = SpecRep (PParamsHKD Identity era)

  toSpecRep (PParams x) = toSpecRep x

instance
  SpecTranslate ctx (PParamsHKD StrictMaybe era) =>
  SpecTranslate ctx (PParamsUpdate era)
  where
  type SpecRep (PParamsUpdate era) = SpecRep (PParamsHKD StrictMaybe era)

  toSpecRep (PParamsUpdate ppu) = toSpecRep ppu

vkeyToInteger :: VKey kd -> Integer
vkeyToInteger = toInteger . bytesToNatural . rawSerialiseVerKeyDSIGN . unVKey

vkeyFromInteger :: Integer -> Maybe (VKey kd)
vkeyFromInteger = fmap VKey . rawDeserialiseVerKeyDSIGN . naturalToBytes 32 . fromInteger

signatureToInteger :: DSIGNAlgorithm v => SigDSIGN v -> Integer
signatureToInteger = toInteger . bytesToNatural . rawSerialiseSigDSIGN

signatureFromInteger :: DSIGNAlgorithm v => Integer -> Maybe (SigDSIGN v)
signatureFromInteger = rawDeserialiseSigDSIGN . naturalToBytes 64 . fromInteger

instance SpecTranslate ctx (VKey k) where
  type SpecRep (VKey k) = Agda.HSVKey

  toSpecRep x = do
    let hvkVKey = vkeyToInteger x
    hvkStoredHash <- toSpecRep (hashVerKeyDSIGN @_ @ADDRHASH $ unVKey x)
    pure Agda.MkHSVKey {..}

instance DSIGNAlgorithm v => SpecTranslate ctx (SignedDSIGN v a) where
  type SpecRep (SignedDSIGN v a) = Integer

  toSpecRep (SignedDSIGN x) = pure $ signatureToInteger x

instance SpecTranslate ctx (WitVKey k) where
  type SpecRep (WitVKey k) = (SpecRep (VKey k), Integer)

  toSpecRep (WitVKey vk sk) = toSpecRep (vk, sk)

instance SpecTranslate ctx TxAuxDataHash where
  type SpecRep TxAuxDataHash = Agda.DataHash

  toSpecRep (TxAuxDataHash x) = toSpecRep x

instance SpecTranslate ctx CommitteeAuthorization where
  type
    SpecRep CommitteeAuthorization =
      SpecRep (Maybe (Credential HotCommitteeRole))

  toSpecRep (CommitteeHotCredential c) = toSpecRep $ Just c
  toSpecRep (CommitteeMemberResigned _) =
    toSpecRep $
      Nothing @(Credential HotCommitteeRole)

instance SpecTranslate ctx (CommitteeState era) where
  type
    SpecRep (CommitteeState era) =
      SpecRep (Map (Credential ColdCommitteeRole) CommitteeAuthorization)

  toSpecRep = toSpecRep . csCommitteeCreds

committeeCredentialToStrictMaybe ::
  CommitteeAuthorization ->
  StrictMaybe (Credential HotCommitteeRole)
committeeCredentialToStrictMaybe (CommitteeHotCredential c) = SJust c
committeeCredentialToStrictMaybe (CommitteeMemberResigned _) = SNothing

instance SpecTranslate ctx IndividualPoolStake where
  type SpecRep IndividualPoolStake = SpecRep Coin

  toSpecRep (IndividualPoolStake _ c _) = toSpecRep c

instance SpecTranslate ctx PoolDistr where
  type SpecRep PoolDistr = Agda.HSMap (SpecRep (KeyHash StakePool)) Agda.Coin

  toSpecRep (PoolDistr ps _) = toSpecRep ps

instance SpecTranslate ctx BlocksMade where
  type SpecRep BlocksMade = Agda.HSMap Integer Integer

  toSpecRep (BlocksMade m) = do
    xs <- forM (Map.toList m) $ \(k, v) -> do
      k' <- toSpecRep k
      pure (k', naturalToInteger v)
    pure $ Agda.MkHSMap xs
