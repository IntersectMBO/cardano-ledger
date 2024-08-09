{-# LANGUAGE ConstrainedClassMethods #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilyDependencies #-}
{-# LANGUAGE UndecidableSuperClasses #-}
{-# LANGUAGE ViewPatterns #-}

module Cardano.Ledger.Core.TxCert (
  EraTxCert (..),
  pattern RegPoolTxCert,
  pattern RetirePoolTxCert,
  Delegation (..),
  PoolCert (..),
  getPoolCertTxCert,
  poolCWitness,
  poolCertKeyHashWitness,
  isRegStakeTxCert,
  isUnRegStakeTxCert,
)
where

import Cardano.Ledger.BaseTypes (kindObject)
import Cardano.Ledger.Binary (DecCBOR (..), EncCBOR (..), FromCBOR, ToCBOR)
import Cardano.Ledger.Binary.Coders (Encode (..), encode, (!>))
import Cardano.Ledger.Coin (Coin)
import Cardano.Ledger.Core.Era (Era (EraCrypto))
import Cardano.Ledger.Core.PParams (PParams)
import Cardano.Ledger.Core.Translation
import Cardano.Ledger.Credential (Credential (..), StakeCredential)
import Cardano.Ledger.Crypto
import Cardano.Ledger.Hashes (ScriptHash)
import Cardano.Ledger.Keys (KeyHash (..), KeyRole (..), asWitness)
import Cardano.Ledger.PoolParams (PoolParams (ppId))
import Cardano.Ledger.Slot (EpochNo (..))
import Control.DeepSeq (NFData (..), rwhnf)
import Data.Aeson (ToJSON (..), (.=))
import Data.Kind (Type)
import Data.Maybe (isJust)
import Data.Void (Void)
import GHC.Generics (Generic)
import NoThunks.Class (NoThunks (..))

class
  ( Era era
  , ToJSON (TxCert era)
  , DecCBOR (TxCert era)
  , EncCBOR (TxCert era)
  , ToCBOR (TxCert era)
  , FromCBOR (TxCert era)
  , NoThunks (TxCert era)
  , NFData (TxCert era)
  , Show (TxCert era)
  , Ord (TxCert era)
  , Eq (TxCert era)
  ) =>
  EraTxCert era
  where
  type TxCert era = (r :: Type) | r -> era

  type TxCertUpgradeError era :: Type
  type TxCertUpgradeError era = Void

  -- | Every era, except Shelley, must be able to upgrade a `TxCert` from a previous
  -- era. However, not all certificates can be upgraded, because some eras lose some of
  -- the certificates, thus return type is an `Either`. Eg. from Babbage to Conway: MIR
  -- and Genesis certificates were removed.
  upgradeTxCert ::
    EraTxCert (PreviousEra era) =>
    TxCert (PreviousEra era) ->
    Either (TxCertUpgradeError era) (TxCert era)

  -- | Return a witness key whenever a certificate requires one
  getVKeyWitnessTxCert :: TxCert era -> Maybe (KeyHash 'Witness (EraCrypto era))

  -- | Return a ScriptHash for certificate types that require a witness
  getScriptWitnessTxCert :: TxCert era -> Maybe (ScriptHash (EraCrypto era))

  mkRegPoolTxCert :: PoolParams (EraCrypto era) -> TxCert era
  getRegPoolTxCert :: TxCert era -> Maybe (PoolParams (EraCrypto era))

  mkRetirePoolTxCert :: KeyHash 'StakePool (EraCrypto era) -> EpochNo -> TxCert era
  getRetirePoolTxCert :: TxCert era -> Maybe (KeyHash 'StakePool (EraCrypto era), EpochNo)

  -- | Extract staking credential from any certificate that can register such credential
  lookupRegStakeTxCert :: TxCert era -> Maybe (Credential 'Staking (EraCrypto era))

  -- | Extract staking credential from any certificate that can unregister such credential
  lookupUnRegStakeTxCert :: TxCert era -> Maybe (Credential 'Staking (EraCrypto era))

  -- | Compute the total deposits from a list of certificates.
  getTotalDepositsTxCerts ::
    Foldable f =>
    PParams era ->
    -- | Check whether stake pool is registered or not
    (KeyHash 'StakePool (EraCrypto era) -> Bool) ->
    f (TxCert era) ->
    Coin

  -- | Compute the total refunds from a list of certificates.
  getTotalRefundsTxCerts ::
    Foldable f =>
    PParams era ->
    -- | Lookup current deposit for Staking credential if one is registered
    (Credential 'Staking (EraCrypto era) -> Maybe Coin) ->
    -- | Lookup current deposit for DRep credential if one is registered
    (Credential 'DRepRole (EraCrypto era) -> Maybe Coin) ->
    f (TxCert era) ->
    Coin

pattern RegPoolTxCert :: EraTxCert era => PoolParams (EraCrypto era) -> TxCert era
pattern RegPoolTxCert d <- (getRegPoolTxCert -> Just d)
  where
    RegPoolTxCert d = mkRegPoolTxCert d

pattern RetirePoolTxCert ::
  EraTxCert era =>
  KeyHash 'StakePool (EraCrypto era) ->
  EpochNo ->
  TxCert era
pattern RetirePoolTxCert poolId epochNo <- (getRetirePoolTxCert -> Just (poolId, epochNo))
  where
    RetirePoolTxCert poolId epochNo = mkRetirePoolTxCert poolId epochNo

getPoolCertTxCert :: EraTxCert era => TxCert era -> Maybe (PoolCert (EraCrypto era))
getPoolCertTxCert = \case
  RegPoolTxCert poolParams -> Just $ RegPool poolParams
  RetirePoolTxCert poolId epochNo -> Just $ RetirePool poolId epochNo
  _ -> Nothing

-- | The delegation of one stake key to another.
data Delegation c = Delegation
  { dDelegator :: !(StakeCredential c)
  , dDelegatee :: !(KeyHash 'StakePool c)
  }
  deriving (Eq, Generic, Show)
{-# DEPRECATED Delegation "No longer used" #-}

instance NFData (Delegation c) where
  rnf = rwhnf

instance NoThunks (Delegation c)

data PoolCert c
  = -- | A stake pool registration certificate.
    RegPool !(PoolParams c)
  | -- | A stake pool retirement certificate.
    RetirePool !(KeyHash 'StakePool c) !EpochNo
  deriving (Show, Generic, Eq, Ord)

instance Crypto c => EncCBOR (PoolCert c) where
  encCBOR =
    encode . \case
      RegPool pp -> Sum RegPool 0 !> To pp
      RetirePool kh eNo -> Sum RetirePool 1 !> To kh !> To eNo

instance NoThunks (PoolCert c)

instance NFData (PoolCert c) where
  rnf = rwhnf

instance Crypto c => ToJSON (PoolCert c) where
  toJSON = \case
    RegPool poolParams ->
      kindObject "RegPool" ["poolParams" .= toJSON poolParams]
    RetirePool poolId epochNo ->
      kindObject "RetirePool" $
        [ "poolId" .= toJSON poolId
        , "epochNo" .= toJSON epochNo
        ]

poolCertKeyHashWitness :: PoolCert c -> KeyHash 'Witness c
poolCertKeyHashWitness = \case
  RegPool poolParams -> asWitness $ ppId poolParams
  RetirePool poolId _ -> asWitness poolId

poolCWitness :: PoolCert c -> Credential 'StakePool c
poolCWitness (RegPool pool) = KeyHashObj $ ppId pool
poolCWitness (RetirePool k _) = KeyHashObj k
{-# DEPRECATED poolCWitness "As useless. PoolIds have nothing to do with credentials" #-}

-- | Check if supplied TxCert is a stake registering certificate
isRegStakeTxCert :: EraTxCert era => TxCert era -> Bool
isRegStakeTxCert = isJust . lookupRegStakeTxCert

-- | Check if supplied TxCert is a stake un-registering certificate
isUnRegStakeTxCert :: EraTxCert era => TxCert era -> Bool
isUnRegStakeTxCert = isJust . lookupUnRegStakeTxCert

-- =================================================================
