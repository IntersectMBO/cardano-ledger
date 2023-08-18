{-# LANGUAGE ConstrainedClassMethods #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE PatternSynonyms #-}
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
  DRep (
    DRepCredential,
    DRepAlwaysAbstain,
    DRepAlwaysNoConfidence
  ),
)
where

import Cardano.Ledger.Binary (DecCBOR (..), EncCBOR (..), FromCBOR, ToCBOR)
import Cardano.Ledger.Binary.Coders (
  Decode (..),
  Encode (..),
  decode,
  encode,
  (!>),
  (<!),
 )
import Cardano.Ledger.Core.Era (Era (EraCrypto))
import Cardano.Ledger.Core.Translation
import Cardano.Ledger.Credential (Credential (..), StakeCredential)
import Cardano.Ledger.Crypto (Crypto)
import Cardano.Ledger.Hashes (ScriptHash)
import Cardano.Ledger.Keys (KeyHash (..), KeyRole (..), asWitness)
import Cardano.Ledger.PoolParams (PoolParams (ppId))
import Cardano.Ledger.Slot (EpochNo (..))
import Cardano.Ledger.TreeDiff (ToExpr)
import Control.DeepSeq (NFData (..), rwhnf)
import Data.Aeson (ToJSON)
import Data.Kind (Type)
import Data.Void (Void)
import GHC.Generics (Generic)
import NoThunks.Class (NoThunks (..))

class
  ( Era era
  , DecCBOR (TxCert era)
  , EncCBOR (TxCert era)
  , ToCBOR (TxCert era)
  , FromCBOR (TxCert era)
  , NoThunks (TxCert era)
  , NFData (TxCert era)
  , Show (TxCert era)
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
  deriving (Show, Generic, Eq)

instance NoThunks (PoolCert c)
instance ToExpr (PoolCert c)

instance NFData (PoolCert c) where
  rnf = rwhnf

poolCertKeyHashWitness :: PoolCert c -> KeyHash 'Witness c
poolCertKeyHashWitness = \case
  RegPool poolParams -> asWitness $ ppId poolParams
  RetirePool poolId _ -> asWitness poolId

poolCWitness :: PoolCert c -> Credential 'StakePool c
poolCWitness (RegPool pool) = KeyHashObj $ ppId pool
poolCWitness (RetirePool k _) = KeyHashObj k

data DRep c
  = DRepKeyHash !(KeyHash 'DRepRole c)
  | DRepScriptHash !(ScriptHash c)
  | DRepAlwaysAbstain
  | DRepAlwaysNoConfidence
  deriving (Show, Eq, Ord, Generic, NoThunks, NFData, ToExpr, ToJSON)

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

pattern DRepCredential :: Credential 'DRepRole c -> DRep c
pattern DRepCredential c <- (dRepToCred -> Just c)
  where
    DRepCredential c = case c of
      ScriptHashObj sh -> DRepScriptHash sh
      KeyHashObj kh -> DRepKeyHash kh

{-# COMPLETE DRepCredential, DRepAlwaysAbstain, DRepAlwaysNoConfidence :: DRep #-}
