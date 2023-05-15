{-# LANGUAGE ConstrainedClassMethods #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TypeFamilyDependencies #-}
{-# LANGUAGE ViewPatterns #-}

module Cardano.Ledger.Core.TxCert (
  EraTxCert (..),
  pattern TxCertPool,
  Delegation (..),
  PoolCert (..),
  poolCWitness,
  poolCertKeyHashWitness,
)
where

import Cardano.Ledger.Binary (DecCBOR, EncCBOR, FromCBOR, ToCBOR)
import Cardano.Ledger.Core.Era (Era (EraCrypto))
import Cardano.Ledger.Credential (Credential (..), StakeCredential)
import Cardano.Ledger.Hashes (ScriptHash)
import Cardano.Ledger.Keys (KeyHash (..), KeyRole (..), asWitness)
import Cardano.Ledger.PoolParams (PoolParams (ppId))
import Cardano.Ledger.Slot (EpochNo (..))
import Control.DeepSeq (NFData (..), rwhnf)
import Data.Kind (Type)
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

  -- | Return a witness key whenever a certificate requires one
  getVKeyWitnessTxCert :: TxCert era -> Maybe (KeyHash 'Witness (EraCrypto era))

  -- | Return a ScriptHash for certificate types that require a witness
  getScriptWitnessTxCert :: TxCert era -> Maybe (ScriptHash (EraCrypto era))

  mkTxCertPool :: PoolCert (EraCrypto era) -> TxCert era

  getTxCertPool :: TxCert era -> Maybe (PoolCert (EraCrypto era))

pattern TxCertPool :: EraTxCert era => PoolCert (EraCrypto era) -> TxCert era
pattern TxCertPool d <- (getTxCertPool -> Just d)
  where
    TxCertPool d = mkTxCertPool d

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

instance NFData (PoolCert c) where
  rnf = rwhnf

poolCertKeyHashWitness :: PoolCert c -> KeyHash 'Witness c
poolCertKeyHashWitness = \case
  RegPool poolParams -> asWitness $ ppId poolParams
  RetirePool poolId _ -> asWitness poolId

poolCWitness :: PoolCert c -> Credential 'StakePool c
poolCWitness (RegPool pool) = KeyHashObj $ ppId pool
poolCWitness (RetirePool k _) = KeyHashObj k
