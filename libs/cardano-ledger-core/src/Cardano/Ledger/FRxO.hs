{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilyDependencies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE UndecidableSuperClasses #-}

module Cardano.Ledger.FRxO where

import Cardano.Ledger.Binary (
  DecCBOR,
  DecShareCBOR (Share),
  EncCBOR,
  FromCBOR,
  Interns,
  ToCBOR (toCBOR),
  decCBOR,
  decodeMap,
 )
import Cardano.Ledger.Binary.Decoding (DecShareCBOR (decShareCBOR))
import Cardano.Ledger.Binary.Plain (fromCBOR)
import Cardano.Ledger.Core (
  Era (EraCrypto),
  EraTxOut (TxOut),
  fromEraCBOR,
  toEraCBOR,
 )
import Cardano.Ledger.Credential (Credential)
import Cardano.Ledger.Crypto (Crypto)
import Cardano.Ledger.Keys (KeyRole (Staking))
import Cardano.Ledger.TxIn (TxIn)
import Control.DeepSeq (NFData)
import Control.Monad ((<$!>))
import Data.Aeson (ToJSON)
import Data.Default.Class (Default)
import qualified Data.Map as Map
import GHC.Generics (Generic)
import NoThunks.Class (NoThunks)
import Quiet (Quiet (Quiet))

-- | The unspent transaction outputs.
newtype FRxO era = FRxO {unFRxO :: Map.Map (TxIn (EraCrypto era)) (TxOut era)}
  deriving (Default, Generic, Semigroup)

instance (EncCBOR (TxOut era), Era era) => ToCBOR (FRxO era) where
  toCBOR = toEraCBOR @era

instance (DecCBOR (TxOut era), Era era) => FromCBOR (FRxO era) where
  fromCBOR = fromEraCBOR @era

deriving instance NoThunks (TxOut era) => NoThunks (FRxO era)

deriving instance (Era era, NFData (TxOut era)) => NFData (FRxO era)

deriving newtype instance (Era era, Eq (TxOut era)) => Eq (FRxO era)

deriving newtype instance Era era => Monoid (FRxO era)

deriving newtype instance (Era era, EncCBOR (TxOut era)) => EncCBOR (FRxO era)

deriving newtype instance (Era era, DecCBOR (TxOut era)) => DecCBOR (FRxO era)

instance
  ( Crypto (EraCrypto era)
  , DecShareCBOR (TxOut era)
  , Share (TxOut era) ~ Interns (Credential 'Staking (EraCrypto era))
  ) =>
  DecShareCBOR (FRxO era)
  where
  type
    Share (FRxO era) =
      Interns (Credential 'Staking (EraCrypto era))
  decShareCBOR credsInterns =
    FRxO <$!> decodeMap decCBOR (decShareCBOR credsInterns)

deriving via
  Quiet (FRxO era)
  instance
    (Show (TxOut era), Crypto (EraCrypto era)) => Show (FRxO era)

deriving newtype instance (Era era, ToJSON (TxOut era)) => ToJSON (FRxO era)