{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE TypeFamilies #-}

module Cardano.Ledger.Alonzo.Redeemer
  ( AlonzoRedeemerPurpose (..)
  , AlonzoScriptPurpose (..)
  ) where

import Cardano.Ledger.Core (Era (..), EraTxCert (..))
import GHC.Generics (Generic)
import NoThunks.Class (NoThunks)
import Control.DeepSeq (NFData (..), rwhnf)
import Cardano.Ledger.TreeDiff (ToExpr)
import Cardano.Ledger.Mary.Value (PolicyID)
import Cardano.Ledger.TxIn (TxIn)
import Cardano.Ledger.Address (RewardAcnt)
import Cardano.Ledger.Binary.Coders (Encode(..), encode, (!>), decode, Decode (..), (<!))
import Cardano.Ledger.Binary (EncCBOR (..), DecCBOR (..))

-- | Marker indicating the part of a transaction for which this script is acting
-- as a validator.
data AlonzoRedeemerPurpose era
  = -- | Validates spending a script-locked UTxO
    Spend
  | -- | Validates minting new tokens
    Mint
  | -- | Validates certificate transactions
    Cert
  | -- | Validates withdrawal from a reward account
    Rewrd
  deriving (Eq, Generic, Ord, Show, Enum, Bounded)

instance NoThunks (AlonzoRedeemerPurpose era)

instance NFData (AlonzoRedeemerPurpose era) where
  rnf = rwhnf

instance ToExpr (AlonzoRedeemerPurpose era)

data AlonzoScriptPurpose era
  = Minting !(PolicyID (EraCrypto era))
  | Spending !(TxIn (EraCrypto era))
  | Rewarding !(RewardAcnt (EraCrypto era))
  | Certifying !(TxCert era)
  deriving (Generic)

instance ToExpr (TxCert era) => ToExpr (AlonzoScriptPurpose era)

deriving instance (Era era, Eq (TxCert era)) => Eq (AlonzoScriptPurpose era)
deriving instance (Era era, Show (TxCert era)) => Show (AlonzoScriptPurpose era)
deriving instance (Era era, NoThunks (TxCert era)) => NoThunks (AlonzoScriptPurpose era)

instance (Era era, NFData (TxCert era)) => NFData (AlonzoScriptPurpose era) where
  rnf = \case
    Certifying c -> rnf c
    sp -> rwhnf sp

instance (Era era, EncCBOR (TxCert era)) => EncCBOR (AlonzoScriptPurpose era) where
  encCBOR (Minting x) = encode (Sum (Minting @era) 0 !> To x)
  encCBOR (Spending x) = encode (Sum (Spending @era) 1 !> To x)
  encCBOR (Rewarding x) = encode (Sum (Rewarding @era) 2 !> To x)
  encCBOR (Certifying x) = encode (Sum Certifying 3 !> To x)

instance (Era era, DecCBOR (TxCert era)) => DecCBOR (AlonzoScriptPurpose era) where
  decCBOR = decode (Summands "ScriptPurpose" dec)
    where
      dec 0 = SumD Minting <! From
      dec 1 = SumD Spending <! From
      dec 2 = SumD Rewarding <! From
      dec 3 = SumD Certifying <! From
      dec n = Invalid n
  {-# INLINE decCBOR #-}
