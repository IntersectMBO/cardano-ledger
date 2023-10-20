{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE UndecidableInstances #-}

module Cardano.Ledger.Conway.Scripts (
  AlonzoScript (..),
  ConwayRedeemerPurpose (..),
  isPlutusScript,
  babbageScriptPrefixTag,
)
where

import Cardano.Ledger.Allegra.Scripts (Timelock)
import Cardano.Ledger.Alonzo.Scripts (
  AlonzoScript (..),
  isPlutusScript,
  AlonzoRedeemerPurpose,
  AlonzoEraScript (..),
  upgradeAlonzoScript, AlonzoScriptPurpose,
 )
import Cardano.Ledger.Babbage.Scripts (babbageScriptPrefixTag)
import Cardano.Ledger.Conway.Era
import Cardano.Ledger.Core
import Cardano.Ledger.Crypto
import Control.DeepSeq (NFData)
import GHC.Generics (Generic)
import NoThunks.Class (NoThunks)
import Cardano.Ledger.Binary (DecCBOR (..), EncCBOR (..))
import Cardano.Ledger.TreeDiff (ToExpr)
import Data.Coerce (coerce)
import Cardano.Ledger.Conway.Governance (VotingProcedure, ProposalProcedure)

data ConwayRedeemerPurpose era
  = AlonzoInConwayRedeemerPurpose (AlonzoRedeemerPurpose era)
  | Vote
  | Propose
  deriving (Generic, NFData, NoThunks, Eq, Ord, Show, ToExpr)

instance Era era => DecCBOR (ConwayRedeemerPurpose era) where
  decCBOR = undefined

instance Era era => EncCBOR (ConwayRedeemerPurpose era) where
  encCBOR = undefined

data ConwayScriptPurpose era
  = AlonzoInConwayScriptPurpose !(AlonzoScriptPurpose era)
  | Voting !(VotingProcedure era)
  | Proposing !(ProposalProcedure era)
  deriving (Generic)

deriving instance (EraPParams era, EraTxCert era) => NFData (ConwayScriptPurpose era)

deriving instance (EraPParams era, EraTxCert era) => NoThunks (ConwayScriptPurpose era)

deriving instance (EraPParams era, EraTxCert era) => Eq (ConwayScriptPurpose era)

deriving instance (EraPParams era, EraTxCert era) => Show (ConwayScriptPurpose era)

deriving instance (EraPParams era, EraTxCert era) => ToExpr (ConwayScriptPurpose era)

instance Era era => DecCBOR (ConwayScriptPurpose era) where
  decCBOR = undefined

instance Era era => EncCBOR (ConwayScriptPurpose era) where
  encCBOR = undefined

instance Crypto c => EraScript (ConwayEra c) where
  type Script (ConwayEra c) = AlonzoScript (ConwayEra c)
  type NativeScript (ConwayEra c) = Timelock (ConwayEra c)

  upgradeScript = upgradeAlonzoScript

  scriptPrefixTag = babbageScriptPrefixTag

  getNativeScript = \case
    TimelockScript ts -> Just ts
    _ -> Nothing

instance EraTxCert (ConwayEra c) => AlonzoEraScript (ConwayEra c) where
  type RedeemerPurpose (ConwayEra c) = ConwayRedeemerPurpose (ConwayEra c)
  type ScriptPurpose (ConwayEra c) = ConwayScriptPurpose (ConwayEra c)

  upgradeRedeemerPurpose = AlonzoInConwayRedeemerPurpose . coerce
