{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}

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
  translateAlonzoScript, AlonzoRedeemerPurpose, AlonzoEraScript (..),
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

data ConwayRedeemerPurpose era
  = AlonzoInConwayRedeemerPurpose (AlonzoRedeemerPurpose era)
  | Voting
  | Proposing
  deriving (Generic, NFData, NoThunks, Eq, Ord, Show, ToExpr)

data ConwayScriptPurpose era
  = AlonzoInConwayScriptPurpose !(AlonzoScriptPurpose era)
  | Voting !(VotingProcedure era)
  | Proposing !(ProposalProcedure era)

instance Era era => DecCBOR (ConwayRedeemerPurpose era) where
  decCBOR = undefined

instance Era era => EncCBOR (ConwayRedeemerPurpose era) where
  encCBOR = undefined

instance Crypto c => EraScript (ConwayEra c) where
  type Script (ConwayEra c) = AlonzoScript (ConwayEra c)
  type NativeScript (ConwayEra c) = Timelock (ConwayEra c)

  upgradeScript = translateAlonzoScript

  scriptPrefixTag = babbageScriptPrefixTag

  getNativeScript = \case
    TimelockScript ts -> Just ts
    _ -> Nothing

instance Crypto c => AlonzoEraScript (ConwayEra c) where
  type RedeemerPurpose (ConwayEra c) = ConwayRedeemerPurpose (ConwayEra c)

  translateRedeemerPurpose = AlonzoInConwayRedeemerPurpose . coerce
