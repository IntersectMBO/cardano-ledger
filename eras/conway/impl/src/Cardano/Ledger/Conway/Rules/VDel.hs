{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE EmptyDataDeriving #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Cardano.Ledger.Conway.Rules.VDel (
  ConwayVDEL,
  ConwayVDelEvent (..),
  VDelEnv (..),
  ConwayVDelPredFailure,
)
where

import Cardano.Ledger.BaseTypes (
  ShelleyBase,
 )
import Cardano.Ledger.Binary (DecCBOR (..), EncCBOR (..), encodeListLen)
import Cardano.Ledger.Binary.Coders
import Cardano.Ledger.CertState (VState)
import Cardano.Ledger.Conway.Era (ConwayVDEL)
import Cardano.Ledger.Conway.TxCert (ConwayCommitteeCert)
import Cardano.Ledger.Core (Era (EraCrypto), EraRule)
import Control.DeepSeq (NFData)
import Control.State.Transition (
  BaseM,
  Environment,
  Event,
  PredicateFailure,
  STS,
  Signal,
  State,
  transitionRules,
 )
import Data.Typeable (Typeable)
import Data.Word (Word8)
import GHC.Generics (Generic)
import NoThunks.Class (NoThunks (..))

data ConwayVDelPredFailure era = ConwayVDelPredFailure
  deriving (Show, Eq, Generic, NoThunks, NFData)

instance Typeable era => EncCBOR (ConwayVDelPredFailure era) where
  encCBOR = \case
    ConwayVDelPredFailure -> encodeListLen 1 <> encCBOR (0 :: Word8)

instance Typeable era => DecCBOR (ConwayVDelPredFailure era) where
  decCBOR = decodeRecordSum "ConwayVDelPredFailure" $
    \case
      0 -> pure (1, ConwayVDelPredFailure)
      k -> invalidKey k

data VDelEnv era = VDelEnv

newtype ConwayVDelEvent era = VDelEvent (Event (EraRule "VDEL" era))

instance
  ( Era era
  , State (EraRule "VDEL" era) ~ VState era
  , Signal (EraRule "VDEL" era) ~ ConwayCommitteeCert (EraCrypto era)
  , Environment (EraRule "VDEL" era) ~ VDelEnv era
  , EraRule "VDEL" era ~ ConwayVDEL era
  , Eq (PredicateFailure (EraRule "VDEL" era))
  , Show (PredicateFailure (EraRule "VDEL" era))
  ) =>
  STS (ConwayVDEL era)
  where
  type State (ConwayVDEL era) = VState era
  type Signal (ConwayVDEL era) = ConwayCommitteeCert (EraCrypto era)
  type Environment (ConwayVDEL era) = VDelEnv era
  type BaseM (ConwayVDEL era) = ShelleyBase
  type PredicateFailure (ConwayVDEL era) = ConwayVDelPredFailure era
  type Event (ConwayVDEL era) = ConwayVDelEvent era

  transitionRules = undefined -- TODO
