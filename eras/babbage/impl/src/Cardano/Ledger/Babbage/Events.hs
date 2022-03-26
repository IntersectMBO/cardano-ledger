{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE UndecidableInstances #-}

module Cardano.Ledger.Babbage.Events
  ( BabbageEvent (..),
    UnwrapEvent (..),
  )
where

import qualified Cardano.Ledger.Alonzo.Rules.Bbody as AlonzoBbody
import qualified Cardano.Ledger.Alonzo.Rules.Utxo as AlonzoUtxo
import qualified Cardano.Ledger.Alonzo.Rules.Utxos as AlonzoUtxos
import qualified Cardano.Ledger.Alonzo.Rules.Utxow as AlonzoUtxow
import Cardano.Ledger.Coin (Coin)
import Cardano.Ledger.Core (EraRule)
import Cardano.Ledger.Credential (Credential)
import Cardano.Ledger.Era (Era (Crypto))
import Cardano.Ledger.Keys (KeyHash, KeyRole (StakePool, Staking))
import Cardano.Ledger.Shelley.Rewards (Reward)
import qualified Cardano.Ledger.Shelley.Rules.Bbody as ShelleyBbody
import qualified Cardano.Ledger.Shelley.Rules.Deleg as ShelleyDeleg
import qualified Cardano.Ledger.Shelley.Rules.Delegs as ShelleyDelegs
import qualified Cardano.Ledger.Shelley.Rules.Ledger as ShelleyLedger
import qualified Cardano.Ledger.Shelley.Rules.Ledgers as ShelleyLedgers
import qualified Cardano.Ledger.Shelley.Rules.PoolReap as ShelleyPoolReap
import qualified Cardano.Ledger.Shelley.Rules.Ppup as ShelleyPpup
import qualified Cardano.Ledger.Shelley.Rules.Rupd as ShelleyRupd
import qualified Cardano.Ledger.Shelley.Rules.Tick as ShelleyTick
import qualified Cardano.Ledger.Shelley.Rules.Updn as ShelleyUpdn
import qualified Cardano.Ledger.Shelley.Rules.Utxow as ShelleyUtxow
import qualified Cardano.Ledger.ShelleyMA.Rules.Utxo as ShelleyMAUtxo
import Cardano.Ledger.Slot (EpochNo)
import Control.State.Transition.Extended (Event)
import Data.Map.Strict (Map)
import Data.Set (Set)

data BabbageEvent era
  = NoEvent
  | NewEpoch EpochNo
  | RetiredPoolsE (RetiredPools era)
  | RupdEvent !EpochNo !(Map (Credential 'Staking (Crypto era)) (Set (Reward (Crypto era))))

data RetiredPools era = RetiredPools
  { refundPools :: Map (Credential 'Staking (Crypto era)) (Map (KeyHash 'StakePool (Crypto era)) Coin),
    unclaimedPools :: Map (Credential 'Staking (Crypto era)) (Map (KeyHash 'StakePool (Crypto era)) Coin),
    epochNo :: EpochNo
  }

class UnwrapEvent event era where
  unwrapEvent :: event -> BabbageEvent era

instance
  ( UnwrapEvent (Event (EraRule "DELEGS" era)) era,
    UnwrapEvent (Event (EraRule "UTXOW" era)) era
  ) =>
  UnwrapEvent (ShelleyLedger.LedgerEvent era) era
  where
  unwrapEvent (ShelleyLedger.UtxowEvent e) = unwrapEvent e
  unwrapEvent (ShelleyLedger.DelegsEvent e) = unwrapEvent e

instance UnwrapEvent (ShelleyPoolReap.PoolreapEvent era) era where
  unwrapEvent (ShelleyPoolReap.RetiredPools {refundPools, unclaimedPools, epochNo}) = RetiredPoolsE $ RetiredPools {..}

instance (crypto ~ Crypto era) => UnwrapEvent (ShelleyRupd.RupdEvent crypto) era where
  unwrapEvent (ShelleyRupd.RupdEvent eno credRew) = RupdEvent eno credRew

instance UnwrapEvent (ShelleyPpup.PpupEvent era) era where
  unwrapEvent (ShelleyPpup.NewEpoch en) = NewEpoch en

instance UnwrapEvent (ShelleyDeleg.DelegEvent era) era where
  unwrapEvent (ShelleyDeleg.NewEpoch en) = NewEpoch en

instance UnwrapEvent (ShelleyUpdn.UpdnEvent era) era where
  unwrapEvent (ShelleyUpdn.NewEpoch en) = NewEpoch en

instance UnwrapEvent (Event (EraRule "UTXOS" era)) era => UnwrapEvent (AlonzoUtxo.UtxoEvent era) era where
  unwrapEvent (AlonzoUtxo.UtxosEvent e) = unwrapEvent e

instance UnwrapEvent (Event (EraRule "LEDGERS" era)) era => UnwrapEvent (AlonzoBbody.AlonzoBbodyEvent era) era where
  unwrapEvent (AlonzoBbody.ShelleyInAlonzoEvent e) = unwrapEvent e

instance UnwrapEvent (Event (EraRule "UTXO" era)) era => UnwrapEvent (AlonzoUtxow.AlonzoEvent era) era where
  unwrapEvent (AlonzoUtxow.WrappedShelleyEraEvent e) = unwrapEvent e

instance UnwrapEvent (Event (EraRule "LEDGERS" era)) era => UnwrapEvent (ShelleyBbody.BbodyEvent era) era where
  unwrapEvent (ShelleyBbody.LedgersEvent e) = unwrapEvent e

instance UnwrapEvent (Event (EraRule "DELPL" era)) era => UnwrapEvent (ShelleyDelegs.DelegsEvent era) era where
  unwrapEvent (ShelleyDelegs.DelplEvent e) = unwrapEvent e

instance UnwrapEvent (Event (EraRule "LEDGER" era)) era => UnwrapEvent (ShelleyLedgers.LedgersEvent era) era where
  unwrapEvent (ShelleyLedgers.LedgerEvent e) = unwrapEvent e

instance (UnwrapEvent (Event (EraRule "NEWEPOCH" era)) era, UnwrapEvent (Event (EraRule "RUPD" era)) era) => UnwrapEvent (ShelleyTick.TickEvent era) era where
  unwrapEvent (ShelleyTick.NewEpochEvent e) = unwrapEvent e
  unwrapEvent (ShelleyTick.RupdEvent e) = unwrapEvent e

instance UnwrapEvent (Event (EraRule "PPUP" era)) era => UnwrapEvent (ShelleyMAUtxo.UtxoEvent era) era where
  unwrapEvent (ShelleyMAUtxo.UpdateEvent e) = unwrapEvent e

instance UnwrapEvent (Event (EraRule "PPUP" era)) era => UnwrapEvent (AlonzoUtxos.UtxosEvent era) era where
  unwrapEvent (AlonzoUtxos.UpdateEvent e) = unwrapEvent e

instance UnwrapEvent (Event (EraRule "UTXO" era)) era => UnwrapEvent (ShelleyUtxow.UtxowEvent era) era where
  unwrapEvent (ShelleyUtxow.UtxoEvent e) = unwrapEvent e
