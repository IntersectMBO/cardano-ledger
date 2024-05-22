{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Cardano.Ledger.Conway.Rules.Bbody where

import Cardano.Ledger.BHeaderView (
  BHeaderView (bhviewBHash, bhviewBSize, bhviewID, bhviewSlot),
  isOverlaySlot,
 )
import Cardano.Ledger.BaseTypes (BlocksMade, ShelleyBase, epochInfoPure)
import Cardano.Ledger.Conway.Era (ConwayBBODY, ConwayEra)
import Cardano.Ledger.Conway.Rules.Deleg (ConwayDelegPredFailure)
import Cardano.Ledger.Conway.Rules.Ledger (ConwayLedgerPredFailure)
import Cardano.Ledger.Conway.Rules.Ledgers ()
import Cardano.Ledger.Conway.Rules.Utxo (ConwayUtxoPredFailure)
import Cardano.Ledger.Conway.Rules.Utxow (ConwayUtxowPredFailure)
import Cardano.Ledger.Conway.Rules.Zone (ConwayZonePredFailure)
import Cardano.Ledger.Conway.Rules.Zones (ConwayZonesPredFailure)
import Cardano.Ledger.Core
import Cardano.Ledger.Keys (DSignable, HasKeyRole (coerceKeyRole), Hash)
import Cardano.Ledger.Shelley.API (
  Block (UnserialisedBlock),
  ShelleyLedgersEnv (LedgersEnv),
 )
import Cardano.Ledger.Shelley.BlockChain (incrBlocks)
import Cardano.Ledger.Shelley.Rules (
  BbodyEnv (BbodyEnv),
  ShelleyBbodyPredFailure,
  ShelleyLedgersPredFailure,
  ShelleyPoolPredFailure,
 )
import Cardano.Ledger.Slot (epochInfoEpoch, epochInfoFirst)
import Control.Monad.Trans.Reader (asks)
import Control.State.Transition (
  Embed (wrapEvent),
  STS (..),
  TRC (TRC),
  TransitionRule,
  judgmentContext,
  liftSTS,
  trans,
  (?!),
 )
import Control.State.Transition.Simple (Embed (wrapFailed))
import Data.Sequence (Seq)
import qualified Data.Sequence.Strict as StrictSeq
import GHC.Generics (Generic)
import Lens.Micro ((^.))
import NoThunks.Class (NoThunks)

data ConwayBbodyState era
  = BbodyState !(State (EraRule "ZONES" era)) !(BlocksMade (EraCrypto era))

data ConwayBbodyPredFailure era
  = WrongBlockBodySizeBBODY
      !Int -- Actual Body Size
      !Int -- Claimed Body Size in Header
  | InvalidBodyHashBBODY
      !(Hash (EraCrypto era) EraIndependentBlockBody) -- Actual Hash
      !(Hash (EraCrypto era) EraIndependentBlockBody) -- Claimed Hash
  | ZonesFailure (PredicateFailure (EraRule "ZONES" era)) -- Subtransition Failures
  | ShellyInConwayBbodyPredFailure (ShelleyBbodyPredFailure era)
  deriving (Generic)

type instance EraRuleFailure "BBODY" (ConwayEra c) = ConwayBbodyPredFailure (ConwayEra c)

instance InjectRuleFailure "BBODY" ConwayBbodyPredFailure (ConwayEra c)

instance InjectRuleFailure "BBODY" ConwayZonesPredFailure (ConwayEra c) where
  injectFailure :: ConwayZonesPredFailure (ConwayEra c) -> ConwayBbodyPredFailure (ConwayEra c)
  injectFailure = ZonesFailure

instance InjectRuleFailure "BBODY" ConwayZonePredFailure (ConwayEra c) where
  injectFailure = ZonesFailure . injectFailure

instance InjectRuleFailure "BBODY" ShelleyLedgersPredFailure (ConwayEra c) where
  injectFailure = ZonesFailure . injectFailure

instance InjectRuleFailure "BBODY" ConwayLedgerPredFailure (ConwayEra c) where
  injectFailure = ZonesFailure . injectFailure

instance InjectRuleFailure "BBODY" ConwayUtxowPredFailure (ConwayEra c) where
  injectFailure = ZonesFailure . injectFailure

instance InjectRuleFailure "BBODY" ConwayUtxoPredFailure (ConwayEra c) where
  injectFailure = ZonesFailure . injectFailure

instance InjectRuleFailure "BBODY" ShelleyPoolPredFailure (ConwayEra c) where
  injectFailure = ZonesFailure . injectFailure

instance InjectRuleFailure "BBODY" ConwayDelegPredFailure (ConwayEra c) where
  injectFailure = ZonesFailure . injectFailure

newtype ConwayBbodyEvent era
  = LedgersEvent (Event (EraRule "ZONES" era))

deriving stock instance
  ( Era era
  , Show (PredicateFailure (EraRule "LEDGERS" era))
  , Show (PredicateFailure (EraRule "ZONES" era))
  ) =>
  Show (ConwayBbodyPredFailure era)

deriving stock instance
  ( Era era
  , Eq (PredicateFailure (EraRule "LEDGERS" era))
  , Eq (PredicateFailure (EraRule "ZONES" era))
  ) =>
  Eq (ConwayBbodyPredFailure era)

instance
  ( Era era
  , NoThunks (PredicateFailure (EraRule "LEDGERS" era))
  , NoThunks (PredicateFailure (EraRule "ZONES" era))
  ) =>
  NoThunks (ConwayBbodyPredFailure era)

instance
  ( EraSegWits era
  , DSignable (EraCrypto era) (Hash (EraCrypto era) EraIndependentTxBody)
  , Embed (EraRule "ZONES" era) (ConwayBBODY era)
  , Environment (EraRule "ZONES" era) ~ ShelleyLedgersEnv era
  , Signal (EraRule "ZONES" era) ~ Seq (Seq (Tx era))
  , Eq (PredicateFailure (EraRule "LEDGERS" era))
  , Show (PredicateFailure (EraRule "LEDGERS" era))
  ) =>
  STS (ConwayBBODY era)
  where
  type State (ConwayBBODY era) = ConwayBbodyState era

  type Signal (ConwayBBODY era) = Block (BHeaderView (EraCrypto era)) era

  type Environment (ConwayBBODY era) = BbodyEnv era

  type BaseM (ConwayBBODY era) = ShelleyBase

  type PredicateFailure (ConwayBBODY era) = ConwayBbodyPredFailure era

  type Event (ConwayBBODY era) = ConwayBbodyEvent era

  initialRules = []
  transitionRules = [bbodyTransition]

bbodyTransition ::
  forall era.
  ( STS (ConwayBBODY era)
  , EraSegWits era
  , Embed (EraRule "ZONES" era) (ConwayBBODY era)
  , Environment (EraRule "ZONES" era) ~ ShelleyLedgersEnv era
  , Signal (EraRule "ZONES" era) ~ Seq (Seq (Tx era))
  ) =>
  TransitionRule (ConwayBBODY era)
bbodyTransition =
  judgmentContext
    >>= \( TRC
            ( BbodyEnv pp account
              , BbodyState ls b
              , UnserialisedBlock bhview txsSeq
              )
          ) -> do
        let txs = fromTxZones txsSeq
            actualBodySize = bBodySize (pp ^. ppProtocolVersionL) txsSeq
            actualBodyHash = hashTxZones txsSeq

        actualBodySize
          == fromIntegral (bhviewBSize bhview)
            ?! WrongBlockBodySizeBBODY actualBodySize (fromIntegral $ bhviewBSize bhview)

        actualBodyHash
          == bhviewBHash bhview
            ?! InvalidBodyHashBBODY actualBodyHash (bhviewBHash bhview)

        ls' <-
          trans @(EraRule "ZONES" era) $
            TRC
              ( LedgersEnv (bhviewSlot bhview) pp account
              , ls
              , StrictSeq.fromStrict <$> StrictSeq.fromStrict txs
              )

        -- Note that this may not actually be a stake pool - it could be a genesis key
        -- delegate. However, this would only entail an overhead of 7 counts, and it's
        -- easier than differentiating here.
        let hkAsStakePool = coerceKeyRole $ bhviewID bhview
            slot = bhviewSlot bhview
        firstSlotNo <- liftSTS $ do
          ei <- asks epochInfoPure
          e <- epochInfoEpoch ei slot
          epochInfoFirst ei e
        let isOverlay = isOverlaySlot firstSlotNo (pp ^. ppDG) slot
        pure $ BbodyState ls' (incrBlocks isOverlay hkAsStakePool b)

instance
  forall era zones.
  ( Era era
  , BaseM zones ~ ShelleyBase
  , zones ~ EraRule "ZONES" era
  , STS zones
  , DSignable (EraCrypto era) (Hash (EraCrypto era) EraIndependentTxBody)
  , Era era
  ) =>
  Embed zones (ConwayBBODY era)
  where
  wrapFailed = ZonesFailure
  wrapEvent = LedgersEvent
