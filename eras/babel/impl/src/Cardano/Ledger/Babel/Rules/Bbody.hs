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

module Cardano.Ledger.Babel.Rules.Bbody where

import Cardano.Ledger.BHeaderView (
  BHeaderView (bhviewBHash, bhviewBSize, bhviewID, bhviewSlot),
  isOverlaySlot,
 )
import Cardano.Ledger.Babel.Era (BabelBBODY, BabelEra)
import Cardano.Ledger.Babel.Rules.Ledger (BabelLedgerPredFailure)
import Cardano.Ledger.Babel.Rules.Ledgers ()
import Cardano.Ledger.Babel.Rules.Utxo (BabelUtxoPredFailure)
import Cardano.Ledger.Babel.Rules.Utxow (BabelUtxowPredFailure)
import Cardano.Ledger.Babel.Rules.Zone (BabelZonePredFailure)
import Cardano.Ledger.Babel.Rules.Zones (BabelZonesPredFailure)
import Cardano.Ledger.BaseTypes (ShelleyBase, epochInfoPure)
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
  ShelleyBbodyState (BbodyState),
  ShelleyLedgersPredFailure,
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
import Data.Functor.Compose (Compose, getCompose)
import Data.Sequence (Seq)
import qualified Data.Sequence.Strict as StrictSeq
import GHC.Generics (Generic)
import Lens.Micro ((^.))
import NoThunks.Class (NoThunks)

data BabelBbodyPredFailure era
  = WrongBlockBodySizeBBODY
      !Int -- Actual Body Size
      !Int -- Claimed Body Size in Header
  | InvalidBodyHashBBODY
      !(Hash (EraCrypto era) EraIndependentBlockBody) -- Actual Hash
      !(Hash (EraCrypto era) EraIndependentBlockBody) -- Claimed Hash
  | ZonesFailure (PredicateFailure (EraRule "ZONES" era)) -- Subtransition Failures
  | ShellyInBabelBbodyPredFailure (ShelleyBbodyPredFailure era)
  deriving (Generic)

type instance EraRuleFailure "BBODY" (BabelEra c) = BabelBbodyPredFailure (BabelEra c)

instance InjectRuleFailure "BBODY" BabelBbodyPredFailure (BabelEra c)

instance InjectRuleFailure "BBODY" BabelZonesPredFailure (BabelEra c) where
  injectFailure :: BabelZonesPredFailure (BabelEra c) -> BabelBbodyPredFailure (BabelEra c)
  injectFailure = ZonesFailure

instance InjectRuleFailure "BBODY" BabelZonePredFailure (BabelEra c) where
  injectFailure = ZonesFailure . injectFailure

instance InjectRuleFailure "BBODY" ShelleyLedgersPredFailure (BabelEra c) where
  injectFailure = ZonesFailure . injectFailure

instance InjectRuleFailure "BBODY" BabelLedgerPredFailure (BabelEra c) where
  injectFailure = ZonesFailure . injectFailure

instance InjectRuleFailure "BBODY" BabelUtxowPredFailure (BabelEra c) where
  injectFailure = ZonesFailure . injectFailure

instance InjectRuleFailure "BBODY" BabelUtxoPredFailure (BabelEra c) where
  injectFailure = ZonesFailure . injectFailure

newtype BabelBbodyEvent era
  = LedgersEvent (Event (EraRule "ZONES" era))

deriving stock instance
  ( Era era
  , Show (PredicateFailure (EraRule "LEDGERS" era))
  , Show (PredicateFailure (EraRule "ZONES" era))
  ) =>
  Show (BabelBbodyPredFailure era)

deriving stock instance
  ( Era era
  , Eq (PredicateFailure (EraRule "LEDGERS" era))
  , Eq (PredicateFailure (EraRule "ZONES" era))
  ) =>
  Eq (BabelBbodyPredFailure era)

instance
  ( Era era
  , NoThunks (PredicateFailure (EraRule "LEDGERS" era))
  , NoThunks (PredicateFailure (EraRule "ZONES" era))
  ) =>
  NoThunks (BabelBbodyPredFailure era)

instance
  ( TxStructure era ~ Compose StrictSeq.StrictSeq StrictSeq.StrictSeq
  , EraSegWits era
  , DSignable (EraCrypto era) (Hash (EraCrypto era) EraIndependentTxBody)
  , Embed (EraRule "ZONES" era) (BabelBBODY era)
  , Environment (EraRule "ZONES" era) ~ ShelleyLedgersEnv era
  , Signal (EraRule "ZONES" era) ~ Seq (Seq (Tx era))
  , Eq (PredicateFailure (EraRule "LEDGERS" era))
  , Show (PredicateFailure (EraRule "LEDGERS" era))
  , State (EraRule "LEDGERS" era) ~ State (EraRule "ZONES" era)
  ) =>
  STS (BabelBBODY era)
  where
  type State (BabelBBODY era) = ShelleyBbodyState era

  type Signal (BabelBBODY era) = Block (BHeaderView (EraCrypto era)) era

  type Environment (BabelBBODY era) = BbodyEnv era

  type BaseM (BabelBBODY era) = ShelleyBase

  type PredicateFailure (BabelBBODY era) = BabelBbodyPredFailure era

  type Event (BabelBBODY era) = BabelBbodyEvent era

  initialRules = []
  transitionRules = [bbodyTransition]

bbodyTransition ::
  forall era.
  ( TxStructure era ~ Compose StrictSeq.StrictSeq StrictSeq.StrictSeq
  , STS (BabelBBODY era)
  , EraSegWits era
  , Embed (EraRule "ZONES" era) (BabelBBODY era)
  , Environment (EraRule "ZONES" era) ~ ShelleyLedgersEnv era
  , Signal (EraRule "ZONES" era) ~ Seq (Seq (Tx era))
  , State (EraRule "LEDGERS" era) ~ State (EraRule "ZONES" era)
  ) =>
  TransitionRule (BabelBBODY era)
bbodyTransition =
  judgmentContext
    >>= \( TRC
            ( BbodyEnv pp account
              , BbodyState ls b
              , UnserialisedBlock bhview txsSeq
              )
          ) -> do
        let
          txs :: StrictSeq.StrictSeq (StrictSeq.StrictSeq (Tx era))
          txs = getCompose $ fromTxZones txsSeq
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
  Embed zones (BabelBBODY era)
  where
  wrapFailed = ZonesFailure
  wrapEvent = LedgersEvent
