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
import Data.Sequence (Seq, fromList)
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

-- newtype ConwayZonesPredFailure era
--   = ZoneFailure (PredicateFailure (EraRule "ZONE" era)) -- Subtransition Failures
--   deriving (Generic)

-- newtype ConwayZonePredFailure era
--   = LedgersFailure (PredicateFailure (EraRule "LEDGERS" era)) -- Subtransition Failures
--   deriving (Generic)

type instance EraRuleFailure "BBODY" (ConwayEra c) = ConwayBbodyPredFailure (ConwayEra c)

instance InjectRuleFailure "BBODY" ConwayBbodyPredFailure (ConwayEra c)

instance InjectRuleFailure "BBODY" ConwayZonesPredFailure (ConwayEra c) where
  injectFailure :: ConwayZonesPredFailure (ConwayEra c) -> ConwayBbodyPredFailure (ConwayEra c)
  injectFailure = undefined -- ZonesFailure . injectFailure

instance InjectRuleFailure "BBODY" ConwayZonePredFailure (ConwayEra c) where
  injectFailure = undefined -- ZonesFailure . injectFailure

-- instance InjectRuleFailure "BBODY" ShelleyLedgersPredFailure (ConwayEra c) where
--   injectFailure = ShelleyInAlonzoBbodyPredFailure . LedgersFailure

instance InjectRuleFailure "BBODY" ConwayLedgerPredFailure (ConwayEra c) where
  injectFailure = undefined -- ZonesFailure . injectFailure

instance InjectRuleFailure "BBODY" ConwayUtxowPredFailure (ConwayEra c) where
  injectFailure = undefined -- ZonesFailure . injectFailure

instance InjectRuleFailure "BBODY" ConwayUtxoPredFailure (ConwayEra c) where
  injectFailure = undefined -- ZonesFailure . injectFailure

instance InjectRuleFailure "BBODY" ShelleyPoolPredFailure (ConwayEra c) where
  injectFailure = undefined -- ZonesFailure . injectFailure

instance InjectRuleFailure "BBODY" ConwayDelegPredFailure (ConwayEra c) where
  injectFailure = undefined -- ZonesFailure . injectFailure

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
  type
    State (ConwayBBODY era) =
      ConwayBbodyState era

  type
    Signal (ConwayBBODY era) =
      Block (BHeaderView (EraCrypto era)) era

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
        let txs = fromTxSeq txsSeq
            actualBodySize = bBodySize (pp ^. ppProtocolVersionL) txsSeq
            actualBodyHash = hashTxSeq txsSeq

        actualBodySize
          == fromIntegral (bhviewBSize bhview)
            ?! WrongBlockBodySizeBBODY actualBodySize (fromIntegral $ bhviewBSize bhview)

        actualBodyHash
          == bhviewBHash bhview
            ?! InvalidBodyHashBBODY actualBodyHash (bhviewBHash bhview)

        ls' <-
          trans @(EraRule "ZONES" era) $
            TRC (LedgersEnv (bhviewSlot bhview) pp account, ls, fromList [StrictSeq.fromStrict txs]) -- TODO WG obviously when we have incoming zones this can be a seq of seqs

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

-- ========================================
-- -- The STS instance

-- bbodyTransition ::
--   forall (someBBODY :: Type -> Type) era.
--   ( STS (someBBODY era)
--   , Signal (someBBODY era) ~ Block (BHeaderView (EraCrypto era)) era
--   , PredicateFailure (someBBODY era) ~ ConwayBbodyPredFailure era
--   , BaseM (someBBODY era) ~ ConwayBase
--   , State (someBBODY era) ~ ConwayBbodyState era
--   , Environment (someBBODY era) ~ BbodyEnv era
--   , Embed (EraRule "ZONES" era) (someBBODY era)
--   , Environment (EraRule "ZONES" era) ~ ConwayLedgersEnv era
--   , State (EraRule "ZONES" era) ~ LedgerState era
--   , Signal (EraRule "ZONES" era) ~ Seq (Tx era)
--   , EraSegWits era
--   , AlonzoEraTxWits era
--   , Era.TxSeq era ~ AlonzoTxSeq era
--   , Tx era ~ AlonzoTx era
--   , AlonzoEraPParams era
--   ) =>
--   TransitionRule (someBBODY era)
-- bbodyTransition =
--   judgmentContext
--     >>= \( TRC
--             ( BbodyEnv pp account
--               , BbodyState ls b
--               , UnserialisedBlock bh txsSeq
--               )
--           ) -> do
--         let txs = txSeqTxns txsSeq
--             actualBodySize = bBodySize (pp ^. ppProtocolVersionL) txsSeq
--             actualBodyHash = hashTxSeq @era txsSeq

--         actualBodySize
--           == fromIntegral (bhviewBSize bh)
--             ?! ConwayInAlonzoBbodyPredFailure
--               ( WrongBlockBodySizeBBODY actualBodySize (fromIntegral $ bhviewBSize bh)
--               )

--         actualBodyHash
--           == bhviewBHash bh
--             ?! ConwayInAlonzoBbodyPredFailure
--               ( InvalidBodyHashBBODY @era actualBodyHash (bhviewBHash bh)
--               )

--         ls' <-
--           trans @(EraRule "ZONES" era) $
--             TRC (LedgersEnv (bhviewSlot bh) pp account, ls, StrictSeq.fromStrict txs)

--         -- Note that this may not actually be a stake pool - it could be a
--         -- genesis key delegate. However, this would only entail an overhead of
--         -- 7 counts, and it's easier than differentiating here.
--         --
--         -- TODO move this computation inside 'incrBlocks' where it belongs. Here
--         -- we make an assumption that 'incrBlocks' must enforce, better for it
--         -- to be done in 'incrBlocks' where we can see that the assumption is
--         -- enforced.
--         let hkAsStakePool = coerceKeyRole . bhviewID $ bh
--             slot = bhviewSlot bh
--         firstSlotNo <- liftSTS $ do
--           ei <- asks epochInfoPure
--           e <- epochInfoEpoch ei slot
--           epochInfoFirst ei e

--         {- ∑(tx ∈ txs)(totExunits tx) ≤ maxBlockExUnits pp  -}
--         let txTotal, ppMax :: ExUnits
--             txTotal = foldMap totExUnits txs
--             ppMax = pp ^. ppMaxBlockExUnitsL
--         pointWiseExUnits (<=) txTotal ppMax ?! TooManyExUnits txTotal ppMax

--         pure $
--           BbodyState @era
--             ls'
--             ( incrBlocks
--                 (isOverlaySlot firstSlotNo (pp ^. ppDG) slot)
--                 hkAsStakePool
--                 b
--             )

-- instance
--   ( DSignable (EraCrypto era) (Hash (EraCrypto era) EraIndependentTxBody)
--   , Embed (EraRule "ZONES" era) (AlonzoBBODY era)
--   , Environment (EraRule "ZONES" era) ~ ConwayLedgersEnv era
--   , State (EraRule "ZONES" era) ~ LedgerState era
--   , Signal (EraRule "ZONES" era) ~ Seq (AlonzoTx era)
--   , AlonzoEraTxWits era
--   , Tx era ~ AlonzoTx era
--   , Era.TxSeq era ~ AlonzoTxSeq era
--   , Tx era ~ AlonzoTx era
--   , EraSegWits era
--   , AlonzoEraPParams era
--   ) =>
--   STS (ConwayBBODY era)
--   where
--   type
--     State (ConwayBBODY era) =
--       ConwayBbodyState era

--   type
--     Signal (ConwayBBODY era) =
--       (Block (BHeaderView (EraCrypto era)) era)

--   type Environment (ConwayBBODY era) = BbodyEnv era

--   type BaseM (ConwayBBODY era) = ConwayBase

--   type PredicateFailure (ConwayBBODY era) = ConwayBbodyPredFailure era
--   type Event (ConwayBBODY era) = AlonzoBbodyEvent era

--   initialRules = []
--   transitionRules = [bbodyTransition @ConwayBBODY]

-- instance
--   ( Era era
--   , BaseM zones ~ ConwayBase
--   , zones ~ EraRule "ZONES" era
--   , STS zones
--   , DSignable (EraCrypto era) (Hash (EraCrypto era) EraIndependentTxBody)
--   , Era era
--   ) =>
--   Embed zones (ConwayBBODY era)
--   where
--   wrapFailed = LedgersFailure
--   wrapEvent = LedgersEvent

-- data ConwayBbodyPredFailure era
--   = WrongBlockBodySizeBBODY
--       !Int -- Actual Body Size
--       !Int -- Claimed Body Size in Header
--   | InvalidBodyHashBBODY
--       !(Hash (EraCrypto era) EraIndependentBlockBody) -- Actual Hash
--       !(Hash (EraCrypto era) EraIndependentBlockBody) -- Claimed Hash
--   | LedgersFailure (PredicateFailure (EraRule "ZONES" era)) -- Subtransition Failures
--   deriving (Generic)

-- newtype ConwayBbodyEvent era
--   = LedgersEvent (Event (EraRule "ZONES" era))

-- type instance EraRuleFailure "BBODY" (ConwayEra c) = ConwayBbodyPredFailure (ConwayEra c)

-- type instance EraRuleEvent "BBODY" (ConwayEra c) = ConwayBbodyEvent (ConwayEra c)

-- instance InjectRuleFailure "BBODY" ConwayBbodyPredFailure (ConwayEra c)

-- instance InjectRuleFailure "BBODY" ConwayBbodyPredFailure (ConwayEra c) where
--   injectFailure = ConwayInAlonzoBbodyPredFailure

-- instance InjectRuleFailure "BBODY" ConwayLedgersPredFailure (ConwayEra c) where
--   injectFailure = ConwayInAlonzoBbodyPredFailure . LedgersFailure

-- instance InjectRuleFailure "BBODY" ConwayLedgerPredFailure (ConwayEra c) where
--   injectFailure = ConwayInAlonzoBbodyPredFailure . LedgersFailure . injectFailure

-- instance InjectRuleFailure "BBODY" ConwayUtxowPredFailure (ConwayEra c) where
--   injectFailure = ConwayInAlonzoBbodyPredFailure . LedgersFailure . injectFailure

-- instance InjectRuleFailure "BBODY" BabbageUtxowPredFailure (ConwayEra c) where
--   injectFailure = ConwayInAlonzoBbodyPredFailure . LedgersFailure . injectFailure

-- instance InjectRuleFailure "BBODY" AlonzoUtxowPredFailure (ConwayEra c) where
--   injectFailure = ConwayInAlonzoBbodyPredFailure . LedgersFailure . injectFailure

-- instance InjectRuleFailure "BBODY" ConwayUtxowPredFailure (ConwayEra c) where
--   injectFailure = ConwayInAlonzoBbodyPredFailure . LedgersFailure . injectFailure

-- instance InjectRuleFailure "BBODY" ConwayUtxoPredFailure (ConwayEra c) where
--   injectFailure = ConwayInAlonzoBbodyPredFailure . LedgersFailure . injectFailure

-- instance InjectRuleFailure "BBODY" BabbageUtxoPredFailure (ConwayEra c) where
--   injectFailure = ConwayInAlonzoBbodyPredFailure . LedgersFailure . injectFailure

-- instance InjectRuleFailure "BBODY" AlonzoUtxoPredFailure (ConwayEra c) where
--   injectFailure = ConwayInAlonzoBbodyPredFailure . LedgersFailure . injectFailure

-- instance InjectRuleFailure "BBODY" AlonzoUtxosPredFailure (ConwayEra c) where
--   injectFailure = ConwayInAlonzoBbodyPredFailure . LedgersFailure . injectFailure

-- instance InjectRuleFailure "BBODY" ConwayUtxosPredFailure (ConwayEra c) where
--   injectFailure = ConwayInAlonzoBbodyPredFailure . LedgersFailure . injectFailure

-- instance InjectRuleFailure "BBODY" ConwayUtxoPredFailure (ConwayEra c) where
--   injectFailure = ConwayInAlonzoBbodyPredFailure . LedgersFailure . injectFailure

-- instance InjectRuleFailure "BBODY" AllegraUtxoPredFailure (ConwayEra c) where
--   injectFailure = ConwayInAlonzoBbodyPredFailure . LedgersFailure . injectFailure

-- instance InjectRuleFailure "BBODY" ConwayCertsPredFailure (ConwayEra c) where
--   injectFailure = ConwayInAlonzoBbodyPredFailure . LedgersFailure . injectFailure

-- instance InjectRuleFailure "BBODY" ConwayCertPredFailure (ConwayEra c) where
--   injectFailure = ConwayInAlonzoBbodyPredFailure . LedgersFailure . injectFailure

-- instance InjectRuleFailure "BBODY" ConwayDelegPredFailure (ConwayEra c) where
--   injectFailure = ConwayInAlonzoBbodyPredFailure . LedgersFailure . injectFailure

-- instance InjectRuleFailure "BBODY" ConwayPoolPredFailure (ConwayEra c) where
--   injectFailure = ConwayInAlonzoBbodyPredFailure . LedgersFailure . injectFailure

-- instance InjectRuleFailure "BBODY" ConwayGovCertPredFailure (ConwayEra c) where
--   injectFailure = ConwayInAlonzoBbodyPredFailure . LedgersFailure . injectFailure

-- instance InjectRuleFailure "BBODY" ConwayGovPredFailure (ConwayEra c) where
--   injectFailure = ConwayInAlonzoBbodyPredFailure . LedgersFailure . injectFailure
