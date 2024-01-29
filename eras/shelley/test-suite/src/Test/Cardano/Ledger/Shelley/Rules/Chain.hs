{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Test.Cardano.Ledger.Shelley.Rules.Chain (
  CHAIN,
  ChainState (..),
  TestChainPredicateFailure (..),
  ChainEvent (..),
  PredicateFailure,
  AdaPots (..),
  initialShelleyState,
  totalAda,
  totalAdaPots,
) where

import Cardano.Ledger.BHeaderView (BHeaderView)
import Cardano.Ledger.BaseTypes (
  BlocksMade (..),
  Globals (..),
  Nonce (..),
  ShelleyBase,
  StrictMaybe (..),
 )
import Cardano.Ledger.Binary (EncCBORGroup)
import Cardano.Ledger.Block (Block (..))
import Cardano.Ledger.CertState (VState (..))
import Cardano.Ledger.Chain (
  ChainPredicateFailure (..),
  chainChecks,
  pparamsToChainChecksPParams,
 )
import Cardano.Ledger.Coin (Coin (..))
import qualified Cardano.Ledger.Core as Core
import Cardano.Ledger.EpochBoundary (emptySnapShots)
import Cardano.Ledger.Keys (
  GenDelegPair (..),
  GenDelegs (..),
  KeyHash,
  KeyRole (..),
  coerceKeyRole,
 )
import Cardano.Ledger.PoolDistr (PoolDistr (..))
import Cardano.Ledger.Shelley (ShelleyEra)
import Cardano.Ledger.Shelley.AdaPots (
  AdaPots (..),
  totalAdaES,
  totalAdaPotsES,
 )
import Cardano.Ledger.Shelley.Core
import Cardano.Ledger.Shelley.LedgerState (
  AccountState (..),
  CertState (..),
  DState (..),
  EpochState (..),
  LedgerState (..),
  NewEpochState (..),
  PState (..),
  StashedAVVMAddresses,
  curPParamsEpochStateL,
  dsGenDelegs,
  nesEpochStateL,
  prevPParamsEpochStateL,
  smartUTxOState,
  updateNES,
 )
import Cardano.Ledger.Shelley.Rules (
  BbodyEnv (..),
  ShelleyBBODY,
  ShelleyBbodyPredFailure,
  ShelleyBbodyState (..),
  ShelleyTICK,
  ShelleyTickEvent,
  ShelleyTickPredFailure,
 )
import Cardano.Ledger.Slot (EpochNo)
import qualified Cardano.Ledger.UMap as UM
import Cardano.Ledger.UTxO (UTxO (..))
import Cardano.Protocol.TPraos.BHeader (
  BHeader,
  HashHeader,
  LastAppliedBlock (..),
  bhHash,
  bhbody,
  bheaderBlockNo,
  bheaderSlotNo,
  lastAppliedHash,
  makeHeaderView,
  prevHashToNonce,
 )
import Cardano.Protocol.TPraos.Rules.Prtcl (
  PRTCL,
  PrtclEnv (..),
  PrtclState (..),
  PrtlSeqFailure,
  prtlSeqChecks,
 )
import Cardano.Protocol.TPraos.Rules.Tickn
import Cardano.Slotting.Slot (SlotNo, WithOrigin (..))
import Control.DeepSeq (NFData)
import Control.Monad.Trans.Reader (asks)
import Control.State.Transition (
  Embed (..),
  STS (..),
  TRC (..),
  TransitionRule,
  failBecause,
  judgmentContext,
  liftSTS,
  trans,
 )
import Data.Default.Class (Default, def)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Void (Void)
import Data.Word (Word64)
import GHC.Generics (Generic)
import Lens.Micro ((&), (.~), (^.))
import NoThunks.Class (NoThunks (..))
import Test.Cardano.Ledger.Shelley.TreeDiff ()
import Test.Cardano.Ledger.TreeDiff (ToExpr (toExpr), defaultExprViaShow)

type instance EraRule "TICKN" (ShelleyEra c) = TICKN

data CHAIN era

data ChainState era = ChainState
  { chainNes :: NewEpochState era
  , chainOCertIssue :: Map.Map (KeyHash 'BlockIssuer (EraCrypto era)) Word64
  , chainEpochNonce :: Nonce
  , chainEvolvingNonce :: Nonce
  , chainCandidateNonce :: Nonce
  , chainPrevEpochNonce :: Nonce
  , chainLastAppliedBlock :: WithOrigin (LastAppliedBlock (EraCrypto era))
  }
  deriving (Generic)

deriving stock instance Show (NewEpochState era) => Show (ChainState era)

deriving stock instance Eq (NewEpochState era) => Eq (ChainState era)

instance NFData (NewEpochState era) => NFData (ChainState era)

data TestChainPredicateFailure era
  = RealChainPredicateFailure !ChainPredicateFailure
  | BbodyFailure !(PredicateFailure (EraRule "BBODY" era)) -- Subtransition Failures
  | TickFailure !(PredicateFailure (EraRule "TICK" era)) -- Subtransition Failures
  | TicknFailure !(PredicateFailure (EraRule "TICKN" era)) -- Subtransition Failures
  | PrtclFailure !(PredicateFailure (PRTCL (EraCrypto era))) -- Subtransition Failures
  | PrtclSeqFailure !(PrtlSeqFailure (EraCrypto era)) -- Subtransition Failures
  deriving (Generic)

data ChainEvent era
  = BbodyEvent !(Event (EraRule "BBODY" era))
  | TickEvent !(Event (EraRule "TICK" era))
  | TicknEvent !(Event (EraRule "TICKN" era))
  | PrtclEvent !(Event (PRTCL (EraCrypto era)))

deriving stock instance
  ( Era era
  , Show (PredicateFailure (EraRule "BBODY" era))
  , Show (PredicateFailure (EraRule "TICK" era))
  , Show (PredicateFailure (EraRule "TICKN" era))
  ) =>
  Show (TestChainPredicateFailure era)

deriving stock instance
  ( Era era
  , Eq (PredicateFailure (EraRule "BBODY" era))
  , Eq (PredicateFailure (EraRule "TICK" era))
  , Eq (PredicateFailure (EraRule "TICKN" era))
  ) =>
  Eq (TestChainPredicateFailure era)

instance
  ( Era era
  , NoThunks (PredicateFailure (EraRule "BBODY" era))
  , NoThunks (PredicateFailure (EraRule "TICK" era))
  , NoThunks (PredicateFailure (EraRule "TICKN" era))
  ) =>
  NoThunks (TestChainPredicateFailure era)

-- | Creates a valid initial chain state
initialShelleyState ::
  forall era.
  ( EraTxOut era
  , EraGov era
  , Default (StashedAVVMAddresses era)
  ) =>
  WithOrigin (LastAppliedBlock (EraCrypto era)) ->
  EpochNo ->
  UTxO era ->
  Coin ->
  Map (KeyHash 'Genesis (EraCrypto era)) (GenDelegPair (EraCrypto era)) ->
  PParams era ->
  Nonce ->
  ChainState era
initialShelleyState lab e utxo reserves genDelegs pp initNonce =
  ChainState
    ( NewEpochState
        e
        (BlocksMade Map.empty)
        (BlocksMade Map.empty)
        ( EpochState
            (AccountState (Coin 0) reserves)
            ( LedgerState
                ( smartUTxOState
                    pp
                    utxo
                    (Coin 0)
                    (Coin 0)
                    emptyGovState
                    mempty
                )
                (CertState def def dState)
            )
            emptySnapShots
            def
            & curPParamsEpochStateL .~ pp
            & prevPParamsEpochStateL .~ pp
        )
        SNothing
        (PoolDistr Map.empty)
        def
    )
    cs
    initNonce
    initNonce
    initNonce
    NeutralNonce
    lab
  where
    cs =
      Map.fromList
        ( fmap
            (\(GenDelegPair hk _) -> (coerceKeyRole hk, 0))
            (Map.elems genDelegs)
        )

    dState :: DState era
    dState =
      DState
        { dsUnified = UM.empty
        , dsFutureGenDelegs = Map.empty
        , dsGenDelegs = GenDelegs genDelegs
        , dsIRewards = def
        }

instance
  ( EraGov era
  , Embed (EraRule "BBODY" era) (CHAIN era)
  , Environment (EraRule "BBODY" era) ~ BbodyEnv era
  , State (EraRule "BBODY" era) ~ ShelleyBbodyState era
  , Signal (EraRule "BBODY" era) ~ Block (BHeaderView (EraCrypto era)) era
  , Embed (EraRule "TICKN" era) (CHAIN era)
  , Environment (EraRule "TICKN" era) ~ TicknEnv
  , State (EraRule "TICKN" era) ~ TicknState
  , Signal (EraRule "TICKN" era) ~ Bool
  , Embed (EraRule "TICK" era) (CHAIN era)
  , Environment (EraRule "TICK" era) ~ ()
  , State (EraRule "TICK" era) ~ NewEpochState era
  , Signal (EraRule "TICK" era) ~ SlotNo
  , Embed (PRTCL (EraCrypto era)) (CHAIN era)
  , EncCBORGroup (TxSeq era)
  , ProtVerAtMost era 6
  , State (Core.EraRule "LEDGERS" era) ~ LedgerState era
  ) =>
  STS (CHAIN era)
  where
  type
    State (CHAIN era) =
      ChainState era

  type
    Signal (CHAIN era) =
      Block (BHeader (EraCrypto era)) era

  type Environment (CHAIN era) = ()
  type BaseM (CHAIN era) = ShelleyBase

  type PredicateFailure (CHAIN era) = TestChainPredicateFailure era
  type Event (CHAIN era) = ChainEvent era

  initialRules = []
  transitionRules = [chainTransition]

chainTransition ::
  forall era.
  ( STS (CHAIN era)
  , Embed (EraRule "BBODY" era) (CHAIN era)
  , Environment (EraRule "BBODY" era) ~ BbodyEnv era
  , State (EraRule "BBODY" era) ~ ShelleyBbodyState era
  , Signal (EraRule "BBODY" era) ~ Block (BHeaderView (EraCrypto era)) era
  , Embed (EraRule "TICKN" era) (CHAIN era)
  , Environment (EraRule "TICKN" era) ~ TicknEnv
  , State (EraRule "TICKN" era) ~ TicknState
  , Signal (EraRule "TICKN" era) ~ Bool
  , Embed (EraRule "TICK" era) (CHAIN era)
  , Environment (EraRule "TICK" era) ~ ()
  , State (EraRule "TICK" era) ~ NewEpochState era
  , Signal (EraRule "TICK" era) ~ SlotNo
  , Embed (PRTCL (EraCrypto era)) (CHAIN era)
  , EncCBORGroup (TxSeq era)
  , ProtVerAtMost era 6
  , State (Core.EraRule "LEDGERS" era) ~ LedgerState era
  , EraGov era
  ) =>
  TransitionRule (CHAIN era)
chainTransition =
  judgmentContext
    >>= \( TRC
            ( _
              , ChainState
                  nes
                  cs
                  eta0
                  etaV
                  etaC
                  etaH
                  lab
              , Block bh txs
              )
          ) -> do
        case prtlSeqChecks lab bh of
          Right () -> pure ()
          Left e -> failBecause $ PrtclSeqFailure e

        let pp = nes ^. nesEpochStateL . curPParamsEpochStateL
            chainChecksData = pparamsToChainChecksPParams pp
            bhView = makeHeaderView bh

        maxpv <- liftSTS $ asks maxMajorPV
        case chainChecks maxpv chainChecksData bhView of
          Right () -> pure ()
          Left e -> failBecause (RealChainPredicateFailure e)

        let s = bheaderSlotNo $ bhbody bh

        nes' <- trans @(EraRule "TICK" era) $ TRC ((), nes, s)

        let NewEpochState e1 _ _ _ _ _ _ = nes
            NewEpochState e2 _ bcur es _ _pd _ = nes'
        let EpochState account ls _ _ = es
            pp' = es ^. curPParamsEpochStateL
        let LedgerState _ (CertState VState {} PState {} DState {dsGenDelegs = genDelegs}) = ls
        let ph = lastAppliedHash lab
            etaPH = prevHashToNonce ph

        TicknState eta0' etaH' <-
          trans @(EraRule "TICKN" era) $
            TRC
              ( TicknEnv (pp' ^. ppExtraEntropyL) etaC etaPH
              , TicknState eta0 etaH
              , e1 /= e2
              )

        PrtclState cs' etaV' etaC' <-
          trans @(PRTCL (EraCrypto era)) $
            TRC
              ( PrtclEnv (pp' ^. ppDL) _pd genDelegs eta0'
              , PrtclState cs etaV etaC
              , bh
              )

        let thouShaltNot = error "A block with a header view should never be hashed"
        BbodyState ls' bcur' <-
          trans @(EraRule "BBODY" era) $
            TRC (BbodyEnv pp' account, BbodyState ls bcur, Block' bhView txs thouShaltNot)

        let nes'' = updateNES nes' bcur' ls'
            bhb = bhbody bh
            lab' =
              At $
                LastAppliedBlock
                  (bheaderBlockNo bhb)
                  (bheaderSlotNo bhb)
                  (bhHash bh)

        pure $ ChainState nes'' cs' eta0' etaV' etaC' etaH' lab'

instance
  ( Era era
  , Era era
  , STS (ShelleyBBODY era)
  , PredicateFailure (EraRule "BBODY" era) ~ ShelleyBbodyPredFailure era
  , Event (EraRule "BBODY" era) ~ Event (ShelleyBBODY era)
  ) =>
  Embed (ShelleyBBODY era) (CHAIN era)
  where
  wrapFailed = BbodyFailure
  wrapEvent = BbodyEvent

instance
  ( Era era
  , Era era
  , PredicateFailure (EraRule "TICKN" era) ~ TicknPredicateFailure
  , Event (EraRule "TICKN" era) ~ Void
  ) =>
  Embed TICKN (CHAIN era)
  where
  wrapFailed = TicknFailure
  wrapEvent = TicknEvent

instance
  ( Era era
  , Era era
  , STS (ShelleyTICK era)
  , PredicateFailure (EraRule "TICK" era) ~ ShelleyTickPredFailure era
  , Event (EraRule "TICK" era) ~ ShelleyTickEvent era
  ) =>
  Embed (ShelleyTICK era) (CHAIN era)
  where
  wrapFailed = TickFailure
  wrapEvent = TickEvent

instance
  ( Era era
  , c ~ EraCrypto era
  , Era era
  , STS (PRTCL c)
  ) =>
  Embed (PRTCL c) (CHAIN era)
  where
  wrapFailed = PrtclFailure
  wrapEvent = PrtclEvent

-- | Calculate the total ada pots in the chain state
totalAdaPots ::
  ( EraTxOut era
  , EraGov era
  ) =>
  ChainState era ->
  AdaPots
totalAdaPots = totalAdaPotsES . nesEs . chainNes

-- | Calculate the total ada in the chain state
totalAda :: (EraTxOut era, EraGov era) => ChainState era -> Coin
totalAda = totalAdaES . nesEs . chainNes

instance
  ( ToExpr (PParams era)
  , ToExpr (TxOut era)
  , ToExpr (StashedAVVMAddresses era)
  , ToExpr (GovState era)
  ) =>
  ToExpr (ChainState era)

instance ToExpr (HashHeader c) where
  toExpr = defaultExprViaShow

instance ToExpr (LastAppliedBlock c)
