{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Shelley.Spec.Ledger.STS.Chain
  ( CHAIN,
    ChainState (..),
    ChainPredicateFailure (..),
    PredicateFailure,
    initialShelleyState,
    totalAda,
    totalAdaES,
    totalAdaPots,
    ChainChecksData (..),
    pparamsToChainChecksData,
    chainChecks,
  )
where

import qualified Cardano.Ledger.Core as Core
import Cardano.Ledger.Era (Crypto, Era)
import Cardano.Ledger.Shelley.Constraints (UsesTxOut, UsesValue)
import qualified Cardano.Ledger.Val as Val
import Cardano.Slotting.Slot (SlotNo, WithOrigin (..))
import Control.DeepSeq (NFData)
import Control.Monad (unless)
import Control.Monad.Except (MonadError, throwError)
import Control.Monad.Trans.Reader (asks)
import Control.State.Transition
  ( Embed (..),
    STS (..),
    TRC (..),
    TransitionRule,
    failBecause,
    judgmentContext,
    liftSTS,
    trans,
  )
import Data.Default.Class (Default, def)
import Data.Foldable (fold)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Word (Word64)
import GHC.Generics (Generic)
import GHC.Records
import NoThunks.Class (NoThunks (..))
import Numeric.Natural (Natural)
import Shelley.Spec.Ledger.BaseTypes
  ( Globals (..),
    Nonce (..),
    ShelleyBase,
    StrictMaybe (..),
    UnitInterval,
  )
import Shelley.Spec.Ledger.BlockChain
  ( BHeader,
    Block (..),
    LastAppliedBlock (..),
    bHeaderSize,
    bhHash,
    bhbody,
    bheaderBlockNo,
    bheaderSlotNo,
    hBbsize,
    lastAppliedHash,
    prevHashToNonce,
  )
import Shelley.Spec.Ledger.Coin (Coin (..))
import Shelley.Spec.Ledger.Delegation.Certificates (PoolDistr (..))
import Shelley.Spec.Ledger.EpochBoundary (BlocksMade (..), emptySnapShots)
import Shelley.Spec.Ledger.Keys
  ( GenDelegPair (..),
    GenDelegs (..),
    KeyHash,
    KeyRole (..),
    coerceKeyRole,
  )
import Shelley.Spec.Ledger.LedgerState
  ( AccountState (..),
    DPState (..),
    DState (..),
    EpochState (..),
    LedgerState (..),
    NewEpochState (..),
    PState (..),
    TransUTxOState,
    UTxOState (..),
    updateNES,
    _genDelegs,
  )
import Shelley.Spec.Ledger.PParams
  ( PParams,
    ProtVer (..),
  )
import Shelley.Spec.Ledger.STS.Bbody (BBODY, BbodyEnv (..), BbodyPredicateFailure, BbodyState (..))
import Shelley.Spec.Ledger.STS.Prtcl
  ( PRTCL,
    PrtclEnv (..),
    PrtclState (..),
    PrtlSeqFailure,
    prtlSeqChecks,
  )
import Shelley.Spec.Ledger.STS.Tick (TICK, TickPredicateFailure)
import Shelley.Spec.Ledger.STS.Tickn
import Shelley.Spec.Ledger.Slot (EpochNo)
import Shelley.Spec.Ledger.UTxO (UTxO (..), balance)

data CHAIN era

data ChainState era = ChainState
  { chainNes :: NewEpochState era,
    chainOCertIssue :: Map.Map (KeyHash 'BlockIssuer (Crypto era)) Word64,
    chainEpochNonce :: Nonce,
    chainEvolvingNonce :: Nonce,
    chainCandidateNonce :: Nonce,
    chainPrevEpochNonce :: Nonce,
    chainLastAppliedBlock :: WithOrigin (LastAppliedBlock (Crypto era))
  }
  deriving (Generic)

deriving stock instance
  TransUTxOState Show era =>
  Show (ChainState era)

deriving stock instance
  TransUTxOState Eq era =>
  Eq (ChainState era)

instance (Era era, TransUTxOState NFData era) => NFData (ChainState era)

data ChainPredicateFailure era
  = HeaderSizeTooLargeCHAIN
      !Natural -- Header Size
      !Natural -- Max Header Size
  | BlockSizeTooLargeCHAIN
      !Natural -- Block Size
      !Natural -- Max Block Size
  | ObsoleteNodeCHAIN
      !Natural -- protocol version used
      !Natural -- max protocol version
  | BbodyFailure !(PredicateFailure (Core.EraRule "BBODY" era)) -- Subtransition Failures
  | TickFailure !(PredicateFailure (Core.EraRule "TICK" era)) -- Subtransition Failures
  | TicknFailure !(PredicateFailure (Core.EraRule "TICKN" era)) -- Subtransition Failures
  | PrtclFailure !(PredicateFailure (PRTCL (Crypto era))) -- Subtransition Failures
  | PrtclSeqFailure !(PrtlSeqFailure (Crypto era)) -- Subtransition Failures
  deriving (Generic)

deriving stock instance
  ( Era era,
    Show (PredicateFailure (Core.EraRule "BBODY" era)),
    Show (PredicateFailure (Core.EraRule "TICK" era)),
    Show (PredicateFailure (Core.EraRule "TICKN" era))
  ) =>
  Show (ChainPredicateFailure era)

deriving stock instance
  ( Era era,
    Eq (PredicateFailure (Core.EraRule "BBODY" era)),
    Eq (PredicateFailure (Core.EraRule "TICK" era)),
    Eq (PredicateFailure (Core.EraRule "TICKN" era))
  ) =>
  Eq (ChainPredicateFailure era)

instance
  ( Era era,
    NoThunks (PredicateFailure (Core.EraRule "BBODY" era)),
    NoThunks (PredicateFailure (Core.EraRule "TICK" era)),
    NoThunks (PredicateFailure (Core.EraRule "TICKN" era))
  ) =>
  NoThunks (ChainPredicateFailure era)

-- | Creates a valid initial chain state
initialShelleyState ::
  ( Default (State (Core.EraRule "PPUP" era)),
    Core.PParams era ~ PParams era
  ) =>
  WithOrigin (LastAppliedBlock (Crypto era)) ->
  EpochNo ->
  UTxO era ->
  Coin ->
  Map (KeyHash 'Genesis (Crypto era)) (GenDelegPair (Crypto era)) ->
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
            emptySnapShots
            ( LedgerState
                ( UTxOState
                    utxo
                    (Coin 0)
                    (Coin 0)
                    def
                )
                (DPState (def {_genDelegs = GenDelegs genDelegs}) def)
            )
            pp
            pp
            def
        )
        SNothing
        (PoolDistr Map.empty)
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

instance
  ( Era era,
    Embed (Core.EraRule "BBODY" era) (CHAIN era),
    Environment (Core.EraRule "BBODY" era) ~ BbodyEnv era,
    State (Core.EraRule "BBODY" era) ~ BbodyState era,
    Signal (Core.EraRule "BBODY" era) ~ Block era,
    Embed (Core.EraRule "TICKN" era) (CHAIN era),
    Environment (Core.EraRule "TICKN" era) ~ TicknEnv,
    State (Core.EraRule "TICKN" era) ~ TicknState,
    Signal (Core.EraRule "TICKN" era) ~ Bool,
    Embed (Core.EraRule "TICK" era) (CHAIN era),
    Environment (Core.EraRule "TICK" era) ~ (),
    State (Core.EraRule "TICK" era) ~ NewEpochState era,
    Signal (Core.EraRule "TICK" era) ~ SlotNo,
    Embed (PRTCL (Crypto era)) (CHAIN era),
    HasField "_maxBHSize" (Core.PParams era) Natural,
    HasField "_maxBBSize" (Core.PParams era) Natural,
    HasField "_protocolVersion" (Core.PParams era) ProtVer,
    HasField "_extraEntropy" (Core.PParams era) Nonce,
    HasField "_d" (Core.PParams era) UnitInterval
  ) =>
  STS (CHAIN era)
  where
  type
    State (CHAIN era) =
      ChainState era

  type
    Signal (CHAIN era) =
      Block era

  type Environment (CHAIN era) = ()
  type BaseM (CHAIN era) = ShelleyBase

  type PredicateFailure (CHAIN era) = ChainPredicateFailure era

  initialRules = []
  transitionRules = [chainTransition]

data ChainChecksData = ChainChecksData
  { ccMaxBHSize :: Natural,
    ccMaxBBSize :: Natural,
    ccProtocolVersion :: ProtVer
  }
  deriving (Show, Eq, Generic, NoThunks)

pparamsToChainChecksData ::
  ( HasField "_maxBHSize" pp Natural,
    HasField "_maxBBSize" pp Natural,
    HasField "_protocolVersion" pp ProtVer
  ) =>
  pp ->
  ChainChecksData
pparamsToChainChecksData pp =
  ChainChecksData
    { ccMaxBHSize = getField @"_maxBHSize" pp,
      ccMaxBBSize = getField @"_maxBBSize" pp,
      ccProtocolVersion = getField @"_protocolVersion" pp
    }

chainChecks ::
  ( Era era,
    MonadError (PredicateFailure (CHAIN era)) m
  ) =>
  Natural ->
  ChainChecksData ->
  BHeader (Crypto era) ->
  m ()
chainChecks maxpv ccd bh = do
  unless (m <= maxpv) $ throwError (ObsoleteNodeCHAIN m maxpv)
  unless (fromIntegral (bHeaderSize bh) <= ccMaxBHSize ccd) $
    throwError $
      HeaderSizeTooLargeCHAIN (fromIntegral $ bHeaderSize bh) (ccMaxBHSize ccd)
  unless (hBbsize (bhbody bh) <= ccMaxBBSize ccd) $
    throwError $
      BlockSizeTooLargeCHAIN (hBbsize (bhbody bh)) (ccMaxBBSize ccd)
  where
    (ProtVer m _) = ccProtocolVersion ccd

chainTransition ::
  forall era.
  ( Era era,
    STS (CHAIN era),
    Embed (Core.EraRule "BBODY" era) (CHAIN era),
    Environment (Core.EraRule "BBODY" era) ~ BbodyEnv era,
    State (Core.EraRule "BBODY" era) ~ BbodyState era,
    Signal (Core.EraRule "BBODY" era) ~ Block era,
    Embed (Core.EraRule "TICKN" era) (CHAIN era),
    Environment (Core.EraRule "TICKN" era) ~ TicknEnv,
    State (Core.EraRule "TICKN" era) ~ TicknState,
    Signal (Core.EraRule "TICKN" era) ~ Bool,
    Embed (Core.EraRule "TICK" era) (CHAIN era),
    Environment (Core.EraRule "TICK" era) ~ (),
    State (Core.EraRule "TICK" era) ~ NewEpochState era,
    Signal (Core.EraRule "TICK" era) ~ SlotNo,
    Embed (PRTCL (Crypto era)) (CHAIN era),
    HasField "_maxBHSize" (Core.PParams era) Natural,
    HasField "_maxBBSize" (Core.PParams era) Natural,
    HasField "_protocolVersion" (Core.PParams era) ProtVer,
    HasField "_extraEntropy" (Core.PParams era) Nonce,
    HasField "_d" (Core.PParams era) UnitInterval
  ) =>
  TransitionRule (CHAIN era)
chainTransition =
  judgmentContext
    >>= \( TRC
             ( _,
               ChainState
                 nes
                 cs
                 eta0
                 etaV
                 etaC
                 etaH
                 lab,
               block@(Block bh _)
               )
           ) -> do
        case prtlSeqChecks lab bh of
          Right () -> pure ()
          Left e -> failBecause $ PrtclSeqFailure e

        let NewEpochState _ _ _ (EpochState _ _ _ _ pp _) _ _ = nes
            chainChecksData = pparamsToChainChecksData pp

        maxpv <- liftSTS $ asks maxMajorPV
        case chainChecks maxpv chainChecksData bh of
          Right () -> pure ()
          Left e -> failBecause e

        let s = bheaderSlotNo $ bhbody bh

        nes' <- trans @(Core.EraRule "TICK" era) $ TRC ((), nes, s)

        let NewEpochState e1 _ _ _ _ _ = nes
            NewEpochState e2 _ bcur es _ _pd = nes'
        let EpochState account _ ls _ pp' _ = es
        let LedgerState _ (DPState (DState _ _ _ _ _genDelegs _) (PState _ _ _)) = ls

        let ph = lastAppliedHash lab
            etaPH = prevHashToNonce ph

        TicknState eta0' etaH' <-
          trans @(Core.EraRule "TICKN" era) $
            TRC
              ( TicknEnv (getField @"_extraEntropy" pp') etaC etaPH,
                TicknState eta0 etaH,
                (e1 /= e2)
              )

        PrtclState cs' etaV' etaC' <-
          trans @(PRTCL (Crypto era)) $
            TRC
              ( PrtclEnv (getField @"_d" pp') _pd _genDelegs eta0',
                PrtclState cs etaV etaC,
                bh
              )

        BbodyState ls' bcur' <-
          trans @(Core.EraRule "BBODY" era) $
            TRC (BbodyEnv pp' account, BbodyState ls bcur, block)

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
  ( Era era,
    Era era,
    STS (BBODY era),
    PredicateFailure (Core.EraRule "BBODY" era) ~ BbodyPredicateFailure era
  ) =>
  Embed (BBODY era) (CHAIN era)
  where
  wrapFailed = BbodyFailure

instance
  ( Era era,
    Era era,
    PredicateFailure (Core.EraRule "TICKN" era) ~ TicknPredicateFailure
  ) =>
  Embed TICKN (CHAIN era)
  where
  wrapFailed = TicknFailure

instance
  ( Era era,
    Era era,
    STS (TICK era),
    PredicateFailure (Core.EraRule "TICK" era) ~ TickPredicateFailure era
  ) =>
  Embed (TICK era) (CHAIN era)
  where
  wrapFailed = TickFailure

instance
  ( Era era,
    c ~ Crypto era,
    Era era,
    STS (PRTCL c)
  ) =>
  Embed (PRTCL c) (CHAIN era)
  where
  wrapFailed = PrtclFailure

data AdaPots = AdaPots
  { treasuryAdaPot :: Coin,
    reservesAdaPot :: Coin,
    rewardsAdaPot :: Coin,
    utxoAdaPot :: Coin,
    depositsAdaPot :: Coin,
    feesAdaPot :: Coin
  }
  deriving (Show, Eq)

-- | Calculate the total ada pots in the epoch state
totalAdaPotsES ::
  (UsesTxOut era, UsesValue era) =>
  EpochState era ->
  AdaPots
totalAdaPotsES (EpochState (AccountState treasury_ reserves_) _ ls _ _ _) =
  AdaPots
    { treasuryAdaPot = treasury_,
      reservesAdaPot = reserves_,
      rewardsAdaPot = rewards_,
      utxoAdaPot = circulation,
      depositsAdaPot = deposits,
      feesAdaPot = fees_
    }
  where
    (UTxOState u deposits fees_ _) = _utxoState ls
    (DPState ds _) = _delegationState ls
    rewards_ = fold (Map.elems (_rewards ds))
    circulation = Val.coin $ balance u

-- | Calculate the total ada pots in the chain state
totalAdaPots ::
  (UsesValue era, UsesTxOut era) =>
  ChainState era ->
  AdaPots
totalAdaPots = totalAdaPotsES . nesEs . chainNes

-- | Calculate the total ada in the epoch state
totalAdaES :: (UsesTxOut era, UsesValue era) => EpochState era -> Coin
totalAdaES cs =
  treasuryAdaPot
    <> reservesAdaPot
    <> rewardsAdaPot
    <> utxoAdaPot
    <> depositsAdaPot
    <> feesAdaPot
  where
    AdaPots
      { treasuryAdaPot,
        reservesAdaPot,
        rewardsAdaPot,
        utxoAdaPot,
        depositsAdaPot,
        feesAdaPot
      } = totalAdaPotsES cs

-- | Calculate the total ada in the chain state
totalAda :: (UsesTxOut era, UsesValue era) => ChainState era -> Coin
totalAda = totalAdaES . nesEs . chainNes
