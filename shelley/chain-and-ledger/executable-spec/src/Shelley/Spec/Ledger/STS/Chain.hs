{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Shelley.Spec.Ledger.STS.Chain
  ( CHAIN,
    ChainState (..),
    PredicateFailure (..),
    initialShelleyState,
    totalAda,
    totalAdaPots,
    chainChecks,
  )
where

import qualified Cardano.Crypto.VRF as VRF
import Cardano.Prelude
  ( MonadError (..),
    NFData,
    NoUnexpectedThunks,
    asks,
    unless,
  )
import Cardano.Slotting.Slot (WithOrigin (..))
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
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Word (Word64)
import GHC.Generics (Generic)
import Numeric.Natural (Natural)
import Shelley.Spec.Ledger.BaseTypes
  ( Globals (..),
    Nonce (..),
    Seed (..),
    ShelleyBase,
    StrictMaybe (..),
  )
import Shelley.Spec.Ledger.BlockChain
  ( BHBody,
    BHeader,
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
import Shelley.Spec.Ledger.Crypto (VRF)
import Shelley.Spec.Ledger.Delegation.Certificates (PoolDistr (..))
import Shelley.Spec.Ledger.EpochBoundary (BlocksMade (..), emptySnapShots)
import Shelley.Spec.Ledger.Keys
  ( DSignable,
    GenDelegPair (..),
    GenDelegs (..),
    Hash,
    KESignable,
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
    OBftSlot,
    PState (..),
    UTxOState (..),
    emptyDState,
    emptyPPUPState,
    emptyPState,
    getGKeys,
    updateNES,
    _genDelegs,
  )
import Shelley.Spec.Ledger.OCert (OCertSignable)
import Shelley.Spec.Ledger.PParams
  ( PParams,
    ProtVer (..),
    _maxBBSize,
    _maxBHSize,
    _protocolVersion,
  )
import Shelley.Spec.Ledger.Rewards (emptyNonMyopic)
import Shelley.Spec.Ledger.STS.Bbody (BBODY, BbodyEnv (..), BbodyState (..))
import Shelley.Spec.Ledger.STS.Prtcl
  ( PRTCL,
    PrtclEnv (..),
    PrtclState (..),
    PrtlSeqFailure,
    prtlSeqChecks,
  )
import Shelley.Spec.Ledger.STS.Tick (TICK, TickEnv (..))
import Shelley.Spec.Ledger.STS.Tickn
import Shelley.Spec.Ledger.Slot (EpochNo, SlotNo)
import Shelley.Spec.Ledger.Tx (TxBody)
import Shelley.Spec.Ledger.UTxO (UTxO (..), balance)
import Shelley.Spec.Ledger.Value

data CHAIN crypto v

data ChainState crypto v = ChainState
  { chainNes :: NewEpochState crypto v,
    chainOCertIssue :: Map.Map (KeyHash 'BlockIssuer crypto) Word64,
    chainEpochNonce :: Nonce,
    chainEvolvingNonce :: Nonce,
    chainCandidateNonce :: Nonce,
    chainPrevEpochNonce :: Nonce,
    chainLastAppliedBlock :: WithOrigin (LastAppliedBlock crypto v)
  }
  deriving (Show, Eq, Generic)

instance CV crypto v => NFData (ChainState crypto v)

-- | Creates a valid initial chain state
initialShelleyState ::
  WithOrigin (LastAppliedBlock crypto v) ->
  EpochNo ->
  UTxO crypto v ->
  Coin ->
  Map (KeyHash 'Genesis crypto) (GenDelegPair crypto) ->
  Map SlotNo (OBftSlot crypto) ->
  PParams ->
  Nonce ->
  ChainState crypto v
initialShelleyState lab e utxo reserves genDelegs os pp initNonce =
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
                    emptyPPUPState
                )
                (DPState (emptyDState {_genDelegs = (GenDelegs genDelegs)}) emptyPState)
            )
            pp
            pp
            emptyNonMyopic
        )
        SNothing
        (PoolDistr Map.empty)
        os
    )
    cs
    initNonce
    initNonce
    initNonce
    NeutralNonce
    lab
  where
    cs = Map.fromList (fmap (\(GenDelegPair hk _) -> (coerceKeyRole hk, 0)) (Map.elems genDelegs))

instance
  ( CV crypto v,
    DSignable crypto (OCertSignable crypto),
    DSignable crypto (Hash crypto (TxBody crypto v)),
    KESignable crypto (BHBody crypto v),
    VRF.Signable (VRF crypto) Seed
  ) =>
  STS (CHAIN crypto v)
  where
  type
    State (CHAIN crypto v) =
      ChainState crypto v

  type
    Signal (CHAIN crypto v) =
      Block crypto v

  type Environment (CHAIN crypto v) = ()
  type BaseM (CHAIN crypto v) = ShelleyBase

  data PredicateFailure (CHAIN crypto v)
    = HeaderSizeTooLargeCHAIN
        !Natural -- Header Size
        !Natural -- Max Header Size
    | BlockSizeTooLargeCHAIN
        !Natural -- Block Size
        !Natural -- Max Block Size
    | ObsoleteNodeCHAIN
        !Natural -- protocol version used
        !Natural -- max protocol version
    | BbodyFailure !(PredicateFailure (BBODY crypto v)) -- Subtransition Failures
    | TickFailure !(PredicateFailure (TICK crypto v)) -- Subtransition Failures
    | TicknFailure !(PredicateFailure TICKN) -- Subtransition Failures
    | PrtclFailure !(PredicateFailure (PRTCL crypto v)) -- Subtransition Failures
    | PrtclSeqFailure !(PrtlSeqFailure crypto v) -- Subtransition Failures
    deriving (Show, Eq, Generic)

  initialRules = []
  transitionRules = [chainTransition]

instance CV crypto v => NoUnexpectedThunks (PredicateFailure (CHAIN crypto v))

chainChecks ::
  (CV crypto v, MonadError (PredicateFailure (CHAIN crypto v)) m) =>
  Natural ->
  PParams ->
  BHeader crypto v ->
  m ()
chainChecks maxpv pp bh = do
  unless (m <= maxpv) $ throwError (ObsoleteNodeCHAIN m maxpv)
  unless (fromIntegral (bHeaderSize bh) <= _maxBHSize pp) $
    throwError $
      HeaderSizeTooLargeCHAIN (fromIntegral $ bHeaderSize bh) (_maxBHSize pp)
  unless (hBbsize (bhbody bh) <= _maxBBSize pp) $
    throwError $
      BlockSizeTooLargeCHAIN (hBbsize (bhbody bh)) (_maxBBSize pp)
  where
    (ProtVer m _) = _protocolVersion pp

chainTransition ::
  forall crypto v.
  ( CV crypto v,
    DSignable crypto (OCertSignable crypto),
    DSignable crypto (Hash crypto (TxBody crypto v)),
    KESignable crypto (BHBody crypto v),
    VRF.Signable (VRF crypto) Seed
  ) =>
  TransitionRule (CHAIN crypto v)
chainTransition =
  judgmentContext
    >>= \(TRC ((), ChainState nes cs eta0 etaV etaC etaH lab, block@(Block bh _))) -> do
      case prtlSeqChecks lab bh of
        Right () -> pure ()
        Left e -> failBecause $ PrtclSeqFailure e

      let NewEpochState _ _ _ (EpochState _ _ _ _ pp _) _ _ _ = nes

      maxpv <- liftSTS $ asks maxMajorPV
      case chainChecks maxpv pp bh of
        Right () -> pure ()
        Left e -> failBecause e

      let s = bheaderSlotNo $ bhbody bh
      let gkeys = getGKeys nes

      nes' <-
        trans @(TICK crypto v) $ TRC (TickEnv gkeys, nes, s)

      let NewEpochState e1 _ _ _ _ _ _ = nes
          NewEpochState e2 _ bcur es _ _pd osched = nes'
      let EpochState account _ ls _ pp' _ = es
      let LedgerState _ (DPState (DState _ _ _ _ _genDelegs _) (PState _ _ _)) = ls

      let ph = lastAppliedHash lab
          etaPH = prevHashToNonce ph

      TicknState eta0' etaH' <-
        trans @TICKN $
          TRC
            ( TicknEnv pp' etaC etaPH,
              TicknState eta0 etaH,
              (e1 /= e2)
            )

      PrtclState cs' etaV' etaC' <-
        trans @(PRTCL crypto v) $
          TRC
            ( PrtclEnv osched _pd _genDelegs eta0',
              PrtclState cs etaV etaC,
              bh
            )

      BbodyState ls' bcur' <-
        trans @(BBODY crypto v) $
          TRC (BbodyEnv (Map.keysSet osched) pp' account, BbodyState ls bcur, block)

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
  ( CV crypto v,
    DSignable crypto (OCertSignable crypto),
    DSignable crypto (Hash crypto (TxBody crypto v)),
    KESignable crypto (BHBody crypto v),
    VRF.Signable (VRF crypto) Seed
  ) =>
  Embed (BBODY crypto v) (CHAIN crypto v)
  where
  wrapFailed = BbodyFailure

instance
  ( CV crypto v,
    DSignable crypto (OCertSignable crypto),
    DSignable crypto (Hash crypto (TxBody crypto v)),
    KESignable crypto (BHBody crypto v),
    VRF.Signable (VRF crypto) Seed
  ) =>
  Embed TICKN (CHAIN crypto v)
  where
  wrapFailed = TicknFailure

instance
  ( CV crypto v,
    DSignable crypto (OCertSignable crypto),
    DSignable crypto (Hash crypto (TxBody crypto v)),
    KESignable crypto (BHBody crypto v),
    VRF.Signable (VRF crypto) Seed
  ) =>
  Embed (TICK crypto v) (CHAIN crypto v)
  where
  wrapFailed = TickFailure

instance
  ( CV crypto v,
    DSignable crypto (OCertSignable crypto),
    DSignable crypto (Hash crypto (TxBody crypto v)),
    KESignable crypto (BHBody crypto v),
    VRF.Signable (VRF crypto) Seed
  ) =>
  Embed (PRTCL crypto v) (CHAIN crypto v)
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

-- | Calculate the total ada pots in the chain state
totalAdaPots :: CV crypto v => ChainState crypto v -> AdaPots
totalAdaPots (ChainState nes _ _ _ _ _ _) =
  AdaPots
    { treasuryAdaPot = treasury_,
      reservesAdaPot = reserves_,
      rewardsAdaPot = rewards_,
      utxoAdaPot = circulation,
      depositsAdaPot = deposits,
      feesAdaPot = fees_
    }
  where
    (EpochState (AccountState treasury_ reserves_) _ ls _ _ _) = nesEs nes
    (UTxOState u deposits fees_ _) = _utxoState ls
    (DPState ds _) = _delegationState ls
    rewards_ = sum (Map.elems (_rewards ds))
    circulation = vcoin $ balance u

-- | Calculate the total ada in the chain state
totalAda :: CV crypto v => ChainState crypto v -> Coin
totalAda cs =
  treasuryAdaPot + reservesAdaPot + rewardsAdaPot + utxoAdaPot + depositsAdaPot + feesAdaPot
  where
    AdaPots
      { treasuryAdaPot,
        reservesAdaPot,
        rewardsAdaPot,
        utxoAdaPot,
        depositsAdaPot,
        feesAdaPot
      } = totalAdaPots cs
