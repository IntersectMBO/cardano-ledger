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
    totalAdaPots,
    ChainChecksData (..),
    pparamsToChainChecksData,
    chainChecks,
  )
where

import qualified Cardano.Crypto.VRF as VRF
import Cardano.Ledger.Crypto (VRF)
import qualified Cardano.Ledger.Crypto as CryptoClass
import Cardano.Ledger.Era (Crypto, Era)
import Cardano.Ledger.Shelley (ShelleyBased, ShelleyEra)
import qualified Cardano.Ledger.Val as Val
import Cardano.Slotting.Slot (WithOrigin (..))
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
import Data.Foldable (fold)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Word (Word64)
import GHC.Generics (Generic)
import NoThunks.Class (NoThunks (..))
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
    PState (..),
    UTxOState (..),
    emptyDState,
    emptyPPUPState,
    emptyPState,
    updateNES,
    _genDelegs,
  )
import Shelley.Spec.Ledger.OCert (OCertSignable)
import Shelley.Spec.Ledger.PParams
  ( PParams,
    PParams' (..),
    ProtVer (..),
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
import Shelley.Spec.Ledger.STS.Tick (TICK)
import Shelley.Spec.Ledger.STS.Tickn
import Shelley.Spec.Ledger.Slot (EpochNo)
import Shelley.Spec.Ledger.TxBody (EraIndependentTxBody)
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
  ShelleyBased era =>
  Show (ChainState era)

instance (Era era) => NFData (ChainState era)

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
  | BbodyFailure !(PredicateFailure (BBODY era)) -- Subtransition Failures
  | TickFailure !(PredicateFailure (TICK era)) -- Subtransition Failures
  | TicknFailure !(PredicateFailure TICKN) -- Subtransition Failures
  | PrtclFailure !(PredicateFailure (PRTCL (Crypto era))) -- Subtransition Failures
  | PrtclSeqFailure !(PrtlSeqFailure (Crypto era)) -- Subtransition Failures
  deriving (Generic)

deriving stock instance
  ( ShelleyBased era,
    Show (PredicateFailure (BBODY era))
  ) =>
  Show (ChainPredicateFailure era)

deriving stock instance
  ( ShelleyBased era,
    Eq (PredicateFailure (BBODY era))
  ) =>
  Eq (ChainPredicateFailure era)

instance
  ( ShelleyBased era,
    NoThunks (PredicateFailure (BBODY era))
  ) =>
  NoThunks (ChainPredicateFailure era)

-- | Creates a valid initial chain state
initialShelleyState ::
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
  ( CryptoClass.Crypto c,
    DSignable c (OCertSignable c),
    DSignable c (Hash c EraIndependentTxBody),
    KESignable c (BHBody c),
    VRF.Signable (VRF c) Seed
  ) =>
  STS (CHAIN (ShelleyEra c))
  where
  type
    State (CHAIN (ShelleyEra c)) =
      ChainState (ShelleyEra c)

  type
    Signal (CHAIN (ShelleyEra c)) =
      Block (ShelleyEra c)

  type Environment (CHAIN (ShelleyEra c)) = ()
  type BaseM (CHAIN (ShelleyEra c)) = ShelleyBase

  type PredicateFailure (CHAIN (ShelleyEra c)) = ChainPredicateFailure (ShelleyEra c)

  initialRules = []
  transitionRules = [chainTransition]

data ChainChecksData = ChainChecksData
  { ccMaxBHSize :: Natural,
    ccMaxBBSize :: Natural,
    ccProtocolVersion :: ProtVer
  }
  deriving (Show, Eq, Generic, NoThunks)

pparamsToChainChecksData :: PParams era -> ChainChecksData
pparamsToChainChecksData pp =
  ChainChecksData
    { ccMaxBHSize = _maxBHSize pp,
      ccMaxBBSize = _maxBBSize pp,
      ccProtocolVersion = _protocolVersion pp
    }

chainChecks ::
  ( Era era,
    PredicateFailure (CHAIN era) ~ ChainPredicateFailure era,
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
  ( ShelleyBased era,
    BaseM (CHAIN era) ~ ShelleyBase,
    State (CHAIN era) ~ ChainState era,
    Signal (CHAIN era) ~ Block era,
    PredicateFailure (CHAIN era) ~ ChainPredicateFailure era,
    Embed (BBODY era) (CHAIN era),
    Environment (BBODY era) ~ BbodyEnv era,
    State (BBODY era) ~ BbodyState era,
    Signal (BBODY era) ~ Block era,
    Embed TICKN (CHAIN era),
    Embed (TICK era) (CHAIN era),
    Embed (PRTCL (Crypto era)) (CHAIN era)
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

        nes' <-
          trans @(TICK era) $ TRC ((), nes, s)

        let NewEpochState e1 _ _ _ _ _ = nes
            NewEpochState e2 _ bcur es _ _pd = nes'
        let EpochState account _ ls _ pp' _ = es
        let LedgerState _ (DPState (DState _ _ _ _ _genDelegs _) (PState _ _ _)) = ls

        let ph = lastAppliedHash lab
            etaPH = prevHashToNonce ph

        TicknState eta0' etaH' <-
          trans @TICKN $
            TRC
              ( TicknEnv (_extraEntropy pp') etaC etaPH,
                TicknState eta0 etaH,
                (e1 /= e2)
              )

        PrtclState cs' etaV' etaC' <-
          trans @(PRTCL (Crypto era)) $
            TRC
              ( PrtclEnv (_d pp') _pd _genDelegs eta0',
                PrtclState cs etaV etaC,
                bh
              )

        BbodyState ls' bcur' <-
          trans @(BBODY era) $
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
  ( CryptoClass.Crypto c,
    DSignable c (OCertSignable c),
    DSignable c (Hash c EraIndependentTxBody),
    KESignable c (BHBody c),
    VRF.Signable (VRF c) Seed
  ) =>
  Embed (BBODY (ShelleyEra c)) (CHAIN (ShelleyEra c))
  where
  wrapFailed = BbodyFailure

instance
  ( CryptoClass.Crypto c,
    DSignable c (OCertSignable c),
    DSignable c (Hash c EraIndependentTxBody),
    KESignable c (BHBody c),
    VRF.Signable (VRF c) Seed
  ) =>
  Embed TICKN (CHAIN (ShelleyEra c))
  where
  wrapFailed = TicknFailure

instance
  ( CryptoClass.Crypto c,
    DSignable c (OCertSignable c),
    DSignable c (Hash c EraIndependentTxBody),
    KESignable c (BHBody c),
    VRF.Signable (VRF c) Seed
  ) =>
  Embed (TICK (ShelleyEra c)) (CHAIN (ShelleyEra c))
  where
  wrapFailed = TickFailure

instance
  ( CryptoClass.Crypto c,
    DSignable c (OCertSignable c),
    DSignable c (Hash c EraIndependentTxBody),
    KESignable c (BHBody c),
    VRF.Signable (VRF c) Seed
  ) =>
  Embed (PRTCL c) (CHAIN (ShelleyEra c))
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
totalAdaPots ::
  ShelleyBased era =>
  ChainState era ->
  AdaPots
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
    rewards_ = fold (Map.elems (_rewards ds))
    circulation = Val.coin $ balance u

-- | Calculate the total ada in the chain state
totalAda :: ShelleyBased era => ChainState era -> Coin
totalAda cs =
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
      } = totalAdaPots cs
