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
{-# LANGUAGE UndecidableInstances #-}

module Test.Cardano.Ledger.Shelley.Rules.Chain
  ( CHAIN,
    ChainState (..),
    TestChainPredicateFailure (..),
    ChainEvent (..),
    PredicateFailure,
    AdaPots (..),
    initialShelleyState,
    totalAda,
    totalAdaPots,
  )
where

import Cardano.Ledger.BHeaderView (BHeaderView)
import Cardano.Ledger.BaseTypes
  ( BlocksMade (..),
    Globals (..),
    Nonce (..),
    ProtVer (..),
    ShelleyBase,
    StrictMaybe (..),
    UnitInterval,
  )
import Cardano.Ledger.Block (Block (..))
import Cardano.Ledger.Chain
  ( ChainPredicateFailure (..),
    chainChecks,
    pparamsToChainChecksPParams,
  )
import Cardano.Ledger.Coin (Coin (..))
import qualified Cardano.Ledger.Core as Core
import Cardano.Ledger.Era (Crypto, Era)
import qualified Cardano.Ledger.Era as Era
import Cardano.Ledger.Keys
  ( GenDelegPair (..),
    GenDelegs (..),
    KeyHash,
    KeyRole (..),
    coerceKeyRole,
  )
import Cardano.Ledger.PoolDistr (PoolDistr (..))
import qualified Cardano.Ledger.Pretty as PP
import Cardano.Ledger.Serialization (ToCBORGroup)
import Cardano.Ledger.Shelley (ShelleyEra)
import Cardano.Ledger.Shelley.AdaPots
  ( AdaPots (..),
    totalAdaES,
    totalAdaPotsES,
  )
import Cardano.Ledger.Shelley.EpochBoundary (emptySnapShots)
import Cardano.Ledger.Shelley.LedgerState
  ( AccountState (..),
    DPState (..),
    DState (..),
    EpochState (..),
    LedgerState (..),
    NewEpochState (..),
    PState (..),
    StashedAVVMAddresses,
    smartUTxOState,
    updateNES,
    _genDelegs,
  )
import Cardano.Ledger.Shelley.Rules.Bbody
  ( BbodyEnv (..),
    ShelleyBBODY,
    ShelleyBbodyPredFailure,
    ShelleyBbodyState (..),
  )
import Cardano.Ledger.Shelley.Rules.Tick (ShelleyTICK, ShelleyTickEvent, ShelleyTickPredFailure)
import Cardano.Ledger.Shelley.UTxO (UTxO (..))
import Cardano.Ledger.Slot (EpochNo)
import Cardano.Protocol.HeaderCrypto
import Cardano.Protocol.TPraos.BHeader
  ( BHeader,
    LastAppliedBlock (..),
    bhHash,
    bhbody,
    bheaderBlockNo,
    bheaderSlotNo,
    lastAppliedHash,
    makeHeaderView,
    prevHashToNonce,
  )
import Cardano.Protocol.TPraos.Rules.Prtcl
  ( PRTCL,
    PrtclEnv (..),
    PrtclState (..),
    PrtlSeqFailure,
    prtlSeqChecks,
  )
import Cardano.Protocol.TPraos.Rules.Tickn
import Cardano.Slotting.Slot (SlotNo, WithOrigin (..))
import Control.DeepSeq (NFData)
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
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Void (Void)
import Data.Word (Word64)
import GHC.Generics (Generic)
import GHC.Records
import NoThunks.Class (NoThunks (..))
import Numeric.Natural (Natural)

type instance Core.EraRule "TICKN" (ShelleyEra c) = TICKN

data CHAIN era hcrypto

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

deriving stock instance Show (NewEpochState era) => Show (ChainState era)

deriving stock instance Eq (NewEpochState era) => Eq (ChainState era)

instance NFData (NewEpochState era) => NFData (ChainState era)

data TestChainPredicateFailure era hcrypto
  = RealChainPredicateFailure !ChainPredicateFailure
  | BbodyFailure !(PredicateFailure (Core.EraRule "BBODY" era)) -- Subtransition Failures
  | TickFailure !(PredicateFailure (Core.EraRule "TICK" era)) -- Subtransition Failures
  | TicknFailure !(PredicateFailure (Core.EraRule "TICKN" era)) -- Subtransition Failures
  | PrtclFailure !(PredicateFailure (PRTCL (Crypto era) hcrypto)) -- Subtransition Failures
  | PrtclSeqFailure !(PrtlSeqFailure (Crypto era)) -- Subtransition Failures
  deriving (Generic)

data ChainEvent era hcrypto
  = BbodyEvent !(Event (Core.EraRule "BBODY" era))
  | TickEvent !(Event (Core.EraRule "TICK" era))
  | TicknEvent !(Event (Core.EraRule "TICKN" era))
  | PrtclEvent !(Event (PRTCL (Crypto era) hcrypto))

deriving stock instance
  ( Era era,
    HeaderCrypto hcrypto,
    Show (PredicateFailure (Core.EraRule "BBODY" era)),
    Show (PredicateFailure (Core.EraRule "TICK" era)),
    Show (PredicateFailure (Core.EraRule "TICKN" era))
  ) =>
  Show (TestChainPredicateFailure era hcrypto)

deriving stock instance
  ( Era era,
    HeaderCrypto hcrypto,
    Eq (PredicateFailure (Core.EraRule "BBODY" era)),
    Eq (PredicateFailure (Core.EraRule "TICK" era)),
    Eq (PredicateFailure (Core.EraRule "TICKN" era))
  ) =>
  Eq (TestChainPredicateFailure era hcrypto)

instance
  ( Era era,
    HeaderCrypto hcrypto,
    NoThunks (PredicateFailure (Core.EraRule "BBODY" era)),
    NoThunks (PredicateFailure (Core.EraRule "TICK" era)),
    NoThunks (PredicateFailure (Core.EraRule "TICKN" era))
  ) =>
  NoThunks (TestChainPredicateFailure era hcrypto)

-- | Creates a valid initial chain state
initialShelleyState ::
  ( Core.EraTxOut era,
    Default (State (Core.EraRule "PPUP" era)),
    Default (StashedAVVMAddresses era)
  ) =>
  WithOrigin (LastAppliedBlock (Crypto era)) ->
  EpochNo ->
  UTxO era ->
  Coin ->
  Map (KeyHash 'Genesis (Crypto era)) (GenDelegPair (Crypto era)) ->
  Core.PParams era ->
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
                ( smartUTxOState
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

instance
  ( Era era,
    HeaderCrypto hcrypto,
    Embed (Core.EraRule "BBODY" era) (CHAIN era hcrypto),
    Environment (Core.EraRule "BBODY" era) ~ BbodyEnv era,
    State (Core.EraRule "BBODY" era) ~ ShelleyBbodyState era,
    Signal (Core.EraRule "BBODY" era) ~ Block (BHeaderView (Crypto era)) era,
    Embed (Core.EraRule "TICKN" era) (CHAIN era hcrypto),
    Environment (Core.EraRule "TICKN" era) ~ TicknEnv,
    State (Core.EraRule "TICKN" era) ~ TicknState,
    Signal (Core.EraRule "TICKN" era) ~ Bool,
    Embed (Core.EraRule "TICK" era) (CHAIN era hcrypto),
    Environment (Core.EraRule "TICK" era) ~ (),
    State (Core.EraRule "TICK" era) ~ NewEpochState era,
    Signal (Core.EraRule "TICK" era) ~ SlotNo,
    Embed (PRTCL (Crypto era) hcrypto) (CHAIN era hcrypto),
    HasField "_maxBHSize" (Core.PParams era) Natural,
    HasField "_maxBBSize" (Core.PParams era) Natural,
    HasField "_protocolVersion" (Core.PParams era) ProtVer,
    HasField "_extraEntropy" (Core.PParams era) Nonce,
    HasField "_d" (Core.PParams era) UnitInterval,
    ToCBORGroup (Era.TxSeq era)
  ) =>
  STS (CHAIN era hcrypto)
  where
  type
    State (CHAIN era hcrypto) =
      ChainState era

  type
    Signal (CHAIN era hcrypto) =
      Block (BHeader (Crypto era) hcrypto) era

  type Environment (CHAIN era hcrypto) = ()
  type BaseM (CHAIN era hcrypto) = ShelleyBase

  type PredicateFailure (CHAIN era hcrypto) = TestChainPredicateFailure era hcrypto
  type Event (CHAIN era hcrypto) = ChainEvent era hcrypto

  initialRules = []
  transitionRules = [chainTransition]

chainTransition ::
  forall era hcrypto.
  ( Era era,
    HeaderCrypto hcrypto,
    STS (CHAIN era hcrypto),
    Embed (Core.EraRule "BBODY" era) (CHAIN era hcrypto),
    Environment (Core.EraRule "BBODY" era) ~ BbodyEnv era,
    State (Core.EraRule "BBODY" era) ~ ShelleyBbodyState era,
    Signal (Core.EraRule "BBODY" era) ~ Block (BHeaderView (Crypto era)) era,
    Embed (Core.EraRule "TICKN" era) (CHAIN era hcrypto),
    Environment (Core.EraRule "TICKN" era) ~ TicknEnv,
    State (Core.EraRule "TICKN" era) ~ TicknState,
    Signal (Core.EraRule "TICKN" era) ~ Bool,
    Embed (Core.EraRule "TICK" era) (CHAIN era hcrypto),
    Environment (Core.EraRule "TICK" era) ~ (),
    State (Core.EraRule "TICK" era) ~ NewEpochState era,
    Signal (Core.EraRule "TICK" era) ~ SlotNo,
    Embed (PRTCL (Crypto era) hcrypto) (CHAIN era hcrypto),
    HasField "_maxBHSize" (Core.PParams era) Natural,
    HasField "_maxBBSize" (Core.PParams era) Natural,
    HasField "_protocolVersion" (Core.PParams era) ProtVer,
    HasField "_extraEntropy" (Core.PParams era) Nonce,
    HasField "_d" (Core.PParams era) UnitInterval,
    ToCBORGroup (Era.TxSeq era)
  ) =>
  TransitionRule (CHAIN era hcrypto)
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
               Block bh txs
               )
           ) -> do
        case prtlSeqChecks lab bh of
          Right () -> pure ()
          Left e -> failBecause $ PrtclSeqFailure e

        let NewEpochState _ _ _ (EpochState _ _ _ _ pp _) _ _ _ = nes
            chainChecksData = pparamsToChainChecksPParams pp
            bhView = makeHeaderView bh

        maxpv <- liftSTS $ asks maxMajorPV
        case chainChecks maxpv chainChecksData bhView of
          Right () -> pure ()
          Left e -> failBecause (RealChainPredicateFailure e)

        let s = bheaderSlotNo $ bhbody bh

        nes' <- trans @(Core.EraRule "TICK" era) $ TRC ((), nes, s)

        let NewEpochState e1 _ _ _ _ _ _ = nes
            NewEpochState e2 _ bcur es _ _pd _ = nes'
        let EpochState account _ ls _ pp' _ = es
        let LedgerState _ (DPState (DState _ _ genDelegs _) (PState _ _ _)) = ls
        let ph = lastAppliedHash lab
            etaPH = prevHashToNonce ph

        TicknState eta0' etaH' <-
          trans @(Core.EraRule "TICKN" era) $
            TRC
              ( TicknEnv (getField @"_extraEntropy" pp') etaC etaPH,
                TicknState eta0 etaH,
                e1 /= e2
              )

        PrtclState cs' etaV' etaC' <-
          trans @(PRTCL (Crypto era) hcrypto) $
            TRC
              ( PrtclEnv (getField @"_d" pp') _pd genDelegs eta0',
                PrtclState cs etaV etaC,
                bh
              )

        let thouShaltNot = error "A block with a header view should never be hashed"
        BbodyState ls' bcur' <-
          trans @(Core.EraRule "BBODY" era) $
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
  ( Era era,
    HeaderCrypto hcrypto,
    STS (ShelleyBBODY era),
    PredicateFailure (Core.EraRule "BBODY" era) ~ ShelleyBbodyPredFailure era,
    Event (Core.EraRule "BBODY" era) ~ Event (ShelleyBBODY era)
  ) =>
  Embed (ShelleyBBODY era) (CHAIN era hcrypto)
  where
  wrapFailed = BbodyFailure
  wrapEvent = BbodyEvent

instance
  ( Era era,
    HeaderCrypto hcrypto,
    PredicateFailure (Core.EraRule "TICKN" era) ~ TicknPredicateFailure,
    Event (Core.EraRule "TICKN" era) ~ Void
  ) =>
  Embed TICKN (CHAIN era hcrypto)
  where
  wrapFailed = TicknFailure
  wrapEvent = TicknEvent

instance
  ( Era era,
    HeaderCrypto hcrypto,
    STS (ShelleyTICK era),
    PredicateFailure (Core.EraRule "TICK" era) ~ ShelleyTickPredFailure era,
    Event (Core.EraRule "TICK" era) ~ ShelleyTickEvent era
  ) =>
  Embed (ShelleyTICK era) (CHAIN era hcrypto)
  where
  wrapFailed = TickFailure
  wrapEvent = TickEvent

instance
  ( Era era,
    c ~ Crypto era,
    HeaderCrypto hcrypto,
    STS (PRTCL c hcrypto)
  ) =>
  Embed (PRTCL c hcrypto) (CHAIN era hcrypto)
  where
  wrapFailed = PrtclFailure
  wrapEvent = PrtclEvent

-- | Calculate the total ada pots in the chain state
totalAdaPots ::
  Core.EraTxOut era =>
  ChainState era ->
  AdaPots
totalAdaPots = totalAdaPotsES . nesEs . chainNes

-- | Calculate the total ada in the chain state
totalAda :: Core.EraTxOut era => ChainState era -> Coin
totalAda = totalAdaES . nesEs . chainNes

ppChainState :: PP.CanPrettyPrintLedgerState era => ChainState era -> PP.PDoc
ppChainState (ChainState nes ocert epochnonce evolvenonce prevnonce candnonce lastab) =
  PP.ppRecord
    "ChainState"
    [ ("newepoch", PP.ppNewEpochState nes),
      ("ocerts", PP.ppMap PP.ppKeyHash PP.ppWord64 ocert),
      ("epochNonce", PP.ppNonce epochnonce),
      ("evolvingNonce", PP.ppNonce evolvenonce),
      ("candidateNonce", PP.ppNonce prevnonce),
      ("prevepochNonce", PP.ppNonce candnonce),
      ("lastApplidBlock", PP.ppWithOrigin PP.ppLastAppliedBlock lastab)
    ]

instance PP.CanPrettyPrintLedgerState era => PP.PrettyA (ChainState era) where
  prettyA = ppChainState
