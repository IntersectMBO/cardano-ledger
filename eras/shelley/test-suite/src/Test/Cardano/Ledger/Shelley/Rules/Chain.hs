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
  chainStateNesL,
) where

import Cardano.Ledger.BHeaderView (BHeaderView)
import Cardano.Ledger.BaseTypes (
  BlocksMade (..),
  Nonce (..),
  ShelleyBase,
  StrictMaybe (..),
 )
import Cardano.Ledger.Binary (EncCBORGroup)
import Cardano.Ledger.Block (Block (..))
import Cardano.Ledger.Chain (
  ChainPredicateFailure (..),
  chainChecks,
  pparamsToChainChecksPParams,
 )
import Cardano.Ledger.Coin (Coin (..))
import Cardano.Ledger.Core
import Cardano.Ledger.Keys (GenDelegPair (..), GenDelegs (..), coerceKeyRole)
import Cardano.Ledger.Shelley (ShelleyEra)
import Cardano.Ledger.Shelley.AdaPots (
  AdaPots (..),
  totalAdaES,
  totalAdaPotsES,
 )
import Cardano.Ledger.Shelley.Core
import Cardano.Ledger.Shelley.LedgerState (
  EpochState (..),
  LedgerState (..),
  NewEpochState (..),
  StashedAVVMAddresses,
  curPParamsEpochStateL,
  futurePParamsEpochStateL,
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
import Cardano.Ledger.Shelley.State
import Cardano.Ledger.Slot (EpochNo)
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
import Control.State.Transition (
  Embed (..),
  STS (..),
  TRC (..),
  TransitionRule,
  failBecause,
  judgmentContext,
  trans,
 )
import Data.Default (Default, def)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Void (Void)
import Data.Word (Word64)
import GHC.Generics (Generic)
import Lens.Micro (Lens', lens, (&), (.~), (^.))
import NoThunks.Class (NoThunks (..))
import Test.Cardano.Ledger.Shelley.ConcreteCryptoTypes (MockCrypto)
import Test.Cardano.Ledger.Shelley.TreeDiff ()
import Test.Cardano.Ledger.TreeDiff (ToExpr (toExpr), defaultExprViaShow)

type instance EraRule "TICKN" ShelleyEra = TICKN

data CHAIN era

data ChainState era = ChainState
  { chainNes :: NewEpochState era
  , chainOCertIssue :: Map.Map (KeyHash 'BlockIssuer) Word64
  , chainEpochNonce :: Nonce
  , chainEvolvingNonce :: Nonce
  , chainCandidateNonce :: Nonce
  , chainPrevEpochNonce :: Nonce
  , chainLastAppliedBlock :: WithOrigin LastAppliedBlock
  }
  deriving (Generic)

deriving stock instance Show (NewEpochState era) => Show (ChainState era)

deriving stock instance Eq (NewEpochState era) => Eq (ChainState era)

instance NFData (NewEpochState era) => NFData (ChainState era)

chainStateNesL :: Lens' (ChainState era) (NewEpochState era)
chainStateNesL = lens chainNes $ \x y -> x {chainNes = y}

data TestChainPredicateFailure era
  = RealChainPredicateFailure ChainPredicateFailure
  | BbodyFailure (PredicateFailure (EraRule "BBODY" era)) -- Subtransition Failures
  | TickFailure (PredicateFailure (EraRule "TICK" era)) -- Subtransition Failures
  | TicknFailure (PredicateFailure (EraRule "TICKN" era)) -- Subtransition Failures
  | PrtclFailure (PredicateFailure (PRTCL MockCrypto)) -- Subtransition Failures
  | PrtclSeqFailure PrtlSeqFailure -- Subtransition Failures
  deriving (Generic)

data ChainEvent era
  = BbodyEvent !(Event (EraRule "BBODY" era))
  | TickEvent !(Event (EraRule "TICK" era))
  | TicknEvent !(Event (EraRule "TICKN" era))
  | PrtclEvent !(Event (PRTCL MockCrypto))

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
  ( EraGov era
  , EraStake era
  , EraCertState era
  , Default (StashedAVVMAddresses era)
  ) =>
  WithOrigin LastAppliedBlock ->
  EpochNo ->
  UTxO era ->
  Coin ->
  Map (KeyHash 'Genesis) GenDelegPair ->
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
            ChainAccountState
              { casTreasury = Coin 0
              , casReserves = reserves
              }
            ( LedgerState
                ( smartUTxOState
                    pp
                    utxo
                    (Coin 0)
                    (Coin 0)
                    emptyGovState
                    mempty
                )
                (def & certDStateL .~ dState)
            )
            emptySnapShots
            def
            & curPParamsEpochStateL .~ pp
            & prevPParamsEpochStateL .~ pp
            & futurePParamsEpochStateL .~ PotentialPParamsUpdate Nothing
        )
        SNothing
        (PoolDistr Map.empty mempty)
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
        { dsAccounts = def
        , dsFutureGenDelegs = Map.empty
        , dsGenDelegs = GenDelegs genDelegs
        , dsIRewards = def
        }

instance
  ( EraGov era
  , Embed (EraRule "BBODY" era) (CHAIN era)
  , Environment (EraRule "BBODY" era) ~ BbodyEnv era
  , State (EraRule "BBODY" era) ~ ShelleyBbodyState era
  , Signal (EraRule "BBODY" era) ~ Block BHeaderView era
  , Embed (EraRule "TICKN" era) (CHAIN era)
  , Environment (EraRule "TICKN" era) ~ TicknEnv
  , State (EraRule "TICKN" era) ~ TicknState
  , Signal (EraRule "TICKN" era) ~ Bool
  , Embed (EraRule "TICK" era) (CHAIN era)
  , Environment (EraRule "TICK" era) ~ ()
  , State (EraRule "TICK" era) ~ NewEpochState era
  , Signal (EraRule "TICK" era) ~ SlotNo
  , Embed (PRTCL MockCrypto) (CHAIN era)
  , EncCBORGroup (BlockBody era)
  , ProtVerAtMost era 6
  , State (EraRule "LEDGERS" era) ~ LedgerState era
  , EraCertState era
  ) =>
  STS (CHAIN era)
  where
  type State (CHAIN era) = ChainState era

  type Signal (CHAIN era) = Block (BHeader MockCrypto) era

  type Environment (CHAIN era) = ()
  type BaseM (CHAIN era) = ShelleyBase

  type PredicateFailure (CHAIN era) = TestChainPredicateFailure era
  type Event (CHAIN era) = ChainEvent era

  initialRules = []
  transitionRules = [chainTransition]

chainTransition ::
  forall era.
  ( Embed (EraRule "BBODY" era) (CHAIN era)
  , Environment (EraRule "BBODY" era) ~ BbodyEnv era
  , State (EraRule "BBODY" era) ~ ShelleyBbodyState era
  , Signal (EraRule "BBODY" era) ~ Block BHeaderView era
  , Embed (EraRule "TICKN" era) (CHAIN era)
  , Environment (EraRule "TICKN" era) ~ TicknEnv
  , State (EraRule "TICKN" era) ~ TicknState
  , Signal (EraRule "TICKN" era) ~ Bool
  , Embed (EraRule "TICK" era) (CHAIN era)
  , Environment (EraRule "TICK" era) ~ ()
  , State (EraRule "TICK" era) ~ NewEpochState era
  , Signal (EraRule "TICK" era) ~ SlotNo
  , Embed (PRTCL MockCrypto) (CHAIN era)
  , ProtVerAtMost era 6
  , State (EraRule "LEDGERS" era) ~ LedgerState era
  , EraGov era
  , EraCertState era
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

        -- We allow one protocol version higher than the current era's maximum, because
        -- that is the way we can get out of the current era into the next one. We test
        -- this functionality with application of PParamsUpdate that initiate an upgrade
        -- to the next era. This, of course, works properly only with HFC (Hard Fork
        -- Combinator), which is implemented in consensus, but for the purpose of the
        -- CHAIN test this is OK.
        case chainChecks (succ (eraProtVerHigh @era)) chainChecksData bhView of
          Right () -> pure ()
          Left e -> failBecause (RealChainPredicateFailure e)

        let s = bheaderSlotNo $ bhbody bh

        nes' <- trans @(EraRule "TICK" era) $ TRC ((), nes, s)

        let NewEpochState e1 _ _ _ _ _ _ = nes
            NewEpochState e2 _ bcur es _ _pd _ = nes'
        let EpochState account ls _ _ = es
            pp' = es ^. curPParamsEpochStateL
        let LedgerState _ certState = ls
            genDelegs = certState ^. certDStateL . dsGenDelegsL
            ph = lastAppliedHash lab
            etaPH = prevHashToNonce ph

        TicknState eta0' etaH' <-
          trans @(EraRule "TICKN" era) $
            TRC
              ( TicknEnv (pp' ^. ppExtraEntropyL) etaC etaPH
              , TicknState eta0 etaH
              , e1 /= e2
              )

        PrtclState cs' etaV' etaC' <-
          trans @(PRTCL MockCrypto) $
            TRC
              ( PrtclEnv (pp' ^. ppDL) _pd genDelegs eta0'
              , PrtclState cs etaV etaC
              , bh
              )

        BbodyState ls' bcur' <-
          trans @(EraRule "BBODY" era) $
            TRC (BbodyEnv pp' account, BbodyState ls bcur, Block bhView txs)

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

instance Era era => Embed (PRTCL MockCrypto) (CHAIN era) where
  wrapFailed = PrtclFailure
  wrapEvent = PrtclEvent

-- | Calculate the total ada pots in the chain state
totalAdaPots ::
  ( EraTxOut era
  , EraGov era
  , EraCertState era
  ) =>
  ChainState era ->
  AdaPots
totalAdaPots = totalAdaPotsES . nesEs . chainNes

-- | Calculate the total ada in the chain state
totalAda :: (EraTxOut era, EraGov era, EraCertState era) => ChainState era -> Coin
totalAda = totalAdaES . nesEs . chainNes

instance
  ( ToExpr (PParams era)
  , ToExpr (TxOut era)
  , ToExpr (StashedAVVMAddresses era)
  , ToExpr (GovState era)
  , ToExpr (CertState era)
  , ToExpr (InstantStake era)
  ) =>
  ToExpr (ChainState era)

instance ToExpr HashHeader where
  toExpr = defaultExprViaShow

instance ToExpr LastAppliedBlock
