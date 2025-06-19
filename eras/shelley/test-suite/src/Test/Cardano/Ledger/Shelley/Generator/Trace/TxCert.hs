{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module Test.Cardano.Ledger.Shelley.Generator.Trace.TxCert (
  CERTS,
  genTxCerts,
) where

import Cardano.Ledger.BaseTypes (CertIx, Globals, ShelleyBase, SlotNo (..), TxIx)
import Cardano.Ledger.Coin (Coin (..))
import Cardano.Ledger.Core
import qualified Cardano.Ledger.Core as Core
import Cardano.Ledger.Credential (SlotNo32 (..))
import Cardano.Ledger.Keys (HasKeyRole (coerceKeyRole), asWitness)
import Cardano.Ledger.Shelley.API (
  DelplEnv (..),
  Ptr (..),
  ShelleyDELPL,
 )
import Cardano.Ledger.Shelley.Rules (ShelleyDelplEvent, ShelleyDelplPredFailure)
import Cardano.Ledger.State
import Cardano.Protocol.Crypto (Crypto)
import Control.Monad.Trans.Reader (runReaderT)
import Control.State.Transition (
  BaseM,
  Embed,
  Environment,
  Event,
  PredicateFailure,
  STS (..),
  Signal,
  State,
  TRC (..),
  TransitionRule,
  initialRules,
  judgmentContext,
  trans,
  transitionRules,
  wrapEvent,
  wrapFailed,
 )
import Data.Functor.Identity (runIdentity)
import Data.List (partition)
import qualified Data.Map.Strict as Map
import Data.Maybe (catMaybes, mapMaybe)
import Data.Proxy (Proxy (..))
import GHC.Generics (Generic)
import GHC.Stack (HasCallStack)
import Lens.Micro ((^.))
import Test.Cardano.Ledger.Core.KeyPair (KeyPair)
import Test.Cardano.Ledger.Shelley.Constants (Constants (..))
import Test.Cardano.Ledger.Shelley.Generator.Core (GenEnv (..), KeySpace (..))
import Test.Cardano.Ledger.Shelley.Generator.EraGen (EraGen (..))
import Test.Cardano.Ledger.Shelley.Generator.ScriptClass (scriptKeyCombination)
import Test.Cardano.Ledger.Shelley.Generator.TxCert (CertCred (..), genTxCert)
import Test.Cardano.Ledger.Shelley.Utils (epochFromSlotNo, testGlobals)
import Test.Control.State.Transition.Trace (TraceOrder (OldestFirst), lastState, traceSignals)
import qualified Test.Control.State.Transition.Trace.Generator.QuickCheck as QC
import Test.QuickCheck (Gen)

-- | This is a non-spec STS used to generate a sequence of certificates with
-- witnesses.
data CERTS era

newtype CertsPredicateFailure era
  = CertsFailure (PredicateFailure (Core.EraRule "DELPL" era))
  deriving (Generic)

newtype CertsEvent era
  = CertsEvent (Event (Core.EraRule "DELPL" era))

deriving stock instance
  Eq (PredicateFailure (Core.EraRule "DELPL" era)) =>
  Eq (CertsPredicateFailure era)

deriving stock instance
  Show (PredicateFailure (Core.EraRule "DELPL" era)) =>
  Show (CertsPredicateFailure era)

instance
  ( Era era
  , Embed (Core.EraRule "DELPL" era) (CERTS era)
  , Environment (Core.EraRule "DELPL" era) ~ DelplEnv era
  , State (Core.EraRule "DELPL" era) ~ CertState era
  , Signal (Core.EraRule "DELPL" era) ~ TxCert era
  ) =>
  STS (CERTS era)
  where
  type Environment (CERTS era) = (SlotNo, TxIx, Core.PParams era, ChainAccountState)
  type State (CERTS era) = (CertState era, CertIx)
  type Signal (CERTS era) = Maybe (TxCert era, CertCred era)
  type PredicateFailure (CERTS era) = CertsPredicateFailure era
  type Event (CERTS era) = CertsEvent era

  type BaseM (CERTS era) = ShelleyBase

  initialRules = []
  transitionRules = [certsTransition]

certsTransition ::
  forall era.
  ( Embed (Core.EraRule "DELPL" era) (CERTS era)
  , Environment (Core.EraRule "DELPL" era) ~ DelplEnv era
  , State (Core.EraRule "DELPL" era) ~ CertState era
  , Signal (Core.EraRule "DELPL" era) ~ TxCert era
  ) =>
  TransitionRule (CERTS era)
certsTransition = do
  TRC
    ( (slot@(SlotNo slot64), txIx, pp, acnt)
      , (dpState, nextCertIx)
      , c
      ) <-
    judgmentContext

  case c of
    Just (cert, _wits) -> do
      let ptr = Ptr (SlotNo32 (fromIntegral slot64)) txIx nextCertIx
      let epoch = epochFromSlotNo slot
      dpState' <-
        trans @(Core.EraRule "DELPL" era) $
          TRC (DelplEnv slot epoch ptr pp acnt, dpState, cert)

      pure (dpState', succ nextCertIx)
    Nothing ->
      pure (dpState, nextCertIx)

instance
  ( Era era
  , STS (ShelleyDELPL era)
  , PredicateFailure (Core.EraRule "DELPL" era) ~ ShelleyDelplPredFailure era
  , Event (Core.EraRule "DELPL" era) ~ ShelleyDelplEvent era
  ) =>
  Embed (ShelleyDELPL era) (CERTS era)
  where
  wrapFailed = CertsFailure
  wrapEvent = CertsEvent

instance
  ( EraGen era
  , Embed (Core.EraRule "DELPL" era) (CERTS era)
  , Environment (Core.EraRule "DELPL" era) ~ DelplEnv era
  , State (Core.EraRule "DELPL" era) ~ CertState era
  , Signal (Core.EraRule "DELPL" era) ~ TxCert era
  , ProtVerAtMost era 8
  , EraCertState era
  , Crypto c
  ) =>
  QC.HasTrace (CERTS era) (GenEnv c era)
  where
  envGen _ = error "HasTrace CERTS - envGen not required"

  sigGen
    ( GenEnv
        ks
        _scriptspace
        constants
      )
    (slot, _txIx, pparams, accountState)
    (dpState, _certIx) =
      genTxCert
        constants
        ks
        pparams
        accountState
        dpState
        slot

  shrinkSignal = const []

  type BaseEnv (CERTS era) = Globals
  interpretSTS globals act = runIdentity $ runReaderT act globals

-- | Generate certificates and also return the associated witnesses and
-- deposits and refunds required.
genTxCerts ::
  forall era c.
  ( EraGen era
  , Embed (Core.EraRule "DELPL" era) (CERTS era)
  , Environment (Core.EraRule "DELPL" era) ~ DelplEnv era
  , State (Core.EraRule "DELPL" era) ~ CertState era
  , Signal (Core.EraRule "DELPL" era) ~ TxCert era
  , Crypto c
  ) =>
  GenEnv c era ->
  Core.PParams era ->
  CertState era ->
  SlotNo ->
  TxIx ->
  ChainAccountState ->
  Gen
    ( [TxCert era]
    , Coin
    , Coin
    , CertState era
    , [KeyPair 'Witness]
    , [(Core.Script era, Core.Script era)]
    )
genTxCerts
  ge@( GenEnv
        KeySpace_ {ksIndexedStakingKeys}
        _scriptspace
        Constants {maxCertsPerTx}
      )
  pp
  certState
  slot
  txIx
  acnt = do
    let env = (slot, txIx, pp, acnt)
        st0 = (certState, minBound)
        certDState = certState ^. certDStateL
        certPState = certState ^. certPStateL

    certsTrace <-
      QC.traceFrom @(CERTS era) testGlobals maxCertsPerTx ge env st0

    let certsCreds = catMaybes . traceSignals OldestFirst $ certsTrace
        (lastState_, _) = lastState certsTrace
        (certs, creds) = unzip certsCreds
        (scriptCreds, keyCreds) = partition isScript creds
        keyCreds' = concat (keyCreds : map scriptWitnesses scriptCreds)

        refunds =
          getTotalRefundsTxCerts
            pp
            (lookupDepositDState certDState)
            (const Nothing)
            certs

        deposits = getTotalDepositsTxCerts pp (`Map.member` psStakePoolParams certPState) certs

        certWits = concat (keyCredAsWitness <$> keyCreds')
        certScripts = extractScriptCred <$> scriptCreds
    pure
      ( certs
      , deposits
      , refunds
      , lastState_
      , certWits
      , certScripts
      )
    where
      isScript (ScriptCred _) = True
      isScript _ = False

      scriptWitnesses :: CertCred era -> [CertCred era]
      scriptWitnesses (ScriptCred (_, stakeScript)) =
        StakeCred <$> witnessHashes''
        where
          witnessHashes = coerceKeyRole <$> scriptKeyCombination (Proxy @era) stakeScript
          witnessHashes'' = coerceKeyRole <$> mapMaybe lookupWit witnessHashes
      scriptWitnesses _ = []

      lookupWit = flip Map.lookup ksIndexedStakingKeys

extractScriptCred ::
  (HasCallStack, Era era, Show (Core.Script era)) =>
  CertCred era ->
  (Core.Script era, Core.Script era)
extractScriptCred (ScriptCred c) = c
extractScriptCred x =
  error $
    "extractScriptCred: use only for Script Credentials - "
      <> show x

keyCredAsWitness ::
  (HasCallStack, Era era, Show (Core.Script era)) =>
  CertCred era ->
  [KeyPair 'Witness]
keyCredAsWitness (DelegateCred c) = asWitness <$> c
keyCredAsWitness (CoreKeyCred c) = asWitness <$> c
keyCredAsWitness (StakeCred c) = [asWitness c]
keyCredAsWitness (PoolCred c) = [asWitness c]
keyCredAsWitness NoCred = []
keyCredAsWitness x =
  error $
    "keyCredAsWitness: use only for Script Credentials - "
      <> show x
