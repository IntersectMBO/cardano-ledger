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
{-# LANGUAGE UndecidableInstances #-}

module Test.Cardano.Ledger.Shelley.Generator.Trace.DCert
  ( CERTS,
    genDCerts,
  )
where

import Cardano.Ledger.BaseTypes (CertIx, Globals, ShelleyBase, TxIx)
import Cardano.Ledger.Coin (Coin)
import qualified Cardano.Ledger.Core as Core
import Cardano.Ledger.Era (Crypto, Era)
import Cardano.Ledger.Keys (HasKeyRole (coerceKeyRole), asWitness)
import Cardano.Ledger.Shelley.API
  ( AccountState,
    DCert,
    DELPL,
    DPState (..),
    DelplEnv (..),
    KeyPair (..),
    KeyRole (..),
    PState (..),
    Ptr (..),
  )
import Cardano.Ledger.Shelley.Delegation.Certificates (isDeRegKey)
import Cardano.Ledger.Shelley.Rules.Delpl (DelplEvent, DelplPredicateFailure)
import Cardano.Ledger.Shelley.UTxO (totalDeposits)
import Cardano.Ledger.Slot (SlotNo (..))
import Cardano.Ledger.Val ((<×>))
import Control.Monad.Trans.Reader (runReaderT)
import Control.State.Transition
  ( BaseM,
    Embed,
    Environment,
    Event,
    PredicateFailure,
    STS,
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
import Control.State.Transition.Trace (TraceOrder (OldestFirst), lastState, traceSignals)
import qualified Control.State.Transition.Trace.Generator.QuickCheck as QC
import Data.Functor.Identity (runIdentity)
import Data.List (partition)
import qualified Data.Map.Strict as Map
import Data.Maybe (catMaybes, mapMaybe)
import Data.Proxy (Proxy (..))
import Data.Sequence.Strict (StrictSeq)
import qualified Data.Sequence.Strict as StrictSeq
import GHC.Generics (Generic)
import GHC.Records (HasField (getField))
import GHC.Stack (HasCallStack)
import Test.Cardano.Ledger.Shelley.Generator.Constants (Constants (..))
import Test.Cardano.Ledger.Shelley.Generator.Core (GenEnv (..), KeySpace (..))
import Test.Cardano.Ledger.Shelley.Generator.Delegation (CertCred (..), genDCert)
import Test.Cardano.Ledger.Shelley.Generator.EraGen (EraGen (..))
import Test.Cardano.Ledger.Shelley.Generator.ScriptClass (scriptKeyCombination)
import Test.Cardano.Ledger.Shelley.Utils (testGlobals)
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
  ( Eq (PredicateFailure (Core.EraRule "DELPL" era))
  ) =>
  Eq (CertsPredicateFailure era)

deriving stock instance
  ( Show (PredicateFailure (Core.EraRule "DELPL" era))
  ) =>
  Show (CertsPredicateFailure era)

instance
  ( Era era,
    Embed (Core.EraRule "DELPL" era) (CERTS era),
    Environment (Core.EraRule "DELPL" era) ~ DelplEnv era,
    State (Core.EraRule "DELPL" era) ~ DPState (Crypto era),
    Signal (Core.EraRule "DELPL" era) ~ DCert (Crypto era)
  ) =>
  STS (CERTS era)
  where
  type Environment (CERTS era) = (SlotNo, TxIx, Core.PParams era, AccountState)
  type State (CERTS era) = (DPState (Crypto era), CertIx)
  type Signal (CERTS era) = Maybe (DCert (Crypto era), CertCred era)
  type PredicateFailure (CERTS era) = CertsPredicateFailure era
  type Event (CERTS era) = CertsEvent era

  type BaseM (CERTS era) = ShelleyBase

  initialRules = []
  transitionRules = [certsTransition]

certsTransition ::
  forall era.
  ( Embed (Core.EraRule "DELPL" era) (CERTS era),
    Environment (Core.EraRule "DELPL" era) ~ DelplEnv era,
    State (Core.EraRule "DELPL" era) ~ DPState (Crypto era),
    Signal (Core.EraRule "DELPL" era) ~ DCert (Crypto era)
  ) =>
  TransitionRule (CERTS era)
certsTransition = do
  TRC
    ( (slot, txIx, pp, acnt),
      (dpState, nextCertIx),
      c
      ) <-
    judgmentContext

  case c of
    Just (cert, _wits) -> do
      let ptr = Ptr slot txIx nextCertIx
      dpState' <-
        trans @(Core.EraRule "DELPL" era) $
          TRC (DelplEnv slot ptr pp acnt, dpState, cert)

      pure (dpState', succ nextCertIx)
    Nothing ->
      pure (dpState, nextCertIx)

instance
  ( Era era,
    STS (DELPL era),
    PredicateFailure (Core.EraRule "DELPL" era) ~ DelplPredicateFailure era,
    Event (Core.EraRule "DELPL" era) ~ DelplEvent era
  ) =>
  Embed (DELPL era) (CERTS era)
  where
  wrapFailed = CertsFailure
  wrapEvent = CertsEvent

instance
  ( EraGen era,
    Embed (Core.EraRule "DELPL" era) (CERTS era),
    Environment (Core.EraRule "DELPL" era) ~ DelplEnv era,
    State (Core.EraRule "DELPL" era) ~ DPState (Crypto era),
    Signal (Core.EraRule "DELPL" era) ~ DCert (Crypto era)
  ) =>
  QC.HasTrace (CERTS era) (GenEnv era)
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
      genDCert
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
genDCerts ::
  forall era.
  ( EraGen era,
    Embed (Core.EraRule "DELPL" era) (CERTS era),
    Environment (Core.EraRule "DELPL" era) ~ DelplEnv era,
    State (Core.EraRule "DELPL" era) ~ DPState (Crypto era),
    Signal (Core.EraRule "DELPL" era) ~ DCert (Crypto era)
  ) =>
  GenEnv era ->
  Core.PParams era ->
  DPState (Crypto era) ->
  SlotNo ->
  TxIx ->
  AccountState ->
  Gen
    ( StrictSeq (DCert (Crypto era)),
      Coin,
      Coin,
      DPState (Crypto era),
      ([KeyPair 'Witness (Crypto era)], [(Core.Script era, Core.Script era)])
    )
genDCerts
  ge@( GenEnv
         KeySpace_ {ksIndexedStakingKeys}
         _scriptspace
         Constants {maxCertsPerTx}
       )
  pparams
  dpState
  slot
  txIx
  acnt = do
    let env = (slot, txIx, pparams, acnt)
        st0 = (dpState, minBound)

    certsTrace <-
      QC.traceFrom @(CERTS era) testGlobals maxCertsPerTx ge env st0

    let certsCreds = catMaybes . traceSignals OldestFirst $ certsTrace
        (lastState_, _) = lastState certsTrace
        (certs, creds) = unzip certsCreds
        deRegStakeCreds = filter isDeRegKey certs
        (scriptCreds, keyCreds) = partition isScript creds
        keyCreds' = concat (keyCreds : map scriptWitnesses scriptCreds)

    pure
      ( StrictSeq.fromList certs,
        totalDeposits pparams (`Map.notMember` pools) certs,
        length deRegStakeCreds <×> getField @"_keyDeposit" pparams,
        lastState_,
        ( concat (keyCredAsWitness <$> keyCreds'),
          extractScriptCred <$> scriptCreds
        )
      )
    where
      pools = _pParams (_pstate dpState)
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
  [KeyPair 'Witness (Crypto era)]
keyCredAsWitness (DelegateCred c) = asWitness <$> c
keyCredAsWitness (CoreKeyCred c) = asWitness <$> c
keyCredAsWitness (StakeCred c) = [asWitness c]
keyCredAsWitness (PoolCred c) = [asWitness c]
keyCredAsWitness NoCred = []
keyCredAsWitness x =
  error $
    "keyCredAsWitness: use only for Script Credentials - "
      <> show x
