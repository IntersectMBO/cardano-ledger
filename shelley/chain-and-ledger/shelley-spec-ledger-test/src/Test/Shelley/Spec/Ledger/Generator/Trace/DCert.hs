{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE UndecidableInstances #-}

module Test.Shelley.Spec.Ledger.Generator.Trace.DCert (genDCerts) where

import Cardano.Ledger.Era (Era)
import Control.Monad.Trans.Reader (runReaderT)
import Control.State.Transition
  ( BaseM,
    Embed,
    Environment,
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
    wrapFailed,
  )
import Control.State.Transition.Trace (TraceOrder (OldestFirst), lastState, traceSignals)
import qualified Control.State.Transition.Trace.Generator.QuickCheck as QC
import Data.Functor.Identity (runIdentity)
import Data.List (partition)
import qualified Data.Map.Strict as Map (lookup)
import Data.Maybe (catMaybes)
import Data.Sequence.Strict (StrictSeq)
import qualified Data.Sequence.Strict as StrictSeq
import GHC.Generics (Generic)
import GHC.Stack (HasCallStack)
import Numeric.Natural (Natural)
import Shelley.Spec.Ledger.API
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
import Shelley.Spec.Ledger.BaseTypes (Globals, ShelleyBase)
import Shelley.Spec.Ledger.Coin (Coin)
import Shelley.Spec.Ledger.Delegation.Certificates (isDeRegKey)
import Shelley.Spec.Ledger.Keys (HasKeyRole (coerceKeyRole), asWitness)
import Shelley.Spec.Ledger.PParams (PParams, PParams' (..))
import Shelley.Spec.Ledger.Scripts (MultiSig)
import Shelley.Spec.Ledger.Slot (SlotNo (..))
import Shelley.Spec.Ledger.Tx (getKeyCombination)
import Shelley.Spec.Ledger.TxBody (Ix)
import Shelley.Spec.Ledger.UTxO (totalDeposits)
import qualified Cardano.Ledger.Val as Val
import Test.QuickCheck (Gen)
import Test.Shelley.Spec.Ledger.Generator.Constants (Constants (..))
import Test.Shelley.Spec.Ledger.Generator.Core (GenEnv (..), KeySpace (..))
import Test.Shelley.Spec.Ledger.Generator.Delegation (CertCred (..), genDCert)
import Test.Shelley.Spec.Ledger.Utils (testGlobals)

-- | This is a non-spec STS used to generate a sequence of certificates with
-- witnesses.
data CERTS era

data CertsPredicateFailure era
  = CertsFailure (PredicateFailure (DELPL era))
  deriving (Show, Eq, Generic)

instance Era era => STS (CERTS era) where
  type Environment (CERTS era) = (SlotNo, Ix, PParams, AccountState)
  type State (CERTS era) = (DPState era, Ix)
  type Signal (CERTS era) = Maybe (DCert era, CertCred era)
  type PredicateFailure (CERTS era) = CertsPredicateFailure era

  type BaseM (CERTS era) = ShelleyBase

  initialRules = []
  transitionRules = [certsTransition]

certsTransition :: forall era. Era era => TransitionRule (CERTS era)
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
      dpState' <- trans @(DELPL era) $ TRC (DelplEnv slot ptr pp acnt, dpState, cert)

      pure (dpState', nextCertIx + 1)
    Nothing ->
      pure (dpState, nextCertIx)

instance Era era => Embed (DELPL era) (CERTS era) where
  wrapFailed = CertsFailure

instance Era era => QC.HasTrace (CERTS era) (GenEnv era) where
  envGen _ = error "HasTrace CERTS - envGen not required"

  sigGen
    ( GenEnv
        ks
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
  Era era =>
  GenEnv era ->
  PParams ->
  DPState era ->
  SlotNo ->
  Natural ->
  AccountState ->
  Gen
    ( StrictSeq (DCert era),
      Coin,
      Coin,
      DPState era,
      ([KeyPair 'Witness era], [(MultiSig era, MultiSig era)])
    )
genDCerts
  ge@( GenEnv
         KeySpace_ {ksIndexedStakingKeys}
         Constants {maxCertsPerTx}
       )
  pparams
  dpState
  slot
  txIx
  acnt = do
    let env = (slot, txIx, pparams, acnt)
        st0 = (dpState, 0)

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
        totalDeposits pparams (_pParams (_pstate dpState)) certs,
        Val.scale (length deRegStakeCreds) (_keyDeposit pparams),
        lastState_,
        ( concat (keyCredAsWitness <$> keyCreds'),
          scriptCredMultisig <$> scriptCreds
        )
      )
    where
      isScript (ScriptCred _) = True
      isScript _ = False

      scriptWitnesses :: CertCred era -> [CertCred era]
      scriptWitnesses (ScriptCred (_, stakeScript)) =
        StakeCred <$> witnessHashes''
        where
          witnessHashes = getKeyCombination stakeScript
          witnessHashes' = fmap coerceKeyRole witnessHashes
          witnessHashes'' = fmap coerceKeyRole (catMaybes (map lookupWit witnessHashes'))
      scriptWitnesses _ = []

      lookupWit = flip Map.lookup ksIndexedStakingKeys

scriptCredMultisig ::
  (HasCallStack, Era era) =>
  CertCred era ->
  (MultiSig era, MultiSig era)
scriptCredMultisig (ScriptCred c) = c
scriptCredMultisig x =
  error $
    "scriptCredMultisig: use only for Script Credentials - "
      <> show x

keyCredAsWitness ::
  (HasCallStack, Era era) =>
  CertCred era ->
  [KeyPair 'Witness era]
keyCredAsWitness (DelegateCred c) = asWitness <$> c
keyCredAsWitness (CoreKeyCred c) = asWitness <$> c
keyCredAsWitness (StakeCred c) = [asWitness c]
keyCredAsWitness (PoolCred c) = [asWitness c]
keyCredAsWitness NoCred = []
keyCredAsWitness x =
  error $
    "keyCredAsWitness: use only for Script Credentials - "
      <> show x
