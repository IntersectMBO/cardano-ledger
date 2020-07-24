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
import Shelley.Spec.Ledger.BaseTypes (Globals, ShelleyBase)
import Shelley.Spec.Ledger.Coin (Coin)
import Shelley.Spec.Ledger.Crypto (Crypto)
import Shelley.Spec.Ledger.Delegation.Certificates (isDeRegKey)
import Shelley.Spec.Ledger.Keys (HasKeyRole (coerceKeyRole), KeyPair, KeyRole (..), asWitness)
import Shelley.Spec.Ledger.LedgerState
  ( AccountState,
    DPState,
    _pParams,
    _pstate,
  )
import Shelley.Spec.Ledger.PParams (PParams, PParams' (..))
import Shelley.Spec.Ledger.STS.Delpl (DELPL, DelplEnv (..))
import Shelley.Spec.Ledger.Scripts (MultiSig)
import Shelley.Spec.Ledger.Slot (SlotNo (..))
import Shelley.Spec.Ledger.Tx (getKeyCombination)
import Shelley.Spec.Ledger.TxData (DCert, Ix, Ptr (..))
import Shelley.Spec.Ledger.UTxO (totalDeposits)
import Test.QuickCheck (Gen)
import Test.Shelley.Spec.Ledger.Generator.Constants (Constants (..))
import Test.Shelley.Spec.Ledger.Generator.Core (GenEnv (..), KeySpace (..))
import Test.Shelley.Spec.Ledger.Generator.Delegation (CertCred (..), genDCert)
import Test.Shelley.Spec.Ledger.Utils (testGlobals)

-- | This is a non-spec STS used to generate a sequence of certificates with
-- witnesses.
data CERTS c

instance Crypto c => STS (CERTS c) where
  type Environment (CERTS c) = (SlotNo, Ix, PParams, AccountState)
  type State (CERTS c) = (DPState c, Ix)
  type Signal (CERTS c) = Maybe (DCert c, CertCred c)

  type BaseM (CERTS c) = ShelleyBase

  data PredicateFailure (CERTS c)
    = CertsFailure (PredicateFailure (DELPL c))
    deriving (Show, Eq, Generic)

  initialRules = []
  transitionRules = [certsTransition]

certsTransition :: forall c. Crypto c => TransitionRule (CERTS c)
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
      dpState' <- trans @(DELPL c) $ TRC (DelplEnv slot ptr pp acnt, dpState, cert)

      pure (dpState', nextCertIx + 1)
    Nothing ->
      pure (dpState, nextCertIx)

instance Crypto c => Embed (DELPL c) (CERTS c) where
  wrapFailed = CertsFailure

instance Crypto c => QC.HasTrace (CERTS c) (GenEnv c) where
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

  type BaseEnv (CERTS c) = Globals
  interpretSTS globals act = runIdentity $ runReaderT act globals

-- | Generate certificates and also return the associated witnesses and
-- deposits and refunds required.
genDCerts ::
  forall c.
  Crypto c =>
  GenEnv c ->
  PParams ->
  DPState c ->
  SlotNo ->
  Natural ->
  AccountState ->
  Gen
    ( StrictSeq (DCert c),
      Coin,
      Coin,
      DPState c,
      ([KeyPair 'Witness c], [(MultiSig c, MultiSig c)])
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
      QC.traceFrom @(CERTS c) testGlobals maxCertsPerTx ge env st0

    let certsCreds = catMaybes . traceSignals OldestFirst $ certsTrace
        (lastState_, _) = lastState certsTrace
        (certs, creds) = unzip certsCreds
        deRegStakeCreds = filter isDeRegKey certs
        (scriptCreds, keyCreds) = partition isScript creds
        keyCreds' = concat (keyCreds : map scriptWitnesses scriptCreds)

    pure
      ( StrictSeq.fromList certs,
        totalDeposits pparams (_pParams (_pstate dpState)) certs,
        (_keyDeposit pparams) * (fromIntegral $ length deRegStakeCreds),
        lastState_,
        ( concat (keyCredAsWitness <$> keyCreds'),
          scriptCredMultisig <$> scriptCreds
        )
      )
    where
      isScript (ScriptCred _) = True
      isScript _ = False

      scriptWitnesses :: CertCred c -> [CertCred c]
      scriptWitnesses (ScriptCred (_, stakeScript)) =
        StakeCred <$> witnessHashes''
        where
          witnessHashes = getKeyCombination stakeScript
          witnessHashes' = fmap coerceKeyRole witnessHashes
          witnessHashes'' = fmap coerceKeyRole (catMaybes (map lookupWit witnessHashes'))
      scriptWitnesses _ = []

      lookupWit = flip Map.lookup ksIndexedStakingKeys

scriptCredMultisig :: (HasCallStack, Crypto c) => CertCred c -> (MultiSig c, MultiSig c)
scriptCredMultisig (ScriptCred c) = c
scriptCredMultisig x = error $ "scriptCredMultisig: use only for Script Credentials - " <> show x

keyCredAsWitness :: (HasCallStack, Crypto c) => CertCred c -> [KeyPair 'Witness c]
keyCredAsWitness (DelegateCred c) = asWitness <$> c
keyCredAsWitness (CoreKeyCred c) = asWitness <$> c
keyCredAsWitness (StakeCred c) = [asWitness c]
keyCredAsWitness (PoolCred c) = [asWitness c]
keyCredAsWitness NoCred = []
keyCredAsWitness x = error $ "keyCredAsWitness: use only for Script Credentials - " <> show x
