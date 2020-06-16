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

import Cardano.Crypto.Hash (HashAlgorithm)
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
import qualified Data.Map.Strict as Map (lookup)
import Data.Maybe (catMaybes)
import Data.Sequence.Strict (StrictSeq)
import qualified Data.Sequence.Strict as StrictSeq
import GHC.Generics (Generic)
import Numeric.Natural (Natural)
import Shelley.Spec.Ledger.BaseTypes (Globals, ShelleyBase)
import Shelley.Spec.Ledger.Coin (Coin)
import Shelley.Spec.Ledger.Delegation.Certificates (isDeRegKey)
import Shelley.Spec.Ledger.Keys (HasKeyRole (coerceKeyRole))
import Shelley.Spec.Ledger.LedgerState
  ( AccountState,
    _pstate,
    _stPools,
  )
import Shelley.Spec.Ledger.PParams (PParams, PParams' (..))
import Shelley.Spec.Ledger.STS.Delpl (DelplEnv (..))
import Shelley.Spec.Ledger.Slot (SlotNo (..))
import Shelley.Spec.Ledger.Tx (getKeyCombination)
import Shelley.Spec.Ledger.TxData (Ix, Ptr (..))
import Shelley.Spec.Ledger.UTxO (totalDeposits)
import Test.QuickCheck (Gen)
import Test.Shelley.Spec.Ledger.ConcreteCryptoTypes (DCert, DELPL, DPState)
import Test.Shelley.Spec.Ledger.Generator.Constants (Constants (..))
import Test.Shelley.Spec.Ledger.Generator.Core (GenEnv (..), KeySpace (..))
import Test.Shelley.Spec.Ledger.Generator.Delegation (CertCred (..), genDCert)
import Test.Shelley.Spec.Ledger.Utils (testGlobals)

-- | This is a non-spec STS used to generate a sequence of certificates with
-- witnesses.
data CERTS h

instance HashAlgorithm h => STS (CERTS h) where
  type Environment (CERTS h) = (SlotNo, Ix, PParams, AccountState)
  type State (CERTS h) = (DPState h, Ix)
  type Signal (CERTS h) = Maybe (DCert h, CertCred h)

  type BaseM (CERTS h) = ShelleyBase

  data PredicateFailure (CERTS h)
    = CertsFailure (PredicateFailure (DELPL h))
    deriving (Show, Eq, Generic)

  initialRules = []
  transitionRules = [certsTransition]

certsTransition :: forall h. HashAlgorithm h => TransitionRule (CERTS h)
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
      dpState' <- trans @(DELPL h) $ TRC (DelplEnv slot ptr pp acnt, dpState, cert)

      pure (dpState', nextCertIx + 1)
    Nothing ->
      pure (dpState, nextCertIx)

instance HashAlgorithm h => Embed (DELPL h) (CERTS h) where
  wrapFailed = CertsFailure

instance HashAlgorithm h => QC.HasTrace (CERTS h) (GenEnv h) where
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

  type BaseEnv (CERTS h) = Globals
  interpretSTS globals act = runIdentity $ runReaderT act globals

-- | Generate certificates and also return the associated witnesses and
-- deposits and refunds required.
genDCerts ::
  forall h.
  HashAlgorithm h =>
  GenEnv h ->
  PParams ->
  DPState h ->
  SlotNo ->
  Natural ->
  AccountState ->
  Gen (StrictSeq (DCert h), [CertCred h], Coin, Coin, DPState h)
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
      QC.traceFrom @(CERTS h) testGlobals maxCertsPerTx ge env st0

    let certsCreds = catMaybes . traceSignals OldestFirst $ certsTrace
        (lastState_, _) = lastState certsTrace
        (certs, creds) = unzip certsCreds
        deRegStakeCreds = filter isDeRegKey certs

    withScriptCreds <- concat <$> mapM extendWithScriptCred creds

    pure
      ( StrictSeq.fromList certs,
        withScriptCreds,
        totalDeposits pparams (_stPools (_pstate dpState)) certs,
        (_keyDeposit pparams) * (fromIntegral $ length deRegStakeCreds),
        lastState_
      )
    where
      extendWithScriptCred cred =
        case cred of
          ScriptCred (_, stakeScript) -> do
            let witnessHashes = getKeyCombination stakeScript
                witnessHashes' = fmap coerceKeyRole witnessHashes
                foo = catMaybes (map lookupWit witnessHashes')
                witnessHashes'' = fmap coerceKeyRole foo
                witnesses = StakeCred <$> witnessHashes''
            pure (witnesses ++ [cred])
          _ ->
            return [cred]
      lookupWit = flip Map.lookup ksIndexedStakingKeys
