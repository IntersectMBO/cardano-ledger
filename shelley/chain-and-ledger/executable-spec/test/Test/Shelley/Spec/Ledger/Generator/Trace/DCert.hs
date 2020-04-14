{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE UndecidableInstances #-}


module Test.Shelley.Spec.Ledger.Generator.Trace.DCert
  (genDCerts)
  where

import           Control.Monad.Trans.Reader (runReaderT)
import           Data.Functor.Identity (runIdentity)
import qualified Data.Map.Strict as Map (lookup)
import           Data.Maybe (catMaybes)
import           Data.Sequence.Strict (StrictSeq)
import qualified Data.Sequence.Strict as StrictSeq
import           Numeric.Natural (Natural)
import           Test.QuickCheck (Gen)

import           Control.State.Transition (BaseM, Embed, Environment, PredicateFailure, STS, Signal,
                     State, TRC (..), TransitionRule, initialRules, judgmentContext, trans,
                     transitionRules, wrapFailed)
import           Control.State.Transition.Trace (TraceOrder (OldestFirst), traceSignals)
import qualified Control.State.Transition.Trace.Generator.QuickCheck as QC
import           GHC.Generics (Generic)
import           Shelley.Spec.Ledger.BaseTypes (Globals, ShelleyBase)
import           Shelley.Spec.Ledger.Coin (Coin)
import           Shelley.Spec.Ledger.Delegation.Certificates (decayKey, isDeRegKey)
import           Shelley.Spec.Ledger.LedgerState (keyRefund, _dstate, _pstate, _stPools, _stkCreds)
import           Shelley.Spec.Ledger.PParams (PParams)
import           Shelley.Spec.Ledger.Slot (SlotNo (..))
import           Shelley.Spec.Ledger.STS.Delpl (DelplEnv (..))
import           Shelley.Spec.Ledger.Tx (getKeyCombination)
import           Shelley.Spec.Ledger.TxData (Ix, Ptr (..))
import           Shelley.Spec.Ledger.UTxO (totalDeposits)

import           Test.Shelley.Spec.Ledger.ConcreteCryptoTypes (DCert, DELPL, DPState)
import           Test.Shelley.Spec.Ledger.Generator.Constants (Constants (..))
import           Test.Shelley.Spec.Ledger.Generator.Core (GenEnv (..), KeySpace (..))
import           Test.Shelley.Spec.Ledger.Generator.Delegation (CertCred (..), genDCert)
import           Test.Shelley.Spec.Ledger.Utils (testGlobals)

-- | This is a non-spec STS used to generate a sequence of certificates with
-- witnesses.
data CERTS

instance STS CERTS where
  type Environment CERTS = (SlotNo, Ix, PParams, Coin)
  type State CERTS = (DPState, Ix)
  type Signal CERTS = Maybe (DCert, CertCred)

  type BaseM CERTS = ShelleyBase

  data PredicateFailure CERTS
    = CertsFailure (PredicateFailure DELPL)
    deriving (Show, Eq, Generic)

  initialRules = []
  transitionRules = [certsTransition]

certsTransition :: TransitionRule CERTS
certsTransition = do
  TRC ( (slot, txIx, pp, reserves)
      , (dpState, nextCertIx)
      , c) <- judgmentContext

  case c of
    Just (cert, _wits) -> do
      let ptr = Ptr slot txIx nextCertIx
      dpState' <- trans @DELPL $ TRC (DelplEnv slot ptr pp reserves, dpState, cert)

      pure (dpState', nextCertIx + 1)
    Nothing ->
      pure (dpState, nextCertIx)

instance Embed DELPL CERTS where
  wrapFailed = CertsFailure

instance QC.HasTrace CERTS GenEnv where
  envGen _ = error "HasTrace CERTS - envGen not required"

  sigGen
    ( GenEnv
        ( KeySpace_
            { ksCoreNodes,
              ksKeyPairs,
              ksKeyPairsByStakeHash,
              ksMSigScripts,
              ksVRFKeyPairs
            }
          )
        constants
      )
    (slot, _txIx, pparams, _reserves)
    (dpState, _certIx) =
      genDCert
        constants
        ksKeyPairs
        ksMSigScripts
        (fst <$> ksCoreNodes)
        ksVRFKeyPairs
        ksKeyPairsByStakeHash
        pparams
        dpState
        slot

  shrinkSignal = const []

  type BaseEnv CERTS = Globals
  interpretSTS globals act = runIdentity $ runReaderT act globals

-- | Generate certificates and also return the associated witnesses and
-- deposits and refunds required.
genDCerts
  :: GenEnv
  -> PParams
  -> DPState
  -> SlotNo
  -> Natural
  -> Natural
  -> Coin
  -> Gen (StrictSeq DCert, [CertCred], Coin, Coin)
genDCerts
  ge@( GenEnv
         KeySpace_ {ksKeyPairsByHash}
         Constants {maxCertsPerTx}
       )
  pparams
  dpState
  slot
  ttl
  txIx
  reserves = do
  let env = (slot, txIx, pparams, reserves)
      st0 = (dpState, 0)

  certsCreds <-
    catMaybes . traceSignals OldestFirst <$>
      QC.traceFrom @CERTS testGlobals maxCertsPerTx ge env st0

  let (certs, creds) = unzip certsCreds
      deRegStakeCreds = filter isDeRegKey certs
      slotWithTTL = slot + SlotNo (fromIntegral ttl)

  withScriptCreds <- concat <$> mapM extendWithScriptCred creds

  pure ( StrictSeq.fromList certs
       , withScriptCreds
       , totalDeposits pparams (_stPools (_pstate dpState)) certs
       , sum (certRefund slotWithTTL <$> deRegStakeCreds) )

  where
    (dval, dmin, lambda) = decayKey pparams
    stkCreds_ = (_stkCreds . _dstate) dpState
    certRefund = keyRefund dval dmin lambda stkCreds_

    extendWithScriptCred cred =
      case cred of
        ScriptCred (_, stakeScript) -> do
          let witnessHashes = getKeyCombination stakeScript
              witnesses = KeyCred <$> catMaybes (map lookupWit witnessHashes)
          pure (witnesses ++ [cred])
        _ ->
          return [cred]

    lookupWit = flip Map.lookup ksKeyPairsByHash
