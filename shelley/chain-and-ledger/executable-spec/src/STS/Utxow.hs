{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module STS.Utxow
  ( UTXOW
  , PredicateFailure(..)
  )
where

import qualified Data.Map.Strict as Map
import qualified Data.Sequence as Seq (filter)
import qualified Data.Set as Set

import           BaseTypes (intervalValue, (==>))
import           Delegation.Certificates (isInstantaneousRewards)
import           Keys
import           Ledger.Core (range, (∩))
import           LedgerState hiding (genDelegs)
import           PParams (_d)
import           STS.Utxo
import           Tx
import           TxData
import           UTxO

import           Control.State.Transition
import           Control.State.Transition.Generator (HasTrace, envGen, sigGen)

import           Hedgehog (Gen)

data UTXOW hashAlgo dsignAlgo vrfAlgo

instance
  ( HashAlgorithm hashAlgo
  , DSIGNAlgorithm dsignAlgo
  , VRFAlgorithm vrfAlgo
  , Signable dsignAlgo (TxBody hashAlgo dsignAlgo vrfAlgo)
  )
  => STS (UTXOW hashAlgo dsignAlgo vrfAlgo)
 where
  type State (UTXOW hashAlgo dsignAlgo vrfAlgo) = UTxOState hashAlgo dsignAlgo vrfAlgo
  type Signal (UTXOW hashAlgo dsignAlgo vrfAlgo) = Tx hashAlgo dsignAlgo vrfAlgo
  type Environment (UTXOW hashAlgo dsignAlgo vrfAlgo) = UtxoEnv hashAlgo dsignAlgo
  data PredicateFailure (UTXOW hashAlgo dsignAlgo vrfAlgo)
    = InvalidWitnessesUTXOW
    | MissingVKeyWitnessesUTXOW
    | MissingScriptWitnessesUTXOW
    | ScriptWitnessNotValidatingUTXOW
    | UtxoFailure (PredicateFailure (UTXO hashAlgo dsignAlgo vrfAlgo))
    | MIRInsufficientGenesisSigsUTXOW
    deriving (Eq, Show)

  transitionRules = [utxoWitnessed]
  initialRules = [initialLedgerStateUTXOW]

initialLedgerStateUTXOW
  :: forall hashAlgo dsignAlgo vrfAlgo
   . ( HashAlgorithm hashAlgo
     , DSIGNAlgorithm dsignAlgo
     , VRFAlgorithm vrfAlgo
     , Signable dsignAlgo (TxBody hashAlgo dsignAlgo vrfAlgo)
     )
   => InitialRule (UTXOW hashAlgo dsignAlgo vrfAlgo)
initialLedgerStateUTXOW = do
  IRC (UtxoEnv slots pp stakeKeys stakePools genDelegs) <- judgmentContext
  trans @(UTXO hashAlgo dsignAlgo vrfAlgo) $ IRC (UtxoEnv slots pp stakeKeys stakePools genDelegs)

utxoWitnessed
  :: forall hashAlgo dsignAlgo vrfAlgo
   . ( HashAlgorithm hashAlgo
     , DSIGNAlgorithm dsignAlgo
     , VRFAlgorithm vrfAlgo
     , Signable dsignAlgo (TxBody hashAlgo dsignAlgo vrfAlgo)
     )
   => TransitionRule (UTXOW hashAlgo dsignAlgo vrfAlgo)
utxoWitnessed = do
  TRC (UtxoEnv slot pp stakeKeys stakePools _genDelegs, u, tx@(Tx txbody wits _))
    <- judgmentContext
  verifiedWits tx == Valid ?! InvalidWitnessesUTXOW
  let witnessKeys = Set.map witKeyHash wits
  let needed = witsVKeyNeeded (_utxo u) tx _genDelegs
  needed `Set.isSubsetOf` witnessKeys  ?! MissingVKeyWitnessesUTXOW

  -- check multi-signature scripts
  let utxo' = _utxo u

  all (\(hs, validator) -> hashScript validator == hs
      && validateScript validator tx) (Map.toList $ txwitsScript tx)
    ?!ScriptWitnessNotValidatingUTXOW

  scriptsNeeded utxo' tx == Map.keysSet (txwitsScript tx)
    ?! MissingScriptWitnessesUTXOW

  -- check genesis keys signatures for instantaneous rewards certificates
  let mirCerts = Seq.filter isInstantaneousRewards $ _certs txbody
      GenDelegs genMapping = _genDelegs
      genSig = (Set.map undiscriminateKeyHash $ range genMapping) ∩ Set.map witKeyHash wits
  (    (not $ null mirCerts)
   ==> (0 < intervalValue (_d pp) && Set.size genSig >= 5))
    ?! MIRInsufficientGenesisSigsUTXOW

  trans @(UTXO hashAlgo dsignAlgo vrfAlgo)
    $ TRC (UtxoEnv slot pp stakeKeys stakePools _genDelegs, u, tx)

instance
  ( HashAlgorithm hashAlgo
  , DSIGNAlgorithm dsignAlgo
  , VRFAlgorithm vrfAlgo
  , Signable dsignAlgo (TxBody hashAlgo dsignAlgo vrfAlgo)
  )
  => Embed (UTXO hashAlgo dsignAlgo vrfAlgo) (UTXOW hashAlgo dsignAlgo vrfAlgo)
 where
  wrapFailed = UtxoFailure

instance
    ( HashAlgorithm hashAlgo
    , DSIGNAlgorithm dsignAlgo
    , VRFAlgorithm vrfAlgo
    , Signable dsignAlgo (TxBody hashAlgo dsignAlgo vrfAlgo)
    )
  => HasTrace (UTXOW hashAlgo dsignAlgo vrfAlgo) where
  envGen _ = undefined :: Gen (UtxoEnv hashAlgo dsignAlgo)
  sigGen _ _ = undefined :: Gen (Tx hashAlgo dsignAlgo vrfAlgo)
