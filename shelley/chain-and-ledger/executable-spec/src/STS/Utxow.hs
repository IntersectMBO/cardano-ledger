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
import qualified Data.Set as Set

import           Keys
import           LedgerState hiding (dms)
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
  IRC (UtxoEnv slots pp stakeKeys stakePools dms) <- judgmentContext
  trans @(UTXO hashAlgo dsignAlgo vrfAlgo) $ IRC (UtxoEnv slots pp stakeKeys stakePools dms)

utxoWitnessed
  :: forall hashAlgo dsignAlgo vrfAlgo
   . ( HashAlgorithm hashAlgo
     , DSIGNAlgorithm dsignAlgo
     , VRFAlgorithm vrfAlgo
     , Signable dsignAlgo (TxBody hashAlgo dsignAlgo vrfAlgo)
     )
   => TransitionRule (UTXOW hashAlgo dsignAlgo vrfAlgo)
utxoWitnessed = do
  TRC (UtxoEnv slot pp stakeKeys stakePools _dms, u, tx@(Tx _ wits _))
    <- judgmentContext
  verifiedWits tx == Valid ?! InvalidWitnessesUTXOW
  let witnessKeys = Set.map witKeyHash wits
  let needed = witsVKeyNeeded (_utxo u) tx _dms
  needed `Set.isSubsetOf` witnessKeys  ?! MissingVKeyWitnessesUTXOW

  -- check multi-signature scripts
  let utxo' = _utxo u

  all (\(hs, validator) -> hashScript validator == hs
      && validateScript validator tx) (Map.toList $ txwitsScript tx)
    ?!ScriptWitnessNotValidatingUTXOW

  scriptsNeeded utxo' tx == Map.keysSet (txwitsScript tx)
    ?! MissingScriptWitnessesUTXOW

  trans @(UTXO hashAlgo dsignAlgo vrfAlgo)
    $ TRC (UtxoEnv slot pp stakeKeys stakePools _dms, u, tx)

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
