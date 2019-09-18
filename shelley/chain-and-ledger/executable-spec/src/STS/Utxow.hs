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
import           Tx
import           TxData
import           UTxO

import           Control.State.Transition

import           STS.Utxo

data UTXOW hashAlgo dsignAlgo

instance
  ( HashAlgorithm hashAlgo
  , DSIGNAlgorithm dsignAlgo
  , Signable dsignAlgo (TxBody hashAlgo dsignAlgo)
  )
  => STS (UTXOW hashAlgo dsignAlgo)
 where
  type State (UTXOW hashAlgo dsignAlgo) = UTxOState hashAlgo dsignAlgo
  type Signal (UTXOW hashAlgo dsignAlgo) = Tx hashAlgo dsignAlgo
  type Environment (UTXOW hashAlgo dsignAlgo) = UtxoEnv hashAlgo dsignAlgo
  data PredicateFailure (UTXOW hashAlgo dsignAlgo)
    = InvalidWitnessesUTXOW
    | MissingVKeyWitnessesUTXOW
    | MissingScriptWitnessesUTXOW
    | ScriptWitnessNotValidatingUTXOW
    | UtxoFailure (PredicateFailure (UTXO hashAlgo dsignAlgo))
    deriving (Eq, Show)

  transitionRules = [utxoWitnessed]
  initialRules = [initialLedgerStateUTXOW]

initialLedgerStateUTXOW
  :: forall hashAlgo dsignAlgo
   . ( HashAlgorithm hashAlgo
     , DSIGNAlgorithm dsignAlgo
     , Signable dsignAlgo (TxBody hashAlgo dsignAlgo)
     )
   => InitialRule (UTXOW hashAlgo dsignAlgo)
initialLedgerStateUTXOW = do
  IRC (UtxoEnv slots pp stakeKeys stakePools dms) <- judgmentContext
  trans @(UTXO hashAlgo dsignAlgo) $ IRC (UtxoEnv slots pp stakeKeys stakePools dms)

utxoWitnessed
  :: forall hashAlgo dsignAlgo
   . ( HashAlgorithm hashAlgo
     , DSIGNAlgorithm dsignAlgo
     , Signable dsignAlgo (TxBody hashAlgo dsignAlgo)
     )
   => TransitionRule (UTXOW hashAlgo dsignAlgo)
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

  trans @(UTXO hashAlgo dsignAlgo)
    $ TRC (UtxoEnv slot pp stakeKeys stakePools _dms, u, tx)

instance
  ( HashAlgorithm hashAlgo
  , DSIGNAlgorithm dsignAlgo
  , Signable dsignAlgo (TxBody hashAlgo dsignAlgo)
  )
  => Embed (UTXO hashAlgo dsignAlgo) (UTXOW hashAlgo dsignAlgo)
 where
  wrapFailed = UtxoFailure
