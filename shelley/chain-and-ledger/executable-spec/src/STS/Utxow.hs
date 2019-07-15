{-# LANGUAGE EmptyDataDecls        #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE UndecidableInstances  #-}

module STS.Utxow
  ( UTXOW
  )
where

import qualified Data.Set                  as Set

import           Delegation.Certificates
import           Keys
import           LedgerState hiding (dms)
import           PParams
import           Slot
import           Tx

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
  type Environment (UTXOW hashAlgo dsignAlgo)
    = ( Slot
      , PParams
      , StakeKeys hashAlgo dsignAlgo
      , StakePools hashAlgo dsignAlgo
      , Dms dsignAlgo
      )
  data PredicateFailure (UTXOW hashAlgo dsignAlgo)
    = InvalidWitnessesUTXOW
    | MissingWitnessesUTXOW
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
  IRC (slots, pp, stakeKeys, stakePools, dms) <- judgmentContext
  trans @(UTXO hashAlgo dsignAlgo) $ IRC (slots, pp, stakeKeys, stakePools, dms)

utxoWitnessed
  :: forall hashAlgo dsignAlgo
   . ( HashAlgorithm hashAlgo
     , DSIGNAlgorithm dsignAlgo
     , Signable dsignAlgo (TxBody hashAlgo dsignAlgo)
     )
   => TransitionRule (UTXOW hashAlgo dsignAlgo)
utxoWitnessed = do
  TRC ((slot, pp, stakeKeys, stakePools, _dms), u, tx@(Tx _ wits _))
    <- judgmentContext
  verifiedWits tx == Valid ?! InvalidWitnessesUTXOW
  let witnessKeys = Set.map (\(WitVKey vk _) -> hashKey vk) wits
  witsNeeded (_utxo u) tx _dms == witnessKeys  ?! MissingWitnessesUTXOW
  trans @(UTXO hashAlgo dsignAlgo)
    $ TRC ((slot, pp, stakeKeys, stakePools, _dms), u, tx)

instance
  ( HashAlgorithm hashAlgo
  , DSIGNAlgorithm dsignAlgo
  , Signable dsignAlgo (TxBody hashAlgo dsignAlgo)
  )
  => Embed (UTXO hashAlgo dsignAlgo) (UTXOW hashAlgo dsignAlgo)
 where
  wrapFailed = UtxoFailure
