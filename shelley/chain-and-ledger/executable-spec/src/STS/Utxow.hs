{-# LANGUAGE EmptyDataDecls        #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}

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
import           UTxO

import           Control.State.Transition

import           STS.Utxo

data UTXOW

instance STS UTXOW where
  type State UTXOW = UTxOState
  type Signal UTXOW = Tx
  type Environment UTXOW = (Slot, PParams, StakeKeys, StakePools, Dms)
  data PredicateFailure UTXOW = InvalidWitnessesUTXOW
                            | MissingWitnessesUTXOW
                            | UtxoFailure (PredicateFailure UTXO)
                                deriving (Eq, Show)
  transitionRules = [utxoWitnessed]
  initialRules = [initialLedgerStateUTXOW]

initialLedgerStateUTXOW :: InitialRule UTXOW
initialLedgerStateUTXOW = do
  IRC (slots, pp, stakeKeys, stakePools, dms) <- judgmentContext
  trans @UTXO $ IRC (slots, pp, stakeKeys, stakePools, dms)

utxoWitnessed :: TransitionRule UTXOW
utxoWitnessed = do
  TRC ((slot, pp, stakeKeys, stakePools, _dms), u, tx@(Tx _ wits))
    <- judgmentContext
  verifiedWits tx == Valid ?! InvalidWitnessesUTXOW
  let witnessKeys = Set.map (\(Wit vk _) -> hashKey vk) wits
  witsNeeded (_utxo u) tx _dms == witnessKeys  ?! MissingWitnessesUTXOW
  trans @UTXO $ TRC ((slot, pp, stakeKeys, stakePools, _dms), u, tx)

instance Embed UTXO UTXOW where
  wrapFailed = UtxoFailure
