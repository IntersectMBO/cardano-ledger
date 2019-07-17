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

import           Data.Maybe (mapMaybe)
import qualified Data.Set                  as Set
import qualified Data.Map.Strict           as Map

import           Delegation.Certificates
import           Keys
import           LedgerState hiding (dms)
import           PParams
import           Slot
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
  type Environment (UTXOW hashAlgo dsignAlgo)
    = ( Slot
      , PParams
      , StakeKeys hashAlgo dsignAlgo
      , StakePools hashAlgo dsignAlgo
      , Dms dsignAlgo
      )
  data PredicateFailure (UTXOW hashAlgo dsignAlgo)
    = InvalidWitnessesUTXOW
    | MissingVKeyWitnessesUTXOW
    | MissingScriptWitnessesUTXOW
    | MissingScriptWitnessesRwdUTXOW
    | ScriptWitnessNotValidatingUTXOW
    | ScriptWitnessNotValidatingRwdUTXOW
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
  TRC ((slot, pp, stakeKeys, stakePools, _dms), u, tx@(Tx txbody wits _))
    <- judgmentContext
  verifiedWits tx == Valid ?! InvalidWitnessesUTXOW
  let witnessKeys = Set.map (\(WitVKey vk _) -> hashKey vk) wits
  let needed = witsNeeded (_utxo u) tx _dms
  needed `Set.isSubsetOf` witnessKeys  ?! MissingVKeyWitnessesUTXOW

  -- check multi-signature scripts
  let utxo' = _utxo u
  let scriptWits = txwitsScript tx
  let scriptIns = txinsScript (txins txbody) utxo'
  let validatorScripts = validators scriptIns utxo' scriptWits
  scriptIns == Map.keysSet validatorScripts
    ?! MissingScriptWitnessesUTXOW
  Map.foldr' (\scr b -> b && validateScript scr tx) True validatorScripts
    ?! ScriptWitnessNotValidatingUTXOW

  -- script locked reward accounts
  let withdrawals = _wdrls txbody
  let addrRwdScr  =
        Map.keysSet $ Map.filterWithKey (\rwd _ ->
                                           case getRwdHK rwd of
                                             ScriptHashStake _ -> True
                                             _                 -> False) withdrawals
  let addrRwdScrHash = extractScriptHash $ map getRwdHK $ Set.toList addrRwdScr
  all (flip Map.member scriptWits) addrRwdScrHash
    ?! MissingScriptWitnessesRwdUTXOW
  let scriptValidators = mapMaybe (flip Map.lookup scriptWits) addrRwdScrHash
  (all (\(hs, scr) -> hs == hashScript scr && validateScript scr tx) $
    zip addrRwdScrHash scriptValidators) ?! ScriptWitnessNotValidatingUTXOW

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
