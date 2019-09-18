{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module STS.Ledgers
  ( LEDGERS
  , LedgersEnv (..)
  )
where

import           Control.Monad (foldM)
import           Data.Foldable (toList)
import qualified Data.Map.Strict as Map
import           Data.Sequence (Seq)
import qualified Data.Set as Set

import           Keys
import           Ledger.Core (dom, range, (⋪), (◁), (⨃))
import           LedgerState
import           PParams
import           Slot
import           Tx
import           Updates (Applications (..), UpdateState (..), apps, newAVs)

import           Control.State.Transition

import           STS.Ledger

data LEDGERS hashAlgo dsignAlgo

data LedgersEnv
  = LedgersEnv Slot PParams

instance
  ( HashAlgorithm hashAlgo
  , DSIGNAlgorithm dsignAlgo
  , Signable dsignAlgo (TxBody hashAlgo dsignAlgo)
  )
  => STS (LEDGERS hashAlgo dsignAlgo)
 where
  type State (LEDGERS hashAlgo dsignAlgo) = LedgerState hashAlgo dsignAlgo
  type Signal (LEDGERS hashAlgo dsignAlgo) = Seq (Tx hashAlgo dsignAlgo)
  type Environment (LEDGERS hashAlgo dsignAlgo) = LedgersEnv
  data PredicateFailure (LEDGERS hashAlgo dsignAlgo)
    = LedgerFailure (PredicateFailure (LEDGER hashAlgo dsignAlgo))
    deriving (Show, Eq)

  initialRules = [pure emptyLedgerState]
  transitionRules = [ledgersTransition]

ledgersTransition
  :: forall hashAlgo dsignAlgo
   . ( HashAlgorithm hashAlgo
     , DSIGNAlgorithm dsignAlgo
     , Signable dsignAlgo (TxBody hashAlgo dsignAlgo)
     )
  => TransitionRule (LEDGERS hashAlgo dsignAlgo)
ledgersTransition = do
  TRC (LedgersEnv slot pp, ls, txwits) <- judgmentContext
  let (u, dw) = (_utxoState ls, _delegationState ls)
  (u'', dw'') <-
    foldM
        (\(u', dw') (ix, tx) ->
          trans @(LEDGER hashAlgo dsignAlgo)
            $ TRC (LedgerEnv slot ix pp, (u', dw'), tx)
        )
        (u, dw)
      $ zip [0 ..] $ toList txwits

  let UTxOState utxo' dep fee (UpdateState ppup aup favs avs) = u''
  let (favs', ready) = Map.partitionWithKey (\s _ -> s > slot) favs
  let avs' = Applications $ apps avs ⨃ (Map.toList . apps $ newAVs avs ready)
  let u''' = UTxOState utxo' dep fee (UpdateState ppup aup favs' avs')

  let ds = _dstate dw''
      ps = _pstate dw''
      fdms_    = _fdms ds
      Dms dms_ = _dms ds
      (curr, fdms') = Map.partitionWithKey (\(s, _) _ -> s <= slot) fdms_
  let maxSlot = maximum . Set.map fst . Map.keysSet
  let latestPerGKey gk =
        ( (maxSlot . Map.filterWithKey (\(_, c) _ -> c == gk)) curr
        , gk)
  let dmsKeys = Set.map
                  latestPerGKey
                  (Set.map snd (Map.keysSet curr))
  let dms' = Map.mapKeys snd $ dmsKeys ◁ curr
  let oldGenDelegs = range (dom dms' ◁ dms_)
  let cs' = (oldGenDelegs ⋪ _cCounters ps) ⨃ fmap (\x -> (x, 0)) (Map.elems dms')
  let dw''' = dw'' { _dstate = ds { _fdms = fdms'
                                  , _dms = Dms $ dms_ ⨃ Map.toList dms'}
                   , _pstate = ps { _cCounters = cs' }
                   }

  pure $ LedgerState u''' dw''' (_txSlotIx ls)

instance
  ( HashAlgorithm hashAlgo
  , DSIGNAlgorithm dsignAlgo
  , Signable dsignAlgo (TxBody hashAlgo dsignAlgo)
  )
  => Embed (LEDGER hashAlgo dsignAlgo) (LEDGERS hashAlgo dsignAlgo)
 where
  wrapFailed = LedgerFailure
