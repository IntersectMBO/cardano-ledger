{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module STS.Ledgers
  ( LEDGERS
  )
where

import           Control.Monad (foldM)
import           Data.Foldable (toList)
import qualified Data.Map.Strict as Map
import           Data.Sequence (Seq)

import           Keys
import           LedgerState
import           PParams
import           Slot
import           Tx
import           Updates (newAVs)

import           Control.State.Transition

import           STS.Ledger

data LEDGERS hashAlgo dsignAlgo

instance
  ( HashAlgorithm hashAlgo
  , DSIGNAlgorithm dsignAlgo
  , Signable dsignAlgo (TxBody hashAlgo dsignAlgo)
  )
  => STS (LEDGERS hashAlgo dsignAlgo)
 where
  type State (LEDGERS hashAlgo dsignAlgo) = LedgerState hashAlgo dsignAlgo
  type Signal (LEDGERS hashAlgo dsignAlgo) = Seq (Tx hashAlgo dsignAlgo)
  type Environment (LEDGERS hashAlgo dsignAlgo) = (Slot, PParams)
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
  TRC ((slot, pp), ls, txwits) <- judgmentContext
  let (u, dw) = (_utxoState ls, _delegationState ls)
  (u'', dw'') <-
    foldM
        (\(u', dw') (ix, tx) ->
          trans @(LEDGER hashAlgo dsignAlgo)
            $ TRC ((slot, ix, pp), (u', dw'), tx)
        )
        (u, dw)
      $ zip [0 ..] $ toList txwits

  let UTxOState utxo' dep fee (ppup, aup, favs, avs) = u''
  let (favs', ready) = Map.partitionWithKey (\s _ -> s > slot) favs
  let avs' = newAVs avs ready
  let u''' = UTxOState utxo' dep fee (ppup, aup, favs', avs')
  pure $ LedgerState u''' dw'' (_txSlotIx ls)

instance
  ( HashAlgorithm hashAlgo
  , DSIGNAlgorithm dsignAlgo
  , Signable dsignAlgo (TxBody hashAlgo dsignAlgo)
  )
  => Embed (LEDGER hashAlgo dsignAlgo) (LEDGERS hashAlgo dsignAlgo)
 where
  wrapFailed = LedgerFailure
