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
  , PredicateFailure(..)
  )
where

import           Control.Monad (foldM)
import           Data.Foldable (toList)
import qualified Data.Map.Strict as Map
import           Data.Sequence (Seq)
import qualified Data.Set as Set

import           Control.State.Transition

import           Coin (Coin)
import           Keys
import           Ledger.Core (dom, range, (⋪), (◁), (⨃))
import           LedgerState
import           PParams
import           Slot
import           STS.Ledger
import           Tx
import           Updates (Applications (..), UpdateState (..), apps, newAVs)

data LEDGERS hashAlgo dsignAlgo vrfAlgo

data LedgersEnv
  = LedgersEnv
    { ledgersSlot     :: Slot
    , ledgersPp       :: PParams
    , ledgersReserves :: Coin
    }

instance
  ( HashAlgorithm hashAlgo
  , DSIGNAlgorithm dsignAlgo
  , Signable dsignAlgo (TxBody hashAlgo dsignAlgo vrfAlgo)
  , VRFAlgorithm vrfAlgo
  )
  => STS (LEDGERS hashAlgo dsignAlgo vrfAlgo)
 where
  type State (LEDGERS hashAlgo dsignAlgo vrfAlgo) = LedgerState hashAlgo dsignAlgo vrfAlgo
  type Signal (LEDGERS hashAlgo dsignAlgo vrfAlgo) = Seq (Tx hashAlgo dsignAlgo vrfAlgo)
  type Environment (LEDGERS hashAlgo dsignAlgo vrfAlgo) = LedgersEnv
  data PredicateFailure (LEDGERS hashAlgo dsignAlgo vrfAlgo)
    = LedgerFailure (PredicateFailure (LEDGER hashAlgo dsignAlgo vrfAlgo))
    deriving (Show, Eq)

  initialRules = [pure emptyLedgerState]
  transitionRules = [ledgersTransition]

ledgersTransition
  :: forall hashAlgo dsignAlgo vrfAlgo
   . ( HashAlgorithm hashAlgo
     , DSIGNAlgorithm dsignAlgo
     , Signable dsignAlgo (TxBody hashAlgo dsignAlgo vrfAlgo)
     , VRFAlgorithm vrfAlgo
     )
  => TransitionRule (LEDGERS hashAlgo dsignAlgo vrfAlgo)
ledgersTransition = do
  TRC (LedgersEnv slot pp _reserves, ls, txwits) <- judgmentContext
  let (u, dw) = (_utxoState ls, _delegationState ls)
  (u'', dw'') <-
    foldM
        (\(u', dw') (ix, tx) ->
          trans @(LEDGER hashAlgo dsignAlgo vrfAlgo)
            $ TRC (LedgerEnv slot ix pp _reserves, (u', dw'), tx)
        )
        (u, dw)
      $ zip [0 ..] $ toList txwits

  let UTxOState utxo' dep fee (UpdateState ppup aup favs avs) = u''
  let (favs', ready) = Map.partitionWithKey (\s _ -> s > slot) favs
  let avs' = Applications $ apps avs ⨃ (Map.toList . apps $ newAVs avs ready)
  let u''' = UTxOState utxo' dep fee (UpdateState ppup aup favs' avs')

  let ds = _dstate dw''
      ps = _pstate dw''
      fGenDelegs_    = _fGenDelegs ds
      GenDelegs genDelegs_ = _genDelegs ds
      (curr, fGenDelegs') = Map.partitionWithKey (\(s, _) _ -> s <= slot) fGenDelegs_
  let maxSlot = maximum . Set.map fst . Map.keysSet
  let latestPerGKey gk =
        ( (maxSlot . Map.filterWithKey (\(_, c) _ -> c == gk)) curr
        , gk)
  let genDelegsKeys = Set.map
                  latestPerGKey
                  (Set.map snd (Map.keysSet curr))
  let genDelegs' = Map.mapKeys snd $ genDelegsKeys ◁ curr
  let oldGenDelegs = range (dom genDelegs' ◁ genDelegs_)
  let cs' = (oldGenDelegs ⋪ _cCounters ps) ⨃ fmap (\x -> (x, 0)) (Map.elems genDelegs')
  let dw''' = dw'' { _dstate = ds { _fGenDelegs = fGenDelegs'
                                  , _genDelegs = GenDelegs $ genDelegs_ ⨃ Map.toList genDelegs'}
                   , _pstate = ps { _cCounters = cs' }
                   }

  pure $ LedgerState u''' dw''' (_txSlotIx ls)

instance
  ( HashAlgorithm hashAlgo
  , DSIGNAlgorithm dsignAlgo
  , Signable dsignAlgo (TxBody hashAlgo dsignAlgo vrfAlgo)
  , VRFAlgorithm vrfAlgo
  )
  => Embed (LEDGER hashAlgo dsignAlgo vrfAlgo) (LEDGERS hashAlgo dsignAlgo vrfAlgo)
 where
  wrapFailed = LedgerFailure
