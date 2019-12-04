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
import           BaseTypes
import           Control.State.Transition

import           Cardano.Ledger.Shelley.Crypto
import           Coin (Coin)
import           Keys
import           Ledger.Core ((◁), (⨃))
import           LedgerState
import           PParams
import           Slot
import           STS.Ledger
import           Tx
import           Updates (Applications (..), UpdateState (..), apps, newAVs)

data LEDGERS crypto

data LedgersEnv
  = LedgersEnv
    { ledgersSlotNo     :: SlotNo
    , ledgersPp       :: PParams
    , ledgersReserves :: Coin
    }

instance
  ( Crypto crypto
  , Signable (DSIGN crypto) (TxBody crypto)
  )
  => STS (LEDGERS crypto)
 where
  type State (LEDGERS crypto) = LedgerState crypto
  type Signal (LEDGERS crypto) = Seq (Tx crypto)
  type Environment (LEDGERS crypto) = LedgersEnv
  type BaseM (LEDGERS crypto) = ShelleyBase
  data PredicateFailure (LEDGERS crypto)
    = LedgerFailure (PredicateFailure (LEDGER crypto))
    deriving (Show, Eq)

  initialRules = [pure emptyLedgerState]
  transitionRules = [ledgersTransition]

ledgersTransition
  :: forall crypto
   . ( Crypto crypto
     , Signable (DSIGN crypto) (TxBody crypto)
     )
  => TransitionRule (LEDGERS crypto)
ledgersTransition = do
  TRC (LedgersEnv slot pp _reserves, ls, txwits) <- judgmentContext
  let (u, dw) = (_utxoState ls, _delegationState ls)
  (u'', dw'') <-
    foldM
        (\(u', dw') (ix, tx) ->
          trans @(LEDGER crypto)
            $ TRC (LedgerEnv slot ix pp _reserves, (u', dw'), tx)
        )
        (u, dw)
      $ zip [0 ..] $ toList txwits

  let UTxOState utxo' dep fee (UpdateState ppup aup favs avs) = u''
  let (favs', ready) = Map.partitionWithKey (\s _ -> s > slot) favs
  let avs' = Applications $ apps avs ⨃ (Map.toList . apps $ newAVs avs ready)
  let u''' = UTxOState utxo' dep fee (UpdateState ppup aup favs' avs')

  let ds = _dstate dw''
      fGenDelegs_    = _fGenDelegs ds
      GenDelegs genDelegs_ = _genDelegs ds
      (curr, fGenDelegs') = Map.partitionWithKey (\(s, _) _ -> s <= slot) fGenDelegs_
  let maxSlotNo = maximum . Set.map fst . Map.keysSet
  let latestPerGKey gk =
        ( (maxSlotNo . Map.filterWithKey (\(_, c) _ -> c == gk)) curr
        , gk)
  let genDelegsKeys = Set.map
                  latestPerGKey
                  (Set.map snd (Map.keysSet curr))
  let genDelegs' = Map.mapKeys snd $ genDelegsKeys ◁ curr
  let dw''' = dw'' { _dstate = ds { _fGenDelegs = fGenDelegs'
                                  , _genDelegs = GenDelegs $ genDelegs_ ⨃ Map.toList genDelegs'
                                  }
                   }

  pure $ LedgerState u''' dw''' (_txSlotIx ls)

instance
  ( Crypto crypto
  , Signable (DSIGN crypto) (TxBody crypto)
  )
  => Embed (LEDGER crypto) (LEDGERS crypto)
 where
  wrapFailed = LedgerFailure
