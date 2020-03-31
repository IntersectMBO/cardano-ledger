{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Shelley.Spec.Ledger.STS.Ledgers
  ( LEDGERS
  , LedgersEnv (..)
  , PredicateFailure(..)
  )
where

import           Cardano.Binary (FromCBOR (..), ToCBOR (..))
import           Cardano.Prelude (NoUnexpectedThunks (..))
import           Control.Monad (foldM)
import           Control.State.Transition
import           Data.Foldable (toList)
import qualified Data.Map.Strict as Map
import           Data.Sequence (Seq)
import qualified Data.Set as Set
import           Data.Typeable (Typeable)
import           GHC.Generics (Generic)
import           Ledger.Core ((◁), (⨃))
import           Shelley.Spec.Ledger.BaseTypes
import           Shelley.Spec.Ledger.Coin (Coin)
import           Shelley.Spec.Ledger.Crypto
import           Shelley.Spec.Ledger.Keys
import           Shelley.Spec.Ledger.LedgerState (DState (..), LedgerState (..), UTxOState (..),
                     emptyLedgerState, _delegationState, _dstate, _fGenDelegs, _genDelegs,
                     _utxoState)
import           Shelley.Spec.Ledger.PParams
import           Shelley.Spec.Ledger.Slot
import           Shelley.Spec.Ledger.STS.Ledger (LEDGER, LedgerEnv (..))
import           Shelley.Spec.Ledger.Tx

data LEDGERS crypto

data LedgersEnv
  = LedgersEnv
    { ledgersSlotNo   :: SlotNo
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
    deriving (Show, Eq, Generic)

  initialRules = [pure emptyLedgerState]
  transitionRules = [ledgersTransition]

instance NoUnexpectedThunks (PredicateFailure (LEDGERS crypto))

instance
  (Typeable crypto, Crypto crypto)
  => ToCBOR (PredicateFailure (LEDGERS crypto))
 where
   toCBOR (LedgerFailure e) = toCBOR e

instance
  (Crypto crypto)
  => FromCBOR (PredicateFailure (LEDGERS crypto))
 where
  fromCBOR = LedgerFailure <$> fromCBOR

ledgersTransition
  :: forall crypto
   . ( Crypto crypto
     , Signable (DSIGN crypto) (TxBody crypto)
     )
  => TransitionRule (LEDGERS crypto)
ledgersTransition = do
  TRC (LedgersEnv slot pp reserves, ls, txwits) <- judgmentContext
  let (u, dp) = (_utxoState ls, _delegationState ls)
  (u'', dp'') <-
    foldM
        (\(u', dp') (ix, tx) ->
          trans @(LEDGER crypto)
            $ TRC (LedgerEnv slot ix pp reserves, (u', dp'), tx)
        )
        (u, dp)
      $ zip [0 ..] $ toList txwits

  let UTxOState utxo deposits fee ppup = u''
  let ds = _dstate dp''
  let DState _ _ _ _ fGenDelegs_ (GenDelegs genDelegs_) _ = ds

  let (curr, fGenDelegs') = Map.partitionWithKey (\(s, _) _ -> s <= slot) fGenDelegs_
  let maxSlotNo = maximum . Set.map fst . Map.keysSet
  let latestPerGKey gk =
        ( (maxSlotNo . Map.filterWithKey (\(_, c) _ -> c == gk)) curr
        , gk)
  let genDelegsKeys = Set.map
                  latestPerGKey
                  (Set.map snd (Map.keysSet curr))
  let genDelegs' = Map.mapKeys snd $ genDelegsKeys ◁ curr

  let u''' = UTxOState utxo deposits fee ppup
  let dp''' = dp'' { _dstate = ds { _fGenDelegs = fGenDelegs'
                                  , _genDelegs = GenDelegs $ genDelegs_ ⨃ Map.toList genDelegs'
                                  }
                   }

  pure $ LedgerState u''' dp'''

instance
  ( Crypto crypto
  , Signable (DSIGN crypto) (TxBody crypto)
  )
  => Embed (LEDGER crypto) (LEDGERS crypto)
 where
  wrapFailed = LedgerFailure
