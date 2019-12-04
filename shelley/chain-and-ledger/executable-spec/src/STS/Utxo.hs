{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module STS.Utxo
  ( UTXO
  , UtxoEnv (..)
  , PredicateFailure(..)
  )
where

import           BaseTypes
import           Cardano.Ledger.Shelley.Crypto
import           Coin
import           Control.Monad.Trans.Reader (runReaderT)
import           Control.State.Transition
import           Control.State.Transition.Generator
import           Data.Foldable (toList)
import           Data.Functor.Identity (runIdentity)
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import           Delegation.Certificates
import           Hedgehog (Gen)
import           Keys
import           Ledger.Core (dom, range, (∪), (⊆), (⋪))
import           LedgerState
import           Lens.Micro ((^.))
import           PParams
import           Slot
import           STS.Up
import           Tx
import           Updates
import           UTxO

data UTXO crypto

data UtxoEnv crypto
  = UtxoEnv
      SlotNo
      PParams
      (StakeCreds crypto)
      (StakePools crypto)
      (GenDelegs crypto)
      deriving(Show)

instance
  Crypto crypto
  => STS (UTXO crypto)
 where
  type State (UTXO crypto) = UTxOState crypto
  type Signal (UTXO crypto) = Tx crypto
  type Environment (UTXO crypto) = UtxoEnv crypto
  type BaseM (UTXO crypto) = ShelleyBase
  data PredicateFailure (UTXO crypto)
    = BadInputsUTxO
    | ExpiredUTxO SlotNo SlotNo
    | MaxTxSizeUTxO Integer Integer
    | InputSetEmptyUTxO
    | FeeTooSmallUTxO Coin Coin
    | ValueNotConservedUTxO Coin Coin
    | NegativeOutputsUTxO
    | UpdateFailure (PredicateFailure (UP crypto))
    deriving (Eq, Show)
  transitionRules = [utxoInductive]
  initialRules = [initialLedgerState]

initialLedgerState :: InitialRule (UTXO crypto)
initialLedgerState = do
  IRC _ <- judgmentContext
  pure $ UTxOState (UTxO Map.empty) (Coin 0) (Coin 0) emptyUpdateState

utxoInductive
  :: forall crypto
   . Crypto crypto
  => TransitionRule (UTXO crypto)
utxoInductive = do
  TRC (UtxoEnv slot_ pp stakeKeys stakePools genDelegs_, u, tx) <- judgmentContext
  let txBody = _body tx

  _ttl txBody >= slot_ ?! ExpiredUTxO (_ttl txBody) slot_

  txins txBody /= Set.empty ?! InputSetEmptyUTxO

  let minFee = minfee pp txBody
      txFee  = txBody ^. txfee
  minFee <= txFee ?! FeeTooSmallUTxO minFee txFee

  txins txBody ⊆ dom (u ^. utxo) ?! BadInputsUTxO

  let consumed_ = consumed pp (u ^. utxo) stakeKeys txBody
      produced_ = produced pp stakePools txBody
  consumed_ == produced_ ?! ValueNotConservedUTxO consumed_ produced_

  -- process Update Proposals
  ups' <- trans @(UP crypto) $ TRC (UpdateEnv slot_ pp genDelegs_, u ^. ups, txup tx)

  let outputCoins = [c | (TxOut _ c) <- Set.toList (range (txouts txBody))]
  all (0 <=) outputCoins ?! NegativeOutputsUTxO

  let maxTxSize_ = fromIntegral (_maxTxSize pp)
      txSize_ = txsize txBody
  txSize_ <= maxTxSize_ ?! MaxTxSizeUTxO txSize_ maxTxSize_

  let refunded = keyRefunds pp stakeKeys txBody
  decayed <- liftSTS $ decayedTx pp stakeKeys txBody
  let txCerts = toList $ txBody ^. certs

      depositChange = deposits pp stakePools txCerts - (refunded + decayed)

  pure UTxOState
        { _utxo      = (txins txBody ⋪ (u ^. utxo)) ∪ txouts txBody
        , _deposited = _deposited u + depositChange
        , _fees      = _fees u + (txBody ^. txfee) + decayed
        , _ups       = ups'
        }

instance Crypto crypto
  => Embed (UP crypto) (UTXO crypto)
 where
  wrapFailed = UpdateFailure

instance
  ( Crypto crypto
  , Signable (DSIGN crypto) (TxBody crypto)
  )
  => HasTrace (UTXO crypto) where
  envGen _ = undefined :: Gen (UtxoEnv crypto)
  sigGen _ _ = undefined :: Gen (Tx crypto)

  type BaseEnv (UTXO crypto) = Globals
  interpretSTS globals act = runIdentity $ runReaderT act globals
