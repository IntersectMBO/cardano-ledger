{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
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
import           Cardano.Prelude (NoUnexpectedThunks (..))
import           Coin
import           Control.Monad.Trans.Reader (runReaderT)
import           Control.State.Transition
import           Control.State.Transition.Generator
import           Data.Foldable (toList)
import           Data.Functor.Identity (runIdentity)
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import           Delegation.Certificates
import           GHC.Generics (Generic)
import           Hedgehog (Gen)
import           Keys
import           Ledger.Core (dom, range, (∪), (⊆), (⋪))
import           LedgerState (UTxOState (..), consumed, decayedTx, keyRefunds, minfee, produced,
                     txsize)
import           Lens.Micro ((^.))
import           PParams
import           Slot
import           STS.Up
import           Tx
import           Updates (emptyUpdateState)
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
    deriving (Eq, Show, Generic)
  transitionRules = [utxoInductive]
  initialRules = [initialLedgerState]

instance NoUnexpectedThunks (PredicateFailure (UTXO crypto))

initialLedgerState :: InitialRule (UTXO crypto)
initialLedgerState = do
  IRC _ <- judgmentContext
  pure $ UTxOState (UTxO Map.empty) (Coin 0) (Coin 0) emptyUpdateState

utxoInductive
  :: forall crypto
   . Crypto crypto
  => TransitionRule (UTXO crypto)
utxoInductive = do
  TRC (UtxoEnv slot pp stakeCreds stakepools genDelegs, u, tx) <- judgmentContext
  let UTxOState utxo deposits' fees ups = u
  let txb = _body tx

  _ttl txb >= slot ?! ExpiredUTxO (_ttl txb) slot

  txins txb /= Set.empty ?! InputSetEmptyUTxO

  let minFee = minfee pp txb
      txFee  = _txfee txb
  minFee <= txFee ?! FeeTooSmallUTxO minFee txFee

  txins txb ⊆ dom utxo ?! BadInputsUTxO

  let consumed_ = consumed pp utxo stakeCreds txb
      produced_ = produced pp stakepools txb
  consumed_ == produced_ ?! ValueNotConservedUTxO consumed_ produced_

  -- process Update Proposals
  ups' <- trans @(UP crypto) $ TRC (UpdateEnv slot pp genDelegs, ups, txup tx)

  let outputCoins = [c | (TxOut _ c) <- Set.toList (range (txouts txb))]
  all (0 <=) outputCoins ?! NegativeOutputsUTxO

  let maxTxSize_ = fromIntegral (_maxTxSize pp)
      txSize_ = txsize txb
  txSize_ <= maxTxSize_ ?! MaxTxSizeUTxO txSize_ maxTxSize_

  let refunded = keyRefunds pp stakeCreds txb
  decayed <- liftSTS $ decayedTx pp stakeCreds txb
  let txCerts = toList $ txb ^. certs
  let depositChange = totalDeposits pp stakepools txCerts - (refunded + decayed)

  pure UTxOState
        { _utxo      = (txins txb ⋪ utxo) ∪ txouts txb
        , _deposited = deposits' + depositChange
        , _fees      = fees + (_txfee txb) + decayed
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
