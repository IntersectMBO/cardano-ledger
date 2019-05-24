-- | Transition system that models the application of multiple transactions to
-- the UTxO part of the ledger state.

module Cardano.Ledger.Spec.STS.UTXOWS where

data UTXOWS id

instance (Ord id, HasTypeReps id) => STS (UTXOWS id) where
  type State (UTXOWS id) = UTxOState id
  type Signal (UTXOWS id) = [TxWits id]
  type Environment (UTXOWS id) = UTxOEnv id
  data PredicateFailure (UTXOWS id)
    = UtxowFailure (PredicateFailure (UTXOW id))
    deriving (Eq, Show)

  initialRules =
    [ do
        IRC env <- judgmentContext
        trans @(UTXOW id) $ IRC env
    ]

  transitionRules =
    [ do
        TRC (env, utxo, txWits) <- judgmentContext
        case (txWits :: [TxWits id]) of
          []     -> return utxo
          (tx:gamma) -> do
            utxo'  <- trans @(UTXOWS id) $ TRC (env, utxo, gamma)
            utxo'' <- trans @(UTXOW id)  $ TRC (env, utxo', tx)
            return utxo''
    ]

instance (Ord id, HasTypeReps id) => Embed (UTXOW id) (UTXOWS id) where
  wrapFailed = UtxowFailure
