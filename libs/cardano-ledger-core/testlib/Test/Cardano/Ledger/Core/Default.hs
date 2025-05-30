module Test.Cardano.Ledger.Core.Default () where

instance Default ChainAccountState where
  def = ChainAccountState (Coin 0) (Coin 0)
