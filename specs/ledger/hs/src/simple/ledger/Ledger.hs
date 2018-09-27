{-# LANGUAGE OverloadedStrings #-}
module Ledger where

import qualified Crypto.Hash           as Crypto
import qualified Data.ByteArray        as BA
import qualified Data.ByteString.Char8 as BS
import           Extension
import           Ledger.Abstract
import           Ledger.Transition
import qualified Data.Set as Set
import qualified Data.Map.Strict as Map

instance Ledger.Abstract.HasHash Tx where
  hash = Crypto.hash

instance Ledger.Abstract.HasHash VKey where
  hash = Crypto.hash

instance BA.ByteArrayAccess VKey where
  length        = BA.length . BS.pack . show
  withByteArray = BA.withByteArray . BS.pack . show

instance BA.ByteArrayAccess Tx where
  length        = BA.length . BS.pack . show
  withByteArray = BA.withByteArray . BS.pack  . show

---------------------------------------------------------------------------------
-- UTxO transitions
---------------------------------------------------------------------------------
getUtxo :: [TxWits] -> UTxO
getUtxo = foldr (\(TxWits tx _) ls -> txins tx /◁ ls `union` txouts tx ) (UTxO Map.empty)

utxoInductive :: Transition
utxoInductive = Transition
  { _tPredicates =
    [ predValidInputs, predNoIncreasedBalance]
  , _tUncheckedApply = \st sig -> case sig of
      NoSignal    -> st
      TxSignal tx -> tx : st
  }
  where
    predValidInputs :: Predicate
    predValidInputs = Predicate
      { _pName = "predValidInputs"
      , _pValidate = \st sig -> case sig of
          NoSignal -> Passed
          TxSignal (TxWits tx _) -> if txins tx `Set.isSubsetOf` unspentInputs (getUtxo st)
              then Passed
              else Failed BadInputs
            where unspentInputs (UTxO utxo) = Map.keysSet utxo
      }
    predNoIncreasedBalance :: Predicate
    predNoIncreasedBalance = Predicate
      { _pName = "predValidInputs"
      , _pValidate = \st sig -> case sig of
          NoSignal -> Passed
          TxSignal (TxWits tx _) -> if balance (txouts tx) <= balance (txins tx ◁ getUtxo st)
            then Passed
            else Failed IncreasedTotalBalance
      }
