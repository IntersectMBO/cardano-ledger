{-# LANGUAGE TypeFamilies      #-}

module Ledger where

import Control.State.Transition
import qualified Crypto.Hash as Crypto
import qualified Data.ByteArray as BA
import qualified Data.ByteString.Char8 as BS
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import Ledger.Abstract
import UTxO

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

data ProtocolConstants = ProtocolConstants
  { pcMinFee :: Tx -> Value }

-- | UTXO transition system
data UTXO

instance STS UTXO where
  type State UTXO = UTxO
  type Signal UTXO = Tx
  type Environment UTXO = ProtocolConstants
  data PredicateFailure UTXO
    = BadInputs
    | FeeTooLow
    | IncreasedTotalBalance
    deriving Show

  rules =
    [ Rule [] $ Base (UTxO Map.empty)
    , utxoInductive
    ]

utxoInductive :: Rule UTXO
utxoInductive = Rule
  [ Predicate
    $ \_ utxo tx -> if balance (txouts tx) <= balance (txins tx ◁ utxo)
        then Passed
        else Failed IncreasedTotalBalance
  , Predicate $ \pc _ tx ->
    if pcMinFee pc tx <= txfee tx then Passed else Failed FeeTooLow
  , Predicate $ \_ utxo tx ->
    let unspentInputs (UTxO utxo) = Map.keysSet utxo
    in
      if txins tx `Set.isSubsetOf` unspentInputs utxo
        then Passed
        else Failed BadInputs
  ]
  (Extension . Transition $ \pc utxo tx -> (txins tx ⋪ utxo) ∪ txouts tx)
