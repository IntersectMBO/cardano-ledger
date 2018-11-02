{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications  #-}

module Cardano.Chain.Txp.Validation
       ( validateTx
       , updateUTxO
       , TxValidationError
       , UTxOValidationError
       ) where

import Cardano.Prelude

import Control.Monad.Except
  (MonadError, liftEither)
import qualified Data.List.NonEmpty as NE
import qualified Data.Set as S

import Cardano.Binary.Class
  (biSize)
import Cardano.Chain.Common
  ( Coeff (..)
  , Coin
  , CoinError
  , TxFeePolicy (..)
  , TxSizeLinear (..)
  , calculateTxSizeLinear
  , integerToCoin
  , mkKnownCoin
  , subCoin
  )
import Cardano.Chain.Txp.Tx
  (Tx (..), TxIn)
import Cardano.Chain.Txp.UTxO
  (UTxO, UTxOError, balance, isRedeemUTxO, txOutputUTxO, (</|), (<|))
import qualified Cardano.Chain.Txp.UTxO as UTxO


data TxValidationError
  = TxValidationFeeTooSmall Tx Coin Coin
  | TxValidationMissingInput TxIn
  | TxValidationCoinError Text CoinError
  | TxValidationUnknownFeePolicy TxFeePolicy
  deriving (Eq, Show)

-- | Validate that:
--
--   1. All @TxIn@s are in domain of @Utxo@
--   2. The fee is not less than the minimum fee
--   3. Output balance + fee = input balance
--
--   These are the conditions of the UTxO inference rule in the spec. We
--   actually assume 3 by calculating the fee as output balance - input balance.
validateTx
  :: MonadError TxValidationError m => TxFeePolicy -> UTxO -> Tx -> m ()
validateTx feePolicy utxo tx = do
  -- Check that every input is in the domain of 'utxo'
  _txInputs tx `forM_` validateTxIn utxo

  -- Calculate the minimum fee from the 'TxFeePolicy'
  minFee <- if isRedeemUTxO inputUTxO
    then pure $ mkKnownCoin @0
    else calculateMinimumFee feePolicy

  -- Calculate the balance of the output 'UTxO'
  balanceOut <-
    liftEither
    . first (TxValidationCoinError "Output Balance")
    $ balance
    $ txOutputUTxO tx

  -- Calculate the balance of the restricted input 'UTxO'
  balanceIn <-
    liftEither . first (TxValidationCoinError "Input Balance") $ balance
      inputUTxO

  -- Calculate the 'fee' as the difference of the balances
  fee <-
    liftEither
    .         first (TxValidationCoinError "Fee")
    $         balanceIn
    `subCoin` balanceOut

  -- Check that the fee is greater than the minimum
  unless (minFee <= fee) $ throwError $ TxValidationFeeTooSmall tx minFee fee
 where
  txSize              = biSize tx

  inputUTxO           = S.fromList (NE.toList (_txInputs tx)) <| utxo

  calculateMinimumFee :: MonadError TxValidationError m => TxFeePolicy -> m Coin
  calculateMinimumFee = \case
    TxFeePolicyTxSizeLinear txSizeLinear ->
      liftEither
        . first (TxValidationCoinError "Minimum Fee")
        . integerToCoin
        . ceiling
        $ calculateTxSizeLinear txSizeLinear txSize
    policy -> throwError $ TxValidationUnknownFeePolicy policy

-- | Validate that 'TxIn' is in the domain of 'UTxO'
validateTxIn :: MonadError TxValidationError m => UTxO -> TxIn -> m ()
validateTxIn utxo txIn
  | txIn `UTxO.member` utxo = pure ()
  | otherwise               = throwError $ TxValidationMissingInput txIn

data UTxOValidationError
  = UTxOValidationTxValidationError TxValidationError
  | UTxOValidationUTxOError UTxOError
  deriving (Eq, Show)

updateUTxO :: MonadError UTxOValidationError m => UTxO -> Tx -> m UTxO
updateUTxO utxo tx = do
  liftEither . first UTxOValidationTxValidationError $ validateTx
    hardcodedTxFeePolicy
    utxo
    tx
  liftEither . first UTxOValidationUTxOError $ UTxO.union
    (S.fromList (NE.toList (_txInputs tx)) </| utxo)
    (txOutputUTxO tx)
 where
  hardcodedTxFeePolicy =
    TxFeePolicyTxSizeLinear $ TxSizeLinear (Coeff 155381) (Coeff 43.946)
