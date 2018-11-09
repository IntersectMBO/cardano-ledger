{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes        #-}
{-# LANGUAGE TypeApplications  #-}

module Cardano.Chain.Txp.Validation
  ( validateTx
  , updateUTxO
  , updateUTxOWitness
  , TxValidationError
  , UTxOValidationError
  )
where


import Cardano.Prelude


import Control.Monad.Except (MonadError, liftEither)
import qualified Data.List.NonEmpty as NE
import qualified Data.Set as S
import qualified Data.Vector as V


import Cardano.Binary.Class (biSize)
import Cardano.Chain.Common
  ( Address
  , Coeff(..)
  , Coin
  , CoinError
  , Script(..)
  , TxFeePolicy(..)
  , TxSizeLinear(..)
  , calculateTxSizeLinear
  , checkPubKeyAddress
  , checkRedeemAddress
  , checkScriptAddress
  , integerToCoin
  , mkKnownCoin
  , subCoin
  )
import Cardano.Chain.Txp.Tx (Tx(..), TxIn)
import Cardano.Chain.Txp.TxAux (TxAux(..))
import Cardano.Chain.Txp.TxWitness (TxInWitness(..), TxSigData(..))
import Cardano.Chain.Txp.UTxO
  (UTxO, UTxOError, balance, isRedeemUTxO, txOutputUTxO, (</|), (<|))
import qualified Cardano.Chain.Txp.UTxO as UTxO
import Cardano.Crypto
  (ProtocolMagic, SignTag(..), checkSig, hash, redeemCheckSig)


-- | A representation of all the ways a transaction might be invalid
data TxValidationError
  = TxValidationCoinError Text CoinError
  | TxValidationFeeTooSmall Tx Coin Coin
  | TxValidationInvalidWitness TxInWitness
  | TxValidationMissingInput TxIn
  | TxValidationScriptWitness
  -- ^ TODO: Remove this once support for script witnesses is added
  | TxValidationUnknownFeePolicy TxFeePolicy
  | TxValidationUnknownWitnessType Word8
  deriving (Eq, Show)


-- | A helper for lifting an 'Either' to a 'MonadError'
--
--   By using this function infix we can move the error handling to the end of
--   an expression, hopefully improving readability.
wrapError :: MonadError e' m => Either e a -> (e -> e') -> m a
wrapError m wrapper = liftEither $ first wrapper m


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
  balanceOut <- balance (txOutputUTxO tx)
    `wrapError` TxValidationCoinError "Output Balance"

  -- Calculate the balance of the restricted input 'UTxO'
  balanceIn <- balance inputUTxO
    `wrapError` TxValidationCoinError "Input Balance"

  -- Calculate the 'fee' as the difference of the balances
  fee <- subCoin balanceIn balanceOut `wrapError` TxValidationCoinError "Fee"

  -- Check that the fee is greater than the minimum
  unless (minFee <= fee) (throwError $ TxValidationFeeTooSmall tx minFee fee)
 where

  txSize    = biSize tx

  inputUTxO = S.fromList (NE.toList (_txInputs tx)) <| utxo

  calculateMinimumFee
    :: MonadError TxValidationError m => TxFeePolicy -> m Coin
  calculateMinimumFee = \case

    TxFeePolicyTxSizeLinear txSizeLinear ->
      integerToCoin (ceiling $ calculateTxSizeLinear txSizeLinear txSize)
        `wrapError` TxValidationCoinError "Minimum Fee"

    policy -> throwError $ TxValidationUnknownFeePolicy policy


-- | Validate that 'TxIn' is in the domain of 'UTxO'
validateTxIn :: MonadError TxValidationError m => UTxO -> TxIn -> m ()
validateTxIn utxo txIn
  | txIn `UTxO.member` utxo = pure ()
  | otherwise               = throwError $ TxValidationMissingInput txIn


-- | Verify that a 'TxInWitness' is a valid witness for the supplied 'TxSigData'
validateWitness
  :: MonadError TxValidationError m
  => ProtocolMagic
  -> TxSigData
  -> Address
  -> TxInWitness
  -> m ()
validateWitness pm sigData addr witness = case witness of

  PkWitness pk sig -> unless
    (checkSig pm SignTx pk sigData sig && checkPubKeyAddress pk addr)
    (throwError $ TxValidationInvalidWitness witness)

  RedeemWitness pk sig -> unless
    (redeemCheckSig pm SignRedeemTx pk sigData sig && checkRedeemAddress pk addr
    )
    (throwError $ TxValidationInvalidWitness witness)

  -- TODO: Support script witnesses for Shelley
  ScriptWitness validator redeemer -> do
    let
      valVersion = scrVersion validator
      redVersion = scrVersion redeemer
    unless
      (valVersion == redVersion && checkScriptAddress validator addr)
      (throwError $ TxValidationInvalidWitness witness)
    txScriptCheck sigData validator redeemer

  UnknownWitnessType t _ -> throwError $ TxValidationUnknownWitnessType t
 where

  txScriptCheck
    :: MonadError TxValidationError m => TxSigData -> Script -> Script -> m ()
  txScriptCheck _ _ _ = throwError TxValidationScriptWitness


data UTxOValidationError
  = UTxOValidationTxValidationError TxValidationError
  | UTxOValidationUTxOError UTxOError
  deriving (Eq, Show)


-- | Validate a transaction and use it to update the 'UTxO'
updateUTxO :: MonadError UTxOValidationError m => UTxO -> Tx -> m UTxO
updateUTxO utxo tx = do

  validateTx hardcodedTxFeePolicy utxo tx
    `wrapError` UTxOValidationTxValidationError

  UTxO.union (S.fromList (NE.toList (_txInputs tx)) </| utxo) (txOutputUTxO tx)
    `wrapError` UTxOValidationUTxOError
 where

  hardcodedTxFeePolicy =
    TxFeePolicyTxSizeLinear $ TxSizeLinear (Coeff 155381) (Coeff 43.946)


-- | Validate a transaction with a witness and use it to update the 'UTxO'
updateUTxOWitness
  :: MonadError UTxOValidationError m
  => ProtocolMagic
  -> UTxO
  -> TxAux
  -> m UTxO
updateUTxOWitness pm utxo (TxAux tx witness) = do
  let sigData = TxSigData $ hash tx

  -- Get the signing addresses for each transaction input from the 'UTxO'
  addresses <-
    mapM (`UTxO.lookupAddress` utxo) (NE.toList $ _txInputs tx)
      `wrapError` UTxOValidationUTxOError

  -- Validate witnesses and their signing addresses
  mapM_
      (uncurry $ validateWitness pm sigData)
      (zip addresses (V.toList witness))
    `wrapError` UTxOValidationTxValidationError

  -- Update 'UTxO' ignoring witnesses
  updateUTxO utxo tx
