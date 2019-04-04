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


import Control.Monad.Except (MonadError)
import qualified Data.List.NonEmpty as NE
import qualified Data.Set as S
import qualified Data.Vector as V


import Cardano.Binary (Annotated(..), biSize)
import Cardano.Chain.Common
  ( Address
  , Lovelace
  , LovelaceError
  , NetworkMagic
  , TxFeePolicy(..)
  , TxSizeLinear(..)
  , addrNetworkMagic
  , calculateTxSizeLinear
  , checkPubKeyAddress
  , checkRedeemAddress
  , makeNetworkMagic
  , mkKnownLovelace
  , subLovelace
  )
import Cardano.Chain.Txp.Tx (Tx(..), TxIn, TxOut(..))
import Cardano.Chain.Txp.TxAux (ATxAux, aTaTx, taTx, taWitness)
import Cardano.Chain.Txp.TxWitness
  (TxInWitness(..), TxSigData(..), recoverSigData)
import Cardano.Chain.Txp.UTxO
  (UTxO, UTxOError, balance, isRedeemUTxO, txOutputUTxO, (</|), (<|))
import qualified Cardano.Chain.Txp.UTxO as UTxO
import Cardano.Crypto
  ( ProtocolMagic(..)
  , ProtocolMagicId
  , SignTag(..)
  , verifySignatureDecoded
  , verifyRedeemSigDecoded
  )


-- | A representation of all the ways a transaction might be invalid
data TxValidationError
  = TxValidationLovelaceError Text LovelaceError
  | TxValidationFeeTooSmall Tx Lovelace Lovelace
  | TxValidationInvalidWitness TxInWitness
  | TxValidationMissingInput TxIn
  | TxValidationNetworkMagicMismatch NetworkMagic NetworkMagic
  -- ^ Fields are <expected> <actual>
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
  :: MonadError TxValidationError m
  => ProtocolMagic -> TxFeePolicy -> UTxO -> Tx -> m ()
validateTx pm feePolicy utxo tx = do

  -- Check that outputs have valid NetworkMagic
  let nm = makeNetworkMagic pm
  txOutputs tx `forM_` validateTxOutNM nm

  -- Check that every input is in the domain of 'utxo'
  txInputs tx `forM_` validateTxIn utxo

  -- Calculate the minimum fee from the 'TxFeePolicy'
  minFee <- if isRedeemUTxO inputUTxO
    then pure $ mkKnownLovelace @0
    else calculateMinimumFee feePolicy

  -- Calculate the balance of the output 'UTxO'
  balanceOut <- balance (txOutputUTxO tx)
    `wrapError` TxValidationLovelaceError "Output Balance"

  -- Calculate the balance of the restricted input 'UTxO'
  balanceIn <- balance inputUTxO
    `wrapError` TxValidationLovelaceError "Input Balance"

  -- Calculate the 'fee' as the difference of the balances
  fee <- subLovelace balanceIn balanceOut
    `wrapError` TxValidationLovelaceError "Fee"

  -- Check that the fee is greater than the minimum
  (minFee <= fee) `orThrowError` TxValidationFeeTooSmall tx minFee fee
 where

  txSize    = biSize tx

  inputUTxO = S.fromList (NE.toList (txInputs tx)) <| utxo

  calculateMinimumFee
    :: MonadError TxValidationError m => TxFeePolicy -> m Lovelace
  calculateMinimumFee = \case

    TxFeePolicyTxSizeLinear txSizeLinear ->
      calculateTxSizeLinear txSizeLinear txSize
        `wrapError` TxValidationLovelaceError "Minimum Fee"


-- | Validate that 'TxIn' is in the domain of 'UTxO'
validateTxIn :: MonadError TxValidationError m => UTxO -> TxIn -> m ()
validateTxIn utxo txIn
  | txIn `UTxO.member` utxo = pure ()
  | otherwise = throwError $ TxValidationMissingInput txIn

-- | Validate the NetworkMagic of a TxOut
validateTxOutNM :: MonadError TxValidationError m => NetworkMagic -> TxOut -> m ()
validateTxOutNM nm txOut =
  let addrNm = addrNetworkMagic . txOutAddress $ txOut
   in (nm == addrNm)
        `orThrowError` TxValidationNetworkMagicMismatch nm addrNm

-- | Verify that a 'TxInWitness' is a valid witness for the supplied 'TxSigData'
validateWitness
  :: MonadError TxValidationError m
  => ProtocolMagicId
  -> Annotated TxSigData ByteString
  -> Address
  -> TxInWitness
  -> m ()
validateWitness pmi sigData addr witness = case witness of

  PkWitness pk sig ->
    (  verifySignatureDecoded pmi SignTx pk sigData sig
      && checkPubKeyAddress pk addr
      )
      `orThrowError` TxValidationInvalidWitness witness

  RedeemWitness pk sig ->
    (  verifyRedeemSigDecoded pmi SignRedeemTx pk sigData sig
      && checkRedeemAddress pk addr
      )
      `orThrowError` TxValidationInvalidWitness witness


data UTxOValidationError
  = UTxOValidationTxValidationError TxValidationError
  | UTxOValidationUTxOError UTxOError
  deriving (Eq, Show)


-- | Validate a transaction and use it to update the 'UTxO'
updateUTxO :: MonadError UTxOValidationError m
           => ProtocolMagic -> UTxO -> Tx -> m UTxO
updateUTxO pm utxo tx = do

  validateTx pm hardcodedTxFeePolicy utxo tx
    `wrapError` UTxOValidationTxValidationError

  UTxO.union (S.fromList (NE.toList (txInputs tx)) </| utxo) (txOutputUTxO tx)
    `wrapError` UTxOValidationUTxOError
 where

  hardcodedTxFeePolicy = TxFeePolicyTxSizeLinear
    $ TxSizeLinear (mkKnownLovelace @155381) (mkKnownLovelace @44)


-- | Validate a transaction with a witness and use it to update the 'UTxO'
updateUTxOWitness
  :: MonadError UTxOValidationError m
  => ProtocolMagic
  -> UTxO
  -> ATxAux ByteString
  -> m UTxO
updateUTxOWitness pm utxo ta = do
  let
    tx      = taTx ta
    witness = taWitness ta
    --TODO:  after rebase: hashAnnotated fix
    sigData = recoverSigData $ aTaTx ta
    pmi = getProtocolMagicId pm

  -- Get the signing addresses for each transaction input from the 'UTxO'
  addresses <-
    mapM (`UTxO.lookupAddress` utxo) (NE.toList $ txInputs tx)
      `wrapError` UTxOValidationUTxOError

  -- Validate witnesses and their signing addresses
  mapM_
      (uncurry $ validateWitness pmi sigData)
      (zip addresses (V.toList witness))
    `wrapError` UTxOValidationTxValidationError

  -- Update 'UTxO' ignoring witnesses
  updateUTxO pm utxo tx
