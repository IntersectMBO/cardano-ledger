{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes        #-}
{-# LANGUAGE TypeApplications  #-}

module Cardano.Chain.UTxO.Validation
  ( validateTx
  , updateUTxO
  , updateUTxOTxWitness
  , updateUTxOTx
  , TxValidationError
  , Environment(..)
  , UTxOValidationError
  )
where

import Cardano.Prelude

import Control.Monad.Except (MonadError)
import qualified Data.ByteString as BS
import qualified Data.List.NonEmpty as NE
import qualified Data.Set as S
import qualified Data.Vector as V

import Cardano.Binary (Annotated(..))
import Cardano.Chain.Common
  ( Address(..)
  , Lovelace
  , LovelaceError
  , NetworkMagic
  , TxFeePolicy(..)
  , addrNetworkMagic
  , calculateTxSizeLinear
  , checkVerKeyAddress
  , checkRedeemAddress
  , makeNetworkMagic
  , mkKnownLovelace
  , subLovelace
  , unknownAttributesLength
  )
import Cardano.Chain.UTxO.Tx (Tx(..), TxIn, TxOut(..))
import Cardano.Chain.UTxO.TxAux (ATxAux, aTaTx, taWitness)
import Cardano.Chain.UTxO.TxWitness
  (TxInWitness(..), TxSigData(..), recoverSigData)
import Cardano.Chain.UTxO.UTxO
  (UTxO, UTxOError, balance, isRedeemUTxO, txOutputUTxO, (</|), (<|))
import qualified Cardano.Chain.UTxO.UTxO as UTxO
import Cardano.Chain.Update (ProtocolParameters(..))
import Cardano.Crypto
  ( AProtocolMagic(..)
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
  | TxValidationTxTooLarge Natural Natural
  | TxValidationUnknownAddressAttributes
  | TxValidationUnknownAttributes
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
  => Environment
  -> UTxO
  -> Annotated Tx ByteString
  -> m ()
validateTx env utxo (Annotated tx txBytes) = do

  -- Check that the size of the transaction is less than the maximum
  txSize <= maxTxSize
    `orThrowError` TxValidationTxTooLarge txSize maxTxSize

  -- Check that the transaction attributes are less than the max size
  unknownAttributesLength (txAttributes tx) < 128
    `orThrowError` TxValidationUnknownAttributes

  -- Check that outputs have valid NetworkMagic
  let nm = makeNetworkMagic protocolMagic
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
  Environment { protocolMagic, protocolParameters } = env

  maxTxSize = ppMaxTxSize protocolParameters
  feePolicy = ppTxFeePolicy protocolParameters

  txSize :: Natural
  txSize = fromIntegral $ BS.length txBytes

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
validateTxOutNM
  :: MonadError TxValidationError m => NetworkMagic -> TxOut -> m ()
validateTxOutNM nm txOut = do
  -- Make sure that the unknown attributes are less than the max size
  unknownAttributesLength (addrAttributes (txOutAddress txOut)) < 128
    `orThrowError` TxValidationUnknownAddressAttributes

  -- Check that the network magic in the address matches the expected one
  (nm == addrNm) `orThrowError` TxValidationNetworkMagicMismatch nm addrNm
  where addrNm = addrNetworkMagic . txOutAddress $ txOut


-- | Verify that a 'TxInWitness' is a valid witness for the supplied 'TxSigData'
validateWitness
  :: MonadError TxValidationError m
  => Annotated ProtocolMagicId ByteString
  -> Annotated TxSigData ByteString
  -> Address
  -> TxInWitness
  -> m ()
validateWitness pmi sigData addr witness = case witness of

  VKWitness vk sig ->
    (  verifySignatureDecoded pmi SignTx vk sigData sig
      && checkVerKeyAddress vk addr
      )
      `orThrowError` TxValidationInvalidWitness witness

  RedeemWitness vk sig ->
    (  verifyRedeemSigDecoded pmi SignRedeemTx vk sigData sig
      && checkRedeemAddress vk addr
      )
      `orThrowError` TxValidationInvalidWitness witness


data Environment = Environment
  { protocolMagic      :: !(AProtocolMagic ByteString)
  , protocolParameters :: !ProtocolParameters
  } deriving (Eq, Show)

data UTxOValidationError
  = UTxOValidationTxValidationError TxValidationError
  | UTxOValidationUTxOError UTxOError
  deriving (Eq, Show)


-- | Validate a transaction and use it to update the 'UTxO'
updateUTxOTx
  :: MonadError UTxOValidationError m
  => Environment
  -> UTxO
  -> Annotated Tx ByteString
  -> m UTxO
updateUTxOTx env utxo aTx@(Annotated tx _) = do

  validateTx env utxo aTx
    `wrapError` UTxOValidationTxValidationError

  UTxO.union (S.fromList (NE.toList (txInputs tx)) </| utxo) (txOutputUTxO tx)
    `wrapError` UTxOValidationUTxOError


-- | Validate a transaction with a witness and use it to update the 'UTxO'
updateUTxOTxWitness
  :: MonadError UTxOValidationError m
  => Environment
  -> UTxO
  -> ATxAux ByteString
  -> m UTxO
updateUTxOTxWitness env utxo ta = do

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
  updateUTxOTx env utxo aTx
 where
  Environment { protocolMagic } = env
  pmi = getAProtocolMagicId protocolMagic

  aTx@(Annotated tx _) = aTaTx ta
  witness = taWitness ta
  sigData = recoverSigData aTx


-- | Update UTxO with a list of transactions
updateUTxO
  :: MonadError UTxOValidationError m
  => Environment
  -> UTxO
  -> [ATxAux ByteString]
  -> m UTxO
updateUTxO = foldM . updateUTxOTxWitness
