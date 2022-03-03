{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeApplications #-}

module Cardano.Chain.UTxO.Validation
  ( validateTx,
    validateTxAux,
    updateUTxO,
    updateUTxOTxWitness,
    updateUTxOTx,
    TxValidationError (..),
    Environment (..),
    UTxOValidationError (..),
  )
where

import Cardano.Binary
  ( Annotated (..),
    Decoder,
    DecoderError (DecoderErrorUnknownTag),
    FromCBOR (..),
    ToCBOR (..),
    decodeListLen,
    decodeWord8,
    encodeListLen,
    enforceSize,
    matchSize,
  )
import Cardano.Chain.Common
  ( Address (..),
    Lovelace,
    LovelaceError,
    NetworkMagic,
    TxFeePolicy (..),
    addrNetworkMagic,
    calculateTxSizeLinear,
    checkRedeemAddress,
    checkVerKeyAddress,
    makeNetworkMagic,
    mkKnownLovelace,
    subLovelace,
    unknownAttributesLength,
  )
import Cardano.Chain.UTxO.Compact (CompactTxOut (..), toCompactTxIn)
import Cardano.Chain.UTxO.Tx (Tx (..), TxIn, TxOut (..))
import Cardano.Chain.UTxO.TxAux (ATxAux (..), aTaTx, taWitness)
import Cardano.Chain.UTxO.TxWitness
  ( TxInWitness (..),
    TxSigData (..),
    recoverSigData,
  )
import Cardano.Chain.UTxO.UTxO
  ( UTxO,
    UTxOError,
    balance,
    isRedeemUTxO,
    txOutputUTxO,
    (</|),
    (<|),
  )
import qualified Cardano.Chain.UTxO.UTxO as UTxO
import Cardano.Chain.UTxO.UTxOConfiguration
import Cardano.Chain.Update (ProtocolParameters (..))
import Cardano.Chain.ValidationMode
  ( ValidationMode,
    unlessNoTxValidation,
    whenTxValidation,
    wrapErrorWithValidationMode,
  )
import Cardano.Crypto
  ( AProtocolMagic (..),
    ProtocolMagicId,
    SignTag (..),
    verifyRedeemSigDecoded,
    verifySignatureDecoded,
  )
import Cardano.Prelude
import qualified Data.ByteString as BS
import qualified Data.List.NonEmpty as NE
import qualified Data.Set as S
import qualified Data.Vector as V

-- | A representation of all the ways a transaction might be invalid
data TxValidationError
  = TxValidationLovelaceError Text LovelaceError
  | TxValidationFeeTooSmall Tx Lovelace Lovelace
  | TxValidationWitnessWrongSignature TxInWitness ProtocolMagicId TxSigData
  | TxValidationWitnessWrongKey TxInWitness Address
  | TxValidationMissingInput TxIn
  | -- | Fields are <expected> <actual>
    TxValidationNetworkMagicMismatch NetworkMagic NetworkMagic
  | TxValidationTxTooLarge Natural Natural
  | TxValidationUnknownAddressAttributes
  | TxValidationUnknownAttributes
  deriving (Eq, Show)

instance ToCBOR TxValidationError where
  toCBOR = \case
    TxValidationLovelaceError text loveLaceError ->
      encodeListLen 3
        <> toCBOR @Word8 0
        <> toCBOR text
        <> toCBOR loveLaceError
    TxValidationFeeTooSmall tx lovelace1 lovelace2 ->
      encodeListLen 4
        <> toCBOR @Word8 1
        <> toCBOR tx
        <> toCBOR lovelace1
        <> toCBOR lovelace2
    TxValidationWitnessWrongSignature txInWitness pmi sigData ->
      encodeListLen 4
        <> toCBOR @Word8 2
        <> toCBOR txInWitness
        <> toCBOR pmi
        <> toCBOR sigData
    TxValidationWitnessWrongKey txInWitness addr ->
      encodeListLen 3
        <> toCBOR @Word8 3
        <> toCBOR txInWitness
        <> toCBOR addr
    TxValidationMissingInput txIn ->
      encodeListLen 2
        <> toCBOR @Word8 4
        <> toCBOR txIn
    TxValidationNetworkMagicMismatch networkMagic1 networkMagic2 ->
      encodeListLen 3
        <> toCBOR @Word8 5
        <> toCBOR networkMagic1
        <> toCBOR networkMagic2
    TxValidationTxTooLarge nat1 nat2 ->
      encodeListLen 3
        <> toCBOR @Word8 6
        <> toCBOR nat1
        <> toCBOR nat2
    TxValidationUnknownAddressAttributes ->
      encodeListLen 1
        <> toCBOR @Word8 7
    TxValidationUnknownAttributes ->
      encodeListLen 1
        <> toCBOR @Word8 8

instance FromCBOR TxValidationError where
  fromCBOR = do
    len <- decodeListLen
    let checkSize :: forall s. Int -> Decoder s ()
        checkSize size = matchSize "TxValidationError" size len
    tag <- decodeWord8
    case tag of
      0 -> checkSize 3 >> TxValidationLovelaceError <$> fromCBOR <*> fromCBOR
      1 -> checkSize 4 >> TxValidationFeeTooSmall <$> fromCBOR <*> fromCBOR <*> fromCBOR
      2 -> checkSize 4 >> TxValidationWitnessWrongSignature <$> fromCBOR <*> fromCBOR <*> fromCBOR
      3 -> checkSize 3 >> TxValidationWitnessWrongKey <$> fromCBOR <*> fromCBOR
      4 -> checkSize 2 >> TxValidationMissingInput <$> fromCBOR
      5 -> checkSize 3 >> TxValidationNetworkMagicMismatch <$> fromCBOR <*> fromCBOR
      6 -> checkSize 3 >> TxValidationTxTooLarge <$> fromCBOR <*> fromCBOR
      7 -> checkSize 1 $> TxValidationUnknownAddressAttributes
      8 -> checkSize 1 $> TxValidationUnknownAttributes
      _ -> cborError $ DecoderErrorUnknownTag "TxValidationError" tag

-- | Validate that:
--
--   1. The fee for a transaction is not less than the minimum fee.
--   2. The size of the transaction is below the maximum size.
--   3. Output balance + fee = input balance
--
--   The transaction size must be calculated _including the witnesses_. As such
--   this cannot be part of 'validateTx'. We actually assume 3 by calculating
--   the fee as output balance - input balance.
validateTxAux ::
  MonadError TxValidationError m =>
  Environment ->
  UTxO ->
  ATxAux ByteString ->
  m ()
validateTxAux env utxo (ATxAux (Annotated tx _) _ txBytes) = do
  -- Check that the size of the transaction is less than the maximum
  txSize <= maxTxSize
    `orThrowError` TxValidationTxTooLarge txSize maxTxSize

  -- Calculate the minimum fee from the 'TxFeePolicy'
  minFee <-
    if isRedeemUTxO inputUTxO
      then pure $ mkKnownLovelace @0
      else calculateMinimumFee feePolicy

  -- Calculate the balance of the output 'UTxO'
  balanceOut <-
    balance (txOutputUTxO tx)
      `wrapError` TxValidationLovelaceError "Output Balance"

  -- Calculate the balance of the restricted input 'UTxO'
  balanceIn <-
    balance inputUTxO
      `wrapError` TxValidationLovelaceError "Input Balance"

  -- Calculate the 'fee' as the difference of the balances
  fee <-
    subLovelace balanceIn balanceOut
      `wrapError` TxValidationLovelaceError "Fee"

  -- Check that the fee is greater than the minimum
  (minFee <= fee) `orThrowError` TxValidationFeeTooSmall tx minFee fee
  where
    Environment {protocolParameters} = env

    maxTxSize = ppMaxTxSize protocolParameters
    feePolicy = ppTxFeePolicy protocolParameters

    txSize :: Natural
    txSize = fromIntegral $ BS.length txBytes

    inputUTxO = S.fromList (NE.toList (txInputs tx)) <| utxo

    calculateMinimumFee ::
      MonadError TxValidationError m => TxFeePolicy -> m Lovelace
    calculateMinimumFee = \case
      TxFeePolicyTxSizeLinear txSizeLinear ->
        calculateTxSizeLinear txSizeLinear txSize
          `wrapError` TxValidationLovelaceError "Minimum Fee"

-- | Validate that:
--
--   1. All @TxIn@s are in domain of @Utxo@
--
--   These are the conditions of the UTxO inference rule in the spec.
validateTx ::
  MonadError TxValidationError m =>
  Environment ->
  UTxO ->
  Annotated Tx ByteString ->
  m ()
validateTx env utxo (Annotated tx _) = do
  -- Check that the transaction attributes are less than the max size
  unknownAttributesLength (txAttributes tx) < 128
    `orThrowError` TxValidationUnknownAttributes

  -- Check that outputs have valid NetworkMagic
  let nm = makeNetworkMagic protocolMagic
  txOutputs tx `forM_` validateTxOutNM nm

  -- Check that every input is in the domain of 'utxo'
  txInputs tx `forM_` validateTxIn utxoConfiguration utxo
  where
    Environment {protocolMagic, utxoConfiguration} = env

-- | Validate that 'TxIn' is in the domain of 'UTxO'
validateTxIn ::
  MonadError TxValidationError m =>
  UTxOConfiguration ->
  UTxO ->
  TxIn ->
  m ()
validateTxIn UTxOConfiguration {tcAssetLockedSrcAddrs} utxo txIn
  | S.null tcAssetLockedSrcAddrs,
    txIn `UTxO.member` utxo =
      pure ()
  | Just txOut <- UTxO.lookupCompact (toCompactTxIn txIn) utxo,
    let (CompactTxOut txOutAddr _) = txOut,
    txOutAddr `S.notMember` tcAssetLockedSrcAddrs =
      pure ()
  | otherwise =
      throwError $ TxValidationMissingInput txIn

-- | Validate the NetworkMagic of a TxOut
validateTxOutNM ::
  MonadError TxValidationError m =>
  NetworkMagic ->
  TxOut ->
  m ()
validateTxOutNM nm txOut = do
  -- Make sure that the unknown attributes are less than the max size
  unknownAttributesLength (addrAttributes (txOutAddress txOut)) < 128
    `orThrowError` TxValidationUnknownAddressAttributes

  -- Check that the network magic in the address matches the expected one
  (nm == addrNm) `orThrowError` TxValidationNetworkMagicMismatch nm addrNm
  where
    addrNm = addrNetworkMagic . txOutAddress $ txOut

-- | Verify that a 'TxInWitness' is a valid witness for the supplied 'TxSigData'
validateWitness ::
  MonadError TxValidationError m =>
  Annotated ProtocolMagicId ByteString ->
  Annotated TxSigData ByteString ->
  Address ->
  TxInWitness ->
  m ()
validateWitness pmi sigData addr witness = case witness of
  VKWitness vk sig -> do
    verifySignatureDecoded pmi SignTx vk sigData sig
      `orThrowError` TxValidationWitnessWrongSignature
        witness
        (unAnnotated pmi)
        (unAnnotated sigData)
    checkVerKeyAddress vk addr
      `orThrowError` TxValidationWitnessWrongKey
        witness
        addr
  RedeemWitness vk sig -> do
    verifyRedeemSigDecoded pmi SignRedeemTx vk sigData sig
      `orThrowError` TxValidationWitnessWrongSignature
        witness
        (unAnnotated pmi)
        (unAnnotated sigData)
    checkRedeemAddress vk addr
      `orThrowError` TxValidationWitnessWrongKey witness addr

data Environment = Environment
  { protocolMagic :: !(AProtocolMagic ByteString),
    protocolParameters :: !ProtocolParameters,
    utxoConfiguration :: !UTxOConfiguration
  }
  deriving (Eq, Show)

data UTxOValidationError
  = UTxOValidationTxValidationError TxValidationError
  | UTxOValidationUTxOError UTxOError
  deriving (Eq, Show)

instance ToCBOR UTxOValidationError where
  toCBOR = \case
    UTxOValidationTxValidationError txValidationError ->
      encodeListLen 2 <> toCBOR @Word8 0 <> toCBOR txValidationError
    UTxOValidationUTxOError uTxOError ->
      encodeListLen 2 <> toCBOR @Word8 1 <> toCBOR uTxOError

instance FromCBOR UTxOValidationError where
  fromCBOR = do
    enforceSize "UTxOValidationError" 2
    decodeWord8 >>= \case
      0 -> UTxOValidationTxValidationError <$> fromCBOR
      1 -> UTxOValidationUTxOError <$> fromCBOR
      tag -> cborError $ DecoderErrorUnknownTag "UTxOValidationError" tag

-- | Validate a transaction and use it to update the 'UTxO'
updateUTxOTx ::
  (MonadError UTxOValidationError m, MonadReader ValidationMode m) =>
  Environment ->
  UTxO ->
  Annotated Tx ByteString ->
  m UTxO
updateUTxOTx env utxo aTx@(Annotated tx _) = do
  unlessNoTxValidation (validateTx env utxo aTx)
    `wrapErrorWithValidationMode` UTxOValidationTxValidationError

  UTxO.union (S.fromList (NE.toList (txInputs tx)) </| utxo) (txOutputUTxO tx)
    `wrapError` UTxOValidationUTxOError

-- | Validate a transaction with a witness and use it to update the 'UTxO'
updateUTxOTxWitness ::
  (MonadError UTxOValidationError m, MonadReader ValidationMode m) =>
  Environment ->
  UTxO ->
  ATxAux ByteString ->
  m UTxO
updateUTxOTxWitness env utxo ta = do
  whenTxValidation $ do
    -- Get the signing addresses for each transaction input from the 'UTxO'
    addresses <-
      mapM (`UTxO.lookupAddress` utxo) (NE.toList $ txInputs tx)
        `wrapError` UTxOValidationUTxOError

    -- Validate witnesses and their signing addresses
    mapM_
      (uncurry $ validateWitness pmi sigData)
      (zip addresses (V.toList witness))
      `wrapError` UTxOValidationTxValidationError

    -- Validate the tx including witnesses
    validateTxAux env utxo ta
      `wrapError` UTxOValidationTxValidationError

  -- Update 'UTxO' ignoring witnesses
  updateUTxOTx env utxo aTx
  where
    Environment {protocolMagic} = env
    pmi = getAProtocolMagicId protocolMagic

    aTx@(Annotated tx _) = aTaTx ta
    witness = taWitness ta
    sigData = recoverSigData aTx

-- | Update UTxO with a list of transactions
updateUTxO ::
  (MonadError UTxOValidationError m, MonadReader ValidationMode m) =>
  Environment ->
  UTxO ->
  [ATxAux ByteString] ->
  m UTxO
updateUTxO env as = foldM (updateUTxOTxWitness env) as
