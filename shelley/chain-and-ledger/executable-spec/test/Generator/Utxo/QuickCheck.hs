{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE UndecidableInstances #-}

module Generator.Utxo.QuickCheck
  ( genTx
  )
  where

import           Control.Monad (when)
import qualified Data.Map.Strict as Map
import qualified Data.Maybe as Maybe (catMaybes)
import           Data.Sequence (Seq)
import           Data.Set (Set)
import qualified Data.Set as Set

import           Test.QuickCheck (Gen)
import qualified Test.QuickCheck as QC

import           Coin (Coin (..), splitCoin)
import           Generator.Core.QuickCheck (findPayKeyPair, findPayScript, genNatural, toAddr)
import           Generator.Delegation.QuickCheck (genDCerts)
import           LedgerState (pattern UTxOState)
import           MockTypes (Addr, CoreKeyPair, DCert, DPState, KeyPair, KeyPairs, MultiSig, Tx,
                     TxBody, TxIn, TxOut, UTxO, UTxOState, VrfKeyPairs)
import           Slot (SlotNo (..))
import           STS.Ledger (LedgerEnv (..))
import           Tx (pattern Tx, pattern TxBody, pattern TxOut, getKeyCombinations, hashScript)
import           TxData (pattern AddrBase, pattern KeyHashObj, pattern ScriptHashObj)
import           Updates (emptyUpdate)
import           UTxO (pattern UTxO, balance, makeGenWitnessesVKey, makeWitnessesFromScriptKeys,
                     makeWitnessesVKey)

import qualified Debug.Trace as D

-- | Generate a new transaction in the context of the LEDGER STS environment and state.
--
-- Selects unspent outputs and spends the funds on a some valid addresses.
-- Also generates valid certificates.
genTx :: LedgerEnv
      -> (UTxOState, DPState)
      -> KeyPairs
      -> [(MultiSig, MultiSig)]
      -> [CoreKeyPair]
      -> VrfKeyPairs
      -> Gen Tx
genTx (LedgerEnv slot _ pparams _) (UTxOState utxo _ _ _, dpState) keys scripts coreKeys vrfKeys = do
  keys' <- QC.shuffle keys
  scripts' <- QC.shuffle scripts

  -- inputs
  (witnessedInputs, spendingBalance) <- pickSpendingInputs keys' scripts' utxo
  let (inputs, spendCredentials) = unzip witnessedInputs
      spendWitnesses = Maybe.catMaybes $
        map (\case
                Left kp -> Just kp
                _       -> Nothing) spendCredentials
      spendScripts = Maybe.catMaybes $
        map (\case
                Right sp -> Just sp
                _        -> Nothing) spendCredentials

  -- output addresses
  recipientAddrs <- genRecipients keys'

  ttl <- genNatural 1 100
  let slotWithTTL = slot + SlotNo (fromIntegral ttl)

  -- certificates
  (certs, certWitnesses, genesisWitnesses, deposits_, refunds_)
    <- genDCerts keys' coreKeys vrfKeys pparams dpState slot ttl

  -- attempt to make provision for certificate deposits (otherwise discard this generator)
  when (spendingBalance < deposits_)
       (D.trace ("(QC) GenTx Discarded - " <> show (spendingBalance, deposits_, refunds_)) QC.discard)
  let balance_ = spendingBalance - deposits_ + refunds_

  -- calc. fees and output amounts
  let (fee, outputs) = calcFeeAndOutputs balance_ recipientAddrs

  -- witnessed transaction
  txBody <- genTxBody (Set.fromList inputs) outputs certs fee slotWithTTL
  let multiSig = Map.fromList $
        map (\(payScript, _) -> (hashScript payScript, payScript)) spendScripts

  -- choose any possible combination of keys for multi-sig scripts
  keysLists <- mapM QC.elements (map getKeyCombinations $ Map.elems multiSig)

  let msigSignatures = foldl Set.union Set.empty $ map Set.fromList keysLists
      !wits = makeWitnessesVKey txBody (spendWitnesses ++ certWitnesses)
              `Set.union` makeGenWitnessesVKey txBody genesisWitnesses
              `Set.union` makeWitnessesFromScriptKeys txBody keys msigSignatures

  return (Tx txBody wits multiSig)

-- | Generate a transaction body with the given inputs/outputs and certificates
genTxBody
  :: Set TxIn
  -> [TxOut]
  -> Seq DCert
  -> Coin
  -> SlotNo
  -> Gen TxBody
genTxBody inputs outputs certs fee slotWithTTL = do
  return $ TxBody
             inputs
             outputs
             certs
             Map.empty -- TODO @uroboros generate withdrawals
             fee
             slotWithTTL
             emptyUpdate -- TODO @uroboros generate updates

-- | Calculate the fee and distribute the remainder of the balance
-- to the given addresses (as transaction outputs)
calcFeeAndOutputs
  :: Coin
  -> [Addr]
  -> (Coin, [TxOut])
calcFeeAndOutputs balance_ addrs =
  ( fee + splitCoinRem
  , (`TxOut` amountPerOutput) <$> addrs)
  where
    -- TODO @uroboros fee=0 works for now since PParams minFeeA/B == 0.
    --      We need to instead add a "draft" TxBody as argument, which will
    --      give a measure of the minimum fee for this transaction.
    --      This estimated fee can then be used to create the actual TxBody,
    --      with the new fee baked in.
    fee = 0
    -- split the available balance into equal portions (one for each address),
    -- if there is a remainder, then add it to the fee.
    balanceAfterFee = balance_ - fee
    (amountPerOutput, splitCoinRem) = splitCoin balanceAfterFee (fromIntegral $ length addrs)

-- | Select unspent output(s) to serve as inputs for a new transaction
--
-- Returns the inputs, paired with the KeyPair required to witness the
-- spending of the input.
-- Also returns the total spendable balance.

-- NOTE: this function needs access to the keys that the given utxo originated
-- from (in order to produce the appropriate witnesses to spend these outputs)
-- If this is not the case, findPayKeyPair will fail by not finding the matching keys.
pickSpendingInputs
  :: KeyPairs
  -> [(MultiSig, MultiSig)]
  -> UTxO
  -> Gen ([(TxIn, Either KeyPair (MultiSig, MultiSig))], Coin)
pickSpendingInputs keys scripts (UTxO utxo) = do
  selectedUtxo <- take <$> QC.choose (1, 5)
                       <*> QC.shuffle (Map.toList utxo)

  return ( witnessedInput <$> selectedUtxo
         , balance (UTxO (Map.fromList selectedUtxo)))
  where
    witnessedInput (input, TxOut addr@(AddrBase (KeyHashObj _) (KeyHashObj _)) _) =
      (input, Left $ findPayKeyPair addr keys)
    witnessedInput (input, TxOut addr@(AddrBase (ScriptHashObj _) (ScriptHashObj _)) _) =
      (input, Right $ findPayScript addr scripts)
    witnessedInput _ = error "unsupported address"

-- | Select recipient addresses that will serve as output targets for a new transaction.
genRecipients
  :: KeyPairs
  -> Gen [Addr]
genRecipients keys = do
  let n = 1 -- TODO @uroboros select _multiple_ recipients
      recipients = take n keys

  return $ toAddr <$> recipients
