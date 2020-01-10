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

import qualified Data.Either as Either (lefts, rights)
import qualified Data.Map.Strict as Map
import qualified Data.Maybe as Maybe (catMaybes)
import           Data.Sequence (Seq)
import           Data.Set (Set)
import qualified Data.Set as Set

import           Test.QuickCheck (Gen)
import qualified Test.QuickCheck as QC

import           Address (scriptToCred, toCred)
import           Coin (Coin (..), splitCoin)
import           ConcreteCryptoTypes (Addr, CoreKeyPair, DCert, DPState, KeyPair, KeyPairs,
                     MultiSig, MultiSigPairs, Tx, TxBody, TxIn, TxOut, UTxO, UTxOState,
                     VrfKeyPairs)
import           Generator.Core.QuickCheck (findPayKeyPair, findPayScript, genNatural)
import           Generator.Delegation.QuickCheck (CertCred (..), genDCerts)
import           LedgerState (pattern UTxOState)
import           Slot (SlotNo (..))
import           STS.Ledger (LedgerEnv (..))
import           Tx (pattern Tx, pattern TxBody, pattern TxOut, getKeyCombinations, hashScript)
import           TxData (pattern AddrBase, pattern KeyHashObj, pattern ScriptHashObj)
import           Updates (emptyUpdate)
import           UTxO (pattern UTxO, balance, makeGenWitnessesVKey, makeWitnessesFromScriptKeys,
                     makeWitnessesVKey)

-- | Generate a new transaction in the context of the LEDGER STS environment and state.
--
-- Selects unspent outputs and spends the funds on a some valid addresses.
-- Also generates valid certificates.
genTx :: LedgerEnv
      -> (UTxOState, DPState)
      -> KeyPairs
      -> MultiSigPairs
      -> [CoreKeyPair]
      -> VrfKeyPairs
      -> Gen Tx
genTx (LedgerEnv slot _ pparams _) (UTxOState utxo _ _ _, dpState) keys scripts coreKeys vrfKeys = do
  keys' <- QC.shuffle keys
  scripts' <- QC.shuffle scripts

  -- inputs
  (witnessedInputs, spendingBalance) <- pickSpendingInputs keys' scripts' utxo
  let (inputs, spendCredentials) = unzip witnessedInputs
      spendWitnesses = Either.lefts spendCredentials
      spendScripts   = Either.rights spendCredentials

  -- output addresses
  recipientAddrs <- genRecipients keys' scripts'

  ttl <- genNatural 1 100
  let slotWithTTL = slot + SlotNo (fromIntegral ttl)

  -- certificates
  (certs, certCreds, deposits_, refunds_)
    <- genDCerts keys' scripts' coreKeys vrfKeys pparams dpState slot ttl

  -- attempt to make provision for certificate deposits (otherwise discard this generator)
  let balance_ = spendingBalance - deposits_ + refunds_
      stakeScripts = Maybe.catMaybes $ map (\case
                                               ScriptCred c -> Just c
                                               _            -> Nothing) certCreds
      genesisWitnesses = foldl (++) [] $
        Maybe.catMaybes $
        map (\case
                CoreKeyCred c -> Just c
                _             -> Nothing) certCreds
      certWitnesses = Maybe.catMaybes $ map (\case
                                                KeyCred c -> Just c
                                                _         -> Nothing) certCreds


  -- calc. fees and output amounts
  let (fee, outputs) = calcFeeAndOutputs balance_ recipientAddrs

  -- witnessed transaction
  txBody <- genTxBody (Set.fromList inputs) outputs certs fee slotWithTTL
  let multiSig = Map.fromList $
        (map (\(payScript, _) -> (hashScript payScript, payScript)) spendScripts) ++
        (map (\(_, sScript) -> (hashScript sScript, sScript)) stakeScripts)

  -- choose any possible combination of keys for multi-sig scripts
  keysLists <- mapM QC.elements (map getKeyCombinations $ Map.elems multiSig)

  let msigSignatures = foldl Set.union Set.empty $ map Set.fromList keysLists
      !wits = makeWitnessesVKey txBody (spendWitnesses ++ certWitnesses)
              `Set.union` makeGenWitnessesVKey txBody genesisWitnesses
              `Set.union` makeWitnessesFromScriptKeys txBody keys msigSignatures

  -- discard if balance is negative, i.e., deposits exceed spending balance
  if spendingBalance < deposits_
    then QC.discard
    else pure (Tx txBody wits multiSig)

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
-- Returns the inputs, paired with the KeyPair or multi-sig script required to
-- witness the spending of the input.
-- Also returns the total spendable balance.

-- NOTE: this function needs access to the keys and multi-sig scripts that the
-- given UTxO originated from (in order to produce the appropriate witnesses to
-- spend these outputs). If this is not the case, `findPayKeyPair` /
-- `findPayScript` will fail by not finding the matching keys or scripts.
pickSpendingInputs
  :: KeyPairs
  -> MultiSigPairs
  -> UTxO
  -> Gen ([(TxIn, Either KeyPair (MultiSig, MultiSig))], Coin)
pickSpendingInputs keys scripts (UTxO utxo) = do
  selectedUtxo <- take <$> QC.choose (1, 5)
                       <*> QC.shuffle (Map.toList utxo)

  return ( witnessedInput <$> selectedUtxo
         , balance (UTxO (Map.fromList selectedUtxo)))
  where
    witnessedInput (input, TxOut addr@(AddrBase (KeyHashObj _) _) _) =
      (input, Left $ findPayKeyPair addr keys)
    witnessedInput (input, TxOut addr@(AddrBase (ScriptHashObj _) _) _) =
      (input, Right $ findPayScript addr scripts)
    witnessedInput _ = error "unsupported address"

-- | Select recipient addresses that will serve as output targets for a new transaction.
genRecipients
  :: KeyPairs
  -> MultiSigPairs
  -> Gen [Addr]
genRecipients keys scripts = do
  n' <- QC.choose (1, 3)

  -- choose m scripts and n keys as recipients
  m  <- QC.choose (0, n' - 1)
  let n = n' - m
  recipientKeys    <- take n <$> QC.shuffle keys
  recipientScripts <- take m <$> QC.shuffle scripts

  let payKeys      = (toCred . fst) <$> recipientKeys
      stakeKeys    = (toCred . snd) <$> recipientKeys
      payScripts   = (scriptToCred . fst) <$> recipientScripts
      stakeScripts = (scriptToCred . fst) <$> recipientScripts

  -- shuffle and zip keys and scripts together as base addresses
  payCreds   <- QC.shuffle (payKeys ++ payScripts)
  stakeCreds <- QC.shuffle (stakeKeys ++ stakeScripts)

  return (zipWith AddrBase payCreds stakeCreds)
