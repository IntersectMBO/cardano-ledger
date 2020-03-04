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
import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import qualified Data.Maybe as Maybe (catMaybes)
import           Data.Sequence (Seq)
import qualified Data.Sequence as Seq
import           Data.Set (Set)
import qualified Data.Set as Set

import           Test.QuickCheck (Gen)
import qualified Test.QuickCheck as QC

import           Address (scriptToCred, toCred)
import           Coin (Coin (..), splitCoin)
import           ConcreteCryptoTypes (Addr, AnyKeyHash, CoreKeyPair, Credential, DCert, DPState,
                     DState, KeyHash, KeyPair, KeyPairs, MultiSig, MultiSigPairs, RewardAcnt, Tx,
                     TxBody, TxIn, TxOut, UTxO, UTxOState, Update, WitVKey)
import           Generator.Core.Constants (frequencyAFewWithdrawals, frequencyNoWithdrawals,
                     frequencyPotentiallyManyWithdrawals, maxAFewWithdrawals, maxNumGenInputs,
                     minNumGenInputs)
import           Generator.Core.QuickCheck (AllPoolKeys, findPayKeyPairAddr, findPayKeyPairCred,
                     findPayScriptFromAddr, findStakeScriptFromCred, genNatural)
import           Generator.DCertTrace (genDCerts)
import           Generator.Delegation.QuickCheck (CertCred (..))
import           Generator.Update.QuickCheck (genUpdate)
import           LedgerState (pattern UTxOState, minfee, _dstate, _ptrs, _rewards)
import           Slot (SlotNo (..))
import           STS.Ledger (LedgerEnv (..))
import           Tx (pattern Tx, pattern TxBody, pattern TxOut, getKeyCombination, hashScript)
import           TxData (pattern AddrBase, pattern AddrPtr, pattern KeyHashObj,
                     pattern ScriptHashObj, Wdrl (..), getRwdCred, _outputs, _txfee)
import           UTxO (pattern UTxO, balance, makeWitnessesFromScriptKeys, makeWitnessesVKey)

import           Debug.Trace as D

-- | Generate a new transaction in the context of the LEDGER STS environment and state.
--
-- Selects unspent outputs and spends the funds on a some valid addresses.
-- Also generates valid certificates.
genTx :: LedgerEnv
      -> (UTxOState, DPState)
      -> KeyPairs
      -> Map AnyKeyHash KeyPair
      -> MultiSigPairs
      -> [(CoreKeyPair, AllPoolKeys)]
      -> Map KeyHash KeyPair -- indexed keys By StakeHash
      -> Gen Tx
genTx (LedgerEnv slot txIx pparams reserves) (utxoSt@(UTxOState utxo _ _ _), dpState) keys keyHashMap scripts coreKeys keysByStakeHash = do
  keys' <- QC.shuffle keys
  scripts' <- QC.shuffle scripts

  -- inputs
  (witnessedInputs, spendingBalanceUtxo) <-
    pickSpendingInputs scripts' keyHashMap utxo

  wdrls <- pickWithdrawals ((_rewards . _dstate) dpState)
  let wdrlCredentials = fmap (mkWdrlWits scripts' keyHashMap) (fmap getRwdCred (Map.keys wdrls))
      wdrlWitnesses = Either.lefts wdrlCredentials
      wdrlScripts   = Either.rights wdrlCredentials

  let spendingBalance = spendingBalanceUtxo + (sum wdrls)

  let (inputs, spendCredentials) = unzip witnessedInputs
      spendWitnesses = Either.lefts spendCredentials
      spendScripts   = Either.rights spendCredentials

  -- output addresses
  recipientAddrs' <- genRecipients (length witnessedInputs) keys' scripts'

  -- maybe convert some addresss to pointer addresses
  recipientAddrs  <- genPtrAddrs (_dstate dpState) recipientAddrs'

  ttl <- genNatural 50 100
  let slotWithTTL = slot + SlotNo (fromIntegral ttl)

  -- certificates
  (certs, certCreds, deposits_, refunds_)
    <- genDCerts keyHashMap pparams dpState slot ttl txIx reserves

  if spendingBalance < deposits_
    then D.trace ("discarded") QC.discard
    else do

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
    let (_, outputs) = calcOutputsFromBalance balance_ recipientAddrs (Coin 0)

    --- PParam + AV Updates
    (update, updateWitnesses) <-
      genUpdate slot coreKeys keysByStakeHash pparams (utxoSt, dpState)

    -- this is the "model" `TxBody` which is used to calculate the fees
    --
    -- while it only contains a pseudo fee value of 0, the constructed
    -- transcation will have the correct set of witnesses.
    --
    -- Once the transaction body and the witnesses are constructed, we can use
    -- this model to calculate the real fee value and update the fees and
    -- transaction outputs in the final, generated transaction.
    txBody <- genTxBody (Set.fromList inputs) outputs certs wdrls update (Coin 0) slotWithTTL
    let multiSig = Map.fromList $
          (map (\(payScript, _) -> (hashScript payScript, payScript)) spendScripts) ++
          (map (\(_, sScript) -> (hashScript sScript, sScript)) (stakeScripts ++ wdrlScripts))

    -- choose one possible combination of keys for multi-sig scripts
    --
    -- TODO mgudemann due to problems with time-outs, we select one combination
    -- deterministically for each script. Varying the script is possible though.
    let keysLists = map getKeyCombination $ Map.elems multiSig
        msigSignatures = foldl Set.union Set.empty $ map Set.fromList keysLists
        wits = mkTxWits
          txBody
          (spendWitnesses ++ certWitnesses ++ wdrlWitnesses ++ updateWitnesses)
          genesisWitnesses
          keyHashMap
          msigSignatures

    let metadata = Nothing -- TODO generate metadata

    -- calculate real fees of witnesses transaction
    let minimalFees = minfee pparams (Tx txBody wits multiSig metadata)

    -- discard generated transaction if the balance cannot cover the fees
    if minimalFees > balance_
      then D.trace (  "discarded bc. of real fees, minimal: "
                   ++ show minimalFees ++ " Output balance: "
                   ++ show balance_
                   ) QC.discard
      else do

      -- update model transaction with real fees and outputs
      let (fees', outputs') =
            calcOutputsFromBalance balance_ recipientAddrs minimalFees
          txBody' = txBody { _txfee = fees'
                           , _outputs = outputs' }
          wits' = mkTxWits
            txBody'
            (spendWitnesses ++ certWitnesses ++ wdrlWitnesses ++ updateWitnesses)
            genesisWitnesses
            keyHashMap
            msigSignatures

      pure (Tx txBody' wits' multiSig metadata)

mkTxWits
  :: TxBody
  -> [KeyPair]
  -> [CoreKeyPair]
  -> Map AnyKeyHash KeyPair
  -> Set AnyKeyHash
  -> Set WitVKey
mkTxWits txBody keyWits genesisWits keyHashMap msigs =
  makeWitnessesVKey txBody keyWits
  `Set.union` makeWitnessesVKey txBody genesisWits
  `Set.union` makeWitnessesFromScriptKeys txBody keyHashMap msigs

-- | Generate a transaction body with the given inputs/outputs and certificates
genTxBody
  :: Set TxIn
  -> Seq TxOut
  -> Seq DCert
  -> Map RewardAcnt Coin
  -> Update
  -> Coin
  -> SlotNo
  -> Gen TxBody
genTxBody inputs outputs certs wdrls update fee slotWithTTL = do
  return $ TxBody
             inputs
             outputs
             certs
             (Wdrl wdrls)
             fee
             slotWithTTL
             update
             Nothing -- TODO generate metadata

-- | Distribute the sum of `balance_` and `fee` over the addresses, return the
-- sum of `fee` and the remainder of the equal distribution and the list ouf
-- transaction outputs that cover the balance and fees.
--
-- The idea is to have an specified spending balance and fees that must be paid
-- by the selected addresses.
calcOutputsFromBalance
  :: Coin
  -> [Addr]
  -> Coin
  -> (Coin, Seq TxOut)
calcOutputsFromBalance balance_ addrs fee =
  ( fee + splitCoinRem
  , (`TxOut` amountPerOutput) <$> Seq.fromList addrs)
  where
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
-- spend these outputs). If this is not the case, `findPayKeyPairAddr` /
-- `findPayScriptFromAddr` will fail by not finding the matching keys or scripts.
pickSpendingInputs
  :: MultiSigPairs
  -> Map AnyKeyHash KeyPair
  -> UTxO
  -> Gen ([(TxIn, Either KeyPair (MultiSig, MultiSig))], Coin)
pickSpendingInputs scripts keyHashMap (UTxO utxo) = do
  selectedUtxo <- take <$> QC.choose (minNumGenInputs, maxNumGenInputs)
                       <*> QC.shuffle (Map.toList utxo)

  return ( witnessedInput <$> selectedUtxo
         , balance (UTxO (Map.fromList selectedUtxo)))
  where
    witnessedInput (input, TxOut addr@(AddrBase (KeyHashObj _) _) _) =
      (input, Left $ findPayKeyPairAddr addr keyHashMap)
    witnessedInput (input, TxOut addr@(AddrBase (ScriptHashObj _) _) _) =
      (input, Right $ findPayScriptFromAddr addr scripts)
    witnessedInput (input, TxOut addr@(AddrPtr (KeyHashObj _) _) _) =
      (input, Left $ findPayKeyPairAddr addr keyHashMap)
    witnessedInput (input, TxOut addr@(AddrPtr (ScriptHashObj _) _) _) =
      (input, Right $ findPayScriptFromAddr addr scripts)
    witnessedInput _ = error "unsupported address"

-- | Select a subset of the reward accounts to use for reward withdrawals.
pickWithdrawals
  :: Map RewardAcnt Coin
  -> Gen (Map RewardAcnt Coin)
pickWithdrawals wdrls = QC.frequency
  [ (frequencyNoWithdrawals,
     pure Map.empty)
  , (frequencyAFewWithdrawals,
     Map.fromList <$> (QC.sublistOf . (take maxAFewWithdrawals) . Map.toList) wdrls
    )
  , (frequencyPotentiallyManyWithdrawals,
     Map.fromList <$> (QC.sublistOf . Map.toList) wdrls)
  ]

-- | Collect witnesses needed for reward withdrawals.
mkWdrlWits
  :: MultiSigPairs
  -> Map AnyKeyHash KeyPair
  -> Credential
  -> Either KeyPair (MultiSig, MultiSig)
mkWdrlWits scripts _ c@(ScriptHashObj _) = Right $ findStakeScriptFromCred c scripts
mkWdrlWits _ keyHashMap c@(KeyHashObj _)    = Left $ findPayKeyPairCred c keyHashMap

-- | Select recipient addresses that will serve as output targets for a new transaction.
genRecipients
  :: Int
  -> KeyPairs
  -> MultiSigPairs
  -> Gen [Addr]
genRecipients len keys scripts = do
  n' <- QC.frequency (  (if len > 1 then [(1, pure (len - 1))] else [])
                     -- ^ contract size of UTxO (only if at least 2 inputs are chosen)
                     ++ [(2, pure len)]
                     -- ^ keep size
                     ++ [(1, pure $ len + 1)])
                     -- ^ expand size of UTxO

  -- choose m scripts and n keys as recipients
  m  <- QC.choose (0, n' - 1)
  -- keys and scripts are shuffled before
  let n = n' - m
      recipientKeys    = take n keys
      recipientScripts = take m scripts

  let payKeys      = (toCred . fst) <$> recipientKeys
      stakeKeys    = (toCred . snd) <$> recipientKeys
      payScripts   = (scriptToCred . fst) <$> recipientScripts
      stakeScripts = (scriptToCred . fst) <$> recipientScripts

  -- shuffle and zip keys and scripts together as base addresses
  payCreds   <- QC.shuffle (payKeys ++ payScripts)
  stakeCreds <- QC.shuffle (stakeKeys ++ stakeScripts)

  return (zipWith AddrBase payCreds stakeCreds)

genPtrAddrs :: DState -> [Addr] -> Gen [Addr]
genPtrAddrs ds addrs = do
  let pointers = _ptrs ds

  n <- QC.choose (0, min (Map.size pointers) (length addrs))
  pointerList <- take n <$> QC.shuffle (Map.keys pointers)

  let addrs' = zipWith baseAddrToPtrAddr (take n addrs) pointerList

  pure (addrs' ++ (drop n addrs))
    where
      baseAddrToPtrAddr a p = case a of
        AddrBase pay _ -> AddrPtr pay p
        _              -> a
