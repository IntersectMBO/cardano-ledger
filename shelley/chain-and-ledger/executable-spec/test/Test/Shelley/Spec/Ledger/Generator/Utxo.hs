{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -fno-warn-redundant-constraints #-}

module Test.Shelley.Spec.Ledger.Generator.Utxo
  ( genTx,
  )
where

import Control.Iterate.SetAlgebra (forwards)
import qualified Data.Either as Either (partitionEithers)
import Data.List (foldl')
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Sequence.Strict (StrictSeq)
import qualified Data.Sequence.Strict as StrictSeq
import qualified Data.Set as Set
import GHC.Stack (HasCallStack)
import Shelley.Spec.Ledger.Address (scriptToCred, toCred, pattern Addr)
import qualified Shelley.Spec.Ledger.Address as Address (RewardAcnt (..))
import Shelley.Spec.Ledger.BaseTypes
  ( Network (..),
    StrictMaybe (..),
    maybeToStrictMaybe,
  )
import Shelley.Spec.Ledger.Coin (Coin (..), splitCoin)
import Shelley.Spec.Ledger.Credential
  ( pattern KeyHashObj,
    pattern ScriptHashObj,
    pattern StakeRefBase,
    pattern StakeRefPtr,
  )
import Shelley.Spec.Ledger.Crypto (Crypto)
import Shelley.Spec.Ledger.Hashing (hashAnnotated)
import Shelley.Spec.Ledger.Keys
  ( Hash,
    KeyHash,
    KeyPair,
    KeyRole (..),
    asWitness,
  )
import Shelley.Spec.Ledger.LedgerState
  ( minfeeBound,
    _dstate,
    _ptrs,
    _rewards,
    pattern UTxOState,
  )
import Shelley.Spec.Ledger.MetaData (MetaDataHash)
import Shelley.Spec.Ledger.STS.Ledger (LedgerEnv (..))
import Shelley.Spec.Ledger.Slot (SlotNo (..))
import Shelley.Spec.Ledger.Tx
  ( WitnessSetHKD (..),
    getKeyCombination,
    hashScript,
    pattern Tx,
    pattern TxBody,
    pattern TxOut,
  )
import Shelley.Spec.Ledger.TxData (Wdrl (..), getRwdCred, _outputs, _txfee)
import Shelley.Spec.Ledger.UTxO
  ( balance,
    makeWitnessesFromScriptKeys,
    makeWitnessesVKey,
    pattern UTxO,
  )
import Test.QuickCheck (Gen)
import qualified Test.QuickCheck as QC
import Test.Shelley.Spec.Ledger.ConcreteCryptoTypes
  ( Addr,
    Credential,
    DCert,
    DPState,
    DState,
    KeyPairs,
    Mock,
    MultiSig,
    MultiSigPairs,
    RewardAcnt,
    ScriptHash,
    Tx,
    TxBody,
    TxIn,
    TxOut,
    UTxO,
    UTxOState,
    Update,
    WitnessSet,
  )
import Test.Shelley.Spec.Ledger.Generator.Constants (Constants (..))
import Test.Shelley.Spec.Ledger.Generator.Core
  ( GenEnv (..),
    KeySpace (..),
    findPayKeyPairAddr,
    findPayKeyPairCred,
    findPayScriptFromAddr,
    findStakeScriptFromCred,
    genNatural,
  )
import Test.Shelley.Spec.Ledger.Generator.MetaData (genMetaData)
import Test.Shelley.Spec.Ledger.Generator.Trace.DCert (genDCerts)
import Test.Shelley.Spec.Ledger.Generator.Update (genUpdate)

-- | Generates a transaction in the context of the LEDGER STS environment
-- and state.
--
--  A generated transaction may not have sufficient spending balance and
-- need to be discarded. In this case we retry the generator,
-- up to 'genTxRetries' times, before failing hard with an error.
--
-- Note: the spending balance emerges from inputs, refund withdrawals,
-- certificate deposits and fees (which in turn depend on number of
-- inputs, outputs, witnesses, metadata etc.). It's hard to avoid this
-- completely, but in practice it is relatively easy to calibrate
-- the generator 'Constants' so that there is sufficient spending balance.
genTx ::
  (HasCallStack, Mock c) =>
  GenEnv c ->
  LedgerEnv ->
  (UTxOState c, DPState c) ->
  Gen (Tx c)
genTx ge@(GenEnv _ (Constants {genTxRetries})) =
  genTxRetry genTxRetries ge

genTxRetry ::
  (HasCallStack, Mock c) =>
  Int ->
  GenEnv c ->
  LedgerEnv ->
  (UTxOState c, DPState c) ->
  Gen (Tx c)
genTxRetry
  n
  ge@( GenEnv
         KeySpace_
           { ksKeyPairs,
             ksCoreNodes,
             ksMSigScripts,
             ksIndexedGenDelegates,
             ksIndexedPaymentKeys,
             ksIndexedStakingKeys,
             ksIndexedPayScripts,
             ksIndexedStakeScripts
           }
         constants
       )
  env@(LedgerEnv slot txIx pparams reserves)
  st@(utxoSt@(UTxOState utxo _ _ _), dpState) =
    do
      keys' <- QC.shuffle ksKeyPairs
      scripts' <- QC.shuffle ksMSigScripts
      -------------------------------------------------------------------------
      -- Generate the building blocks of a TxBody
      -------------------------------------------------------------------------
      (inputs, spendingBalanceUtxo, (spendWits, spendScripts)) <-
        genInputs constants ksIndexedPaymentKeys ksIndexedPayScripts utxo
      (wdrls, (wdrlWits, wdrlScripts)) <-
        genWithdrawals constants ksIndexedStakeScripts ksIndexedStakingKeys ((_rewards . _dstate) dpState)
      (update, updateWits) <-
        genUpdate constants slot ksCoreNodes ksIndexedGenDelegates pparams (utxoSt, dpState)
      (certs, deposits, refunds, dpState', (certWits, certScripts)) <-
        genDCerts ge pparams dpState slot txIx reserves
      (metadata, metadataHash) <- genMetaData constants
      ttl <- genTimeToLive slot
      -------------------------------------------------------------------------
      -- Gather Key Witnesses and Scripts, prepare a constructor for Tx Wits
      -------------------------------------------------------------------------
      let wits = spendWits ++ wdrlWits ++ certWits ++ updateWits
          scripts = mkScriptWits spendScripts (certScripts ++ wdrlScripts)
          mkTxWits' = mkTxWits ksIndexedPaymentKeys ksIndexedStakingKeys wits scripts . hashAnnotated
      -------------------------------------------------------------------------
      -- SpendingBalance, Output Addresses (including some Pointer addresses)
      -- and a Outputs builder that distributes the given balance over addresses.
      -------------------------------------------------------------------------
      let spendingBalance = spendingBalanceUtxo + (sum (snd <$> wdrls)) - deposits + refunds
      outputAddrs <-
        genRecipients (length inputs) keys' scripts'
          >>= genPtrAddrs (_dstate dpState')
      let mkOutputs = calcOutputsFromBalance spendingBalance outputAddrs
      -------------------------------------------------------------------------
      -- Build a Draft Tx and use it to calculate transaction fees
      -------------------------------------------------------------------------
      let draftFee = Coin 0
          draftOutputs = snd (mkOutputs draftFee)
      draftTxBody <- genTxBody inputs draftOutputs certs wdrls update draftFee ttl metadataHash
      let draftTx = Tx draftTxBody (mkTxWits' draftTxBody) metadata
          fees = minfeeBound pparams draftTx
      -------------------------------------------------------------------------
      -- Generate final Tx now that we have the real fees. We need to recompute
      -- the output amounts and in turn the txBody and its witness set.
      -------------------------------------------------------------------------
      if spendingBalance >= fees
        then do
          let (actualFees', outputs') = mkOutputs fees
              txBody = draftTxBody {_txfee = actualFees', _outputs = outputs'}
          pure $ Tx txBody (mkTxWits' txBody) metadata
        else retryOrFail n
    where
      retryOrFail 0 = error "genTx: insufficient spending balance"
      retryOrFail n' = genTxRetry (n' - 1) ge env st

genTimeToLive :: SlotNo -> Gen SlotNo
genTimeToLive currentSlot = do
  ttl <- genNatural 50 100
  pure $ currentSlot + SlotNo (fromIntegral ttl)

mkScriptWits ::
  (HasCallStack, Crypto c) =>
  [(MultiSig c, MultiSig c)] ->
  [(MultiSig c, MultiSig c)] ->
  Map (ScriptHash c) (MultiSig c)
mkScriptWits payScripts stakeScripts =
  Map.fromList $
    (hashPayScript <$> payScripts)
      ++ (hashStakeScript <$> stakeScripts)
  where
    hashPayScript (payScript, _) = (hashScript payScript, payScript)
    hashStakeScript (_, sScript) = (hashScript sScript, sScript)

mkTxWits ::
  (HasCallStack, Mock c) =>
  Map (KeyHash 'Payment c) (KeyPair 'Payment c) ->
  Map (KeyHash 'Staking c) (KeyPair 'Staking c) ->
  [KeyPair 'Witness c] ->
  Map (ScriptHash c) (MultiSig c) ->
  Hash c (TxBody c) ->
  WitnessSet c
mkTxWits
  indexedPaymentKeys
  indexedStakingKeys
  awits
  msigs
  txBodyHash =
    WitnessSet
      { addrWits =
          makeWitnessesVKey txBodyHash awits
            `Set.union` makeWitnessesFromScriptKeys
              txBodyHash
              ( indexedPaymentKeysAsWitnesses
                  `Map.union` indexedStakingKeysAsWitnesses
              )
              msigSignatures,
        msigWits = msigs,
        bootWits = mempty
      }
    where
      indexedPaymentKeysAsWitnesses =
        Map.fromAscList
          . map (\(a, b) -> (asWitness a, asWitness b))
          . Map.toAscList
          $ indexedPaymentKeys
      indexedStakingKeysAsWitnesses =
        Map.fromAscList
          . map (\(a, b) -> (asWitness a, asWitness b))
          . Map.toAscList
          $ indexedStakingKeys
      keysLists = map getKeyCombination $ Map.elems msigs
      msigSignatures = foldl' Set.union Set.empty $ map Set.fromList keysLists

-- | Generate a transaction body with the given inputs/outputs and certificates
genTxBody ::
  (HasCallStack, Crypto c) =>
  [TxIn c] ->
  StrictSeq (TxOut c) ->
  StrictSeq (DCert c) ->
  [(RewardAcnt c, Coin)] ->
  Maybe (Update c) ->
  Coin ->
  SlotNo ->
  StrictMaybe (MetaDataHash c) ->
  Gen (TxBody c)
genTxBody inputs outputs certs wdrls update fee slotWithTTL mdHash = do
  return $
    TxBody
      (Set.fromList inputs)
      outputs
      certs
      (Wdrl (Map.fromList wdrls))
      fee
      slotWithTTL
      (maybeToStrictMaybe update)
      mdHash

-- | Distribute the sum of `balance_` and `fee` over the addresses, return the
-- sum of `fee` and the remainder of the equal distribution and the list ouf
-- transaction outputs that cover the balance and fees.
--
-- The idea is to have an specified spending balance and fees that must be paid
-- by the selected addresses.
calcOutputsFromBalance ::
  (HasCallStack, Crypto c) =>
  Coin ->
  [Addr c] ->
  Coin ->
  (Coin, StrictSeq (TxOut c))
calcOutputsFromBalance balance_ addrs fee =
  ( fee + splitCoinRem,
    (`TxOut` amountPerOutput) <$> StrictSeq.fromList addrs
  )
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
genInputs ::
  (HasCallStack, Crypto c) =>
  Constants ->
  Map (KeyHash 'Payment c) (KeyPair 'Payment c) ->
  Map (ScriptHash c) (MultiSig c, MultiSig c) ->
  UTxO c ->
  Gen ([TxIn c], Coin, ([KeyPair 'Witness c], [(MultiSig c, MultiSig c)]))
genInputs Constants {minNumGenInputs, maxNumGenInputs} keyHashMap payScriptMap (UTxO utxo) = do
  selectedUtxo <-
    take <$> QC.choose (minNumGenInputs, maxNumGenInputs)
      <*> QC.shuffle (Map.toList utxo)

  let (inputs, witnesses) = unzip (witnessedInput <$> selectedUtxo)
  return
    ( inputs,
      balance (UTxO (Map.fromList selectedUtxo)),
      Either.partitionEithers witnesses
    )
  where
    witnessedInput (input, TxOut addr@(Addr _ (KeyHashObj _) _) _) =
      (input, Left . asWitness $ findPayKeyPairAddr addr keyHashMap)
    witnessedInput (input, TxOut addr@(Addr _ (ScriptHashObj _) _) _) =
      (input, Right $ findPayScriptFromAddr addr payScriptMap)
    witnessedInput _ = error "unsupported address"

-- | Select a subset of the reward accounts to use for reward withdrawals.
genWithdrawals ::
  (HasCallStack, Crypto c) =>
  Constants ->
  Map (ScriptHash c) (MultiSig c, MultiSig c) ->
  Map (KeyHash 'Staking c) (KeyPair 'Staking c) ->
  Map (Credential c 'Staking) Coin ->
  Gen
    ( [(RewardAcnt c, Coin)],
      ([KeyPair 'Witness c], [(MultiSig c, MultiSig c)])
    )
genWithdrawals
  Constants
    { frequencyNoWithdrawals,
      frequencyAFewWithdrawals,
      frequencyPotentiallyManyWithdrawals,
      maxAFewWithdrawals
    }
  ksIndexedStakeScripts
  ksIndexedStakingKeys
  wdrls =
    QC.frequency
      [ ( frequencyNoWithdrawals,
          pure ([], ([], []))
        ),
        ( frequencyAFewWithdrawals,
          genWrdls (take maxAFewWithdrawals . Map.toList $ wdrls)
        ),
        ( frequencyPotentiallyManyWithdrawals,
          genWrdls (Map.toList wdrls)
        )
      ]
    where
      toRewardAcnt (rwd, coin) = (Address.RewardAcnt Testnet rwd, coin)
      genWrdls wdrls_ = do
        selectedWrdls <- map toRewardAcnt <$> QC.sublistOf wdrls_
        let wits = (mkWdrlWits ksIndexedStakeScripts ksIndexedStakingKeys . getRwdCred . fst) <$> selectedWrdls
        return (selectedWrdls, Either.partitionEithers wits)

-- | Collect witnesses needed for reward withdrawals.
mkWdrlWits ::
  (HasCallStack, Crypto c) =>
  Map (ScriptHash c) (MultiSig c, MultiSig c) ->
  Map (KeyHash 'Staking c) (KeyPair 'Staking c) ->
  Credential c 'Staking ->
  Either (KeyPair 'Witness c) (MultiSig c, MultiSig c)
mkWdrlWits scriptsByStakeHash _ c@(ScriptHashObj _) =
  Right $
    findStakeScriptFromCred (asWitness c) scriptsByStakeHash
mkWdrlWits _ keyHashMap c@(KeyHashObj _) =
  Left $
    asWitness $
      findPayKeyPairCred c keyHashMap

-- | Select recipient addresses that will serve as output targets for a new transaction.
genRecipients ::
  (HasCallStack, Crypto c) =>
  Int ->
  KeyPairs c ->
  MultiSigPairs c ->
  Gen [Addr c]
genRecipients len keys scripts = do
  n' <-
    QC.frequency
      ( (if len > 1 then [(1, pure (len - 1))] else [])
          -- contract size of UTxO (only if at least 2 inputs are chosen)
          ++ [(2, pure len)]
          -- keep size
          ++ [(1, pure $ len + 1)]
      )
  -- expand size of UTxO

  -- choose m scripts and n keys as recipients
  m <- QC.choose (0, n' - 1)
  -- keys and scripts are shuffled before
  let n = n' - m
      recipientKeys = take n keys
      recipientScripts = take m scripts

  let payKeys = (toCred . fst) <$> recipientKeys
      stakeKeys = (toCred . snd) <$> recipientKeys
      payScripts = (scriptToCred . fst) <$> recipientScripts
      stakeScripts = (scriptToCred . fst) <$> recipientScripts

  -- shuffle and zip keys and scripts together as base addresses
  payCreds <- QC.shuffle (payKeys ++ payScripts)
  stakeCreds <- QC.shuffle (stakeKeys ++ stakeScripts)
  let stakeCreds' = fmap StakeRefBase stakeCreds

  return (zipWith (Addr Testnet) payCreds stakeCreds')

genPtrAddrs :: HasCallStack => DState h -> [Addr h] -> Gen [Addr h]
genPtrAddrs ds addrs = do
  let pointers = forwards (_ptrs ds)

  n <- QC.choose (0, min (Map.size pointers) (length addrs))
  pointerList <- take n <$> QC.shuffle (Map.keys pointers)

  let addrs' = zipWith baseAddrToPtrAddr (take n addrs) pointerList

  pure (addrs' ++ (drop n addrs))
  where
    baseAddrToPtrAddr a p = case a of
      Addr n pay _ -> Addr n pay (StakeRefPtr p)
      _ -> a
