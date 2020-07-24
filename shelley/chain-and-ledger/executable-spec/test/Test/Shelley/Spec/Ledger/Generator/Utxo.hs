{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -fno-warn-redundant-constraints #-}

module Test.Shelley.Spec.Ledger.Generator.Utxo
  ( genTx,
  )
where

import Control.Iterate.SetAlgebra (forwards)
import qualified Data.Either as Either (lefts, rights)
import Data.List (foldl')
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import qualified Data.Maybe as Maybe (mapMaybe)
import Data.Sequence.Strict (StrictSeq)
import qualified Data.Sequence.Strict as StrictSeq
import Data.Set (Set)
import qualified Data.Set as Set
import GHC.Stack (HasCallStack)
import Shelley.Spec.Ledger.Address (scriptToCred, toCred, pattern Addr)
import qualified Shelley.Spec.Ledger.Address as Address (RewardAcnt (..))
import Shelley.Spec.Ledger.BaseTypes
  ( Network (..),
    StrictMaybe (..),
    maybeToStrictMaybe,
  )
import Shelley.Spec.Ledger.Coin (Coin (..))
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
    toTxUTxO,
    fromTxUTxO,
  )
import Shelley.Spec.Ledger.Value (zeroV, coinToValue, splitValueFee, getAdaAmount, subv, geq)
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
    Value,
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
import Test.Shelley.Spec.Ledger.Generator.Delegation (CertCred (..))
import Test.Shelley.Spec.Ledger.Generator.Trace.DCert (genDCerts)
import Test.Shelley.Spec.Ledger.Generator.Update (genUpdate)

-- TODO make witnesses for forges
-- generate value properly
genValue :: Integer -> Integer -> Gen (Value h)
genValue _ _ = do
  pure zeroV

-- | Generate a new transaction in the context of the LEDGER STS environment and state.
--
-- Selects unspent outputs and spends the funds on a some valid addresses.
-- Also generates valid certificates.
genTx ::
  (HasCallStack, Mock c) =>
  GenEnv c ->
  LedgerEnv ->
  (UTxOState c, DPState c) ->
  Gen (Tx c)
genTx
  ge@( GenEnv
         KeySpace_
           { ksKeyPairs,
             ksGenesisDelegates,
             ksCoreNodes,
             ksIndexedPaymentKeys,
             ksIndexedStakingKeys,
             ksMSigScripts
           }
         constants
       )
  (LedgerEnv slot txIx pparams reserves)
  (utxoSt@(UTxOState utxo _ _ _), dpState) =
    do
      keys' <- QC.shuffle ksKeyPairs
      scripts' <- QC.shuffle ksMSigScripts

      -- inputs
      (witnessedInputs, spendingBalanceUtxo) <-
        pickSpendingInputs constants scripts' ksIndexedPaymentKeys utxo

      wdrls <- pickWithdrawals constants (Map.mapKeys (Address.RewardAcnt Testnet) $ (_rewards . _dstate) dpState)

      let rwdCreds = fmap getRwdCred (Map.keys wdrls)
          wdrlCredentials = fmap (mkWdrlWits scripts' ksIndexedStakingKeys) rwdCreds
          wdrlWitnesses = Either.lefts wdrlCredentials
          wdrlScripts = Either.rights wdrlCredentials

      txforge <- genValue 20 20

      let spendingBalance = spendingBalanceUtxo <> coinToValue (sum wdrls) <> txforge

      let (inputs, spendCredentials) = unzip witnessedInputs
          spendWitnesses = Either.lefts spendCredentials
          spendScripts = Either.rights spendCredentials

      -- output addresses
      recipientAddrs' <- genRecipients (length witnessedInputs) keys' scripts'

      -- maybe convert some addresss to pointer addresses
      recipientAddrs <- genPtrAddrs (_dstate dpState) recipientAddrs'

      ttl <- genNatural 50 100

      let slotWithTTL = slot + SlotNo (fromIntegral ttl)

      -- certificates
      (certs, certCreds, deposits_, refunds_, dpState') <-
        genDCerts ge pparams dpState slot txIx reserves

      let balance_ = spendingBalance <> coinToValue (refunds_ - deposits_ )
      if not (geq balance_ zeroV)
        then QC.discard
        else do
          -- attempt to make provision for certificate deposits (otherwise discard this generator)
          let stakeScripts = pickStakeScripts certCreds
              genesisWitnesses = pickGenesisWitnesses certCreds
              genesisDelegationWitnesses = pickGenesisDelegateWitnesses certCreds
              certWitnesses = pickAWitnesses certCreds

          -- calc. fees and output amounts
          let (_, outputs) = calcOutputsFromBalance balance_ recipientAddrs (Coin 0)

          --- PParam + AV Updates
          (update, updateWitnesses) <-
            genUpdate constants slot ksCoreNodes ((snd <$> ksCoreNodes) <> ksGenesisDelegates) pparams (utxoSt, dpState')

          -- this is the "model" `TxBody` which is used to calculate the fees
          --
          -- while it only contains a pseudo fee value of 0, the constructed
          -- transcation will have the correct set of witnesses.
          --
          -- Once the transaction body and the witnesses are constructed, we can use
          -- this model to calculate the real fee value and update the fees and
          -- transaction outputs in the final, generated transaction.
          txBody <- genTxBody (Set.fromList inputs) outputs certs txforge wdrls update (Coin 0) slotWithTTL
          let multiSig =
                Map.fromList $
                  ( map
                      ( \(payScript, _) ->
                          (hashScript payScript, payScript)
                      )
                      spendScripts
                  )
                    ++ ( map
                           ( \(_, sScript) ->
                               (hashScript sScript, sScript)
                           )
                           (stakeScripts ++ wdrlScripts)
                       )

          -- choose one possible combination of keys for multi-sig scripts
          --
          -- TODO mgudemann due to problems with time-outs, we select one combination
          -- deterministically for each script. Varying the script is possible though.

          let wits =
                mkTxWits
                  ksIndexedPaymentKeys
                  ksIndexedStakingKeys
                  (hashAnnotated txBody)
                  ( spendWitnesses
                      ++ wdrlWitnesses
                      ++ certWitnesses
                      ++ updateWitnesses
                      ++ genesisWitnesses
                      ++ genesisDelegationWitnesses
                  )
                  multiSig

          let metadata = SNothing -- TODO generate metadata

          -- calculate real fees of witnesses transaction
          let minimalFees = minfeeBound pparams (Tx txBody wits metadata)

          -- discard generated transaction if the balance cannot cover the fees
          if minimalFees > getAdaAmount balance_
            then QC.discard
            else do
              -- update model transaction with real fees and outputs
              let (fees', outputs') =
                    calcOutputsFromBalance balance_ recipientAddrs minimalFees
                  txBody' =
                    txBody
                      { _txfee = fees',
                        _outputs = outputs'
                      }
                  wits' =
                    mkTxWits
                      ksIndexedPaymentKeys
                      ksIndexedStakingKeys
                      (hashAnnotated txBody')
                      ( spendWitnesses
                          ++ wdrlWitnesses
                          ++ certWitnesses
                          ++ updateWitnesses
                          ++ genesisWitnesses
                          ++ genesisDelegationWitnesses
                      )
                      multiSig

              pure (Tx txBody' wits' metadata)
    where
      pickGenesisDelegateWitnesses certs =
        foldl' (++) [] $
          Maybe.mapMaybe
            ( \case
                DelegateCred c -> Just $ asWitness <$> c
                _ -> Nothing
            )
            certs
      pickGenesisWitnesses certs =
        foldl' (++) [] $
          Maybe.mapMaybe
            ( \case
                CoreKeyCred c -> Just $ asWitness <$> c
                _ -> Nothing
            )
            certs
      pickStakeScripts =
        Maybe.mapMaybe
          ( \case
              ScriptCred c -> Just c
              _ -> Nothing
          )
      pickAWitnesses =
        Maybe.mapMaybe
          ( \case
              StakeCred c -> Just $ asWitness c
              PoolCred c -> Just $ asWitness c
              _ -> Nothing
          )

mkTxWits ::
  (HasCallStack, Mock c) =>
  Map (KeyHash 'Payment c) (KeyPair 'Payment c) ->
  Map (KeyHash 'Staking c) (KeyPair 'Staking c) ->
  Hash c (TxBody c) ->
  [KeyPair 'Witness c] ->
  Map (ScriptHash c) (MultiSig c) ->
  WitnessSet c
mkTxWits
  indexedPaymentKeys
  indexedStakingKeys
  txBodyHash
  awits
  msigs =
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
  Set (TxIn c) ->
  StrictSeq (TxOut c) ->
  StrictSeq (DCert c) ->
  Value c ->
  Map (RewardAcnt c) Coin ->
  Maybe (Update c) ->
  Coin ->
  SlotNo ->
  Gen (TxBody c)
genTxBody inputs outputs certs forge wdrls update fee slotWithTTL = do
  return $
    TxBody
      inputs
      outputs
      certs
      forge
      (Wdrl wdrls)
      fee
      slotWithTTL
      (maybeToStrictMaybe update)
      SNothing -- TODO generate metadata

-- | Distribute the sum of `balance_` and `fee` over the addresses, return the
-- sum of `fee` and the remainder of the equal distribution and the list of
-- transaction outputs that cover the balance and fees.
--
-- The idea is to have an specified spending balance and fees that must be paid
-- by the selected addresses.
calcOutputsFromBalance ::
  (HasCallStack, Crypto c) =>
  Value c ->
  [Addr c] ->
  Coin ->
  (Coin, StrictSeq (TxOut c))
calcOutputsFromBalance balance_ addrs fee =
  ( fee <> splitCoinRem,
    (`TxOut` amountPerOutput) <$> StrictSeq.fromList addrs
  )
  where
    -- split the available balance into equal portions (one for each address),
    -- if there is a remainder, then add it to the fee.
    balanceAfterFee = subv balance_ $ coinToValue fee
    (amountPerOutput, splitCoinRem) = splitValueFee balanceAfterFee (fromIntegral $ length addrs)

-- | Select unspent output(s) to serve as inputs for a new transaction
--
-- Returns the inputs, paired with the KeyPair or multi-sig script required to
-- witness the spending of the input.
-- Also returns the total spendable balance.

-- NOTE: this function needs access to the keys and multi-sig scripts that the
-- given UTxO originated from (in order to produce the appropriate witnesses to
-- spend these outputs). If this is not the case, `findPayKeyPairAddr` /
-- `findPayScriptFromAddr` will fail by not finding the matching keys or scripts.
pickSpendingInputs ::
  (HasCallStack, Crypto c) =>
  Constants ->
  MultiSigPairs c ->
  Map (KeyHash 'Payment c) (KeyPair 'Payment c) ->
  UTxO c ->
  Gen ([(TxIn c, Either (KeyPair 'Witness c) (MultiSig c, MultiSig c))], Value c)
pickSpendingInputs Constants {minNumGenInputs, maxNumGenInputs} scripts keyHashMap (UTxO utxo) = do
  selectedUtxo <-
    take <$> QC.choose (minNumGenInputs, maxNumGenInputs)
      <*> QC.shuffle (toTxUTxO (UTxO utxo))

  return
    ( witnessedInput <$> selectedUtxo,
      balance (fromTxUTxO selectedUtxo)
    )
  where
    witnessedInput (input, TxOut addr@(Addr _ (KeyHashObj _) _) _) =
      (input, Left . asWitness $ findPayKeyPairAddr addr keyHashMap)
    witnessedInput (input, TxOut addr@(Addr _ (ScriptHashObj _) _) _) =
      (input, Right $ findPayScriptFromAddr addr scripts)
    witnessedInput _ = error "unsupported address"

-- | Select a subset of the reward accounts to use for reward withdrawals.
pickWithdrawals ::
  HasCallStack =>
  Constants ->
  Map (RewardAcnt h) Coin ->
  Gen (Map (RewardAcnt h) Coin)
pickWithdrawals
  Constants
    { frequencyNoWithdrawals,
      frequencyAFewWithdrawals,
      frequencyPotentiallyManyWithdrawals,
      maxAFewWithdrawals
    }
  wdrls =
    QC.frequency
      [ ( frequencyNoWithdrawals,
          pure Map.empty
        ),
        ( frequencyAFewWithdrawals,
          Map.fromList <$> (QC.sublistOf . (take maxAFewWithdrawals) . Map.toList) wdrls
        ),
        ( frequencyPotentiallyManyWithdrawals,
          Map.fromList <$> (QC.sublistOf . Map.toList) wdrls
        )
      ]

-- | Collect witnesses needed for reward withdrawals.
mkWdrlWits ::
  (HasCallStack, Crypto c) =>
  MultiSigPairs c ->
  Map (KeyHash 'Staking c) (KeyPair 'Staking c) ->
  Credential c 'Staking ->
  Either (KeyPair 'Witness c) (MultiSig c, MultiSig c)
mkWdrlWits scripts _ c@(ScriptHashObj _) =
  Right $
    findStakeScriptFromCred (asWitness c) scripts
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
