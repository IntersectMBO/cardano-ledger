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
{-# OPTIONS_GHC -Wno-redundant-constraints #-}

module Test.Shelley.Spec.Ledger.Generator.Utxo
  ( GenTxException (..),
    genTx,
    tryGenTx,
    splitCoin,
  )
where

import Cardano.Ledger.Era (Crypto, Era)
import qualified Control.Exception as Exn
import Control.Iterate.SetAlgebra (forwards)
import qualified Data.Either as Either (partitionEithers)
import Data.Foldable (fold)
import Data.List (foldl')
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Sequence.Strict (StrictSeq)
import qualified Data.Sequence.Strict as StrictSeq
import qualified Data.Set as Set
import Data.Word (Word64)
import GHC.Stack (HasCallStack)
import Shelley.Spec.Ledger.API
  ( DCert,
    MultiSig,
    ScriptHash,
    Update,
  )
import Shelley.Spec.Ledger.Address
  ( Addr (..),
    RewardAcnt (..),
    scriptToCred,
    toCred,
  )
import Shelley.Spec.Ledger.BaseTypes
  ( Network (..),
    StrictMaybe (..),
    maybeToStrictMaybe,
  )
import Shelley.Spec.Ledger.Coin (Coin (..))
import Shelley.Spec.Ledger.Credential (Credential (..), StakeReference (..))
import Shelley.Spec.Ledger.Hashing (hashAnnotated)
import Shelley.Spec.Ledger.Keys
  ( Hash,
    KeyHash,
    KeyPair,
    KeyRole (..),
    asWitness,
  )
import Shelley.Spec.Ledger.LedgerState
  ( DPState (..),
    DState (..),
    KeyPairs,
    UTxOState (..),
    minfeeBound,
    _dstate,
    _ptrs,
    _rewards,
  )
import Shelley.Spec.Ledger.MetaData (MetaDataHash)
import Shelley.Spec.Ledger.STS.Ledger (LedgerEnv (..))
import Shelley.Spec.Ledger.Slot (SlotNo (..))
import Shelley.Spec.Ledger.Tx
  ( Tx (..),
    TxBody (..),
    TxIn,
    TxOut (..),
    WitnessSet,
    WitnessSetHKD (..),
    getKeyCombination,
    hashScript,
  )
import Shelley.Spec.Ledger.TxBody (Wdrl (..))
import Shelley.Spec.Ledger.UTxO
  ( UTxO (..),
    balance,
    makeWitnessesFromScriptKeys,
    makeWitnessesVKey,
  )
import qualified Cardano.Ledger.Val as Val
import Test.QuickCheck (Gen)
import qualified Test.QuickCheck as QC
import Test.Shelley.Spec.Ledger.ConcreteCryptoTypes
  ( Mock,
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
import Test.Shelley.Spec.Ledger.Utils (MultiSigPairs)

splitCoin :: Coin -> Integer -> (Coin, Coin)
splitCoin (Coin n) 0 = (Coin 0, Coin n)
splitCoin (Coin n) m
  | m <= 0 = error "must split coins into positive parts"
  | otherwise = (Coin $ n `div` m, Coin $ n `rem` m)

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
  (HasCallStack, Era era, Mock (Crypto era)) =>
  GenEnv era ->
  LedgerEnv ->
  (UTxOState era, DPState era) ->
  Gen (Tx era)
genTx ge le st =
  tryGenTx ge le st >>= \case
    Left exn -> Exn.throw exn
    Right tx -> pure tx

data GenTxException
  = -- | Every transaction that the generator attempted had fees that exceeded
    -- the available balance.
    GenTxExceptionInsufficientSpendingBalance !InsufficientSpendingBalanceInfo
  deriving (Show)

instance Exn.Exception GenTxException

-- | The data for 'GenTxExceptionInsufficientSpendingBalance'.
data InsufficientSpendingBalanceInfo = InsufficientSpendingBalanceInfo
  { -- | The smallest amount by which the fees exceeded the unspent balance for
    -- any of the transactions attempted.
    isbiDiff :: !Coin
  }
  deriving (Show)

tryGenTx ::
  (HasCallStack, Era era, Mock (Crypto era)) =>
  GenEnv era ->
  LedgerEnv ->
  (UTxOState era, DPState era) ->
  Gen (Either GenTxException (Tx era))
tryGenTx ge@(GenEnv _ (Constants {genTxRetries})) =
  genTxRetry genTxRetries (Coin (fromIntegral (maxBound :: Word64))) ge

genTxRetry ::
  (HasCallStack, Era era, Mock (Crypto era)) =>
  Int ->
  Coin ->
  GenEnv era ->
  LedgerEnv ->
  (UTxOState era, DPState era) ->
  Gen (Either GenTxException (Tx era))
genTxRetry
  n
  minDiffSoFar
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
          mkTxWits' =
            mkTxWits
              ksIndexedPaymentKeys
              ksIndexedStakingKeys
              wits
              scripts
              . hashAnnotated
      -------------------------------------------------------------------------
      -- SpendingBalance, Output Addresses (including some Pointer addresses)
      -- and a Outputs builder that distributes the given balance over addresses.
      -------------------------------------------------------------------------
      let spendingBalance =
            spendingBalanceUtxo
              <> (fold (snd <$> wdrls)) Val.~~ deposits
              <> refunds
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
          pure $ Right $ Tx txBody (mkTxWits' txBody) metadata
        else do
          let minDiffSoFar' = min minDiffSoFar (fees Val.~~ spendingBalance)
          if n < 1
            then
              pure $
                Left $
                  GenTxExceptionInsufficientSpendingBalance
                    InsufficientSpendingBalanceInfo
                      { isbiDiff = minDiffSoFar'
                      }
            else genTxRetry (n - 1) minDiffSoFar' ge env st

genTimeToLive :: SlotNo -> Gen SlotNo
genTimeToLive currentSlot = do
  ttl <- genNatural 50 100
  pure $ currentSlot + SlotNo (fromIntegral ttl)

mkScriptWits ::
  (HasCallStack, Era era) =>
  [(MultiSig era, MultiSig era)] ->
  [(MultiSig era, MultiSig era)] ->
  Map (ScriptHash era) (MultiSig era)
mkScriptWits payScripts stakeScripts =
  Map.fromList $
    (hashPayScript <$> payScripts)
      ++ (hashStakeScript <$> stakeScripts)
  where
    hashPayScript (payScript, _) = (hashScript payScript, payScript)
    hashStakeScript (_, sScript) = (hashScript sScript, sScript)

mkTxWits ::
  (Era era, HasCallStack, Mock (Crypto era)) =>
  Map (KeyHash 'Payment era) (KeyPair 'Payment era) ->
  Map (KeyHash 'Staking era) (KeyPair 'Staking era) ->
  [KeyPair 'Witness era] ->
  Map (ScriptHash era) (MultiSig era) ->
  Hash era (TxBody era) ->
  WitnessSet era
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
  (HasCallStack, Era era) =>
  [TxIn era] ->
  StrictSeq (TxOut era) ->
  StrictSeq (DCert era) ->
  [(RewardAcnt era, Coin)] ->
  Maybe (Update era) ->
  Coin ->
  SlotNo ->
  StrictMaybe (MetaDataHash era) ->
  Gen (TxBody era)
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
  (HasCallStack, Era era) =>
  Coin ->
  [Addr era] ->
  Coin ->
  (Coin, StrictSeq (TxOut era))
calcOutputsFromBalance balance_ addrs fee =
  ( fee <> splitCoinRem,
    (`TxOut` amountPerOutput) <$> StrictSeq.fromList addrs
  )
  where
    -- split the available balance into equal portions (one for each address),
    -- if there is a remainder, then add it to the fee.
    balanceAfterFee = balance_ Val.~~ fee
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
  (HasCallStack, Era era) =>
  Constants ->
  Map (KeyHash 'Payment era) (KeyPair 'Payment era) ->
  Map (ScriptHash era) (MultiSig era, MultiSig era) ->
  UTxO era ->
  Gen ([TxIn era], Coin, ([KeyPair 'Witness era], [(MultiSig era, MultiSig era)]))
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
  (HasCallStack, Era era) =>
  Constants ->
  Map (ScriptHash era) (MultiSig era, MultiSig era) ->
  Map (KeyHash 'Staking era) (KeyPair 'Staking era) ->
  Map (Credential 'Staking era) Coin ->
  Gen
    ( [(RewardAcnt era, Coin)],
      ([KeyPair 'Witness era], [(MultiSig era, MultiSig era)])
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
      toRewardAcnt (rwd, coin) = (RewardAcnt Testnet rwd, coin)
      genWrdls wdrls_ = do
        selectedWrdls <- map toRewardAcnt <$> QC.sublistOf wdrls_
        let wits = (mkWdrlWits ksIndexedStakeScripts ksIndexedStakingKeys . getRwdCred . fst) <$> selectedWrdls
        return (selectedWrdls, Either.partitionEithers wits)

-- | Collect witnesses needed for reward withdrawals.
mkWdrlWits ::
  (HasCallStack, Era era) =>
  Map (ScriptHash era) (MultiSig era, MultiSig era) ->
  Map (KeyHash 'Staking era) (KeyPair 'Staking era) ->
  Credential 'Staking era ->
  Either (KeyPair 'Witness era) (MultiSig era, MultiSig era)
mkWdrlWits scriptsByStakeHash _ era@(ScriptHashObj _) =
  Right $
    findStakeScriptFromCred (asWitness era) scriptsByStakeHash
mkWdrlWits _ keyHashMap era@(KeyHashObj _) =
  Left $
    asWitness $
      findPayKeyPairCred era keyHashMap

-- | Select recipient addresses that will serve as output targets for a new transaction.
genRecipients ::
  (HasCallStack, Era era) =>
  Int ->
  KeyPairs era ->
  MultiSigPairs era ->
  Gen [Addr era]
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
