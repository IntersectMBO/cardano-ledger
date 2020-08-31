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

module Test.Shelley.Spec.Ledger.Generator.Utxo
  ( genTx,
    Delta (..),
  )
where

import Debug.Trace
import Data.Hashable(hash)
import Cardano.Binary (serialize)
import Cardano.Slotting.Slot(SlotNo(..))
import Control.Iterate.SetAlgebra (forwards)
import qualified Data.ByteString.Lazy as BSL
import qualified Data.Either as Either (partitionEithers)
import Data.List (foldl')
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Sequence.Strict (StrictSeq)
import qualified Data.Sequence.Strict as StrictSeq
import qualified Data.Set as Set
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
  ( DPState (..),
    DState (..),
    KeyPairs,
    UTxOState (..),
    minfee,
    _dstate,
    _ptrs,
    _rewards,
  )
import Shelley.Spec.Ledger.MetaData (MetaDataHash)
import Shelley.Spec.Ledger.PParams (PParams, PParams' (..))
import Shelley.Spec.Ledger.STS.Ledger (LedgerEnv (..))
import Shelley.Spec.Ledger.Tx
  ( Tx (..),
    TxBody (..),
    TxIn(..),
    TxOut (..),
    WitnessSet,
    WitnessSetHKD (..),
    getKeyCombination,
    hashScript,
  )
import Shelley.Spec.Ledger.TxData (Wdrl (..))
import Shelley.Spec.Ledger.UTxO
  ( UTxO (..),
    balance,
    makeWitnessesFromScriptKeys,
    makeWitnessesVKey,
  )
-- import Shelley.Spec.Ledger.Value (Val (..))
import Shelley.Spec.Ledger.Value
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

-- | Generates a transaction in the context of the LEDGER STS environment
-- and state.
--
--  A generated transaction may not have sufficient spending balance and
-- need to be discarded. In that case we try to compute a Delta, that when
-- added (applyDelta) to the transaction, repairs it. The repair is made
-- by adding additional inputs from which more Ada can flow into the fee.
-- If that doesn't fix it, we add add more inputs to the Delta.
-- Experience shows that this converges quite quickly (in traces we never saw
-- more than 3 iterations).
--
-- Note: the spending balance emerges from inputs, refund withdrawals,
-- certificate deposits and fees (which in turn depend on number of
-- inputs, outputs, witnesses, metadata etc.). It's hard to avoid this
-- completely, but in practice it is relatively easy to calibrate
-- the generator 'Constants' so that there is sufficient spending balance.

genTx :: forall c v.
  (HasCallStack, Mock c, CVNC c v) =>
  GenEnv c ->
  LedgerEnv ->
  (UTxOState c v, DPState c) ->
  Gen (Tx c v)
genTx
  ge@( GenEnv
         keySpace@( KeySpace_
                      { ksKeyPairs,
                        ksCoreNodes,
                        ksMSigScripts,
                        ksIndexedGenDelegates,
                        ksIndexedPaymentKeys,
                        ksIndexedStakingKeys,
                        ksIndexedPayScripts,
                        ksIndexedStakeScripts
                      }
                    )
         constants
       )
  (LedgerEnv slot txIx pparams reserves)
  (utxoSt@(UTxOState utxo _ _ _), dpState) =
    do
      keys' <- QC.shuffle ksKeyPairs
      scripts' <- QC.shuffle ksMSigScripts
      -------------------------------------------------------------------------
      -- Generate the building blocks of a TxBody
      -------------------------------------------------------------------------
      (inputs, spendingBalanceUtxo, (spendWits, spendScripts)) <-
        genInputs
          (minNumGenInputs constants, maxNumGenInputs constants)
          ksIndexedPaymentKeys
          ksIndexedPayScripts
          utxo
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
      let spendingBalance = vplus spendingBalanceUtxo (vinject $ (sum (snd <$> wdrls)) - deposits + refunds)
          n = if (Map.size . unUTxO) utxo < 100 then 3 else 0
      outputAddrs <-
        genRecipients (length inputs + n) keys' scripts'
          >>= genPtrAddrs (_dstate dpState')
      -------------------------------------------------------------------------
      -- Build a Draft Tx and repeatedly add to Delta until all fees are accounted for.
      -------------------------------------------------------------------------
      let draftFee = Coin 0
          (remainderCoin,draftOutputs) = calcOutputsFromBalance spendingBalance outputAddrs draftFee
      draftTxBody <- genTxBody inputs draftOutputs certs wdrls update draftFee ttl metadataHash
      let draftTx = Tx draftTxBody (mkTxWits' draftTxBody) metadata
      -- We add now repeatedly add inputs until the process converges.
      draftTx2 <- converge remainderCoin wits scripts keys' scripts' utxo pparams keySpace draftTx

      let fee = _txfee (_body draftTx2)
          utxoSize = (Map.size . unUTxO) utxo
          infrequent = hash(fromIntegral (unSlotNo slot) + fromIntegral (unCoin fee) + utxoSize)::Int
      -- infrequently we trace the final fee and the UTxO size to show progress, and to be sure the UtxO does not become empty.
      return $ if (infrequent `mod` 5000) == 0
                  then trace ("GenTx final fee: "++show(unCoin fee)++", UTxO size: "++ show(utxoSize)++".") draftTx2
                  else draftTx2

-- |- Collect additional inputs (and witnesses and keys and scripts) to make the transaction balance.
data Delta c v = Delta
  { dfees :: Coin,
    extraInputs :: Set.Set (TxIn c v),
    extraWitnesses :: WitnessSet c v,
    change :: TxOut c v,
    deltaVKeys :: [KeyPair 'Witness c],
    deltaScripts :: [(MultiSig c, MultiSig c)]
  }

-- |- We need this instance to know when delta has stopped growing. We don't actually need to compare all
-- the fields, because if the extraInputs has not changed then the Scripts and keys will not have changed.

instance (CV c v) => Eq (Delta c v) where
  a == b =
    dfees a == dfees b
      && extraInputs a == extraInputs b
      && extraWitnesses a == extraWitnesses b
      -- deltaVKeys and deltaScripts equality are implied by extraWitnesses equality, at least in the use case below.
      && change a == change b

deltaZero :: (Mock c, CVNC c v) => Coin -> Coin -> Addr c -> Delta c v
deltaZero initialfee minAda addr = Delta
   (-minAda + initialfee)
    mempty
    mempty
    (TxOut addr (vinject minAda))
    mempty
    mempty

-- |- Do the work of computing what additioanl inputs we need to 'fix-up' the transaction so that it will balance.

genNextDelta :: forall c v.
  (Mock c, CVNC c v) =>
  UTxO c v ->
  PParams ->
  KeySpace c ->
  Tx c v ->
  Delta c v ->
  Gen (Delta c v)
genNextDelta
  utxo
  pparams
  ( KeySpace_
      { ksIndexedStakingKeys,
        ksIndexedPaymentKeys,
        ksIndexedPayScripts
      }
    )
  tx
  delta@(Delta dfees extraInputs extraWitnesses change _ _) =
    let baseTxFee = minfee pparams tx
        encodedLen x = fromIntegral $ BSL.length (serialize x)
        -- based on the current contents of delta, how much will the fee increase when we add the delta to the tx?
        deltaFee =
          (fromIntegral $ _minfeeA pparams)
            * sum
              [ 5, -- safety net in case the coin or a list prefix rolls over into a larger encoding
                encodedLen dfees - 1,
                foldr (\a b -> b + encodedLen a) 0 extraInputs,
                encodedLen change,
                encodedLen extraWitnesses
              ]
        totalFee = baseTxFee + deltaFee :: Coin
        remainingFee = totalFee - dfees :: Coin
        changeAmount = getChangeAmount change
        minAda = _minUTxOValue pparams
     in if remainingFee <= 0 -- we've paid for all the fees
          then pure delta -- we're done
          else -- the change covers what we need, so shift Coin from change to dfees.
            if remainingFee <= changeAmount - minAda
              then
                pure $
                  delta
                    { dfees = totalFee,
                      change =
                        deltaChange
                          (`vminus` vinject remainingFee)
                          change
                    }
              else -- add a new input to cover the fee
              do
                let utxo' =   -- Remove possible inputs from Utxo, if they already appear in inputs.
                      UTxO $
                        Map.withoutKeys
                          (unUTxO utxo)
                          ((_inputs . _body) tx <> extraInputs)
                (inputs, value, (vkeyPairs, msigPairs)) <- genInputs (1, 1) ksIndexedPaymentKeys ksIndexedPayScripts utxo'
                -- It is possible that the Utxo has no possible inputs left, so fail. We try and keep this from happening
                -- by using feedback: adding to the number of ouputs (in the call to genRecipients) in genTx above. Adding to the
                -- outputs means in the next cycle the size of the UTxO will grow.
                _ <- if (null inputs) then (error "Not enough money in the world") else pure ()
                let newWits =
                      mkTxWits
                        ksIndexedPaymentKeys
                        ksIndexedStakingKeys
                        vkeyPairs
                        (mkScriptWits msigPairs mempty)
                        (hashAnnotated $ _body tx)
                pure $
                  delta
                    { extraWitnesses = extraWitnesses <> newWits,
                      extraInputs = extraInputs <> Set.fromList inputs,
                      change = deltaChange (vplus value) change,
                      deltaVKeys =  vkeyPairs <> deltaVKeys delta,
                      deltaScripts = msigPairs <> deltaScripts delta
                    }

    where
      deltaChange :: CV c v => (v -> v) -> TxOut c v -> TxOut c v
      deltaChange f (TxOut addr val) = TxOut addr $ f val
      getChangeAmount (TxOut _ v) = vcoin v


-- calculates fixed point of getNextDelta such that
-- reqFees (tx + delta) = dfees delta
-- start with zero delta
-- genNextDelta repeatedly until genNextDelta delta = delta

genNextDeltaTilFixPoint ::
  (Mock c, CVNC c v) =>
  Coin ->
  KeyPairs c ->
  MultiSigPairs c ->
  UTxO c v ->
  PParams ->
  KeySpace c ->
  Tx c v ->
  Gen (Delta c v)
genNextDeltaTilFixPoint initialfee randomKeys randomScripts utxo pparams keySpace tx = do
  addr <- genRecipients 1 randomKeys randomScripts
  fix
    (genNextDelta utxo pparams keySpace tx)
    (deltaZero initialfee (_minUTxOValue pparams) (head addr))



applyDelta :: (Mock c, CVNC c v) =>
 [KeyPair 'Witness c] ->
 Map (ScriptHash c) (MultiSig c) ->
 KeySpace c ->
 Tx c v -> Delta c v -> Tx c v
applyDelta
  neededKeys
  neededScripts
  (KeySpace_ {ksIndexedPaymentKeys, ksIndexedStakingKeys})
  tx@(Tx body@TxBody {_inputs, _outputs, _txfee} _wits _md)
  (Delta deltafees extraIn _extraWits change extraKeys extraScripts) =
    --fix up the witnesses here?
    -- Adds extraInputs, extraWitnesses, and change from delta to tx
    let outputs' = _outputs StrictSeq.|> change
        body' =
          body
            { _txfee = deltafees,
              _inputs = _inputs <> extraIn,
              _outputs = outputs'
            }
        kw = neededKeys <> extraKeys
        sw = neededScripts <> mkScriptWits extraScripts mempty
        newWitnessSet = mkTxWits ksIndexedPaymentKeys ksIndexedStakingKeys kw sw (hashAnnotated body')
     in tx {_body = body', _witnessSet = newWitnessSet}

fix :: (Eq d, Monad m) => (d -> m d) -> d -> m d
fix f d = do d1 <- f d; if d1 == d then pure d else fix f d1

converge ::
  (Mock c, CVNC c v) =>
  Coin ->
  [KeyPair 'Witness c] ->
  Map (ScriptHash c) (MultiSig c) ->
  KeyPairs c ->
  MultiSigPairs c ->
  UTxO c v ->
  PParams ->
  KeySpace c ->
  Tx c v ->
  Gen (Tx c v)
converge initialfee neededKeys neededScripts randomKeys randomScripts utxo pparams keySpace tx = do
  delta <- genNextDeltaTilFixPoint initialfee randomKeys randomScripts utxo pparams keySpace tx
  pure (applyDelta neededKeys neededScripts keySpace tx delta)

-- ======================================================

genTimeToLive :: SlotNo -> Gen SlotNo
genTimeToLive currentSlot = do
  ttl <- genNatural 50 100
  pure $ currentSlot + SlotNo (fromIntegral ttl)

mkScriptWits ::
  (Crypto c) =>
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
  (Mock c, CVNC c v) =>
  Map (KeyHash 'Payment c) (KeyPair 'Payment c) ->
  Map (KeyHash 'Staking c) (KeyPair 'Staking c) ->
  [KeyPair 'Witness c] ->
  Map (ScriptHash c) (MultiSig c) ->
  Hash c (TxBody c v) ->
  WitnessSet c v
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
  (CV c v) =>
  [TxIn c v] ->
  StrictSeq (TxOut c v) ->
  StrictSeq (DCert c) ->
  [(RewardAcnt c, Coin)] ->
  Maybe (Update c) ->
  Coin ->
  SlotNo ->
  StrictMaybe (MetaDataHash c) ->
  Gen (TxBody c v)
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
-- TODO need right splitting of v!
calcOutputsFromBalance ::
  (CV c v) =>
  v ->
  [Addr c] ->
  Coin ->
  (Coin, StrictSeq (TxOut c v))
calcOutputsFromBalance balance_ addrs fee =
  ( fee + splitCoinRem,
    StrictSeq.fromList $ zipWith TxOut addrs amountPerOutput
  )
  where
    -- split the available balance into equal portions (one for each address),
    -- if there is a remainder, then add it to the fee.
    balanceAfterFee = vminus balance_ (vinject fee)
    (amountPerOutput, splitCoinRem) = vsplit balanceAfterFee (fromIntegral $ length addrs)

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
  (HasCallStack, CV c v) =>
  (Int, Int) ->
  Map (KeyHash 'Payment c) (KeyPair 'Payment c) ->
  Map (ScriptHash c) (MultiSig c, MultiSig c) ->
  UTxO c v ->
  Gen ([TxIn c v], v, ([KeyPair 'Witness c], [(MultiSig c, MultiSig c)]))
genInputs (minNumGenInputs, maxNumGenInputs) keyHashMap payScriptMap (UTxO utxo) = do
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
  Map (Credential 'Staking c) Coin ->
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
  wdrls = do
    (a, b) <-
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
    pure ( a, b)
    where
      toRewardAcnt (rwd, coin) = (RewardAcnt Testnet rwd, coin)
      genWrdls wdrls_ = do
        selectedWrdls <- map toRewardAcnt <$> QC.sublistOf wdrls_
        let wits = (mkWdrlWits ksIndexedStakeScripts ksIndexedStakingKeys . getRwdCred . fst) <$> selectedWrdls
        return (selectedWrdls, Either.partitionEithers wits)

-- | Collect witnesses needed for reward withdrawals.
mkWdrlWits ::
  (HasCallStack, Crypto c) =>
  Map (ScriptHash c) (MultiSig c, MultiSig c) ->
  Map (KeyHash 'Staking c) (KeyPair 'Staking c) ->
  Credential 'Staking c ->
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
  (Crypto c) =>
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

genPtrAddrs :: DState h -> [Addr h] -> Gen [Addr h]
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
