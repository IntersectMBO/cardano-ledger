{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE UndecidableInstances #-}

module Test.Shelley.Spec.Ledger.Generator.Utxo
  ( genTx,
    Delta (..),
    showBalance,
  )
where

import Cardano.Binary (serialize)
import Cardano.Ledger.AuxiliaryData (hashAuxiliaryData)
import Cardano.Ledger.Coin (Coin (..))
import qualified Cardano.Ledger.Core as Core
import Cardano.Ledger.Era (Crypto, ValidateScript (..))
import Cardano.Ledger.Hashes (EraIndependentTxBody)
import Cardano.Ledger.SafeHash (SafeHash, hashAnnotated)
import Cardano.Ledger.Shelley.Constraints
  ( TransValue,
    UsesAuxiliary,
    UsesScript,
    UsesTxBody,
    UsesTxOut (..),
    UsesValue,
  )
import Cardano.Ledger.Val (Val (..), sumVal, (<+>), (<->), (<×>))
import Control.Monad (when)
import Control.SetAlgebra (forwards)
import Control.State.Transition
import qualified Data.ByteString.Lazy as BSL
import qualified Data.Either as Either (partitionEithers)
import Data.List (foldl', nub)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Proxy (Proxy (..))
import Data.Sequence.Strict (StrictSeq)
import qualified Data.Sequence.Strict as StrictSeq
import Data.Set (Set)
import qualified Data.Set as Set
import GHC.Records (HasField (..))
import GHC.Stack (HasCallStack)
import Shelley.Spec.Ledger.API
  ( DCert,
    ScriptHash,
  )
import Shelley.Spec.Ledger.Address
  ( Addr (..),
    RewardAcnt (..),
    toCred,
  )
import Shelley.Spec.Ledger.BaseTypes
  ( Network (..),
    maybeToStrictMaybe,
  )
import Shelley.Spec.Ledger.Credential (Credential (..), StakeReference (..))
import Shelley.Spec.Ledger.Keys
  ( KeyHash,
    KeyPair,
    KeyRole (..),
    asWitness,
  )
import Shelley.Spec.Ledger.LedgerState
  ( DPState (..),
    DState (..),
    KeyPairs,
    PState (..),
    UTxOState (..),
    consumed,
    minfee,
    produced,
    _dstate,
    _ptrs,
    _rewards,
  )
import Shelley.Spec.Ledger.PParams (PParams, PParams' (..))
import Shelley.Spec.Ledger.STS.Delpl (DelplEnv)
import Shelley.Spec.Ledger.STS.Ledger (LedgerEnv (..))
import Shelley.Spec.Ledger.Tx
  ( Tx (..),
    TxIn (..),
    WitnessSet,
    WitnessSetHKD (..),
  )
import Shelley.Spec.Ledger.TxBody (Wdrl (..))
import Shelley.Spec.Ledger.UTxO
  ( UTxO (..),
    balance,
    makeWitnessesFromScriptKeys,
    makeWitnessesVKey,
  )
import Test.QuickCheck (Gen, discard)
import qualified Test.QuickCheck as QC
import Test.Shelley.Spec.Ledger.ConcreteCryptoTypes
  ( Mock,
  )
import Test.Shelley.Spec.Ledger.Generator.Constants (Constants (..), defaultConstants)
import Test.Shelley.Spec.Ledger.Generator.Core
  ( GenEnv (..),
    KeySpace (..),
    findPayKeyPairAddr,
    findPayKeyPairCred,
    findPayScriptFromAddr,
    findStakeScriptFromCred,
  )
import Test.Shelley.Spec.Ledger.Generator.EraGen (EraGen (..))
import Test.Shelley.Spec.Ledger.Generator.ScriptClass (scriptKeyCombination)
import Test.Shelley.Spec.Ledger.Generator.Trace.DCert (CERTS, genDCerts)
import Test.Shelley.Spec.Ledger.Generator.Update (genUpdate)
import Test.Shelley.Spec.Ledger.Utils (ShelleyTest, Split (..))

-- =======================================================

showBalance ::
  forall era.
  ( ShelleyTest era,
    HasField "inputs" (Core.TxBody era) (Set (TxIn (Crypto era))),
    HasField "wdrls" (Core.TxBody era) (Wdrl (Crypto era)),
    HasField "certs" (Core.TxBody era) (StrictSeq (DCert (Crypto era)))
  ) =>
  LedgerEnv era ->
  UTxOState era ->
  DPState (Crypto era) ->
  Tx era ->
  String
showBalance
  (LedgerEnv _ _ pparams _)
  (UTxOState utxo _ _ _)
  (DPState _ (PState stakepools _ _))
  (Tx txBody _ _) =
    "\n\nConsumed: " ++ show (consumed pparams utxo txBody)
      ++ "  Produced: "
      ++ show (produced @era pparams stakepools txBody)

--  ========================================================================

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

-- Note: the spending balance emerges from inputs, refund withdrawals,
-- certificate deposits and fees (which in turn depend on number of
-- inputs, outputs, witnesses, metadata etc.). It's hard to avoid this
-- completely, but in practice it is relatively easy to calibrate
-- the generator 'Constants' so that there is sufficient spending balance.

genTx ::
  forall era.
  ( HasCallStack,
    ShelleyTest era,
    EraGen era,
    Mock (Crypto era),
    Embed (Core.EraRule "DELPL" era) (CERTS era),
    Environment (Core.EraRule "DELPL" era) ~ DelplEnv era,
    State (Core.EraRule "DELPL" era) ~ DPState (Crypto era),
    Signal (Core.EraRule "DELPL" era) ~ DCert (Crypto era),
    HasField "inputs" (Core.TxBody era) (Set (TxIn (Crypto era)))
  ) =>
  GenEnv era ->
  LedgerEnv era ->
  (UTxOState era, DPState (Crypto era)) ->
  Gen (Tx era)
genTx
  ge@( GenEnv
         keySpace@KeySpace_
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
  (LedgerEnv slot txIx pparams reserves)
  (utxoSt@(UTxOState utxo _ _ _), dpState) =
    do
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
        genWithdrawals
          @era
          constants
          ksIndexedStakeScripts
          ksIndexedStakingKeys
          ((_rewards . _dstate) dpState)
      (update, updateWits) <-
        genUpdate
          constants
          slot
          ksCoreNodes
          ksIndexedGenDelegates
          pparams
          (utxoSt, dpState)
      (certs, deposits, refunds, dpState', (certWits, certScripts)) <-
        genDCerts ge pparams dpState slot txIx reserves
      metadata <- genEraAuxiliaryData @era constants
      -------------------------------------------------------------------------
      -- Gather Key Witnesses and Scripts, prepare a constructor for Tx Wits
      -------------------------------------------------------------------------
      let txWits = spendWits ++ wdrlWits ++ certWits ++ updateWits
          scripts = mkScriptWits @era spendScripts (certScripts ++ wdrlScripts)
          mkTxWits' =
            mkTxWits @era ksIndexedPaymentKeys ksIndexedStakingKeys txWits scripts
              . hashAnnotated
      -------------------------------------------------------------------------
      -- SpendingBalance, Output Addresses (including some Pointer addresses)
      -- and a Outputs builder that distributes the given balance over
      -- addresses.
      -------------------------------------------------------------------------
      let withdrawals = sumVal (snd <$> wdrls)
          !spendingBalance =
            spendingBalanceUtxo
              <+> inject ((withdrawals <-> deposits) <+> refunds)
          n =
            if (Map.size . unUTxO) utxo < genTxStableUtxoSize defaultConstants -- something moderate 80-120
              then genTxUtxoIncrement defaultConstants -- something small 2-5
              else 0 -- no change at all
              -- This algorithm has an instability in that if we don't balance
              -- genTxStableUtxoSize and genTxUtxoIncrement correctly the size
              -- of the UTxO gradually shrinks so small we cannot support
              -- generating a transaction. If we get unexplained failures one
              -- might investigate changing these constants.

      -- Occasionally we have a transaction generated with insufficient inputs
      -- to cover the deposits. In this case we discard the test case.
      !_ <- when (coin spendingBalance <= Coin 0) discard
      outputAddrs <-
        genRecipients @era (length inputs + n) ksKeyPairs ksMSigScripts
          >>= genPtrAddrs (_dstate dpState')
      -------------------------------------------------------------------------
      -- Build a Draft Tx and repeatedly add to Delta until all fees are
      -- accounted for.
      -------------------------------------------------------------------------
      let draftFee = Coin 0
          (remainderCoin, draftOutputs) =
            calcOutputsFromBalance @era
              spendingBalance
              outputAddrs
              draftFee
      (draftTxBody, additionalScripts) <-
        genEraTxBody
          ge
          pparams
          slot
          (Set.fromList inputs)
          draftOutputs
          certs
          (Wdrl (Map.fromList wdrls))
          draftFee
          (maybeToStrictMaybe update)
          (hashAuxiliaryData @era <$> metadata)
      let draftTx = Tx draftTxBody (mkTxWits' draftTxBody) metadata
          scripts' = Map.fromList $ map (\s -> (hashScript @era s, s)) additionalScripts
      -- We add now repeatedly add inputs until the process converges.
      converge
        remainderCoin
        txWits
        (scripts `Map.union` scripts')
        ksKeyPairs
        ksMSigScripts
        utxo
        pparams
        keySpace
        draftTx

-- | - Collect additional inputs (and witnesses and keys and scripts) to make
-- the transaction balance.
data Delta era = Delta
  { dfees :: Coin,
    extraInputs :: Set.Set (TxIn (Crypto era)),
    extraWitnesses :: WitnessSet era,
    change :: Core.TxOut era,
    deltaVKeys :: [KeyPair 'Witness (Crypto era)],
    deltaScripts :: [(Core.Script era, Core.Script era)]
  }

-- | - We need this instance to know when delta has stopped growing. We don't
--  actually need to compare all the fields, because if the extraInputs has not
--  changed then the Scripts and keys will not have changed.
instance (UsesTxOut era, UsesScript era, TransValue Eq era) => Eq (Delta era) where
  a == b =
    dfees a == dfees b
      && extraInputs a == extraInputs b
      && extraWitnesses a == extraWitnesses b
      -- deltaVKeys and deltaScripts equality are implied by extraWitnesses
      -- equality, at least in the use case below.
      && change a == change b

deltaZero ::
  forall era.
  ( UsesScript era,
    UsesTxOut era
  ) =>
  Coin ->
  Coin ->
  Addr (Crypto era) ->
  Delta era
deltaZero initialfee minAda addr =
  Delta
    (initialfee <-> minAda)
    mempty
    mempty
    (makeTxOut (Proxy @era) addr (inject minAda))
    mempty
    mempty

-- | - Do the work of computing what additioanl inputs we need to 'fix-up' the
-- transaction so that it will balance.
genNextDelta ::
  forall era.
  ( EraGen era,
    UsesTxOut era,
    Mock (Crypto era),
    HasField "inputs" (Core.TxBody era) (Set (TxIn (Crypto era)))
  ) =>
  UTxO era ->
  PParams era ->
  KeySpace era ->
  Tx era ->
  Delta era ->
  Gen (Delta era)
genNextDelta
  utxo
  pparams
  KeySpace_
    { ksIndexedStakingKeys,
      ksIndexedPaymentKeys,
      ksIndexedPayScripts
    }
  tx
  delta@(Delta dfees extraInputs extraWitnesses change _ _) =
    let baseTxFee = minfee pparams tx
        encodedLen x = fromIntegral $ BSL.length (serialize x)
        -- based on the current contents of delta, how much will the fee
        -- increase when we add the delta to the tx?
        draftSize =
          sum
            [ 5 :: Integer, -- safety net in case the coin or a list prefix rolls over into a larger encoding
              12 :: Integer, -- TODO the size calculation somehow needs extra buffer when minting tokens
              encodedLen (max dfees (Coin 0)) - 1,
              foldr (\a b -> b + encodedLen a) 0 extraInputs,
              encodedLen change,
              encodedLen extraWitnesses
            ]

        deltaFee = draftSize <×> Coin (fromIntegral (_minfeeA pparams))
        totalFee = baseTxFee <+> deltaFee :: Coin
        remainingFee = totalFee <-> dfees :: Coin
        changeAmount = getChangeAmount change
        minAda = _minUTxOValue pparams
     in if remainingFee <= Coin 0 -- we've paid for all the fees
          then pure delta -- we're done
          else -- the change covers what we need, so shift Coin from change to dfees.

            if remainingFee <= (changeAmount <-> minAda)
              then
                pure $
                  delta
                    { dfees = totalFee,
                      change =
                        deltaChange
                          (<-> inject remainingFee)
                          change
                    }
              else -- add a new input to cover the fee
              do
                let txBody = getField @"body" tx
                    utxo' :: UTxO era
                    utxo' =
                      -- Remove possible inputs from Utxo, if they already
                      -- appear in inputs.
                      UTxO $
                        Map.withoutKeys
                          (unUTxO utxo)
                          (getField @"inputs" txBody <> extraInputs)
                (inputs, value, (vkeyPairs, msigPairs)) <-
                  genInputs (1, 1) ksIndexedPaymentKeys ksIndexedPayScripts utxo'
                -- It is possible that the Utxo has no possible inputs left, so
                -- fail. We try and keep this from happening by using feedback:
                -- adding to the number of ouputs (in the call to genRecipients)
                -- in genTx above. Adding to the outputs means in the next cycle
                -- the size of the UTxO will grow.
                _ <-
                  if null inputs
                    then error "Not enough money in the world"
                    else pure ()
                let newWits =
                      mkTxWits
                        ksIndexedPaymentKeys
                        ksIndexedStakingKeys
                        vkeyPairs
                        (mkScriptWits @era msigPairs mempty)
                        (hashAnnotated txBody)
                pure $
                  delta
                    { extraWitnesses = extraWitnesses <> newWits,
                      extraInputs = extraInputs <> Set.fromList inputs,
                      change = deltaChange (<+> value) change, -- <+> is plus of the Val class
                      deltaVKeys = vkeyPairs <> deltaVKeys delta,
                      deltaScripts = msigPairs <> deltaScripts delta
                    }
    where
      deltaChange ::
        (Core.Value era -> Core.Value era) ->
        Core.TxOut era ->
        Core.TxOut era
      deltaChange f out =
        makeTxOut (Proxy @era) (getField @"address" out) (f $ getField @"value" out)
      getChangeAmount out = coin $ getField @"value" out

-- calculates fixed point of getNextDelta such that
-- reqFees (tx + delta) = dfees delta
-- start with zero delta
-- genNextDelta repeatedly until genNextDelta delta = delta

genNextDeltaTilFixPoint ::
  forall era.
  ( EraGen era,
    UsesTxOut era,
    Mock (Crypto era),
    HasField "inputs" (Core.TxBody era) (Set (TxIn (Crypto era)))
  ) =>
  Coin ->
  KeyPairs (Crypto era) ->
  [(Core.Script era, Core.Script era)] ->
  UTxO era ->
  PParams era ->
  KeySpace era ->
  Tx era ->
  Gen (Delta era)
genNextDeltaTilFixPoint initialfee keys scripts utxo pparams keySpace tx = do
  addr <- genRecipients @era 1 keys scripts
  fix
    (genNextDelta utxo pparams keySpace tx)
    (deltaZero initialfee (safetyOffset <+> _minUTxOValue pparams) (head addr))
  where
    -- add a small offset here to ensure outputs above minUtxo value
    safetyOffset = Coin 5

applyDelta ::
  forall era.
  ( EraGen era,
    UsesTxBody era,
    UsesAuxiliary era,
    Mock (Crypto era),
    HasField "inputs" (Core.TxBody era) (Set (TxIn (Crypto era))),
    Core.Witnesses era ~ WitnessSet era
  ) =>
  [KeyPair 'Witness (Crypto era)] ->
  Map (ScriptHash (Crypto era)) (Core.Script era) ->
  KeySpace era ->
  Tx era ->
  Delta era ->
  Tx era
applyDelta
  neededKeys
  neededScripts
  KeySpace_ {ksIndexedPaymentKeys, ksIndexedStakingKeys}
  tx
  (Delta deltafees extraIn _extraWits change extraKeys extraScripts) =
    --fix up the witnesses here?
    -- Adds extraInputs, extraWitnesses, and change from delta to tx
    let txBody = getField @"body" tx
        outputs' = getField @"outputs" txBody StrictSeq.|> change
        body2 =
          (updateEraTxBody @era)
            txBody
            deltafees
            (getField @"inputs" txBody <> extraIn)
            outputs'
        kw = neededKeys <> extraKeys
        sw = neededScripts <> mkScriptWits @era extraScripts mempty
        newWitnessSet =
          mkTxWits @era
            ksIndexedPaymentKeys
            ksIndexedStakingKeys
            kw
            sw
            (hashAnnotated body2)
     in tx {body = body2, wits = newWitnessSet}

fix :: (Eq d, Monad m) => (d -> m d) -> d -> m d
fix f d = do d1 <- f d; if d1 == d then pure d else fix f d1

converge ::
  ( EraGen era,
    UsesTxBody era,
    UsesTxOut era,
    UsesAuxiliary era,
    Mock (Crypto era),
    HasField "inputs" (Core.TxBody era) (Set (TxIn (Crypto era))),
    Core.Witnesses era ~ WitnessSet era
  ) =>
  Coin ->
  [KeyPair 'Witness (Crypto era)] ->
  Map (ScriptHash (Crypto era)) (Core.Script era) ->
  KeyPairs (Crypto era) ->
  [(Core.Script era, Core.Script era)] ->
  UTxO era ->
  PParams era ->
  KeySpace era ->
  Tx era ->
  Gen (Tx era)
converge
  initialfee
  neededKeys
  neededScripts
  keys
  scripts
  utxo
  pparams
  keySpace
  tx = do
    delta <- genNextDeltaTilFixPoint initialfee keys scripts utxo pparams keySpace tx
    pure (applyDelta neededKeys neededScripts keySpace tx delta)

-- | Return up to /k/ random elements from /items/
-- (instead of the less efficient /take k <$> QC.shuffle items/)
ruffle :: Int -> [a] -> Gen [a]
ruffle _ [] = pure []
ruffle k items = do
  indices <- nub <$> QC.vectorOf k pickIndex
  pure $ map (items !!) indices
  where
    pickIndex = QC.choose (0, length items - 1)

mkScriptWits ::
  forall era.
  EraGen era =>
  [(Core.Script era, Core.Script era)] ->
  [(Core.Script era, Core.Script era)] ->
  Map (ScriptHash (Crypto era)) (Core.Script era)
mkScriptWits payScripts stakeScripts =
  Map.fromList $
    (hashPayScript <$> payScripts)
      ++ (hashStakeScript <$> stakeScripts)
  where
    hashPayScript ::
      (Core.Script era, Core.Script era) ->
      (ScriptHash (Crypto era), Core.Script era)
    hashPayScript (payScript, _) =
      (hashScript @era payScript, payScript)

    hashStakeScript ::
      (Core.Script era, Core.Script era) ->
      (ScriptHash (Crypto era), Core.Script era)
    hashStakeScript (_, sScript) =
      (hashScript @era sScript, sScript)

mkTxWits ::
  forall era.
  ( EraGen era,
    Mock (Crypto era)
  ) =>
  Map (KeyHash 'Payment (Crypto era)) (KeyPair 'Payment (Crypto era)) ->
  Map (KeyHash 'Staking (Crypto era)) (KeyPair 'Staking (Crypto era)) ->
  [KeyPair 'Witness (Crypto era)] ->
  Map (ScriptHash (Crypto era)) (Core.Script era) ->
  SafeHash (Crypto era) EraIndependentTxBody ->
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
        scriptWits = msigs,
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
      keysLists = map (scriptKeyCombination (Proxy @era)) (Map.elems msigs)
      msigSignatures = foldl' Set.union Set.empty $ map Set.fromList keysLists

-- | Distribute the sum of `balance_` and `fee` over the addresses, return the
-- sum of `fee` and the remainder of the equal distribution and the list ouf
-- transaction outputs that cover the balance and fees.
--
-- The idea is to have an specified spending balance and fees that must be paid
-- by the selected addresses.
-- TODO need right splitting of v!
calcOutputsFromBalance ::
  forall era.
  ( UsesValue era,
    UsesTxOut era,
    Split (Core.Value era)
  ) =>
  Core.Value era ->
  [Addr (Crypto era)] ->
  Coin ->
  (Coin, StrictSeq (Core.TxOut era))
calcOutputsFromBalance balance_ addrs fee =
  ( fee <+> splitCoinRem,
    StrictSeq.fromList $ zipWith (makeTxOut $ Proxy @era) addrs amountPerOutput
  )
  where
    -- split the available balance into equal portions (one for each address),
    -- if there is a remainder, then add it to the fee.
    balanceAfterFee = balance_ <-> inject fee
    (amountPerOutput, splitCoinRem) =
      vsplit balanceAfterFee (fromIntegral $ length addrs)

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
  forall era.
  (UsesTxOut era) =>
  (Int, Int) ->
  Map (KeyHash 'Payment (Crypto era)) (KeyPair 'Payment (Crypto era)) ->
  Map (ScriptHash (Crypto era)) (Core.Script era, Core.Script era) ->
  UTxO era ->
  Gen
    ( [TxIn (Crypto era)],
      Core.Value era,
      ([KeyPair 'Witness (Crypto era)], [(Core.Script era, Core.Script era)])
    )
genInputs (minNumGenInputs, maxNumGenInputs) keyHashMap payScriptMap (UTxO utxo) = do
  numInputs <- QC.choose (minNumGenInputs, maxNumGenInputs)
  selectedUtxo <- ruffle numInputs (Map.toList utxo)

  let (inputs, witnesses) = unzip (witnessedInput <$> selectedUtxo)
  return
    ( inputs,
      balance (UTxO (Map.fromList selectedUtxo) :: UTxO era),
      Either.partitionEithers witnesses
    )
  where
    witnessedInput (input, output) =
      case getField @"address" output of
        addr@(Addr _ (KeyHashObj _) _) ->
          (input, Left . asWitness $ findPayKeyPairAddr @era addr keyHashMap)
        addr@(Addr _ (ScriptHashObj _) _) ->
          (input, Right $ findPayScriptFromAddr @era addr payScriptMap)
        _ -> error "unsupported address"

-- | Select a subset of the reward accounts to use for reward withdrawals.
genWithdrawals ::
  forall era.
  Constants ->
  Map (ScriptHash (Crypto era)) (Core.Script era, Core.Script era) ->
  Map (KeyHash 'Staking (Crypto era)) (KeyPair 'Staking (Crypto era)) ->
  Map (Credential 'Staking (Crypto era)) Coin ->
  Gen
    ( [(RewardAcnt (Crypto era), Coin)],
      ([KeyPair 'Witness (Crypto era)], [(Core.Script era, Core.Script era)])
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
    pure (a, b)
    where
      toRewardAcnt (rwd, coinx) = (RewardAcnt Testnet rwd, coinx)
      genWrdls wdrls_ = do
        selectedWrdls <- map toRewardAcnt <$> QC.sublistOf wdrls_
        let txwits =
              mkWdrlWits @era ksIndexedStakeScripts ksIndexedStakingKeys
                . getRwdCred
                . fst
                <$> selectedWrdls
        return (selectedWrdls, Either.partitionEithers txwits)

-- | Collect witnesses needed for reward withdrawals.
mkWdrlWits ::
  forall era.
  Map (ScriptHash (Crypto era)) (Core.Script era, Core.Script era) ->
  Map (KeyHash 'Staking (Crypto era)) (KeyPair 'Staking (Crypto era)) ->
  Credential 'Staking (Crypto era) ->
  Either (KeyPair 'Witness (Crypto era)) (Core.Script era, Core.Script era)
mkWdrlWits scriptsByStakeHash _ c@(ScriptHashObj _) =
  Right $
    findStakeScriptFromCred @era (asWitness c) scriptsByStakeHash
mkWdrlWits _ keyHashMap c@(KeyHashObj _) =
  Left $
    asWitness $
      findPayKeyPairCred @era c keyHashMap

-- | Select recipient addresses that will serve as output targets for a new
-- transaction.
genRecipients ::
  forall era.
  EraGen era =>
  Int ->
  KeyPairs (Crypto era) ->
  [(Core.Script era, Core.Script era)] ->
  Gen [Addr (Crypto era)]
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
  let n = n' - m
  recipientKeys <- ruffle n keys
  recipientScripts <- ruffle m scripts

  let payKeys = (toCred . fst) <$> recipientKeys
      stakeKeys = (toCred . snd) <$> recipientKeys
      payScripts = (scriptToCred' . fst) <$> recipientScripts
      stakeScripts = (scriptToCred' . snd) <$> recipientScripts

  -- zip keys and scripts together as base addresses
  let payCreds = payKeys ++ payScripts
      stakeCreds = stakeKeys ++ stakeScripts
  let stakeCreds' = fmap StakeRefBase stakeCreds

  return (zipWith (Addr Testnet) payCreds stakeCreds')
  where
    scriptToCred' :: Core.Script era -> Credential kr (Crypto era)
    scriptToCred' = ScriptHashObj . hashScript @era

genPtrAddrs :: DState crypto -> [Addr crypto] -> Gen [Addr crypto]
genPtrAddrs ds addrs = do
  let pointers = forwards (_ptrs ds)

  n <- QC.choose (0, min (Map.size pointers) (length addrs))
  pointerList <- ruffle n (Map.keys pointers)

  let addrs' = zipWith baseAddrToPtrAddr (take n addrs) pointerList

  pure (addrs' ++ drop n addrs)
  where
    baseAddrToPtrAddr a p = case a of
      Addr n pay _ -> Addr n pay (StakeRefPtr p)
      _ -> a
