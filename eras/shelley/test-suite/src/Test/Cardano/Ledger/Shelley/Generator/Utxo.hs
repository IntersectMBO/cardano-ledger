{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module Test.Cardano.Ledger.Shelley.Generator.Utxo (
  genTx,
  Delta (..),
  encodedLen,
  pickRandomFromMap,
)
where

import Cardano.Ledger.Address (
  Addr (..),
  RewardAccount (..),
 )
import Cardano.Ledger.BaseTypes (
  Network (..),
  inject,
  maybeToStrictMaybe,
 )
import Cardano.Ledger.Binary (EncCBOR, serialize)
import Cardano.Ledger.CertState (EraCertState (..))
import Cardano.Ledger.Coin (Coin (..))
import Cardano.Ledger.Core
import Cardano.Ledger.Credential (Credential (..), StakeReference (..))
import Cardano.Ledger.Keys (asWitness)
import Cardano.Ledger.Shelley.LedgerState (
  DState (..),
  LedgerState (..),
  UTxOState (..),
  ptrsMap,
  rewards,
 )
import Cardano.Ledger.Shelley.Rules (DelplEnv, LedgerEnv (..))
import Cardano.Ledger.Shelley.TxBody (Withdrawals (..))
import Cardano.Ledger.State (
  EraUTxO,
  UTxO (..),
  getMinFeeTxUtxo,
  sumAllValue,
 )
import Cardano.Ledger.TxIn (TxIn (..))
import qualified Cardano.Ledger.UMap as UM
import Cardano.Ledger.Val (Val (..), sumVal, (<+>), (<->), (<×>))
import Control.Monad (when)
import Control.State.Transition
import qualified Data.ByteString.Lazy as BSL
import qualified Data.Either as Either (partitionEithers)
import Data.Foldable as F (foldl')
import qualified Data.IntSet as IntSet
import Data.List.NonEmpty (nonEmpty)
import qualified Data.List.NonEmpty as NE
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Proxy (Proxy (..))
import Data.Sequence.Strict (StrictSeq)
import qualified Data.Sequence.Strict as StrictSeq
import qualified Data.Set as Set
import qualified Data.Vector as V
import Lens.Micro
import NoThunks.Class ()
import Test.Cardano.Ledger.Binary.Random (QC (..))
import Test.Cardano.Ledger.Common (tracedDiscard)
import Test.Cardano.Ledger.Core.Arbitrary (uniformSubMapElems)
import Test.Cardano.Ledger.Core.KeyPair (
  KeyPair,
  KeyPairs,
  makeWitnessesFromScriptKeys,
  mkAddr,
  mkCredential,
  mkWitnessesVKey,
 )
import Test.Cardano.Ledger.Shelley.Constants (Constants (..), defaultConstants)
import Test.Cardano.Ledger.Shelley.Generator.Core (
  GenEnv (..),
  KeySpace (..),
  ScriptInfo,
  ScriptSpace (..),
  findPayKeyPairAddr,
  findPayKeyPairCred,
  findPayScriptFromAddr,
  findStakeScriptFromCred,
 )
import Test.Cardano.Ledger.Shelley.Generator.EraGen (EraGen (..))
import Test.Cardano.Ledger.Shelley.Generator.ScriptClass (scriptKeyCombination)
import Test.Cardano.Ledger.Shelley.Generator.Trace.TxCert (CERTS, genTxCerts)
import Test.Cardano.Ledger.Shelley.Generator.Update (genUpdate)
import Test.Cardano.Ledger.Shelley.Utils (Split (..))
import Test.QuickCheck (Gen)
import qualified Test.QuickCheck as QC

-- Instances only

-- ====================================================

-- | Generates a transaction in the context of the LEDGER STS environment
-- and state.
--
--  A generated transaction may not have sufficient spending balance and
-- need to be discarded. In that case we try to compute a Delta, that when
-- added (applyDelta) to the transaction, repairs it. The repair is made
-- by adding additional inputs from which more Ada can flow into the fee.
-- If that doesn't fix it, we add more inputs to the Delta.
-- Experience shows that this converges quite quickly (in traces we never saw
-- more than 3 iterations).

-- Note: the spending balance emerges from inputs, refund withdrawals,
-- certificate deposits and fees (which in turn depend on number of
-- inputs, outputs, witnesses, metadata etc.). It's hard to avoid this
-- completely, but in practice it is relatively easy to calibrate
-- the generator 'Constants' so that there is sufficient spending balance.

genTx ::
  forall era.
  ( EraGen era
  , EraUTxO era
  , Embed (EraRule "DELPL" era) (CERTS era)
  , Environment (EraRule "DELPL" era) ~ DelplEnv era
  , State (EraRule "DELPL" era) ~ CertState era
  , Signal (EraRule "DELPL" era) ~ TxCert era
  , EraCertState era
  ) =>
  GenEnv era ->
  LedgerEnv era ->
  LedgerState era ->
  Gen (Tx era)
genTx
  ge@( GenEnv
        keySpace@KeySpace_
          { ksKeyPairs
          , ksCoreNodes
          , ksMSigScripts
          , ksIndexedGenDelegates
          , ksIndexedPaymentKeys
          , ksIndexedStakingKeys
          , ksIndexedPayScripts
          , ksIndexedStakeScripts
          }
        scriptspace
        constants
      )
  (LedgerEnv slot _ txIx pparams reserves)
  (LedgerState utxoSt@(UTxOState utxo _ _ _ _ _) dpState) =
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
          ((Map.map (UM.fromCompact . UM.rdReward) . UM.unUnify . rewards) $ dpState ^. certDStateL)
      (update, updateWits) <-
        genUpdate
          constants
          slot
          ksCoreNodes
          ksIndexedGenDelegates
          pparams
          (utxoSt, dpState)
      (certs, deposits, refunds, dpState', certWits, certScripts) <-
        genTxCerts ge pparams dpState slot txIx reserves
      metadata <- genEraAuxiliaryData @era constants
      -------------------------------------------------------------------------
      -- Gather Key TxWits and Scripts, prepare a constructor for Tx Wits
      -------------------------------------------------------------------------
      let txWits = spendWits ++ wdrlWits ++ certWits ++ updateWits
          scripts = mkScriptWits @era spendScripts (certScripts ++ wdrlScripts)
          mkTxWits' txbody =
            mkTxWits @era
              (utxo, txbody, (ssHash3 scriptspace, ssHash2 scriptspace))
              ksIndexedPaymentKeys
              ksIndexedStakingKeys
              txWits
              scripts
              (hashAnnotated txbody)
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
            if Map.size (unUTxO utxo) < genTxStableUtxoSize defaultConstants
              then -- something moderate 80-120 ^
                genTxUtxoIncrement defaultConstants -- something small 2-5
              else 0 -- no change at all
              -- This algorithm has an instability in that if we don't balance
              -- genTxStableUtxoSize and genTxUtxoIncrement correctly the size
              -- of the UTxO gradually shrinks so small we cannot support
              -- generating a transaction. If we get unexplained failures one
              -- might investigate changing these constants.

      -- !_ = occaisionally (length inputs * length ksKeyPairs * length ksMSigScripts) 10000 ("UTxOSize = "++show (Map.size (unUTxO utxo)))

      outputAddrs <-
        genRecipients @era (length inputs + n) ksKeyPairs ksMSigScripts
          >>= genPtrAddrs (dpState' ^. certDStateL)

      !_ <-
        when (coin spendingBalance < mempty) $
          tracedDiscard $
            "Negative spending balance " <> show (coin spendingBalance)

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

      -- Occasionally we have a transaction generated with insufficient inputs
      -- to cover the deposits. In this case we discard the test case.
      let enough = sumVal (getMinCoinTxOut pparams <$> draftOutputs)
      !_ <-
        when (coin spendingBalance < enough) $
          tracedDiscard $
            "No inputs left. Utxo.hs " <> show enough

      (draftTxBody, additionalScripts) <-
        genEraTxBody
          ge
          utxo
          pparams
          slot
          (Set.fromList inputs)
          draftOutputs
          (StrictSeq.fromList certs)
          (Withdrawals (Map.fromList wdrls))
          draftFee
          (maybeToStrictMaybe update)
          (hashTxAuxData @era <$> metadata)
      let draftTx =
            constructTx @era
              draftTxBody
              (mkTxWits' draftTxBody)
              metadata
          scripts' = Map.fromList $ map (\s -> (hashScript @era s, s)) additionalScripts
      -- We add now repeatedly add inputs until the process converges.
      tx <-
        converge
          (ssHash3 scriptspace, ssHash2 scriptspace)
          remainderCoin
          txWits
          (scripts `Map.union` scripts')
          ksKeyPairs
          ksMSigScripts
          utxo
          pparams
          keySpace
          draftTx
      let txOuts = tx ^. bodyTxL . outputsTxBodyL
      !_ <-
        when (any (\txOut -> getMinCoinTxOut pparams txOut > txOut ^. coinTxOutL) txOuts) $
          tracedDiscard $
            "TxOut value is too small " <> show txOuts
      pure tx

-- | Collect additional inputs (and witnesses and keys and scripts) to make
-- the transaction balance.
data Delta era = Delta
  { dfees :: Coin
  , extraInputs :: Set.Set TxIn
  , extraWitnesses :: TxWits era
  , change :: TxOut era
  , deltaVKeys :: [KeyPair 'Witness]
  , deltaScripts :: [(Script era, Script era)]
  }

instance Show (Delta era) where
  show (Delta fee is _wit _change dvs ds) =
    "(Delta"
      ++ show fee
      ++ " "
      ++ show (Set.size is)
      ++ " wit change "
      ++ show (length dvs)
      ++ " "
      ++ show (length ds)
      ++ ")"

-- | - We need this instance to know when delta has stopped growing. We don't
--  actually need to compare all the fields, because if the extraInputs has not
--  changed then the Scripts and keys will not have changed.
instance
  ( EraTxOut era
  , Eq (TxWits era)
  ) =>
  Eq (Delta era)
  where
  a == b =
    dfees a == dfees b
      && extraInputs a == extraInputs b
      && extraWitnesses a == extraWitnesses b
      -- deltaVKeys and deltaScripts equality are implied by extraWitnesses
      -- equality, at least in the use case below.
      && change a == change b

deltaZero ::
  forall era.
  ( EraTxOut era
  , Monoid (TxWits era)
  ) =>
  Coin ->
  PParams era ->
  Addr ->
  Delta era
deltaZero initialfee pp addr =
  Delta
    (initialfee <-> txOut ^. coinTxOutL)
    mempty
    mempty
    txOut
    mempty
    mempty
  where
    txOut = setMinCoinTxOut pp (mkBasicTxOut addr mempty)

-- Same function as in cardano-ledger-api. We don't want to depend on the api though,
-- because it will be problematic for dependencies (cardano-ledger-api test suite depends
-- on this package)
setMinCoinTxOut :: EraTxOut era => PParams era -> TxOut era -> TxOut era
setMinCoinTxOut pp = go
  where
    go txOut =
      let curMinCoin = getMinCoinTxOut pp txOut
          curCoin = txOut ^. coinTxOutL
       in if curCoin == curMinCoin
            then txOut
            else go (txOut & coinTxOutL .~ curMinCoin)

encodedLen :: forall era t. (Era era, EncCBOR t) => t -> Integer
encodedLen x = fromIntegral $ BSL.length (serialize (eraProtVerHigh @era) x)

-- | Do the work of computing what additioanl inputs we need to 'fix-up' the transaction
-- so that it will balance.
genNextDelta ::
  forall era.
  (EraGen era, EraUTxO era) =>
  ScriptInfo era ->
  UTxO era ->
  PParams era ->
  KeySpace era ->
  Tx era ->
  Int ->
  Delta era ->
  Gen (Delta era)
genNextDelta
  scriptinfo
  utxo
  pparams
  KeySpace_
    { ksIndexedStakingKeys
    , ksIndexedPaymentKeys
    , ksIndexedPayScripts
    }
  tx
  _count -- the counter of the fix loop
  delta@(Delta dfees extraInputs extraWitnesses change _ extraScripts) =
    let !baseTxFee = getMinFeeTxUtxo pparams tx utxo
        -- based on the current contents of delta, how much will the fee
        -- increase when we add the delta to the tx?
        draftSize =
          sum
            [ 11000 :: Integer -- safety net in case the coin or a list prefix rolls over into a
            -- larger encoding, or some other fudge factor occurs. Sometimes we need extra buffer
            -- when minting tokens. 1100 has been empirically determined to make non-failing Txs
            , encodedLen @era (max dfees (Coin 0)) - 1
            , (foldr (\a b -> b + encodedLen @era a) 0 extraInputs) * 2
            , --  inputs end up in collateral as well, so we ^ multiply by 2
              encodedLen @era change
            , encodedLen @era extraWitnesses
            ]
        deltaScriptCost = foldr accum (Coin 0) extraScripts
          where
            accum (s1, _) ans = genEraScriptCost @era pparams s1 <+> ans
        deltaFee = draftSize <×> pparams ^. ppMinFeeAL <+> deltaScriptCost
        totalFee = baseTxFee <+> deltaFee :: Coin
        remainingFee = totalFee <-> dfees :: Coin
        changeAmount = getChangeAmount change
        minAda = getMinCoinTxOut pparams change
     in if remainingFee <= Coin 0 -- we've paid for all the fees
          then pure delta -- we're done
          else -- the change covers what we need, so shift Coin from change to dfees.
            if remainingFee <= (changeAmount <-> minAda)
              then
                pure $
                  delta
                    { dfees = totalFee
                    , change =
                        deltaChange
                          (<-> inject remainingFee)
                          change
                    }
              else -- add a new input to cover the fee
                do
                  let txBody = tx ^. bodyTxL
                      inputsInUse = txBody ^. inputsTxBodyL <> extraInputs
                      utxo' :: UTxO era
                      utxo' =
                        -- Remove possible inputs from Utxo, if they already
                        -- appear in inputs.
                        UTxO $
                          Map.filterWithKey
                            ( \k v ->
                                (k `Set.notMember` inputsInUse) && genEraGoodTxOut v
                            )
                            -- filter out UTxO entries where the TxOut are not
                            -- appropriate for this Era (i.e. Keylocked in
                            -- AlonzoEra)
                            (unUTxO utxo)
                  (inputs, value, (vkeyPairs, msigPairs)) <-
                    genInputs (1, 1) ksIndexedPaymentKeys ksIndexedPayScripts utxo'
                  -- It is possible that the Utxo has no possible inputs left, so
                  -- fail. We try and keep this from happening by using feedback:
                  -- adding to the number of ouputs (in the call to genRecipients)
                  -- in genTx above. Adding to the outputs means in the next cycle
                  -- the size of the UTxO will grow. In rare cases, this cannot be avoided
                  -- So we discard this test case. This should happen very rarely.
                  -- If it does happen, It is NOT a test failure, but an inadequacy in the
                  -- testing framework to generate almost-random transactions that always succeed every time.
                  -- Experience suggests that this happens less than 1% of the time, and does not lead to backtracking.
                  !_ <- when (null inputs) $ tracedDiscard $ "NoMoneyleft Utxo.hs " <> show (coin value)
                  let newWits =
                        mkTxWits @era
                          (utxo, txBody, scriptinfo)
                          ksIndexedPaymentKeys
                          ksIndexedStakingKeys
                          vkeyPairs
                          (mkScriptWits @era msigPairs mempty)
                          (hashAnnotated txBody)
                  pure $
                    delta
                      { extraWitnesses = extraWitnesses <> newWits
                      , extraInputs = extraInputs <> Set.fromList inputs
                      , change = deltaChange (<+> value) change -- <+> is plus of the Val class
                      , deltaVKeys = vkeyPairs <> deltaVKeys delta
                      , deltaScripts = msigPairs <> deltaScripts delta
                      }
    where
      deltaChange ::
        (Value era -> Value era) ->
        TxOut era ->
        TxOut era
      deltaChange f txOut = txOut & valueTxOutL %~ f
      getChangeAmount txOut = txOut ^. coinTxOutL

-- calculates fixed point of getNextDelta such that
-- reqFees (tx + delta) = dfees delta
-- start with zero delta
-- genNextDelta repeatedly until genNextDelta delta = delta

genNextDeltaTilFixPoint ::
  forall era.
  ( EraGen era
  , EraUTxO era
  ) =>
  ScriptInfo era ->
  Coin ->
  KeyPairs ->
  [(Script era, Script era)] ->
  UTxO era ->
  PParams era ->
  KeySpace era ->
  Tx era ->
  Gen (Delta era)
genNextDeltaTilFixPoint scriptinfo initialfee keys scripts utxo pparams keySpace tx = do
  addrs <- genRecipients @era 1 keys scripts
  let addr = maybe (error "genNextDeltaTilFixPoint: empty addrs") NE.head $ nonEmpty addrs
  fix
    0
    (genNextDelta scriptinfo utxo pparams keySpace tx)
    (deltaZero initialfee pparams addr)

applyDelta ::
  forall era.
  EraGen era =>
  UTxO era ->
  ScriptInfo era ->
  PParams era ->
  [KeyPair 'Witness] ->
  Map ScriptHash (Script era) ->
  KeySpace era ->
  Tx era ->
  Delta era ->
  Tx era
applyDelta
  utxo
  scriptinfo
  pparams
  neededKeys
  neededScripts
  KeySpace_ {ksIndexedPaymentKeys, ksIndexedStakingKeys}
  tx
  (Delta deltafees extraIn _extraWits change extraKeys extraScripts) =
    -- fix up the witnesses here?
    -- Adds extraInputs, extraWitnesses, and change from delta to tx
    let txBody = tx ^. bodyTxL
        oldWitnessSet =
          mkTxWits @era
            (utxo, addInputs @era txBody extraIn, scriptinfo)
            ksIndexedPaymentKeys
            ksIndexedStakingKeys
            kw
            sw
            (hashAnnotated txBody)
        body2 =
          (updateEraTxBody @era)
            utxo
            pparams
            oldWitnessSet
            txBody
            deltafees -- Override the existing fee
            extraIn -- Union with existing inputs
            change -- Append to end of the existing outputs
        kw = neededKeys <> extraKeys
        sw = neededScripts <> mkScriptWits @era extraScripts mempty
        newWitnessSet =
          mkTxWits @era
            (utxo, body2, scriptinfo)
            ksIndexedPaymentKeys
            ksIndexedStakingKeys
            kw
            sw
            (hashAnnotated body2)
     in constructTx @era body2 newWitnessSet (tx ^. auxDataTxL)

fix :: (Eq d, Monad m) => Int -> (Int -> d -> m d) -> d -> m d
fix n f d = do d1 <- f n d; if d1 == d then pure d else fix (n + 1) f d1

converge ::
  forall era.
  (EraGen era, EraUTxO era) =>
  ScriptInfo era ->
  Coin ->
  [KeyPair 'Witness] ->
  Map ScriptHash (Script era) ->
  KeyPairs ->
  [(Script era, Script era)] ->
  UTxO era ->
  PParams era ->
  KeySpace era ->
  Tx era ->
  Gen (Tx era)
converge
  scriptinfo
  initialfee
  neededKeys
  neededScripts
  keys
  scripts
  utxo
  pparams
  keySpace
  tx = do
    delta <- genNextDeltaTilFixPoint scriptinfo initialfee keys scripts utxo pparams keySpace tx
    genEraDone @era
      utxo
      pparams
      (applyDelta utxo scriptinfo pparams neededKeys neededScripts keySpace tx delta)

-- | Return up to /k/ random elements from /items/
-- (instead of the less efficient /take k <$> QC.shuffle items/)
ruffle :: Int -> [a] -> Gen [a]
ruffle _ [] = pure []
ruffle k items = do
  (indices, _) <- genIndices k (0, length itemsV - 1)
  pure $ map (itemsV V.!) indices
  where
    itemsV = V.fromList items

-- | Generate @k@ number of unique `Int`s in the supplied range.
genIndices :: Int -> (Int, Int) -> Gen ([Int], IntSet.IntSet)
genIndices k (l', u')
  | k < 0 || u - l + 1 < k =
      error $
        "Cannot generate "
          ++ show k
          ++ " indices in the range ["
          ++ show l
          ++ ", "
          ++ show u
          ++ "]"
  | u - l < k * 2 = do
      xs <- take k <$> QC.shuffle [l .. u]
      pure (xs, IntSet.fromList xs)
  | otherwise = go k [] mempty
  where
    (l, u) =
      if l' <= u'
        then (l', u')
        else (u', l')
    go n !res !acc
      | n <= 0 = pure (res, acc)
      | otherwise = do
          i <- QC.choose (l, u)
          if IntSet.member i acc
            then go n res acc
            else go (n - 1) (i : res) $ IntSet.insert i acc

-- | Select @n@ random key value pairs from the supplied map. Order of keys with
-- respect to each other will also be random, i.e. not sorted.
pickRandomFromMap :: Int -> Map.Map k t -> Gen [(k, t)]
pickRandomFromMap n initMap = uniformSubMapElems (\k v -> ((k, v) :)) (Just n) initMap QC

mkScriptWits ::
  forall era.
  EraGen era =>
  [(Script era, Script era)] ->
  [(Script era, Script era)] ->
  Map ScriptHash (Script era)
mkScriptWits payScripts stakeScripts =
  Map.fromList $
    (hashPayScript <$> payScripts)
      ++ (hashStakeScript <$> stakeScripts)
  where
    hashPayScript ::
      (Script era, Script era) ->
      (ScriptHash, Script era)
    hashPayScript (payScript, _) =
      (hashScript @era payScript, payScript)

    hashStakeScript ::
      (Script era, Script era) ->
      (ScriptHash, Script era)
    hashStakeScript (_, sScript) =
      (hashScript @era sScript, sScript)

mkTxWits ::
  forall era.
  EraGen era =>
  (UTxO era, TxBody era, ScriptInfo era) ->
  Map (KeyHash 'Payment) (KeyPair 'Payment) ->
  Map (KeyHash 'Staking) (KeyPair 'Staking) ->
  [KeyPair 'Witness] ->
  Map ScriptHash (Script era) ->
  SafeHash EraIndependentTxBody ->
  TxWits era
mkTxWits
  (utxo, txbody, scriptinfo)
  indexedPaymentKeys
  indexedStakingKeys
  awits
  msigs
  txBodyHash =
    genEraTxWits @era
      (utxo, txbody, scriptinfo)
      ( mkWitnessesVKey txBodyHash awits
          `Set.union` makeWitnessesFromScriptKeys
            txBodyHash
            ( indexedPaymentKeysAsWitnesses
                `Map.union` indexedStakingKeysAsWitnesses
            )
            msigSignatures
      )
      msigs
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
      msigSignatures = F.foldl' Set.union Set.empty $ map Set.fromList keysLists

-- | Distribute the sum of `balance_` and `fee` over the addresses, return the
-- sum of `fee` and the remainder of the equal distribution and the list ouf
-- transaction outputs that cover the balance and fees.
--
-- The idea is to have an specified spending balance and fees that must be paid
-- by the selected addresses.
-- TODO need right splitting of v!
calcOutputsFromBalance ::
  forall era.
  ( EraTxOut era
  , Split (Value era)
  ) =>
  Value era ->
  [Addr] ->
  Coin ->
  (Coin, StrictSeq (TxOut era))
calcOutputsFromBalance balance_ addrs fee =
  ( fee <+> splitCoinRem
  , StrictSeq.fromList $ zipWith mkBasicTxOut addrs amountPerOutput
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
  EraTxOut era =>
  (Int, Int) ->
  Map (KeyHash 'Payment) (KeyPair 'Payment) ->
  Map ScriptHash (Script era, Script era) ->
  UTxO era ->
  Gen
    ( [TxIn]
    , Value era
    , ([KeyPair 'Witness], [(Script era, Script era)])
    )
genInputs (minNumGenInputs, maxNumGenInputs) keyHashMap payScriptMap (UTxO utxo) = do
  numInputs <- QC.choose (minNumGenInputs, maxNumGenInputs)
  selectedUtxo <- pickRandomFromMap numInputs utxo
  let (inputs, witnesses) = unzip (fmap witnessedInput <$> selectedUtxo)
  return
    ( inputs
    , sumAllValue @era (snd <$> selectedUtxo)
    , Either.partitionEithers witnesses
    )
  where
    witnessedInput output =
      case output ^. addrTxOutL of
        addr@(Addr _ (KeyHashObj _) _) ->
          Left . asWitness $ findPayKeyPairAddr addr keyHashMap
        addr@(Addr _ (ScriptHashObj _) _) ->
          Right $ findPayScriptFromAddr @era addr payScriptMap
        _ -> error "unsupported address"

-- | Select a subset of the reward accounts to use for reward withdrawals.
genWithdrawals ::
  forall era.
  Constants ->
  Map ScriptHash (Script era, Script era) ->
  Map (KeyHash 'Staking) (KeyPair 'Staking) ->
  Map (Credential 'Staking) Coin ->
  Gen
    ( [(RewardAccount, Coin)]
    , ([KeyPair 'Witness], [(Script era, Script era)])
    )
genWithdrawals
  Constants
    { frequencyNoWithdrawals
    , frequencyAFewWithdrawals
    , frequencyPotentiallyManyWithdrawals
    , maxAFewWithdrawals
    }
  ksIndexedStakeScripts
  ksIndexedStakingKeys
  withdrawals = do
    (a, b) <-
      QC.frequency
        [
          ( frequencyNoWithdrawals
          , pure ([], ([], []))
          )
        ,
          ( frequencyAFewWithdrawals
          , genWrdls (take maxAFewWithdrawals . Map.toList $ withdrawals)
          )
        ,
          ( frequencyPotentiallyManyWithdrawals
          , genWrdls (Map.toList withdrawals)
          )
        ]
    pure (a, b)
    where
      toRewardAccount (rwd, coinx) = (RewardAccount Testnet rwd, coinx)
      genWrdls withdrawals_ = do
        selectedWrdls <- map toRewardAccount <$> QC.sublistOf withdrawals_
        let txwits =
              mkWithdrawalsWits @era ksIndexedStakeScripts ksIndexedStakingKeys
                . raCredential
                . fst
                <$> selectedWrdls
        return (selectedWrdls, Either.partitionEithers txwits)

-- | Collect witnesses needed for reward withdrawals.
mkWithdrawalsWits ::
  forall era.
  Map ScriptHash (Script era, Script era) ->
  Map (KeyHash 'Staking) (KeyPair 'Staking) ->
  Credential 'Staking ->
  Either (KeyPair 'Witness) (Script era, Script era)
mkWithdrawalsWits scriptsByStakeHash _ c@(ScriptHashObj _) =
  Right $
    findStakeScriptFromCred @era (asWitness c) scriptsByStakeHash
mkWithdrawalsWits _ keyHashMap c@(KeyHashObj _) =
  Left $
    asWitness $
      findPayKeyPairCred c keyHashMap

-- | Select recipient addresses that will serve as output targets for a new
-- transaction.
genRecipients ::
  forall era.
  EraGen era =>
  Int ->
  KeyPairs ->
  [(Script era, Script era)] ->
  Gen [Addr]
genRecipients nRecipients' keys scripts = do
  nRecipients <-
    max 1
      <$> QC.frequency
        [ (1, pure (nRecipients' - 1)) -- contract size of UTxO
        , (2, pure nRecipients') -- keep size
        , (1, pure (nRecipients' + 1)) -- expand size of UTxO
        ]

  -- We want to choose more Keys than Scripts by a factor of 2 or more.
  nScripts <- QC.choose (0, nRecipients * 2 `div` 3) -- Average is about nRecipients / 3
  let nKeys = nRecipients - nScripts

  recipientKeys <- ruffle nKeys keys
  recipientScripts <- ruffle nScripts scripts

  let payKeys = mkCredential . fst <$> recipientKeys
      stakeKeys = mkCredential . snd <$> recipientKeys
      payScripts = mkCredential . hashScript . fst <$> recipientScripts
      stakeScripts = mkCredential . hashScript . snd <$> recipientScripts

  -- zip keys and scripts together as base addresses
  let payCreds :: [Credential 'Payment]
      payCreds = payKeys ++ payScripts
      stakeCreds :: [Credential 'Staking]
      stakeCreds = stakeKeys ++ stakeScripts

  return (zipWith mkAddr payCreds stakeCreds)

genPtrAddrs :: DState era -> [Addr] -> Gen [Addr]
genPtrAddrs ds addrs = do
  let pointers = ptrsMap ds
  n <- QC.choose (0, min (Map.size pointers) (length addrs))
  pointerList <- map fst <$> pickRandomFromMap n pointers

  let addrs' = zipWith baseAddrToPtrAddr (take n addrs) pointerList

  pure (addrs' ++ drop n addrs)
  where
    baseAddrToPtrAddr a p = case a of
      Addr n pay _ -> Addr n pay (StakeRefPtr p)
      _ -> a
