{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE UndecidableSuperClasses #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Cardano.Ledger.Alonzo.UTxO (
  AlonzoEraUTxO (..),
  getAlonzoSpendingDatum,
  getAlonzoSpendingTxIn,

  -- * Scripts needed
  AlonzoScriptsNeeded (..),
  getAlonzoScriptsNeeded,
  getSpendingScriptsNeeded,
  getRewardingScriptsNeeded,
  getMintingScriptsNeeded,
  getAlonzoScriptsHashesNeeded,
  zipAsIxItem,

  -- * Datums needed
  getInputDataHashesTxBody,

  -- * WitsVKey needed
  getAlonzoWitsVKeyNeeded,
)
where

import Cardano.Ledger.Alonzo.Core
import Cardano.Ledger.Alonzo.Era (AlonzoEra)
import Cardano.Ledger.Alonzo.Scripts (AlonzoPlutusPurpose (..))
import Cardano.Ledger.Alonzo.Tx (isTwoPhaseScriptAddressFromMap)
import Cardano.Ledger.Alonzo.TxWits (unTxDats)
import Cardano.Ledger.BaseTypes (StrictMaybe (..))
import Cardano.Ledger.CertState (CertState)
import Cardano.Ledger.Credential (credScriptHash)
import Cardano.Ledger.Crypto
import Cardano.Ledger.Keys (KeyHash, KeyRole (Witness))
import Cardano.Ledger.Mary.UTxO (getConsumedMaryValue)
import Cardano.Ledger.Mary.Value (PolicyID (..))
import Cardano.Ledger.Plutus.Data (Data, Datum (..))
import Cardano.Ledger.Shelley.TxBody (raCredential)
import Cardano.Ledger.Shelley.UTxO (getShelleyWitsVKeyNeeded, shelleyProducedValue)
import Cardano.Ledger.TxIn
import Cardano.Ledger.UTxO (
  EraUTxO (..),
  ScriptsProvided (..),
  UTxO (..),
  getScriptHash,
 )
import Control.SetAlgebra (eval, (◁))
import Data.Foldable (foldl', toList)
import qualified Data.Map.Strict as Map
import Data.Maybe (catMaybes, fromMaybe)
import qualified Data.Set as Set
import Data.Word (Word32)
import Lens.Micro ((^.))
import Lens.Micro.Extras (view)

-- | Alonzo era style `ScriptsNeeded` require also a `PlutusPurpose`, not only the `ScriptHash`
newtype AlonzoScriptsNeeded era
  = AlonzoScriptsNeeded [(PlutusPurpose AsIxItem era, ScriptHash (EraCrypto era))]
  deriving (Monoid, Semigroup)

deriving instance AlonzoEraScript era => Eq (AlonzoScriptsNeeded era)
deriving instance AlonzoEraScript era => Show (AlonzoScriptsNeeded era)

instance Crypto c => EraUTxO (AlonzoEra c) where
  type ScriptsNeeded (AlonzoEra c) = AlonzoScriptsNeeded (AlonzoEra c)

  getConsumedValue = getConsumedMaryValue

  getProducedValue = shelleyProducedValue

  getScriptsProvided _ tx = ScriptsProvided (tx ^. witsTxL . scriptTxWitsL)

  getScriptsNeeded = getAlonzoScriptsNeeded

  getScriptsHashesNeeded = getAlonzoScriptsHashesNeeded

  getWitsVKeyNeeded = getAlonzoWitsVKeyNeeded

class EraUTxO era => AlonzoEraUTxO era where
  -- | Get data hashes for a transaction that are not required. Such datums are optional,
  -- but they can be added to the witness set. In a broaded terms datums corresponding to
  -- the inputs that might be spent are the required datums and the datums corresponding
  -- to the outputs and reference inputs are the supplemental datums.
  getSupplementalDataHashes ::
    UTxO era ->
    TxBody era ->
    Set.Set (DataHash (EraCrypto era))

  -- | Lookup the TxIn from the `Spending` ScriptPurpose and find the datum needed for
  -- spending that input. This function will return `Nothing` for all script purposes,
  -- except spending, because only spending scripts require an extra datum.
  --
  -- This is the same function as in the spec:
  --
  -- @
  --   getDatum :: Tx era -> UTxO era -> ScriptPurpose era -> [Data era]
  -- @
  getSpendingDatum ::
    UTxO era ->
    Tx era ->
    PlutusPurpose AsItem era ->
    Maybe (Data era)

instance Crypto c => AlonzoEraUTxO (AlonzoEra c) where
  getSupplementalDataHashes _ = getAlonzoSupplementalDataHashes

  getSpendingDatum = getAlonzoSpendingDatum

getAlonzoSupplementalDataHashes ::
  (EraTxBody era, AlonzoEraTxOut era) =>
  TxBody era ->
  Set.Set (DataHash (EraCrypto era))
getAlonzoSupplementalDataHashes txBody =
  Set.fromList
    [ dh
    | txOut <- toList $ txBody ^. outputsTxBodyL
    , SJust dh <- [txOut ^. dataHashTxOutL]
    ]

-- | Get the Data associated with a ScriptPurpose. Only the Spending ScriptPurpose
--  contains Data. Nothing is returned for the other kinds.
getAlonzoSpendingDatum ::
  (AlonzoEraTxWits era, AlonzoEraTxOut era, EraTx era) =>
  UTxO era ->
  Tx era ->
  PlutusPurpose AsItem era ->
  Maybe (Data era)
getAlonzoSpendingDatum (UTxO m) tx sp = do
  AsItem txIn <- toSpendingPurpose sp
  txOut <- Map.lookup txIn m
  SJust hash <- Just $ txOut ^. dataHashTxOutL
  Map.lookup hash (unTxDats $ tx ^. witsTxL . datsTxWitsL)

-- | Only the Spending ScriptPurpose contains TxIn
getAlonzoSpendingTxIn :: AlonzoPlutusPurpose AsItem era -> Maybe (TxIn (EraCrypto era))
getAlonzoSpendingTxIn = \case
  AlonzoSpending (AsItem txIn) -> Just txIn
  AlonzoMinting _policyId -> Nothing
  AlonzoRewarding _rewardAccount -> Nothing
  AlonzoCertifying _txCert -> Nothing
{-# DEPRECATED getAlonzoSpendingTxIn "In favor of more general `toSpendingPurpose`" #-}

getAlonzoScriptsHashesNeeded :: AlonzoScriptsNeeded era -> Set.Set (ScriptHash (EraCrypto era))
getAlonzoScriptsHashesNeeded (AlonzoScriptsNeeded sn) = Set.fromList (map snd sn)

-- | Compute two sets for all TwoPhase scripts in a Tx.
--
--   1) DataHashes for each Two phase Script in a TxIn that has a DataHash
--   2) TxIns that are TwoPhase scripts, and should have a DataHash but don't.
--
-- @{ h | (_ → (a,_,h)) ∈ txins tx ◁ utxo, isNonNativeScriptAddress tx a}@
getInputDataHashesTxBody ::
  (EraTxBody era, AlonzoEraTxOut era, EraScript era) =>
  UTxO era ->
  TxBody era ->
  ScriptsProvided era ->
  (Set.Set (DataHash (EraCrypto era)), Set.Set (TxIn (EraCrypto era)))
getInputDataHashesTxBody (UTxO mp) txBody (ScriptsProvided scriptsProvided) =
  Map.foldlWithKey' accum (Set.empty, Set.empty) spendUTxO
  where
    spendInputs = txBody ^. inputsTxBodyL
    spendUTxO = eval (spendInputs ◁ mp)
    accum ans@(!hashSet, !inputSet) txIn txOut =
      let addr = txOut ^. addrTxOutL
          isTwoPhaseScriptAddress = isTwoPhaseScriptAddressFromMap scriptsProvided addr
       in case txOut ^. datumTxOutF of
            NoDatum
              | isTwoPhaseScriptAddress -> (hashSet, Set.insert txIn inputSet)
            DatumHash dataHash
              | isTwoPhaseScriptAddress -> (Set.insert dataHash hashSet, inputSet)
            -- Though it is somewhat odd to allow non-two-phase-scripts to include a datum,
            -- the Alonzo era already set the precedent with datum hashes, and several dapp
            -- developers see this as a helpful feature.
            _ -> ans

-- |
-- Uses of inputs in ‘txscripts’ and ‘neededScripts’
-- There are currently 3 sets of inputs (spending, collateral, reference). A particular TxInput
-- can appear in more than one of the sets. Even in all three at the same, but that may not be
-- a really useful case. Inputs are where you find scripts with the 'Spending' purpose.
--
-- 1) Collateral inputs are only spent if phase two fails. Their corresponding TxOut can only have
--    Key (not Script) Pay credentials, so ‘neededScripts’ does not look there.
-- 2) Reference inputs are not spent in the current Tx, unless that same input also appears in one
--    of the other sets. If that is not the case, their credentials are never needed, so anyone can
--    access the inline datums and scripts in their corresponding TxOut, without needing any
--    authorizing credentials. So ‘neededScripts’ does not look there.
-- 3) Spending inputs are always spent. So their Pay credentials are always needed.
--
-- Collect information (purpose and ScriptHash) about all the Credentials that refer to scripts
-- that will be needed to run in a TxBody in the Utxow rule. Note there may be credentials that
-- cannot be run, so are not collected. In Babbage, reference inputs, fit that description.
-- Purposes include
-- 1) Spending (payment script credentials, but NOT staking scripts) in the Addr of a TxOut, pointed
--    to by some input that needs authorization. Be sure (txBody ^. inputsTxBodyL) gets all such inputs.
--    In some Eras there may be multiple sets of inputs, which ones should be included? Currently that
--    is only the spending inputs. Because collateral inputs can only have key-locked credentials,
--    and reference inputs are never authorized. That might not always be the case.
-- 2) Rewarding (Withdrawals),
-- 3) Minting (minted field), and
-- 4) Certifying (Delegating) scripts.
--
-- 'getAlonzoScriptsNeeded' is an aggregation of the needed Credentials referring to
-- Scripts used in Utxow rule.  The flip side of 'getAlonzoScriptsNeeded' (which collects
-- script hashes) is 'txscripts' which finds the actual scripts. We maintain an invariant
-- that every script credential refers to some actual script.  This is tested in the test
-- function 'validateMissingScripts' in the Utxow rule.
getAlonzoScriptsNeeded ::
  (MaryEraTxBody era, AlonzoEraScript era) =>
  UTxO era ->
  TxBody era ->
  AlonzoScriptsNeeded era
getAlonzoScriptsNeeded utxo txBody =
  getSpendingScriptsNeeded utxo txBody
    <> getRewardingScriptsNeeded txBody
    <> certifyingScriptsNeeded
    <> getMintingScriptsNeeded txBody
  where
    certifyingScriptsNeeded =
      AlonzoScriptsNeeded $
        case foldl' addUniqueTxCertPurpose (Map.empty, 0, []) (txBody ^. certsTxBodyL) of
          (_, _, certPurposes) -> reverse certPurposes
      where
        -- We need to do this funny index manipulation here because we've allowed
        -- duplicate certificates all the way until Conway. This prevented second
        -- occurance of a duplicate certificate in the sequence to be used. In order to
        -- preserve this behavior we need to use the index of the first occurrence of a
        -- duplicate certificate.
        --
        -- The `ix + 1` part is to count the actual index of each element. It is only when
        -- we see a duplicate we use the index of the first occurrence of the element, but
        -- that should not affect indices of other elements.
        --
        -- For example if these are our certificates:
        -- cert = [c0, c1, c2, c3, c4, c5]
        --
        -- Let's say `c3` is locked by a native script or a key witness, so it does not
        -- participate in the script purpose
        --
        -- Also, let's say `c1 == c4`. Here is what we should get for the plutus purpose:
        -- plutusPurpose = [(0, c0), (1, c1), (2, c2), (1, c4), (5, c5)]
        --
        -- The count must continue no matter what, thus the counter `ix + 1`, but
        -- whenever we find a duplicate we use the stored `ix'`.
        --
        addUniqueTxCertPurpose (!certScriptHashes, !ix, !certPurposes) txCert =
          fromMaybe (certScriptHashes, ix + 1, certPurposes) $ do
            scriptHash <- getScriptWitnessTxCert txCert
            case Map.lookup scriptHash certScriptHashes of
              Nothing -> do
                let !purpose = CertifyingPurpose (AsIxItem ix txCert)
                    !certScriptHashes' = Map.insert scriptHash ix certScriptHashes
                pure (certScriptHashes', ix + 1, (purpose, scriptHash) : certPurposes)
              Just ix' -> do
                let !purpose = CertifyingPurpose (AsIxItem ix' txCert)
                pure (certScriptHashes, ix + 1, (purpose, scriptHash) : certPurposes)
{-# INLINEABLE getAlonzoScriptsNeeded #-}

zipAsIxItem :: Foldable f => f it -> (AsIxItem Word32 it -> c) -> [c]
zipAsIxItem xs f =
  -- Past experience showed that enumeration for Int is faster than for Word32
  zipWith (\it ix -> f (AsIxItem (fromIntegral @Int @Word32 ix) it)) (toList xs) [0 ..]
{-# INLINE zipAsIxItem #-}

getSpendingScriptsNeeded ::
  (AlonzoEraScript era, EraTxBody era) =>
  UTxO era ->
  TxBody era ->
  AlonzoScriptsNeeded era
getSpendingScriptsNeeded (UTxO utxo) txBody =
  AlonzoScriptsNeeded $
    catMaybes $
      zipAsIxItem (txBody ^. inputsTxBodyL) $
        \asIxItem@(AsIxItem _ txIn) -> do
          addr <- view addrTxOutL <$> Map.lookup txIn utxo
          hash <- getScriptHash addr
          return (SpendingPurpose asIxItem, hash)
{-# INLINEABLE getSpendingScriptsNeeded #-}

getRewardingScriptsNeeded ::
  (AlonzoEraScript era, EraTxBody era) =>
  TxBody era ->
  AlonzoScriptsNeeded era
getRewardingScriptsNeeded txBody =
  AlonzoScriptsNeeded $
    catMaybes $
      zipAsIxItem (Map.keys (unWithdrawals $ txBody ^. withdrawalsTxBodyL)) $
        \asIxItem@(AsIxItem _ rewardAccount) ->
          (RewardingPurpose asIxItem,) <$> credScriptHash (raCredential rewardAccount)
{-# INLINEABLE getRewardingScriptsNeeded #-}

getMintingScriptsNeeded ::
  (AlonzoEraScript era, MaryEraTxBody era) =>
  TxBody era ->
  AlonzoScriptsNeeded era
getMintingScriptsNeeded txBody =
  AlonzoScriptsNeeded $
    zipAsIxItem (txBody ^. mintedTxBodyF) $
      \asIxItem@(AsIxItem _ (PolicyID scriptHash)) -> (MintingPurpose asIxItem, scriptHash)
{-# INLINEABLE getMintingScriptsNeeded #-}

-- | Just like `getShelleyWitsVKeyNeeded`, but also requires `reqSignerHashesTxBodyL`.
getAlonzoWitsVKeyNeeded ::
  forall era.
  (EraTx era, AlonzoEraTxBody era, ShelleyEraTxBody era) =>
  CertState era ->
  UTxO era ->
  TxBody era ->
  Set.Set (KeyHash 'Witness (EraCrypto era))
getAlonzoWitsVKeyNeeded certState utxo txBody =
  getShelleyWitsVKeyNeeded certState utxo txBody
    `Set.union` (txBody ^. reqSignerHashesTxBodyL)
{-# INLINEABLE getAlonzoWitsVKeyNeeded #-}
