{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Test.Cardano.Ledger.AllegraEraGen (
  -- export EraGen instance for AllegraEra and helpers shared with MaryEra
  quantifyTL,
  unQuantifyTL,
  someLeaf,
  genValidityInterval,
) where

import Cardano.Ledger.Allegra (AllegraEra, Tx (..))
import Cardano.Ledger.Allegra.Core
import Cardano.Ledger.Allegra.Scripts (
  AllegraEraScript,
  Timelock (..),
  pattern RequireTimeExpire,
  pattern RequireTimeStart,
 )
import Cardano.Ledger.Allegra.TxBody (TxBody (AllegraTxBody))
import Cardano.Ledger.BaseTypes (StrictMaybe (..))
import Cardano.Ledger.Binary (encCBOR, serialize')
import Cardano.Ledger.Coin (Coin)
import Cardano.Ledger.Shelley.PParams (Update)
import Cardano.Ledger.Shelley.Scripts (
  pattern RequireAllOf,
  pattern RequireAnyOf,
  pattern RequireMOf,
  pattern RequireSignature,
 )
import Cardano.Ledger.Shelley.Tx (pattern ShelleyTx)
import Cardano.Ledger.Shelley.TxOut (ShelleyTxOut (..))
import Cardano.Ledger.Shelley.TxWits (pattern ShelleyTxWits)
import Cardano.Ledger.TxIn (TxIn)
import Cardano.Ledger.Val ((<+>))
import Cardano.Slotting.Slot (SlotNo (SlotNo))
import Control.Monad (replicateM)
import Data.Hashable (hash)
import Data.Sequence.Strict (StrictSeq (..), fromList)
import qualified Data.Set as Set
import Lens.Micro
import Test.Cardano.Ledger.Allegra.Arbitrary ()
import Test.Cardano.Ledger.Shelley.Constants (Constants (..))
import Test.Cardano.Ledger.Shelley.Generator.Core (GenEnv (..), genCoin)
import Test.Cardano.Ledger.Shelley.Generator.EraGen (EraGen (..), MinGenTxout (..))
import Test.Cardano.Ledger.Shelley.Generator.ScriptClass (
  Quantifier (..),
  ScriptClass (..),
 )
import Test.Cardano.Ledger.Shelley.Generator.Update (genPParams, genShelleyPParamsUpdate)
import Test.Cardano.Ledger.ShelleyMA.Serialisation.Generators ()
import Test.QuickCheck (Arbitrary, Gen, arbitrary, frequency)

-- ==========================================================

{------------------------------------------------------------------------------
 EraGen instance for AllegraEra - This instance makes it possible to run the
 Shelley property tests for AllegraEra

 This instance is layered on top of the ShelleyMA instances
 in Cardano.Ledger.ShelleyMA.Scripts:

 `type instance Script AllegraEra = Timelock AllegraEra`
 `instance ValidateScript (ShelleyMAEra ma c) where ...`
------------------------------------------------------------------------------}

instance ScriptClass AllegraEra where
  isKey _ (RequireSignature hk) = Just hk
  isKey _ _ = Nothing
  basescript _proxy = someLeaf @AllegraEra
  quantify _ = quantifyTL
  unQuantify _ = unQuantifyTL

instance EraGen AllegraEra where
  genGenesisValue (GenEnv _keySpace _scriptspace Constants {minGenesisOutputVal, maxGenesisOutputVal}) =
    genCoin minGenesisOutputVal maxGenesisOutputVal
  genEraTxBody _ge _utxo _pparams = genTxBody
  genEraAuxiliaryData = genAuxiliaryData
  updateEraTxBody _utxo _pp _wits txBody fee ins out =
    txBody
      & inputsTxBodyL %~ (<> ins)
      & outputsTxBodyL %~ (:|> out)
      & feeTxBodyL .~ fee
  genEraPParamsUpdate = genShelleyPParamsUpdate
  genEraPParams = genPParams
  genEraTxWits _scriptinfo setWitVKey mapScriptWit = ShelleyTxWits setWitVKey mapScriptWit mempty
  constructTx x y z = MkAllegraTx $ ShelleyTx x y z

genTxBody ::
  SlotNo ->
  Set.Set TxIn ->
  StrictSeq (TxOut AllegraEra) ->
  StrictSeq (TxCert AllegraEra) ->
  Withdrawals ->
  Coin ->
  StrictMaybe (Update AllegraEra) ->
  StrictMaybe TxAuxDataHash ->
  Gen (TxBody TopTx AllegraEra, [Timelock AllegraEra])
genTxBody slot ins outs cert wdrl fee upd ad = do
  validityInterval <- genValidityInterval slot
  pure
    ( AllegraTxBody
        ins
        outs
        cert
        wdrl
        fee
        validityInterval
        upd
        ad
    , [] -- Allegra does not need any additional script witnesses
    )

instance MinGenTxout AllegraEra where
  calcEraMinUTxO _txout pp = pp ^. ppMinUTxOValueL
  addValToTxOut v (ShelleyTxOut a u) = ShelleyTxOut a (v <+> u)
  genEraTxOut _genenv genVal addrs = do
    values <- replicateM (length addrs) genVal
    pure (zipWith mkBasicTxOut addrs values)

{------------------------------------------------------------------------------
  ShelleyMA helpers, shared by Allegra and Mary
------------------------------------------------------------------------------}

quantifyTL ::
  AllegraEraScript era =>
  NativeScript era ->
  Quantifier (NativeScript era)
quantifyTL (RequireAllOf xs) = AllOf (foldr (:) [] xs)
quantifyTL (RequireAnyOf xs) = AnyOf (foldr (:) [] xs)
quantifyTL (RequireMOf n xs) = MOf n (foldr (:) [] xs)
quantifyTL t = Leaf t

unQuantifyTL :: AllegraEraScript era => Quantifier (NativeScript era) -> NativeScript era
unQuantifyTL (AllOf xs) = RequireAllOf (fromList xs)
unQuantifyTL (AnyOf xs) = RequireAnyOf (fromList xs)
unQuantifyTL (MOf n xs) = RequireMOf n (fromList xs)
unQuantifyTL (Leaf t) = t

genAuxiliaryData ::
  Arbitrary (TxAuxData era) =>
  Constants ->
  Gen (StrictMaybe (TxAuxData era))
genAuxiliaryData Constants {frequencyTxWithMetadata} =
  frequency
    [ (frequencyTxWithMetadata, SJust <$> arbitrary)
    , (100 - frequencyTxWithMetadata, pure SNothing)
    ]

-- | Generates a trivial validity interval that is valid for the current slot.
--
-- Note: the validity interval must be a subset of all timelock
-- script intervals that apply to the transaction. This depends on
-- which generated scripts are actually required to validate the transaction
-- (which is itself not always deterministic, e.g. 'RequireMOf n scripts').
--
-- A more sophisticated generator would compute which set of scripts
-- would validate the transaction, and from that compute a minimal
-- ValidityInterval that fits into all timelock slot ranges.
genValidityInterval :: SlotNo -> Gen ValidityInterval
genValidityInterval cs@(SlotNo currentSlot) =
  pure $
    ValidityInterval
      (SJust cs)
      (SJust . SlotNo $ currentSlot + 1)

-- | Generate some Leaf Timelock (i.e. a Signature or TimeStart or TimeExpire).
--
-- Because we don't know how these "leaf scripts" will be situated in larger scripts
-- (e.g. the script generated here might form part of a 'RequireAll' or 'RequireMOf' script)
-- we must make sure that all timelocks generated here are valid for all slots.
--
-- To achieve this we arrange the timelock scripts like so:
--  RequireAnyOf [
--     RequireAllOf [RequireTimeExpire k, RequireSignature x],
--     RequireAllOf [RequireTimeStart k, RequireSignature x]
--  ]
-- where k is arbitrary. This means that regardless of slot, there will be a
-- valid sub-branch of script.
someLeaf ::
  forall era.
  (AllegraEraScript era, NativeScript era ~ Timelock era) =>
  KeyHash 'Witness ->
  NativeScript era
someLeaf x =
  let n = mod (hash (serialize' (eraProtVerLow @era) (encCBOR x))) 200
   in partition @era [n] [RequireSignature x]

partition ::
  forall era.
  AllegraEraScript era =>
  [Int] ->
  [NativeScript era] ->
  NativeScript era
partition splits scripts =
  RequireAnyOf . fromList $
    zipWith pair (intervals @era splits) (cycle scripts)
  where
    pair a b = RequireAllOf $ fromList [a, b]

intervals ::
  forall era.
  AllegraEraScript era =>
  [Int] ->
  [NativeScript era]
intervals xs = zipWith mkInterval padded (drop 1 padded)
  where
    padded = Nothing : (Just . SlotNo . fromIntegral <$> xs) ++ [Nothing]
    start Nothing = []
    start (Just x) = [RequireTimeStart x]
    end Nothing = []
    end (Just x) = [RequireTimeExpire x]
    mkInterval s e = RequireAllOf . fromList $ start s ++ end e
