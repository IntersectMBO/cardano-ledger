{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Test.Cardano.Ledger.AllegraEraGen (
  -- export EraGen instance for AllegraEra and helpers shared with MaryEra
  quantifyTL,
  unQuantifyTL,
  someLeaf,
  genValidityInterval,
)
where

import Cardano.Ledger.Allegra (AllegraEra)
import Cardano.Ledger.Allegra.Core
import Cardano.Ledger.Allegra.Scripts (Timelock (..))
import Cardano.Ledger.Allegra.TxBody (
  AllegraTxBody (..),
  ValidityInterval (ValidityInterval),
 )
import Cardano.Ledger.AuxiliaryData (AuxiliaryDataHash)
import Cardano.Ledger.BaseTypes (StrictMaybe (..))
import Cardano.Ledger.Binary (encCBOR, serialize')
import Cardano.Ledger.Coin (Coin)
import qualified Cardano.Ledger.Crypto as CryptoClass
import Cardano.Ledger.Keys (KeyHash)
import Cardano.Ledger.Pretty.Mary ()
import Cardano.Ledger.Shelley.API (KeyRole (Witness))
import Cardano.Ledger.Shelley.PParams (Update)
import Cardano.Ledger.Shelley.Tx (pattern ShelleyTx)
import Cardano.Ledger.Shelley.TxBody (ShelleyTxOut (..))
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
 Shelley property tests for (AllegraEra c)

 This instance is layered on top of the ShelleyMA instances
 in Cardano.Ledger.ShelleyMA.Scripts:

 `type instance Script (AllegraEra c) = Timelock (AllegraEra c)`
 `instance ValidateScript (ShelleyMAEra ma c) where ...`
------------------------------------------------------------------------------}

instance (CryptoClass.Crypto c) => ScriptClass (AllegraEra c) where
  isKey _ (RequireSignature hk) = Just hk
  isKey _ _ = Nothing
  basescript _proxy = someLeaf @(AllegraEra c)
  quantify _ = quantifyTL
  unQuantify _ = unQuantifyTL

instance CryptoClass.Crypto c => EraGen (AllegraEra c) where
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
  constructTx = ShelleyTx

genTxBody ::
  AllegraEraTxBody era =>
  SlotNo ->
  Set.Set (TxIn (EraCrypto era)) ->
  StrictSeq (TxOut era) ->
  StrictSeq (DCert era) ->
  Withdrawals (EraCrypto era) ->
  Coin ->
  StrictMaybe (Update era) ->
  StrictMaybe (AuxiliaryDataHash (EraCrypto era)) ->
  Gen (AllegraTxBody era, [Timelock era])
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

instance CryptoClass.Crypto c => MinGenTxout (AllegraEra c) where
  calcEraMinUTxO _txout pp = pp ^. ppMinUTxOValueL
  addValToTxOut v (ShelleyTxOut a u) = ShelleyTxOut a (v <+> u)
  genEraTxOut _genenv genVal addrs = do
    values <- replicateM (length addrs) genVal
    pure (zipWith mkBasicTxOut addrs values)

{------------------------------------------------------------------------------
  ShelleyMA helpers, shared by Allegra and Mary
------------------------------------------------------------------------------}

quantifyTL :: Era era => Timelock era -> Quantifier (Timelock era)
quantifyTL (RequireAllOf xs) = AllOf (foldr (:) [] xs)
quantifyTL (RequireAnyOf xs) = AnyOf (foldr (:) [] xs)
quantifyTL (RequireMOf n xs) = MOf n (foldr (:) [] xs)
quantifyTL t = Leaf t

unQuantifyTL :: Era era => Quantifier (Timelock era) -> Timelock era
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
  Era era =>
  KeyHash 'Witness (EraCrypto era) ->
  Timelock era
someLeaf x =
  let n = mod (hash (serialize' (eraProtVerLow @era) (encCBOR x))) 200
   in partition @era [n] [RequireSignature x]

partition ::
  forall era.
  Era era =>
  [Int] ->
  [Timelock era] ->
  Timelock era
partition splits scripts =
  RequireAnyOf . fromList $
    zipWith pair (intervals @era splits) (cycle scripts)
  where
    pair a b = RequireAllOf $ fromList [a, b]

intervals ::
  forall era.
  Era era =>
  [Int] ->
  [Timelock era]
intervals xs = zipWith mkInterval padded (tail padded)
  where
    padded = Nothing : (Just . SlotNo . fromIntegral <$> xs) ++ [Nothing]
    start Nothing = []
    start (Just x) = [RequireTimeStart x]
    end Nothing = []
    end (Just x) = [RequireTimeExpire x]
    mkInterval s e = RequireAllOf . fromList $ start s ++ end e
