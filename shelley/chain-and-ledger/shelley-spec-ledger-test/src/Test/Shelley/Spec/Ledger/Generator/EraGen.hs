{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Test.Shelley.Spec.Ledger.Generator.EraGen (genUtxo0, genesisId, EraGen (..)) where

import Cardano.Binary (ToCBOR (toCBOR))
import qualified Cardano.Crypto.Hash as Hash
import Cardano.Ledger.AuxiliaryData (AuxiliaryDataHash)
import qualified Cardano.Ledger.Core as Core
import qualified Cardano.Ledger.Crypto as CC (HASH)
import Cardano.Ledger.Era (Crypto)
import Cardano.Ledger.Shelley.Constraints (ShelleyBased)
import Cardano.Slotting.Slot (SlotNo)
import Data.Coerce (coerce)
import Data.Sequence.Strict (StrictSeq)
import Data.Set (Set)
import Shelley.Spec.Ledger.API
  ( Addr (Addr),
    Credential (ScriptHashObj),
    StakeReference (StakeRefBase),
  )
import Shelley.Spec.Ledger.Address (toAddr)
import Shelley.Spec.Ledger.BaseTypes (Network (..), StrictMaybe)
import Shelley.Spec.Ledger.Coin (Coin)
import Shelley.Spec.Ledger.PParams (Update)
import Shelley.Spec.Ledger.Tx
  ( TxId (TxId),
    ValidateScript (..),
  )
import Shelley.Spec.Ledger.TxBody (DCert, TxIn, TxOut, Wdrl)
import Shelley.Spec.Ledger.UTxO (UTxO)
import Test.QuickCheck (Gen)
import Test.Shelley.Spec.Ledger.Generator.Constants (Constants (..))
import Test.Shelley.Spec.Ledger.Generator.Core
  ( GenEnv (..),
    genTxOut,
    genesisCoins,
  )
import Test.Shelley.Spec.Ledger.Generator.Presets (someKeyPairs)
import Test.Shelley.Spec.Ledger.Generator.ScriptClass (ScriptClass, someScripts)
import Test.Shelley.Spec.Ledger.Utils (Split (..))

{------------------------------------------------------------------------------
 An EraGen instance makes it possible to run the Shelley property tests
 -----------------------------------------------------------------------------}

class
  ( ShelleyBased era,
    ValidateScript era,
    Split (Core.Value era),
    Show (Core.Script era),
    ScriptClass era
  ) =>
  EraGen era
  where
  -- | Generate a genesis value for the Era
  genGenesisValue :: GenEnv era -> Gen (Core.Value era)

  -- | Given some pre-generated data, generate an era-specific TxBody
  genEraTxBody ::
    GenEnv era ->
    SlotNo ->
    Set (TxIn (Crypto era)) ->
    StrictSeq (TxOut era) ->
    StrictSeq (DCert (Crypto era)) ->
    Wdrl (Crypto era) ->
    Coin ->
    StrictMaybe (Update era) ->
    StrictMaybe (AuxiliaryDataHash (Crypto era)) ->
    Gen (Core.TxBody era)

  -- | Generate era-specific auxiliary data
  genEraAuxiliaryData :: Constants -> Gen (StrictMaybe (Core.AuxiliaryData era))

  -- | Update an era-specific TxBody
  updateEraTxBody ::
    Core.TxBody era ->
    Coin ->
    Set (TxIn (Crypto era)) ->
    StrictSeq (TxOut era) ->
    Core.TxBody era

{------------------------------------------------------------------------------
  Generators shared across eras
 -----------------------------------------------------------------------------}

genUtxo0 ::
  forall era.
  EraGen era =>
  GenEnv era ->
  Gen (UTxO era)
genUtxo0 ge@(GenEnv _ c@Constants {minGenesisUTxOouts, maxGenesisUTxOouts}) = do
  genesisKeys <- someKeyPairs c minGenesisUTxOouts maxGenesisUTxOouts
  genesisScripts <- someScripts @era c minGenesisUTxOouts maxGenesisUTxOouts
  outs <-
    genTxOut
      (genGenesisValue @era ge)
      (fmap (toAddr Testnet) genesisKeys ++ fmap (scriptsToAddr' Testnet) genesisScripts)
  return (genesisCoins genesisId outs)
  where
    scriptsToAddr' :: Network -> (Core.Script era, Core.Script era) -> Addr (Crypto era)
    scriptsToAddr' n (payScript, stakeScript) =
      Addr n (scriptToCred' payScript) (StakeRefBase $ scriptToCred' stakeScript)

    scriptToCred' :: Core.Script era -> Credential kr (Crypto era)
    scriptToCred' = ScriptHashObj . hashScript @era

-- | We share this dummy TxId as genesis transaction id across eras
genesisId ::
  Hash.HashAlgorithm (CC.HASH crypto) =>
  TxId crypto
genesisId = TxId (mkDummyHash 0)
  where
    mkDummyHash :: forall h a. Hash.HashAlgorithm h => Int -> Hash.Hash h a
    mkDummyHash = coerce . Hash.hashWithSerialiser @h toCBOR
