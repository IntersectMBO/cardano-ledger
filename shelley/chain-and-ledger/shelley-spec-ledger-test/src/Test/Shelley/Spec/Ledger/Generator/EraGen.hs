{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Test.Shelley.Spec.Ledger.Generator.EraGen (genesisId,genUtxo0) where

import qualified Cardano.Ledger.Core as Core
import qualified Cardano.Ledger.Crypto as CC (Crypto)
import Cardano.Ledger.Shelley (ShelleyBased, ShelleyEra)
import Data.Sequence.Strict (StrictSeq)
import Data.Set (Set)
import Shelley.Spec.Ledger.API
  ( Addr (Addr),
    Coin (..),
    Credential (ScriptHashObj),
    DCert,
    StakeReference (StakeRefBase),
    Update,
  )
import Shelley.Spec.Ledger.Address (toAddr)
import Shelley.Spec.Ledger.BaseTypes (Network (..), StrictMaybe (..))
import Shelley.Spec.Ledger.MetaData (MetaDataHash)
import Shelley.Spec.Ledger.Slot (SlotNo (..))
import Shelley.Spec.Ledger.Tx (TxIn (..), TxOut (..), ValidateScript (..))
import Shelley.Spec.Ledger.TxBody (TxBody (TxBody, _inputs, _outputs, _txfee), Wdrl (..))
import Shelley.Spec.Ledger.UTxO (UTxO)
import Test.QuickCheck (Gen)
import Test.Shelley.Spec.Ledger.Generator.Constants(Constants (..))
import Test.Shelley.Spec.Ledger.Generator.Core
  ( EraGen (..),
    genNatural,
    genTxOut,
    genesisCoins,
  )
import Test.Shelley.Spec.Ledger.Generator.Presets(someKeyPairs)
import Test.Shelley.Spec.Ledger.Generator.Scripts
  ( ScriptClass,
    ValueClass(..),
    TxBodyClass(..),
    someScripts,
    genesisId,
  )
import Data.Proxy(Proxy(..))
-- ===============================================================

instance CC.Crypto c => EraGen (ShelleyEra c) where
  genEraUtxo0 = genUtxo0
  genEraTxBody = genTxBody

  updateEraTxBody body fee ins outs =
    body
      { _txfee = fee,
        _inputs = ins,
        _outputs = outs
      }

-- | Generate a transaction body with the given inputs/outputs and certificates
genTxBody ::
  (ShelleyBased era) =>
  SlotNo ->
  Set (TxIn era) ->
  StrictSeq (TxOut era) ->
  StrictSeq (DCert era) ->
  Wdrl era ->
  Coin ->
  StrictMaybe (Update era) ->
  StrictMaybe (MetaDataHash era) ->
  Gen (TxBody era)
genTxBody slot inputs outputs certs wdrls fee update mdHash = do
  ttl <- genTimeToLive slot
  return $
    TxBody
      inputs
      outputs
      certs
      wdrls
      fee
      ttl
      update
      mdHash

genTimeToLive :: SlotNo -> Gen SlotNo
genTimeToLive currentSlot = do
  ttl <- genNatural 50 200
  pure $ currentSlot + SlotNo (fromIntegral ttl)

genUtxo0 ::
  forall era.
  ( ScriptClass era,
    ValueClass era,
    TxBodyClass era,
    ShelleyBased era
  ) =>
  Constants ->
  Gen (UTxO era)
genUtxo0 c@Constants {minGenesisUTxOouts, maxGenesisUTxOouts, maxGenesisOutputVal, minGenesisOutputVal} = do
  genesisKeys <- someKeyPairs c minGenesisUTxOouts maxGenesisUTxOouts
  genesisScripts <- someScripts (Proxy @era) c minGenesisUTxOouts maxGenesisUTxOouts
  let scriptHashList = map (hashScript . fst) genesisScripts
  outs <-
    genTxOut
      (genValue @era scriptHashList minGenesisOutputVal maxGenesisOutputVal)
      (fmap (toAddr Testnet) genesisKeys ++ fmap (scriptsToAddr' Testnet) genesisScripts)
  return (genesisCoins (genesisId @era)  outs)
  where
    scriptsToAddr' :: Network -> (Core.Script era, Core.Script era) -> Addr era
    scriptsToAddr' n (payScript, stakeScript) =
      Addr n (scriptToCred' payScript) (StakeRefBase $ scriptToCred' stakeScript)
    scriptToCred' = ScriptHashObj . hashScript