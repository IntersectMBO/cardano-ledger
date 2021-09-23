{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Test.Cardano.Ledger.Example
  ( -- export EraGen instance for ExampleEra and helpers shared with MaryEra
    genCoin,
  )
where

import qualified Cardano.Crypto.DSIGN as DSIGN
import qualified Cardano.Crypto.KES as KES
import Cardano.Crypto.Util (SignableRepresentation)
import Cardano.Ledger.AuxiliaryData (AuxiliaryDataHash)
import Cardano.Ledger.BaseTypes (StrictMaybe (..))
import qualified Cardano.Ledger.Core as Core
import Cardano.Ledger.Crypto (DSIGN, KES)
import qualified Cardano.Ledger.Crypto as CC (Crypto)
import Cardano.Ledger.Era (Crypto)
import Cardano.Ledger.Example (ExampleEra)
import Cardano.Ledger.Shelley.API
  ( Coin (..),
    DCert,
    PraosCrypto,
    Update,
  )
import Cardano.Ledger.Shelley.PParams (PParams' (..))
import Cardano.Ledger.Shelley.Rules.EraMapping ()
import qualified Cardano.Ledger.Shelley.Rules.Utxo as STS
import Cardano.Ledger.Shelley.Scripts (MultiSig (..))
import Cardano.Ledger.Shelley.Tx
  ( TxIn (..),
    TxOut (..),
    WitnessSetHKD (WitnessSet),
    pattern Tx,
  )
import Cardano.Ledger.Shelley.TxBody (TxBody (TxBody, _inputs, _outputs, _txfee), Wdrl (..))
import Cardano.Ledger.Slot (SlotNo (..))
import Cardano.Ledger.Val ((<+>))
import Control.Monad (replicateM)
import Data.Sequence.Strict (StrictSeq ((:|>)))
import Data.Set (Set)
import Generic.Random (genericArbitraryU)
import Test.Cardano.Ledger.Shelley.ConcreteCryptoTypes
import Test.Cardano.Ledger.Shelley.Generator.Constants (Constants (..))
import Test.Cardano.Ledger.Shelley.Generator.Core
  ( GenEnv (..),
    genCoin,
    genNatural,
  )
import Test.Cardano.Ledger.Shelley.Generator.EraGen (EraGen (..), MinGenTxout (..))
import Test.Cardano.Ledger.Shelley.Generator.Metadata (genMetadata)
import Test.Cardano.Ledger.Shelley.Generator.ScriptClass
  ( Quantifier (..),
    ScriptClass (..),
  )
import Test.Cardano.Ledger.Shelley.Generator.Trace.Chain ()
import Test.Cardano.Ledger.Shelley.Generator.Update (genPParams, genShelleyPParamsDelta)
import Test.Cardano.Ledger.Shelley.Serialisation.EraIndepGenerators ()
import Test.Cardano.Ledger.Shelley.Utils (ShelleyTest)
import Test.QuickCheck

{------------------------------------------------------------------------------
  ExampleEra instances for EraGen and ScriptClass
 -----------------------------------------------------------------------------}

instance
  ( PraosCrypto c,
    DSIGN.Signable (DSIGN c) ~ SignableRepresentation,
    KES.Signable (KES c) ~ SignableRepresentation,
    MinGenTxout (ExampleEra c)
  ) =>
  EraGen (ExampleEra c)
  where
  genGenesisValue
    ( GenEnv
        _keySpace
        _ScriptSpace
        Constants {minGenesisOutputVal, maxGenesisOutputVal}
      ) =
      genCoin minGenesisOutputVal maxGenesisOutputVal
  genEraTxBody _ge _utxo = genTxBody
  genEraAuxiliaryData = genMetadata
  genEraPParamsDelta = genShelleyPParamsDelta
  genEraPParams = genPParams
  genEraWitnesses _triple setWitVKey mapScriptWit = WitnessSet setWitVKey mapScriptWit mempty

  updateEraTxBody _utxo _pp _wits body fee ins out =
    body
      { _txfee = fee,
        _inputs = (_inputs body) <> ins,
        _outputs = (_outputs body) :|> out
      }
  constructTx = Tx

instance CC.Crypto c => ScriptClass (ExampleEra c) where
  basescript _proxy = RequireSignature
  isKey _ (RequireSignature hk) = Just hk
  isKey _ _ = Nothing
  quantify _ (RequireAllOf xs) = AllOf xs
  quantify _ (RequireAnyOf xs) = AnyOf xs
  quantify _ (RequireMOf n xs) = MOf n xs
  quantify _ t = Leaf t
  unQuantify _ (AllOf xs) = RequireAllOf xs
  unQuantify _ (AnyOf xs) = RequireAnyOf xs
  unQuantify _ (MOf n xs) = RequireMOf n xs
  unQuantify _ (Leaf t) = t

{------------------------------------------------------------------------------
  ExampleEra generators
 -----------------------------------------------------------------------------}

genTxBody ::
  (ShelleyTest era) =>
  Core.PParams era ->
  SlotNo ->
  Set (TxIn (Crypto era)) ->
  StrictSeq (TxOut era) ->
  StrictSeq (DCert (Crypto era)) ->
  Wdrl (Crypto era) ->
  Coin ->
  StrictMaybe (Update era) ->
  StrictMaybe (AuxiliaryDataHash (Crypto era)) ->
  Gen (TxBody era, [MultiSig (Crypto era)])
genTxBody _pparams slot inputs outputs certs wdrls fee update adHash = do
  ttl <- genTimeToLive slot
  return
    ( TxBody
        inputs
        outputs
        certs
        wdrls
        fee
        ttl
        update
        adHash,
      [] -- Shelley does not need any additional script witnesses
    )

genTimeToLive :: SlotNo -> Gen SlotNo
genTimeToLive currentSlot = do
  ttl <- genNatural 50 100
  pure $ currentSlot + SlotNo (fromIntegral ttl)

instance Mock c => Arbitrary (TxBody (ExampleEra c)) where
  arbitrary =
    TxBody
      <$> arbitrary
      <*> arbitrary
      <*> arbitrary
      <*> arbitrary
      <*> arbitrary
      <*> arbitrary
      <*> arbitrary
      <*> arbitrary

instance Mock c => Arbitrary (STS.UtxoPredicateFailure (ExampleEra c)) where
  arbitrary = genericArbitraryU
  shrink _ = []

instance Mock c => MinGenTxout (ExampleEra c) where
  calcEraMinUTxO _txout pp = (_minUTxOValue pp)
  addValToTxOut v (TxOut a u) = TxOut a (v <+> u)
  genEraTxOut _genenv genVal addrs = do
    values <- replicateM (length addrs) genVal
    let makeTxOut (addr, val) = TxOut addr val
    pure (makeTxOut <$> zip addrs values)
