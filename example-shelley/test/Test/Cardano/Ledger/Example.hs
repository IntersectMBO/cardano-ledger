{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Test.Cardano.Ledger.Example
  ( -- export EraGen instance for ExampleEra and helpers shared with MaryEra
    genCoin
  )
where

import qualified Cardano.Crypto.DSIGN as DSIGN
import qualified Cardano.Crypto.KES as KES
import Cardano.Crypto.Util (SignableRepresentation)
import Cardano.Ledger.AuxiliaryData (AuxiliaryDataHash)
import qualified Cardano.Ledger.Core as Core
import Cardano.Ledger.Crypto (DSIGN, KES)
import qualified Cardano.Ledger.Crypto as CC (Crypto)
import Cardano.Ledger.Era (Crypto)
import Cardano.Ledger.Example (ExampleEra)
import Data.Sequence.Strict (StrictSeq((:|>)))
import Data.Set (Set)
import Generic.Random (genericArbitraryU)
import Shelley.Spec.Ledger.API
  ( Coin (..),
    DCert,
    PraosCrypto,
    Update,
  )
import Cardano.Ledger.BaseTypes (StrictMaybe (..))
import Shelley.Spec.Ledger.STS.EraMapping ()
import Shelley.Spec.Ledger.Scripts (MultiSig (..))
import Cardano.Ledger.Slot (SlotNo (..))
import Shelley.Spec.Ledger.Tx
  ( TxIn (..),
    TxOut (..),
    WitnessSetHKD(WitnessSet),
  )
import Shelley.Spec.Ledger.TxBody (TxBody (TxBody, _inputs, _outputs, _txfee), Wdrl (..))
import qualified Shelley.Spec.Ledger.STS.Utxo as STS
import Test.QuickCheck
import Test.Shelley.Spec.Ledger.ConcreteCryptoTypes
import Test.Shelley.Spec.Ledger.Generator.Constants (Constants (..))
import Test.Shelley.Spec.Ledger.Generator.Core
  ( GenEnv (..),
    genCoin,
    genNatural,
  )
import Test.Shelley.Spec.Ledger.Generator.EraGen (EraGen (..))
import Test.Shelley.Spec.Ledger.Generator.Metadata (genMetadata)
import Test.Shelley.Spec.Ledger.Generator.ScriptClass
  ( Quantifier (..),
    ScriptClass (..),
  )
import Test.Shelley.Spec.Ledger.Generator.Trace.Chain ()
import Test.Shelley.Spec.Ledger.Serialisation.EraIndepGenerators ()
import Test.Shelley.Spec.Ledger.Utils (ShelleyTest)
import Test.Shelley.Spec.Ledger.Generator.Update(genShelleyPParamsDelta,genPParams)
import Shelley.Spec.Ledger.PParams(PParams'(..))
import Test.Shelley.Spec.Ledger.Generator.EraGen(MinGenTxout(..))
import Cardano.Ledger.Val((<+>))
import Control.Monad (replicateM)

{------------------------------------------------------------------------------
  ExampleEra instances for EraGen and ScriptClass
 -----------------------------------------------------------------------------}

instance
  ( PraosCrypto c,
    DSIGN.Signable (DSIGN c) ~ SignableRepresentation,
    KES.Signable (KES c) ~ SignableRepresentation,
    MinGenTxout(ExampleEra c)
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
  unsafeApplyTx x = x


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
     let  makeTxOut (addr,val) = TxOut addr val
     pure (makeTxOut <$> zip addrs values)