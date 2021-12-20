{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Test.Cardano.Ledger.Shelley.Generator.ShelleyEraGen (genCoin) where

import qualified Cardano.Crypto.DSIGN as DSIGN
import qualified Cardano.Crypto.KES as KES
import Cardano.Crypto.Util (SignableRepresentation)
import Cardano.Ledger.AuxiliaryData (AuxiliaryDataHash)
import Cardano.Ledger.BaseTypes (StrictMaybe (..))
import Cardano.Ledger.Crypto (DSIGN, KES)
import qualified Cardano.Ledger.Crypto as CC (Crypto)
import Cardano.Ledger.Era (Crypto)
import Cardano.Ledger.Shelley (ShelleyEra)
import Cardano.Ledger.Shelley.API
  ( Coin (..),
    DCert,
    Update,
  )
import Cardano.Ledger.Shelley.PParams (PParams, PParams' (..))
import Cardano.Ledger.Shelley.Rules.EraMapping ()
import Cardano.Ledger.Shelley.Scripts (MultiSig (..))
import Cardano.Ledger.Shelley.Tx
  ( Tx (..),
    TxIn (..),
    TxOut (..),
    pattern WitnessSet,
  )
import Cardano.Ledger.Shelley.TxBody (TxBody (TxBody, _inputs, _outputs, _txfee), Wdrl (..))
import Cardano.Ledger.Slot (SlotNo (..))
import Cardano.Ledger.Val ((<+>))
import Cardano.Protocol.TPraos.API (PraosCrypto)
import Control.Monad (replicateM)
import Data.Sequence.Strict (StrictSeq ((:|>)))
import Data.Set (Set)
import Test.Cardano.Ledger.Shelley.ConcreteCryptoTypes (Mock)
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
import Test.Cardano.Ledger.Shelley.Utils (ShelleyTest)
import Test.QuickCheck (Gen)

{------------------------------------------------------------------------------
  ShelleyEra instances for EraGen and ScriptClass
 -----------------------------------------------------------------------------}

instance
  ( PraosCrypto c,
    DSIGN.Signable (DSIGN c) ~ SignableRepresentation,
    KES.Signable (KES c) ~ SignableRepresentation
  ) =>
  EraGen (ShelleyEra c)
  where
  genGenesisValue
    ( GenEnv
        _keySpace
        _scriptspace
        Constants {minGenesisOutputVal, maxGenesisOutputVal}
      ) =
      genCoin minGenesisOutputVal maxGenesisOutputVal
  genEraTxBody _ge _utxo = genTxBody
  genEraAuxiliaryData = genMetadata

  updateEraTxBody _utxo _pp _wits body' fee ins out =
    body'
      { _txfee = fee,
        _inputs = (_inputs body') <> ins,
        _outputs = (_outputs body') :|> out
      }
  genEraPParamsDelta = genShelleyPParamsDelta
  genEraPParams = genPParams

  genEraWitnesses _ setWitVKey mapScriptWit = WitnessSet setWitVKey mapScriptWit mempty
  constructTx = Tx

instance CC.Crypto c => ScriptClass (ShelleyEra c) where
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
  ShelleyEra generators
 -----------------------------------------------------------------------------}

genTxBody ::
  (ShelleyTest era) =>
  PParams era ->
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

instance Mock c => MinGenTxout (ShelleyEra c) where
  calcEraMinUTxO _txout pp = (_minUTxOValue pp)
  addValToTxOut v (TxOut a u) = TxOut a (v <+> u)
  genEraTxOut _genenv genVal addrs = do
    values <- replicateM (length addrs) genVal
    let makeTxOut (addr, val) = TxOut addr val
    pure (makeTxOut <$> zip addrs values)
