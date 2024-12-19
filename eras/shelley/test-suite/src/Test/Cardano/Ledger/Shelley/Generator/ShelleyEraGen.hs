{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Test.Cardano.Ledger.Shelley.Generator.ShelleyEraGen (genCoin) where

import Cardano.Ledger.BaseTypes (StrictMaybe (..))
import Cardano.Ledger.Core
import Cardano.Ledger.Shelley (ShelleyEra)
import Cardano.Ledger.Shelley.API (
  Coin (..),
  Update,
 )
import Cardano.Ledger.Shelley.Scripts (
  MultiSig,
  pattern RequireAllOf,
  pattern RequireAnyOf,
  pattern RequireMOf,
  pattern RequireSignature,
 )
import Cardano.Ledger.Shelley.TxBody (
  ShelleyTxBody (ShelleyTxBody, stbInputs, stbOutputs, stbTxFee),
  Withdrawals (..),
 )
import Cardano.Ledger.Shelley.TxOut (ShelleyTxOut (..))
import Cardano.Ledger.Shelley.TxWits (ShelleyTxWits (ShelleyTxWits))
import Cardano.Ledger.Slot (SlotNo (..))
import Cardano.Ledger.TxIn (TxIn (..))
import Cardano.Ledger.Val ((<+>))
import Control.Monad (replicateM)
import Data.Foldable (toList)
import Data.Sequence.Strict (StrictSeq ((:|>)), fromList)
import Data.Set (Set)
import Lens.Micro.Extras (view)
import Test.Cardano.Ledger.Shelley.Constants (Constants (..))
import Test.Cardano.Ledger.Shelley.Generator.Core (
  GenEnv (..),
  genCoin,
  genNatural,
 )
import Test.Cardano.Ledger.Shelley.Generator.EraGen (EraGen (..), MinGenTxout (..))
import Test.Cardano.Ledger.Shelley.Generator.ScriptClass (
  Quantifier (..),
  ScriptClass (..),
 )
import Test.Cardano.Ledger.Shelley.Generator.Trace.Chain ()
import Test.Cardano.Ledger.Shelley.Generator.TxAuxData (genMetadata)
import Test.Cardano.Ledger.Shelley.Generator.Update (genPParams, genShelleyPParamsUpdate)
import Test.QuickCheck (Gen)

{------------------------------------------------------------------------------
  ShelleyEra instances for EraGen and ScriptClass
 -----------------------------------------------------------------------------}

instance EraGen ShelleyEra where
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
      { stbTxFee = fee
      , stbInputs = stbInputs body' <> ins
      , stbOutputs = stbOutputs body' :|> out
      }
  genEraPParamsUpdate = genShelleyPParamsUpdate @ShelleyEra
  genEraPParams = genPParams

  genEraTxWits _ setWitVKey mapScriptWit = ShelleyTxWits setWitVKey mapScriptWit mempty

instance ScriptClass ShelleyEra where
  basescript _proxy = RequireSignature
  isKey _ (RequireSignature hk) = Just hk
  isKey _ _ = Nothing
  quantify _ (RequireAllOf xs) = AllOf (toList xs)
  quantify _ (RequireAnyOf xs) = AnyOf (toList xs)
  quantify _ (RequireMOf n xs) = MOf n (toList xs)
  quantify _ t = Leaf t
  unQuantify _ (AllOf xs) = RequireAllOf (fromList xs)
  unQuantify _ (AnyOf xs) = RequireAnyOf (fromList xs)
  unQuantify _ (MOf n xs) = RequireMOf n (fromList xs)
  unQuantify _ (Leaf t) = t

{------------------------------------------------------------------------------
  ShelleyEra generators
 -----------------------------------------------------------------------------}

genTxBody ::
  ( EraTxOut era
  , EraTxCert era
  ) =>
  PParams era ->
  SlotNo ->
  Set TxIn ->
  StrictSeq (TxOut era) ->
  StrictSeq (TxCert era) ->
  Withdrawals ->
  Coin ->
  StrictMaybe (Update era) ->
  StrictMaybe TxAuxDataHash ->
  Gen (ShelleyTxBody era, [MultiSig era])
genTxBody _pparams slot inputs outputs certs withdrawals fee update adHash = do
  ttl <- genTimeToLive slot
  return
    ( ShelleyTxBody
        inputs
        outputs
        certs
        withdrawals
        fee
        ttl
        update
        adHash
    , [] -- Shelley does not need any additional script witnesses
    )

genTimeToLive :: SlotNo -> Gen SlotNo
genTimeToLive currentSlot = do
  ttl <- genNatural 50 100
  pure $ currentSlot + SlotNo (fromIntegral ttl)

instance MinGenTxout ShelleyEra where
  calcEraMinUTxO _txout = view ppMinUTxOValueL
  addValToTxOut v (ShelleyTxOut a u) = ShelleyTxOut a (v <+> u)
  genEraTxOut _genenv genVal addrs = do
    values <- replicateM (length addrs) genVal
    pure (zipWith mkBasicTxOut addrs values)
