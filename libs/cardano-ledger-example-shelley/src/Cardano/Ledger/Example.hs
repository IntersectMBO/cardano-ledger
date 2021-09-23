{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

-- | This is a stub example of a Shelley era, designed for testing,
-- prototyping, and demo purposes.
module Cardano.Ledger.Example where

import Cardano.Ledger.AuxiliaryData
  ( AuxiliaryDataHash (..),
    ValidateAuxiliaryData (..),
  )
import Cardano.Ledger.Coin (Coin)
import qualified Cardano.Ledger.Core as Core
import qualified Cardano.Ledger.Crypto as CryptoClass
import Cardano.Ledger.Era (Era (Crypto), SupportsSegWit (..), ValidateScript (..))
import Cardano.Ledger.Hashes (EraIndependentAuxiliaryData)
import Cardano.Ledger.SafeHash (makeHashWithExplicitProxys)
import Cardano.Ledger.Shelley.API
  ( AccountState (AccountState),
    ApplyBlock,
    ApplyTx,
    CanStartFromGenesis (..),
    Coin (Coin),
    DPState (DPState),
    DState (_genDelegs),
    EpochState (EpochState),
    GenDelegs (GenDelegs),
    GetLedgerView,
    LedgerState (LedgerState),
    NewEpochState (NewEpochState),
    PoolDistr (PoolDistr),
    PraosCrypto,
    ShelleyBasedEra,
    ShelleyGenesis (sgGenDelegs, sgMaxLovelaceSupply, sgProtocolParams),
    StrictMaybe (SNothing),
    TxOut (..),
    UTxOState (UTxOState),
    balance,
    genesisUTxO,
    word64ToCoin,
  )
import qualified Cardano.Ledger.Shelley.BlockChain as Shelley
  ( TxSeq (..),
    bbHash,
    txSeqTxns,
  )
import Cardano.Ledger.Shelley.Constraints (UsesPParams (..), UsesTxBody, UsesTxOut (..), UsesValue)
import Cardano.Ledger.Shelley.EpochBoundary (BlocksMade (..), emptySnapShots)
import Cardano.Ledger.Shelley.Metadata (Metadata (Metadata), validMetadatum)
import Cardano.Ledger.Shelley.PParams ()
import Cardano.Ledger.Shelley.PParams as Shelley
import qualified Cardano.Ledger.Shelley.PParams as SPP
import Cardano.Ledger.Shelley.Rules.Bbody (BBODY)
import Cardano.Ledger.Shelley.Rules.Deleg (DELEG)
import Cardano.Ledger.Shelley.Rules.Delegs (DELEGS)
import Cardano.Ledger.Shelley.Rules.Delpl (DELPL)
import Cardano.Ledger.Shelley.Rules.Epoch (EPOCH)
import Cardano.Ledger.Shelley.Rules.Ledger (LEDGER)
import Cardano.Ledger.Shelley.Rules.Ledgers (LEDGERS)
import Cardano.Ledger.Shelley.Rules.Mir (MIR)
import Cardano.Ledger.Shelley.Rules.NewEpoch (NEWEPOCH)
import Cardano.Ledger.Shelley.Rules.Newpp (NEWPP)
import Cardano.Ledger.Shelley.Rules.Pool (POOL)
import Cardano.Ledger.Shelley.Rules.PoolReap (POOLREAP)
import Cardano.Ledger.Shelley.Rules.Ppup (PPUP)
import Cardano.Ledger.Shelley.Rules.Rupd (RUPD)
import Cardano.Ledger.Shelley.Rules.Snap (SNAP)
import Cardano.Ledger.Shelley.Rules.Tick (TICK, TICKF)
import Cardano.Ledger.Shelley.Rules.Tickn (TICKN)
import Cardano.Ledger.Shelley.Rules.Upec (UPEC)
import Cardano.Ledger.Shelley.Rules.Utxo (UTXO)
import Cardano.Ledger.Shelley.Rules.Utxow (UTXOW)
import Cardano.Ledger.Shelley.Scripts (MultiSig)
import Cardano.Ledger.Shelley.Tx
  ( Tx,
    WitnessSet,
    validateNativeMultiSigScript,
  )
import Cardano.Ledger.Shelley.TxBody (TxBody (..))
import Cardano.Ledger.Val (Val ((<->)))
import Cardano.Protocol.TPraos.Rules.OCert (OCERT)
import Cardano.Protocol.TPraos.Rules.Overlay (OVERLAY)
import qualified Data.ByteString as BS
import Data.Default.Class (def)
import qualified Data.Map.Strict as Map
import Data.Proxy

data ExampleEra c

instance CryptoClass.Crypto c => Era (ExampleEra c) where
  type Crypto (ExampleEra c) = c

instance CryptoClass.Crypto c => UsesValue (ExampleEra c)

instance CryptoClass.Crypto c => UsesTxOut (ExampleEra c) where
  makeTxOut _ a v = TxOut a v

--------------------------------------------------------------------------------
-- Core instances
--------------------------------------------------------------------------------

type instance Core.Tx (ExampleEra c) = Tx (ExampleEra c)

type instance Core.Value (ExampleEra _c) = Coin

type instance Core.TxBody (ExampleEra c) = TxBody (ExampleEra c)

type instance Core.TxOut (ExampleEra c) = TxOut (ExampleEra c)

type instance Core.Script (ExampleEra c) = MultiSig c

type instance Core.AuxiliaryData (ExampleEra c) = Metadata (ExampleEra c)

type instance Core.PParams (ExampleEra c) = SPP.PParams (ExampleEra c)

type instance Core.PParamsDelta (ExampleEra c) = SPP.PParamsUpdate (ExampleEra c)

type instance Core.Witnesses (ExampleEra c) = WitnessSet (ExampleEra c)

--------------------------------------------------------------------------------
-- Ledger data instances
--------------------------------------------------------------------------------

instance
  (CryptoClass.Crypto c) =>
  UsesPParams (ExampleEra c)
  where
  mergePPUpdates _ = SPP.updatePParams

nativeMultiSigTag :: BS.ByteString
nativeMultiSigTag = "\00"

instance
  (CryptoClass.Crypto c, UsesTxBody (ExampleEra c)) =>
  ValidateScript (ExampleEra c)
  where
  validateScript = validateNativeMultiSigScript
  scriptPrefixTag _script = nativeMultiSigTag

instance CryptoClass.Crypto c => SupportsSegWit (ExampleEra c) where
  type TxSeq (ExampleEra c) = Shelley.TxSeq (ExampleEra c)
  fromTxSeq = Shelley.txSeqTxns
  toTxSeq = Shelley.TxSeq
  hashTxSeq = Shelley.bbHash
  numSegComponents = 3

instance CryptoClass.Crypto c => ValidateAuxiliaryData (ExampleEra c) c where
  hashAuxiliaryData metadata = AuxiliaryDataHash (makeHashWithExplicitProxys (Proxy @c) index metadata)
    where
      index = Proxy @EraIndependentAuxiliaryData
  validateAuxiliaryData (Metadata m) = all validMetadatum m

instance PraosCrypto c => ApplyTx (ExampleEra c)

instance PraosCrypto c => ApplyBlock (ExampleEra c)

instance PraosCrypto c => GetLedgerView (ExampleEra c)

instance PraosCrypto c => ShelleyBasedEra (ExampleEra c)

instance (CryptoClass.Crypto c) => CanStartFromGenesis (ExampleEra c) where
  initialState sg () =
    NewEpochState
      initialEpochNo
      (BlocksMade Map.empty)
      (BlocksMade Map.empty)
      ( EpochState
          (AccountState (Coin 0) reserves)
          emptySnapShots
          ( LedgerState
              ( UTxOState
                  initialUtxo
                  (Coin 0)
                  (Coin 0)
                  def
              )
              (DPState (def {_genDelegs = GenDelegs genDelegs}) def)
          )
          pp
          pp
          def
      )
      SNothing
      (PoolDistr Map.empty)
    where
      initialEpochNo = 0
      initialUtxo = genesisUTxO sg
      reserves =
        word64ToCoin (sgMaxLovelaceSupply sg)
          <-> balance initialUtxo
      genDelegs = sgGenDelegs sg
      pp = sgProtocolParams sg

-- These rules are all inherited from Shelley
-- The types on the right are all instances of class STS, ultimately defined in Control.State.Transition.Extended
type instance Core.EraRule "BBODY" (ExampleEra c) = BBODY (ExampleEra c) -- Block body

type instance Core.EraRule "DELEG" (ExampleEra c) = DELEG (ExampleEra c)

type instance Core.EraRule "DELEGS" (ExampleEra c) = DELEGS (ExampleEra c)

type instance Core.EraRule "DELPL" (ExampleEra c) = DELPL (ExampleEra c)

type instance Core.EraRule "EPOCH" (ExampleEra c) = EPOCH (ExampleEra c)

type instance Core.EraRule "LEDGER" (ExampleEra c) = LEDGER (ExampleEra c)

type instance Core.EraRule "LEDGERS" (ExampleEra c) = LEDGERS (ExampleEra c)

type instance Core.EraRule "MIR" (ExampleEra c) = MIR (ExampleEra c)

type instance Core.EraRule "NEWEPOCH" (ExampleEra c) = NEWEPOCH (ExampleEra c)

type instance Core.EraRule "NEWPP" (ExampleEra c) = NEWPP (ExampleEra c)

type instance Core.EraRule "OCERT" (ExampleEra c) = OCERT (ExampleEra c)

type instance Core.EraRule "OVERLAY" (ExampleEra c) = OVERLAY (ExampleEra c)

type instance Core.EraRule "POOL" (ExampleEra c) = POOL (ExampleEra c)

type instance Core.EraRule "POOLREAP" (ExampleEra c) = POOLREAP (ExampleEra c)

type instance Core.EraRule "PPUP" (ExampleEra c) = PPUP (ExampleEra c)

type instance Core.EraRule "RUPD" (ExampleEra c) = RUPD (ExampleEra c)

type instance Core.EraRule "SNAP" (ExampleEra c) = SNAP (ExampleEra c)

type instance Core.EraRule "TICK" (ExampleEra c) = TICK (ExampleEra c)

type instance Core.EraRule "TICKF" (ExampleEra c) = TICKF (ExampleEra c)

type instance Core.EraRule "TICKN" (ExampleEra _c) = TICKN

type instance Core.EraRule "UPEC" (ExampleEra c) = UPEC (ExampleEra c)

type instance Core.EraRule "UTXO" (ExampleEra c) = UTXO (ExampleEra c)

type instance Core.EraRule "UTXOW" (ExampleEra c) = UTXOW (ExampleEra c)
