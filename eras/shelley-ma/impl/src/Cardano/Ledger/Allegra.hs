{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Cardano.Ledger.Allegra
  ( AllegraEra,
    Self,
    TxOut,
    TxBody,
    Value,
    Script,
    AuxiliaryData,
    PParams,
    PParamsDelta,
    Tx,
    Witnesses,
  )
where

import Cardano.Ledger.BaseTypes (BlocksMade (..))
import Cardano.Ledger.Core (Era (Crypto))
import qualified Cardano.Ledger.Crypto as CC
import Cardano.Ledger.Hashes (EraIndependentTxBody)
import Cardano.Ledger.Keys (DSignable)
import Cardano.Ledger.Shelley.API hiding (PParams, Tx, TxBody, TxOut, WitnessSet)
import Cardano.Ledger.Shelley.EpochBoundary (emptySnapShots)
import Cardano.Ledger.Shelley.LedgerState (minfee)
import Cardano.Ledger.Shelley.PParams (ShelleyPParamsUpdate)
import Cardano.Ledger.ShelleyMA
import Cardano.Ledger.ShelleyMA.Rules.Utxo (consumed)
import Cardano.Ledger.ShelleyMA.Rules.Utxow ()
import Cardano.Ledger.ShelleyMA.Timelocks (Timelock)
import Cardano.Ledger.ShelleyMA.TxBody ()
import Cardano.Ledger.Val (Val ((<->)))
import Data.Default.Class (def)
import qualified Data.Map.Strict as Map

type AllegraEra = ShelleyMAEra 'Allegra

--------------------------------------------------------------------------------
-- Mempool instances
--------------------------------------------------------------------------------

instance
  (CC.Crypto crypto, DSignable crypto (Hash crypto EraIndependentTxBody)) =>
  ApplyTx (AllegraEra crypto)

instance
  (CC.Crypto crypto, DSignable crypto (Hash crypto EraIndependentTxBody)) =>
  ApplyBlock (AllegraEra crypto)

instance CC.Crypto crypto => CanStartFromGenesis (AllegraEra crypto) where
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
                  (IStake mempty mempty)
              )
              (DPState (def {_genDelegs = GenDelegs genDelegs}) def)
          )
          pp
          pp
          def
      )
      SNothing
      (PoolDistr Map.empty)
      ()
    where
      initialEpochNo = 0
      initialUtxo = genesisUTxO sg
      reserves =
        word64ToCoin (sgMaxLovelaceSupply sg)
          <-> balance initialUtxo
      genDelegs = sgGenDelegs sg
      pp = sgProtocolParams sg

instance CC.Crypto c => CLI (AllegraEra c) where
  evaluateMinFee = minfee

  evaluateConsumed = consumed

  addKeyWitnesses = addShelleyKeyWitnesses

  evaluateMinLovelaceOutput pp _out = _minUTxOValue pp

-- Self-Describing type synomyms

type Self c = ShelleyMAEra 'Allegra c

{-# DEPRECATED Self "Use `MaryEra` instead" #-}

type Script era = Timelock (Crypto era)

{-# DEPRECATED Script "Use `Timelock` instead" #-}

type Value era = Coin

{-# DEPRECATED Value "Use `Coin` instead" #-}

type Witnesses era = ShelleyWitnesses (Crypto era)

{-# DEPRECATED Witnesses "Use `Timelock` instead" #-}

type PParamsDelta era = ShelleyPParamsUpdate era

{-# DEPRECATED PParamsDelta "Use `ShelleyPParamsUpdate` instead" #-}
