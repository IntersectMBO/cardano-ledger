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
import Cardano.Ledger.Crypto (Crypto)
import qualified Cardano.Ledger.Crypto as CC
import qualified Cardano.Ledger.Era as E (Era (Crypto))
import Cardano.Ledger.Shelley.API hiding (PParams, Tx, TxBody, TxOut, WitnessSet)
import Cardano.Ledger.Shelley.EpochBoundary (emptySnapShots)
import Cardano.Ledger.Shelley.LedgerState (minfee)
import qualified Cardano.Ledger.Shelley.PParams as Shelley (PParamsUpdate)
import Cardano.Ledger.Shelley.Tx (WitnessSet)
import Cardano.Ledger.ShelleyMA
import Cardano.Ledger.ShelleyMA.Rules.EraMapping ()
import Cardano.Ledger.ShelleyMA.Rules.Utxo (consumed)
import Cardano.Ledger.ShelleyMA.Timelocks (Timelock)
import Cardano.Ledger.ShelleyMA.TxBody ()
import Cardano.Ledger.Val (Val ((<->)))
import Data.Default.Class (def)
import qualified Data.Map.Strict as Map

type AllegraEra = ShelleyMAEra 'Allegra

instance ShelleyEraCrypto c => ApplyTx (AllegraEra c)

instance ShelleyEraCrypto c => ApplyBlock (AllegraEra c)

instance
  ( Crypto c
  ) =>
  CanStartFromGenesis (AllegraEra c)
  where
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
    where
      initialEpochNo = 0
      initialUtxo = genesisUTxO sg
      reserves =
        word64ToCoin (sgMaxLovelaceSupply sg)
          <-> balance initialUtxo
      genDelegs = sgGenDelegs sg
      pp = sgProtocolParams sg

instance ShelleyEraCrypto c => ShelleyBasedEra (AllegraEra c)

instance CC.Crypto c => CLI (AllegraEra c) where
  evaluateMinFee = minfee

  evaluateConsumed = consumed

  addKeyWitnesses = addShelleyKeyWitnesses

  evaluateMinLovelaceOutput pp _out = _minUTxOValue pp

-- Self-Describing type synomyms

type Self c = ShelleyMAEra 'Allegra c

type Script era = Timelock (E.Crypto era)

type Value era = Coin

type Witnesses era = WitnessSet (E.Crypto era)

type PParamsDelta era = Shelley.PParamsUpdate era
