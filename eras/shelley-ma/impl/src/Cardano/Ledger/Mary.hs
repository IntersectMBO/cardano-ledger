{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Cardano.Ledger.Mary
  ( MaryEra,
    Self,
    ShelleyTx,
    ShelleyTxOut,
    MaryValue,
    MATxBody,
    ShelleyPParams,
    ShelleyPParamsUpdate,
    MAAuxiliaryData,

    -- * Deprecated
    Cardano.Ledger.Shelley.API.Tx,
    Cardano.Ledger.Shelley.API.TxOut,
    Cardano.Ledger.ShelleyMA.TxBody,
    Cardano.Ledger.Shelley.PParams.PParams,
    Cardano.Ledger.Mary.Value,
    Cardano.Ledger.Mary.Script,
    Cardano.Ledger.Mary.PParamsDelta,
    Cardano.Ledger.ShelleyMA.AuxiliaryData.AuxiliaryData,
  )
where

import Cardano.Ledger.BaseTypes (BlocksMade (..))
import qualified Cardano.Ledger.Core as Core
import qualified Cardano.Ledger.Crypto as CC
import Cardano.Ledger.Hashes (EraIndependentTxBody)
import Cardano.Ledger.Keys (DSignable)
import Cardano.Ledger.Mary.Value (MaryValue)
import Cardano.Ledger.Shelley.API hiding (TxBody)
import Cardano.Ledger.Shelley.EpochBoundary (emptySnapShots)
import Cardano.Ledger.Shelley.LedgerState (minfee)
import Cardano.Ledger.Shelley.PParams (ShelleyPParamsUpdate)
import qualified Cardano.Ledger.Shelley.PParams
import Cardano.Ledger.ShelleyMA
import Cardano.Ledger.ShelleyMA.AuxiliaryData (AuxiliaryData)
import Cardano.Ledger.ShelleyMA.Rules.Utxo (consumed, scaledMinDeposit)
import Cardano.Ledger.ShelleyMA.Rules.Utxow ()
import Cardano.Ledger.ShelleyMA.Timelocks (Timelock)
import Cardano.Ledger.Val (Val ((<->)), coin, inject)
import Data.Default.Class (def)
import qualified Data.Map.Strict as Map

instance
  (CC.Crypto crypto, DSignable crypto (Hash crypto EraIndependentTxBody)) =>
  ApplyTx (MaryEra crypto)

instance
  (CC.Crypto crypto, DSignable crypto (Hash crypto EraIndependentTxBody)) =>
  ApplyBlock (MaryEra crypto)

instance CC.Crypto c => CanStartFromGenesis (MaryEra c) where
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
        coin $
          inject (word64ToCoin (sgMaxLovelaceSupply sg))
            <-> balance initialUtxo
      genDelegs = sgGenDelegs sg
      pp = sgProtocolParams sg

instance CC.Crypto c => CLI (MaryEra c) where
  evaluateMinFee = minfee

  evaluateConsumed = consumed

  addKeyWitnesses = addShelleyKeyWitnesses

  evaluateMinLovelaceOutput pp (ShelleyTxOut _ v) = scaledMinDeposit v (_minUTxOValue pp)

-- Self-Describing type synomyms

type MaryEra c = ShelleyMAEra 'Mary c

type Self c = ShelleyMAEra 'Mary c

{-# DEPRECATED Self "Use `MaryEra` instead" #-}

type Script era = Timelock (Core.Crypto era)

{-# DEPRECATED Script "Use `Timelock` instead" #-}

type Value era = MaryValue (Core.Crypto era)

{-# DEPRECATED Value "Use `MaryValue` instead" #-}

type PParamsDelta era = ShelleyPParamsUpdate era

{-# DEPRECATED PParamsDelta "Use `ShelleyPParamsUpdate` instead" #-}
