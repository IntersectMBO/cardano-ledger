{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Cardano.Ledger.Mary
  ( MaryEra,
    Self,
    TxOut,
    Value,
    TxBody,
    Script,
    AuxiliaryData,
    PParams,
    PParamsDelta,
    Tx,
  )
where

import Cardano.Ledger.BaseTypes (BlocksMade (..))
import Cardano.Ledger.Crypto (Crypto)
import qualified Cardano.Ledger.Crypto as CC
import qualified Cardano.Ledger.Era as E (Era (Crypto))
import qualified Cardano.Ledger.Mary.Value as V (Value)
import Cardano.Ledger.Shelley.API hiding (TxBody)
import Cardano.Ledger.Shelley.EpochBoundary (emptySnapShots)
import Cardano.Ledger.Shelley.LedgerState (minfee)
import qualified Cardano.Ledger.Shelley.PParams as Shelley (PParamsUpdate)
import Cardano.Ledger.ShelleyMA
import Cardano.Ledger.ShelleyMA.Rules.EraMapping ()
import Cardano.Ledger.ShelleyMA.Rules.Utxo (consumed, scaledMinDeposit)
import Cardano.Ledger.ShelleyMA.Rules.Utxow ()
import Cardano.Ledger.ShelleyMA.Timelocks (Timelock)
import Cardano.Ledger.Val (Val ((<->)), coin, inject)
import Data.Default.Class (def)
import qualified Data.Map.Strict as Map

instance ShelleyEraCrypto c => ApplyTx (MaryEra c)

instance ShelleyEraCrypto c => ApplyBlock (MaryEra c)

instance Crypto c => CanStartFromGenesis (MaryEra c) where
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
        coin $
          inject (word64ToCoin (sgMaxLovelaceSupply sg))
            <-> balance initialUtxo
      genDelegs = sgGenDelegs sg
      pp = sgProtocolParams sg

instance ShelleyEraCrypto c => ShelleyBasedEra (MaryEra c)

instance CC.Crypto c => CLI (MaryEra c) where
  evaluateMinFee = minfee

  evaluateConsumed = consumed

  addKeyWitnesses = addShelleyKeyWitnesses

  evaluateMinLovelaceOutput pp (TxOut _ v) = scaledMinDeposit v (_minUTxOValue pp)

-- Self-Describing type synomyms

type MaryEra c = ShelleyMAEra 'Mary c

type Self c = ShelleyMAEra 'Mary c

type Script era = Timelock (E.Crypto era)

type Value era = V.Value (E.Crypto era)

type PParamsDelta era = Shelley.PParamsUpdate era
