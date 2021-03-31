{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
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

import Cardano.Ledger.Crypto (Crypto)
import qualified Cardano.Ledger.Era as E (Era (Crypto))
import qualified Cardano.Ledger.Mary.Value as V (Value)
import Cardano.Ledger.ShelleyMA
import Cardano.Ledger.ShelleyMA.Rules.EraMapping ()
import Cardano.Ledger.ShelleyMA.Rules.Utxo ()
import Cardano.Ledger.ShelleyMA.Rules.Utxow ()
import Cardano.Ledger.ShelleyMA.Timelocks (Timelock)
import Cardano.Ledger.Val (Val ((<->)), coin, inject)
import Data.Default.Class (def)
import qualified Data.Map.Strict as Map
import Shelley.Spec.Ledger.API hiding (TxBody)
import Shelley.Spec.Ledger.EpochBoundary (BlocksMade (..), emptySnapShots)
import qualified Shelley.Spec.Ledger.PParams as Shelley (PParamsUpdate)

instance PraosCrypto c => ApplyTx (MaryEra c)

instance PraosCrypto c => ApplyBlock (MaryEra c)

instance PraosCrypto c => GetLedgerView (MaryEra c)

instance
  ( Crypto c
  ) =>
  CanStartFromGenesis (MaryEra c)
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

instance PraosCrypto c => ShelleyBasedEra (MaryEra c)

-- Self-Describing type synomyms

type MaryEra c = ShelleyMAEra 'Mary c

type Self c = ShelleyMAEra 'Mary c

type Script era = Timelock (E.Crypto era)

type Value era = V.Value (E.Crypto era)

type PParamsDelta era = Shelley.PParamsUpdate era
