{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Cardano.Ledger.Allegra
  ( AllegraEra,
    Self,
    TxOut,
    Value,
    TxBody,
    Script,
    AuxiliaryData,
    PParams,
    Tx,
  )
where

import Cardano.Ledger.Crypto (Crypto)
import qualified Cardano.Ledger.Era as E (Era (Crypto))
import Cardano.Ledger.ShelleyMA
  ( AuxiliaryData,
    MaryOrAllegra (..),
    PParams,
    ShelleyMAEra,
    Tx,
    TxBody,
    TxOut,
  )
import Cardano.Ledger.ShelleyMA.Rules.EraMapping ()
import Cardano.Ledger.ShelleyMA.Timelocks (Timelock)
import Cardano.Ledger.ShelleyMA.TxBody ()
import Cardano.Ledger.Val (Val ((<->)))
import Data.Default.Class (def)
import qualified Data.Map.Strict as Map
import Shelley.Spec.Ledger.API hiding (PParams, Tx, TxBody, TxOut)
import Shelley.Spec.Ledger.EpochBoundary (BlocksMade (..), emptySnapShots)

type AllegraEra = ShelleyMAEra 'Allegra

instance PraosCrypto c => ApplyTx (AllegraEra c)

instance PraosCrypto c => ApplyBlock (AllegraEra c)

instance PraosCrypto c => GetLedgerView (AllegraEra c)

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

instance PraosCrypto c => ShelleyBasedEra (AllegraEra c)

-- Self-Describing type synomyms

type Self c = ShelleyMAEra 'Allegra c

type Script era = Timelock (E.Crypto era)

type Value era = Coin
