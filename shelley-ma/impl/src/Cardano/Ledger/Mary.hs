{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Cardano.Ledger.Mary where

import Cardano.Ledger.Crypto (Crypto)
import Cardano.Ledger.ShelleyMA
import Cardano.Ledger.ShelleyMA.Rules.EraMapping ()
import Cardano.Ledger.ShelleyMA.Rules.Utxo ()
import Cardano.Ledger.ShelleyMA.Rules.Utxow ()
import Cardano.Ledger.Val (Val ((<->)), coin, inject)
import Data.Default.Class (def)
import qualified Data.Map.Strict as Map
import Shelley.Spec.Ledger.API
import Shelley.Spec.Ledger.EpochBoundary (BlocksMade (..), emptySnapShots)

type MaryEra = ShelleyMAEra 'Mary

instance PraosCrypto c => ApplyTx (MaryEra c) UtxoEnv

instance PraosCrypto c => ApplyBlock (MaryEra c) UtxoEnv

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

instance PraosCrypto c => ShelleyBasedEra (MaryEra c) UtxoEnv
