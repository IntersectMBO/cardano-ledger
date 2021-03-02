{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Cardano.Ledger.Allegra where

import Cardano.Ledger.CoreUtxow (CoreUtxow (..))
import Cardano.Ledger.Crypto (Crypto)
import Cardano.Ledger.ShelleyMA
import Cardano.Ledger.ShelleyMA.Rules.EraMapping ()
import Cardano.Ledger.ShelleyMA.TxBody (TxBody)
import Cardano.Ledger.Val (Val ((<->)))
import Data.Default.Class (def)
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import GHC.Records (HasField (..))
import Shelley.Spec.Ledger.API hiding (TxBody)
import Shelley.Spec.Ledger.CompactAddr (decompactAddr)
import Shelley.Spec.Ledger.EpochBoundary (BlocksMade (..), emptySnapShots)
import Shelley.Spec.Ledger.Tx (WitnessSetHKD (..))

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

instance Crypto c => CoreUtxow (ShelleyMAEra 'Allegra c) Tx TxBody WitnessSet TxOut where
  bodyTx (Tx' body _wit _meta _) = body
  witTx (Tx' _body wit _meta _) = wit
  metaTx (Tx' _body _wit meta _) = meta
  addrWit x = addrWits' x
  bootWit x = bootWits' x
  scriptWit x = scriptWits' x
  updateBody x = getField @"update" x
  wdrlsBody x = getField @"wdrls" x
  certsBody x = getField @"certs" x
  inputsBody x = getField @"inputs" x
  mintBody _ = Set.empty
  adHashBody x = getField @"adHash" x
  addressOut (TxOutCompact ca _) = decompactAddr ca
