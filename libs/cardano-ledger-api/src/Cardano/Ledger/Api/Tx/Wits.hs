{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE UndecidableSuperClasses #-}

module Cardano.Ledger.Api.Tx.Wits (
  -- * Shelley onwards
  EraTxWits (TxWits),
  mkBasicTxWits,

  -- ** Address witness
  addrTxWitsL,
  KeyRole (Witness),

  -- *** WitVKey
  WitVKey (WitVKey),
  witVKeyHash,

  -- ** Byron address witness
  bootAddrTxWitsL,
  BootstrapWitness,

  -- ** Script witness
  scriptTxWitsL,
  hashScriptTxWitsL,

  -- * Any era
  AnyEraTxWits (..),

  -- * Alonzo onwards
  AlonzoEraTxWits,
  datsTxWitsL,
  hashDataTxWitsL,
  TxDats (..),
  unTxDats,
  unTxDatsL,
  rdmrsTxWitsL,
  Redeemers (..),
  unRedeemers,
  unRedeemersL,
  PlutusPurpose,
  AlonzoPlutusPurpose (..),
  AsIx (..),
  AsItem (..),

  -- ** Conway
  ConwayPlutusPurpose (..),

  -- * Upgrade
  binaryUpgradeTxWits,
  upgradeTxWits,
) where

import Cardano.Ledger.Alonzo.Scripts (
  AlonzoPlutusPurpose (..),
  AsItem (..),
  AsIx (..),
  PlutusPurpose,
 )
import Cardano.Ledger.Alonzo.TxWits (
  AlonzoEraTxWits,
  Redeemers (..),
  TxDats (..),
  datsTxWitsL,
  hashDataTxWitsL,
  rdmrsTxWitsL,
  unRedeemers,
  unRedeemersL,
  unTxDats,
  unTxDatsL,
 )
import Cardano.Ledger.Api.Era
import Cardano.Ledger.Api.Scripts (AnyEraScript)
import Cardano.Ledger.Conway.Scripts (ConwayPlutusPurpose (..))
import Cardano.Ledger.Core (EraTxWits (..), binaryUpgradeTxWits, hashScriptTxWitsL)
import Cardano.Ledger.Keys (KeyRole (Witness))
import Cardano.Ledger.Keys.Bootstrap (BootstrapWitness)
import Cardano.Ledger.Keys.WitVKey (WitVKey (WitVKey), witVKeyHash)
import Lens.Micro

class (EraTxWits era, AnyEraScript era) => AnyEraTxWits era where
  datsTxWitsG :: SimpleGetter (TxWits era) (Maybe (TxDats era))
  default datsTxWitsG :: AlonzoEraTxWits era => SimpleGetter (TxWits era) (Maybe (TxDats era))
  datsTxWitsG = datsTxWitsL . to Just

  rdmrsTxWitsG :: SimpleGetter (TxWits era) (Maybe (Redeemers era))
  default rdmrsTxWitsG :: AlonzoEraTxWits era => SimpleGetter (TxWits era) (Maybe (Redeemers era))
  rdmrsTxWitsG = rdmrsTxWitsL . to Just

instance AnyEraTxWits ShelleyEra where
  datsTxWitsG = to (const Nothing)
  rdmrsTxWitsG = to (const Nothing)

instance AnyEraTxWits AllegraEra where
  datsTxWitsG = to (const Nothing)
  rdmrsTxWitsG = to (const Nothing)

instance AnyEraTxWits MaryEra where
  datsTxWitsG = to (const Nothing)
  rdmrsTxWitsG = to (const Nothing)

instance AnyEraTxWits AlonzoEra

instance AnyEraTxWits BabbageEra

instance AnyEraTxWits ConwayEra

instance AnyEraTxWits DijkstraEra
