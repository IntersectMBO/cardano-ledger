{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE UndecidableSuperClasses #-}

module Cardano.Ledger.Api.Tx.AuxData (
  EraTxAuxData (TxAuxData),
  mkBasicTxAuxData,
  metadataTxAuxDataL,
  hashTxAuxData,
  validateTxAuxData,

  -- * Any era
  AnyEraTxAuxData (..),

  -- * Shelley
  ShelleyTxAuxData (..),
  Metadatum (..),

  -- * Allegra
  AllegraEraTxAuxData,
  nativeScriptsTxAuxDataL,
  AllegraTxAuxData (..),

  -- * Alonzo
  AlonzoEraTxAuxData,
  plutusScriptsTxAuxDataL,
  Language (..),
  PlutusBinary (..),
  AlonzoTxAuxData (..),
  mkAlonzoTxAuxData,
  getAlonzoTxAuxDataScripts,

  -- * Upgrade
  binaryUpgradeTxAuxData,
  upgradeTxAuxData,
) where

import Cardano.Ledger.Allegra.TxAuxData (AllegraEraTxAuxData (..), AllegraTxAuxData (..))
import Cardano.Ledger.Alonzo.TxAuxData (
  AlonzoEraTxAuxData (..),
  AlonzoTxAuxData (..),
  getAlonzoTxAuxDataScripts,
  mkAlonzoTxAuxData,
 )
import Cardano.Ledger.Api.Era
import Cardano.Ledger.Core (EraTxAuxData (..), NativeScript, binaryUpgradeTxAuxData, hashTxAuxData)
import Cardano.Ledger.Plutus.Language (Language (..), PlutusBinary (..))
import Cardano.Ledger.Shelley.TxAuxData (Metadatum (..), ShelleyTxAuxData (..))
import Data.List.NonEmpty (NonEmpty)
import Data.Map (Map)
import Data.Sequence.Strict (StrictSeq)
import Lens.Micro

class EraTxAuxData era => AnyEraTxAuxData era where
  nativeScriptsTxAuxDataG :: SimpleGetter (TxAuxData era) (Maybe (StrictSeq (NativeScript era)))
  default nativeScriptsTxAuxDataG ::
    AllegraEraTxAuxData era =>
    SimpleGetter (TxAuxData era) (Maybe (StrictSeq (NativeScript era)))
  nativeScriptsTxAuxDataG = nativeScriptsTxAuxDataL . to Just

  plutusScriptsTxAuxDataG ::
    SimpleGetter (TxAuxData era) (Maybe (Map Language (NonEmpty PlutusBinary)))
  default plutusScriptsTxAuxDataG ::
    AlonzoEraTxAuxData era =>
    SimpleGetter (TxAuxData era) (Maybe (Map Language (NonEmpty PlutusBinary)))
  plutusScriptsTxAuxDataG = plutusScriptsTxAuxDataL . to Just

instance AnyEraTxAuxData ShelleyEra where
  nativeScriptsTxAuxDataG = to (const Nothing)
  plutusScriptsTxAuxDataG = to (const Nothing)

instance AnyEraTxAuxData AllegraEra where
  plutusScriptsTxAuxDataG = to (const Nothing)

instance AnyEraTxAuxData MaryEra where
  plutusScriptsTxAuxDataG = to (const Nothing)

instance AnyEraTxAuxData AlonzoEra

instance AnyEraTxAuxData BabbageEra

instance AnyEraTxAuxData ConwayEra

instance AnyEraTxAuxData DijkstraEra
