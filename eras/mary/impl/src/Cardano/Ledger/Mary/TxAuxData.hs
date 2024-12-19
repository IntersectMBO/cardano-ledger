{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Cardano.Ledger.Mary.TxAuxData (
  module Cardano.Ledger.Allegra.TxAuxData,
)
where

import Cardano.Ledger.Allegra.TxAuxData (
  AllegraEraTxAuxData (..),
  AllegraTxAuxData (..),
  metadataAllegraTxAuxDataL,
  timelockScriptsAllegraTxAuxDataL,
 )
import Cardano.Ledger.AuxiliaryData (AuxiliaryDataHash (..))
import Cardano.Ledger.Core
import Cardano.Ledger.Mary.Era (MaryEra)
import Cardano.Ledger.Mary.Scripts ()
import Cardano.Ledger.Shelley.TxAuxData (validMetadatum)
import Control.DeepSeq (deepseq)

-- =======================================

instance EraTxAuxData MaryEra where
  type TxAuxData MaryEra = AllegraTxAuxData MaryEra

  mkBasicTxAuxData = AllegraTxAuxData mempty mempty

  metadataTxAuxDataL = metadataAllegraTxAuxDataL

  upgradeTxAuxData (AllegraTxAuxData md scripts) = AllegraTxAuxData md $ upgradeScript <$> scripts

  validateTxAuxData _ (AllegraTxAuxData md as) = as `deepseq` all validMetadatum md

  hashTxAuxData aux = AuxiliaryDataHash (hashAnnotated aux)

instance AllegraEraTxAuxData MaryEra where
  timelockScriptsTxAuxDataL = timelockScriptsAllegraTxAuxDataL
