{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ViewPatterns #-}

module Cardano.Ledger.AuxiliaryData
  {-# DEPRECATED "Use `Cardano.Ledger.Hashes.TxAuxDataHash` instead" #-} (
  AuxiliaryDataHash,
  pattern AuxiliaryDataHash,
  unsafeAuxiliaryDataHash,
)
where

import Cardano.Ledger.Hashes

type AuxiliaryDataHash = TxAuxDataHash

pattern AuxiliaryDataHash :: SafeHash EraIndependentTxAuxData -> TxAuxDataHash
pattern AuxiliaryDataHash {unsafeAuxiliaryDataHash} <- (unTxAuxDataHash -> unsafeAuxiliaryDataHash)
  where
    AuxiliaryDataHash h = TxAuxDataHash h
