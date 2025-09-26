{-# LANGUAGE CPP #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ViewPatterns #-}

module Cardano.Ledger.AuxiliaryData
  {-# DEPRECATED "Use `Cardano.Ledger.Hashes.TxAuxDataHash` instead" #-} (
  AuxiliaryDataHash,
#if __GLASGOW_HASKELL__ >= 914
  data AuxiliaryDataHash,
#else
  pattern AuxiliaryDataHash,
#endif
  unsafeAuxiliaryDataHash,
) where

import Cardano.Ledger.Hashes

type AuxiliaryDataHash = TxAuxDataHash

pattern AuxiliaryDataHash :: SafeHash EraIndependentTxAuxData -> TxAuxDataHash
pattern AuxiliaryDataHash {unsafeAuxiliaryDataHash} <- (unTxAuxDataHash -> unsafeAuxiliaryDataHash)
  where
    AuxiliaryDataHash h = TxAuxDataHash h
