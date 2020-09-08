{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeSynonymInstances #-}

module Test.Shelley.Spec.Ledger.Rules.TestUtxow
  ( requiredMSigSignaturesSubset,
  )
where

import Control.State.Transition.Trace
  ( SourceSignalTarget,
    signal,
  )
import qualified Data.Set as Set (fromList, isSubsetOf, map)
import Shelley.Spec.Ledger.API
  ( UTXOW,
  )
import Shelley.Spec.Ledger.Tx
  ( addrWits,
    getKeyCombinations,
    msigWits,
    _witnessSet,
  )
import Shelley.Spec.Ledger.TxBody
  ( witKeyHash,
  )
import Test.QuickCheck (Property, conjoin)
import Test.Shelley.Spec.Ledger.ConcreteCryptoTypes
  ( C,
  )

--------------------------
-- Properties for UTXOW --
--------------------------

-- | Check for required signatures in case of Multi-Sig. There has to be one set
-- of possible signatures for a multi-sig script which is a sub-set of the
-- signatures of the tansaction.
--
-- TODO @mgudemann
-- This property is currenty disabled du to time-out problems with getting all
-- possible combinations for multi-sig.
requiredMSigSignaturesSubset :: [SourceSignalTarget (UTXOW C)] -> Property
requiredMSigSignaturesSubset tr =
  conjoin $
    map signaturesSubset tr
  where
    signaturesSubset sst =
      let khs = keyHashSet sst
       in all (existsReqKeyComb khs) (msigWits . _witnessSet $ signal sst)
    existsReqKeyComb keyHashes msig =
      any (\kl -> (Set.fromList kl) `Set.isSubsetOf` keyHashes) (getKeyCombinations msig)
    keyHashSet sst =
      Set.map witKeyHash (addrWits . _witnessSet $ signal sst)
