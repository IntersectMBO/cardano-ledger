{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilyDependencies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE UndecidableSuperClasses #-}

module Cardano.Ledger.Conway.FRxO where

import Cardano.Ledger.Conway.TxBody (
  ConwayEraTxBody (fulfillsTxBodyL, requestsTxBodyL, requiredTxsTxBodyL),
 )
import Cardano.Ledger.Core (
  Era (EraCrypto),
  EraTxBody (TxBody),
  EraTxOut (TxOut),
  txIdTxBody,
 )
import Cardano.Ledger.FRxO (FRxO (FRxO))
import Cardano.Ledger.TxIn (TxIn (TxIn))
import Data.Foldable (toList)
import qualified Data.Map as Map
import qualified Data.Sequence.Strict as SSeq
import Data.Set (Set)
import Lens.Micro ((^.))

-- | The unspent transaction outputs.
-- | Compute the transaction requests of a transaction.
-- TODO WG: Put this in the FRxO module (along with other helpers). Probably refactor so the actual logic is done on maps, then unwrap both UTxO and FRxO and call the functions you refactored.
txfrxo ::
  forall era.
  ConwayEraTxBody era =>
  TxBody era ->
  FRxO era
txfrxo txBody =
  FRxO $
    Map.fromList
      [ (TxIn transId idx, out)
      | (out, idx) <- zip (toList $ txBody ^. requestsTxBodyL) [minBound ..]
      ]
  where
    transId = txIdTxBody txBody

txrequests :: ConwayEraTxBody era => TxBody era -> SSeq.StrictSeq (TxOut era)
txrequests = (^. requestsTxBodyL)

txrequired :: ConwayEraTxBody era => TxBody era -> Set (TxIn (EraCrypto era))
txrequired = (^. requiredTxsTxBodyL)

txfulfills :: ConwayEraTxBody era => TxBody era -> Set (TxIn (EraCrypto era))
txfulfills = (^. fulfillsTxBodyL)
