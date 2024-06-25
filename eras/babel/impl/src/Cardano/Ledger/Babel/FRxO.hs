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

module Cardano.Ledger.Babel.FRxO where

import Cardano.Ledger.Babel.TxBody (
  BabelEraTxBody (fulfillsTxBodyL, requestsTxBodyL, requiredTxsTxBodyL),
 )
import Cardano.Ledger.Binary (sizedValue)
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
  BabelEraTxBody era =>
  TxBody era ->
  FRxO era
txfrxo txBody =
  FRxO $
    Map.fromList
      [ (TxIn transId idx, out)
      | (out, idx) <-
          zip
            (toList $ fmap sizedValue $ txBody ^. requestsTxBodyL)
            [minBound ..]
      ]
  where
    transId = txIdTxBody txBody

txrequests :: BabelEraTxBody era => TxBody era -> SSeq.StrictSeq (TxOut era)
txrequests = fmap sizedValue . (^. requestsTxBodyL)

txrequired :: BabelEraTxBody era => TxBody era -> Set (TxIn (EraCrypto era))
txrequired = (^. requiredTxsTxBodyL)

txfulfills :: BabelEraTxBody era => TxBody era -> Set (TxIn (EraCrypto era))
txfulfills = (^. fulfillsTxBodyL)
