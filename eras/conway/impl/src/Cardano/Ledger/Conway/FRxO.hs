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

-- -- | Compute the UTxO inputs of a transaction.
-- -- txins has the same problems as txouts, see notes below.
-- txins ::
--   EraTxBody era =>
--   TxBody era ->
--   Set (TxIn (EraCrypto era))
-- txins = (^. inputsTxBodyL)

-- -- | Compute the transaction outputs of a transaction.
-- txouts ::
--   forall era.
--   EraTxBody era =>
--   TxBody era ->
--   UTxO era
-- txouts txBody =
--   UTxO $
--     Map.fromList
--       [ (TxIn transId idx, out)
--       | (out, idx) <- zip (toList $ txBody ^. outputsTxBodyL) [minBound ..]
--       ]
--   where
--     transId = txIdTxBody txBody

-- -- | Lookup a txin for a given UTxO collection
-- txinLookup ::
--   TxIn (EraCrypto era) ->
--   UTxO era ->
--   Maybe (TxOut era)
-- txinLookup txin (UTxO utxo') = Map.lookup txin utxo'

-- -- | Filter out TxIn's from the `UTxO` map
-- txInsFilter ::
--   -- | Source `UTxO`
--   UTxO era ->
--   -- | Which of the `TxIn`s you would like to keep.
--   Set (TxIn (EraCrypto era)) ->
--   UTxO era
-- txInsFilter (UTxO utxo') txIns = UTxO (utxo' `Map.restrictKeys` txIns)

-- -- | Verify a transaction body witness
-- verifyWitVKey ::
--   ( Typeable kr
--   , Crypto c
--   , DSignable c (Hash c EraIndependentTxBody)
--   ) =>
--   Hash c EraIndependentTxBody ->
--   WitVKey kr c ->
--   Bool
-- verifyWitVKey txbodyHash (WitVKey vkey sig) = verifySignedDSIGN vkey txbodyHash (coerce sig)
-- {-# INLINE verifyWitVKey #-}

-- -- | Determine the total balance contained in the UTxO.
-- balance :: EraTxOut era => UTxO era -> Value era
-- balance = sumAllValue . unUTxO
-- {-# INLINE balance #-}

-- -- | Determine the total Ada only balance contained in the UTxO. This is
-- -- equivalent to `coin . balance`, but it will be more efficient
-- coinBalance :: EraTxOut era => UTxO era -> Coin
-- coinBalance = sumAllCoin . unUTxO
-- {-# INLINE coinBalance #-}

-- -- | Sum all the value in any Foldable with 'TxOut's
-- sumAllValue :: (EraTxOut era, Foldable f) => f (TxOut era) -> Value era
-- sumAllValue = foldMap' (^. valueTxOutL)
-- {-# INLINE sumAllValue #-}

-- -- | Sum all the 'Coin's in any Foldable with with 'TxOut's. Care should be
-- -- taken since it is susceptible to integer overflow, therefore make sure this
-- -- function is not applied to unvalidated 'TxOut's
-- sumAllCoin :: (EraTxOut era, Foldable f) => f (TxOut era) -> Coin
-- sumAllCoin = fromCompact . CompactCoin . getSum . foldMap' getCoinWord64
--   where
--     getCoinWord64 txOut =
--       case txOut ^. compactCoinTxOutL of
--         CompactCoin w64 -> Sum w64
-- {-# INLINE sumAllCoin #-}

-- -- | Check whether any of the supplied 'TxOut's contain any MultiAssets. Returns
-- -- True if non of them do.
-- areAllAdaOnly :: (EraTxOut era, Foldable f) => f (TxOut era) -> Bool
-- areAllAdaOnly = all (^. isAdaOnlyTxOutF)
-- {-# INLINE areAllAdaOnly #-}

-- -- | Extract script hash from value address with script.
-- getScriptHash :: Addr c -> Maybe (ScriptHash c)
-- getScriptHash (Addr _ (ScriptHashObj hs) _) = Just hs
-- getScriptHash _ = Nothing

-- -- | The only reason it is a newtype instead of just a Map is becuase for later eras is
-- -- expensive to compute the actual map, so we want to use the type safety guidance to
-- -- avoid redundant work.
-- newtype ScriptsProvided era = ScriptsProvided
--   { unScriptsProvided :: Map.Map (ScriptHash (EraCrypto era)) (Script era)
--   }
--   deriving (Generic)

-- deriving instance (Era era, Eq (Script era)) => Eq (ScriptsProvided era)
-- deriving instance (Era era, Ord (Script era)) => Ord (ScriptsProvided era)
-- deriving instance (Era era, Show (Script era)) => Show (ScriptsProvided era)
-- deriving instance (Era era, NFData (Script era)) => NFData (ScriptsProvided era)

-- class EraTx era => EraUTxO era where
--   -- | A customizable type on per era basis for the information required to find all
--   -- scripts needed for the transaction.
--   type ScriptsNeeded era = (r :: Type) | r -> era

--   -- | Calculate all the value that is being consumed by the transaction.
--   getConsumedValue ::
--     PParams era ->
--     -- | Function that can lookup current delegation deposits
--     (Credential 'Staking (EraCrypto era) -> Maybe Coin) ->
--     -- | Function that can lookup current drep deposits
--     (Credential 'DRepRole (EraCrypto era) -> Maybe Coin) ->
--     UTxO era ->
--     TxBody era ->
--     Value era

--   getProducedValue ::
--     PParams era ->
--     -- | Check whether a pool with a supplied PoolStakeId is already registered.
--     (KeyHash 'StakePool (EraCrypto era) -> Bool) ->
--     TxBody era ->
--     Value era

--   -- | Initial eras will look into witness set to find all of the available scripts, but
--   -- starting with Babbage we can look for available scripts in the UTxO using reference
--   -- inputs.
--   getScriptsProvided ::
--     -- | For some era it is necessary to look into the UTxO to find all of the available
--     -- scripts for the transaction
--     UTxO era ->
--     Tx era ->
--     ScriptsProvided era

--   -- | Produce all the information required for figuring out which scripts are required
--   -- for the transaction to be valid, once those scripts are evaluated
--   getScriptsNeeded :: UTxO era -> TxBody era -> ScriptsNeeded era

--   -- | Extract the set of all script hashes that are needed for script validation.
--   getScriptsHashesNeeded :: ScriptsNeeded era -> Set (ScriptHash (EraCrypto era))

--   -- | Extract all of the KeyHash witnesses that are required for validating the transaction
--   getWitsVKeyNeeded ::
--     CertState era -> UTxO era -> TxBody era -> Set (KeyHash 'Witness (EraCrypto era))

--   -- | Minimum fee computation, excluding witnesses and including ref scripts size
--   getMinFeeTxUtxo :: PParams era -> Tx era -> UTxO era -> Coin
