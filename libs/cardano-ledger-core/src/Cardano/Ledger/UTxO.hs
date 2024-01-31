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

module Cardano.Ledger.UTxO (
  -- * Primitives
  UTxO (..),
  EraUTxO (..),
  ScriptsProvided (..),

  -- * Functions
  txins,
  txinLookup,
  txInsFilter,
  txouts,
  balance,
  coinBalance,
  sumAllValue,
  sumAllCoin,
  areAllAdaOnly,
  verifyWitVKey,
  getScriptHash,
)
where

import Cardano.Ledger.Address (Addr (..))
import Cardano.Ledger.Binary (
  DecCBOR (..),
  DecShareCBOR (Share, decShareCBOR),
  EncCBOR (..),
  FromCBOR (..),
  Interns,
  ToCBOR (..),
  decodeMap,
 )
import Cardano.Ledger.CertState (CertState)
import Cardano.Ledger.Coin (Coin, CompactForm (CompactCoin))
import Cardano.Ledger.Compactible (Compactible (..))
import Cardano.Ledger.Core
import Cardano.Ledger.Credential (Credential (..))
import Cardano.Ledger.Crypto (Crypto)
import Cardano.Ledger.Keys (
  DSignable,
  Hash,
  KeyHash,
  KeyRole (..),
  verifySignedDSIGN,
 )
import Cardano.Ledger.Keys.WitVKey (WitVKey (..))
import Cardano.Ledger.TxIn (TxIn (..))
import Control.DeepSeq (NFData)
import Control.Monad ((<$!>))
import Data.Aeson (ToJSON)
import Data.Coerce (coerce)
import Data.Default.Class (Default)
import Data.Foldable (foldMap', toList)
import Data.Kind (Type)
import qualified Data.Map.Strict as Map
import Data.Monoid (Sum (..))
import Data.Set (Set)
import Data.Typeable (Typeable)
import GHC.Generics (Generic)
import Lens.Micro ((^.))
import NoThunks.Class (NoThunks (..))
import Quiet (Quiet (Quiet))

-- ===============================================

-- | The unspent transaction outputs.
newtype UTxO era = UTxO {unUTxO :: Map.Map (TxIn (EraCrypto era)) (TxOut era)}
  deriving (Default, Generic, Semigroup)

instance (EncCBOR (TxOut era), Era era) => ToCBOR (UTxO era) where
  toCBOR = toEraCBOR @era

instance (DecCBOR (TxOut era), Era era) => FromCBOR (UTxO era) where
  fromCBOR = fromEraCBOR @era

deriving instance NoThunks (TxOut era) => NoThunks (UTxO era)

deriving instance (Era era, NFData (TxOut era)) => NFData (UTxO era)

deriving newtype instance (Era era, Eq (TxOut era)) => Eq (UTxO era)

deriving newtype instance Era era => Monoid (UTxO era)

deriving newtype instance (Era era, EncCBOR (TxOut era)) => EncCBOR (UTxO era)

deriving newtype instance (Era era, DecCBOR (TxOut era)) => DecCBOR (UTxO era)

instance
  ( Crypto (EraCrypto era)
  , DecShareCBOR (TxOut era)
  , Share (TxOut era) ~ Interns (Credential 'Staking (EraCrypto era))
  ) =>
  DecShareCBOR (UTxO era)
  where
  type
    Share (UTxO era) =
      Interns (Credential 'Staking (EraCrypto era))
  decShareCBOR credsInterns =
    UTxO <$!> decodeMap decCBOR (decShareCBOR credsInterns)

deriving via
  Quiet (UTxO era)
  instance
    (Show (TxOut era), Crypto (EraCrypto era)) => Show (UTxO era)

deriving newtype instance (Era era, ToJSON (TxOut era)) => ToJSON (UTxO era)

-- | Compute the UTxO inputs of a transaction.
-- txins has the same problems as txouts, see notes below.
txins ::
  EraTxBody era =>
  TxBody era ->
  Set (TxIn (EraCrypto era))
txins = (^. inputsTxBodyL)

-- | Compute the transaction outputs of a transaction.
txouts ::
  forall era.
  EraTxBody era =>
  TxBody era ->
  UTxO era
txouts txBody =
  UTxO $
    Map.fromList
      [ (TxIn transId idx, out)
      | (out, idx) <- zip (toList $ txBody ^. outputsTxBodyL) [minBound ..]
      ]
  where
    transId = txIdTxBody txBody

-- | Lookup a txin for a given UTxO collection
txinLookup ::
  TxIn (EraCrypto era) ->
  UTxO era ->
  Maybe (TxOut era)
txinLookup txin (UTxO utxo') = Map.lookup txin utxo'

-- | Filter out TxIn's from the `UTxO` map
txInsFilter ::
  -- | Source `UTxO`
  UTxO era ->
  -- | Which of the `TxIn`s you would like to keep.
  Set (TxIn (EraCrypto era)) ->
  UTxO era
txInsFilter (UTxO utxo') txIns = UTxO (utxo' `Map.restrictKeys` txIns)

-- | Verify a transaction body witness
verifyWitVKey ::
  ( Typeable kr
  , Crypto c
  , DSignable c (Hash c EraIndependentTxBody)
  ) =>
  Hash c EraIndependentTxBody ->
  WitVKey kr c ->
  Bool
verifyWitVKey txbodyHash (WitVKey vkey sig) = verifySignedDSIGN vkey txbodyHash (coerce sig)
{-# INLINE verifyWitVKey #-}

-- | Determine the total balance contained in the UTxO.
balance :: EraTxOut era => UTxO era -> Value era
balance = sumAllValue . unUTxO
{-# INLINE balance #-}

-- | Determine the total Ada only balance contained in the UTxO. This is
-- equivalent to `coin . balance`, but it will be more efficient
coinBalance :: EraTxOut era => UTxO era -> Coin
coinBalance = sumAllCoin . unUTxO
{-# INLINE coinBalance #-}

-- | Sum all the value in any Foldable with 'TxOut's
sumAllValue :: (EraTxOut era, Foldable f) => f (TxOut era) -> Value era
sumAllValue = foldMap' (^. valueTxOutL)
{-# INLINE sumAllValue #-}

-- | Sum all the 'Coin's in any Foldable with with 'TxOut's. Care should be
-- taken since it is susceptible to integer overflow, therefore make sure this
-- function is not applied to unvalidated 'TxOut's
sumAllCoin :: (EraTxOut era, Foldable f) => f (TxOut era) -> Coin
sumAllCoin = fromCompact . CompactCoin . getSum . foldMap' getCoinWord64
  where
    getCoinWord64 txOut =
      case txOut ^. compactCoinTxOutL of
        CompactCoin w64 -> Sum w64
{-# INLINE sumAllCoin #-}

-- | Check whether any of the supplied 'TxOut's contain any MultiAssets. Returns
-- True if non of them do.
areAllAdaOnly :: (EraTxOut era, Foldable f) => f (TxOut era) -> Bool
areAllAdaOnly = all (^. isAdaOnlyTxOutF)
{-# INLINE areAllAdaOnly #-}

-- | Extract script hash from value address with script.
getScriptHash :: Addr c -> Maybe (ScriptHash c)
getScriptHash (Addr _ (ScriptHashObj hs) _) = Just hs
getScriptHash _ = Nothing

-- | The only reason it is a newtype instead of just a Map is becuase for later eras is
-- expensive to compute the actual map, so we want to use the type safety guidance to
-- avoid redundant work.
newtype ScriptsProvided era = ScriptsProvided
  { unScriptsProvided :: Map.Map (ScriptHash (EraCrypto era)) (Script era)
  }
  deriving (Generic)

deriving instance (Era era, Eq (Script era)) => Eq (ScriptsProvided era)
deriving instance (Era era, Ord (Script era)) => Ord (ScriptsProvided era)
deriving instance (Era era, Show (Script era)) => Show (ScriptsProvided era)
deriving instance (Era era, NFData (Script era)) => NFData (ScriptsProvided era)

class EraTx era => EraUTxO era where
  -- | A customizable type on per era basis for the information required to find all
  -- scripts needed for the transaction.
  type ScriptsNeeded era = (r :: Type) | r -> era

  -- | Calculate all the value that is being consumed by the transaction.
  getConsumedValue ::
    PParams era ->
    -- | Function that can lookup current delegation deposits
    (Credential 'Staking (EraCrypto era) -> Maybe Coin) ->
    -- | Function that can lookup current drep deposits
    (Credential 'DRepRole (EraCrypto era) -> Maybe Coin) ->
    UTxO era ->
    TxBody era ->
    Value era

  getProducedValue ::
    PParams era ->
    -- | Check whether a pool with a supplied PoolStakeId is already registered.
    (KeyHash 'StakePool (EraCrypto era) -> Bool) ->
    TxBody era ->
    Value era

  -- | Initial eras will look into witness set to find all of the available scripts, but
  -- starting with Babbage we can look for available scripts in the UTxO using reference
  -- inputs.
  getScriptsProvided ::
    -- | For some era it is necessary to look into the UTxO to find all of the available
    -- scripts for the transaction
    UTxO era ->
    Tx era ->
    ScriptsProvided era

  -- | Produce all the information required for figuring out which scripts are required
  -- for the transaction to be valid, once those scripts are evaluated
  getScriptsNeeded :: UTxO era -> TxBody era -> ScriptsNeeded era

  -- | Extract the set of all script hashes that are needed for script validation.
  getScriptsHashesNeeded :: ScriptsNeeded era -> Set (ScriptHash (EraCrypto era))

  -- | Extract all of the KeyHash witnesses that are required for validating the transaction
  getWitsVKeyNeeded ::
    CertState era -> UTxO era -> TxBody era -> Set (KeyHash 'Witness (EraCrypto era))

  -- | Minimum fee computation, excluding witnesses and including ref scripts size
  getMinFeeTxUtxo :: PParams era -> Tx era -> UTxO era -> Coin
