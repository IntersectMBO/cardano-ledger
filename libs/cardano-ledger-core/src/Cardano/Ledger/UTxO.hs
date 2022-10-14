{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilyDependencies #-}
{-# LANGUAGE UndecidableInstances #-}

module Cardano.Ledger.UTxO
  ( -- * Primitives
    UTxO (..),
    EraUTxO (..),

    -- * Functions
    txins,
    txinLookup,
    txouts,
    balance,
    coinBalance,
    sumAllValue,
    sumAllCoin,
    areAllAdaOnly,
    makeWitnessVKey,
    makeWitnessesVKey,
    makeWitnessesFromScriptKeys,
    verifyWitVKey,
    getScriptHash,
  )
where

import qualified Cardano.Crypto.Hash as CH
import Cardano.Ledger.Address (Addr (..))
import Cardano.Ledger.Binary
  ( FromCBOR (..),
    FromSharedCBOR (Share, fromSharedCBOR),
    Interns,
    ToCBOR (..),
    decodeMapNoDuplicates,
  )
import Cardano.Ledger.Block (txid)
import Cardano.Ledger.Coin (Coin, CompactForm (CompactCoin))
import Cardano.Ledger.Compactible (Compactible (..))
import Cardano.Ledger.Core
import Cardano.Ledger.Credential (Credential (..))
import Cardano.Ledger.Crypto (Crypto, HASH)
import Cardano.Ledger.Keys
  ( DSignable,
    Hash,
    KeyHash (..),
    KeyPair (..),
    KeyRole (..),
    asWitness,
    signedDSIGN,
    verifySignedDSIGN,
  )
import Cardano.Ledger.Keys.WitVKey
import Cardano.Ledger.SafeHash (SafeHash, extractHash)
import Cardano.Ledger.TxIn (TxIn (..))
import Control.DeepSeq (NFData)
import Control.Monad ((<$!>))
import Data.Coerce (coerce)
import Data.Default.Class (Default)
import Data.Foldable (foldMap', toList)
import Data.Kind
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Monoid (Sum (..))
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Typeable (Typeable)
import GHC.Generics (Generic)
import Lens.Micro ((^.))
import NoThunks.Class (NoThunks (..))
import Quiet (Quiet (Quiet))

-- ===============================================

-- | The unspent transaction outputs.
newtype UTxO era = UTxO {unUTxO :: Map.Map (TxIn (EraCrypto era)) (TxOut era)}
  deriving (Default, Generic, Semigroup)

deriving instance NoThunks (TxOut era) => NoThunks (UTxO era)

deriving instance (Era era, NFData (TxOut era)) => NFData (UTxO era)

deriving newtype instance
  (Eq (TxOut era), Crypto (EraCrypto era)) => Eq (UTxO era)

deriving newtype instance Crypto (EraCrypto era) => Monoid (UTxO era)

deriving newtype instance (Era era, ToCBOR (TxOut era)) => ToCBOR (UTxO era)

instance
  ( Crypto (EraCrypto era),
    FromSharedCBOR (TxOut era),
    Share (TxOut era) ~ Interns (Credential 'Staking (EraCrypto era))
  ) =>
  FromSharedCBOR (UTxO era)
  where
  type
    Share (UTxO era) =
      Interns (Credential 'Staking (EraCrypto era))
  fromSharedCBOR credsInterns =
    UTxO <$!> decodeMapNoDuplicates fromCBOR (fromSharedCBOR credsInterns)

instance
  ( FromCBOR (TxOut era),
    Era era
  ) =>
  FromCBOR (UTxO era)
  where
  fromCBOR = UTxO <$!> decodeMapNoDuplicates fromCBOR fromCBOR

deriving via
  Quiet (UTxO era)
  instance
    (Show (TxOut era), Crypto (EraCrypto era)) => Show (UTxO era)

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
    transId = txid txBody

-- | Lookup a txin for a given UTxO collection
txinLookup ::
  TxIn (EraCrypto era) ->
  UTxO era ->
  Maybe (TxOut era)
txinLookup txin (UTxO utxo') = Map.lookup txin utxo'

-- | Verify a transaction body witness
verifyWitVKey ::
  ( Typeable kr,
    Crypto c,
    DSignable c (Hash c EraIndependentTxBody)
  ) =>
  Hash c EraIndependentTxBody ->
  WitVKey kr c ->
  Bool
verifyWitVKey txbodyHash (WitVKey vkey sig) = verifySignedDSIGN vkey txbodyHash (coerce sig)

-- | Create a witness for transaction
makeWitnessVKey ::
  forall c kr.
  ( Crypto c,
    DSignable c (CH.Hash (HASH c) EraIndependentTxBody)
  ) =>
  SafeHash c EraIndependentTxBody ->
  KeyPair kr c ->
  WitVKey 'Witness c
makeWitnessVKey safe keys =
  WitVKey (asWitness $ vKey keys) (coerce $ signedDSIGN @c (sKey keys) (extractHash safe))

-- | Create witnesses for transaction
makeWitnessesVKey ::
  forall c kr.
  ( Crypto c,
    DSignable c (CH.Hash (HASH c) EraIndependentTxBody)
  ) =>
  SafeHash c EraIndependentTxBody ->
  [KeyPair kr c] ->
  Set (WitVKey 'Witness c)
makeWitnessesVKey safe xs = Set.fromList (fmap (makeWitnessVKey safe) xs)

-- | From a list of key pairs and a set of key hashes required for a multi-sig
-- scripts, return the set of required keys.
makeWitnessesFromScriptKeys ::
  (Crypto c, DSignable c (Hash c EraIndependentTxBody)) =>
  SafeHash c EraIndependentTxBody ->
  Map (KeyHash kr c) (KeyPair kr c) ->
  Set (KeyHash kr c) ->
  Set (WitVKey 'Witness c)
makeWitnessesFromScriptKeys txbodyHash hashKeyMap scriptHashes =
  let witKeys = Map.restrictKeys hashKeyMap scriptHashes
   in makeWitnessesVKey txbodyHash (Map.elems witKeys)

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

class EraTxBody era => EraUTxO era where
  -- | A customizable type on per era basis for the information required to find all
  -- scripts needed for the transaction.
  type ScriptsNeeded era = (r :: Type) | r -> era

  -- | Calculate all the value that is being consumed by the transaction.
  getConsumedValue :: PParams era -> UTxO era -> TxBody era -> Value era

  -- | Produce all the information required for figuring out which scripts are required
  -- for the transaction to be valid, once those scripts are evaluated
  getScriptsNeeded :: UTxO era -> TxBody era -> ScriptsNeeded era

  -- | Extract the set of all script hashes that are needed for script validation.
  getScriptsHashesNeeded :: ScriptsNeeded era -> Set (ScriptHash (EraCrypto era))
