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

module Cardano.Ledger.Shelley.UTxO
  ( -- * Primitives
    UTxO (..),
    EraUTxO (..),
    ShelleyScriptsNeeded (..),

    -- * Functions
    txins,
    txinLookup,
    txouts,
    txup,
    balance,
    coinBalance,
    sumAllValue,
    sumAllCoin,
    areAllAdaOnly,
    totalDeposits,
    makeWitnessVKey,
    makeWitnessesVKey,
    makeWitnessesFromScriptKeys,
    verifyWitVKey,
    getScriptHash,
    scriptsNeeded,
    getShelleyScriptsNeeded,
    scriptCred,
    scriptStakeCred,
    getConsumedCoin,
    produced,
    keyRefunds,
  )
where

import Cardano.Binary (FromCBOR (..), ToCBOR (..))
import qualified Cardano.Crypto.Hash as CH
import Cardano.Ledger.Address (Addr (..))
import Cardano.Ledger.BaseTypes (strictMaybeToMaybe)
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
import Cardano.Ledger.SafeHash (SafeHash, extractHash)
import Cardano.Ledger.Shelley.Delegation.Certificates
  ( DCert (..),
    isDeRegKey,
    isRegKey,
    requiresVKeyWitness,
  )
import Cardano.Ledger.Shelley.Era (ShelleyEra)
import Cardano.Ledger.Shelley.PParams (Update)
import Cardano.Ledger.Shelley.TxBody
  ( PoolCert (..),
    PoolParams (..),
    ShelleyEraTxBody (..),
    Wdrl (..),
    WitVKey (..),
    getRwdCred,
    pattern DeRegKey,
    pattern Delegate,
    pattern Delegation,
  )
import Cardano.Ledger.TxIn (TxIn (..))
import Cardano.Ledger.Val ((<+>), (<×>))
import qualified Cardano.Ledger.Val as Val
import Control.DeepSeq (NFData)
import Control.Monad ((<$!>))
import Data.Coders (decodeMapNoDuplicates, encodeMap)
import Data.Coerce (coerce)
import Data.Default.Class (Default)
import Data.Foldable (Foldable (fold), foldMap', toList)
import Data.Kind
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import qualified Data.Maybe as Maybe
import Data.Monoid (Sum (..))
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Sharing (FromSharedCBOR (Share, fromSharedCBOR), Interns)
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

instance (Era era, ToCBOR (TxOut era)) => ToCBOR (UTxO era) where
  toCBOR = encodeMap toCBOR toCBOR . unUTxO

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

-- | Determine the total deposit amount needed.
-- The block may (legitimately) contain multiple registration certificates
-- for the same pool, where the first will be treated as a registration and
-- any subsequent ones as re-registration. As such, we must only take a
-- deposit for the first such registration.
--
-- Note that this is not an issue for key registrations since subsequent
-- registration certificates would be invalid.
totalDeposits ::
  ( EraPParams era
  ) =>
  PParams era ->
  (KeyHash 'StakePool c -> Bool) ->
  [DCert c] ->
  Coin
totalDeposits pp isNewPool certs =
  (numKeys <×> pp ^. ppKeyDepositL)
    <+> (numNewPools <×> pp ^. ppPoolDepositL)
  where
    numKeys = length $ filter isRegKey certs
    pools = Set.fromList $ Maybe.mapMaybe getKeyHashFromRegPool certs
    numNewPools = length $ Set.filter isNewPool pools

getKeyHashFromRegPool :: DCert c -> Maybe (KeyHash 'StakePool c)
getKeyHashFromRegPool (DCertPool (RegPool p)) = Just $ _poolId p
getKeyHashFromRegPool _ = Nothing

txup :: (EraTx era, ShelleyEraTxBody era) => Tx era -> Maybe (Update era)
txup tx = strictMaybeToMaybe (tx ^. bodyTxL . updateTxBodyL)

-- | Extract script hash from value address with script.
getScriptHash :: Addr c -> Maybe (ScriptHash c)
getScriptHash (Addr _ (ScriptHashObj hs) _) = Just hs
getScriptHash _ = Nothing

scriptStakeCred :: DCert c -> Maybe (ScriptHash c)
scriptStakeCred (DCertDeleg (DeRegKey (KeyHashObj _))) = Nothing
scriptStakeCred (DCertDeleg (DeRegKey (ScriptHashObj hs))) = Just hs
scriptStakeCred (DCertDeleg (Delegate (Delegation (KeyHashObj _) _))) = Nothing
scriptStakeCred (DCertDeleg (Delegate (Delegation (ScriptHashObj hs) _))) = Just hs
scriptStakeCred _ = Nothing

scriptCred :: Credential kr c -> Maybe (ScriptHash c)
scriptCred (KeyHashObj _) = Nothing
scriptCred (ScriptHashObj hs) = Just hs

-- | Computes the set of script hashes required to unlock the transaction inputs
-- and the withdrawals.
scriptsNeeded ::
  forall era.
  (EraTx era, ShelleyEraTxBody era) =>
  UTxO era ->
  Tx era ->
  Set (ScriptHash (EraCrypto era))
scriptsNeeded u tx =
  case getShelleyScriptsNeeded u (tx ^. bodyTxL) of
    ShelleyScriptsNeeded sn -> sn
{-# DEPRECATED scriptsNeeded "In favor of `getScriptsNeeded`" #-}

getShelleyScriptsNeeded ::
  forall era.
  (EraTxBody era, ShelleyEraTxBody era) =>
  UTxO era ->
  TxBody era ->
  ShelleyScriptsNeeded era
getShelleyScriptsNeeded u txBody =
  ShelleyScriptsNeeded
    ( scriptHashes
        `Set.union` Set.fromList
          [sh | w <- withdrawals, Just sh <- [scriptCred (getRwdCred w)]]
        `Set.union` Set.fromList
          [sh | c <- certificates, requiresVKeyWitness c, Just sh <- [scriptStakeCred c]]
    )
  where
    withdrawals = Map.keys (unWdrl (txBody ^. wdrlsTxBodyL))
    scriptHashes = txinsScriptHashes (txBody ^. inputsTxBodyL) u
    certificates = toList (txBody ^. certsTxBodyL)

-- | Compute the subset of inputs of the set 'txInps' for which each input is
-- locked by a script in the UTxO 'u'.
txinsScriptHashes ::
  EraTxOut era =>
  Set (TxIn (EraCrypto era)) ->
  UTxO era ->
  Set (ScriptHash (EraCrypto era))
txinsScriptHashes txInps (UTxO u) = foldr add Set.empty txInps
  where
    -- to get subset, start with empty, and only insert those inputs in txInps
    -- that are locked in u
    add input ans = case Map.lookup input u of
      Just txOut -> case txOut ^. addrTxOutL of
        Addr _ (ScriptHashObj h) _ -> Set.insert h ans
        _ -> ans
      Nothing -> ans

-- | Compute the lovelace which are created by the transaction
produced ::
  forall era.
  ( ShelleyEraTxBody era
  ) =>
  PParams era ->
  (KeyHash 'StakePool (EraCrypto era) -> Bool) ->
  TxBody era ->
  Value era
produced pp isNewPool txBody =
  balance (txouts txBody)
    <+> Val.inject
      ( txBody ^. feeTxBodyL
          <+> totalDeposits pp isNewPool (toList $ txBody ^. certsTxBodyL)
      )

-- | Compute the lovelace which are destroyed by the transaction
getConsumedCoin ::
  forall era.
  ( ShelleyEraTxBody era
  ) =>
  PParams era ->
  UTxO era ->
  TxBody era ->
  Coin
getConsumedCoin pp (UTxO u) txBody =
  {- balance (txins tx ◁ u) + wbalance (txwdrls tx) + keyRefunds pp tx -}
  coinBalance (UTxO (Map.restrictKeys u (txBody ^. inputsTxBodyL)))
    <> refunds
    <> withdrawals
  where
    refunds = keyRefunds pp txBody
    withdrawals = fold . unWdrl $ txBody ^. wdrlsTxBodyL

-- | Compute the key deregistration refunds in a transaction
keyRefunds ::
  (
    ShelleyEraTxBody era
  ) =>PParams era ->
  TxBody era ->
  Coin
keyRefunds pp tx = length deregistrations <×> pp ^. ppKeyDepositL
  where
    deregistrations = filter isDeRegKey (toList $ tx ^. certsTxBodyL)

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

newtype ShelleyScriptsNeeded era = ShelleyScriptsNeeded (Set (ScriptHash (EraCrypto era)))
  deriving (Eq, Show)

instance Crypto c => EraUTxO (ShelleyEra c) where
  type ScriptsNeeded (ShelleyEra c) = ShelleyScriptsNeeded (ShelleyEra c)
  getConsumedValue = getConsumedCoin

  getScriptsNeeded = getShelleyScriptsNeeded

  getScriptsHashesNeeded (ShelleyScriptsNeeded scriptsHashes) = scriptsHashes
