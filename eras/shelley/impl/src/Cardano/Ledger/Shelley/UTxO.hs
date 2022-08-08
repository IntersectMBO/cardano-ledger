{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE BangPatterns #-}
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
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Cardano.Ledger.Shelley.UTxO
  ( -- * Primitives
    UTxO (..),

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
    scriptCred,
    scriptStakeCred,
    consumed,
    produced,
    keyRefunds,
  )
where

import Cardano.Binary (FromCBOR (..), ToCBOR (..))
import qualified Cardano.Crypto.Hash as CH
import Cardano.Ledger.Address (Addr (..))
import Cardano.Ledger.BaseTypes (strictMaybeToMaybe)
import Cardano.Ledger.Block (txid)
import Cardano.Ledger.Coin (Coin (..))
import Cardano.Ledger.Core
import Cardano.Ledger.Credential (Credential (..))
import qualified Cardano.Ledger.Crypto as CC (Crypto, HASH)
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
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import qualified Data.Maybe as Maybe
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Sharing (FromSharedCBOR (Share, fromSharedCBOR), Interns)
import Data.Typeable (Typeable)
import GHC.Generics (Generic)
import GHC.Records (HasField (..))
import Lens.Micro ((^.))
import NoThunks.Class (NoThunks (..))
import Quiet (Quiet (Quiet))

-- ===============================================

-- | The unspent transaction outputs.
newtype UTxO era = UTxO {unUTxO :: Map.Map (TxIn (Crypto era)) (TxOut era)}
  deriving (Default, Generic, Semigroup)

deriving instance NoThunks (TxOut era) => NoThunks (UTxO era)

deriving instance (Era era, NFData (TxOut era)) => NFData (UTxO era)

deriving newtype instance
  (Eq (TxOut era), CC.Crypto (Crypto era)) => Eq (UTxO era)

deriving newtype instance CC.Crypto (Crypto era) => Monoid (UTxO era)

instance (Era era, ToCBOR (TxOut era)) => ToCBOR (UTxO era) where
  toCBOR = encodeMap toCBOR toCBOR . unUTxO

instance
  ( CC.Crypto (Crypto era),
    FromSharedCBOR (TxOut era),
    Share (TxOut era) ~ Interns (Credential 'Staking (Crypto era))
  ) =>
  FromSharedCBOR (UTxO era)
  where
  type
    Share (UTxO era) =
      Interns (Credential 'Staking (Crypto era))
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
    (Show (TxOut era), CC.Crypto (Crypto era)) => Show (UTxO era)

-- | Compute the UTxO inputs of a transaction.
-- txins has the same problems as txouts, see notes below.
txins ::
  EraTxBody era =>
  TxBody era ->
  Set (TxIn (Crypto era))
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
  TxIn (Crypto era) ->
  UTxO era ->
  Maybe (TxOut era)
txinLookup txin (UTxO utxo') = Map.lookup txin utxo'

-- | Verify a transaction body witness
verifyWitVKey ::
  ( Typeable kr,
    CC.Crypto crypto,
    DSignable crypto (Hash crypto EraIndependentTxBody)
  ) =>
  Hash crypto EraIndependentTxBody ->
  WitVKey kr crypto ->
  Bool
verifyWitVKey txbodyHash (WitVKey vkey sig) = verifySignedDSIGN vkey txbodyHash (coerce sig)

-- | Create a witness for transaction
makeWitnessVKey ::
  forall c kr.
  ( CC.Crypto c,
    DSignable c (CH.Hash (CC.HASH c) EraIndependentTxBody)
  ) =>
  SafeHash c EraIndependentTxBody ->
  KeyPair kr c ->
  WitVKey 'Witness c
makeWitnessVKey safe keys =
  WitVKey (asWitness $ vKey keys) (coerce $ signedDSIGN @c (sKey keys) (extractHash safe))

-- | Create witnesses for transaction
makeWitnessesVKey ::
  forall c kr.
  ( CC.Crypto c,
    DSignable c (CH.Hash (CC.HASH c) EraIndependentTxBody)
  ) =>
  SafeHash c EraIndependentTxBody ->
  [KeyPair kr c] ->
  Set (WitVKey 'Witness c)
makeWitnessesVKey safe xs = Set.fromList (fmap (makeWitnessVKey safe) xs)

-- | From a list of key pairs and a set of key hashes required for a multi-sig
-- scripts, return the set of required keys.
makeWitnessesFromScriptKeys ::
  ( CC.Crypto crypto,
    DSignable crypto (Hash crypto EraIndependentTxBody)
  ) =>
  SafeHash crypto EraIndependentTxBody ->
  Map (KeyHash kr crypto) (KeyPair kr crypto) ->
  Set (KeyHash kr crypto) ->
  Set (WitVKey 'Witness crypto)
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

-- | Sum all the coin in any Foldable with with 'TxOut's
sumAllCoin :: (EraTxOut era, Foldable f) => f (TxOut era) -> Coin
sumAllCoin = foldMap' (^. coinTxOutL)
{-# INLINE sumAllCoin #-}

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
  ( HasField "_poolDeposit" pp Coin,
    HasField "_keyDeposit" pp Coin
  ) =>
  pp ->
  (KeyHash 'StakePool crypto -> Bool) ->
  [DCert crypto] ->
  Coin
totalDeposits pp isNewPool certs =
  (numKeys <×> getField @"_keyDeposit" pp)
    <+> (numNewPools <×> getField @"_poolDeposit" pp)
  where
    numKeys = length $ filter isRegKey certs
    pools = Set.fromList $ Maybe.mapMaybe getKeyHashFromRegPool certs
    numNewPools = length $ Set.filter isNewPool pools

getKeyHashFromRegPool :: DCert crypto -> Maybe (KeyHash 'StakePool crypto)
getKeyHashFromRegPool (DCertPool (RegPool p)) = Just $ _poolId p
getKeyHashFromRegPool _ = Nothing

txup :: (EraTx era, ShelleyEraTxBody era) => Tx era -> Maybe (Update era)
txup tx = strictMaybeToMaybe (tx ^. bodyTxL . updateTxBodyL)

-- | Extract script hash from value address with script.
getScriptHash :: Addr crypto -> Maybe (ScriptHash crypto)
getScriptHash (Addr _ (ScriptHashObj hs) _) = Just hs
getScriptHash _ = Nothing

scriptStakeCred :: DCert crypto -> Maybe (ScriptHash crypto)
scriptStakeCred (DCertDeleg (DeRegKey (KeyHashObj _))) = Nothing
scriptStakeCred (DCertDeleg (DeRegKey (ScriptHashObj hs))) = Just hs
scriptStakeCred (DCertDeleg (Delegate (Delegation (KeyHashObj _) _))) = Nothing
scriptStakeCred (DCertDeleg (Delegate (Delegation (ScriptHashObj hs) _))) = Just hs
scriptStakeCred _ = Nothing

scriptCred :: Credential kr crypto -> Maybe (ScriptHash crypto)
scriptCred (KeyHashObj _) = Nothing
scriptCred (ScriptHashObj hs) = Just hs

-- | Computes the set of script hashes required to unlock the transaction inputs
-- and the withdrawals.
scriptsNeeded ::
  forall era.
  (EraTx era, ShelleyEraTxBody era) =>
  UTxO era ->
  Tx era ->
  Set (ScriptHash (Crypto era))
scriptsNeeded u tx =
  scriptHashes
    `Set.union` Set.fromList
      [sh | w <- withdrawals, Just sh <- [scriptCred (getRwdCred w)]]
    `Set.union` Set.fromList
      [sh | c <- certificates, requiresVKeyWitness c, Just sh <- [scriptStakeCred c]]
    `Set.union` (txbody ^. mintedTxBodyF) -- This might be Set.empty in some Eras.
  where
    txbody = tx ^. bodyTxL
    withdrawals = Map.keys (unWdrl (txbody ^. wdrlsTxBodyL))
    scriptHashes = txinsScriptHashes (txbody ^. inputsTxBodyL) u
    certificates = toList (txbody ^. certsTxBodyL)

-- | Compute the subset of inputs of the set 'txInps' for which each input is
-- locked by a script in the UTxO 'u'.
txinsScriptHashes ::
  EraTxOut era =>
  Set (TxIn (Crypto era)) ->
  UTxO era ->
  Set (ScriptHash (Crypto era))
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
  forall era pp.
  ( ShelleyEraTxBody era,
    HasField "_keyDeposit" pp Coin,
    HasField "_poolDeposit" pp Coin
  ) =>
  pp ->
  (KeyHash 'StakePool (Crypto era) -> Bool) ->
  TxBody era ->
  Value era
produced pp isNewPool txBody =
  balance (txouts txBody)
    <+> Val.inject
      ( txBody ^. feeTxBodyL
          <+> totalDeposits pp isNewPool (toList $ txBody ^. certsTxBodyL)
      )

-- | Compute the lovelace which are destroyed by the transaction
consumed ::
  forall era pp.
  ( ShelleyEraTxBody era,
    HasField "_keyDeposit" pp Coin
  ) =>
  pp ->
  UTxO era ->
  TxBody era ->
  Value era
consumed pp (UTxO u) txBody =
  {- balance (txins tx ◁ u) + wbalance (txwdrls tx) + keyRefunds pp tx -}
  Set.foldl' lookupAddTxOut mempty (txins @era txBody)
    <> Val.inject (refunds <+> withdrawals)
  where
    lookupAddTxOut acc txin = maybe acc (addTxOut acc) $ Map.lookup txin u
    addTxOut !b out = out ^. valueTxOutL <+> b
    refunds = keyRefunds pp txBody
    withdrawals = fold . unWdrl $ txBody ^. wdrlsTxBodyL

-- | Compute the key deregistration refunds in a transaction
keyRefunds ::
  ( HasField "_keyDeposit" pp Coin,
    ShelleyEraTxBody era
  ) =>
  pp ->
  TxBody era ->
  Coin
keyRefunds pp tx = length deregistrations <×> getField @"_keyDeposit" pp
  where
    deregistrations = filter isDeRegKey (toList $ tx ^. certsTxBodyL)
