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
    sumAllValue,
    totalDeposits,
    makeWitnessVKey,
    makeWitnessesVKey,
    makeWitnessesFromScriptKeys,
    verifyWitVKey,
    getScriptHash,
    scriptsNeeded,
    scriptCred,
    scriptStakeCred,

    -- * Utilities
    TransUTxO,
  )
where

import Cardano.Binary (FromCBOR (..), ToCBOR (..))
import qualified Cardano.Crypto.Hash as CH
import Cardano.Ledger.Address (Addr (..))
import Cardano.Ledger.BaseTypes (StrictMaybe, strictMaybeToMaybe)
import Cardano.Ledger.Coin (Coin (..))
import qualified Cardano.Ledger.Core as Core
import Cardano.Ledger.Credential (Credential (..))
import qualified Cardano.Ledger.Crypto as CC (Crypto, HASH)
import Cardano.Ledger.Era
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
    isRegKey,
    requiresVKeyWitness,
  )
import Cardano.Ledger.Shelley.PParams (Update)
import Cardano.Ledger.Shelley.Scripts
import Cardano.Ledger.Shelley.TxBody
  ( EraIndependentTxBody,
    PoolCert (..),
    PoolParams (..),
    TransTxId,
    Wdrl (..),
    WitVKey (..),
    getRwdCred,
    pattern DeRegKey,
    pattern Delegate,
    pattern Delegation,
  )
import Cardano.Ledger.TxIn (TxIn (..))
import qualified Cardano.Ledger.TxIn as Core (txid)
import Cardano.Ledger.Val ((<+>), (<×>))
import Control.DeepSeq (NFData)
import Control.Monad ((<$!>))
import Data.Coders (decodeSplitMap, encodeSplitMap)
import Data.Coerce (coerce)
import qualified Data.Compact.SplitMap as SplitMap
import Data.Constraint (Constraint)
import Data.Foldable (foldMap', toList)
import Data.Kind (Type)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import qualified Data.Maybe as Maybe
import Data.Sequence.Strict (StrictSeq)
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Sharing
import Data.Typeable (Typeable)
import GHC.Generics (Generic)
import GHC.Records (HasField (..))
import NoThunks.Class (NoThunks (..))
import Quiet

-- ===============================================

-- | The unspent transaction outputs.
newtype UTxO era = UTxO {unUTxO :: SplitMap.SplitMap (TxIn (Crypto era)) (Core.TxOut era)}
  deriving (Generic, Semigroup)

type TransUTxO (c :: Type -> Constraint) era = (c (Core.TxOut era), TransTxId c era)

deriving instance TransUTxO NoThunks era => NoThunks (UTxO era)

deriving instance (Era era, NFData (Core.TxOut era)) => NFData (UTxO era)

deriving newtype instance
  (Eq (Core.TxOut era), CC.Crypto (Crypto era)) => Eq (UTxO era)

deriving newtype instance CC.Crypto (Crypto era) => Monoid (UTxO era)

instance (Era era, ToCBOR (Core.TxOut era)) => ToCBOR (UTxO era) where
  toCBOR = encodeSplitMap toCBOR toCBOR . unUTxO

instance
  ( CC.Crypto (Crypto era),
    FromSharedCBOR (Core.TxOut era),
    Share (Core.TxOut era) ~ Interns (Credential 'Staking (Crypto era))
  ) =>
  FromSharedCBOR (UTxO era)
  where
  type
    Share (UTxO era) =
      Interns (Credential 'Staking (Crypto era))
  fromSharedCBOR credsInterns =
    UTxO <$!> decodeSplitMap fromCBOR (fromSharedCBOR credsInterns)

instance
  ( FromCBOR (Core.TxOut era),
    Era era
  ) =>
  FromCBOR (UTxO era)
  where
  fromCBOR = UTxO <$!> decodeSplitMap fromCBOR fromCBOR

deriving via
  Quiet (UTxO era)
  instance
    (Show (Core.TxOut era), CC.Crypto (Crypto era)) => Show (UTxO era)

-- | Compute the UTxO inputs of a transaction.
-- txins has the same problems as txouts, see notes below.
txins ::
  ( HasField "inputs" (Core.TxBody era) (Set (TxIn (Crypto era)))
  ) =>
  Core.TxBody era ->
  Set (TxIn (Crypto era))
txins = getField @"inputs"

-- Because its only input (Core.TxBody) is a type family this
-- function tends to give errors like
-- "Could not deduce: Core.TxOut era0 ~ Core.TxOut era"
-- The way to fix this is to use TypeApplications like this:  txouts @era body
-- where the type variable: era, is in scope (use ScopedTypeVariables)

-- | Compute the transaction outputs of a transaction.
txouts ::
  forall era.
  Era era =>
  Core.TxBody era ->
  UTxO era
txouts tx =
  UTxO $
    SplitMap.fromList
      [ (TxIn transId idx, out)
        | (out, idx) <- zip (toList $ getField @"outputs" tx) [minBound ..]
      ]
  where
    transId = Core.txid tx

-- | Lookup a txin for a given UTxO collection
txinLookup ::
  TxIn (Crypto era) ->
  UTxO era ->
  Maybe (Core.TxOut era)
txinLookup txin (UTxO utxo') = SplitMap.lookup txin utxo'

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
balance ::
  forall era.
  Era era =>
  UTxO era ->
  Core.Value era
balance = sumAllValue @era . unUTxO
{-# INLINE balance #-}

-- | Sum all the value in any Foldable with elements that have a field "value"
sumAllValue ::
  forall era tx f.
  (Foldable f, HasField "value" tx (Core.Value era), Monoid (Core.Value era)) =>
  f tx ->
  Core.Value era
sumAllValue = foldMap' (getField @"value")
{-# INLINE sumAllValue #-}

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
totalDeposits pp isNewPool cs =
  (numKeys <×> getField @"_keyDeposit" pp)
    <+> (numNewPools <×> getField @"_poolDeposit" pp)
  where
    numKeys = length $ filter isRegKey cs
    pools = Set.fromList $ Maybe.mapMaybe getKeyHashFromRegPool cs
    numNewPools = length $ Set.filter isNewPool pools

getKeyHashFromRegPool :: DCert crypto -> Maybe (KeyHash 'StakePool crypto)
getKeyHashFromRegPool (DCertPool (RegPool p)) = Just . _poolId $ p
getKeyHashFromRegPool _ = Nothing

txup ::
  forall era tx.
  ( HasField "update" (Core.TxBody era) (StrictMaybe (Update era)),
    HasField "body" tx (Core.TxBody era)
  ) =>
  tx ->
  Maybe (Update era)
txup tx = strictMaybeToMaybe (getField @"update" txbody)
  where
    txbody :: Core.TxBody era
    txbody = getField @"body" tx

-- | Extract script hash from value address with script.
getScriptHash :: Addr crypto -> Maybe (ScriptHash crypto)
getScriptHash (Addr _ (ScriptHashObj hs) _) = Just hs
getScriptHash _ = Nothing

scriptStakeCred ::
  DCert crypto ->
  Maybe (ScriptHash crypto)
scriptStakeCred (DCertDeleg (DeRegKey (KeyHashObj _))) = Nothing
scriptStakeCred (DCertDeleg (DeRegKey (ScriptHashObj hs))) = Just hs
scriptStakeCred (DCertDeleg (Delegate (Delegation (KeyHashObj _) _))) = Nothing
scriptStakeCred (DCertDeleg (Delegate (Delegation (ScriptHashObj hs) _))) = Just hs
scriptStakeCred _ = Nothing

scriptCred ::
  Credential kr crypto ->
  Maybe (ScriptHash crypto)
scriptCred (KeyHashObj _) = Nothing
scriptCred (ScriptHashObj hs) = Just hs

-- | Computes the set of script hashes required to unlock the transcation inputs
-- and the withdrawals.
scriptsNeeded ::
  forall era tx.
  ( Era era,
    HasField "body" tx (Core.TxBody era),
    HasField "certs" (Core.TxBody era) (StrictSeq (DCert (Crypto era))),
    HasField "wdrls" (Core.TxBody era) (Wdrl (Crypto era)),
    HasField "inputs" (Core.TxBody era) (Set (TxIn (Crypto era)))
  ) =>
  UTxO era ->
  tx ->
  Set (ScriptHash (Crypto era))
scriptsNeeded u tx =
  scriptHashes
    `Set.union` Set.fromList
      [sh | w <- withdrawals, Just sh <- [scriptCred (getRwdCred w)]]
    `Set.union` Set.fromList
      [sh | c <- certificates, requiresVKeyWitness c, Just sh <- [scriptStakeCred c]]
    `Set.union` getField @"minted" txbody -- This might be Set.empty in some Eras.
  where
    txbody = getField @"body" tx
    withdrawals = Map.keys (unWdrl (getField @"wdrls" txbody))
    scriptHashes = txinsScriptHashes (getField @"inputs" txbody) u
    certificates = toList (getField @"certs" txbody)

-- | Compute the subset of inputs of the set 'txInps' for which each input is
-- locked by a script in the UTxO 'u'.
txinsScriptHashes ::
  Era era =>
  Set (TxIn (Crypto era)) ->
  UTxO era ->
  Set (ScriptHash (Crypto era))
txinsScriptHashes txInps (UTxO u) = foldr add Set.empty txInps
  where
    -- to get subset, start with empty, and only insert those inputs in txInps
    -- that are locked in u
    add input ans = case SplitMap.lookup input u of
      Just out -> case getTxOutAddr out of
        Addr _ (ScriptHashObj h) _ -> Set.insert h ans
        _ -> ans
      Nothing -> ans
