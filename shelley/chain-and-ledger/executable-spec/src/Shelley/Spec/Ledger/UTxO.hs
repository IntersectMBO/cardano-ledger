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
{-# LANGUAGE ViewPatterns #-}

-- |
-- Module      : UTxO
-- Description : Simple UTxO Ledger
--
-- This module defines the types and functions for a simple UTxO Ledger
-- as specified in /A Simplified Formal Specification of a UTxO Ledger/.
module Shelley.Spec.Ledger.UTxO
  ( -- * Primitives
    UTxO (..),

    -- * Functions
    txid,
    txins,
    txinLookup,
    txouts,
    txup,
    balance,
    totalDeposits,
    makeWitnessVKey,
    makeWitnessesVKey,
    makeWitnessesFromScriptKeys,
    verifyWitVKey,
    getScriptHash,
    scriptsNeeded,
    scriptCred,
    scriptStakeCred,
    txinsScript,
  )
where

import Cardano.Binary (FromCBOR (..), ToCBOR (..))
import qualified Cardano.Crypto.Hash as CH
import qualified Cardano.Ledger.Core as Core
import qualified Cardano.Ledger.Crypto as CC (Crypto, HASH)
import Cardano.Ledger.Era
import Cardano.Ledger.SafeHash (HashAnnotated, SafeHash, extractHash, hashAnnotated)
import Cardano.Ledger.Shelley.Constraints (UsesTxOut)
import Cardano.Ledger.Val (Val (..), (<+>), (<×>))
import Control.DeepSeq (NFData)
import Control.Iterate.SetAlgebra
  ( BaseRep (MapR),
    Embed (..),
    Exp (Base),
    HasExp (toExp),
    eval,
    (◁),
  )
import Data.Coerce (coerce)
import Data.Constraint (Constraint)
import Data.Foldable (toList)
import Data.Kind (Type)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import qualified Data.Maybe as Maybe
import Data.Sequence.Strict (StrictSeq)
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Typeable (Typeable)
import GHC.Generics (Generic)
import GHC.Records (HasField (..))
import NoThunks.Class (NoThunks (..))
import Quiet
import Shelley.Spec.Ledger.Address (Addr (..))
import Shelley.Spec.Ledger.BaseTypes (StrictMaybe, strictMaybeToMaybe)
import Shelley.Spec.Ledger.Coin (Coin (..))
import Shelley.Spec.Ledger.Credential (Credential (..))
import Shelley.Spec.Ledger.Delegation.Certificates
  ( DCert (..),
    isRegKey,
    requiresVKeyWitness,
  )
import Shelley.Spec.Ledger.Keys
  ( DSignable,
    Hash,
    KeyHash (..),
    KeyPair (..),
    KeyRole (StakePool, Witness),
    asWitness,
    signedDSIGN,
    verifySignedDSIGN,
  )
import Shelley.Spec.Ledger.PParams (Update)
import Shelley.Spec.Ledger.Scripts
import Shelley.Spec.Ledger.Tx (TransTx, Tx (..))
import Shelley.Spec.Ledger.TxBody
  ( EraIndependentTxBody,
    PoolCert (..),
    PoolParams (..),
    TransTxId,
    TxId (..),
    TxIn (..),
    Wdrl (..),
    WitVKey (..),
    getRwdCred,
    pattern DeRegKey,
    pattern Delegate,
    pattern Delegation,
  )

-- ===============================================

instance
  (Crypto era ~ crypto, Core.TxOut era ~ out) =>
  HasExp (UTxO era) (Map (TxIn crypto) out)
  where
  toExp (UTxO x) = Base MapR x

instance
  (Crypto era ~ crypto, Core.TxOut era ~ out) =>
  Embed (UTxO era) (Map (TxIn crypto) out)
  where
  toBase (UTxO x) = x
  fromBase x = (UTxO x)

-- | The unspent transaction outputs.
newtype UTxO era = UTxO {unUTxO :: Map (TxIn (Crypto era)) (Core.TxOut era)}
  deriving (Generic)

type TransUTxO (c :: Type -> Constraint) era = (c (Core.TxOut era), TransTxId c era)

deriving instance TransUTxO NoThunks era => NoThunks (UTxO era)

deriving instance (Era era, NFData (Core.TxOut era)) => NFData (UTxO era)

deriving newtype instance
  TransUTxO Eq era =>
  Eq (UTxO era)

deriving newtype instance
  (TransUTxO ToCBOR era, Era era) =>
  ToCBOR (UTxO era)

deriving newtype instance
  (TransUTxO FromCBOR era, Era era) =>
  FromCBOR (UTxO era)

deriving via
  Quiet (UTxO era)
  instance
    (TransUTxO Show era) =>
    Show (UTxO era)

-- | Compute the id of a transaction.
txid ::
  forall era.
  ( Era era, -- The Crypto functions are determined
    HashAnnotated -- Core.TxBody hashes with the right tag: EraIndependentTxBody
      (Core.TxBody era)
      EraIndependentTxBody
      (Crypto era)
  ) =>
  Core.TxBody era ->
  TxId (Crypto era)
txid = TxId . hashAnnotated

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
  ( Era era,
    HashAnnotated (Core.TxBody era) EraIndependentTxBody (Crypto era),
    HasField "outputs" (Core.TxBody era) (StrictSeq (Core.TxOut era))
  ) =>
  Core.TxBody era ->
  UTxO era
txouts tx =
  UTxO $
    Map.fromList
      [ (TxIn transId idx, out)
        | (out, idx) <- zip (toList $ getField @"outputs" tx) [0 ..]
      ]
  where
    transId = txid @era tx

-- | Lookup a txin for a given UTxO collection
txinLookup ::
  TxIn (Crypto era) ->
  UTxO era ->
  Maybe (Core.TxOut era)
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
balance ::
  ( Val (Core.Value era),
    HasField "value" (Core.TxOut era) (Core.Value era)
  ) =>
  UTxO era ->
  Core.Value era
balance (UTxO utxo) = Map.foldl' addTxOuts mempty utxo
  where
    addTxOuts !b out = (getField @"value" out) <+> b

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
  Map (KeyHash 'StakePool crypto) (PoolParams crypto) ->
  [DCert crypto] ->
  Coin
totalDeposits pp stpools cs =
  (numKeys <×> getField @"_keyDeposit" pp)
    <+> (numNewPools <×> getField @"_poolDeposit" pp)
  where
    numKeys = length $ filter isRegKey cs
    pools = Set.fromList $ Maybe.mapMaybe getKeyHashFromRegPool cs
    numNewPools = length $ pools `Set.difference` Map.keysSet stpools

getKeyHashFromRegPool :: DCert crypto -> Maybe (KeyHash 'StakePool crypto)
getKeyHashFromRegPool (DCertPool (RegPool p)) = Just . _poolId $ p
getKeyHashFromRegPool _ = Nothing

txup ::
  ( TransTx ToCBOR era,
    HasField "update" (Core.TxBody era) (StrictMaybe (Update era))
  ) =>
  Tx era ->
  Maybe (Update era)
txup (Tx txbody _ _) = strictMaybeToMaybe (getField @"update" txbody)

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
  forall era.
  ( UsesTxOut era,
    TransTx ToCBOR era,
    HasField "certs" (Core.TxBody era) (StrictSeq (DCert (Crypto era))),
    HasField "wdrls" (Core.TxBody era) (Wdrl (Crypto era)),
    HasField "inputs" (Core.TxBody era) (Set (TxIn (Crypto era)))
  ) =>
  UTxO era ->
  Tx era ->
  Set (ScriptHash (Crypto era))
scriptsNeeded u tx =
  Set.fromList (Map.elems $ Map.mapMaybe (getScriptHash . (getField @"address")) u'')
    `Set.union` Set.fromList
      ( Maybe.mapMaybe (scriptCred . getRwdCred) $
          Map.keys withdrawals
      )
    `Set.union` Set.fromList
      ( Maybe.mapMaybe
          scriptStakeCred
          (filter requiresVKeyWitness certificates)
      )
  where
    withdrawals = unWdrl $ getField @"wdrls" $ _body tx
    u'' = eval ((txinsScript (getField @"inputs" $ _body tx) u) ◁ u)
    certificates = (toList . getField @"certs" . _body) tx

-- | Compute the subset of inputs of the set 'txInps' for which each input is
-- locked by a script in the UTxO 'u'.
txinsScript ::
  ( UsesTxOut era
  ) =>
  Set (TxIn (Crypto era)) ->
  UTxO era ->
  Set (TxIn (Crypto era))
txinsScript txInps (UTxO u) = foldr add Set.empty txInps
  where
    -- to get subset, start with empty, and only insert those inputs in txInps that are locked in u
    add input ans = case Map.lookup input u of
      Just out -> case getField @"address" out of
        Addr _ (ScriptHashObj _) _ -> Set.insert input ans
        _ -> ans
      Nothing -> ans
