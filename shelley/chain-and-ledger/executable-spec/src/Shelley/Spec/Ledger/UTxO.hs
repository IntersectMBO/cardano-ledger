{-# LANGUAGE BangPatterns #-}
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
-- for the Relation instance
{-# OPTIONS_GHC -Wno-orphans #-}

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
    scriptsNeeded,
    txinsScript,
  )
where

import Cardano.Binary (FromCBOR (..), ToCBOR (..))
import qualified Cardano.Ledger.Core as Core
import Cardano.Ledger.Era
import Cardano.Ledger.Shelley (ShelleyBased)
import qualified Cardano.Ledger.Shelley as Shelley
import Cardano.Ledger.Val ((<+>), (<×>))
import Control.DeepSeq (NFData)
import Control.Iterate.SetAlgebra
  ( BaseRep (MapR),
    Embed (..),
    Exp (Base),
    HasExp (toExp),
  )
import Data.Coerce (coerce)
import Data.Foldable (toList)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import qualified Data.Maybe as Maybe
import Data.Relation (Relation (..))
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
import Shelley.Spec.Ledger.Hashing (hashAnnotated)
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
import Shelley.Spec.Ledger.PParams (PParams, Update, _keyDeposit, _poolDeposit)
import Shelley.Spec.Ledger.Scripts
import Shelley.Spec.Ledger.Tx (Tx (..))
import Shelley.Spec.Ledger.TxBody
  ( EraIndependentTxBody,
    PoolCert (..),
    PoolParams (..),
    TxId (..),
    TxIn (..),
    TxOut (..),
    Wdrl (..),
    WitVKey (..),
    getRwdCred,
    pattern DeRegKey,
    pattern Delegate,
    pattern Delegation,
  )

instance HasExp (UTxO era) (Map (TxIn era) (TxOut era)) where
  toExp (UTxO x) = Base MapR x

instance Embed (UTxO era) (Map (TxIn era) (TxOut era)) where
  toBase (UTxO x) = x
  fromBase x = (UTxO x)

-- | The unspent transaction outputs.
newtype UTxO era = UTxO {unUTxO :: Map (TxIn era) (TxOut era)}
  deriving (NoThunks, Generic, NFData)

deriving newtype instance
  ShelleyBased era =>
  Eq (UTxO era)

deriving newtype instance
  ShelleyBased era =>
  ToCBOR (UTxO era)

deriving newtype instance
  ShelleyBased era =>
  FromCBOR (UTxO era)

deriving via
  Quiet (UTxO era)
  instance
    ShelleyBased era =>
    Show (UTxO era)

instance Relation (UTxO era) where
  type Domain (UTxO era) = TxIn era
  type Range (UTxO era) = TxOut era

  singleton k v = UTxO $ Map.singleton k v

  dom (UTxO utxo) = dom utxo

  range (UTxO utxo) = range utxo

  s ◁ (UTxO utxo) = UTxO $ s ◁ utxo

  s ⋪ (UTxO utxo) = UTxO $ s ⋪ utxo

  (UTxO utxo) ▷ s = UTxO $ utxo ▷ s

  (UTxO utxo) ⋫ s = UTxO $ utxo ⋫ s

  (UTxO a) ∪ (UTxO b) = UTxO $ a ∪ b

  (UTxO a) ⨃ (UTxO b) = UTxO $ a ⨃ b

  size (UTxO utxo) = size utxo

  {-# INLINE haskey #-}
  haskey k (UTxO x) = case Map.lookup k x of Just _ -> True; Nothing -> False

  {-# INLINE addpair #-}
  addpair k v (UTxO x) = UTxO (Map.insertWith (\y _z -> y) k v x)

  {-# INLINE removekey #-}
  removekey k (UTxO m) = UTxO (Map.delete k m)

-- | Compute the id of a transaction.
txid ::
  forall era.
  (Shelley.TxBodyConstraints era) =>
  Core.TxBody era ->
  TxId era
txid = TxId . hashAnnotated @_ @era

-- | Compute the UTxO inputs of a transaction.
txins ::
  ( HasField "inputs" (Core.TxBody era) (Set (TxIn era))
  ) =>
  Core.TxBody era ->
  Set (TxIn era)
txins = getField @"inputs"

-- | Compute the transaction outputs of a transaction.
txouts ::
  ( ShelleyBased era,
    HasField "outputs" (Core.TxBody era) (StrictSeq (TxOut era))
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
    transId = txid tx

-- | Lookup a txin for a given UTxO collection
txinLookup ::
  TxIn era ->
  UTxO era ->
  Maybe (TxOut era)
txinLookup txin (UTxO utxo') = Map.lookup txin utxo'

-- | Verify a transaction body witness
verifyWitVKey ::
  ( Typeable kr,
    Era era,
    DSignable (Crypto era) (Hash (Crypto era) EraIndependentTxBody)
  ) =>
  Hash (Crypto era) EraIndependentTxBody ->
  WitVKey kr era ->
  Bool
verifyWitVKey txbodyHash (WitVKey vkey sig) = verifySignedDSIGN vkey txbodyHash (coerce sig)

-- | Create a witness for transaction
makeWitnessVKey ::
  forall era kr.
  ( Era era,
    DSignable (Crypto era) (Hash (Crypto era) EraIndependentTxBody)
  ) =>
  Hash (Crypto era) EraIndependentTxBody ->
  KeyPair kr (Crypto era) ->
  WitVKey 'Witness era
makeWitnessVKey txbodyHash keys =
  WitVKey (asWitness $ vKey keys) (coerce $ signedDSIGN @(Crypto era) (sKey keys) txbodyHash)

-- | Create witnesses for transaction
makeWitnessesVKey ::
  forall era kr.
  ( Era era,
    DSignable (Crypto era) (Hash (Crypto era) EraIndependentTxBody)
  ) =>
  Hash (Crypto era) EraIndependentTxBody ->
  [KeyPair kr (Crypto era)] ->
  Set (WitVKey 'Witness era)
makeWitnessesVKey txbodyHash = Set.fromList . fmap (makeWitnessVKey txbodyHash)

-- | From a list of key pairs and a set of key hashes required for a multi-sig
-- scripts, return the set of required keys.
makeWitnessesFromScriptKeys ::
  ( Era era,
    DSignable (Crypto era) (Hash (Crypto era) EraIndependentTxBody)
  ) =>
  Hash (Crypto era) EraIndependentTxBody ->
  Map (KeyHash kr (Crypto era)) (KeyPair kr (Crypto era)) ->
  Set (KeyHash kr (Crypto era)) ->
  Set (WitVKey 'Witness era)
makeWitnessesFromScriptKeys txbodyHash hashKeyMap scriptHashes =
  let witKeys = Map.restrictKeys hashKeyMap scriptHashes
   in makeWitnessesVKey txbodyHash (Map.elems witKeys)

-- | Determine the total balance contained in the UTxO.
balance ::
  ShelleyBased era =>
  UTxO era ->
  Core.Value era
balance (UTxO utxo) = Map.foldl' addTxOuts mempty utxo
  where
    addTxOuts !b (TxOutCompact _ (Core.fromCompact -> a)) = a <+> b

-- | Determine the total deposit amount needed.
-- The block may (legitimately) contain multiple registration certificates
-- for the same pool, where the first will be treated as a registration and
-- any subsequent ones as re-registration. As such, we must only take a
-- deposit for the first such registration.
--
-- Note that this is not an issue for key registrations since subsequent
-- registration certificates would be invalid.
totalDeposits ::
  PParams era ->
  Map (KeyHash 'StakePool (Crypto era)) (PoolParams era) ->
  [DCert era] ->
  Coin
totalDeposits pp stpools cs =
  (numKeys <×> _keyDeposit pp) <+> (numNewPools <×> _poolDeposit pp)
  where
    numKeys = length $ filter isRegKey cs
    pools = Set.fromList . Maybe.catMaybes $ fmap getKeyHashFromRegPool cs
    numNewPools = length $ pools `Set.difference` (Map.keysSet stpools)

getKeyHashFromRegPool :: DCert era -> Maybe (KeyHash 'StakePool (Crypto era))
getKeyHashFromRegPool (DCertPool (RegPool p)) = Just . _poolPubKey $ p
getKeyHashFromRegPool _ = Nothing

txup ::
  ( ShelleyBased era,
    HasField "update" (Core.TxBody era) (StrictMaybe (Update era))
  ) =>
  Tx era ->
  Maybe (Update era)
txup (Tx txbody _ _) = strictMaybeToMaybe (getField @"update" txbody)

-- | Extract script hash from value address with script.
getScriptHash :: Addr era -> Maybe (ScriptHash era)
getScriptHash (Addr _ (ScriptHashObj hs) _) = Just hs
getScriptHash _ = Nothing

scriptStakeCred ::
  DCert era ->
  Maybe (ScriptHash era)
scriptStakeCred (DCertDeleg (DeRegKey (KeyHashObj _))) = Nothing
scriptStakeCred (DCertDeleg (DeRegKey (ScriptHashObj hs))) = Just hs
scriptStakeCred (DCertDeleg (Delegate (Delegation (KeyHashObj _) _))) = Nothing
scriptStakeCred (DCertDeleg (Delegate (Delegation (ScriptHashObj hs) _))) = Just hs
scriptStakeCred _ = Nothing

scriptCred ::
  Credential kr era ->
  Maybe (ScriptHash era)
scriptCred (KeyHashObj _) = Nothing
scriptCred (ScriptHashObj hs) = Just hs

-- | Computes the set of script hashes required to unlock the transcation inputs
-- and the withdrawals.
scriptsNeeded ::
  ( ShelleyBased era,
    HasField "certs" (Core.TxBody era) (StrictSeq (DCert era)),
    HasField "wdrls" (Core.TxBody era) (Wdrl era),
    HasField "inputs" (Core.TxBody era) (Set (TxIn era))
  ) =>
  UTxO era ->
  Tx era ->
  Set (ScriptHash era)
scriptsNeeded u tx =
  Set.fromList (Map.elems $ Map.mapMaybe (getScriptHash . unTxOut) u'')
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
    unTxOut (TxOut a _) = a
    withdrawals = unWdrl $ getField @"wdrls" $ _body tx
    UTxO u'' = (txinsScript (getField @"inputs" $ _body tx) u) ◁ u
    -- u'' = Map.restrictKeys v (txinsScript (txins $ _body tx) u)  TODO
    certificates = (toList . getField @"certs" . _body) tx

-- | Compute the subset of inputs of the set 'txInps' for which each input is
-- locked by a script in the UTxO 'u'.
txinsScript ::
  ShelleyBased era =>
  Set (TxIn era) ->
  UTxO era ->
  Set (TxIn era)
txinsScript txInps (UTxO u) = foldr add Set.empty txInps
  where
    -- to get subset, start with empty, and only insert those inputs in txInps that are locked in u
    add input ans = case Map.lookup input u of
      Just (TxOut (Addr _ (ScriptHashObj _) _) _) -> Set.insert input ans
      Just _ -> ans
      Nothing -> ans
