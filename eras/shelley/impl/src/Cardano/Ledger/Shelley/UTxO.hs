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
{-# OPTIONS_GHC -Wno-orphans #-}

module Cardano.Ledger.Shelley.UTxO
  ( ShelleyScriptsNeeded (..),
    totalDeposits,
    scriptsNeeded,
    getShelleyScriptsNeeded,
    scriptCred,
    scriptStakeCred,
    getConsumedCoin,
    produced,
    keyRefunds,
    txup,
    module Core,
  )
where

import Cardano.Ledger.Address (Addr (..))
import Cardano.Ledger.BaseTypes (strictMaybeToMaybe)
import Cardano.Ledger.Coin (Coin)
import Cardano.Ledger.Core
import Cardano.Ledger.Credential (Credential (..))
import Cardano.Ledger.Crypto (Crypto)
import Cardano.Ledger.Keys (KeyHash (..), KeyRole (..))
import Cardano.Ledger.Shelley.Delegation.Certificates
  ( DCert (..),
    isDeRegKey,
    isRegKey,
    requiresVKeyWitness,
  )
import Cardano.Ledger.Shelley.Era (ShelleyEra)
import Cardano.Ledger.Shelley.PParams (ShelleyPParamsHKD (..), Update)
import Cardano.Ledger.Shelley.TxBody
  ( PoolCert (..),
    PoolParams (..),
    ShelleyEraTxBody (..),
    Wdrl (..),
    getRwdCred,
    pattern DeRegKey,
    pattern Delegate,
    pattern Delegation,
  )
import Cardano.Ledger.TxIn (TxIn (..))
import Cardano.Ledger.UTxO
import qualified Cardano.Ledger.UTxO as Core
import Cardano.Ledger.Val ((<+>), (<×>))
import qualified Cardano.Ledger.Val as Val
import Data.Foldable (Foldable (fold), toList)
import qualified Data.Map.Strict as Map
import qualified Data.Maybe as Maybe
import qualified Data.Set as Set
import GHC.Records (HasField (..))
import Lens.Micro ((^.))

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
  (KeyHash 'StakePool c -> Bool) ->
  [DCert c] ->
  Coin
totalDeposits pp isNewPool certs =
  (numKeys <×> getField @"_keyDeposit" pp)
    <+> (numNewPools <×> getField @"_poolDeposit" pp)
  where
    numKeys = length $ filter isRegKey certs
    pools = Set.fromList $ Maybe.mapMaybe getKeyHashFromRegPool certs
    numNewPools = length $ Set.filter isNewPool pools

getKeyHashFromRegPool :: DCert c -> Maybe (KeyHash 'StakePool c)
getKeyHashFromRegPool (DCertPool (RegPool p)) = Just $ ppId p
getKeyHashFromRegPool _ = Nothing

txup :: (EraTx era, ShelleyEraTxBody era) => Tx era -> Maybe (Update era)
txup tx = strictMaybeToMaybe (tx ^. bodyTxL . updateTxBodyL)

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
  Set.Set (ScriptHash (EraCrypto era))
scriptsNeeded u tx =
  case getShelleyScriptsNeeded u (tx ^. bodyTxL) of
    ShelleyScriptsNeeded sn -> sn
{-# DEPRECATED scriptsNeeded "In favor of `getScriptsNeeded`" #-}

-- | Compute the subset of inputs of the set 'txInps' for which each input is
-- locked by a script in the UTxO 'u'.
txinsScriptHashes ::
  EraTxOut era =>
  Set.Set (TxIn (EraCrypto era)) ->
  UTxO era ->
  Set.Set (ScriptHash (EraCrypto era))
txinsScriptHashes txInps (UTxO u) = foldr add Set.empty txInps
  where
    -- to get subset, start with empty, and only insert those inputs in txInps
    -- that are locked in u
    add input ans = case Map.lookup input u of
      Just txOut -> case txOut ^. addrTxOutL of
        Addr _ (ScriptHashObj h) _ -> Set.insert h ans
        _ -> ans
      Nothing -> ans

getShelleyScriptsNeeded ::
  forall era.
  (ShelleyEraTxBody era) =>
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

-- | Compute the lovelace which are created by the transaction
produced ::
  forall era pp.
  ( ShelleyEraTxBody era,
    HasField "_keyDeposit" pp Coin,
    HasField "_poolDeposit" pp Coin
  ) =>
  pp ->
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
  forall era pp.
  ( ShelleyEraTxBody era,
    HasField "_keyDeposit" pp Coin
  ) =>
  pp ->
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
  ( HasField "_keyDeposit" pp Coin,
    ShelleyEraTxBody era
  ) =>
  pp ->
  TxBody era ->
  Coin
keyRefunds pp tx = length deregistrations <×> getField @"_keyDeposit" pp
  where
    deregistrations = filter isDeRegKey (toList $ tx ^. certsTxBodyL)

newtype ShelleyScriptsNeeded era = ShelleyScriptsNeeded (Set.Set (ScriptHash (EraCrypto era)))
  deriving (Eq, Show)

instance Crypto c => EraUTxO (ShelleyEra c) where
  type ScriptsNeeded (ShelleyEra c) = ShelleyScriptsNeeded (ShelleyEra c)
  getConsumedValue = getConsumedCoin

  getScriptsNeeded = getShelleyScriptsNeeded

  getScriptsHashesNeeded (ShelleyScriptsNeeded scriptsHashes) = scriptsHashes
