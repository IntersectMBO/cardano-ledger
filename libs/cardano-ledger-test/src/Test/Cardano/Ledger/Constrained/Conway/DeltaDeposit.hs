{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE UndecidableInstances #-}

-- | The Spec and the STS rules differ on where Deposits for Certificates are
--   recorded in the Spec.  In the Spec Deposits are recorded in the UTXO rule
--   in the STS rules (in ConwayEra) they are recorded in the "CERT rule.
--   This module helps track those differences.
module Test.Cardano.Ledger.Constrained.Conway.DeltaDeposit where

import Cardano.Ledger.BaseTypes (Inject (..), Network)
import Cardano.Ledger.CertState
import Cardano.Ledger.Coin (Coin (..))
import Cardano.Ledger.Conway.Governance (GovActionId (..), VotingProcedures)
import Cardano.Ledger.Conway.TxCert
import Cardano.Ledger.Core
import Cardano.Ledger.Credential (Credential (..))
import Cardano.Ledger.Crypto (Crypto)
import Cardano.Ledger.Keys (KeyHash (..), KeyRole (..))
import Cardano.Ledger.PoolParams (PoolParams (..))
import Cardano.Ledger.Shelley.Rules
import Cardano.Ledger.UMap (CompactForm (CompactCoin), RDPair (..), rdDepositCoin, rdPairMap)
import qualified Cardano.Ledger.UMap as UMap
import Constrained (HasSimpleRep (..), HasSpec (..))
import Control.DeepSeq (NFData)
import qualified Data.Foldable as F (foldl')
import qualified Data.Map as Map
import Data.Map.Strict (Map)
import Data.Maybe.Strict (StrictMaybe (..))
import GHC.Generics (Generic)
import Lens.Micro
import Test.Cardano.Ledger.Constrained.Conway.Instances (IsConwayUniv)
import Test.Cardano.Ledger.Conway.TreeDiff (ToExpr)
import Test.Cardano.Ledger.Generic.PrettyCore (
  PrettyA (prettyA),
  pcCredential,
  pcGovActionId,
  pcKeyHash,
  ppRecord,
  ppSexp,
 )

-- ==============================================================

-- A Sum type of all the different ways Deposits are recorded.
-- In the STS rules each kind of deposit is stored in map with
-- Credential or KeyHash or GovActionId as the key, Using this type
-- we can use a single Map with DepositPurpose as the key. This is
-- how the Spec does it, rather than in 4 maps in different parts of the Spec.
data DepositPurpose c
  = CredentialDeposit !(Credential 'Staking c)
  | PoolDeposit !(KeyHash 'StakePool c)
  | DRepDeposit !(Credential 'DRepRole c)
  | GovActionDeposit !(GovActionId c)
  deriving (Generic, Eq, Show, Ord)

instance ToExpr (DepositPurpose c)
instance Crypto c => NFData (DepositPurpose c)
instance HasSimpleRep (DepositPurpose c)
instance (IsConwayUniv fn, Crypto c) => HasSpec fn (DepositPurpose c)

-- ======================================================================================

-- Deposits are recorded when the deposit is made (registering) and refunded (deregistering)
data DepositAction = MakeDeposit Coin | RefundDeposit Coin deriving (Show)

-- Tracks the difference between the Spec and the STS rules.
newtype DeltaDeposit c = DeltaDeposit {unDeltaDeposit :: Map (DepositPurpose c) DepositAction}

-- | There are no differences
emptyDeltaDeposit :: DeltaDeposit c
emptyDeltaDeposit = DeltaDeposit Map.empty

-- ========================================================================

-- | Extend a DeltaDeposit from the deposits in a group of Certs
deltaFromCerts ::
  (EraPParams era, Foldable t) =>
  PParams era ->
  DeltaDeposit (EraCrypto era) ->
  t (ConwayTxCert era) ->
  DeltaDeposit (EraCrypto era)
deltaFromCerts pp dd x = F.foldl' accum dd x
  where
    accum ans (ConwayTxCertDeleg (ConwayRegCert cred (SJust dep))) =
      insertCert (CredentialDeposit cred) (MakeDeposit dep) ans
    accum ans (ConwayTxCertDeleg (ConwayUnRegCert cred (SJust dep))) =
      insertCert (CredentialDeposit cred) (RefundDeposit dep) ans
    accum ans (ConwayTxCertDeleg (ConwayRegDelegCert cred _ dep)) =
      insertCert (CredentialDeposit cred) (RefundDeposit dep) ans
    accum ans (ConwayTxCertDeleg _) = ans
    accum ans (ConwayTxCertPool (RegPool (PoolParams {ppId = poolhash}))) =
      insertCert (PoolDeposit poolhash) (MakeDeposit (pp ^. ppPoolDepositL)) ans
    accum ans (ConwayTxCertPool _) = ans
    accum ans (ConwayTxCertGov (ConwayRegDRep cred dep _)) =
      insertCert (DRepDeposit cred) (MakeDeposit dep) ans
    accum ans (ConwayTxCertGov (ConwayUnRegDRep cred dep)) =
      insertCert (DRepDeposit cred) (RefundDeposit dep) ans
    accum ans (ConwayTxCertGov _) = ans

-- | insert a DepositAction for a Cert with key DepositPurpose into a DeltaDepositMap
--   If the key is already in the map, then the two DepositAction's MUST be inverses with matching amounts.
--   This is because one can only pay a deposit once, and can only refund it once,
--   and the payment and refund must be the same. If the inverses cancel each other, then the key is removed.
insertCert :: DepositPurpose c -> DepositAction -> DeltaDeposit c -> DeltaDeposit c
insertCert key depositkind (DeltaDeposit m) = DeltaDeposit (Map.alter (alterF key depositkind) key m)

-- | implements the logic described for the function 'insertCert' above.
alterF :: DepositPurpose c -> DepositAction -> Maybe DepositAction -> Maybe DepositAction
-- The key is not present, just insert.
alterF _ depkind Nothing = Just depkind
-- Inverse kinds
alterF key x@(MakeDeposit (Coin n)) (Just y@(RefundDeposit (Coin m))) =
  if n == m
    then Nothing
    else
      error
        ( "Non Matching Coin in inverse DepositActions " ++ show x ++ " " ++ show y ++ " at key " ++ show key
        )
-- Inverse kinds
alterF key x@(RefundDeposit (Coin m)) (Just y@(MakeDeposit (Coin n))) =
  if n == m
    then Nothing
    else
      error
        ( "Non Matching Coin in inverse DepositActions " ++ show x ++ " " ++ show y ++ " at key " ++ show key
        )
-- Registering the same key twice
alterF key x@(MakeDeposit _) (Just y@(MakeDeposit _)) =
  error ("Double Deposit " ++ show x ++ " " ++ show y ++ " for same key " ++ show key)
-- DeRegistration of the same key twice.
alterF key x@(RefundDeposit _) (Just y@(RefundDeposit _)) =
  error ("Double Refund " ++ show x ++ " " ++ show y ++ " for same key " ++ show key)

-- ========================================================================

applyDeltaDepositDState :: DeltaDeposit (EraCrypto era) -> DState era -> DState era
applyDeltaDepositDState (DeltaDeposit dd) ds = ds {dsUnified = Map.foldlWithKey' accum (dsUnified ds) dd}
  where
    accum um (CredentialDeposit cred) depkind = UMap.adjust fixRDPair cred (UMap.RewDepUView um)
      where
        fixRDPair (RDPair reward deposit) = case depkind of
          -- The 'deposit' is computed by the STS rule, and should match the coin in the DepositAction computed by deltaFormCerts
          MakeDeposit coin ->
            if UMap.fromCompact deposit == coin
              then -- Keep the Rewards, but delete the deposit
              -- Since we can't actually delete the entry from the UMap, the best we can do
              -- is make the rdDeposit field of the RDPair 0
                (RDPair reward (CompactCoin 0))
              else error ("Deposit in UMap " ++ show deposit ++ " does not match the DepositAction: " ++ show depkind)
          RefundDeposit coin ->
            if UMap.fromCompact deposit == coin
              then -- The Rewards should be deleted, but the deposit should remain
                (RDPair (CompactCoin 0) deposit)
              else error ("Deposit in UMap  " ++ show deposit ++ " does not match the DepositAction: " ++ show depkind)
    accum um _ _ = um

applyDeltaDepositVState :: DeltaDeposit (EraCrypto era) -> VState era -> VState era
applyDeltaDepositVState (DeltaDeposit dd) (VState drepmap y z) = VState (Map.foldlWithKey' accum drepmap dd) y z
  where
    accum dmap (DRepDeposit cred) depkind = Map.adjust fixDRepstate cred dmap
      where
        fixDRepstate (DRepState expiry anchor deposit) = case depkind of
          --- The 'deposit' is computed by the STS rule, and should match the coin in the DepositAction computed by deltaFormCerts
          MakeDeposit coin ->
            if deposit == coin
              then -- Zero out the deposit
                DRepState expiry anchor (Coin 0)
              else
                error
                  ( "Deposit in VState DRepState "
                      ++ show deposit
                      ++ " does not match the DepositAction: "
                      ++ show depkind
                  )
          RefundDeposit coin ->
            if deposit == coin
              then -- the deposit should remain
                DRepState expiry anchor deposit
              else
                error
                  ( "Deposit in VState DRepState "
                      ++ show deposit
                      ++ " does not match the DepositAction: "
                      ++ show depkind
                  )
    accum dmap _ _ = dmap

applyDeltaDepositPState :: DeltaDeposit (EraCrypto era) -> PState era -> PState era
applyDeltaDepositPState (DeltaDeposit dd) ps = ps {psDeposits = Map.foldlWithKey' accum (psDeposits ps) dd}
  where
    accum pmap (PoolDeposit cred) depkind = Map.alter fixPoolDeposits cred pmap
      where
        fixPoolDeposits Nothing = Nothing
        fixPoolDeposits (Just deposit) = case depkind of
          -- The 'deposit' is computed by the STS rule, and should match the coin in the DepositAction computed by deltaFormCerts
          MakeDeposit coin ->
            if deposit == coin
              then -- Zero out the deposit
                Nothing -- Means delete that entry from the map
              else
                error
                  ("Deposit in PState " ++ show deposit ++ " does not match the DepositAction: " ++ show depkind)
          RefundDeposit coin ->
            if deposit == coin
              then -- the deposit should remain
                Just deposit
              else
                error
                  ("Deposit in PState " ++ show deposit ++ " does not match the DepositAction: " ++ show depkind)
    accum pmap _ _ = pmap

applyDeltaDepositCertState :: DeltaDeposit (EraCrypto era) -> CertState era -> CertState era
applyDeltaDepositCertState dd cs =
  cs
    { certVState = applyDeltaDepositVState dd (certVState cs)
    , certPState = applyDeltaDepositPState dd (certPState cs)
    , certDState = applyDeltaDepositDState dd (certDState cs)
    }

-- =======================================================

-- | In order to impose exactly the same structure on every ExecEnv that needs
--   the DeltaDeposit trick, we define a type parameterized by the env
--   that adds exactly the same Delta stuff for every use.
data DeltaExecEnv env era = DeltaExecEnv
  { deeEnv :: env
  , deeDeposits :: !(Map (DepositPurpose (EraCrypto era)) Coin) -- A function of deeSignal
  , deeWithdrawals :: !(Map (Network, Credential 'Staking (EraCrypto era)) Coin)
  , deeVotes :: !(VotingProcedures era)
  }
  deriving (Generic)

deriving instance
  (Eq env, Eq (PParamsHKD Identity era)) =>
  Eq (DeltaExecEnv env era)
deriving instance
  (Show env, Show (PParamsHKD Identity era)) =>
  Show (DeltaExecEnv env era)
instance
  (Era era, ToExpr env, ToExpr (PParamsHKD Identity era)) =>
  ToExpr (DeltaExecEnv env era)
instance
  (NFData env, EraPParams era) =>
  NFData (DeltaExecEnv env era)
instance HasSimpleRep (DeltaExecEnv env era)
instance
  ( IsConwayUniv fn
  , EraPParams era
  , HasSpec fn (PParams era)
  , HasSpec fn env
  ) =>
  HasSpec fn (DeltaExecEnv env era)

instance PrettyA (DepositPurpose c) where
  prettyA (CredentialDeposit x) = ppSexp "Stake" [pcCredential x]
  prettyA (PoolDeposit x) = ppSexp "Pool" [pcKeyHash x]
  prettyA (DRepDeposit x) = ppSexp "DRep" [pcCredential x]
  prettyA (GovActionDeposit x) = ppSexp "GovAction" [pcGovActionId x]

instance
  PrettyA env =>
  PrettyA (DeltaExecEnv env era)
  where
  prettyA (DeltaExecEnv env dep with votes) =
    ppRecord
      "DeltaExecEnv"
      [ ("env", prettyA env)
      , ("deposits", prettyA dep)
      , ("withdrawals", prettyA with)
      , ("votes", prettyA votes)
      ]

-- Here are all the Inject instances.
instance Inject (DeltaExecEnv p era) p where inject = deeEnv

-- ========================================================================

agdaDepositFromDstate :: DState era -> Map.Map (DepositPurpose (EraCrypto era)) Coin
agdaDepositFromDstate dstate = Map.map rdDepositCoin (Map.mapKeys CredentialDeposit (rdPairMap (dsUnified dstate)))

agdaDepositFromPstate :: PState era -> Map.Map (DepositPurpose (EraCrypto era)) Coin
agdaDepositFromPstate pstate = Map.mapKeys PoolDeposit (psDeposits pstate)

agdaDepositFromVstate :: VState era -> Map.Map (DepositPurpose (EraCrypto era)) Coin
agdaDepositFromVstate vstate = Map.map drepDeposit (Map.mapKeys DRepDeposit (vsDReps vstate))

agdaDepositFromCertstate :: CertState era -> Map.Map (DepositPurpose (EraCrypto era)) Coin
agdaDepositFromCertstate x =
  Map.unions
    [ agdaDepositFromDstate (certDState x)
    , agdaDepositFromVstate (certVState x)
    , agdaDepositFromPstate (certPState x)
    ]
