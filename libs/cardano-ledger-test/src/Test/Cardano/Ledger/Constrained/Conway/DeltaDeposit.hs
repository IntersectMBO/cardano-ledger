{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
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
import Cardano.Ledger.Conway.Rules (CertEnv (..), CertsEnv (..), ConwayDelegEnv (..))
import Cardano.Ledger.Conway.TxCert
import Cardano.Ledger.Core
import Cardano.Ledger.Credential (Credential (..))
import Cardano.Ledger.Crypto (Crypto, StandardCrypto)
import Cardano.Ledger.Keys (KeyHash (..), KeyRole (..))
import Cardano.Ledger.PoolParams (PoolParams (..))
import Cardano.Ledger.UMap (CompactForm (CompactCoin), RDPair (..))
import qualified Cardano.Ledger.UMap as UMap
import Constrained (HasSimpleRep (..), HasSpec (..))
import Control.DeepSeq (NFData)
import Data.Foldable (Foldable (..))
import qualified Data.Map as Map
import Data.Map.Strict (Map)
import Data.Maybe.Strict (StrictMaybe (..))
import Data.Sequence (Seq)
import GHC.Generics (Generic)
import Lens.Micro
import Test.Cardano.Ledger.Constrained.Conway.Instances (IsConwayUniv)
import Test.Cardano.Ledger.Conway.TreeDiff (ToExpr)
import Test.QuickCheck (Gen)

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
data DepositKind = MakeDeposit Coin | RefundDeposit Coin deriving (Show)

-- Tracks the difference between the Spec and the STS rules.
newtype DeltaDeposit c = DeltaDeposit (Map (DepositPurpose c) DepositKind)

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
deltaFromCerts pp dd x = foldl' accum dd x
  where
    accum ans (ConwayTxCertDeleg (ConwayRegCert cred (SJust dep))) =
      insertCert (CredentialDeposit cred) (MakeDeposit dep) ans
    accum ans (ConwayTxCertDeleg (ConwayUnRegCert cred (SJust dep))) =
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

-- | insert a DepositKind for a Cert with key DepositPurpose into a DeltaDepositMap
--   If the key is already in the map, then the two DepositKind's MUST be inverses with matching amounts.
--   This is because one can only pay a deposit once, and can only refund it once,
--   and the payment and refund must be the same. If the inverses cancel each other, then the key is removed.
insertCert :: DepositPurpose c -> DepositKind -> DeltaDeposit c -> DeltaDeposit c
insertCert key depositkind (DeltaDeposit m) = DeltaDeposit (Map.alter (alterF key depositkind) key m)

-- | implements the logic described for the function 'insertCert' above.
alterF :: DepositPurpose c -> DepositKind -> Maybe DepositKind -> Maybe DepositKind
-- The key is not present, just insert.
alterF _ depkind Nothing = Just depkind
-- Inverse kinds
alterF key x@(MakeDeposit (Coin n)) (Just y@(RefundDeposit (Coin m))) =
  if n == m
    then Nothing
    else
      error
        ("Non Matching Coin in inverse DepositKinds " ++ show x ++ " " ++ show y ++ " at key " ++ show key)
-- Inverse kinds
alterF key x@(RefundDeposit (Coin m)) (Just y@(MakeDeposit (Coin n))) =
  if n == m
    then Nothing
    else
      error
        ("Non Matching Coin in inverse DepositKinds " ++ show x ++ " " ++ show y ++ " at key " ++ show key)
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
          -- The 'deposit' is computed by the STS rule, and should match the coin in the DepositKind computed by deltaFormCerts
          MakeDeposit coin ->
            if UMap.fromCompact deposit == coin
              then -- Keep the Rewards, but delete the deposit
                (RDPair reward (CompactCoin 0))
              else error ("Deposit in UMap " ++ show deposit ++ " does not match the DepositKind: " ++ show depkind)
          RefundDeposit coin ->
            if UMap.fromCompact deposit == coin
              then -- The Rewards should be deleted, but the deposit should remain
                (RDPair (CompactCoin 0) deposit)
              else error ("Deposit in UMap  " ++ show deposit ++ " does not match the DepositKind: " ++ show depkind)
    accum um _ _ = um

applyDeltaDepositVState :: DeltaDeposit (EraCrypto era) -> VState era -> VState era
applyDeltaDepositVState (DeltaDeposit dd) (VState drepmap y z) = VState (Map.foldlWithKey' accum drepmap dd) y z
  where
    accum dmap (DRepDeposit cred) depkind = Map.adjust fixDRepstate cred dmap
      where
        fixDRepstate (DRepState expiry anchor deposit) = case depkind of
          --- The 'deposit' is computed by the STS rule, and should match the coin in the DepositKind computed by deltaFormCerts
          MakeDeposit coin ->
            if deposit == coin
              then -- Zero out the deposit
                DRepState expiry anchor (Coin 0)
              else
                error
                  ( "Deposit in VState DRepState "
                      ++ show deposit
                      ++ " does not match the DepositKind: "
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
                      ++ " does not match the DepositKind: "
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
          -- The 'deposit' is computed by the STS rule, and should match the coin in the DepositKind computed by deltaFormCerts
          MakeDeposit coin ->
            if deposit == coin
              then -- Zero out the deposit
                Nothing -- Means delete that entry from the map
              else error ("Deposit in PState " ++ show deposit ++ " does not match the DepositKind: " ++ show depkind)
          RefundDeposit coin ->
            if deposit == coin
              then -- the deposit should remain
                Just deposit
              else error ("Deposit in PState " ++ show deposit ++ " does not match the DepositKind: " ++ show depkind)
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
--   the DeltaDeposit trick, we define a type parameterized by the env, signal, and state
--   that adds exactly the same Delta stuff for every use.
data DeltaExecEnv env state signal era = DeltaExecEnv
  { deeEnv :: env
  , deeSignal :: signal
  , deeState :: state
  , deeDeposits :: !(Map (DepositPurpose (EraCrypto era)) Coin) -- A function of deeSignal
  , deeWithdrawals :: !(Map (Network, Credential 'Staking (EraCrypto era)) Coin)
  , deeVotes :: !(VotingProcedures era)
  }

-- All the ExexSpecRule environment types are type synonym correctly parameterized.
type DelegExecEnv era =
  DeltaExecEnv (ConwayDelegEnv era) (DState era) (ConwayDelegCert StandardCrypto) era
type GovCertExecEnv era =
  DeltaExecEnv (PParams era) (VState era) (ConwayGovCert StandardCrypto) era
type CertExecEnv era = DeltaExecEnv (CertEnv era) (CertState era) (ConwayTxCert era) era
type CertsExecEnv era = DeltaExecEnv (CertsEnv era) (CertState era) (Seq (ConwayTxCert era)) era

-- Here are all the Inject instances.
instance Inject (DelegExecEnv era) (ConwayDelegEnv era) where inject = deeEnv
instance Inject (GovCertExecEnv era) (PParams era) where inject = deeEnv
instance Inject (CertExecEnv era) (CertEnv era) where inject = deeEnv
instance Inject (CertsExecEnv era) (CertsEnv era) where inject = deeEnv

-- Here are all the ExecSpecRule State generators
genDeltaExecState :: () -> DeltaExecEnv env state signal era -> Gen state
genDeltaExecState () x = pure (deeState x)

-- Here are all the ExecSpecRule Signal generators
genDeltaExecSignal :: () -> DeltaExecEnv env state signal era -> state -> Gen signal
genDeltaExecSignal () x _ = pure (deeSignal x)

{-
-- All we need now is to write tne ExecSpecRule Env generators!
genDELEGDeltaEnv :: () -> Gen (DelegExecEnv era)
genGOVCERTDeltaEnv :: () -> Gen (GovCertExecEnv era)
genCERTDeltaEnv :: () -> Gen (CertExecEnv era)
genCERTSDeltaEnv :: () -> Gen (CertsExecEnv era)
-}
