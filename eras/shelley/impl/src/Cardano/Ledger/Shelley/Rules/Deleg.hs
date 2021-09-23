{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Cardano.Ledger.Shelley.Rules.Deleg
  ( DELEG,
    DelegEnv (..),
    PredicateFailure,
    DelegPredicateFailure (..),
  )
where

import Cardano.Binary
  ( FromCBOR (..),
    ToCBOR (..),
    encodeListLen,
  )
import Cardano.Ledger.BaseTypes
  ( Globals (..),
    ShelleyBase,
    epochInfo,
    invalidKey,
  )
import Cardano.Ledger.Coin (Coin (..), DeltaCoin (..), addDeltaCoin, toDeltaCoin)
import qualified Cardano.Ledger.Core as Core
import Cardano.Ledger.Credential (Credential)
import Cardano.Ledger.Era (Crypto, Era)
import Cardano.Ledger.Keys
  ( GenDelegPair (..),
    GenDelegs (..),
    Hash,
    KeyHash,
    KeyRole (..),
    VerKeyVRF,
  )
import Cardano.Ledger.Serialization (decodeRecordSum)
import Cardano.Ledger.Shelley.HardForks as HardForks
import Cardano.Ledger.Shelley.LedgerState
  ( AccountState (..),
    DState,
    FutureGenDeleg (..),
    InstantaneousRewards (..),
    availableAfterMIR,
    _delegations,
    _fGenDelegs,
    _genDelegs,
    _irwd,
    _ptrs,
    _rewards,
  )
import Cardano.Ledger.Shelley.PParams (ProtVer)
import Cardano.Ledger.Shelley.TxBody
  ( DCert (..),
    DelegCert (..),
    Delegation (..),
    GenesisDelegCert (..),
    MIRCert (..),
    MIRPot (..),
    MIRTarget (..),
    Ptr,
  )
import Cardano.Ledger.Slot
  ( Duration (..),
    EpochNo (..),
    SlotNo,
    epochInfoEpoch,
    epochInfoFirst,
    (*-),
    (+*),
  )
import Control.Monad.Trans.Reader (asks)
import Control.SetAlgebra (dom, eval, range, setSingleton, singleton, (∈), (∉), (∪), (⋪), (⋫), (⨃))
import Control.State.Transition
import Data.Foldable (fold)
import Data.Group (Group (..))
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import Data.Typeable (Typeable)
import Data.Word (Word8)
import GHC.Generics (Generic)
import GHC.Records (HasField)
import NoThunks.Class (NoThunks (..))

data DELEG era

data DelegEnv era = DelegEnv
  { slotNo :: SlotNo,
    ptr_ :: Ptr,
    acnt_ :: AccountState,
    ppDE :: Core.PParams era -- The protocol parameters are only used for the HardFork mechanism
  }

deriving instance (Show (Core.PParams era)) => Show (DelegEnv era)

deriving instance (Eq (Core.PParams era)) => Eq (DelegEnv era)

data DelegPredicateFailure era
  = StakeKeyAlreadyRegisteredDELEG
      !(Credential 'Staking (Crypto era)) -- Credential which is already registered
  | -- | Indicates that the stake key is somehow already in the rewards map.
    --   This error is now redundant with StakeKeyAlreadyRegisteredDELEG.
    --   We should remove it and replace its one use with StakeKeyAlreadyRegisteredDELEG.
    StakeKeyInRewardsDELEG
      !(Credential 'Staking (Crypto era)) -- DEPRECATED, now redundant with StakeKeyAlreadyRegisteredDELEG
  | StakeKeyNotRegisteredDELEG
      !(Credential 'Staking (Crypto era)) -- Credential which is not registered
  | StakeKeyNonZeroAccountBalanceDELEG
      !(Maybe Coin) -- The remaining reward account balance, if it exists
  | StakeDelegationImpossibleDELEG
      !(Credential 'Staking (Crypto era)) -- Credential that is not registered
  | WrongCertificateTypeDELEG -- The DCertPool constructor should not be used by this transition
  | GenesisKeyNotInMappingDELEG
      !(KeyHash 'Genesis (Crypto era)) -- Unknown Genesis KeyHash
  | DuplicateGenesisDelegateDELEG
      !(KeyHash 'GenesisDelegate (Crypto era)) -- Keyhash which is already delegated to
  | InsufficientForInstantaneousRewardsDELEG
      !MIRPot -- which pot the rewards are to be drawn from, treasury or reserves
      !Coin -- amount of rewards to be given out
      !Coin -- size of the pot from which the lovelace is drawn
  | MIRCertificateTooLateinEpochDELEG
      !SlotNo -- current slot
      !SlotNo -- Core.EraRule "MIR" must be submitted before this slot
  | DuplicateGenesisVRFDELEG
      !(Hash (Crypto era) (VerKeyVRF (Crypto era))) --VRF KeyHash which is already delegated to
  | MIRTransferNotCurrentlyAllowed
  | MIRNegativesNotCurrentlyAllowed
  | InsufficientForTransferDELEG
      !MIRPot -- which pot the rewards are to be drawn from, treasury or reserves
      !Coin -- amount attempted to transfer
      !Coin -- amount available
  | MIRProducesNegativeUpdate
  deriving (Show, Eq, Generic)

newtype DelegEvent era = NewEpoch EpochNo

instance
  ( Typeable era,
    HasField "_protocolVersion" (Core.PParams era) ProtVer
  ) =>
  STS (DELEG era)
  where
  type State (DELEG era) = DState (Crypto era)
  type Signal (DELEG era) = DCert (Crypto era)
  type Environment (DELEG era) = DelegEnv era
  type BaseM (DELEG era) = ShelleyBase
  type PredicateFailure (DELEG era) = DelegPredicateFailure era
  type Event (DELEG era) = DelegEvent era

  transitionRules = [delegationTransition]

instance NoThunks (DelegPredicateFailure era)

instance
  (Typeable era, Era era, Typeable (Core.Script era)) =>
  ToCBOR (DelegPredicateFailure era)
  where
  toCBOR = \case
    StakeKeyAlreadyRegisteredDELEG cred ->
      encodeListLen 2 <> toCBOR (0 :: Word8) <> toCBOR cred
    StakeKeyNotRegisteredDELEG cred ->
      encodeListLen 2 <> toCBOR (1 :: Word8) <> toCBOR cred
    StakeKeyNonZeroAccountBalanceDELEG rewardBalance ->
      encodeListLen 2 <> toCBOR (2 :: Word8) <> toCBOR rewardBalance
    StakeDelegationImpossibleDELEG cred ->
      encodeListLen 2 <> toCBOR (3 :: Word8) <> toCBOR cred
    WrongCertificateTypeDELEG ->
      encodeListLen 1 <> toCBOR (4 :: Word8)
    GenesisKeyNotInMappingDELEG gkh ->
      encodeListLen 2 <> toCBOR (5 :: Word8) <> toCBOR gkh
    DuplicateGenesisDelegateDELEG kh ->
      encodeListLen 2 <> toCBOR (6 :: Word8) <> toCBOR kh
    InsufficientForInstantaneousRewardsDELEG pot needed potAmount ->
      encodeListLen 4 <> toCBOR (7 :: Word8)
        <> toCBOR pot
        <> toCBOR needed
        <> toCBOR potAmount
    MIRCertificateTooLateinEpochDELEG sNow sTooLate ->
      encodeListLen 3 <> toCBOR (8 :: Word8) <> toCBOR sNow <> toCBOR sTooLate
    DuplicateGenesisVRFDELEG vrf ->
      encodeListLen 2 <> toCBOR (9 :: Word8) <> toCBOR vrf
    StakeKeyInRewardsDELEG cred ->
      encodeListLen 2 <> toCBOR (10 :: Word8) <> toCBOR cred
    MIRTransferNotCurrentlyAllowed ->
      encodeListLen 1 <> toCBOR (11 :: Word8)
    MIRNegativesNotCurrentlyAllowed ->
      encodeListLen 1 <> toCBOR (12 :: Word8)
    InsufficientForTransferDELEG pot needed available ->
      encodeListLen 4 <> toCBOR (13 :: Word8)
        <> toCBOR pot
        <> toCBOR needed
        <> toCBOR available
    MIRProducesNegativeUpdate ->
      encodeListLen 1 <> toCBOR (14 :: Word8)

instance
  (Era era, Typeable (Core.Script era)) =>
  FromCBOR (DelegPredicateFailure era)
  where
  fromCBOR = decodeRecordSum "PredicateFailure (DELEG era)" $
    \case
      0 -> do
        kh <- fromCBOR
        pure (2, StakeKeyAlreadyRegisteredDELEG kh)
      1 -> do
        kh <- fromCBOR
        pure (2, StakeKeyNotRegisteredDELEG kh)
      2 -> do
        b <- fromCBOR
        pure (2, StakeKeyNonZeroAccountBalanceDELEG b)
      3 -> do
        kh <- fromCBOR
        pure (2, StakeDelegationImpossibleDELEG kh)
      4 -> do
        pure (1, WrongCertificateTypeDELEG)
      5 -> do
        gkh <- fromCBOR
        pure (2, GenesisKeyNotInMappingDELEG gkh)
      6 -> do
        kh <- fromCBOR
        pure (2, DuplicateGenesisDelegateDELEG kh)
      7 -> do
        pot <- fromCBOR
        needed <- fromCBOR
        potAmount <- fromCBOR
        pure (4, InsufficientForInstantaneousRewardsDELEG pot needed potAmount)
      8 -> do
        sNow <- fromCBOR
        sTooLate <- fromCBOR
        pure (3, MIRCertificateTooLateinEpochDELEG sNow sTooLate)
      9 -> do
        vrf <- fromCBOR
        pure (2, DuplicateGenesisVRFDELEG vrf)
      10 -> do
        kh <- fromCBOR
        pure (2, StakeKeyInRewardsDELEG kh)
      11 -> do
        pure (1, MIRTransferNotCurrentlyAllowed)
      12 -> do
        pure (1, MIRNegativesNotCurrentlyAllowed)
      13 -> do
        pot <- fromCBOR
        needed <- fromCBOR
        available <- fromCBOR
        pure (4, InsufficientForTransferDELEG pot needed available)
      14 -> do
        pure (1, MIRProducesNegativeUpdate)
      k -> invalidKey k

delegationTransition ::
  ( Typeable era,
    HasField "_protocolVersion" (Core.PParams era) ProtVer
  ) =>
  TransitionRule (DELEG era)
delegationTransition = do
  TRC (DelegEnv slot ptr acnt pp, ds, c) <- judgmentContext
  case c of
    DCertDeleg (RegKey hk) -> do
      eval (hk ∉ dom (_rewards ds)) ?! StakeKeyAlreadyRegisteredDELEG hk

      pure $
        ds
          { _rewards = eval (_rewards ds ∪ singleton hk mempty),
            _ptrs = eval (_ptrs ds ∪ singleton ptr hk)
          }
    DCertDeleg (DeRegKey hk) -> do
      -- note that pattern match is used instead of cwitness, as in the spec
      eval (hk ∈ dom (_rewards ds)) ?! StakeKeyNotRegisteredDELEG hk

      let rewardCoin = Map.lookup hk (_rewards ds)
      rewardCoin == Just mempty ?! StakeKeyNonZeroAccountBalanceDELEG rewardCoin

      pure $
        ds
          { _rewards = eval (setSingleton hk ⋪ _rewards ds),
            _delegations = eval (setSingleton hk ⋪ _delegations ds),
            _ptrs = eval (_ptrs ds ⋫ setSingleton hk)
          }
    DCertDeleg (Delegate (Delegation hk dpool)) -> do
      -- note that pattern match is used instead of cwitness and dpool, as in the spec
      eval (hk ∈ dom (_rewards ds)) ?! StakeDelegationImpossibleDELEG hk

      pure $
        ds
          { _delegations = eval (_delegations ds ⨃ singleton hk dpool)
          }
    DCertGenesis (GenesisDelegCert gkh vkh vrf) -> do
      sp <- liftSTS $ asks stabilityWindow
      -- note that pattern match is used instead of genesisDeleg, as in the spec
      let s' = slot +* Duration sp
          (GenDelegs genDelegs) = _genDelegs ds

      -- gkh ∈ dom genDelegs ?! GenesisKeyNotInMappingDELEG gkh
      (case Map.lookup gkh genDelegs of Just _ -> True; Nothing -> False) ?! GenesisKeyNotInMappingDELEG gkh

      let cod =
            range $
              Map.filterWithKey (\g _ -> g /= gkh) genDelegs
          fod =
            range $
              Map.filterWithKey (\(FutureGenDeleg _ g) _ -> g /= gkh) (_fGenDelegs ds)
          currentOtherColdKeyHashes = Set.map genDelegKeyHash cod
          currentOtherVrfKeyHashes = Set.map genDelegVrfHash cod
          futureOtherColdKeyHashes = Set.map genDelegKeyHash fod
          futureOtherVrfKeyHashes = Set.map genDelegVrfHash fod

      eval (vkh ∉ (currentOtherColdKeyHashes ∪ futureOtherColdKeyHashes))
        ?! DuplicateGenesisDelegateDELEG vkh
      eval (vrf ∉ (currentOtherVrfKeyHashes ∪ futureOtherVrfKeyHashes))
        ?! DuplicateGenesisVRFDELEG vrf

      pure $
        ds
          { _fGenDelegs = eval (_fGenDelegs ds ⨃ singleton (FutureGenDeleg s' gkh) (GenDelegPair vkh vrf))
          }
    DCertMir (MIRCert targetPot (StakeAddressesMIR credCoinMap)) -> do
      if HardForks.allowMIRTransfer pp
        then do
          sp <- liftSTS $ asks stabilityWindow
          ei <- liftSTS $ asks epochInfo
          EpochNo currEpoch <- liftSTS $ epochInfoEpoch ei slot
          let newEpoch = EpochNo (currEpoch + 1)
          tellEvent (NewEpoch newEpoch)
          firstSlot <- liftSTS $ epochInfoFirst ei newEpoch
          let tooLate = firstSlot *- Duration sp
          slot < tooLate
            ?! MIRCertificateTooLateinEpochDELEG slot tooLate

          let (potAmount, delta, instantaneousRewards) =
                case targetPot of
                  ReservesMIR -> (_reserves acnt, deltaReserves . _irwd $ ds, iRReserves $ _irwd ds)
                  TreasuryMIR -> (_treasury acnt, deltaTreasury . _irwd $ ds, iRTreasury $ _irwd ds)
              credCoinMap' = Map.map (\(DeltaCoin x) -> Coin x) credCoinMap
              combinedMap = Map.unionWith (<>) credCoinMap' instantaneousRewards
              requiredForRewards = fold combinedMap
              available = potAmount `addDeltaCoin` delta

          all (>= mempty) combinedMap ?! MIRProducesNegativeUpdate

          requiredForRewards <= available
            ?! InsufficientForInstantaneousRewardsDELEG targetPot requiredForRewards available

          case targetPot of
            ReservesMIR -> pure $ ds {_irwd = (_irwd ds) {iRReserves = combinedMap}}
            TreasuryMIR -> pure $ ds {_irwd = (_irwd ds) {iRTreasury = combinedMap}}
        else do
          sp <- liftSTS $ asks stabilityWindow
          ei <- liftSTS $ asks epochInfo
          EpochNo currEpoch <- liftSTS $ epochInfoEpoch ei slot
          let newEpoch = EpochNo (currEpoch + 1)
          tellEvent (NewEpoch newEpoch)
          firstSlot <- liftSTS $ epochInfoFirst ei newEpoch
          let tooLate = firstSlot *- Duration sp
          slot < tooLate
            ?! MIRCertificateTooLateinEpochDELEG slot tooLate

          all (>= mempty) credCoinMap ?! MIRNegativesNotCurrentlyAllowed

          let (potAmount, instantaneousRewards) =
                case targetPot of
                  ReservesMIR -> (_reserves acnt, iRReserves $ _irwd ds)
                  TreasuryMIR -> (_treasury acnt, iRTreasury $ _irwd ds)
          let credCoinMap' = Map.map (\(DeltaCoin x) -> Coin x) credCoinMap
              combinedMap = Map.union credCoinMap' instantaneousRewards
              requiredForRewards = fold combinedMap
          requiredForRewards <= potAmount
            ?! InsufficientForInstantaneousRewardsDELEG targetPot requiredForRewards potAmount

          case targetPot of
            ReservesMIR -> pure $ ds {_irwd = (_irwd ds) {iRReserves = combinedMap}}
            TreasuryMIR -> pure $ ds {_irwd = (_irwd ds) {iRTreasury = combinedMap}}
    DCertMir (MIRCert targetPot (SendToOppositePotMIR coin)) ->
      if HardForks.allowMIRTransfer pp
        then do
          sp <- liftSTS $ asks stabilityWindow
          ei <- liftSTS $ asks epochInfo
          EpochNo currEpoch <- liftSTS $ epochInfoEpoch ei slot
          let newEpoch = EpochNo (currEpoch + 1)
          tellEvent (NewEpoch newEpoch)
          firstSlot <- liftSTS $ epochInfoFirst ei newEpoch
          let tooLate = firstSlot *- Duration sp
          slot < tooLate
            ?! MIRCertificateTooLateinEpochDELEG slot tooLate

          let available = availableAfterMIR targetPot acnt (_irwd ds)
          coin <= available
            ?! InsufficientForTransferDELEG targetPot coin available

          let ir = _irwd ds
              dr = deltaReserves ir
              dt = deltaTreasury ir
          case targetPot of
            ReservesMIR ->
              pure $
                ds
                  { _irwd =
                      ir
                        { deltaReserves = dr <> invert (toDeltaCoin coin),
                          deltaTreasury = dt <> toDeltaCoin coin
                        }
                  }
            TreasuryMIR ->
              pure $
                ds
                  { _irwd =
                      ir
                        { deltaReserves = dr <> toDeltaCoin coin,
                          deltaTreasury = dt <> invert (toDeltaCoin coin)
                        }
                  }
        else do
          failBecause MIRTransferNotCurrentlyAllowed
          pure ds
    DCertPool _ -> do
      failBecause WrongCertificateTypeDELEG -- this always fails
      pure ds
