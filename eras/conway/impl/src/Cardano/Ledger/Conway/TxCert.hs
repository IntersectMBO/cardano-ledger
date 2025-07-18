{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE UndecidableSuperClasses #-}
{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Cardano.Ledger.Conway.TxCert (
  ConwayTxCert (..),
  ConwayTxCertUpgradeError (..),
  ConwayDelegCert (..),
  ConwayGovCert (..),
  Delegatee (..),
  mkDelegatee,
  ConwayEraTxCert (..),
  fromShelleyDelegCert,
  toShelleyDelegCert,
  getScriptWitnessConwayTxCert,
  getVKeyWitnessConwayTxCert,
  getDelegateeTxCert,
  getStakePoolDelegatee,
  getDRepDelegatee,
  getVoteDelegatee,
  conwayDRepDepositsTxCerts,
  conwayDRepRefundsTxCerts,
  conwayTotalDepositsTxCerts,
  conwayTotalRefundsTxCerts,
  pattern RegDepositTxCert,
  pattern UnRegDepositTxCert,
  pattern DelegTxCert,
  pattern RegDepositDelegTxCert,
  pattern AuthCommitteeHotKeyTxCert,
  pattern ResignCommitteeColdTxCert,
  pattern RegDRepTxCert,
  pattern UnRegDRepTxCert,
  pattern UpdateDRepTxCert,
) where

import Cardano.Ledger.Babbage.Core
import Cardano.Ledger.BaseTypes (Anchor, StrictMaybe (..), invalidKey, kindObject)
import Cardano.Ledger.Binary (
  DecCBOR (..),
  Decoder,
  EncCBOR (..),
  FromCBOR (..),
  ToCBOR (..),
  decodeNullStrictMaybe,
  decodeRecordSum,
  encodeListLen,
  encodeNullStrictMaybe,
  encodeWord8,
  toPlainEncoding,
 )
import Cardano.Ledger.Binary.Coders (Decode (..), Encode (..), decode, encode, (!>), (<!))
import Cardano.Ledger.Coin (Coin (..))
import Cardano.Ledger.Conway.Era (ConwayEra)
import Cardano.Ledger.Conway.PParams (ConwayEraPParams, ppDRepDepositL)
import Cardano.Ledger.Credential (
  Credential (..),
  StakeCredential,
  credKeyHashWitness,
  credScriptHash,
 )
import Cardano.Ledger.DRep (DRep)
import Cardano.Ledger.Shelley.TxCert (
  ShelleyDelegCert (..),
  encodePoolCert,
  encodeShelleyDelegCert,
  poolTxCertDecoder,
  shelleyTotalDepositsTxCerts,
  shelleyTotalRefundsTxCerts,
  shelleyTxCertDelegDecoder,
 )
import Cardano.Ledger.Val (Val (..))
import Control.DeepSeq (NFData)
import Data.Aeson (FromJSON (..), ToJSON (..), withObject, (.:?), (.=))
import Data.Foldable as F (foldMap', foldl')
import qualified Data.Map.Strict as Map
import Data.Monoid (Sum (getSum))
import GHC.Generics (Generic)
import Lens.Micro
import NoThunks.Class (NoThunks)

data ConwayTxCertUpgradeError
  = MirTxCertExpunged
  | GenesisDelegTxCertExpunged
  deriving (Eq, Show)

instance EraTxCert ConwayEra where
  type TxCert ConwayEra = ConwayTxCert ConwayEra

  type TxCertUpgradeError ConwayEra = ConwayTxCertUpgradeError

  upgradeTxCert = \case
    RegPoolTxCert poolParams -> Right $ RegPoolTxCert poolParams
    RetirePoolTxCert poolId epochNo -> Right $ RetirePoolTxCert poolId epochNo
    RegTxCert cred -> Right $ RegTxCert cred
    UnRegTxCert cred -> Right $ UnRegTxCert cred
    DelegStakeTxCert cred poolId -> Right $ DelegStakeTxCert cred poolId
    MirTxCert {} -> Left MirTxCertExpunged
    -- Using wildcard here instead of a pattern match on GenesisDelegTxCert in order to
    -- workaround ghc-8.10 disrespecting the completeness pragma.
    _ -> Left GenesisDelegTxCertExpunged

  getVKeyWitnessTxCert = getVKeyWitnessConwayTxCert

  getScriptWitnessTxCert = getScriptWitnessConwayTxCert

  mkRegPoolTxCert = ConwayTxCertPool . RegPool

  getRegPoolTxCert (ConwayTxCertPool (RegPool poolParams)) = Just poolParams
  getRegPoolTxCert _ = Nothing

  mkRetirePoolTxCert poolId epochNo = ConwayTxCertPool $ RetirePool poolId epochNo

  getRetirePoolTxCert (ConwayTxCertPool (RetirePool poolId epochNo)) = Just (poolId, epochNo)
  getRetirePoolTxCert _ = Nothing

  lookupRegStakeTxCert = \case
    RegTxCert c -> Just c
    RegDepositTxCert c _ -> Just c
    RegDepositDelegTxCert c _ _ -> Just c
    _ -> Nothing
  lookupUnRegStakeTxCert = \case
    UnRegTxCert c -> Just c
    UnRegDepositTxCert c _ -> Just c
    _ -> Nothing

  getTotalRefundsTxCerts = conwayTotalRefundsTxCerts

  getTotalDepositsTxCerts = conwayTotalDepositsTxCerts

instance ShelleyEraTxCert ConwayEra where
  mkRegTxCert c = ConwayTxCertDeleg $ ConwayRegCert c SNothing

  getRegTxCert (ConwayTxCertDeleg (ConwayRegCert c SNothing)) = Just c
  getRegTxCert _ = Nothing

  mkUnRegTxCert c = ConwayTxCertDeleg $ ConwayUnRegCert c SNothing

  getUnRegTxCert (ConwayTxCertDeleg (ConwayUnRegCert c SNothing)) = Just c
  getUnRegTxCert _ = Nothing

  mkDelegStakeTxCert c kh = ConwayTxCertDeleg $ ConwayDelegCert c (DelegStake kh)

  getDelegStakeTxCert (ConwayTxCertDeleg (ConwayDelegCert c (DelegStake kh))) = Just (c, kh)
  getDelegStakeTxCert _ = Nothing

  mkGenesisDelegTxCert = notSupportedInThisEra
  getGenesisDelegTxCert _ = Nothing

  mkMirTxCert = notSupportedInThisEra
  getMirTxCert = const Nothing

class ShelleyEraTxCert era => ConwayEraTxCert era where
  mkRegDepositTxCert :: StakeCredential -> Coin -> TxCert era
  getRegDepositTxCert :: TxCert era -> Maybe (StakeCredential, Coin)

  mkUnRegDepositTxCert :: StakeCredential -> Coin -> TxCert era
  getUnRegDepositTxCert :: TxCert era -> Maybe (StakeCredential, Coin)

  mkDelegTxCert ::
    StakeCredential -> Delegatee -> TxCert era
  getDelegTxCert ::
    TxCert era -> Maybe (StakeCredential, Delegatee)

  mkRegDepositDelegTxCert ::
    StakeCredential -> Delegatee -> Coin -> TxCert era
  getRegDepositDelegTxCert ::
    TxCert era -> Maybe (StakeCredential, Delegatee, Coin)

  mkAuthCommitteeHotKeyTxCert ::
    Credential 'ColdCommitteeRole ->
    Credential 'HotCommitteeRole ->
    TxCert era
  getAuthCommitteeHotKeyTxCert ::
    TxCert era ->
    Maybe (Credential 'ColdCommitteeRole, Credential 'HotCommitteeRole)

  mkResignCommitteeColdTxCert ::
    Credential 'ColdCommitteeRole -> StrictMaybe Anchor -> TxCert era
  getResignCommitteeColdTxCert ::
    TxCert era ->
    Maybe (Credential 'ColdCommitteeRole, StrictMaybe Anchor)

  mkRegDRepTxCert ::
    Credential 'DRepRole -> Coin -> StrictMaybe Anchor -> TxCert era
  getRegDRepTxCert ::
    TxCert era ->
    Maybe (Credential 'DRepRole, Coin, StrictMaybe Anchor)

  mkUnRegDRepTxCert :: Credential 'DRepRole -> Coin -> TxCert era
  getUnRegDRepTxCert :: TxCert era -> Maybe (Credential 'DRepRole, Coin)

  mkUpdateDRepTxCert ::
    Credential 'DRepRole -> StrictMaybe Anchor -> TxCert era
  getUpdateDRepTxCert ::
    TxCert era -> Maybe (Credential 'DRepRole, StrictMaybe Anchor)

instance ConwayEraTxCert ConwayEra where
  mkRegDepositTxCert cred c = ConwayTxCertDeleg $ ConwayRegCert cred $ SJust c

  getRegDepositTxCert (ConwayTxCertDeleg (ConwayRegCert cred (SJust c))) = Just (cred, c)
  getRegDepositTxCert _ = Nothing

  mkUnRegDepositTxCert cred c = ConwayTxCertDeleg $ ConwayUnRegCert cred (SJust c)
  getUnRegDepositTxCert (ConwayTxCertDeleg (ConwayUnRegCert cred (SJust c))) = Just (cred, c)
  getUnRegDepositTxCert _ = Nothing

  mkDelegTxCert cred d = ConwayTxCertDeleg $ ConwayDelegCert cred d
  getDelegTxCert (ConwayTxCertDeleg (ConwayDelegCert cred d)) = Just (cred, d)
  getDelegTxCert _ = Nothing

  mkRegDepositDelegTxCert cred d c = ConwayTxCertDeleg $ ConwayRegDelegCert cred d c
  getRegDepositDelegTxCert (ConwayTxCertDeleg (ConwayRegDelegCert cred d c)) = Just (cred, d, c)
  getRegDepositDelegTxCert _ = Nothing

  mkAuthCommitteeHotKeyTxCert ck hk = ConwayTxCertGov $ ConwayAuthCommitteeHotKey ck hk
  getAuthCommitteeHotKeyTxCert (ConwayTxCertGov (ConwayAuthCommitteeHotKey ck hk)) = Just (ck, hk)
  getAuthCommitteeHotKeyTxCert _ = Nothing

  mkResignCommitteeColdTxCert ck a = ConwayTxCertGov $ ConwayResignCommitteeColdKey ck a
  getResignCommitteeColdTxCert (ConwayTxCertGov (ConwayResignCommitteeColdKey ck a)) = Just (ck, a)
  getResignCommitteeColdTxCert _ = Nothing

  mkRegDRepTxCert cred deposit mAnchor = ConwayTxCertGov $ ConwayRegDRep cred deposit mAnchor
  getRegDRepTxCert = \case
    ConwayTxCertGov (ConwayRegDRep cred deposit mAnchor) -> Just (cred, deposit, mAnchor)
    _ -> Nothing

  mkUnRegDRepTxCert cred deposit = ConwayTxCertGov $ ConwayUnRegDRep cred deposit
  getUnRegDRepTxCert = \case
    ConwayTxCertGov (ConwayUnRegDRep cred deposit) -> Just (cred, deposit)
    _ -> Nothing

  mkUpdateDRepTxCert cred mAnchor = ConwayTxCertGov $ ConwayUpdateDRep cred mAnchor
  getUpdateDRepTxCert = \case
    ConwayTxCertGov (ConwayUpdateDRep cred mAnchor) -> Just (cred, mAnchor)
    _ -> Nothing

pattern RegDepositTxCert ::
  ConwayEraTxCert era =>
  StakeCredential ->
  Coin ->
  TxCert era
pattern RegDepositTxCert cred c <- (getRegDepositTxCert -> Just (cred, c))
  where
    RegDepositTxCert cred c = mkRegDepositTxCert cred c

pattern UnRegDepositTxCert ::
  ConwayEraTxCert era =>
  StakeCredential ->
  Coin ->
  TxCert era
pattern UnRegDepositTxCert cred c <- (getUnRegDepositTxCert -> Just (cred, c))
  where
    UnRegDepositTxCert cred c = mkUnRegDepositTxCert cred c

pattern DelegTxCert ::
  ConwayEraTxCert era =>
  StakeCredential ->
  Delegatee ->
  TxCert era
pattern DelegTxCert cred d <- (getDelegTxCert -> Just (cred, d))
  where
    DelegTxCert cred d = mkDelegTxCert cred d

pattern RegDepositDelegTxCert ::
  ConwayEraTxCert era =>
  StakeCredential ->
  Delegatee ->
  Coin ->
  TxCert era
pattern RegDepositDelegTxCert cred d c <- (getRegDepositDelegTxCert -> Just (cred, d, c))
  where
    RegDepositDelegTxCert cred d c = mkRegDepositDelegTxCert cred d c

pattern AuthCommitteeHotKeyTxCert ::
  ConwayEraTxCert era =>
  Credential 'ColdCommitteeRole ->
  Credential 'HotCommitteeRole ->
  TxCert era
pattern AuthCommitteeHotKeyTxCert ck hk <- (getAuthCommitteeHotKeyTxCert -> Just (ck, hk))
  where
    AuthCommitteeHotKeyTxCert ck hk = mkAuthCommitteeHotKeyTxCert ck hk

pattern ResignCommitteeColdTxCert ::
  ConwayEraTxCert era =>
  Credential 'ColdCommitteeRole ->
  StrictMaybe Anchor ->
  TxCert era
pattern ResignCommitteeColdTxCert ck a <- (getResignCommitteeColdTxCert -> Just (ck, a))
  where
    ResignCommitteeColdTxCert ck = mkResignCommitteeColdTxCert ck

pattern RegDRepTxCert ::
  ConwayEraTxCert era =>
  Credential 'DRepRole ->
  Coin ->
  StrictMaybe Anchor ->
  TxCert era
pattern RegDRepTxCert cred deposit mAnchor <- (getRegDRepTxCert -> Just (cred, deposit, mAnchor))
  where
    RegDRepTxCert cred deposit mAnchor = mkRegDRepTxCert cred deposit mAnchor

pattern UnRegDRepTxCert ::
  ConwayEraTxCert era =>
  Credential 'DRepRole ->
  Coin ->
  TxCert era
pattern UnRegDRepTxCert cred deposit <- (getUnRegDRepTxCert -> Just (cred, deposit))
  where
    UnRegDRepTxCert cred deposit = mkUnRegDRepTxCert cred deposit

pattern UpdateDRepTxCert ::
  ConwayEraTxCert era =>
  Credential 'DRepRole ->
  StrictMaybe Anchor ->
  TxCert era
pattern UpdateDRepTxCert cred mAnchor <- (getUpdateDRepTxCert -> Just (cred, mAnchor))
  where
    UpdateDRepTxCert cred mAnchor = mkUpdateDRepTxCert cred mAnchor

{-# COMPLETE
  RegPoolTxCert
  , RetirePoolTxCert
  , RegTxCert
  , UnRegTxCert
  , RegDepositTxCert
  , UnRegDepositTxCert
  , DelegTxCert
  , RegDepositDelegTxCert
  , AuthCommitteeHotKeyTxCert
  , ResignCommitteeColdTxCert
  , RegDRepTxCert
  , UnRegDRepTxCert
  , UpdateDRepTxCert ::
    ConwayEra
  #-}

getDelegateeTxCert :: ConwayEraTxCert era => TxCert era -> Maybe Delegatee
getDelegateeTxCert = \case
  DelegTxCert _ delegatee -> Just delegatee
  RegDepositDelegTxCert _ delegatee _ -> Just delegatee
  _ -> Nothing

-- | First type argument is the deposit
data Delegatee
  = DelegStake !(KeyHash 'StakePool)
  | DelegVote !DRep
  | DelegStakeVote !(KeyHash 'StakePool) !DRep
  deriving (Show, Generic, Eq, Ord)

instance ToJSON Delegatee where
  toJSON = \case
    DelegStake poolId ->
      kindObject "DelegStake" ["poolId" .= toJSON poolId]
    DelegVote dRep ->
      kindObject "DelegVote" ["dRep" .= toJSON dRep]
    DelegStakeVote poolId dRep ->
      kindObject "DelegStakeVote" $
        [ "poolId" .= toJSON poolId
        , "dRep" .= toJSON dRep
        ]

instance FromJSON Delegatee where
  parseJSON = withObject "Delegatee" $ \obj -> do
    poolId <- obj .:? "poolId"
    dRep <- obj .:? "dRep"
    case (poolId, dRep) of
      (Just poolId', Nothing) -> pure $ DelegStake poolId'
      (Nothing, Just dRep') -> pure $ DelegVote dRep'
      (Just poolId', Just dRep') -> pure $ DelegStakeVote poolId' dRep'
      _ -> fail "Object does not contain a dRep or a poolId field"

instance EncCBOR Delegatee where
  encCBOR =
    encode . \case
      DelegStake kh -> Sum DelegStake 0 !> To kh
      DelegVote dRep -> Sum DelegVote 1 !> To dRep
      DelegStakeVote kh dRep -> Sum DelegStakeVote 2 !> To kh !> To dRep

instance DecCBOR Delegatee where
  decCBOR = decode $
    Summands "Delegatee" $ \case
      0 -> SumD DelegStake <! From
      1 -> SumD DelegVote <! From
      2 -> SumD DelegStakeVote <! From <! From
      k -> Invalid k

mkDelegatee :: Maybe (KeyHash StakePool) -> Maybe DRep -> Maybe Delegatee
mkDelegatee mStakePool mDRep =
  case (mStakePool, mDRep) of
    (Nothing, Nothing) -> Nothing
    (Just pool, Nothing) -> Just $ DelegStake pool
    (Nothing, Just dRep) -> Just $ DelegVote dRep
    (Just pool, Just dRep) -> Just $ DelegStakeVote pool dRep

getStakePoolDelegatee :: Delegatee -> Maybe (KeyHash 'StakePool)
getStakePoolDelegatee = \case
  DelegStake targetPool -> Just targetPool
  DelegVote {} -> Nothing
  DelegStakeVote targetPool _ -> Just targetPool

getDRepDelegatee :: Delegatee -> Maybe DRep
getDRepDelegatee DelegStake {} = Nothing
getDRepDelegatee (DelegVote x) = Just x
getDRepDelegatee (DelegStakeVote _ x) = Just x

getVoteDelegatee :: Delegatee -> Maybe DRep
getVoteDelegatee = getDRepDelegatee
{-# DEPRECATED getVoteDelegatee "In favor of `getDRepDelegatee`" #-}

instance NFData Delegatee

instance NoThunks Delegatee

-- | Certificates for registration and delegation of stake to Pools and DReps. Comparing
-- to previous eras, there is now ability to:
--
-- * Register and delegate with a single certificate: `ConwayRegDelegCert`
--
-- * Ability to delegate to DReps with `DelegVote` and `DelegStakeVote`
--
-- * Ability to specify the deposit amount. Deposits during registration and
--   unregistration in Conway are optional, which will change in the future era. They are
--   optional only for the smooth transition from Babbage to Conway. Validity of deposits
--   is checked by the @CERT@ rule.
data ConwayDelegCert
  = -- | Register staking credential. Deposit, when present, must match the expected deposit
    -- amount specified by `ppKeyDepositL` in the protocol parameters.
    ConwayRegCert !StakeCredential !(StrictMaybe Coin)
  | -- | De-Register the staking credential. Deposit, if present, must match the amount
    -- that was left as a deposit upon stake credential registration.
    ConwayUnRegCert !StakeCredential !(StrictMaybe Coin)
  | -- | Delegate staking credentials to a delegatee. Staking credential must already be registered.
    ConwayDelegCert !StakeCredential !Delegatee
  | -- | This is a new type of certificate, which allows to register staking credential
    -- and delegate within a single certificate. Deposit is required and must match the
    -- expected deposit amount specified by `ppKeyDepositL` in the protocol parameters.
    ConwayRegDelegCert !StakeCredential !Delegatee !Coin
  deriving (Show, Generic, Eq, Ord)

instance EncCBOR ConwayDelegCert where
  encCBOR = \case
    -- Shelley backwards compatibility
    ConwayRegCert cred SNothing -> encodeShelleyDelegCert $ ShelleyRegCert cred
    ConwayUnRegCert cred SNothing -> encodeShelleyDelegCert $ ShelleyUnRegCert cred
    ConwayDelegCert cred (DelegStake poolId) -> encodeShelleyDelegCert $ ShelleyDelegCert cred poolId
    -- New in Conway
    ConwayRegCert cred (SJust deposit) ->
      encodeListLen 3
        <> encodeWord8 7
        <> encCBOR cred
        <> encCBOR deposit
    ConwayUnRegCert cred (SJust deposit) ->
      encodeListLen 3
        <> encodeWord8 8
        <> encCBOR cred
        <> encCBOR deposit
    ConwayDelegCert cred (DelegVote drep) ->
      encodeListLen 3
        <> encodeWord8 9
        <> encCBOR cred
        <> encCBOR drep
    ConwayDelegCert cred (DelegStakeVote poolId dRep) ->
      encodeListLen 4
        <> encodeWord8 10
        <> encCBOR cred
        <> encCBOR poolId
        <> encCBOR dRep
    ConwayRegDelegCert cred (DelegStake poolId) deposit ->
      encodeListLen 4
        <> encodeWord8 11
        <> encCBOR cred
        <> encCBOR poolId
        <> encCBOR deposit
    ConwayRegDelegCert cred (DelegVote drep) deposit ->
      encodeListLen 4
        <> encodeWord8 12
        <> encCBOR cred
        <> encCBOR drep
        <> encCBOR deposit
    ConwayRegDelegCert cred (DelegStakeVote poolId dRep) deposit ->
      encodeListLen 5
        <> encodeWord8 13
        <> encCBOR cred
        <> encCBOR poolId
        <> encCBOR dRep
        <> encCBOR deposit

instance NFData ConwayDelegCert

instance NoThunks ConwayDelegCert

instance ToJSON ConwayDelegCert where
  toJSON = \case
    ConwayRegCert cred deposit ->
      kindObject "RegCert" $
        [ "credential" .= toJSON cred
        , "deposit" .= toJSON deposit
        ]
    ConwayUnRegCert cred refund ->
      kindObject "UnRegCert" $
        [ "credential" .= toJSON cred
        , "refund" .= toJSON refund
        ]
    ConwayDelegCert cred delegatee ->
      kindObject "DelegCert" $
        [ "credential" .= toJSON cred
        , "delegatee" .= toJSON delegatee
        ]
    ConwayRegDelegCert cred delegatee deposit ->
      kindObject "RegDelegCert" $
        [ "credential" .= toJSON cred
        , "delegatee" .= toJSON delegatee
        , "deposit" .= toJSON deposit
        ]

data ConwayGovCert
  = ConwayRegDRep !(Credential 'DRepRole) !Coin !(StrictMaybe Anchor)
  | ConwayUnRegDRep !(Credential 'DRepRole) !Coin
  | ConwayUpdateDRep !(Credential 'DRepRole) !(StrictMaybe Anchor)
  | ConwayAuthCommitteeHotKey !(Credential 'ColdCommitteeRole) !(Credential 'HotCommitteeRole)
  | ConwayResignCommitteeColdKey !(Credential 'ColdCommitteeRole) !(StrictMaybe Anchor)
  deriving (Show, Generic, Eq, Ord)

instance NFData ConwayGovCert

instance NoThunks ConwayGovCert

instance ToJSON ConwayGovCert where
  toJSON = \case
    ConwayRegDRep dRep deposit anchor ->
      kindObject "RegDRep" $
        [ "dRep" .= toJSON dRep
        , "deposit" .= toJSON deposit
        , "anchor" .= toJSON anchor
        ]
    ConwayUnRegDRep dRep refund ->
      kindObject "UnRegDRep" $
        [ "dRep" .= toJSON dRep
        , "refund" .= toJSON refund
        ]
    ConwayUpdateDRep dRep anchor ->
      kindObject "UpdateDRep" $
        [ "dRep" .= toJSON dRep
        , "anchor" .= toJSON anchor
        ]
    ConwayAuthCommitteeHotKey coldCred hotCred ->
      kindObject "AuthCommitteeHotKey" $
        [ "coldCredential" .= toJSON coldCred
        , "hotCredential" .= toJSON hotCred
        ]
    ConwayResignCommitteeColdKey coldCred anchor ->
      kindObject "ResignCommitteeColdKey" $
        [ "coldCredential" .= toJSON coldCred
        , "anchor" .= toJSON anchor
        ]

instance EncCBOR ConwayGovCert where
  encCBOR = \case
    ConwayAuthCommitteeHotKey cred key ->
      encodeListLen 3
        <> encodeWord8 14
        <> encCBOR cred
        <> encCBOR key
    ConwayResignCommitteeColdKey cred a ->
      encodeListLen 3
        <> encodeWord8 15
        <> encCBOR cred
        <> encodeNullStrictMaybe encCBOR a
    ConwayRegDRep cred deposit mAnchor ->
      encodeListLen 4
        <> encodeWord8 16
        <> encCBOR cred
        <> encCBOR deposit
        <> encodeNullStrictMaybe encCBOR mAnchor
    ConwayUnRegDRep cred deposit ->
      encodeListLen 3
        <> encodeWord8 17
        <> encCBOR cred
        <> encCBOR deposit
    ConwayUpdateDRep cred mAnchor ->
      encodeListLen 3
        <> encodeWord8 18
        <> encCBOR cred
        <> encodeNullStrictMaybe encCBOR mAnchor

data ConwayTxCert era
  = ConwayTxCertDeleg !ConwayDelegCert
  | ConwayTxCertPool !PoolCert
  | ConwayTxCertGov !ConwayGovCert
  deriving (Show, Generic, Eq, Ord)

instance NFData (ConwayTxCert era)

instance NoThunks (ConwayTxCert era)

instance Era era => ToJSON (ConwayTxCert era) where
  toJSON = \case
    ConwayTxCertDeleg delegCert -> toJSON delegCert
    ConwayTxCertPool poolCert -> toJSON poolCert
    ConwayTxCertGov govCert -> toJSON govCert

instance
  ( ShelleyEraTxCert era
  , TxCert era ~ ConwayTxCert era
  ) =>
  FromCBOR (ConwayTxCert era)
  where
  fromCBOR = fromEraCBOR @era

instance
  ( ConwayEraTxCert era
  , TxCert era ~ ConwayTxCert era
  ) =>
  DecCBOR (ConwayTxCert era)
  where
  decCBOR = decodeRecordSum "ConwayTxCert" $ \case
    t
      | 0 <= t && t < 3 -> shelleyTxCertDelegDecoder t
      | 3 <= t && t < 5 -> poolTxCertDecoder t
      | t == 5 -> fail "Genesis delegation certificates are no longer supported"
      | t == 6 -> fail "MIR certificates are no longer supported"
      | 7 <= t -> conwayTxCertDelegDecoder t
    t -> invalidKey t

conwayTxCertDelegDecoder :: ConwayEraTxCert era => Word -> Decoder s (Int, TxCert era)
conwayTxCertDelegDecoder = \case
  7 -> do
    cred <- decCBOR
    deposit <- decCBOR
    pure (3, RegDepositTxCert cred deposit)
  8 -> do
    cred <- decCBOR
    deposit <- decCBOR
    pure (3, UnRegDepositTxCert cred deposit)
  9 -> delegCertDecoder 3 (DelegVote <$> decCBOR)
  10 -> delegCertDecoder 4 (DelegStakeVote <$> decCBOR <*> decCBOR)
  11 -> regDelegCertDecoder 4 (DelegStake <$> decCBOR)
  12 -> regDelegCertDecoder 4 (DelegVote <$> decCBOR)
  13 -> regDelegCertDecoder 5 (DelegStakeVote <$> decCBOR <*> decCBOR)
  14 -> do
    cred <- decCBOR
    key <- decCBOR
    pure (3, AuthCommitteeHotKeyTxCert cred key)
  15 -> do
    cred <- decCBOR
    a <- decodeNullStrictMaybe decCBOR
    pure (3, ResignCommitteeColdTxCert cred a)
  16 -> do
    cred <- decCBOR
    deposit <- decCBOR
    mAnchor <- decodeNullStrictMaybe decCBOR
    pure (4, RegDRepTxCert cred deposit mAnchor)
  17 -> do
    cred <- decCBOR
    deposit <- decCBOR
    pure (3, UnRegDRepTxCert cred deposit)
  18 -> do
    cred <- decCBOR
    mAnchor <- decodeNullStrictMaybe decCBOR
    pure (3, UpdateDRepTxCert cred mAnchor)
  k -> invalidKey k
  where
    delegCertDecoder n decodeDelegatee = do
      cred <- decCBOR
      delegatee <- decodeDelegatee
      pure (n, DelegTxCert cred delegatee)
    {-# INLINE delegCertDecoder #-}
    regDelegCertDecoder n decodeDelegatee = do
      cred <- decCBOR
      delegatee <- decodeDelegatee
      deposit <- decCBOR
      pure (n, RegDepositDelegTxCert cred delegatee deposit)
    {-# INLINE regDelegCertDecoder #-}
{-# INLINE conwayTxCertDelegDecoder #-}

instance (Era era, Val (Value era)) => ToCBOR (ConwayTxCert era) where
  toCBOR = toPlainEncoding (eraProtVerLow @era) . encCBOR

instance (Era era, Val (Value era)) => EncCBOR (ConwayTxCert era) where
  encCBOR = \case
    ConwayTxCertDeleg delegCert -> encCBOR delegCert
    ConwayTxCertPool poolCert -> encodePoolCert poolCert
    ConwayTxCertGov govCert -> encCBOR govCert

fromShelleyDelegCert :: ShelleyDelegCert -> ConwayDelegCert
fromShelleyDelegCert = \case
  ShelleyRegCert cred -> ConwayRegCert cred SNothing
  ShelleyUnRegCert cred -> ConwayUnRegCert cred SNothing
  ShelleyDelegCert cred poolId -> ConwayDelegCert cred (DelegStake poolId)

toShelleyDelegCert :: ConwayDelegCert -> Maybe ShelleyDelegCert
toShelleyDelegCert = \case
  ConwayRegCert cred SNothing -> Just $ ShelleyRegCert cred
  ConwayUnRegCert cred SNothing -> Just $ ShelleyUnRegCert cred
  ConwayDelegCert cred (DelegStake poolId) -> Just $ ShelleyDelegCert cred poolId
  _ -> Nothing

-- For both of the functions `getScriptWitnessConwayTxCert` and
-- `getVKeyWitnessConwayTxCert` we preserve the old behavior of not requiring a witness
-- for staking credential registration, but only during the transitional period of Conway
-- era and only for staking credential registration certificates without a deposit. Future
-- eras will require a witness for registration certificates, because the one without a
-- deposit will be removed.

getScriptWitnessConwayTxCert ::
  ConwayTxCert era ->
  Maybe ScriptHash
getScriptWitnessConwayTxCert = \case
  ConwayTxCertDeleg delegCert ->
    case delegCert of
      ConwayRegCert _ SNothing -> Nothing
      ConwayRegCert cred (SJust _) -> credScriptHash cred
      ConwayUnRegCert cred _ -> credScriptHash cred
      ConwayDelegCert cred _ -> credScriptHash cred
      ConwayRegDelegCert cred _ _ -> credScriptHash cred
  -- PoolIds can't be Scripts
  ConwayTxCertPool {} -> Nothing
  ConwayTxCertGov govCert -> govWitness govCert
  where
    govWitness :: ConwayGovCert -> Maybe ScriptHash
    govWitness = \case
      ConwayAuthCommitteeHotKey coldCred _hotCred -> credScriptHash coldCred
      ConwayResignCommitteeColdKey coldCred _ -> credScriptHash coldCred
      ConwayRegDRep cred _ _ -> credScriptHash cred
      ConwayUnRegDRep cred _ -> credScriptHash cred
      ConwayUpdateDRep cred _ -> credScriptHash cred

getVKeyWitnessConwayTxCert :: ConwayTxCert era -> Maybe (KeyHash 'Witness)
getVKeyWitnessConwayTxCert = \case
  ConwayTxCertDeleg delegCert ->
    case delegCert of
      ConwayRegCert _ SNothing -> Nothing
      ConwayRegCert cred (SJust _) -> credKeyHashWitness cred
      ConwayUnRegCert cred _ -> credKeyHashWitness cred
      ConwayDelegCert cred _ -> credKeyHashWitness cred
      ConwayRegDelegCert cred _ _ -> credKeyHashWitness cred
  ConwayTxCertPool poolCert -> Just $ poolCertKeyHashWitness poolCert
  ConwayTxCertGov govCert -> govWitness govCert
  where
    govWitness :: ConwayGovCert -> Maybe (KeyHash 'Witness)
    govWitness = \case
      ConwayAuthCommitteeHotKey coldCred _hotCred -> credKeyHashWitness coldCred
      ConwayResignCommitteeColdKey coldCred _ -> credKeyHashWitness coldCred
      ConwayRegDRep cred _ _ -> credKeyHashWitness cred
      ConwayUnRegDRep cred _ -> credKeyHashWitness cred
      ConwayUpdateDRep cred _ -> credKeyHashWitness cred

-- | Determine the total deposit amount needed from a TxBody.
-- The block may (legitimately) contain multiple registration certificates
-- for the same pool, where the first will be treated as a registration and
-- any subsequent ones as re-registration. As such, we must only take a
-- deposit for the first such registration. It is even possible for a single
-- transaction to have multiple pool registration for the same pool, so as
-- we process pool registrations, we must keep track of those that are already
-- registered, so we do not add a Deposit for the same pool twice.
--
-- Note that this is not an issue for key registrations since subsequent
-- registration certificates would be invalid.
conwayTotalDepositsTxCerts ::
  (ConwayEraPParams era, Foldable f, ConwayEraTxCert era) =>
  PParams era ->
  -- | Check whether a pool with a supplied PoolStakeId is already registered.
  (KeyHash 'StakePool -> Bool) ->
  f (TxCert era) ->
  Coin
conwayTotalDepositsTxCerts pp isRegPoolRegistered certs =
  shelleyTotalDepositsTxCerts pp isRegPoolRegistered certs
    <+> conwayDRepDepositsTxCerts pp certs

conwayDRepDepositsTxCerts ::
  (ConwayEraPParams era, Foldable f, ConwayEraTxCert era) =>
  PParams era ->
  f (TxCert era) ->
  Coin
conwayDRepDepositsTxCerts pp certs = nDReps <×> depositPerDRep
  where
    nDReps = getSum @Int (foldMap' (\case RegDRepTxCert {} -> 1; _ -> 0) certs)
    depositPerDRep = pp ^. ppDRepDepositL

-- | Compute the key deregistration refunds in a transaction
conwayTotalRefundsTxCerts ::
  (EraPParams era, Foldable f, ConwayEraTxCert era) =>
  PParams era ->
  -- | Function that can lookup current deposit, in case when the Staking credential is registered.
  (Credential 'Staking -> Maybe Coin) ->
  -- | Function that can lookup current deposit, in case when the DRep credential is registered.
  (Credential 'DRepRole -> Maybe Coin) ->
  f (TxCert era) ->
  Coin
conwayTotalRefundsTxCerts pp lookupStakingDeposit lookupDRepDeposit certs =
  shelleyTotalRefundsTxCerts pp lookupStakingDeposit certs
    <+> conwayDRepRefundsTxCerts lookupDRepDeposit certs

-- | Compute the Refunds from a TxBody, given a function that computes a partial Coin for
-- known Credentials.
conwayDRepRefundsTxCerts ::
  (Foldable f, ConwayEraTxCert era) =>
  (Credential 'DRepRole -> Maybe Coin) ->
  f (TxCert era) ->
  Coin
conwayDRepRefundsTxCerts lookupDRepDeposit = snd . F.foldl' go (Map.empty, Coin 0)
  where
    go accum@(!drepRegsInTx, !totalRefund) = \case
      RegDRepTxCert cred deposit _ ->
        -- Track registrations
        (Map.insert cred deposit drepRegsInTx, totalRefund)
      UnRegDRepTxCert cred _
        -- DRep previously registered in the same tx.
        | Just deposit <- Map.lookup cred drepRegsInTx ->
            (Map.delete cred drepRegsInTx, totalRefund <+> deposit)
        -- DRep previously registered in some other tx.
        | Just deposit <- lookupDRepDeposit cred -> (drepRegsInTx, totalRefund <+> deposit)
      _ -> accum
