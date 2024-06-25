{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
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

module Cardano.Ledger.Babel.TxCert (
  -- BabelTxCert (..),
  BabelTxCertUpgradeError (..),
  BabelDelegCert (..),
  BabelGovCert (..),
  Delegatee (..),
  BabelEraTxCert,
  fromShelleyDelegCert,
  toShelleyDelegCert,
  -- getScriptWitnessBabelTxCert,
  pattern RegDepositTxCert,
  pattern UnRegDepositTxCert,
  pattern DelegTxCert,
  pattern RegDepositDelegTxCert,
  pattern AuthCommitteeHotKeyTxCert,
  pattern ResignCommitteeColdTxCert,
  pattern RegDRepTxCert,
  pattern UnRegDRepTxCert,
  pattern UpdateDRepTxCert,
)
where

import Cardano.Ledger.Babbage.Core
import Cardano.Ledger.Babel.Era (BabelEra)
import Cardano.Ledger.Babel.PParams ()
import Cardano.Ledger.BaseTypes (StrictMaybe (..), kindObject)
import Cardano.Ledger.Coin (Coin (..))
import Cardano.Ledger.Conway.Governance (Anchor)
import Cardano.Ledger.Conway.PParams (ConwayEraPParams, ppDRepDepositL)
import Cardano.Ledger.Conway.TxCert (
  ConwayDelegCert (ConwayDelegCert, ConwayRegCert, ConwayRegDelegCert, ConwayUnRegCert),
  ConwayEraTxCert (..),
  ConwayGovCert (
    ConwayAuthCommitteeHotKey,
    ConwayRegDRep,
    ConwayResignCommitteeColdKey,
    ConwayUnRegDRep,
    ConwayUpdateDRep
  ),
  ConwayTxCert (ConwayTxCertDeleg, ConwayTxCertGov, ConwayTxCertPool),
  Delegatee (..),
  getScriptWitnessConwayTxCert,
  getVKeyWitnessConwayTxCert,
 )
import Cardano.Ledger.Credential (
  Credential (..),
  StakeCredential,
 )
import Cardano.Ledger.Crypto
import Cardano.Ledger.Keys (KeyHash, KeyRole (..))
import Cardano.Ledger.Shelley.TxCert (
  ShelleyDelegCert (..),
  shelleyTotalDepositsTxCerts,
  shelleyTotalRefundsTxCerts,
 )
import Cardano.Ledger.Val (Val (..))
import Control.DeepSeq (NFData)
import Data.Aeson (ToJSON (..), (.=))
import Data.Foldable (foldMap', foldl')
import qualified Data.Map.Strict as Map
import Data.Monoid (Sum (getSum))
import GHC.Generics (Generic)
import Lens.Micro
import NoThunks.Class (NoThunks)

data BabelTxCertUpgradeError
  = MirTxCertExpunged
  | GenesisDelegTxCertExpunged
  deriving (Eq, Show)

instance Crypto c => EraTxCert (BabelEra c) where
  type TxCert (BabelEra c) = ConwayTxCert (BabelEra c)

  type TxCertUpgradeError (BabelEra c) = BabelTxCertUpgradeError

  upgradeTxCert = \case
    RegPoolTxCert poolParams -> Right $ RegPoolTxCert poolParams
    RetirePoolTxCert poolId epochNo -> Right $ RetirePoolTxCert poolId epochNo
    RegTxCert cred -> Right $ RegTxCert cred
    UnRegTxCert cred -> Right $ UnRegTxCert cred
    DelegStakeTxCert cred poolId -> Right $ DelegStakeTxCert cred poolId
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

  getTotalRefundsTxCerts = babelTotalRefundsTxCerts

  getTotalDepositsTxCerts = babelTotalDepositsTxCerts

instance Crypto c => ShelleyEraTxCert (BabelEra c) where
  mkRegTxCert c = ConwayTxCertDeleg $ ConwayRegCert c SNothing

  getRegTxCert (ConwayTxCertDeleg (ConwayRegCert c _)) = Just c
  getRegTxCert _ = Nothing

  mkUnRegTxCert c = ConwayTxCertDeleg $ ConwayUnRegCert c SNothing

  getUnRegTxCert (ConwayTxCertDeleg (ConwayUnRegCert c _)) = Just c
  getUnRegTxCert _ = Nothing

  mkDelegStakeTxCert c kh = ConwayTxCertDeleg $ ConwayDelegCert c (DelegStake kh)

  getDelegStakeTxCert (ConwayTxCertDeleg (ConwayDelegCert c (DelegStake kh))) = Just (c, kh)
  getDelegStakeTxCert _ = Nothing

  mkGenesisDelegTxCert = notSupportedInThisEra
  getGenesisDelegTxCert _ = Nothing

  mkMirTxCert = notSupportedInThisEra
  getMirTxCert = const Nothing

instance Crypto c => ConwayEraTxCert (BabelEra c) where
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

class ConwayEraTxCert era => BabelEraTxCert era

instance Crypto c => BabelEraTxCert (BabelEra c)

pattern RegDepositTxCert ::
  BabelEraTxCert era =>
  StakeCredential (EraCrypto era) ->
  Coin ->
  TxCert era
pattern RegDepositTxCert cred c <- (getRegDepositTxCert -> Just (cred, c))
  where
    RegDepositTxCert cred c = mkRegDepositTxCert cred c

pattern UnRegDepositTxCert ::
  BabelEraTxCert era =>
  StakeCredential (EraCrypto era) ->
  Coin ->
  TxCert era
pattern UnRegDepositTxCert cred c <- (getUnRegDepositTxCert -> Just (cred, c))
  where
    UnRegDepositTxCert cred c = mkUnRegDepositTxCert cred c

pattern DelegTxCert ::
  BabelEraTxCert era =>
  StakeCredential (EraCrypto era) ->
  Delegatee (EraCrypto era) ->
  TxCert era
pattern DelegTxCert cred d <- (getDelegTxCert -> Just (cred, d))
  where
    DelegTxCert cred d = mkDelegTxCert cred d

pattern RegDepositDelegTxCert ::
  BabelEraTxCert era =>
  StakeCredential (EraCrypto era) ->
  Delegatee (EraCrypto era) ->
  Coin ->
  TxCert era
pattern RegDepositDelegTxCert cred d c <- (getRegDepositDelegTxCert -> Just (cred, d, c))
  where
    RegDepositDelegTxCert cred d c = mkRegDepositDelegTxCert cred d c

pattern AuthCommitteeHotKeyTxCert ::
  BabelEraTxCert era =>
  Credential 'ColdCommitteeRole (EraCrypto era) ->
  Credential 'HotCommitteeRole (EraCrypto era) ->
  TxCert era
pattern AuthCommitteeHotKeyTxCert ck hk <- (getAuthCommitteeHotKeyTxCert -> Just (ck, hk))
  where
    AuthCommitteeHotKeyTxCert ck hk = mkAuthCommitteeHotKeyTxCert ck hk

pattern ResignCommitteeColdTxCert ::
  BabelEraTxCert era =>
  Credential 'ColdCommitteeRole (EraCrypto era) ->
  StrictMaybe (Anchor (EraCrypto era)) ->
  TxCert era
pattern ResignCommitteeColdTxCert ck a <- (getResignCommitteeColdTxCert -> Just (ck, a))
  where
    ResignCommitteeColdTxCert ck = mkResignCommitteeColdTxCert ck

pattern RegDRepTxCert ::
  BabelEraTxCert era =>
  Credential 'DRepRole (EraCrypto era) ->
  Coin ->
  StrictMaybe (Anchor (EraCrypto era)) ->
  TxCert era
pattern RegDRepTxCert cred deposit mAnchor <- (getRegDRepTxCert -> Just (cred, deposit, mAnchor))
  where
    RegDRepTxCert cred deposit mAnchor = mkRegDRepTxCert cred deposit mAnchor

pattern UnRegDRepTxCert ::
  BabelEraTxCert era =>
  Credential 'DRepRole (EraCrypto era) ->
  Coin ->
  TxCert era
pattern UnRegDRepTxCert cred deposit <- (getUnRegDRepTxCert -> Just (cred, deposit))
  where
    UnRegDRepTxCert cred deposit = mkUnRegDRepTxCert cred deposit

pattern UpdateDRepTxCert ::
  BabelEraTxCert era =>
  Credential 'DRepRole (EraCrypto era) ->
  StrictMaybe (Anchor (EraCrypto era)) ->
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
  , UpdateDRepTxCert
  #-}

-- | Certificates for registration and delegation of stake to Pools and DReps. Comparing
-- to previous eras, there is now ability to:
--
-- * Register and delegate with a single certificate: `BabelRegDelegCert`
--
-- * Ability to delegate to DReps with `DelegVote` and `DelegStakeVote`
--
-- * Ability to specify the deposit amount. Deposits during registration and
--   unregistration in Babel are optional, which will change in the future era. They are
--   optional only for the smooth transition from Babbage to Babel. Validity of deposits
--   is checked by the @CERT@ rule.
data BabelDelegCert c
  = -- | Register staking credential. Deposit, when present, must match the expected deposit
    -- amount specified by `ppKeyDepositL` in the protocol parameters.
    BabelRegCert !(StakeCredential c) !(StrictMaybe Coin)
  | -- | De-Register the staking credential. Deposit, if present, must match the amount
    -- that was left as a deposit upon stake credential registration.
    BabelUnRegCert !(StakeCredential c) !(StrictMaybe Coin)
  | -- | Redelegate to another delegatee. Staking credential must already be registered.
    BabelDelegCert !(StakeCredential c) !(Delegatee c)
  | -- | This is a new type of certificate, which allows to register staking credential
    -- and delegate within a single certificate. Deposit is required and must match the
    -- expected deposit amount specified by `ppKeyDepositL` in the protocol parameters.
    BabelRegDelegCert !(StakeCredential c) !(Delegatee c) !Coin
  deriving (Show, Generic, Eq, Ord)

instance NFData (BabelDelegCert c)

instance NoThunks (BabelDelegCert c)

instance Crypto c => ToJSON (BabelDelegCert c) where
  toJSON = \case
    BabelRegCert cred deposit ->
      kindObject "RegCert" $
        [ "credential" .= toJSON cred
        , "deposit" .= toJSON deposit
        ]
    BabelUnRegCert cred refund ->
      kindObject "UnRegCert" $
        [ "credential" .= toJSON cred
        , "refund" .= toJSON refund
        ]
    BabelDelegCert cred delegatee ->
      kindObject "DelegCert" $
        [ "credential" .= toJSON cred
        , "delegatee" .= toJSON delegatee
        ]
    BabelRegDelegCert cred delegatee deposit ->
      kindObject "RegDelegCert" $
        [ "credential" .= toJSON cred
        , "delegatee" .= toJSON delegatee
        , "deposit" .= toJSON deposit
        ]

data BabelGovCert c
  = BabelRegDRep !(Credential 'DRepRole c) !Coin !(StrictMaybe (Anchor c))
  | BabelUnRegDRep !(Credential 'DRepRole c) !Coin
  | BabelUpdateDRep !(Credential 'DRepRole c) !(StrictMaybe (Anchor c))
  | BabelAuthCommitteeHotKey !(Credential 'ColdCommitteeRole c) !(Credential 'HotCommitteeRole c)
  | BabelResignCommitteeColdKey !(Credential 'ColdCommitteeRole c) !(StrictMaybe (Anchor c))
  deriving (Show, Generic, Eq, Ord)

instance Crypto c => NFData (BabelGovCert c)

instance NoThunks (BabelGovCert c)

instance Crypto c => ToJSON (BabelGovCert c) where
  toJSON = \case
    BabelRegDRep dRep deposit anchor ->
      kindObject "RegDRep" $
        [ "dRep" .= toJSON dRep
        , "deposit" .= toJSON deposit
        , "anchor" .= toJSON anchor
        ]
    BabelUnRegDRep dRep refund ->
      kindObject "UnRegDRep" $
        [ "dRep" .= toJSON dRep
        , "refund" .= toJSON refund
        ]
    BabelUpdateDRep dRep anchor ->
      kindObject "UpdateDRep" $
        [ "dRep" .= toJSON dRep
        , "anchor" .= toJSON anchor
        ]
    BabelAuthCommitteeHotKey coldCred hotCred ->
      kindObject "AuthCommitteeHotKey" $
        [ "coldCredential" .= toJSON coldCred
        , "hotCredential" .= toJSON hotCred
        ]
    BabelResignCommitteeColdKey coldCred anchor ->
      kindObject "ResignCommitteeColdKey" $
        [ "coldCredential" .= toJSON coldCred
        , "anchor" .= toJSON anchor
        ]

-- data BabelTxCert era
--   = BabelTxCertDeleg !(BabelDelegCert (EraCrypto era))
--   | BabelTxCertPool !(PoolCert (EraCrypto era))
--   | BabelTxCertGov !(BabelGovCert (EraCrypto era))
--   deriving (Show, Generic, Eq, Ord)

-- instance Crypto (EraCrypto era) => NFData (BabelTxCert era)

-- instance NoThunks (BabelTxCert era)

-- instance Era era => ToJSON (BabelTxCert era) where
--   toJSON = \case
--     BabelTxCertDeleg delegCert -> toJSON delegCert
--     BabelTxCertPool poolCert -> toJSON poolCert
--     BabelTxCertGov govCert -> toJSON govCert

-- instance
--   ( ShelleyEraTxCert era
--   , TxCert era ~ BabelTxCert era
--   ) =>
--   FromCBOR (BabelTxCert era)
--   where
--   fromCBOR = toPlainDecoder (eraProtVerLow @era) decCBOR

-- instance
--   ( BabelEraTxCert era
--   , TxCert era ~ BabelTxCert era
--   ) =>
--   DecCBOR (BabelTxCert era)
--   where
--   decCBOR = decodeRecordSum "BabelTxCert" $ \case
--     t
--       | 0 <= t && t < 3 -> shelleyTxCertDelegDecoder t
--       | 3 <= t && t < 5 -> poolTxCertDecoder t
--       | t == 5 -> fail "Genesis delegation certificates are no longer supported"
--       | t == 6 -> fail "MIR certificates are no longer supported"
--       | 7 <= t -> babelTxCertDelegDecoder t
--     t -> invalidKey t

-- babelTxCertDelegDecoder :: BabelEraTxCert era => Word -> Decoder s (Int, TxCert era)
-- babelTxCertDelegDecoder = \case
--   7 -> do
--     cred <- decCBOR
--     deposit <- decCBOR
--     pure (3, RegDepositTxCert cred deposit)
--   8 -> do
--     cred <- decCBOR
--     deposit <- decCBOR
--     pure (3, UnRegDepositTxCert cred deposit)
--   9 -> delegCertDecoder 3 (DelegVote <$> decCBOR)
--   10 -> delegCertDecoder 4 (DelegStakeVote <$> decCBOR <*> decCBOR)
--   11 -> regDelegCertDecoder 4 (DelegStake <$> decCBOR)
--   12 -> regDelegCertDecoder 4 (DelegVote <$> decCBOR)
--   13 -> regDelegCertDecoder 5 (DelegStakeVote <$> decCBOR <*> decCBOR)
--   14 -> do
--     cred <- decCBOR
--     key <- decCBOR
--     pure (3, AuthCommitteeHotKeyTxCert cred key)
--   15 -> do
--     cred <- decCBOR
--     a <- decodeNullStrictMaybe decCBOR
--     pure (3, ResignCommitteeColdTxCert cred a)
--   16 -> do
--     cred <- decCBOR
--     deposit <- decCBOR
--     mAnchor <- decodeNullStrictMaybe decCBOR
--     pure (4, RegDRepTxCert cred deposit mAnchor)
--   17 -> do
--     cred <- decCBOR
--     deposit <- decCBOR
--     pure (3, UnRegDRepTxCert cred deposit)
--   18 -> do
--     cred <- decCBOR
--     mAnchor <- decodeNullStrictMaybe decCBOR
--     pure (3, UpdateDRepTxCert cred mAnchor)
--   k -> invalidKey k
--   where
--     delegCertDecoder n decodeDelegatee = do
--       cred <- decCBOR
--       delegatee <- decodeDelegatee
--       pure (n, DelegTxCert cred delegatee)
--     {-# INLINE delegCertDecoder #-}
--     regDelegCertDecoder n decodeDelegatee = do
--       cred <- decCBOR
--       delegatee <- decodeDelegatee
--       deposit <- decCBOR
--       pure (n, RegDepositDelegTxCert cred delegatee deposit)
--     {-# INLINE regDelegCertDecoder #-}
-- {-# INLINE babelTxCertDelegDecoder #-}

-- instance (Era era, Val (Value era)) => ToCBOR (BabelTxCert era) where
--   toCBOR = toPlainEncoding (eraProtVerLow @era) . encCBOR

-- instance (Era era, Val (Value era)) => EncCBOR (BabelTxCert era) where
--   encCBOR = \case
--     BabelTxCertDeleg delegCert -> encodeBabelDelegCert delegCert
--     BabelTxCertPool poolCert -> encodePoolCert poolCert
--     BabelTxCertGov govCert -> encodeGovCert govCert

-- encodeBabelDelegCert :: Crypto c => BabelDelegCert c -> Encoding
-- encodeBabelDelegCert = \case
--   -- Shelley backwards compatibility
--   BabelRegCert cred SNothing -> encodeShelleyDelegCert $ ShelleyRegCert cred
--   BabelUnRegCert cred SNothing -> encodeShelleyDelegCert $ ShelleyUnRegCert cred
--   BabelDelegCert cred (DelegStake poolId) -> encodeShelleyDelegCert $ ShelleyDelegCert cred poolId
--   -- New in Babel
--   BabelRegCert cred (SJust deposit) ->
--     encodeListLen 3
--       <> encodeWord8 7
--       <> encCBOR cred
--       <> encCBOR deposit
--   BabelUnRegCert cred (SJust deposit) ->
--     encodeListLen 3
--       <> encodeWord8 8
--       <> encCBOR cred
--       <> encCBOR deposit
--   BabelDelegCert cred (DelegVote drep) ->
--     encodeListLen 3
--       <> encodeWord8 9
--       <> encCBOR cred
--       <> encCBOR drep
--   BabelDelegCert cred (DelegStakeVote poolId dRep) ->
--     encodeListLen 4
--       <> encodeWord8 10
--       <> encCBOR cred
--       <> encCBOR poolId
--       <> encCBOR dRep
--   BabelRegDelegCert cred (DelegStake poolId) deposit ->
--     encodeListLen 4
--       <> encodeWord8 11
--       <> encCBOR cred
--       <> encCBOR poolId
--       <> encCBOR deposit
--   BabelRegDelegCert cred (DelegVote drep) deposit ->
--     encodeListLen 4
--       <> encodeWord8 12
--       <> encCBOR cred
--       <> encCBOR drep
--       <> encCBOR deposit
--   BabelRegDelegCert cred (DelegStakeVote poolId dRep) deposit ->
--     encodeListLen 5
--       <> encodeWord8 13
--       <> encCBOR cred
--       <> encCBOR poolId
--       <> encCBOR dRep
--       <> encCBOR deposit

-- encodeGovCert :: Crypto c => BabelGovCert c -> Encoding
-- encodeGovCert = \case
--   BabelAuthCommitteeHotKey cred key ->
--     encodeListLen 3
--       <> encodeWord8 14
--       <> encCBOR cred
--       <> encCBOR key
--   BabelResignCommitteeColdKey cred a ->
--     encodeListLen 3
--       <> encodeWord8 15
--       <> encCBOR cred
--       <> encodeNullStrictMaybe encCBOR a
--   BabelRegDRep cred deposit mAnchor ->
--     encodeListLen 4
--       <> encodeWord8 16
--       <> encCBOR cred
--       <> encCBOR deposit
--       <> encodeNullStrictMaybe encCBOR mAnchor
--   BabelUnRegDRep cred deposit ->
--     encodeListLen 3
--       <> encodeWord8 17
--       <> encCBOR cred
--       <> encCBOR deposit
--   BabelUpdateDRep cred mAnchor ->
--     encodeListLen 3
--       <> encodeWord8 18
--       <> encCBOR cred
--       <> encodeNullStrictMaybe encCBOR mAnchor

fromShelleyDelegCert :: ShelleyDelegCert c -> BabelDelegCert c
fromShelleyDelegCert = \case
  ShelleyRegCert cred -> BabelRegCert cred SNothing
  ShelleyUnRegCert cred -> BabelUnRegCert cred SNothing
  ShelleyDelegCert cred poolId -> BabelDelegCert cred (DelegStake poolId)

toShelleyDelegCert :: BabelDelegCert c -> Maybe (ShelleyDelegCert c)
toShelleyDelegCert = \case
  BabelRegCert cred SNothing -> Just $ ShelleyRegCert cred
  BabelUnRegCert cred SNothing -> Just $ ShelleyUnRegCert cred
  BabelDelegCert cred (DelegStake poolId) -> Just $ ShelleyDelegCert cred poolId
  _ -> Nothing

-- For both of the functions `getScriptWitnessBabelTxCert` and
-- `getVKeyWitnessBabelTxCert` we preserve the old behavior of not requiring a witness
-- for staking credential registration, but only during the transitional period of Babel
-- era and only for staking credential registration certificates without a deposit. Future
-- eras will require a witness for registration certificates, because the one without a
-- deposit will be removed.

-- getScriptWitnessBabelTxCert ::
--   BabelTxCert era ->
--   Maybe (ScriptHash (EraCrypto era))
-- getScriptWitnessBabelTxCert = \case
--   BabelTxCertDeleg delegCert ->
--     case delegCert of
--       BabelRegCert _ SNothing -> Nothing
--       BabelRegCert cred (SJust _) -> credScriptHash cred
--       BabelUnRegCert cred _ -> credScriptHash cred
--       BabelDelegCert cred _ -> credScriptHash cred
--       BabelRegDelegCert cred _ _ -> credScriptHash cred
--   -- PoolIds can't be Scripts
--   BabelTxCertPool {} -> Nothing
--   BabelTxCertGov govCert -> govWitness govCert
--   where
--     govWitness :: BabelGovCert c -> Maybe (ScriptHash c)
--     govWitness = \case
--       BabelAuthCommitteeHotKey coldCred _hotCred -> credScriptHash coldCred
--       BabelResignCommitteeColdKey coldCred _ -> credScriptHash coldCred
--       BabelRegDRep cred _ _ -> credScriptHash cred
--       BabelUnRegDRep cred _ -> credScriptHash cred
--       BabelUpdateDRep cred _ -> credScriptHash cred

-- getVKeyWitnessBabelTxCert :: BabelTxCert era -> Maybe (KeyHash 'Witness (EraCrypto era))
-- getVKeyWitnessBabelTxCert = \case
--   BabelTxCertDeleg delegCert ->
--     case delegCert of
--       BabelRegCert _ SNothing -> Nothing
--       BabelRegCert cred (SJust _) -> credKeyHashWitness cred
--       BabelUnRegCert cred _ -> credKeyHashWitness cred
--       BabelDelegCert cred _ -> credKeyHashWitness cred
--       BabelRegDelegCert cred _ _ -> credKeyHashWitness cred
--   BabelTxCertPool poolCert -> Just $ poolCertKeyHashWitness poolCert
--   BabelTxCertGov govCert -> govWitness govCert
--   where
--     govWitness :: BabelGovCert c -> Maybe (KeyHash 'Witness c)
--     govWitness = \case
--       BabelAuthCommitteeHotKey coldCred _hotCred -> credKeyHashWitness coldCred
--       BabelResignCommitteeColdKey coldCred _ -> credKeyHashWitness coldCred
--       BabelRegDRep cred _ _ -> credKeyHashWitness cred
--       BabelUnRegDRep cred _ -> credKeyHashWitness cred
--       BabelUpdateDRep cred _ -> credKeyHashWitness cred

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
babelTotalDepositsTxCerts ::
  (ConwayEraPParams era, Foldable f, BabelEraTxCert era) =>
  PParams era ->
  -- | Check whether a pool with a supplied PoolStakeId is already registered.
  (KeyHash 'StakePool (EraCrypto era) -> Bool) ->
  f (TxCert era) ->
  Coin
babelTotalDepositsTxCerts pp isRegPoolRegistered certs =
  shelleyTotalDepositsTxCerts pp isRegPoolRegistered certs
    <+> babelDRepDepositsTxCerts pp certs

babelDRepDepositsTxCerts ::
  (ConwayEraPParams era, Foldable f, BabelEraTxCert era) =>
  PParams era ->
  f (TxCert era) ->
  Coin
babelDRepDepositsTxCerts pp certs = nDReps <×> depositPerDRep
  where
    nDReps = getSum @Int (foldMap' (\case RegDRepTxCert {} -> 1; _ -> 0) certs)
    depositPerDRep = pp ^. ppDRepDepositL

-- | Compute the key deregistration refunds in a transaction
babelTotalRefundsTxCerts ::
  (EraPParams era, Foldable f, BabelEraTxCert era) =>
  PParams era ->
  -- | Function that can lookup current deposit, in case when the Staking credential is registered.
  (Credential 'Staking (EraCrypto era) -> Maybe Coin) ->
  -- | Function that can lookup current deposit, in case when the DRep credential is registered.
  (Credential 'DRepRole (EraCrypto era) -> Maybe Coin) ->
  f (TxCert era) ->
  Coin
babelTotalRefundsTxCerts pp lookupStakingDeposit lookupDRepDeposit certs =
  shelleyTotalRefundsTxCerts pp lookupStakingDeposit certs
    <+> babelDRepRefundsTxCerts lookupDRepDeposit certs

-- | Compute the Refunds from a TxBody, given a function that computes a partial Coin for
-- known Credentials.
babelDRepRefundsTxCerts ::
  (Foldable f, BabelEraTxCert era) =>
  (Credential 'DRepRole (EraCrypto era) -> Maybe Coin) ->
  f (TxCert era) ->
  Coin
babelDRepRefundsTxCerts lookupDRepDeposit = snd . foldl' go (Map.empty, Coin 0)
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
