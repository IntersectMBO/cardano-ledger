{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Cardano.Ledger.Shelley.State.CertState (
  ShelleyCertState (..),
  mkShelleyCertState,
  shelleyCertDStateL,
  shelleyCertPStateL,
  shelleyObligationCertState,
  shelleyCertsTotalDepositsTxBody,
  shelleyCertsTotalRefundsTxBody,
) where

import Cardano.Ledger.BaseTypes (KeyValuePairs (..), ToKeyValuePairs (..))
import Cardano.Ledger.Binary (
  DecShareCBOR (..),
  EncCBOR (..),
  Interns,
  decSharePlusLensCBOR,
  decodeRecordNamedT,
  encodeListLen,
 )
import Cardano.Ledger.Coin (Coin (..))
import Cardano.Ledger.Compactible (fromCompact)
import Cardano.Ledger.Core
import Cardano.Ledger.Credential (Credential (..))
import Cardano.Ledger.Shelley.Era (ShelleyEra)
import Cardano.Ledger.Shelley.State.Account ()
import Cardano.Ledger.State
import Control.DeepSeq (NFData (..))
import Data.Aeson (ToJSON (..), (.=))
import Data.Default (Default (..))
import qualified Data.Foldable as F
import qualified Data.Map.Strict as Map
import GHC.Generics (Generic)
import Lens.Micro (Lens', lens, (&), (.~), (^.))
import NoThunks.Class (NoThunks (..))

data ShelleyCertState era = ShelleyCertState
  { shelleyCertPState :: !(PState era)
  , shelleyCertDState :: !(DState era)
  }
  deriving (Generic)

deriving instance Eq (Accounts era) => Eq (ShelleyCertState era)

deriving instance Show (Accounts era) => Show (ShelleyCertState era)

deriving via
  KeyValuePairs (ShelleyCertState era)
  instance
    ToJSON (Accounts era) => ToJSON (ShelleyCertState era)

mkShelleyCertState :: EraCertState era => PState era -> DState era -> CertState era
mkShelleyCertState p d =
  def
    & certPStateL .~ p
    & certDStateL .~ d

shelleyCertDStateL :: Lens' (ShelleyCertState era) (DState era)
shelleyCertDStateL = lens shelleyCertDState (\ds u -> ds {shelleyCertDState = u})
{-# INLINE shelleyCertDStateL #-}

shelleyCertPStateL :: Lens' (ShelleyCertState era) (PState era)
shelleyCertPStateL = lens shelleyCertPState (\ds u -> ds {shelleyCertPState = u})
{-# INLINE shelleyCertPStateL #-}

shelleyObligationCertState :: EraCertState era => CertState era -> Obligations
shelleyObligationCertState certState =
  Obligations
    { oblStake = sumDepositsAccounts (certState ^. certDStateL . accountsL)
    , oblPool = fromCompact $ F.foldMap' spsDeposit (certState ^. certPStateL . psStakePoolsL)
    , oblDRep = Coin 0
    , oblProposal = Coin 0
    }

shelleyCertsTotalDepositsTxBody ::
  EraTxBody era => PParams era -> ShelleyCertState era -> TxBody era -> Coin
shelleyCertsTotalDepositsTxBody pp ShelleyCertState {shelleyCertPState} =
  getTotalDepositsTxBody pp (`Map.member` psStakePools shelleyCertPState)

shelleyCertsTotalRefundsTxBody ::
  (EraTxBody era, EraAccounts era) => PParams era -> ShelleyCertState era -> TxBody era -> Coin
shelleyCertsTotalRefundsTxBody pp ShelleyCertState {shelleyCertDState} =
  getTotalRefundsTxBody
    pp
    (lookupDepositDState shelleyCertDState)
    (const Nothing)

instance EraCertState ShelleyEra where
  type CertState ShelleyEra = ShelleyCertState ShelleyEra

  certDStateL = shelleyCertDStateL
  {-# INLINE certDStateL #-}

  certPStateL = shelleyCertPStateL
  {-# INLINE certPStateL #-}

  obligationCertState = shelleyObligationCertState

  certsTotalDepositsTxBody = shelleyCertsTotalDepositsTxBody

  certsTotalRefundsTxBody = shelleyCertsTotalRefundsTxBody

instance ToJSON (Accounts era) => ToKeyValuePairs (ShelleyCertState era) where
  toKeyValuePairs certState@(ShelleyCertState _ _) =
    let ShelleyCertState {..} = certState
     in [ "dstate" .= shelleyCertDState
        , "pstate" .= shelleyCertPState
        ]

instance EraAccounts era => EncCBOR (ShelleyCertState era) where
  encCBOR ShelleyCertState {shelleyCertPState, shelleyCertDState} =
    encodeListLen 2
      <> encCBOR shelleyCertPState
      <> encCBOR shelleyCertDState

instance EraAccounts era => DecShareCBOR (ShelleyCertState era) where
  type
    Share (ShelleyCertState era) =
      ( Interns (Credential 'Staking)
      , Interns (KeyHash 'StakePool)
      , Interns (Credential 'DRepRole)
      , Interns (Credential 'HotCommitteeRole)
      )
  decSharePlusCBOR = decodeRecordNamedT "ShelleyCertState" (const 2) $ do
    shelleyCertPState <-
      decSharePlusLensCBOR $
        lens (\(_, ks, _, _) -> (mempty, ks)) (\(cs, _, cd, ch) (_, ks) -> (cs, ks, cd, ch))
    shelleyCertDState <-
      decSharePlusLensCBOR $
        lens (\(cs, ks, cd, _) -> (cs, ks, cd)) (\(_, _, _, ch) (cs, ks, cd) -> (cs, ks, cd, ch))
    pure ShelleyCertState {..}

instance Default (Accounts era) => Default (ShelleyCertState era) where
  def = ShelleyCertState def def

instance (Era era, NoThunks (Accounts era)) => NoThunks (ShelleyCertState era)

instance (Era era, NFData (Accounts era)) => NFData (ShelleyCertState era)
