{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE UndecidableSuperClasses #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Cardano.Ledger.Conway.State.CertState (
  ConwayCertState (..),
  ConwayEraCertState (..),
  csCommitteeCredsL,
  epochStateRegDrepL,
  mkConwayCertState,
  conwayCertDStateL,
  conwayCertPStateL,
  conwayCertVStateL,
  conwayObligationCertState,
  conwayCertsTotalDepositsTxBody,
  conwayCertsTotalRefundsTxBody,
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
import Cardano.Ledger.Compactible (Compactible (..))
import Cardano.Ledger.Conway.Era (ConwayEra)
import Cardano.Ledger.Conway.State.Account (ConwayEraAccounts)
import Cardano.Ledger.Conway.State.VState
import Cardano.Ledger.Core
import Cardano.Ledger.Credential (Credential (..))
import Cardano.Ledger.Shelley.LedgerState (EpochState (..), esLStateL, lsCertStateL)
import Cardano.Ledger.Shelley.State
import Control.DeepSeq (NFData (..))
import Data.Aeson (ToJSON (..), (.=))
import Data.Default (Default (def))
import qualified Data.Foldable as F
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import GHC.Generics (Generic)
import Lens.Micro (Lens', lens, (&), (.~), (^.), _2)
import NoThunks.Class (NoThunks (..))

data ConwayCertState era = ConwayCertState
  { conwayCertVState :: !(VState era)
  , conwayCertPState :: !(PState era)
  , conwayCertDState :: !(DState era)
  }
  deriving (Generic)

deriving instance Eq (Accounts era) => Eq (ConwayCertState era)

deriving instance Show (Accounts era) => Show (ConwayCertState era)

deriving via
  KeyValuePairs (ConwayCertState era)
  instance
    ToJSON (Accounts era) => ToJSON (ConwayCertState era)

-- ===================================
-- VState

csCommitteeCredsL ::
  Lens' (CommitteeState era) (Map (Credential 'ColdCommitteeRole) CommitteeAuthorization)
csCommitteeCredsL = lens csCommitteeCreds (\cs u -> cs {csCommitteeCreds = u})

epochStateRegDrepL ::
  ConwayEraCertState era => Lens' (EpochState era) (Map (Credential 'DRepRole) DRepState)
epochStateRegDrepL = esLStateL . lsCertStateL . certVStateL . vsDRepsL

class (EraCertState era, ConwayEraAccounts era) => ConwayEraCertState era where
  certVStateL :: Lens' (CertState era) (VState era)

mkConwayCertState ::
  ConwayEraCertState era => VState era -> PState era -> DState era -> CertState era
mkConwayCertState v p d =
  mkShelleyCertState p d & certVStateL .~ v

conwayCertDStateL :: Lens' (ConwayCertState era) (DState era)
conwayCertDStateL = lens conwayCertDState (\ds u -> ds {conwayCertDState = u})
{-# INLINE conwayCertDStateL #-}

conwayCertPStateL :: Lens' (ConwayCertState era) (PState era)
conwayCertPStateL = lens conwayCertPState (\ds u -> ds {conwayCertPState = u})
{-# INLINE conwayCertPStateL #-}

conwayCertVStateL :: Lens' (ConwayCertState era) (VState era)
conwayCertVStateL = lens conwayCertVState (\ds u -> ds {conwayCertVState = u})
{-# INLINE conwayCertVStateL #-}

instance ToJSON (Accounts era) => ToKeyValuePairs (ConwayCertState era) where
  toKeyValuePairs certState@(ConwayCertState _ _ _) =
    let ConwayCertState {..} = certState
     in [ "dstate" .= conwayCertDState
        , "pstate" .= conwayCertPState
        , "vstate" .= conwayCertVState
        ]

conwayObligationCertState :: ConwayEraCertState era => CertState era -> Obligations
conwayObligationCertState certState =
  let accum ans drepState = ans <> drepDeposit drepState
   in (shelleyObligationCertState certState)
        { oblDRep = fromCompact $ F.foldl' accum mempty (certState ^. certVStateL . vsDRepsL)
        }

conwayCertsTotalDepositsTxBody ::
  EraTxBody era => PParams era -> ConwayCertState era -> TxBody era -> Coin
conwayCertsTotalDepositsTxBody pp ConwayCertState {conwayCertPState} =
  getTotalDepositsTxBody pp (`Map.member` psStakePoolState conwayCertPState)

conwayCertsTotalRefundsTxBody ::
  (EraTxBody era, EraAccounts era) => PParams era -> ConwayCertState era -> TxBody era -> Coin
conwayCertsTotalRefundsTxBody pp ConwayCertState {conwayCertDState, conwayCertVState} =
  getTotalRefundsTxBody
    pp
    (lookupDepositDState conwayCertDState)
    (lookupDepositVState conwayCertVState)

instance EraCertState ConwayEra where
  type CertState ConwayEra = ConwayCertState ConwayEra

  certDStateL = conwayCertDStateL
  {-# INLINE certDStateL #-}

  certPStateL = conwayCertPStateL
  {-# INLINE certPStateL #-}

  obligationCertState = conwayObligationCertState

  certsTotalDepositsTxBody = conwayCertsTotalDepositsTxBody

  certsTotalRefundsTxBody = conwayCertsTotalRefundsTxBody

instance ConwayEraCertState ConwayEra where
  certVStateL = conwayCertVStateL
  {-# INLINE certVStateL #-}

instance EraAccounts era => EncCBOR (ConwayCertState era) where
  encCBOR ConwayCertState {conwayCertPState, conwayCertDState, conwayCertVState} =
    encodeListLen 3
      <> encCBOR conwayCertVState
      <> encCBOR conwayCertPState
      <> encCBOR conwayCertDState

instance EraAccounts era => DecShareCBOR (ConwayCertState era) where
  type
    Share (ConwayCertState era) =
      ( Interns (Credential 'Staking)
      , Interns (KeyHash 'StakePool)
      , Interns (Credential 'DRepRole)
      , Interns (Credential 'HotCommitteeRole)
      )
  decSharePlusCBOR = decodeRecordNamedT "ConwayCertState" (const 3) $ do
    conwayCertVState <-
      decSharePlusLensCBOR $
        lens (\(cs, _, cd, ch) -> (cs, cd, ch)) (\(_, ks, _, _) (cs, cd, ch) -> (cs, ks, cd, ch))
    conwayCertPState <- decSharePlusLensCBOR _2
    conwayCertDState <-
      decSharePlusLensCBOR $
        lens (\(cs, ks, cd, _) -> (cs, ks, cd)) (\(_, _, _, ch) (cs, ks, cd) -> (cs, ks, cd, ch))
    pure ConwayCertState {..}

instance Default (Accounts era) => Default (ConwayCertState era) where
  def = ConwayCertState def def def

instance (Era era, NoThunks (Accounts era)) => NoThunks (ConwayCertState era)

instance (Era era, NFData (Accounts era)) => NFData (ConwayCertState era)
