{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Cardano.Ledger.Shelley.CertState (
  ShelleyCertState (..),
  toCertStatePairs,
  mkShelleyCertState,
  shelleyCertDStateL,
  shelleyCertVStateL,
  shelleyCertPStateL,
  shelleyObligationCertState,
  shelleyCertsTotalDepositsTxBody,
  shelleyCertsTotalRefundsTxBody,
) where

import Cardano.Ledger.Binary (
  DecShareCBOR (..),
  EncCBOR (..),
  Interns,
  decSharePlusLensCBOR,
  decodeRecordNamedT,
  encodeListLen,
 )
import Cardano.Ledger.CertState
import Cardano.Ledger.Coin (Coin (..))
import Cardano.Ledger.Core
import Cardano.Ledger.Credential (Credential (..))
import Cardano.Ledger.Shelley.Era (ShelleyEra)
import qualified Cardano.Ledger.UMap as UM
import Control.DeepSeq (NFData (..))
import Data.Aeson (KeyValue, ToJSON (..), object, pairs, (.=))
import Data.Default (Default (..))
import qualified Data.Foldable as F
import qualified Data.Map.Strict as Map
import GHC.Generics (Generic)
import Lens.Micro (Lens', lens, _2)
import NoThunks.Class (NoThunks (..))

data ShelleyCertState era = ShelleyCertState
  { shelleyCertVState :: !(VState era)
  , shelleyCertPState :: !(PState era)
  , shelleyCertDState :: !(DState era)
  }
  deriving (Show, Eq, Generic)

mkShelleyCertState :: VState era -> PState era -> DState era -> ShelleyCertState era
mkShelleyCertState v p d =
  ShelleyCertState
    { shelleyCertVState = v
    , shelleyCertPState = p
    , shelleyCertDState = d
    }

shelleyCertDStateL :: Lens' (ShelleyCertState era) (DState era)
shelleyCertDStateL = lens shelleyCertDState (\ds u -> ds {shelleyCertDState = u})
{-# INLINE shelleyCertDStateL #-}

shelleyCertPStateL :: Lens' (ShelleyCertState era) (PState era)
shelleyCertPStateL = lens shelleyCertPState (\ds u -> ds {shelleyCertPState = u})
{-# INLINE shelleyCertPStateL #-}

shelleyCertVStateL :: Lens' (ShelleyCertState era) (VState era)
shelleyCertVStateL = lens shelleyCertVState (\ds u -> ds {shelleyCertVState = u})
{-# INLINE shelleyCertVStateL #-}

toCertStatePairs :: KeyValue e a => ShelleyCertState era -> [a]
toCertStatePairs ShelleyCertState {..} =
  [ "dstate" .= shelleyCertDState
  , "pstate" .= shelleyCertPState
  ]

shelleyObligationCertState :: ShelleyCertState era -> Obligations
shelleyObligationCertState (ShelleyCertState VState {vsDReps} PState {psDeposits} DState {dsUnified}) =
  let accum ans drepState = ans <> drepDeposit drepState
   in Obligations
        { oblStake = UM.fromCompact (UM.sumDepositUView (UM.RewDepUView dsUnified))
        , oblPool = F.foldl' (<>) (Coin 0) psDeposits
        , oblDRep = F.foldl' accum (Coin 0) vsDReps
        , oblProposal = Coin 0
        }

shelleyCertsTotalDepositsTxBody ::
  EraTxBody era => PParams era -> ShelleyCertState era -> TxBody era -> Coin
shelleyCertsTotalDepositsTxBody pp ShelleyCertState {shelleyCertPState} =
  getTotalDepositsTxBody pp (`Map.member` psStakePoolParams shelleyCertPState)

shelleyCertsTotalRefundsTxBody ::
  EraTxBody era => PParams era -> ShelleyCertState era -> TxBody era -> Coin
shelleyCertsTotalRefundsTxBody pp ShelleyCertState {shelleyCertDState, shelleyCertVState} =
  getTotalRefundsTxBody
    pp
    (lookupDepositDState shelleyCertDState)
    (lookupDepositVState shelleyCertVState)

instance EraCertState ShelleyEra where
  type CertState ShelleyEra = ShelleyCertState ShelleyEra

  mkCertState = mkShelleyCertState

  upgradeCertState = error "Impossible: ByronEra does not have `EraCertState` instance"

  certDStateL = shelleyCertDStateL
  {-# INLINE certDStateL #-}

  certVStateL = shelleyCertVStateL
  {-# INLINE certVStateL #-}

  certPStateL = shelleyCertPStateL
  {-# INLINE certPStateL #-}

  obligationCertState = shelleyObligationCertState

  certsTotalDepositsTxBody = shelleyCertsTotalDepositsTxBody

  certsTotalRefundsTxBody = shelleyCertsTotalRefundsTxBody

instance ToJSON (ShelleyCertState era) where
  toJSON = object . toCertStatePairs
  toEncoding = pairs . mconcat . toCertStatePairs

instance Era era => EncCBOR (ShelleyCertState era) where
  encCBOR ShelleyCertState {shelleyCertPState, shelleyCertDState, shelleyCertVState} =
    encodeListLen 3
      <> encCBOR shelleyCertVState
      <> encCBOR shelleyCertPState
      <> encCBOR shelleyCertDState

instance Era era => DecShareCBOR (ShelleyCertState era) where
  type
    Share (ShelleyCertState era) =
      ( Interns (Credential 'Staking)
      , Interns (KeyHash 'StakePool)
      , Interns (Credential 'DRepRole)
      , Interns (Credential 'HotCommitteeRole)
      )
  decSharePlusCBOR = decodeRecordNamedT "CertState" (const 3) $ do
    shelleyCertVState <-
      decSharePlusLensCBOR $
        lens (\(cs, _, cd, ch) -> (cs, cd, ch)) (\(_, ks, _, _) (cs, cd, ch) -> (cs, ks, cd, ch))
    shelleyCertPState <- decSharePlusLensCBOR _2
    shelleyCertDState <-
      decSharePlusLensCBOR $
        lens (\(cs, ks, cd, _) -> (cs, ks, cd)) (\(_, _, _, ch) (cs, ks, cd) -> (cs, ks, cd, ch))
    pure ShelleyCertState {..}

instance Default (ShelleyCertState era) where
  def = ShelleyCertState def def def

instance Era era => NoThunks (ShelleyCertState era)

instance Era era => NFData (ShelleyCertState era)
