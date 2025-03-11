{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies #-}
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

import Cardano.Ledger.Binary (
  DecShareCBOR (..),
  EncCBOR (..),
  Interns,
  decSharePlusLensCBOR,
  decodeRecordNamedT,
  encodeListLen,
 )
import Cardano.Ledger.Coin (Coin (..))
import Cardano.Ledger.Core
import Cardano.Ledger.Credential (Credential (..))
import Cardano.Ledger.Shelley.Era (ShelleyEra)
import Cardano.Ledger.State
import qualified Cardano.Ledger.UMap as UM
import Control.DeepSeq (NFData (..))
import Data.Aeson (KeyValue, ToJSON (..), object, pairs, (.=))
import Data.Default (Default (..))
import qualified Data.Foldable as F
import qualified Data.Map.Strict as Map
import GHC.Generics (Generic)
import Lens.Micro (Lens', lens, (&), (.~), (^.), _2)
import NoThunks.Class (NoThunks (..))

data ShelleyCertState era = ShelleyCertState
  { shelleyCertPState :: !(PState era)
  , shelleyCertDState :: !(DState era)
  }
  deriving (Show, Eq, Generic)

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

toCertStatePairs :: KeyValue e a => ShelleyCertState era -> [a]
toCertStatePairs certState@(ShelleyCertState _ _) =
  let ShelleyCertState {..} = certState
   in [ "dstate" .= shelleyCertDState
      , "pstate" .= shelleyCertPState
      ]

shelleyObligationCertState :: EraCertState era => CertState era -> Obligations
shelleyObligationCertState certState =
  Obligations
    { oblStake =
        UM.fromCompact (UM.sumDepositUView (UM.RewDepUView (certState ^. certDStateL . dsUnifiedL)))
    , oblPool = F.foldl' (<>) (Coin 0) (certState ^. certPStateL . psDepositsL)
    , oblDRep = Coin 0
    , oblProposal = Coin 0
    }

shelleyCertsTotalDepositsTxBody ::
  EraTxBody era => PParams era -> ShelleyCertState era -> TxBody era -> Coin
shelleyCertsTotalDepositsTxBody pp ShelleyCertState {shelleyCertPState} =
  getTotalDepositsTxBody pp (`Map.member` psStakePoolParams shelleyCertPState)

shelleyCertsTotalRefundsTxBody ::
  EraTxBody era => PParams era -> ShelleyCertState era -> TxBody era -> Coin
shelleyCertsTotalRefundsTxBody pp ShelleyCertState {shelleyCertDState} =
  getTotalRefundsTxBody
    pp
    (lookupDepositDState shelleyCertDState)
    (const Nothing)

instance EraCertState ShelleyEra where
  type CertState ShelleyEra = ShelleyCertState ShelleyEra

  upgradeCertState = error "Impossible: ByronEra does not have `EraCertState` instance"

  certDStateL = shelleyCertDStateL
  {-# INLINE certDStateL #-}

  certPStateL = shelleyCertPStateL
  {-# INLINE certPStateL #-}

  obligationCertState = shelleyObligationCertState

  certsTotalDepositsTxBody = shelleyCertsTotalDepositsTxBody

  certsTotalRefundsTxBody = shelleyCertsTotalRefundsTxBody

instance ToJSON (ShelleyCertState era) where
  toJSON = object . toCertStatePairs
  toEncoding = pairs . mconcat . toCertStatePairs

instance Era era => EncCBOR (ShelleyCertState era) where
  encCBOR ShelleyCertState {shelleyCertPState, shelleyCertDState} =
    encodeListLen 2
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
  decSharePlusCBOR = decodeRecordNamedT "ShelleyCertState" (const 2) $ do
    shelleyCertPState <- decSharePlusLensCBOR _2
    shelleyCertDState <-
      decSharePlusLensCBOR $
        lens (\(cs, ks, cd, _) -> (cs, ks, cd)) (\(_, _, _, ch) (cs, ks, cd) -> (cs, ks, cd, ch))
    pure ShelleyCertState {..}

instance Default (ShelleyCertState era) where
  def = ShelleyCertState def def

instance Era era => NoThunks (ShelleyCertState era)

instance Era era => NFData (ShelleyCertState era)
