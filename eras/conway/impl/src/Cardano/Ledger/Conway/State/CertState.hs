{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableSuperClasses #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Cardano.Ledger.Conway.State.CertState (
  ConwayCertState (..),
  ConwayEraCertState (..),
  VState (..),
  vsDRepsL,
  vsCommitteeStateL,
  vsNumDormantEpochsL,
  vsActualDRepExpiry,
  csCommitteeCredsL,
  lookupDepositVState,
  epochStateRegDrepL,
  mkConwayCertState,
  conwayCertDStateL,
  conwayCertPStateL,
  conwayCertVStateL,
  conwayObligationCertState,
  conwayCertsTotalDepositsTxBody,
  conwayCertsTotalRefundsTxBody,
)
where

import Cardano.Ledger.BaseTypes (binOpEpochNo)
import Cardano.Ledger.Binary (
  DecCBOR (..),
  DecShareCBOR (..),
  EncCBOR (..),
  Interns,
  decNoShareCBOR,
  decSharePlusLensCBOR,
  decodeMap,
  decodeRecordNamedT,
  encodeListLen,
  interns,
  internsFromSet,
 )
import Cardano.Ledger.Binary.Coders (Decode (..), Encode (..), decode, encode, (!>), (<!))
import Cardano.Ledger.Coin (Coin (..))
import Cardano.Ledger.Conway.Era (ConwayEra)
import Cardano.Ledger.Core
import Cardano.Ledger.Credential (Credential (..))
import Cardano.Ledger.Shelley.LedgerState (EpochState (..), esLStateL, lsCertStateL)
import Cardano.Ledger.Shelley.State
import Cardano.Ledger.Slot (EpochNo (..))
import Control.DeepSeq (NFData (..))
import Data.Aeson (KeyValue, ToJSON (..), object, pairs, (.=))
import Data.Coerce (coerce)
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
  deriving (Show, Eq, Generic)

-- | The state that tracks the voting entities (DReps and Constitutional Committee
-- members). In the formal ledger specification this type is called @GState@
data VState era = VState
  { vsDReps :: !(Map (Credential 'DRepRole) DRepState)
  , vsCommitteeState :: !(CommitteeState era)
  , vsNumDormantEpochs :: !EpochNo
  -- ^ Number of contiguous epochs in which there are exactly zero
  -- active governance proposals to vote on. It is incremented in every
  -- EPOCH rule if the number of active governance proposals to vote on
  -- continues to be zero. It is reset to zero when a new governance
  -- action is successfully proposed. We need this counter in order to
  -- bump DRep expiries through dormant periods when DReps do not have
  -- an opportunity to vote on anything.
  }
  deriving (Show, Eq, Generic)

-- | Function that looks up the deposit for currently registered DRep
lookupDepositVState :: VState era -> Credential 'DRepRole -> Maybe Coin
lookupDepositVState vstate = fmap drepDeposit . flip Map.lookup (vstate ^. vsDRepsL)

instance Default (VState era) where
  def = VState def def (EpochNo 0)

instance NoThunks (VState era)

instance NFData (VState era)

instance Era era => DecShareCBOR (VState era) where
  type
    Share (VState era) =
      ( Interns (Credential 'Staking)
      , Interns (Credential 'DRepRole)
      , Interns (Credential 'HotCommitteeRole)
      )
  getShare VState {vsDReps, vsCommitteeState} =
    (internsFromSet (foldMap drepDelegs vsDReps), fst (getShare vsDReps), getShare vsCommitteeState)
  decShareCBOR (cs, cd, _) =
    decode $
      RecD VState
        <! D (decodeMap (interns cd <$> decCBOR) (decShareCBOR cs))
        <! D decNoShareCBOR
        <! From

instance Era era => DecCBOR (VState era) where
  decCBOR = decNoShareCBOR

instance Era era => EncCBOR (VState era) where
  encCBOR VState {..} =
    encode $
      Rec (VState @era)
        !> To vsDReps
        !> To vsCommitteeState
        !> To vsNumDormantEpochs

instance ToJSON (VState era) where
  toJSON = object . toVStatePair
  toEncoding = pairs . mconcat . toVStatePair

toVStatePair :: KeyValue e a => VState era -> [a]
toVStatePair vs@(VState _ _ _) =
  let VState {..} = vs
   in [ "dreps" .= vsDReps
      , "committeeState" .= vsCommitteeState
      , "numDormantEpochs" .= vsNumDormantEpochs
      ]

-- ===================================
-- VState

vsDRepsL :: Lens' (VState era) (Map (Credential 'DRepRole) DRepState)
vsDRepsL = lens vsDReps (\vs u -> vs {vsDReps = u})

vsCommitteeStateL :: Lens' (VState era) (CommitteeState era)
vsCommitteeStateL = lens vsCommitteeState (\vs u -> vs {vsCommitteeState = u})

vsNumDormantEpochsL :: Lens' (VState era) EpochNo
vsNumDormantEpochsL = lens vsNumDormantEpochs (\vs u -> vs {vsNumDormantEpochs = u})

vsActualDRepExpiry :: Credential 'DRepRole -> VState era -> Maybe EpochNo
vsActualDRepExpiry cred vs =
  binOpEpochNo (+) (vsNumDormantEpochs vs) . drepExpiry <$> Map.lookup cred (vsDReps vs)

csCommitteeCredsL ::
  Lens' (CommitteeState era) (Map (Credential 'ColdCommitteeRole) CommitteeAuthorization)
csCommitteeCredsL = lens csCommitteeCreds (\cs u -> cs {csCommitteeCreds = u})

epochStateRegDrepL ::
  ConwayEraCertState era => Lens' (EpochState era) (Map (Credential 'DRepRole) DRepState)
epochStateRegDrepL = esLStateL . lsCertStateL . certVStateL . vsDRepsL

class EraCertState era => ConwayEraCertState era where
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

toCertStatePairs :: KeyValue e a => ConwayCertState era -> [a]
toCertStatePairs certState@(ConwayCertState _ _ _) =
  let ConwayCertState {..} = certState
   in [ "dstate" .= conwayCertDState
      , "pstate" .= conwayCertPState
      , "vstate" .= conwayCertVState
      ]

conwayObligationCertState :: ConwayEraCertState era => CertState era -> Obligations
conwayObligationCertState certState =
  let accum ans drepState = ans <> drepDeposit drepState
   in (shelleyObligationCertState certState)
        { oblDRep = F.foldl' accum (Coin 0) (certState ^. certVStateL . vsDRepsL)
        }

conwayCertsTotalDepositsTxBody ::
  EraTxBody era => PParams era -> ConwayCertState era -> TxBody era -> Coin
conwayCertsTotalDepositsTxBody pp ConwayCertState {conwayCertPState} =
  getTotalDepositsTxBody pp (`Map.member` psStakePoolParams conwayCertPState)

conwayCertsTotalRefundsTxBody ::
  EraTxBody era => PParams era -> ConwayCertState era -> TxBody era -> Coin
conwayCertsTotalRefundsTxBody pp ConwayCertState {conwayCertDState, conwayCertVState} =
  getTotalRefundsTxBody
    pp
    (lookupDepositDState conwayCertDState)
    (lookupDepositVState conwayCertVState)

instance EraCertState ConwayEra where
  type CertState ConwayEra = ConwayCertState ConwayEra

  -- TODO: use the Translation interface instead
  upgradeCertState (ShelleyCertState p d) = ConwayCertState def (coerce p) (coerce d)

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

instance ToJSON (ConwayCertState era) where
  toJSON = object . toCertStatePairs
  toEncoding = pairs . mconcat . toCertStatePairs

instance Era era => EncCBOR (ConwayCertState era) where
  encCBOR ConwayCertState {conwayCertPState, conwayCertDState, conwayCertVState} =
    encodeListLen 3
      <> encCBOR conwayCertVState
      <> encCBOR conwayCertPState
      <> encCBOR conwayCertDState

instance Era era => DecShareCBOR (ConwayCertState era) where
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

instance Default (ConwayCertState era) where
  def = ConwayCertState def def def

instance Era era => NoThunks (ConwayCertState era)

instance Era era => NFData (ConwayCertState era)
