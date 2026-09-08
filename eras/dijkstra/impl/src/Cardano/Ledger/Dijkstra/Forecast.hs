{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE RoleAnnotations #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Cardano.Ledger.Dijkstra.Forecast (
  DijkstraForecast (..),
  mkDijkstraForecast,
  dfPoolDistrL,
  dfMaxBlockHeaderSizeL,
  dfMaxBlockBodySizeL,
  dfProtocolVersionL,
  dfLeiosCommitteeL,
  dfLeiosCommitteeSizeL,
  dfLeiosQuorumStakeThresholdL,
  dfLeiosAnnouncementPeriodLengthL,
  dfLeiosVotePeriodLengthL,
  dfLeiosDiffusionPeriodLengthL,
  dfMaxEndorserBlockReferencesSizeL,
  dfMaxEndorserBlockTxsSizeL,
  dfMaxEndorserBlockExUnitsL,
  dfMaxRefScriptSizePerEndorserBlockL,
) where

import Cardano.Ledger.BaseTypes (Milliseconds32, ProtVer, UnitInterval)
import Cardano.Ledger.Conway.Rules ()
import Cardano.Ledger.Core
import Cardano.Ledger.Dijkstra.Era (DijkstraEra)
import Cardano.Ledger.Dijkstra.Governance ()
import Cardano.Ledger.Dijkstra.PParams (
  DijkstraEraPParams,
  ppLeiosAnnouncementPeriodLengthL,
  ppLeiosCommitteeSizeL,
  ppLeiosDiffusionPeriodLengthL,
  ppLeiosQuorumStakeThresholdL,
  ppLeiosVotePeriodLengthL,
  ppMaxEndorserBlockExUnitsL,
  ppMaxEndorserBlockReferencesSizeL,
  ppMaxEndorserBlockTxsSizeL,
  ppMaxRefScriptSizePerEndorserBlockL,
 )
import Cardano.Ledger.Dijkstra.State.CertState ()
import Cardano.Ledger.Plutus.ExUnits (OrdExUnits)
import Cardano.Ledger.Shelley.API.Forecast (
  EraForecast (..),
  Timeline (..),
 )
import Cardano.Ledger.Shelley.LedgerState (
  NewEpochState (..),
  curPParamsEpochStateL,
  esSnapshotsL,
  nesEsL,
  nesPdL,
 )
import Cardano.Ledger.State (
  EraGov,
  LeiosCommittee,
  PoolDistr (..),
  ssLeiosCommitteeL,
  ssStakeSetL,
 )
import Control.DeepSeq (NFData)
import Data.Word (Word16, Word32)
import GHC.Generics (Generic)
import Lens.Micro (Lens', lens, (^.))
import NoThunks.Class (NoThunks (..))

-- | Forecast data for Leios eras: the Praos fields, plus the epoch's voting
-- committee and the Leios protocol parameters.
--
-- The Leios fields are fixed at an epoch boundary -- the committee is seated on
-- the stake snapshot by the SNAP rule, the rest are protocol parameters -- so
-- they are forecastable for the same reason the Praos fields are. That is what
-- lets a node verify a certificate against the committee and quorum in force at
-- the slot of the block that announced the endorser block, without having
-- applied that block.
--
-- Leios field names follow CIP-164 where it names the same quantity.
data DijkstraForecast (t :: Timeline) era = DijkstraForecast
  { dfPoolDistr :: !PoolDistr
  , dfMaxBlockHeaderSize :: !Word16
  , dfMaxBlockBodySize :: !Word32
  , dfProtocolVersion :: !ProtVer
  , dfLeiosCommittee :: !LeiosCommittee
  , dfLeiosCommitteeSize :: !Word16
  -- ^ @N_c@. Redundant against 'dfLeiosCommittee' once seated, but it is the
  -- parameter that governs seating, and a zero means Leios is not enabled.
  , dfLeiosQuorumStakeThreshold :: !UnitInterval
  -- ^ @tau@: the fraction of active stake a certificate's votes must carry.
  , dfLeiosAnnouncementPeriodLength :: !Milliseconds32
  -- ^ @L_hdr@
  , dfLeiosVotePeriodLength :: !Milliseconds32
  -- ^ @L_vote@
  , dfLeiosDiffusionPeriodLength :: !Milliseconds32
  -- ^ @L_diff@
  , dfMaxEndorserBlockReferencesSize :: !Word32
  -- ^ Cap on the endorsement itself: all transaction references and sizes.
  , dfMaxEndorserBlockTxsSize :: !Word32
  -- ^ Cap on the total size of an endorser block's endorsed transactions.
  , dfMaxEndorserBlockExUnits :: !OrdExUnits
  -- ^ Cap on an endorser block's total script execution units.
  , dfMaxRefScriptSizePerEndorserBlock :: !Word32
  -- ^ Cap on the combined reference-script bytes across an endorser block.
  }
  deriving (Eq, Show, Generic)

type role DijkstraForecast phantom phantom

instance NFData (DijkstraForecast t era)

instance NoThunks (DijkstraForecast t era)

mkDijkstraForecast ::
  (DijkstraEraPParams era, EraGov era) =>
  NewEpochState era ->
  DijkstraForecast t era
mkDijkstraForecast nes =
  DijkstraForecast
    { dfPoolDistr = nes ^. nesPdL
    , dfMaxBlockHeaderSize = pp ^. ppMaxBHSizeL
    , dfMaxBlockBodySize = pp ^. ppMaxBBSizeL
    , dfProtocolVersion = pp ^. ppProtocolVersionL
    , dfLeiosCommittee = nes ^. nesEsL . esSnapshotsL . ssStakeSetL . ssLeiosCommitteeL
    , dfLeiosCommitteeSize = pp ^. ppLeiosCommitteeSizeL
    , dfLeiosQuorumStakeThreshold = pp ^. ppLeiosQuorumStakeThresholdL
    , dfLeiosAnnouncementPeriodLength = pp ^. ppLeiosAnnouncementPeriodLengthL
    , dfLeiosVotePeriodLength = pp ^. ppLeiosVotePeriodLengthL
    , dfLeiosDiffusionPeriodLength = pp ^. ppLeiosDiffusionPeriodLengthL
    , dfMaxEndorserBlockReferencesSize = pp ^. ppMaxEndorserBlockReferencesSizeL
    , dfMaxEndorserBlockTxsSize = pp ^. ppMaxEndorserBlockTxsSizeL
    , dfMaxEndorserBlockExUnits = pp ^. ppMaxEndorserBlockExUnitsL
    , dfMaxRefScriptSizePerEndorserBlock = pp ^. ppMaxRefScriptSizePerEndorserBlockL
    }
 where
  pp = nes ^. nesEsL . curPParamsEpochStateL

dfPoolDistrL :: Lens' (DijkstraForecast t era) PoolDistr
dfPoolDistrL = lens dfPoolDistr $ \s x -> s {dfPoolDistr = x}

dfMaxBlockHeaderSizeL :: Lens' (DijkstraForecast t era) Word16
dfMaxBlockHeaderSizeL = lens dfMaxBlockHeaderSize $ \s x -> s {dfMaxBlockHeaderSize = x}

dfMaxBlockBodySizeL :: Lens' (DijkstraForecast t era) Word32
dfMaxBlockBodySizeL = lens dfMaxBlockBodySize $ \s x -> s {dfMaxBlockBodySize = x}

dfProtocolVersionL :: Lens' (DijkstraForecast t era) ProtVer
dfProtocolVersionL = lens dfProtocolVersion $ \s x -> s {dfProtocolVersion = x}

dfLeiosCommitteeL :: Lens' (DijkstraForecast t era) LeiosCommittee
dfLeiosCommitteeL = lens dfLeiosCommittee $ \s x -> s {dfLeiosCommittee = x}

dfLeiosCommitteeSizeL :: Lens' (DijkstraForecast t era) Word16
dfLeiosCommitteeSizeL = lens dfLeiosCommitteeSize $ \s x -> s {dfLeiosCommitteeSize = x}

dfLeiosQuorumStakeThresholdL :: Lens' (DijkstraForecast t era) UnitInterval
dfLeiosQuorumStakeThresholdL =
  lens dfLeiosQuorumStakeThreshold $ \s x -> s {dfLeiosQuorumStakeThreshold = x}

dfLeiosAnnouncementPeriodLengthL :: Lens' (DijkstraForecast t era) Milliseconds32
dfLeiosAnnouncementPeriodLengthL =
  lens dfLeiosAnnouncementPeriodLength $ \s x -> s {dfLeiosAnnouncementPeriodLength = x}

dfLeiosVotePeriodLengthL :: Lens' (DijkstraForecast t era) Milliseconds32
dfLeiosVotePeriodLengthL =
  lens dfLeiosVotePeriodLength $ \s x -> s {dfLeiosVotePeriodLength = x}

dfLeiosDiffusionPeriodLengthL :: Lens' (DijkstraForecast t era) Milliseconds32
dfLeiosDiffusionPeriodLengthL =
  lens dfLeiosDiffusionPeriodLength $ \s x -> s {dfLeiosDiffusionPeriodLength = x}

dfMaxEndorserBlockReferencesSizeL :: Lens' (DijkstraForecast t era) Word32
dfMaxEndorserBlockReferencesSizeL =
  lens dfMaxEndorserBlockReferencesSize $ \s x -> s {dfMaxEndorserBlockReferencesSize = x}

dfMaxEndorserBlockTxsSizeL :: Lens' (DijkstraForecast t era) Word32
dfMaxEndorserBlockTxsSizeL =
  lens dfMaxEndorserBlockTxsSize $ \s x -> s {dfMaxEndorserBlockTxsSize = x}

dfMaxEndorserBlockExUnitsL :: Lens' (DijkstraForecast t era) OrdExUnits
dfMaxEndorserBlockExUnitsL =
  lens dfMaxEndorserBlockExUnits $ \s x -> s {dfMaxEndorserBlockExUnits = x}

dfMaxRefScriptSizePerEndorserBlockL :: Lens' (DijkstraForecast t era) Word32
dfMaxRefScriptSizePerEndorserBlockL =
  lens dfMaxRefScriptSizePerEndorserBlock $ \s x -> s {dfMaxRefScriptSizePerEndorserBlock = x}

instance EraForecast DijkstraEra where
  type Forecast t DijkstraEra = DijkstraForecast t DijkstraEra
  mkForecast = mkDijkstraForecast
  poolDistrForecastL = dfPoolDistrL
  maxBlockHeaderSizeForecastL = dfMaxBlockHeaderSizeL
  maxBlockBodySizeForecastL = dfMaxBlockBodySizeL
  protocolVersionForecastL = dfProtocolVersionL
