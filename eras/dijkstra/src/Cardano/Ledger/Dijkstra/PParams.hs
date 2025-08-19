{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableSuperClasses #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Cardano.Ledger.Dijkstra.PParams (
  DijkstraPParams (..),
  DijkstraEraPParams (..),
  UpgradeDijkstraPParams (..),
  -- Lenses
  ppRefScriptCostMultiplierL,
  ppRefScriptCostStrideL,
  ppMaxRefScriptSizePerTxL,
  ppMaxRefScriptSizePerBlockL,
  ppuRefScriptCostMultiplierL,
  ppuRefScriptCostStrideL,
  ppuMaxRefScriptSizePerTxL,
  ppuMaxRefScriptSizePerBlockL,
) where

import Cardano.Ledger.Alonzo.PParams
import Cardano.Ledger.Babbage.PParams
import Cardano.Ledger.BaseTypes (
  EpochInterval (..),
  KeyValuePairs (..),
  NonNegativeInterval,
  NonZero,
  PositiveInterval,
  ProtVer (..),
  StrictMaybe (..),
  ToKeyValuePairs (..),
  UnitInterval,
  knownNonZeroBounded,
 )
import Cardano.Ledger.Binary (DecCBOR (..), EncCBOR (..))
import Cardano.Ledger.Binary.Coders (Decode (..), Encode (..), decode, encode, (!>), (<!))
import Cardano.Ledger.Coin (Coin (..), CompactForm (..))
import Cardano.Ledger.Conway (ConwayEra)
import Cardano.Ledger.Conway.PParams
import Cardano.Ledger.Core
import Cardano.Ledger.Dijkstra.Era (DijkstraEra)
import Cardano.Ledger.HKD (HKDFunctor (..), HKDNoUpdate, NoUpdate (..))
import Cardano.Ledger.Plutus (
  CostModels,
  ExUnits (..),
  Prices (..),
  emptyCostModels,
  updateCostModels,
 )
import Cardano.Ledger.Shelley.PParams
import Cardano.Ledger.Val (Val (..))
import Control.DeepSeq (NFData)
import Data.Aeson (FromJSON, ToJSON (..), withObject, (.:), (.=))
import qualified Data.Aeson as Aeson
import Data.Data (Proxy (..))
import Data.Default (Default (..))
import Data.Functor.Identity (Identity)
import Data.Word (Word16, Word32)
import GHC.Generics (Generic)
import Lens.Micro (Lens', lens, to, (^.))
import NoThunks.Class (NoThunks)
import Numeric.Natural (Natural)

-- | Dijkstra Protocol parameters. The following parameters have been added since Dijkstra:
-- * @maxRefScriptSizePerBlock@
-- * @maxRefScriptSizePerTx@
-- * @refScriptCostStride@
-- * @refScriptCostMultiplier@
data DijkstraPParams f era = DijkstraPParams
  { dppMinFeeA :: !(THKD ('PPGroups 'EconomicGroup 'SecurityGroup) f Coin)
  -- ^ The linear factor for the minimum fee calculation
  , dppMinFeeB :: !(THKD ('PPGroups 'EconomicGroup 'SecurityGroup) f Coin)
  -- ^ The constant factor for the minimum fee calculation
  , dppMaxBBSize :: !(THKD ('PPGroups 'NetworkGroup 'SecurityGroup) f Word32)
  -- ^ Maximal block body size
  , dppMaxTxSize :: !(THKD ('PPGroups 'NetworkGroup 'SecurityGroup) f Word32)
  -- ^ Maximal transaction size
  , dppMaxBHSize :: !(THKD ('PPGroups 'NetworkGroup 'SecurityGroup) f Word16)
  -- ^ Maximal block header size
  , dppKeyDeposit :: !(THKD ('PPGroups 'EconomicGroup 'NoStakePoolGroup) f Coin)
  -- ^ The amount of a key registration deposit
  , dppPoolDeposit :: !(THKD ('PPGroups 'EconomicGroup 'NoStakePoolGroup) f (CompactForm Coin))
  -- ^ The amount of a pool registration deposit
  , dppEMax :: !(THKD ('PPGroups 'TechnicalGroup 'NoStakePoolGroup) f EpochInterval)
  -- ^ Maximum number of epochs in the future a pool retirement is allowed to
  -- be scheduled for.
  , dppNOpt :: !(THKD ('PPGroups 'TechnicalGroup 'NoStakePoolGroup) f Word16)
  -- ^ Desired number of pools
  , dppA0 :: !(THKD ('PPGroups 'TechnicalGroup 'NoStakePoolGroup) f NonNegativeInterval)
  -- ^ Pool influence
  , dppRho :: !(THKD ('PPGroups 'EconomicGroup 'NoStakePoolGroup) f UnitInterval)
  -- ^ Monetary expansion
  , dppTau :: !(THKD ('PPGroups 'EconomicGroup 'NoStakePoolGroup) f UnitInterval)
  -- ^ Treasury expansion
  , dppProtocolVersion :: !(HKDNoUpdate f ProtVer)
  -- ^ Protocol version
  , dppMinPoolCost :: !(THKD ('PPGroups 'EconomicGroup 'NoStakePoolGroup) f Coin)
  -- ^ Minimum Stake Pool Cost
  , dppCoinsPerUTxOByte :: !(THKD ('PPGroups 'EconomicGroup 'SecurityGroup) f CoinPerByte)
  -- ^ Cost in lovelace per byte of UTxO storage
  , dppCostModels :: !(THKD ('PPGroups 'TechnicalGroup 'NoStakePoolGroup) f CostModels)
  -- ^ Cost models for non-native script languages
  , dppPrices :: !(THKD ('PPGroups 'EconomicGroup 'NoStakePoolGroup) f Prices)
  -- ^ Prices of execution units (for non-native script languages)
  , dppMaxTxExUnits :: !(THKD ('PPGroups 'NetworkGroup 'NoStakePoolGroup) f OrdExUnits)
  -- ^ Max total script execution resources units allowed per tx
  , dppMaxBlockExUnits :: !(THKD ('PPGroups 'NetworkGroup 'SecurityGroup) f OrdExUnits)
  -- ^ Max total script execution resources units allowed per block
  , dppMaxValSize :: !(THKD ('PPGroups 'NetworkGroup 'SecurityGroup) f Word32)
  -- ^ Max size of a Value in an output
  , dppCollateralPercentage :: !(THKD ('PPGroups 'TechnicalGroup 'NoStakePoolGroup) f Word16)
  -- ^ Percentage of the txfee which must be provided as collateral when
  -- including non-native scripts.
  , dppMaxCollateralInputs :: !(THKD ('PPGroups 'NetworkGroup 'NoStakePoolGroup) f Word16)
  -- ^ Maximum number of collateral inputs allowed in a transaction
  , -- New ones for Dijkstra:
    dppPoolVotingThresholds :: !(THKD ('PPGroups 'GovGroup 'NoStakePoolGroup) f PoolVotingThresholds)
  -- ^ Thresholds for SPO votes
  , dppDRepVotingThresholds :: !(THKD ('PPGroups 'GovGroup 'NoStakePoolGroup) f DRepVotingThresholds)
  -- ^ Thresholds for DRep votes
  , dppCommitteeMinSize :: !(THKD ('PPGroups 'GovGroup 'NoStakePoolGroup) f Word16)
  -- ^ Minimum size of the Constitutional Committee
  , dppCommitteeMaxTermLength :: !(THKD ('PPGroups 'GovGroup 'NoStakePoolGroup) f EpochInterval)
  -- ^ The Constitutional Committee Term limit in number of Slots
  , dppGovActionLifetime :: !(THKD ('PPGroups 'GovGroup 'NoStakePoolGroup) f EpochInterval)
  -- ^ Gov action lifetime in number of Epochs
  , dppGovActionDeposit :: !(THKD ('PPGroups 'GovGroup 'SecurityGroup) f Coin)
  -- ^ The amount of the Gov Action deposit
  , dppDRepDeposit :: !(THKD ('PPGroups 'GovGroup 'NoStakePoolGroup) f (CompactForm Coin))
  -- ^ The amount of a DRep registration deposit
  , dppDRepActivity :: !(THKD ('PPGroups 'GovGroup 'NoStakePoolGroup) f EpochInterval)
  -- ^ The number of Epochs that a DRep can perform no activity without losing their @Active@ status.
  , dppMinFeeRefScriptCostPerByte ::
      !(THKD ('PPGroups 'EconomicGroup 'SecurityGroup) f NonNegativeInterval)
  -- ^ Reference scripts fee for the minimum fee calculation
  -- TODO ensure that the groups here make sense
  , dppMaxRefScriptSizePerBlock :: !(THKD ('PPGroups 'NetworkGroup 'SecurityGroup) f Word32)
  -- ^ Limit on the total number of bytes of all reference scripts combined from
  -- all transactions within a block.
  , dppMaxRefScriptSizePerTx :: !(THKD ('PPGroups 'NetworkGroup 'SecurityGroup) f Word32)
  -- ^ Limit on the total number of bytes of reference scripts that a transaction can use.
  , dppRefScriptCostStride :: !(THKD ('PPGroups 'NetworkGroup 'SecurityGroup) f (NonZero Word32))
  , dppRefScriptCostMultiplier :: !(THKD ('PPGroups 'NetworkGroup 'SecurityGroup) f PositiveInterval)
  }
  deriving (Generic)

dijkstraApplyPPUpdates ::
  forall era.
  DijkstraPParams Identity era ->
  DijkstraPParams StrictMaybe era ->
  DijkstraPParams Identity era
dijkstraApplyPPUpdates pp ppu = do
  DijkstraPParams
    { dppMinFeeA = ppApplyUpdate dppMinFeeA
    , dppMinFeeB = ppApplyUpdate dppMinFeeB
    , dppMaxBBSize = ppApplyUpdate dppMaxBBSize
    , dppMaxTxSize = ppApplyUpdate dppMaxTxSize
    , dppMaxBHSize = ppApplyUpdate dppMaxBHSize
    , dppKeyDeposit = ppApplyUpdate dppKeyDeposit
    , dppPoolDeposit = ppApplyUpdate dppPoolDeposit
    , dppEMax = ppApplyUpdate dppEMax
    , dppNOpt = ppApplyUpdate dppNOpt
    , dppA0 = ppApplyUpdate dppA0
    , dppRho = ppApplyUpdate dppRho
    , dppTau = ppApplyUpdate dppTau
    , dppProtocolVersion = dppProtocolVersion pp
    , dppMinPoolCost = ppApplyUpdate dppMinPoolCost
    , dppCoinsPerUTxOByte = ppApplyUpdate dppCoinsPerUTxOByte
    , dppCostModels =
        case dppCostModels ppu of
          THKD SNothing -> dppCostModels pp
          THKD (SJust costModelUpdate) ->
            THKD $ updateCostModels (unTHKD (dppCostModels pp)) costModelUpdate
    , dppPrices = ppApplyUpdate dppPrices
    , dppMaxTxExUnits = ppApplyUpdate dppMaxTxExUnits
    , dppMaxBlockExUnits = ppApplyUpdate dppMaxBlockExUnits
    , dppMaxValSize = ppApplyUpdate dppMaxValSize
    , dppCollateralPercentage = ppApplyUpdate dppCollateralPercentage
    , dppMaxCollateralInputs = ppApplyUpdate dppMaxCollateralInputs
    , dppPoolVotingThresholds = ppApplyUpdate dppPoolVotingThresholds
    , dppDRepVotingThresholds = ppApplyUpdate dppDRepVotingThresholds
    , dppCommitteeMinSize = ppApplyUpdate dppCommitteeMinSize
    , dppCommitteeMaxTermLength = ppApplyUpdate dppCommitteeMaxTermLength
    , dppGovActionLifetime = ppApplyUpdate dppGovActionLifetime
    , dppGovActionDeposit = ppApplyUpdate dppGovActionDeposit
    , dppDRepDeposit = ppApplyUpdate dppDRepDeposit
    , dppDRepActivity = ppApplyUpdate dppDRepActivity
    , dppMinFeeRefScriptCostPerByte = ppApplyUpdate dppMinFeeRefScriptCostPerByte
    , dppMaxRefScriptSizePerBlock = ppApplyUpdate dppMaxRefScriptSizePerBlock
    , dppMaxRefScriptSizePerTx = ppApplyUpdate dppMaxRefScriptSizePerTx
    , dppRefScriptCostStride = ppApplyUpdate dppRefScriptCostStride
    , dppRefScriptCostMultiplier = ppApplyUpdate dppRefScriptCostMultiplier
    }
  where
    ppApplyUpdate :: (forall f. DijkstraPParams f era -> THKD g f a) -> THKD g Identity a
    ppApplyUpdate dppGet =
      case dppGet ppu of
        THKD SNothing -> dppGet pp
        THKD (SJust ppNewValue) -> THKD ppNewValue

deriving instance Eq (DijkstraPParams Identity era)

deriving instance Ord (DijkstraPParams Identity era)

deriving instance Show (DijkstraPParams Identity era)

instance NoThunks (DijkstraPParams Identity era)

instance NFData (DijkstraPParams Identity era)

deriving instance Eq (DijkstraPParams StrictMaybe era)

deriving instance Ord (DijkstraPParams StrictMaybe era)

deriving instance Show (DijkstraPParams StrictMaybe era)

instance NoThunks (DijkstraPParams StrictMaybe era)

instance NFData (DijkstraPParams StrictMaybe era)

data UpgradeDijkstraPParams f era = UpgradeDijkstraPParams
  { udppMaxRefScriptSizePerBlock :: !(HKD f Word32)
  , udppMaxRefScriptSizePerTx :: !(HKD f Word32)
  , udppRefScriptCostStride :: !(HKD f (NonZero Word32))
  , udppRefScriptCostMultiplier :: !(HKD f PositiveInterval)
  }
  deriving (Generic)

deriving instance Eq (UpgradeDijkstraPParams Identity era)

deriving instance Show (UpgradeDijkstraPParams Identity era)

instance FromJSON (UpgradeDijkstraPParams Identity era) where
  parseJSON = withObject "UpgradeDijkstraPParams" $ \o -> do
    udppMaxRefScriptSizePerBlock <- o .: "maxRefScriptSizePerBlock"
    udppMaxRefScriptSizePerTx <- o .: "maxRefScriptSizePerTx"
    udppRefScriptCostStride <- o .: "refScriptCostStride"
    udppRefScriptCostMultiplier <- o .: "refScriptCostMultiplier"
    pure UpgradeDijkstraPParams {..}

instance ToKeyValuePairs (UpgradeDijkstraPParams Identity era) where
  toKeyValuePairs udpp =
    [ "maxRefScriptSizePerBlock" .= udppMaxRefScriptSizePerBlock udpp
    , "maxRefScriptSizePerTx" .= udppMaxRefScriptSizePerTx udpp
    , "refScriptCostStride" .= udppRefScriptCostStride udpp
    , "refScriptCostMultiplier" .= udppRefScriptCostMultiplier udpp
    ]

deriving via
  KeyValuePairs (UpgradeDijkstraPParams Identity era)
  instance
    ToJSON (UpgradeDijkstraPParams Identity era)

instance NFData (UpgradeDijkstraPParams Identity era)

instance NoThunks (UpgradeDijkstraPParams Identity era)

instance Era era => DecCBOR (UpgradeDijkstraPParams Identity era) where
  decCBOR =
    decode $
      RecD UpgradeDijkstraPParams
        <! From
        <! From
        <! From
        <! From

instance Era era => EncCBOR (UpgradeDijkstraPParams Identity era) where
  encCBOR UpgradeDijkstraPParams {..} =
    encode $
      Rec (UpgradeDijkstraPParams @Identity)
        !> To udppMaxRefScriptSizePerBlock
        !> To udppMaxRefScriptSizePerTx
        !> To udppRefScriptCostStride
        !> To udppRefScriptCostMultiplier

emptyDijkstraUpgradePParamsUpdate :: UpgradeDijkstraPParams StrictMaybe era
emptyDijkstraUpgradePParamsUpdate = UpgradeDijkstraPParams SNothing SNothing SNothing SNothing

upgradeDijkstraPParams ::
  UpgradeDijkstraPParams f DijkstraEra ->
  ConwayPParams f ConwayEra ->
  DijkstraPParams f DijkstraEra
upgradeDijkstraPParams UpgradeDijkstraPParams {..} ConwayPParams {..} =
  DijkstraPParams
    { dppMinFeeA = cppMinFeeA
    , dppMinFeeB = cppMinFeeB
    , dppMaxBBSize = cppMaxBBSize
    , dppMaxTxSize = cppMaxTxSize
    , dppMaxBHSize = cppMaxBHSize
    , dppKeyDeposit = cppKeyDeposit
    , dppPoolDeposit = cppPoolDeposit
    , dppEMax = cppEMax
    , dppNOpt = cppNOpt
    , dppA0 = cppA0
    , dppRho = cppRho
    , dppTau = cppTau
    , dppProtocolVersion = cppProtocolVersion
    , dppMinPoolCost = cppMinPoolCost
    , dppCoinsPerUTxOByte = cppCoinsPerUTxOByte
    , dppCostModels = cppCostModels
    , dppPrices = cppPrices
    , dppMaxTxExUnits = cppMaxTxExUnits
    , dppMaxBlockExUnits = cppMaxBlockExUnits
    , dppMaxValSize = cppMaxValSize
    , dppCollateralPercentage = cppCollateralPercentage
    , dppMaxCollateralInputs = cppMaxCollateralInputs
    , dppPoolVotingThresholds = cppPoolVotingThresholds
    , dppDRepVotingThresholds = cppDRepVotingThresholds
    , dppCommitteeMinSize = cppCommitteeMinSize
    , dppCommitteeMaxTermLength = cppCommitteeMaxTermLength
    , dppGovActionLifetime = cppGovActionLifetime
    , dppGovActionDeposit = cppGovActionDeposit
    , dppDRepDeposit = cppDRepDeposit
    , dppDRepActivity = cppDRepActivity
    , dppMinFeeRefScriptCostPerByte = cppMinFeeRefScriptCostPerByte
    , dppMaxRefScriptSizePerBlock = THKD udppMaxRefScriptSizePerBlock
    , dppMaxRefScriptSizePerTx = THKD udppMaxRefScriptSizePerTx
    , dppRefScriptCostStride = THKD udppRefScriptCostStride
    , dppRefScriptCostMultiplier = THKD udppRefScriptCostMultiplier
    }

downgradeDijkstraPParams :: DijkstraPParams f DijkstraEra -> ConwayPParams f ConwayEra
downgradeDijkstraPParams DijkstraPParams {..} =
  ConwayPParams
    { cppMinFeeA = dppMinFeeA
    , cppMinFeeB = dppMinFeeB
    , cppMaxBBSize = dppMaxBBSize
    , cppMaxTxSize = dppMaxTxSize
    , cppMaxBHSize = dppMaxBHSize
    , cppKeyDeposit = dppKeyDeposit
    , cppPoolDeposit = dppPoolDeposit
    , cppEMax = dppEMax
    , cppNOpt = dppNOpt
    , cppA0 = dppA0
    , cppRho = dppRho
    , cppTau = dppTau
    , cppProtocolVersion = dppProtocolVersion
    , cppMinPoolCost = dppMinPoolCost
    , cppCoinsPerUTxOByte = dppCoinsPerUTxOByte
    , cppCostModels = dppCostModels
    , cppPrices = dppPrices
    , cppMaxTxExUnits = dppMaxTxExUnits
    , cppMaxBlockExUnits = dppMaxBlockExUnits
    , cppMaxValSize = dppMaxValSize
    , cppCollateralPercentage = dppCollateralPercentage
    , cppMaxCollateralInputs = dppMaxCollateralInputs
    , cppPoolVotingThresholds = dppPoolVotingThresholds
    , cppDRepVotingThresholds = dppDRepVotingThresholds
    , cppCommitteeMinSize = dppCommitteeMinSize
    , cppCommitteeMaxTermLength = dppCommitteeMaxTermLength
    , cppGovActionLifetime = dppGovActionLifetime
    , cppGovActionDeposit = dppGovActionDeposit
    , cppDRepDeposit = dppDRepDeposit
    , cppDRepActivity = dppDRepActivity
    , cppMinFeeRefScriptCostPerByte = dppMinFeeRefScriptCostPerByte
    }

instance EraPParams DijkstraEra where
  type PParamsHKD f DijkstraEra = DijkstraPParams f DijkstraEra
  type UpgradePParams f DijkstraEra = UpgradeDijkstraPParams f DijkstraEra
  type DowngradePParams f DijkstraEra = ()

  emptyPParamsIdentity = emptyDijkstraPParams
  emptyPParamsStrictMaybe = emptyDijkstraPParamsUpdate

  applyPPUpdates (PParams pp) (PParamsUpdate ppu) =
    PParams $ dijkstraApplyPPUpdates pp ppu

  upgradePParamsHKD = upgradeDijkstraPParams
  downgradePParamsHKD _ = downgradeDijkstraPParams
  emptyUpgradePParamsUpdate = emptyDijkstraUpgradePParamsUpdate

  hkdMinFeeAL = lens (unTHKD . dppMinFeeA) $ \pp x -> pp {dppMinFeeA = THKD x}
  hkdMinFeeBL = lens (unTHKD . dppMinFeeB) $ \pp x -> pp {dppMinFeeB = THKD x}
  hkdMaxBBSizeL = lens (unTHKD . dppMaxBBSize) $ \pp x -> pp {dppMaxBBSize = THKD x}
  hkdMaxTxSizeL = lens (unTHKD . dppMaxTxSize) $ \pp x -> pp {dppMaxTxSize = THKD x}
  hkdMaxBHSizeL = lens (unTHKD . dppMaxBHSize) $ \pp x -> pp {dppMaxBHSize = THKD x}
  hkdKeyDepositL = lens (unTHKD . dppKeyDeposit) $ \pp x -> pp {dppKeyDeposit = THKD x}
  hkdPoolDepositCompactL = lens (unTHKD . dppPoolDeposit) $ \pp x -> pp {dppPoolDeposit = THKD x}
  hkdEMaxL = lens (unTHKD . dppEMax) $ \pp x -> pp {dppEMax = THKD x}
  hkdNOptL = lens (unTHKD . dppNOpt) $ \pp x -> pp {dppNOpt = THKD x}
  hkdA0L = lens (unTHKD . dppA0) $ \pp x -> pp {dppA0 = THKD x}
  hkdRhoL = lens (unTHKD . dppRho) $ \pp x -> pp {dppRho = THKD x}
  hkdTauL = lens (unTHKD . dppTau) $ \pp x -> pp {dppTau = THKD x}
  hkdProtocolVersionL = notSupportedInThisEraL
  hkdMinPoolCostL = lens (unTHKD . dppMinPoolCost) $ \pp x -> pp {dppMinPoolCost = THKD x}
  ppProtocolVersionL = ppLensHKD . lens dppProtocolVersion (\pp x -> pp {dppProtocolVersion = x})

  ppDG = to (const minBound)
  ppuProtocolVersionL = notSupportedInThisEraL
  hkdDL = notSupportedInThisEraL
  hkdExtraEntropyL = notSupportedInThisEraL
  hkdMinUTxOValueL = notSupportedInThisEraL
  eraPParams =
    [ ppMinFeeA
    , ppMinFeeB
    , ppMaxBBSize
    , ppMaxTxSize
    , ppMaxBHSize
    , ppKeyDeposit
    , ppPoolDeposit
    , ppEMax
    , ppNOpt
    , ppA0
    , ppRho
    , ppTau
    , ppGovProtocolVersion
    , ppMinPoolCost
    , ppCoinsPerUTxOByte
    , ppCostModels
    , ppPrices
    , ppMaxTxExUnits
    , ppMaxBlockExUnits
    , ppMaxValSize
    , ppCollateralPercentage
    , ppMaxCollateralInputs
    , ppPoolVotingThresholds
    , ppDRepVotingThresholds
    , ppCommitteeMinSize
    , ppCommitteeMaxTermLength
    , ppGovActionLifetime
    , ppGovActionDeposit
    , ppDRepDeposit
    , ppDRepActivity
    , ppMinFeeRefScriptCostPerByte
    , ppMaxRefScriptSizePerBlock
    , ppMaxRefScriptSizePerTx
    , ppRefScriptCostStride
    , ppRefScriptCostMultiplier
    ]

ppMaxRefScriptSizePerBlock :: PParam DijkstraEra
ppMaxRefScriptSizePerBlock =
  PParam
    { ppName = "maxRefScriptSizePerBlock"
    , ppLens = ppMaxRefScriptSizePerBlockL
    , ppUpdate =
        Just
          PParamUpdate
            { ppuTag = 34
            , ppuLens = ppuMaxRefScriptSizePerBlockL
            }
    }

ppMaxRefScriptSizePerTx :: PParam DijkstraEra
ppMaxRefScriptSizePerTx =
  PParam
    { ppName = "maxRefScriptSizePerTx"
    , ppLens = ppMaxRefScriptSizePerTxL
    , ppUpdate =
        Just
          PParamUpdate
            { ppuTag = 35
            , ppuLens = ppuMaxRefScriptSizePerTxL
            }
    }

ppRefScriptCostStride :: PParam DijkstraEra
ppRefScriptCostStride =
  PParam
    { ppName = "refScriptCostStride"
    , ppLens = ppRefScriptCostStrideL
    , ppUpdate =
        Just
          PParamUpdate
            { ppuTag = 36
            , ppuLens = ppuRefScriptCostStrideL
            }
    }

ppRefScriptCostMultiplier :: PParam DijkstraEra
ppRefScriptCostMultiplier =
  PParam
    { ppName = "refScriptCostMultiplier"
    , ppLens = ppRefScriptCostMultiplierL
    , ppUpdate =
        Just
          PParamUpdate
            { ppuTag = 37
            , ppuLens = ppuRefScriptCostMultiplierL
            }
    }

instance AlonzoEraPParams DijkstraEra where
  hkdCoinsPerUTxOWordL = notSupportedInThisEraL
  hkdCostModelsL = lens (unTHKD . dppCostModels) $ \pp x -> pp {dppCostModels = THKD x}
  hkdPricesL = lens (unTHKD . dppPrices) $ \pp x -> pp {dppPrices = THKD x}

  hkdMaxTxExUnitsL :: forall f. HKDFunctor f => Lens' (PParamsHKD f DijkstraEra) (HKD f ExUnits)
  hkdMaxTxExUnitsL =
    lens (hkdMap (Proxy @f) unOrdExUnits . unTHKD . dppMaxTxExUnits) $ \pp x ->
      pp {dppMaxTxExUnits = THKD $ hkdMap (Proxy @f) OrdExUnits x}
  hkdMaxBlockExUnitsL :: forall f. HKDFunctor f => Lens' (PParamsHKD f DijkstraEra) (HKD f ExUnits)
  hkdMaxBlockExUnitsL =
    lens (hkdMap (Proxy @f) unOrdExUnits . unTHKD . dppMaxBlockExUnits) $ \pp x ->
      pp {dppMaxBlockExUnits = THKD $ hkdMap (Proxy @f) OrdExUnits x}
  hkdMaxValSizeL :: forall f. HKDFunctor f => Lens' (PParamsHKD f DijkstraEra) (HKD f Natural)
  hkdMaxValSizeL =
    lens (asNaturalHKD @f @Word32 . (unTHKD . dppMaxValSize)) $
      \pp x -> pp {dppMaxValSize = THKD (asBoundedIntegralHKD @f @Natural @Word32 x)}
  hkdCollateralPercentageL ::
    forall f. HKDFunctor f => Lens' (PParamsHKD f DijkstraEra) (HKD f Natural)
  hkdCollateralPercentageL =
    lens (asNaturalHKD @f @Word16 . (unTHKD . dppCollateralPercentage)) $
      \pp x -> pp {dppCollateralPercentage = THKD (asBoundedIntegralHKD @f @Natural @Word16 x)}
  hkdMaxCollateralInputsL ::
    forall f. HKDFunctor f => Lens' (PParamsHKD f DijkstraEra) (HKD f Natural)
  hkdMaxCollateralInputsL =
    lens (asNaturalHKD @f @Word16 . (unTHKD . dppMaxCollateralInputs)) $
      \pp x -> pp {dppMaxCollateralInputs = THKD (asBoundedIntegralHKD @f @Natural @Word16 x)}

instance BabbageEraPParams DijkstraEra where
  hkdCoinsPerUTxOByteL =
    lens (unTHKD . dppCoinsPerUTxOByte) $ \pp x -> pp {dppCoinsPerUTxOByte = THKD x}

instance ConwayEraPParams DijkstraEra where
  ppuWellFormed _pv ppu =
    and
      [ -- Numbers
        isValid (/= 0) ppuMaxBBSizeL
      , isValid (/= 0) ppuMaxTxSizeL
      , isValid (/= 0) ppuMaxBHSizeL
      , isValid (/= 0) ppuMaxValSizeL
      , isValid (/= 0) ppuCollateralPercentageL
      , isValid (/= EpochInterval 0) ppuCommitteeMaxTermLengthL
      , isValid (/= EpochInterval 0) ppuGovActionLifetimeL
      , -- Coins
        isValid (/= mempty) ppuPoolDepositL
      , isValid (/= zero) ppuGovActionDepositL
      , isValid (/= zero) ppuDRepDepositL
      , isValid ((/= zero) . unCoinPerByte) ppuCoinsPerUTxOByteL
      , ppu /= emptyPParamsUpdate
      ]
    where
      isValid ::
        (t -> Bool) ->
        Lens' (PParamsUpdate DijkstraEra) (StrictMaybe t) ->
        Bool
      isValid p l = case ppu ^. l of
        SJust x -> p x
        SNothing -> True
  hkdPoolVotingThresholdsL =
    lens (unTHKD . dppPoolVotingThresholds) $ \pp x -> pp {dppPoolVotingThresholds = THKD x}
  hkdDRepVotingThresholdsL =
    lens (unTHKD . dppDRepVotingThresholds) $ \pp x -> pp {dppDRepVotingThresholds = THKD x}
  hkdCommitteeMinSizeL :: forall f. HKDFunctor f => Lens' (PParamsHKD f DijkstraEra) (HKD f Natural)
  hkdCommitteeMinSizeL =
    lens (asNaturalHKD @f @Word16 . (unTHKD . dppCommitteeMinSize)) $
      \pp x -> pp {dppCommitteeMinSize = THKD (asBoundedIntegralHKD @f @Natural @Word16 x)}
  hkdCommitteeMaxTermLengthL =
    lens (unTHKD . dppCommitteeMaxTermLength) $ \pp x -> pp {dppCommitteeMaxTermLength = THKD x}
  hkdGovActionLifetimeL =
    lens (unTHKD . dppGovActionLifetime) $ \pp x -> pp {dppGovActionLifetime = THKD x}
  hkdGovActionDepositL =
    lens (unTHKD . dppGovActionDeposit) $ \pp x -> pp {dppGovActionDeposit = THKD x}
  hkdDRepDepositCompactL =
    lens (unTHKD . dppDRepDeposit) $ \pp x -> pp {dppDRepDeposit = THKD x}
  hkdDRepActivityL =
    lens (unTHKD . dppDRepActivity) $ \pp x -> pp {dppDRepActivity = THKD x}
  hkdMinFeeRefScriptCostPerByteL =
    lens (unTHKD . dppMinFeeRefScriptCostPerByte) $ \pp x -> pp {dppMinFeeRefScriptCostPerByte = THKD x}
  ppMaxRefScriptSizePerTxG = ppLensHKD . hkdMaxRefScriptSizePerTxL
  ppMaxRefScriptSizePerBlockG = ppLensHKD . hkdMaxRefScriptSizePerBlockL
  ppRefScriptCostMultiplierG = ppLensHKD . hkdRefScriptCostMultiplierL
  ppRefScriptCostStrideG = ppLensHKD . hkdRefScriptCostStrideL

-- | Returns a basic "empty" `PParams` structure with all zero values.
emptyDijkstraPParams :: forall era. Era era => DijkstraPParams Identity era
emptyDijkstraPParams =
  DijkstraPParams
    { dppMinFeeA = THKD (Coin 0)
    , dppMinFeeB = THKD (Coin 0)
    , dppMaxBBSize = THKD 0
    , dppMaxTxSize = THKD 2048
    , dppMaxBHSize = THKD 0
    , dppKeyDeposit = THKD (Coin 0)
    , dppPoolDeposit = THKD (CompactCoin 0)
    , dppEMax = THKD (EpochInterval 0)
    , dppNOpt = THKD 100
    , dppA0 = THKD minBound
    , dppRho = THKD minBound
    , dppTau = THKD minBound
    , dppProtocolVersion = ProtVer (eraProtVerLow @era) 0
    , dppMinPoolCost = THKD mempty
    , dppCoinsPerUTxOByte = THKD (CoinPerByte $ Coin 0)
    , dppCostModels = THKD emptyCostModels
    , dppPrices = THKD (Prices minBound minBound)
    , dppMaxTxExUnits = THKD (OrdExUnits $ ExUnits 0 0)
    , dppMaxBlockExUnits = THKD (OrdExUnits $ ExUnits 0 0)
    , dppMaxValSize = THKD 0
    , dppCollateralPercentage = THKD 150
    , dppMaxCollateralInputs = THKD 5
    , dppPoolVotingThresholds = THKD def
    , dppDRepVotingThresholds = THKD def
    , dppCommitteeMinSize = THKD 0
    , dppCommitteeMaxTermLength = THKD (EpochInterval 0)
    , dppGovActionLifetime = THKD (EpochInterval 0)
    , dppGovActionDeposit = THKD (Coin 0)
    , dppDRepDeposit = THKD (CompactCoin 0)
    , dppDRepActivity = THKD (EpochInterval 0)
    , dppMinFeeRefScriptCostPerByte = THKD minBound
    , dppMaxRefScriptSizePerBlock = THKD 0
    , dppMaxRefScriptSizePerTx = THKD 0
    , dppRefScriptCostStride = THKD $ knownNonZeroBounded @1
    , dppRefScriptCostMultiplier = THKD minBound
    }

emptyDijkstraPParamsUpdate :: DijkstraPParams StrictMaybe era
emptyDijkstraPParamsUpdate =
  DijkstraPParams
    { dppMinFeeA = THKD SNothing
    , dppMinFeeB = THKD SNothing
    , dppMaxBBSize = THKD SNothing
    , dppMaxTxSize = THKD SNothing
    , dppMaxBHSize = THKD SNothing
    , dppKeyDeposit = THKD SNothing
    , dppPoolDeposit = THKD SNothing
    , dppEMax = THKD SNothing
    , dppNOpt = THKD SNothing
    , dppA0 = THKD SNothing
    , dppRho = THKD SNothing
    , dppTau = THKD SNothing
    , dppProtocolVersion = NoUpdate
    , dppMinPoolCost = THKD SNothing
    , dppCoinsPerUTxOByte = THKD SNothing
    , dppCostModels = THKD SNothing
    , dppPrices = THKD SNothing
    , dppMaxTxExUnits = THKD SNothing
    , dppMaxBlockExUnits = THKD SNothing
    , dppMaxValSize = THKD SNothing
    , dppCollateralPercentage = THKD SNothing
    , dppMaxCollateralInputs = THKD SNothing
    , dppPoolVotingThresholds = THKD SNothing
    , dppDRepVotingThresholds = THKD SNothing
    , dppCommitteeMinSize = THKD SNothing
    , dppCommitteeMaxTermLength = THKD SNothing
    , dppGovActionLifetime = THKD SNothing
    , dppGovActionDeposit = THKD SNothing
    , dppDRepDeposit = THKD SNothing
    , dppDRepActivity = THKD SNothing
    , dppMinFeeRefScriptCostPerByte = THKD SNothing
    , dppMaxRefScriptSizePerBlock = THKD SNothing
    , dppMaxRefScriptSizePerTx = THKD SNothing
    , dppRefScriptCostStride = THKD SNothing
    , dppRefScriptCostMultiplier = THKD SNothing
    }

class DijkstraEraPParams era => DijkstraEraPParams era where
  hkdMaxRefScriptSizePerBlockL :: Lens' (PParamsHKD f era) (HKD f Word32)
  hkdMaxRefScriptSizePerTxL :: Lens' (PParamsHKD f era) (HKD f Word32)
  hkdRefScriptCostStrideL :: Lens' (PParamsHKD f era) (HKD f (NonZero Word32))
  hkdRefScriptCostMultiplierL :: Lens' (PParamsHKD f era) (HKD f PositiveInterval)

instance DijkstraEraPParams DijkstraEra where
  hkdMaxRefScriptSizePerBlockL = lens (unTHKD . dppMaxRefScriptSizePerBlock) $ \pp x -> pp {dppMaxRefScriptSizePerBlock = THKD x}
  hkdMaxRefScriptSizePerTxL = lens (unTHKD . dppMaxRefScriptSizePerTx) $ \pp x -> pp {dppMaxRefScriptSizePerTx = THKD x}
  hkdRefScriptCostStrideL = lens (unTHKD . dppRefScriptCostStride) $ \pp x -> pp {dppRefScriptCostStride = THKD x}
  hkdRefScriptCostMultiplierL = lens (unTHKD . dppRefScriptCostMultiplier) $ \pp x -> pp {dppRefScriptCostMultiplier = THKD x}

ppMaxRefScriptSizePerBlockL :: DijkstraEraPParams era => Lens' (PParams era) Word32
ppMaxRefScriptSizePerBlockL = ppLensHKD . hkdMaxRefScriptSizePerBlockL @_ @Identity

ppMaxRefScriptSizePerTxL :: DijkstraEraPParams era => Lens' (PParams era) Word32
ppMaxRefScriptSizePerTxL = ppLensHKD . hkdMaxRefScriptSizePerTxL @_ @Identity

ppRefScriptCostStrideL :: DijkstraEraPParams era => Lens' (PParams era) (NonZero Word32)
ppRefScriptCostStrideL = ppLensHKD . hkdRefScriptCostStrideL @_ @Identity

ppRefScriptCostMultiplierL :: DijkstraEraPParams era => Lens' (PParams era) PositiveInterval
ppRefScriptCostMultiplierL = ppLensHKD . hkdRefScriptCostMultiplierL @_ @Identity

ppuMaxRefScriptSizePerBlockL ::
  DijkstraEraPParams era => Lens' (PParamsUpdate era) (StrictMaybe Word32)
ppuMaxRefScriptSizePerBlockL = ppuLensHKD . hkdMaxRefScriptSizePerBlockL @_ @StrictMaybe

ppuMaxRefScriptSizePerTxL ::
  DijkstraEraPParams era => Lens' (PParamsUpdate era) (StrictMaybe Word32)
ppuMaxRefScriptSizePerTxL = ppuLensHKD . hkdMaxRefScriptSizePerTxL @_ @StrictMaybe

ppuRefScriptCostStrideL ::
  DijkstraEraPParams era => Lens' (PParamsUpdate era) (StrictMaybe (NonZero Word32))
ppuRefScriptCostStrideL = ppuLensHKD . hkdRefScriptCostStrideL @_ @StrictMaybe

ppuRefScriptCostMultiplierL ::
  DijkstraEraPParams era => Lens' (PParamsUpdate era) (StrictMaybe PositiveInterval)
ppuRefScriptCostMultiplierL = ppuLensHKD . hkdRefScriptCostMultiplierL @_ @StrictMaybe
