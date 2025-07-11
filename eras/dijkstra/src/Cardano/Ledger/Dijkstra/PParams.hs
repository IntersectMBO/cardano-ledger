{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableSuperClasses #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE StandaloneDeriving #-}

module Cardano.Ledger.Dijkstra.PParams 
  ( DijkstraPParams (..)
  , DijkstraEraPParams (..)
  ) where

import Cardano.Ledger.Alonzo.PParams
import Cardano.Ledger.Babbage.PParams
import Cardano.Ledger.BaseTypes (
  EpochInterval (..),
  NonNegativeInterval,
  ProtVer (..),
  StrictMaybe (..),
  UnitInterval,
 )
import Cardano.Ledger.Coin (Coin (..))
import Cardano.Ledger.Conway.PParams
import Cardano.Ledger.Core hiding (ppUpdate)
import Cardano.Ledger.Dijkstra.Era (DijkstraEra)
import Cardano.Ledger.HKD (HKDFunctor (..), HKDNoUpdate, NoUpdate (..))
import Cardano.Ledger.Plutus (CostModels, ExUnits (..), Prices (..), updateCostModels, emptyCostModels)
import Cardano.Ledger.Shelley.PParams
import Cardano.Ledger.Val (Val (..))
import Data.Data (Proxy (..))
import Data.Word (Word16, Word32)
import GHC.Generics (Generic)
import Lens.Micro (Lens', lens, to, (^.))
import Numeric.Natural (Natural)
import Data.Functor.Identity (Identity)
import Data.Set (Set)
import Data.Default (Default(..))
import NoThunks.Class (NoThunks)
import Control.DeepSeq (NFData)

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
  , dppPoolDeposit :: !(THKD ('PPGroups 'EconomicGroup 'NoStakePoolGroup) f Coin)
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
  , dppDRepDeposit :: !(THKD ('PPGroups 'GovGroup 'NoStakePoolGroup) f Coin)
  -- ^ The amount of a DRep registration deposit
  , dppDRepActivity :: !(THKD ('PPGroups 'GovGroup 'NoStakePoolGroup) f EpochInterval)
  -- ^ The number of Epochs that a DRep can perform no activity without losing their @Active@ status.
  , dppMinFeeRefScriptCostPerByte ::
      !(THKD ('PPGroups 'EconomicGroup 'SecurityGroup) f NonNegativeInterval)
  -- ^ Reference scripts fee for the minimum fee calculation
  -- TODO ensure that the groups here make sense
  , dppMaxRefScriptSizePerBlock :: !(THKD ('PPGroups 'NetworkGroup 'SecurityGroup) f Int)
  -- ^ Limit on the total number of bytes of all reference scripts combined from
  -- all transactions within a block.
  , dppMaxRefScriptSizePerTx :: !(THKD ('PPGroups 'NetworkGroup 'SecurityGroup) f Int)
  -- ^ Limit on the total number of bytes of reference scripts that a transaction can use.
  , dppRefScriptCostStride :: !(THKD ('PPGroups 'NetworkGroup 'SecurityGroup) f Int)
  , dppRefScriptCostMultiplier :: !(THKD ('PPGroups 'NetworkGroup 'SecurityGroup) f Int)
  }
  deriving (Generic)


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

dijkstraModifiedPPGroups :: DijkstraPParams StrictMaybe era -> Set PPGroups
dijkstraModifiedPPGroups
  ( DijkstraPParams
      p01
      p02
      p03
      p04
      p05
      p06
      p07
      p08
      p09
      p10
      p11
      p12
      _protocolVersion
      p14
      p15
      p16
      p17
      p18
      p19
      p20
      p21
      p22
      p23
      p24
      p25
      p26
      p27
      p28
      p29
      p30
      p31
      p32
      p33
      p34
      p35
    ) =
    mconcat
      [ ppGroup p01
      , ppGroup p02
      , ppGroup p03
      , ppGroup p04
      , ppGroup p05
      , ppGroup p06
      , ppGroup p07
      , ppGroup p08
      , ppGroup p09
      , ppGroup p10
      , ppGroup p11
      , ppGroup p12
      , ppGroup p14
      , ppGroup p15
      , ppGroup p16
      , ppGroup p17
      , ppGroup p18
      , ppGroup p19
      , ppGroup p20
      , ppGroup p21
      , ppGroup p22
      , ppGroup p23
      , ppGroup p24
      , ppGroup p25
      , ppGroup p26
      , ppGroup p27
      , ppGroup p28
      , ppGroup p29
      , ppGroup p30
      , ppGroup p31
      , ppGroup p32
      , ppGroup p33
      , ppGroup p34
      , ppGroup p35
      ]

instance EraPParams DijkstraEra where
  type PParamsHKD f DijkstraEra = DijkstraPParams f DijkstraEra
  type UpgradePParams f DijkstraEra = ()
  type DowngradePParams f DijkstraEra = ()

  emptyPParamsIdentity = emptyDijkstraPParams
  emptyPParamsStrictMaybe = emptyDijkstraPParamsUpdate

  upgradePParamsHKD _ = undefined
  downgradePParamsHKD _ = undefined

  hkdMinFeeAL = lens (unTHKD . dppMinFeeA) $ \pp x -> pp {dppMinFeeA = THKD x}
  hkdMinFeeBL = lens (unTHKD . dppMinFeeB) $ \pp x -> pp {dppMinFeeB = THKD x}
  hkdMaxBBSizeL = lens (unTHKD . dppMaxBBSize) $ \pp x -> pp {dppMaxBBSize = THKD x}
  hkdMaxTxSizeL = lens (unTHKD . dppMaxTxSize) $ \pp x -> pp {dppMaxTxSize = THKD x}
  hkdMaxBHSizeL = lens (unTHKD . dppMaxBHSize) $ \pp x -> pp {dppMaxBHSize = THKD x}
  hkdKeyDepositL = lens (unTHKD . dppKeyDeposit) $ \pp x -> pp {dppKeyDeposit = THKD x}
  hkdPoolDepositL = lens (unTHKD . dppPoolDeposit) $ \pp x -> pp {dppPoolDeposit = THKD x}
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
    ]

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
  modifiedPPGroups (PParamsUpdate ppu) = dijkstraModifiedPPGroups ppu
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
        isValid (/= zero) ppuPoolDepositL
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
  hkdDRepDepositL =
    lens (unTHKD . dppDRepDeposit) $ \pp x -> pp {dppDRepDeposit = THKD x}
  hkdDRepActivityL =
    lens (unTHKD . dppDRepActivity) $ \pp x -> pp {dppDRepActivity = THKD x}
  hkdMinFeeRefScriptCostPerByteL =
    lens (unTHKD . dppMinFeeRefScriptCostPerByte) $ \pp x -> pp {dppMinFeeRefScriptCostPerByte = THKD x}

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
    , dppPoolDeposit = THKD (Coin 0)
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
    , -- New in Dijkstra
      dppPoolVotingThresholds = THKD def
    , dppDRepVotingThresholds = THKD def
    , dppCommitteeMinSize = THKD 0
    , dppCommitteeMaxTermLength = THKD (EpochInterval 0)
    , dppGovActionLifetime = THKD (EpochInterval 0)
    , dppGovActionDeposit = THKD (Coin 0)
    , dppDRepDeposit = THKD (Coin 0)
    , dppDRepActivity = THKD (EpochInterval 0)
    , dppMinFeeRefScriptCostPerByte = THKD minBound
    , dppMaxRefScriptSizePerBlock = THKD 0
    , dppMaxRefScriptSizePerTx = THKD 0
    , dppRefScriptCostStride = THKD 0
    , dppRefScriptCostMultiplier = THKD 0
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
    , -- New for Dijkstra
      dppPoolVotingThresholds = THKD SNothing
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
  maxRefScriptSizePerBlockL :: Lens' (PParamsHKD f era) (HKD f Int)
  maxRefScriptSizePerTxL :: Lens' (PParamsHKD f era) (HKD f Int)
  refScriptCostStrideL :: Lens' (PParamsHKD f era) (HKD f Int)
  refScriptCostMultiplierL :: Lens' (PParamsHKD f era) (HKD f Int)

instance DijkstraEraPParams DijkstraEra where
  maxRefScriptSizePerBlockL = lens (unTHKD . dppMaxRefScriptSizePerBlock) $ \pp x -> pp {dppMaxRefScriptSizePerBlock = THKD x}
  maxRefScriptSizePerTxL = lens (unTHKD . dppMaxRefScriptSizePerTx) $ \pp x -> pp {dppMaxRefScriptSizePerTx = THKD x}
  refScriptCostStrideL = lens (unTHKD . dppRefScriptCostStride) $ \pp x -> pp {dppRefScriptCostStride = THKD x}
  refScriptCostMultiplierL = lens (unTHKD . dppRefScriptCostMultiplier) $ \pp x -> pp {dppRefScriptCostMultiplier = THKD x}
