module Cardano.Ledger.Api.PParams (
  -- * PParams
  PParams,
  emptyPParams,
  UpgradePParams,
  upgradePParams,
  DowngradePParams,
  downgradePParams,

  -- * PParamsUpdate
  PParamsUpdate,
  emptyPParamsUpdate,
  upgradePParamsUpdate,
  downgradePParamsUpdate,

  -- * Shelley params

  -- | Protocol parameters introduced in Shelley era

  -- ** @TxFeePerByte@

  -- | Min fee factor
  ppTxFeePerByteL,
  ppuTxFeePerByteL,
  -- | Deprecated
  ppMinFeeAL,
  ppuMinFeeAL,

  -- ** @TxFeeFixed@

  -- | Min fee constant
  ppTxFeeFixedL,
  ppuTxFeeFixedL,
  -- | Deprecated
  ppMinFeeBL,
  ppuMinFeeBL,

  -- ** @MaxBBSize@

  -- | Max block body size
  ppMaxBBSizeL,
  ppuMaxBBSizeL,

  -- ** @AaxBHSize@

  -- | Max block header size
  ppMaxBHSizeL,
  ppuMaxBHSizeL,

  -- ** @PoolDeposit@

  -- | Stake pool deposit
  ppPoolDepositL,
  ppuPoolDepositL,

  -- ** @EMax@

  -- | Epoch bound on pool retirement
  ppEMaxL,
  ppuEMaxL,

  -- ** @NOpt@

  -- | Desired number of pools
  ppNOptL,
  ppuNOptL,

  -- ** @A0@

  -- | Pool influence
  ppA0L,
  ppuA0L,

  -- ** @Tau@

  -- | Treasury expansion
  ppTauL,
  ppuTauL,

  -- ** @Rho@

  -- | Monetary expansion
  ppRhoL,
  ppuRhoL,

  -- ** @D@

  -- | Decentralization paramteter
  --
  -- /Note/ - Removed in Babbage
  ppDL,
  ppDG,
  ppuDL,

  -- ** @ExtraEntropy@

  -- | Extra entropy
  --
  -- /Note/ - Removed in Babbage
  ppExtraEntropyL,
  ppuExtraEntropyL,

  -- ** @ProtocolVersion@

  -- | Protocol version
  ppProtocolVersionL,
  ppuProtocolVersionL,

  -- ** @MinUTxOValue@

  -- | Minimum allowed value of a new TxOut
  ppuMinUTxOValueL,
  ppMinUTxOValueL,

  -- ** @MinPoolCast@

  -- | Miminum allowed stake pool cost
  ppMinPoolCostL,
  ppuMinPoolCostL,

  -- ** @KeyDeposit@
  ppKeyDepositL,
  ppuKeyDepositL,

  -- ** @MaxTxSize@
  ppMaxTxSizeL,
  ppuMaxTxSizeL,

  -- * Alonzo params

  -- | Protocol parameters introduced in Alonzo era
  UpgradeAlonzoPParams (..),
  DowngradeAlonzoPParams (..),
  LangDepView (..),
  getLanguageView,

  -- ** @CostModels@

  -- | Plutus `Cardano.Ledegr.Api.Scripts.CostModels`
  --
  -- To convert resource primitives into the more abstract
  -- `Cardano.Ledegr.Api.Scripts.ExUnits` during script execution a cost model needs to be
  -- supplied to the interpreter. The cost models required for this purpose are recorded
  -- in the @CostModels@ protocol parameter.
  ppCostModelsL,
  ppuCostModelsL,

  -- ** @Prices@

  -- | The calculation of the actual cost, in Ada, of running a script that takes
  -- `Cardano.Ledegr.Api.Scripts.ExUnits` resources to run, is done by a formula in the
  -- ledger rules, which uses the @Prices@ parameter
  ppPricesL,
  ppuPricesL,

  -- ** @MaxTxExUnits@

  -- | Limit the total per-transaction resource use for phase-2 scripts.
  ppMaxTxExUnitsL,
  ppuMaxTxExUnitsL,

  -- ** @MaxBlockExUnits@

  -- | Limit the total per-transaction and per-block resource use for phase-2 scripts.
  ppMaxBlockExUnitsL,
  ppuMaxBlockExUnitsL,

  -- ** @MaxValSize@

  -- | The new parameter maxValSize replaces the constant @maxValSize@ used Mary era to
  -- limit the size of the Value part of an output in a serialised transaction.
  ppMaxValSizeL,
  ppuMaxValSizeL,

  -- ** @CoinsPerUTxOWord@

  -- | In Alonzo, the protocol parameter @minUTxOValue@ is deprecated, and replaced by
  -- @coinsPerUTxOWord@. This specifies directly the deposit required for storing bytes of
  -- data on the ledger in the form of UTxO entries.
  CoinPerWord (..),
  ppCoinsPerUTxOWordL,
  ppuCoinsPerUTxOWordL,

  -- ** @CollateralPercentage@

  -- | The parameter collateralPercent is used to specify the percentage of the total
  -- transaction fee its collateral must (at minimum) cover.
  ppCollateralPercentageL,
  ppuCollateralPercentageL,

  -- ** @MaxCollateralInputs@

  -- | The parameter @maxCollateralInputs@ is used to limit, additionally, the total number
  -- of collateral inputs, and thus the total number of additional signatures that must be
  -- checked during validation.
  ppMaxCollateralInputsL,
  ppuMaxCollateralInputsL,

  -- * Babbage params

  -- | Protocol parameters introduced in Babbage era
  DowngradeBabbagePParams (..),

  -- ** @CoinsPerUTxOByte@

  -- | Cost in the amount of lovelace ber byte.
  --
  -- /Note/ - This parameter is a replacement for @coinsPerUTxOWord@
  CoinPerByte (..),
  coinsPerUTxOWordToCoinsPerUTxOByte,
  coinsPerUTxOByteToCoinsPerUTxOWord,
  ppCoinsPerUTxOByteL,
  ppuCoinsPerUTxOByteL,

  -- * Conway params

  -- | Protocol parameters introduced in Conway era
  UpgradeConwayPParams (..),

  -- ** @PoolVotingThresholds@

  -- | Thresholds for stake pool votes on governance actions
  PoolVotingThresholds (..),
  pvtMotionNoConfidenceL,
  pvtCommitteeNormalL,
  pvtCommitteeNoConfidenceL,
  pvtHardForkInitiationL,
  pvtPPSecurityGroupL,
  ppPoolVotingThresholdsL,
  ppuPoolVotingThresholdsL,

  -- ** @DRepVotingThresholds@

  -- | Thresholds for DRep votes on governance actions
  DRepVotingThresholds (..),
  dvtMotionNoConfidenceL,
  dvtCommitteeNormalL,
  dvtCommitteeNoConfidenceL,
  dvtUpdateToConstitutionL,
  dvtHardForkInitiationL,
  dvtPPNetworkGroupL,
  dvtPPEconomicGroupL,
  dvtPPTechnicalGroupL,
  dvtPPGovGroupL,
  dvtTreasuryWithdrawalL,
  ppDRepVotingThresholdsL,
  ppuDRepVotingThresholdsL,

  -- ** @CommitteeMinSize@

  -- | Minimum size of the Constitutional Committee
  ppCommitteeMinSizeL,
  ppuCommitteeMinSizeL,

  -- ** @CommitteeMaxTermLength@

  -- | Maximum term length of a Constitutional Committee member, in number of epochs
  ppCommitteeMaxTermLengthL,
  ppuCommitteeMaxTermLengthL,

  -- ** @GovActionLifetime@

  -- | Number of epochs a governance action can remain active without being enacted
  -- before it expires
  ppGovActionLifetimeL,
  ppuGovActionLifetimeL,

  -- ** @GovActionDeposit@

  -- | Deposit that has to be paid for submitting a governance action proposal
  ppGovActionDepositL,
  ppuGovActionDepositL,

  -- ** @DRepDeposit@

  -- | Deposit that has to be paid for a DRep registration
  ppDRepDepositL,
  ppuDRepDepositL,

  -- ** @DRepActivity@

  -- | Number of epochs that a DRep can perform no activity without losing their @Active@
  -- status
  ppDRepActivityL,
  ppuDRepActivityL,

  -- ** @MinFeeRefScriptCostPerByte@

  -- | Base price in lovelace per byte of reference scripts used by a transaction, which is
  -- used for the reference scripts part of the minimum fee calculation
  ppMinFeeRefScriptCostPerByteL,
  ppuMinFeeRefScriptCostPerByteL,

  -- * Dijkstra params

  -- | Protocol parameters introduced in Dijkstra era
  UpgradeDijkstraPParams (..),

  -- ** @MaxRefScriptSizePerBlock@

  -- | Limit on the total number of bytes of all reference scripts combined from all
  -- transactions within a block
  ppMaxRefScriptSizePerBlockL,
  ppuMaxRefScriptSizePerBlockL,

  -- ** @MaxRefScriptSizePerTx@

  -- | Limit on the total number of bytes of reference scripts that a transaction can use
  ppMaxRefScriptSizePerTxL,
  ppuMaxRefScriptSizePerTxL,

  -- ** @RefScriptCostStride@

  -- | Size in bytes of each tier in the reference scripts fee calculation, after which the
  -- price per byte is multiplied by @RefScriptCostMultiplier@
  ppRefScriptCostStrideL,
  ppuRefScriptCostStrideL,

  -- ** @RefScriptCostMultiplier@

  -- | Growth factor of the reference scripts price per byte for each subsequent tier of
  -- @RefScriptCostStride@ bytes
  ppRefScriptCostMultiplierL,
  ppuRefScriptCostMultiplierL,

  -- ** @MaxPledgeLeverage@

  -- | Maximum ratio of a stake pool's stake to its pledge that counts towards its rewards, as
  -- introduced in CIP-50. When unset, the stake is not limited by the pledge.
  MaxPledgeLeverage (..),
  ppMaxPledgeLeverageL,
  ppuMaxPledgeLeverageL,

  -- ** @MinPoolMargin@

  -- | Lower bound on the margin a stake pool is credited with in the calculation of stake
  -- pool rewards, as introduced in CIP-23
  ppMinPoolMarginL,
  ppuMinPoolMarginL,

  -- ** @LeiosAnnouncementPeriodLength@

  -- | Duration in milliseconds for announcement headers (extended praos headers) to
  -- propagate through the network, @L_hdr@ in CIP-164
  ppLeiosAnnouncementPeriodLengthL,
  ppuLeiosAnnouncementPeriodLengthL,

  -- ** @LeiosVotePeriodLength@

  -- | Duration in milliseconds during which committee members can vote on endorser blocks,
  -- @L_vote@ in CIP-164
  ppLeiosVotePeriodLengthL,
  ppuLeiosVotePeriodLengthL,

  -- ** @LeiosDiffusionPeriodLength@

  -- | Duration in milliseconds after voting that ensures network-wide availability of a
  -- certified endorser block, @L_diff@ in CIP-164
  ppLeiosDiffusionPeriodLengthL,
  ppuLeiosDiffusionPeriodLengthL,

  -- ** @LeiosCommitteeSize@

  -- | Number of top-stake pools seated on the epoch's voting committee, @N_c@ in CIP-164
  ppLeiosCommitteeSizeL,
  ppuLeiosCommitteeSizeL,

  -- ** @LeiosQuorumStakeThreshold@

  -- | Minimum fraction of total active stake that the votes in a certificate must
  -- represent, @tau@ in CIP-164
  ppLeiosQuorumStakeThresholdL,
  ppuLeiosQuorumStakeThresholdL,

  -- ** @MaxEndorserBlockReferencesSize@

  -- | Limit on the size of the endorsement itself, i.e. size of all transaction references
  -- and sizes contained in an endorser block
  ppMaxEndorserBlockReferencesSizeL,
  ppuMaxEndorserBlockReferencesSizeL,

  -- ** @MaxEndorserBlockTxsSize@

  -- | Limit on the total size of all transactions endorsed by an endorser block
  ppMaxEndorserBlockTxsSizeL,
  ppuMaxEndorserBlockTxsSizeL,

  -- ** @MaxEndorserBlockExUnits@

  -- | Limit on the total resource use of phase-2 scripts per endorser block
  ppMaxEndorserBlockExUnitsL,
  ppuMaxEndorserBlockExUnitsL,

  -- ** @MaxRefScriptSizePerEndorserBlock@

  -- | Limit on the total number of bytes of all reference scripts combined from all
  -- transactions within an endorser block
  ppMaxRefScriptSizePerEndorserBlockL,
  ppuMaxRefScriptSizePerEndorserBlockL,

  -- ** @PerasMinCandidateBlockAge@

  -- | Minimum age in slots for a block to be eligible for voting in a Peras round
  ppPerasMinCandidateBlockAgeL,
  ppuPerasMinCandidateBlockAgeL,

  -- ** @PerasHealingFactor@

  -- | Healing time coefficient used to derive the length of a Peras cooldown period
  ppPerasHealingFactorL,
  ppuPerasHealingFactorL,

  -- ** @PerasCertBoost@

  -- | Extra chain weight that a Peras certificate gives to a boosted block
  ppPerasCertBoostL,
  ppuPerasCertBoostL,

  -- ** @PerasTargetCommitteeSize@

  -- | Target size of the Peras voting committee
  ppPerasTargetCommitteeSizeL,
  ppuPerasTargetCommitteeSizeL,

  -- ** @PerasBootstrapRound@

  -- | Optional round number used to manually bootstrap Peras voting at a specific time
  ppPerasBootstrapRoundL,
  ppuPerasBootstrapRoundL,

  -- ** @PerasQuorumThresholdSafetyMargin@

  -- | Extra safety margin added on top of the 75% Peras quorum threshold baseline. Must be
  -- between 0 and 0.25, inclusive.
  ppPerasQuorumThresholdSafetyMarginL,
  ppuPerasQuorumThresholdSafetyMarginL,

  -- ** @RefInputsCostPerMultiAssetByte@

  -- | Fee charged per byte of binary overhead of the multi-assets present in the values
  --  of a transaction’s reference inputs.
  ppRefInputsCostPerMultiAssetByteL,
  ppuRefInputsCostPerMultiAssetByteL,

  -- ** @RefInputsCostPerDatumByte@

  -- | Fee charged per byte of datum attached to a transaction's reference inputs
  ppRefInputsCostPerDatumByteL,
  ppuRefInputsCostPerDatumByteL,

  -- * Type classes
  EraPParams,
  AlonzoEraPParams,
  BabbageEraPParams,
  ConwayEraPParams,
  DijkstraEraPParams,
) where

import Cardano.Ledger.Alonzo.Core (
  AlonzoEraPParams,
  CoinPerWord (..),
  ppCoinsPerUTxOWordL,
  ppCollateralPercentageL,
  ppCostModelsL,
  ppMaxBlockExUnitsL,
  ppMaxCollateralInputsL,
  ppMaxTxExUnitsL,
  ppMaxValSizeL,
  ppPricesL,
  ppuCoinsPerUTxOWordL,
  ppuCollateralPercentageL,
  ppuCostModelsL,
  ppuMaxBlockExUnitsL,
  ppuMaxCollateralInputsL,
  ppuMaxTxExUnitsL,
  ppuMaxValSizeL,
  ppuPricesL,
 )
import Cardano.Ledger.Alonzo.PParams (
  DowngradeAlonzoPParams (..),
  LangDepView (..),
  UpgradeAlonzoPParams (..),
  getLanguageView,
 )
import Cardano.Ledger.Api.Era ()
import Cardano.Ledger.Babbage.Core (
  BabbageEraPParams (..),
  CoinPerByte (..),
  ppCoinsPerUTxOByteL,
  ppuCoinsPerUTxOByteL,
 )
import Cardano.Ledger.Babbage.PParams (
  DowngradeBabbagePParams (..),
  coinsPerUTxOByteToCoinsPerUTxOWord,
  coinsPerUTxOWordToCoinsPerUTxOByte,
 )
import Cardano.Ledger.Conway.PParams (
  ConwayEraPParams,
  DRepVotingThresholds (..),
  PoolVotingThresholds (..),
  UpgradeConwayPParams (..),
  dvtCommitteeNoConfidenceL,
  dvtCommitteeNormalL,
  dvtHardForkInitiationL,
  dvtMotionNoConfidenceL,
  dvtPPEconomicGroupL,
  dvtPPGovGroupL,
  dvtPPNetworkGroupL,
  dvtPPTechnicalGroupL,
  dvtTreasuryWithdrawalL,
  dvtUpdateToConstitutionL,
  ppCommitteeMaxTermLengthL,
  ppCommitteeMinSizeL,
  ppDRepActivityL,
  ppDRepDepositL,
  ppDRepVotingThresholdsL,
  ppGovActionDepositL,
  ppGovActionLifetimeL,
  ppMinFeeRefScriptCostPerByteL,
  ppPoolVotingThresholdsL,
  ppuCommitteeMaxTermLengthL,
  ppuCommitteeMinSizeL,
  ppuDRepActivityL,
  ppuDRepDepositL,
  ppuDRepVotingThresholdsL,
  ppuGovActionDepositL,
  ppuGovActionLifetimeL,
  ppuMinFeeRefScriptCostPerByteL,
  ppuPoolVotingThresholdsL,
  pvtCommitteeNoConfidenceL,
  pvtCommitteeNormalL,
  pvtHardForkInitiationL,
  pvtMotionNoConfidenceL,
  pvtPPSecurityGroupL,
 )
import Cardano.Ledger.Core (
  EraPParams (DowngradePParams, UpgradePParams, ppDG),
  MaxPledgeLeverage (..),
  PParams (..),
  PParamsUpdate (..),
  downgradePParams,
  downgradePParamsUpdate,
  emptyPParams,
  emptyPParamsUpdate,
  ppA0L,
  ppDL,
  ppEMaxL,
  ppExtraEntropyL,
  ppKeyDepositL,
  ppMaxBBSizeL,
  ppMaxBHSizeL,
  ppMaxTxSizeL,
  ppMinFeeAL,
  ppMinFeeBL,
  ppMinPoolCostL,
  ppMinUTxOValueL,
  ppNOptL,
  ppPoolDepositL,
  ppProtocolVersionL,
  ppRhoL,
  ppTauL,
  ppTxFeeFixedL,
  ppTxFeePerByteL,
  ppuA0L,
  ppuDL,
  ppuEMaxL,
  ppuExtraEntropyL,
  ppuKeyDepositL,
  ppuMaxBBSizeL,
  ppuMaxBHSizeL,
  ppuMaxTxSizeL,
  ppuMinFeeAL,
  ppuMinFeeBL,
  ppuMinPoolCostL,
  ppuMinUTxOValueL,
  ppuNOptL,
  ppuPoolDepositL,
  ppuProtocolVersionL,
  ppuRhoL,
  ppuTauL,
  ppuTxFeeFixedL,
  ppuTxFeePerByteL,
  upgradePParams,
  upgradePParamsUpdate,
 )
import Cardano.Ledger.Dijkstra.PParams (
  DijkstraEraPParams,
  UpgradeDijkstraPParams (..),
  ppLeiosAnnouncementPeriodLengthL,
  ppLeiosCommitteeSizeL,
  ppLeiosDiffusionPeriodLengthL,
  ppLeiosQuorumStakeThresholdL,
  ppLeiosVotePeriodLengthL,
  ppMaxEndorserBlockExUnitsL,
  ppMaxEndorserBlockReferencesSizeL,
  ppMaxEndorserBlockTxsSizeL,
  ppMaxPledgeLeverageL,
  ppMaxRefScriptSizePerBlockL,
  ppMaxRefScriptSizePerEndorserBlockL,
  ppMaxRefScriptSizePerTxL,
  ppMinPoolMarginL,
  ppPerasBootstrapRoundL,
  ppPerasCertBoostL,
  ppPerasHealingFactorL,
  ppPerasMinCandidateBlockAgeL,
  ppPerasQuorumThresholdSafetyMarginL,
  ppPerasTargetCommitteeSizeL,
  ppRefInputsCostPerDatumByteL,
  ppRefInputsCostPerMultiAssetByteL,
  ppRefScriptCostMultiplierL,
  ppRefScriptCostStrideL,
  ppuLeiosAnnouncementPeriodLengthL,
  ppuLeiosCommitteeSizeL,
  ppuLeiosDiffusionPeriodLengthL,
  ppuLeiosQuorumStakeThresholdL,
  ppuLeiosVotePeriodLengthL,
  ppuMaxEndorserBlockExUnitsL,
  ppuMaxEndorserBlockReferencesSizeL,
  ppuMaxEndorserBlockTxsSizeL,
  ppuMaxPledgeLeverageL,
  ppuMaxRefScriptSizePerBlockL,
  ppuMaxRefScriptSizePerEndorserBlockL,
  ppuMaxRefScriptSizePerTxL,
  ppuMinPoolMarginL,
  ppuPerasBootstrapRoundL,
  ppuPerasCertBoostL,
  ppuPerasHealingFactorL,
  ppuPerasMinCandidateBlockAgeL,
  ppuPerasQuorumThresholdSafetyMarginL,
  ppuPerasTargetCommitteeSizeL,
  ppuRefInputsCostPerDatumByteL,
  ppuRefInputsCostPerMultiAssetByteL,
  ppuRefScriptCostMultiplierL,
  ppuRefScriptCostStrideL,
 )
