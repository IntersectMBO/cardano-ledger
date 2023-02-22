module Cardano.Ledger.Api.PParams (
  -- * PParams
  PParams (..),
  emptyPParams,
  UpgradePParams,
  upgradePParams,
  DowngradePParams,
  downgradePParams,

  -- * PParamsUpdate
  PParamsUpdate (..),
  emptyPParamsUpdate,
  upgradePParamsUpdate,
  downgradePParamsUpdate,

  -- * Babbage params

  -- | Protocol parameters introduced in Shelley era

  -- ** @MinFeeA@

  -- | Min fee factor
  ppMinFeeAL,
  ppuMinFeeAL,

  -- ** @MinFeeB@

  -- | Min fee constant
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
  UpgradeAlonzoPParams (..),
  DowngradeAlonzoPParams (..),
  -- | Protocolo parameters introduced in Alonzo era

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

  -- * Type classes
  EraPParams,
  AlonzoEraPParams,
  BabbageEraPParams,
)
where

import Cardano.Ledger.Alonzo.Core (
  AlonzoEraPParams (..),
  CoinPerWord (..),
  DowngradeAlonzoPParams (..),
  UpgradeAlonzoPParams (..),
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
import Cardano.Ledger.Babbage.Core (
  BabbageEraPParams (..),
  CoinPerByte (..),
  DowngradeBabbagePParams (..),
  ppCoinsPerUTxOByteL,
  ppuCoinsPerUTxOByteL,
 )
import Cardano.Ledger.Core (
  EraPParams (DowngradePParams, UpgradePParams, ppDG),
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
  upgradePParams,
  upgradePParamsUpdate,
 )
