{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Cardano.Ledger.Allegra.PParams () where

import Cardano.Ledger.Allegra.Era (AllegraEra)
import Cardano.Ledger.Core
import Cardano.Ledger.Shelley.Governance
import Cardano.Ledger.Shelley.PParams
import Data.Coerce
import Lens.Micro

instance EraPParams AllegraEra where
  type PParamsHKD f AllegraEra = ShelleyPParams f AllegraEra

  type UpgradePParams f AllegraEra = ()
  type DowngradePParams f AllegraEra = ()

  emptyPParamsIdentity = emptyShelleyPParams
  emptyPParamsStrictMaybe = emptyShelleyPParamsUpdate

  upgradePParamsHKD () = coerce
  downgradePParamsHKD () = coerce

  hkdMinFeeAL = lens sppMinFeeA $ \pp x -> pp {sppMinFeeA = x}
  hkdMinFeeBL = lens sppMinFeeB $ \pp x -> pp {sppMinFeeB = x}
  hkdMaxBBSizeL = lens sppMaxBBSize $ \pp x -> pp {sppMaxBBSize = x}
  hkdMaxTxSizeL = lens sppMaxTxSize $ \pp x -> pp {sppMaxTxSize = x}
  hkdMaxBHSizeL = lens sppMaxBHSize $ \pp x -> pp {sppMaxBHSize = x}
  hkdKeyDepositL = lens sppKeyDeposit $ \pp x -> pp {sppKeyDeposit = x}
  hkdPoolDepositCompactL = lens sppPoolDeposit $ \pp x -> pp {sppPoolDeposit = x}
  hkdEMaxL = lens sppEMax $ \pp x -> pp {sppEMax = x}
  hkdNOptL = lens sppNOpt $ \pp x -> pp {sppNOpt = x}
  hkdA0L = lens sppA0 $ \pp x -> pp {sppA0 = x}
  hkdRhoL = lens sppRho $ \pp x -> pp {sppRho = x}
  hkdTauL = lens sppTau $ \pp x -> pp {sppTau = x}
  hkdDL = lens sppD $ \pp x -> pp {sppD = x}
  hkdExtraEntropyL = lens sppExtraEntropy $ \pp x -> pp {sppExtraEntropy = x}
  hkdProtocolVersionL = lens sppProtocolVersion $ \pp x -> pp {sppProtocolVersion = x}
  hkdMinUTxOValueL = lens sppMinUTxOValue $ \pp x -> pp {sppMinUTxOValue = x}
  hkdMinPoolCostL = lens sppMinPoolCost $ \pp x -> pp {sppMinPoolCost = x}

  eraPParams = shelleyPParams

instance EraGov AllegraEra where
  type GovState AllegraEra = ShelleyGovState AllegraEra
  emptyGovState = emptyShelleyGovState

  curPParamsGovStateL = curPParamsShelleyGovStateL

  prevPParamsGovStateL = prevPParamsShelleyGovStateL

  futurePParamsGovStateL = futurePParamsShelleyGovStateL

  obligationGovState = const mempty
