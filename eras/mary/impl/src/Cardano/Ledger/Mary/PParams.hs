{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Cardano.Ledger.Mary.PParams () where

import Cardano.Ledger.Core
import Cardano.Ledger.Mary.Era (MaryEra)
import Cardano.Ledger.Shelley.Governance (
  EraGov (..),
  ShelleyGovState (..),
  curPParamsShelleyGovStateL,
  emptyShelleyGovState,
  futurePParamsShelleyGovStateL,
  prevPParamsShelleyGovStateL,
 )
import Cardano.Ledger.Shelley.PParams
import Data.Coerce
import Lens.Micro

instance EraPParams MaryEra where
  type PParamsHKD f MaryEra = ShelleyPParams f MaryEra

  type UpgradePParams f MaryEra = ()
  type DowngradePParams f MaryEra = ()

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
  hkdPoolDepositL = lens sppPoolDeposit $ \pp x -> pp {sppPoolDeposit = x}
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

instance  EraGov MaryEra where
  type GovState MaryEra = ShelleyGovState MaryEra
  emptyGovState = emptyShelleyGovState

  getProposedPPUpdates = Just . sgsCurProposals

  curPParamsGovStateL = curPParamsShelleyGovStateL

  prevPParamsGovStateL = prevPParamsShelleyGovStateL

  futurePParamsGovStateL = futurePParamsShelleyGovStateL

  obligationGovState = const mempty
