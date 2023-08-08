{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Cardano.Ledger.Mary.PParams () where

import Cardano.Ledger.Core
import Cardano.Ledger.Crypto
import Cardano.Ledger.Mary.Era (MaryEra)
import Cardano.Ledger.Shelley.Gov (EraGov (..), ShelleyGovState (..), curPParamsShelleyGovStateL, prevPParamsShelleyGovStateL)
import Cardano.Ledger.Shelley.PParams
import Data.Coerce
import Lens.Micro

instance Crypto c => EraPParams (MaryEra c) where
  type PParamsHKD f (MaryEra c) = ShelleyPParams f (MaryEra c)

  type UpgradePParams f (MaryEra c) = ()
  type DowngradePParams f (MaryEra c) = ()

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

instance Crypto c => EraGov (MaryEra c) where
  type GovState (MaryEra c) = ShelleyGovState (MaryEra c)
  emptyGovState =
    ShelleyGovState
      emptyPPPUpdates
      emptyPPPUpdates
      emptyPParams
      emptyPParams

  getProposedPPUpdates = Just . proposals

  curPParamsGovStateL = curPParamsShelleyGovStateL

  prevPParamsGovStateL = prevPParamsShelleyGovStateL
