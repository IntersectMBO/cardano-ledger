{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Test.Cardano.Ledger.Shelley.Binary.Golden (
  goldenNewEpochStateExpectation,
) where

import Cardano.Ledger.BaseTypes (BlocksMade (..), EpochNo (..))
import Cardano.Ledger.Binary (ToCBOR)
import Cardano.Ledger.Binary.Plain
import Cardano.Ledger.Core
import Cardano.Ledger.EpochBoundary
import Cardano.Ledger.Shelley.Core
import Cardano.Ledger.Shelley.LedgerState
import Data.Functor.Identity (Identity)
import qualified Data.Map.Strict as Map
import qualified Data.VMap as VMap
import Test.Cardano.Ledger.Binary.Plain.Golden
import Test.Cardano.Ledger.Common
import Test.Cardano.Ledger.Shelley.Arbitrary ()

goldenNewEpochStateExpectation ::
  forall era.
  ( HasCallStack
  , EraTxOut era
  , EncCBOR (StashedAVVMAddresses era)
  , EncCBOR (PParamsHKD Identity era)
  , ToCBOR (StashedAVVMAddresses era)
  , ToCBOR (TallyState era)
  , ToCBOR (PPUPState era)
  ) =>
  NewEpochState era ->
  Expectation
goldenNewEpochStateExpectation
  nes@NewEpochState
    { nesEs =
      EpochState
        { esAccountState = AccountState {..}
        , esSnapshots = SnapShots {..}
        , ..
        }
    , ..
    } =
    expectGoldenEncCBOR DiffCBOR nes $
      mconcat
        [ E (TkListLen 7)
        , E (TkWord64 (unEpochNo nesEL))
        , mapEnc (unBlocksMade nesBprev)
        , mapEnc (unBlocksMade nesBcur)
        , Em
            [ E (TkListLen 6)
            , Em
                [ E (TkListLen 2)
                , E asTreasury
                , E asReserves
                ]
            , E esLState
            , Em
                [ E (TkListLen 4)
                , snapShotEnc ssStakeMark
                , snapShotEnc ssStakeSet
                , snapShotEnc ssStakeGo
                , E ssFee
                ]
            , E esPrevPp
            , E esPp
            , Ev ver esNonMyopic
            ]
        , Ev ver nesRu
        , Ev ver nesPd
        , E stashedAVVMAddresses
        ]
    where
      ver = eraProtVerLow @era
      mapEnc m =
        Em
          [ E (TkMapLen (fromIntegral (Map.size m)))
          , Em [Ev ver k <> Ev ver v | (k, v) <- Map.toList m]
          ]
      snapShotEnc SnapShot {..} =
        Em
          [ E (TkListLen 3)
          , mapEnc (VMap.toMap (unStake ssStake))
          , Ev ver ssDelegations
          , Ev ver ssPoolParams
          ]
