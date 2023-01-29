{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Test.Cardano.Ledger.Shelley.Binary.Golden (
  goldenNewEpochStateExpectation,
) where

import Cardano.Ledger.BaseTypes (BlocksMade (..), EpochNo (..))
import Cardano.Ledger.Binary.Plain
import Cardano.Ledger.EpochBoundary
import Cardano.Ledger.Shelley.Core
import Cardano.Ledger.Shelley.LedgerState
import qualified Data.Map.Strict as Map
import qualified Data.VMap as VMap
import Test.Cardano.Ledger.Binary.Plain.Golden
import Test.Cardano.Ledger.Common
import Test.Cardano.Ledger.Shelley.Arbitrary ()

goldenNewEpochStateExpectation ::
  ( HasCallStack
  , EraTxOut era
  , EncCBOR (StashedAVVMAddresses era)
  , EncCBOR (PPUPState era)
  , EncCBOR (TallyState era)
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
            , E esNonMyopic
            ]
        , E nesRu
        , E nesPd
        , E stashedAVVMAddresses
        ]
    where
      mapEnc m =
        Em
          [ E (TkMapLen (fromIntegral (Map.size m)))
          , Em [E k <> E v | (k, v) <- Map.toList m]
          ]
      snapShotEnc (SnapShot {..}) =
        Em
          [ E (TkListLen 3)
          , mapEnc (VMap.toMap (unStake ssStake))
          , E ssDelegations
          , E ssPoolParams
          ]
