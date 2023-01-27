{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Test.Cardano.Ledger.Shelley.Binary.Golden (
  goldenNewEpochStateExpectation,
) where

import Cardano.Ledger.BaseTypes (BlocksMade (..))
import Cardano.Ledger.Binary.Plain
import Cardano.Ledger.EpochBoundary
import Cardano.Ledger.Shelley.Core
import Cardano.Ledger.Shelley.LedgerState
import qualified Data.Map.Strict as Map
import Test.Cardano.Ledger.Binary.Plain.Golden
import Test.Cardano.Ledger.Common
import Test.Cardano.Ledger.Shelley.Arbitrary ()

goldenNewEpochStateExpectation ::
  ( EraTxOut era
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
        , E nesEL
        , blockMadeEnc nesBprev
        , blockMadeEnc nesBcur
        , Em
            [ E (TkListLen 6)
            , Em
                [ E (TkListLen 2)
                , E asTreasury
                , E asReserves
                ]
            , Em
                [ E (TkListLen 4)
                , E ssStakeMark
                , E ssStakeSet
                , E ssStakeGo
                , E ssFee
                ]
            , E esLState
            , E esPrevPp
            , E esPp
            , E esNonMyopic
            ]
        , E nesRu
        , E nesPd
        , E stashedAVVMAddresses
        ]
    where
      blockMadeEnc (BlocksMade m) =
        Em
          [ E (TkMapLen (fromIntegral (Map.size m)))
          , Em [E k <> E v | (k, v) <- Map.toList m]
          ]
