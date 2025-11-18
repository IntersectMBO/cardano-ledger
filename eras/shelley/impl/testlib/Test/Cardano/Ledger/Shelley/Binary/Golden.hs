{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Test.Cardano.Ledger.Shelley.Binary.Golden (
  goldenNewEpochStateExpectation,
  duplicateCertsTx,
  module Test.Cardano.Ledger.Core.Binary.Golden,
) where

import Cardano.Ledger.BaseTypes (BlocksMade (..), EpochNo (..))
import Cardano.Ledger.Binary (
  EncCBOR,
  ToCBOR (..),
  Tokens (..),
  Version,
  lengthThreshold,
 )
import Cardano.Ledger.Coin (Coin (..))
import Cardano.Ledger.Core
import Cardano.Ledger.Credential (Credential (..))
import Cardano.Ledger.Shelley.Core
import Cardano.Ledger.Shelley.LedgerState
import Cardano.Ledger.State
import Cardano.Ledger.TxIn (TxIn)
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import qualified Data.VMap as VMap
import Test.Cardano.Ledger.Binary.Plain.Golden
import Test.Cardano.Ledger.Common
import Test.Cardano.Ledger.Core.Binary.Golden
import Test.Cardano.Ledger.Shelley.Arbitrary ()
import Test.Cardano.Ledger.Shelley.Era (ShelleyEraTest)
import Test.Cardano.Ledger.Core.KeyPair (mkKeyHash)

duplicateCertsTx :: forall era. ShelleyEraTest era => Version -> Enc
duplicateCertsTx v =
  mconcat
    [ E $ TkMapLen 4
    , Em [E @Int 0, Ev v $ Set.empty @TxIn]
    , Em [E @Int 1, Ev v $ [] @(TxOut era)]
    , Em [E @Int 2, E $ Coin 0]
    , Em
        [ E @Int 4
        , Em
            [ E $ TkTag 258
            , E $ TkListLen 2
            , cert
            , cert
            ]
        ]
    ]
  where
    cert =
      Em
        [ E $ TkListLen 3
        , E @Int 2
        , E . KeyHashObj $ mkKeyHash @Staking 0
        , E $ mkKeyHash @StakePool 1
        ]

goldenNewEpochStateExpectation ::
  forall era.
  ( HasCallStack
  , EraTxOut era
  , EraGov era
  , EraStake era
  , ToCBOR (StashedAVVMAddresses era)
  , EncCBOR (StashedAVVMAddresses era)
  , EraCertState era
  ) =>
  NewEpochState era ->
  Expectation
goldenNewEpochStateExpectation
  nes@NewEpochState
    { nesEs =
      EpochState
        { esChainAccountState = ChainAccountState {..}
        , esSnapshots = SnapShots {..}
        , ..
        }
    , ..
    } =
    expectGoldenToCBOR DiffHex nes $
      mconcat
        [ E (TkListLen 7)
        , E (TkWord64 (unEpochNo nesEL))
        , mapEnc (unBlocksMade nesBprev)
        , mapEnc (unBlocksMade nesBcur)
        , Em
            [ E (TkListLen 4)
            , Em
                [ E (TkListLen 2)
                , E casTreasury
                , E casReserves
                ]
            , E esLState
            , Em
                [ E (TkListLen 4)
                , snapShotEnc ssStakeMark
                , snapShotEnc ssStakeSet
                , snapShotEnc ssStakeGo
                , E ssFee
                ]
            , Ev ver esNonMyopic
            ]
        , Ev ver nesRu
        , Ev ver nesPd
        , E stashedAVVMAddresses
        ]
    where
      ver = eraProtVerLow @era
      mapEnc m
        | Map.size m > lengthThreshold =
            Em [E TkMapBegin, me, E TkBreak]
        | otherwise =
            Em [E (TkMapLen (fromIntegral (Map.size m))), me]
        where
          me = Em [Ev ver k <> Ev ver v | (k, v) <- Map.toList m]
      snapShotEnc SnapShot {..} =
        Em
          [ E (TkListLen 3)
          , mapEnc (VMap.toMap (unStake ssStake))
          , Ev ver ssDelegations
          , Ev ver ssPoolParams
          ]
