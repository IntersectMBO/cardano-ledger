{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}

module Test.Cardano.Ledger.State.StakePoolSpec (spec) where

import Cardano.Ledger.BaseTypes (EpochNo, StrictMaybe (..))
import Cardano.Ledger.Coin
import Cardano.Ledger.Core
import Cardano.Ledger.Credential (Credential)
import Cardano.Ledger.State
import Data.Set (Set)
import Test.Cardano.Ledger.Common
import Test.Cardano.Ledger.Core.Arbitrary ()

spec :: Spec
spec = do
  describe "StakePoolState" $ do
    prop "mkStakePoolState/stakePoolStateToPoolParams round-trip" $
      \( stakePoolParams :: StakePoolParams era
         , deposit :: CompactForm Coin
         , delegs :: Set (Credential Staking)
         , epochNo :: EpochNo
         , blsKeyState
         ) ->
          let poolId = sppId stakePoolParams
              network = aaNetworkId $ sppAccountAddress stakePoolParams
              stakePoolState = mkStakePoolState epochNo blsKeyState deposit delegs stakePoolParams
              stakePoolParams' = stakePoolStateToStakePoolParams network poolId stakePoolState
           in stakePoolParams === stakePoolParams'

  describe "BLS key registration epoch" $ do
    blsKeyUpdateProp "No key in the state, none in the params" NoStateKey NoParamsKey NoKeyRegistration
    blsKeyUpdateProp "No key in the state, one in the params" NoStateKey FreshKey FreshRegistration
    blsKeyUpdateProp "Existing key is dropped" HasStateKey NoParamsKey NoKeyRegistration
    blsKeyUpdateProp "The same key is registered again" HasStateKey SameAsStateKey OriginalRegistration
    blsKeyUpdateProp "A different key is registered" HasStateKey FreshKey FreshRegistration

data StateKey = NoStateKey | HasStateKey deriving (Eq, Show)

data ParamsKey = NoParamsKey | SameAsStateKey | FreshKey deriving (Eq, Show)

data ExpectedStateOutcome = NoKeyRegistration | OriginalRegistration | FreshRegistration
  deriving (Eq, Show)

blsKeyUpdateProp :: String -> StateKey -> ParamsKey -> ExpectedStateOutcome -> Spec
blsKeyUpdateProp msg keyInStateSpec keyInParamsSpec expectedOutcome =
  prop msg $
    \epochNo (blsKeyState :: BlsKeyState) freshBlsKey deposit delegs spp ->
      bksKey blsKeyState
        /= freshBlsKey
          ==> let
                keyInParams = case keyInParamsSpec of
                  NoParamsKey -> SNothing
                  SameAsStateKey -> SJust $ bksKey blsKeyState
                  FreshKey -> SJust freshBlsKey
                mbBlsKeyState = case keyInStateSpec of
                  NoStateKey -> SNothing
                  HasStateKey -> SJust blsKeyState
                updatedBlsKeyState =
                  spsBlsKey $ mkStakePoolState epochNo mbBlsKeyState deposit delegs (spp {sppBlsKey = keyInParams})
                expectedBlsKeyState = case expectedOutcome of
                  NoKeyRegistration -> SNothing
                  OriginalRegistration -> mbBlsKeyState
                  FreshRegistration -> SJust (BlsKeyState freshBlsKey epochNo)
               in
                updatedBlsKeyState === expectedBlsKeyState
