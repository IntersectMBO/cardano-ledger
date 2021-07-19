{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE TypeApplications #-}

-- | Examples of the application of the delegation rules.
module Test.Byron.Spec.Ledger.Delegation.Examples
  ( deleg,
  )
where

import Byron.Spec.Ledger.Core
  ( BlockCount (BlockCount),
    Epoch (Epoch),
    Owner (Owner),
    Sig (Sig),
    Slot (Slot),
    VKey (VKey),
    VKeyGenesis (VKeyGenesis),
    owner,
  )
import Byron.Spec.Ledger.Delegation
  ( ADELEG,
    ADELEGS,
    DCert (DCert),
    DELEG,
    DIState (DIState),
    DSEnv (DSEnv),
    DSState (DSState),
    DState (DState),
    SDELEG,
    _dIStateDelegationMap,
    _dIStateKeyEpochDelegations,
    _dIStateLastDelegation,
    _dIStateScheduledDelegations,
    _dSEnvAllowedDelegators,
    _dSEnvEpoch,
    _dSEnvK,
    _dSEnvSlot,
  )
import Control.State.Transition.Trace (checkTrace, (.-), (.->))
import Data.Functor.Identity (runIdentity)
import Data.Set (Set, fromList)
import Data.Word (Word64)
import Numeric.Natural (Natural)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase)

-- | Delegation examples.
deleg :: [TestTree]
deleg =
  [ testGroup
      "Activation"
      [ testCase "Example 0" $
          checkTrace @ADELEG runIdentity genKeys $
            pure (DState [] [])
              .- (s 0, (gk 0, k 10))
                .-> DState
                  [(gk 0, k 10)]
                  [(gk 0, s 0)]
              .- (s 1, (gk 1, k 11))
                .-> DState
                  [(gk 0, k 10), (gk 1, k 11)]
                  [(gk 0, s 0), (gk 1, s 1)]
              -- Here we try to delegate to a key @k 11@ that is already delegated (by
              -- @gk 0@), so the state remains unaltered.
              .- (s 2, (gk 0, k 11))
                .-> DState
                  [(gk 0, k 10), (gk 1, k 11)]
                  [(gk 0, s 0), (gk 1, s 1)]
              .- (s 3, (gk 2, k 12))
                .-> DState
                  [(gk 0, k 10), (gk 1, k 11), (gk 2, k 12)]
                  [(gk 0, s 0), (gk 1, s 1), (gk 2, s 3)],
        testCase "Example 1" $
          checkTrace @ADELEG runIdentity genKeys $
            pure (DState [] [])
              .- (s 0, (gk 0, k 2))
                .-> DState
                  [(gk 0, k 2)]
                  [(gk 0, s 0)]
              -- Trying to delegate to a key that was delegated already has no effect
              -- should be a no-op on the delegation state.
              .- (s 1, (gk 1, k 2))
                .-> DState
                  [(gk 0, k 2)]
                  [(gk 0, s 0)],
        testCase "Example 2" $
          checkTrace @ADELEG runIdentity genKeys $
            pure (DState [] [])
              .- (s 6, (gk 1, k 2))
                .-> DState
                  [(gk 1, k 2)]
                  [(gk 1, s 6)]
              .- (s 7, (gk 2, k 2))
                .-> DState
                  [(gk 1, k 2)]
                  [(gk 1, s 6)]
              .- (s 16, (gk 1, k 0))
                .-> DState
                  [(gk 1, k 0)]
                  [(gk 1, s 16)]
              .- (s 19, (gk 2, k 0))
                .-> DState
                  [(gk 1, k 0)]
                  [(gk 1, s 16)]
      ],
    testGroup
      "Multiple Activations"
      [ testCase "Example 0" $
          checkTrace @ADELEGS runIdentity genKeys $
            pure (DState [] [])
              .- [ (s 4, (gk 1, k 0)),
                   (s 5, (gk 2, k 0)),
                   (s 5, (gk 1, k 1))
                 ]
                .-> DState
                  [(gk 1, k 1)]
                  [(gk 1, s 5)]
      ],
    testGroup
      "Scheduling"
      [ testCase "Example 0" $
          checkTrace @SDELEG runIdentity (DSEnv [gk 0, gk 1, gk 2] (e 8) (s 2) (bk 2160)) $
            pure (DSState [] [])
              .- dc (gk 0) (k 10) (e 8)
                .-> DSState
                  [(s 4322, (gk 0, k 10))]
                  [(e 8, gk 0)]
              .- dc (gk 1) (k 11) (e 8)
                .-> DSState
                  [(s 4322, (gk 0, k 10)), (s 4322, (gk 1, k 11))]
                  [(e 8, gk 0), (e 8, gk 1)]
              .- dc (gk 2) (k 10) (e 8)
                .-> DSState
                  [(s 4322, (gk 0, k 10)), (s 4322, (gk 1, k 11)), (s 4322, (gk 2, k 10))]
                  [(e 8, gk 0), (e 8, gk 1), (e 8, gk 2)]
      ],
    testGroup
      "Interface"
      [ testCase "Non-injective scheduled delegations are ignored." $
          let env =
                DSEnv
                  { _dSEnvAllowedDelegators = [gk 0, gk 1],
                    _dSEnvEpoch = e 0,
                    _dSEnvSlot = s 21,
                    _dSEnvK = bk 5
                  }
              st =
                DIState
                  { _dIStateDelegationMap =
                      [ ( gk 0,
                          k 0
                        ),
                        ( gk 1,
                          k 1
                        )
                      ],
                    _dIStateLastDelegation =
                      [ ( gk 0,
                          s 15
                        ),
                        ( gk 1,
                          s 0
                        )
                      ],
                    _dIStateScheduledDelegations =
                      [ ( s 21,
                          ( gk 1,
                            k 0
                          )
                        )
                      ],
                    _dIStateKeyEpochDelegations =
                      fromList
                        [ ( e 0,
                            gk 0
                          ),
                          ( e 0,
                            gk 1
                          ),
                          ( e 1,
                            gk 0
                          )
                        ]
                  }
           in checkTrace @DELEG runIdentity env $
                pure st .- [] .-> st {_dIStateScheduledDelegations = []}
      ]
  ]
  where
    s :: Word64 -> Slot
    s = Slot

    k :: Natural -> VKey
    k = VKey . Owner

    gk :: Natural -> VKeyGenesis
    gk = VKeyGenesis . k

    e :: Word64 -> Epoch
    e = Epoch

    bk :: Word64 -> BlockCount
    bk = BlockCount

    dc :: VKeyGenesis -> VKey -> Epoch -> DCert
    dc vkg vk ep = DCert vkg vk ep (Sig (vk, ep) (owner vkg))

    genKeys :: Set VKeyGenesis
    genKeys = fromList $ map (VKeyGenesis . VKey . Owner) [0 .. 6]
