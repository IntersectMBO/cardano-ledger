{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE OverloadedLists #-}
-- | Examples of the application of the delegation rules.
module Ledger.Delegation.Examples
  ( deleg
  )
where

import qualified Data.Bimap as Bimap (fromList)
import Data.Set (fromList, Set)
import Data.Word (Word64)
import Numeric.Natural (Natural)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase)

import Ledger.Core
  ( BlockCount(BlockCount)
  , Epoch(Epoch)
  , Owner(Owner)
  , Sig(Sig)
  , Slot(Slot)
  , VKey(VKey)
  , VKeyGenesis(VKeyGenesis)
  , owner
  )
import Ledger.Delegation
  ( ADELEG
  , ADELEGS
  , DCert(DCert)
  , DSEnv(DSEnv)
  , DSState(DSState)
  , DState(DState)
  , SDELEG
  )
import Control.State.Transition.Trace ((.-), (.->), checkTrace)

-- | Delegation examples.
deleg :: [TestTree]
deleg =
  [ testGroup "Activation"
    [ testCase "Example 0" $ checkTrace @ADELEG genKeys $

      pure (DState (Bimap.fromList []) [])

      .- (s 0, (gk 0, k 10)) .-> DState (Bimap.fromList [(gk 0, k 10)])
                                        [(gk 0, s 0)]

      .- (s 1, (gk 1, k 11)) .-> DState (Bimap.fromList [(gk 0, k 10), (gk 1, k 11)])
                                        [(gk 0, s 0), (gk 1, s 1)]

      -- Here we try to delegate to a key @k 11@ that is already delegated (by
      -- @gk 0@), so the state remains unaltered.
      .- (s 2, (gk 0, k 11)) .-> DState (Bimap.fromList [(gk 0, k 10), (gk 1, k 11)])
                                        [(gk 0, s 0), (gk 1, s 1)]

      .- (s 3, (gk 2, k 12)) .-> DState (Bimap.fromList [(gk 0, k 10), (gk 1, k 11), (gk 2, k 12)])
                                        [(gk 0, s 0), (gk 1, s 1), (gk 2, s 3)]

    , testCase "Example 1" $ checkTrace @ADELEG genKeys $

      pure (DState (Bimap.fromList []) [])

      .- (s 0, (gk 0, k 2)) .-> DState (Bimap.fromList [(gk 0, k 2)])
                                       [(gk 0, s 0)]

      -- Trying to delegate to a key that was delegated already has no effect
      -- should be a no-op on the delegation state.
      .- (s 1, (gk 1, k 2)) .-> DState (Bimap.fromList [(gk 0, k 2)])
                                       [(gk 0, s 0)]
    , testCase "Example 2" $ checkTrace @ADELEG genKeys $

      pure (DState (Bimap.fromList []) [])

      .- (s 6, (gk 1, k 2)) .-> DState (Bimap.fromList [(gk 1, k 2)])
                                       [(gk 1, s 6)]

      .- (s 7, (gk 2, k 2)) .-> DState (Bimap.fromList [(gk 1, k 2)])
                                       [(gk 1, s 6)]

      .- (s 16, (gk 1, k 0)) .-> DState (Bimap.fromList [(gk 1, k 0)])
                                        [(gk 1, s 16)]

      .- (s 19, (gk 2, k 0)) .-> DState (Bimap.fromList [(gk 1, k 0)])
                                        [(gk 1, s 16)]

    ]

  , testGroup "Multiple Activations"
    [ testCase "Example 0" $ checkTrace @ADELEGS genKeys $

      pure (DState (Bimap.fromList []) [])

      .- [ (s 4, (gk 1, k 0))
         , (s 5, (gk 2, k 0))
         , (s 5, (gk 1, k 1)) ] .-> DState (Bimap.fromList [(gk 1, k 1)])
                                           [(gk 1, s 5)]

    ]

  , testGroup "Scheduling"
    [ testCase "Example 0" $ checkTrace @SDELEG (DSEnv [gk 0, gk 1, gk 2] (e 8) (s 2) (bk 2160)) $

      pure (DSState [] [])

    .- dc (gk 0) (k 10) (e 8) .-> DSState [(s 4322, (gk 0, k 10))]
                                          [(e 8, gk 0)]

    .- dc (gk 1) (k 11) (e 8) .-> DSState [(s 4322, (gk 0, k 10)), (s 4322, (gk 1, k 11))]
                                          [(e 8, gk 0), (e 8, gk 1)]

    .- dc (gk 2) (k 10) (e 8) .-> DSState [(s 4322, (gk 0, k 10)), (s 4322, (gk 1, k 11)), (s 4322, (gk 2, k 10))]
                                          [(e 8, gk 0), (e 8, gk 1), (e 8, gk 2)]
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
    dc vkg vk ep = DCert (vk, ep) (Sig vkg (owner vkg)) (vkg, vk) ep

    genKeys :: Set VKeyGenesis
    genKeys = fromList $ map (VKeyGenesis . VKey . Owner) [0 .. 6]
