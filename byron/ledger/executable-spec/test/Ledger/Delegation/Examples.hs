{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE OverloadedLists #-}
-- | Examples of the application of the delegation rules.
module Ledger.Delegation.Examples
  ( deleg
  )
where

import Data.Set (fromList, Set)
import Data.Word (Word64)
import Numeric.Natural (Natural)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase)

import Ledger.Core
  ( Epoch(Epoch)
  , Owner(Owner)
  , Sig(Sig)
  , Slot(Slot)
  , BlockCount(BlockCount)
  , VKey(VKey)
  , VKeyGenesis(VKeyGenesis)
  , owner
  )
import Ledger.Delegation
  ( ADELEG
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

      pure (DState [] [])

      .- (s 0, (gk 0, k 10)) .-> DState [(gk 0, k 10)]
                                        [(gk 0, s 0)]

      .- (s 1, (gk 1, k 11)) .-> DState [(gk 0, k 10), (gk 1, k 11)]
                                        [(gk 0, s 0), (gk 1, s 1)]

      .- (s 2, (gk 0, k 11)) .-> DState [(gk 0, k 11), (gk 1, k 11)]
                                        [(gk 0, s 2), (gk 1, s 1)]

      .- (s 3, (gk 2, k 12)) .-> DState [(gk 0, k 11), (gk 1, k 11), (gk 2, k 12)]
                                        [(gk 0, s 2), (gk 1, s 1), (gk 2, s 3)]
    ]

  , testGroup "Scheduling"
    [ testCase "Example 0" $ checkTrace @SDELEG (DSEnv [gk 0, gk 1, gk 2] (e 8) (s 2) (bc 5)) $

      pure (DSState [] [])

    .- dc (gk 0) (k 10) (e 8) .-> DSState [(s 12, (gk 0, k 10))]
                                          [(e 8, gk 0)]

    .- dc (gk 1) (k 11) (e 8) .-> DSState [(s 12, (gk 0, k 10)), (s 12, (gk 1, k 11))]
                                          [(e 8, gk 0), (e 8, gk 1)]

    .- dc (gk 2) (k 10) (e 8) .-> DSState [(s 12, (gk 0, k 10)), (s 12, (gk 1, k 11)), (s 12, (gk 2, k 10))]
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

    bc :: Word64 -> BlockCount
    bc = BlockCount


    e :: Word64 -> Epoch
    e = Epoch

    dc :: VKeyGenesis -> VKey -> Epoch -> DCert
    dc vkg vk ep = DCert (vk, ep) (Sig vkg (owner vkg)) (vkg, vk) ep

    genKeys :: Set VKeyGenesis
    genKeys = fromList $ map (VKeyGenesis . VKey . Owner) [0 .. 6]
