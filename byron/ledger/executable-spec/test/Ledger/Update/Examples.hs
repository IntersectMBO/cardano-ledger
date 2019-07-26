{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE TypeApplications #-}

-- | Examples of the application of the update rules.
module Ledger.Update.Examples where

import           GHC.Exts (fromList)

import           Test.Tasty (TestTree, testGroup)
import           Test.Tasty.HUnit (testCase)

import           Ledger.Core
import           Ledger.Update

import           Control.State.Transition.Trace (checkTrace, (.-), (.->))

upiendExamples :: [TestTree]
upiendExamples =
  [ testGroup "UPIEND"
    [ testCase "Example 0" $
      let
        oldPParams =
          PParams
          { _maxBkSz = 10000
          , _maxHdrSz = 1000
          , _maxTxSz = 500
          , _maxPropSz = 10
          , _bkSgnCntT = 0.7857142857142857
          , _bkSlotsPerEpoch = SlotCount { unSlotCount = 10 }
          , _upTtl = SlotCount { unSlotCount = 10 }
          , _scriptVersion = 0
          , _cfmThd = 0.6
          , _upAdptThd = 0.6
          , _factorA = 1
          , _factorB = 2
          }
        newPParams =
          PParams
          { _maxBkSz = 9900
          , _maxHdrSz = 1000
          , _maxTxSz = 489
          , _maxPropSz = 10
          , _bkSgnCntT = 0.7857142857142857
          , _bkSlotsPerEpoch = SlotCount { unSlotCount = 10 }
          , _upTtl = SlotCount { unSlotCount = 2 }
          , _scriptVersion = 0
          , _cfmThd = 0.0
          , _upAdptThd = 0.0
          , _factorA = 0
          , _factorB = 0
          }
      in
        checkTrace @UPIEND
          ( Slot { unSlot = 15 }
          , [ ( VKeyGenesis { unVKeyGenesis = VKey Owner { unOwner = 0 } }
              , VKey Owner { unOwner = 0 }
              )
            , ( VKeyGenesis { unVKeyGenesis = VKey Owner { unOwner = 1 } }
              , VKey Owner { unOwner = 1 }
              )
            , ( VKeyGenesis { unVKeyGenesis = VKey Owner { unOwner = 2 } }
              , VKey Owner { unOwner = 2 }
              )
            , ( VKeyGenesis { unVKeyGenesis = VKey Owner { unOwner = 3 } }
              , VKey Owner { unOwner = 3 }
              )
            , ( VKeyGenesis { unVKeyGenesis = VKey Owner { unOwner = 4 } }
              , VKey Owner { unOwner = 4 }
              )
            ]
          , BlockCount { unBlockCount = 2 }
          , 5
          ) $

          pure
          ( ( ProtVer { _pvMaj = 0 , _pvMin = 0 , _pvAlt = 0 }
            , oldPParams
            )
          , []
          , fromList []
          , fromList
              [ ( UpId 1
                , ( ProtVer { _pvMaj = 1 , _pvMin = 0 , _pvAlt = 0 }
                  , newPParams
                  )
                )
              ]
          , fromList [ ( UpId 1 , ( ApName "" , ApVer 0 , Metadata ) ) ]
          , fromList [ ( UpId 1 , Slot { unSlot = 5 } ) ]
          , fromList
              [ ( UpId 1
                , VKeyGenesis { unVKeyGenesis = VKey Owner { unOwner = 1 } }
                )
              , ( UpId 1
                , VKeyGenesis { unVKeyGenesis = VKey Owner { unOwner = 2 } }
                )
              , ( UpId 1
                , VKeyGenesis { unVKeyGenesis = VKey Owner { unOwner = 3 } }
                )
              ]
          , fromList
              [ ( ProtVer { _pvMaj = 1 , _pvMin = 0 , _pvAlt = 0 }
                , VKeyGenesis { unVKeyGenesis = VKey Owner { unOwner = 1 } }
                )
              , ( ProtVer { _pvMaj = 1 , _pvMin = 0 , _pvAlt = 0 }
                , VKeyGenesis { unVKeyGenesis = VKey Owner { unOwner = 3 } }
                )
              ]
          , fromList [ ( UpId 1 , Slot { unSlot = 2 } ) ]
          )

        .- (ProtVer { _pvMaj = 1 , _pvMin = 0 , _pvAlt = 0 }, VKey Owner { unOwner = 0 }) .->

          ( ( ProtVer { _pvMaj = 0 , _pvMin = 0 , _pvAlt = 0 }
            , oldPParams
            )
          , [ ( Slot {unSlot = 15}
                       , ( ProtVer {_pvMaj = 1, _pvMin = 0, _pvAlt = 0}
                         , newPParams
                         )
                       )
                     ]
          , fromList []
          , fromList
              [ ( UpId 1
                , ( ProtVer { _pvMaj = 1 , _pvMin = 0 , _pvAlt = 0 }
                  , newPParams
                  )
                )
              ]
          , fromList [ ( UpId 1 , ( ApName "" , ApVer 0 , Metadata ) ) ]
          , fromList [ ( UpId 1 , Slot { unSlot = 5 } ) ]
          , fromList
              [ ( UpId 1
                , VKeyGenesis { unVKeyGenesis = VKey Owner { unOwner = 1 } }
                )
              , ( UpId 1
                , VKeyGenesis { unVKeyGenesis = VKey Owner { unOwner = 2 } }
                )
              , ( UpId 1
                , VKeyGenesis { unVKeyGenesis = VKey Owner { unOwner = 3 } }
                )
              ]
          , fromList
              [ ( ProtVer { _pvMaj = 1 , _pvMin = 0 , _pvAlt = 0 }
                , VKeyGenesis { unVKeyGenesis = VKey Owner { unOwner = 0 } }
                )
              , ( ProtVer { _pvMaj = 1 , _pvMin = 0 , _pvAlt = 0 }
                , VKeyGenesis { unVKeyGenesis = VKey Owner { unOwner = 1 } }
                )
              , ( ProtVer { _pvMaj = 1 , _pvMin = 0 , _pvAlt = 0 }
                , VKeyGenesis { unVKeyGenesis = VKey Owner { unOwner = 3 } }
                )
              ]
          , fromList [ ( UpId 1 , Slot { unSlot = 2 } ) ]
          )

    ]
  ]
