-- | Module containing the imports needed to test that a given abstract trace
-- passes the concrete validation. This is useful when debugging
-- counterexamples.
--
-- Usage from the ghci repl:
--
--
-- > import Hedgehog
-- > Hedgehog.check $ property $ passConcreteValidation trace0
--
-- Replace @trace0@ by the trace under consideration.
--
module Test.Cardano.Chain.Block.Model.Examples where

import GHC.Exts
import Cardano.Prelude hiding (trace, State)

import Control.State.Transition
import Control.State.Transition.Trace

import Ledger.Core
import Ledger.Delegation
import Ledger.Update
import Ledger.UTxO
import Cardano.Ledger.Spec.STS.UTXO

import Cardano.Spec.Chain.STS.Block
import Cardano.Spec.Chain.STS.Rule.Chain

trace0 :: Trace CHAIN
trace0 = mkTrace traceEnv0 traceInitState0 traceTrans0
  where
    traceEnv0 =
      ( Slot { unSlot = 32768 }
      , UTxO
          { unUTxO =
              fromList
                [ ( TxIn TxId { getTxId = Hash { unHash = 9 } } 0
                  , TxOut
                      { addr = Addr (VKey Owner { unOwner = 9 })
                      , value = Lovelace { unLovelace = 1 }
                      }
                  )
                ]
          }
      , fromList
          [ VKeyGenesis { unVKeyGenesis = VKey Owner { unOwner = 0 } } ]
      , PParams
          { _maxBkSz = 10000
          , _maxHdrSz = 1000
          , _maxTxSz = 500
          , _maxPropSz = 10
          , _bkSgnCntT = 1.5285714285714287
          , _bkSlotsPerEpoch = SlotCount { unSlotCount = 10 }
          , _upTtl = SlotCount { unSlotCount = 10 }
          , _scriptVersion = 0
          , _upAdptThd = 0.6
          , _factorA = 1
          , _factorB = 2
          }
      , BlockCount { unBlockCount = 10 }
      )
    traceInitState0 =
      ( Slot { unSlot = 0 }
      , fromList []
      , Hash { unHash = -2578643520546668380 }
      , UTxOState
          { utxo =
              UTxO
                { unUTxO =
                    fromList
                      [ ( TxIn TxId { getTxId = Hash { unHash = 9 } } 0
                        , TxOut
                            { addr = Addr (VKey Owner { unOwner = 9 })
                            , value = Lovelace { unLovelace = 1 }
                            }
                        )
                      ]
                }
          , reserves = Lovelace { unLovelace = 44999999999999999 }
          }
      , DIState
          { _dIStateDelegationMap =
              fromList
                [ ( VKeyGenesis { unVKeyGenesis = VKey Owner { unOwner = 0 } }
                  , VKey Owner { unOwner = 0 }
                  )
                ]
          , _dIStateLastDelegation =
              fromList
                [ ( VKeyGenesis { unVKeyGenesis = VKey Owner { unOwner = 0 } }
                  , Slot { unSlot = 0 }
                  )
                ]
          , _dIStateScheduledDelegations = []
          , _dIStateKeyEpochDelegations = fromList []
          }
      , ( ( ProtVer { _pvMaj = 0 , _pvMin = 0 , _pvAlt = 0 }
          , PParams
              { _maxBkSz = 10000
              , _maxHdrSz = 1000
              , _maxTxSz = 500
              , _maxPropSz = 10
              , _bkSgnCntT = 1.5285714285714287
              , _bkSlotsPerEpoch = SlotCount { unSlotCount = 10 }
              , _upTtl = SlotCount { unSlotCount = 10 }
              , _scriptVersion = 0
              , _upAdptThd = 0.6
              , _factorA = 1
              , _factorB = 2
              }
          )
        , []
        , fromList []
        , fromList []
        , fromList []
        , fromList []
        , fromList []
        , fromList []
        , fromList []
        )
      )
    traceTrans0 =
      [ ( ( Slot { unSlot = 2 }
          , fromList
              [ VKeyGenesis { unVKeyGenesis = VKey Owner { unOwner = 0 } }
              , VKeyGenesis { unVKeyGenesis = VKey Owner { unOwner = 0 } }
              ]
          , Hash { unHash = 4712748416679222613 }
          , UTxOState
              { utxo =
                  UTxO
                    { unUTxO =
                        fromList
                          [ ( TxIn TxId { getTxId = Hash { unHash = 9 } } 0
                            , TxOut
                                { addr = Addr (VKey Owner { unOwner = 9 })
                                , value = Lovelace { unLovelace = 1 }
                                }
                            )
                          ]
                    }
              , reserves = Lovelace { unLovelace = 44999999999999999 }
              }
          , DIState
              { _dIStateDelegationMap =
                  fromList
                    [ ( VKeyGenesis { unVKeyGenesis = VKey Owner { unOwner = 0 } }
                      , VKey Owner { unOwner = 0 }
                      )
                    ]
              , _dIStateLastDelegation =
                  fromList
                    [ ( VKeyGenesis { unVKeyGenesis = VKey Owner { unOwner = 0 } }
                      , Slot { unSlot = 0 }
                      )
                    ]
              , _dIStateScheduledDelegations = []
              , _dIStateKeyEpochDelegations = fromList []
              }
          , ( ( ProtVer { _pvMaj = 0 , _pvMin = 0 , _pvAlt = 0 }
              , PParams
                  { _maxBkSz = 10000
                  , _maxHdrSz = 1000
                  , _maxTxSz = 500
                  , _maxPropSz = 10
                  , _bkSgnCntT = 1.5285714285714287
                  , _bkSlotsPerEpoch = SlotCount { unSlotCount = 10 }
                  , _upTtl = SlotCount { unSlotCount = 10 }
                  , _scriptVersion = 0
                  , _upAdptThd = 0.6
                  , _factorA = 1
                  , _factorB = 2
                  }
              )
            , []
            , fromList []
            , fromList
                [ ( UpId 7
                  , ( ProtVer { _pvMaj = 1 , _pvMin = 0 , _pvAlt = 0 }
                    , PParams
                        { _maxBkSz = 9900
                        , _maxHdrSz = 1000
                        , _maxTxSz = 489
                        , _maxPropSz = 10
                        , _bkSgnCntT = 1.5285714285714287
                        , _bkSlotsPerEpoch = SlotCount { unSlotCount = 10 }
                        , _upTtl = SlotCount { unSlotCount = 2 }
                        , _scriptVersion = 0
                        , _upAdptThd = 0.0
                        , _factorA = 0
                        , _factorB = 0
                        }
                    )
                  )
                ]
            , fromList [ ( UpId 7 , ( ApName "" , ApVer 0 , Metadata ) ) ]
            , fromList [ ( UpId 7 , Slot { unSlot = 2 } ) ]
            , fromList
                [ ( UpId 7
                  , VKeyGenesis { unVKeyGenesis = VKey Owner { unOwner = 0 } }
                  )
                ]
            , fromList []
            , fromList [ ( UpId 7 , Slot { unSlot = 1 } ) ]
            )
          )
        , Block
            { _bHeader =
                MkBlockHeader
                  { _bhPrevHash = Hash { unHash = -7234408896617878313 }
                  , _bhSlot = Slot { unSlot = 2 }
                  , _bhIssuer = VKey Owner { unOwner = 0 }
                  , _bhSig =
                      Sig Hash { unHash = 4712748416679222613 } Owner { unOwner = 0 }
                  , _bhUtxoHash = Hash { unHash = 839657738087498284 }
                  , _bhDlgHash = Hash { unHash = 839657738087498284 }
                  , _bhUpdHash = Hash { unHash = 2381335483050850259 }
                  }
            , _bBody =
                BlockBody
                  { _bDCerts = []
                  , _bUtxo = []
                  , _bUpdProp = Nothing
                  , _bUpdVotes =
                      [ Vote
                          { _vCaster = VKey Owner { unOwner = 0 }
                          , _vPropId = UpId 7
                          , _vSig = Sig (UpId 7) Owner { unOwner = 0 }
                          }
                      ]
                  , _bProtVer = ProtVer { _pvMaj = 0 , _pvMin = 0 , _pvAlt = 0 }
                  }
            }
        )
      ,
        ( ( Slot { unSlot = 1 }
          , fromList
              [ VKeyGenesis { unVKeyGenesis = VKey Owner { unOwner = 0 } } ]
          , Hash { unHash = -7234408896617878313 }
          , UTxOState
              { utxo =
                  UTxO
                    { unUTxO =
                        fromList
                          [ ( TxIn TxId { getTxId = Hash { unHash = 9 } } 0
                            , TxOut
                                { addr = Addr (VKey Owner { unOwner = 9 })
                                , value = Lovelace { unLovelace = 1 }
                                }
                            )
                          ]
                    }
              , reserves = Lovelace { unLovelace = 44999999999999999 }
              }
          , DIState
              { _dIStateDelegationMap =
                  fromList
                    [ ( VKeyGenesis { unVKeyGenesis = VKey Owner { unOwner = 0 } }
                      , VKey Owner { unOwner = 0 }
                      )
                    ]
              , _dIStateLastDelegation =
                  fromList
                    [ ( VKeyGenesis { unVKeyGenesis = VKey Owner { unOwner = 0 } }
                      , Slot { unSlot = 0 }
                      )
                    ]
              , _dIStateScheduledDelegations = []
              , _dIStateKeyEpochDelegations = fromList []
              }
          , ( ( ProtVer { _pvMaj = 0 , _pvMin = 0 , _pvAlt = 0 }
              , PParams
                  { _maxBkSz = 10000
                  , _maxHdrSz = 1000
                  , _maxTxSz = 500
                  , _maxPropSz = 10
                  , _bkSgnCntT = 1.5285714285714287
                  , _bkSlotsPerEpoch = SlotCount { unSlotCount = 10 }
                  , _upTtl = SlotCount { unSlotCount = 10 }
                  , _scriptVersion = 0
                  , _upAdptThd = 0.6
                  , _factorA = 1
                  , _factorB = 2
                  }
              )
            , []
            , fromList []
            , fromList
                [ ( UpId 7
                  , ( ProtVer { _pvMaj = 1 , _pvMin = 0 , _pvAlt = 0 }
                    , PParams
                        { _maxBkSz = 9900
                        , _maxHdrSz = 1000
                        , _maxTxSz = 489
                        , _maxPropSz = 10
                        , _bkSgnCntT = 1.5285714285714287
                        , _bkSlotsPerEpoch = SlotCount { unSlotCount = 10 }
                        , _upTtl = SlotCount { unSlotCount = 2 }
                        , _scriptVersion = 0
                        , _upAdptThd = 0.0
                        , _factorA = 0
                        , _factorB = 0
                        }
                    )
                  )
                ]
            , fromList [ ( UpId 7 , ( ApName "" , ApVer 0 , Metadata ) ) ]
            , fromList []
            , fromList []
            , fromList []
            , fromList [ ( UpId 7 , Slot { unSlot = 1 } ) ]
            )
          )
        , Block
            { _bHeader =
                MkBlockHeader
                  { _bhPrevHash = Hash { unHash = -2578643520546668380 }
                  , _bhSlot = Slot { unSlot = 1 }
                  , _bhIssuer = VKey Owner { unOwner = 0 }
                  , _bhSig =
                      Sig Hash { unHash = -7234408896617878313 } Owner { unOwner = 0 }
                  , _bhUtxoHash = Hash { unHash = 839657738087498284 }
                  , _bhDlgHash = Hash { unHash = 839657738087498284 }
                  , _bhUpdHash = Hash { unHash = -4515862477882442474 }
                  }
            , _bBody =
                BlockBody
                  { _bDCerts = []
                  , _bUtxo = []
                  , _bUpdProp =
                      Just
                        UProp
                          { _upId = UpId 7
                          , _upIssuer = VKey Owner { unOwner = 0 }
                          , _upParams =
                              PParams
                                { _maxBkSz = 9900
                                , _maxHdrSz = 1000
                                , _maxTxSz = 489
                                , _maxPropSz = 10
                                , _bkSgnCntT = 1.5285714285714287
                                , _bkSlotsPerEpoch = SlotCount { unSlotCount = 10 }
                                , _upTtl = SlotCount { unSlotCount = 2 }
                                , _scriptVersion = 0
                                , _upAdptThd = 0.0
                                , _factorA = 0
                                , _factorB = 0
                                }
                          , _upPV = ProtVer { _pvMaj = 1 , _pvMin = 0 , _pvAlt = 0 }
                          , _upSwVer = SwVer { _svName = ApName "" , _svVer = ApVer 0 }
                          , _upSig =
                              Sig
                                ( ProtVer { _pvMaj = 1 , _pvMin = 0 , _pvAlt = 0 }
                                , PParams
                                    { _maxBkSz = 9900
                                    , _maxHdrSz = 1000
                                    , _maxTxSz = 489
                                    , _maxPropSz = 10
                                    , _bkSgnCntT = 1.5285714285714287
                                    , _bkSlotsPerEpoch = SlotCount { unSlotCount = 10 }
                                    , _upTtl = SlotCount { unSlotCount = 2 }
                                    , _scriptVersion = 0
                                    , _upAdptThd = 0.0
                                    , _factorA = 0
                                    , _factorB = 0
                                    }
                                , SwVer { _svName = ApName "" , _svVer = ApVer 0 }
                                )
                                Owner { unOwner = 0 }
                          , _upSTags = fromList []
                          , _upMdt = Metadata
                          }
                  , _bUpdVotes = []
                  , _bProtVer = ProtVer { _pvMaj = 0 , _pvMin = 0 , _pvAlt = 0 }
                  }
            }
        )
      ]
