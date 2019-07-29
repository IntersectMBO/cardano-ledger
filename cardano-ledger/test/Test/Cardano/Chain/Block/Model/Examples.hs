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

trace1 :: Trace CHAIN
trace1 = mkTrace traceEnv1 traceInitState1 traceTrans1
  where
    traceEnv1 =
      ( Slot { unSlot = 1875963617 }
      , UTxO
          { unUTxO =
              fromList
                [ ( TxIn TxId { getTxId = Hash { unHash = 0 } } 0
                  , TxOut
                      { addr = Addr (VKey Owner { unOwner = 0 })
                      , value = Lovelace { unLovelace = 6733 }
                      }
                  )
                , ( TxIn TxId { getTxId = Hash { unHash = 1 } } 0
                  , TxOut
                      { addr = Addr (VKey Owner { unOwner = 1 })
                      , value = Lovelace { unLovelace = 4795 }
                      }
                  )
                , ( TxIn TxId { getTxId = Hash { unHash = 2 } } 0
                  , TxOut
                      { addr = Addr (VKey Owner { unOwner = 2 })
                      , value = Lovelace { unLovelace = 8020 }
                      }
                  )
                , ( TxIn TxId { getTxId = Hash { unHash = 3 } } 0
                  , TxOut
                      { addr = Addr (VKey Owner { unOwner = 3 })
                      , value = Lovelace { unLovelace = 1631 }
                      }
                  )
                , ( TxIn TxId { getTxId = Hash { unHash = 4 } } 0
                  , TxOut
                      { addr = Addr (VKey Owner { unOwner = 4 })
                      , value = Lovelace { unLovelace = 7829 }
                      }
                  )
                , ( TxIn TxId { getTxId = Hash { unHash = 7 } } 0
                  , TxOut
                      { addr = Addr (VKey Owner { unOwner = 7 })
                      , value = Lovelace { unLovelace = 6863 }
                      }
                  )
                , ( TxIn TxId { getTxId = Hash { unHash = 8 } } 0
                  , TxOut
                      { addr = Addr (VKey Owner { unOwner = 8 })
                      , value = Lovelace { unLovelace = 8623 }
                      }
                  )
                , ( TxIn TxId { getTxId = Hash { unHash = 9 } } 0
                  , TxOut
                      { addr = Addr (VKey Owner { unOwner = 9 })
                      , value = Lovelace { unLovelace = 7230 }
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
          , _bkSgnCntT = 2.071538545155317
          , _bkSlotsPerEpoch = SlotCount { unSlotCount = 10 }
          , _upTtl = SlotCount { unSlotCount = 10 }
          , _scriptVersion = 0
          , _upAdptThd = 0.6
          , _factorA = 1
          , _factorB = 2
          }
      , BlockCount { unBlockCount = 2 }
      )
    traceInitState1 =
      ( Slot { unSlot = 0 }
      , fromList []
      , Hash { unHash = -2578643520546668380 }
      , UTxOState
          { utxo =
              UTxO
                { unUTxO =
                    fromList
                      [ ( TxIn TxId { getTxId = Hash { unHash = 0 } } 0
                        , TxOut
                            { addr = Addr (VKey Owner { unOwner = 0 })
                            , value = Lovelace { unLovelace = 6733 }
                            }
                        )
                      , ( TxIn TxId { getTxId = Hash { unHash = 1 } } 0
                        , TxOut
                            { addr = Addr (VKey Owner { unOwner = 1 })
                            , value = Lovelace { unLovelace = 4795 }
                            }
                        )
                      , ( TxIn TxId { getTxId = Hash { unHash = 2 } } 0
                        , TxOut
                            { addr = Addr (VKey Owner { unOwner = 2 })
                            , value = Lovelace { unLovelace = 8020 }
                            }
                        )
                      , ( TxIn TxId { getTxId = Hash { unHash = 3 } } 0
                        , TxOut
                            { addr = Addr (VKey Owner { unOwner = 3 })
                            , value = Lovelace { unLovelace = 1631 }
                            }
                        )
                      , ( TxIn TxId { getTxId = Hash { unHash = 4 } } 0
                        , TxOut
                            { addr = Addr (VKey Owner { unOwner = 4 })
                            , value = Lovelace { unLovelace = 7829 }
                            }
                        )
                      , ( TxIn TxId { getTxId = Hash { unHash = 7 } } 0
                        , TxOut
                            { addr = Addr (VKey Owner { unOwner = 7 })
                            , value = Lovelace { unLovelace = 6863 }
                            }
                        )
                      , ( TxIn TxId { getTxId = Hash { unHash = 8 } } 0
                        , TxOut
                            { addr = Addr (VKey Owner { unOwner = 8 })
                            , value = Lovelace { unLovelace = 8623 }
                            }
                        )
                      , ( TxIn TxId { getTxId = Hash { unHash = 9 } } 0
                        , TxOut
                            { addr = Addr (VKey Owner { unOwner = 9 })
                            , value = Lovelace { unLovelace = 7230 }
                            }
                        )
                      ]
                }
          , reserves = Lovelace { unLovelace = 44999999999948276 }
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
              , _bkSgnCntT = 2.071538545155317
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
    traceTrans1 =
      [ ( ( Slot { unSlot = 186 }
          , fromList
              [ VKeyGenesis { unVKeyGenesis = VKey Owner { unOwner = 0 } }
              , VKeyGenesis { unVKeyGenesis = VKey Owner { unOwner = 0 } }
              ]
          , Hash { unHash = -300149826283586134 }
          , UTxOState
              { utxo =
                  UTxO
                    { unUTxO =
                        fromList
                          [ ( TxIn TxId { getTxId = Hash { unHash = 0 } } 0
                            , TxOut
                                { addr = Addr (VKey Owner { unOwner = 0 })
                                , value = Lovelace { unLovelace = 6733 }
                                }
                            )
                          , ( TxIn TxId { getTxId = Hash { unHash = 1 } } 0
                            , TxOut
                                { addr = Addr (VKey Owner { unOwner = 1 })
                                , value = Lovelace { unLovelace = 4795 }
                                }
                            )
                          , ( TxIn TxId { getTxId = Hash { unHash = 2 } } 0
                            , TxOut
                                { addr = Addr (VKey Owner { unOwner = 2 })
                                , value = Lovelace { unLovelace = 8020 }
                                }
                            )
                          , ( TxIn TxId { getTxId = Hash { unHash = 3 } } 0
                            , TxOut
                                { addr = Addr (VKey Owner { unOwner = 3 })
                                , value = Lovelace { unLovelace = 1631 }
                                }
                            )
                          , ( TxIn TxId { getTxId = Hash { unHash = 4 } } 0
                            , TxOut
                                { addr = Addr (VKey Owner { unOwner = 4 })
                                , value = Lovelace { unLovelace = 7829 }
                                }
                            )
                          , ( TxIn TxId { getTxId = Hash { unHash = 7 } } 0
                            , TxOut
                                { addr = Addr (VKey Owner { unOwner = 7 })
                                , value = Lovelace { unLovelace = 6863 }
                                }
                            )
                          , ( TxIn TxId { getTxId = Hash { unHash = 8 } } 0
                            , TxOut
                                { addr = Addr (VKey Owner { unOwner = 8 })
                                , value = Lovelace { unLovelace = 8623 }
                                }
                            )
                          , ( TxIn TxId { getTxId = Hash { unHash = 9 } } 0
                            , TxOut
                                { addr = Addr (VKey Owner { unOwner = 9 })
                                , value = Lovelace { unLovelace = 7230 }
                                }
                            )
                          ]
                    }
              , reserves = Lovelace { unLovelace = 44999999999948276 }
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
          , ( ( ProtVer { _pvMaj = 2 , _pvMin = 0 , _pvAlt = 0 }
              , PParams
                  { _maxBkSz = 110505
                  , _maxHdrSz = 1490
                  , _maxTxSz = 17458
                  , _maxPropSz = 52
                  , _bkSgnCntT = 2.0744323473896933
                  , _bkSlotsPerEpoch = SlotCount { unSlotCount = 10 }
                  , _upTtl = SlotCount { unSlotCount = 18 }
                  , _scriptVersion = 2
                  , _upAdptThd = 0.41270235937865996
                  , _factorA = 3
                  , _factorB = 4
                  }
              )
            , []
            , fromList
                [ ( ApName "" , ( ApVer 0 , Slot { unSlot = 147 } , Metadata ) )
                , ( ApName "3DK" , ( ApVer 2 , Slot { unSlot = 114 } , Metadata ) )
                , ( ApName "`" , ( ApVer 0 , Slot { unSlot = 120 } , Metadata ) )
                ]
            , fromList
                [ ( UpId 0
                  , ( ProtVer { _pvMaj = 3 , _pvMin = 0 , _pvAlt = 0 }
                    , PParams
                        { _maxBkSz = 159489
                        , _maxHdrSz = 1870
                        , _maxTxSz = 27880
                        , _maxPropSz = 73
                        , _bkSgnCntT = 2.0734217703083053
                        , _bkSlotsPerEpoch = SlotCount { unSlotCount = 10 }
                        , _upTtl = SlotCount { unSlotCount = 29 }
                        , _scriptVersion = 3
                        , _upAdptThd = 0.8509655148487065
                        , _factorA = 1
                        , _factorB = 4
                        }
                    )
                  )
                ]
            , fromList []
            , fromList []
            , fromList []
            , fromList []
            , fromList [ ( UpId 0 , Slot { unSlot = 186 } ) ]
            )
          )
        , Block
            { _bHeader =
                MkBlockHeader
                  { _bhPrevHash = Hash { unHash = 1755836368263885276 }
                  , _bhSlot = Slot { unSlot = 186 }
                  , _bhIssuer = VKey Owner { unOwner = 0 }
                  , _bhSig =
                      Sig Hash { unHash = -300149826283586134 } Owner { unOwner = 0 }
                  , _bhUtxoHash = Hash { unHash = 839657738087498284 }
                  , _bhDlgHash = Hash { unHash = 839657738087498284 }
                  , _bhUpdHash = Hash { unHash = 4675560675707589458 }
                  }
            , _bBody =
                BlockBody
                  { _bDCerts = []
                  , _bUtxo = []
                  , _bUpdProp =
                      Just
                        UProp
                          { _upId = UpId 0
                          , _upIssuer = VKey Owner { unOwner = 0 }
                          , _upParams =
                              PParams
                                { _maxBkSz = 159489
                                , _maxHdrSz = 1870
                                , _maxTxSz = 27880
                                , _maxPropSz = 73
                                , _bkSgnCntT = 2.0734217703083053
                                , _bkSlotsPerEpoch = SlotCount { unSlotCount = 10 }
                                , _upTtl = SlotCount { unSlotCount = 29 }
                                , _scriptVersion = 3
                                , _upAdptThd = 0.8509655148487065
                                , _factorA = 1
                                , _factorB = 4
                                }
                          , _upPV = ProtVer { _pvMaj = 3 , _pvMin = 0 , _pvAlt = 0 }
                          , _upSwVer = SwVer { _svName = ApName "3DK" , _svVer = ApVer 2 }
                          , _upSig =
                              Sig
                                ( ProtVer { _pvMaj = 3 , _pvMin = 0 , _pvAlt = 0 }
                                , PParams
                                    { _maxBkSz = 159489
                                    , _maxHdrSz = 1870
                                    , _maxTxSz = 27880
                                    , _maxPropSz = 73
                                    , _bkSgnCntT = 2.0734217703083053
                                    , _bkSlotsPerEpoch = SlotCount { unSlotCount = 10 }
                                    , _upTtl = SlotCount { unSlotCount = 29 }
                                    , _scriptVersion = 3
                                    , _upAdptThd = 0.8509655148487065
                                    , _factorA = 1
                                    , _factorB = 4
                                    }
                                , SwVer { _svName = ApName "3DK" , _svVer = ApVer 2 }
                                )
                                Owner { unOwner = 0 }
                          , _upSTags =
                              fromList
                                [ "\n\STX\FS\DLEE\"" , ",j\FS)" , "G'\SOHBP\FS}P\SOH" , "GH3" ]
                          , _upMdt = Metadata
                          }
                  , _bUpdVotes = []
                  , _bProtVer = ProtVer { _pvMaj = 2 , _pvMin = 0 , _pvAlt = 0 }
                  }
            }
        )
      , ( ( Slot { unSlot = 181 }
          , fromList
              [ VKeyGenesis { unVKeyGenesis = VKey Owner { unOwner = 0 } }
              , VKeyGenesis { unVKeyGenesis = VKey Owner { unOwner = 0 } }
              ]
          , Hash { unHash = 1755836368263885276 }
          , UTxOState
              { utxo =
                  UTxO
                    { unUTxO =
                        fromList
                          [ ( TxIn TxId { getTxId = Hash { unHash = 0 } } 0
                            , TxOut
                                { addr = Addr (VKey Owner { unOwner = 0 })
                                , value = Lovelace { unLovelace = 6733 }
                                }
                            )
                          , ( TxIn TxId { getTxId = Hash { unHash = 1 } } 0
                            , TxOut
                                { addr = Addr (VKey Owner { unOwner = 1 })
                                , value = Lovelace { unLovelace = 4795 }
                                }
                            )
                          , ( TxIn TxId { getTxId = Hash { unHash = 2 } } 0
                            , TxOut
                                { addr = Addr (VKey Owner { unOwner = 2 })
                                , value = Lovelace { unLovelace = 8020 }
                                }
                            )
                          , ( TxIn TxId { getTxId = Hash { unHash = 3 } } 0
                            , TxOut
                                { addr = Addr (VKey Owner { unOwner = 3 })
                                , value = Lovelace { unLovelace = 1631 }
                                }
                            )
                          , ( TxIn TxId { getTxId = Hash { unHash = 4 } } 0
                            , TxOut
                                { addr = Addr (VKey Owner { unOwner = 4 })
                                , value = Lovelace { unLovelace = 7829 }
                                }
                            )
                          , ( TxIn TxId { getTxId = Hash { unHash = 7 } } 0
                            , TxOut
                                { addr = Addr (VKey Owner { unOwner = 7 })
                                , value = Lovelace { unLovelace = 6863 }
                                }
                            )
                          , ( TxIn TxId { getTxId = Hash { unHash = 8 } } 0
                            , TxOut
                                { addr = Addr (VKey Owner { unOwner = 8 })
                                , value = Lovelace { unLovelace = 8623 }
                                }
                            )
                          , ( TxIn TxId { getTxId = Hash { unHash = 9 } } 0
                            , TxOut
                                { addr = Addr (VKey Owner { unOwner = 9 })
                                , value = Lovelace { unLovelace = 7230 }
                                }
                            )
                          ]
                    }
              , reserves = Lovelace { unLovelace = 44999999999948276 }
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
          , ( ( ProtVer { _pvMaj = 2 , _pvMin = 0 , _pvAlt = 0 }
              , PParams
                  { _maxBkSz = 110505
                  , _maxHdrSz = 1490
                  , _maxTxSz = 17458
                  , _maxPropSz = 52
                  , _bkSgnCntT = 2.0744323473896933
                  , _bkSlotsPerEpoch = SlotCount { unSlotCount = 10 }
                  , _upTtl = SlotCount { unSlotCount = 18 }
                  , _scriptVersion = 2
                  , _upAdptThd = 0.41270235937865996
                  , _factorA = 3
                  , _factorB = 4
                  }
              )
            , []
            , fromList
                [ ( ApName "" , ( ApVer 0 , Slot { unSlot = 147 } , Metadata ) )
                , ( ApName "3DK" , ( ApVer 2 , Slot { unSlot = 114 } , Metadata ) )
                , ( ApName "`" , ( ApVer 0 , Slot { unSlot = 120 } , Metadata ) )
                ]
            , fromList []
            , fromList []
            , fromList []
            , fromList []
            , fromList []
            , fromList []
            )
          )
        , Block
            { _bHeader =
                MkBlockHeader
                  { _bhPrevHash = Hash { unHash = -1832404817119284709 }
                  , _bhSlot = Slot { unSlot = 181 }
                  , _bhIssuer = VKey Owner { unOwner = 0 }
                  , _bhSig =
                      Sig Hash { unHash = 1755836368263885276 } Owner { unOwner = 0 }
                  , _bhUtxoHash = Hash { unHash = 839657738087498284 }
                  , _bhDlgHash = Hash { unHash = 839657738087498284 }
                  , _bhUpdHash = Hash { unHash = 0 }
                  }
            , _bBody =
                BlockBody
                  { _bDCerts = []
                  , _bUtxo = []
                  , _bUpdProp = Nothing
                  , _bUpdVotes = []
                  , _bProtVer = ProtVer { _pvMaj = 2 , _pvMin = 0 , _pvAlt = 0 }
                  }
            }
        )
      , ( ( Slot { unSlot = 177 }
          , fromList
              [ VKeyGenesis { unVKeyGenesis = VKey Owner { unOwner = 0 } }
              , VKeyGenesis { unVKeyGenesis = VKey Owner { unOwner = 0 } }
              ]
          , Hash { unHash = -1832404817119284709 }
          , UTxOState
              { utxo =
                  UTxO
                    { unUTxO =
                        fromList
                          [ ( TxIn TxId { getTxId = Hash { unHash = 0 } } 0
                            , TxOut
                                { addr = Addr (VKey Owner { unOwner = 0 })
                                , value = Lovelace { unLovelace = 6733 }
                                }
                            )
                          , ( TxIn TxId { getTxId = Hash { unHash = 1 } } 0
                            , TxOut
                                { addr = Addr (VKey Owner { unOwner = 1 })
                                , value = Lovelace { unLovelace = 4795 }
                                }
                            )
                          , ( TxIn TxId { getTxId = Hash { unHash = 2 } } 0
                            , TxOut
                                { addr = Addr (VKey Owner { unOwner = 2 })
                                , value = Lovelace { unLovelace = 8020 }
                                }
                            )
                          , ( TxIn TxId { getTxId = Hash { unHash = 3 } } 0
                            , TxOut
                                { addr = Addr (VKey Owner { unOwner = 3 })
                                , value = Lovelace { unLovelace = 1631 }
                                }
                            )
                          , ( TxIn TxId { getTxId = Hash { unHash = 4 } } 0
                            , TxOut
                                { addr = Addr (VKey Owner { unOwner = 4 })
                                , value = Lovelace { unLovelace = 7829 }
                                }
                            )
                          , ( TxIn TxId { getTxId = Hash { unHash = 7 } } 0
                            , TxOut
                                { addr = Addr (VKey Owner { unOwner = 7 })
                                , value = Lovelace { unLovelace = 6863 }
                                }
                            )
                          , ( TxIn TxId { getTxId = Hash { unHash = 8 } } 0
                            , TxOut
                                { addr = Addr (VKey Owner { unOwner = 8 })
                                , value = Lovelace { unLovelace = 8623 }
                                }
                            )
                          , ( TxIn TxId { getTxId = Hash { unHash = 9 } } 0
                            , TxOut
                                { addr = Addr (VKey Owner { unOwner = 9 })
                                , value = Lovelace { unLovelace = 7230 }
                                }
                            )
                          ]
                    }
              , reserves = Lovelace { unLovelace = 44999999999948276 }
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
          , ( ( ProtVer { _pvMaj = 2 , _pvMin = 0 , _pvAlt = 0 }
              , PParams
                  { _maxBkSz = 110505
                  , _maxHdrSz = 1490
                  , _maxTxSz = 17458
                  , _maxPropSz = 52
                  , _bkSgnCntT = 2.0744323473896933
                  , _bkSlotsPerEpoch = SlotCount { unSlotCount = 10 }
                  , _upTtl = SlotCount { unSlotCount = 18 }
                  , _scriptVersion = 2
                  , _upAdptThd = 0.41270235937865996
                  , _factorA = 3
                  , _factorB = 4
                  }
              )
            , []
            , fromList
                [ ( ApName "" , ( ApVer 0 , Slot { unSlot = 147 } , Metadata ) )
                , ( ApName "3DK" , ( ApVer 2 , Slot { unSlot = 114 } , Metadata ) )
                , ( ApName "`" , ( ApVer 0 , Slot { unSlot = 120 } , Metadata ) )
                ]
            , fromList
                [ ( UpId 0
                  , ( ProtVer { _pvMaj = 2 , _pvMin = 1 , _pvAlt = 0 }
                    , PParams
                        { _maxBkSz = 117984
                        , _maxHdrSz = 1703
                        , _maxTxSz = 18053
                        , _maxPropSz = 59
                        , _bkSgnCntT = 2.0715537879492922
                        , _bkSlotsPerEpoch = SlotCount { unSlotCount = 10 }
                        , _upTtl = SlotCount { unSlotCount = 20 }
                        , _scriptVersion = 2
                        , _upAdptThd = 0.6218538961839393
                        , _factorA = 8
                        , _factorB = 8
                        }
                    )
                  )
                ]
            , fromList []
            , fromList []
            , fromList []
            , fromList []
            , fromList [ ( UpId 0 , Slot { unSlot = 161 } ) ]
            )
          )
        , Block
            { _bHeader =
                MkBlockHeader
                  { _bhPrevHash = Hash { unHash = -5359143169281908104 }
                  , _bhSlot = Slot { unSlot = 177 }
                  , _bhIssuer = VKey Owner { unOwner = 0 }
                  , _bhSig =
                      Sig Hash { unHash = -1832404817119284709 } Owner { unOwner = 0 }
                  , _bhUtxoHash = Hash { unHash = 839657738087498284 }
                  , _bhDlgHash = Hash { unHash = 839657738087498284 }
                  , _bhUpdHash = Hash { unHash = 0 }
                  }
            , _bBody =
                BlockBody
                  { _bDCerts = []
                  , _bUtxo = []
                  , _bUpdProp = Nothing
                  , _bUpdVotes = []
                  , _bProtVer = ProtVer { _pvMaj = 2 , _pvMin = 0 , _pvAlt = 0 }
                  }
            }
        )
      , ( ( Slot { unSlot = 171 }
          , fromList
              [ VKeyGenesis { unVKeyGenesis = VKey Owner { unOwner = 0 } }
              , VKeyGenesis { unVKeyGenesis = VKey Owner { unOwner = 0 } }
              ]
          , Hash { unHash = -5359143169281908104 }
          , UTxOState
              { utxo =
                  UTxO
                    { unUTxO =
                        fromList
                          [ ( TxIn TxId { getTxId = Hash { unHash = 0 } } 0
                            , TxOut
                                { addr = Addr (VKey Owner { unOwner = 0 })
                                , value = Lovelace { unLovelace = 6733 }
                                }
                            )
                          , ( TxIn TxId { getTxId = Hash { unHash = 1 } } 0
                            , TxOut
                                { addr = Addr (VKey Owner { unOwner = 1 })
                                , value = Lovelace { unLovelace = 4795 }
                                }
                            )
                          , ( TxIn TxId { getTxId = Hash { unHash = 2 } } 0
                            , TxOut
                                { addr = Addr (VKey Owner { unOwner = 2 })
                                , value = Lovelace { unLovelace = 8020 }
                                }
                            )
                          , ( TxIn TxId { getTxId = Hash { unHash = 3 } } 0
                            , TxOut
                                { addr = Addr (VKey Owner { unOwner = 3 })
                                , value = Lovelace { unLovelace = 1631 }
                                }
                            )
                          , ( TxIn TxId { getTxId = Hash { unHash = 4 } } 0
                            , TxOut
                                { addr = Addr (VKey Owner { unOwner = 4 })
                                , value = Lovelace { unLovelace = 7829 }
                                }
                            )
                          , ( TxIn TxId { getTxId = Hash { unHash = 7 } } 0
                            , TxOut
                                { addr = Addr (VKey Owner { unOwner = 7 })
                                , value = Lovelace { unLovelace = 6863 }
                                }
                            )
                          , ( TxIn TxId { getTxId = Hash { unHash = 8 } } 0
                            , TxOut
                                { addr = Addr (VKey Owner { unOwner = 8 })
                                , value = Lovelace { unLovelace = 8623 }
                                }
                            )
                          , ( TxIn TxId { getTxId = Hash { unHash = 9 } } 0
                            , TxOut
                                { addr = Addr (VKey Owner { unOwner = 9 })
                                , value = Lovelace { unLovelace = 7230 }
                                }
                            )
                          ]
                    }
              , reserves = Lovelace { unLovelace = 44999999999948276 }
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
          , ( ( ProtVer { _pvMaj = 2 , _pvMin = 0 , _pvAlt = 0 }
              , PParams
                  { _maxBkSz = 110505
                  , _maxHdrSz = 1490
                  , _maxTxSz = 17458
                  , _maxPropSz = 52
                  , _bkSgnCntT = 2.0744323473896933
                  , _bkSlotsPerEpoch = SlotCount { unSlotCount = 10 }
                  , _upTtl = SlotCount { unSlotCount = 18 }
                  , _scriptVersion = 2
                  , _upAdptThd = 0.41270235937865996
                  , _factorA = 3
                  , _factorB = 4
                  }
              )
            , []
            , fromList
                [ ( ApName "" , ( ApVer 0 , Slot { unSlot = 147 } , Metadata ) )
                , ( ApName "3DK" , ( ApVer 2 , Slot { unSlot = 114 } , Metadata ) )
                , ( ApName "`" , ( ApVer 0 , Slot { unSlot = 120 } , Metadata ) )
                ]
            , fromList
                [ ( UpId 0
                  , ( ProtVer { _pvMaj = 2 , _pvMin = 1 , _pvAlt = 0 }
                    , PParams
                        { _maxBkSz = 117984
                        , _maxHdrSz = 1703
                        , _maxTxSz = 18053
                        , _maxPropSz = 59
                        , _bkSgnCntT = 2.0715537879492922
                        , _bkSlotsPerEpoch = SlotCount { unSlotCount = 10 }
                        , _upTtl = SlotCount { unSlotCount = 20 }
                        , _scriptVersion = 2
                        , _upAdptThd = 0.6218538961839393
                        , _factorA = 8
                        , _factorB = 8
                        }
                    )
                  )
                ]
            , fromList []
            , fromList []
            , fromList []
            , fromList []
            , fromList [ ( UpId 0 , Slot { unSlot = 161 } ) ]
            )
          )
        , Block
            { _bHeader =
                MkBlockHeader
                  { _bhPrevHash = Hash { unHash = 7807318021991913425 }
                  , _bhSlot = Slot { unSlot = 171 }
                  , _bhIssuer = VKey Owner { unOwner = 0 }
                  , _bhSig =
                      Sig Hash { unHash = -5359143169281908104 } Owner { unOwner = 0 }
                  , _bhUtxoHash = Hash { unHash = 839657738087498284 }
                  , _bhDlgHash = Hash { unHash = 839657738087498284 }
                  , _bhUpdHash = Hash { unHash = 0 }
                  }
            , _bBody =
                BlockBody
                  { _bDCerts = []
                  , _bUtxo = []
                  , _bUpdProp = Nothing
                  , _bUpdVotes = []
                  , _bProtVer = ProtVer { _pvMaj = 2 , _pvMin = 0 , _pvAlt = 0 }
                  }
            }
        )
      , ( ( Slot { unSlot = 170 }
          , fromList
              [ VKeyGenesis { unVKeyGenesis = VKey Owner { unOwner = 0 } }
              , VKeyGenesis { unVKeyGenesis = VKey Owner { unOwner = 0 } }
              ]
          , Hash { unHash = 7807318021991913425 }
          , UTxOState
              { utxo =
                  UTxO
                    { unUTxO =
                        fromList
                          [ ( TxIn TxId { getTxId = Hash { unHash = 0 } } 0
                            , TxOut
                                { addr = Addr (VKey Owner { unOwner = 0 })
                                , value = Lovelace { unLovelace = 6733 }
                                }
                            )
                          , ( TxIn TxId { getTxId = Hash { unHash = 1 } } 0
                            , TxOut
                                { addr = Addr (VKey Owner { unOwner = 1 })
                                , value = Lovelace { unLovelace = 4795 }
                                }
                            )
                          , ( TxIn TxId { getTxId = Hash { unHash = 2 } } 0
                            , TxOut
                                { addr = Addr (VKey Owner { unOwner = 2 })
                                , value = Lovelace { unLovelace = 8020 }
                                }
                            )
                          , ( TxIn TxId { getTxId = Hash { unHash = 3 } } 0
                            , TxOut
                                { addr = Addr (VKey Owner { unOwner = 3 })
                                , value = Lovelace { unLovelace = 1631 }
                                }
                            )
                          , ( TxIn TxId { getTxId = Hash { unHash = 4 } } 0
                            , TxOut
                                { addr = Addr (VKey Owner { unOwner = 4 })
                                , value = Lovelace { unLovelace = 7829 }
                                }
                            )
                          , ( TxIn TxId { getTxId = Hash { unHash = 7 } } 0
                            , TxOut
                                { addr = Addr (VKey Owner { unOwner = 7 })
                                , value = Lovelace { unLovelace = 6863 }
                                }
                            )
                          , ( TxIn TxId { getTxId = Hash { unHash = 8 } } 0
                            , TxOut
                                { addr = Addr (VKey Owner { unOwner = 8 })
                                , value = Lovelace { unLovelace = 8623 }
                                }
                            )
                          , ( TxIn TxId { getTxId = Hash { unHash = 9 } } 0
                            , TxOut
                                { addr = Addr (VKey Owner { unOwner = 9 })
                                , value = Lovelace { unLovelace = 7230 }
                                }
                            )
                          ]
                    }
              , reserves = Lovelace { unLovelace = 44999999999948276 }
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
          , ( ( ProtVer { _pvMaj = 2 , _pvMin = 0 , _pvAlt = 0 }
              , PParams
                  { _maxBkSz = 110505
                  , _maxHdrSz = 1490
                  , _maxTxSz = 17458
                  , _maxPropSz = 52
                  , _bkSgnCntT = 2.0744323473896933
                  , _bkSlotsPerEpoch = SlotCount { unSlotCount = 10 }
                  , _upTtl = SlotCount { unSlotCount = 18 }
                  , _scriptVersion = 2
                  , _upAdptThd = 0.41270235937865996
                  , _factorA = 3
                  , _factorB = 4
                  }
              )
            , []
            , fromList
                [ ( ApName "" , ( ApVer 0 , Slot { unSlot = 147 } , Metadata ) )
                , ( ApName "3DK" , ( ApVer 2 , Slot { unSlot = 114 } , Metadata ) )
                , ( ApName "`" , ( ApVer 0 , Slot { unSlot = 120 } , Metadata ) )
                ]
            , fromList
                [ ( UpId 0
                  , ( ProtVer { _pvMaj = 2 , _pvMin = 1 , _pvAlt = 0 }
                    , PParams
                        { _maxBkSz = 117984
                        , _maxHdrSz = 1703
                        , _maxTxSz = 18053
                        , _maxPropSz = 59
                        , _bkSgnCntT = 2.0715537879492922
                        , _bkSlotsPerEpoch = SlotCount { unSlotCount = 10 }
                        , _upTtl = SlotCount { unSlotCount = 20 }
                        , _scriptVersion = 2
                        , _upAdptThd = 0.6218538961839393
                        , _factorA = 8
                        , _factorB = 8
                        }
                    )
                  )
                ]
            , fromList []
            , fromList []
            , fromList []
            , fromList []
            , fromList [ ( UpId 0 , Slot { unSlot = 161 } ) ]
            )
          )
        , Block
            { _bHeader =
                MkBlockHeader
                  { _bhPrevHash = Hash { unHash = -3358099569741935365 }
                  , _bhSlot = Slot { unSlot = 170 }
                  , _bhIssuer = VKey Owner { unOwner = 0 }
                  , _bhSig =
                      Sig Hash { unHash = 7807318021991913425 } Owner { unOwner = 0 }
                  , _bhUtxoHash = Hash { unHash = 839657738087498284 }
                  , _bhDlgHash = Hash { unHash = 839657738087498284 }
                  , _bhUpdHash = Hash { unHash = 0 }
                  }
            , _bBody =
                BlockBody
                  { _bDCerts = []
                  , _bUtxo = []
                  , _bUpdProp = Nothing
                  , _bUpdVotes = []
                  , _bProtVer = ProtVer { _pvMaj = 2 , _pvMin = 0 , _pvAlt = 0 }
                  }
            }
        )
      , ( ( Slot { unSlot = 164 }
          , fromList
              [ VKeyGenesis { unVKeyGenesis = VKey Owner { unOwner = 0 } }
              , VKeyGenesis { unVKeyGenesis = VKey Owner { unOwner = 0 } }
              ]
          , Hash { unHash = -3358099569741935365 }
          , UTxOState
              { utxo =
                  UTxO
                    { unUTxO =
                        fromList
                          [ ( TxIn TxId { getTxId = Hash { unHash = 0 } } 0
                            , TxOut
                                { addr = Addr (VKey Owner { unOwner = 0 })
                                , value = Lovelace { unLovelace = 6733 }
                                }
                            )
                          , ( TxIn TxId { getTxId = Hash { unHash = 1 } } 0
                            , TxOut
                                { addr = Addr (VKey Owner { unOwner = 1 })
                                , value = Lovelace { unLovelace = 4795 }
                                }
                            )
                          , ( TxIn TxId { getTxId = Hash { unHash = 2 } } 0
                            , TxOut
                                { addr = Addr (VKey Owner { unOwner = 2 })
                                , value = Lovelace { unLovelace = 8020 }
                                }
                            )
                          , ( TxIn TxId { getTxId = Hash { unHash = 3 } } 0
                            , TxOut
                                { addr = Addr (VKey Owner { unOwner = 3 })
                                , value = Lovelace { unLovelace = 1631 }
                                }
                            )
                          , ( TxIn TxId { getTxId = Hash { unHash = 4 } } 0
                            , TxOut
                                { addr = Addr (VKey Owner { unOwner = 4 })
                                , value = Lovelace { unLovelace = 7829 }
                                }
                            )
                          , ( TxIn TxId { getTxId = Hash { unHash = 7 } } 0
                            , TxOut
                                { addr = Addr (VKey Owner { unOwner = 7 })
                                , value = Lovelace { unLovelace = 6863 }
                                }
                            )
                          , ( TxIn TxId { getTxId = Hash { unHash = 8 } } 0
                            , TxOut
                                { addr = Addr (VKey Owner { unOwner = 8 })
                                , value = Lovelace { unLovelace = 8623 }
                                }
                            )
                          , ( TxIn TxId { getTxId = Hash { unHash = 9 } } 0
                            , TxOut
                                { addr = Addr (VKey Owner { unOwner = 9 })
                                , value = Lovelace { unLovelace = 7230 }
                                }
                            )
                          ]
                    }
              , reserves = Lovelace { unLovelace = 44999999999948276 }
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
          , ( ( ProtVer { _pvMaj = 2 , _pvMin = 0 , _pvAlt = 0 }
              , PParams
                  { _maxBkSz = 110505
                  , _maxHdrSz = 1490
                  , _maxTxSz = 17458
                  , _maxPropSz = 52
                  , _bkSgnCntT = 2.0744323473896933
                  , _bkSlotsPerEpoch = SlotCount { unSlotCount = 10 }
                  , _upTtl = SlotCount { unSlotCount = 18 }
                  , _scriptVersion = 2
                  , _upAdptThd = 0.41270235937865996
                  , _factorA = 3
                  , _factorB = 4
                  }
              )
            , []
            , fromList
                [ ( ApName "" , ( ApVer 0 , Slot { unSlot = 147 } , Metadata ) )
                , ( ApName "3DK" , ( ApVer 2 , Slot { unSlot = 114 } , Metadata ) )
                , ( ApName "`" , ( ApVer 0 , Slot { unSlot = 120 } , Metadata ) )
                ]
            , fromList
                [ ( UpId 0
                  , ( ProtVer { _pvMaj = 2 , _pvMin = 1 , _pvAlt = 0 }
                    , PParams
                        { _maxBkSz = 117984
                        , _maxHdrSz = 1703
                        , _maxTxSz = 18053
                        , _maxPropSz = 59
                        , _bkSgnCntT = 2.0715537879492922
                        , _bkSlotsPerEpoch = SlotCount { unSlotCount = 10 }
                        , _upTtl = SlotCount { unSlotCount = 20 }
                        , _scriptVersion = 2
                        , _upAdptThd = 0.6218538961839393
                        , _factorA = 8
                        , _factorB = 8
                        }
                    )
                  )
                ]
            , fromList []
            , fromList []
            , fromList []
            , fromList []
            , fromList [ ( UpId 0 , Slot { unSlot = 161 } ) ]
            )
          )
        , Block
            { _bHeader =
                MkBlockHeader
                  { _bhPrevHash = Hash { unHash = 1831587336708319567 }
                  , _bhSlot = Slot { unSlot = 164 }
                  , _bhIssuer = VKey Owner { unOwner = 0 }
                  , _bhSig =
                      Sig Hash { unHash = -3358099569741935365 } Owner { unOwner = 0 }
                  , _bhUtxoHash = Hash { unHash = 839657738087498284 }
                  , _bhDlgHash = Hash { unHash = 839657738087498284 }
                  , _bhUpdHash = Hash { unHash = 0 }
                  }
            , _bBody =
                BlockBody
                  { _bDCerts = []
                  , _bUtxo = []
                  , _bUpdProp = Nothing
                  , _bUpdVotes = []
                  , _bProtVer = ProtVer { _pvMaj = 2 , _pvMin = 0 , _pvAlt = 0 }
                  }
            }
        )
      , ( ( Slot { unSlot = 161 }
          , fromList
              [ VKeyGenesis { unVKeyGenesis = VKey Owner { unOwner = 0 } }
              , VKeyGenesis { unVKeyGenesis = VKey Owner { unOwner = 0 } }
              ]
          , Hash { unHash = 1831587336708319567 }
          , UTxOState
              { utxo =
                  UTxO
                    { unUTxO =
                        fromList
                          [ ( TxIn TxId { getTxId = Hash { unHash = 0 } } 0
                            , TxOut
                                { addr = Addr (VKey Owner { unOwner = 0 })
                                , value = Lovelace { unLovelace = 6733 }
                                }
                            )
                          , ( TxIn TxId { getTxId = Hash { unHash = 1 } } 0
                            , TxOut
                                { addr = Addr (VKey Owner { unOwner = 1 })
                                , value = Lovelace { unLovelace = 4795 }
                                }
                            )
                          , ( TxIn TxId { getTxId = Hash { unHash = 2 } } 0
                            , TxOut
                                { addr = Addr (VKey Owner { unOwner = 2 })
                                , value = Lovelace { unLovelace = 8020 }
                                }
                            )
                          , ( TxIn TxId { getTxId = Hash { unHash = 3 } } 0
                            , TxOut
                                { addr = Addr (VKey Owner { unOwner = 3 })
                                , value = Lovelace { unLovelace = 1631 }
                                }
                            )
                          , ( TxIn TxId { getTxId = Hash { unHash = 4 } } 0
                            , TxOut
                                { addr = Addr (VKey Owner { unOwner = 4 })
                                , value = Lovelace { unLovelace = 7829 }
                                }
                            )
                          , ( TxIn TxId { getTxId = Hash { unHash = 7 } } 0
                            , TxOut
                                { addr = Addr (VKey Owner { unOwner = 7 })
                                , value = Lovelace { unLovelace = 6863 }
                                }
                            )
                          , ( TxIn TxId { getTxId = Hash { unHash = 8 } } 0
                            , TxOut
                                { addr = Addr (VKey Owner { unOwner = 8 })
                                , value = Lovelace { unLovelace = 8623 }
                                }
                            )
                          , ( TxIn TxId { getTxId = Hash { unHash = 9 } } 0
                            , TxOut
                                { addr = Addr (VKey Owner { unOwner = 9 })
                                , value = Lovelace { unLovelace = 7230 }
                                }
                            )
                          ]
                    }
              , reserves = Lovelace { unLovelace = 44999999999948276 }
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
          , ( ( ProtVer { _pvMaj = 2 , _pvMin = 0 , _pvAlt = 0 }
              , PParams
                  { _maxBkSz = 110505
                  , _maxHdrSz = 1490
                  , _maxTxSz = 17458
                  , _maxPropSz = 52
                  , _bkSgnCntT = 2.0744323473896933
                  , _bkSlotsPerEpoch = SlotCount { unSlotCount = 10 }
                  , _upTtl = SlotCount { unSlotCount = 18 }
                  , _scriptVersion = 2
                  , _upAdptThd = 0.41270235937865996
                  , _factorA = 3
                  , _factorB = 4
                  }
              )
            , []
            , fromList
                [ ( ApName "" , ( ApVer 0 , Slot { unSlot = 147 } , Metadata ) )
                , ( ApName "3DK" , ( ApVer 2 , Slot { unSlot = 114 } , Metadata ) )
                , ( ApName "`" , ( ApVer 0 , Slot { unSlot = 120 } , Metadata ) )
                ]
            , fromList
                [ ( UpId 0
                  , ( ProtVer { _pvMaj = 2 , _pvMin = 1 , _pvAlt = 0 }
                    , PParams
                        { _maxBkSz = 117984
                        , _maxHdrSz = 1703
                        , _maxTxSz = 18053
                        , _maxPropSz = 59
                        , _bkSgnCntT = 2.0715537879492922
                        , _bkSlotsPerEpoch = SlotCount { unSlotCount = 10 }
                        , _upTtl = SlotCount { unSlotCount = 20 }
                        , _scriptVersion = 2
                        , _upAdptThd = 0.6218538961839393
                        , _factorA = 8
                        , _factorB = 8
                        }
                    )
                  )
                ]
            , fromList []
            , fromList []
            , fromList []
            , fromList []
            , fromList [ ( UpId 0 , Slot { unSlot = 161 } ) ]
            )
          )
        , Block
            { _bHeader =
                MkBlockHeader
                  { _bhPrevHash = Hash { unHash = 6997989119837610044 }
                  , _bhSlot = Slot { unSlot = 161 }
                  , _bhIssuer = VKey Owner { unOwner = 0 }
                  , _bhSig =
                      Sig Hash { unHash = 1831587336708319567 } Owner { unOwner = 0 }
                  , _bhUtxoHash = Hash { unHash = 839657738087498284 }
                  , _bhDlgHash = Hash { unHash = 839657738087498284 }
                  , _bhUpdHash = Hash { unHash = 8563841077916744981 }
                  }
            , _bBody =
                BlockBody
                  { _bDCerts = []
                  , _bUtxo = []
                  , _bUpdProp =
                      Just
                        UProp
                          { _upId = UpId 0
                          , _upIssuer = VKey Owner { unOwner = 0 }
                          , _upParams =
                              PParams
                                { _maxBkSz = 117984
                                , _maxHdrSz = 1703
                                , _maxTxSz = 18053
                                , _maxPropSz = 59
                                , _bkSgnCntT = 2.0715537879492922
                                , _bkSlotsPerEpoch = SlotCount { unSlotCount = 10 }
                                , _upTtl = SlotCount { unSlotCount = 20 }
                                , _scriptVersion = 2
                                , _upAdptThd = 0.6218538961839393
                                , _factorA = 8
                                , _factorB = 8
                                }
                          , _upPV = ProtVer { _pvMaj = 2 , _pvMin = 1 , _pvAlt = 0 }
                          , _upSwVer = SwVer { _svName = ApName "3DK" , _svVer = ApVer 2 }
                          , _upSig =
                              Sig
                                ( ProtVer { _pvMaj = 2 , _pvMin = 1 , _pvAlt = 0 }
                                , PParams
                                    { _maxBkSz = 117984
                                    , _maxHdrSz = 1703
                                    , _maxTxSz = 18053
                                    , _maxPropSz = 59
                                    , _bkSgnCntT = 2.0715537879492922
                                    , _bkSlotsPerEpoch = SlotCount { unSlotCount = 10 }
                                    , _upTtl = SlotCount { unSlotCount = 20 }
                                    , _scriptVersion = 2
                                    , _upAdptThd = 0.6218538961839393
                                    , _factorA = 8
                                    , _factorB = 8
                                    }
                                , SwVer { _svName = ApName "3DK" , _svVer = ApVer 2 }
                                )
                                Owner { unOwner = 0 }
                          , _upSTags = fromList [ ">" , "JC%\SOl?" ]
                          , _upMdt = Metadata
                          }
                  , _bUpdVotes = []
                  , _bProtVer = ProtVer { _pvMaj = 2 , _pvMin = 0 , _pvAlt = 0 }
                  }
            }
        )
      , ( ( Slot { unSlot = 154 }
          , fromList
              [ VKeyGenesis { unVKeyGenesis = VKey Owner { unOwner = 0 } }
              , VKeyGenesis { unVKeyGenesis = VKey Owner { unOwner = 0 } }
              ]
          , Hash { unHash = 6997989119837610044 }
          , UTxOState
              { utxo =
                  UTxO
                    { unUTxO =
                        fromList
                          [ ( TxIn TxId { getTxId = Hash { unHash = 0 } } 0
                            , TxOut
                                { addr = Addr (VKey Owner { unOwner = 0 })
                                , value = Lovelace { unLovelace = 6733 }
                                }
                            )
                          , ( TxIn TxId { getTxId = Hash { unHash = 1 } } 0
                            , TxOut
                                { addr = Addr (VKey Owner { unOwner = 1 })
                                , value = Lovelace { unLovelace = 4795 }
                                }
                            )
                          , ( TxIn TxId { getTxId = Hash { unHash = 2 } } 0
                            , TxOut
                                { addr = Addr (VKey Owner { unOwner = 2 })
                                , value = Lovelace { unLovelace = 8020 }
                                }
                            )
                          , ( TxIn TxId { getTxId = Hash { unHash = 3 } } 0
                            , TxOut
                                { addr = Addr (VKey Owner { unOwner = 3 })
                                , value = Lovelace { unLovelace = 1631 }
                                }
                            )
                          , ( TxIn TxId { getTxId = Hash { unHash = 4 } } 0
                            , TxOut
                                { addr = Addr (VKey Owner { unOwner = 4 })
                                , value = Lovelace { unLovelace = 7829 }
                                }
                            )
                          , ( TxIn TxId { getTxId = Hash { unHash = 7 } } 0
                            , TxOut
                                { addr = Addr (VKey Owner { unOwner = 7 })
                                , value = Lovelace { unLovelace = 6863 }
                                }
                            )
                          , ( TxIn TxId { getTxId = Hash { unHash = 8 } } 0
                            , TxOut
                                { addr = Addr (VKey Owner { unOwner = 8 })
                                , value = Lovelace { unLovelace = 8623 }
                                }
                            )
                          , ( TxIn TxId { getTxId = Hash { unHash = 9 } } 0
                            , TxOut
                                { addr = Addr (VKey Owner { unOwner = 9 })
                                , value = Lovelace { unLovelace = 7230 }
                                }
                            )
                          ]
                    }
              , reserves = Lovelace { unLovelace = 44999999999948276 }
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
          , ( ( ProtVer { _pvMaj = 1 , _pvMin = 2 , _pvAlt = 1 }
              , PParams
                  { _maxBkSz = 69516
                  , _maxHdrSz = 1448
                  , _maxTxSz = 17303
                  , _maxPropSz = 41
                  , _bkSgnCntT = 2.0797865763684675
                  , _bkSlotsPerEpoch = SlotCount { unSlotCount = 10 }
                  , _upTtl = SlotCount { unSlotCount = 13 }
                  , _scriptVersion = 1
                  , _upAdptThd = 0.4431824320461192
                  , _factorA = 0
                  , _factorB = 2
                  }
              )
            , [ ( Slot { unSlot = 154 }
                , ( ProtVer { _pvMaj = 2 , _pvMin = 0 , _pvAlt = 0 }
                  , PParams
                      { _maxBkSz = 110505
                      , _maxHdrSz = 1490
                      , _maxTxSz = 17458
                      , _maxPropSz = 52
                      , _bkSgnCntT = 2.0744323473896933
                      , _bkSlotsPerEpoch = SlotCount { unSlotCount = 10 }
                      , _upTtl = SlotCount { unSlotCount = 18 }
                      , _scriptVersion = 2
                      , _upAdptThd = 0.41270235937865996
                      , _factorA = 3
                      , _factorB = 4
                      }
                  )
                )
              ]
            , fromList
                [ ( ApName "" , ( ApVer 0 , Slot { unSlot = 147 } , Metadata ) )
                , ( ApName "3DK" , ( ApVer 2 , Slot { unSlot = 114 } , Metadata ) )
                , ( ApName "`" , ( ApVer 0 , Slot { unSlot = 120 } , Metadata ) )
                ]
            , fromList
                [ ( UpId 1
                  , ( ProtVer { _pvMaj = 2 , _pvMin = 0 , _pvAlt = 0 }
                    , PParams
                        { _maxBkSz = 110505
                        , _maxHdrSz = 1490
                        , _maxTxSz = 17458
                        , _maxPropSz = 52
                        , _bkSgnCntT = 2.0744323473896933
                        , _bkSlotsPerEpoch = SlotCount { unSlotCount = 10 }
                        , _upTtl = SlotCount { unSlotCount = 18 }
                        , _scriptVersion = 2
                        , _upAdptThd = 0.41270235937865996
                        , _factorA = 3
                        , _factorB = 4
                        }
                    )
                  )
                ]
            , fromList []
            , fromList [ ( UpId 1 , Slot { unSlot = 142 } ) ]
            , fromList
                [ ( UpId 1
                  , VKeyGenesis { unVKeyGenesis = VKey Owner { unOwner = 0 } }
                  )
                ]
            , fromList
                [ ( ProtVer { _pvMaj = 2 , _pvMin = 0 , _pvAlt = 0 }
                  , VKeyGenesis { unVKeyGenesis = VKey Owner { unOwner = 0 } }
                  )
                ]
            , fromList [ ( UpId 1 , Slot { unSlot = 142 } ) ]
            )
          )
        , Block
            { _bHeader =
                MkBlockHeader
                  { _bhPrevHash = Hash { unHash = 3197051540362653882 }
                  , _bhSlot = Slot { unSlot = 154 }
                  , _bhIssuer = VKey Owner { unOwner = 0 }
                  , _bhSig =
                      Sig Hash { unHash = 6997989119837610044 } Owner { unOwner = 0 }
                  , _bhUtxoHash = Hash { unHash = 839657738087498284 }
                  , _bhDlgHash = Hash { unHash = 839657738087498284 }
                  , _bhUpdHash = Hash { unHash = 0 }
                  }
            , _bBody =
                BlockBody
                  { _bDCerts = []
                  , _bUtxo = []
                  , _bUpdProp = Nothing
                  , _bUpdVotes = []
                  , _bProtVer = ProtVer { _pvMaj = 2 , _pvMin = 0 , _pvAlt = 0 }
                  }
            }
        )
      , ( ( Slot { unSlot = 147 }
          , fromList
              [ VKeyGenesis { unVKeyGenesis = VKey Owner { unOwner = 0 } }
              , VKeyGenesis { unVKeyGenesis = VKey Owner { unOwner = 0 } }
              ]
          , Hash { unHash = 3197051540362653882 }
          , UTxOState
              { utxo =
                  UTxO
                    { unUTxO =
                        fromList
                          [ ( TxIn TxId { getTxId = Hash { unHash = 0 } } 0
                            , TxOut
                                { addr = Addr (VKey Owner { unOwner = 0 })
                                , value = Lovelace { unLovelace = 6733 }
                                }
                            )
                          , ( TxIn TxId { getTxId = Hash { unHash = 1 } } 0
                            , TxOut
                                { addr = Addr (VKey Owner { unOwner = 1 })
                                , value = Lovelace { unLovelace = 4795 }
                                }
                            )
                          , ( TxIn TxId { getTxId = Hash { unHash = 2 } } 0
                            , TxOut
                                { addr = Addr (VKey Owner { unOwner = 2 })
                                , value = Lovelace { unLovelace = 8020 }
                                }
                            )
                          , ( TxIn TxId { getTxId = Hash { unHash = 3 } } 0
                            , TxOut
                                { addr = Addr (VKey Owner { unOwner = 3 })
                                , value = Lovelace { unLovelace = 1631 }
                                }
                            )
                          , ( TxIn TxId { getTxId = Hash { unHash = 4 } } 0
                            , TxOut
                                { addr = Addr (VKey Owner { unOwner = 4 })
                                , value = Lovelace { unLovelace = 7829 }
                                }
                            )
                          , ( TxIn TxId { getTxId = Hash { unHash = 7 } } 0
                            , TxOut
                                { addr = Addr (VKey Owner { unOwner = 7 })
                                , value = Lovelace { unLovelace = 6863 }
                                }
                            )
                          , ( TxIn TxId { getTxId = Hash { unHash = 8 } } 0
                            , TxOut
                                { addr = Addr (VKey Owner { unOwner = 8 })
                                , value = Lovelace { unLovelace = 8623 }
                                }
                            )
                          , ( TxIn TxId { getTxId = Hash { unHash = 9 } } 0
                            , TxOut
                                { addr = Addr (VKey Owner { unOwner = 9 })
                                , value = Lovelace { unLovelace = 7230 }
                                }
                            )
                          ]
                    }
              , reserves = Lovelace { unLovelace = 44999999999948276 }
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
          , ( ( ProtVer { _pvMaj = 1 , _pvMin = 2 , _pvAlt = 1 }
              , PParams
                  { _maxBkSz = 69516
                  , _maxHdrSz = 1448
                  , _maxTxSz = 17303
                  , _maxPropSz = 41
                  , _bkSgnCntT = 2.0797865763684675
                  , _bkSlotsPerEpoch = SlotCount { unSlotCount = 10 }
                  , _upTtl = SlotCount { unSlotCount = 13 }
                  , _scriptVersion = 1
                  , _upAdptThd = 0.4431824320461192
                  , _factorA = 0
                  , _factorB = 2
                  }
              )
            , []
            , fromList
                [ ( ApName "" , ( ApVer 0 , Slot { unSlot = 147 } , Metadata ) )
                , ( ApName "3DK" , ( ApVer 2 , Slot { unSlot = 114 } , Metadata ) )
                , ( ApName "`" , ( ApVer 0 , Slot { unSlot = 120 } , Metadata ) )
                ]
            , fromList
                [ ( UpId 1
                  , ( ProtVer { _pvMaj = 2 , _pvMin = 0 , _pvAlt = 0 }
                    , PParams
                        { _maxBkSz = 110505
                        , _maxHdrSz = 1490
                        , _maxTxSz = 17458
                        , _maxPropSz = 52
                        , _bkSgnCntT = 2.0744323473896933
                        , _bkSlotsPerEpoch = SlotCount { unSlotCount = 10 }
                        , _upTtl = SlotCount { unSlotCount = 18 }
                        , _scriptVersion = 2
                        , _upAdptThd = 0.41270235937865996
                        , _factorA = 3
                        , _factorB = 4
                        }
                    )
                  )
                ]
            , fromList []
            , fromList [ ( UpId 1 , Slot { unSlot = 142 } ) ]
            , fromList
                [ ( UpId 1
                  , VKeyGenesis { unVKeyGenesis = VKey Owner { unOwner = 0 } }
                  )
                ]
            , fromList []
            , fromList [ ( UpId 1 , Slot { unSlot = 142 } ) ]
            )
          )
        , Block
            { _bHeader =
                MkBlockHeader
                  { _bhPrevHash = Hash { unHash = 3835091342536618975 }
                  , _bhSlot = Slot { unSlot = 147 }
                  , _bhIssuer = VKey Owner { unOwner = 0 }
                  , _bhSig =
                      Sig Hash { unHash = 3197051540362653882 } Owner { unOwner = 0 }
                  , _bhUtxoHash = Hash { unHash = 839657738087498284 }
                  , _bhDlgHash = Hash { unHash = 839657738087498284 }
                  , _bhUpdHash = Hash { unHash = 0 }
                  }
            , _bBody =
                BlockBody
                  { _bDCerts = []
                  , _bUtxo = []
                  , _bUpdProp = Nothing
                  , _bUpdVotes = []
                  , _bProtVer = ProtVer { _pvMaj = 1 , _pvMin = 2 , _pvAlt = 1 }
                  }
            }
        )
      , ( ( Slot { unSlot = 142 }
          , fromList
              [ VKeyGenesis { unVKeyGenesis = VKey Owner { unOwner = 0 } }
              , VKeyGenesis { unVKeyGenesis = VKey Owner { unOwner = 0 } }
              ]
          , Hash { unHash = 3835091342536618975 }
          , UTxOState
              { utxo =
                  UTxO
                    { unUTxO =
                        fromList
                          [ ( TxIn TxId { getTxId = Hash { unHash = 0 } } 0
                            , TxOut
                                { addr = Addr (VKey Owner { unOwner = 0 })
                                , value = Lovelace { unLovelace = 6733 }
                                }
                            )
                          , ( TxIn TxId { getTxId = Hash { unHash = 1 } } 0
                            , TxOut
                                { addr = Addr (VKey Owner { unOwner = 1 })
                                , value = Lovelace { unLovelace = 4795 }
                                }
                            )
                          , ( TxIn TxId { getTxId = Hash { unHash = 2 } } 0
                            , TxOut
                                { addr = Addr (VKey Owner { unOwner = 2 })
                                , value = Lovelace { unLovelace = 8020 }
                                }
                            )
                          , ( TxIn TxId { getTxId = Hash { unHash = 3 } } 0
                            , TxOut
                                { addr = Addr (VKey Owner { unOwner = 3 })
                                , value = Lovelace { unLovelace = 1631 }
                                }
                            )
                          , ( TxIn TxId { getTxId = Hash { unHash = 4 } } 0
                            , TxOut
                                { addr = Addr (VKey Owner { unOwner = 4 })
                                , value = Lovelace { unLovelace = 7829 }
                                }
                            )
                          , ( TxIn TxId { getTxId = Hash { unHash = 7 } } 0
                            , TxOut
                                { addr = Addr (VKey Owner { unOwner = 7 })
                                , value = Lovelace { unLovelace = 6863 }
                                }
                            )
                          , ( TxIn TxId { getTxId = Hash { unHash = 8 } } 0
                            , TxOut
                                { addr = Addr (VKey Owner { unOwner = 8 })
                                , value = Lovelace { unLovelace = 8623 }
                                }
                            )
                          , ( TxIn TxId { getTxId = Hash { unHash = 9 } } 0
                            , TxOut
                                { addr = Addr (VKey Owner { unOwner = 9 })
                                , value = Lovelace { unLovelace = 7230 }
                                }
                            )
                          ]
                    }
              , reserves = Lovelace { unLovelace = 44999999999948276 }
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
          , ( ( ProtVer { _pvMaj = 1 , _pvMin = 2 , _pvAlt = 1 }
              , PParams
                  { _maxBkSz = 69516
                  , _maxHdrSz = 1448
                  , _maxTxSz = 17303
                  , _maxPropSz = 41
                  , _bkSgnCntT = 2.0797865763684675
                  , _bkSlotsPerEpoch = SlotCount { unSlotCount = 10 }
                  , _upTtl = SlotCount { unSlotCount = 13 }
                  , _scriptVersion = 1
                  , _upAdptThd = 0.4431824320461192
                  , _factorA = 0
                  , _factorB = 2
                  }
              )
            , []
            , fromList
                [ ( ApName "3DK" , ( ApVer 2 , Slot { unSlot = 114 } , Metadata ) )
                , ( ApName "`" , ( ApVer 0 , Slot { unSlot = 120 } , Metadata ) )
                ]
            , fromList
                [ ( UpId 1
                  , ( ProtVer { _pvMaj = 2 , _pvMin = 0 , _pvAlt = 0 }
                    , PParams
                        { _maxBkSz = 110505
                        , _maxHdrSz = 1490
                        , _maxTxSz = 17458
                        , _maxPropSz = 52
                        , _bkSgnCntT = 2.0744323473896933
                        , _bkSlotsPerEpoch = SlotCount { unSlotCount = 10 }
                        , _upTtl = SlotCount { unSlotCount = 18 }
                        , _scriptVersion = 2
                        , _upAdptThd = 0.41270235937865996
                        , _factorA = 3
                        , _factorB = 4
                        }
                    )
                  )
                ]
            , fromList [ ( UpId 1 , ( ApName "" , ApVer 0 , Metadata ) ) ]
            , fromList [ ( UpId 1 , Slot { unSlot = 142 } ) ]
            , fromList
                [ ( UpId 1
                  , VKeyGenesis { unVKeyGenesis = VKey Owner { unOwner = 0 } }
                  )
                ]
            , fromList []
            , fromList [ ( UpId 1 , Slot { unSlot = 142 } ) ]
            )
          )
        , Block
            { _bHeader =
                MkBlockHeader
                  { _bhPrevHash = Hash { unHash = -7422713345559720919 }
                  , _bhSlot = Slot { unSlot = 142 }
                  , _bhIssuer = VKey Owner { unOwner = 0 }
                  , _bhSig =
                      Sig Hash { unHash = 3835091342536618975 } Owner { unOwner = 0 }
                  , _bhUtxoHash = Hash { unHash = 839657738087498284 }
                  , _bhDlgHash = Hash { unHash = 839657738087498284 }
                  , _bhUpdHash = Hash { unHash = -1887257797319275310 }
                  }
            , _bBody =
                BlockBody
                  { _bDCerts = []
                  , _bUtxo = []
                  , _bUpdProp =
                      Just
                        UProp
                          { _upId = UpId 1
                          , _upIssuer = VKey Owner { unOwner = 0 }
                          , _upParams =
                              PParams
                                { _maxBkSz = 110505
                                , _maxHdrSz = 1490
                                , _maxTxSz = 17458
                                , _maxPropSz = 52
                                , _bkSgnCntT = 2.0744323473896933
                                , _bkSlotsPerEpoch = SlotCount { unSlotCount = 10 }
                                , _upTtl = SlotCount { unSlotCount = 18 }
                                , _scriptVersion = 2
                                , _upAdptThd = 0.41270235937865996
                                , _factorA = 3
                                , _factorB = 4
                                }
                          , _upPV = ProtVer { _pvMaj = 2 , _pvMin = 0 , _pvAlt = 0 }
                          , _upSwVer = SwVer { _svName = ApName "" , _svVer = ApVer 0 }
                          , _upSig =
                              Sig
                                ( ProtVer { _pvMaj = 2 , _pvMin = 0 , _pvAlt = 0 }
                                , PParams
                                    { _maxBkSz = 110505
                                    , _maxHdrSz = 1490
                                    , _maxTxSz = 17458
                                    , _maxPropSz = 52
                                    , _bkSgnCntT = 2.0744323473896933
                                    , _bkSlotsPerEpoch = SlotCount { unSlotCount = 10 }
                                    , _upTtl = SlotCount { unSlotCount = 18 }
                                    , _scriptVersion = 2
                                    , _upAdptThd = 0.41270235937865996
                                    , _factorA = 3
                                    , _factorB = 4
                                    }
                                , SwVer { _svName = ApName "" , _svVer = ApVer 0 }
                                )
                                Owner { unOwner = 0 }
                          , _upSTags =
                              fromList
                                [ "\SOHQ\DLEA\r\\7~n4" , "\ESCXiwq-0\STX" , "Y\FS" , "vt?\ESCK" ]
                          , _upMdt = Metadata
                          }
                  , _bUpdVotes =
                      [ Vote
                          { _vCaster = VKey Owner { unOwner = 0 }
                          , _vPropId = UpId 1
                          , _vSig = Sig (UpId 1) Owner { unOwner = 0 }
                          }
                      ]
                  , _bProtVer = ProtVer { _pvMaj = 1 , _pvMin = 2 , _pvAlt = 1 }
                  }
            }
        )
      , ( ( Slot { unSlot = 136 }
          , fromList
              [ VKeyGenesis { unVKeyGenesis = VKey Owner { unOwner = 0 } }
              , VKeyGenesis { unVKeyGenesis = VKey Owner { unOwner = 0 } }
              ]
          , Hash { unHash = -7422713345559720919 }
          , UTxOState
              { utxo =
                  UTxO
                    { unUTxO =
                        fromList
                          [ ( TxIn TxId { getTxId = Hash { unHash = 0 } } 0
                            , TxOut
                                { addr = Addr (VKey Owner { unOwner = 0 })
                                , value = Lovelace { unLovelace = 6733 }
                                }
                            )
                          , ( TxIn TxId { getTxId = Hash { unHash = 1 } } 0
                            , TxOut
                                { addr = Addr (VKey Owner { unOwner = 1 })
                                , value = Lovelace { unLovelace = 4795 }
                                }
                            )
                          , ( TxIn TxId { getTxId = Hash { unHash = 2 } } 0
                            , TxOut
                                { addr = Addr (VKey Owner { unOwner = 2 })
                                , value = Lovelace { unLovelace = 8020 }
                                }
                            )
                          , ( TxIn TxId { getTxId = Hash { unHash = 3 } } 0
                            , TxOut
                                { addr = Addr (VKey Owner { unOwner = 3 })
                                , value = Lovelace { unLovelace = 1631 }
                                }
                            )
                          , ( TxIn TxId { getTxId = Hash { unHash = 4 } } 0
                            , TxOut
                                { addr = Addr (VKey Owner { unOwner = 4 })
                                , value = Lovelace { unLovelace = 7829 }
                                }
                            )
                          , ( TxIn TxId { getTxId = Hash { unHash = 7 } } 0
                            , TxOut
                                { addr = Addr (VKey Owner { unOwner = 7 })
                                , value = Lovelace { unLovelace = 6863 }
                                }
                            )
                          , ( TxIn TxId { getTxId = Hash { unHash = 8 } } 0
                            , TxOut
                                { addr = Addr (VKey Owner { unOwner = 8 })
                                , value = Lovelace { unLovelace = 8623 }
                                }
                            )
                          , ( TxIn TxId { getTxId = Hash { unHash = 9 } } 0
                            , TxOut
                                { addr = Addr (VKey Owner { unOwner = 9 })
                                , value = Lovelace { unLovelace = 7230 }
                                }
                            )
                          ]
                    }
              , reserves = Lovelace { unLovelace = 44999999999948276 }
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
          , ( ( ProtVer { _pvMaj = 1 , _pvMin = 1 , _pvAlt = 0 }
              , PParams
                  { _maxBkSz = 43200
                  , _maxHdrSz = 1453
                  , _maxTxSz = 8874
                  , _maxPropSz = 41
                  , _bkSgnCntT = 2.07902716432624
                  , _bkSlotsPerEpoch = SlotCount { unSlotCount = 10 }
                  , _upTtl = SlotCount { unSlotCount = 20 }
                  , _scriptVersion = 1
                  , _upAdptThd = 0.5500429331445129
                  , _factorA = 3
                  , _factorB = 6
                  }
              )
            , [ ( Slot { unSlot = 136 }
                , ( ProtVer { _pvMaj = 1 , _pvMin = 2 , _pvAlt = 1 }
                  , PParams
                      { _maxBkSz = 69516
                      , _maxHdrSz = 1448
                      , _maxTxSz = 17303
                      , _maxPropSz = 41
                      , _bkSgnCntT = 2.0797865763684675
                      , _bkSlotsPerEpoch = SlotCount { unSlotCount = 10 }
                      , _upTtl = SlotCount { unSlotCount = 13 }
                      , _scriptVersion = 1
                      , _upAdptThd = 0.4431824320461192
                      , _factorA = 0
                      , _factorB = 2
                      }
                  )
                )
              , ( Slot { unSlot = 120 }
                , ( ProtVer { _pvMaj = 1 , _pvMin = 2 , _pvAlt = 0 }
                  , PParams
                      { _maxBkSz = 55127
                      , _maxHdrSz = 1968
                      , _maxTxSz = 10677
                      , _maxPropSz = 56
                      , _bkSgnCntT = 2.071832156909479
                      , _bkSlotsPerEpoch = SlotCount { unSlotCount = 10 }
                      , _upTtl = SlotCount { unSlotCount = 26 }
                      , _scriptVersion = 1
                      , _upAdptThd = 0.5135004796204581
                      , _factorA = 3
                      , _factorB = 5
                      }
                  )
                )
              ]
            , fromList
                [ ( ApName "3DK" , ( ApVer 2 , Slot { unSlot = 114 } , Metadata ) )
                , ( ApName "`" , ( ApVer 0 , Slot { unSlot = 120 } , Metadata ) )
                ]
            , fromList
                [ ( UpId 0
                  , ( ProtVer { _pvMaj = 1 , _pvMin = 2 , _pvAlt = 0 }
                    , PParams
                        { _maxBkSz = 55127
                        , _maxHdrSz = 1968
                        , _maxTxSz = 10677
                        , _maxPropSz = 56
                        , _bkSgnCntT = 2.071832156909479
                        , _bkSlotsPerEpoch = SlotCount { unSlotCount = 10 }
                        , _upTtl = SlotCount { unSlotCount = 26 }
                        , _scriptVersion = 1
                        , _upAdptThd = 0.5135004796204581
                        , _factorA = 3
                        , _factorB = 5
                        }
                    )
                  )
                , ( UpId 8
                  , ( ProtVer { _pvMaj = 1 , _pvMin = 2 , _pvAlt = 1 }
                    , PParams
                        { _maxBkSz = 69516
                        , _maxHdrSz = 1448
                        , _maxTxSz = 17303
                        , _maxPropSz = 41
                        , _bkSgnCntT = 2.0797865763684675
                        , _bkSlotsPerEpoch = SlotCount { unSlotCount = 10 }
                        , _upTtl = SlotCount { unSlotCount = 13 }
                        , _scriptVersion = 1
                        , _upAdptThd = 0.4431824320461192
                        , _factorA = 0
                        , _factorB = 2
                        }
                    )
                  )
                ]
            , fromList []
            , fromList
                [ ( UpId 0 , Slot { unSlot = 110 } )
                , ( UpId 8 , Slot { unSlot = 114 } )
                ]
            , fromList
                [ ( UpId 0
                  , VKeyGenesis { unVKeyGenesis = VKey Owner { unOwner = 0 } }
                  )
                , ( UpId 8
                  , VKeyGenesis { unVKeyGenesis = VKey Owner { unOwner = 0 } }
                  )
                ]
            , fromList
                [ ( ProtVer { _pvMaj = 1 , _pvMin = 2 , _pvAlt = 0 }
                  , VKeyGenesis { unVKeyGenesis = VKey Owner { unOwner = 0 } }
                  )
                , ( ProtVer { _pvMaj = 1 , _pvMin = 2 , _pvAlt = 1 }
                  , VKeyGenesis { unVKeyGenesis = VKey Owner { unOwner = 0 } }
                  )
                ]
            , fromList
                [ ( UpId 0 , Slot { unSlot = 100 } )
                , ( UpId 8 , Slot { unSlot = 114 } )
                ]
            )
          )
        , Block
            { _bHeader =
                MkBlockHeader
                  { _bhPrevHash = Hash { unHash = 2618685024855499545 }
                  , _bhSlot = Slot { unSlot = 136 }
                  , _bhIssuer = VKey Owner { unOwner = 0 }
                  , _bhSig =
                      Sig Hash { unHash = -7422713345559720919 } Owner { unOwner = 0 }
                  , _bhUtxoHash = Hash { unHash = 839657738087498284 }
                  , _bhDlgHash = Hash { unHash = 839657738087498284 }
                  , _bhUpdHash = Hash { unHash = 0 }
                  }
            , _bBody =
                BlockBody
                  { _bDCerts = []
                  , _bUtxo = []
                  , _bUpdProp = Nothing
                  , _bUpdVotes = []
                  , _bProtVer = ProtVer { _pvMaj = 1 , _pvMin = 2 , _pvAlt = 1 }
                  }
            }
        )
      , ( ( Slot { unSlot = 131 }
          , fromList
              [ VKeyGenesis { unVKeyGenesis = VKey Owner { unOwner = 0 } }
              , VKeyGenesis { unVKeyGenesis = VKey Owner { unOwner = 0 } }
              ]
          , Hash { unHash = 2618685024855499545 }
          , UTxOState
              { utxo =
                  UTxO
                    { unUTxO =
                        fromList
                          [ ( TxIn TxId { getTxId = Hash { unHash = 0 } } 0
                            , TxOut
                                { addr = Addr (VKey Owner { unOwner = 0 })
                                , value = Lovelace { unLovelace = 6733 }
                                }
                            )
                          , ( TxIn TxId { getTxId = Hash { unHash = 1 } } 0
                            , TxOut
                                { addr = Addr (VKey Owner { unOwner = 1 })
                                , value = Lovelace { unLovelace = 4795 }
                                }
                            )
                          , ( TxIn TxId { getTxId = Hash { unHash = 2 } } 0
                            , TxOut
                                { addr = Addr (VKey Owner { unOwner = 2 })
                                , value = Lovelace { unLovelace = 8020 }
                                }
                            )
                          , ( TxIn TxId { getTxId = Hash { unHash = 3 } } 0
                            , TxOut
                                { addr = Addr (VKey Owner { unOwner = 3 })
                                , value = Lovelace { unLovelace = 1631 }
                                }
                            )
                          , ( TxIn TxId { getTxId = Hash { unHash = 4 } } 0
                            , TxOut
                                { addr = Addr (VKey Owner { unOwner = 4 })
                                , value = Lovelace { unLovelace = 7829 }
                                }
                            )
                          , ( TxIn TxId { getTxId = Hash { unHash = 7 } } 0
                            , TxOut
                                { addr = Addr (VKey Owner { unOwner = 7 })
                                , value = Lovelace { unLovelace = 6863 }
                                }
                            )
                          , ( TxIn TxId { getTxId = Hash { unHash = 8 } } 0
                            , TxOut
                                { addr = Addr (VKey Owner { unOwner = 8 })
                                , value = Lovelace { unLovelace = 8623 }
                                }
                            )
                          , ( TxIn TxId { getTxId = Hash { unHash = 9 } } 0
                            , TxOut
                                { addr = Addr (VKey Owner { unOwner = 9 })
                                , value = Lovelace { unLovelace = 7230 }
                                }
                            )
                          ]
                    }
              , reserves = Lovelace { unLovelace = 44999999999948276 }
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
          , ( ( ProtVer { _pvMaj = 1 , _pvMin = 1 , _pvAlt = 0 }
              , PParams
                  { _maxBkSz = 43200
                  , _maxHdrSz = 1453
                  , _maxTxSz = 8874
                  , _maxPropSz = 41
                  , _bkSgnCntT = 2.07902716432624
                  , _bkSlotsPerEpoch = SlotCount { unSlotCount = 10 }
                  , _upTtl = SlotCount { unSlotCount = 20 }
                  , _scriptVersion = 1
                  , _upAdptThd = 0.5500429331445129
                  , _factorA = 3
                  , _factorB = 6
                  }
              )
            , [ ( Slot { unSlot = 120 }
                , ( ProtVer { _pvMaj = 1 , _pvMin = 2 , _pvAlt = 0 }
                  , PParams
                      { _maxBkSz = 55127
                      , _maxHdrSz = 1968
                      , _maxTxSz = 10677
                      , _maxPropSz = 56
                      , _bkSgnCntT = 2.071832156909479
                      , _bkSlotsPerEpoch = SlotCount { unSlotCount = 10 }
                      , _upTtl = SlotCount { unSlotCount = 26 }
                      , _scriptVersion = 1
                      , _upAdptThd = 0.5135004796204581
                      , _factorA = 3
                      , _factorB = 5
                      }
                  )
                )
              ]
            , fromList
                [ ( ApName "3DK" , ( ApVer 2 , Slot { unSlot = 114 } , Metadata ) )
                , ( ApName "`" , ( ApVer 0 , Slot { unSlot = 120 } , Metadata ) )
                ]
            , fromList
                [ ( UpId 0
                  , ( ProtVer { _pvMaj = 1 , _pvMin = 2 , _pvAlt = 0 }
                    , PParams
                        { _maxBkSz = 55127
                        , _maxHdrSz = 1968
                        , _maxTxSz = 10677
                        , _maxPropSz = 56
                        , _bkSgnCntT = 2.071832156909479
                        , _bkSlotsPerEpoch = SlotCount { unSlotCount = 10 }
                        , _upTtl = SlotCount { unSlotCount = 26 }
                        , _scriptVersion = 1
                        , _upAdptThd = 0.5135004796204581
                        , _factorA = 3
                        , _factorB = 5
                        }
                    )
                  )
                , ( UpId 8
                  , ( ProtVer { _pvMaj = 1 , _pvMin = 2 , _pvAlt = 1 }
                    , PParams
                        { _maxBkSz = 69516
                        , _maxHdrSz = 1448
                        , _maxTxSz = 17303
                        , _maxPropSz = 41
                        , _bkSgnCntT = 2.0797865763684675
                        , _bkSlotsPerEpoch = SlotCount { unSlotCount = 10 }
                        , _upTtl = SlotCount { unSlotCount = 13 }
                        , _scriptVersion = 1
                        , _upAdptThd = 0.4431824320461192
                        , _factorA = 0
                        , _factorB = 2
                        }
                    )
                  )
                ]
            , fromList []
            , fromList
                [ ( UpId 0 , Slot { unSlot = 110 } )
                , ( UpId 8 , Slot { unSlot = 114 } )
                ]
            , fromList
                [ ( UpId 0
                  , VKeyGenesis { unVKeyGenesis = VKey Owner { unOwner = 0 } }
                  )
                , ( UpId 8
                  , VKeyGenesis { unVKeyGenesis = VKey Owner { unOwner = 0 } }
                  )
                ]
            , fromList
                [ ( ProtVer { _pvMaj = 1 , _pvMin = 2 , _pvAlt = 0 }
                  , VKeyGenesis { unVKeyGenesis = VKey Owner { unOwner = 0 } }
                  )
                ]
            , fromList
                [ ( UpId 0 , Slot { unSlot = 100 } )
                , ( UpId 8 , Slot { unSlot = 114 } )
                ]
            )
          )
        , Block
            { _bHeader =
                MkBlockHeader
                  { _bhPrevHash = Hash { unHash = -6156232021413761824 }
                  , _bhSlot = Slot { unSlot = 131 }
                  , _bhIssuer = VKey Owner { unOwner = 0 }
                  , _bhSig =
                      Sig Hash { unHash = 2618685024855499545 } Owner { unOwner = 0 }
                  , _bhUtxoHash = Hash { unHash = 839657738087498284 }
                  , _bhDlgHash = Hash { unHash = 839657738087498284 }
                  , _bhUpdHash = Hash { unHash = 0 }
                  }
            , _bBody =
                BlockBody
                  { _bDCerts = []
                  , _bUtxo = []
                  , _bUpdProp = Nothing
                  , _bUpdVotes = []
                  , _bProtVer = ProtVer { _pvMaj = 1 , _pvMin = 2 , _pvAlt = 0 }
                  }
            }
        )
      , ( ( Slot { unSlot = 125 }
          , fromList
              [ VKeyGenesis { unVKeyGenesis = VKey Owner { unOwner = 0 } }
              , VKeyGenesis { unVKeyGenesis = VKey Owner { unOwner = 0 } }
              ]
          , Hash { unHash = -6156232021413761824 }
          , UTxOState
              { utxo =
                  UTxO
                    { unUTxO =
                        fromList
                          [ ( TxIn TxId { getTxId = Hash { unHash = 0 } } 0
                            , TxOut
                                { addr = Addr (VKey Owner { unOwner = 0 })
                                , value = Lovelace { unLovelace = 6733 }
                                }
                            )
                          , ( TxIn TxId { getTxId = Hash { unHash = 1 } } 0
                            , TxOut
                                { addr = Addr (VKey Owner { unOwner = 1 })
                                , value = Lovelace { unLovelace = 4795 }
                                }
                            )
                          , ( TxIn TxId { getTxId = Hash { unHash = 2 } } 0
                            , TxOut
                                { addr = Addr (VKey Owner { unOwner = 2 })
                                , value = Lovelace { unLovelace = 8020 }
                                }
                            )
                          , ( TxIn TxId { getTxId = Hash { unHash = 3 } } 0
                            , TxOut
                                { addr = Addr (VKey Owner { unOwner = 3 })
                                , value = Lovelace { unLovelace = 1631 }
                                }
                            )
                          , ( TxIn TxId { getTxId = Hash { unHash = 4 } } 0
                            , TxOut
                                { addr = Addr (VKey Owner { unOwner = 4 })
                                , value = Lovelace { unLovelace = 7829 }
                                }
                            )
                          , ( TxIn TxId { getTxId = Hash { unHash = 7 } } 0
                            , TxOut
                                { addr = Addr (VKey Owner { unOwner = 7 })
                                , value = Lovelace { unLovelace = 6863 }
                                }
                            )
                          , ( TxIn TxId { getTxId = Hash { unHash = 8 } } 0
                            , TxOut
                                { addr = Addr (VKey Owner { unOwner = 8 })
                                , value = Lovelace { unLovelace = 8623 }
                                }
                            )
                          , ( TxIn TxId { getTxId = Hash { unHash = 9 } } 0
                            , TxOut
                                { addr = Addr (VKey Owner { unOwner = 9 })
                                , value = Lovelace { unLovelace = 7230 }
                                }
                            )
                          ]
                    }
              , reserves = Lovelace { unLovelace = 44999999999948276 }
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
          , ( ( ProtVer { _pvMaj = 1 , _pvMin = 1 , _pvAlt = 0 }
              , PParams
                  { _maxBkSz = 43200
                  , _maxHdrSz = 1453
                  , _maxTxSz = 8874
                  , _maxPropSz = 41
                  , _bkSgnCntT = 2.07902716432624
                  , _bkSlotsPerEpoch = SlotCount { unSlotCount = 10 }
                  , _upTtl = SlotCount { unSlotCount = 20 }
                  , _scriptVersion = 1
                  , _upAdptThd = 0.5500429331445129
                  , _factorA = 3
                  , _factorB = 6
                  }
              )
            , [ ( Slot { unSlot = 120 }
                , ( ProtVer { _pvMaj = 1 , _pvMin = 2 , _pvAlt = 0 }
                  , PParams
                      { _maxBkSz = 55127
                      , _maxHdrSz = 1968
                      , _maxTxSz = 10677
                      , _maxPropSz = 56
                      , _bkSgnCntT = 2.071832156909479
                      , _bkSlotsPerEpoch = SlotCount { unSlotCount = 10 }
                      , _upTtl = SlotCount { unSlotCount = 26 }
                      , _scriptVersion = 1
                      , _upAdptThd = 0.5135004796204581
                      , _factorA = 3
                      , _factorB = 5
                      }
                  )
                )
              ]
            , fromList
                [ ( ApName "3DK" , ( ApVer 2 , Slot { unSlot = 114 } , Metadata ) )
                , ( ApName "`" , ( ApVer 0 , Slot { unSlot = 120 } , Metadata ) )
                ]
            , fromList
                [ ( UpId 0
                  , ( ProtVer { _pvMaj = 1 , _pvMin = 2 , _pvAlt = 0 }
                    , PParams
                        { _maxBkSz = 55127
                        , _maxHdrSz = 1968
                        , _maxTxSz = 10677
                        , _maxPropSz = 56
                        , _bkSgnCntT = 2.071832156909479
                        , _bkSlotsPerEpoch = SlotCount { unSlotCount = 10 }
                        , _upTtl = SlotCount { unSlotCount = 26 }
                        , _scriptVersion = 1
                        , _upAdptThd = 0.5135004796204581
                        , _factorA = 3
                        , _factorB = 5
                        }
                    )
                  )
                , ( UpId 8
                  , ( ProtVer { _pvMaj = 1 , _pvMin = 2 , _pvAlt = 1 }
                    , PParams
                        { _maxBkSz = 69516
                        , _maxHdrSz = 1448
                        , _maxTxSz = 17303
                        , _maxPropSz = 41
                        , _bkSgnCntT = 2.0797865763684675
                        , _bkSlotsPerEpoch = SlotCount { unSlotCount = 10 }
                        , _upTtl = SlotCount { unSlotCount = 13 }
                        , _scriptVersion = 1
                        , _upAdptThd = 0.4431824320461192
                        , _factorA = 0
                        , _factorB = 2
                        }
                    )
                  )
                ]
            , fromList []
            , fromList
                [ ( UpId 0 , Slot { unSlot = 110 } )
                , ( UpId 8 , Slot { unSlot = 114 } )
                ]
            , fromList
                [ ( UpId 0
                  , VKeyGenesis { unVKeyGenesis = VKey Owner { unOwner = 0 } }
                  )
                , ( UpId 8
                  , VKeyGenesis { unVKeyGenesis = VKey Owner { unOwner = 0 } }
                  )
                ]
            , fromList
                [ ( ProtVer { _pvMaj = 1 , _pvMin = 2 , _pvAlt = 0 }
                  , VKeyGenesis { unVKeyGenesis = VKey Owner { unOwner = 0 } }
                  )
                ]
            , fromList
                [ ( UpId 0 , Slot { unSlot = 100 } )
                , ( UpId 8 , Slot { unSlot = 114 } )
                ]
            )
          )
        , Block
            { _bHeader =
                MkBlockHeader
                  { _bhPrevHash = Hash { unHash = 2198649532482978511 }
                  , _bhSlot = Slot { unSlot = 125 }
                  , _bhIssuer = VKey Owner { unOwner = 0 }
                  , _bhSig =
                      Sig Hash { unHash = -6156232021413761824 } Owner { unOwner = 0 }
                  , _bhUtxoHash = Hash { unHash = 839657738087498284 }
                  , _bhDlgHash = Hash { unHash = 839657738087498284 }
                  , _bhUpdHash = Hash { unHash = 0 }
                  }
            , _bBody =
                BlockBody
                  { _bDCerts = []
                  , _bUtxo = []
                  , _bUpdProp = Nothing
                  , _bUpdVotes = []
                  , _bProtVer = ProtVer { _pvMaj = 1 , _pvMin = 2 , _pvAlt = 0 }
                  }
            }
        )
      , ( ( Slot { unSlot = 120 }
          , fromList
              [ VKeyGenesis { unVKeyGenesis = VKey Owner { unOwner = 0 } }
              , VKeyGenesis { unVKeyGenesis = VKey Owner { unOwner = 0 } }
              ]
          , Hash { unHash = 2198649532482978511 }
          , UTxOState
              { utxo =
                  UTxO
                    { unUTxO =
                        fromList
                          [ ( TxIn TxId { getTxId = Hash { unHash = 0 } } 0
                            , TxOut
                                { addr = Addr (VKey Owner { unOwner = 0 })
                                , value = Lovelace { unLovelace = 6733 }
                                }
                            )
                          , ( TxIn TxId { getTxId = Hash { unHash = 1 } } 0
                            , TxOut
                                { addr = Addr (VKey Owner { unOwner = 1 })
                                , value = Lovelace { unLovelace = 4795 }
                                }
                            )
                          , ( TxIn TxId { getTxId = Hash { unHash = 2 } } 0
                            , TxOut
                                { addr = Addr (VKey Owner { unOwner = 2 })
                                , value = Lovelace { unLovelace = 8020 }
                                }
                            )
                          , ( TxIn TxId { getTxId = Hash { unHash = 3 } } 0
                            , TxOut
                                { addr = Addr (VKey Owner { unOwner = 3 })
                                , value = Lovelace { unLovelace = 1631 }
                                }
                            )
                          , ( TxIn TxId { getTxId = Hash { unHash = 4 } } 0
                            , TxOut
                                { addr = Addr (VKey Owner { unOwner = 4 })
                                , value = Lovelace { unLovelace = 7829 }
                                }
                            )
                          , ( TxIn TxId { getTxId = Hash { unHash = 7 } } 0
                            , TxOut
                                { addr = Addr (VKey Owner { unOwner = 7 })
                                , value = Lovelace { unLovelace = 6863 }
                                }
                            )
                          , ( TxIn TxId { getTxId = Hash { unHash = 8 } } 0
                            , TxOut
                                { addr = Addr (VKey Owner { unOwner = 8 })
                                , value = Lovelace { unLovelace = 8623 }
                                }
                            )
                          , ( TxIn TxId { getTxId = Hash { unHash = 9 } } 0
                            , TxOut
                                { addr = Addr (VKey Owner { unOwner = 9 })
                                , value = Lovelace { unLovelace = 7230 }
                                }
                            )
                          ]
                    }
              , reserves = Lovelace { unLovelace = 44999999999948276 }
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
          , ( ( ProtVer { _pvMaj = 1 , _pvMin = 1 , _pvAlt = 0 }
              , PParams
                  { _maxBkSz = 43200
                  , _maxHdrSz = 1453
                  , _maxTxSz = 8874
                  , _maxPropSz = 41
                  , _bkSgnCntT = 2.07902716432624
                  , _bkSlotsPerEpoch = SlotCount { unSlotCount = 10 }
                  , _upTtl = SlotCount { unSlotCount = 20 }
                  , _scriptVersion = 1
                  , _upAdptThd = 0.5500429331445129
                  , _factorA = 3
                  , _factorB = 6
                  }
              )
            , [ ( Slot { unSlot = 120 }
                , ( ProtVer { _pvMaj = 1 , _pvMin = 2 , _pvAlt = 0 }
                  , PParams
                      { _maxBkSz = 55127
                      , _maxHdrSz = 1968
                      , _maxTxSz = 10677
                      , _maxPropSz = 56
                      , _bkSgnCntT = 2.071832156909479
                      , _bkSlotsPerEpoch = SlotCount { unSlotCount = 10 }
                      , _upTtl = SlotCount { unSlotCount = 26 }
                      , _scriptVersion = 1
                      , _upAdptThd = 0.5135004796204581
                      , _factorA = 3
                      , _factorB = 5
                      }
                  )
                )
              ]
            , fromList
                [ ( ApName "3DK" , ( ApVer 2 , Slot { unSlot = 114 } , Metadata ) )
                , ( ApName "`" , ( ApVer 0 , Slot { unSlot = 120 } , Metadata ) )
                ]
            , fromList
                [ ( UpId 0
                  , ( ProtVer { _pvMaj = 1 , _pvMin = 2 , _pvAlt = 0 }
                    , PParams
                        { _maxBkSz = 55127
                        , _maxHdrSz = 1968
                        , _maxTxSz = 10677
                        , _maxPropSz = 56
                        , _bkSgnCntT = 2.071832156909479
                        , _bkSlotsPerEpoch = SlotCount { unSlotCount = 10 }
                        , _upTtl = SlotCount { unSlotCount = 26 }
                        , _scriptVersion = 1
                        , _upAdptThd = 0.5135004796204581
                        , _factorA = 3
                        , _factorB = 5
                        }
                    )
                  )
                , ( UpId 8
                  , ( ProtVer { _pvMaj = 1 , _pvMin = 2 , _pvAlt = 1 }
                    , PParams
                        { _maxBkSz = 69516
                        , _maxHdrSz = 1448
                        , _maxTxSz = 17303
                        , _maxPropSz = 41
                        , _bkSgnCntT = 2.0797865763684675
                        , _bkSlotsPerEpoch = SlotCount { unSlotCount = 10 }
                        , _upTtl = SlotCount { unSlotCount = 13 }
                        , _scriptVersion = 1
                        , _upAdptThd = 0.4431824320461192
                        , _factorA = 0
                        , _factorB = 2
                        }
                    )
                  )
                ]
            , fromList []
            , fromList
                [ ( UpId 0 , Slot { unSlot = 110 } )
                , ( UpId 8 , Slot { unSlot = 114 } )
                ]
            , fromList
                [ ( UpId 0
                  , VKeyGenesis { unVKeyGenesis = VKey Owner { unOwner = 0 } }
                  )
                , ( UpId 8
                  , VKeyGenesis { unVKeyGenesis = VKey Owner { unOwner = 0 } }
                  )
                ]
            , fromList
                [ ( ProtVer { _pvMaj = 1 , _pvMin = 2 , _pvAlt = 0 }
                  , VKeyGenesis { unVKeyGenesis = VKey Owner { unOwner = 0 } }
                  )
                ]
            , fromList
                [ ( UpId 0 , Slot { unSlot = 100 } )
                , ( UpId 8 , Slot { unSlot = 114 } )
                ]
            )
          )
        , Block
            { _bHeader =
                MkBlockHeader
                  { _bhPrevHash = Hash { unHash = -4630346549659696065 }
                  , _bhSlot = Slot { unSlot = 120 }
                  , _bhIssuer = VKey Owner { unOwner = 0 }
                  , _bhSig =
                      Sig Hash { unHash = 2198649532482978511 } Owner { unOwner = 0 }
                  , _bhUtxoHash = Hash { unHash = 839657738087498284 }
                  , _bhDlgHash = Hash { unHash = 839657738087498284 }
                  , _bhUpdHash = Hash { unHash = 0 }
                  }
            , _bBody =
                BlockBody
                  { _bDCerts = []
                  , _bUtxo = []
                  , _bUpdProp = Nothing
                  , _bUpdVotes = []
                  , _bProtVer = ProtVer { _pvMaj = 1 , _pvMin = 2 , _pvAlt = 0 }
                  }
            }
        )
      , ( ( Slot { unSlot = 114 }
          , fromList
              [ VKeyGenesis { unVKeyGenesis = VKey Owner { unOwner = 0 } }
              , VKeyGenesis { unVKeyGenesis = VKey Owner { unOwner = 0 } }
              ]
          , Hash { unHash = -4630346549659696065 }
          , UTxOState
              { utxo =
                  UTxO
                    { unUTxO =
                        fromList
                          [ ( TxIn TxId { getTxId = Hash { unHash = 0 } } 0
                            , TxOut
                                { addr = Addr (VKey Owner { unOwner = 0 })
                                , value = Lovelace { unLovelace = 6733 }
                                }
                            )
                          , ( TxIn TxId { getTxId = Hash { unHash = 1 } } 0
                            , TxOut
                                { addr = Addr (VKey Owner { unOwner = 1 })
                                , value = Lovelace { unLovelace = 4795 }
                                }
                            )
                          , ( TxIn TxId { getTxId = Hash { unHash = 2 } } 0
                            , TxOut
                                { addr = Addr (VKey Owner { unOwner = 2 })
                                , value = Lovelace { unLovelace = 8020 }
                                }
                            )
                          , ( TxIn TxId { getTxId = Hash { unHash = 3 } } 0
                            , TxOut
                                { addr = Addr (VKey Owner { unOwner = 3 })
                                , value = Lovelace { unLovelace = 1631 }
                                }
                            )
                          , ( TxIn TxId { getTxId = Hash { unHash = 4 } } 0
                            , TxOut
                                { addr = Addr (VKey Owner { unOwner = 4 })
                                , value = Lovelace { unLovelace = 7829 }
                                }
                            )
                          , ( TxIn TxId { getTxId = Hash { unHash = 7 } } 0
                            , TxOut
                                { addr = Addr (VKey Owner { unOwner = 7 })
                                , value = Lovelace { unLovelace = 6863 }
                                }
                            )
                          , ( TxIn TxId { getTxId = Hash { unHash = 8 } } 0
                            , TxOut
                                { addr = Addr (VKey Owner { unOwner = 8 })
                                , value = Lovelace { unLovelace = 8623 }
                                }
                            )
                          , ( TxIn TxId { getTxId = Hash { unHash = 9 } } 0
                            , TxOut
                                { addr = Addr (VKey Owner { unOwner = 9 })
                                , value = Lovelace { unLovelace = 7230 }
                                }
                            )
                          ]
                    }
              , reserves = Lovelace { unLovelace = 44999999999948276 }
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
          , ( ( ProtVer { _pvMaj = 1 , _pvMin = 1 , _pvAlt = 0 }
              , PParams
                  { _maxBkSz = 43200
                  , _maxHdrSz = 1453
                  , _maxTxSz = 8874
                  , _maxPropSz = 41
                  , _bkSgnCntT = 2.07902716432624
                  , _bkSlotsPerEpoch = SlotCount { unSlotCount = 10 }
                  , _upTtl = SlotCount { unSlotCount = 20 }
                  , _scriptVersion = 1
                  , _upAdptThd = 0.5500429331445129
                  , _factorA = 3
                  , _factorB = 6
                  }
              )
            , []
            , fromList
                [ ( ApName "3DK" , ( ApVer 2 , Slot { unSlot = 114 } , Metadata ) )
                ]
            , fromList
                [ ( UpId 0
                  , ( ProtVer { _pvMaj = 1 , _pvMin = 2 , _pvAlt = 0 }
                    , PParams
                        { _maxBkSz = 55127
                        , _maxHdrSz = 1968
                        , _maxTxSz = 10677
                        , _maxPropSz = 56
                        , _bkSgnCntT = 2.071832156909479
                        , _bkSlotsPerEpoch = SlotCount { unSlotCount = 10 }
                        , _upTtl = SlotCount { unSlotCount = 26 }
                        , _scriptVersion = 1
                        , _upAdptThd = 0.5135004796204581
                        , _factorA = 3
                        , _factorB = 5
                        }
                    )
                  )
                , ( UpId 8
                  , ( ProtVer { _pvMaj = 1 , _pvMin = 2 , _pvAlt = 1 }
                    , PParams
                        { _maxBkSz = 69516
                        , _maxHdrSz = 1448
                        , _maxTxSz = 17303
                        , _maxPropSz = 41
                        , _bkSgnCntT = 2.0797865763684675
                        , _bkSlotsPerEpoch = SlotCount { unSlotCount = 10 }
                        , _upTtl = SlotCount { unSlotCount = 13 }
                        , _scriptVersion = 1
                        , _upAdptThd = 0.4431824320461192
                        , _factorA = 0
                        , _factorB = 2
                        }
                    )
                  )
                ]
            , fromList [ ( UpId 8 , ( ApName "`" , ApVer 0 , Metadata ) ) ]
            , fromList
                [ ( UpId 0 , Slot { unSlot = 110 } )
                , ( UpId 8 , Slot { unSlot = 114 } )
                ]
            , fromList
                [ ( UpId 0
                  , VKeyGenesis { unVKeyGenesis = VKey Owner { unOwner = 0 } }
                  )
                , ( UpId 8
                  , VKeyGenesis { unVKeyGenesis = VKey Owner { unOwner = 0 } }
                  )
                ]
            , fromList []
            , fromList
                [ ( UpId 0 , Slot { unSlot = 100 } )
                , ( UpId 8 , Slot { unSlot = 114 } )
                ]
            )
          )
        , Block
            { _bHeader =
                MkBlockHeader
                  { _bhPrevHash = Hash { unHash = 9089843413711555245 }
                  , _bhSlot = Slot { unSlot = 114 }
                  , _bhIssuer = VKey Owner { unOwner = 0 }
                  , _bhSig =
                      Sig Hash { unHash = -4630346549659696065 } Owner { unOwner = 0 }
                  , _bhUtxoHash = Hash { unHash = 839657738087498284 }
                  , _bhDlgHash = Hash { unHash = 839657738087498284 }
                  , _bhUpdHash = Hash { unHash = -6594130697407361032 }
                  }
            , _bBody =
                BlockBody
                  { _bDCerts = []
                  , _bUtxo = []
                  , _bUpdProp =
                      Just
                        UProp
                          { _upId = UpId 8
                          , _upIssuer = VKey Owner { unOwner = 0 }
                          , _upParams =
                              PParams
                                { _maxBkSz = 69516
                                , _maxHdrSz = 1448
                                , _maxTxSz = 17303
                                , _maxPropSz = 41
                                , _bkSgnCntT = 2.0797865763684675
                                , _bkSlotsPerEpoch = SlotCount { unSlotCount = 10 }
                                , _upTtl = SlotCount { unSlotCount = 13 }
                                , _scriptVersion = 1
                                , _upAdptThd = 0.4431824320461192
                                , _factorA = 0
                                , _factorB = 2
                                }
                          , _upPV = ProtVer { _pvMaj = 1 , _pvMin = 2 , _pvAlt = 1 }
                          , _upSwVer = SwVer { _svName = ApName "`" , _svVer = ApVer 0 }
                          , _upSig =
                              Sig
                                ( ProtVer { _pvMaj = 1 , _pvMin = 2 , _pvAlt = 1 }
                                , PParams
                                    { _maxBkSz = 69516
                                    , _maxHdrSz = 1448
                                    , _maxTxSz = 17303
                                    , _maxPropSz = 41
                                    , _bkSgnCntT = 2.0797865763684675
                                    , _bkSlotsPerEpoch = SlotCount { unSlotCount = 10 }
                                    , _upTtl = SlotCount { unSlotCount = 13 }
                                    , _scriptVersion = 1
                                    , _upAdptThd = 0.4431824320461192
                                    , _factorA = 0
                                    , _factorB = 2
                                    }
                                , SwVer { _svName = ApName "`" , _svVer = ApVer 0 }
                                )
                                Owner { unOwner = 0 }
                          , _upSTags =
                              fromList [ "(U2\SOHL3a" , "+u\SUBN'7\FSp" , "w!dK\ETX4\SOHt\ESC2" ]
                          , _upMdt = Metadata
                          }
                  , _bUpdVotes =
                      [ Vote
                          { _vCaster = VKey Owner { unOwner = 0 }
                          , _vPropId = UpId 8
                          , _vSig = Sig (UpId 8) Owner { unOwner = 0 }
                          }
                      ]
                  , _bProtVer = ProtVer { _pvMaj = 1 , _pvMin = 1 , _pvAlt = 0 }
                  }
            }
        )
      , ( ( Slot { unSlot = 110 }
          , fromList
              [ VKeyGenesis { unVKeyGenesis = VKey Owner { unOwner = 0 } }
              , VKeyGenesis { unVKeyGenesis = VKey Owner { unOwner = 0 } }
              ]
          , Hash { unHash = 9089843413711555245 }
          , UTxOState
              { utxo =
                  UTxO
                    { unUTxO =
                        fromList
                          [ ( TxIn TxId { getTxId = Hash { unHash = 0 } } 0
                            , TxOut
                                { addr = Addr (VKey Owner { unOwner = 0 })
                                , value = Lovelace { unLovelace = 6733 }
                                }
                            )
                          , ( TxIn TxId { getTxId = Hash { unHash = 1 } } 0
                            , TxOut
                                { addr = Addr (VKey Owner { unOwner = 1 })
                                , value = Lovelace { unLovelace = 4795 }
                                }
                            )
                          , ( TxIn TxId { getTxId = Hash { unHash = 2 } } 0
                            , TxOut
                                { addr = Addr (VKey Owner { unOwner = 2 })
                                , value = Lovelace { unLovelace = 8020 }
                                }
                            )
                          , ( TxIn TxId { getTxId = Hash { unHash = 3 } } 0
                            , TxOut
                                { addr = Addr (VKey Owner { unOwner = 3 })
                                , value = Lovelace { unLovelace = 1631 }
                                }
                            )
                          , ( TxIn TxId { getTxId = Hash { unHash = 4 } } 0
                            , TxOut
                                { addr = Addr (VKey Owner { unOwner = 4 })
                                , value = Lovelace { unLovelace = 7829 }
                                }
                            )
                          , ( TxIn TxId { getTxId = Hash { unHash = 7 } } 0
                            , TxOut
                                { addr = Addr (VKey Owner { unOwner = 7 })
                                , value = Lovelace { unLovelace = 6863 }
                                }
                            )
                          , ( TxIn TxId { getTxId = Hash { unHash = 8 } } 0
                            , TxOut
                                { addr = Addr (VKey Owner { unOwner = 8 })
                                , value = Lovelace { unLovelace = 8623 }
                                }
                            )
                          , ( TxIn TxId { getTxId = Hash { unHash = 9 } } 0
                            , TxOut
                                { addr = Addr (VKey Owner { unOwner = 9 })
                                , value = Lovelace { unLovelace = 7230 }
                                }
                            )
                          ]
                    }
              , reserves = Lovelace { unLovelace = 44999999999948276 }
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
          , ( ( ProtVer { _pvMaj = 1 , _pvMin = 1 , _pvAlt = 0 }
              , PParams
                  { _maxBkSz = 43200
                  , _maxHdrSz = 1453
                  , _maxTxSz = 8874
                  , _maxPropSz = 41
                  , _bkSgnCntT = 2.07902716432624
                  , _bkSlotsPerEpoch = SlotCount { unSlotCount = 10 }
                  , _upTtl = SlotCount { unSlotCount = 20 }
                  , _scriptVersion = 1
                  , _upAdptThd = 0.5500429331445129
                  , _factorA = 3
                  , _factorB = 6
                  }
              )
            , []
            , fromList
                [ ( ApName "3DK" , ( ApVer 1 , Slot { unSlot = 86 } , Metadata ) )
                ]
            , fromList
                [ ( UpId 0
                  , ( ProtVer { _pvMaj = 1 , _pvMin = 2 , _pvAlt = 0 }
                    , PParams
                        { _maxBkSz = 55127
                        , _maxHdrSz = 1968
                        , _maxTxSz = 10677
                        , _maxPropSz = 56
                        , _bkSgnCntT = 2.071832156909479
                        , _bkSlotsPerEpoch = SlotCount { unSlotCount = 10 }
                        , _upTtl = SlotCount { unSlotCount = 26 }
                        , _scriptVersion = 1
                        , _upAdptThd = 0.5135004796204581
                        , _factorA = 3
                        , _factorB = 5
                        }
                    )
                  )
                ]
            , fromList [ ( UpId 0 , ( ApName "3DK" , ApVer 2 , Metadata ) ) ]
            , fromList [ ( UpId 0 , Slot { unSlot = 110 } ) ]
            , fromList
                [ ( UpId 0
                  , VKeyGenesis { unVKeyGenesis = VKey Owner { unOwner = 0 } }
                  )
                ]
            , fromList []
            , fromList [ ( UpId 0 , Slot { unSlot = 100 } ) ]
            )
          )
        , Block
            { _bHeader =
                MkBlockHeader
                  { _bhPrevHash = Hash { unHash = -3013368912743255925 }
                  , _bhSlot = Slot { unSlot = 110 }
                  , _bhIssuer = VKey Owner { unOwner = 0 }
                  , _bhSig =
                      Sig Hash { unHash = 9089843413711555245 } Owner { unOwner = 0 }
                  , _bhUtxoHash = Hash { unHash = 839657738087498284 }
                  , _bhDlgHash = Hash { unHash = 839657738087498284 }
                  , _bhUpdHash = Hash { unHash = 1 }
                  }
            , _bBody =
                BlockBody
                  { _bDCerts = []
                  , _bUtxo = []
                  , _bUpdProp = Nothing
                  , _bUpdVotes =
                      [ Vote
                          { _vCaster = VKey Owner { unOwner = 0 }
                          , _vPropId = UpId 0
                          , _vSig = Sig (UpId 0) Owner { unOwner = 0 }
                          }
                      ]
                  , _bProtVer = ProtVer { _pvMaj = 1 , _pvMin = 1 , _pvAlt = 0 }
                  }
            }
        )
      , ( ( Slot { unSlot = 105 }
          , fromList
              [ VKeyGenesis { unVKeyGenesis = VKey Owner { unOwner = 0 } }
              , VKeyGenesis { unVKeyGenesis = VKey Owner { unOwner = 0 } }
              ]
          , Hash { unHash = -3013368912743255925 }
          , UTxOState
              { utxo =
                  UTxO
                    { unUTxO =
                        fromList
                          [ ( TxIn TxId { getTxId = Hash { unHash = 0 } } 0
                            , TxOut
                                { addr = Addr (VKey Owner { unOwner = 0 })
                                , value = Lovelace { unLovelace = 6733 }
                                }
                            )
                          , ( TxIn TxId { getTxId = Hash { unHash = 1 } } 0
                            , TxOut
                                { addr = Addr (VKey Owner { unOwner = 1 })
                                , value = Lovelace { unLovelace = 4795 }
                                }
                            )
                          , ( TxIn TxId { getTxId = Hash { unHash = 2 } } 0
                            , TxOut
                                { addr = Addr (VKey Owner { unOwner = 2 })
                                , value = Lovelace { unLovelace = 8020 }
                                }
                            )
                          , ( TxIn TxId { getTxId = Hash { unHash = 3 } } 0
                            , TxOut
                                { addr = Addr (VKey Owner { unOwner = 3 })
                                , value = Lovelace { unLovelace = 1631 }
                                }
                            )
                          , ( TxIn TxId { getTxId = Hash { unHash = 4 } } 0
                            , TxOut
                                { addr = Addr (VKey Owner { unOwner = 4 })
                                , value = Lovelace { unLovelace = 7829 }
                                }
                            )
                          , ( TxIn TxId { getTxId = Hash { unHash = 7 } } 0
                            , TxOut
                                { addr = Addr (VKey Owner { unOwner = 7 })
                                , value = Lovelace { unLovelace = 6863 }
                                }
                            )
                          , ( TxIn TxId { getTxId = Hash { unHash = 8 } } 0
                            , TxOut
                                { addr = Addr (VKey Owner { unOwner = 8 })
                                , value = Lovelace { unLovelace = 8623 }
                                }
                            )
                          , ( TxIn TxId { getTxId = Hash { unHash = 9 } } 0
                            , TxOut
                                { addr = Addr (VKey Owner { unOwner = 9 })
                                , value = Lovelace { unLovelace = 7230 }
                                }
                            )
                          ]
                    }
              , reserves = Lovelace { unLovelace = 44999999999948276 }
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
          , ( ( ProtVer { _pvMaj = 1 , _pvMin = 1 , _pvAlt = 0 }
              , PParams
                  { _maxBkSz = 43200
                  , _maxHdrSz = 1453
                  , _maxTxSz = 8874
                  , _maxPropSz = 41
                  , _bkSgnCntT = 2.07902716432624
                  , _bkSlotsPerEpoch = SlotCount { unSlotCount = 10 }
                  , _upTtl = SlotCount { unSlotCount = 20 }
                  , _scriptVersion = 1
                  , _upAdptThd = 0.5500429331445129
                  , _factorA = 3
                  , _factorB = 6
                  }
              )
            , []
            , fromList
                [ ( ApName "3DK" , ( ApVer 1 , Slot { unSlot = 86 } , Metadata ) )
                ]
            , fromList
                [ ( UpId 0
                  , ( ProtVer { _pvMaj = 1 , _pvMin = 2 , _pvAlt = 0 }
                    , PParams
                        { _maxBkSz = 55127
                        , _maxHdrSz = 1968
                        , _maxTxSz = 10677
                        , _maxPropSz = 56
                        , _bkSgnCntT = 2.071832156909479
                        , _bkSlotsPerEpoch = SlotCount { unSlotCount = 10 }
                        , _upTtl = SlotCount { unSlotCount = 26 }
                        , _scriptVersion = 1
                        , _upAdptThd = 0.5135004796204581
                        , _factorA = 3
                        , _factorB = 5
                        }
                    )
                  )
                ]
            , fromList [ ( UpId 0 , ( ApName "3DK" , ApVer 2 , Metadata ) ) ]
            , fromList []
            , fromList []
            , fromList []
            , fromList [ ( UpId 0 , Slot { unSlot = 100 } ) ]
            )
          )
        , Block
            { _bHeader =
                MkBlockHeader
                  { _bhPrevHash = Hash { unHash = 7020241738507949760 }
                  , _bhSlot = Slot { unSlot = 105 }
                  , _bhIssuer = VKey Owner { unOwner = 0 }
                  , _bhSig =
                      Sig Hash { unHash = -3013368912743255925 } Owner { unOwner = 0 }
                  , _bhUtxoHash = Hash { unHash = 839657738087498284 }
                  , _bhDlgHash = Hash { unHash = 839657738087498284 }
                  , _bhUpdHash = Hash { unHash = 0 }
                  }
            , _bBody =
                BlockBody
                  { _bDCerts = []
                  , _bUtxo = []
                  , _bUpdProp = Nothing
                  , _bUpdVotes = []
                  , _bProtVer = ProtVer { _pvMaj = 1 , _pvMin = 1 , _pvAlt = 0 }
                  }
            }
        )
      , ( ( Slot { unSlot = 100 }
          , fromList
              [ VKeyGenesis { unVKeyGenesis = VKey Owner { unOwner = 0 } }
              , VKeyGenesis { unVKeyGenesis = VKey Owner { unOwner = 0 } }
              ]
          , Hash { unHash = 7020241738507949760 }
          , UTxOState
              { utxo =
                  UTxO
                    { unUTxO =
                        fromList
                          [ ( TxIn TxId { getTxId = Hash { unHash = 0 } } 0
                            , TxOut
                                { addr = Addr (VKey Owner { unOwner = 0 })
                                , value = Lovelace { unLovelace = 6733 }
                                }
                            )
                          , ( TxIn TxId { getTxId = Hash { unHash = 1 } } 0
                            , TxOut
                                { addr = Addr (VKey Owner { unOwner = 1 })
                                , value = Lovelace { unLovelace = 4795 }
                                }
                            )
                          , ( TxIn TxId { getTxId = Hash { unHash = 2 } } 0
                            , TxOut
                                { addr = Addr (VKey Owner { unOwner = 2 })
                                , value = Lovelace { unLovelace = 8020 }
                                }
                            )
                          , ( TxIn TxId { getTxId = Hash { unHash = 3 } } 0
                            , TxOut
                                { addr = Addr (VKey Owner { unOwner = 3 })
                                , value = Lovelace { unLovelace = 1631 }
                                }
                            )
                          , ( TxIn TxId { getTxId = Hash { unHash = 4 } } 0
                            , TxOut
                                { addr = Addr (VKey Owner { unOwner = 4 })
                                , value = Lovelace { unLovelace = 7829 }
                                }
                            )
                          , ( TxIn TxId { getTxId = Hash { unHash = 7 } } 0
                            , TxOut
                                { addr = Addr (VKey Owner { unOwner = 7 })
                                , value = Lovelace { unLovelace = 6863 }
                                }
                            )
                          , ( TxIn TxId { getTxId = Hash { unHash = 8 } } 0
                            , TxOut
                                { addr = Addr (VKey Owner { unOwner = 8 })
                                , value = Lovelace { unLovelace = 8623 }
                                }
                            )
                          , ( TxIn TxId { getTxId = Hash { unHash = 9 } } 0
                            , TxOut
                                { addr = Addr (VKey Owner { unOwner = 9 })
                                , value = Lovelace { unLovelace = 7230 }
                                }
                            )
                          ]
                    }
              , reserves = Lovelace { unLovelace = 44999999999948276 }
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
          , ( ( ProtVer { _pvMaj = 1 , _pvMin = 1 , _pvAlt = 0 }
              , PParams
                  { _maxBkSz = 43200
                  , _maxHdrSz = 1453
                  , _maxTxSz = 8874
                  , _maxPropSz = 41
                  , _bkSgnCntT = 2.07902716432624
                  , _bkSlotsPerEpoch = SlotCount { unSlotCount = 10 }
                  , _upTtl = SlotCount { unSlotCount = 20 }
                  , _scriptVersion = 1
                  , _upAdptThd = 0.5500429331445129
                  , _factorA = 3
                  , _factorB = 6
                  }
              )
            , []
            , fromList
                [ ( ApName "3DK" , ( ApVer 1 , Slot { unSlot = 86 } , Metadata ) )
                ]
            , fromList
                [ ( UpId 0
                  , ( ProtVer { _pvMaj = 1 , _pvMin = 2 , _pvAlt = 0 }
                    , PParams
                        { _maxBkSz = 55127
                        , _maxHdrSz = 1968
                        , _maxTxSz = 10677
                        , _maxPropSz = 56
                        , _bkSgnCntT = 2.071832156909479
                        , _bkSlotsPerEpoch = SlotCount { unSlotCount = 10 }
                        , _upTtl = SlotCount { unSlotCount = 26 }
                        , _scriptVersion = 1
                        , _upAdptThd = 0.5135004796204581
                        , _factorA = 3
                        , _factorB = 5
                        }
                    )
                  )
                ]
            , fromList [ ( UpId 0 , ( ApName "3DK" , ApVer 2 , Metadata ) ) ]
            , fromList []
            , fromList []
            , fromList []
            , fromList [ ( UpId 0 , Slot { unSlot = 100 } ) ]
            )
          )
        , Block
            { _bHeader =
                MkBlockHeader
                  { _bhPrevHash = Hash { unHash = -8184217684611921972 }
                  , _bhSlot = Slot { unSlot = 100 }
                  , _bhIssuer = VKey Owner { unOwner = 0 }
                  , _bhSig =
                      Sig Hash { unHash = 7020241738507949760 } Owner { unOwner = 0 }
                  , _bhUtxoHash = Hash { unHash = 839657738087498284 }
                  , _bhDlgHash = Hash { unHash = 839657738087498284 }
                  , _bhUpdHash = Hash { unHash = 8425607116791613621 }
                  }
            , _bBody =
                BlockBody
                  { _bDCerts = []
                  , _bUtxo = []
                  , _bUpdProp =
                      Just
                        UProp
                          { _upId = UpId 0
                          , _upIssuer = VKey Owner { unOwner = 0 }
                          , _upParams =
                              PParams
                                { _maxBkSz = 55127
                                , _maxHdrSz = 1968
                                , _maxTxSz = 10677
                                , _maxPropSz = 56
                                , _bkSgnCntT = 2.071832156909479
                                , _bkSlotsPerEpoch = SlotCount { unSlotCount = 10 }
                                , _upTtl = SlotCount { unSlotCount = 26 }
                                , _scriptVersion = 1
                                , _upAdptThd = 0.5135004796204581
                                , _factorA = 3
                                , _factorB = 5
                                }
                          , _upPV = ProtVer { _pvMaj = 1 , _pvMin = 2 , _pvAlt = 0 }
                          , _upSwVer = SwVer { _svName = ApName "3DK" , _svVer = ApVer 2 }
                          , _upSig =
                              Sig
                                ( ProtVer { _pvMaj = 1 , _pvMin = 2 , _pvAlt = 0 }
                                , PParams
                                    { _maxBkSz = 55127
                                    , _maxHdrSz = 1968
                                    , _maxTxSz = 10677
                                    , _maxPropSz = 56
                                    , _bkSgnCntT = 2.071832156909479
                                    , _bkSlotsPerEpoch = SlotCount { unSlotCount = 10 }
                                    , _upTtl = SlotCount { unSlotCount = 26 }
                                    , _scriptVersion = 1
                                    , _upAdptThd = 0.5135004796204581
                                    , _factorA = 3
                                    , _factorB = 5
                                    }
                                , SwVer { _svName = ApName "3DK" , _svVer = ApVer 2 }
                                )
                                Owner { unOwner = 0 }
                          , _upSTags = fromList [ "+" , "E*" , "p:" ]
                          , _upMdt = Metadata
                          }
                  , _bUpdVotes = []
                  , _bProtVer = ProtVer { _pvMaj = 1 , _pvMin = 1 , _pvAlt = 0 }
                  }
            }
        )
      , ( ( Slot { unSlot = 93 }
          , fromList
              [ VKeyGenesis { unVKeyGenesis = VKey Owner { unOwner = 0 } }
              , VKeyGenesis { unVKeyGenesis = VKey Owner { unOwner = 0 } }
              ]
          , Hash { unHash = -8184217684611921972 }
          , UTxOState
              { utxo =
                  UTxO
                    { unUTxO =
                        fromList
                          [ ( TxIn TxId { getTxId = Hash { unHash = 0 } } 0
                            , TxOut
                                { addr = Addr (VKey Owner { unOwner = 0 })
                                , value = Lovelace { unLovelace = 6733 }
                                }
                            )
                          , ( TxIn TxId { getTxId = Hash { unHash = 1 } } 0
                            , TxOut
                                { addr = Addr (VKey Owner { unOwner = 1 })
                                , value = Lovelace { unLovelace = 4795 }
                                }
                            )
                          , ( TxIn TxId { getTxId = Hash { unHash = 2 } } 0
                            , TxOut
                                { addr = Addr (VKey Owner { unOwner = 2 })
                                , value = Lovelace { unLovelace = 8020 }
                                }
                            )
                          , ( TxIn TxId { getTxId = Hash { unHash = 3 } } 0
                            , TxOut
                                { addr = Addr (VKey Owner { unOwner = 3 })
                                , value = Lovelace { unLovelace = 1631 }
                                }
                            )
                          , ( TxIn TxId { getTxId = Hash { unHash = 4 } } 0
                            , TxOut
                                { addr = Addr (VKey Owner { unOwner = 4 })
                                , value = Lovelace { unLovelace = 7829 }
                                }
                            )
                          , ( TxIn TxId { getTxId = Hash { unHash = 7 } } 0
                            , TxOut
                                { addr = Addr (VKey Owner { unOwner = 7 })
                                , value = Lovelace { unLovelace = 6863 }
                                }
                            )
                          , ( TxIn TxId { getTxId = Hash { unHash = 8 } } 0
                            , TxOut
                                { addr = Addr (VKey Owner { unOwner = 8 })
                                , value = Lovelace { unLovelace = 8623 }
                                }
                            )
                          , ( TxIn TxId { getTxId = Hash { unHash = 9 } } 0
                            , TxOut
                                { addr = Addr (VKey Owner { unOwner = 9 })
                                , value = Lovelace { unLovelace = 7230 }
                                }
                            )
                          ]
                    }
              , reserves = Lovelace { unLovelace = 44999999999948276 }
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
          , ( ( ProtVer { _pvMaj = 1 , _pvMin = 0 , _pvAlt = 0 }
              , PParams
                  { _maxBkSz = 24401
                  , _maxHdrSz = 1083
                  , _maxTxSz = 7588
                  , _maxPropSz = 25
                  , _bkSgnCntT = 2.0713749634593
                  , _bkSlotsPerEpoch = SlotCount { unSlotCount = 10 }
                  , _upTtl = SlotCount { unSlotCount = 14 }
                  , _scriptVersion = 1
                  , _upAdptThd = 0.3315717672316232
                  , _factorA = 8
                  , _factorB = 0
                  }
              )
            , [ ( Slot { unSlot = 89 }
                , ( ProtVer { _pvMaj = 1 , _pvMin = 1 , _pvAlt = 0 }
                  , PParams
                      { _maxBkSz = 43200
                      , _maxHdrSz = 1453
                      , _maxTxSz = 8874
                      , _maxPropSz = 41
                      , _bkSgnCntT = 2.07902716432624
                      , _bkSlotsPerEpoch = SlotCount { unSlotCount = 10 }
                      , _upTtl = SlotCount { unSlotCount = 20 }
                      , _scriptVersion = 1
                      , _upAdptThd = 0.5500429331445129
                      , _factorA = 3
                      , _factorB = 6
                      }
                  )
                )
              ]
            , fromList
                [ ( ApName "3DK" , ( ApVer 1 , Slot { unSlot = 86 } , Metadata ) )
                ]
            , fromList
                [ ( UpId 7
                  , ( ProtVer { _pvMaj = 1 , _pvMin = 1 , _pvAlt = 0 }
                    , PParams
                        { _maxBkSz = 43200
                        , _maxHdrSz = 1453
                        , _maxTxSz = 8874
                        , _maxPropSz = 41
                        , _bkSgnCntT = 2.07902716432624
                        , _bkSlotsPerEpoch = SlotCount { unSlotCount = 10 }
                        , _upTtl = SlotCount { unSlotCount = 20 }
                        , _scriptVersion = 1
                        , _upAdptThd = 0.5500429331445129
                        , _factorA = 3
                        , _factorB = 6
                        }
                    )
                  )
                ]
            , fromList []
            , fromList [ ( UpId 7 , Slot { unSlot = 82 } ) ]
            , fromList
                [ ( UpId 7
                  , VKeyGenesis { unVKeyGenesis = VKey Owner { unOwner = 0 } }
                  )
                ]
            , fromList
                [ ( ProtVer { _pvMaj = 1 , _pvMin = 1 , _pvAlt = 0 }
                  , VKeyGenesis { unVKeyGenesis = VKey Owner { unOwner = 0 } }
                  )
                ]
            , fromList [ ( UpId 7 , Slot { unSlot = 82 } ) ]
            )
          )
        , Block
            { _bHeader =
                MkBlockHeader
                  { _bhPrevHash = Hash { unHash = 1466883654095282019 }
                  , _bhSlot = Slot { unSlot = 93 }
                  , _bhIssuer = VKey Owner { unOwner = 0 }
                  , _bhSig =
                      Sig Hash { unHash = -8184217684611921972 } Owner { unOwner = 0 }
                  , _bhUtxoHash = Hash { unHash = 839657738087498284 }
                  , _bhDlgHash = Hash { unHash = 839657738087498284 }
                  , _bhUpdHash = Hash { unHash = 0 }
                  }
            , _bBody =
                BlockBody
                  { _bDCerts = []
                  , _bUtxo = []
                  , _bUpdProp = Nothing
                  , _bUpdVotes = []
                  , _bProtVer = ProtVer { _pvMaj = 1 , _pvMin = 1 , _pvAlt = 0 }
                  }
            }
        )
      , ( ( Slot { unSlot = 89 }
          , fromList
              [ VKeyGenesis { unVKeyGenesis = VKey Owner { unOwner = 0 } }
              , VKeyGenesis { unVKeyGenesis = VKey Owner { unOwner = 0 } }
              ]
          , Hash { unHash = 1466883654095282019 }
          , UTxOState
              { utxo =
                  UTxO
                    { unUTxO =
                        fromList
                          [ ( TxIn TxId { getTxId = Hash { unHash = 0 } } 0
                            , TxOut
                                { addr = Addr (VKey Owner { unOwner = 0 })
                                , value = Lovelace { unLovelace = 6733 }
                                }
                            )
                          , ( TxIn TxId { getTxId = Hash { unHash = 1 } } 0
                            , TxOut
                                { addr = Addr (VKey Owner { unOwner = 1 })
                                , value = Lovelace { unLovelace = 4795 }
                                }
                            )
                          , ( TxIn TxId { getTxId = Hash { unHash = 2 } } 0
                            , TxOut
                                { addr = Addr (VKey Owner { unOwner = 2 })
                                , value = Lovelace { unLovelace = 8020 }
                                }
                            )
                          , ( TxIn TxId { getTxId = Hash { unHash = 3 } } 0
                            , TxOut
                                { addr = Addr (VKey Owner { unOwner = 3 })
                                , value = Lovelace { unLovelace = 1631 }
                                }
                            )
                          , ( TxIn TxId { getTxId = Hash { unHash = 4 } } 0
                            , TxOut
                                { addr = Addr (VKey Owner { unOwner = 4 })
                                , value = Lovelace { unLovelace = 7829 }
                                }
                            )
                          , ( TxIn TxId { getTxId = Hash { unHash = 7 } } 0
                            , TxOut
                                { addr = Addr (VKey Owner { unOwner = 7 })
                                , value = Lovelace { unLovelace = 6863 }
                                }
                            )
                          , ( TxIn TxId { getTxId = Hash { unHash = 8 } } 0
                            , TxOut
                                { addr = Addr (VKey Owner { unOwner = 8 })
                                , value = Lovelace { unLovelace = 8623 }
                                }
                            )
                          , ( TxIn TxId { getTxId = Hash { unHash = 9 } } 0
                            , TxOut
                                { addr = Addr (VKey Owner { unOwner = 9 })
                                , value = Lovelace { unLovelace = 7230 }
                                }
                            )
                          ]
                    }
              , reserves = Lovelace { unLovelace = 44999999999948276 }
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
          , ( ( ProtVer { _pvMaj = 1 , _pvMin = 0 , _pvAlt = 0 }
              , PParams
                  { _maxBkSz = 24401
                  , _maxHdrSz = 1083
                  , _maxTxSz = 7588
                  , _maxPropSz = 25
                  , _bkSgnCntT = 2.0713749634593
                  , _bkSlotsPerEpoch = SlotCount { unSlotCount = 10 }
                  , _upTtl = SlotCount { unSlotCount = 14 }
                  , _scriptVersion = 1
                  , _upAdptThd = 0.3315717672316232
                  , _factorA = 8
                  , _factorB = 0
                  }
              )
            , [ ( Slot { unSlot = 89 }
                , ( ProtVer { _pvMaj = 1 , _pvMin = 1 , _pvAlt = 0 }
                  , PParams
                      { _maxBkSz = 43200
                      , _maxHdrSz = 1453
                      , _maxTxSz = 8874
                      , _maxPropSz = 41
                      , _bkSgnCntT = 2.07902716432624
                      , _bkSlotsPerEpoch = SlotCount { unSlotCount = 10 }
                      , _upTtl = SlotCount { unSlotCount = 20 }
                      , _scriptVersion = 1
                      , _upAdptThd = 0.5500429331445129
                      , _factorA = 3
                      , _factorB = 6
                      }
                  )
                )
              ]
            , fromList
                [ ( ApName "3DK" , ( ApVer 1 , Slot { unSlot = 86 } , Metadata ) )
                ]
            , fromList
                [ ( UpId 7
                  , ( ProtVer { _pvMaj = 1 , _pvMin = 1 , _pvAlt = 0 }
                    , PParams
                        { _maxBkSz = 43200
                        , _maxHdrSz = 1453
                        , _maxTxSz = 8874
                        , _maxPropSz = 41
                        , _bkSgnCntT = 2.07902716432624
                        , _bkSlotsPerEpoch = SlotCount { unSlotCount = 10 }
                        , _upTtl = SlotCount { unSlotCount = 20 }
                        , _scriptVersion = 1
                        , _upAdptThd = 0.5500429331445129
                        , _factorA = 3
                        , _factorB = 6
                        }
                    )
                  )
                ]
            , fromList []
            , fromList [ ( UpId 7 , Slot { unSlot = 82 } ) ]
            , fromList
                [ ( UpId 7
                  , VKeyGenesis { unVKeyGenesis = VKey Owner { unOwner = 0 } }
                  )
                ]
            , fromList
                [ ( ProtVer { _pvMaj = 1 , _pvMin = 1 , _pvAlt = 0 }
                  , VKeyGenesis { unVKeyGenesis = VKey Owner { unOwner = 0 } }
                  )
                ]
            , fromList [ ( UpId 7 , Slot { unSlot = 82 } ) ]
            )
          )
        , Block
            { _bHeader =
                MkBlockHeader
                  { _bhPrevHash = Hash { unHash = -3462860178553108040 }
                  , _bhSlot = Slot { unSlot = 89 }
                  , _bhIssuer = VKey Owner { unOwner = 0 }
                  , _bhSig =
                      Sig Hash { unHash = 1466883654095282019 } Owner { unOwner = 0 }
                  , _bhUtxoHash = Hash { unHash = 839657738087498284 }
                  , _bhDlgHash = Hash { unHash = 839657738087498284 }
                  , _bhUpdHash = Hash { unHash = 0 }
                  }
            , _bBody =
                BlockBody
                  { _bDCerts = []
                  , _bUtxo = []
                  , _bUpdProp = Nothing
                  , _bUpdVotes = []
                  , _bProtVer = ProtVer { _pvMaj = 1 , _pvMin = 1 , _pvAlt = 0 }
                  }
            }
        )
      , ( ( Slot { unSlot = 86 }
          , fromList
              [ VKeyGenesis { unVKeyGenesis = VKey Owner { unOwner = 0 } }
              , VKeyGenesis { unVKeyGenesis = VKey Owner { unOwner = 0 } }
              ]
          , Hash { unHash = -3462860178553108040 }
          , UTxOState
              { utxo =
                  UTxO
                    { unUTxO =
                        fromList
                          [ ( TxIn TxId { getTxId = Hash { unHash = 0 } } 0
                            , TxOut
                                { addr = Addr (VKey Owner { unOwner = 0 })
                                , value = Lovelace { unLovelace = 6733 }
                                }
                            )
                          , ( TxIn TxId { getTxId = Hash { unHash = 1 } } 0
                            , TxOut
                                { addr = Addr (VKey Owner { unOwner = 1 })
                                , value = Lovelace { unLovelace = 4795 }
                                }
                            )
                          , ( TxIn TxId { getTxId = Hash { unHash = 2 } } 0
                            , TxOut
                                { addr = Addr (VKey Owner { unOwner = 2 })
                                , value = Lovelace { unLovelace = 8020 }
                                }
                            )
                          , ( TxIn TxId { getTxId = Hash { unHash = 3 } } 0
                            , TxOut
                                { addr = Addr (VKey Owner { unOwner = 3 })
                                , value = Lovelace { unLovelace = 1631 }
                                }
                            )
                          , ( TxIn TxId { getTxId = Hash { unHash = 4 } } 0
                            , TxOut
                                { addr = Addr (VKey Owner { unOwner = 4 })
                                , value = Lovelace { unLovelace = 7829 }
                                }
                            )
                          , ( TxIn TxId { getTxId = Hash { unHash = 7 } } 0
                            , TxOut
                                { addr = Addr (VKey Owner { unOwner = 7 })
                                , value = Lovelace { unLovelace = 6863 }
                                }
                            )
                          , ( TxIn TxId { getTxId = Hash { unHash = 8 } } 0
                            , TxOut
                                { addr = Addr (VKey Owner { unOwner = 8 })
                                , value = Lovelace { unLovelace = 8623 }
                                }
                            )
                          , ( TxIn TxId { getTxId = Hash { unHash = 9 } } 0
                            , TxOut
                                { addr = Addr (VKey Owner { unOwner = 9 })
                                , value = Lovelace { unLovelace = 7230 }
                                }
                            )
                          ]
                    }
              , reserves = Lovelace { unLovelace = 44999999999948276 }
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
          , ( ( ProtVer { _pvMaj = 1 , _pvMin = 0 , _pvAlt = 0 }
              , PParams
                  { _maxBkSz = 24401
                  , _maxHdrSz = 1083
                  , _maxTxSz = 7588
                  , _maxPropSz = 25
                  , _bkSgnCntT = 2.0713749634593
                  , _bkSlotsPerEpoch = SlotCount { unSlotCount = 10 }
                  , _upTtl = SlotCount { unSlotCount = 14 }
                  , _scriptVersion = 1
                  , _upAdptThd = 0.3315717672316232
                  , _factorA = 8
                  , _factorB = 0
                  }
              )
            , []
            , fromList
                [ ( ApName "3DK" , ( ApVer 1 , Slot { unSlot = 86 } , Metadata ) )
                ]
            , fromList
                [ ( UpId 7
                  , ( ProtVer { _pvMaj = 1 , _pvMin = 1 , _pvAlt = 0 }
                    , PParams
                        { _maxBkSz = 43200
                        , _maxHdrSz = 1453
                        , _maxTxSz = 8874
                        , _maxPropSz = 41
                        , _bkSgnCntT = 2.07902716432624
                        , _bkSlotsPerEpoch = SlotCount { unSlotCount = 10 }
                        , _upTtl = SlotCount { unSlotCount = 20 }
                        , _scriptVersion = 1
                        , _upAdptThd = 0.5500429331445129
                        , _factorA = 3
                        , _factorB = 6
                        }
                    )
                  )
                ]
            , fromList []
            , fromList [ ( UpId 7 , Slot { unSlot = 82 } ) ]
            , fromList
                [ ( UpId 7
                  , VKeyGenesis { unVKeyGenesis = VKey Owner { unOwner = 0 } }
                  )
                ]
            , fromList []
            , fromList [ ( UpId 7 , Slot { unSlot = 82 } ) ]
            )
          )
        , Block
            { _bHeader =
                MkBlockHeader
                  { _bhPrevHash = Hash { unHash = 8872530134924202890 }
                  , _bhSlot = Slot { unSlot = 86 }
                  , _bhIssuer = VKey Owner { unOwner = 0 }
                  , _bhSig =
                      Sig Hash { unHash = -3462860178553108040 } Owner { unOwner = 0 }
                  , _bhUtxoHash = Hash { unHash = 839657738087498284 }
                  , _bhDlgHash = Hash { unHash = 839657738087498284 }
                  , _bhUpdHash = Hash { unHash = 0 }
                  }
            , _bBody =
                BlockBody
                  { _bDCerts = []
                  , _bUtxo = []
                  , _bUpdProp = Nothing
                  , _bUpdVotes = []
                  , _bProtVer = ProtVer { _pvMaj = 1 , _pvMin = 0 , _pvAlt = 0 }
                  }
            }
        )
      , ( ( Slot { unSlot = 83 }
          , fromList
              [ VKeyGenesis { unVKeyGenesis = VKey Owner { unOwner = 0 } }
              , VKeyGenesis { unVKeyGenesis = VKey Owner { unOwner = 0 } }
              ]
          , Hash { unHash = 8872530134924202890 }
          , UTxOState
              { utxo =
                  UTxO
                    { unUTxO =
                        fromList
                          [ ( TxIn TxId { getTxId = Hash { unHash = 0 } } 0
                            , TxOut
                                { addr = Addr (VKey Owner { unOwner = 0 })
                                , value = Lovelace { unLovelace = 6733 }
                                }
                            )
                          , ( TxIn TxId { getTxId = Hash { unHash = 1 } } 0
                            , TxOut
                                { addr = Addr (VKey Owner { unOwner = 1 })
                                , value = Lovelace { unLovelace = 4795 }
                                }
                            )
                          , ( TxIn TxId { getTxId = Hash { unHash = 2 } } 0
                            , TxOut
                                { addr = Addr (VKey Owner { unOwner = 2 })
                                , value = Lovelace { unLovelace = 8020 }
                                }
                            )
                          , ( TxIn TxId { getTxId = Hash { unHash = 3 } } 0
                            , TxOut
                                { addr = Addr (VKey Owner { unOwner = 3 })
                                , value = Lovelace { unLovelace = 1631 }
                                }
                            )
                          , ( TxIn TxId { getTxId = Hash { unHash = 4 } } 0
                            , TxOut
                                { addr = Addr (VKey Owner { unOwner = 4 })
                                , value = Lovelace { unLovelace = 7829 }
                                }
                            )
                          , ( TxIn TxId { getTxId = Hash { unHash = 7 } } 0
                            , TxOut
                                { addr = Addr (VKey Owner { unOwner = 7 })
                                , value = Lovelace { unLovelace = 6863 }
                                }
                            )
                          , ( TxIn TxId { getTxId = Hash { unHash = 8 } } 0
                            , TxOut
                                { addr = Addr (VKey Owner { unOwner = 8 })
                                , value = Lovelace { unLovelace = 8623 }
                                }
                            )
                          , ( TxIn TxId { getTxId = Hash { unHash = 9 } } 0
                            , TxOut
                                { addr = Addr (VKey Owner { unOwner = 9 })
                                , value = Lovelace { unLovelace = 7230 }
                                }
                            )
                          ]
                    }
              , reserves = Lovelace { unLovelace = 44999999999948276 }
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
          , ( ( ProtVer { _pvMaj = 1 , _pvMin = 0 , _pvAlt = 0 }
              , PParams
                  { _maxBkSz = 24401
                  , _maxHdrSz = 1083
                  , _maxTxSz = 7588
                  , _maxPropSz = 25
                  , _bkSgnCntT = 2.0713749634593
                  , _bkSlotsPerEpoch = SlotCount { unSlotCount = 10 }
                  , _upTtl = SlotCount { unSlotCount = 14 }
                  , _scriptVersion = 1
                  , _upAdptThd = 0.3315717672316232
                  , _factorA = 8
                  , _factorB = 0
                  }
              )
            , []
            , fromList
                [ ( ApName "3DK" , ( ApVer 0 , Slot { unSlot = 43 } , Metadata ) )
                ]
            , fromList
                [ ( UpId 7
                  , ( ProtVer { _pvMaj = 1 , _pvMin = 1 , _pvAlt = 0 }
                    , PParams
                        { _maxBkSz = 43200
                        , _maxHdrSz = 1453
                        , _maxTxSz = 8874
                        , _maxPropSz = 41
                        , _bkSgnCntT = 2.07902716432624
                        , _bkSlotsPerEpoch = SlotCount { unSlotCount = 10 }
                        , _upTtl = SlotCount { unSlotCount = 20 }
                        , _scriptVersion = 1
                        , _upAdptThd = 0.5500429331445129
                        , _factorA = 3
                        , _factorB = 6
                        }
                    )
                  )
                ]
            , fromList [ ( UpId 7 , ( ApName "3DK" , ApVer 1 , Metadata ) ) ]
            , fromList [ ( UpId 7 , Slot { unSlot = 82 } ) ]
            , fromList
                [ ( UpId 7
                  , VKeyGenesis { unVKeyGenesis = VKey Owner { unOwner = 0 } }
                  )
                ]
            , fromList []
            , fromList [ ( UpId 7 , Slot { unSlot = 82 } ) ]
            )
          )
        , Block
            { _bHeader =
                MkBlockHeader
                  { _bhPrevHash = Hash { unHash = 7188431154727439823 }
                  , _bhSlot = Slot { unSlot = 83 }
                  , _bhIssuer = VKey Owner { unOwner = 0 }
                  , _bhSig =
                      Sig Hash { unHash = 8872530134924202890 } Owner { unOwner = 0 }
                  , _bhUtxoHash = Hash { unHash = 839657738087498284 }
                  , _bhDlgHash = Hash { unHash = 839657738087498284 }
                  , _bhUpdHash = Hash { unHash = 0 }
                  }
            , _bBody =
                BlockBody
                  { _bDCerts = []
                  , _bUtxo = []
                  , _bUpdProp = Nothing
                  , _bUpdVotes = []
                  , _bProtVer = ProtVer { _pvMaj = 1 , _pvMin = 0 , _pvAlt = 0 }
                  }
            }
        )
      , ( ( Slot { unSlot = 82 }
          , fromList
              [ VKeyGenesis { unVKeyGenesis = VKey Owner { unOwner = 0 } }
              , VKeyGenesis { unVKeyGenesis = VKey Owner { unOwner = 0 } }
              ]
          , Hash { unHash = 7188431154727439823 }
          , UTxOState
              { utxo =
                  UTxO
                    { unUTxO =
                        fromList
                          [ ( TxIn TxId { getTxId = Hash { unHash = 0 } } 0
                            , TxOut
                                { addr = Addr (VKey Owner { unOwner = 0 })
                                , value = Lovelace { unLovelace = 6733 }
                                }
                            )
                          , ( TxIn TxId { getTxId = Hash { unHash = 1 } } 0
                            , TxOut
                                { addr = Addr (VKey Owner { unOwner = 1 })
                                , value = Lovelace { unLovelace = 4795 }
                                }
                            )
                          , ( TxIn TxId { getTxId = Hash { unHash = 2 } } 0
                            , TxOut
                                { addr = Addr (VKey Owner { unOwner = 2 })
                                , value = Lovelace { unLovelace = 8020 }
                                }
                            )
                          , ( TxIn TxId { getTxId = Hash { unHash = 3 } } 0
                            , TxOut
                                { addr = Addr (VKey Owner { unOwner = 3 })
                                , value = Lovelace { unLovelace = 1631 }
                                }
                            )
                          , ( TxIn TxId { getTxId = Hash { unHash = 4 } } 0
                            , TxOut
                                { addr = Addr (VKey Owner { unOwner = 4 })
                                , value = Lovelace { unLovelace = 7829 }
                                }
                            )
                          , ( TxIn TxId { getTxId = Hash { unHash = 7 } } 0
                            , TxOut
                                { addr = Addr (VKey Owner { unOwner = 7 })
                                , value = Lovelace { unLovelace = 6863 }
                                }
                            )
                          , ( TxIn TxId { getTxId = Hash { unHash = 8 } } 0
                            , TxOut
                                { addr = Addr (VKey Owner { unOwner = 8 })
                                , value = Lovelace { unLovelace = 8623 }
                                }
                            )
                          , ( TxIn TxId { getTxId = Hash { unHash = 9 } } 0
                            , TxOut
                                { addr = Addr (VKey Owner { unOwner = 9 })
                                , value = Lovelace { unLovelace = 7230 }
                                }
                            )
                          ]
                    }
              , reserves = Lovelace { unLovelace = 44999999999948276 }
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
          , ( ( ProtVer { _pvMaj = 1 , _pvMin = 0 , _pvAlt = 0 }
              , PParams
                  { _maxBkSz = 24401
                  , _maxHdrSz = 1083
                  , _maxTxSz = 7588
                  , _maxPropSz = 25
                  , _bkSgnCntT = 2.0713749634593
                  , _bkSlotsPerEpoch = SlotCount { unSlotCount = 10 }
                  , _upTtl = SlotCount { unSlotCount = 14 }
                  , _scriptVersion = 1
                  , _upAdptThd = 0.3315717672316232
                  , _factorA = 8
                  , _factorB = 0
                  }
              )
            , []
            , fromList
                [ ( ApName "3DK" , ( ApVer 0 , Slot { unSlot = 43 } , Metadata ) )
                ]
            , fromList
                [ ( UpId 7
                  , ( ProtVer { _pvMaj = 1 , _pvMin = 1 , _pvAlt = 0 }
                    , PParams
                        { _maxBkSz = 43200
                        , _maxHdrSz = 1453
                        , _maxTxSz = 8874
                        , _maxPropSz = 41
                        , _bkSgnCntT = 2.07902716432624
                        , _bkSlotsPerEpoch = SlotCount { unSlotCount = 10 }
                        , _upTtl = SlotCount { unSlotCount = 20 }
                        , _scriptVersion = 1
                        , _upAdptThd = 0.5500429331445129
                        , _factorA = 3
                        , _factorB = 6
                        }
                    )
                  )
                ]
            -- This is wrong!!!!! At slot 80 the UP expires, and we shouldn't
            -- see this since the block we applied (@ slot 82, just proposes to
            -- leave things as is!)
            , fromList [ ( UpId 7 , ( ApName "3DK" , ApVer 1 , Metadata ) ) ]
            , fromList [ ( UpId 7 , Slot { unSlot = 82 } ) ]
            , fromList
                [ ( UpId 7
                  , VKeyGenesis { unVKeyGenesis = VKey Owner { unOwner = 0 } }
                  )
                ]
            , fromList []
            , fromList [ ( UpId 7 , Slot { unSlot = 82 } ) ]
            )
          )
        , Block
            { _bHeader =
                MkBlockHeader
                  { _bhPrevHash = Hash { unHash = 2457170027119863869 }
                  , _bhSlot = Slot { unSlot = 82 }
                  , _bhIssuer = VKey Owner { unOwner = 0 }
                  , _bhSig =
                      Sig Hash { unHash = 7188431154727439823 } Owner { unOwner = 0 }
                  , _bhUtxoHash = Hash { unHash = 839657738087498284 }
                  , _bhDlgHash = Hash { unHash = 839657738087498284 }
                  , _bhUpdHash = Hash { unHash = -5478374611058476079 }
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
                                { _maxBkSz = 43200
                                , _maxHdrSz = 1453
                                , _maxTxSz = 8874
                                , _maxPropSz = 41
                                , _bkSgnCntT = 2.07902716432624
                                , _bkSlotsPerEpoch = SlotCount { unSlotCount = 10 }
                                , _upTtl = SlotCount { unSlotCount = 20 }
                                , _scriptVersion = 1
                                , _upAdptThd = 0.5500429331445129
                                , _factorA = 3
                                , _factorB = 6
                                }
                          , _upPV = ProtVer { _pvMaj = 1 , _pvMin = 1 , _pvAlt = 0 }
                          , _upSwVer = SwVer { _svName = ApName "3DK" , _svVer = ApVer 0 }
                          , _upSig =
                              Sig
                                ( ProtVer { _pvMaj = 1 , _pvMin = 1 , _pvAlt = 0 }
                                , PParams
                                    { _maxBkSz = 43200
                                    , _maxHdrSz = 1453
                                    , _maxTxSz = 8874
                                    , _maxPropSz = 41
                                    , _bkSgnCntT = 2.07902716432624
                                    , _bkSlotsPerEpoch = SlotCount { unSlotCount = 10 }
                                    , _upTtl = SlotCount { unSlotCount = 20 }
                                    , _scriptVersion = 1
                                    , _upAdptThd = 0.5500429331445129
                                    , _factorA = 3
                                    , _factorB = 6
                                    }
                                , SwVer { _svName = ApName "3DK" , _svVer = ApVer 0 }
                                )
                                Owner { unOwner = 0 }
                          , _upSTags =
                              fromList [ "\t8tD?" , "%EYp_\EOT: =\ETX" , "o\DC4\ESC" , "vJ" ]
                          , _upMdt = Metadata
                          }
                  , _bUpdVotes =
                      [ Vote
                          { _vCaster = VKey Owner { unOwner = 0 }
                          , _vPropId = UpId 7
                          , _vSig = Sig (UpId 7) Owner { unOwner = 0 }
                          }
                      ]
                  , _bProtVer = ProtVer { _pvMaj = 1 , _pvMin = 0 , _pvAlt = 0 }
                  }
            }
        )
      , ( ( Slot { unSlot = 79 }
          , fromList
              [ VKeyGenesis { unVKeyGenesis = VKey Owner { unOwner = 0 } }
              , VKeyGenesis { unVKeyGenesis = VKey Owner { unOwner = 0 } }
              ]
          , Hash { unHash = 2457170027119863869 }
          , UTxOState
              { utxo =
                  UTxO
                    { unUTxO =
                        fromList
                          [ ( TxIn TxId { getTxId = Hash { unHash = 0 } } 0
                            , TxOut
                                { addr = Addr (VKey Owner { unOwner = 0 })
                                , value = Lovelace { unLovelace = 6733 }
                                }
                            )
                          , ( TxIn TxId { getTxId = Hash { unHash = 1 } } 0
                            , TxOut
                                { addr = Addr (VKey Owner { unOwner = 1 })
                                , value = Lovelace { unLovelace = 4795 }
                                }
                            )
                          , ( TxIn TxId { getTxId = Hash { unHash = 2 } } 0
                            , TxOut
                                { addr = Addr (VKey Owner { unOwner = 2 })
                                , value = Lovelace { unLovelace = 8020 }
                                }
                            )
                          , ( TxIn TxId { getTxId = Hash { unHash = 3 } } 0
                            , TxOut
                                { addr = Addr (VKey Owner { unOwner = 3 })
                                , value = Lovelace { unLovelace = 1631 }
                                }
                            )
                          , ( TxIn TxId { getTxId = Hash { unHash = 4 } } 0
                            , TxOut
                                { addr = Addr (VKey Owner { unOwner = 4 })
                                , value = Lovelace { unLovelace = 7829 }
                                }
                            )
                          , ( TxIn TxId { getTxId = Hash { unHash = 7 } } 0
                            , TxOut
                                { addr = Addr (VKey Owner { unOwner = 7 })
                                , value = Lovelace { unLovelace = 6863 }
                                }
                            )
                          , ( TxIn TxId { getTxId = Hash { unHash = 8 } } 0
                            , TxOut
                                { addr = Addr (VKey Owner { unOwner = 8 })
                                , value = Lovelace { unLovelace = 8623 }
                                }
                            )
                          , ( TxIn TxId { getTxId = Hash { unHash = 9 } } 0
                            , TxOut
                                { addr = Addr (VKey Owner { unOwner = 9 })
                                , value = Lovelace { unLovelace = 7230 }
                                }
                            )
                          ]
                    }
              , reserves = Lovelace { unLovelace = 44999999999948276 }
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
          , ( ( ProtVer { _pvMaj = 0 , _pvMin = 1 , _pvAlt = 0 }
              , PParams
                  { _maxBkSz = 17739
                  , _maxHdrSz = 993
                  , _maxTxSz = 2021
                  , _maxPropSz = 16
                  , _bkSgnCntT = 2.077941229871697
                  , _bkSlotsPerEpoch = SlotCount { unSlotCount = 10 }
                  , _upTtl = SlotCount { unSlotCount = 20 }
                  , _scriptVersion = 1
                  , _upAdptThd = 0.2614549740866021
                  , _factorA = 2
                  , _factorB = 0
                  }
              )
            , [ ( Slot { unSlot = 73 }
                , ( ProtVer { _pvMaj = 1 , _pvMin = 0 , _pvAlt = 0 }
                  , PParams
                      { _maxBkSz = 24401
                      , _maxHdrSz = 1083
                      , _maxTxSz = 7588
                      , _maxPropSz = 25
                      , _bkSgnCntT = 2.0713749634593
                      , _bkSlotsPerEpoch = SlotCount { unSlotCount = 10 }
                      , _upTtl = SlotCount { unSlotCount = 14 }
                      , _scriptVersion = 1
                      , _upAdptThd = 0.3315717672316232
                      , _factorA = 8
                      , _factorB = 0
                      }
                  )
                )
              ]
            , fromList
                [ ( ApName "3DK" , ( ApVer 0 , Slot { unSlot = 43 } , Metadata ) )
                ]
            , fromList
                [ ( UpId 0
                  , ( ProtVer { _pvMaj = 1 , _pvMin = 0 , _pvAlt = 0 }
                    , PParams
                        { _maxBkSz = 24401
                        , _maxHdrSz = 1083
                        , _maxTxSz = 7588
                        , _maxPropSz = 25
                        , _bkSgnCntT = 2.0713749634593
                        , _bkSlotsPerEpoch = SlotCount { unSlotCount = 10 }
                        , _upTtl = SlotCount { unSlotCount = 14 }
                        , _scriptVersion = 1
                        , _upAdptThd = 0.3315717672316232
                        , _factorA = 8
                        , _factorB = 0
                        }
                    )
                  )
                ]
            , fromList [ ( UpId 7 , ( ApName "3DK" , ApVer 1 , Metadata ) ) ]
            , fromList [ ( UpId 0 , Slot { unSlot = 63 } ) ]
            , fromList
                [ ( UpId 0
                  , VKeyGenesis { unVKeyGenesis = VKey Owner { unOwner = 0 } }
                  )
                ]
            , fromList
                [ ( ProtVer { _pvMaj = 1 , _pvMin = 0 , _pvAlt = 0 }
                  , VKeyGenesis { unVKeyGenesis = VKey Owner { unOwner = 0 } }
                  )
                ]
            , fromList
                [ ( UpId 0 , Slot { unSlot = 60 } )
                , ( UpId 7 , Slot { unSlot = 69 } )
                ]
            )
          )
        , Block
            { _bHeader =
                MkBlockHeader
                  { _bhPrevHash = Hash { unHash = -5176675414016044832 }
                  , _bhSlot = Slot { unSlot = 79 }
                  , _bhIssuer = VKey Owner { unOwner = 0 }
                  , _bhSig =
                      Sig Hash { unHash = 2457170027119863869 } Owner { unOwner = 0 }
                  , _bhUtxoHash = Hash { unHash = 839657738087498284 }
                  , _bhDlgHash = Hash { unHash = 839657738087498284 }
                  , _bhUpdHash = Hash { unHash = 0 }
                  }
            , _bBody =
                BlockBody
                  { _bDCerts = []
                  , _bUtxo = []
                  , _bUpdProp = Nothing
                  , _bUpdVotes = []
                  , _bProtVer = ProtVer { _pvMaj = 1 , _pvMin = 0 , _pvAlt = 0 }
                  }
            }
        )
      , ( ( Slot { unSlot = 73 }
          , fromList
              [ VKeyGenesis { unVKeyGenesis = VKey Owner { unOwner = 0 } }
              , VKeyGenesis { unVKeyGenesis = VKey Owner { unOwner = 0 } }
              ]
          , Hash { unHash = -5176675414016044832 }
          , UTxOState
              { utxo =
                  UTxO
                    { unUTxO =
                        fromList
                          [ ( TxIn TxId { getTxId = Hash { unHash = 0 } } 0
                            , TxOut
                                { addr = Addr (VKey Owner { unOwner = 0 })
                                , value = Lovelace { unLovelace = 6733 }
                                }
                            )
                          , ( TxIn TxId { getTxId = Hash { unHash = 1 } } 0
                            , TxOut
                                { addr = Addr (VKey Owner { unOwner = 1 })
                                , value = Lovelace { unLovelace = 4795 }
                                }
                            )
                          , ( TxIn TxId { getTxId = Hash { unHash = 2 } } 0
                            , TxOut
                                { addr = Addr (VKey Owner { unOwner = 2 })
                                , value = Lovelace { unLovelace = 8020 }
                                }
                            )
                          , ( TxIn TxId { getTxId = Hash { unHash = 3 } } 0
                            , TxOut
                                { addr = Addr (VKey Owner { unOwner = 3 })
                                , value = Lovelace { unLovelace = 1631 }
                                }
                            )
                          , ( TxIn TxId { getTxId = Hash { unHash = 4 } } 0
                            , TxOut
                                { addr = Addr (VKey Owner { unOwner = 4 })
                                , value = Lovelace { unLovelace = 7829 }
                                }
                            )
                          , ( TxIn TxId { getTxId = Hash { unHash = 7 } } 0
                            , TxOut
                                { addr = Addr (VKey Owner { unOwner = 7 })
                                , value = Lovelace { unLovelace = 6863 }
                                }
                            )
                          , ( TxIn TxId { getTxId = Hash { unHash = 8 } } 0
                            , TxOut
                                { addr = Addr (VKey Owner { unOwner = 8 })
                                , value = Lovelace { unLovelace = 8623 }
                                }
                            )
                          , ( TxIn TxId { getTxId = Hash { unHash = 9 } } 0
                            , TxOut
                                { addr = Addr (VKey Owner { unOwner = 9 })
                                , value = Lovelace { unLovelace = 7230 }
                                }
                            )
                          ]
                    }
              , reserves = Lovelace { unLovelace = 44999999999948276 }
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
          , ( ( ProtVer { _pvMaj = 0 , _pvMin = 1 , _pvAlt = 0 }
              , PParams
                  { _maxBkSz = 17739
                  , _maxHdrSz = 993
                  , _maxTxSz = 2021
                  , _maxPropSz = 16
                  , _bkSgnCntT = 2.077941229871697
                  , _bkSlotsPerEpoch = SlotCount { unSlotCount = 10 }
                  , _upTtl = SlotCount { unSlotCount = 20 }
                  , _scriptVersion = 1
                  , _upAdptThd = 0.2614549740866021
                  , _factorA = 2
                  , _factorB = 0
                  }
              )
            , [ ( Slot { unSlot = 73 }
                , ( ProtVer { _pvMaj = 1 , _pvMin = 0 , _pvAlt = 0 }
                  , PParams
                      { _maxBkSz = 24401
                      , _maxHdrSz = 1083
                      , _maxTxSz = 7588
                      , _maxPropSz = 25
                      , _bkSgnCntT = 2.0713749634593
                      , _bkSlotsPerEpoch = SlotCount { unSlotCount = 10 }
                      , _upTtl = SlotCount { unSlotCount = 14 }
                      , _scriptVersion = 1
                      , _upAdptThd = 0.3315717672316232
                      , _factorA = 8
                      , _factorB = 0
                      }
                  )
                )
              ]
            , fromList
                [ ( ApName "3DK" , ( ApVer 0 , Slot { unSlot = 43 } , Metadata ) )
                ]
            , fromList
                [ ( UpId 0
                  , ( ProtVer { _pvMaj = 1 , _pvMin = 0 , _pvAlt = 0 }
                    , PParams
                        { _maxBkSz = 24401
                        , _maxHdrSz = 1083
                        , _maxTxSz = 7588
                        , _maxPropSz = 25
                        , _bkSgnCntT = 2.0713749634593
                        , _bkSlotsPerEpoch = SlotCount { unSlotCount = 10 }
                        , _upTtl = SlotCount { unSlotCount = 14 }
                        , _scriptVersion = 1
                        , _upAdptThd = 0.3315717672316232
                        , _factorA = 8
                        , _factorB = 0
                        }
                    )
                  )
                ]
            , fromList [ ( UpId 7 , ( ApName "3DK" , ApVer 1 , Metadata ) ) ]
            , fromList [ ( UpId 0 , Slot { unSlot = 63 } ) ]
            , fromList
                [ ( UpId 0
                  , VKeyGenesis { unVKeyGenesis = VKey Owner { unOwner = 0 } }
                  )
                ]
            , fromList
                [ ( ProtVer { _pvMaj = 1 , _pvMin = 0 , _pvAlt = 0 }
                  , VKeyGenesis { unVKeyGenesis = VKey Owner { unOwner = 0 } }
                  )
                ]
            , fromList
                [ ( UpId 0 , Slot { unSlot = 60 } )
                , ( UpId 7 , Slot { unSlot = 69 } )
                ]
            )
          )
        , Block
            { _bHeader =
                MkBlockHeader
                  { _bhPrevHash = Hash { unHash = -9110570555792327149 }
                  , _bhSlot = Slot { unSlot = 73 }
                  , _bhIssuer = VKey Owner { unOwner = 0 }
                  , _bhSig =
                      Sig Hash { unHash = -5176675414016044832 } Owner { unOwner = 0 }
                  , _bhUtxoHash = Hash { unHash = 839657738087498284 }
                  , _bhDlgHash = Hash { unHash = 839657738087498284 }
                  , _bhUpdHash = Hash { unHash = 0 }
                  }
            , _bBody =
                BlockBody
                  { _bDCerts = []
                  , _bUtxo = []
                  , _bUpdProp = Nothing
                  , _bUpdVotes = []
                  , _bProtVer = ProtVer { _pvMaj = 1 , _pvMin = 0 , _pvAlt = 0 }
                  }
            }
        )
      , ( ( Slot { unSlot = 69 }
          , fromList
              [ VKeyGenesis { unVKeyGenesis = VKey Owner { unOwner = 0 } }
              , VKeyGenesis { unVKeyGenesis = VKey Owner { unOwner = 0 } }
              ]
          , Hash { unHash = -9110570555792327149 }
          , UTxOState
              { utxo =
                  UTxO
                    { unUTxO =
                        fromList
                          [ ( TxIn TxId { getTxId = Hash { unHash = 0 } } 0
                            , TxOut
                                { addr = Addr (VKey Owner { unOwner = 0 })
                                , value = Lovelace { unLovelace = 6733 }
                                }
                            )
                          , ( TxIn TxId { getTxId = Hash { unHash = 1 } } 0
                            , TxOut
                                { addr = Addr (VKey Owner { unOwner = 1 })
                                , value = Lovelace { unLovelace = 4795 }
                                }
                            )
                          , ( TxIn TxId { getTxId = Hash { unHash = 2 } } 0
                            , TxOut
                                { addr = Addr (VKey Owner { unOwner = 2 })
                                , value = Lovelace { unLovelace = 8020 }
                                }
                            )
                          , ( TxIn TxId { getTxId = Hash { unHash = 3 } } 0
                            , TxOut
                                { addr = Addr (VKey Owner { unOwner = 3 })
                                , value = Lovelace { unLovelace = 1631 }
                                }
                            )
                          , ( TxIn TxId { getTxId = Hash { unHash = 4 } } 0
                            , TxOut
                                { addr = Addr (VKey Owner { unOwner = 4 })
                                , value = Lovelace { unLovelace = 7829 }
                                }
                            )
                          , ( TxIn TxId { getTxId = Hash { unHash = 7 } } 0
                            , TxOut
                                { addr = Addr (VKey Owner { unOwner = 7 })
                                , value = Lovelace { unLovelace = 6863 }
                                }
                            )
                          , ( TxIn TxId { getTxId = Hash { unHash = 8 } } 0
                            , TxOut
                                { addr = Addr (VKey Owner { unOwner = 8 })
                                , value = Lovelace { unLovelace = 8623 }
                                }
                            )
                          , ( TxIn TxId { getTxId = Hash { unHash = 9 } } 0
                            , TxOut
                                { addr = Addr (VKey Owner { unOwner = 9 })
                                , value = Lovelace { unLovelace = 7230 }
                                }
                            )
                          ]
                    }
              , reserves = Lovelace { unLovelace = 44999999999948276 }
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
          , ( ( ProtVer { _pvMaj = 0 , _pvMin = 1 , _pvAlt = 0 }
              , PParams
                  { _maxBkSz = 17739
                  , _maxHdrSz = 993
                  , _maxTxSz = 2021
                  , _maxPropSz = 16
                  , _bkSgnCntT = 2.077941229871697
                  , _bkSlotsPerEpoch = SlotCount { unSlotCount = 10 }
                  , _upTtl = SlotCount { unSlotCount = 20 }
                  , _scriptVersion = 1
                  , _upAdptThd = 0.2614549740866021
                  , _factorA = 2
                  , _factorB = 0
                  }
              )
            , []
            , fromList
                [ ( ApName "3DK" , ( ApVer 0 , Slot { unSlot = 43 } , Metadata ) )
                ]
            , fromList
                [ ( UpId 0
                  , ( ProtVer { _pvMaj = 1 , _pvMin = 0 , _pvAlt = 0 }
                    , PParams
                        { _maxBkSz = 24401
                        , _maxHdrSz = 1083
                        , _maxTxSz = 7588
                        , _maxPropSz = 25
                        , _bkSgnCntT = 2.0713749634593
                        , _bkSlotsPerEpoch = SlotCount { unSlotCount = 10 }
                        , _upTtl = SlotCount { unSlotCount = 14 }
                        , _scriptVersion = 1
                        , _upAdptThd = 0.3315717672316232
                        , _factorA = 8
                        , _factorB = 0
                        }
                    )
                  )
                ]
            , fromList [ ( UpId 7 , ( ApName "3DK" , ApVer 1 , Metadata ) ) ]
            , fromList [ ( UpId 0 , Slot { unSlot = 63 } ) ]
            , fromList
                [ ( UpId 0
                  , VKeyGenesis { unVKeyGenesis = VKey Owner { unOwner = 0 } }
                  )
                ]
            , fromList []
            , fromList
                [ ( UpId 0 , Slot { unSlot = 60 } )
                , ( UpId 7 , Slot { unSlot = 69 } )
                ]
            )
          )
        , Block
            { _bHeader =
                MkBlockHeader
                  { _bhPrevHash = Hash { unHash = -119653622106078804 }
                  , _bhSlot = Slot { unSlot = 69 }
                  , _bhIssuer = VKey Owner { unOwner = 0 }
                  , _bhSig =
                      Sig Hash { unHash = -9110570555792327149 } Owner { unOwner = 0 }
                  , _bhUtxoHash = Hash { unHash = 839657738087498284 }
                  , _bhDlgHash = Hash { unHash = 839657738087498284 }
                  , _bhUpdHash = Hash { unHash = 6743822580465775223 }
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
                                { _maxBkSz = 17739
                                , _maxHdrSz = 993
                                , _maxTxSz = 2021
                                , _maxPropSz = 16
                                , _bkSgnCntT = 2.077941229871697
                                , _bkSlotsPerEpoch = SlotCount { unSlotCount = 10 }
                                , _upTtl = SlotCount { unSlotCount = 20 }
                                , _scriptVersion = 1
                                , _upAdptThd = 0.2614549740866021
                                , _factorA = 2
                                , _factorB = 0
                                }
                          , _upPV = ProtVer { _pvMaj = 0 , _pvMin = 1 , _pvAlt = 0 }
                          , _upSwVer = SwVer { _svName = ApName "3DK" , _svVer = ApVer 1 }
                          , _upSig =
                              Sig
                                ( ProtVer { _pvMaj = 0 , _pvMin = 1 , _pvAlt = 0 }
                                , PParams
                                    { _maxBkSz = 17739
                                    , _maxHdrSz = 993
                                    , _maxTxSz = 2021
                                    , _maxPropSz = 16
                                    , _bkSgnCntT = 2.077941229871697
                                    , _bkSlotsPerEpoch = SlotCount { unSlotCount = 10 }
                                    , _upTtl = SlotCount { unSlotCount = 20 }
                                    , _scriptVersion = 1
                                    , _upAdptThd = 0.2614549740866021
                                    , _factorA = 2
                                    , _factorB = 0
                                    }
                                , SwVer { _svName = ApName "3DK" , _svVer = ApVer 1 }
                                )
                                Owner { unOwner = 0 }
                          , _upSTags = fromList [ "\NUL\DC1vN93+D" , "ex{F" , "hJ" ]
                          , _upMdt = Metadata
                          }
                  , _bUpdVotes = []
                  , _bProtVer = ProtVer { _pvMaj = 0 , _pvMin = 1 , _pvAlt = 0 }
                  }
            }
        )
      , ( ( Slot { unSlot = 66 }
          , fromList
              [ VKeyGenesis { unVKeyGenesis = VKey Owner { unOwner = 0 } }
              , VKeyGenesis { unVKeyGenesis = VKey Owner { unOwner = 0 } }
              ]
          , Hash { unHash = -119653622106078804 }
          , UTxOState
              { utxo =
                  UTxO
                    { unUTxO =
                        fromList
                          [ ( TxIn TxId { getTxId = Hash { unHash = 0 } } 0
                            , TxOut
                                { addr = Addr (VKey Owner { unOwner = 0 })
                                , value = Lovelace { unLovelace = 6733 }
                                }
                            )
                          , ( TxIn TxId { getTxId = Hash { unHash = 1 } } 0
                            , TxOut
                                { addr = Addr (VKey Owner { unOwner = 1 })
                                , value = Lovelace { unLovelace = 4795 }
                                }
                            )
                          , ( TxIn TxId { getTxId = Hash { unHash = 2 } } 0
                            , TxOut
                                { addr = Addr (VKey Owner { unOwner = 2 })
                                , value = Lovelace { unLovelace = 8020 }
                                }
                            )
                          , ( TxIn TxId { getTxId = Hash { unHash = 3 } } 0
                            , TxOut
                                { addr = Addr (VKey Owner { unOwner = 3 })
                                , value = Lovelace { unLovelace = 1631 }
                                }
                            )
                          , ( TxIn TxId { getTxId = Hash { unHash = 4 } } 0
                            , TxOut
                                { addr = Addr (VKey Owner { unOwner = 4 })
                                , value = Lovelace { unLovelace = 7829 }
                                }
                            )
                          , ( TxIn TxId { getTxId = Hash { unHash = 7 } } 0
                            , TxOut
                                { addr = Addr (VKey Owner { unOwner = 7 })
                                , value = Lovelace { unLovelace = 6863 }
                                }
                            )
                          , ( TxIn TxId { getTxId = Hash { unHash = 8 } } 0
                            , TxOut
                                { addr = Addr (VKey Owner { unOwner = 8 })
                                , value = Lovelace { unLovelace = 8623 }
                                }
                            )
                          , ( TxIn TxId { getTxId = Hash { unHash = 9 } } 0
                            , TxOut
                                { addr = Addr (VKey Owner { unOwner = 9 })
                                , value = Lovelace { unLovelace = 7230 }
                                }
                            )
                          ]
                    }
              , reserves = Lovelace { unLovelace = 44999999999948276 }
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
          , ( ( ProtVer { _pvMaj = 0 , _pvMin = 1 , _pvAlt = 0 }
              , PParams
                  { _maxBkSz = 17739
                  , _maxHdrSz = 993
                  , _maxTxSz = 2021
                  , _maxPropSz = 16
                  , _bkSgnCntT = 2.077941229871697
                  , _bkSlotsPerEpoch = SlotCount { unSlotCount = 10 }
                  , _upTtl = SlotCount { unSlotCount = 20 }
                  , _scriptVersion = 1
                  , _upAdptThd = 0.2614549740866021
                  , _factorA = 2
                  , _factorB = 0
                  }
              )
            , []
            , fromList
                [ ( ApName "3DK" , ( ApVer 0 , Slot { unSlot = 43 } , Metadata ) )
                ]
            , fromList
                [ ( UpId 0
                  , ( ProtVer { _pvMaj = 1 , _pvMin = 0 , _pvAlt = 0 }
                    , PParams
                        { _maxBkSz = 24401
                        , _maxHdrSz = 1083
                        , _maxTxSz = 7588
                        , _maxPropSz = 25
                        , _bkSgnCntT = 2.0713749634593
                        , _bkSlotsPerEpoch = SlotCount { unSlotCount = 10 }
                        , _upTtl = SlotCount { unSlotCount = 14 }
                        , _scriptVersion = 1
                        , _upAdptThd = 0.3315717672316232
                        , _factorA = 8
                        , _factorB = 0
                        }
                    )
                  )
                ]
            , fromList []
            , fromList [ ( UpId 0 , Slot { unSlot = 63 } ) ]
            , fromList
                [ ( UpId 0
                  , VKeyGenesis { unVKeyGenesis = VKey Owner { unOwner = 0 } }
                  )
                ]
            , fromList []
            , fromList [ ( UpId 0 , Slot { unSlot = 60 } ) ]
            )
          )
        , Block
            { _bHeader =
                MkBlockHeader
                  { _bhPrevHash = Hash { unHash = -272531372536039742 }
                  , _bhSlot = Slot { unSlot = 66 }
                  , _bhIssuer = VKey Owner { unOwner = 0 }
                  , _bhSig =
                      Sig Hash { unHash = -119653622106078804 } Owner { unOwner = 0 }
                  , _bhUtxoHash = Hash { unHash = 839657738087498284 }
                  , _bhDlgHash = Hash { unHash = 839657738087498284 }
                  , _bhUpdHash = Hash { unHash = 0 }
                  }
            , _bBody =
                BlockBody
                  { _bDCerts = []
                  , _bUtxo = []
                  , _bUpdProp = Nothing
                  , _bUpdVotes = []
                  , _bProtVer = ProtVer { _pvMaj = 0 , _pvMin = 1 , _pvAlt = 0 }
                  }
            }
        )
      , ( ( Slot { unSlot = 63 }
          , fromList
              [ VKeyGenesis { unVKeyGenesis = VKey Owner { unOwner = 0 } }
              , VKeyGenesis { unVKeyGenesis = VKey Owner { unOwner = 0 } }
              ]
          , Hash { unHash = -272531372536039742 }
          , UTxOState
              { utxo =
                  UTxO
                    { unUTxO =
                        fromList
                          [ ( TxIn TxId { getTxId = Hash { unHash = 0 } } 0
                            , TxOut
                                { addr = Addr (VKey Owner { unOwner = 0 })
                                , value = Lovelace { unLovelace = 6733 }
                                }
                            )
                          , ( TxIn TxId { getTxId = Hash { unHash = 1 } } 0
                            , TxOut
                                { addr = Addr (VKey Owner { unOwner = 1 })
                                , value = Lovelace { unLovelace = 4795 }
                                }
                            )
                          , ( TxIn TxId { getTxId = Hash { unHash = 2 } } 0
                            , TxOut
                                { addr = Addr (VKey Owner { unOwner = 2 })
                                , value = Lovelace { unLovelace = 8020 }
                                }
                            )
                          , ( TxIn TxId { getTxId = Hash { unHash = 3 } } 0
                            , TxOut
                                { addr = Addr (VKey Owner { unOwner = 3 })
                                , value = Lovelace { unLovelace = 1631 }
                                }
                            )
                          , ( TxIn TxId { getTxId = Hash { unHash = 4 } } 0
                            , TxOut
                                { addr = Addr (VKey Owner { unOwner = 4 })
                                , value = Lovelace { unLovelace = 7829 }
                                }
                            )
                          , ( TxIn TxId { getTxId = Hash { unHash = 7 } } 0
                            , TxOut
                                { addr = Addr (VKey Owner { unOwner = 7 })
                                , value = Lovelace { unLovelace = 6863 }
                                }
                            )
                          , ( TxIn TxId { getTxId = Hash { unHash = 8 } } 0
                            , TxOut
                                { addr = Addr (VKey Owner { unOwner = 8 })
                                , value = Lovelace { unLovelace = 8623 }
                                }
                            )
                          , ( TxIn TxId { getTxId = Hash { unHash = 9 } } 0
                            , TxOut
                                { addr = Addr (VKey Owner { unOwner = 9 })
                                , value = Lovelace { unLovelace = 7230 }
                                }
                            )
                          ]
                    }
              , reserves = Lovelace { unLovelace = 44999999999948276 }
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
          , ( ( ProtVer { _pvMaj = 0 , _pvMin = 1 , _pvAlt = 0 }
              , PParams
                  { _maxBkSz = 17739
                  , _maxHdrSz = 993
                  , _maxTxSz = 2021
                  , _maxPropSz = 16
                  , _bkSgnCntT = 2.077941229871697
                  , _bkSlotsPerEpoch = SlotCount { unSlotCount = 10 }
                  , _upTtl = SlotCount { unSlotCount = 20 }
                  , _scriptVersion = 1
                  , _upAdptThd = 0.2614549740866021
                  , _factorA = 2
                  , _factorB = 0
                  }
              )
            , []
            , fromList
                [ ( ApName "3DK" , ( ApVer 0 , Slot { unSlot = 43 } , Metadata ) )
                ]
            , fromList
                [ ( UpId 0
                  , ( ProtVer { _pvMaj = 1 , _pvMin = 0 , _pvAlt = 0 }
                    , PParams
                        { _maxBkSz = 24401
                        , _maxHdrSz = 1083
                        , _maxTxSz = 7588
                        , _maxPropSz = 25
                        , _bkSgnCntT = 2.0713749634593
                        , _bkSlotsPerEpoch = SlotCount { unSlotCount = 10 }
                        , _upTtl = SlotCount { unSlotCount = 14 }
                        , _scriptVersion = 1
                        , _upAdptThd = 0.3315717672316232
                        , _factorA = 8
                        , _factorB = 0
                        }
                    )
                  )
                ]
            , fromList []
            , fromList [ ( UpId 0 , Slot { unSlot = 63 } ) ]
            , fromList
                [ ( UpId 0
                  , VKeyGenesis { unVKeyGenesis = VKey Owner { unOwner = 0 } }
                  )
                ]
            , fromList []
            , fromList [ ( UpId 0 , Slot { unSlot = 60 } ) ]
            )
          )
        , Block
            { _bHeader =
                MkBlockHeader
                  { _bhPrevHash = Hash { unHash = 3562330038026782003 }
                  , _bhSlot = Slot { unSlot = 63 }
                  , _bhIssuer = VKey Owner { unOwner = 0 }
                  , _bhSig =
                      Sig Hash { unHash = -272531372536039742 } Owner { unOwner = 0 }
                  , _bhUtxoHash = Hash { unHash = 839657738087498284 }
                  , _bhDlgHash = Hash { unHash = 839657738087498284 }
                  , _bhUpdHash = Hash { unHash = 1 }
                  }
            , _bBody =
                BlockBody
                  { _bDCerts = []
                  , _bUtxo = []
                  , _bUpdProp = Nothing
                  , _bUpdVotes =
                      [ Vote
                          { _vCaster = VKey Owner { unOwner = 0 }
                          , _vPropId = UpId 0
                          , _vSig = Sig (UpId 0) Owner { unOwner = 0 }
                          }
                      ]
                  , _bProtVer = ProtVer { _pvMaj = 0 , _pvMin = 1 , _pvAlt = 0 }
                  }
            }
        )
      , ( ( Slot { unSlot = 60 }
          , fromList
              [ VKeyGenesis { unVKeyGenesis = VKey Owner { unOwner = 0 } }
              , VKeyGenesis { unVKeyGenesis = VKey Owner { unOwner = 0 } }
              ]
          , Hash { unHash = 3562330038026782003 }
          , UTxOState
              { utxo =
                  UTxO
                    { unUTxO =
                        fromList
                          [ ( TxIn TxId { getTxId = Hash { unHash = 0 } } 0
                            , TxOut
                                { addr = Addr (VKey Owner { unOwner = 0 })
                                , value = Lovelace { unLovelace = 6733 }
                                }
                            )
                          , ( TxIn TxId { getTxId = Hash { unHash = 1 } } 0
                            , TxOut
                                { addr = Addr (VKey Owner { unOwner = 1 })
                                , value = Lovelace { unLovelace = 4795 }
                                }
                            )
                          , ( TxIn TxId { getTxId = Hash { unHash = 2 } } 0
                            , TxOut
                                { addr = Addr (VKey Owner { unOwner = 2 })
                                , value = Lovelace { unLovelace = 8020 }
                                }
                            )
                          , ( TxIn TxId { getTxId = Hash { unHash = 3 } } 0
                            , TxOut
                                { addr = Addr (VKey Owner { unOwner = 3 })
                                , value = Lovelace { unLovelace = 1631 }
                                }
                            )
                          , ( TxIn TxId { getTxId = Hash { unHash = 4 } } 0
                            , TxOut
                                { addr = Addr (VKey Owner { unOwner = 4 })
                                , value = Lovelace { unLovelace = 7829 }
                                }
                            )
                          , ( TxIn TxId { getTxId = Hash { unHash = 7 } } 0
                            , TxOut
                                { addr = Addr (VKey Owner { unOwner = 7 })
                                , value = Lovelace { unLovelace = 6863 }
                                }
                            )
                          , ( TxIn TxId { getTxId = Hash { unHash = 8 } } 0
                            , TxOut
                                { addr = Addr (VKey Owner { unOwner = 8 })
                                , value = Lovelace { unLovelace = 8623 }
                                }
                            )
                          , ( TxIn TxId { getTxId = Hash { unHash = 9 } } 0
                            , TxOut
                                { addr = Addr (VKey Owner { unOwner = 9 })
                                , value = Lovelace { unLovelace = 7230 }
                                }
                            )
                          ]
                    }
              , reserves = Lovelace { unLovelace = 44999999999948276 }
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
          , ( ( ProtVer { _pvMaj = 0 , _pvMin = 1 , _pvAlt = 0 }
              , PParams
                  { _maxBkSz = 17739
                  , _maxHdrSz = 993
                  , _maxTxSz = 2021
                  , _maxPropSz = 16
                  , _bkSgnCntT = 2.077941229871697
                  , _bkSlotsPerEpoch = SlotCount { unSlotCount = 10 }
                  , _upTtl = SlotCount { unSlotCount = 20 }
                  , _scriptVersion = 1
                  , _upAdptThd = 0.2614549740866021
                  , _factorA = 2
                  , _factorB = 0
                  }
              )
            , []
            , fromList
                [ ( ApName "3DK" , ( ApVer 0 , Slot { unSlot = 43 } , Metadata ) )
                ]
            , fromList
                [ ( UpId 0
                  , ( ProtVer { _pvMaj = 1 , _pvMin = 0 , _pvAlt = 0 }
                    , PParams
                        { _maxBkSz = 24401
                        , _maxHdrSz = 1083
                        , _maxTxSz = 7588
                        , _maxPropSz = 25
                        , _bkSgnCntT = 2.0713749634593
                        , _bkSlotsPerEpoch = SlotCount { unSlotCount = 10 }
                        , _upTtl = SlotCount { unSlotCount = 14 }
                        , _scriptVersion = 1
                        , _upAdptThd = 0.3315717672316232
                        , _factorA = 8
                        , _factorB = 0
                        }
                    )
                  )
                ]
            , fromList []
            , fromList []
            , fromList []
            , fromList []
            , fromList [ ( UpId 0 , Slot { unSlot = 60 } ) ]
            )
          )
        , Block
            { _bHeader =
                MkBlockHeader
                  { _bhPrevHash = Hash { unHash = 2009953981399213263 }
                  , _bhSlot = Slot { unSlot = 60 }
                  , _bhIssuer = VKey Owner { unOwner = 0 }
                  , _bhSig =
                      Sig Hash { unHash = 3562330038026782003 } Owner { unOwner = 0 }
                  , _bhUtxoHash = Hash { unHash = 839657738087498284 }
                  , _bhDlgHash = Hash { unHash = 839657738087498284 }
                  , _bhUpdHash = Hash { unHash = 7218145561093074451 }
                  }
            , _bBody =
                BlockBody
                  { _bDCerts = []
                  , _bUtxo = []
                  , _bUpdProp =
                      Just
                        UProp
                          { _upId = UpId 0
                          , _upIssuer = VKey Owner { unOwner = 0 }
                          , _upParams =
                              PParams
                                { _maxBkSz = 24401
                                , _maxHdrSz = 1083
                                , _maxTxSz = 7588
                                , _maxPropSz = 25
                                , _bkSgnCntT = 2.0713749634593
                                , _bkSlotsPerEpoch = SlotCount { unSlotCount = 10 }
                                , _upTtl = SlotCount { unSlotCount = 14 }
                                , _scriptVersion = 1
                                , _upAdptThd = 0.3315717672316232
                                , _factorA = 8
                                , _factorB = 0
                                }
                          , _upPV = ProtVer { _pvMaj = 1 , _pvMin = 0 , _pvAlt = 0 }
                          , _upSwVer = SwVer { _svName = ApName "3DK" , _svVer = ApVer 0 }
                          , _upSig =
                              Sig
                                ( ProtVer { _pvMaj = 1 , _pvMin = 0 , _pvAlt = 0 }
                                , PParams
                                    { _maxBkSz = 24401
                                    , _maxHdrSz = 1083
                                    , _maxTxSz = 7588
                                    , _maxPropSz = 25
                                    , _bkSgnCntT = 2.0713749634593
                                    , _bkSlotsPerEpoch = SlotCount { unSlotCount = 10 }
                                    , _upTtl = SlotCount { unSlotCount = 14 }
                                    , _scriptVersion = 1
                                    , _upAdptThd = 0.3315717672316232
                                    , _factorA = 8
                                    , _factorB = 0
                                    }
                                , SwVer { _svName = ApName "3DK" , _svVer = ApVer 0 }
                                )
                                Owner { unOwner = 0 }
                          , _upSTags = fromList []
                          , _upMdt = Metadata
                          }
                  , _bUpdVotes = []
                  , _bProtVer = ProtVer { _pvMaj = 0 , _pvMin = 1 , _pvAlt = 0 }
                  }
            }
        )
      , ( ( Slot { unSlot = 59 }
          , fromList
              [ VKeyGenesis { unVKeyGenesis = VKey Owner { unOwner = 0 } }
              , VKeyGenesis { unVKeyGenesis = VKey Owner { unOwner = 0 } }
              ]
          , Hash { unHash = 2009953981399213263 }
          , UTxOState
              { utxo =
                  UTxO
                    { unUTxO =
                        fromList
                          [ ( TxIn TxId { getTxId = Hash { unHash = 0 } } 0
                            , TxOut
                                { addr = Addr (VKey Owner { unOwner = 0 })
                                , value = Lovelace { unLovelace = 6733 }
                                }
                            )
                          , ( TxIn TxId { getTxId = Hash { unHash = 1 } } 0
                            , TxOut
                                { addr = Addr (VKey Owner { unOwner = 1 })
                                , value = Lovelace { unLovelace = 4795 }
                                }
                            )
                          , ( TxIn TxId { getTxId = Hash { unHash = 2 } } 0
                            , TxOut
                                { addr = Addr (VKey Owner { unOwner = 2 })
                                , value = Lovelace { unLovelace = 8020 }
                                }
                            )
                          , ( TxIn TxId { getTxId = Hash { unHash = 3 } } 0
                            , TxOut
                                { addr = Addr (VKey Owner { unOwner = 3 })
                                , value = Lovelace { unLovelace = 1631 }
                                }
                            )
                          , ( TxIn TxId { getTxId = Hash { unHash = 4 } } 0
                            , TxOut
                                { addr = Addr (VKey Owner { unOwner = 4 })
                                , value = Lovelace { unLovelace = 7829 }
                                }
                            )
                          , ( TxIn TxId { getTxId = Hash { unHash = 7 } } 0
                            , TxOut
                                { addr = Addr (VKey Owner { unOwner = 7 })
                                , value = Lovelace { unLovelace = 6863 }
                                }
                            )
                          , ( TxIn TxId { getTxId = Hash { unHash = 8 } } 0
                            , TxOut
                                { addr = Addr (VKey Owner { unOwner = 8 })
                                , value = Lovelace { unLovelace = 8623 }
                                }
                            )
                          , ( TxIn TxId { getTxId = Hash { unHash = 9 } } 0
                            , TxOut
                                { addr = Addr (VKey Owner { unOwner = 9 })
                                , value = Lovelace { unLovelace = 7230 }
                                }
                            )
                          ]
                    }
              , reserves = Lovelace { unLovelace = 44999999999948276 }
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
                  , _bkSgnCntT = 2.071538545155317
                  , _bkSlotsPerEpoch = SlotCount { unSlotCount = 10 }
                  , _upTtl = SlotCount { unSlotCount = 10 }
                  , _scriptVersion = 0
                  , _upAdptThd = 0.6
                  , _factorA = 1
                  , _factorB = 2
                  }
              )
            , [ ( Slot { unSlot = 47 }
                , ( ProtVer { _pvMaj = 0 , _pvMin = 1 , _pvAlt = 0 }
                  , PParams
                      { _maxBkSz = 17739
                      , _maxHdrSz = 993
                      , _maxTxSz = 2021
                      , _maxPropSz = 16
                      , _bkSgnCntT = 2.077941229871697
                      , _bkSlotsPerEpoch = SlotCount { unSlotCount = 10 }
                      , _upTtl = SlotCount { unSlotCount = 20 }
                      , _scriptVersion = 1
                      , _upAdptThd = 0.2614549740866021
                      , _factorA = 2
                      , _factorB = 0
                      }
                  )
                )
              ]
            , fromList
                [ ( ApName "3DK" , ( ApVer 0 , Slot { unSlot = 43 } , Metadata ) )
                ]
            , fromList
                [ ( UpId 3
                  , ( ProtVer { _pvMaj = 0 , _pvMin = 1 , _pvAlt = 0 }
                    , PParams
                        { _maxBkSz = 17739
                        , _maxHdrSz = 993
                        , _maxTxSz = 2021
                        , _maxPropSz = 16
                        , _bkSgnCntT = 2.077941229871697
                        , _bkSlotsPerEpoch = SlotCount { unSlotCount = 10 }
                        , _upTtl = SlotCount { unSlotCount = 20 }
                        , _scriptVersion = 1
                        , _upAdptThd = 0.2614549740866021
                        , _factorA = 2
                        , _factorB = 0
                        }
                    )
                  )
                , ( UpId 7
                  , ( ProtVer { _pvMaj = 1 , _pvMin = 0 , _pvAlt = 0 }
                    , PParams
                        { _maxBkSz = 20000
                        , _maxHdrSz = 1284
                        , _maxTxSz = 1236
                        , _maxPropSz = 9
                        , _bkSgnCntT = 2.072055274116413
                        , _bkSlotsPerEpoch = SlotCount { unSlotCount = 10 }
                        , _upTtl = SlotCount { unSlotCount = 13 }
                        , _scriptVersion = 1
                        , _upAdptThd = 0.292387600526244
                        , _factorA = 5
                        , _factorB = 10
                        }
                    )
                  )
                ]
            , fromList []
            , fromList
                [ ( UpId 3 , Slot { unSlot = 36 } )
                , ( UpId 7 , Slot { unSlot = 59 } )
                ]
            , fromList
                [ ( UpId 3
                  , VKeyGenesis { unVKeyGenesis = VKey Owner { unOwner = 0 } }
                  )
                , ( UpId 7
                  , VKeyGenesis { unVKeyGenesis = VKey Owner { unOwner = 0 } }
                  )
                ]
            , fromList
                [ ( ProtVer { _pvMaj = 0 , _pvMin = 1 , _pvAlt = 0 }
                  , VKeyGenesis { unVKeyGenesis = VKey Owner { unOwner = 0 } }
                  )
                ]
            , fromList
                [ ( UpId 3 , Slot { unSlot = 36 } )
                , ( UpId 7 , Slot { unSlot = 59 } )
                ]
            )
          )
        , Block
            { _bHeader =
                MkBlockHeader
                  { _bhPrevHash = Hash { unHash = 5395671326036378010 }
                  , _bhSlot = Slot { unSlot = 59 }
                  , _bhIssuer = VKey Owner { unOwner = 0 }
                  , _bhSig =
                      Sig Hash { unHash = 2009953981399213263 } Owner { unOwner = 0 }
                  , _bhUtxoHash = Hash { unHash = 839657738087498284 }
                  , _bhDlgHash = Hash { unHash = 839657738087498284 }
                  , _bhUpdHash = Hash { unHash = 3252463905130121897 }
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
                                { _maxBkSz = 20000
                                , _maxHdrSz = 1284
                                , _maxTxSz = 1236
                                , _maxPropSz = 9
                                , _bkSgnCntT = 2.072055274116413
                                , _bkSlotsPerEpoch = SlotCount { unSlotCount = 10 }
                                , _upTtl = SlotCount { unSlotCount = 13 }
                                , _scriptVersion = 1
                                , _upAdptThd = 0.292387600526244
                                , _factorA = 5
                                , _factorB = 10
                                }
                          , _upPV = ProtVer { _pvMaj = 1 , _pvMin = 0 , _pvAlt = 0 }
                          , _upSwVer = SwVer { _svName = ApName "3DK" , _svVer = ApVer 0 }
                          , _upSig =
                              Sig
                                ( ProtVer { _pvMaj = 1 , _pvMin = 0 , _pvAlt = 0 }
                                , PParams
                                    { _maxBkSz = 20000
                                    , _maxHdrSz = 1284
                                    , _maxTxSz = 1236
                                    , _maxPropSz = 9
                                    , _bkSgnCntT = 2.072055274116413
                                    , _bkSlotsPerEpoch = SlotCount { unSlotCount = 10 }
                                    , _upTtl = SlotCount { unSlotCount = 13 }
                                    , _scriptVersion = 1
                                    , _upAdptThd = 0.292387600526244
                                    , _factorA = 5
                                    , _factorB = 10
                                    }
                                , SwVer { _svName = ApName "3DK" , _svVer = ApVer 0 }
                                )
                                Owner { unOwner = 0 }
                          , _upSTags = fromList [ "p\\{ " ]
                          , _upMdt = Metadata
                          }
                  , _bUpdVotes =
                      [ Vote
                          { _vCaster = VKey Owner { unOwner = 0 }
                          , _vPropId = UpId 7
                          , _vSig = Sig (UpId 7) Owner { unOwner = 0 }
                          }
                      ]
                  , _bProtVer = ProtVer { _pvMaj = 0 , _pvMin = 1 , _pvAlt = 0 }
                  }
            }
        )
      , ( ( Slot { unSlot = 57 }
          , fromList
              [ VKeyGenesis { unVKeyGenesis = VKey Owner { unOwner = 0 } }
              , VKeyGenesis { unVKeyGenesis = VKey Owner { unOwner = 0 } }
              ]
          , Hash { unHash = 5395671326036378010 }
          , UTxOState
              { utxo =
                  UTxO
                    { unUTxO =
                        fromList
                          [ ( TxIn TxId { getTxId = Hash { unHash = 0 } } 0
                            , TxOut
                                { addr = Addr (VKey Owner { unOwner = 0 })
                                , value = Lovelace { unLovelace = 6733 }
                                }
                            )
                          , ( TxIn TxId { getTxId = Hash { unHash = 1 } } 0
                            , TxOut
                                { addr = Addr (VKey Owner { unOwner = 1 })
                                , value = Lovelace { unLovelace = 4795 }
                                }
                            )
                          , ( TxIn TxId { getTxId = Hash { unHash = 2 } } 0
                            , TxOut
                                { addr = Addr (VKey Owner { unOwner = 2 })
                                , value = Lovelace { unLovelace = 8020 }
                                }
                            )
                          , ( TxIn TxId { getTxId = Hash { unHash = 3 } } 0
                            , TxOut
                                { addr = Addr (VKey Owner { unOwner = 3 })
                                , value = Lovelace { unLovelace = 1631 }
                                }
                            )
                          , ( TxIn TxId { getTxId = Hash { unHash = 4 } } 0
                            , TxOut
                                { addr = Addr (VKey Owner { unOwner = 4 })
                                , value = Lovelace { unLovelace = 7829 }
                                }
                            )
                          , ( TxIn TxId { getTxId = Hash { unHash = 7 } } 0
                            , TxOut
                                { addr = Addr (VKey Owner { unOwner = 7 })
                                , value = Lovelace { unLovelace = 6863 }
                                }
                            )
                          , ( TxIn TxId { getTxId = Hash { unHash = 8 } } 0
                            , TxOut
                                { addr = Addr (VKey Owner { unOwner = 8 })
                                , value = Lovelace { unLovelace = 8623 }
                                }
                            )
                          , ( TxIn TxId { getTxId = Hash { unHash = 9 } } 0
                            , TxOut
                                { addr = Addr (VKey Owner { unOwner = 9 })
                                , value = Lovelace { unLovelace = 7230 }
                                }
                            )
                          ]
                    }
              , reserves = Lovelace { unLovelace = 44999999999948276 }
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
                  , _bkSgnCntT = 2.071538545155317
                  , _bkSlotsPerEpoch = SlotCount { unSlotCount = 10 }
                  , _upTtl = SlotCount { unSlotCount = 10 }
                  , _scriptVersion = 0
                  , _upAdptThd = 0.6
                  , _factorA = 1
                  , _factorB = 2
                  }
              )
            , [ ( Slot { unSlot = 47 }
                , ( ProtVer { _pvMaj = 0 , _pvMin = 1 , _pvAlt = 0 }
                  , PParams
                      { _maxBkSz = 17739
                      , _maxHdrSz = 993
                      , _maxTxSz = 2021
                      , _maxPropSz = 16
                      , _bkSgnCntT = 2.077941229871697
                      , _bkSlotsPerEpoch = SlotCount { unSlotCount = 10 }
                      , _upTtl = SlotCount { unSlotCount = 20 }
                      , _scriptVersion = 1
                      , _upAdptThd = 0.2614549740866021
                      , _factorA = 2
                      , _factorB = 0
                      }
                  )
                )
              ]
            , fromList
                [ ( ApName "3DK" , ( ApVer 0 , Slot { unSlot = 43 } , Metadata ) )
                ]
            , fromList
                [ ( UpId 3
                  , ( ProtVer { _pvMaj = 0 , _pvMin = 1 , _pvAlt = 0 }
                    , PParams
                        { _maxBkSz = 17739
                        , _maxHdrSz = 993
                        , _maxTxSz = 2021
                        , _maxPropSz = 16
                        , _bkSgnCntT = 2.077941229871697
                        , _bkSlotsPerEpoch = SlotCount { unSlotCount = 10 }
                        , _upTtl = SlotCount { unSlotCount = 20 }
                        , _scriptVersion = 1
                        , _upAdptThd = 0.2614549740866021
                        , _factorA = 2
                        , _factorB = 0
                        }
                    )
                  )
                ]
            , fromList []
            , fromList [ ( UpId 3 , Slot { unSlot = 36 } ) ]
            , fromList
                [ ( UpId 3
                  , VKeyGenesis { unVKeyGenesis = VKey Owner { unOwner = 0 } }
                  )
                ]
            , fromList
                [ ( ProtVer { _pvMaj = 0 , _pvMin = 1 , _pvAlt = 0 }
                  , VKeyGenesis { unVKeyGenesis = VKey Owner { unOwner = 0 } }
                  )
                ]
            , fromList [ ( UpId 3 , Slot { unSlot = 36 } ) ]
            )
          )
        , Block
            { _bHeader =
                MkBlockHeader
                  { _bhPrevHash = Hash { unHash = 1686699441951555645 }
                  , _bhSlot = Slot { unSlot = 57 }
                  , _bhIssuer = VKey Owner { unOwner = 0 }
                  , _bhSig =
                      Sig Hash { unHash = 5395671326036378010 } Owner { unOwner = 0 }
                  , _bhUtxoHash = Hash { unHash = 839657738087498284 }
                  , _bhDlgHash = Hash { unHash = 839657738087498284 }
                  , _bhUpdHash = Hash { unHash = 0 }
                  }
            , _bBody =
                BlockBody
                  { _bDCerts = []
                  , _bUtxo = []
                  , _bUpdProp = Nothing
                  , _bUpdVotes = []
                  , _bProtVer = ProtVer { _pvMaj = 0 , _pvMin = 1 , _pvAlt = 0 }
                  }
            }
        )
      , ( ( Slot { unSlot = 52 }
          , fromList
              [ VKeyGenesis { unVKeyGenesis = VKey Owner { unOwner = 0 } }
              , VKeyGenesis { unVKeyGenesis = VKey Owner { unOwner = 0 } }
              ]
          , Hash { unHash = 1686699441951555645 }
          , UTxOState
              { utxo =
                  UTxO
                    { unUTxO =
                        fromList
                          [ ( TxIn TxId { getTxId = Hash { unHash = 0 } } 0
                            , TxOut
                                { addr = Addr (VKey Owner { unOwner = 0 })
                                , value = Lovelace { unLovelace = 6733 }
                                }
                            )
                          , ( TxIn TxId { getTxId = Hash { unHash = 1 } } 0
                            , TxOut
                                { addr = Addr (VKey Owner { unOwner = 1 })
                                , value = Lovelace { unLovelace = 4795 }
                                }
                            )
                          , ( TxIn TxId { getTxId = Hash { unHash = 2 } } 0
                            , TxOut
                                { addr = Addr (VKey Owner { unOwner = 2 })
                                , value = Lovelace { unLovelace = 8020 }
                                }
                            )
                          , ( TxIn TxId { getTxId = Hash { unHash = 3 } } 0
                            , TxOut
                                { addr = Addr (VKey Owner { unOwner = 3 })
                                , value = Lovelace { unLovelace = 1631 }
                                }
                            )
                          , ( TxIn TxId { getTxId = Hash { unHash = 4 } } 0
                            , TxOut
                                { addr = Addr (VKey Owner { unOwner = 4 })
                                , value = Lovelace { unLovelace = 7829 }
                                }
                            )
                          , ( TxIn TxId { getTxId = Hash { unHash = 7 } } 0
                            , TxOut
                                { addr = Addr (VKey Owner { unOwner = 7 })
                                , value = Lovelace { unLovelace = 6863 }
                                }
                            )
                          , ( TxIn TxId { getTxId = Hash { unHash = 8 } } 0
                            , TxOut
                                { addr = Addr (VKey Owner { unOwner = 8 })
                                , value = Lovelace { unLovelace = 8623 }
                                }
                            )
                          , ( TxIn TxId { getTxId = Hash { unHash = 9 } } 0
                            , TxOut
                                { addr = Addr (VKey Owner { unOwner = 9 })
                                , value = Lovelace { unLovelace = 7230 }
                                }
                            )
                          ]
                    }
              , reserves = Lovelace { unLovelace = 44999999999948276 }
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
                  , _bkSgnCntT = 2.071538545155317
                  , _bkSlotsPerEpoch = SlotCount { unSlotCount = 10 }
                  , _upTtl = SlotCount { unSlotCount = 10 }
                  , _scriptVersion = 0
                  , _upAdptThd = 0.6
                  , _factorA = 1
                  , _factorB = 2
                  }
              )
            , [ ( Slot { unSlot = 47 }
                , ( ProtVer { _pvMaj = 0 , _pvMin = 1 , _pvAlt = 0 }
                  , PParams
                      { _maxBkSz = 17739
                      , _maxHdrSz = 993
                      , _maxTxSz = 2021
                      , _maxPropSz = 16
                      , _bkSgnCntT = 2.077941229871697
                      , _bkSlotsPerEpoch = SlotCount { unSlotCount = 10 }
                      , _upTtl = SlotCount { unSlotCount = 20 }
                      , _scriptVersion = 1
                      , _upAdptThd = 0.2614549740866021
                      , _factorA = 2
                      , _factorB = 0
                      }
                  )
                )
              ]
            , fromList
                [ ( ApName "3DK" , ( ApVer 0 , Slot { unSlot = 43 } , Metadata ) )
                ]
            , fromList
                [ ( UpId 3
                  , ( ProtVer { _pvMaj = 0 , _pvMin = 1 , _pvAlt = 0 }
                    , PParams
                        { _maxBkSz = 17739
                        , _maxHdrSz = 993
                        , _maxTxSz = 2021
                        , _maxPropSz = 16
                        , _bkSgnCntT = 2.077941229871697
                        , _bkSlotsPerEpoch = SlotCount { unSlotCount = 10 }
                        , _upTtl = SlotCount { unSlotCount = 20 }
                        , _scriptVersion = 1
                        , _upAdptThd = 0.2614549740866021
                        , _factorA = 2
                        , _factorB = 0
                        }
                    )
                  )
                ]
            , fromList []
            , fromList [ ( UpId 3 , Slot { unSlot = 36 } ) ]
            , fromList
                [ ( UpId 3
                  , VKeyGenesis { unVKeyGenesis = VKey Owner { unOwner = 0 } }
                  )
                ]
            , fromList
                [ ( ProtVer { _pvMaj = 0 , _pvMin = 1 , _pvAlt = 0 }
                  , VKeyGenesis { unVKeyGenesis = VKey Owner { unOwner = 0 } }
                  )
                ]
            , fromList [ ( UpId 3 , Slot { unSlot = 36 } ) ]
            )
          )
        , Block
            { _bHeader =
                MkBlockHeader
                  { _bhPrevHash = Hash { unHash = 8262470091909970073 }
                  , _bhSlot = Slot { unSlot = 52 }
                  , _bhIssuer = VKey Owner { unOwner = 0 }
                  , _bhSig =
                      Sig Hash { unHash = 1686699441951555645 } Owner { unOwner = 0 }
                  , _bhUtxoHash = Hash { unHash = 839657738087498284 }
                  , _bhDlgHash = Hash { unHash = 839657738087498284 }
                  , _bhUpdHash = Hash { unHash = 0 }
                  }
            , _bBody =
                BlockBody
                  { _bDCerts = []
                  , _bUtxo = []
                  , _bUpdProp = Nothing
                  , _bUpdVotes = []
                  , _bProtVer = ProtVer { _pvMaj = 0 , _pvMin = 1 , _pvAlt = 0 }
                  }
            }
        )
      , ( ( Slot { unSlot = 47 }
          , fromList
              [ VKeyGenesis { unVKeyGenesis = VKey Owner { unOwner = 0 } }
              , VKeyGenesis { unVKeyGenesis = VKey Owner { unOwner = 0 } }
              ]
          , Hash { unHash = 8262470091909970073 }
          , UTxOState
              { utxo =
                  UTxO
                    { unUTxO =
                        fromList
                          [ ( TxIn TxId { getTxId = Hash { unHash = 0 } } 0
                            , TxOut
                                { addr = Addr (VKey Owner { unOwner = 0 })
                                , value = Lovelace { unLovelace = 6733 }
                                }
                            )
                          , ( TxIn TxId { getTxId = Hash { unHash = 1 } } 0
                            , TxOut
                                { addr = Addr (VKey Owner { unOwner = 1 })
                                , value = Lovelace { unLovelace = 4795 }
                                }
                            )
                          , ( TxIn TxId { getTxId = Hash { unHash = 2 } } 0
                            , TxOut
                                { addr = Addr (VKey Owner { unOwner = 2 })
                                , value = Lovelace { unLovelace = 8020 }
                                }
                            )
                          , ( TxIn TxId { getTxId = Hash { unHash = 3 } } 0
                            , TxOut
                                { addr = Addr (VKey Owner { unOwner = 3 })
                                , value = Lovelace { unLovelace = 1631 }
                                }
                            )
                          , ( TxIn TxId { getTxId = Hash { unHash = 4 } } 0
                            , TxOut
                                { addr = Addr (VKey Owner { unOwner = 4 })
                                , value = Lovelace { unLovelace = 7829 }
                                }
                            )
                          , ( TxIn TxId { getTxId = Hash { unHash = 7 } } 0
                            , TxOut
                                { addr = Addr (VKey Owner { unOwner = 7 })
                                , value = Lovelace { unLovelace = 6863 }
                                }
                            )
                          , ( TxIn TxId { getTxId = Hash { unHash = 8 } } 0
                            , TxOut
                                { addr = Addr (VKey Owner { unOwner = 8 })
                                , value = Lovelace { unLovelace = 8623 }
                                }
                            )
                          , ( TxIn TxId { getTxId = Hash { unHash = 9 } } 0
                            , TxOut
                                { addr = Addr (VKey Owner { unOwner = 9 })
                                , value = Lovelace { unLovelace = 7230 }
                                }
                            )
                          ]
                    }
              , reserves = Lovelace { unLovelace = 44999999999948276 }
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
                  , _bkSgnCntT = 2.071538545155317
                  , _bkSlotsPerEpoch = SlotCount { unSlotCount = 10 }
                  , _upTtl = SlotCount { unSlotCount = 10 }
                  , _scriptVersion = 0
                  , _upAdptThd = 0.6
                  , _factorA = 1
                  , _factorB = 2
                  }
              )
            , [ ( Slot { unSlot = 47 }
                , ( ProtVer { _pvMaj = 0 , _pvMin = 1 , _pvAlt = 0 }
                  , PParams
                      { _maxBkSz = 17739
                      , _maxHdrSz = 993
                      , _maxTxSz = 2021
                      , _maxPropSz = 16
                      , _bkSgnCntT = 2.077941229871697
                      , _bkSlotsPerEpoch = SlotCount { unSlotCount = 10 }
                      , _upTtl = SlotCount { unSlotCount = 20 }
                      , _scriptVersion = 1
                      , _upAdptThd = 0.2614549740866021
                      , _factorA = 2
                      , _factorB = 0
                      }
                  )
                )
              ]
            , fromList
                [ ( ApName "3DK" , ( ApVer 0 , Slot { unSlot = 43 } , Metadata ) )
                ]
            , fromList
                [ ( UpId 3
                  , ( ProtVer { _pvMaj = 0 , _pvMin = 1 , _pvAlt = 0 }
                    , PParams
                        { _maxBkSz = 17739
                        , _maxHdrSz = 993
                        , _maxTxSz = 2021
                        , _maxPropSz = 16
                        , _bkSgnCntT = 2.077941229871697
                        , _bkSlotsPerEpoch = SlotCount { unSlotCount = 10 }
                        , _upTtl = SlotCount { unSlotCount = 20 }
                        , _scriptVersion = 1
                        , _upAdptThd = 0.2614549740866021
                        , _factorA = 2
                        , _factorB = 0
                        }
                    )
                  )
                ]
            , fromList []
            , fromList [ ( UpId 3 , Slot { unSlot = 36 } ) ]
            , fromList
                [ ( UpId 3
                  , VKeyGenesis { unVKeyGenesis = VKey Owner { unOwner = 0 } }
                  )
                ]
            , fromList
                [ ( ProtVer { _pvMaj = 0 , _pvMin = 1 , _pvAlt = 0 }
                  , VKeyGenesis { unVKeyGenesis = VKey Owner { unOwner = 0 } }
                  )
                ]
            , fromList [ ( UpId 3 , Slot { unSlot = 36 } ) ]
            )
          )
        , Block
            { _bHeader =
                MkBlockHeader
                  { _bhPrevHash = Hash { unHash = 5313759645883497668 }
                  , _bhSlot = Slot { unSlot = 47 }
                  , _bhIssuer = VKey Owner { unOwner = 0 }
                  , _bhSig =
                      Sig Hash { unHash = 8262470091909970073 } Owner { unOwner = 0 }
                  , _bhUtxoHash = Hash { unHash = 839657738087498284 }
                  , _bhDlgHash = Hash { unHash = 839657738087498284 }
                  , _bhUpdHash = Hash { unHash = 0 }
                  }
            , _bBody =
                BlockBody
                  { _bDCerts = []
                  , _bUtxo = []
                  , _bUpdProp = Nothing
                  , _bUpdVotes = []
                  , _bProtVer = ProtVer { _pvMaj = 0 , _pvMin = 1 , _pvAlt = 0 }
                  }
            }
        )
      , ( ( Slot { unSlot = 43 }
          , fromList
              [ VKeyGenesis { unVKeyGenesis = VKey Owner { unOwner = 0 } }
              , VKeyGenesis { unVKeyGenesis = VKey Owner { unOwner = 0 } }
              ]
          , Hash { unHash = 5313759645883497668 }
          , UTxOState
              { utxo =
                  UTxO
                    { unUTxO =
                        fromList
                          [ ( TxIn TxId { getTxId = Hash { unHash = 0 } } 0
                            , TxOut
                                { addr = Addr (VKey Owner { unOwner = 0 })
                                , value = Lovelace { unLovelace = 6733 }
                                }
                            )
                          , ( TxIn TxId { getTxId = Hash { unHash = 1 } } 0
                            , TxOut
                                { addr = Addr (VKey Owner { unOwner = 1 })
                                , value = Lovelace { unLovelace = 4795 }
                                }
                            )
                          , ( TxIn TxId { getTxId = Hash { unHash = 2 } } 0
                            , TxOut
                                { addr = Addr (VKey Owner { unOwner = 2 })
                                , value = Lovelace { unLovelace = 8020 }
                                }
                            )
                          , ( TxIn TxId { getTxId = Hash { unHash = 3 } } 0
                            , TxOut
                                { addr = Addr (VKey Owner { unOwner = 3 })
                                , value = Lovelace { unLovelace = 1631 }
                                }
                            )
                          , ( TxIn TxId { getTxId = Hash { unHash = 4 } } 0
                            , TxOut
                                { addr = Addr (VKey Owner { unOwner = 4 })
                                , value = Lovelace { unLovelace = 7829 }
                                }
                            )
                          , ( TxIn TxId { getTxId = Hash { unHash = 7 } } 0
                            , TxOut
                                { addr = Addr (VKey Owner { unOwner = 7 })
                                , value = Lovelace { unLovelace = 6863 }
                                }
                            )
                          , ( TxIn TxId { getTxId = Hash { unHash = 8 } } 0
                            , TxOut
                                { addr = Addr (VKey Owner { unOwner = 8 })
                                , value = Lovelace { unLovelace = 8623 }
                                }
                            )
                          , ( TxIn TxId { getTxId = Hash { unHash = 9 } } 0
                            , TxOut
                                { addr = Addr (VKey Owner { unOwner = 9 })
                                , value = Lovelace { unLovelace = 7230 }
                                }
                            )
                          ]
                    }
              , reserves = Lovelace { unLovelace = 44999999999948276 }
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
                  , _bkSgnCntT = 2.071538545155317
                  , _bkSlotsPerEpoch = SlotCount { unSlotCount = 10 }
                  , _upTtl = SlotCount { unSlotCount = 10 }
                  , _scriptVersion = 0
                  , _upAdptThd = 0.6
                  , _factorA = 1
                  , _factorB = 2
                  }
              )
            , []
            , fromList
                [ ( ApName "3DK" , ( ApVer 0 , Slot { unSlot = 43 } , Metadata ) )
                ]
            , fromList
                [ ( UpId 3
                  , ( ProtVer { _pvMaj = 0 , _pvMin = 1 , _pvAlt = 0 }
                    , PParams
                        { _maxBkSz = 17739
                        , _maxHdrSz = 993
                        , _maxTxSz = 2021
                        , _maxPropSz = 16
                        , _bkSgnCntT = 2.077941229871697
                        , _bkSlotsPerEpoch = SlotCount { unSlotCount = 10 }
                        , _upTtl = SlotCount { unSlotCount = 20 }
                        , _scriptVersion = 1
                        , _upAdptThd = 0.2614549740866021
                        , _factorA = 2
                        , _factorB = 0
                        }
                    )
                  )
                ]
            , fromList []
            , fromList [ ( UpId 3 , Slot { unSlot = 36 } ) ]
            , fromList
                [ ( UpId 3
                  , VKeyGenesis { unVKeyGenesis = VKey Owner { unOwner = 0 } }
                  )
                ]
            , fromList []
            , fromList [ ( UpId 3 , Slot { unSlot = 36 } ) ]
            )
          )
        , Block
            { _bHeader =
                MkBlockHeader
                  { _bhPrevHash = Hash { unHash = -496724121815684163 }
                  , _bhSlot = Slot { unSlot = 43 }
                  , _bhIssuer = VKey Owner { unOwner = 0 }
                  , _bhSig =
                      Sig Hash { unHash = 5313759645883497668 } Owner { unOwner = 0 }
                  , _bhUtxoHash = Hash { unHash = 839657738087498284 }
                  , _bhDlgHash = Hash { unHash = 839657738087498284 }
                  , _bhUpdHash = Hash { unHash = 0 }
                  }
            , _bBody =
                BlockBody
                  { _bDCerts = []
                  , _bUtxo = []
                  , _bUpdProp = Nothing
                  , _bUpdVotes = []
                  , _bProtVer = ProtVer { _pvMaj = 0 , _pvMin = 0 , _pvAlt = 0 }
                  }
            }
        )
      , ( ( Slot { unSlot = 36 }
          , fromList
              [ VKeyGenesis { unVKeyGenesis = VKey Owner { unOwner = 0 } }
              , VKeyGenesis { unVKeyGenesis = VKey Owner { unOwner = 0 } }
              ]
          , Hash { unHash = -496724121815684163 }
          , UTxOState
              { utxo =
                  UTxO
                    { unUTxO =
                        fromList
                          [ ( TxIn TxId { getTxId = Hash { unHash = 0 } } 0
                            , TxOut
                                { addr = Addr (VKey Owner { unOwner = 0 })
                                , value = Lovelace { unLovelace = 6733 }
                                }
                            )
                          , ( TxIn TxId { getTxId = Hash { unHash = 1 } } 0
                            , TxOut
                                { addr = Addr (VKey Owner { unOwner = 1 })
                                , value = Lovelace { unLovelace = 4795 }
                                }
                            )
                          , ( TxIn TxId { getTxId = Hash { unHash = 2 } } 0
                            , TxOut
                                { addr = Addr (VKey Owner { unOwner = 2 })
                                , value = Lovelace { unLovelace = 8020 }
                                }
                            )
                          , ( TxIn TxId { getTxId = Hash { unHash = 3 } } 0
                            , TxOut
                                { addr = Addr (VKey Owner { unOwner = 3 })
                                , value = Lovelace { unLovelace = 1631 }
                                }
                            )
                          , ( TxIn TxId { getTxId = Hash { unHash = 4 } } 0
                            , TxOut
                                { addr = Addr (VKey Owner { unOwner = 4 })
                                , value = Lovelace { unLovelace = 7829 }
                                }
                            )
                          , ( TxIn TxId { getTxId = Hash { unHash = 7 } } 0
                            , TxOut
                                { addr = Addr (VKey Owner { unOwner = 7 })
                                , value = Lovelace { unLovelace = 6863 }
                                }
                            )
                          , ( TxIn TxId { getTxId = Hash { unHash = 8 } } 0
                            , TxOut
                                { addr = Addr (VKey Owner { unOwner = 8 })
                                , value = Lovelace { unLovelace = 8623 }
                                }
                            )
                          , ( TxIn TxId { getTxId = Hash { unHash = 9 } } 0
                            , TxOut
                                { addr = Addr (VKey Owner { unOwner = 9 })
                                , value = Lovelace { unLovelace = 7230 }
                                }
                            )
                          ]
                    }
              , reserves = Lovelace { unLovelace = 44999999999948276 }
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
                  , _bkSgnCntT = 2.071538545155317
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
                [ ( UpId 3
                  , ( ProtVer { _pvMaj = 0 , _pvMin = 1 , _pvAlt = 0 }
                    , PParams
                        { _maxBkSz = 17739
                        , _maxHdrSz = 993
                        , _maxTxSz = 2021
                        , _maxPropSz = 16
                        , _bkSgnCntT = 2.077941229871697
                        , _bkSlotsPerEpoch = SlotCount { unSlotCount = 10 }
                        , _upTtl = SlotCount { unSlotCount = 20 }
                        , _scriptVersion = 1
                        , _upAdptThd = 0.2614549740866021
                        , _factorA = 2
                        , _factorB = 0
                        }
                    )
                  )
                ]
            , fromList [ ( UpId 3 , ( ApName "3DK" , ApVer 0 , Metadata ) ) ]
            , fromList [ ( UpId 3 , Slot { unSlot = 36 } ) ]
            , fromList
                [ ( UpId 3
                  , VKeyGenesis { unVKeyGenesis = VKey Owner { unOwner = 0 } }
                  )
                ]
            , fromList []
            , fromList [ ( UpId 3 , Slot { unSlot = 36 } ) ]
            )
          )
        , Block
            { _bHeader =
                MkBlockHeader
                  { _bhPrevHash = Hash { unHash = -6364631995355836823 }
                  , _bhSlot = Slot { unSlot = 36 }
                  , _bhIssuer = VKey Owner { unOwner = 0 }
                  , _bhSig =
                      Sig Hash { unHash = -496724121815684163 } Owner { unOwner = 0 }
                  , _bhUtxoHash = Hash { unHash = 839657738087498284 }
                  , _bhDlgHash = Hash { unHash = 839657738087498284 }
                  , _bhUpdHash = Hash { unHash = 6191864222830453908 }
                  }
            , _bBody =
                BlockBody
                  { _bDCerts = []
                  , _bUtxo = []
                  , _bUpdProp =
                      Just
                        UProp
                          { _upId = UpId 3
                          , _upIssuer = VKey Owner { unOwner = 0 }
                          , _upParams =
                              PParams
                                { _maxBkSz = 17739
                                , _maxHdrSz = 993
                                , _maxTxSz = 2021
                                , _maxPropSz = 16
                                , _bkSgnCntT = 2.077941229871697
                                , _bkSlotsPerEpoch = SlotCount { unSlotCount = 10 }
                                , _upTtl = SlotCount { unSlotCount = 20 }
                                , _scriptVersion = 1
                                , _upAdptThd = 0.2614549740866021
                                , _factorA = 2
                                , _factorB = 0
                                }
                          , _upPV = ProtVer { _pvMaj = 0 , _pvMin = 1 , _pvAlt = 0 }
                          , _upSwVer = SwVer { _svName = ApName "3DK" , _svVer = ApVer 0 }
                          , _upSig =
                              Sig
                                ( ProtVer { _pvMaj = 0 , _pvMin = 1 , _pvAlt = 0 }
                                , PParams
                                    { _maxBkSz = 17739
                                    , _maxHdrSz = 993
                                    , _maxTxSz = 2021
                                    , _maxPropSz = 16
                                    , _bkSgnCntT = 2.077941229871697
                                    , _bkSlotsPerEpoch = SlotCount { unSlotCount = 10 }
                                    , _upTtl = SlotCount { unSlotCount = 20 }
                                    , _scriptVersion = 1
                                    , _upAdptThd = 0.2614549740866021
                                    , _factorA = 2
                                    , _factorB = 0
                                    }
                                , SwVer { _svName = ApName "3DK" , _svVer = ApVer 0 }
                                )
                                Owner { unOwner = 0 }
                          , _upSTags = fromList [ "" , "d]" ]
                          , _upMdt = Metadata
                          }
                  , _bUpdVotes =
                      [ Vote
                          { _vCaster = VKey Owner { unOwner = 0 }
                          , _vPropId = UpId 3
                          , _vSig = Sig (UpId 3) Owner { unOwner = 0 }
                          }
                      ]
                  , _bProtVer = ProtVer { _pvMaj = 0 , _pvMin = 0 , _pvAlt = 0 }
                  }
            }
        )
      , ( ( Slot { unSlot = 31 }
          , fromList
              [ VKeyGenesis { unVKeyGenesis = VKey Owner { unOwner = 0 } }
              , VKeyGenesis { unVKeyGenesis = VKey Owner { unOwner = 0 } }
              ]
          , Hash { unHash = -6364631995355836823 }
          , UTxOState
              { utxo =
                  UTxO
                    { unUTxO =
                        fromList
                          [ ( TxIn TxId { getTxId = Hash { unHash = 0 } } 0
                            , TxOut
                                { addr = Addr (VKey Owner { unOwner = 0 })
                                , value = Lovelace { unLovelace = 6733 }
                                }
                            )
                          , ( TxIn TxId { getTxId = Hash { unHash = 1 } } 0
                            , TxOut
                                { addr = Addr (VKey Owner { unOwner = 1 })
                                , value = Lovelace { unLovelace = 4795 }
                                }
                            )
                          , ( TxIn TxId { getTxId = Hash { unHash = 2 } } 0
                            , TxOut
                                { addr = Addr (VKey Owner { unOwner = 2 })
                                , value = Lovelace { unLovelace = 8020 }
                                }
                            )
                          , ( TxIn TxId { getTxId = Hash { unHash = 3 } } 0
                            , TxOut
                                { addr = Addr (VKey Owner { unOwner = 3 })
                                , value = Lovelace { unLovelace = 1631 }
                                }
                            )
                          , ( TxIn TxId { getTxId = Hash { unHash = 4 } } 0
                            , TxOut
                                { addr = Addr (VKey Owner { unOwner = 4 })
                                , value = Lovelace { unLovelace = 7829 }
                                }
                            )
                          , ( TxIn TxId { getTxId = Hash { unHash = 7 } } 0
                            , TxOut
                                { addr = Addr (VKey Owner { unOwner = 7 })
                                , value = Lovelace { unLovelace = 6863 }
                                }
                            )
                          , ( TxIn TxId { getTxId = Hash { unHash = 8 } } 0
                            , TxOut
                                { addr = Addr (VKey Owner { unOwner = 8 })
                                , value = Lovelace { unLovelace = 8623 }
                                }
                            )
                          , ( TxIn TxId { getTxId = Hash { unHash = 9 } } 0
                            , TxOut
                                { addr = Addr (VKey Owner { unOwner = 9 })
                                , value = Lovelace { unLovelace = 7230 }
                                }
                            )
                          ]
                    }
              , reserves = Lovelace { unLovelace = 44999999999948276 }
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
                  , _bkSgnCntT = 2.071538545155317
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
        , Block
            { _bHeader =
                MkBlockHeader
                  { _bhPrevHash = Hash { unHash = -7034666815307557948 }
                  , _bhSlot = Slot { unSlot = 31 }
                  , _bhIssuer = VKey Owner { unOwner = 0 }
                  , _bhSig =
                      Sig Hash { unHash = -6364631995355836823 } Owner { unOwner = 0 }
                  , _bhUtxoHash = Hash { unHash = 839657738087498284 }
                  , _bhDlgHash = Hash { unHash = 839657738087498284 }
                  , _bhUpdHash = Hash { unHash = 0 }
                  }
            , _bBody =
                BlockBody
                  { _bDCerts = []
                  , _bUtxo = []
                  , _bUpdProp = Nothing
                  , _bUpdVotes = []
                  , _bProtVer = ProtVer { _pvMaj = 0 , _pvMin = 0 , _pvAlt = 0 }
                  }
            }
        )
      , ( ( Slot { unSlot = 27 }
          , fromList
              [ VKeyGenesis { unVKeyGenesis = VKey Owner { unOwner = 0 } }
              , VKeyGenesis { unVKeyGenesis = VKey Owner { unOwner = 0 } }
              ]
          , Hash { unHash = -7034666815307557948 }
          , UTxOState
              { utxo =
                  UTxO
                    { unUTxO =
                        fromList
                          [ ( TxIn TxId { getTxId = Hash { unHash = 0 } } 0
                            , TxOut
                                { addr = Addr (VKey Owner { unOwner = 0 })
                                , value = Lovelace { unLovelace = 6733 }
                                }
                            )
                          , ( TxIn TxId { getTxId = Hash { unHash = 1 } } 0
                            , TxOut
                                { addr = Addr (VKey Owner { unOwner = 1 })
                                , value = Lovelace { unLovelace = 4795 }
                                }
                            )
                          , ( TxIn TxId { getTxId = Hash { unHash = 2 } } 0
                            , TxOut
                                { addr = Addr (VKey Owner { unOwner = 2 })
                                , value = Lovelace { unLovelace = 8020 }
                                }
                            )
                          , ( TxIn TxId { getTxId = Hash { unHash = 3 } } 0
                            , TxOut
                                { addr = Addr (VKey Owner { unOwner = 3 })
                                , value = Lovelace { unLovelace = 1631 }
                                }
                            )
                          , ( TxIn TxId { getTxId = Hash { unHash = 4 } } 0
                            , TxOut
                                { addr = Addr (VKey Owner { unOwner = 4 })
                                , value = Lovelace { unLovelace = 7829 }
                                }
                            )
                          , ( TxIn TxId { getTxId = Hash { unHash = 7 } } 0
                            , TxOut
                                { addr = Addr (VKey Owner { unOwner = 7 })
                                , value = Lovelace { unLovelace = 6863 }
                                }
                            )
                          , ( TxIn TxId { getTxId = Hash { unHash = 8 } } 0
                            , TxOut
                                { addr = Addr (VKey Owner { unOwner = 8 })
                                , value = Lovelace { unLovelace = 8623 }
                                }
                            )
                          , ( TxIn TxId { getTxId = Hash { unHash = 9 } } 0
                            , TxOut
                                { addr = Addr (VKey Owner { unOwner = 9 })
                                , value = Lovelace { unLovelace = 7230 }
                                }
                            )
                          ]
                    }
              , reserves = Lovelace { unLovelace = 44999999999948276 }
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
                  , _bkSgnCntT = 2.071538545155317
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
                [ ( UpId 0
                  , ( ProtVer { _pvMaj = 0 , _pvMin = 1 , _pvAlt = 0 }
                    , PParams
                        { _maxBkSz = 14650
                        , _maxHdrSz = 1090
                        , _maxTxSz = 2842
                        , _maxPropSz = 12
                        , _bkSgnCntT = 2.0642003815119785
                        , _bkSlotsPerEpoch = SlotCount { unSlotCount = 10 }
                        , _upTtl = SlotCount { unSlotCount = 6 }
                        , _scriptVersion = 0
                        , _upAdptThd = 0.6326888533778547
                        , _factorA = 4
                        , _factorB = 3
                        }
                    )
                  )
                ]
            , fromList [ ( UpId 0 , ( ApName "" , ApVer 0 , Metadata ) ) ]
            , fromList []
            , fromList []
            , fromList []
            , fromList [ ( UpId 0 , Slot { unSlot = 20 } ) ]
            )
          )
        , Block
            { _bHeader =
                MkBlockHeader
                  { _bhPrevHash = Hash { unHash = 7399332426686957517 }
                  , _bhSlot = Slot { unSlot = 27 }
                  , _bhIssuer = VKey Owner { unOwner = 0 }
                  , _bhSig =
                      Sig Hash { unHash = -7034666815307557948 } Owner { unOwner = 0 }
                  , _bhUtxoHash = Hash { unHash = 839657738087498284 }
                  , _bhDlgHash = Hash { unHash = 839657738087498284 }
                  , _bhUpdHash = Hash { unHash = 0 }
                  }
            , _bBody =
                BlockBody
                  { _bDCerts = []
                  , _bUtxo = []
                  , _bUpdProp = Nothing
                  , _bUpdVotes = []
                  , _bProtVer = ProtVer { _pvMaj = 0 , _pvMin = 0 , _pvAlt = 0 }
                  }
            }
        )
      , ( ( Slot { unSlot = 20 }
          , fromList
              [ VKeyGenesis { unVKeyGenesis = VKey Owner { unOwner = 0 } }
              , VKeyGenesis { unVKeyGenesis = VKey Owner { unOwner = 0 } }
              ]
          , Hash { unHash = 7399332426686957517 }
          , UTxOState
              { utxo =
                  UTxO
                    { unUTxO =
                        fromList
                          [ ( TxIn TxId { getTxId = Hash { unHash = 0 } } 0
                            , TxOut
                                { addr = Addr (VKey Owner { unOwner = 0 })
                                , value = Lovelace { unLovelace = 6733 }
                                }
                            )
                          , ( TxIn TxId { getTxId = Hash { unHash = 1 } } 0
                            , TxOut
                                { addr = Addr (VKey Owner { unOwner = 1 })
                                , value = Lovelace { unLovelace = 4795 }
                                }
                            )
                          , ( TxIn TxId { getTxId = Hash { unHash = 2 } } 0
                            , TxOut
                                { addr = Addr (VKey Owner { unOwner = 2 })
                                , value = Lovelace { unLovelace = 8020 }
                                }
                            )
                          , ( TxIn TxId { getTxId = Hash { unHash = 3 } } 0
                            , TxOut
                                { addr = Addr (VKey Owner { unOwner = 3 })
                                , value = Lovelace { unLovelace = 1631 }
                                }
                            )
                          , ( TxIn TxId { getTxId = Hash { unHash = 4 } } 0
                            , TxOut
                                { addr = Addr (VKey Owner { unOwner = 4 })
                                , value = Lovelace { unLovelace = 7829 }
                                }
                            )
                          , ( TxIn TxId { getTxId = Hash { unHash = 7 } } 0
                            , TxOut
                                { addr = Addr (VKey Owner { unOwner = 7 })
                                , value = Lovelace { unLovelace = 6863 }
                                }
                            )
                          , ( TxIn TxId { getTxId = Hash { unHash = 8 } } 0
                            , TxOut
                                { addr = Addr (VKey Owner { unOwner = 8 })
                                , value = Lovelace { unLovelace = 8623 }
                                }
                            )
                          , ( TxIn TxId { getTxId = Hash { unHash = 9 } } 0
                            , TxOut
                                { addr = Addr (VKey Owner { unOwner = 9 })
                                , value = Lovelace { unLovelace = 7230 }
                                }
                            )
                          ]
                    }
              , reserves = Lovelace { unLovelace = 44999999999948276 }
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
                  , _bkSgnCntT = 2.071538545155317
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
                [ ( UpId 0
                  , ( ProtVer { _pvMaj = 0 , _pvMin = 1 , _pvAlt = 0 }
                    , PParams
                        { _maxBkSz = 14650
                        , _maxHdrSz = 1090
                        , _maxTxSz = 2842
                        , _maxPropSz = 12
                        , _bkSgnCntT = 2.0642003815119785
                        , _bkSlotsPerEpoch = SlotCount { unSlotCount = 10 }
                        , _upTtl = SlotCount { unSlotCount = 6 }
                        , _scriptVersion = 0
                        , _upAdptThd = 0.6326888533778547
                        , _factorA = 4
                        , _factorB = 3
                        }
                    )
                  )
                ]
            , fromList [ ( UpId 0 , ( ApName "" , ApVer 0 , Metadata ) ) ]
            , fromList []
            , fromList []
            , fromList []
            , fromList [ ( UpId 0 , Slot { unSlot = 20 } ) ]
            )
          )
        , Block
            { _bHeader =
                MkBlockHeader
                  { _bhPrevHash = Hash { unHash = 7160429774867247529 }
                  , _bhSlot = Slot { unSlot = 20 }
                  , _bhIssuer = VKey Owner { unOwner = 0 }
                  , _bhSig =
                      Sig Hash { unHash = 7399332426686957517 } Owner { unOwner = 0 }
                  , _bhUtxoHash = Hash { unHash = 839657738087498284 }
                  , _bhDlgHash = Hash { unHash = 839657738087498284 }
                  , _bhUpdHash = Hash { unHash = 3929471932281199052 }
                  }
            , _bBody =
                BlockBody
                  { _bDCerts = []
                  , _bUtxo = []
                  , _bUpdProp =
                      Just
                        UProp
                          { _upId = UpId 0
                          , _upIssuer = VKey Owner { unOwner = 0 }
                          , _upParams =
                              PParams
                                { _maxBkSz = 14650
                                , _maxHdrSz = 1090
                                , _maxTxSz = 2842
                                , _maxPropSz = 12
                                , _bkSgnCntT = 2.0642003815119785
                                , _bkSlotsPerEpoch = SlotCount { unSlotCount = 10 }
                                , _upTtl = SlotCount { unSlotCount = 6 }
                                , _scriptVersion = 0
                                , _upAdptThd = 0.6326888533778547
                                , _factorA = 4
                                , _factorB = 3
                                }
                          , _upPV = ProtVer { _pvMaj = 0 , _pvMin = 1 , _pvAlt = 0 }
                          , _upSwVer = SwVer { _svName = ApName "" , _svVer = ApVer 0 }
                          , _upSig =
                              Sig
                                ( ProtVer { _pvMaj = 0 , _pvMin = 1 , _pvAlt = 0 }
                                , PParams
                                    { _maxBkSz = 14650
                                    , _maxHdrSz = 1090
                                    , _maxTxSz = 2842
                                    , _maxPropSz = 12
                                    , _bkSgnCntT = 2.0642003815119785
                                    , _bkSlotsPerEpoch = SlotCount { unSlotCount = 10 }
                                    , _upTtl = SlotCount { unSlotCount = 6 }
                                    , _scriptVersion = 0
                                    , _upAdptThd = 0.6326888533778547
                                    , _factorA = 4
                                    , _factorB = 3
                                    }
                                , SwVer { _svName = ApName "" , _svVer = ApVer 0 }
                                )
                                Owner { unOwner = 0 }
                          , _upSTags = fromList [ "\EM\nI\CANyu/[" , "\US" , "w" ]
                          , _upMdt = Metadata
                          }
                  , _bUpdVotes = []
                  , _bProtVer = ProtVer { _pvMaj = 0 , _pvMin = 0 , _pvAlt = 0 }
                  }
            }
        )
      , ( ( Slot { unSlot = 17 }
          , fromList
              [ VKeyGenesis { unVKeyGenesis = VKey Owner { unOwner = 0 } }
              , VKeyGenesis { unVKeyGenesis = VKey Owner { unOwner = 0 } }
              ]
          , Hash { unHash = 7160429774867247529 }
          , UTxOState
              { utxo =
                  UTxO
                    { unUTxO =
                        fromList
                          [ ( TxIn TxId { getTxId = Hash { unHash = 0 } } 0
                            , TxOut
                                { addr = Addr (VKey Owner { unOwner = 0 })
                                , value = Lovelace { unLovelace = 6733 }
                                }
                            )
                          , ( TxIn TxId { getTxId = Hash { unHash = 1 } } 0
                            , TxOut
                                { addr = Addr (VKey Owner { unOwner = 1 })
                                , value = Lovelace { unLovelace = 4795 }
                                }
                            )
                          , ( TxIn TxId { getTxId = Hash { unHash = 2 } } 0
                            , TxOut
                                { addr = Addr (VKey Owner { unOwner = 2 })
                                , value = Lovelace { unLovelace = 8020 }
                                }
                            )
                          , ( TxIn TxId { getTxId = Hash { unHash = 3 } } 0
                            , TxOut
                                { addr = Addr (VKey Owner { unOwner = 3 })
                                , value = Lovelace { unLovelace = 1631 }
                                }
                            )
                          , ( TxIn TxId { getTxId = Hash { unHash = 4 } } 0
                            , TxOut
                                { addr = Addr (VKey Owner { unOwner = 4 })
                                , value = Lovelace { unLovelace = 7829 }
                                }
                            )
                          , ( TxIn TxId { getTxId = Hash { unHash = 7 } } 0
                            , TxOut
                                { addr = Addr (VKey Owner { unOwner = 7 })
                                , value = Lovelace { unLovelace = 6863 }
                                }
                            )
                          , ( TxIn TxId { getTxId = Hash { unHash = 8 } } 0
                            , TxOut
                                { addr = Addr (VKey Owner { unOwner = 8 })
                                , value = Lovelace { unLovelace = 8623 }
                                }
                            )
                          , ( TxIn TxId { getTxId = Hash { unHash = 9 } } 0
                            , TxOut
                                { addr = Addr (VKey Owner { unOwner = 9 })
                                , value = Lovelace { unLovelace = 7230 }
                                }
                            )
                          ]
                    }
              , reserves = Lovelace { unLovelace = 44999999999948276 }
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
                  , _bkSgnCntT = 2.071538545155317
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
        , Block
            { _bHeader =
                MkBlockHeader
                  { _bhPrevHash = Hash { unHash = 7007312888132765430 }
                  , _bhSlot = Slot { unSlot = 17 }
                  , _bhIssuer = VKey Owner { unOwner = 0 }
                  , _bhSig =
                      Sig Hash { unHash = 7160429774867247529 } Owner { unOwner = 0 }
                  , _bhUtxoHash = Hash { unHash = 839657738087498284 }
                  , _bhDlgHash = Hash { unHash = 839657738087498284 }
                  , _bhUpdHash = Hash { unHash = 0 }
                  }
            , _bBody =
                BlockBody
                  { _bDCerts = []
                  , _bUtxo = []
                  , _bUpdProp = Nothing
                  , _bUpdVotes = []
                  , _bProtVer = ProtVer { _pvMaj = 0 , _pvMin = 0 , _pvAlt = 0 }
                  }
            }
        )
      , ( ( Slot { unSlot = 12 }
          , fromList
              [ VKeyGenesis { unVKeyGenesis = VKey Owner { unOwner = 0 } }
              , VKeyGenesis { unVKeyGenesis = VKey Owner { unOwner = 0 } }
              ]
          , Hash { unHash = 7007312888132765430 }
          , UTxOState
              { utxo =
                  UTxO
                    { unUTxO =
                        fromList
                          [ ( TxIn TxId { getTxId = Hash { unHash = 0 } } 0
                            , TxOut
                                { addr = Addr (VKey Owner { unOwner = 0 })
                                , value = Lovelace { unLovelace = 6733 }
                                }
                            )
                          , ( TxIn TxId { getTxId = Hash { unHash = 1 } } 0
                            , TxOut
                                { addr = Addr (VKey Owner { unOwner = 1 })
                                , value = Lovelace { unLovelace = 4795 }
                                }
                            )
                          , ( TxIn TxId { getTxId = Hash { unHash = 2 } } 0
                            , TxOut
                                { addr = Addr (VKey Owner { unOwner = 2 })
                                , value = Lovelace { unLovelace = 8020 }
                                }
                            )
                          , ( TxIn TxId { getTxId = Hash { unHash = 3 } } 0
                            , TxOut
                                { addr = Addr (VKey Owner { unOwner = 3 })
                                , value = Lovelace { unLovelace = 1631 }
                                }
                            )
                          , ( TxIn TxId { getTxId = Hash { unHash = 4 } } 0
                            , TxOut
                                { addr = Addr (VKey Owner { unOwner = 4 })
                                , value = Lovelace { unLovelace = 7829 }
                                }
                            )
                          , ( TxIn TxId { getTxId = Hash { unHash = 7 } } 0
                            , TxOut
                                { addr = Addr (VKey Owner { unOwner = 7 })
                                , value = Lovelace { unLovelace = 6863 }
                                }
                            )
                          , ( TxIn TxId { getTxId = Hash { unHash = 8 } } 0
                            , TxOut
                                { addr = Addr (VKey Owner { unOwner = 8 })
                                , value = Lovelace { unLovelace = 8623 }
                                }
                            )
                          , ( TxIn TxId { getTxId = Hash { unHash = 9 } } 0
                            , TxOut
                                { addr = Addr (VKey Owner { unOwner = 9 })
                                , value = Lovelace { unLovelace = 7230 }
                                }
                            )
                          ]
                    }
              , reserves = Lovelace { unLovelace = 44999999999948276 }
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
                  , _bkSgnCntT = 2.071538545155317
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
                [ ( UpId 1
                  , ( ProtVer { _pvMaj = 1 , _pvMin = 0 , _pvAlt = 0 }
                    , PParams
                        { _maxBkSz = 16666
                        , _maxHdrSz = 1185
                        , _maxTxSz = 3312
                        , _maxPropSz = 10
                        , _bkSgnCntT = 2.0778685295671804
                        , _bkSlotsPerEpoch = SlotCount { unSlotCount = 10 }
                        , _upTtl = SlotCount { unSlotCount = 6 }
                        , _scriptVersion = 1
                        , _upAdptThd = 1.0
                        , _factorA = 6
                        , _factorB = 0
                        }
                    )
                  )
                ]
            , fromList
                [ ( UpId 1 , ( ApName "VX2(\ETXf\DC4yM" , ApVer 0 , Metadata ) ) ]
            , fromList []
            , fromList []
            , fromList []
            , fromList [ ( UpId 1 , Slot { unSlot = 3 } ) ]
            )
          )
        , Block
            { _bHeader =
                MkBlockHeader
                  { _bhPrevHash = Hash { unHash = 5393371259205618634 }
                  , _bhSlot = Slot { unSlot = 12 }
                  , _bhIssuer = VKey Owner { unOwner = 0 }
                  , _bhSig =
                      Sig Hash { unHash = 7007312888132765430 } Owner { unOwner = 0 }
                  , _bhUtxoHash = Hash { unHash = 839657738087498284 }
                  , _bhDlgHash = Hash { unHash = 839657738087498284 }
                  , _bhUpdHash = Hash { unHash = 0 }
                  }
            , _bBody =
                BlockBody
                  { _bDCerts = []
                  , _bUtxo = []
                  , _bUpdProp = Nothing
                  , _bUpdVotes = []
                  , _bProtVer = ProtVer { _pvMaj = 0 , _pvMin = 0 , _pvAlt = 0 }
                  }
            }
        )
      , ( ( Slot { unSlot = 9 }
          , fromList
              [ VKeyGenesis { unVKeyGenesis = VKey Owner { unOwner = 0 } }
              , VKeyGenesis { unVKeyGenesis = VKey Owner { unOwner = 0 } }
              ]
          , Hash { unHash = 5393371259205618634 }
          , UTxOState
              { utxo =
                  UTxO
                    { unUTxO =
                        fromList
                          [ ( TxIn TxId { getTxId = Hash { unHash = 0 } } 0
                            , TxOut
                                { addr = Addr (VKey Owner { unOwner = 0 })
                                , value = Lovelace { unLovelace = 6733 }
                                }
                            )
                          , ( TxIn TxId { getTxId = Hash { unHash = 1 } } 0
                            , TxOut
                                { addr = Addr (VKey Owner { unOwner = 1 })
                                , value = Lovelace { unLovelace = 4795 }
                                }
                            )
                          , ( TxIn TxId { getTxId = Hash { unHash = 2 } } 0
                            , TxOut
                                { addr = Addr (VKey Owner { unOwner = 2 })
                                , value = Lovelace { unLovelace = 8020 }
                                }
                            )
                          , ( TxIn TxId { getTxId = Hash { unHash = 3 } } 0
                            , TxOut
                                { addr = Addr (VKey Owner { unOwner = 3 })
                                , value = Lovelace { unLovelace = 1631 }
                                }
                            )
                          , ( TxIn TxId { getTxId = Hash { unHash = 4 } } 0
                            , TxOut
                                { addr = Addr (VKey Owner { unOwner = 4 })
                                , value = Lovelace { unLovelace = 7829 }
                                }
                            )
                          , ( TxIn TxId { getTxId = Hash { unHash = 7 } } 0
                            , TxOut
                                { addr = Addr (VKey Owner { unOwner = 7 })
                                , value = Lovelace { unLovelace = 6863 }
                                }
                            )
                          , ( TxIn TxId { getTxId = Hash { unHash = 8 } } 0
                            , TxOut
                                { addr = Addr (VKey Owner { unOwner = 8 })
                                , value = Lovelace { unLovelace = 8623 }
                                }
                            )
                          , ( TxIn TxId { getTxId = Hash { unHash = 9 } } 0
                            , TxOut
                                { addr = Addr (VKey Owner { unOwner = 9 })
                                , value = Lovelace { unLovelace = 7230 }
                                }
                            )
                          ]
                    }
              , reserves = Lovelace { unLovelace = 44999999999948276 }
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
                  , _bkSgnCntT = 2.071538545155317
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
                [ ( UpId 1
                  , ( ProtVer { _pvMaj = 1 , _pvMin = 0 , _pvAlt = 0 }
                    , PParams
                        { _maxBkSz = 16666
                        , _maxHdrSz = 1185
                        , _maxTxSz = 3312
                        , _maxPropSz = 10
                        , _bkSgnCntT = 2.0778685295671804
                        , _bkSlotsPerEpoch = SlotCount { unSlotCount = 10 }
                        , _upTtl = SlotCount { unSlotCount = 6 }
                        , _scriptVersion = 1
                        , _upAdptThd = 1.0
                        , _factorA = 6
                        , _factorB = 0
                        }
                    )
                  )
                ]
            , fromList
                [ ( UpId 1 , ( ApName "VX2(\ETXf\DC4yM" , ApVer 0 , Metadata ) ) ]
            , fromList []
            , fromList []
            , fromList []
            , fromList [ ( UpId 1 , Slot { unSlot = 3 } ) ]
            )
          )
        , Block
            { _bHeader =
                MkBlockHeader
                  { _bhPrevHash = Hash { unHash = -7234408896584323075 }
                  , _bhSlot = Slot { unSlot = 9 }
                  , _bhIssuer = VKey Owner { unOwner = 0 }
                  , _bhSig =
                      Sig Hash { unHash = 5393371259205618634 } Owner { unOwner = 0 }
                  , _bhUtxoHash = Hash { unHash = 839657738087498284 }
                  , _bhDlgHash = Hash { unHash = 839657738087498284 }
                  , _bhUpdHash = Hash { unHash = 0 }
                  }
            , _bBody =
                BlockBody
                  { _bDCerts = []
                  , _bUtxo = []
                  , _bUpdProp = Nothing
                  , _bUpdVotes = []
                  , _bProtVer = ProtVer { _pvMaj = 0 , _pvMin = 0 , _pvAlt = 0 }
                  }
            }
        )
      , ( ( Slot { unSlot = 3 }
          , fromList
              [ VKeyGenesis { unVKeyGenesis = VKey Owner { unOwner = 0 } } ]
          , Hash { unHash = -7234408896584323075 }
          , UTxOState
              { utxo =
                  UTxO
                    { unUTxO =
                        fromList
                          [ ( TxIn TxId { getTxId = Hash { unHash = 0 } } 0
                            , TxOut
                                { addr = Addr (VKey Owner { unOwner = 0 })
                                , value = Lovelace { unLovelace = 6733 }
                                }
                            )
                          , ( TxIn TxId { getTxId = Hash { unHash = 1 } } 0
                            , TxOut
                                { addr = Addr (VKey Owner { unOwner = 1 })
                                , value = Lovelace { unLovelace = 4795 }
                                }
                            )
                          , ( TxIn TxId { getTxId = Hash { unHash = 2 } } 0
                            , TxOut
                                { addr = Addr (VKey Owner { unOwner = 2 })
                                , value = Lovelace { unLovelace = 8020 }
                                }
                            )
                          , ( TxIn TxId { getTxId = Hash { unHash = 3 } } 0
                            , TxOut
                                { addr = Addr (VKey Owner { unOwner = 3 })
                                , value = Lovelace { unLovelace = 1631 }
                                }
                            )
                          , ( TxIn TxId { getTxId = Hash { unHash = 4 } } 0
                            , TxOut
                                { addr = Addr (VKey Owner { unOwner = 4 })
                                , value = Lovelace { unLovelace = 7829 }
                                }
                            )
                          , ( TxIn TxId { getTxId = Hash { unHash = 7 } } 0
                            , TxOut
                                { addr = Addr (VKey Owner { unOwner = 7 })
                                , value = Lovelace { unLovelace = 6863 }
                                }
                            )
                          , ( TxIn TxId { getTxId = Hash { unHash = 8 } } 0
                            , TxOut
                                { addr = Addr (VKey Owner { unOwner = 8 })
                                , value = Lovelace { unLovelace = 8623 }
                                }
                            )
                          , ( TxIn TxId { getTxId = Hash { unHash = 9 } } 0
                            , TxOut
                                { addr = Addr (VKey Owner { unOwner = 9 })
                                , value = Lovelace { unLovelace = 7230 }
                                }
                            )
                          ]
                    }
              , reserves = Lovelace { unLovelace = 44999999999948276 }
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
                  , _bkSgnCntT = 2.071538545155317
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
                [ ( UpId 1
                  , ( ProtVer { _pvMaj = 1 , _pvMin = 0 , _pvAlt = 0 }
                    , PParams
                        { _maxBkSz = 16666
                        , _maxHdrSz = 1185
                        , _maxTxSz = 3312
                        , _maxPropSz = 10
                        , _bkSgnCntT = 2.0778685295671804
                        , _bkSlotsPerEpoch = SlotCount { unSlotCount = 10 }
                        , _upTtl = SlotCount { unSlotCount = 6 }
                        , _scriptVersion = 1
                        , _upAdptThd = 1.0
                        , _factorA = 6
                        , _factorB = 0
                        }
                    )
                  )
                ]
            , fromList
                [ ( UpId 1 , ( ApName "VX2(\ETXf\DC4yM" , ApVer 0 , Metadata ) ) ]
            , fromList []
            , fromList []
            , fromList []
            , fromList [ ( UpId 1 , Slot { unSlot = 3 } ) ]
            )
          )
        , Block
            { _bHeader =
                MkBlockHeader
                  { _bhPrevHash = Hash { unHash = -2578643520546668380 }
                  , _bhSlot = Slot { unSlot = 3 }
                  , _bhIssuer = VKey Owner { unOwner = 0 }
                  , _bhSig =
                      Sig Hash { unHash = -7234408896584323075 } Owner { unOwner = 0 }
                  , _bhUtxoHash = Hash { unHash = 839657738087498284 }
                  , _bhDlgHash = Hash { unHash = 839657738087498284 }
                  , _bhUpdHash = Hash { unHash = 8743001241490413738 }
                  }
            , _bBody =
                BlockBody
                  { _bDCerts = []
                  , _bUtxo = []
                  , _bUpdProp =
                      Just
                        UProp
                          { _upId = UpId 1
                          , _upIssuer = VKey Owner { unOwner = 0 }
                          , _upParams =
                              PParams
                                { _maxBkSz = 16666
                                , _maxHdrSz = 1185
                                , _maxTxSz = 3312
                                , _maxPropSz = 10
                                , _bkSgnCntT = 2.0778685295671804
                                , _bkSlotsPerEpoch = SlotCount { unSlotCount = 10 }
                                , _upTtl = SlotCount { unSlotCount = 6 }
                                , _scriptVersion = 1
                                , _upAdptThd = 1.0
                                , _factorA = 6
                                , _factorB = 0
                                }
                          , _upPV = ProtVer { _pvMaj = 1 , _pvMin = 0 , _pvAlt = 0 }
                          , _upSwVer =
                              SwVer { _svName = ApName "VX2(\ETXf\DC4yM" , _svVer = ApVer 0 }
                          , _upSig =
                              Sig
                                ( ProtVer { _pvMaj = 1 , _pvMin = 0 , _pvAlt = 0 }
                                , PParams
                                    { _maxBkSz = 16666
                                    , _maxHdrSz = 1185
                                    , _maxTxSz = 3312
                                    , _maxPropSz = 10
                                    , _bkSgnCntT = 2.0778685295671804
                                    , _bkSlotsPerEpoch = SlotCount { unSlotCount = 10 }
                                    , _upTtl = SlotCount { unSlotCount = 6 }
                                    , _scriptVersion = 1
                                    , _upAdptThd = 1.0
                                    , _factorA = 6
                                    , _factorB = 0
                                    }
                                , SwVer { _svName = ApName "VX2(\ETXf\DC4yM" , _svVer = ApVer 0 }
                                )
                                Owner { unOwner = 0 }
                          , _upSTags = fromList [ "&e\SUBc" , "{" ]
                          , _upMdt = Metadata
                          }
                  , _bUpdVotes = []
                  , _bProtVer = ProtVer { _pvMaj = 0 , _pvMin = 0 , _pvAlt = 0 }
                  }
            }
        )
      ]
