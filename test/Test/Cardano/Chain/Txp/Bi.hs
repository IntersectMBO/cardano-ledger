{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE TypeApplications  #-}

module Test.Cardano.Chain.Txp.Bi
  ( tests
  )
where

import Cardano.Prelude
import Test.Cardano.Prelude

import qualified Data.Map.Strict as M
import Data.Typeable (typeRep)
import Data.Vector (Vector)

import Hedgehog (Gen, Property)
import qualified Hedgehog as H

import Cardano.Binary.Class (Bi, Case(..), LengthOf, SizeOverride(..), szCases)
import Cardano.Chain.Common (AddrAttributes(..), Attributes(..), mkAttributes)
import Cardano.Chain.Txp
  (Tx(..), TxIn(..), TxInWitness(..), TxOut(..), TxSigData(..), taTx, taWitness)
import Cardano.Crypto (ProtocolMagicId(..), SignTag(..), Signature, sign)

import Test.Cardano.Binary.Helpers (SizeTestConfig(..), scfg, sizeTest)
import Test.Cardano.Binary.Helpers.GoldenRoundTrip
  (goldenTestBi, roundTripsBiBuildable, roundTripsBiShow)
import Test.Cardano.Chain.Txp.Example
  ( exampleHashTx
  , exampleRedeemSignature
  , exampleTxId
  , exampleTxInList
  , exampleTxInUtxo
  , exampleTxOut
  , exampleTxOut1
  , exampleTxOutList
  , exampleTxPayload1
  , exampleTxProof
  , exampleTxSig
  , exampleTxSigData
  , exampleTxWitness
  )
import Test.Cardano.Chain.Txp.Gen
  ( genTx
  , genTxAttributes
  , genTxAux
  , genTxHash
  , genTxId
  , genTxIn
  , genTxInList
  , genTxInWitness
  , genTxOut
  , genTxOutList
  , genTxPayload
  , genTxProof
  , genTxSig
  , genTxSigData
  , genTxWitness
  )
import Test.Cardano.Crypto.Example
  (examplePublicKey, exampleRedeemPublicKey, exampleSecretKey)
import Test.Cardano.Crypto.Gen (feedPM)
import Test.Options (TestScenario, TSProperty, eachOfTS)


--------------------------------------------------------------------------------
-- Tx
--------------------------------------------------------------------------------

goldenTx :: Property
goldenTx = goldenTestBi tx "test/golden/bi/txp/Tx"
  where tx = UnsafeTx exampleTxInList exampleTxOutList (mkAttributes ())

ts_roundTripTx :: TSProperty
ts_roundTripTx = eachOfTS 50 genTx roundTripsBiBuildable

--------------------------------------------------------------------------------
-- TxAttributes
--------------------------------------------------------------------------------

goldenTxAttributes :: Property
goldenTxAttributes = goldenTestBi txA "test/golden/bi/txp/TxAttributes"
  where txA = mkAttributes ()


ts_roundTripTxAttributes :: TSProperty
ts_roundTripTxAttributes = eachOfTS 10 genTxAttributes roundTripsBiBuildable

--------------------------------------------------------------------------------
-- TxAux
--------------------------------------------------------------------------------

ts_roundTripTxAux :: TSProperty
ts_roundTripTxAux = eachOfTS 100 (feedPM genTxAux) roundTripsBiBuildable

--------------------------------------------------------------------------------
-- Tx Hash
--------------------------------------------------------------------------------

goldenHashTx :: Property
goldenHashTx = goldenTestBi exampleHashTx "test/golden/bi/txp/HashTx"

ts_roundTripHashTx :: TSProperty
ts_roundTripHashTx = eachOfTS 50 genTxHash roundTripsBiBuildable

--------------------------------------------------------------------------------
-- TxIn
--------------------------------------------------------------------------------


goldenTxInUtxo :: Property
goldenTxInUtxo = goldenTestBi exampleTxInUtxo "test/golden/bi/txp/TxIn_Utxo"

ts_roundTripTxIn :: TSProperty
ts_roundTripTxIn = eachOfTS 100 genTxIn roundTripsBiBuildable


--------------------------------------------------------------------------------
-- TxId
--------------------------------------------------------------------------------

goldenTxId :: Property
goldenTxId = goldenTestBi exampleTxId "test/golden/bi/txp/TxId"

ts_roundTripTxId :: TSProperty
ts_roundTripTxId = eachOfTS 50 genTxId roundTripsBiBuildable

--------------------------------------------------------------------------------
-- TxInList
--------------------------------------------------------------------------------

goldenTxInList :: Property
goldenTxInList = goldenTestBi exampleTxInList "test/golden/bi/txp/TxInList"

ts_roundTripTxInList :: TSProperty
ts_roundTripTxInList = eachOfTS 50 genTxInList roundTripsBiShow

--------------------------------------------------------------------------------
-- TxInWitness
--------------------------------------------------------------------------------

goldenPkWitness :: Property
goldenPkWitness = goldenTestBi
  pkWitness
  "test/golden/bi/txp/TxInWitness_PkWitness"
  where pkWitness = PkWitness examplePublicKey exampleTxSig

goldenRedeemWitness :: Property
goldenRedeemWitness = goldenTestBi
  redeemWitness
  "test/golden/bi/txp/TxInWitness_RedeemWitness"
 where
  redeemWitness = RedeemWitness exampleRedeemPublicKey exampleRedeemSignature

ts_roundTripTxInWitness :: TSProperty
ts_roundTripTxInWitness = eachOfTS 50 (feedPM genTxInWitness) roundTripsBiBuildable

--------------------------------------------------------------------------------
-- TxOutList
--------------------------------------------------------------------------------

goldenTxOutList :: Property
goldenTxOutList = goldenTestBi exampleTxOutList "test/golden/bi/txp/TxOutList"

ts_roundTripTxOutList :: TSProperty
ts_roundTripTxOutList = eachOfTS 50 genTxOutList roundTripsBiShow

--------------------------------------------------------------------------------
-- TxOut
--------------------------------------------------------------------------------

goldenTxOut :: Property
goldenTxOut = goldenTestBi exampleTxOut "test/golden/bi/txp/TxOut"

goldenTxOut1 :: Property
goldenTxOut1 = goldenTestBi exampleTxOut1 "test/golden/bi/txp/TxOut1"

ts_roundTripTxOut :: TSProperty
ts_roundTripTxOut = eachOfTS 50 genTxOut roundTripsBiBuildable

--------------------------------------------------------------------------------
-- TxPayload
--------------------------------------------------------------------------------

goldenTxPayload1 :: Property
goldenTxPayload1 = goldenTestBi exampleTxPayload1 "test/golden/bi/txp/TxPayload1"

ts_roundTripTxPayload :: TSProperty
ts_roundTripTxPayload = eachOfTS 50 (feedPM genTxPayload) roundTripsBiShow

--------------------------------------------------------------------------------
-- TxProof
--------------------------------------------------------------------------------

goldenTxProof :: Property
goldenTxProof = goldenTestBi exampleTxProof "test/golden/bi/txp/TxProof"

ts_roundTripTxProof :: TSProperty
ts_roundTripTxProof = eachOfTS 50 (feedPM genTxProof) roundTripsBiBuildable

--------------------------------------------------------------------------------
-- TxSig
--------------------------------------------------------------------------------

goldenTxSig :: Property
goldenTxSig = goldenTestBi txSigGold "test/golden/bi/txp/TxSig"
 where
  txSigGold = sign
    (ProtocolMagicId 0)
    SignForTestingOnly
    exampleSecretKey
    exampleTxSigData

ts_roundTripTxSig :: TSProperty
ts_roundTripTxSig = eachOfTS 50 (feedPM genTxSig) roundTripsBiBuildable

--------------------------------------------------------------------------------
-- TxSigData
--------------------------------------------------------------------------------

goldenTxSigData :: Property
goldenTxSigData = goldenTestBi exampleTxSigData "test/golden/bi/txp/TxSigData"

ts_roundTripTxSigData :: TSProperty
ts_roundTripTxSigData = eachOfTS 50 genTxSigData roundTripsBiShow

--------------------------------------------------------------------------------
-- TxWitness
--------------------------------------------------------------------------------

goldenTxWitness :: Property
goldenTxWitness = goldenTestBi exampleTxWitness "test/golden/bi/txp/TxWitness"

ts_roundTripTxWitness :: TSProperty
ts_roundTripTxWitness = eachOfTS 20 (feedPM genTxWitness) roundTripsBiShow

sizeEstimates :: H.Group
sizeEstimates
  = let
      sizeTestGen :: (Show a, Bi a) => Gen a -> Property
      sizeTestGen g = sizeTest $ scfg { gen = g }
      pm           = ProtocolMagicId 0

      -- Explicit bounds for types, based on the generators from Gen.
      attrUnitSize = (typeRep (Proxy @(Attributes ())), SizeConstant 1)
      attrAddrSize =
        ( typeRep (Proxy @(Attributes AddrAttributes))
        , SizeConstant (szCases [Case "min" 1, Case "max" 1024])
        )
      txSigSize = (typeRep (Proxy @(Signature TxSigData)), SizeConstant 66)
    in H.Group
      "Encoded size bounds for core types."
      [ ("TxId", sizeTestGen genTxId)
      , ( "Tx"
        , sizeTest $ scfg
          { gen         = genTx
          , addlCtx     = M.fromList [attrUnitSize, attrAddrSize]
          , computedCtx = \tx -> M.fromList
            [ ( typeRep (Proxy @(LengthOf [TxIn]))
              , SizeConstant (fromIntegral $ length $ txInputs tx)
              )
            , ( typeRep (Proxy @(LengthOf [TxOut]))
              , SizeConstant (fromIntegral $ length $ txOutputs tx)
              )
            ]
          }
        )
      , ("TxIn", sizeTestGen genTxIn)
      , ( "TxOut"
        , sizeTest
          $ scfg { gen = genTxOut, addlCtx = M.fromList [attrAddrSize] }
        )
      , ( "TxAux"
        , sizeTest $ scfg
          { gen         = genTxAux pm
          , addlCtx     = M.fromList [attrUnitSize, attrAddrSize, txSigSize]
          , computedCtx = \ta -> M.fromList
            [ ( typeRep (Proxy @(LengthOf [TxIn]))
              , SizeConstant (fromIntegral $ length $ txInputs $ taTx ta)
              )
            , ( typeRep (Proxy @(LengthOf (Vector TxInWitness)))
              , SizeConstant (fromIntegral $ length $ taWitness ta)
              )
            , ( typeRep (Proxy @(LengthOf [TxOut]))
              , SizeConstant (fromIntegral $ length $ txOutputs $ taTx ta)
              )
            ]
          }
        )
      , ( "TxInWitness"
        , sizeTest
          $ scfg { gen = genTxInWitness pm, addlCtx = M.fromList [txSigSize] }
        )
      , ("TxSigData", sizeTestGen genTxSigData)
      , ( "Signature TxSigData"
        , sizeTest
          $ scfg { gen = genTxSig pm, addlCtx = M.fromList [txSigSize] }
        )
      ]

-----------------------------------------------------------------------
-- Main test export
-----------------------------------------------------------------------

tests :: TestScenario -> IO Bool
tests ts = and <$> sequence
  [ H.checkSequential $$discoverGolden
  , H.checkParallel (($$discoverRoundTripArg :: TestScenario -> H.Group) ts)
  , H.checkParallel sizeEstimates
  ]

