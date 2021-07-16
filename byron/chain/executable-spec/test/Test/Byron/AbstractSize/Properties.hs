{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Test.Byron.AbstractSize.Properties (testAbstractSize) where

import Byron.Spec.Chain.STS.Block (Block (..), BlockBody (..), BlockHeader (..))
import Byron.Spec.Chain.STS.Rule.Chain (CHAIN)
import Byron.Spec.Ledger.Core hiding ((<|))
import Byron.Spec.Ledger.Delegation (DCert)
import Byron.Spec.Ledger.UTxO
import Byron.Spec.Ledger.Update (ProtVer (..), UProp (..), Vote)
import Control.State.Transition.Generator (trace)
import Control.State.Transition.Trace (TraceOrder (OldestFirst), traceSignals)
import Data.AbstractSize
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe (maybeToList)
import Data.Sequence ((<|), (><))
import qualified Data.Sequence as Seq
import qualified Data.Set as Set
import Data.Typeable (TypeRep, Typeable, typeOf)
import Data.Word (Word64)
import Hedgehog (MonadTest, Property, diff, forAll, property, withTests, (===))
import Numeric.Natural (Natural)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (Assertion, testCase, (@?=))
import Test.Tasty.Hedgehog

--------------------------------------------------------------------------------
-- Example typeReps for Block/Header/Body
--------------------------------------------------------------------------------

aTx :: TxBody
aTx = undefined

aTxId :: TxId
aTxId = TxId (hash aTx)

aHeader :: BlockHeader
aHeader =
  BlockHeader
    { _bhPrevHash = undefined :: Hash,
      _bhSlot = undefined :: Slot,
      _bhIssuer = undefined :: VKey,
      _bhSig = undefined :: Sig Hash,
      _bhUtxoHash = undefined :: Hash,
      _bhDlgHash = undefined :: Hash,
      _bhUpdHash = undefined :: Hash
    }

aTxWits :: Tx
aTxWits =
  let (in0, in1) = (TxIn aTxId 0, TxIn aTxId 1)
      outs = []
      wits = []
   in Tx (TxBody [in0, in1] outs) wits

aBody :: BlockBody
aBody =
  BlockBody
    { _bDCerts = [],
      _bUtxo = [aTxWits, aTxWits],
      _bUpdProp = Nothing,
      _bUpdVotes = [],
      _bProtVer =
        ProtVer
          { _pvMaj = 0,
            _pvMin = 1,
            _pvAlt = 1
          }
    }

aBlock :: Block
aBlock =
  Block
    { _bHeader = aHeader,
      _bBody = aBody
    }

-- | A BlockHeader term has fixed typeReps
exampleTypeRepsBlockHeader :: Assertion
exampleTypeRepsBlockHeader =
  typeReps aHeader
    @?= Seq.fromList
      [ typeOf (undefined :: BlockHeader),
        typeOf (undefined :: Hash),
        typeOf (undefined :: Hash),
        typeOf (undefined :: Hash),
        typeOf (undefined :: Hash),
        typeOf (undefined :: Slot),
        typeOf (undefined :: Word64),
        typeOf (undefined :: VKey),
        typeOf (undefined :: Owner),
        typeOf (undefined :: Natural),
        typeOf (undefined :: Sig Hash)
      ]

-- | A BlockBody has variable typeReps, depending on the collections
-- [DCert], [TxWits], [Vote] and [STag] (in UProp)
--
--   In this example, we can see the repetition of typeReps for 2 TxWits
exampleTypeRepsBlockBody :: Assertion
exampleTypeRepsBlockBody =
  typeReps aBody
    @?= Seq.fromList
      [ typeOf (undefined :: BlockBody),
        typeOf (undefined :: [DCert]),
        typeOf (undefined :: [Tx])
      ]
    >< typeReps aTxWits
    >< typeReps aTxWits
    >< Seq.fromList
      [ typeOf (undefined :: Maybe UProp),
        typeOf (undefined :: [Vote]),
        typeOf (undefined :: ProtVer),
        typeOf (undefined :: Natural),
        typeOf (undefined :: Natural),
        typeOf (undefined :: Natural)
      ]

-- | The typeReps for a 'Block' is a combination of typeReps for
-- the header and body in the block.
exampleTypeRepsBlock :: Assertion
exampleTypeRepsBlock =
  typeReps aBlock @?= typeOf (undefined :: Block)
    <| typeReps aHeader
    >< typeReps aBody

--------------------------------------------------------------------------------
-- Properties of abstractSize for Block/Header/Body
--------------------------------------------------------------------------------

-- | Make a singleton cost of "1" for the given term's type
mkCost :: forall a. Typeable a => Map TypeRep Size
mkCost = Map.singleton (typeOf (undefined :: a)) 1

-- | This property tests that for abstractSize
--   - we should account for each DCert/TxWits/Vote/UProp/STag in a Block Body
--   - the BlockHeader and BlockBody should each be counted only once
propMultipleOfSizesBlock ::
  MonadTest m => Block -> m ()
propMultipleOfSizesBlock b =
  let body_ = _bBody b
   in do
        abstractSize (mkCost @DCert) b === length (_bDCerts body_)
        abstractSize (mkCost @Tx) b === length (_bUtxo body_)
        abstractSize (mkCost @Vote) b === length (_bUpdVotes body_)
        abstractSize (mkCost @UProp) b === length (maybeToList (_bUpdProp body_))
        -- A STag is a string, so we need to make sure that all the characters are
        -- accounted for in the size computation. We cannot use equality, since
        -- characters might appear in other parts of the block.
        diff
          (maybe 0 (sum . fmap length . Set.toList . _upSTags) (_bUpdProp body_))
          (<=)
          (abstractSize (mkCost @Char) b)

        -- BlockHeader appears only once
        abstractSize (mkCost @BlockHeader) b === 1
        -- BlockBody appears only once
        abstractSize (mkCost @BlockBody) b === 1

propBlockAbstractSize :: Property
propBlockAbstractSize =
  withTests 50 $
    property $ do
      tr <- forAll (trace @CHAIN () 100)
      let blocks = traceSignals OldestFirst tr :: [Block]
      mapM_ propMultipleOfSizesBlock blocks

testAbstractSize :: TestTree
testAbstractSize =
  testGroup
    "Test abstractSize"
    [ testCase "AbstractSize - example - BlockHeader" exampleTypeRepsBlockHeader,
      testCase "AbstractSize - example - BlockBody" exampleTypeRepsBlockBody,
      testCase "AbstractSize - example - Block" exampleTypeRepsBlock,
      testProperty "AbstractSize - Block/Header/Body" propBlockAbstractSize
    ]
