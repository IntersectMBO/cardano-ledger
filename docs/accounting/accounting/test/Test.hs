import Hedgehog
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import Test.Tasty
import Test.Tasty.Hedgehog
import Test.Tasty.HUnit

import qualified Data.Map as Map
import Lens.Micro ((&), (.~))

import Accounting

-- There are one million lovelace per ada
fortyFiveBillionAda :: Coin
fortyFiveBillionAda = Coin (45 * oneBillion * oneMillion)
  where
    oneThousand = 1000
    oneMillion = oneThousand * oneThousand
    oneBillion = oneThousand * oneThousand * oneThousand

conservationOfLovelace :: Property
conservationOfLovelace =
  property $ do
    actions <- forAll $ Gen.list (Range.linear 100 10000) gaction
    ls <- forAll gledgerstate
    total (foldl applyAction ls actions) === fortyFiveBillionAda

gcoin :: Coin -> Coin -> Gen Coin
gcoin (Coin cmin) (Coin cmax) = Coin <$> Gen.integral (Range.linear cmin cmax)

gslot :: Gen Slot
gslot = Slot <$> Gen.integral (Range.linear 0 1000)

gcert :: Gen Cert
gcert = Gen.choice
  [ KeyReg <$> Gen.integral (Range.linear 0 1000)
  , PoolReg <$> Gen.integral (Range.linear 0 1000) ]

gobligations :: Gen (Cert, Slot)
gobligations = do
  c <- gcert
  s <- gslot
  return (c, s)

gpconsts :: Gen PrtclConsts
gpconsts = PrtclConsts
  <$> gcoin (Coin 0) (Coin 1000)
  <*> gcoin (Coin 0) (Coin 1000)
  <*> Gen.realFloat (Range.exponentialFloat 0 1) -- Is exponentialFloat good here?
  <*> Gen.realFloat (Range.exponentialFloat 0 1000)
  <*> Gen.realFloat (Range.exponentialFloat 0 1)
  <*> Gen.realFloat (Range.exponentialFloat 0 1)

gledgerstate :: Gen LedgerState
gledgerstate = do
  -- generate obligations and the current slot
  obl <- Gen.map (Range.linear 0 1000) gobligations
  sl <- gslot
  let
    creations = Map.elems obl
    cslot = sl + (if Prelude.null creations then 0 else maximum creations)

  -- protocol constants
  pc <- gpconsts

  -- set the deposit pool
  let deps = sum [deposit cert pc | cert <- Map.keys obl]

  -- generate the other categories by incrementally removing some of what
  -- is left of the fort five billion
  fee <- gcoin (Coin 0) (fortyFiveBillionAda - deps)
  rpool <- gcoin (Coin 0) (fortyFiveBillionAda - (deps + fee))
  treas <- gcoin (Coin 0) (fortyFiveBillionAda - (deps + fee + rpool))
  reward <- gcoin (Coin 0) (fortyFiveBillionAda - (deps + fee + rpool + treas))
  reserv <- gcoin (Coin 0) (fortyFiveBillionAda - (deps + fee + rpool + treas + reward))

  -- set the circulation to the remainder of the fort five billion ada
  let circ = fortyFiveBillionAda - (deps + fee + rpool + treas + reward + reserv)


  return (LedgerState circ deps treas reserv reward rpool fee obl pc cslot cslot)

gaction :: Gen Accounting.Action
gaction = Gen.choice
  [ ActTxBody <$> gcoin (Coin 0) (Coin 1000)
  , ActAddCert <$> gcert
  , ActDelCert <$> gcert
  , ActWithdrawal <$> gcoin (Coin 0) (Coin 1000000)
  , ActEpochNoVote <$> Gen.realFloat (Range.exponentialFloat 0 1)
  , ActEpochWithVote <$> gpconsts <*> Gen.realFloat (Range.exponentialFloat 0 1)
  , return ActNextSlot
  ]

examplePc :: PrtclConsts
examplePc = PrtclConsts
         { _keyRegDep = Coin 200
         , _poolRegDep = Coin 500
         , _minDep = 0.25
         , _decayRate = 0.0001
         , _tau = 0.2
         , _rho = 0.0001
         }

exampleLS :: LedgerState
exampleLS = LedgerState {
    _circulation = Coin 29999999999999800
  , _deposits = Coin 200
  , _treasury = Coin 0
  , _reserves = Coin 14999999999999900
  , _rewards = Coin 100
  , _rewardPool = Coin 0
  , _fees = Coin 0
  , _obligations = Map.singleton (KeyReg 0) (Slot 3)
  , _pconsts = examplePc
  , _slot = Slot 100
  , _lastEpoch = Slot 50
  }

epochLS :: LedgerState
epochLS = LedgerState {
    _circulation = Coin 29999999999999800
  , _deposits = Coin 198
  , _treasury = Coin 300000018432
  , _reserves = Coin 14998499999973276
  , _rewards = Coin 900000055396
  , _rewardPool = Coin 299999952898
  , _fees = Coin 0
  , _obligations = Map.singleton (KeyReg 0) (Slot 3)
  , _pconsts = examplePc
  , _slot = Slot 100
  , _lastEpoch = Slot 100
  }

testObligation :: Assertion
testObligation = obligation exampleLS @?= Coin 198

testPreservation :: LedgerState -> Assertion
testPreservation ls = total ls @?= fortyFiveBillionAda

testTxPres :: Assertion
testTxPres = testPreservation $ applyAction exampleLS (ActTxBody $ Coin 2)

testTxCalc :: Assertion
testTxCalc = applyAction exampleLS (ActTxBody $ Coin 2) @?=
  (exampleLS & circulation .~ Coin 29999999999999798
             & fees .~ Coin 2)

testAddCertPres :: Assertion
testAddCertPres = testPreservation $ applyAction exampleLS (ActAddCert $ KeyReg 1)

testAddCertCalc :: Assertion
testAddCertCalc = applyAction exampleLS (ActAddCert $ KeyReg 1) @?=
  (exampleLS & circulation .~ Coin 29999999999999600
             & deposits .~ Coin 400
             & obligations .~ Map.fromList [(KeyReg 0, Slot 3), (KeyReg 1, Slot 100)])

testDelCertPres :: Assertion
testDelCertPres = testPreservation $ applyAction exampleLS (ActDelCert $ KeyReg 0)

testDelCertCalc :: Assertion
testDelCertCalc = applyAction exampleLS (ActDelCert $ KeyReg 0) @?=
  (exampleLS & circulation .~ Coin 29999999999999998
             & deposits .~ Coin 1
             & fees .~ Coin 1
             & obligations .~ Map.empty)

testWithdrawalPres :: Assertion
testWithdrawalPres = testPreservation $ applyAction exampleLS (ActWithdrawal $ Coin 5)

testWithdrawalCalc :: Assertion
testWithdrawalCalc = applyAction exampleLS (ActWithdrawal $ Coin 5) @?=
  (exampleLS & circulation .~ Coin 29999999999999805
             & rewards .~ Coin 95)

testEpochPres :: Assertion
testEpochPres = testPreservation $ applyAction exampleLS (ActEpochNoVote 0.75)

testEpochCalc :: Assertion
testEpochCalc = applyAction exampleLS (ActEpochNoVote 0.75) @?= epochLS

testFasterDecayPres :: Assertion
testFasterDecayPres = testPreservation $ applyAction exampleLS (ActEpochWithVote pc 0.75)
  where pc = examplePc & decayRate .~ 0.0002

testFasterDecayCalc :: Assertion
testFasterDecayCalc = applyAction exampleLS (ActEpochWithVote pc 0.75) @?=
  (epochLS & deposits .~ Coin 197
           & reserves .~ Coin 14998499999973277
           & pconsts .~ pc)
  where pc = examplePc {_decayRate = 0.0002}

testSlowerDecayPres :: Assertion
testSlowerDecayPres = testPreservation $ applyAction exampleLS (ActEpochWithVote pc 0.75)
  where pc = examplePc {_decayRate = 0.00005}

testSlowerDecayCalc :: Assertion
testSlowerDecayCalc = applyAction exampleLS (ActEpochWithVote pc 0.75) @?=
  (epochLS & deposits .~ Coin 199
           & reserves .~ Coin 14998499999973275
           & pconsts .~ pc)
  where pc = examplePc {_decayRate = 0.00005}

testDelCertAfterVote :: Assertion
testDelCertAfterVote = total (foldl applyAction ls actions) @?= fortyFiveBillionAda
  where
    actions = [ ActAddCert (KeyReg 147)
              , ActNextSlot
              , ActEpochWithVote
                  PrtclConsts
                    { _keyRegDep = Coin 1
                    , _poolRegDep = Coin 0
                    , _minDep = 0.0
                    , _decayRate = 2.9802326e-8
                    , _tau = 0.0
                    , _rho = 0.0
                    }
                  0.0
              , ActDelCert (KeyReg 147)
              , ActTxBody (Coin 0)
              ]
    ls = LedgerState
           { _circulation = Coin 45000000000000000
           , _deposits = Coin 0
           , _treasury = Coin 0
           , _reserves = Coin 0
           , _rewards = Coin 0
           , _rewardPool = Coin 0
           , _fees = Coin 0
           , _obligations = Map.empty
           , _pconsts =
               PrtclConsts
                 { _keyRegDep = Coin 0
                 , _poolRegDep = Coin 0
                 , _minDep = 0.0
                 , _decayRate = 0.0
                 , _tau = 0.0
                 , _rho = 0.0
                 }
           , _slot = Slot 0
           , _lastEpoch = Slot 0
           }

unitTests :: TestTree
unitTests = testGroup "Unit Tests"
  [ testCase "obligation" testObligation

  , testCase "transaction preservation" testTxPres
  , testCase "transaction calculation" testTxCalc

  , testCase "add cert preservation" testAddCertPres
  , testCase "add cert" testAddCertCalc

  , testCase "delete cert preservation" testDelCertPres
  , testCase "delete cert" testDelCertCalc

  , testCase "reward withdrawal preservation" testWithdrawalPres
  , testCase "reward withdrawal" testWithdrawalCalc

  , testCase "epoch preservation" testEpochPres
  , testCase "epoch change calculation" testEpochCalc

  , testCase "faster decay preservation" testFasterDecayPres
  , testCase "faster decay calculation" testFasterDecayCalc

  , testCase "slower decay preservation" testSlowerDecayPres
  , testCase "slower decay calculation" testSlowerDecayCalc

  , testCase "delete a certificate afte a vote" testDelCertAfterVote
  ]

propTests :: TestTree
propTests = testGroup "Property Tests"
  [ testProperty "conservation of lovelace" conservationOfLovelace ]

tests :: TestTree
tests = testGroup "Accounting" [unitTests, propTests]


main :: IO ()
main = defaultMain tests
