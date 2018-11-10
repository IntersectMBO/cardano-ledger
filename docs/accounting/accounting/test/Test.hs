import Hedgehog
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import Test.Tasty
import Test.Tasty.Hedgehog
import Test.Tasty.HUnit

import qualified Data.Map as Map
import Lens.Micro ((&), (.~))
import Numeric.Natural (Natural)

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

gnat :: Natural -> Natural -> Gen Natural
gnat lower upper = Gen.integral (Range.linear lower upper)

gcoin :: Coin -> Coin -> Gen Coin
gcoin (Coin cmin) (Coin cmax) = Coin <$> gnat cmin cmax

gcert :: Gen Cert
gcert = Gen.choice
  [ KeyReg <$> gnat 0 1000
  , PoolReg <$> gnat 0 1000
  , KeyDeReg <$> gnat 0 1000
  , PoolRetire <$> gnat 0 1000 <*> (Duration <$> gnat 0 1000)]

gobligations :: Gen (Cert, Slot)
gobligations = do
  c <- gcert
  s <- gnat 0 1000
  return (c, Slot s)

gretiring :: Slot -> Gen (Natural, Slot)
gretiring cslot = do
  certId <- gnat 0 1000
  d <- gnat 0 1000
  return (certId, cslot +* Duration d)

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
  sl <- gnat 0 1000
  let
    creations = Map.elems obl
    minSlot = if Prelude.null creations then Slot 0 else maximum creations
    cslot = Slot sl + minSlot

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

  -- set the retiring pools
  ret <- Gen.map (Range.linear 0 1000) (gretiring cslot)

  return (LedgerState circ deps treas reserv reward rpool fee obl ret pc cslot cslot)

gaction :: Gen Accounting.Action
gaction = Gen.choice
  [ ActTxBody <$> gcoin (Coin 0) (Coin 1000)
  , ActCert <$> gcert
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
    _circulation = Coin 29999999999998800
  , _deposits = Coin 1200
  , _treasury = Coin 0
  , _reserves = Coin 14999999999999900
  , _rewards = Coin 100
  , _rewardPool = Coin 0
  , _fees = Coin 0
  , _obligations = Map.fromList
      [ (KeyReg 0, Slot 3)
      , (PoolReg 1, Slot 5)
      , (PoolReg 2, Slot 6)]
  , _retiring = Map.singleton 1 (Slot 100)
  , _pconsts = examplePc
  , _slot = Slot 100
  , _lastEpoch = Slot 50
  }

epochLS :: LedgerState
epochLS = LedgerState {
    _circulation = Coin 29999999999998800
  , _deposits = Coin 694
  , _treasury = Coin 300000018432
  , _reserves = Coin 14998499999973276
  , _rewards = Coin 900000055892
  , _rewardPool = Coin 299999952906
  , _fees = Coin 0
  , _obligations = Map.fromList
      [ (KeyReg 0, Slot 3)
      , (PoolReg 2, Slot 6)]
  , _retiring = Map.empty
  , _pconsts = examplePc
  , _slot = Slot 100
  , _lastEpoch = Slot 100
  }

testObligation :: Assertion
testObligation = obligation exampleLS @?= Coin 1190

testPreservation :: LedgerState -> Assertion
testPreservation ls = total ls @?= fortyFiveBillionAda

testTxPres :: Assertion
testTxPres = testPreservation $ applyAction exampleLS (ActTxBody $ Coin 2)

testTxCalc :: Assertion
testTxCalc = applyAction exampleLS (ActTxBody $ Coin 2) @?=
  (exampleLS & circulation .~ Coin 29999999999998798
             & fees .~ Coin 2)

testAddKeyCertPres :: Assertion
testAddKeyCertPres = testPreservation $ applyAction exampleLS (ActCert $ KeyReg 1)

testAddKeyCertCalc :: Assertion
testAddKeyCertCalc = applyAction exampleLS (ActCert $ KeyReg 10) @?=
  (exampleLS & circulation .~ Coin 29999999999998600
             & deposits .~ Coin 1400
             & obligations .~ Map.fromList [ (KeyReg 0, Slot 3)
                                           , (PoolReg 1, Slot 5)
                                           , (PoolReg 2, Slot 6)
                                           , (KeyReg 10, Slot 100)])

testAddPoolCertPres :: Assertion
testAddPoolCertPres = testPreservation $ applyAction exampleLS (ActCert $ PoolReg 1)

testAddPoolCertCalc :: Assertion
testAddPoolCertCalc = applyAction exampleLS (ActCert $ PoolReg 11) @?=
  (exampleLS & circulation .~ Coin 29999999999998300
             & deposits .~ Coin 1700
             & obligations .~ Map.fromList [ (KeyReg 0, Slot 3)
                                           , (PoolReg 1, Slot 5)
                                           , (PoolReg 2, Slot 6)
                                           , (PoolReg 11, Slot 100)])

testDelKeyCertPres :: Assertion
testDelKeyCertPres = testPreservation $ applyAction exampleLS (ActCert $ KeyDeReg 0)

testDelKeyCertCalc :: Assertion
testDelKeyCertCalc = applyAction exampleLS (ActCert $ KeyDeReg 0) @?=
  (exampleLS & circulation .~ Coin 29999999999998998
             & deposits .~ Coin 1001
             & fees .~ Coin 1
             & obligations .~ Map.fromList [ (PoolReg 1, Slot 5)
                                           , (PoolReg 2, Slot 6)])

testRetirePoolPres :: Assertion
testRetirePoolPres = testPreservation $ applyAction exampleLS (ActCert $ PoolRetire 0 (Duration 5))

testRetirePoolCalc :: Assertion
testRetirePoolCalc = applyAction exampleLS (ActCert $ PoolRetire 1 (Duration 5)) @?=
  (exampleLS & retiring .~ Map.singleton 1 (Slot 105))

testWithdrawalPres :: Assertion
testWithdrawalPres = testPreservation $ applyAction exampleLS (ActWithdrawal $ Coin 5)

testWithdrawalCalc :: Assertion
testWithdrawalCalc = applyAction exampleLS (ActWithdrawal $ Coin 5) @?=
  (exampleLS & circulation .~ Coin 29999999999998805
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
  (epochLS & deposits .~ Coin 690
           & reserves .~ Coin 14998499999973280
           & pconsts .~ pc)
  where pc = examplePc {_decayRate = 0.0002}

testSlowerDecayPres :: Assertion
testSlowerDecayPres = testPreservation $ applyAction exampleLS (ActEpochWithVote pc 0.75)
  where pc = examplePc {_decayRate = 0.00005}

testSlowerDecayCalc :: Assertion
testSlowerDecayCalc = applyAction exampleLS (ActEpochWithVote pc 0.75) @?=
  (epochLS & deposits .~ Coin 697
           & reserves .~ Coin 14998499999973273
           & pconsts .~ pc)
  where pc = examplePc {_decayRate = 0.00005}

testDelCertAfterVote :: Assertion
testDelCertAfterVote = total (foldl applyAction ls actions) @?= fortyFiveBillionAda
  where
    actions = [ ActCert (KeyReg 147)
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
              , ActCert (KeyDeReg 147)
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
           , _retiring = Map.empty
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

  , testCase "add key cert preservation" testAddKeyCertPres
  , testCase "add key cert" testAddKeyCertCalc

  , testCase "add pool cert preservation" testAddPoolCertPres
  , testCase "add pool cert" testAddPoolCertCalc

  , testCase "delete key cert preservation" testDelKeyCertPres
  , testCase "delete key cert" testDelKeyCertCalc

  , testCase "retire pool cert preservation" testRetirePoolPres
  , testCase "retire pool cert" testRetirePoolCalc

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
