{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE UndecidableInstances #-}

module Constrained.Examples.Basic where

import GHC.Generics

import Test.QuickCheck qualified as QC

import Constrained

leqPair :: Specification BaseFn (Int, Int)
leqPair = constrained $ \p ->
  match p $ \x y ->
    x <=. y

simplePairSpec :: Specification BaseFn (Int, Int)
simplePairSpec = constrained $ \p ->
  match p $ \x y ->
    [ assert $ x /=. 0
    , assert $ y /=. 0
    , -- You can use `monitor` to add QuickCheck property modifiers for
      -- monitoring distribution, like classify, label, and cover, to your
      -- specification
      monitor $ \eval ->
        QC.classify (eval y > 0) "positive y"
          . QC.classify (eval x > 0) "positive x"
    ]

sizeAddOrSub1 :: Specification BaseFn Integer
sizeAddOrSub1 = constrained $ \s ->
  4 ==. s + 2

sizeAddOrSub2 :: Specification BaseFn Integer
sizeAddOrSub2 = constrained $ \s ->
  4 ==. 2 + s

sizeAddOrSub3 :: Specification BaseFn Integer
sizeAddOrSub3 = constrained $ \s ->
  4 ==. s - 2

-- | We expect a negative Integer, so ltSpec tests for that.
sizeAddOrSub4 :: Specification BaseFn Integer
sizeAddOrSub4 = ltSpec 0 <> (constrained $ \s -> 4 ==. 2 - s)

sizeAddOrSub5 :: Specification BaseFn Integer
sizeAddOrSub5 = constrained $ \s ->
  2 ==. 12 - s

listSubSize :: Specification BaseFn [Int]
listSubSize = constrained $ \s ->
  2 ==. 12 - (sizeOf_ s)

orPair :: Specification BaseFn (Int, Int)
orPair = constrained' $ \x y ->
  x <=. 5 ||. y <=. 5

trickyCompositional :: Specification BaseFn (Int, Int)
trickyCompositional = constrained $ \p ->
  satisfies p simplePairSpec <> assert (fst_ p ==. 1000)

data Foo = Foo Int | Bar Int Int
  deriving (Show, Eq, Ord, Generic)

instance HasSimpleRep Foo
instance BaseUniverse fn => HasSpec fn Foo

fooSpec :: Specification BaseFn Foo
fooSpec = constrained $ \foo ->
  (caseOn foo)
    ( branch $ \i ->
        [ assert $ 0 <=. i
        , monitor $ \_ -> QC.cover 40 True "Foo"
        ]
    )
    ( branch $ \i j ->
        [ assert $ i <=. j
        , monitor $ \_ -> QC.cover 40 True "Bar"
        ]
    )

intSpec :: Specification BaseFn (Int, Int)
intSpec = constrained' $ \a b ->
  reify a (`mod` 10) $ \a' -> b ==. a'

mapElemKeySpec :: Specification BaseFn Int
mapElemKeySpec = constrained $ \n ->
  letBind (pair_ n $ lit (False, 4)) $ \(p :: Term BaseFn (Int, (Bool, Int))) ->
    letBind (snd_ (snd_ p)) $ \x ->
      [x <. 10, 0 <. x, not_ $ elem_ n $ lit []]

intRangeSpec :: Int -> Specification BaseFn Int
intRangeSpec a = constrained $ \n -> n <. lit a

testRewriteSpec :: Specification BaseFn ((Int, Int), (Int, Int))
testRewriteSpec = constrained' $ \x y ->
  x ==. fromGeneric_ (toGeneric_ y)

pairSingletonSpec :: Specification BaseFn (Int, Int)
pairSingletonSpec = constrained $ \q ->
  forAll (singleton_ q) $ \p ->
    letBind (fst_ p) $ \x ->
      letBind (snd_ p) $ \y ->
        x <=. y

parallelLet :: Specification BaseFn (Int, Int)
parallelLet = constrained $ \p ->
  [ letBind (fst_ p) $ \x -> 0 <. x
  , letBind (snd_ p) $ \x -> x <. 0
  ]

letExists :: Specification BaseFn (Int, Int)
letExists = constrained $ \p ->
  [ letBind (fst_ p) $ \x -> 0 <. x
  , exists (\eval -> pure $ snd (eval p)) $
      \x ->
        [ x <. 0
        , snd_ p ==. x
        ]
  ]

letExistsLet :: Specification BaseFn (Int, Int)
letExistsLet = constrained $ \p ->
  [ letBind (fst_ p) $ \x -> 0 <. x
  , exists (\eval -> pure $ snd (eval p)) $
      \x ->
        [ assert $ x <. 0
        , letBind (snd_ p) $ \y ->
            [ x ==. y
            , y <. -1
            ]
        ]
  ]

dependencyWeirdness :: Specification BaseFn (Int, Int, Int)
dependencyWeirdness = constrained' $ \x y z ->
  reify (x + y) id $ \zv -> z ==. zv

parallelLetPair :: Specification BaseFn (Int, Int)
parallelLetPair = constrained $ \p ->
  [ match p $ \x y ->
      [ assert $ x <=. y
      , y `dependsOn` x
      ]
  , match p $ \x y -> y <=. x
  ]

existsUnfree :: Specification BaseFn Int
existsUnfree = constrained $ \_ -> exists (\_ -> pure 1) $ \y -> y `elem_` lit [1, 2 :: Int]

reifyYucky :: Specification BaseFn (Int, Int, Int)
reifyYucky = constrained' $ \x y z ->
  [ reify x id $ \w ->
      [ y ==. w
      , z ==. w
      ]
  , z `dependsOn` y
  ]

basicSpec :: Specification BaseFn Int
basicSpec = constrained $ \x ->
  exists (\eval -> pure $ eval x) $ \y ->
    satisfies x $ constrained $ \x' ->
      x' <=. 1 + y

canFollowLike :: Specification BaseFn ((Int, Int), (Int, Int))
canFollowLike = constrained' $ \p q ->
  match p $ \ma mi ->
    match q $ \ma' mi' ->
      [ ifElse
          (ma' ==. ma)
          (mi' ==. mi + 1)
          (mi' ==. 0)
      , assert $ ma' <=. ma + 1
      , assert $ ma <=. ma'
      , ma' `dependsOn` ma
      ]

ifElseBackwards :: Specification BaseFn (Int, Int)
ifElseBackwards = constrained' $ \p q ->
  [ ifElse
      (p ==. 1)
      (q <=. 0)
      (0 <. q)
  , p `dependsOn` q
  ]

assertReal :: Specification BaseFn Int
assertReal = constrained $ \x ->
  [ assert $ x <=. 10
  , assertReified x (<= 10)
  ]

assertRealMultiple :: Specification BaseFn (Int, Int)
assertRealMultiple = constrained' $ \x y ->
  [ assert $ x <=. 10
  , assert $ 11 <=. y
  , assertReified (pair_ x y) $ uncurry (/=)
  ]

reifiesMultiple :: Specification BaseFn (Int, Int, Int)
reifiesMultiple = constrained' $ \x y z ->
  [ reifies (x + y) z id
  , x `dependsOn` y
  ]

data Three = One | Two | Three deriving (Ord, Eq, Show, Generic)
instance HasSimpleRep Three
instance BaseUniverse fn => HasSpec fn Three

trueSpecUniform :: Specification BaseFn Three
trueSpecUniform = constrained $ \o -> monitor $ \eval -> QC.cover 30 True (show $ eval o)

three :: Specification BaseFn Three
three = constrained $ \o ->
  [ caseOn
      o
      ( branchW 1 $ \_ -> True
      )
      ( branchW 1 $ \_ -> True
      )
      ( branchW 1 $ \_ -> True
      )
  , monitor $ \eval -> QC.cover 30 True (show $ eval o)
  ]

three' :: Specification BaseFn Three
three' = three <> three

threeSpecific :: Specification BaseFn Three
threeSpecific = constrained $ \o ->
  [ caseOn
      o
      ( branchW 1 $ \_ -> True
      )
      ( branchW 1 $ \_ -> True
      )
      ( branchW 2 $ \_ -> True
      )
  , monitor $ \eval ->
      QC.coverTable "TheValue" [("One", 22), ("Two", 22), ("Three", 47)]
        . QC.tabulate "TheValue" [show $ eval o]
  ]

threeSpecific' :: Specification BaseFn Three
threeSpecific' = threeSpecific <> threeSpecific

posNegDistr :: Specification BaseFn Int
posNegDistr =
  constrained $ \x ->
    [ monitor $ \eval -> QC.cover 60 (0 < eval x) "x positive"
    , x `satisfies` chooseSpec (1, constrained (<. 0)) (2, constrained (0 <.))
    ]

ifElseMany :: Specification BaseFn (Bool, Int, Int)
ifElseMany = constrained' $ \b x y ->
  ifElse
    b
    [ x <. 0
    , y <. 10
    ]
    [ 0 <. x
    , 10 <. y
    ]

propBack :: Specification BaseFn (Int, Int)
propBack = constrained' $ \x y ->
  [ x ==. y + 10
  , x <. 20
  , 8 <. y
  ]

propBack' :: Specification BaseFn (Int, Int)
propBack' = constrained' $ \x y ->
  [ y ==. x - 10
  , 20 >. x
  , 8 >. y
  , y >. x - 20
  ]
