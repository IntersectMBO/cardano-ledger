{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE GADTs               #-}

{-# OPTIONS_GHC  -fno-warn-orphans #-}
{-# OPTIONS_GHC -Wno-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

module Test.Shelley.Spec.Ledger.Value where

import Data.ByteString (ByteString)
import Data.Map(Map,empty,fromList,assocs)
import Cardano.Ledger.Era (Era)
import Cardano.Ledger.Val( Val (..), LabeledInt(..) )
import Cardano.Ledger.Value
import Cardano.Ledger.Crypto(Crypto(..))
import Shelley.Spec.Ledger.Coin (Coin (..))
import Shelley.Spec.Ledger.Scripts(ScriptHash(..))
import Test.Shelley.Spec.Ledger.Serialisation.Generators() -- get: instance Era era => Arbitrary (ScriptHash era)
import Test.Shelley.Spec.Ledger.ConcreteCryptoTypes(C)

import Test.QuickCheck
import Test.QuickCheck.Instances.ByteString
import System.IO.Unsafe(unsafePerformIO)
-- ====================================================

genCoin :: Gen Coin
genCoin = fmap Coin (choose (-2,10))

genB :: Gen ByteString
genB = resize 4 arbitrary

genID = fmap AssetID genB

assetChoices:: [ AssetID ]
assetChoices = unsafePerformIO(generate(vectorOf 25 genID))

policyChoices :: [ ScriptHash C ]
policyChoices = unsafePerformIO(generate(vectorOf 15 arbitrary))

genAssetID :: Gen AssetID
genAssetID =  oneof (map return assetChoices)

genPolicyID :: Gen (PolicyID C)
genPolicyID = oneof (map (return . PolicyID) policyChoices)

genTriple :: Gen (PolicyID C, AssetID, Integer)
genTriple = (,,) <$> genPolicyID  <*> genAssetID <*> choose (-2,4)

genMap :: Gen [(PolicyID C, AssetID, Integer)]  -- Most maps have 1 or 2 Assets
genMap = frequency [(1,vectorOf 0 genTriple),
                    (4,vectorOf 1 genTriple),
                    (5,vectorOf 2 genTriple),
                    (1,vectorOf 3 genTriple)]

valueFromList :: [(PolicyID C, AssetID, Integer)] -> Integer -> Value C
valueFromList list c = foldr acc (Value c empty) list
  where acc (policy,asset,count) m = insert (+) policy asset count m

genValue :: Gen (Value C)
genValue = valueFromList <$> genMap <*> choose(-2,10)

foo = do s <- fmap showValue $ generate genValue
         putStrLn s

gettriples :: Value era -> (Integer,[(PolicyID era,AssetID, Integer)])
gettriples (Value c m1) = (c,foldr accum1 [] (assocs m1))
    where accum1 (policy,m2) ans = foldr accum2 ans (assocs m2) where accum2 (asset,cnt) ans2 = (policy,asset,cnt):ans2

showValue:: Value C -> String
showValue v = show c ++ "\n" ++ unlines (map trans ts)
  where (c,ts) = gettriples v
        trans (PolicyID x,hash,cnt) = show x++",  "++show hash++",  "++show cnt


bar = quickCheck $ zeroScale CoinR

qqq = defMinus @Coin

-- =====================================================

data Rep t where
  CoinR :: Rep Coin
  ValueR :: Rep (Value C)

defMinus :: forall v. Val v => ( v -> v -> Bool)
defMinus x y = x <-> y == x <+> (invert y)

defInvert x = invert x == (-1) <×> x

commute x y = x <+> y == y <+> x

assoc x y z = x <+> (y <+> z) == (y <+> x) <+> z

addIdent x = (zero <+> x == x <+> zero) && (zero <+> x == x)

cancel x = x <-> x == zero

distr1 r x y = r <×> (x <+> y) == (r <×> x) <+> (r <×> y)

distr2 r s x = (r + s) <×> x == (r <×> x) <+> (s <×> x)

distr3 r s x = (r * s) <×> x == r <×> (s <×> x)

multIdent x = 1 <×> x == x

minusCancel x = (x <-> x) == zero

plusMinusAssoc x y = ((x <+> y) <-> y == x <+> (y <-> y)) && (x <+> (y <-> y) == x)

plusInvertCancel x = (x <+> (invert x) == (x <-> x)) && (x <-> x == zero)

minusZero x = (x <-> zero) == x

zeroMinus x = (zero <-> x) == invert x

invertScale x = invert x == (-1)  <×> x

scaleZero v = 0 <×> v == zero

zeroScale :: forall v. Val v => Rep v -> Int -> Bool
zeroScale _ n = n <×> (zero @v) == (zero @v)

scaleInject :: forall v. Val v => Rep v -> Int -> Coin -> Bool
scaleInject _ n c = n <×> (inject @v c) == inject @v (n <×> c)

scaleOne x = 1 <×> x == x

scalePlus n x y = n <×> (x <+> y) == (n <×> x) <+> (n <×> y)

scaleScale n m v = n <×> (m <×> v) == (n * m) <×> v

scaleCoin n v = n <×> (coin v) == coin (n <×> v)

unfoldScale x = 3 <×> x == x <+> x <+> x

coinZero :: forall v. Val v => Rep v -> Bool
coinZero _ = coin (zero @v) == zero

coinPlus x y = coin (x <+> y) == coin x <+> coin y

coinScale n v = coin (n <×> v) == n <×> (coin v)

coinInject :: forall v. Val v => Rep v -> Coin -> Bool
coinInject _ x = coin @v (inject @v x) == x

coinModify :: forall era. (Crypto era, Era era) => (Coin -> Coin) -> Value era -> Bool
coinModify f v = coin (modifyCoin f v) == modifyCoin f (coin @(Value era) v)

coinInsert comb c t n v = coin (insert comb c t n v) == coin v
