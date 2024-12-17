{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

module Test.Cardano.Ledger.Shelley.SafeHash (safeHashTest) where

import Cardano.Ledger.Hashes
import Data.ByteString (ByteString)
import Data.ByteString.Short (ShortByteString, toShort)
import Data.Proxy
import Data.String (fromString)
import Test.Tasty
import Test.Tasty.HUnit

data FooI -- HashAnnotated indexes which are analogs of our EraIndependentXXX

long :: ByteString
long = fromString "abc"

short :: ShortByteString
short = toShort long

-- Any newtype over some type that is SafeToHash can easily
-- derive SafeToHash and also assign its HashAnnotated type, and
-- thus become a client of 'hashAnnotated'

newtype Foo = Foo ShortByteString
  deriving (Show, SafeToHash)

instance HashAnnotated Foo FooI

foo :: Foo
foo = Foo short

-- ===================================
-- Lets run some examples, we'll need some concrete Crypto

foohash :: SafeHash FooI
foohash = hashAnnotated foo

shorthash :: SafeHash ShortByteString
shorthash = makeHashWithExplicitProxys (Proxy @ShortByteString) short

longhash :: SafeHash ByteString
longhash = makeHashWithExplicitProxys (Proxy @ByteString) long

test1, test2 :: TestTree
test1 =
  testCase
    "short==long"
    (assertEqual "ShortByteString and ByteString don't hash the same" shorthash (castSafeHash longhash))
test2 =
  testCase
    "newtype==underlyingtype"
    (assertEqual "A newtype and its underlying type dont hash the same" shorthash (castSafeHash foohash))

safeHashTest :: TestTree
safeHashTest =
  testGroup
    "SafeHash"
    [ testGroup "StandardCrypto" [test1, test2]
    ]
