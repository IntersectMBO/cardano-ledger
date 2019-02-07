{-# LANGUAGE AllowAmbiguousTypes        #-}
{-# LANGUAGE ConstraintKinds            #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE TypeApplications           #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}

module Test.Cardano.Binary.Helpers
  ( IdTestingRequiredClassesAlmost

       -- * From/to
  , binaryEncodeDecode
  , binaryTest
  , showReadId
  , showReadTest
  , identityTest

       -- * Binary test helpers
  , U
  , U24
  , extensionProperty

       -- * Message length
  , msgLenLimitedTest

       -- * Static size estimates
  , SizeTestConfig(..)
  , cfg
  , scfg
  , sizeTest
  )
where

import Cardano.Prelude
import Test.Cardano.Prelude

import Codec.CBOR.FlatTerm (toFlatTerm, validFlatTerm)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LBS
import Data.Map (Map)
import qualified Data.Map as M
import Data.String (String)
import Data.Text.Lazy (unpack)
import Data.Text.Lazy.Builder (toLazyText)
import Data.Typeable (TypeRep, typeRep)
import Formatting (Buildable, bprint, build, formatToString, int)
import Hedgehog (annotate, failure, forAllWith, success)
import qualified Hedgehog as HH
import qualified Hedgehog.Gen as HH.Gen
import Prelude (read)
import Test.Hspec (Spec, describe)
import Test.Hspec.QuickCheck (modifyMaxSuccess, prop)
import Test.QuickCheck
  ( Arbitrary(arbitrary)
  , Gen
  , Property
  , choose
  , conjoin
  , counterexample
  , forAll
  , property
  , resize
  , suchThat
  , vectorOf
  , (.&&.)
  , (===)
  )
import Test.QuickCheck.Instances ()

import Cardano.Binary.Class
  ( Bi(..)
  , Range(..)
  , Size
  , SizeOverride(..)
  , decodeListLenOf
  , decodeUnknownCborDataItem
  , encodeListLen
  , encodeUnknownCborDataItem
  , serialize
  , serialize'
  , szSimplify
  , szWithCtx
  , toLazyByteString
  , unsafeDeserialize
  )
import Cardano.Binary.Limit (Limit(..))


--------------------------------------------------------------------------------
-- From/to tests
--------------------------------------------------------------------------------

-- | Basic binary serialization/deserialization identity.
binaryEncodeDecode :: (Show a, Eq a, Bi a) => a -> Property
binaryEncodeDecode a = (unsafeDeserialize . serialize $ a) === a

-- | Machinery to test we perform "flat" encoding.
cborFlatTermValid :: Bi a => a -> Property
cborFlatTermValid = property . validFlatTerm . toFlatTerm . encode

showReadId :: (Show a, Eq a, Read a) => a -> Property
showReadId a = read (show a) === a

type IdTestingRequiredClassesAlmost a = (Eq a, Show a, Arbitrary a, Typeable a)

type IdTestingRequiredClasses f a = (Eq a, Show a, Arbitrary a, Typeable a, f a)

identityTest
  :: forall a . (IdTestingRequiredClassesAlmost a) => (a -> Property) -> Spec
identityTest = prop typeName
 where
  typeName :: String
  typeName = show $ typeRep (Proxy @a)

binaryTest :: forall a . IdTestingRequiredClasses Bi a => Spec
binaryTest =
  identityTest @a $ \x -> binaryEncodeDecode x .&&. cborFlatTermValid x

showReadTest :: forall a . IdTestingRequiredClasses Read a => Spec
showReadTest = identityTest @a showReadId

--------------------------------------------------------------------------------

-- Type to be used to simulate a breaking change in the serialisation
-- schema, so we can test instances which uses the `UnknownXX` pattern
-- for extensibility.
-- Check the `extensionProperty` for more details.
data U = U Word8 BS.ByteString deriving (Show, Eq)

instance Bi U where
  encode (U word8 bs) =
    encodeListLen 2 <> encode (word8 :: Word8) <> encodeUnknownCborDataItem
      (LBS.fromStrict bs)
  decode = do
    decodeListLenOf 2
    U <$> decode <*> decodeUnknownCborDataItem

instance Arbitrary U where
  arbitrary = U <$> choose (0, 255) <*> arbitrary

-- | Like `U`, but we expect to read back the Cbor Data Item when decoding.
data U24 = U24 Word8 BS.ByteString deriving (Show, Eq)

instance Bi U24 where
  encode (U24 word8 bs) =
    encodeListLen 2 <> encode (word8 :: Word8) <> encodeUnknownCborDataItem
      (LBS.fromStrict bs)
  decode = do
    decodeListLenOf 2
    U24 <$> decode <*> decodeUnknownCborDataItem

-- | Given a data type which can be extended, verify we can indeed do so
-- without breaking anything. This should work with every time which adopted
-- the schema of having at least one constructor of the form:
-- .... | Unknown Word8 ByteString
extensionProperty :: forall a . (Arbitrary a, Eq a, Show a, Bi a) => Property
extensionProperty = forAll @a (arbitrary :: Gen a) $ \input ->
{- This function works as follows:

   1. When we call `serialized`, we are implicitly assuming (as contract of this
      function) that the input type would be of a shape such as:

      data MyType = Constructor1 Int Bool
                  | Constructor2 String
                  | UnknownConstructor Word8 ByteString

      Such type will be encoded, roughly, like this:

      encode (Constructor1 a b) = encodeWord 0 <> encodeKnownCborDataItem (a,b)
      encode (Constructor2 a b) = encodeWord 1 <> encodeKnownCborDataItem a
      encode (UnknownConstructor tag bs) = encodeWord tag <> encodeUnknownCborDataItem bs

      In CBOR terms, we would produce something like this:

      <tag :: Word32><Tag24><CborDataItem :: ByteString>

   2. Now, when we call `unsafeDeserialize serialized`, we are effectively asking to produce as
      output a value of type `U`. `U` is defined by only 1 constructor, it
      being `U Word8 ByteString`, but this is still compatible with our `tag + cborDataItem`
      format. So now we will have something like:

      U <tag :: Word32> <CborDataItem :: ByteString>

      (The <Tag24> has been removed as part of the decoding process).

   3. We now call `unsafeDeserialize (serialize u)`, which means: Can you produce a CBOR binary
      from `U`, and finally try to decode it into a value of type `a`? This will work because
      our intermediate encoding into `U` didn't touch the inital `<tag :: Word32>`, so we will
      be able to reconstruct the original object back.
      More specifically, `serialize u` would produce once again:

      <tag :: Word32><Tag24><CborDataItem :: ByteString>

      (The <Tag24> has been added as part of the encoding process).

      `unsafeDeserialize` would then consume the tag (to understand which type constructor this corresponds to),
      remove the <Tag24> token and finally proceed to deserialise the rest.

-}
  let
    serialized     = serialize input             -- Step 1
    (u :: U      ) = unsafeDeserialize serialized      -- Step 2
    (encoded :: a) = unsafeDeserialize (serialize u)   -- Step 3
  in encoded === input

--------------------------------------------------------------------------------
-- Message length
--------------------------------------------------------------------------------

msgLenLimitedCheck :: Bi a => Limit a -> a -> Property
msgLenLimitedCheck limit msg = if sz <= fromIntegral limit
  then property True
  else flip counterexample False $ formatToString
    ("Message size (max found " . int . ") exceeds limit (" . int . ")")
    sz
    limit
  where sz = BS.length . serialize' $ msg

msgLenLimitedTest'
  :: forall a
   . IdTestingRequiredClasses Bi a
  => Limit a
  -> String
  -> (a -> Bool)
  -> Spec
msgLenLimitedTest' limit desc whetherTest =
  -- instead of checking for `arbitrary` values, we'd better generate
  -- many values and find maximal message size - it allows user to get
  -- correct limit on the spot, if needed.
  addDesc $ modifyMaxSuccess (const 1) $ identityTest @a $ \_ ->
    findLargestCheck .&&. listsCheck
 where
  addDesc :: Spec -> Spec
  addDesc act = if null desc then act else describe desc act

  genNice          = arbitrary `suchThat` whetherTest

  findLargestCheck = forAll (resize 1 $ vectorOf 50 genNice) $ \samples ->
    counterexample desc $ msgLenLimitedCheck limit $ maximumBy
      (comparing $ BS.length . serialize')
      samples

  -- In this test we increase length of lists, maps, etc. generated
  -- by `arbitrary` (by default lists sizes are bounded by 100).
  --
  -- Motivation: if your structure contains lists, you should ensure
  -- their lengths are limited in practise. If you did, use `MaxSize`
  -- wrapper to generate `arbitrary` objects of that type with lists of
  -- exactly maximal possible size.
  listsCheck =
    let
      doCheck :: Int -> Property
      doCheck power = forAll (resize (2 ^ power) genNice) $ \a ->
        counterexample desc
          $ counterexample "Potentially unlimited size!"
          $ msgLenLimitedCheck limit a
    in 
       -- Increase lists length gradually to avoid hanging.
       conjoin $ doCheck <$> [1 .. 13 :: Int]

msgLenLimitedTest
  :: forall a . (IdTestingRequiredClasses Bi a) => Limit a -> Spec
msgLenLimitedTest lim = msgLenLimitedTest' @a lim "" (const True)

--------------------------------------------------------------------------------
-- Orphans
--------------------------------------------------------------------------------

deriving instance Bi bi => Bi (SmallGenerator bi)

--------------------------------------------------------------------------------
-- Static size estimates
--------------------------------------------------------------------------------

bshow :: Buildable a => a -> String
bshow = unpack . toLazyText . bprint build

-- | Configuration for a single test case.
data SizeTestConfig a = SizeTestConfig
    { debug       :: a -> String     -- ^ Pretty-print values
    , gen         :: HH.Gen a        -- ^ Generator
    , precise     :: Bool            -- ^ Must estimates be exact?
    , addlCtx     :: Map TypeRep SizeOverride -- ^ Additional size overrides
    , computedCtx :: a -> Map TypeRep SizeOverride
      -- ^ Size overrides computed from a concrete instance.
    }

-- | Default configuration, for @Buildable@ types.
cfg :: Buildable a => SizeTestConfig a
cfg = SizeTestConfig
  { debug       = bshow
  , gen         = HH.Gen.discard
  , precise     = False
  , addlCtx     = M.fromList []
  , computedCtx = const (M.fromList [])
  }

-- | Default configuration, for @Show@able types.
scfg :: Show a => SizeTestConfig a
scfg = SizeTestConfig
  { debug       = show
  , gen         = HH.Gen.discard
  , precise     = False
  , addlCtx     = M.fromList []
  , computedCtx = const (M.fromList [])
  }

-- | Create a test case from the given test configuration.
sizeTest :: forall a . Bi a => SizeTestConfig a -> HH.Property
sizeTest SizeTestConfig {..} = HH.property $ do
  x <- forAllWith debug gen

  let
    ctx = M.union (computedCtx x) addlCtx

    badBounds :: Natural -> Range Natural -> HH.PropertyT IO ()
    badBounds sz bounds = do
      annotate ("Computed bounds: " <> bshow bounds)
      annotate ("Actual size:     " <> show sz)
      annotate ("Value: " <> debug x)

  case szVerify ctx x of
    Exact -> success
    WithinBounds _ _ | not precise -> success
    WithinBounds sz bounds -> do
      badBounds sz bounds
      annotate "Bounds were not exact."
      failure
    BoundsAreSymbolic bounds -> do
      annotate ("Bounds are symbolic: " <> bshow bounds)
      failure
    OutOfBounds sz bounds -> do
      badBounds sz bounds
      annotate "Size fell outside of bounds."
      failure

-- | The possible results from @szVerify@, describing various ways
--   a size can or cannot be found within a certain range.
data ComparisonResult
    = Exact                          -- ^ Size matched the bounds, and the bounds were exact.
    | WithinBounds Natural (Range Natural) -- ^ Size matched the bounds, but the bounds are not exact.
    | BoundsAreSymbolic Size         -- ^ The bounds could not be reduced to a numerical range.
    | OutOfBounds Natural (Range Natural)  -- ^ The size fell outside of the bounds.

-- | For a given value @x :: a@ with @Bi a@, check that the encoded size
--   of @x@ falls within the statically-computed size range for @a@.
szVerify :: Bi a => Map TypeRep SizeOverride -> a -> ComparisonResult
szVerify ctx x = case szSimplify (szWithCtx ctx (pure x)) of
  Left bounds -> BoundsAreSymbolic bounds
  Right range | lo range <= sz && sz <= hi range ->
    if lo range == hi range then Exact else WithinBounds sz range
  Right range -> OutOfBounds sz range
 where
  sz :: Natural
  sz = fromIntegral $ LBS.length $ toLazyByteString $ encode x
