{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Test.Cardano.Ledger.ShelleyMA.Serialisation.Coders
  ( codersTest,
    roundTrip,
    roundTrip',
    embedTrip,
    embedTrip',
    roundTripAnn,
    embedTripAnn,
    RoundTripResult,
  )
where

import Cardano.Binary
  ( Annotator (..),
    FromCBOR (fromCBOR),
    FullByteString (Full),
    ToCBOR (toCBOR),
    encodeListLen,
    encodeWord,
  )
import Codec.CBOR.Decoding (Decoder)
import Codec.CBOR.Encoding (Encoding)
import Codec.CBOR.FlatTerm (TermToken, toFlatTerm)
import Codec.CBOR.Read (DeserialiseFailure, deserialiseFromBytes)
import Codec.CBOR.Write (toLazyByteString)
import qualified Data.ByteString.Lazy as Lazy
import Data.Coders
  ( Decode (..),
    Density (..),
    Dual (..),
    Encode (..),
    Field,
    Wrapped (..),
    decode,
    decodeList,
    decodeRecordSum,
    decodeStrictSeq,
    encode,
    encodeFoldable,
    field,
    invalidKey,
    runE,
    (!>),
    (<!),
  )
import Data.Sequence.Strict (StrictSeq, fromList)
import Data.Text (Text, pack)
import Data.Typeable
import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck hiding (scale)

-- =====================================================================

type RoundTripResult t = Either Codec.CBOR.Read.DeserialiseFailure (Lazy.ByteString, t)

roundTrip :: (ToCBOR t, FromCBOR t) => t -> RoundTripResult t
roundTrip s = deserialiseFromBytes fromCBOR (toLazyByteString (toCBOR s))

roundTrip' :: (t -> Encoding) -> (forall s. Decoder s t) -> t -> RoundTripResult t
roundTrip' enc dec t = deserialiseFromBytes dec (toLazyByteString (enc t))

roundTripAnn :: (ToCBOR t, FromCBOR (Annotator t)) => t -> RoundTripResult t
roundTripAnn s =
  let bytes = toLazyByteString (toCBOR s)
   in case deserialiseFromBytes fromCBOR bytes of
        Left err -> Left err
        Right (leftover, Annotator f) -> Right (leftover, f (Full bytes))

-- | Can we serialise a type, and then deserialise it as something else?
embedTrip :: (ToCBOR t, FromCBOR s) => t -> RoundTripResult s
embedTrip s = deserialiseFromBytes fromCBOR (toLazyByteString (toCBOR s))

embedTrip' :: (s -> Encoding) -> (forall x. Decoder x t) -> s -> RoundTripResult t
embedTrip' enc dec s = deserialiseFromBytes dec (toLazyByteString (enc s))

embedTripAnn :: forall s t. (ToCBOR t, FromCBOR (Annotator s)) => t -> RoundTripResult s
embedTripAnn s =
  let bytes = toLazyByteString (toCBOR s)
   in case deserialiseFromBytes fromCBOR bytes of
        Left err -> Left err
        Right (leftover, Annotator f) -> Right (leftover, f (Full bytes))

-- ==========================================================================

data TT = A Int | B Int Bool | G [Int] | H (StrictSeq Bool) deriving (Show)

instance FromCBOR TT where
  fromCBOR = decodeRecordSum "TT" $
    \case
      0 -> do i <- fromCBOR; pure (2, A i) -- Tag for A is 0
      1 -> do i <- fromCBOR; b <- fromCBOR; pure (3, B i b) -- Tag for B is 1
      2 -> do l <- decodeList fromCBOR; pure (2, G l) -- Tag for G is 2
      3 -> do i <- decodeStrictSeq fromCBOR; pure (2, H i) -- Tag for H is 3
      k -> invalidKey k

-- =============================================================================================
-- JUST A NOTE about the (instance ToCBOR a => ToCBOR [a]). This uses the Begin .. End encoding,
-- while encodeList uses the list-length encoding.
--     toCBOR [5::Int,2]     --> [TkListBegin,TkInt 5,TkInt 2,TkBreak].
--     encodeList [5::Int,2] --> [TkListLen 2,TkInt 5,TkInt 2]
-- the (instance FromCBOR a => FromCBOR [a]) will ONLY RECOGNIZE THE BEGIN END ENCODING.
-- but the decoder (decodeList fromCBOR) will recognize both styles of encoding. So in a decoder
-- or FromCBOR instance it is always preferable to use (decodeList fromCBOR) over (fromCBOR)
-- For example in the instance above, we could write either of these 2 lines
--       2 -> do { l <- decodeList fromCBOR; pure(2,G l) }
--       2 -> do { l <- fromCBOR; pure(2,G l) }
-- BUT THE FIRST IS MORE GENERAL. The following instance should be replaced
-- instance FromCBOR a => FromCBOR [a]  -- Defined in ‘cardano-binary-1.5.0:Cardano.Binary.FromCBOR’

instance ToCBOR TT where
  toCBOR (A i) = encodeListLen 2 <> encodeWord 0 <> toCBOR i
  toCBOR (B i b) = encodeListLen 3 <> encodeWord 1 <> toCBOR i <> toCBOR b
  toCBOR (G is) = encodeListLen 2 <> encodeWord 2 <> toCBOR is
  toCBOR (H bs) = encodeListLen 2 <> encodeWord 3 <> encodeStrictSeq bs

-- The Key is that in (G constr tag <@> ...)
-- The 'tag' for 'constr' aligns with the Tag in the case match
-- in the FromCBOR instance for TT above.

sA, sB, sG, sGa, sH :: Encode 'Open TT
sA = Sum A 0 !> To 7 -- Tag for A is 0
sB = Sum B 1 !> To 13 !> To True -- Tag for B is 1
sG = Sum G 2 !> To [3, 4, 5] -- Tag for G is 2
sGa = Sum G 2 !> E encodeList [2, 5] -- Tag for G is 2
sH = Sum H 3 !> E encodeStrictSeq (fromList [False, True]) -- Tag for H is 3

encodeList :: ToCBOR t => [t] -> Encoding
encodeList = encodeFoldable

encodeStrictSeq :: ToCBOR t => StrictSeq t -> Encoding
encodeStrictSeq = encodeFoldable

-- ===============================================================

-- ===================================
-- Examples

data Two = Two Int Bool
  deriving (Show)

decTwo :: Decode ('Closed 'Dense) Two
encTwo :: Two -> Encode ('Closed 'Dense) Two
decTwo = RecD Two <! From <! From

encTwo (Two a b) = Rec Two !> To a !> To b

instance ToCBOR Two where
  toCBOR two = encode $ encTwo two

instance FromCBOR Two where
  fromCBOR = decode decTwo

-- ============

data Test = Test Int Two Integer
  deriving (Show)

test1 :: Test
test1 = Test 3 (Two 9 True) 33

decTestWithGroupForTwo :: Decode ('Closed 'Dense) Test
encTestWithGroupForTwo :: Test -> Encode ('Closed 'Dense) Test
decTestWithGroupForTwo = RecD Test <! From <! decTwo <! From

encTestWithGroupForTwo (Test a b c) = Rec Test !> To a !> encTwo b !> To c

instance ToCBOR Test where
  toCBOR = encode . encTestWithGroupForTwo

instance FromCBOR Test where
  fromCBOR = decode decTestWithGroupForTwo

-- ===========

data Three = In Int | N Bool Integer | F Two
  deriving (Show)

three1, three2, three3 :: Three
three1 = In 7
three2 = N True 22
three3 = F (Two 1 False)

-- The following values 'decThree' and 'encThree' are meant to simulate the following instances
{-
instance ToCBOR Three where
  toCBOR (In x) = encodeListLen 2 <> encodeWord 0 <> toCBOR x
  toCBOR (N b i) = encodeListLen 3 <> encodeWord 1 <> toCBOR b <> toCBOR i
  toCBOR (F (Two i b)) = encodeListLen 3 <> encodeWord 2 <> toCBOR i <>  toCBOR b
     -- even though F has only 1 argument, we inline the two parts of Two,
     -- so it appears to have 2 arguments. This mimics CBORGROUP instances

instance FromCBOR Three where
  fromCBOR = decodeRecordSum "Three" $
    \case
      0 -> do
        x <- fromCBOR
        pure (2, In x)
      1 -> do
        b <- fromCBOR
        i <- fromCBOR
        pure (3, N b i)
      2 -> do
        i <- fromCBOR
        b <- fromCBOR
        pure (3,F (Two i b))
      k -> invalidKey k
-}

decThree :: Word -> Decode 'Open Three
decThree 0 = SumD In <! From
decThree 1 = SumD N <! From <! From
decThree 2 = SumD F <! decTwo
decThree k = Invalid k

encThree :: Three -> Encode 'Open Three
encThree (In x) = Sum In 0 !> To x
encThree (N b i) = Sum N 1 !> To b !> To i
encThree (F t) = Sum F 2 !> encTwo t

instance FromCBOR Three where
  fromCBOR = decode (Summands "Three" decThree)

instance ToCBOR Three where
  toCBOR x = encode (encThree x)

-- ================================================================
-- In this test we nest many Records, and flatten out everything

data Big = Big Int Bool Integer deriving (Show)

data Bigger = Bigger Test Two Big deriving (Show)

bigger :: Bigger
bigger = Bigger (Test 2 (Two 4 True) 99) (Two 7 False) (Big 5 False 102)

-- Note there are 9 individual items, each which fits in one CBOR Token
-- So we expect the encoding to have 10 items, 1 prefix and 9 others

biggerItems :: [TermToken]
biggerItems = toFlatTerm (encode (encBigger bigger))

decBigger :: Decode ('Closed 'Dense) Bigger
decBigger =
  RecD Bigger <! (RecD Test <! From <! (RecD Two <! From <! From) <! From)
    <! (RecD Two <! From <! From)
    <! (RecD Big <! From <! From <! From)

encBigger :: Bigger -> Encode ('Closed 'Dense) Bigger
encBigger (Bigger (Test a (Two b c) d) (Two e f) (Big g h i)) =
  Rec Bigger !> (Rec Test !> To a !> (Rec Two !> To b !> To c) !> To d)
    !> (Rec Two !> To e !> To f)
    !> (Rec Big !> To g !> To h !> To i)

instance ToCBOR Bigger where
  toCBOR = encode . encBigger

instance FromCBOR Bigger where
  fromCBOR = decode decBigger

-- ======================================================================
-- There are two ways to write smart encoders and decoders that don't put
-- fields with default values in the Encoding, and that reconstruct them
-- on the decoding side. These techniques work on record datatypes, i.e.
-- those with only one constructor. We will illustrate the two approaches
-- in the datatype A

data M = M Int [Bool] Text
  deriving (Show, Typeable)

a0, a1, a2, a3 :: M
a0 = M 0 [] "ABC"
a1 = M 0 [True] "ABC"
a2 = M 9 [] "ABC"
a3 = M 9 [False] "ABC"

-- ==========================================================================
-- The virtual constructor stategy pretends there are mutiple constructors
-- Even though there is only one. We use invariants about the data to avoid
-- encoding some of the values.

encM :: M -> Encode 'Open M
encM (M 0 [] t) = Sum M 0 !> OmitC 0 !> OmitC [] !> To t
encM (M 0 bs t) = Sum M 1 !> OmitC 0 !> To bs !> To t
encM (M n [] t) = Sum M 2 !> To n !> OmitC [] !> To t
encM (M n bs t) = Sum M 3 !> To n !> To bs !> To t

decM :: Word -> Decode 'Open M
decM 0 = SumD M <! Emit 0 <! Emit [] <! From
decM 1 = SumD M <! Emit 0 <! From <! From
decM 2 = SumD M <! From <! Emit [] <! From
decM 3 = SumD M <! From <! From <! From
decM n = Invalid n

dualMvirtual :: Dual M
dualMvirtual = Dual (encode . encM) (decode (Summands "M" decM))

-- ================================================================================
-- The Sparse encoding strategy uses N keys, one for each field that is not defaulted
-- encode (baz (M 9 [True] (pack "hi"))) --Here No fields are defaulted, should be 3 keys
-- [TkMapLen 3,TkInt 0,TkInt 9,TkInt 1,TkListBegin,TkBool True,TkBreak,TkInt 2,TkString "hi"]
--                   ^key            ^key                                    ^key
-- So the user supplies a function, that encodes every field, each field must use a unique
-- key, and fields with default values have Omit wrapped around the Key encoding.
-- The user must ensure that there is NOT an Omit on a required field. 'baz' is an example.

baz :: M -> Encode ('Closed 'Sparse) M
baz (M n xs t) = Keyed M !> Omit (== 0) (Key 0 (To n)) !> Omit null (Key 1 (To xs)) !> Key 2 (To t)

-- To write an Decoder we must pair a decoder for each field, with a function that updates only
-- that field. We use the Field GADT to construct these pairs, and we must write a function, that
-- for each field tag, picks out the correct pair. If the Encode and Decode don't agree on how the
-- tags correspond to a particular field, things will fail.

boxM :: Word -> Field M
boxM 0 = field update0 From
  where
    update0 n (M _ xs t) = M n xs t
boxM 1 = field update1 From
  where
    update1 xs (M n _ t) = M n xs t
boxM 2 = field update2 From
  where
    update2 t (M n xs _) = M n xs t
boxM n = field (\_ t -> t) (Invalid n)

-- Finally there is a new constructor for Decode, called SparseKeyed, that decodes field
-- keyed sparse objects. The user supplies an initial value and pick function, and a list
-- of tags of the required fields. The initial value should have default values and
-- any well type value in required fields. If the encode function (baz above) is
-- encoded properly the required fields in the initial value should always be over
-- overwritten. If it is not written properly, or a bad encoding comes from somewhere
-- else, the intial values in the required fields might survive decoding. The list
-- of required fields is checked.

decodeM :: Decode ('Closed 'Dense) M -- Only the field with Key 2 is required
decodeM = SparseKeyed "M" (M 0 [] (pack "a")) boxM [(2, "Stringpart")]

dualM :: Dual M
dualM = Dual (encode . baz) (decode decodeM)

type Answer t = Either Codec.CBOR.Read.DeserialiseFailure (Lazy.ByteString, t)

-- FIXME: why is it unused
_testM :: M -> Answer M
_testM = roundTrip' (encode . baz) (decode decodeM)

roundtrip :: Show t => String -> Dual t -> t -> TestTree
roundtrip name (Dual enc dec) v =
  testCase
    ("roundtrip " ++ name ++ " =(" ++ show v ++ ")")
    ( assertBool
        name
        ( case roundTrip' enc dec v of
            Right _ -> True
            Left s -> error (show s)
        )
    )

testEncode ::
  (FromCBOR t) =>
  Encode w t ->
  Either Codec.CBOR.Read.DeserialiseFailure (Lazy.ByteString, t)
testEncode s = deserialiseFromBytes fromCBOR (toLazyByteString (encode s))

deCodeTest :: (FromCBOR t, Show t) => String -> Encode w t -> TestTree
deCodeTest name sym =
  testProperty (name ++ ": Decoding " ++ show (runE sym)) $
    case testEncode sym of
      Right _ -> True
      Left s -> error ("Fail to decode " ++ show (runE sym) ++ " with error " ++ show s)

-- ====================================================================
-- Some tests

q0, q1, q2, q3 :: Answer M
q0 = roundTrip' (encode . baz) (decode decodeM) (M 0 [] (pack "MBC"))
q1 = roundTrip' (encode . baz) (decode decodeM) (M 0 [True] (pack "MBC"))
q2 = roundTrip' (encode . baz) (decode decodeM) (M 42 [] (pack "MBC"))
q3 = roundTrip' (encode . baz) (decode decodeM) (M 9 [True, False] (pack "MBC"))

-- FIXME: Why is this unused?
_ok :: Bool
_ok = all isRight [q0, q1, q2, q3]
  where
    isRight (Right _) = True
    isRight (Left _) = False

-- In the examples Let  Int and C have ToCBOR instances, and dualB :: Dual B
-- An example with 1 constructor (a record) uses Rec and RecD

newtype C = C Text deriving (Show)

instance ToCBOR C where toCBOR (C t) = toCBOR t

instance FromCBOR C where fromCBOR = C <$> fromCBOR

newtype BB = BB Text deriving (Show)

dualBB :: Dual BB
dualBB = Dual (\(BB t) -> toCBOR t) (BB <$> fromCBOR)

-- Record Type

data A = ACon Int BB C deriving (Show)

encodeA :: A -> Encode ('Closed 'Dense) A
encodeA (ACon i b c) = Rec ACon !> To i !> ED dualBB b !> To c

decodeA :: Decode ('Closed 'Dense) A
decodeA = RecD ACon <! From <! DD dualBB <! From

instance ToCBOR A where toCBOR x = encode (encodeA x)

instance FromCBOR A where fromCBOR = decode decodeA

dualA :: Dual A
dualA = Dual (encode . encodeA) (decode decodeA)

recordTests :: TestTree
recordTests =
  testGroup
    "Record tests"
    [ roundtrip "A1" dualA (ACon 34 (BB "HI") (C "There")),
      roundtrip "A2" dualA (ACon 9 (BB "One") (C "Two"))
    ]

-- An example with multiple constructors uses Sum, SumD, and Summands

data N = N1 Int | N2 BB Bool | N3 A deriving (Show)

encodeN :: N -> Encode 'Open N
encodeN (N1 i) = Sum N1 0 !> To i
encodeN (N2 b tf) = Sum N2 1 !> ED dualBB b !> To tf
encodeN (N3 a) = Sum N3 2 !> To a

decodeN :: Decode ('Closed 'Dense) N
decodeN = Summands "N" decodeNx
  where
    decodeNx 0 = SumD N1 <! From
    decodeNx 1 = SumD N2 <! DD dualBB <! From
    decodeNx 2 = SumD N3 <! From
    decodeNx k = Invalid k

dualN :: Dual N
dualN = Dual (encode . encodeN) (decode decodeN)

-- ============================================================

codersTest :: TestTree
codersTest =
  testGroup
    "Coders"
    [ testGroup
        "Simple Coders"
        [ deCodeTest "sA" sA,
          deCodeTest "sB" sB,
          deCodeTest "sG" sG,
          deCodeTest "sGA" sGa,
          deCodeTest "sH" sH,
          deCodeTest "Three1" (encThree three1),
          deCodeTest "Three2" (encThree three2),
          deCodeTest "Three3" (encThree three3),
          deCodeTest "test1" (encTestWithGroupForTwo test1),
          testProperty "encode Bigger is compact" (length biggerItems === 10),
          deCodeTest "Bigger inlines" (encBigger bigger)
        ],
      recordTests,
      testGroup
        "Sparse tests"
        [ roundtrip "a0" dualM a0,
          roundtrip "a1" dualM a1,
          roundtrip "a2" dualM a2,
          roundtrip "a3" dualM a3
        ],
      testGroup
        "Virtual Cosntructor tests"
        [ roundtrip "a0v" dualMvirtual a0,
          roundtrip "a1v" dualMvirtual a1,
          roundtrip "a2v" dualMvirtual a2,
          roundtrip "a3v" dualMvirtual a3
        ],
      testGroup
        "Sum tests"
        [ roundtrip "N1" dualN (N1 4),
          roundtrip "N2" dualN (N2 (BB "N2") True),
          roundtrip "N3" dualN (N3 (ACon 6 (BB "N3") (C "Test")))
        ]
    ]
