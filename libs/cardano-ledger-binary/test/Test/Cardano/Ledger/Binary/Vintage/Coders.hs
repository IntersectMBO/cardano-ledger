{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}

module Test.Cardano.Ledger.Binary.Vintage.Coders (spec) where

import Cardano.Ledger.Binary
import Cardano.Ledger.Binary.Coders
import Cardano.Ledger.Binary.FlatTerm (FlatTerm, toFlatTerm)
import Data.Sequence.Strict (StrictSeq, fromList)
import Data.Text (Text, pack)
import Data.Typeable
import Test.Cardano.Ledger.Binary.RoundTrip (Trip (..), cborTrip, mkTrip, roundTripExpectation)
import Test.Hspec

-- ==========================================================================

data TT
  = A Int
  | B Int Bool
  | G [Int]
  | H (StrictSeq Bool)
  deriving (Show, Eq)

instance DecCBOR TT where
  decCBOR = decodeRecordSum "TT" $
    \case
      0 -> (\i -> (2, A i)) <$> decCBOR -- Tag for A is 0
      1 -> do
        -- (,) 3 . B <$> decCBOR <*> decCBOR
        i <- decCBOR
        b <- decCBOR
        pure (3, B i b) -- Tag for B is 1
      2 -> do
        l <- decodeList decCBOR
        pure (2, G l) -- Tag for G is 2
      3 -> do
        i <- decodeStrictSeq decCBOR
        pure (2, H i) -- Tag for H is 3
      k -> invalidKey k

-- =============================================================================================
-- JUST A NOTE about the (instance EncCBOR a => EncCBOR [a]). This uses the Begin .. End encoding,
-- while encodeList uses the list-length encoding.
--     encCBOR [5::Int,2]     --> [TkListBegin,TkInt 5,TkInt 2,TkBreak].
--     encodeList [5::Int,2] --> [TkListLen 2,TkInt 5,TkInt 2]
-- the (instance DecCBOR a => DecCBOR [a]) will ONLY RECOGNIZE THE BEGIN END ENCODING.
-- but the decoder (decodeList decCBOR) will recognize both styles of encoding. So in a decoder
-- or DecCBOR instance it is always preferable to use (decodeList decCBOR) over (decCBOR)
-- For example in the instance above, we could write either of these 2 lines
--       2 -> do { l <- decodeList decCBOR; pure(2,G l) }
--       2 -> do { l <- decCBOR; pure(2,G l) }
-- BUT THE FIRST IS MORE GENERAL. The following instance should be replaced
-- instance DecCBOR a => DecCBOR [a]  -- Defined in ‘cardano-binary-1.5.0:Cardano.Binary.DecCBOR’

instance EncCBOR TT where
  encCBOR (A i) = encodeListLen 2 <> encodeWord 0 <> encCBOR i
  encCBOR (B i b) = encodeListLen 3 <> encodeWord 1 <> encCBOR i <> encCBOR b
  encCBOR (G is) = encodeListLen 2 <> encodeWord 2 <> encCBOR is
  encCBOR (H bs) = encodeListLen 2 <> encodeWord 3 <> encCBOR bs

-- The Key is that in (G constr tag <@> ...)
-- The 'tag' for 'constr' aligns with the Tag in the case match
-- in the DecCBOR instance for TT above.

-- ===============================================================

-- ===================================
-- Examples

data Two = Two Int Bool
  deriving (Show, Eq)

decTwo :: Decode ('Closed 'Dense) Two
encTwo :: Two -> Encode ('Closed 'Dense) Two
decTwo = RecD Two <! From <! From

encTwo (Two a b) = Rec Two !> To a !> To b

instance EncCBOR Two where
  encCBOR two = encode $ encTwo two

instance DecCBOR Two where
  decCBOR = decode decTwo

-- ============

data Test = Test Int Two Integer
  deriving (Show, Eq)

test1 :: Test
test1 = Test 3 (Two 9 True) 33

decTestWithGroupForTwo :: Decode ('Closed 'Dense) Test
encTestWithGroupForTwo :: Test -> Encode ('Closed 'Dense) Test
decTestWithGroupForTwo = RecD Test <! From <! decTwo <! From

encTestWithGroupForTwo (Test a b c) = Rec Test !> To a !> encTwo b !> To c

instance EncCBOR Test where
  encCBOR = encode . encTestWithGroupForTwo

instance DecCBOR Test where
  decCBOR = decode decTestWithGroupForTwo

-- ===========

data Three = In Int | N Bool Integer | F Two
  deriving (Show, Eq)

three1, three2, three3 :: Three
three1 = In 7
three2 = N True 22
three3 = F (Two 1 False)

-- The following values 'decThree' and 'encThree' are meant to simulate the following instances
{-
instance EncCBOR Three where
  encCBOR (In x) = encodeListLen 2 <> encodeWord 0 <> encCBOR x
  encCBOR (N b i) = encodeListLen 3 <> encodeWord 1 <> encCBOR b <> encCBOR i
  encCBOR (F (Two i b)) = encodeListLen 3 <> encodeWord 2 <> encCBOR i <>  encCBOR b
     -- even though F has only 1 argument, we inline the two parts of Two,
     -- so it appears to have 2 arguments. This mimics CBORGROUP instances

instance DecCBOR Three where
  decCBOR = decodeRecordSum "Three" $
    \case
      0 -> do
        x <- decCBOR
        pure (2, In x)
      1 -> do
        b <- decCBOR
        i <- decCBOR
        pure (3, N b i)
      2 -> do
        i <- decCBOR
        b <- decCBOR
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

instance DecCBOR Three where
  decCBOR = decode (Summands "Three" decThree)

instance EncCBOR Three where
  encCBOR x = encode (encThree x)

-- ================================================================
-- In this test we nest many Records, and flatten out everything

data Big = Big Int Bool Integer
  deriving (Show, Eq)

data Bigger = Bigger Test Two Big
  deriving (Show, Eq)

bigger :: Bigger
bigger = Bigger (Test 2 (Two 4 True) 99) (Two 7 False) (Big 5 False 102)

-- Note there are 9 individual items, each which fits in one CBOR Token
-- So we expect the encoding to have 10 items, 1 prefix and 9 others

biggerItems :: FlatTerm
biggerItems = toFlatTerm shelleyProtVer (encode (encBigger bigger))

decBigger :: Decode ('Closed 'Dense) Bigger
decBigger =
  RecD Bigger
    <! (RecD Test <! From <! (RecD Two <! From <! From) <! From)
    <! (RecD Two <! From <! From)
    <! (RecD Big <! From <! From <! From)

encBigger :: Bigger -> Encode ('Closed 'Dense) Bigger
encBigger (Bigger (Test a (Two b c) d) (Two e f) (Big g h i)) =
  Rec Bigger
    !> (Rec Test !> To a !> (Rec Two !> To b !> To c) !> To d)
    !> (Rec Two !> To e !> To f)
    !> (Rec Big !> To g !> To h !> To i)

instance EncCBOR Bigger where
  encCBOR = encode . encBigger

instance DecCBOR Bigger where
  decCBOR = decode decBigger

-- ======================================================================
-- There are two ways to write smart encoders and decoders that don't put
-- fields with default values in the Encoding, and that reconstruct them
-- on the decoding side. These techniques work on record datatypes, i.e.
-- those with only one constructor. We will illustrate the two approaches
-- in the datatype A

data M = M Int [Bool] Text
  deriving (Show, Eq)

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

dualMvirtual :: Trip M M
dualMvirtual = mkTrip (encode . encM) (decode (Summands "M" decM))

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
boxM n = invalidField n

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

dualM :: Trip M M
dualM = mkTrip (encode . baz) (decode decodeM)

roundTripSpec :: (HasCallStack, Show t, Eq t, Typeable t) => String -> Trip t t -> t -> Spec
roundTripSpec name trip val = it name $ roundTripExpectation trip val

-- | Check that a value can be encoded using Coders and decoded using DecCBOR
encodeSpec :: (HasCallStack, Show t, Eq t, DecCBOR t) => String -> Encode w t -> t -> Spec
encodeSpec name enc = roundTripSpec name (mkTrip (const (encode enc)) decCBOR)

newtype C = C Text
  deriving (Show, Eq)

instance EncCBOR C where
  encCBOR (C t) = encCBOR t

instance DecCBOR C where
  decCBOR = C <$> decCBOR

newtype BB = BB Text
  deriving (Show, Eq)

dualBB :: Trip BB BB
dualBB = mkTrip (\(BB t) -> encCBOR t) (BB <$> decCBOR)

-- Record Type

data A = ACon Int BB C
  deriving (Show, Eq)

encodeA :: A -> Encode ('Closed 'Dense) A
encodeA (ACon i b c) = Rec ACon !> To i !> E (tripEncoder dualBB) b !> To c

decodeA :: Decode ('Closed 'Dense) A
decodeA = RecD ACon <! From <! D (tripDecoder dualBB) <! From

instance EncCBOR A where
  encCBOR x = encode (encodeA x)

instance DecCBOR A where
  decCBOR = decode decodeA

dualA :: Trip A A
dualA = cborTrip

recordTests :: Spec
recordTests =
  describe "Record tests" $ do
    roundTripSpec "A1" dualA (ACon 34 (BB "HI") (C "There"))
    roundTripSpec "A2" dualA (ACon 9 (BB "One") (C "Two"))

-- An example with multiple constructors uses Sum, SumD, and Summands

data N
  = N1 Int
  | N2 BB Bool
  | N3 A
  deriving (Show, Eq)

encodeN :: N -> Encode 'Open N
encodeN (N1 i) = Sum N1 0 !> To i
encodeN (N2 b tf) = Sum N2 1 !> E (tripEncoder dualBB) b !> To tf
encodeN (N3 a) = Sum N3 2 !> To a

decodeN :: Decode ('Closed 'Dense) N
decodeN = Summands "N" decodeNx
  where
    decodeNx 0 = SumD N1 <! From
    decodeNx 1 = SumD N2 <! D (tripDecoder dualBB) <! From
    decodeNx 2 = SumD N3 <! From
    decodeNx k = Invalid k

dualN :: Trip N N
dualN = mkTrip (encode . encodeN) (decode decodeN)

-- ============================================================

ttSpec :: Spec
ttSpec =
  describe "Encode TT" $ do
    -- Tag for A is 0
    encodeSpec "sA" (Sum A 0 !> To 7) (A 7)
    -- Tag for B is 1
    encodeSpec "sB" (Sum B 1 !> To 13 !> To True) (B 13 True)
    -- Tag for G is 2
    encodeSpec "sG" (Sum G 2 !> To [3, 4, 5]) (G [3, 4, 5])
    encodeSpec "sGa" (Sum G 2 !> E encCBOR [2, 5]) (G [2, 5])
    -- Tag for H is 3
    let sseq = fromList [False, True]
    encodeSpec "sH" (Sum H 3 !> E encCBOR sseq) (H sseq)

spec :: Spec
spec =
  describe "Coders" $ do
    describe "Simple Coders" $ do
      it "encode Bigger is compact" (length biggerItems `shouldBe` 10)
      ttSpec
      describe "Encode TT" $ do
        encodeSpec "Three1" (encThree three1) three1
        encodeSpec "Three2" (encThree three2) three2
        encodeSpec "Three3" (encThree three3) three3
      encodeSpec "test1" (encTestWithGroupForTwo test1) test1
      encodeSpec "Bigger inlines" (encBigger bigger) bigger
    recordTests
    describe "Sparse tests" $ do
      roundTripSpec "a0" dualM a0
      roundTripSpec "a1" dualM a1
      roundTripSpec "a2" dualM a2
      roundTripSpec "a3" dualM a3
    describe "Virtual Cosntructor tests" $ do
      roundTripSpec "a0v" dualMvirtual a0
      roundTripSpec "a1v" dualMvirtual a1
      roundTripSpec "a2v" dualMvirtual a2
      roundTripSpec "a3v" dualMvirtual a3
    describe "Sum tests" $ do
      roundTripSpec "N1" dualN (N1 4)
      roundTripSpec "N2" dualN (N2 (BB "N2") True)
      roundTripSpec "N3" dualN (N3 (ACon 6 (BB "N3") (C "Test")))
