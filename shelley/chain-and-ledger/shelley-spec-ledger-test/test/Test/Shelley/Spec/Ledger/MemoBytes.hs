{-# LANGUAGE LambdaCase  #-}
{-# LANGUAGE  FlexibleContexts  #-}
{-# LANGUAGE DataKinds #-}

module Test.Shelley.Spec.Ledger.MemoBytes
  ( memoBytesTest,
    testT,
  )
where

import Cardano.Binary
  ( FromCBOR (fromCBOR),
    ToCBOR (toCBOR),
    encodeListLen,
    encodeWord,
    Annotator(..),
    FullByteString(Full),
  )
import Cardano.Ledger.ShelleyMA.Timelocks
import Cardano.Slotting.Slot (SlotNo (..))
import Codec.CBOR.FlatTerm(TermToken,toFlatTerm)
import Codec.CBOR.Read(DeserialiseFailure,deserialiseFromBytes)
import Codec.CBOR.Write (toLazyByteString)
import qualified Data.ByteString.Lazy as Lazy
import Data.Sequence.Strict (StrictSeq,fromList)
import Shelley.Spec.Ledger.MemoBytes
import Shelley.Spec.Ledger.BaseTypes (StrictMaybe (SJust, SNothing),invalidKey)
import Shelley.Spec.Ledger.Serialization( decodeRecordSum )
import Test.Shelley.Spec.Ledger.ConcreteCryptoTypes(C)
import Test.Tasty
import Test.Tasty.QuickCheck hiding (scale)

data TT = A Int | B Int Bool | G [Int] | H (StrictSeq Bool) deriving (Show)

instance FromCBOR TT where
  fromCBOR = decodeRecordSum "TT" $
    \n -> case n of
      0 -> do i <- fromCBOR; pure (2, A i)                  -- Tag for A is 0
      1 -> do i <- fromCBOR; b <- fromCBOR; pure (3, B i b) -- Tag for B is 1
      2 -> do l <- decodeList fromCBOR; pure (2, G l)       -- Tag for G is 2
      3 -> do i <- decodeStrictSeq fromCBOR; pure (2, H i)  -- Tag for H is 3
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
sA  = Sum A 0 !> To 7                                        -- Tag for A is 0
sB  = Sum B 1 !> To 13 !> To True                            -- Tag for B is 1
sG  = Sum G 2 !> To  [3, 4, 5]                               -- Tag for G is 2
sGa = Sum G 2 !> E encodeList [2, 5]                         -- Tag for G is 2
sH =  Sum H 3 !> E encodeStrictSeq (fromList [False, True])    -- Tag for H is 3

-- ===============================================================

-- ===================================
-- Examples

data Two = Two Int Bool
  deriving Show

decTwo :: Decode 'Closed Two
encTwo :: Two -> Encode 'Closed Two

decTwo           = (RecD Two <! From <! From)
encTwo (Two a b) = (Rec Two  !> To a !> To b)

instance ToCBOR Two where
  toCBOR two = encode $ encTwo two

instance FromCBOR Two where
  fromCBOR = decode decTwo

-- ============

data Test = Test Int Two Integer
  deriving Show

test1 :: Test
test1 = Test 3 (Two 9 True) 33

decTestWithGroupForTwo :: Decode 'Closed Test
encTestWithGroupForTwo :: Test -> Encode 'Closed Test

decTestWithGroupForTwo =              (RecD Test <! From <! decTwo   <! From)
encTestWithGroupForTwo (Test a b c) = (Rec Test  !> To a !> encTwo b !> To c)

instance ToCBOR Test where
  toCBOR = encode . encTestWithGroupForTwo

instance FromCBOR Test where
  fromCBOR = decode decTestWithGroupForTwo

-- ===========

data Three = M Int | N Bool Integer | F Two
  deriving Show

three1, three2 , three3 :: Three
three1 = M 7
three2 = N True 22
three3 = F (Two 1 False)

-- The following values 'decThree' and 'encThree' are meant to simulate the following instances
{-
instance ToCBOR Three where
  toCBOR (M x) = encodeListLen 2 <> encodeWord 0 <> toCBOR x
  toCBOR (N b i) = encodeListLen 3 <> encodeWord 1 <> toCBOR b <> toCBOR i
  toCBOR (F (Two i b)) = encodeListLen 3 <> encodeWord 2 <> toCBOR i <>  toCBOR b
     -- even though F has only 1 argument, we inline the two parts of Two,
     -- so it appears to have 2 arguments. This mimics CBORGROUP instances


instance FromCBOR Three where
  fromCBOR = decodeRecordSum "Three" $
    \case
      0 -> do
        x <- fromCBOR
        pure (2, M x)
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
decThree 0 = (SumD M <! From)
decThree 1 = (SumD N <! From <! From)
decThree 2 = (SumD F <! decTwo)
decThree k = Invalid k

encThree :: Three -> Encode 'Open Three
encThree (M x)   = (Sum M 0) !> To x
encThree (N b i) = (Sum N 1) !> To b !> To i
encThree (F t)   = (Sum F 2) !> encTwo t


instance FromCBOR Three where
   fromCBOR = decode (Summands "Three" decThree)


instance ToCBOR Three where
   toCBOR x = encode (encThree x)


-- ================================================================
-- In this test we nest many Records, and flatten out everything

data Big = Big Int Bool Integer deriving Show
data Bigger = Bigger Test Two Big  deriving Show

bigger :: Bigger
bigger = Bigger (Test 2 (Two 4 True) 99) (Two 7 False) (Big 5 False 102)

-- Note there are 9 individual items, each which fits in one CBOR Token
-- So we expect the encoding to have 10 items, 1 prefix and 9 others

biggerItems :: [ TermToken ]
biggerItems = toFlatTerm (encode (encBigger bigger))

decBigger :: Decode 'Closed Bigger
decBigger = RecD Bigger <! (RecD Test <! From <! (RecD Two <! From <! From) <! From) <!
                           (RecD Two <! From <! From) <!
                           (RecD Big <! From <! From <! From)

encBigger :: Bigger -> Encode 'Closed Bigger
encBigger (Bigger (Test a (Two b c) d) (Two e f) (Big g h i)) =
            Rec Bigger !> (Rec Test !> To a !> (Rec Two !> To b !> To c ) !> To d) !>
                          (Rec Two !> To e !> To f) !>
                          (Rec Big !> To g !> To h !> To i)

instance ToCBOR Bigger where
  toCBOR = encode .encBigger

instance FromCBOR Bigger where
  fromCBOR = decode decBigger



-- ================================================================

s1 :: Timelock C
s1 = Interval (SJust (SlotNo 12)) SNothing

s2 :: Timelock C
s2 = Interval (SJust (SlotNo 12)) (SJust(SlotNo 23))

s4 :: Timelock C
s4 = TimelockAnd (fromList [s1,s2])


-- ================================================================

testEncode ::
  (FromCBOR t) =>
  Encode w t ->
  Either Codec.CBOR.Read.DeserialiseFailure (Lazy.ByteString, t)
testEncode s = deserialiseFromBytes fromCBOR (toLazyByteString (encode s))

testT :: (ToCBOR t,FromCBOR t) => t -> Either Codec.CBOR.Read.DeserialiseFailure (Lazy.ByteString, t)
testT s = deserialiseFromBytes fromCBOR (toLazyByteString (toCBOR s))

testAnn :: (ToCBOR t,FromCBOR (Annotator t)) => t -> Either Codec.CBOR.Read.DeserialiseFailure (Lazy.ByteString, t)
testAnn s = let bytes = (toLazyByteString (toCBOR s))
            in case deserialiseFromBytes fromCBOR bytes of
                Left err -> Left err
                Right(leftover,Annotator f) -> Right(leftover,f (Full bytes))


deCodeTest :: (FromCBOR t, Show t) => String -> Encode w t -> TestTree
deCodeTest name sym =
   testProperty (name++": Decoding "++show(runE sym)) $
     case testEncode sym of
       Right _ -> True
       Left s -> error ("Fail to decode "++show(runE sym)++" with error "++show s)

annTest :: (FromCBOR (Annotator t), ToCBOR t,Show t) => String -> t -> TestTree
annTest nm t = testProperty ("RoundTrip: "++nm) $
                    case testAnn t of
                      Right _ -> True
                      Left s -> error ("Fail to roundtrip "++show t++" with error "++show s)

memoBytesTest :: TestTree
memoBytesTest = testGroup "MemoBytesTest"
    [ deCodeTest "sA" sA,
      deCodeTest "sB" sB,
      deCodeTest "sG" sG,
      deCodeTest "sGA" sGa,
      deCodeTest "sH" sH,
      deCodeTest "Three1" (encThree three1),
      deCodeTest "Three2" (encThree three2),
      deCodeTest "Three3" (encThree three3),
      deCodeTest "test1"  (encTestWithGroupForTwo test1),
      testProperty "encode Bigger is compact" (length biggerItems === 10),
      deCodeTest "Bigger inlines" (encBigger bigger),
      annTest ("s1 "++showTimelock s1) s1,
      annTest ("s2 "++showTimelock s2) s2,
      annTest ("s4 "++showTimelock s4) s4
    ]
