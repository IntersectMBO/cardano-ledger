

module Test.Shelley.Spec.Ledger.MemoBytes
  ( memoBytesTest,
  )
where

import Data.Sequence.Strict (StrictSeq,fromList)
import qualified Data.ByteString.Lazy as Lazy
import Shelley.Spec.Ledger.MemoBytes
import Shelley.Spec.Ledger.BaseTypes (invalidKey)
import Shelley.Spec.Ledger.Serialization( decodeRecordSum )
import Cardano.Binary
  ( FromCBOR (fromCBOR),
    ToCBOR (toCBOR),
    encodeListLen,
    encodeWord,
  )
import Codec.CBOR.Read(DeserialiseFailure,deserialiseFromBytes)
import Codec.CBOR.Write (toLazyByteString)

import Test.Tasty
import Test.Tasty.QuickCheck hiding (scale)


data TT = A Int | B Int Bool | C [Int] | D (StrictSeq Bool) deriving (Show)

instance FromCBOR TT where
  fromCBOR = decodeRecordSum "TT" $
    \n -> case n of
      0 -> do i <- fromCBOR; pure (2, A i)                  -- Tag for A is 0
      1 -> do i <- fromCBOR; b <- fromCBOR; pure (3, B i b) -- Tag for B is 1
      2 -> do l <- decodeList fromCBOR; pure (2, C l)       -- Tag for C is 2
      3 -> do i <- decodeStrictSeq fromCBOR; pure (2, D i)  -- Tag for D is 3
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
--       2 -> do { l <- decodeList fromCBOR; pure(2,C l) }
--       2 -> do { l <- fromCBOR; pure(2,C l) }
-- BUT THE FIRST IS MORE GENERAL. The following instance should be replaced
-- instance FromCBOR a => FromCBOR [a]  -- Defined in ‘cardano-binary-1.5.0:Cardano.Binary.FromCBOR’

instance ToCBOR TT where
  toCBOR (A i) = encodeListLen 2 <> encodeWord 0 <> toCBOR i
  toCBOR (B i b) = encodeListLen 3 <> encodeWord 1 <> toCBOR i <> toCBOR b
  toCBOR (C is) = encodeListLen 2 <> encodeWord 2 <> toCBOR is
  toCBOR (D bs) = encodeListLen 2 <> encodeWord 3 <> encodeStrictSeq bs

-- The Key is that in (C constr tag <@> ...)
-- The 'tag' for 'constr' aligns with the Tag in the case match
-- in the FromCBOR instance for TT above.

sA, sB, sC, sCa, sD :: Symbolic TT
sA = Con A 0 <@> 7                                            -- Tag for A is 0
sB = Con B 1 <@> 13 <@> True                                  -- Tag for B is 1
sC = Con C 2 <@> [3, 4, 5]                                    -- Tag for C is 2
sCa = Con C 2 <#> (encodeList, [2, 5])                        -- Tag for C is 2
sD = Con D 3 <#> (encodeStrictSeq, fromList [False, True])    -- Tag for D is 3

testSym ::
  (FromCBOR t) =>
  Symbolic t ->
  Either Codec.CBOR.Read.DeserialiseFailure (Lazy.ByteString, t)
testSym s = deserialiseFromBytes fromCBOR (toLazyByteString (encodeSym s))

deCodeTest :: (FromCBOR t, Show t) => Symbolic t -> TestTree
deCodeTest sym = testProperty ("Decoding "++show(runSym sym)) $
                    case testSym sym of
                      Right _ -> True
                      Left s -> error ("Fail to decode "++show(runSym sym)++" with error "++show s)

memoBytesTest :: TestTree
memoBytesTest = testGroup "MemoBytesTest"
    [ deCodeTest sA,
      deCodeTest sB,
      deCodeTest sC,
      deCodeTest sCa,
      deCodeTest sD
    ]