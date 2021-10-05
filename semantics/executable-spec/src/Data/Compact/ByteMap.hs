

module ByteMap where


import Data.ByteString.Short (ShortByteString,toShort,fromShort,pack,unpack)
import qualified Data.ByteString.Short as SBS
import Data.ByteString(ByteString)
import Data.Foldable (foldr',foldl')
import Cardano.Prelude (HeapWords (..),runST)
import Data.Word(Word8)
import qualified Data.Primitive.Array as PA
import Data.Bits ((.&.), (.|.), complement, popCount, unsafeShiftL, unsafeShiftR,shiftL, shiftR)
import qualified Data.Text as Text
import Data.Text.Encoding(encodeUtf8)
import Data.Compact.Class
-- ===================================================

w1:: Word8
w1 = 196

bin x = reverse (binary x)

bytes :: String -> ShortByteString
bytes str = toShort $ encodeUtf8 $ Text.pack str


-------------------------------------------------------------

depth, bits, size :: Int
depth = 4
bits = 4
size = 2 ^ bits
indices = [0,1] -- [0.. size -1]


lo4bits, hi4bits :: Word8
lo4bits = 15                -- 00001111
hi4bits = shiftL lo4bits 4  -- 11110000

split4 :: Word8 -> (Word8,Word8)
split4 w = (shiftR w 4, w .&. lo4bits)

combine4 :: (Word8,Word8) -> Word8
combine4 (hi,lo) = hi * 16 + lo

-- | Split a ByteString into two parts. The first is a [Word8]
--   its length is depth, and each Word8 is in the range (0.. 2^bits)
--   The second part is the remainder of the byte string
split :: ShortByteString -> ([Int],ShortByteString)
split bs | SBS.length bs >= 3 =
   let (w1 : w2 : more) = unpack bs
       (x1,x2) = split4 w1
       (x3,x4) = split4 w2
   in (map fromIntegral [x1,x2,x3,x4],pack more)
split bs = error ("Byte string is too short to split with a prefix of length "++show depth)

data ByteMap v = ByteMap (PA.Array (PA.Array (PA.Array (PA.Array [(ShortByteString,v)]))))
  deriving Show

update :: PA.Array t -> Int -> t -> PA.Array t
update arr i t | i<0 || i >= (isize arr) = error ("index out of bounds in update "++show i)
update arr i t = fst(withMutArray (isize arr) action)
  where action marr = do
          mcopy marr 0 arr 0 i
          mwrite marr i t
          mcopy marr (i+1) arr (i+1) (isize arr - 1)

arrayOf :: Int -> a -> PA.Array a
arrayOf n a =  runST $ do
  marr <- PA.newArray n a
  arr <- mfreeze marr
  pure arr

insert:: ShortByteString -> v -> ByteMap v -> ByteMap v
insert bs v (ByteMap arr1) = ByteMap $
                             update arr1 x1 $
                             update arr2 x2 $
                             update arr3 x3 $
                             update arr4 x4 ((more,v):pairs)
   where ([x1,x2,x3,x4],more) = split bs
         arr2 = index arr1 x1
         arr3 = index arr2 x2
         arr4 = index arr3 x3
         pairs = index arr4 x4



emptyByteMap :: ByteMap v
emptyByteMap = ByteMap arr4
   where arr1 = arrayOf 16 []
         arr2 = arrayOf 16 arr1
         arr3 = arrayOf 16 arr2
         arr4 = arrayOf 16 arr3

m1@(ByteMap a1) = insert (bytes "ABCDE") True emptyByteMap
a2 = index a1 4
a3 = index a2 1
a4 = index a3 4