{-# LANGUAGE NamedFieldPuns #-}

module Test.Cardano.Ledger.Shelley.Generator.Metadata
  ( genMetadata,
    genMetadata',
  )
where

import Cardano.Ledger.BaseTypes
  ( StrictMaybe (..),
  )
import Cardano.Ledger.Shelley.Metadata
  ( Metadata (..),
    Metadatum (..),
  )
import Control.Exception (assert)
import qualified Data.ByteString.Char8 as BS (length, pack)
import qualified Data.Map.Strict as Map
import qualified Data.Text as T (pack)
import qualified Data.Text.Encoding as T
import Data.Word (Word64)
import Test.Cardano.Ledger.Shelley.Generator.Constants (Constants (..))
import Test.QuickCheck (Gen)
import qualified Test.QuickCheck as QC

-- | Max size of generated Metadatum List and Map
collectionDatumMaxSize :: Int
collectionDatumMaxSize = 5

-- | Max size of generated Metadata map
metadataMaxSize :: Int
metadataMaxSize = 3

-- | Generate Metadata (and compute hash) with frequency 'frequencyTxWithMetadata'
genMetadata :: Constants -> Gen (StrictMaybe (Metadata era))
genMetadata (Constants {frequencyTxWithMetadata}) =
  QC.frequency
    [ (frequencyTxWithMetadata, SJust <$> genMetadata'),
      (100 - frequencyTxWithMetadata, pure SNothing)
    ]

-- | Generate Metadata (and compute hash) of size up to 'metadataMaxSize'
genMetadata' :: Gen (Metadata era)
genMetadata' = do
  n <- QC.choose (1, metadataMaxSize)
  Metadata . Map.fromList
    <$> QC.vectorOf n genMetadatum

-- | Generate one of the Metadatum
genMetadatum :: Gen (Word64, Metadatum)
genMetadatum = do
  (,) <$> QC.arbitrary
    <*> ( QC.oneof
            [ genDatumInt,
              genDatumString,
              genDatumBytestring,
              genMetadatumList,
              genMetadatumMap
            ]
        )

genDatumInt :: Gen Metadatum
genDatumInt =
  I
    <$> QC.frequency
      [ (8, QC.choose (minVal, maxVal)),
        (1, pure minVal),
        (1, pure maxVal)
      ]
  where
    minVal, maxVal :: Integer
    minVal = -maxVal
    maxVal = fromIntegral (maxBound :: Word64)

genDatumString :: Gen Metadatum
genDatumString =
  QC.sized $ \sz -> do
    n <- QC.choose (0, min sz 64)
    cs <- genUtf8StringOfSize n
    let s = T.pack cs
    assert (BS.length (T.encodeUtf8 s) == n) $
      return (S s)

-- | Produce an arbitrary Unicode string such that it's UTF8 encoding size in
-- bytes is exactly the given length.
genUtf8StringOfSize :: Int -> Gen [Char]
genUtf8StringOfSize 0 = return []
genUtf8StringOfSize n = do
  cz <- QC.choose (1, min n 4)
  c <- case cz of
    1 -> QC.choose ('\x00000', '\x00007f')
    2 -> QC.choose ('\x00080', '\x0007ff')
    3 ->
      QC.oneof
        [ QC.choose ('\x00800', '\x00d7ff'),
          -- skipping UTF-16 surrogates d800--dfff
          QC.choose ('\x0e000', '\x00ffff')
        ]
    _ -> QC.choose ('\x10000', '\x10ffff')
  cs <- genUtf8StringOfSize (n - cz)
  return (c : cs)

genDatumBytestring :: Gen Metadatum
genDatumBytestring =
  QC.sized $ \sz -> do
    n <- QC.choose (0, min sz 64)
    B . BS.pack <$> QC.vectorOf n QC.arbitrary

-- | Generate a 'MD.List [Metadatum]'
--
-- Note: to limit generated metadata size, impact on transaction fees and
-- cost of hashing, we generate only lists of "simple" Datums, not lists
-- of list or map Datum.
genMetadatumList :: Gen Metadatum
genMetadatumList = List <$> vectorOfMetadatumSimple

-- | Generate a 'MD.Map ('[(Metadatum, Metadatum)]')
genMetadatumMap :: Gen Metadatum
genMetadatumMap =
  Map <$> (zip <$> vectorOfMetadatumSimple <*> vectorOfMetadatumSimple)

vectorOfMetadatumSimple :: Gen [Metadatum]
vectorOfMetadatumSimple = do
  n <- QC.choose (1, collectionDatumMaxSize)
  QC.vectorOf
    n
    ( QC.oneof
        [ genDatumInt,
          genDatumString,
          genDatumBytestring
        ]
    )
