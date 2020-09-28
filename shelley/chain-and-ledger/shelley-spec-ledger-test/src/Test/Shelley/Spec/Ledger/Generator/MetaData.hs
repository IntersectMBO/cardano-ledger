{-# LANGUAGE NamedFieldPuns #-}

module Test.Shelley.Spec.Ledger.Generator.MetaData
  ( genMetaData,
  )
where

import qualified Data.ByteString.Char8 as BS (pack, length)
import qualified Data.Map.Strict as Map
import qualified Data.Text as T (pack)
import qualified Data.Text.Encoding as T
import Data.Word (Word64)
import Shelley.Spec.Ledger.BaseTypes
  ( StrictMaybe (..),
  )

import Cardano.Ledger.Era (Era)
import Shelley.Spec.Ledger.MetaData (MetaData (..), MetaDataHash, MetaDatum (..), hashMetaData)
import Test.QuickCheck (Gen)
import qualified Test.QuickCheck as QC
import Test.Shelley.Spec.Ledger.Generator.Constants (Constants (..))

-- | Max size of generated MetaDatum List and Map
collectionDatumMaxSize :: Int
collectionDatumMaxSize = 5

-- | Max size of generated MetaData map
metadataMaxSize :: Int
metadataMaxSize = 3

-- | Generate Metadata (and compute hash) with frequency 'frequencyTxWithMetaData'
genMetaData ::
  Era era =>
  Constants ->
  Gen (StrictMaybe MetaData, StrictMaybe (MetaDataHash era))
genMetaData (Constants {frequencyTxWithMetaData}) =
  QC.frequency
    [ (frequencyTxWithMetaData, genMetaData'),
      (100 - frequencyTxWithMetaData, pure (SNothing, SNothing))
    ]

-- | Generate Metadata (and compute hash) of size up to 'metadataMaxSize'
genMetaData' ::
  Era era =>
  Gen (StrictMaybe MetaData, StrictMaybe (MetaDataHash era))
genMetaData' = do
  n <- QC.choose (1, metadataMaxSize)
  md <-
    SJust . MetaData . Map.fromList
      <$> QC.vectorOf n genMetaDatum
  pure (md, hashMetaData <$> md)

-- | Generate one of the MetaDatum
genMetaDatum :: Gen (Word64, MetaDatum)
genMetaDatum = do
  (,) <$> QC.arbitrary
    <*> ( QC.oneof
            [ genDatumInt,
              genDatumString,
              genDatumBytestring,
              genMetaDatumList,
              genMetaDatumMap
            ]
        )

genDatumInt :: Gen MetaDatum
genDatumInt = I <$> QC.frequency [ (8, QC.choose (minVal, maxVal))
                                 , (1, pure minVal)
                                 , (1, pure maxVal) ]
  where
    minVal, maxVal :: Integer
    minVal = -maxVal
    maxVal = fromIntegral (maxBound :: Word64)

genDatumString :: Gen MetaDatum
genDatumString =
    fmap S $
    QC.sized $ \sz -> do
      n <- QC.choose (0, min sz 64)
      fmap T.pack (QC.vectorOf n QC.arbitrary) `QC.suchThat` withinRange
  where
    withinRange s = BS.length (T.encodeUtf8 s) <= 64

genDatumBytestring :: Gen MetaDatum
genDatumBytestring =
    QC.sized $ \sz -> do
      n <- QC.choose (0, min sz 64)
      B . BS.pack <$> QC.vectorOf n QC.arbitrary

-- | Generate a 'MD.List [MetaDatum]'
--
-- Note: to limit generated metadata size, impact on transaction fees and
-- cost of hashing, we generate only lists of "simple" Datums, not lists
-- of list or map Datum.
genMetaDatumList :: Gen MetaDatum
genMetaDatumList = List <$> vectorOfMetaDatumSimple

-- | Generate a 'MD.Map ('[(MetaDatum, MetaDatum)]')
genMetaDatumMap :: Gen MetaDatum
genMetaDatumMap =
  Map <$> (zip <$> vectorOfMetaDatumSimple <*> vectorOfMetaDatumSimple)

vectorOfMetaDatumSimple :: Gen [MetaDatum]
vectorOfMetaDatumSimple = do
  n <- QC.choose (1, collectionDatumMaxSize)
  QC.vectorOf
    n
    ( QC.oneof
        [ genDatumInt,
          genDatumString,
          genDatumBytestring
        ]
    )
