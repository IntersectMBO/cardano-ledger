{-# LANGUAGE NamedFieldPuns #-}

module Test.Shelley.Spec.Ledger.Generator.MetaData
  ( genMetaData,
  )
where

import qualified Data.ByteString.Char8 as BS (pack)
import qualified Data.Map.Strict as Map
import qualified Data.Text as T (pack)
import Data.Word (Word64)
import Shelley.Spec.Ledger.BaseTypes
  ( StrictMaybe (..),
  )
import Shelley.Spec.Ledger.Crypto (Crypto)
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
  Crypto c =>
  Constants ->
  Gen (StrictMaybe MetaData, StrictMaybe (MetaDataHash c))
genMetaData (Constants {frequencyTxWithMetaData}) =
  QC.frequency
    [ (frequencyTxWithMetaData, genMetaData'),
      (100 - frequencyTxWithMetaData, pure (SNothing, SNothing))
    ]

-- | Generate Metadata (and compute hash) of size up to 'metadataMaxSize'
genMetaData' ::
  Crypto c =>
  Gen (StrictMaybe MetaData, StrictMaybe (MetaDataHash c))
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
genDatumInt = I <$> QC.arbitrary

genDatumString :: Gen MetaDatum
genDatumString = S . T.pack <$> QC.arbitrary

genDatumBytestring :: Gen MetaDatum
genDatumBytestring = B . BS.pack <$> QC.arbitrary

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
