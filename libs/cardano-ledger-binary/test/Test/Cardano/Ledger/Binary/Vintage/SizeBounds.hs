{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

module Test.Cardano.Ledger.Binary.Vintage.SizeBounds (tests) where

import Cardano.Ledger.Binary
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LBS
import qualified Data.Map as M
import Data.Proxy (Proxy (Proxy))
import Data.Tagged (Tagged (..))
import qualified Data.Text as T
import Data.Typeable (typeRep)
import Data.Word (Word32, Word8)
import Hedgehog (Gen, Group (..), checkParallel)
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import Test.Cardano.Ledger.Binary.Vintage.Helpers

tests :: IO Bool
tests =
  let listOf :: Gen a -> Gen [a]
      listOf = Gen.list (Range.linear 0 300)
   in checkParallel $
        Group
          "Encoded size bounds for core types."
          [ ("()", sizeTest $ scfg {gen = pure (), precise = True})
          , ("Bool", sizeTest $ cfg {gen = Gen.bool, precise = True})
          , ("Word", sizeTest $ cfg {gen = Gen.word Range.exponentialBounded})
          , ("Word8", sizeTest $ cfg {gen = Gen.word8 Range.exponentialBounded})
          , ("Word16", sizeTest $ cfg {gen = Gen.word16 Range.exponentialBounded})
          , ("Word32", sizeTest $ cfg {gen = Gen.word32 Range.exponentialBounded})
          , ("Word64", sizeTest $ cfg {gen = Gen.word64 Range.exponentialBounded})
          , ("Int", sizeTest $ cfg {gen = Gen.int Range.exponentialBounded})
          ,
            ( "Int (precision)"
            , sizeTest $
                cfg
                  { gen = Gen.int Range.exponentialBounded
                  , computedCtx = \x ->
                      M.fromList
                        [
                          ( typeRep (Proxy @Int)
                          , SizeConstant $ fromIntegral (withWordSize x :: Integer)
                          )
                        ]
                  , precise = True
                  }
            )
          ,
            ( "Float"
            , sizeTest $
                cfg {gen = Gen.float (Range.exponentialFloat (-1000000) 1000000)}
            )
          , ("Int32", sizeTest $ cfg {gen = Gen.int32 Range.exponentialBounded})
          , ("Int64", sizeTest $ cfg {gen = Gen.int64 Range.exponentialBounded})
          ,
            ( "Tagged () Word32"
            , sizeTest $
                (scfg @(Tagged () Word32))
                  { gen = Tagged <$> Gen.word32 Range.exponentialBounded
                  }
            )
          ,
            ( "(Bool, Bool)"
            , sizeTest $
                scfg {gen = (,) <$> Gen.bool <*> Gen.bool, precise = True}
            )
          ,
            ( "(Bool, Bool, Bool)"
            , sizeTest $
                scfg
                  { gen = (,,) <$> Gen.bool <*> Gen.bool <*> Gen.bool
                  , precise = True
                  }
            )
          ,
            ( "(Bool, Bool, Bool, Bool)"
            , sizeTest $
                scfg
                  { gen = (,,,) <$> Gen.bool <*> Gen.bool <*> Gen.bool <*> Gen.bool
                  , precise = True
                  }
            )
          ,
            ( "ByteString"
            , sizeTest $
                (scfg @BS.ByteString)
                  { debug = show . (BS.unpack :: BS.ByteString -> [Word8])
                  , gen = Gen.bytes (Range.linear 0 1000)
                  , computedCtx = \bs ->
                      M.fromList
                        [
                          ( typeRep (Proxy @(LengthOf BS.ByteString))
                          , SizeConstant $ fromIntegral $ BS.length bs
                          )
                        ]
                  , precise = True
                  }
            )
          ,
            ( "Lazy.ByteString"
            , sizeTest $
                (scfg @LBS.ByteString)
                  { debug = show . (LBS.unpack :: LBS.ByteString -> [Word8])
                  , computedCtx = \bs ->
                      M.fromList
                        [
                          ( typeRep (Proxy @(LengthOf LBS.ByteString))
                          , SizeConstant $ fromIntegral $ LBS.length bs
                          )
                        ]
                  , gen = LBS.fromStrict <$> Gen.bytes (Range.linear 0 1000)
                  , precise = True
                  }
            )
          ,
            ( "Text"
            , sizeTest $
                cfg
                  { gen = Gen.text (Range.linear 0 1000) Gen.latin1
                  , computedCtx = \bs ->
                      M.fromList
                        [
                          ( typeRep (Proxy @(LengthOf T.Text))
                          , SizeConstant $ fromIntegral $ T.length bs
                          )
                        ]
                  }
            )
          ,
            ( "Text 2"
            , sizeTest $
                cfg
                  { gen = Gen.text (Range.linear 0 1000) Gen.unicode
                  , computedCtx = \bs ->
                      M.fromList
                        [
                          ( typeRep (Proxy @(LengthOf T.Text))
                          , SizeConstant $ fromIntegral $ T.length bs
                          )
                        ]
                  }
            )
          ,
            ( "[Bool]"
            , sizeTest $
                scfg
                  { gen = listOf Gen.bool
                  , computedCtx = \bs ->
                      M.fromList
                        [
                          ( typeRep (Proxy @(LengthOf [Bool]))
                          , SizeConstant $ fromIntegral $ length bs
                          )
                        ]
                  , precise = True
                  }
            )
          ,
            ( "NonEmpty Bool"
            , sizeTest $
                scfg
                  { gen = listOf Gen.bool
                  , computedCtx = \bs ->
                      M.fromList
                        [
                          ( typeRep (Proxy @(LengthOf [Bool]))
                          , SizeConstant $ fromIntegral $ length bs
                          )
                        ]
                  , precise = True
                  }
            )
          ,
            ( "Either Bool Bool"
            , sizeTest $
                (scfg @(Either Bool Bool))
                  { gen = Left <$> Gen.bool
                  , precise = True
                  }
            )
          ,
            ( "Either Bool Bool"
            , sizeTest $
                (scfg @(Either Bool Bool))
                  { gen = Right <$> Gen.bool
                  , precise = True
                  }
            )
          , ("Maybe Bool", sizeTest $ cfg {gen = Gen.bool, precise = True})
          ]
