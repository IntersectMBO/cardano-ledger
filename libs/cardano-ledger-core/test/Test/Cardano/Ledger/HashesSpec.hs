{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeApplications #-}

-- | Check agreement and lengths of the strict and short byte accessors for their
-- defaults and representative instances, including preservation of memoized
-- non-canonical CBOR bytes.
module Test.Cardano.Ledger.HashesSpec (spec) where

import qualified Cardano.Crypto.Hash as Hash
import Cardano.Ledger.Binary (decodeFull', serialize')
import Cardano.Ledger.Hashes (HASH, SafeToHash (..))
import Cardano.Ledger.MemoBytes (MemoBytes (Memo), getMemoBytesType)
import Cardano.Ledger.Plutus.Language (PlutusBinary (..))
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Short as SBS
import Data.Word (Word64)
import Test.Cardano.Ledger.Common

-- Exercise each accessor default with the other accessor explicitly implemented.
newtype OriginalBytesOnly = OriginalBytesOnly ByteString

instance SafeToHash OriginalBytesOnly where
  originalBytes (OriginalBytesOnly bytes) = bytes

newtype OriginalShortBytesOnly = OriginalShortBytesOnly SBS.ShortByteString

instance SafeToHash OriginalShortBytesOnly where
  originalShortBytes (OriginalShortBytesOnly bytes) = bytes

newtype MemoWrapper = MemoWrapper (MemoBytes ())
  deriving newtype (SafeToHash)

spec :: Spec
spec = describe "SafeToHash byte accessors" $ do
  originalShortBytesSpec "Default originalShortBytes" OriginalBytesOnly
  originalShortBytesSpec "Default originalBytes" (OriginalShortBytesOnly . SBS.toShort)
  originalShortBytesSpec "ByteString" id
  originalShortBytesSpec "ShortByteString" SBS.toShort
  originalShortBytesSpec "Hash" (Hash.hashWith @HASH id)
  originalShortBytesSpec "MemoBytes" (Memo () . SBS.toShort)
  originalShortBytesSpec "Derived instance" (MemoWrapper . Memo () . SBS.toShort)
  originalShortBytesSpec "PlutusBinary" (PlutusBinary . SBS.toShort)
  it "Preserves non-canonical CBOR bytes" $ do
    let bytes = BS.pack [0x18, 0x00]
    memo <- expectRight $ decodeFull' @(MemoBytes Word64) minBound bytes
    getMemoBytesType memo `shouldBe` 0
    serialize' minBound (getMemoBytesType memo) `shouldNotBe` bytes
    originalShortBytes memo `shouldBe` SBS.toShort bytes

originalShortBytesSpec :: SafeToHash t => String -> (ByteString -> t) -> Spec
originalShortBytesSpec name fromBytes = describe name $ do
  prop "Short and strict accessors preserve the same bytes" $ \bytes -> do
    let value = fromBytes $ BS.pack bytes
        shortBytes = originalShortBytes value
    SBS.fromShort shortBytes `shouldBe` originalBytes value
    SBS.length shortBytes `shouldBe` originalBytesSize value
