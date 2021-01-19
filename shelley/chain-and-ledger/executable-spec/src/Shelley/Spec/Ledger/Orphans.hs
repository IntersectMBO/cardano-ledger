{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Shelley.Spec.Ledger.Orphans where

import Cardano.Crypto.Hash (Hash (..))
import qualified Cardano.Crypto.Hash as Hash
import qualified Cardano.Crypto.Hash.Class as HS
import Cardano.Crypto.Util (SignableRepresentation (..))
import qualified Cardano.Crypto.Wallet as WC
import Cardano.Prelude (HeapWords (..), readEither)
import Cardano.Slotting.Slot (WithOrigin (..))
import Control.DeepSeq (NFData (rnf))
import Data.Aeson
import qualified Data.ByteString as Long (ByteString, empty)
import qualified Data.ByteString.Lazy as Lazy (ByteString, empty)
import qualified Data.ByteString.Short as Short (ShortByteString, empty)
import Data.Default.Class (Default (..))
import Data.Foldable
import Data.IP (IPv4, IPv6)
import Data.Sequence.Strict (StrictSeq, fromList, fromStrict)
import qualified Data.Sequence.Strict as SS
import qualified Data.Text as Text
import NoThunks.Class (NoThunks (..))
import Shelley.Spec.Ledger.BaseTypes (Network (..), StrictMaybe (..), UnitInterval, interval0)
import Shelley.Spec.Ledger.Keys (KeyHash (..))
import Shelley.Spec.Ledger.Slot (BlockNo, EpochNo)

instance FromJSON IPv4 where
  parseJSON =
    withText "IPv4" $ \txt -> case readEither (Text.unpack txt) of
      Right ipv4 -> return ipv4
      Left _ -> fail $ "failed to read as IPv4 " ++ show txt

instance ToJSON IPv4 where
  toJSON = toJSON . show

instance FromJSON IPv6 where
  parseJSON =
    withText "IPv6" $ \txt -> case readEither (Text.unpack txt) of
      Right ipv6 -> return ipv6
      Left _ -> fail $ "failed to read as IPv6 " ++ show txt

instance ToJSON IPv6 where
  toJSON = toJSON . show

instance FromJSON a => FromJSON (StrictSeq a) where
  parseJSON = fmap fromList . parseJSON

instance ToJSON a => ToJSON (StrictSeq a) where
  toJSON = toJSON . toList

instance NoThunks IPv4

instance NoThunks IPv6

instance NFData IPv4

instance NFData IPv6

{- The following NFData instances probably belong in base -}
instance NFData EpochNo

instance NFData (StrictSeq a) where
  rnf x = case fromStrict x of _any -> ()

-- By defintion it is strict, so as long as the (hidden) constructor is evident, it is in normal form

instance NFData a => NFData (WithOrigin a)

instance NFData BlockNo

instance NoThunks WC.XSignature where
  wNoThunks ctxt s = wNoThunks ctxt (WC.unXSignature s)
  showTypeOf _proxy = "XSignature"

instance SignableRepresentation (Hash.Hash a b) where
  getSignableRepresentation = Hash.hashToBytes

-- ===============================================
-- Blank instance needed to compute Provenance

instance Default UnitInterval where
  def = interval0

instance Default Network where
  def = Mainnet

instance Default (KeyHash a b) where
  def = KeyHash def

instance Default (SS.StrictSeq t) where
  def = SS.Empty

instance Default (StrictMaybe t) where
  def = SNothing

instance Default Short.ShortByteString where
  def = Short.empty

instance Default Long.ByteString where
  def = Long.empty

instance Default Lazy.ByteString where
  def = Lazy.empty

instance Default (Hash a b) where
  def = (UnsafeHash def)

instance Default Bool where
  def = False

deriving newtype instance HeapWords (HS.Hash h a)
