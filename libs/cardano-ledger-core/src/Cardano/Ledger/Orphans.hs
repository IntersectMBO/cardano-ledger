{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Cardano.Ledger.Orphans where

import Cardano.Crypto.Hash (Hash (..))
import qualified Cardano.Crypto.Hash as Hash
import qualified Cardano.Crypto.Hash.Class as HS
import Cardano.Crypto.Util (SignableRepresentation (..))
import qualified Cardano.Crypto.Wallet as WC
import qualified Data.ByteString as Long (ByteString, empty)
import qualified Data.ByteString.Lazy as Lazy (ByteString, empty)
import qualified Data.ByteString.Short as Short (ShortByteString, empty, pack)
import Data.Default (Default (..))
import Data.Fixed (Fixed (..))
import Data.Proxy
import qualified Data.Sequence.Strict as SS
import NoThunks.Class (NoThunks (..))

instance NoThunks WC.XSignature where
  wNoThunks ctxt s = wNoThunks ctxt (WC.unXSignature s)
  showTypeOf _proxy = "XSignature"

instance SignableRepresentation (Hash.Hash a b) where
  getSignableRepresentation = Hash.hashToBytes

-- | TODO: We should upstream instance
-- HasResolution p => NoThunks (Fixed p) into the nothunks package.
deriving newtype instance NoThunks (Fixed p)

-- ===============================================
-- Blank instance needed to compute Provenance

instance Default (SS.StrictSeq t) where
  def = SS.Empty

instance Default Short.ShortByteString where
  def = Short.empty

instance Default Long.ByteString where
  def = Long.empty

instance Default Lazy.ByteString where
  def = Lazy.empty

instance HS.HashAlgorithm h => Default (Hash h b) where
  def =
    UnsafeHash $
      Short.pack $
        replicate (fromIntegral (Hash.sizeHash (Proxy :: Proxy h))) 0
