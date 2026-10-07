{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Test.Cardano.Ledger.Conformance.ExecSpecRule.Base (
  externalFunctions,
) where

import Cardano.Crypto.DSIGN (
  BLS12381MinSigDSIGN,
  PossessionProofDSIGN,
  SignedDSIGN (..),
  VerKeyDSIGN,
  verifyPossessionProofDSIGN,
  verifySignedDSIGN,
 )
import Cardano.Crypto.DSIGN.BLS12381.Internal (minSigPoPDST)
import Cardano.Crypto.Util (naturalToBytes)
import Cardano.Ledger.Binary (FixedSizeCodec (..), fixedSize)
import Cardano.Ledger.Core (HASH, Hash)
import Cardano.Ledger.Keys (DSIGN, VKey (..))
import Data.ByteString (ByteString)
import Data.Either (isRight)
import Data.Maybe (fromMaybe)
import Data.Proxy (Proxy (..))
import qualified MAlonzo.Code.Ledger.Core.Foreign.API as Agda
import Test.Cardano.Ledger.Conformance.SpecTranslate.Core (signatureFromInteger, vkeyFromInteger)
import Test.Cardano.Ledger.Conformance.Utils (integerToHash)

externalFunctions :: Agda.ExternalFunctions
externalFunctions = Agda.MkExternalFunctions {..}
  where
    extIsSigned vk ser sig =
      isRight $
        verifySignedDSIGN
          @DSIGN
          @(Hash HASH ByteString)
          ()
          vkey
          hash
          signature
      where
        vkey =
          unVKey
            . fromMaybe (error "Failed to convert an Agda VKey to a Haskell VKey")
            $ vkeyFromInteger vk
        hash =
          fromMaybe
            (error $ "Failed to get hash from integer:\n" <> show ser)
            $ integerToHash ser
        signature =
          SignedDSIGN
            . fromMaybe
              (error "Failed to decode the signature")
            $ signatureFromInteger sig

    extIsValidPoP vk proof =
      case ( decodeInteger @(VerKeyDSIGN BLS12381MinSigDSIGN) vk
           , decodeInteger @(PossessionProofDSIGN BLS12381MinSigDSIGN) proof
           ) of
        (Just key, Just pop) -> isRight (verifyPossessionProofDSIGN minSigPoPDST key pop)
        _ -> False

    decodeInteger :: forall a. FixedSizeCodec a => Integer -> Maybe a
    decodeInteger n
      | n < 0 || n >= 256 ^ fixedSize (Proxy @a) = Nothing
      | otherwise =
          rawDecodeFixedSized (naturalToBytes (fromIntegral (fixedSize (Proxy @a))) (fromInteger n))

    extValidPlutusScript = True
