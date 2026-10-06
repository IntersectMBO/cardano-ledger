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
import Cardano.Ledger.Core (HASH, Hash)
import Cardano.Ledger.Keys (DSIGN, VKey (..))
import Data.ByteString (ByteString)
import Data.Either (isRight)
import Data.Maybe (fromMaybe)
import qualified MAlonzo.Code.Ledger.Core.Foreign.API as Agda
import Test.Cardano.Ledger.Conformance.SpecTranslate.Core (
  popFromInteger,
  signatureFromInteger,
  verkeyFromInteger96,
  vkeyFromInteger,
 )
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

    extIsValidPoP vk pop =
      isRight $
        verifyPossessionProofDSIGN minSigPoPDST verkey proofofpossesion
      where
        verkey :: VerKeyDSIGN BLS12381MinSigDSIGN
        verkey =
          fromMaybe (error "Failed to convert an Agda VerKey to a Haskell VerKey")
            . verkeyFromInteger96
            $ vk

        proofofpossesion :: PossessionProofDSIGN BLS12381MinSigDSIGN
        proofofpossesion =
          fromMaybe (error "Failed to decode the PoP")
            . popFromInteger
            $ pop

    extValidPlutusScript = True
