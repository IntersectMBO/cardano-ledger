{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Test.Shelley.Spec.Ledger.NonTraceProperties.Serialization where

import qualified Data.ByteString.Short as SBS
import Shelley.Spec.Ledger.Address (Addr (..), deserialiseAddrStakeRef, serialiseAddr)
import qualified Shelley.Spec.Ledger.DeserializeShort as Short
import Test.Shelley.Spec.Ledger.ConcreteCryptoTypes (C)
import Test.Shelley.Spec.Ledger.Serialisation.Generators ()

propDeserializeAddrStakeReference :: Addr C -> Bool
propDeserializeAddrStakeReference addr =
  let bytes = serialiseAddr addr
   in case addr of
        AddrBootstrap _ -> deserialiseAddrStakeRef @C bytes == Nothing
        Addr _ _ stakeRef -> deserialiseAddrStakeRef @C bytes == Just stakeRef

propDeserializeAddrStakeReferenceShortIncrediblyLongName :: Addr C -> Bool
propDeserializeAddrStakeReferenceShortIncrediblyLongName addr =
  let bytes = serialiseAddr addr
      sbs = SBS.toShort bytes
   in deserialiseAddrStakeRef @C bytes == Short.deserialiseAddrStakeRef @C sbs
