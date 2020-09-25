{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Test.Shelley.Spec.Ledger.Serialisation.StakeRef where

import qualified Data.ByteString.Short as SBS
import Shelley.Spec.Ledger.Address (Addr (..), deserialiseAddrStakeRef, serialiseAddr)
import qualified Shelley.Spec.Ledger.DeserializeShort as Short
import Test.Shelley.Spec.Ledger.Serialisation.Generators ()
import Cardano.Ledger.Era (Era(..))

propDeserializeAddrStakeReference :: forall era. Era era => Addr era -> Bool
propDeserializeAddrStakeReference addr =
  let bytes = serialiseAddr addr
   in case addr of
        AddrBootstrap _ -> deserialiseAddrStakeRef @era bytes == Nothing
        Addr _ _ stakeRef -> deserialiseAddrStakeRef @era bytes == Just stakeRef

propDeserializeAddrStakeReferenceShort::  forall era. Era era => Addr era -> Bool
propDeserializeAddrStakeReferenceShort addr =
  let bytes = serialiseAddr addr
      sbs = SBS.toShort bytes
   in deserialiseAddrStakeRef @era bytes == Short.deserialiseAddrStakeRef @era sbs
