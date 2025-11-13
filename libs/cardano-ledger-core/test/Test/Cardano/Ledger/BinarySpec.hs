{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}

module Test.Cardano.Ledger.BinarySpec (spec) where

import Cardano.Ledger.BaseTypes
import Cardano.Ledger.Binary
import Cardano.Ledger.Coin
import Cardano.Ledger.Compactible
import Cardano.Ledger.DRep (DRep (..), DRepState (..))
import Cardano.Ledger.Hashes (EraIndependentData, SafeHash, ScriptHash)
import Cardano.Ledger.Keys
import Cardano.Ledger.TxIn
import qualified PlutusLedgerApi.V1 as PV1
import Test.Cardano.Ledger.Binary (decoderEquivalenceSpec)
import Test.Cardano.Ledger.Binary.RoundTrip
import Test.Cardano.Ledger.Common (Spec, describe, prop)
import Test.Cardano.Ledger.Core.Arbitrary ()
import Test.Cardano.Ledger.Core.Binary.Annotator ()

spec :: Spec
spec = do
  describe "RoundTrip" $ do
    roundTripCborSpec @Coin
    roundTripCborSpec @(CompactForm Coin)
    prop "Encode CompactCoin - Decode Coin" $
      roundTripExpectation @Coin (mkTrip (encCBOR . compactCoinOrError) decCBOR)
    prop "Encode Coin - Decode CompactCoin" $
      roundTripExpectation @Coin (mkTrip encCBOR (fromCompact <$> decCBOR))
    roundTripCborSpec @ProtVer
    roundTripCborSpec @Nonce
    roundTripCborSpec @Url
    roundTripCborSpec @DnsName
    roundTripCborSpec @Port
    roundTripCborSpec @ActiveSlotCoeff
    roundTripCborSpec @Network
    roundTripCborSpec @BlocksMade
    roundTripCborSpec @TxIx
    roundTripCborSpec @CertIx
    roundTripCborSpec @Anchor
    roundTripAnnCborSpec @BootstrapWitness
    roundTripCborSpec @BootstrapWitness
    roundTripCborSpec @TxId
    roundTripCborSpec @GenDelegPair
    roundTripCborSpec @GenDelegs
    roundTripCborSpec @DRepState
    roundTripCborSpec @DRep
    roundTripCborSpec @ScriptHash
    roundTripCborSpec @(SafeHash EraIndependentData)

  describe "DecCBOR instances equivalence" $ do
    decoderEquivalenceSpec @BootstrapWitness minBound maxBound
    decoderEquivalenceSpec @(WitVKey Witness) minBound maxBound
    decoderEquivalenceSpec @PV1.Data minBound maxBound
