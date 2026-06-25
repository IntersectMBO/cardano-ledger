{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Test.Cardano.Ledger.Allegra.Binary.Golden (
  spec,
  allegraDecodeDuplicateDelegCertSucceeds,
  module Test.Cardano.Ledger.Shelley.Binary.Golden,
) where

import Cardano.Ledger.Allegra.Core (
  AllegraEraTxBody (..),
  ShelleyEraTxCert,
  ValidityInterval (..),
  pattern DelegStakeTxCert,
 )
import Cardano.Ledger.Binary (Version)
import Cardano.Ledger.Credential (Credential (..))
import Cardano.Ledger.MemoBytes (EqRaw (..))
import Cardano.Ledger.Shelley.Core (EraTxBody (..), TxLevel (..))
import Data.Maybe.Strict (StrictMaybe (..))
import qualified Data.Sequence.Strict as SSeq
import Lens.Micro ((&), (.~))
import Test.Cardano.Ledger.Allegra.Era (AllegraEraTest)
import Test.Cardano.Ledger.Common (Spec, describe, it, prop)
import Test.Cardano.Ledger.Core.KeyPair (mkKeyHash)
import Test.Cardano.Ledger.Imp.Common (forEachEraVersion)
import Test.Cardano.Ledger.Shelley.Binary.Golden

allegraDecodeDuplicateDelegCertSucceeds ::
  forall era. (AllegraEraTest era, ShelleyEraTxCert era) => Version -> Spec
allegraDecodeDuplicateDelegCertSucceeds version =
  it "Decodes duplicate delegation certificates successfully" $ do
    let testCert = DelegStakeTxCert @era (KeyHashObj $ mkKeyHash 0) (mkKeyHash 1)
    expectDecoderSuccessAnnWith eqRaw version (duplicateDelegCertsTxBody @era version) $
      mkBasicTxBody @era @TopTx
        & certsTxBodyL .~ SSeq.fromList [testCert, testCert]
        & vldtTxBodyL .~ ValidityInterval SNothing (SJust 300)

spec ::
  forall era.
  ( AllegraEraTest era
  , ShelleyEraTxCert era
  ) =>
  Spec
spec =
  describe "Golden" $ do
    prop "NewEpochState" $ goldenNewEpochStateExpectation @era
    describe "TxCerts" $ do
      forEachEraVersion @era $ allegraDecodeDuplicateDelegCertSucceeds @era
