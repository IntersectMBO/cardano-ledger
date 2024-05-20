{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

module Test.Cardano.Ledger.Allegra.Imp.UtxowSpec (
  spec,
) where

import Cardano.Ledger.BaseTypes
import Cardano.Ledger.Core
import Cardano.Ledger.Shelley.Rules (ShelleyUtxowPredFailure (..))
import Cardano.Ledger.Shelley.TxAuxData (Metadatum (..))
import qualified Data.ByteString.Char8 as BS
import qualified Data.Map as Map
import qualified Data.Text as T
import Data.Word (Word64)
import Lens.Micro
import Test.Cardano.Ledger.Imp.Common
import Test.Cardano.Ledger.Shelley.Arbitrary (genUtf8StringOfSize)
import Test.Cardano.Ledger.Shelley.ImpTest

spec ::
  forall era.
  ( ShelleyEraImp era
  , InjectRuleFailure "LEDGER" ShelleyUtxowPredFailure era
  ) =>
  SpecWith (ImpTestState era)
spec = describe "UTXOW" $ do
  it "InvalidMetadata" $ do
    invalidMetadatum <- genInvalidMetadata
    let auxData = mkBasicTxAuxData & metadataTxAuxDataL .~ invalidMetadatum
    let auxDataHash = hashTxAuxData auxData
    let tx =
          mkBasicTx mkBasicTxBody
            & bodyTxL . auxDataHashTxBodyL .~ SJust auxDataHash
            & auxDataTxL .~ SJust auxData
    submitFailingTx tx [injectFailure InvalidMetadata]

genInvalidMetadata :: ImpTestM era (Map.Map Word64 Metadatum)
genInvalidMetadata = do
  size <- choose (65, 1000)
  let genM =
        oneof
          [ B . BS.pack <$> vectorOf size arbitrary
          , S . T.pack <$> liftGen (genUtf8StringOfSize size)
          ]
  Map.fromList <$> listOf1 ((,) <$> arbitrary <*> genM)
