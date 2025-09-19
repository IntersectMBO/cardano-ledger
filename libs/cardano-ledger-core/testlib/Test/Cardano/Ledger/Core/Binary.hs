{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

module Test.Cardano.Ledger.Core.Binary (
  decoderEquivalenceSpec,
  decoderEquivalenceEraSpec,
  txSizeSpec,
  decoderEquivalenceCoreEraTypesSpec,
  Mem,
) where

import Cardano.Ledger.Core
import Cardano.Ledger.MemoBytes (Mem)
import Lens.Micro
import Test.Cardano.Ledger.Binary (decoderEquivalenceSpec)
import Test.Cardano.Ledger.Common
import Test.Cardano.Ledger.Core.Arbitrary ()
import Test.Cardano.Ledger.Core.Binary.Annotator

txSizeSpec ::
  forall era.
  ( EraTx era
  , Arbitrary (Tx TopTx era)
  , SafeToHash (TxWits era)
  ) =>
  Spec
txSizeSpec =
  describe "Transaction size" $ do
    prop "should match the size of the cbor encoding" $ \(tx :: Tx TopTx era) -> do
      let txSize = sizeTxForFeeCalculation tx
      txSize `shouldBe` tx ^. sizeTxF
