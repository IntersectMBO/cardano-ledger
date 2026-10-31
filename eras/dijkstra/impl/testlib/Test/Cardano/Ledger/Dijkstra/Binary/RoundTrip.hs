{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Test.Cardano.Ledger.Dijkstra.Binary.RoundTrip (
  roundTripDijkstraTxSpec,
) where

import Cardano.Ledger.Alonzo.Tx (AlonzoEraTx (..), IsPhase2Valid (..))
import Cardano.Ledger.Binary (Tokens (..), decodeFull, serialize')
import qualified Cardano.Ledger.Binary.Plain as Plain
import Cardano.Ledger.Core (
  EraTx (..),
  EraTxBody (mkBasicTxBody),
  SafeToHash (..),
  TxLevel (..),
  eraProtVerHigh,
 )
import Cardano.Ledger.Dijkstra (DijkstraEra)
import Cardano.Ledger.Hashes (hashAnnotated)
import Cardano.Ledger.MempoolTx.Internal (MempoolTx (..), unwrapMempoolTx)
import Data.Either (isLeft)
import Lens.Micro ((&), (.~), (^.))
import Test.Cardano.Ledger.Binary.Golden (decodeEnc)
import Test.Cardano.Ledger.Binary.Plain.Golden (Enc (..))
import Test.Cardano.Ledger.Binary.RoundTrip (embedTripAnn)
import Test.Cardano.Ledger.Common
import Test.Cardano.Ledger.Core.Binary.RoundTrip (
  roundTripAnnEraExpectation,
  roundTripEraExpectation,
 )
import Test.Cardano.Ledger.Dijkstra.Arbitrary ()
import Test.Cardano.Ledger.Dijkstra.Binary.Annotator ()

-- | Round-trip tests for the two serializations of a top-level transaction: the block format
-- of `Tx`, which carries the `IsPhase2Valid` flag, and the mempool format of `MempoolTx`.
roundTripDijkstraTxSpec :: Spec
roundTripDijkstraTxSpec =
  describe "Tx TopTx" $ do
    prop "Block format with any IsPhase2Valid flag" $
      \(tx :: Tx TopTx DijkstraEra) ->
        conjoin
          [ roundTripEraExpectation @DijkstraEra tx
          , roundTripAnnEraExpectation @DijkstraEra tx
          ]
    prop "Mempool round trip preserves the reconstructed canonical block hash" $
      \(mempoolTx :: MempoolTx DijkstraEra) ->
        case embedTripAnn (eraProtVerHigh @DijkstraEra) (eraProtVerHigh @DijkstraEra) mempoolTx of
          Left err -> expectationFailure $ show err
          Right (decodedMempoolTx :: MempoolTx DijkstraEra) -> do
            let decodedTx = unwrapMempoolTx decodedMempoolTx
            hashAnnotated decodedTx `shouldBe` hashAnnotated (unwrapMempoolTx mempoolTx)
            roundTripAnnEraExpectation @DijkstraEra decodedTx
    describe "Wire formats" $
      forEachEraVersion @DijkstraEra $ \version -> do
        let tx = mkBasicTx @DijkstraEra @TopTx (mkBasicTxBody @DijkstraEra @TopTx)
            authorFields =
              mconcat
                [ Ev version (tx ^. bodyTxL)
                , Ev version (tx ^. witsTxL)
                , E TkNull
                ]
            mempoolEnc = E (TkListLen 3) <> authorFields
            legacyMempoolEnc isValid =
              mconcat
                [ E (TkListLen 4)
                , Ev version (tx ^. bodyTxL)
                , Ev version (tx ^. witsTxL)
                , E (TkBool isValid)
                , E TkNull
                ]
            blockEnc isValid = E (TkListLen 4) <> authorFields <> E (TkBool isValid)
            indefiniteBlockEnc isValid =
              E TkListBegin <> authorFields <> E (TkBool isValid) <> E TkBreak

        it "Encodes the mempool and block layouts with their distinct field order" $
          forM_ [(Phase2Valid, True), (Phase2Invalid, False)] $ \(flag, isValid) -> do
            let txWithFlag = tx & isPhase2ValidTxL .~ flag
            serialize' version txWithFlag `shouldBe` Plain.serialize' (blockEnc isValid)
            serialize' version (UnsafeMkMempoolTx txWithFlag) `shouldBe` Plain.serialize' mempoolEnc

        it "Rejects block transactions in both mempool decoders" $
          forM_ [True, False] $ \isValid -> do
            let enc = blockEnc isValid
            decodeEnc @(MempoolTx DijkstraEra) version enc `shouldSatisfy` isLeft
            decodeFull @(MempoolTx DijkstraEra) version (Plain.serialize enc) `shouldSatisfy` isLeft

        it "Rejects both mempool layouts in both block transaction decoders" $
          forM_ [mempoolEnc, legacyMempoolEnc True] $ \enc -> do
            decodeEnc @(Tx TopTx DijkstraEra) version enc `shouldSatisfy` isLeft
            decodeFull @(Tx TopTx DijkstraEra) version (Plain.serialize enc) `shouldSatisfy` isLeft

        it "Rejects a legacy false validity flag in both mempool decoders" $ do
          let enc = legacyMempoolEnc False
          decodeEnc @(MempoolTx DijkstraEra) version enc `shouldSatisfy` isLeft
          decodeFull @(MempoolTx DijkstraEra) version (Plain.serialize enc) `shouldSatisfy` isLeft

        it "Normalizes legacy and indefinite mempool envelopes into the canonical layouts"
          $ forM_
            [ mempoolEnc
            , legacyMempoolEnc True
            , E TkListBegin <> authorFields <> E TkBreak
            ]
          $ \enc -> do
            decodedAnn <- expectRight $ decodeEnc @(MempoolTx DijkstraEra) version enc
            decodedPlain <-
              expectRight $ decodeFull @(MempoolTx DijkstraEra) version (Plain.serialize enc)
            forM_ [decodedAnn, decodedPlain] $ \decoded -> do
              let blockTx = unwrapMempoolTx decoded
              serialize' version decoded `shouldBe` Plain.serialize' mempoolEnc
              serialize' version blockTx `shouldBe` Plain.serialize' (blockEnc True)
              originalBytes blockTx `shouldBe` Plain.serialize' (blockEnc True)
              hashAnnotated blockTx `shouldBe` hashAnnotated tx

        it "Preserves noncanonical witness bytes through both mempool decoders" $ do
          let witsEnc = E TkMapBegin <> E TkBreak
              fields = Ev version (tx ^. bodyTxL) <> witsEnc <> E TkNull
              mempoolWithWits = E (TkListLen 3) <> fields
              blockWithWits = E (TkListLen 4) <> fields <> E (TkBool True)
          decodedAnn <- expectRight $ decodeEnc @(MempoolTx DijkstraEra) version mempoolWithWits
          decodedPlain <-
            expectRight $
              decodeFull @(MempoolTx DijkstraEra) version (Plain.serialize mempoolWithWits)
          forM_ [decodedAnn, decodedPlain] $ \decoded -> do
            let blockTx = unwrapMempoolTx decoded
            originalBytes (blockTx ^. witsTxL) `shouldBe` Plain.serialize' witsEnc
            serialize' version decoded `shouldBe` Plain.serialize' mempoolWithWits
            originalBytes blockTx `shouldBe` Plain.serialize' blockWithWits
            hashAnnotated (blockTx ^. bodyTxL) `shouldBe` hashAnnotated (tx ^. bodyTxL)
            hashAnnotated blockTx `shouldNotBe` hashAnnotated tx

        it "Preserves indefinite block bytes through both public Tx decoders" $
          forM_ [True, False] $ \isValid -> do
            let enc = indefiniteBlockEnc isValid
                bytes = Plain.serialize' enc
            decodedAnn <- expectRight $ decodeEnc @(Tx TopTx DijkstraEra) version enc
            decodedPlain <-
              expectRight $ decodeFull @(Tx TopTx DijkstraEra) version (Plain.serialize enc)
            forM_ [decodedAnn, decodedPlain] $ \decoded -> do
              originalBytes decoded `shouldBe` bytes
              serialize' version decoded `shouldBe` bytes
