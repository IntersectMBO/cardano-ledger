{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Test.Cardano.Ledger.Dijkstra.Imp.SubUtxoSpec (spec) where

import Cardano.Ledger.Address (BootstrapAddress)
import Cardano.Ledger.BaseTypes (
  Mismatch (..),
  Network (..),
  ProtVer,
  StrictMaybe (..),
  pvMajor,
 )
import Cardano.Ledger.Binary (EncCBOR, serialize)
import Cardano.Ledger.Coin (Coin (..))
import Cardano.Ledger.Core
import Cardano.Ledger.Credential (Credential (..), StakeReference (..))
import Cardano.Ledger.Dijkstra.Core
import Cardano.Ledger.Dijkstra.Rules (
  DijkstraSubUtxoPredFailure (..),
  DijkstraUtxoPredFailure (..),
 )
import Cardano.Ledger.Mary.Value (
  AssetName,
  MaryValue (..),
  MultiAsset,
  PolicyID (..),
  multiAssetFromList,
 )
import Cardano.Ledger.Plutus (SLanguage (..), hashPlutusScript)
import Cardano.Ledger.Shelley.Scripts (pattern RequireSignature)
import Cardano.Ledger.Tools (ensureMinCoinTxOut, setMinCoinTxOut)
import Cardano.Ledger.TxIn (TxIn, mkTxInPartial)
import Cardano.Ledger.Val (inject)
import Control.Monad.State (gets)
import qualified Data.ByteString.Lazy as BSL
import Data.Maybe (isNothing)
import qualified Data.OMap.Strict as OMap
import qualified Data.Sequence.Strict as SSeq
import qualified Data.Set.NonEmpty as NES
import Data.Word (Word64)
import Lens.Micro ((&), (.~), (<>~), (^.))
import Test.Cardano.Ledger.Dijkstra.ImpTest
import Test.Cardano.Ledger.Imp.Common
import Test.Cardano.Ledger.Plutus.Examples (alwaysFailsWithDatum)

spec :: forall era. DijkstraEraImp era => SpecWith (ImpInit (LedgerSpec era))
spec = describe "SUBUTXO" $ do
  describe "SubOutsideValidityIntervalUTxO" $ do
    it "the validity interval starts after the current slot" $ do
      currentSlot <- gets (^. impCurSlotNoG)
      let validityInterval = ValidityInterval (SJust (currentSlot + 1)) SNothing
      submitFailingSubTx
        (mkBasicTx $ mkBasicTxBody & vldtTxBodyL .~ validityInterval)
        [injectFailure $ SubOutsideValidityIntervalUTxO @era validityInterval currentSlot]

    disableInConformanceIt "the validity interval ends at the current slot" $ do
      currentSlot <- gets (^. impCurSlotNoG)
      let validityInterval = ValidityInterval SNothing (SJust currentSlot)
      submitFailingSubTx
        (mkBasicTx $ mkBasicTxBody & vldtTxBodyL .~ validityInterval)
        [injectFailure $ SubOutsideValidityIntervalUTxO @era validityInterval currentSlot]

  describe "SubOutputTooBigUTxO" $ do
    it "an output holding a minted asset, once only ada-only values fit" $ do
      restrictMaxValSizeToAdaOnly
      pp <- getsPParams id
      (multiAsset, txOut) <- freshAssetOutput
      submitFailingSubTx
        ( mkBasicTx $
            mkBasicTxBody
              & mintTxBodyL .~ multiAsset
              & outputsTxBodyL .~ [txOut]
        )
        [injectFailure $ SubOutputTooBigUTxO @era [outputTooBigEntry pp txOut]]

    it "several such outputs, reported in the reverse of their order in the body" $ do
      restrictMaxValSizeToAdaOnly
      pp <- getsPParams id
      (firstAsset, firstTxOut) <- freshAssetOutput
      (secondAsset, secondTxOut) <- freshAssetOutput
      submitFailingSubTx
        ( mkBasicTx $
            mkBasicTxBody
              & mintTxBodyL .~ firstAsset <> secondAsset
              & outputsTxBodyL .~ [firstTxOut, secondTxOut]
        )
        [ injectFailure $
            SubOutputTooBigUTxO @era
              [outputTooBigEntry pp secondTxOut, outputTooBigEntry pp firstTxOut]
        ]

  describe "SubInputSetEmptyUTxO" $
    it "a sub-transaction with no inputs" $ do
      txIn <- freshFundedTxIn
      withPostFixup (moveSubTxInputsToTopLevel txIn) $
        submitFailingSubTx
          (mkBasicTx $ mkBasicTxBody & inputsTxBodyL .~ [txIn])
          [injectFailure $ SubInputSetEmptyUTxO @era]

  describe "SubBadInputsUTxO" $ do
    it "a reference input in no UTxO fails only the check against the original UTxO" $ do
      let badReferenceInput = neverSubmittedTxIn @era 0
      submitFailingSubTx
        (mkBasicTx $ mkBasicTxBody & referenceInputsTxBodyL .~ [badReferenceInput])
        [injectFailure . SubBadInputsUTxO @era $ NES.singleton badReferenceInput]

    it "an input spent by an earlier sub-transaction fails only the check against the threaded UTxO" $ do
      (sharedTxIn, subTxs) <- subTxsSpendingOneInput
      submitFailingTx
        (txWithSubTxs subTxs)
        [injectFailure . SubBadInputsUTxO @era $ NES.singleton sharedTxIn]

    it "an input in no UTxO fails both checks" $ do
      let badInput = neverSubmittedTxIn @era 0
      submitFailingSubTx
        (mkBasicTx $ mkBasicTxBody & inputsTxBodyL .~ [badInput])
        [ injectFailure . SubBadInputsUTxO @era $ NES.singleton badInput
        , injectFailure . SubBadInputsUTxO @era $ NES.singleton badInput
        ]

    it
      "the original UTxO check reports both bad inputs, the threaded check only the bad spend input"
      $ do
        let badInput = neverSubmittedTxIn @era 0
            badReferenceInput = neverSubmittedTxIn @era 1
        submitFailingSubTx
          ( mkBasicTx $
              mkBasicTxBody
                & inputsTxBodyL .~ [badInput]
                & referenceInputsTxBodyL .~ [badReferenceInput]
          )
          [ injectFailure . SubBadInputsUTxO @era $ NES.singleton badInput
          , injectFailure . SubBadInputsUTxO @era $
              NES.singleton badInput <> NES.singleton badReferenceInput
          ]

  describe "Inputs produced or spent within the batch" $ do
    it "spending an earlier sibling's output fails only the check against the original UTxO" $ do
      (producingSubTx, producedTxIn) <- freshSubTxProducingOutput
      submitFailingTx
        ( txWithSubTxs
            [ producingSubTx
            , mkBasicTx $ mkBasicTxBody & inputsTxBodyL .~ [producedTxIn]
            ]
        )
        [injectFailure . SubBadInputsUTxO @era $ NES.singleton producedTxIn]

    it "referencing an earlier sibling's output fails only the check against the original UTxO" $ do
      (producingSubTx, producedTxIn) <- freshSubTxProducingOutput
      submitFailingTx
        ( txWithSubTxs
            [ producingSubTx
            , mkBasicTx $ mkBasicTxBody & referenceInputsTxBodyL .~ [producedTxIn]
            ]
        )
        [injectFailure . SubBadInputsUTxO @era $ NES.singleton producedTxIn]

    it "spending a later sibling's output fails both checks" $ do
      (producingSubTx, producedTxIn) <- freshSubTxProducingOutput
      submitFailingTx
        ( txWithSubTxs
            [ mkBasicTx $ mkBasicTxBody & inputsTxBodyL .~ [producedTxIn]
            , producingSubTx
            ]
        )
        [ injectFailure . SubBadInputsUTxO @era $ NES.singleton producedTxIn
        , injectFailure . SubBadInputsUTxO @era $ NES.singleton producedTxIn
        ]

    it "referencing an input that an earlier sibling spends is accepted, and that input is consumed" $ do
      txIn <- freshFundedTxIn
      submitTx_ $
        txWithSubTxs
          [ mkBasicTx $ mkBasicTxBody & inputsTxBodyL .~ [txIn]
          , mkBasicTx $ mkBasicTxBody & referenceInputsTxBodyL .~ [txIn]
          ]
      getUTxO >>= (`expectUTxOContent` [(txIn, isNothing)])

  describe "SubOutputBootAddrAttrsTooBig" $ do
    disableInConformanceIt "an output to a bootstrap address whose attributes exceed the limit" $ do
      txOut <- bootstrapOutput =<< freshBootstrapAddressOversizedPayload
      submitFailingSubTx
        (mkBasicTx $ mkBasicTxBody & outputsTxBodyL .~ [txOut])
        [injectFailure $ SubOutputBootAddrAttrsTooBig @era [txOut]]

    disableInConformanceIt "several such outputs, reported in their order in the body" $ do
      firstTxOut <- bootstrapOutput =<< freshBootstrapAddressOversizedPayload
      secondTxOut <- bootstrapOutput =<< freshBootstrapAddressOversizedPayload
      submitFailingSubTx
        (mkBasicTx $ mkBasicTxBody & outputsTxBodyL .~ [firstTxOut, secondTxOut])
        [injectFailure $ SubOutputBootAddrAttrsTooBig @era [firstTxOut, secondTxOut]]

  describe "SubBabbageOutputTooSmallUTxO" $ do
    it "an output that holds less than the minimum coin" $ do
      pp <- getsPParams id
      txOut <- freshTxOutWithCoin $ Coin 1
      submitFailingSubTx
        (mkBasicTx $ mkBasicTxBody & outputsTxBodyL .~ [txOut])
        [injectFailure $ SubBabbageOutputTooSmallUTxO @era [(txOut, getMinCoinTxOut pp txOut)]]

    it "several such outputs, reported in their order in the body" $ do
      pp <- getsPParams id
      firstTxOut <- freshTxOutWithCoin $ Coin 1
      secondTxOut <- freshTxOutWithCoin $ Coin 2
      submitFailingSubTx
        (mkBasicTx $ mkBasicTxBody & outputsTxBodyL .~ [firstTxOut, secondTxOut])
        [ injectFailure $
            SubBabbageOutputTooSmallUTxO @era
              [ (firstTxOut, getMinCoinTxOut pp firstTxOut)
              , (secondTxOut, getMinCoinTxOut pp secondTxOut)
              ]
        ]

  describe "SubWrongNetwork" $ do
    it "an output to a mainnet address" $ do
      pp <- getsPParams id
      addr <- freshMainnetAddr
      let txOut = ensureMinCoinTxOut pp $ mkBasicTxOut addr mempty
      submitFailingSubTx
        (mkBasicTx $ mkBasicTxBody & outputsTxBodyL .~ [txOut])
        [injectFailure . SubWrongNetwork @era Testnet $ NES.singleton addr]

    it "several outputs to mainnet addresses" $ do
      pp <- getsPParams id
      firstAddr <- freshMainnetAddr
      secondAddr <- freshMainnetAddr
      let firstTxOut = ensureMinCoinTxOut pp $ mkBasicTxOut firstAddr mempty
          secondTxOut = ensureMinCoinTxOut pp $ mkBasicTxOut secondAddr mempty
      submitFailingSubTx
        (mkBasicTx $ mkBasicTxBody & outputsTxBodyL .~ [firstTxOut, secondTxOut])
        [ injectFailure . SubWrongNetwork @era Testnet $
            NES.singleton firstAddr <> NES.singleton secondAddr
        ]

  describe "SubWrongNetworkInTxBody" $
    it "a sub-transaction body with a mainnet network id" $
      submitFailingSubTx
        (mkBasicTx $ mkBasicTxBody & networkIdTxBodyL .~ SJust Mainnet)
        [ injectFailure . SubWrongNetworkInTxBody @era $
            Mismatch {mismatchSupplied = Mainnet, mismatchExpected = Testnet}
        ]

  it "a sub-transaction larger than maxTxSize is rejected only by the top level rule" $ do
    pp <- getsPParams id
    addr <- freshKeyAddr_
    let txOut = setMinCoinTxOut pp $ mkBasicTxOut addr mempty
        subTx :: Tx SubTx era
        subTx = mkBasicTx $ mkBasicTxBody & outputsTxBodyL .~ SSeq.fromList (replicate 20 txOut)
        maxTxSize = subTx ^. sizeTxF - 1
    modifyPParams $ ppMaxTxSizeL .~ maxTxSize
    submitFailingTxM (txWithSubTxs [subTx]) $ \fixedUpTx ->
      pure
        [ injectFailure . MaxTxSizeUTxO @era $
            Mismatch {mismatchSupplied = fixedUpTx ^. sizeTxF, mismatchExpected = maxTxSize}
        ]

  it "one input listed as both a spend and a reference input is accepted, and consumed" $ do
    txIn <- freshFundedTxIn
    submitTx_ . txWithSubTxs . pure . mkBasicTx $
      mkBasicTxBody
        & inputsTxBodyL .~ [txIn]
        & referenceInputsTxBodyL .~ [txIn]
    getUTxO >>= (`expectUTxOContent` [(txIn, isNothing)])

  describe "Composite tests" $ do
    disableInConformanceIt
      "seven failures of one sub-transaction, in the reverse of the order the rule checks them"
      $ do
        restrictMaxValSizeToAdaOnly
        pp <- getsPParams id
        currentSlot <- gets (^. impCurSlotNoG)
        (multiAsset, tooBigTxOut) <- freshAssetOutput
        bootstrapTxOut <- bootstrapOutput =<< freshBootstrapAddressOversizedPayload
        mainnetAddr <- freshMainnetAddr
        let badReferenceInput = neverSubmittedTxIn @era 0
            validityInterval = ValidityInterval (SJust (currentSlot + 1)) SNothing
            tooSmallTxOut = mkBasicTxOut mainnetAddr . inject $ Coin 1
        submitFailingSubTx
          ( mkBasicTx $
              mkBasicTxBody
                & vldtTxBodyL .~ validityInterval
                & mintTxBodyL .~ multiAsset
                & referenceInputsTxBodyL .~ [badReferenceInput]
                & outputsTxBodyL .~ [tooBigTxOut, bootstrapTxOut, tooSmallTxOut]
                & networkIdTxBodyL .~ SJust Mainnet
          )
          [ injectFailure . SubWrongNetworkInTxBody @era $
              Mismatch {mismatchSupplied = Mainnet, mismatchExpected = Testnet}
          , injectFailure . SubWrongNetwork @era Testnet $ NES.singleton mainnetAddr
          , injectFailure $
              SubBabbageOutputTooSmallUTxO @era
                [(tooSmallTxOut, getMinCoinTxOut pp tooSmallTxOut)]
          , injectFailure $ SubOutputBootAddrAttrsTooBig @era [bootstrapTxOut]
          , injectFailure . SubBadInputsUTxO @era $ NES.singleton badReferenceInput
          , injectFailure $ SubOutputTooBigUTxO @era [outputTooBigEntry pp tooBigTxOut]
          , injectFailure $ SubOutsideValidityIntervalUTxO @era validityInterval currentSlot
          ]

    it "failures of several sub-transactions, in sub-transaction order" $ do
      currentSlot <- gets (^. impCurSlotNoG)
      let validityInterval = ValidityInterval (SJust (currentSlot + 1)) SNothing
          wrongNetworkSubTx = mkBasicTx $ mkBasicTxBody & networkIdTxBodyL .~ SJust Mainnet
          outsideValiditySubTx = mkBasicTx $ mkBasicTxBody & vldtTxBodyL .~ validityInterval
      submitFailingTx
        (txWithSubTxs [wrongNetworkSubTx, outsideValiditySubTx])
        [ injectFailure . SubWrongNetworkInTxBody @era $
            Mismatch {mismatchSupplied = Mainnet, mismatchExpected = Testnet}
        , injectFailure $ SubOutsideValidityIntervalUTxO @era validityInterval currentSlot
        ]

  describe "Accepted at the boundary" $ do
    it "a validity interval that starts at the current slot and ends at the next one" $ do
      currentSlot <- gets (^. impCurSlotNoG)
      submitTx_ . txWithSubTxs . pure . mkBasicTx $
        mkBasicTxBody
          & vldtTxBodyL .~ ValidityInterval (SJust currentSlot) (SJust (currentSlot + 1))

    it "an output that holds exactly the minimum coin" $ do
      pp <- getsPParams id
      addr <- freshKeyAddr_
      submitTx_ . txWithSubTxs . pure . mkBasicTx $
        mkBasicTxBody & outputsTxBodyL .~ [setMinCoinTxOut pp $ mkBasicTxOut addr mempty]

    disableInConformanceIt "an output to a bootstrap address whose payload is the largest allowed size" $ do
      txOut <-
        bootstrapOutput
          =<< freshBootstrapAddressWithPayloadSize (Just largestBootstrapAddressAttrsSize)
      submitTx_ . txWithSubTxs . pure . mkBasicTx $ mkBasicTxBody & outputsTxBodyL .~ [txOut]

  describe "A phase-2 invalid top level transaction" $ do
    it "still rejects a sub-transaction with the wrong network id in its body" $ do
      let subTx :: Tx SubTx era
          subTx = mkBasicTx $ mkBasicTxBody & networkIdTxBodyL .~ SJust Mainnet
      topTx <- phase2InvalidTxWithSubTxs [subTx]
      withNoFixup $
        submitFailingTx
          topTx
          [ injectFailure . SubWrongNetworkInTxBody @era $
              Mismatch {mismatchSupplied = Mainnet, mismatchExpected = Testnet}
          ]

    it "does not check the threaded UTxO, so two sub-transactions may name one input" $ do
      (sharedTxIn, subTxs) <- subTxsSpendingOneInput
      topTx <- phase2InvalidTxWithSubTxs subTxs
      withNoFixup $ submitTx_ topTx
      void $ impGetUTxO sharedTxIn

    it "applies no sub-transaction output, so spending an earlier sibling's output fails twice" $ do
      (producingSubTx, producedTxIn) <- freshSubTxProducingOutput
      topTx <-
        phase2InvalidTxWithSubTxs
          [ producingSubTx
          , mkBasicTx $ mkBasicTxBody & inputsTxBodyL .~ [producedTxIn]
          ]
      withNoFixup $
        submitFailingTx
          topTx
          [ injectFailure . SubBadInputsUTxO @era $ NES.singleton producedTxIn
          , injectFailure . SubBadInputsUTxO @era $ NES.singleton producedTxIn
          ]

-- | Empty the input set of every sub-transaction and spend the given input at
-- the top level instead, leaving the value consumed by the batch unchanged.
moveSubTxInputsToTopLevel ::
  (HasCallStack, DijkstraEraImp era) =>
  TxIn ->
  Tx TopTx era ->
  ImpTestM era (Tx TopTx era)
moveSubTxInputsToTopLevel txIn tx = do
  subTxs <-
    traverse (resetAddrTxWits . (bodyTxL . inputsTxBodyL .~ mempty))
      . OMap.elems
      $ tx ^. bodyTxL . subTransactionsTxBodyL
  resetAddrTxWits $
    tx
      & bodyTxL . subTransactionsTxBodyL .~ OMap.fromFoldable subTxs
      & bodyTxL . inputsTxBodyL <>~ [txIn]

freshFundedTxIn :: DijkstraEraImp era => ImpTestM era TxIn
freshFundedTxIn = freshKeyAddr_ >>= (`sendCoinTo` Coin 3_000_000)

freshSubTxProducingOutput ::
  (HasCallStack, DijkstraEraImp era) =>
  ImpTestM era (Tx SubTx era, TxIn)
freshSubTxProducingOutput = do
  pp <- getsPParams id
  spentTxIn <- freshFundedTxIn
  producedAddr <- freshKeyAddr_
  let subTx =
        mkBasicTx $
          mkBasicTxBody
            & inputsTxBodyL .~ [spentTxIn]
            & outputsTxBodyL .~ [setMinCoinTxOut pp $ mkBasicTxOut producedAddr mempty]
  pure (subTx, mkTxInPartial (txIdTx subTx) 0)

freshTxOutWithCoin :: DijkstraEraImp era => Coin -> ImpTestM era (TxOut era)
freshTxOutWithCoin coin = do
  addr <- freshKeyAddr_
  pure . mkBasicTxOut addr $ inject coin

freshMainnetAddr :: DijkstraEraImp era => ImpTestM era Addr
freshMainnetAddr = do
  keyHash <- freshKeyHash @Payment
  pure $ Addr Mainnet (KeyHashObj keyHash) StakeRefNull

bootstrapOutput :: DijkstraEraImp era => BootstrapAddress -> ImpTestM era (TxOut era)
bootstrapOutput bootAddr = do
  pp <- getsPParams id
  pure . ensureMinCoinTxOut pp . mkBasicTxOut (AddrBootstrap bootAddr) $ mempty

freshAssetOutput :: DijkstraEraImp era => ImpTestM era (MultiAsset, TxOut era)
freshAssetOutput = do
  pp <- getsPParams id
  policyId <- PolicyID <$> (impAddNativeScript . RequireSignature =<< freshKeyHash)
  assetName <- arbitrary @AssetName
  addr <- freshKeyAddr_
  let multiAsset = multiAssetFromList [(policyId, assetName, 1)]
  pure (multiAsset, ensureMinCoinTxOut pp . mkBasicTxOut addr $ MaryValue mempty multiAsset)

subTxsSpendingOneInput :: DijkstraEraImp era => ImpTestM era (TxIn, [Tx SubTx era])
subTxsSpendingOneInput = do
  sharedTxIn <- freshFundedTxIn
  otherTxIn <- freshFundedTxIn
  pure
    ( sharedTxIn
    ,
      [ mkBasicTx $ mkBasicTxBody & inputsTxBodyL .~ [sharedTxIn]
      , mkBasicTx $ mkBasicTxBody & inputsTxBodyL .~ [sharedTxIn, otherTxIn]
      ]
    )

neverSubmittedTxIn :: forall era. DijkstraEraImp era => Integer -> TxIn
neverSubmittedTxIn = mkTxInPartial . txIdTx $ (mkBasicTx mkBasicTxBody :: Tx TopTx era)

restrictMaxValSizeToAdaOnly :: forall era. DijkstraEraImp era => ImpTestM era ()
restrictMaxValSizeToAdaOnly = do
  protVer <- getProtVer
  let largestAdaOnlyValue = inject . Coin . toInteger $ (maxBound :: Word64) :: Value era
      largestAdaOnlyValueSize = serializedValueSize protVer largestAdaOnlyValue
  modifyPParams $ ppMaxValSizeL .~ fromIntegral largestAdaOnlyValueSize

serializedValueSize :: EncCBOR value => ProtVer -> value -> Int
serializedValueSize protVer = fromIntegral . BSL.length . serialize (pvMajor protVer)

outputTooBigEntry :: DijkstraEraImp era => PParams era -> TxOut era -> (Int, Int, TxOut era)
outputTooBigEntry pp txOut =
  ( serializedValueSize (pp ^. ppProtocolVersionL) $ txOut ^. valueTxOutL
  , fromIntegral $ pp ^. ppMaxValSizeL
  , txOut
  )

phase2InvalidTxWithSubTxs ::
  (HasCallStack, DijkstraEraImp era) =>
  [Tx SubTx era] ->
  ImpTestM era (Tx TopTx era)
phase2InvalidTxWithSubTxs subTxs = do
  failingScriptTxIn <- produceScript . hashPlutusScript $ alwaysFailsWithDatum SPlutusV3
  fixedUpTx <- fixupTx $ txWithSubTxs subTxs & bodyTxL . inputsTxBodyL .~ [failingScriptTxIn]
  pure $ fixedUpTx & isPhase2ValidTxL .~ Phase2Invalid
