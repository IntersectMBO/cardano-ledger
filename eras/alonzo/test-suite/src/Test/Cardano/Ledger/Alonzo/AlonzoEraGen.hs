{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

-- |  AlonzoEra instances for EraGen and ScriptClass
module Test.Cardano.Ledger.Alonzo.AlonzoEraGen where

import Cardano.Ledger.Address (Addr (..))
import Cardano.Ledger.Allegra.Scripts (
  AllegraEraScript,
  Timelock (..),
  translateTimelock,
  pattern RequireTimeExpire,
  pattern RequireTimeStart,
 )
import Cardano.Ledger.Allegra.TxAuxData (AllegraTxAuxData (..))
import Cardano.Ledger.Alonzo (AlonzoEra, Tx (..))
import Cardano.Ledger.Alonzo.Core
import Cardano.Ledger.Alonzo.PParams
import Cardano.Ledger.Alonzo.Plutus.Context (EraPlutusTxInfo, mkSupportedPlutusScript)
import Cardano.Ledger.Alonzo.Rules (vKeyLocked)
import Cardano.Ledger.Alonzo.Scripts as Alonzo (
  AlonzoPlutusPurpose (..),
  AlonzoScript (..),
  ExUnits (..),
  Prices (..),
  isPlutusScript,
  plutusScriptLanguage,
  pointWiseExUnits,
  toAsIx,
  txscriptfee,
 )
import Cardano.Ledger.Alonzo.Tx (
  AlonzoTx (AlonzoTx),
  IsValid (..),
  hashScriptIntegrity,
  totExUnits,
 )
import Cardano.Ledger.Alonzo.TxAuxData (AlonzoTxAuxData (..), mkAlonzoTxAuxData)
import Cardano.Ledger.Alonzo.TxBody (
  AlonzoTxOut (..),
  TxBody (..),
  utxoEntrySize,
 )
import Cardano.Ledger.Alonzo.TxWits (
  AlonzoTxWits (..),
  Redeemers (..),
  TxDats (..),
  unRedeemersL,
 )
import Cardano.Ledger.Alonzo.UTxO (AlonzoScriptsNeeded (..))
import Cardano.Ledger.BaseTypes
import Cardano.Ledger.Binary (EncCBOR)
import Cardano.Ledger.Coin (Coin (..))
import Cardano.Ledger.Credential (Credential (..))
import Cardano.Ledger.Mary (MaryEra)
import Cardano.Ledger.Mary.Value (
  AssetName (..),
  MultiAsset (..),
  PolicyID (..),
  multiAssetFromList,
  policies,
 )
import Cardano.Ledger.Plutus.Data (Data (..))
import Cardano.Ledger.Plutus.Language (Language (..), SLanguage (..))
import Cardano.Ledger.Shelley.PParams (Update)
import Cardano.Ledger.Shelley.Scripts
import Cardano.Ledger.State (
  EraUTxO (..),
  UTxO (..),
  getScriptsNeeded,
  sumCoinUTxO,
  txInsFilter,
 )
import Cardano.Ledger.TxIn (TxIn)
import Cardano.Ledger.Val (Val (isAdaOnly, (<+>), (<×>)))
import Control.Monad (replicateM)
import Data.Foldable as F
import qualified Data.List as List
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Proxy (Proxy (..))
import Data.Ratio ((%))
import Data.Sequence.Strict (StrictSeq ((:|>)))
import qualified Data.Sequence.Strict as Seq (fromList)
import Data.Set (Set)
import qualified Data.Set as Set
import Lens.Micro
import Lens.Micro.Extras (view)
import Numeric.Natural (Natural)
import qualified PlutusLedgerApi.Common as P (Data (..))
import System.Random
import Test.Cardano.Ledger.AllegraEraGen (genValidityInterval)
import Test.Cardano.Ledger.Alonzo.Arbitrary ()
import Test.Cardano.Ledger.Binary.Random
import Test.Cardano.Ledger.Common (tracedDiscard)
import Test.Cardano.Ledger.MaryEraGen (addTokens, genMint, maryGenesisValue, policyIndex)
import Test.Cardano.Ledger.Plutus (alwaysFailsPlutus, alwaysSucceedsPlutus, zeroTestingCostModels)
import Test.Cardano.Ledger.Plutus.Examples
import Test.Cardano.Ledger.Shelley.Constants (Constants (..))
import Test.Cardano.Ledger.Shelley.Generator.Core (
  GenEnv (..),
  ScriptInfo,
  TwoPhase2ArgInfo (..),
  TwoPhase3ArgInfo (..),
  findPlutus,
  genNatural,
  hashData,
 )
import Test.Cardano.Ledger.Shelley.Generator.EraGen (EraGen (..), MinGenTxout (..))
import Test.Cardano.Ledger.Shelley.Generator.ScriptClass (Quantifier (..), ScriptClass (..))
import Test.Cardano.Ledger.Shelley.Generator.Update (genM, genShelleyPParamsUpdate)
import qualified Test.Cardano.Ledger.Shelley.Generator.Update as Shelley (genPParams)
import Test.Cardano.Ledger.Shelley.Generator.Utxo (encodedLen)
import Test.Cardano.Ledger.Shelley.Utils (unsafeBoundRational)
import Test.QuickCheck hiding ((><))

-- ============================================================

-- | We are choosing new TxOut to pay fees, We want only Key locked addresss with Ada only values.
vKeyLockedAdaOnly :: TxOut AlonzoEra -> Bool
vKeyLockedAdaOnly txOut = vKeyLocked txOut && isAdaOnly (txOut ^. valueTxOutL)

phase2scripts3Arg :: EraPlutusTxInfo 'PlutusV1 era => [TwoPhase3ArgInfo era]
phase2scripts3Arg =
  [ mkTwoPhase3ArgInfo
      (mkSupportedPlutusScript (alwaysSucceedsPlutus @'PlutusV1 3))
      (P.I 1)
      (P.I 1, bigMem, bigStep)
      True
  , mkTwoPhase3ArgInfo
      (mkSupportedPlutusScript (redeemerSameAsDatum SPlutusV1))
      (P.I 9)
      (P.I 9, bigMem, bigStep)
      True
  , mkTwoPhase3ArgInfo
      (mkSupportedPlutusScript (evenDatum SPlutusV1))
      (P.I 8)
      (P.I 8, bigMem, bigStep)
      True
  , mkTwoPhase3ArgInfo
      (mkSupportedPlutusScript (alwaysFailsPlutus @'PlutusV1 3))
      (P.I 1)
      (P.I 1, bigMem, bigStep)
      False
  , mkTwoPhase3ArgInfo
      (mkSupportedPlutusScript (purposeIsWellformedWithDatum SPlutusV1))
      (P.I 3)
      (P.I 4, bigMem, bigStep)
      True
  , mkTwoPhase3ArgInfo
      (mkSupportedPlutusScript (datumIsWellformed SPlutusV1))
      (P.I 5)
      (P.I 6, bigMem, bigStep)
      True
  , mkTwoPhase3ArgInfo
      (mkSupportedPlutusScript (inputsOutputsAreNotEmptyWithDatum SPlutusV1))
      (P.I 7)
      (P.I 9, bigMem, bigStep)
      True
  ]
  where
    mkTwoPhase3ArgInfo plutusScript =
      let script = fromPlutusScript plutusScript
       in TwoPhase3ArgInfo script (hashScript script)

phase2scripts2Arg :: EraPlutusTxInfo 'PlutusV1 era => [TwoPhase2ArgInfo era]
phase2scripts2Arg =
  [ mkTwoPhase2ArgInfo
      (mkSupportedPlutusScript (alwaysSucceedsPlutus @'PlutusV1 2))
      (P.I 1, bigMem, bigStep)
      True
  , mkTwoPhase2ArgInfo
      (mkSupportedPlutusScript (evenRedeemerNoDatum SPlutusV1))
      (P.I 14, bigMem, bigStep)
      True
  , mkTwoPhase2ArgInfo
      (mkSupportedPlutusScript (alwaysFailsPlutus @'PlutusV1 2))
      (P.I 1, bigMem, bigStep)
      False
  , mkTwoPhase2ArgInfo
      (mkSupportedPlutusScript (purposeIsWellformedNoDatum SPlutusV1))
      (P.I 14, bigMem, bigStep)
      True
  , mkTwoPhase2ArgInfo
      (mkSupportedPlutusScript (inputsOutputsAreNotEmptyNoDatum SPlutusV1))
      (P.I 15, bigMem, bigStep)
      True
  ]
  where
    mkTwoPhase2ArgInfo plutusScript =
      let script = fromPlutusScript plutusScript
       in TwoPhase2ArgInfo script (hashScript script)

phase2scripts3ArgSucceeds ::
  forall era.
  EraPlutusTxInfo 'PlutusV1 era =>
  Script era ->
  Bool
phase2scripts3ArgSucceeds script =
  maybe True getSucceeds3 $
    List.find (\info -> getScript3 info == script) phase2scripts3Arg

phase2scripts2ArgSucceeds ::
  forall era.
  EraPlutusTxInfo 'PlutusV1 era =>
  Script era ->
  Bool
phase2scripts2ArgSucceeds script =
  maybe True getSucceeds2 $
    List.find (\info -> getScript2 info == script) phase2scripts2Arg

genPlutus2Arg ::
  EraPlutusTxInfo 'PlutusV1 era =>
  Gen (Maybe (TwoPhase2ArgInfo era))
genPlutus2Arg = frequency [(10, Just <$> elements phase2scripts2Arg), (90, pure Nothing)]

-- | Gen a Mint value in the Alonzo Era, with a 10% chance that it includes an AlonzoScript
genAlonzoMint :: MultiAsset -> Gen (MultiAsset, [AlonzoScript AlonzoEra])
genAlonzoMint startvalue = do
  ans <- genPlutus2Arg
  case ans of
    Nothing -> pure (startvalue, [])
    Just (TwoPhase2ArgInfo script shash _ _) -> do
      count <- chooseEnum (1, 10)
      let assetname = AssetName "purple"
      pure (multiAssetFromList [(PolicyID shash, assetname, count)] <> startvalue, [script])

genPair :: Gen a -> Gen b -> Gen (a, b)
genPair x y = (,) <$> x <*> y

genPlutusData :: Gen P.Data
genPlutusData = resize 5 (sized gendata)
  where
    gendata n
      | n > 0 =
          oneof
            [ P.I <$> arbitrary
            , P.B <$> arbitrary
            , P.Map <$> listOf (genPair (gendata (n `div` 2)) (gendata (n `div` 2)))
            , P.Constr <$> arbitrary <*> listOf (gendata (n `div` 2))
            , P.List <$> listOf (gendata (n `div` 2))
            ]
    gendata _ = oneof [P.I <$> arbitrary, P.B <$> arbitrary]

genSet :: Ord a => Gen a -> Gen (Set a)
genSet gen =
  frequency
    [ (1, pure Set.empty)
    , (2, Set.fromList <$> sequence [gen])
    , (1, Set.fromList <$> sequence [gen, gen])
    ]

genAux :: Constants -> Gen (StrictMaybe (AlonzoTxAuxData AlonzoEra))
genAux constants = do
  maybeAux <- genEraAuxiliaryData @MaryEra constants
  pure $
    fmap
      (\(AllegraTxAuxData x y) -> mkAlonzoTxAuxData x (TimelockScript . translateTimelock <$> y))
      maybeAux

instance ScriptClass AlonzoEra where
  basescript = someLeaf
  isKey _ (TimelockScript x) = isKey (Proxy @MaryEra) $ translateTimelock x
  isKey _ (PlutusScript _) = Nothing
  isOnePhase _ (TimelockScript _) = True
  isOnePhase _ (PlutusScript _) = False
  quantify _ (TimelockScript x) = fmap (TimelockScript . translateTimelock) (quantify (Proxy @MaryEra) (translateTimelock x))
  quantify _ x = Leaf x
  unQuantify _ quant =
    TimelockScript . translateTimelock $
      unQuantify (Proxy @MaryEra) (fmap (translateTimelock . unTime) quant)

unTime :: AlonzoScript era -> Timelock era
unTime (TimelockScript x) = x
unTime (PlutusScript _) = error "Plutus in Timelock"

okAsCollateral :: UTxO AlonzoEra -> TxIn -> Bool
okAsCollateral utxo inputx =
  maybe False vKeyLockedAdaOnly $ Map.lookup inputx (unUTxO utxo)

genAlonzoTxBody ::
  GenEnv c AlonzoEra ->
  UTxO AlonzoEra ->
  PParams AlonzoEra ->
  SlotNo ->
  Set.Set TxIn ->
  StrictSeq (TxOut AlonzoEra) ->
  StrictSeq (TxCert AlonzoEra) ->
  Withdrawals ->
  Coin ->
  StrictMaybe (Update AlonzoEra) ->
  StrictMaybe TxAuxDataHash ->
  Gen (TxBody AlonzoEra, [Script AlonzoEra])
genAlonzoTxBody _genenv utxo pparams currentslot input txOuts certs withdrawals fee updates auxDHash = do
  netid <- genM $ pure Testnet -- frequency [(2, pure Mainnet), (1, pure Testnet)]
  startvalue <- genMint
  (minted, plutusScripts) <- genAlonzoMint startvalue
  let (minted2, txouts2) = case addTokens (Proxy @AlonzoEra) mempty pparams minted txOuts of
        Nothing -> (mempty, txOuts)
        Just os -> (minted, os)
      scriptsFromPolicies = (policyIndex Map.!) <$> Set.toList (policies startvalue)
      txouts3 = fmap addMaybeDataHashToTxOut txouts2
  validityInterval <- genValidityInterval currentslot
  return
    ( AlonzoTxBody
        input
        (Set.filter (okAsCollateral utxo) input) -- Set.empty -- collateral -- TODO do something better here (use genenv ?)
        txouts3
        certs
        withdrawals
        fee
        validityInterval -- (ValidityInterval SNothing SNothing) -- (ValidityInterval low high)
        updates
        -- reqSignerHashes
        Set.empty -- TODO do something better here
        minted2
        -- scriptIntegrityHash starts out with empty Redeemers,
        -- as Remdeemers are added it is recomputed in updateEraTxBody
        (hashScriptIntegrity @AlonzoEra Set.empty (Redeemers Map.empty) (TxDats Map.empty))
        auxDHash
        netid
    , List.map TimelockScript scriptsFromPolicies <> plutusScripts
    )

genSlotAfter :: SlotNo -> Gen SlotNo
genSlotAfter currentSlot = do
  ttl <- genNatural 50 100
  pure $ currentSlot + SlotNo (fromIntegral ttl)

-- | Gen an Alonzo PParamsUpdate, by adding to a Shelley PParamsData
genAlonzoPParamsUpdate ::
  Constants ->
  PParams AlonzoEra ->
  Gen (PParamsUpdate AlonzoEra)
genAlonzoPParamsUpdate constants pp = do
  maryPPUpdate <-
    genShelleyPParamsUpdate @MaryEra constants $
      downgradePParams (DowngradeAlonzoPParams {dappMinUTxOValue = Coin 100}) pp
  coinPerWord <- genM (CoinPerWord . Coin <$> choose (1, 5))
  let genPrice = unsafeBoundRational . (% 100) <$> choose (0, 200)
  prices <- genM (Prices <$> genPrice <*> genPrice)
  maxTxExUnits <- genM genMaxTxExUnits
  maxBlockExUnits <- genM genMaxBlockExUnits
  -- Not too small for maxValSize, if this is too small then any Tx with Value
  -- that has lots of policyIds will fail. The Shelley Era uses hard coded 4000
  maxValSize <- genM (genNatural 4000 5000)
  let alonzoUpgrade =
        UpgradeAlonzoPParams
          { uappCoinsPerUTxOWord = coinPerWord
          , uappCostModels = SJust $ zeroTestingCostModels [PlutusV1]
          , uappPrices = prices
          , uappMaxTxExUnits = maxTxExUnits
          , uappMaxBlockExUnits = maxBlockExUnits
          , uappMaxValSize = maxValSize
          , uappCollateralPercentage = SJust 25 -- percent of fee in collateral
          , uappMaxCollateralInputs = SJust 100 -- max number of inputs in collateral
          }
  pure $ upgradePParamsUpdate alonzoUpgrade maryPPUpdate

genAlonzoPParams ::
  Constants ->
  Gen (PParams AlonzoEra)
genAlonzoPParams constants = do
  -- This ensures that "_d" field is not 0, and that the major protocol version
  -- is large enough to not trigger plutus script failures
  -- (no bultins are alllowed before major version 5).
  maryPP' <- Shelley.genPParams @MaryEra constants
  let maryPP = maryPP' & ppProtocolVersionL .~ ProtVer (natVersion @5) 0
      prices = Prices minBound minBound
  coinPerWord <- CoinPerWord . Coin <$> choose (1, 5)
  -- prices <- Prices <$> (Coin <$> choose (100, 5000)) <*> (Coin <$> choose (100, 5000))
  maxTxExUnits <- genMaxTxExUnits
  maxBlockExUnits <- genMaxBlockExUnits
  maxValSize <- genNatural 4000 10000 -- This can't be too small. Shelley uses Hard coded 4000
  let alonzoUpgrade =
        UpgradeAlonzoPParams
          { uappCoinsPerUTxOWord = coinPerWord
          , uappCostModels = zeroTestingCostModels [PlutusV1]
          , uappPrices = prices
          , uappMaxTxExUnits = maxTxExUnits
          , uappMaxBlockExUnits = maxBlockExUnits
          , uappMaxValSize = maxValSize
          , uappCollateralPercentage = 25 -- percent of fee in collateral
          , uappMaxCollateralInputs = 100 -- max number of inputs in collateral
          }
  pure $ upgradePParams alonzoUpgrade maryPP

bigMem :: Natural
bigMem = 50000

bigStep :: Natural
bigStep = 99999

genMaxTxExUnits :: Gen ExUnits
genMaxTxExUnits =
  ExUnits
    -- Accommodate the maximum number of scripts in a transaction
    <$> genNatural (10 * bigMem + 1) (20 * bigMem + 1)
    <*> genNatural (10 * bigStep + 1) (20 * bigStep + 1)

genMaxBlockExUnits :: Gen ExUnits
genMaxBlockExUnits =
  ExUnits
    -- Accommodate the maximum number of scripts in all transactions in a block
    <$> genNatural (60 * bigMem + 1) (100 * bigMem + 1)
    <*> genNatural (60 * bigStep + 1) (100 * bigStep + 1)

instance EraGen AlonzoEra where
  genEraAuxiliaryData = genAux
  genGenesisValue = maryGenesisValue
  genEraTwoPhase3Arg = phase2scripts3Arg
  genEraTwoPhase2Arg = phase2scripts2Arg

  genEraTxBody = genAlonzoTxBody
  updateEraTxBody utxo pp wits txb coinx txin txout =
    txb
      { atbInputs = newInputs
      , atbCollateral = newCollaterals
      , atbTxFee = coinx
      , atbOutputs = newOutputs
      , -- The wits may have changed, recompute the scriptIntegrityHash.
        atbScriptIntegrityHash =
          hashScriptIntegrity
            langViews
            (wits ^. rdmrsTxWitsL)
            (wits ^. datsTxWitsL)
      }
    where
      langs = langsUsed @AlonzoEra (wits ^. scriptTxWitsL)
      langViews = Set.map (getLanguageView pp) langs
      requiredCollateral = ceiling $ fromIntegral (pp ^. ppCollateralPercentageL) * unCoin coinx % 100
      potentialCollateral = Set.filter (okAsCollateral utxo) txin
      txInAmounts = List.sortOn snd . Map.toList . Map.map (unCoin . view coinTxOutL) . unUTxO . txInsFilter utxo
      takeUntilSum s = map fst . takeUntil ((s >=) . snd) . scanl1 (\(_, s') (x, n) -> (x, s' + n))
      takeUntil p xs = let (y, n) = span p xs in y ++ take 1 n
      newCollaterals =
        if null $ wits ^. rdmrsTxWitsL . unRedeemersL
          then mempty
          else Set.fromList . takeUntilSum requiredCollateral $ txInAmounts potentialCollateral
      newInputs = atbInputs txb <> txin
      newOutputs = atbOutputs txb :|> txout

  addInputs txb txin = txb {atbInputs = atbInputs txb <> txin}

  genEraPParamsUpdate = genAlonzoPParamsUpdate
  genEraPParams = genAlonzoPParams
  genEraTxWits (utxo, txbody, scriptinfo) setWitVKey mapScriptWit = new
    where
      new =
        AlonzoTxWits
          setWitVKey
          Set.empty
          mapScriptWit
          -- (dataMapFromTxOut (Prelude.foldr (:) [] (outputs' txbody)) (TxDats (getDataMap scriptinfo mapScriptWit)))
          (dataMapFromTxOut smallUtxo (TxDats (getDataMap scriptinfo mapScriptWit)))
          -- The data hashes come from two places
          (Redeemers rdmrMap)
      txinputs = txbody ^. inputsTxBodyL
      smallUtxo :: [TxOut AlonzoEra]
      smallUtxo = Map.elems (unUTxO (txInsFilter utxo txinputs))
      AlonzoScriptsNeeded purposeHashPairs = getScriptsNeeded @AlonzoEra utxo txbody
      rdmrMap = List.foldl' accum Map.empty purposeHashPairs -- Search through the pairs for Plutus scripts
      accum ans (purpose, hash1) =
        case Map.lookup hash1 mapScriptWit of
          Nothing -> ans
          Just script ->
            if isNativeScript @AlonzoEra script
              then ans -- Native scripts don't have redeemers
              else case Map.lookup hash1 (fst scriptinfo) of -- It could be one of the known 3-Arg Plutus Scripts
                Just info -> addRedeemMap (getRedeemer3 info) purpose ans -- Add it to the redeemer map
                Nothing -> case Map.lookup hash1 (snd scriptinfo) of -- It could be one of the known 2-Arg Plutus Scripts
                  Just info -> addRedeemMap (getRedeemer2 info) purpose ans -- Add it to the redeemer map
                  Nothing -> ans

  constructTx bod wit auxdata = MkAlonzoTx $ AlonzoTx bod wit (IsValid v) auxdata
    where
      v = all twoPhaseValidates (txscripts wit)
      twoPhaseValidates script =
        isNativeScript @AlonzoEra script
          || (phase2scripts3ArgSucceeds script && phase2scripts2ArgSucceeds script)

  genEraGoodTxOut = vKeyLockedAdaOnly

  genEraScriptCost pp script =
    if isPlutusScript script
      then case List.find (\info -> getScript3 @AlonzoEra info == script) genEraTwoPhase3Arg of
        Just (TwoPhase3ArgInfo _script _hash inputdata (rdmr, mems, steps) _succeed) ->
          txscriptfee (pp ^. ppPricesL) (ExUnits mems steps)
            <+> storageCost 10 pp (rdmr, ExUnits mems steps) -- Extra 10 for the RdmrPtr
            <+> storageCost 32 pp inputdata -- Extra 32 for the hash
            <+> storageCost 0 pp script
        Nothing -> storageCost 0 pp script
      else storageCost 0 pp script

  -- For some reason, the EraGen generators occasionally generate an extra script witness.
  --    There is some evidence that this arises because the script hash appears as the PolicyId
  --    in a Value. But that is not been verified. Regardless of the cause, we can fix this by
  --    discarding the trace. Note that this is failure to generate a "random" but valid
  --    transaction. Discarding the trace adjust for this inadequacy in the generation process.
  --    This only appears in the Alonzo era, so this "fix" is applied here, in the genEraDone
  --    method of the EraGen class in the AlonzoEra instance.
  genEraDone utxo pp tx =
    let theFee = tx ^. bodyTxL . feeTxBodyL -- Coin supplied to pay fees
        minimumFee = getMinFeeTxUtxo @AlonzoEra pp tx utxo
        neededHashes = getScriptsHashesNeeded $ getScriptsNeeded utxo (tx ^. bodyTxL)
        oldScriptWits = tx ^. witsTxL . scriptTxWitsL
        newWits = oldScriptWits `Map.restrictKeys` neededHashes
     in if minimumFee <= theFee
          then
            if oldScriptWits == newWits
              then pure tx
              else tracedDiscard $ "Random extra scriptwitness: genEraDone: " <> show newWits
          else tracedDiscard $ "MinFee violation: genEraDone: " <> show theFee

  genEraTweakBlock pp txns =
    let txTotal, ppMax :: ExUnits
        txTotal = foldMap totExUnits txns
        ppMax = pp ^. ppMaxBlockExUnitsL
     in if pointWiseExUnits (<=) txTotal ppMax
          then pure txns
          else
            tracedDiscard $
              "TotExUnits violation: genEraTweakBlock: "
                <> show (unWrapExUnits txTotal)
                <> " instead of "
                <> show (unWrapExUnits ppMax)

  hasFailedScripts tx = IsValid False == tx ^. isValidTxL

  feeOrCollateral tx utxo =
    case tx ^. isValidTxL of
      IsValid True -> tx ^. bodyTxL . feeTxBodyL
      IsValid False -> sumCollateral tx utxo

sumCollateral :: (EraTx era, AlonzoEraTxBody era) => Tx era -> UTxO era -> Coin
sumCollateral tx utxo =
  sumCoinUTxO $ txInsFilter utxo $ tx ^. bodyTxL . collateralInputsTxBodyL

storageCost :: forall era t. (EraPParams era, EncCBOR t) => Integer -> PParams era -> t -> Coin
storageCost extra pp x = (extra + encodedLen @era x) <×> pp ^. ppMinFeeAL

addRedeemMap ::
  (P.Data, Natural, Natural) ->
  AlonzoPlutusPurpose AsIxItem AlonzoEra ->
  Map (AlonzoPlutusPurpose AsIx AlonzoEra) (Data AlonzoEra, ExUnits) ->
  Map (AlonzoPlutusPurpose AsIx AlonzoEra) (Data AlonzoEra, ExUnits)
addRedeemMap (dat, space, steps) purpose ans =
  let ptr = hoistPlutusPurpose toAsIx purpose
   in Map.insert ptr (Data dat, ExUnits space steps) ans

getDataMap ::
  forall era.
  Era era =>
  ScriptInfo era ->
  Map ScriptHash (Script era) ->
  Map DataHash (Data era)
getDataMap (scriptInfo3, _) = Map.foldlWithKey' accum Map.empty
  where
    accum ans hsh _script =
      case Map.lookup hsh scriptInfo3 of
        Nothing -> ans
        Just (TwoPhase3ArgInfo _script _hash dat _redeem _) ->
          Map.insert (hashData dat) (Data dat) ans

instance MinGenTxout AlonzoEra where
  calcEraMinUTxO txOut pp = utxoEntrySize txOut <×> unCoinPerWord (pp ^. ppCoinsPerUTxOWordL)
  addValToTxOut v (AlonzoTxOut a u _b) = AlonzoTxOut a (v <+> u) (dataFromAddr a)
  genEraTxOut genv genVal addrs = do
    values <- replicateM (length addrs) genVal
    let makeTxOut addr val =
          case addr of
            Addr _network (ScriptHashObj shash) _stakeref ->
              AlonzoTxOut addr val $ snd $ findPlutus genv shash
            _ -> AlonzoTxOut addr val SNothing
    pure (zipWith makeTxOut addrs values)

-- | If an Address is script address, we can find a potential data hash for it from
--   genEraTwoPhase3Arg, which contains all known 3 arg plutus scripts in the tests set.
-- If the script has is not in that map, then its data hash is SNothing.
dataFromAddr :: Addr -> StrictMaybe DataHash
dataFromAddr (Addr _network (ScriptHashObj shash) _stakeref) =
  let f info = shash == hashScript @AlonzoEra (getScript3 @AlonzoEra info)
   in case List.find f genEraTwoPhase3Arg of
        Just info -> SJust (hashData (getData3 info))
        Nothing -> SNothing
dataFromAddr _ = SNothing

-- | We can find the data associated with the data hashes in the TxOuts, since
--   genEraTwoPhase3Arg, which contains all known 3 arg plutus scripts stores the data.
dataMapFromTxOut ::
  [TxOut AlonzoEra] ->
  TxDats AlonzoEra ->
  TxDats AlonzoEra
dataMapFromTxOut txouts datahashmap = F.foldl' accum datahashmap txouts
  where
    f dhash info = hashData (getData3 info) == dhash
    accum !ans (AlonzoTxOut _ _ SNothing) = ans
    accum ans@(TxDats m) (AlonzoTxOut _ _ (SJust dhash)) =
      case List.find (f dhash) (genEraTwoPhase3Arg @AlonzoEra) of
        Just info -> TxDats (Map.insert dhash (Data (getData3 info)) m)
        Nothing -> ans

addMaybeDataHashToTxOut :: TxOut AlonzoEra -> TxOut AlonzoEra
addMaybeDataHashToTxOut (AlonzoTxOut addr val _) = AlonzoTxOut addr val (dataFromAddr addr)

someLeaf ::
  forall era.
  ( AllegraEraScript era
  , NativeScript era ~ Timelock era
  ) =>
  Proxy era ->
  KeyHash 'Witness ->
  AlonzoScript era
someLeaf _proxy keyHash =
  let
    -- We use KeyHash as a source of entropy for initialization of an StdGen for
    -- generating slot and mode
    (s, g) = uniformR (0, 199) $ mkHashStdGen keyHash
    slot = SlotNo s
    (mode, _) = uniformR (0 :: Int, 2) g -- mode==0 is a time leaf,  mode 1 or 2 is a signature leaf
   in
    case mode of
      0 ->
        TimelockScript $
          (RequireAnyOf . Seq.fromList) [RequireTimeStart slot, RequireTimeExpire slot]
      _ -> TimelockScript $ RequireSignature keyHash

-- | given the "txscripts" field of the TxWits, compute the set of languages used in a transaction
langsUsed ::
  AlonzoEraScript era =>
  Map.Map ScriptHash (Script era) ->
  Set Language
langsUsed hashScriptMap =
  Set.fromList
    [ plutusScriptLanguage plutusScript
    | script <- Map.elems hashScriptMap
    , Just plutusScript <- [toPlutusScript script]
    ]
