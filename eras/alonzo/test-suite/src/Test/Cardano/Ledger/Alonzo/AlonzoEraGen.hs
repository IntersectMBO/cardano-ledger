{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

-- |  AlonzoEra instances for EraGen and ScriptClass
module Test.Cardano.Ledger.Alonzo.AlonzoEraGen where

import Cardano.Binary (ToCBOR (toCBOR), serializeEncoding')
import Cardano.Ledger.Address (Addr (..))
import Cardano.Ledger.Alonzo (AlonzoEra)
import Cardano.Ledger.Alonzo.Data (AlonzoAuxiliaryData (..), Data (..))
import Cardano.Ledger.Alonzo.Language (Language (..))
import Cardano.Ledger.Alonzo.PParams (AlonzoPParams, AlonzoPParamsHKD (..), extendPP, getLanguageView, retractPP)
import Cardano.Ledger.Alonzo.PlutusScriptApi (scriptsNeededFromBody)
import Cardano.Ledger.Alonzo.PlutusScriptApi as Alonzo (language)
import Cardano.Ledger.Alonzo.Rules (utxoEntrySize, vKeyLocked)
import Cardano.Ledger.Alonzo.Scripts (isPlutusScript, pointWiseExUnits, txscriptfee)
import Cardano.Ledger.Alonzo.Scripts as Alonzo
  ( AlonzoScript (..),
    CostModel,
    CostModels (..),
    ExUnits (..),
    Prices (..),
    mkCostModel,
  )
import Cardano.Ledger.Alonzo.Tx
  ( AlonzoEraTx (..),
    AlonzoTx (..),
    IsValid (..),
    ScriptPurpose (..),
    hashScriptIntegrity,
    minfee,
    rdptr,
    totExUnits,
  )
import Cardano.Ledger.Alonzo.TxBody
  ( AlonzoEraTxBody (..),
    AlonzoTxBody (..),
    AlonzoTxOut (..),
    inputs',
  )
import Cardano.Ledger.Alonzo.TxWitness
  ( AlonzoEraWitnesses (..),
    RdmrPtr (..),
    Redeemers (..),
    TxDats (..),
    TxWitness (..),
  )
import Cardano.Ledger.AuxiliaryData (AuxiliaryDataHash)
import Cardano.Ledger.BaseTypes (Network (..), StrictMaybe (..))
import Cardano.Ledger.Coin (Coin (..))
import Cardano.Ledger.Core
import Cardano.Ledger.Credential (Credential (..))
import qualified Cardano.Ledger.Crypto as CC
import Cardano.Ledger.Keys (KeyHash, KeyRole (Witness))
import Cardano.Ledger.Mary (MaryEra)
import Cardano.Ledger.Mary.Value (AssetName (..), MaryValue, PolicyID (..), policies, valueFromList)
import Cardano.Ledger.Pretty.Alonzo ()
import Cardano.Ledger.Shelley.PParams (Update)
import Cardano.Ledger.Shelley.TxBody (DCert, Wdrl)
import Cardano.Ledger.Shelley.UTxO (UTxO (..), balance)
import Cardano.Ledger.ShelleyMA.AuxiliaryData (MAAuxiliaryData (..))
import Cardano.Ledger.ShelleyMA.Timelocks (Timelock (..))
import Cardano.Ledger.TxIn (TxIn)
import Cardano.Ledger.Val (Val (coin, isAdaOnly, (<+>), (<×>)))
import Cardano.Slotting.Slot (SlotNo (..))
import Control.Monad (replicateM)
import Data.Either (fromRight)
import Data.Hashable (Hashable (..))
import qualified Data.List as List
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Proxy (Proxy (..))
import Data.Ratio ((%))
import Data.Sequence.Strict (StrictSeq ((:|>)))
import qualified Data.Sequence.Strict as Seq (fromList)
import Data.Set as Set
import GHC.Records (HasField (..))
import Lens.Micro
import Numeric.Natural (Natural)
import Plutus.V1.Ledger.Api (costModelParamNames)
import qualified PlutusTx as P (Data (..))
import qualified PlutusTx as Plutus
import Test.Cardano.Ledger.AllegraEraGen (genValidityInterval)
import Test.Cardano.Ledger.Alonzo.PlutusScripts
  ( evenRedeemer2,
    evendata3,
    guessTheNumber3,
    oddRedeemer2,
    odddata3,
    redeemerIs102,
    sumsTo103,
  )
import Test.Cardano.Ledger.Alonzo.Scripts (alwaysFails, alwaysSucceeds)
import Test.Cardano.Ledger.MaryEraGen (addTokens, genMint, maryGenesisValue, policyIndex)
import Test.Cardano.Ledger.Shelley.ConcreteCryptoTypes (Mock)
import Test.Cardano.Ledger.Shelley.Generator.Constants (Constants (..))
import Test.Cardano.Ledger.Shelley.Generator.Core
  ( GenEnv (..),
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
import Test.Cardano.Ledger.Shelley.Generator.Utxo (encodedLen, myDiscard)
import Test.Cardano.Ledger.Shelley.Utils (unsafeBoundRational)
import Test.QuickCheck hiding ((><))

-- ============================================================

-- | We are choosing new TxOut to pay fees, We want only Key locked addresss with Ada only values.
vKeyLockedAdaOnly :: Mock c => TxOut (AlonzoEra c) -> Bool
vKeyLockedAdaOnly txOut = vKeyLocked txOut && isAdaOnly (txOut ^. valueTxOutL)

phase2scripts3Arg :: forall c. Mock c => [TwoPhase3ArgInfo (AlonzoEra c)]
phase2scripts3Arg =
  [ TwoPhase3ArgInfo
      (alwaysSucceeds PlutusV1 3)
      (hashScript @(AlonzoEra c) (alwaysSucceeds PlutusV1 3))
      (P.I 1)
      (P.I 1, bigMem, bigStep)
      True,
    TwoPhase3ArgInfo guessTheNumber3 (hashScript @(AlonzoEra c) guessTheNumber3) (P.I 9) (P.I 9, bigMem, bigStep) True,
    TwoPhase3ArgInfo evendata3 (hashScript @(AlonzoEra c) evendata3) (P.I 8) (P.I 8, bigMem, bigStep) True,
    TwoPhase3ArgInfo odddata3 (hashScript @(AlonzoEra c) odddata3) (P.I 9) (P.I 9, bigMem, bigStep) True,
    TwoPhase3ArgInfo sumsTo103 (hashScript @(AlonzoEra c) sumsTo103) (P.I 1) (P.I 9, bigMem, bigStep) True,
    TwoPhase3ArgInfo
      (alwaysFails PlutusV1 3)
      (hashScript @(AlonzoEra c) (alwaysFails PlutusV1 3))
      (P.I 1)
      (P.I 1, bigMem, bigStep)
      False
  ]

phase2scripts2Arg :: forall c. Mock c => [TwoPhase2ArgInfo (AlonzoEra c)]
phase2scripts2Arg =
  [ TwoPhase2ArgInfo
      (alwaysSucceeds PlutusV1 2)
      (hashScript @(AlonzoEra c) (alwaysSucceeds PlutusV1 2))
      (P.I 1, bigMem, bigStep)
      True,
    TwoPhase2ArgInfo oddRedeemer2 (hashScript @(AlonzoEra c) oddRedeemer2) (P.I 13, bigMem, bigStep) True,
    TwoPhase2ArgInfo evenRedeemer2 (hashScript @(AlonzoEra c) evenRedeemer2) (P.I 14, bigMem, bigStep) True,
    TwoPhase2ArgInfo redeemerIs102 (hashScript @(AlonzoEra c) redeemerIs102) (P.I 10, bigMem, bigStep) True,
    TwoPhase2ArgInfo (alwaysFails PlutusV1 2) (hashScript @(AlonzoEra c) (alwaysFails PlutusV1 2)) (P.I 1, bigMem, bigStep) False
  ]

phase2scripts3ArgSucceeds :: forall c. Mock c => AlonzoScript (AlonzoEra c) -> Bool
phase2scripts3ArgSucceeds script =
  maybe True getSucceeds3 $
    List.find (\info -> getScript3 @(AlonzoEra c) info == script) phase2scripts3Arg

phase2scripts2ArgSucceeds :: forall c. Mock c => AlonzoScript (AlonzoEra c) -> Bool
phase2scripts2ArgSucceeds script =
  maybe True getSucceeds2 $
    List.find (\info -> getScript2 @(AlonzoEra c) info == script) phase2scripts2Arg

genPlutus2Arg :: Mock c => Gen (Maybe (TwoPhase2ArgInfo (AlonzoEra c)))
genPlutus2Arg = frequency [(10, Just <$> elements phase2scripts2Arg), (90, pure Nothing)]

-- | Gen a Mint value in the Alonzo Era, with a 10% chance that it includes an AlonzoScript
genAlonzoMint :: Mock c => MaryValue c -> Gen (MaryValue c, [AlonzoScript (AlonzoEra c)])
genAlonzoMint startvalue = do
  ans <- genPlutus2Arg
  case ans of
    Nothing -> pure (startvalue, [])
    Just (TwoPhase2ArgInfo script shash _ _) -> do
      count <- chooseEnum (1, 10)
      let assetname = AssetName "purple"
      pure (valueFromList 0 [(PolicyID shash, assetname, count)] <> startvalue, [script])

-- ================================================================

-- | A cost model that sets everything as being free
freeCostModel :: CostModel
freeCostModel =
  fromRight (error "freeCostModel is not well-formed") $
    Alonzo.mkCostModel PlutusV1 $ Map.fromSet (const 0) costModelParamNames

-- ================================================================

genPair :: Gen a -> Gen b -> Gen (a, b)
genPair x y = do a <- x; b <- y; pure (a, b)

genPlutusData :: Gen Plutus.Data
genPlutusData = resize 5 (sized gendata)
  where
    gendata n
      | n > 0 =
          oneof
            [ Plutus.I <$> arbitrary,
              Plutus.B <$> arbitrary,
              Plutus.Map <$> listOf (genPair (gendata (n `div` 2)) (gendata (n `div` 2))),
              Plutus.Constr <$> arbitrary <*> listOf (gendata (n `div` 2)),
              Plutus.List <$> listOf (gendata (n `div` 2))
            ]
    gendata _ = oneof [Plutus.I <$> arbitrary, Plutus.B <$> arbitrary]

genSet :: Ord a => Gen a -> Gen (Set a)
genSet gen =
  frequency
    [ (1, pure Set.empty),
      (2, Set.fromList <$> sequence [gen]),
      (1, Set.fromList <$> sequence [gen, gen])
    ]

genAux :: forall c. Mock c => Constants -> Gen (StrictMaybe (AlonzoAuxiliaryData (AlonzoEra c)))
genAux constants = do
  maybeAux <- genEraAuxiliaryData @(MaryEra c) constants
  pure $
    fmap
      (\(MAAuxiliaryData x y) -> AlonzoAuxiliaryData x (TimelockScript <$> y))
      maybeAux

instance CC.Crypto c => ScriptClass (AlonzoEra c) where
  basescript proxy key = someLeaf proxy key
  isKey _ (TimelockScript x) = isKey (Proxy @(MaryEra c)) x
  isKey _ (PlutusScript _ _) = Nothing
  isOnePhase _ (TimelockScript _) = True
  isOnePhase _ (PlutusScript _ _) = False
  quantify _ (TimelockScript x) = fmap TimelockScript (quantify (Proxy @(MaryEra c)) x)
  quantify _ x = Leaf x
  unQuantify _ quant = TimelockScript $ unQuantify (Proxy @(MaryEra c)) (fmap unTime quant)

unTime :: AlonzoScript era -> Timelock (Crypto era)
unTime (TimelockScript x) = x
unTime (PlutusScript _ _) = error "Plutus in Timelock"

okAsCollateral :: forall c. Mock c => UTxO (AlonzoEra c) -> TxIn c -> Bool
okAsCollateral utxo inputx =
  maybe False vKeyLockedAdaOnly $ Map.lookup inputx (unUTxO utxo)

genAlonzoTxBody ::
  forall c.
  Mock c =>
  GenEnv (AlonzoEra c) ->
  UTxO (AlonzoEra c) ->
  PParams (AlonzoEra c) ->
  SlotNo ->
  Set.Set (TxIn c) ->
  StrictSeq (TxOut (AlonzoEra c)) ->
  StrictSeq (DCert c) ->
  Wdrl c ->
  Coin ->
  StrictMaybe (Update (AlonzoEra c)) ->
  StrictMaybe (AuxiliaryDataHash c) ->
  Gen (TxBody (AlonzoEra c), [Script (AlonzoEra c)])
genAlonzoTxBody _genenv utxo pparams currentslot input txOuts certs wdrls fee updates auxDHash = do
  netid <- genM $ pure Testnet -- frequency [(2, pure Mainnet), (1, pure Testnet)]
  startvalue <- genMint
  (minted, plutusScripts) <- genAlonzoMint startvalue
  let (minted2, txouts2) = case addTokens (Proxy @(AlonzoEra c)) mempty pparams minted txOuts of
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
        wdrls
        fee
        validityInterval -- (ValidityInterval SNothing SNothing) -- (ValidityInterval low high)
        updates
        -- reqSignerHashes
        Set.empty -- TODO do something better here
        minted2
        -- scriptIntegrityHash starts out with empty Redeemers,
        -- as Remdeemers are added it is recomputed in updateEraTxBody
        (hashScriptIntegrity @(AlonzoEra c) Set.empty (Redeemers Map.empty) (TxDats Map.empty))
        auxDHash
        netid,
      List.map TimelockScript scriptsFromPolicies <> plutusScripts
    )

genSlotAfter :: SlotNo -> Gen SlotNo
genSlotAfter currentSlot = do
  ttl <- genNatural 50 100
  pure $ currentSlot + SlotNo (fromIntegral ttl)

-- | Gen an Alonzo PParamsUpdate, by adding to a Shelley PParamsData
genAlonzoPParamsUpdate ::
  forall c.
  Constants ->
  AlonzoPParams (AlonzoEra c) ->
  Gen (PParamsUpdate (AlonzoEra c))
genAlonzoPParamsUpdate constants pp = do
  shelleypp <- genShelleyPParamsUpdate @(MaryEra c) constants (retractPP (Coin 100) pp)
  ada <- genM (Coin <$> choose (1, 5))
  let genPrice = unsafeBoundRational . (% 100) <$> choose (0, 200)
  price <- genM (Prices <$> genPrice <*> genPrice)
  let mxTx = SNothing
  -- mxTx <- genM (ExUnits <$> (choose (100, 5000)) <*> (choose (100, 5000)))
  mxBl <- genM (ExUnits <$> genNatural 100 5000 <*> genNatural 100 5000)
  -- Not too small for mxV, if this is too small then any Tx with Value
  -- that has lots of policyIds will fail. The Shelley Era uses hard coded 4000
  mxV <- genM (genNatural 4000 5000)
  let cost = SJust . CostModels $ Map.singleton PlutusV1 freeCostModel
      c = SJust 25 -- percent of fee in collateral
      mxC = SJust 100 -- max number of inputs in collateral
  pure (extendPP shelleypp ada cost price mxTx mxBl mxV c mxC)

genAlonzoPParams ::
  forall c.
  Constants ->
  Gen (PParams (AlonzoEra c))
genAlonzoPParams constants = do
  let constants' = constants {minMajorPV = 5}
  -- This ensures that "_d" field is not 0, and that the major protocol version
  -- is large enough to not trigger plutus script failures
  -- (no bultins are alllowed before major version 5).
  shelleypp <- Shelley.genPParams @(MaryEra c) constants'
  ada <- Coin <$> choose (1, 5)
  let price = Prices minBound minBound
  -- price <- Prices <$> (Coin <$> choose (100, 5000)) <*> (Coin <$> choose (100, 5000))
  let mxTx = ExUnits (5 * bigMem + 1) (5 * bigStep + 1)
  -- mxTx <- ExUnits <$> (choose (100, 5000)) <*> (choose (100, 5000))
  mxBl <-
    ExUnits <$> genNatural (20 * bigMem + 1) (30 * bigMem + 1)
      <*> genNatural (20 * bigStep + 1) (30 * bigStep + 1)
  mxV <- genNatural 4000 10000 -- This can't be too small. Shelley uses Hard coded 4000
  let cost = CostModels $ Map.singleton PlutusV1 freeCostModel
      c = 25 -- percent of fee in collateral
      mxC = 100 -- max number of inputs in collateral
  pure (extendPP shelleypp ada cost price mxTx mxBl mxV c mxC)

-- | Since Alonzo PParams don't have this field, we have to compute something here.
instance HasField "_minUTxOValue" (AlonzoPParams (AlonzoEra c)) Coin where
  getField _ = Coin 4000

bigMem :: Natural
bigMem = 50000

bigStep :: Natural
bigStep = 99999

instance Mock c => EraGen (AlonzoEra c) where
  genEraAuxiliaryData = genAux
  genGenesisValue = maryGenesisValue
  genEraTwoPhase3Arg = phase2scripts3Arg
  genEraTwoPhase2Arg = phase2scripts2Arg

  genEraTxBody = genAlonzoTxBody
  updateEraTxBody utxo pp witnesses txb coinx txin txout = new
    where
      langs = langsUsed @(AlonzoEra c) (witnesses ^. scriptWitsL)
      langViews = Set.map (getLanguageView pp) langs
      new =
        txb
          { inputs = inputs txb <> txin,
            collateral = collateral txb <> Set.filter (okAsCollateral utxo) txin, -- In Alonzo, extra inputs also are added to collateral
            txfee = coinx,
            outputs = outputs txb :|> txout,
            -- The witnesses may have changed, recompute the scriptIntegrityHash.
            scriptIntegrityHash =
              hashScriptIntegrity
                langViews
                (witnesses ^. rdmrsWitsL)
                (witnesses ^. datsWitsL)
          }

  addInputs txb txin = txb {inputs = inputs txb <> txin}

  genEraPParamsUpdate = genAlonzoPParamsUpdate
  genEraPParams = genAlonzoPParams
  genEraWitnesses (utxo, txbody, scriptinfo) setWitVKey mapScriptWit = new
    where
      new =
        TxWitness
          setWitVKey
          Set.empty
          mapScriptWit
          -- (dataMapFromTxOut (Prelude.foldr (:) [] (outputs' txbody)) (TxDats (getDataMap scriptinfo mapScriptWit)))
          (dataMapFromTxOut smallUtxo (TxDats (getDataMap scriptinfo mapScriptWit)))
          -- The data hashes come from two places
          (Redeemers rdmrMap)
      txinputs = inputs' txbody
      smallUtxo :: [TxOut (AlonzoEra c)]
      smallUtxo = Map.elems (unUTxO utxo `Map.restrictKeys` txinputs)
      purposeHashPairs = scriptsNeededFromBody @(AlonzoEra c) utxo txbody
      rdmrMap = List.foldl' accum Map.empty purposeHashPairs -- Search through the pairs for Plutus scripts
      accum ans (purpose, hash1) =
        case Map.lookup hash1 mapScriptWit of
          Nothing -> ans
          Just script ->
            if isNativeScript @(AlonzoEra c) script
              then ans -- Native scripts don't have redeemers
              else case Map.lookup hash1 (fst scriptinfo) of -- It could be one of the known 3-Arg Plutus Scripts
                Just info -> addRedeemMap txbody (getRedeemer3 info) purpose ans -- Add it to the redeemer map
                Nothing -> case Map.lookup hash1 (snd scriptinfo) of -- It could be one of the known 2-Arg Plutus Scripts
                  Just info -> addRedeemMap txbody (getRedeemer2 info) purpose ans -- Add it to the redeemer map
                  Nothing -> ans

  constructTx bod wit auxdata = AlonzoTx bod wit (IsValid v) auxdata
    where
      v = all twoPhaseValidates (txscripts' wit)
      twoPhaseValidates script =
        isNativeScript @(AlonzoEra c) script
          || (phase2scripts3ArgSucceeds script && phase2scripts2ArgSucceeds script)

  genEraGoodTxOut = vKeyLockedAdaOnly

  genEraScriptCost pp script =
    if isPlutusScript script
      then case List.find (\info -> getScript3 @(AlonzoEra c) info == script) genEraTwoPhase3Arg of
        Just (TwoPhase3ArgInfo _script _hash inputdata (rdmr, mems, steps) _succeed) ->
          txscriptfee (getField @"_prices" pp) (ExUnits mems steps)
            <+> storageCost 10 pp (rdmr, ExUnits mems steps) -- Extra 10 for the RdmrPtr
            <+> storageCost 32 pp inputdata -- Extra 32 for the hash
            <+> storageCost 0 pp script
        Nothing -> storageCost 0 pp script
      else storageCost 0 pp script

  genEraDone pp tx =
    let theFee = tx ^. bodyTxL . feeTxBodyL -- Coin supplied to pay fees
        minimumFee = minfee @(AlonzoEra c) pp tx
     in if minimumFee <= theFee
          then pure tx
          else myDiscard "MinFeee violation: genEraDne: AlonzoEraGen.hs"

  genEraTweakBlock pp txns =
    let txTotal, ppMax :: ExUnits
        txTotal = foldMap totExUnits txns
        ppMax = getField @"_maxBlockExUnits" pp
     in if pointWiseExUnits (<=) txTotal ppMax
          then pure txns
          else myDiscard "TotExUnits violation: genEraTweakBlock: AlonzoEraGen.hs"

  hasFailedScripts tx = IsValid False == tx ^. isValidTxL

  feeOrCollateral tx utxo =
    case tx ^. isValidTxL of
      IsValid True -> tx ^. bodyTxL . feeTxBodyL
      IsValid False -> sumCollateral tx utxo

sumCollateral :: (EraTx era, AlonzoEraTxBody era) => Tx era -> UTxO era -> Coin
sumCollateral tx (UTxO utxo) =
  coin . balance . UTxO $ Map.restrictKeys utxo collateral_
  where
    collateral_ = tx ^. bodyTxL . collateralInputsTxBodyL

storageCost :: ToCBOR t => Integer -> AlonzoPParams era -> t -> Coin
storageCost extra pp x = (extra + encodedLen x) <×> Coin (fromIntegral (getField @"_minfeeA" pp))

addRedeemMap ::
  forall c.
  CC.Crypto c =>
  TxBody (AlonzoEra c) ->
  (Plutus.Data, Natural, Natural) ->
  ScriptPurpose c ->
  Map RdmrPtr (Data (AlonzoEra c), ExUnits) ->
  Map RdmrPtr (Data (AlonzoEra c), ExUnits)
addRedeemMap body1 (dat, space, steps) purpose ans =
  case (purpose, rdptr @(AlonzoEra c) body1 purpose) of
    (Spending _, SJust ptr) -> Map.insert ptr (Data dat, ExUnits space steps) ans
    (Minting _, SJust ptr) -> Map.insert ptr (Data dat, ExUnits space steps) ans
    (Rewarding _, SJust ptr) -> Map.insert ptr (Data dat, ExUnits space steps) ans
    (Certifying _, SJust ptr) -> Map.insert ptr (Data dat, ExUnits space steps) ans
    _ -> ans

getDataMap ::
  forall era.
  Era era =>
  ScriptInfo era ->
  Map (ScriptHash (Crypto era)) (Script era) ->
  Map (DataHash (Crypto era)) (Data era)
getDataMap (scriptInfo3, _) = Map.foldlWithKey' accum Map.empty
  where
    accum ans hsh _script =
      case Map.lookup hsh scriptInfo3 of
        Nothing -> ans
        Just (TwoPhase3ArgInfo _script _hash dat _redeem _) ->
          Map.insert (hashData @era dat) (Data dat) ans

instance Mock c => MinGenTxout (AlonzoEra c) where
  calcEraMinUTxO tout pp = utxoEntrySize tout <×> getField @"_coinsPerUTxOWord" pp
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
dataFromAddr :: forall c. Mock c => Addr c -> StrictMaybe (DataHash c)
dataFromAddr (Addr _network (ScriptHashObj shash) _stakeref) =
  let f info = shash == hashScript @(AlonzoEra c) (getScript3 @(AlonzoEra c) info)
   in case List.find f genEraTwoPhase3Arg of
        Just info -> SJust (hashData @(AlonzoEra c) (getData3 info))
        Nothing -> SNothing
dataFromAddr _ = SNothing

-- | We can find the data associated with the data hashes in the TxOuts, since
--   genEraTwoPhase3Arg, which contains all known 3 arg plutus scripts stores the data.
dataMapFromTxOut ::
  forall c.
  Mock c =>
  [TxOut (AlonzoEra c)] ->
  TxDats (AlonzoEra c) ->
  TxDats (AlonzoEra c)
dataMapFromTxOut txouts datahashmap = Prelude.foldl accum datahashmap txouts
  where
    f dhash info = hashData @(AlonzoEra c) (getData3 info) == dhash
    accum !ans (AlonzoTxOut _ _ SNothing) = ans
    accum ans@(TxDats' m) (AlonzoTxOut _ _ (SJust dhash)) =
      case List.find (f dhash) (genEraTwoPhase3Arg @(AlonzoEra c)) of
        Just info -> TxDats (Map.insert dhash (Data (getData3 info)) m)
        Nothing -> ans

addMaybeDataHashToTxOut :: Mock c => TxOut (AlonzoEra c) -> TxOut (AlonzoEra c)
addMaybeDataHashToTxOut (AlonzoTxOut addr val _) = AlonzoTxOut addr val (dataFromAddr addr)

someLeaf ::
  forall era.
  Era era =>
  Proxy era ->
  KeyHash 'Witness (Crypto era) ->
  AlonzoScript era
someLeaf _proxy x =
  let n = hash (serializeEncoding' (toCBOR x)) -- We don't really care about the hash, we only
      slot = SlotNo (fromIntegral (mod n 200)) -- use it to pseudo-randomly pick a slot and mode
      mode = mod n 3 -- mode==0 is a time leaf,  mode 1 or 2 is a signature leaf
   in case mode of
        0 ->
          TimelockScript $
            (RequireAnyOf . Seq.fromList) [RequireTimeStart slot, RequireTimeExpire slot]
        _ -> TimelockScript $ RequireSignature x

-- | given the "txscripts" field of the TxWits, compute the set of languages used in a transaction
langsUsed ::
  forall era.
  (Script era ~ AlonzoScript era, EraScript era) =>
  Map.Map (ScriptHash (Crypto era)) (AlonzoScript era) ->
  Set Language
langsUsed hashScriptMap =
  Set.fromList
    [ l | (_hash, script) <- Map.toList hashScriptMap, (not . isNativeScript @era) script, Just l <- [language @era script]
    ]
