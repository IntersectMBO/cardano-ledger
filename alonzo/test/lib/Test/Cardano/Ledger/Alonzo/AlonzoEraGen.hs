{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

-- |  AlonzoEra instances for EraGen and ScriptClass
module Test.Cardano.Ledger.Alonzo.AlonzoEraGen where

import Cardano.Binary (ToCBOR (toCBOR), serializeEncoding')
import Cardano.Ledger.Alonzo (AlonzoEra)
import Cardano.Ledger.Alonzo.Data as Alonzo (AuxiliaryData (..), Data (..), DataHash)
import Cardano.Ledger.Alonzo.Language (Language (PlutusV1))
import Cardano.Ledger.Alonzo.PParams (PParams' (..))
import qualified Cardano.Ledger.Alonzo.PParams as Alonzo (PParams, extendPP, retractPP)
import Cardano.Ledger.Alonzo.PlutusScriptApi (scriptsNeededFromBody)
import Cardano.Ledger.Alonzo.Rules.Utxo (utxoEntrySize)
import Cardano.Ledger.Alonzo.Rules.Utxow (langsUsed)
import Cardano.Ledger.Alonzo.Scripts (isPlutusScript, scriptfee)
import Cardano.Ledger.Alonzo.Scripts as Alonzo
  ( CostModel (..),
    ExUnits (..),
    Prices (..),
    Script (..),
    alwaysSucceeds,
  )
import Cardano.Ledger.Alonzo.Tx (IsValidating (..), ScriptPurpose (..), ValidatedTx (..), hashWitnessPPData, rdptr)
import Cardano.Ledger.Alonzo.TxBody (TxBody (..), TxOut (..), inputs')
import Cardano.Ledger.Alonzo.TxWitness (RdmrPtr (..), Redeemers (..), TxDats (..), TxWitness (..))
import Cardano.Ledger.AuxiliaryData (AuxiliaryDataHash)
import Cardano.Ledger.BaseTypes (Network (..), StrictMaybe (..))
import Cardano.Ledger.Coin (Coin (..))
import qualified Cardano.Ledger.Core as Core (PParams, PParamsDelta, Script, TxOut)
import qualified Cardano.Ledger.Crypto as CC
import Cardano.Ledger.Era (Crypto, Era (..), ValidateScript (..))
import Cardano.Ledger.Hashes (ScriptHash)
import Cardano.Ledger.Keys (KeyHash, KeyRole (Witness))
import Cardano.Ledger.Mary (MaryEra)
import Cardano.Ledger.Mary.Value (policies)
import Cardano.Ledger.ShelleyMA.AuxiliaryData as Mary (pattern AuxiliaryData)
import Cardano.Ledger.ShelleyMA.Timelocks (Timelock (..))
import Cardano.Ledger.Tx (Tx (Tx))
import Cardano.Ledger.Val (adaOnly, (<+>), (<×>))
import Cardano.Slotting.Slot (SlotNo (..))
import Control.Iterate.SetAlgebra (eval, (◁))
import Control.Monad (replicateM)
import Data.ByteString.Short (ShortByteString, toShort)
import Data.Hashable (Hashable (..))
import qualified Data.List as List
import Data.Map as Map
import Data.Maybe (fromJust)
import Data.Proxy (Proxy (..))
import Data.Sequence.Strict (StrictSeq ((:|>)))
import qualified Data.Sequence.Strict as Seq (fromList)
import Data.Set as Set
import Data.Word (Word64)
import Flat (flat)
import GHC.Records (HasField (..))
import Plutus.V1.Ledger.Api (defaultCostModelParams)
import qualified Plutus.V1.Ledger.Scripts as P
import qualified PlutusTx as P (Data (..), compile)
import qualified PlutusTx as Plutus
import qualified PlutusTx.Prelude as P
import Shelley.Spec.Ledger.Address (Addr (..))
import Shelley.Spec.Ledger.Credential (Credential (..))
import Shelley.Spec.Ledger.PParams (Update)
import Shelley.Spec.Ledger.TxBody (DCert, TxIn, Wdrl)
import Shelley.Spec.Ledger.UTxO (UTxO (..))
import Test.Cardano.Ledger.AllegraEraGen (genValidityInterval)
import Test.Cardano.Ledger.MaryEraGen (addTokens, genMint, maryGenesisValue, policyIndex)
import Test.QuickCheck hiding ((><))
import Test.Shelley.Spec.Ledger.ConcreteCryptoTypes (Mock)
import Test.Shelley.Spec.Ledger.Generator.Constants (Constants (..))
import Test.Shelley.Spec.Ledger.Generator.Core
  ( GenEnv (..),
    ScriptInfo,
    TwoPhaseInfo (..),
    findPlutus,
    genNatural,
    hashData,
  )
import Test.Shelley.Spec.Ledger.Generator.EraGen (EraGen (..), MinGenTxout (..))
import Test.Shelley.Spec.Ledger.Generator.ScriptClass (Quantifier (..), ScriptClass (..))
import Test.Shelley.Spec.Ledger.Generator.Update (genM, genShelleyPParamsDelta)
import qualified Test.Shelley.Spec.Ledger.Generator.Update as Shelley (genPParams)
import Test.Shelley.Spec.Ledger.Generator.Utxo (encodedLen)

isKeyHashAddr :: Addr crypto -> Bool
isKeyHashAddr (AddrBootstrap _) = True
isKeyHashAddr (Addr _ (KeyHashObj _) _) = True
isKeyHashAddr _ = False

-- | We are choosing new TxOut to pay fees, We want only Key locked addresss with Ada only values.
vKeyLocked :: Mock c => Core.TxOut (AlonzoEra c) -> Bool
vKeyLocked txout =
  isKeyHashAddr (getField @"address" txout)
    && adaOnly (getField @"value" txout)

phase2scripts :: forall c. Mock c => [TwoPhaseInfo (AlonzoEra c)]
phase2scripts =
  [ TwoPhaseInfo (alwaysSucceeds 3) (hashScript @(AlonzoEra c) (alwaysSucceeds 3)) (P.I 1) (P.I 1, bigMem, bigStep),
    TwoPhaseInfo (alwaysSucceeds 3) (hashScript @(AlonzoEra c) (alwaysSucceeds 3)) (P.I 1) (P.I 1, bigMem, bigStep),
    TwoPhaseInfo (alwaysSucceeds 3) (hashScript @(AlonzoEra c) (alwaysSucceeds 3)) (P.I 1) (P.I 1, bigMem, bigStep),
    TwoPhaseInfo (alwaysSucceeds 3) (hashScript @(AlonzoEra c) (alwaysSucceeds 3)) (P.I 1) (P.I 1, bigMem, bigStep),
    TwoPhaseInfo guess (hashScript @(AlonzoEra c) guess) (P.I 9) (P.I 9, bigMem, bigStep),
    TwoPhaseInfo guess (hashScript @(AlonzoEra c) guess) (P.I 9) (P.I 9, bigMem, bigStep)
  ]

-- ================================================================

-- | A cost model that sets everything as being free
freeCostModel :: CostModel
freeCostModel = CostModel $ 0 <$ fromJust defaultCostModelParams

-- ================================================================

genPair :: Gen a -> Gen b -> Gen (a, b)
genPair x y = do a <- x; b <- y; pure (a, b)

genPlutusData :: Gen Plutus.Data
genPlutusData = resize 5 (sized gendata)
  where
    gendata n
      | n > 0 =
        oneof
          [ (Plutus.I <$> arbitrary),
            (Plutus.B <$> arbitrary),
            (Plutus.Map <$> listOf (genPair (gendata (n `div` 2)) (gendata (n `div` 2)))),
            (Plutus.Constr <$> arbitrary <*> listOf (gendata (n `div` 2))),
            (Plutus.List <$> listOf (gendata (n `div` 2)))
          ]
    gendata _ = oneof [Plutus.I <$> arbitrary, Plutus.B <$> arbitrary]

genSet :: Ord a => Gen a -> Gen (Set a)
genSet gen =
  frequency
    [ (1, pure Set.empty),
      (2, Set.fromList <$> sequence [gen]),
      (1, Set.fromList <$> sequence [gen, gen])
    ]

genAux :: forall c. Mock c => Constants -> Gen (StrictMaybe (Alonzo.AuxiliaryData (AlonzoEra c)))
genAux constants =
  do
    maybeAux <- genEraAuxiliaryData @(MaryEra c) constants
    case maybeAux of
      SNothing -> pure SNothing
      SJust (Mary.AuxiliaryData x y) ->
        SJust
          <$> ( Alonzo.AuxiliaryData
                  <$> pure x
                  <*> pure (TimelockScript <$> y)
              )

instance CC.Crypto c => ScriptClass (AlonzoEra c) where
  -- basescript _ key = TimelockScript (basescript (Proxy @(MaryEra c)) key) -- The old style from Mary
  basescript proxy key = (someLeaf proxy key)
  isKey _ (TimelockScript x) = isKey (Proxy @(MaryEra c)) x
  isKey _ (PlutusScript _) = Nothing
  isOnePhase _ (TimelockScript _) = True
  isOnePhase _ (PlutusScript _) = False
  quantify _ (TimelockScript x) = fmap TimelockScript (quantify (Proxy @(MaryEra c)) x)
  quantify _ x = Leaf x
  unQuantify _ quant = TimelockScript $ unQuantify (Proxy @(MaryEra c)) (fmap unTime quant)

unTime :: Alonzo.Script era -> Timelock (Crypto era)
unTime (TimelockScript x) = x
unTime (PlutusScript _) = error "Plutus in Timelock"

okAsCollateral :: forall c. Mock c => UTxO (AlonzoEra c) -> TxIn c -> Bool
okAsCollateral utxo inputx =
  case Map.lookup inputx (unUTxO utxo) of
    Nothing -> False
    Just outputx -> vKeyLocked outputx

genAlonzoTxBody ::
  forall c.
  Mock c =>
  GenEnv (AlonzoEra c) ->
  UTxO (AlonzoEra c) ->
  Core.PParams (AlonzoEra c) ->
  SlotNo ->
  Set.Set (TxIn c) ->
  StrictSeq (TxOut (AlonzoEra c)) ->
  StrictSeq (DCert c) ->
  Wdrl c ->
  Coin ->
  StrictMaybe (Update (AlonzoEra c)) ->
  StrictMaybe (AuxiliaryDataHash c) ->
  Gen (TxBody (AlonzoEra c), [Core.Script (AlonzoEra c)])
genAlonzoTxBody _genenv utxo pparams currentslot input txOuts certs wdrls fee updates auxDHash = do
  _low <- genM (genSlotAfter currentslot)
  _high <- genM (genSlotAfter (currentslot + 50))
  netid <- genM $ pure Testnet -- frequency [(2, pure Mainnet), (1, pure Testnet)]
  minted <- genMint
  let (minted2, txouts2) = case addTokens (Proxy @(AlonzoEra c)) mempty pparams minted txOuts of
        Nothing -> (mempty, txOuts)
        Just os -> (minted, os)
      scriptsFromPolicies = List.map (\p -> (Map.!) policyIndex p) (Set.toList $ policies minted)
      txouts3 = fmap addMaybeDataHashToTxOut txouts2
  validityInterval <- genValidityInterval currentslot
  return
    ( TxBody
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
        -- wppHash starts out with empty Redeemers, as Remdeemers are added it is recomputed in updateEraTxBody
        (hashWitnessPPData pparams (langsUsed @(AlonzoEra c) Map.empty) (Redeemers Map.empty) (TxDats Map.empty))
        auxDHash
        netid,
      List.map TimelockScript scriptsFromPolicies
    )

genSlotAfter :: SlotNo -> Gen SlotNo
genSlotAfter currentSlot = do
  ttl <- genNatural 50 100
  pure $ currentSlot + SlotNo (fromIntegral ttl)

-- | Gen an Alonzo PParamsDelta, by adding to a Shelley PParamsData
genAlonzoPParamsDelta ::
  forall c.
  Constants ->
  Alonzo.PParams (AlonzoEra c) ->
  Gen (Core.PParamsDelta (AlonzoEra c))
genAlonzoPParamsDelta constants pp = do
  shelleypp <- genShelleyPParamsDelta @(MaryEra c) constants (Alonzo.retractPP (Coin 100) pp)
  ada <- genM (Coin <$> choose (1, 5))
  cost <- genM (pure (Map.singleton PlutusV1 freeCostModel)) -- TODO what is a better assumption for this?
  price <- genM (Prices <$> (Coin <$> choose (0, 2)) <*> (Coin <$> choose (0, 2)))
  mxTx <- pure SNothing -- genM (ExUnits <$> (choose (100, 5000)) <*> (choose (100, 5000)))
  mxBl <- genM (ExUnits <$> (choose (100, 5000)) <*> (choose (100, 5000)))
  -- Not too small for mxV, if this is too small then any Tx with Value
  -- that has lots of policyIds will fail. The Shelley Era uses hard coded 4000
  mxV <- genM (genNatural 4000 5000)
  let c = SJust 25 -- percent of fee in collateral
      mxC = SJust 100 -- max number of inputs in collateral
  pure (Alonzo.extendPP shelleypp ada cost price mxTx mxBl mxV c mxC)

genAlonzoPParams ::
  forall c.
  Constants ->
  Gen (Core.PParams (AlonzoEra c))
genAlonzoPParams constants = do
  shelleypp <- Shelley.genPParams @(MaryEra c) constants -- This ensures that "_d" field is not 0.
  ada <- (Coin <$> choose (1, 5))
  cost <- pure (Map.singleton PlutusV1 freeCostModel) -- There are no other Languages, and there must be something for PlutusV1
  price <- pure (Prices (Coin 0) (Coin 0)) -- (Prices <$> (Coin <$> choose (100, 5000)) <*> (Coin <$> choose (100, 5000)))
  mxTx <- pure (ExUnits (5 * bigMem + 1) (5 * bigStep + 1)) -- (ExUnits <$> (choose (100, 5000)) <*> (choose (100, 5000)))
  mxBl <- (ExUnits <$> (choose ((20 * bigMem + 1), (30 * bigMem + 1))) <*> choose ((20 * bigStep + 1), (30 * bigStep + 1)))
  mxV <- (genNatural 4000 10000) -- This can't be too small. Shelley uses Hard coded 4000
  let c = 25 -- percent of fee in collateral
      mxC = 100 -- max number of inputs in collateral
  pure (Alonzo.extendPP shelleypp ada cost price mxTx mxBl mxV c mxC)

-- | Since Alonzo PParams don't have this field, we have to compute something here.
instance HasField "_minUTxOValue" (Alonzo.PParams (AlonzoEra c)) Coin where
  getField _ = Coin 4000

bigStep, bigMem :: Word64
bigStep = 999 -- 9999999990
bigMem = 500 -- 50000000

instance Mock c => EraGen (AlonzoEra c) where
  genEraAuxiliaryData = genAux
  genGenesisValue = maryGenesisValue
  genEraTwoPhaseScripts = phase2scripts

  genEraTxBody = genAlonzoTxBody
  updateEraTxBody utxo pp witnesses txb coinx txin txout = new
    where
      new =
        txb
          { inputs = (inputs txb) <> txin,
            collateral = (collateral txb) <> Set.filter (okAsCollateral utxo) txin, -- In Alonzo, extra inputs also are added to collateral
            txfee = coinx,
            outputs = (outputs txb) :|> txout,
            -- The witnesses may have changed, recompute the wpphash.
            wppHash =
              hashWitnessPPData
                pp
                (langsUsed @(AlonzoEra c) (getField @"txscripts" witnesses))
                (getField @"txrdmrs" witnesses)
                (getField @"txdats" witnesses)
          }

  addInputs txb txin = txb {inputs = (inputs txb) <> txin}

  genEraPParamsDelta = genAlonzoPParamsDelta
  genEraPParams = genAlonzoPParams
  genEraWitnesses (utxo, txbody, scriptinfo) setWitVKey mapScriptWit = new
    where
      new =
        TxWitness
          setWitVKey
          Set.empty
          mapScriptWit
          -- (dataMapFromTxOut (Prelude.foldr (:) [] (outputs' txbody)) (TxDats (getDataMap scriptinfo mapScriptWit)))
          (dataMapFromTxOut (Prelude.foldr (:) [] (Map.elems smallUtxo)) (TxDats (getDataMap scriptinfo mapScriptWit)))
          -- The data hashes come from two places
          (Redeemers rdmrMap)
      txinputs = inputs' txbody
      smallUtxo = eval (txinputs ◁ utxo) :: Map.Map (TxIn c) (Core.TxOut (AlonzoEra c))
      purposeHashPairs = scriptsNeededFromBody @(AlonzoEra c) utxo txbody
      rdmrMap = List.foldl' accum Map.empty purposeHashPairs -- Search through the pairs for Plutus scripts
      accum ans (purpose, hash1) =
        case Map.lookup hash1 mapScriptWit of
          Nothing -> ans
          Just script ->
            if isNativeScript @(AlonzoEra c) script
              then ans -- Native scripts don't have redeemers
              else case Map.lookup hash1 scriptinfo of -- It should be one of the known Plutus Scripts
                Nothing -> ans
                Just info -> addRedeemMap txbody info purpose ans -- Add it to the redeemer map

  unsafeApplyTx (Tx bod wit auxdata) = ValidatedTx bod wit (IsValidating True) auxdata

  genEraGoodTxOut = vKeyLocked
  genEraScriptCost pp script =
    if isPlutusScript script
      then case List.find (\info -> (getScript @(AlonzoEra c) info) == script) genEraTwoPhaseScripts of
        Just (TwoPhaseInfo _script _hash inputdata (rdmr, mems, steps)) ->
          scriptfee (getField @"_prices" pp) (ExUnits mems steps)
            <+> storageCost 10 pp (rdmr, ExUnits mems steps) -- Extra 10 for the RdmrPtr
            <+> storageCost 32 pp inputdata -- Extra 32 for the hash
        Nothing -> Coin 0
      else Coin 0

  genEraDone x = x -- ptrace "\nDone " x x

storageCost :: ToCBOR t => Integer -> (Alonzo.PParams era) -> t -> Coin
storageCost extra pp x = (extra + encodedLen x) <×> Coin (fromIntegral (getField @"_minfeeA" pp))

addRedeemMap ::
  forall c.
  TxBody (AlonzoEra c) ->
  TwoPhaseInfo (AlonzoEra c) ->
  ScriptPurpose c ->
  Map RdmrPtr (Data (AlonzoEra c), ExUnits) ->
  Map RdmrPtr (Data (AlonzoEra c), ExUnits)
addRedeemMap body1 (TwoPhaseInfo _ _ _ (dat, space, steps)) purpose ans =
  case (purpose, rdptr @(AlonzoEra c) body1 purpose) of
    (Spending _, SJust ptr) -> Map.insert ptr (Data dat, ExUnits space steps) ans
    (Minting _, SJust ptr) -> Map.insert ptr (Data dat, ExUnits space steps) ans
    (Rewarding _, SJust ptr) -> Map.insert ptr (Data dat, ExUnits space steps) ans
    (Certifying _, SJust ptr) -> Map.insert ptr (Data dat, ExUnits space steps) ans
    _ -> ans

getDataMap :: forall era. Era era => ScriptInfo era -> Map (ScriptHash (Crypto era)) (Core.Script era) -> Map (DataHash (Crypto era)) (Data era)
getDataMap scriptinfo scrips = Map.foldlWithKey' accum Map.empty scrips
  where
    accum ans hsh _script =
      case Map.lookup hsh scriptinfo of
        Nothing -> ans
        Just (TwoPhaseInfo _script _hash dat _redeem) -> Map.insert (hashData @era dat) (Data dat) ans

instance Mock c => MinGenTxout (AlonzoEra c) where
  calcEraMinUTxO tout pp = (utxoEntrySize tout <×> getField @"_adaPerUTxOWord" pp)
  addValToTxOut v (TxOut a u _b) = TxOut a (v <+> u) (dataFromAddr a) -- _b
  genEraTxOut genv genVal addrs = do
    values <- (replicateM (length addrs) genVal)
    let makeTxOut (addr@(Addr _network (ScriptHashObj shash) _stakeref)) val = TxOut addr val maybedatahash
          where
            (_, maybedatahash) = findPlutus genv shash
        makeTxOut addr val = TxOut addr val SNothing
    pure (zipWith makeTxOut addrs values)

-- | If an Address is script address, we can find a potential data hash for it from
--   genEraTwoPhaseScripts, which contains all known plutus scripts in the tests set.
-- If the script has is not in that map, then its data hash is SNothing.
dataFromAddr :: forall c. Mock c => Addr c -> StrictMaybe (DataHash c)
dataFromAddr (Addr _network (ScriptHashObj shash) _stakeref) =
  case List.find (\info -> shash == hashScript @(AlonzoEra c) (getScript @(AlonzoEra c) info)) genEraTwoPhaseScripts of
    Just info -> SJust (hashData @(AlonzoEra c) (getData info))
    Nothing -> SNothing
dataFromAddr _ = SNothing

-- | We can find the data associated with the data hashes in the TxOuts, since
--   genEraTwoPhaseScripts, which contains all known plutus scripts stores the data.
dataMapFromTxOut :: forall c. Mock c => [TxOut (AlonzoEra c)] -> TxDats (AlonzoEra c) -> TxDats (AlonzoEra c)
dataMapFromTxOut txouts datahashmap = Prelude.foldl accum datahashmap txouts
  where
    accum !ans (TxOut _ _ SNothing) = ans
    accum !ans (TxOut _ _ (SJust dhash)) =
      case List.find (\info -> hashData @(AlonzoEra c) (getData info) == dhash) (genEraTwoPhaseScripts @(AlonzoEra c)) of
        Just info -> let TxDats' m = ans in TxDats (Map.insert dhash (Data (getData info)) m)
        Nothing -> ans

addMaybeDataHashToTxOut :: Mock c => TxOut (AlonzoEra c) -> TxOut (AlonzoEra c)
addMaybeDataHashToTxOut (TxOut addr val _) = TxOut addr val (dataFromAddr addr)

someLeaf ::
  forall era.
  Era era =>
  Proxy era ->
  KeyHash 'Witness (Crypto era) ->
  Script era
someLeaf _proxy x =
  let n = hash (serializeEncoding' (toCBOR x)) -- We don't really care about the hash, we only
      slot = SlotNo (fromIntegral (mod n 200)) -- use it to pseudo-randomly pick a slot and mode
      mode = mod n 3 -- mode==0 is a time leaf,  mode 1 or 2 is a signature leaf
   in case mode of
        0 -> TimelockScript $ (RequireAnyOf . Seq.fromList) [RequireTimeStart slot, RequireTimeExpire slot]
        _ -> TimelockScript $ RequireSignature x

{-
The bytestring below is a compiled (and then converted to ShortByteString) version of
this PlutusScript. See the source Test.Cardano.Ledger.Alonzo.Examples
It has three arguments 1) the data 2) the redeemer, 3) the context. Designed to be used
as a script locking an output.

guessTheNumber3args' :: P.Data -> P.Data -> P.Data -> ()
guessTheNumber3args' d1 d2 _d3 = if d1 P.== d2 then () else (P.error ())

guessTheNumber3args :: ShortByteString
guessTheNumber3args =
  toShort . flat . P.fromCompiledCode $
    $$(P.compile [||guessTheNumber3args'||])
-}

{-
guessTheNumber3args:: ShortByteString
guessTheNumber3args = read "\SOH\NUL\NUL2\NUL2\NUL2\NUL32\NUL \STX\NUL3 \STX\NUL32\NUL \STX\NUL2\NUL2\NUL2\NUL2\NUL3 \STX\NUL333 \STX\NUL \STX\NUL \STX\NUL2\NUL2\NUL2\NUL2\NUL2\NUL2\NUL\NUL\DC2\NUL \STX\NUL350\NAK35\NULS\NULq \NUL\SOH\NUL0\STX \NUL\SUB \ETXP\ENQ\SOH\160\EM\DC2\NULa \NUL\SOH5\NUL\"\NUL \NUL\SOH5\NUL\DC2\NUL \NUL\STX5UP\SYN\DC2\NUL \STX\NUL3\NUL2\NUL \STX\NUL3350\a\NUL\"\NUL333S\NUL\128\STX \STX\NUL3\SOH0\ETX\NUL\"\NUL \STX\NUL\SOH\130\NUL \NUL\ETB \STX\NUL\SOHr\NUL \NUL\ETB\SOH\130\NUL \ETX3350\t\NUL2\NUL \NUL\CAN \STX\NUL \ETX3S\SOH\131\&0\DC4\NULP\ETX \ETX3S\SOHq \NUL\SOH0\v\DC2\NUL\NUL\DLE\ENQ\NUL2\NUL\SOH\160\FS \STX\NUL\SOH\130\NUL \NUL\CAN \STX\NUL\SOH\128\EM \ETX3350\b\NUL\"\NUL \NUL\ETB \STX\NUL \NUL\CAN \STX\NUL3\SOH \ETX\NUL\"\NUL \NUL\ETB \STX\NUL\SOHp\CAN \ETX3350\b\NUL\"\NUL \NUL\ETB \STX\NUL \NUL\CAN \STX\NUL\SOHr\NUL \ETX3S\SOHA \NUL\SOH0\b\DC2\NUL\NUL\DLE\ETX\NUL\"\NUL \NUL\ETB\SOH\130\NUL333S\NUL\128\STX \STX\NUL\SOHr\NUL \STX\NUL\SOH\130\NUL \NUL\ETB \STX\NUL\SOHr\NUL \ETX3S\SOHA \NUL\SOH\NULp\ETX\NUL \CAN \STX\NUL55P\f\NUL\"\NUL \ETXSU\NUL\224\ETX \STX\NUL350\SYN30\b\DC2\NUL\NUL\DLE\EOT\NUL\"\NUL30\t\DC2\NUL\NUL\DLE\EOT\NUL\"\NUL\SOH\128\SUB \SOH \STX\NUL \STX\NUL \ETX\NULP\ACK \STX\NUL\DC2\NUL \STX\NUL \STX\NUL3\NUL@\a\NULb\NUL\DC2\NUL \STX\NUL \STX\NUL0\ETX\NULb\NUL\DC2\NUL \STX\NUL \STX\NUL0\STX\NULb\NUL\DC2\NUL \STX\NUL \STX\NUL0\SOH\NULb\NUL\NUL\DC1\DC2\NUL \SOH \ETX0\SOH\NUL0\STX\DC1 \NUL\SOH \STX\NUL2\NUL35z\128\b\EOT\128A\155\164\NUL\128\EOT\128\b\NUL\200\NUL\204\213\234\NUL \DLE\NUL\230o\NUL\STX\NUL\DC3P\SOH \NUL\SOH\DC2\NUL0\SOH5P\n \STX\NUL\DC2\NUL \STX\NUL355\NUL\192\STX \ETX3SP\r\NUL\"\NUL\NUL\178\NUL \STX\NUL\NUL\192\f \STX\NUL \ETX3SP\SI\NULB\NUL\NUL\194\NUL \STX\NUL350\r3\NUL\144\ACK\NUL2\NUL350\f\DC2\NUL\NUL\DLE\n\NUL`\ETX \NUL\SI\SOH\DLE\SO\NUL\177 \STX\NUL\NUL! \STX\NUL\NUL\DC2\NUL\NUL\DC1 \NUL\SOH \NUL\SOH\DC1 \STX\NUL\NUL! \STX\NUL\DC2\NUL \ETX0\SOH\NUL@\ETX\DC2\NUL\NUL\DC1\DC2\NUL2\NUL0\SOH\NUL\DC2\NUL \ETX0\ETX2\NUL0\SOH\NUL\DLE\STX\NUL\DC1\DC1\DC2\NUL3P\STX \SOH \ETX0\SOH \ETXP\ETX \STX\NUL0\STX\NUL2\NUL5\NUL2\NUL \ETX\NUL\DLE\ETX\NUL\DC1 \ETX \ETX \ETX\NUL\DLE\SOH \STX\NUL3\NUL3 \ETX\NUL\DLE\SOH\NUL \SOH \STX\NUL\DC2\NUL50\EOT\DC2\NUL50\EOT\NUL3P\ETX\NUL\DLE\SOH\SOH"
-}

guess :: Alonzo.Script era
guess = Alonzo.PlutusScript guessTheNumber3

guessTheNumber'3 :: P.Data -> P.Data -> P.Data -> ()
guessTheNumber'3 d1 d2 _d3 = if d1 P.== d2 then () else (P.error ())

guessTheNumber3 :: ShortByteString
guessTheNumber3 =
  toShort . flat . P.fromCompiledCode $
    $$(P.compile [||guessTheNumber'3||])
