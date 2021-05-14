{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

-- |  AlonzoEra instances for EraGen and ScriptClass
module Test.Cardano.Ledger.Alonzo.AlonzoEraGen where

import Cardano.Binary (serializeEncoding', toCBOR)
import Cardano.Ledger.Alonzo (AlonzoEra)
import Cardano.Ledger.Alonzo.Data as Alonzo (AuxiliaryData (..), Data (..))
import Cardano.Ledger.Alonzo.Language (Language (PlutusV1))
import Cardano.Ledger.Alonzo.PParams (PParams' (..))
import qualified Cardano.Ledger.Alonzo.PParams as Alonzo (PParams, extendPP, retractPP)
import Cardano.Ledger.Alonzo.Rules.Utxo (utxoEntrySize)
import Cardano.Ledger.Alonzo.Scripts as Alonzo (CostModel (..), ExUnits (..), Prices (..), Script (..))
import Cardano.Ledger.Alonzo.Tx (IsValidating (..), ValidatedTx (..))
import Cardano.Ledger.Alonzo.TxBody (TxBody (..), TxOut (..))
import Cardano.Ledger.Alonzo.TxWitness (Redeemers (..), TxWitness (..))
import Cardano.Ledger.AuxiliaryData (AuxiliaryDataHash)
import Cardano.Ledger.Coin (Coin (..))
import qualified Cardano.Ledger.Core as Core (PParams, PParamsDelta, Script)
import qualified Cardano.Ledger.Crypto as CC
import Cardano.Ledger.Era (Crypto, Era (..))
import Cardano.Ledger.Keys (KeyHash, KeyRole (Witness))
import Cardano.Ledger.Mary (MaryEra)
import Cardano.Ledger.Mary.Value (policies)
import Cardano.Ledger.ShelleyMA.AuxiliaryData as Mary (pattern AuxiliaryData)
import Cardano.Ledger.ShelleyMA.Timelocks (Timelock (..))
import Cardano.Ledger.Tx (Tx (Tx))
import Cardano.Ledger.Val ((<+>), (<×>))
import Cardano.Slotting.Slot (SlotNo (..))
import Control.Monad (replicateM)
import Data.Hashable (hash)
import qualified Data.List as List
import Data.Map as Map
import Data.Proxy (Proxy (..))
import Data.Sequence.Strict (StrictSeq)
import qualified Data.Sequence.Strict as Seq (fromList)
import Data.Set as Set
import GHC.Records (HasField (..))
import qualified PlutusTx as Plutus
import Shelley.Spec.Ledger.BaseTypes (Network (..), StrictMaybe (..))
import Shelley.Spec.Ledger.PParams (Update)
import Shelley.Spec.Ledger.TxBody (DCert, TxIn, Wdrl)
import Test.Cardano.Ledger.AllegraEraGen (genValidityInterval)
import Test.Cardano.Ledger.MaryEraGen (addTokens, genMint, maryGenesisValue, policyIndex)
import Test.QuickCheck hiding ((><))
import Test.Shelley.Spec.Ledger.ConcreteCryptoTypes (Mock)
import Test.Shelley.Spec.Ledger.Generator.Constants (Constants (..))
import Test.Shelley.Spec.Ledger.Generator.Core (GenEnv, genNatural)
import Test.Shelley.Spec.Ledger.Generator.EraGen (EraGen (..), MinGenTxout (..))
import Test.Shelley.Spec.Ledger.Generator.ScriptClass (Quantifier (..), ScriptClass (..))
import Test.Shelley.Spec.Ledger.Generator.Update (genM, genShelleyPParamsDelta)
import qualified Test.Shelley.Spec.Ledger.Generator.Update as Shelley (genPParams)

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
                  <*> genSet (Alonzo.Data <$> genPlutusData)
              )

instance CC.Crypto c => ScriptClass (AlonzoEra c) where
  -- basescript _ key = TimelockScript (basescript (Proxy @(MaryEra c)) key) -- The old style from Mary
  basescript proxy key = TimelockScript (someLeaf proxy key)
  isKey _ (TimelockScript x) = isKey (Proxy @(MaryEra c)) x
  isKey _ (PlutusScript _) = Nothing
  quantify _ (TimelockScript x) = fmap TimelockScript (quantify (Proxy @(MaryEra c)) x)
  quantify _ x = Leaf x
  unQuantify _ quant = TimelockScript $ unQuantify (Proxy @(MaryEra c)) (fmap unTime quant)

unTime :: Alonzo.Script era -> Timelock (Crypto era)
unTime (TimelockScript x) = x
unTime (PlutusScript _) = error "Plutus in Timelock"

genAlonzoTxBody ::
  forall c.
  Mock c =>
  GenEnv (AlonzoEra c) ->
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
genAlonzoTxBody _genenv pparams currentslot input txOuts certs wdrls fee updates auxDHash = do
  _low <- genM (genSlotAfter currentslot)
  _high <- genM (genSlotAfter (currentslot + 50))
  netid <- genM $ pure Testnet -- frequency [(2, pure Mainnet), (1, pure Testnet)]
  minted <- genMint
  let (minted2, txouts') = case addTokens (Proxy @(AlonzoEra c)) mempty pparams minted txOuts of
        Nothing -> (mempty, txOuts)
        Just os -> (minted, os)
      scriptsFromPolicies = List.map (\p -> (Map.!) policyIndex p) (Set.toList $ policies minted)
  validityInterval <- genValidityInterval currentslot
  return
    ( TxBody
        input
        Set.empty -- collaeral -- TODO do something better here (use genenv ?)
        txouts'
        certs
        wdrls
        fee
        validityInterval -- (ValidityInterval SNothing SNothing) -- (ValidityInterval low high)
        updates
        -- reqSignerHashes
        Set.empty -- TODO do something better here
        minted2
        -- wppHash
        SNothing -- TODO do something better here
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
  cost <- genM (pure (Map.singleton PlutusV1 (CostModel Map.empty))) -- TODO what is a better assumption for this?
  price <- genM (Prices <$> (Coin <$> choose (100, 5000)) <*> (Coin <$> choose (100, 5000)))
  mxTx <- genM (ExUnits <$> (choose (100, 5000)) <*> (choose (100, 5000)))
  mxBl <- genM (ExUnits <$> (choose (100, 5000)) <*> (choose (100, 5000)))
  -- Not too small for mxV, if this is too small then any Tx with Value
  -- that has lots of policyIds will fail. The Shelley Era uses hard coded 4000
  mxV <- genM (genNatural 4000 5000)
  let c = SJust 150
      mxC = SJust 10
  pure (Alonzo.extendPP shelleypp ada cost price mxTx mxBl mxV c mxC)

genAlonzoPParams ::
  forall c.
  Constants ->
  Gen (Core.PParams (AlonzoEra c))
genAlonzoPParams constants = do
  shelleypp <- Shelley.genPParams @(MaryEra c) constants -- This ensures that "_d" field is not 0.
  ada <- (Coin <$> choose (1, 5))
  cost <- pure (Map.singleton PlutusV1 (CostModel Map.empty)) -- There are no other Languages, and there must be something for PlutusV1
  price <- (Prices <$> (Coin <$> choose (100, 5000)) <*> (Coin <$> choose (100, 5000)))
  mxTx <- (ExUnits <$> (choose (100, 5000)) <*> (choose (100, 5000)))
  mxBl <- (ExUnits <$> (choose (100, 5000)) <*> (choose (100, 5000)))
  mxV <- (genNatural 4000 10000) -- This can't be too small. Shelley uses Hard coded 4000
  let c = 150
      mxC = 10
  pure (Alonzo.extendPP shelleypp ada cost price mxTx mxBl mxV c mxC)

-- | Since Alonzo PParams don't have this field, we have to compute something here.
instance HasField "_minUTxOValue" (Alonzo.PParams (AlonzoEra c)) Coin where
  getField _ = Coin 4000

instance Mock c => EraGen (AlonzoEra c) where
  genEraAuxiliaryData = genAux
  genGenesisValue = maryGenesisValue
  genEraTxBody = genAlonzoTxBody
  updateEraTxBody txb coinx txin txout = new
    where
      new = txb {inputs = txin, txfee = coinx, outputs = txout}
  genEraPParamsDelta = genAlonzoPParamsDelta
  genEraPParams = genAlonzoPParams
  genEraWitnesses setWitVKey mapScriptWit = TxWitness setWitVKey Set.empty mapScriptWit Map.empty (Redeemers Map.empty)
  unsafeApplyTx (Tx bod wit auxdata) = ValidatedTx bod wit (IsValidating True) auxdata

instance Mock c => MinGenTxout (AlonzoEra c) where
  calcEraMinUTxO tout pp = (utxoEntrySize tout <×> getField @"_adaPerUTxOWord" pp)
  addValToTxOut v (TxOut a u b) = TxOut a (v <+> u) b
  genEraTxOut genVal addrs = do
    values <- replicateM (length addrs) genVal
    let pairs = zip addrs values
        makeTxOut (addr, val) = TxOut addr val SNothing
    pure (makeTxOut <$> pairs)

someLeaf ::
  forall era.
  Era era =>
  Proxy era ->
  KeyHash 'Witness (Crypto era) ->
  Timelock (Crypto era)
someLeaf _proxy x =
  let n = hash (serializeEncoding' (toCBOR x)) -- We don't really care about the hash, we only
      slot = SlotNo (fromIntegral (mod n 200)) -- use it to pseudo-randomly pick a slot and mode
      mode = mod n 3 -- mode==0 is a time leaf, mode=1 or 2 is a signature leaf
   in case mode of
        0 -> (RequireAnyOf . Seq.fromList) [RequireTimeStart slot, RequireTimeExpire slot]
        _ -> RequireSignature x
