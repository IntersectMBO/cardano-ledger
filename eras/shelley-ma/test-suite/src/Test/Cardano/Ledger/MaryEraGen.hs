{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

-- | Export the EraGen instance for MaryEra, as well as some reusable functions for future Eras
module Test.Cardano.Ledger.MaryEraGen (genMint, maryGenesisValue, policyIndex, addTokens) where

import Cardano.Ledger.Allegra.Scripts (Timelock (..))
import Cardano.Ledger.AuxiliaryData (AuxiliaryDataHash)
import Cardano.Ledger.BaseTypes (StrictMaybe (..))
import Cardano.Ledger.Coin (Coin (..))
import Cardano.Ledger.Core
import qualified Cardano.Ledger.Crypto as CC
import Cardano.Ledger.Mary.TxBody (MaryTxBody (MaryTxBody))
import Cardano.Ledger.Mary.TxOut (scaledMinDeposit)
import Cardano.Ledger.Mary.Value
  ( AssetName (..),
    MaryValue (..),
    MultiAsset,
    PolicyID (..),
    multiAssetFromList,
    policies,
  )
import Cardano.Ledger.Shelley.PParams (ShelleyPParams, ShelleyPParamsHKD (..), ShelleyPParamsUpdate, Update)
import Cardano.Ledger.Shelley.Tx
  ( ShelleyTxOut (..),
    TxIn,
  )
import Cardano.Ledger.Shelley.TxBody (DCert, Wdrl)
import Cardano.Ledger.Shelley.TxWits (ShelleyTxWits (ShelleyTxWits))
import Cardano.Ledger.Val ((<+>))
import qualified Cardano.Ledger.Val as Val
import Cardano.Slotting.Slot (SlotNo)
import Control.Monad (replicateM)
import Data.Map (Map)
import qualified Data.Map.Strict as Map
import Data.Proxy (Proxy (..))
import Data.Sequence.Strict (StrictSeq (..), (<|), (><))
import qualified Data.Sequence.Strict as StrictSeq
import qualified Data.Set as Set
import GHC.Exts (fromString)
import Lens.Micro
import Test.Cardano.Ledger.AllegraEraGen
  ( genValidityInterval,
    quantifyTL,
    someLeaf,
    unQuantifyTL,
  )
import Test.Cardano.Ledger.EraBuffet (MaryEra)
import Test.Cardano.Ledger.Shelley.ConcreteCryptoTypes (Mock)
import Test.Cardano.Ledger.Shelley.Generator.Constants (Constants (..))
import Test.Cardano.Ledger.Shelley.Generator.Core (GenEnv (..), genInteger)
import Test.Cardano.Ledger.Shelley.Generator.EraGen (EraGen (..), MinGenTxout (..))
import Test.Cardano.Ledger.Shelley.Generator.ScriptClass
  ( ScriptClass (..),
    exponential,
  )
import Test.Cardano.Ledger.Shelley.Generator.Update (genPParams, genShelleyPParamsUpdate)
import Test.Cardano.Ledger.Shelley.Utils (ShelleyTest)
import Test.QuickCheck (Gen, arbitrary, frequency)
import qualified Test.QuickCheck as QC

{------------------------------------------------------------------------------
 EraGen instance for MaryEra - This instance makes it possible to run the
 Shelley property tests for (MaryEra c)

 This instance is layered on top of the ShelleyMA instances
 in Cardano.Ledger.ShelleyMA.Scripts:

   `type instance Script (MaryEra c) = Timelock (MaryEra c)`
   `instance ValidateScript (ShelleyMAEra ma c) where ... `
------------------------------------------------------------------------------}

instance (CC.Crypto c) => ScriptClass (MaryEra c) where
  isKey _ (RequireSignature hk) = Just hk
  isKey _ _ = Nothing
  basescript _proxy = someLeaf @(MaryEra c)
  quantify _ = quantifyTL
  unQuantify _ = unQuantifyTL

instance (CC.Crypto c, Mock c, ShelleyTest c) => EraGen (MaryEra c) where
  genGenesisValue = maryGenesisValue
  genEraTxBody _ge _utxo = _ -- genTxBody
  genEraAuxiliaryData = genAuxiliaryData
  updateEraTxBody _utxo _pp _wits txBody fee ins out =
    txBody
      & inputsTxBodyL %~ (<> ins)
      & outputsTxBodyL %~ (:|> out)
      & feeTxBodyL .~ fee
  genEraPParamsUpdate = genShelleyPParamsUpdate
  genEraPParams = genPParams
  genEraTxWits _scriptinfo setWitVKey mapScriptWit = ShelleyTxWits setWitVKey mapScriptWit mempty

genAuxiliaryData ::
  Mock c =>
  Constants ->
  Gen (StrictMaybe (TxAuxData (MaryEra c)))
genAuxiliaryData Constants {frequencyTxWithMetadata} =
  frequency
    [ (frequencyTxWithMetadata, SJust <$> arbitrary),
      (100 - frequencyTxWithMetadata, pure SNothing)
    ]

-- | Carefully crafted to apply in any Era where Value is MaryValue
maryGenesisValue :: forall era c. CC.Crypto c => GenEnv era -> Gen (MaryValue c)
maryGenesisValue (GenEnv _ _ Constants {minGenesisOutputVal, maxGenesisOutputVal}) =
  Val.inject . Coin <$> exponential minGenesisOutputVal maxGenesisOutputVal

--------------------------------------------------------
-- Permissionless Tokens                              --
--                                                    --
-- We introduce three token bundles, each which has a --
-- permissionless minting policy and each which has a --
-- different minting behavior (use of asset names).   --
--------------------------------------------------------

-- | An infinite indexed collection of trivial policies.
--  They are trivial in the sense that they require no
--  signature and can be submitted at any time.
trivialPolicy :: Era era => Int -> Timelock era
trivialPolicy i
  | i == 0 = RequireAllOf (StrictSeq.fromList [])
  | otherwise = RequireAllOf (StrictSeq.fromList [trivialPolicy (i - 1)])

coloredCoinMinMint :: Integer
coloredCoinMinMint = 1000

coloredCoinMaxMint :: Integer
coloredCoinMaxMint = 1000 * 1000

--------------------------------------------------------
-- Red Coins                                          --
--                                                    --
-- These tokens are always minted with the same asset --
-- name, "red".                                       --
--------------------------------------------------------

redCoins :: Era era => Timelock era
redCoins = trivialPolicy 0

redCoinId :: forall c. CC.Crypto c => PolicyID c
redCoinId = PolicyID $ hashScript @(MaryEra c) redCoins

red :: AssetName
red = AssetName "red"

genRed :: CC.Crypto c => Gen (MultiAsset c)
genRed = do
  n <- genInteger coloredCoinMinMint coloredCoinMaxMint
  pure $ multiAssetFromList [(redCoinId, red, n)]

--------------------------------------------------------
-- Blue Coins                                         --
--                                                    --
-- These tokens are (nearly) always minted with a new --
-- asset name.
--------------------------------------------------------

blueCoins :: Era era => Timelock era
blueCoins = trivialPolicy 1

blueCoinId :: forall c. CC.Crypto c => PolicyID c
blueCoinId = PolicyID $ hashScript @(MaryEra c) blueCoins

maxBlueMint :: Int
maxBlueMint = 5

-- TODO these blue coins are actually problematic since our
-- current coin selection algorithm does not prevent creating
-- a multi-asset that is too large.

genBlue :: CC.Crypto c => Gen (MultiAsset c)
genBlue = do
  as <- QC.resize maxBlueMint $ QC.listOf genSingleBlue
  -- the transaction size gets too big if we mint too many assets
  pure $ multiAssetFromList (map (\(asset, count) -> (blueCoinId, asset, count)) as)
  where
    genSingleBlue = do
      n <- genInteger coloredCoinMinMint coloredCoinMaxMint
      a <- arbitrary
      pure (AssetName a, n)

--------------------------------------------------------
-- Yellow Coins                                       --
--                                                    --
-- These tokens are minted with a small variety of    --
-- asset names.                                       --
--------------------------------------------------------

yellowCoins :: Era era => Timelock era
yellowCoins = trivialPolicy 2

yellowCoinId :: forall c. CC.Crypto c => PolicyID c
yellowCoinId = PolicyID $ hashScript @(MaryEra c) yellowCoins

yellowNumAssets :: Int
yellowNumAssets = 5

genYellow :: CC.Crypto c => Gen (MultiAsset c)
genYellow = do
  xs <- QC.sublistOf [0 .. yellowNumAssets]
  as <- mapM genSingleYellow xs
  pure $ multiAssetFromList (map (\(asset, count) -> (yellowCoinId, asset, count)) as)
  where
    genSingleYellow x = do
      y <- genInteger coloredCoinMinMint coloredCoinMaxMint
      let an = AssetName . fromString $ "yellow" <> show x
      pure (an, y)

-- | Carefully crafted to apply in any Era where Value is MaryValue
-- | This map allows us to lookup a minting policy by the policy ID.
policyIndex :: Era era => Map (PolicyID (EraCrypto era)) (Timelock era)
policyIndex =
  Map.fromList
    [ (redCoinId, redCoins),
      (blueCoinId, blueCoins),
      (yellowCoinId, yellowCoins)
    ]

--------------------------------------------------------
-- Minting Frequencies                                --
--                                                    --
-- The frequencies represent a percent chance of any  --
-- given transaction to mint one of the three token   --
-- bundles.                                           --
--------------------------------------------------------

redFreq :: Int
redFreq = 10

blueFreq :: Int
blueFreq = 1

yellowFreq :: Int
yellowFreq = 20

genBundle :: Int -> Gen (MultiAsset c) -> Gen (MultiAsset c)
genBundle freq g = QC.frequency [(freq, g), (100 - freq, pure mempty)]

genMint :: CC.Crypto c => Gen (MultiAsset c)
genMint = do
  r <- genBundle redFreq genRed
  b <- genBundle blueFreq genBlue
  y <- genBundle yellowFreq genYellow
  pure $ r <> b <> y

-------------------------------
-- END Permissionless Tokens --
-------------------------------

-- | Carefully crafted to apply to any Era where Value is MaryValue
-- We attempt to Add tokens to a non-empty list of transaction outputs.
-- It will add them to the first output that has enough lovelace
-- to meet the minUTxO requirment, if such an output exists.
addTokens ::
  forall era.
  ( EraGen era,
    Value era ~ MaryValue (EraCrypto era)
  ) =>
  Proxy era ->
  StrictSeq (TxOut era) -> -- This is an accumuating parameter
  PParams era ->
  MultiAsset (EraCrypto era) ->
  StrictSeq (TxOut era) ->
  Maybe (StrictSeq (TxOut era))
addTokens proxy tooLittleLovelace pparams ts (txOut :<| os) =
  let v = txOut ^. valueTxOutL
   in if Val.coin v < scaledMinDeposit v (pparams ^. ppMinUTxOValueL)
        then addTokens proxy (txOut :<| tooLittleLovelace) pparams ts os
        else Just $ tooLittleLovelace >< addValToTxOut @era (MaryValue 0 ts) txOut <| os
addTokens _proxy _ _ _ StrictSeq.Empty = Nothing

-- | This function is only good in the Mary Era
genTxBody ::
  forall era.
  ( ShelleyTest era,
    EraGen era,
    Value era ~ MaryValue (EraCrypto era),
    PParams era ~ ShelleyPParams era,
    PParamsUpdate era ~ ShelleyPParamsUpdate era,
    TxOut era ~ ShelleyTxOut era
  ) =>
  PParams era ->
  SlotNo ->
  Set.Set (TxIn (EraCrypto era)) ->
  StrictSeq (ShelleyTxOut era) ->
  StrictSeq (DCert (EraCrypto era)) ->
  Wdrl (EraCrypto era) ->
  Coin ->
  StrictMaybe (Update era) ->
  StrictMaybe (AuxiliaryDataHash (EraCrypto era)) ->
  Gen (MaryTxBody era, [Timelock era])
genTxBody pparams slot ins outs cert wdrl fee upd meta = do
  validityInterval <- genValidityInterval slot
  mint <- genMint
  let (mint', outs') = case addTokens (Proxy @era) StrictSeq.Empty pparams mint outs of
        Nothing -> (mempty, outs)
        Just os -> (mint, os)
      ps =
        map (\k -> Map.findWithDefault (error $ "Cannot find policy: " ++ show k) k policyIndex) $
          Set.toList $
            policies mint
  pure
    ( MaryTxBody
        ins
        outs'
        cert
        wdrl
        fee
        validityInterval
        upd
        meta
        mint',
      ps -- These additional scripts are for the minting policies.
    )

instance Split (MaryValue era) where
  vsplit (MaryValue n _) 0 = ([], Coin n)
  vsplit (MaryValue n mp) m
    | m <= 0 = error "must split coins into positive parts"
    | otherwise =
      ( take (fromIntegral m) (MaryValue (n `div` m) mp : repeat (MaryValue (n `div` m) mempty)),
        Coin (n `rem` m)
      )

instance Mock c => MinGenTxout (MaryEra c) where
  calcEraMinUTxO _txout pp = pp ^. ppMinUTxOValueL
  addValToTxOut v (ShelleyTxOut a u) = ShelleyTxOut a (v <+> u)
  genEraTxOut _genenv genVal addrs = do
    values <- replicateM (length addrs) genVal
    let makeTxOut (addr, val) = ShelleyTxOut addr val
    pure (makeTxOut <$> zip addrs values)
