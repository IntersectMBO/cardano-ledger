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

module Test.Cardano.Ledger.Mary () where -- export the EraGen instance for MaryEra

import Cardano.Ledger.AuxiliaryData (AuxiliaryDataHash)
import qualified Cardano.Ledger.Core as Core (AuxiliaryData, Value)
import qualified Cardano.Ledger.Crypto as CryptoClass
import Cardano.Ledger.Era (Crypto)
import Cardano.Ledger.Mary.Value
  ( AssetName (..),
    PolicyID (..),
    Value (..),
    policies,
  )
import Cardano.Ledger.Shelley.Constraints
  ( UsesAuxiliary,
    UsesPParams,
    UsesValue,
  )
import Cardano.Ledger.ShelleyMA.Rules.Utxo (scaledMinDeposit)
import Cardano.Ledger.ShelleyMA.Timelocks (Timelock (..))
import Cardano.Ledger.ShelleyMA.TxBody (StrictMaybe, TxBody (..))
import qualified Cardano.Ledger.Val as Val
import Cardano.Slotting.Slot (SlotNo)
import qualified Data.ByteString.Char8 as BS
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Sequence.Strict (StrictSeq (..), (<|), (><))
import qualified Data.Sequence.Strict as StrictSeq
import qualified Data.Set as Set
import Shelley.Spec.Ledger.BaseTypes (StrictMaybe (..))
import Shelley.Spec.Ledger.Coin (Coin (..))
import Shelley.Spec.Ledger.PParams (PParams, PParams' (..), Update)
import Shelley.Spec.Ledger.Tx (TxIn, TxOut (..), hashScript)
import Shelley.Spec.Ledger.TxBody (DCert, Wdrl)
import Test.Cardano.Ledger.Allegra
  ( genValidityInterval,
    quantifyTL,
    someLeaf,
    unQuantifyTL,
  )
import Test.Cardano.Ledger.EraBuffet (MaryEra)
import Test.QuickCheck (Gen, arbitrary, frequency)
import qualified Test.QuickCheck as QC
import Test.Shelley.Spec.Ledger.ConcreteCryptoTypes (Mock)
import Test.Shelley.Spec.Ledger.Generator.Constants (Constants (..))
import Test.Shelley.Spec.Ledger.Generator.Core (GenEnv (..), genInteger)
import Test.Shelley.Spec.Ledger.Generator.EraGen (EraGen (..))
import Test.Shelley.Spec.Ledger.Generator.ScriptClass
  ( ScriptClass (..),
    exponential,
  )
import Test.Shelley.Spec.Ledger.Utils (Split (..))

{------------------------------------------------------------------------------
 EraGen instance for MaryEra - This instance makes it possible to run the
 Shelley property tests for (MaryEra crypto)

 This instance is layered on top of the ShelleyMA instances
 in Cardano.Ledger.ShelleyMA.Scripts:

   `type instance Core.Script (MaryEra c) = Timelock (MaryEra c)`
   `type instance ValidateScript (ShelleyMAEra ma c) = ... `
------------------------------------------------------------------------------}

instance (CryptoClass.Crypto c) => ScriptClass (MaryEra c) where
  isKey _ (RequireSignature hk) = Just hk
  isKey _ _ = Nothing
  basescript _proxy = someLeaf @(MaryEra c)
  quantify _ = quantifyTL
  unQuantify _ = unQuantifyTL

instance (CryptoClass.Crypto c, Mock c) => EraGen (MaryEra c) where
  genGenesisValue (GenEnv _ Constants {minGenesisOutputVal, maxGenesisOutputVal}) =
    Val.inject . Coin <$> exponential minGenesisOutputVal maxGenesisOutputVal
  genEraTxBody _ge = genTxBody
  genEraAuxiliaryData = genAuxiliaryData
  updateEraTxBody (TxBody _in _out cert wdrl _txfee vi upd meta mint) fee ins outs =
    TxBody ins outs cert wdrl fee vi upd meta mint

genAuxiliaryData ::
  Mock crypto =>
  Constants ->
  Gen (StrictMaybe (Core.AuxiliaryData (MaryEra crypto)))
genAuxiliaryData Constants {frequencyTxWithMetadata} =
  frequency
    [ (frequencyTxWithMetadata, SJust <$> arbitrary),
      (100 - frequencyTxWithMetadata, pure SNothing)
    ]

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
trivialPolicy :: CryptoClass.Crypto c => Int -> Timelock c
trivialPolicy i | i == 0 = RequireAllOf (StrictSeq.fromList [])
trivialPolicy i | otherwise = RequireAllOf (StrictSeq.fromList [trivialPolicy (i -1)])

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

redCoins :: CryptoClass.Crypto c => Timelock c
redCoins = trivialPolicy 0

redCoinId :: forall c. CryptoClass.Crypto c => PolicyID c
redCoinId = PolicyID $ hashScript @(MaryEra c) redCoins

red :: AssetName
red = AssetName $ BS.pack "redCoin"

genRed :: CryptoClass.Crypto c => Gen (Value c)
genRed = do
  n <- genInteger coloredCoinMinMint coloredCoinMaxMint
  pure $ Value 0 (Map.singleton redCoinId (Map.singleton red n))

--------------------------------------------------------
-- Blue Coins                                         --
--                                                    --
-- These tokens are (nearly) always minted with a new --
-- asset name.
--------------------------------------------------------

blueCoins :: CryptoClass.Crypto c => Timelock c
blueCoins = trivialPolicy 1

blueCoinId :: forall c. CryptoClass.Crypto c => PolicyID c
blueCoinId = PolicyID $ hashScript @(MaryEra c) blueCoins

maxBlueMint :: Int
maxBlueMint = 5

-- TODO these blue coins are actually problematic since our
-- current coin selection algorithm does not prevent creating
-- a multi-asset that is too large.

genBlue :: CryptoClass.Crypto c => Gen (Value c)
genBlue = do
  as <- QC.resize maxBlueMint $ QC.listOf genSingleBlue
  -- the transaction size gets too big if we mint too many assets
  pure $ Value 0 (Map.singleton blueCoinId (Map.fromList as))
  where
    genSingleBlue = do
      n <- genInteger coloredCoinMinMint coloredCoinMaxMint
      a <- arbitrary
      pure $ (AssetName a, n)

--------------------------------------------------------
-- Yellow Coins                                       --
--                                                    --
-- These tokens are minted with a small variety of    --
-- asset names.                                       --
--------------------------------------------------------

yellowCoins :: CryptoClass.Crypto c => Timelock c
yellowCoins = trivialPolicy 2

yellowCoinId :: forall c. CryptoClass.Crypto c => PolicyID c
yellowCoinId = PolicyID $ hashScript @(MaryEra c) yellowCoins

yellowNumAssets :: Int
yellowNumAssets = 5

genYellow :: CryptoClass.Crypto c => Gen (Value c)
genYellow = do
  xs <- QC.sublistOf [0 .. yellowNumAssets]
  as <- mapM genSingleYellow xs
  pure $ Value 0 (Map.singleton yellowCoinId (Map.fromList as))
  where
    genSingleYellow x = do
      y <- genInteger coloredCoinMinMint coloredCoinMaxMint
      let an = AssetName . BS.pack $ "yellow" <> show x
      pure $ (an, y)

-- | This map allows us to lookup a minting policy by the policy ID.
policyIndex :: CryptoClass.Crypto c => Map (PolicyID c) (Timelock c)
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

genBundle :: Int -> Gen (Value c) -> Gen (Value c)
genBundle freq g = QC.frequency [(freq, g), (100 - freq, pure mempty)]

genMint :: CryptoClass.Crypto c => Gen (Value c)
genMint = do
  r <- genBundle redFreq genRed
  b <- genBundle blueFreq genBlue
  y <- genBundle yellowFreq genYellow
  pure $ r <> b <> y

-------------------------------
-- END Permissionless Tokens --
-------------------------------

-- | Attempt to Add tokens to a non-empty list of transaction outputs.
-- It will add them to the first output that has enough lovelace
-- to meet the minUTxO requirment, if such an output exists.
addTokens ::
  ( Core.Value era ~ Value (Crypto era),
    EraGen era
  ) =>
  PParams era ->
  Value (Crypto era) ->
  StrictSeq (TxOut era) ->
  Maybe (StrictSeq (TxOut era))
addTokens = addTokens' StrictSeq.Empty
  where
    addTokens' tooLittleLovelace pparams ts (o@(TxOut a v) :<| os) =
      if Val.coin v < scaledMinDeposit v (_minUTxOValue pparams)
        then addTokens' (o :<| tooLittleLovelace ) pparams ts os
        else (Just $ tooLittleLovelace >< TxOut a (v <> ts) <| os)
    addTokens' _ _ _ StrictSeq.Empty = Nothing

genTxBody ::
  forall era.
  ( UsesValue era,
    Core.Value era ~ Value (Crypto era),
    UsesPParams era,
    UsesAuxiliary era,
    EraGen era
  ) =>
  PParams era ->
  SlotNo ->
  Set.Set (TxIn (Crypto era)) ->
  StrictSeq (TxOut era) ->
  StrictSeq (DCert (Crypto era)) ->
  Wdrl (Crypto era) ->
  Coin ->
  StrictMaybe (Update era) ->
  StrictMaybe (AuxiliaryDataHash (Crypto era)) ->
  Gen (TxBody era, [Timelock (Crypto era)])
genTxBody pparams slot ins outs cert wdrl fee upd meta = do
  validityInterval <- genValidityInterval slot
  mint <- genMint
  let (mint', outs') = case addTokens pparams mint outs of
                         Nothing -> (mempty, outs)
                         Just os -> (mint, os)
      ps = map (\p -> (Map.!) policyIndex p) (Set.toList $ policies mint)
  pure $
    ( TxBody
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

instance Split (Value era) where
  vsplit (Value n _) 0 = ([], Coin n)
  vsplit (Value n mp) m
    | m Prelude.<= 0 = error "must split coins into positive parts"
    | otherwise =
      ( take (fromIntegral m) ((Value (n `div` m) mp) : (repeat (Value (n `div` m) Map.empty))),
        Coin (n `rem` m)
      )
