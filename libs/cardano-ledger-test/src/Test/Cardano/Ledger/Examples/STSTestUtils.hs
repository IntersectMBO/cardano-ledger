{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE UndecidableSuperClasses #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Test.Cardano.Ledger.Examples.STSTestUtils (
  EraModel (..),
  PlutusPurposeTag (..),
  initUTxO,
  mkGenesisTxIn,
  mkTxDats,
  mkSingleRedeemer,
  someAddr,
  someKeys,
  someScriptAddr,
  alwaysFailsHash,
  alwaysSucceedsHash,
  timelockScript,
  timelockHash,
) where

import Cardano.Ledger.Allegra.Scripts (AllegraEraScript, pattern RequireTimeStart)
import Cardano.Ledger.Alonzo.Scripts (AlonzoEraScript (..), AsIx, ExUnits (..))
import Cardano.Ledger.Alonzo.TxWits (Redeemers (..), TxDats (..))
import Cardano.Ledger.BaseTypes (StrictMaybe (..), mkTxIxPartial)
import Cardano.Ledger.Coin (Coin (..))
import Cardano.Ledger.Conway.Core (AlonzoEraTxOut (..), ScriptIntegrityHash)
import Cardano.Ledger.Plutus (Language)
import Cardano.Ledger.Plutus.Data (Data (..), hashData)
import Cardano.Ledger.Shelley.Core hiding (TranslationError)
import Cardano.Ledger.Shelley.Scripts (
  ShelleyEraScript,
  pattern RequireAllOf,
  pattern RequireSignature,
 )
import Cardano.Ledger.State
import Cardano.Ledger.TxIn (TxIn (..))
import Cardano.Ledger.Val (inject)
import Cardano.Slotting.Slot (SlotNo (..))
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Word (Word32)
import GHC.Generics (Generic)
import GHC.Stack
import Lens.Micro (Lens', (&), (.~))
import Numeric.Natural (Natural)
import qualified PlutusLedgerApi.V1 as PV1
import Test.Cardano.Ledger.Common hiding (Result)
import Test.Cardano.Ledger.Core.KeyPair (KeyPair (..), mkAddr)
import Test.Cardano.Ledger.Generic.Indexed (theKeyHash)
import Test.Cardano.Ledger.Generic.ModelState (Model)
import Test.Cardano.Ledger.Shelley.Era (EraTest)
import Test.Cardano.Ledger.Shelley.Generator.EraGen (genesisId)
import Test.Cardano.Ledger.Shelley.Utils (RawSeed (..), mkKeyPair, mkKeyPair')

data PlutusPurposeTag
  = Spending
  | Minting
  | Certifying
  | Withdrawing
  | Voting
  | Proposing
  deriving (Eq, Ord, Show, Enum, Bounded, Generic)

instance ToExpr PlutusPurposeTag

class EraTest era => EraModel era where
  applyTx :: Int -> SlotNo -> Model era -> Tx TopTx era -> Model era
  applyCert :: Model era -> TxCert era -> Model era

  mkRedeemersFromTags :: [((PlutusPurposeTag, Word32), (Data era, ExUnits))] -> Redeemers era
  mkRedeemersFromTags = error $ "No redeemers in " <> eraName @era

  mkRedeemers :: [(PlutusPurpose AsIx era, (Data era, ExUnits))] -> Redeemers era
  mkRedeemers = error $ "No redeemers in " <> eraName @era

  newScriptIntegrityHash ::
    PParams era ->
    [Language] ->
    Redeemers era ->
    TxDats era ->
    StrictMaybe ScriptIntegrityHash
  newScriptIntegrityHash _ _ _ _ = SNothing

  mkPlutusPurposePointer :: PlutusPurposeTag -> Word32 -> PlutusPurpose AsIx era
  mkPlutusPurposePointer = error $ "mkPlutusPurposePointer not available in " <> eraName @era

  always :: Natural -> Script era

  never :: Natural -> Script era

  collateralReturnTxBodyT :: Lens' (TxBody TopTx era) (StrictMaybe (TxOut era))

  validTxOut :: Map ScriptHash (Script era) -> TxOut era -> Bool

-- =================================================================
-- =========================  Shared data  =========================
--   Data with specific semantics ("constants")
-- =================================================================

alwaysFailsHash :: forall era. (ShelleyEraScript era, EraModel era) => Natural -> ScriptHash
alwaysFailsHash n = hashScript @era $ never n

alwaysSucceedsHash :: forall era. (ShelleyEraScript era, EraModel era) => Natural -> ScriptHash
alwaysSucceedsHash n = hashScript @era $ always n

someKeys :: KeyPair Payment
someKeys = KeyPair vk sk
  where
    (sk, vk) = mkKeyPair (RawSeed 1 1 1 1 1)

someAddr :: Addr
someAddr = mkAddr someKeys $ mkKeyPair' @Staking (RawSeed 0 0 0 0 2)

-- Create an address with a given payment script.
someScriptAddr :: forall era. EraScript era => Script era -> Addr
someScriptAddr s = mkAddr (hashScript s) $ mkKeyPair' @Staking (RawSeed 0 0 0 0 0)

timelockScript :: AllegraEraScript era => SlotNo -> Script era
timelockScript s =
  fromNativeScript $
    RequireAllOf
      [ RequireSignature $ theKeyHash 1
      , RequireTimeStart (100 + s)
      ]

timelockHash ::
  forall era.
  AllegraEraScript era =>
  SlotNo ->
  ScriptHash
timelockHash n = hashScript @era $ timelockScript n

-- ======================================================================
-- ========================= Initial Utxo ===============================
-- ======================================================================

initUTxO ::
  forall era.
  ( AllegraEraScript era
  , AlonzoEraTxOut era
  , EraModel era
  ) =>
  UTxO era
initUTxO =
  UTxO $
    Map.fromList $
      [ (mkGenesisTxIn 1, alwaysSucceedsOutput)
      , (mkGenesisTxIn 2, alwaysFailsOutput)
      ]
        ++ map (\i -> (mkGenesisTxIn i, someOutput)) [3 .. 8]
        ++ map (\i -> (mkGenesisTxIn i, collateralOutput)) [11 .. 18]
        ++ [ (mkGenesisTxIn 100, timelockOut)
           , (mkGenesisTxIn 101, unspendableOut)
           , (mkGenesisTxIn 102, alwaysSucceedsOutputV1)
           , (mkGenesisTxIn 103, nonScriptOutWithDatum)
           ]
  where
    alwaysSucceedsOutput =
      mkBasicTxOut (someScriptAddr @era $ always 3) (inject $ Coin 5000)
        & dataHashTxOutL .~ SJust (hashData $ datumExample1 @era)
    alwaysFailsOutput =
      mkBasicTxOut (someScriptAddr @era $ never 0) (inject $ Coin 3000)
        & dataHashTxOutL .~ SJust (hashData $ datumExample2 @era)
    someOutput = mkBasicTxOut someAddr (inject $ Coin 1000)
    collateralOutput = mkBasicTxOut someAddr (inject $ Coin 5)
    timelockOut = mkBasicTxOut timelockAddr (inject $ Coin 1)
    timelockAddr = mkAddr tlh $ mkKeyPair' @Staking (RawSeed 0 0 0 0 2)
      where
        tlh = hashScript @era $ tls 0
        tls s =
          fromNativeScript @era $
            RequireAllOf
              [ RequireSignature $ theKeyHash 1
              , RequireTimeStart (100 + s)
              ]
    -- This output is unspendable since it is locked by a plutus script, but has no datum hash.
    unspendableOut =
      mkBasicTxOut (someScriptAddr @era $ always 3) (inject $ Coin 5000)
    alwaysSucceedsOutputV1 =
      unspendableOut & dataHashTxOutL .~ SJust (hashData (datumExample1 @era))
    nonScriptOutWithDatum =
      mkBasicTxOut someAddr (inject $ Coin 1221)
        & dataHashTxOutL .~ SJust (hashData (datumExample1 @era))

datumExample1 :: Era era => Data era
datumExample1 = Data (PV1.I 123)

datumExample2 :: Era era => Data era
datumExample2 = Data (PV1.I 0)

-- ======================================================================
-- ========================= Shared helper functions  ===================
-- ======================================================================

mkGenesisTxIn :: HasCallStack => Integer -> TxIn
mkGenesisTxIn = TxIn genesisId . mkTxIxPartial

mkTxDats :: Era era => Data era -> TxDats era
mkTxDats d = TxDats $ Map.singleton (hashData d) d

mkSingleRedeemer ::
  forall era. AlonzoEraScript era => PlutusPurpose AsIx era -> Data era -> Redeemers era
mkSingleRedeemer tag datum =
  Redeemers @era $ Map.singleton tag (datum, ExUnits 5000 5000)
