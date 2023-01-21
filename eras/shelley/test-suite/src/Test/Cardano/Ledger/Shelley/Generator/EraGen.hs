{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

-- | Infrastructure for generating STS Traces over any Era
module Test.Cardano.Ledger.Shelley.Generator.EraGen (
  genUtxo0,
  genesisId,
  EraGen (..),
  MinLEDGER_STS,
  MinCHAIN_STS,
  MinUTXO_STS,
  MinGenTxout (..),
  Label (..),
  Sets (..),
  someKeyPairs,
  allScripts,
  mkDummyHash,
)
where

import qualified Cardano.Crypto.Hash as Hash
import Cardano.Ledger.AuxiliaryData (AuxiliaryDataHash)
import Cardano.Ledger.BaseTypes (Network (..), ShelleyBase, StrictMaybe)
import qualified Cardano.Ledger.Binary.Plain as Plain
import Cardano.Ledger.Coin (Coin (..))
import Cardano.Ledger.Core
import qualified Cardano.Ledger.Crypto as CC (Crypto, HASH)
import Cardano.Ledger.Keys (KeyRole (Witness))
import Cardano.Ledger.Pretty (PrettyA (..))
import Cardano.Ledger.SafeHash (unsafeMakeSafeHash)
import Cardano.Ledger.Shelley.API (
  Addr (Addr),
  Block (..),
  Credential (ScriptHashObj),
  LedgerEnv,
  LedgerState,
  ShelleyLedgersEnv,
  StakeReference (StakeRefBase),
 )
import Cardano.Ledger.Shelley.LedgerState (StashedAVVMAddresses, UTxOState (..))
import Cardano.Ledger.Shelley.PParams (Update)
import Cardano.Ledger.Shelley.Rules (UtxoEnv)
import Cardano.Ledger.Shelley.TxBody (DCert, ShelleyEraTxBody, WitVKey, Withdrawals)
import Cardano.Ledger.TxIn (TxId (TxId), TxIn)
import Cardano.Ledger.UTxO (UTxO)
import Cardano.Protocol.TPraos.BHeader (BHeader)
import Cardano.Slotting.Slot (SlotNo)
import Control.State.Transition.Extended (STS (..))
import qualified Data.ByteString as BS
import Data.Default.Class (Default)
import Data.Hashable (Hashable (..))
import Data.Map (Map)
import Data.Sequence (Seq)
import Data.Sequence.Strict (StrictSeq)
import Data.Set (Set)
import Lens.Micro
import Test.Cardano.Ledger.Binary.Random (mkDummyHash)
import Test.Cardano.Ledger.Core.KeyPair (KeyPairs, mkAddr)
import Test.Cardano.Ledger.Shelley.Generator.Constants (Constants (..))
import Test.Cardano.Ledger.Shelley.Generator.Core (
  GenEnv (..),
  ScriptInfo,
  TwoPhase2ArgInfo (..),
  TwoPhase3ArgInfo (..),
  genesisCoins,
 )
import Test.Cardano.Ledger.Shelley.Generator.ScriptClass (
  ScriptClass,
  baseScripts,
  combinedScripts,
  keyPairs,
 )
import Test.Cardano.Ledger.Shelley.Rules.Chain (CHAIN, ChainState)
import Test.Cardano.Ledger.Shelley.Utils (Split)
import Test.QuickCheck (Gen, choose, shuffle)

{------------------------------------------------------------------------------
 An EraGen instance makes it possible to run the Shelley property tests. The idea
 is to generate (not fully) random Transactions, i.e. with enough coherency to be
 a real transaction, but which are strung together into Traces. In each step of the trace
 one of these (not fully) random transactions is applied. The idea is that some property should
 hold on any trace. Because we want these tests to work in any Era there are two things
 to consider:
 1) A Transaction generator needs to be parametric over all Eras.
 2) Since the Internals of the STS rules differ from Era to Era, the STS instances
    must also adapt to many Eras.

 To account for the "not fully" random nature of tranactions we use the type GenEnv which
 holds enough information to build "not fully" random transactions that are still coherent.

 For Transactions, we account for these differences by using the type families found in
 Cardano.Ledger.Core and other modules, and by a set of Era specific generators encoded
 in the EraGen class. Generally there is some "method" in the class for each type family.

 For traces we use the "class HasTrace (CHAIN era) (GenEnv era)"

 The following constraints encode the minimal properties needed to build a chain for
 any Era. It should be an invariant that these properties hold for all Eras (Shelley, Allegra, Mary, Alonzo ...)
 If we introduce a new Era, where they do not hold, we must adjust these things, so they do.
 1) Add a new type family
 2) Add new methods to EraGen
 3) Change the minimal constraints, so that they now hold for all Eras
 4) Change the generators to use the new methods.

 -----------------------------------------------------------------------------}

-- | Minimal requirements on the LEDGER and LEDGERS instances
type MinLEDGER_STS era =
  ( Environment (EraRule "LEDGERS" era) ~ ShelleyLedgersEnv era
  , BaseM (EraRule "LEDGER" era) ~ ShelleyBase
  , Signal (EraRule "LEDGER" era) ~ Tx era
  , State (EraRule "LEDGER" era) ~ LedgerState era
  , Environment (EraRule "LEDGER" era) ~ LedgerEnv era
  , BaseM (EraRule "LEDGERS" era) ~ ShelleyBase
  , State (EraRule "LEDGERS" era) ~ LedgerState era
  , Signal (EraRule "LEDGERS" era) ~ Seq (Tx era)
  , STS (EraRule "LEDGER" era)
  )

-- | Minimal requirements on the CHAIN instances
type MinCHAIN_STS era =
  ( STS (CHAIN era)
  , BaseM (CHAIN era) ~ ShelleyBase
  , Environment (CHAIN era) ~ ()
  , State (CHAIN era) ~ ChainState era
  , Signal (CHAIN era) ~ Block (BHeader (EraCrypto era)) era
  )

-- | Minimal requirements on the UTxO instances
type MinUTXO_STS era =
  ( STS (EraRule "UTXOW" era)
  , BaseM (EraRule "UTXOW" era) ~ ShelleyBase
  , State (EraRule "UTXOW" era) ~ UTxOState era
  , Environment (EraRule "UTXOW" era) ~ UtxoEnv era
  , Signal (EraRule "UTXOW" era) ~ Tx era
  , State (EraRule "UTXO" era) ~ UTxOState era
  , Environment (EraRule "UTXO" era) ~ UtxoEnv era
  , Signal (EraRule "UTXO" era) ~ Tx era
  )

class Show (TxOut era) => MinGenTxout era where
  calcEraMinUTxO :: TxOut era -> PParams era -> Coin
  addValToTxOut :: Value era -> TxOut era -> TxOut era
  genEraTxOut :: GenEnv era -> Gen (Value era) -> [Addr (EraCrypto era)] -> Gen [TxOut era]

-- ======================================================================================
-- The EraGen class. Generally one method for each type family in Cardano.Ledger.Core
-- ======================================================================================

class
  ( EraSegWits era
  , ShelleyEraTxBody era
  , Split (Value era)
  , ScriptClass era
  , EraPParams era
  , MinGenTxout era
  , PrettyA (Tx era)
  , PrettyA (TxBody era)
  , PrettyA (TxWits era)
  , PrettyA (Value era)
  , Default (StashedAVVMAddresses era)
  ) =>
  EraGen era
  where
  -- | Generate a genesis value for the Era
  genGenesisValue :: GenEnv era -> Gen (Value era)

  -- | A list of three-phase scripts that can be chosen for payment when building a transaction
  genEraTwoPhase3Arg :: [TwoPhase3ArgInfo era]
  genEraTwoPhase3Arg = []

  -- | A list of two-phase scripts that can be chosen for Delegating, Minting, or Rewarding when building a transaction
  genEraTwoPhase2Arg :: [TwoPhase2ArgInfo era]
  genEraTwoPhase2Arg = []

  -- | Given some pre-generated data, generate an era-specific TxBody,
  -- and a list of additional scripts for eras that sometimes require
  -- additional script witnessing.
  genEraTxBody ::
    GenEnv era ->
    UTxO era ->
    PParams era ->
    SlotNo ->
    Set (TxIn (EraCrypto era)) ->
    StrictSeq (TxOut era) ->
    StrictSeq (DCert (EraCrypto era)) ->
    Withdrawals (EraCrypto era) ->
    Coin ->
    StrictMaybe (Update era) ->
    StrictMaybe (AuxiliaryDataHash (EraCrypto era)) ->
    Gen (TxBody era, [Script era])

  -- | Generate era-specific auxiliary data
  genEraAuxiliaryData :: Constants -> Gen (StrictMaybe (TxAuxData era))

  -- | Update an era-specific TxBody
  updateEraTxBody ::
    UTxO era ->
    PParams era ->
    TxWits era ->
    TxBody era ->
    Coin ->
    -- | This overrides the existing TxFee
    Set (TxIn (EraCrypto era)) ->
    -- | This is to be Unioned with the existing TxIn
    TxOut era ->
    -- | This is to be Appended to the end of the existing TxOut
    TxBody era

  -- |  Union the TxIn with the existing TxIn in the TxBody
  addInputs :: TxBody era -> Set (TxIn (EraCrypto era)) -> TxBody era
  addInputs txb _ins = txb

  genEraPParamsUpdate :: Constants -> PParams era -> Gen (PParamsUpdate era)

  genEraPParams :: Constants -> Gen (PParams era)

  -- Its is VERY IMPORTANT that the decentralisation parameter "_d" be non-zero and less than 1.
  -- The system will deadlock if d==0 and there are no registered stake pools.
  -- use Test.Cardano.Ledger.Shelley.Generator.Update(genDecentralisationParam) in your instance.

  genEraTxWits ::
    (UTxO era, TxBody era, ScriptInfo era) ->
    Set (WitVKey 'Witness (EraCrypto era)) ->
    Map (ScriptHash (EraCrypto era)) (Script era) ->
    TxWits era

  -- When choosing new recipients from the UTxO, choose only those whose Outputs meet this predicate.
  genEraGoodTxOut :: TxOut era -> Bool
  genEraGoodTxOut _ = True -- The default implementation marks every TxOut as good.

  -- | Construct a transaction given its constituent parts.
  constructTx ::
    TxBody era ->
    TxWits era ->
    StrictMaybe (TxAuxData era) ->
    Tx era
  constructTx txBody txWits txAuxData =
    mkBasicTx txBody & witsTxL .~ txWits & auxDataTxL .~ txAuxData

  -- | compute the delta cost of an additional script on  per Era basis.
  genEraScriptCost :: PParams era -> Script era -> Coin
  genEraScriptCost _pp _script = Coin 0

  -- | A final opportunity to tweak things when the generator is done. Possible uses
  --   1) Add tracing when debugging on a per Era basis
  genEraDone :: UTxO era -> PParams era -> Tx era -> Gen (Tx era)
  genEraDone _utxo _pp x = pure x

  -- | A final opportunity to tweak things at the block level. Possible uses
  --   2) Run a test that might decide to 'discard' the test, because we got unlucky, and a rare unfixible condition has occurred.
  genEraTweakBlock :: PParams era -> Seq (Tx era) -> Gen (Seq (Tx era))
  genEraTweakBlock _pp seqTx = pure seqTx

  hasFailedScripts :: Tx era -> Bool
  hasFailedScripts = const False

  feeOrCollateral :: Tx era -> UTxO era -> Coin
  feeOrCollateral tx _ = tx ^. bodyTxL . feeTxBodyL

{------------------------------------------------------------------------------
  Generators shared across eras
 -----------------------------------------------------------------------------}

-- | Select between _lower_ and _upper_ keys from 'keyPairs'
someKeyPairs :: CC.Crypto c => Constants -> Int -> Int -> Gen (KeyPairs c)
someKeyPairs c lower upper =
  take
    <$> choose (lower, upper)
    <*> shuffle (keyPairs c)

genUtxo0 :: forall era. EraGen era => GenEnv era -> Gen (UTxO era)
genUtxo0 ge@(GenEnv _ _ c@Constants {minGenesisUTxOouts, maxGenesisUTxOouts}) = do
  genesisKeys <- someKeyPairs c minGenesisUTxOouts maxGenesisUTxOouts
  genesisScripts <- someScripts @era c minGenesisUTxOouts maxGenesisUTxOouts
  outs <-
    (genEraTxOut @era ge)
      (genGenesisValue @era ge)
      (fmap mkAddr genesisKeys ++ fmap (scriptsmkAddr' Testnet) genesisScripts)
  return (genesisCoins genesisId outs)
  where
    scriptsmkAddr' :: Network -> (Script era, Script era) -> Addr (EraCrypto era)
    scriptsmkAddr' n (payScript, stakeScript) =
      Addr n (scriptToCred' payScript) (StakeRefBase $ scriptToCred' stakeScript)

    scriptToCred' :: Script era -> Credential kr (EraCrypto era)
    scriptToCred' = ScriptHashObj . hashScript @era

-- | We share this dummy TxId as genesis transaction id across eras
genesisId ::
  Hash.HashAlgorithm (CC.HASH c) =>
  TxId c
genesisId = TxId (unsafeMakeSafeHash (mkDummyHash (0 :: Int)))

-- ==========================================================

-- | Select between _lower_ and _upper_ scripts from the possible combinations
-- of the first `numBaseScripts` multi-sig scripts of `mSigScripts` (i.e compound scripts) AND
-- some simple scripts (NOT compound. ie either signature or Plutus scripts).
someScripts ::
  forall era.
  EraGen era =>
  Constants ->
  Int ->
  Int ->
  Gen [(Script era, Script era)]
someScripts c lower upper = take <$> choose (lower, upper) <*> shuffle (allScripts @era c)

-- | A list of all possible kinds of scripts in the current Era.
--   Might include Keylocked scripts, Start-Finish Timelock scripts, Quantified scripts (All, Any, MofN), Plutus Scripts
--   Note that 'genEraTwoPhase3Arg' and 'genEraTwoPhase2Arg' may be the empty list ([]) in some Eras.
allScripts :: forall era. EraGen era => Constants -> [(Script era, Script era)]
allScripts c =
  plutusPairs genEraTwoPhase3Arg genEraTwoPhase2Arg (take 3 simple)
    ++ take (numSimpleScripts c) simple -- 10 means about 5% of allScripts are Plutus Scripts
    -- Plutus scripts in some Eras ([] in other Eras)
    -- [(payment,staking)] where the either payment or staking may be a plutus script
    ++
    -- Simple scripts (key locked, Start-Finish timelocks)
    combinedScripts @era c
  where
    -- Quantifed scripts (All, Any, MofN)

    simple = baseScripts @era c
    plutusPairs :: [TwoPhase3ArgInfo era] -> [TwoPhase2ArgInfo era] -> [(Script era, Script era)] -> [(Script era, Script era)]
    plutusPairs [] _ _ = []
    plutusPairs _ [] _ = []
    plutusPairs _ _ [] = []
    plutusPairs args3 args2 ((pay, stake) : more) = pair : plutusPairs args3 args2 more
      where
        count3 = length args3 - 1
        count2 = length args2 - 1
        payBytes = Plain.serializeEncoding' $ Plain.encCBOR pay
        n = randomByHash 0 count3 $ Plain.serializeEncoding' $ Plain.encCBOR stake
        m = randomByHash 0 count2 payBytes
        mode = randomByHash 1 3 payBytes
        pair = case mode of
          1 -> (getScript3 (args3 !! n), stake)
          2 -> (pay, getScript2 (args2 !! m))
          3 -> (getScript3 (args3 !! n), getScript2 (args2 !! m))
          i -> error ("mod function returns value out of bounds: " ++ show i)

randomByHash :: Int -> Int -> BS.ByteString -> Int
randomByHash low high x = low + remainder
  where
    n = hash x
    -- We don't really care about the hash, we only
    -- use it to pseudo-randomly pick a number bewteen low and high
    m = high - low + 1
    remainder = mod n m -- mode==0 is a time leaf,  mode 1 or 2 is a signature leaf

-- =========================================================

data Label t where
  Body' :: Label (TxBody era)
  Wits' :: Label (TxWits era)

class Sets (x :: Label t) y where
  set :: Label t -> y -> y
