{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Test.Cardano.Ledger.Constrained.Trace.SimpleTx where

import Cardano.Crypto.Signing (SigningKey)
import Cardano.Ledger.Address (Addr (..))
import Cardano.Ledger.Alonzo.Tx (IsValid (..))
import Cardano.Ledger.Alonzo.TxWits (TxDats (..))
import Cardano.Ledger.Alonzo.UTxO (AlonzoScriptsNeeded (..))
import Cardano.Ledger.BaseTypes (TxIx, inject)
import Cardano.Ledger.Coin (Coin (..))
import Cardano.Ledger.Core (
  Era (..),
  EraTx (..),
  EraTxBody (..),
  EraTxOut (..),
  Script,
  Tx,
  TxBody,
  Value,
  coinTxOutL,
 )
import Cardano.Ledger.Credential (Credential (..), StakeReference (..))
import Cardano.Ledger.Hashes (DataHash, ScriptHash)
import Cardano.Ledger.Keys (GenDelegs (..), KeyHash (..), KeyRole (..))
import Cardano.Ledger.Mary.Value (MaryValue (..), MultiAsset (..), PolicyID (..))
import Cardano.Ledger.Plutus (ExUnits (..))
import Cardano.Ledger.Plutus.Data (Data (..))
import Cardano.Ledger.Shelley.LedgerState (LedgerState (..))
import Cardano.Ledger.Shelley.Rules (LedgerEnv (..))
import Cardano.Ledger.Shelley.UTxO (ShelleyScriptsNeeded (..))
import Cardano.Ledger.UTxO (EraUTxO (..), UTxO (..), getScriptsNeeded)
import Cardano.Ledger.Val (Val (..))
import Data.Foldable (toList)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Sequence.Strict (StrictSeq (Empty, (:<|)))
import Data.Set (Set)
import qualified Data.Set as Set
import qualified Debug.Trace as Debug
import Lens.Micro
import qualified PlutusLedgerApi.V1 as PV1
import Test.Cardano.Ledger.Constrained.Ast (runTarget, runTerm)
import Test.Cardano.Ledger.Constrained.Classes (
  PParamsF (..),
  PlutusPointerF (..),
  PlutusPurposeF (..),
  ScriptF (..),
  TxBodyF (..),
  TxCertF (..),
  TxF (..),
  TxOutF (..),
  liftUTxO,
  unScriptF,
 )
import Test.Cardano.Ledger.Constrained.Env (Env)
import Test.Cardano.Ledger.Constrained.Monad (Typed)
import Test.Cardano.Ledger.Constrained.Preds.Tx (
  adjustNeededByRefScripts,
  allValid,
  bootWitsT,
  computeFinalFee,
  getPlutusDataHashes,
  getRdmrPtrs,
  hashBody,
  makeKeyWitness,
  minusMultiValue,
  necessaryKeyHashes,
  sufficientKeyHashes,
 )
import Test.Cardano.Ledger.Constrained.Trace.TraceMonad (
  TraceM,
  fromSetTerm,
  getTerm,
  liftGen,
  reqSig,
 )
import qualified Test.Cardano.Ledger.Constrained.Trace.TraceMonad as TraceMonad (refInputs)
import Test.Cardano.Ledger.Constrained.Vars
import Test.Cardano.Ledger.Core.KeyPair (KeyPair (..))
import Test.Cardano.Ledger.Generic.Fields (TxBodyField (..), TxField (..), WitnessesField (..))
import Test.Cardano.Ledger.Generic.GenState (mkRedeemers)
import Test.Cardano.Ledger.Generic.Proof (
  Proof (..),
  Reflect (..),
  UTxOWit (..),
  ValueWit (..),
  whichUTxO,
  whichValue,
 )
import Test.Cardano.Ledger.Generic.Updaters (merge, newTx, newTxBody, newWitnesses)
import Test.QuickCheck (discard, shuffle)

-- ===================================================

-- | Given an (Env era) construct the pair the the LendgerEnv and the LedgerState
getSTSLedgerEnv ::
  Reflect era => Proof era -> TxIx -> Env era -> Typed (LedgerEnv era, LedgerState era)
getSTSLedgerEnv proof txIx env = do
  ledgerstate <- runTarget env (ledgerStateT proof)
  slot <- runTerm env currentSlot
  (PParamsF _ pp) <- runTerm env (pparams proof)
  accntState <- runTarget env accountStateT
  pure (LedgerEnv slot txIx pp accntState False, ledgerstate)

-- ====================================================================
-- Picking things that have no Plutus Scripts inside. To make very
-- simple Transactions we need to avoid plutus scripts as they require
-- Collateral inputs and hashes that ensure prootocl versions match.
-- ====================================================================

-- | Does a TxOut have only Non-Plutus Scripts. Non-Plutus status is measured by non-membership
--   in the Map of script hashes of all Plutus scripts.
plutusFree :: forall era a. Reflect era => Map (ScriptHash (EraCrypto era)) a -> TxOut era -> Bool
plutusFree plutusmap txout =
  plutusFreeAddr plutusmap (txout ^. addrTxOutL)
    && plutusFreeValue (reify @era) plutusmap (txout ^. valueTxOutL)

plutusFreeAddr :: Map (ScriptHash c) a -> Addr c -> Bool
plutusFreeAddr plutusmap addr = case addr of
  Addr _ (ScriptHashObj h1) (StakeRefBase (ScriptHashObj h2)) -> Map.notMember h1 plutusmap && Map.notMember h2 plutusmap
  Addr _ (ScriptHashObj h1) _ -> Map.notMember h1 plutusmap
  Addr _ _ (StakeRefBase (ScriptHashObj h2)) -> Map.notMember h2 plutusmap
  _ -> True

plutusFreeValue :: Proof era -> Map (ScriptHash (EraCrypto era)) a -> Value era -> Bool
plutusFreeValue proof plutusmap v = case (whichValue proof, v) of
  (ValueShelleyToAllegra, _) -> True
  (ValueMaryToConway, MaryValue _ (MultiAsset m)) -> all (plutusFreePolicyID plutusmap) (Map.keysSet m)

plutusFreePolicyID :: Map (ScriptHash c) a -> PolicyID c -> Bool
plutusFreePolicyID plutusmap (PolicyID h) = Map.notMember h plutusmap

plutusFreeCredential :: Map (ScriptHash c) a -> Credential kr c -> Bool
plutusFreeCredential _ (KeyHashObj _) = True
plutusFreeCredential plutusmap (ScriptHashObj h) = Map.notMember h plutusmap

-- ====================================================================
-- Code to generate simple, but valid Transactions
-- ====================================================================

-- | Make a simple TxBody with 1 input and 1 output. We estimate that such a TxBody will lead to a fee
--   of less than 'feeEstimate'. So only pick inputs from the utxo that have at least that much coin. Balance the
--   Coin in the TxOut with the feeEstimate and the actual Coin in the UTxO output that corresponds to the input.
--   Only works if the internal Env of the TraceM monad stores variables capable of creating the LedgerState
--   See 'genLedgerStateEnv' for an example of how to do that.
simpleTxBody :: Reflect era => Proof era -> Coin -> TraceM era [TxBodyField era]
simpleTxBody proof feeEstimate = do
  plutusmap <- getTerm plutusUniv
  let ok (_, TxOutF _ v) = (v ^. coinTxOutL) >= feeEstimate && plutusFree plutusmap v
  utxopairs <- (filter ok . Map.toList) <$> getTerm (utxo proof)
  (input, TxOutF _ out) <- do
    zs <- liftGen (shuffle utxopairs)
    case zs of
      [] ->
        Debug.trace
          ( "There are no entries in the UTxO that are big enough for the feeEstimate: "
              ++ show feeEstimate
              ++ " Discard"
          )
          discard
      (x : _) -> pure x
  let inputCoin = out ^. coinTxOutL
  addr <- fromSetTerm addrUniv
  vldt <- getTerm validityInterval
  let output = mkBasicTxOut addr (inject (inputCoin <-> feeEstimate))
  pure
    [ Inputs' [input]
    , Outputs' [output]
    , Txfee feeEstimate
    , Vldt vldt
    , Mint (liftMultiAsset (minusMultiValue proof (output ^. valueTxOutL) (out ^. valueTxOutL)))
    ]

-- | Generate a (Tx era) from a simple (TxBody era), with 1 input and 1 output.
--   Apply the "finishing" function 'completeTxBody' to make the result a valid Tx.
--   Only works if the internal Env of the TraceM monad stores variables capable of
--   creating the LedgerState. The parameter 'maxFeeEstimate' has to be chosen by
--   experience. The fee for most simpleTx as less than 60000. But in at least one
--   case we have seen as high as 108407. If that case happens we will discard.
--   A large fee is rare because it is caused by many scripts and fees that need large witnesses,
simpleTx :: Reflect era => Proof era -> Coin -> TraceM era (Tx era)
simpleTx proof maxFeeEstimate = do
  fields <- simpleTxBody proof maxFeeEstimate
  let txb = newTxBody proof fields
  completeTxBody proof maxFeeEstimate txb

-- ====================================================================================
-- Once we have a TxBody, we need to make a complete Tx, By filling in Missing pieces.
-- ====================================================================================

-- | Complete a TxBody, by running a fix-point computation that
--   1) Adds the appropriate witnesses.
--   2) Adjusts the the first output to pay the estimated fee.
--   Run the computation until both the fee and the hash of the TxBody reach a fixpoint.
--   Only works if the internal Env of the TraceM monad stores variables capable of creating the LedgerState
completeTxBody :: Reflect era => Proof era -> Coin -> TxBody era -> TraceM era (Tx era)
completeTxBody proof maxFeeEstimate txBody = do
  u <- getTerm (utxo proof)
  scriptuniv <- getTerm (allScriptUniv proof)
  plutusuniv <- getTerm plutusUniv
  byronuniv <- getTerm byronAddrUniv
  datauniv <- getTerm dataUniv
  gd <- getTerm genDelegs
  pp <- getTerm (pparams proof)
  keymapuniv <- getTerm keymapUniv
  -- compute the inital inputs to the fix-point computation
  let hash1 = hashBody proof txBody
      tx =
        addWitnesses
          proof
          (fmap unScriptF scriptuniv)
          plutusuniv
          byronuniv
          keymapuniv
          datauniv
          txBody
          (liftUTxO u)
          (GenDelegs gd)
      initialfee = computeFinalFee pp (TxF proof tx) u
  let loop _ _ fee _ | fee >= maxFeeEstimate = Debug.trace ("LOOP: fee >= maxFeeEstimate, Discard") $ discard
      loop count txx fee hash = do
        let adjustedtx = adjustTxForFee proof fee txx
            txb = adjustedtx ^. bodyTxL
            hash2 = hashBody proof txb
            completedtx =
              addWitnesses
                proof
                (fmap unScriptF scriptuniv)
                plutusuniv
                byronuniv
                keymapuniv
                datauniv
                txb
                (liftUTxO u)
                (GenDelegs gd)
            newfee = computeFinalFee pp (TxF proof completedtx) u
        if (fee == newfee) && (hash == hash2)
          then pure completedtx
          else
            if count > (0 :: Int)
              then loop (count - 1) completedtx newfee hash2
              else Debug.trace "LOOP: count <= 0, fee is oscillating, Discard" discard
  loop 10 tx initialfee hash1

-- ========================================================================================

-- | Add witnesses to the TxBody to construct a Tx with the appropriate witnesses.
--   This is a compilcated function, but it should be applicable to ANY Tx generated using
--   the Universes.
addWitnesses ::
  forall era.
  Reflect era =>
  Proof era ->
  Map (ScriptHash (EraCrypto era)) (Script era) ->
  Map (ScriptHash (EraCrypto era)) (IsValid, ScriptF era) ->
  Map (KeyHash 'Payment (EraCrypto era)) (Addr (EraCrypto era), SigningKey) ->
  Map (KeyHash 'Witness (EraCrypto era)) (KeyPair 'Witness (EraCrypto era)) ->
  Map (DataHash (EraCrypto era)) (Data era) ->
  TxBody era ->
  UTxO era ->
  GenDelegs (EraCrypto era) ->
  Tx era
addWitnesses proof scriptUniv plutusuniv byronuniv keymapuniv datauniv txb ut gd = tx
  where
    needed = getScriptsNeeded ut txb
    neededWits, scriptwits :: Map (ScriptHash (EraCrypto era)) (Script era)
    plutusValids :: [IsValid]
    rptrs :: Set (PlutusPointerF era)
    dataw :: Map (DataHash (EraCrypto era)) (Data era)
    (scriptwits, neededWits, plutusValids, rptrs, dataw) = case whichUTxO proof of
      UTxOShelleyToMary -> (witss, witss, [], Set.empty, Map.empty)
        where
          ShelleyScriptsNeeded setneed = needed
          witss = Map.restrictKeys scriptUniv setneed
      UTxOAlonzoToConway ->
        let AlonzoScriptsNeeded xs = needed
            xs' = [(PlutusPurposeF proof p, d) | (p, d) <- xs]
            neededHashset = Set.fromList (fmap snd xs)
            refAdjusted =
              adjustNeededByRefScripts
                proof
                (txb ^. inputsTxBodyL)
                (TraceMonad.refInputs proof txb)
                (fmap (TxOutF proof) (unUTxO ut))
                neededHashset
            validities = fmap fst (Map.elems (Map.restrictKeys plutusuniv neededHashset))
         in ( Map.restrictKeys scriptUniv refAdjusted
            , Map.restrictKeys scriptUniv neededHashset
            , validities
            , getRdmrPtrs xs' plutusuniv
            , Map.restrictKeys
                datauniv
                ( getPlutusDataHashes
                    (fmap (TxOutF proof) (unUTxO ut))
                    (TxBodyF proof txb)
                    (Map.map (ScriptF proof) scriptUniv)
                )
            )
    sufficient =
      sufficientKeyHashes
        proof
        (fmap (ScriptF proof) neededWits)
        (toList (fmap (TxCertF proof) (txb ^. certsTxBodyL)))
        (unGenDelegs gd)
    necessaryKH =
      necessaryKeyHashes
        (TxBodyF proof txb)
        (fmap (TxOutF proof) (unUTxO ut))
        (unGenDelegs gd)
        (reqSig proof txb)
    keywits =
      makeKeyWitness
        (TxBodyF proof txb)
        necessaryKH
        sufficient
        keymapuniv
        (fmap (ScriptF proof) scriptwits)
        (unGenDelegs gd)
        byronuniv
    bootwits =
      bootWitsT
        proof
        (Map.restrictKeys (fmap (TxOutF proof) (unUTxO ut)) (txb ^. inputsTxBodyL))
        (TxBodyF proof txb)
        byronuniv
    redeem =
      Set.foldl'
        (\ans (PlutusPointerF _ x) -> (x, (Data @era (PV1.I 1), ExUnits 3 3)) : ans)
        -- It doesn't actuallly matter what the Data is, and the ExUnits only need to be small
        -- as maxTxExUnits is usually something big like (ExUnits mem=1217 data=3257)
        []
        rptrs
    wits =
      newWitnesses
        merge
        proof
        [ AddrWits keywits
        , BootWits bootwits
        , ScriptWits scriptwits
        , DataWits (TxDats dataw)
        , RdmrWits (mkRedeemers proof redeem)
        ]
    tx =
      newTx
        proof
        [ Body txb
        , TxWits wits
        , --  , AuxData' (fixM auxs)
          Valid (allValid plutusValids)
        ]

-- =======================================================================

-- | adjust a Tx for the fee, by setting the fee to the correct value then
--   moving the excess to the outputs
adjustTxForFee :: EraTx era => Proof era -> Coin -> Tx era -> Tx era
adjustTxForFee _proof actualfee tx =
  tx
    & feeCoinL
      .~ actualfee
    & firstOutputCoinL
      .~ (outputCoin <+> (currentfee <-> actualfee))
  where
    currentfee = tx ^. feeCoinL
    outputCoin = tx ^. firstOutputCoinL

feeCoinL :: EraTx era => Lens' (Tx era) Coin
feeCoinL = bodyTxL . feeTxBodyL

firstOutputCoinL :: EraTx era => Lens' (Tx era) Coin
firstOutputCoinL = bodyTxL . outputsTxBodyL . strictSeqHeadL . coinTxOutL

strictSeqHeadL :: Lens' (StrictSeq a) a
strictSeqHeadL =
  lens
    gethead
    ( \x h -> case x of
        (_ :<| xs) -> h :<| xs
        Empty -> h :<| Empty
    )
  where
    gethead (x :<| _) = x
    gethead _ = error "Empty sequence in strictSeqHeadL"
