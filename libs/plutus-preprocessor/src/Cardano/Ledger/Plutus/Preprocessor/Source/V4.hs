{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE TemplateHaskell #-}

module Cardano.Ledger.Plutus.Preprocessor.Source.V4 where

import Language.Haskell.TH
import qualified PlutusLedgerApi.Data.V4 as PV4D
import PlutusTx (fromBuiltinData, unsafeFromBuiltinData)
import qualified PlutusTx.Builtins as P
import qualified PlutusTx.Data.AssocMap as PAMD
import qualified PlutusTx.Data.List as PLD
import qualified PlutusTx.Prelude as P

alwaysSucceedsNoDatumQ :: Q [Dec]
alwaysSucceedsNoDatumQ =
  [d|
    alwaysSucceedsNoDatum :: P.BuiltinData -> P.BuiltinUnit
    alwaysSucceedsNoDatum arg =
      let PV4D.ScriptContext _txInfo (PV4D.Redeemer _redeemer) scriptInfo _sh =
            P.unsafeFromBuiltinData arg
       in P.check $
            case scriptInfo of
              -- We fail if this is a spending script with a Datum
              PV4D.SpendingScript _ (Just _) -> False
              _ -> True
    |]

alwaysSucceedsWithDatumQ :: Q [Dec]
alwaysSucceedsWithDatumQ =
  [d|
    alwaysSucceedsWithDatum :: P.BuiltinData -> P.BuiltinUnit
    alwaysSucceedsWithDatum arg =
      let PV4D.ScriptContext _txInfo (PV4D.Redeemer _redeemer) scriptPurpose _sh =
            P.unsafeFromBuiltinData arg
       in P.check $
            case scriptPurpose of
              PV4D.SpendingScript _ (Just _) -> True
              -- Expecting a spending script with a Datum, thus failing when it is not
              _ -> False
    |]

alwaysFailsNoDatumQ :: Q [Dec]
alwaysFailsNoDatumQ =
  [d|
    alwaysFailsNoDatum :: P.BuiltinData -> P.BuiltinUnit
    alwaysFailsNoDatum arg =
      P.check $
        case fromBuiltinData arg of
          Just (PV4D.ScriptContext _txInfo (PV4D.Redeemer _redeemer) scriptInfo _sh) ->
            case scriptInfo of
              -- We fail only if this is not a spending script with a Datum
              PV4D.SpendingScript _ (Just _) -> True
              _ -> False
          Nothing -> True
    |]

alwaysFailsWithDatumQ :: Q [Dec]
alwaysFailsWithDatumQ =
  [d|
    alwaysFailsWithDatum :: P.BuiltinData -> P.BuiltinUnit
    alwaysFailsWithDatum arg =
      P.check $
        case fromBuiltinData arg of
          Just (PV4D.ScriptContext _txInfo (PV4D.Redeemer _redeemer) scriptInfo _sh) ->
            case scriptInfo of
              -- We fail only if this is a spending script with a Datum
              PV4D.SpendingScript _ (Just _) -> False
              _ -> True
          Nothing -> True
    |]

redeemerSameAsDatumQ :: Q [Dec]
redeemerSameAsDatumQ =
  [d|
    redeemerSameAsDatum :: P.BuiltinData -> P.BuiltinUnit
    redeemerSameAsDatum arg =
      P.check $
        case unsafeFromBuiltinData arg of
          PV4D.ScriptContext
            _txInfo
            (PV4D.Redeemer redeemer)
            (PV4D.SpendingScript _ (Just (PV4D.Datum datum)))
            _sh ->
              -- Expecting a spending script with a Datum, thus failing when it is not
              datum P.== redeemer
          _ -> False
    |]

evenDatumQ :: Q [Dec]
evenDatumQ =
  [d|
    evenDatum :: P.BuiltinData -> P.BuiltinUnit
    evenDatum arg =
      P.check $
        case unsafeFromBuiltinData arg of
          PV4D.ScriptContext _txInfo _redeemer (PV4D.SpendingScript _ (Just (PV4D.Datum datum))) _sh ->
            -- Expecting a spending script with a Datum, thus failing when it is not
            P.modulo (P.unsafeDataAsI datum) 2 P.== 0
    |]

evenRedeemerNoDatumQ :: Q [Dec]
evenRedeemerNoDatumQ =
  [d|
    evenRedeemerNoDatum :: P.BuiltinData -> P.BuiltinUnit
    evenRedeemerNoDatum arg =
      P.check $
        case unsafeFromBuiltinData arg of
          PV4D.ScriptContext _txInfo (PV4D.Redeemer redeemer) scriptInfo _sh ->
            case scriptInfo of
              -- Expecting No Datum, therefore should fail when it is supplied
              PV4D.SpendingScript _ (Just _) -> False
              _ -> P.modulo (P.unsafeDataAsI redeemer) 2 P.== 0
    |]

evenRedeemerWithDatumQ :: Q [Dec]
evenRedeemerWithDatumQ =
  [d|
    evenRedeemerWithDatum :: P.BuiltinData -> P.BuiltinUnit
    evenRedeemerWithDatum arg =
      P.check $
        case unsafeFromBuiltinData arg of
          PV4D.ScriptContext _txInfo (PV4D.Redeemer redeemer) (PV4D.SpendingScript _ (Just _)) _sh ->
            -- Expecting a spending script with a Datum, thus failing when it is not
            P.modulo (P.unsafeDataAsI redeemer) 2 P.== 0
          _ -> False
    |]

purposeIsWellformedNoDatumQ :: Q [Dec]
purposeIsWellformedNoDatumQ =
  [d|
    purposeIsWellformedNoDatum :: P.BuiltinData -> P.BuiltinUnit
    purposeIsWellformedNoDatum arg =
      P.check $
        case unsafeFromBuiltinData arg of
          PV4D.ScriptContext
            PV4D.TxInfo
              { PV4D.txInfoMint = infoMint
              , PV4D.txInfoInputs = infoInputs
              , PV4D.txInfoTxCerts = infoTxCerts
              , PV4D.txInfoVotes = infoVotes
              , PV4D.txInfoWithdrawals = infoWithdrawals
              , PV4D.txInfoGuards = infoGuards
              , PV4D.txInfoSubTxIx = infoSubTxIx
              }
            _redeemer
            scriptInfo
            sh -> case scriptInfo of
              PV4D.MintingScript cs ->
                PAMD.member cs $ PV4D.getValue $ PV4D.mintValueMinted infoMint
              -- Expecting No Datum, therefore should fail when it is supplied
              PV4D.SpendingScript txOutRef mDatum ->
                case mDatum of
                  Just _ -> False
                  Nothing ->
                    not $ PLD.null $ PLD.filter ((txOutRef P.==) . PV4D.txInInfoOutRef) infoInputs
              PV4D.CertifyingScript _idx txCert ->
                not $ PLD.null $ PLD.filter (txCert P.==) infoTxCerts
              PV4D.VotingScript voter ->
                PAMD.member voter infoVotes
              PV4D.ProposingScript _idx _propProc -> True
              -- Eq instance for proposals have been removed. I am not sure if this is a
              -- good idea, but it only affects ledger test script, so we'll deal with
              -- this later:
              --
              -- null $ P.filter (propProc P.==) $ PV4.txInfoProposalProcedures txInfo
              PV4D.WithdrawingScript (PV4D.AccountId account) -> PAMD.member account infoWithdrawals
              PV4D.GuardingScript ix topTxInfo ->
                (PV4D.ScriptCredential sh P.== infoGuards PLD.!! ix)
                  P.&& (P.isJust infoSubTxIx P.== P.isNothing topTxInfo)
    |]

purposeIsWellformedWithDatumQ :: Q [Dec]
purposeIsWellformedWithDatumQ =
  [d|
    purposeIsWellformedWithDatum :: P.BuiltinData -> P.BuiltinUnit
    purposeIsWellformedWithDatum arg =
      P.check $
        case unsafeFromBuiltinData arg of
          PV4D.ScriptContext txInfo _redeemer (PV4D.SpendingScript txOutRef (Just _)) _sh ->
            not $ PLD.null $ PLD.filter ((txOutRef P.==) . PV4D.txInInfoOutRef) $ PV4D.txInfoInputs txInfo
          _ -> False
    |]

datumIsWellformedQ :: Q [Dec]
datumIsWellformedQ =
  [d|
    datumIsWellformed :: P.BuiltinData -> P.BuiltinUnit
    datumIsWellformed arg =
      P.check $
        case unsafeFromBuiltinData arg of
          PV4D.ScriptContext txInfo _redeemer (PV4D.SpendingScript _txOutRef (Just datum)) _sh ->
            not $ PLD.null $ PLD.filter (datum P.==) $ PAMD.elems $ PV4D.txInfoData txInfo
          _ -> False
    |]

inputsOutputsAreNotEmptyNoDatumQ :: Q [Dec]
inputsOutputsAreNotEmptyNoDatumQ =
  [d|
    inputsOutputsAreNotEmptyNoDatum :: P.BuiltinData -> P.BuiltinUnit
    inputsOutputsAreNotEmptyNoDatum arg =
      P.check $
        case unsafeFromBuiltinData arg of
          -- When there is a datum supplied, we need to fail.
          PV4D.ScriptContext _txInfo _redeemer (PV4D.SpendingScript _txOutRef (Just _)) _sh -> False
          PV4D.ScriptContext txInfo _redeemer _scriptPurpose _sh ->
            not $ PLD.null (PV4D.txInfoInputs txInfo) || PLD.null (PV4D.txInfoOutputs txInfo)
    |]

inputsOutputsAreNotEmptyWithDatumQ :: Q [Dec]
inputsOutputsAreNotEmptyWithDatumQ =
  [d|
    inputsOutputsAreNotEmptyWithDatum :: P.BuiltinData -> P.BuiltinUnit
    inputsOutputsAreNotEmptyWithDatum arg =
      P.check $
        case unsafeFromBuiltinData arg of
          PV4D.ScriptContext _txInfo _redeemer (PV4D.SpendingScript _txOutRef Nothing) _sh -> False
          PV4D.ScriptContext txInfo _redeemer _scriptPurpose _sh ->
            not $ PLD.null (PV4D.txInfoInputs txInfo) || PLD.null (PV4D.txInfoOutputs txInfo)
    |]

inputsOverlapsWithRefInputsQ :: Q [Dec]
inputsOverlapsWithRefInputsQ =
  [d|
    inputsOverlapsWithRefInputs :: P.BuiltinData -> P.BuiltinUnit
    inputsOverlapsWithRefInputs context =
      P.check $
        case unsafeFromBuiltinData context of
          PV4D.ScriptContext txInfo _redeemer _scriptPurpose _sh ->
            PLD.any (\x -> P.isJust . PLD.find (P.== x) $ PV4D.txInfoReferenceInputs txInfo) $
              PV4D.txInfoInputs txInfo
          _ -> False
    |]

-- | This ensures that a single TreasuryWithdrawal can't withdraw enough to
-- make the treasury have less ADA than the specified reserve amount.
ensureTreasuryReserveQ :: Q [Dec]
ensureTreasuryReserveQ =
  [d|
    ensureTreasuryReserve :: P.BuiltinData -> P.BuiltinUnit
    ensureTreasuryReserve context =
      P.check $
        case unsafeFromBuiltinData context of
          PV4D.ScriptContext
            txInfo
            _
            (PV4D.ProposingScript _ (PV4D.ProposalProcedure _ _ (PV4D.TreasuryWithdrawals withdrawals _)))
            _sh ->
              let
                totalWithdrawal = PAMD.foldr (P.+) 0 withdrawals
               in
                case PV4D.txInfoCurrentTreasuryAmount txInfo of
                  Just treasury -> treasury P.- totalWithdrawal P.>= 100_000_000
                  _ -> False
          _ -> False
    |]
