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
              PV4D.WithdrawingScript (PV4D.AccountId cred) -> PAMD.member cred infoWithdrawals
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

-- | Script performing tx-level validation for the UTxOs it locks, by acting as both their
-- spending script and their guard:
--
-- * As a spending script it only succeeds when its own hash is in `txInfoGuards`, i.e. it
--   delegates validation to its guard execution.
-- * As a guard it expects a `CurrencySymbol` redeemer and only succeeds when every input
--   locked by its own hash holds a token of that policy.
--
-- This could also be written as two separate scripts: a spending script that
-- requires a guard hash (e.g. supplied as a script parameter) to be in
-- `txInfoGuards`, and a guard script that checks the inputs locked by the
-- spending script hash (e.g. supplied as a script parameter). Doing so has the
-- following advantages:
--
-- * The tx-level logic can be upgraded without changing the address of the UTxOs: new UTxOs
--   reference the upgraded guard, while the spending script hash stays the same.
-- * Each script is smaller, so the spending script, which runs once per input, costs less to
--   deserialise and to include as a witness or reference script.
-- * Each script has a single purpose, which makes it simpler to audit.
inputSpendingGuardQ :: Q [Dec]
inputSpendingGuardQ =
  [d|
    inputSpendingGuard :: P.BuiltinData -> P.BuiltinUnit
    inputSpendingGuard arg =
      P.check $
        case unsafeFromBuiltinData arg of
          PV4D.ScriptContext txInfo (PV4D.Redeemer redeemer) scriptInfo ownHash ->
            case scriptInfo of
              PV4D.SpendingScript _txOutRef _mDatum ->
                PLD.elem (PV4D.ScriptCredential ownHash) (PV4D.txInfoGuards txInfo)
              PV4D.GuardingScript _ix _topTxInfo ->
                let protocolPolicy = unsafeFromBuiltinData redeemer :: PV4D.CurrencySymbol
                    holdsProtocolToken txInInfo =
                      let txOut = PV4D.txInInfoResolved txInInfo
                       in case PV4D.addressCredential (PV4D.txOutAddress txOut) of
                            PV4D.ScriptCredential sh ->
                              P.not (sh P.== ownHash)
                                P.|| (PV4D.currencySymbolValueOf (PV4D.txOutValue txOut) protocolPolicy P.> 0)
                            _ -> True
                 in PLD.all holdsProtocolToken (PV4D.txInfoInputs txInfo)
              _ -> False
    |]

partialWithdrawalVestingScriptQ :: Q [Dec]
partialWithdrawalVestingScriptQ =
  [d|
    partialWithdrawalVestingScript ::
      P.BuiltinData -> P.BuiltinData -> P.BuiltinData -> P.BuiltinData -> P.BuiltinData -> P.BuiltinUnit
    partialWithdrawalVestingScript beneficiaryData totalVestingAmountData startTimeData endTimeData scriptContextData =
      P.check $
        case unsafeFromBuiltinData scriptContextData of
          PV4D.ScriptContext txInfo (PV4D.Redeemer _redeemer) scriptInfo _ownHash ->
            let beneficiary = unsafeFromBuiltinData beneficiaryData :: PV4D.PubKeyHash
                -- Lovelace vested linearly between `startTime` and `endTime` (POSIX times in milliseconds)
                totalVestingAmount = unsafeFromBuiltinData totalVestingAmountData :: Integer
                startTime = unsafeFromBuiltinData startTimeData :: Integer
                endTime = unsafeFromBuiltinData endTimeData :: Integer
                -- Key hash guards are the required signers of the transaction
                isSignedByBeneficiary =
                  PLD.elem (PV4D.PubKeyCredential beneficiary) (PV4D.txInfoGuards txInfo)
                -- Lovelace still locked at `time`, rounded up so that rounding never releases
                -- funds early
                lockedVestingAmount time
                  | time P.<= startTime = totalVestingAmount
                  | time P.>= endTime = 0
                  | P.otherwise =
                      let duration = endTime P.- startTime
                       in (totalVestingAmount P.* (endTime P.- time) P.+ duration P.- 1) `P.divide` duration
                -- The ledger checks the account balance against this lower bound before the
                -- withdrawals of the transaction are applied
                accountBalanceLowerBound accountId =
                  case PV4D.txInfoAccountBalanceIntervals txInfo of
                    PV4D.AccountBalanceIntervals intervals ->
                      case PAMD.lookup accountId intervals of
                        Just (PV4D.AccountBalanceLowerBound (PV4D.Lovelace balanceLowerBound)) -> Just balanceLowerBound
                        Just (PV4D.AccountBalanceBothBounds (PV4D.Lovelace balanceLowerBound) _) -> Just balanceLowerBound
                        Just (PV4D.AccountBalanceExact (PV4D.Lovelace balanceLowerBound)) -> Just balanceLowerBound
                        _ -> Nothing
                withdrawalAmount cred =
                  case PAMD.lookup cred (PV4D.txInfoWithdrawals txInfo) of
                    Just (PV4D.Lovelace w) -> w
                    Nothing -> 0
                -- The ledger requires the current time to be at or after the lower bound of the
                -- validity range, so the beneficiary can only pick an earlier `time`, which locks more
                remainsLockedAtLeast accountId@(PV4D.AccountId cred) =
                  case (PV4D.fromInclusive (PV4D.txInfoValidRange txInfo), accountBalanceLowerBound accountId) of
                    (Just (PV4D.POSIXTime time), Just balanceLowerBound) -> balanceLowerBound P.>= lockedVestingAmount time P.+ withdrawalAmount cred
                    _ -> False
             in case scriptInfo of
                  PV4D.WithdrawingScript accountId -> isSignedByBeneficiary P.&& remainsLockedAtLeast accountId
                  PV4D.CertifyingScript _ix txCert ->
                    case txCert of
                      -- Anyone can register the account, since they pay the deposit
                      PV4D.TxCertRegAccount _ _ -> True
                      PV4D.TxCertDelegAccount _ _ -> isSignedByBeneficiary
                      PV4D.TxCertRegAccountDeleg {} -> isSignedByBeneficiary
                      _ -> False
                  _ -> False
    |]
