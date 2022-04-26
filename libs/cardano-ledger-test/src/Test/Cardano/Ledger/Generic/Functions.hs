{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

-- | Functions in this module take a (Proof era) as their first
--   parameter and do something potentially different in each Era.
module Test.Cardano.Ledger.Generic.Functions where

import Cardano.Ledger.Address (Addr (..))
import Cardano.Ledger.Alonzo.Data (DataHash, binaryDataToData, hashData)
import Cardano.Ledger.Alonzo.Language (Language (..))
import Cardano.Ledger.Alonzo.PParams (PParams, PParams' (..))
import Cardano.Ledger.Alonzo.PlutusScriptApi (scriptsNeededFromBody)
import Cardano.Ledger.Alonzo.Scripts (ExUnits (..), Tag (..))
import Cardano.Ledger.Alonzo.Tx
  ( IsValid (..),
    ScriptIntegrityHash,
    ValidatedTx (..),
    hashScriptIntegrity,
    minfee,
  )
import Cardano.Ledger.Alonzo.TxBody (TxOut (..))
import Cardano.Ledger.Alonzo.TxInfo (languages)
import Cardano.Ledger.Alonzo.TxWitness (Redeemers (..), TxDats (..))
import qualified Cardano.Ledger.Babbage.PParams as Babbage (PParams, PParams' (..))
import Cardano.Ledger.Babbage.Scripts (refScripts)
import Cardano.Ledger.Babbage.TxBody as Babbage (referenceInputs', spendInputs')
import qualified Cardano.Ledger.Babbage.TxBody as Babbage (Datum (..), TxOut (..))
import Cardano.Ledger.Coin (Coin (..))
import qualified Cardano.Ledger.Core as Core
import Cardano.Ledger.Credential (Credential, StakeReference (..))
import Cardano.Ledger.Era (Era (Crypto))
import Cardano.Ledger.Keys (KeyHash (..), KeyRole (..))
import Cardano.Ledger.Shelley.EpochBoundary (obligation)
import qualified Cardano.Ledger.Shelley.LedgerState as Shelley (minfee)
import qualified Cardano.Ledger.Shelley.PParams as Shelley (PParams, PParams' (..))
import Cardano.Ledger.Shelley.Scripts (ScriptHash (..))
import Cardano.Ledger.Shelley.TxBody (DCert (..), DelegCert (..), PoolCert (..), PoolParams (..))
import qualified Cardano.Ledger.Shelley.TxBody as Shelley (TxOut (..))
import Cardano.Ledger.Shelley.UTxO (UTxO (..), balance, scriptsNeeded, totalDeposits)
import Cardano.Ledger.TxIn (TxIn (..))
import Cardano.Ledger.Val (Val (coin, inject, (<+>), (<->)))
import Control.State.Transition.Extended (STS (State))
import Data.Default.Class (Default (def))
import Data.Foldable (toList)
import qualified Data.List as List
import Data.Map (Map, keysSet, restrictKeys)
import Data.Maybe.Strict (StrictMaybe (..))
import Data.Set (Set)
import qualified Data.Set as Set
import GHC.Records (HasField (getField))
import Numeric.Natural
import Test.Cardano.Ledger.Alonzo.Scripts (alwaysFails, alwaysSucceeds)
import Test.Cardano.Ledger.Generic.Fields (TxField (..), TxOutField (..), initialTx)
import Test.Cardano.Ledger.Generic.ModelState (MUtxo, fromMUtxo)
import Test.Cardano.Ledger.Generic.Proof
import Test.Cardano.Ledger.Generic.Scriptic (Scriptic (..))
import Test.Cardano.Ledger.Generic.Updaters (updateTx)

-- ====================================================================
-- Era agnostic actions on (Core.PParams era) (Core.TxOut era) and
-- other Core.XX types Mostly by pattern matching against Proof objects

maxCollateralInputs' :: Proof era -> Core.PParams era -> Natural
maxCollateralInputs' (Alonzo _) x = _maxCollateralInputs x
maxCollateralInputs' (Babbage _) x = Babbage._maxCollateralInputs x
maxCollateralInputs' _proof _x = 0

maxTxExUnits' :: Proof era -> Core.PParams era -> ExUnits
maxTxExUnits' (Alonzo _) x = _maxTxExUnits x
maxTxExUnits' (Babbage _) x = Babbage._maxTxExUnits x
maxTxExUnits' _proof _x = mempty

collateralPercentage' :: Proof era -> Core.PParams era -> Natural
collateralPercentage' (Alonzo _) x = _collateralPercentage x
collateralPercentage' (Babbage _) x = Babbage._collateralPercentage x
collateralPercentage' _proof _x = 0

obligation' ::
  forall era c.
  (c ~ Crypto era) =>
  Proof era ->
  Core.PParams era ->
  Map (Credential 'Staking c) Coin ->
  Map (KeyHash 'StakePool c) (PoolParams c) ->
  Coin
obligation' (Babbage _) = obligation @c @(Babbage.PParams era) @Map
obligation' (Alonzo _) = obligation @c @(PParams era) @Map
obligation' (Mary _) = obligation @c @(Shelley.PParams era) @Map
obligation' (Allegra _) = obligation @c @(Shelley.PParams era) @Map
obligation' (Shelley _) = obligation @c @(Shelley.PParams era) @Map

totalDeposits' ::
  forall era.
  Proof era ->
  Core.PParams era ->
  (KeyHash 'StakePool (Crypto era) -> Bool) ->
  [DCert (Crypto era)] ->
  Coin
totalDeposits' (Babbage _) pp = totalDeposits pp
totalDeposits' (Alonzo _) pp = totalDeposits pp
totalDeposits' (Mary _) pp = totalDeposits pp
totalDeposits' (Allegra _) pp = totalDeposits pp
totalDeposits' (Shelley _) pp = totalDeposits pp

-- | Positive numbers are "deposits owed", negative amounts are "refunds gained"
depositsAndRefunds :: Proof era -> Core.PParams era -> [DCert (Crypto era)] -> Coin
depositsAndRefunds proof pp certificates = List.foldl' accum (Coin 0) certificates
  where
    accum ans (DCertDeleg (RegKey _)) = keydep <+> ans
    accum ans (DCertDeleg (DeRegKey _)) = ans <-> keydep
    accum ans (DCertPool (RegPool _)) = pooldep <+> ans
    accum ans (DCertPool (RetirePool _ _)) = ans <-> pooldep
    accum ans _ = ans
    (keydep, pooldep :: Coin) = keyPoolDeposits proof pp

keyPoolDeposits :: Proof era -> Core.PParams era -> (Coin, Coin)
keyPoolDeposits proof pp = case proof of
  Babbage _ -> (getField @"_keyDeposit" pp, getField @"_poolDeposit" pp)
  Alonzo _ -> (getField @"_keyDeposit" pp, getField @"_poolDeposit" pp)
  Mary _ -> (getField @"_keyDeposit" pp, getField @"_poolDeposit" pp)
  Allegra _ -> (getField @"_keyDeposit" pp, getField @"_poolDeposit" pp)
  Shelley _ -> (getField @"_keyDeposit" pp, getField @"_poolDeposit" pp)

-- | Compute the set of ScriptHashes for which there should be ScriptWitnesses. In Babbage
--  Era and later, where inline Scripts are allowed, they should not appear in this set.
scriptsNeeded' :: Proof era -> MUtxo era -> Core.TxBody era -> Set (ScriptHash (Crypto era))
scriptsNeeded' (Babbage _) utxo txbody = regularScripts `Set.difference` inlineScripts
  where
    theUtxo = fromMUtxo utxo
    inputs = spendInputs' txbody `Set.union` referenceInputs' txbody
    inlineScripts = keysSet $ refScripts inputs theUtxo
    regularScripts = Set.fromList (map snd (scriptsNeededFromBody theUtxo txbody))
scriptsNeeded' (Alonzo _) utxo txbody = Set.fromList (map snd (scriptsNeededFromBody (fromMUtxo utxo) txbody))
scriptsNeeded' p@(Mary _) utxo txbody = scriptsNeeded (fromMUtxo utxo) (updateTx p (initialTx p) (Body txbody))
scriptsNeeded' p@(Allegra _) utxo txbody = scriptsNeeded (fromMUtxo utxo) (updateTx p (initialTx p) (Body txbody))
scriptsNeeded' p@(Shelley _) utxo txbody = scriptsNeeded (fromMUtxo utxo) (updateTx p (initialTx p) (Body txbody))

minfee' :: forall era. Proof era -> Core.PParams era -> Core.Tx era -> Coin
minfee' (Alonzo _) = minfee
minfee' (Babbage _) = minfee
minfee' (Mary _) = Shelley.minfee
minfee' (Allegra _) = Shelley.minfee
minfee' (Shelley _) = Shelley.minfee

txInBalance ::
  forall era.
  Era era =>
  Set (TxIn (Crypto era)) ->
  MUtxo era ->
  Coin
txInBalance txinSet m = coin (balance (fromMUtxo (restrictKeys m txinSet)))

hashScriptIntegrity' ::
  Proof era ->
  Core.PParams era ->
  Set Language ->
  Redeemers era ->
  TxDats era ->
  StrictMaybe (ScriptIntegrityHash (Crypto era))
hashScriptIntegrity' (Babbage _) = hashScriptIntegrity
hashScriptIntegrity' (Alonzo _) = hashScriptIntegrity
hashScriptIntegrity' _proof = (\_pp _l _r _d -> SNothing)

-- | Break a TxOut into its mandatory and optional parts
txoutFields :: Proof era -> Core.TxOut era -> (Addr (Crypto era), Core.Value era, [TxOutField era])
txoutFields (Alonzo _) (TxOut addr val dh) = (addr, val, [DHash dh])
txoutFields (Babbage _) (Babbage.TxOut addr val d h) = (addr, val, [Datum d, RefScript h])
txoutFields (Mary _) (Shelley.TxOut addr val) = (addr, val, [])
txoutFields (Allegra _) (Shelley.TxOut addr val) = (addr, val, [])
txoutFields (Shelley _) (Shelley.TxOut addr val) = (addr, val, [])

injectFee :: Proof era -> Coin -> Core.TxOut era -> Core.TxOut era
injectFee (Babbage _) fee (Babbage.TxOut addr val d ref) = Babbage.TxOut addr (val <+> inject fee) d ref
injectFee (Alonzo _) fee (TxOut addr val mdh) = TxOut addr (val <+> inject fee) mdh
injectFee (Mary _) fee (Shelley.TxOut addr val) = Shelley.TxOut addr (val <+> inject fee)
injectFee (Allegra _) fee (Shelley.TxOut addr val) = Shelley.TxOut addr (val <+> inject fee)
injectFee (Shelley _) fee (Shelley.TxOut addr val) = Shelley.TxOut addr (val <+> inject fee)

getTxOutVal :: Proof era -> Core.TxOut era -> Core.Value era
getTxOutVal (Babbage _) (Babbage.TxOut _ v _ _) = v
getTxOutVal (Alonzo _) (TxOut _ v _) = v
getTxOutVal (Mary _) (Shelley.TxOut _ v) = v
getTxOutVal (Allegra _) (Shelley.TxOut _ v) = v
getTxOutVal (Shelley _) (Shelley.TxOut _ v) = v

getTxOutCoin :: Proof era -> Core.TxOut era -> Coin
getTxOutCoin (Babbage _) (Babbage.TxOut _ v _ _) = coin v
getTxOutCoin (Alonzo _) (TxOut _ v _) = coin v
getTxOutCoin (Mary _) (Shelley.TxOut _ v) = coin v
getTxOutCoin (Allegra _) (Shelley.TxOut _ v) = coin v
getTxOutCoin (Shelley _) (Shelley.TxOut _ v) = coin v

getTxOutRefScript :: Proof era -> Core.TxOut era -> StrictMaybe (Core.Script era)
getTxOutRefScript (Babbage _) (Babbage.TxOut _ _ _ ms) = ms
getTxOutRefScript _ _ = SNothing

emptyPPUPstate :: forall era. Proof era -> State (Core.EraRule "PPUP" era)
emptyPPUPstate (Babbage _) = def
emptyPPUPstate (Alonzo _) = def
emptyPPUPstate (Mary _) = def
emptyPPUPstate (Allegra _) = def
emptyPPUPstate (Shelley _) = def

maxRefInputs :: Proof era -> Int
maxRefInputs (Babbage _) = 3
maxRefInputs _ = 0

isValid' :: Proof era -> Core.Tx era -> IsValid
isValid' (Alonzo _) x = isValid x
isValid' (Babbage _) x = isValid x
isValid' _ _ = IsValid True

-- | Does the TxOut have evidence of credentials and data.
--   Evidence of data is either ScriptHash or (in Babbage) an inline Datum
--   Evidence of credentials can come from the Addr
txoutEvidence ::
  forall era.
  Proof era ->
  Core.TxOut era ->
  ([Credential 'Payment (Crypto era)], Maybe (DataHash (Crypto era)))
txoutEvidence (Alonzo _) (TxOut addr _ (SJust dh)) =
  (addrCredentials addr, Just dh)
txoutEvidence (Alonzo _) (TxOut addr _ SNothing) =
  (addrCredentials addr, Nothing)
txoutEvidence (Babbage _) (Babbage.TxOut addr _ Babbage.NoDatum _) =
  (addrCredentials addr, Nothing)
txoutEvidence (Babbage _) (Babbage.TxOut addr _ (Babbage.DatumHash dh) _) =
  (addrCredentials addr, Just dh)
txoutEvidence (Babbage _) (Babbage.TxOut addr _ (Babbage.Datum _d) _) =
  (addrCredentials addr, Just (hashData @era (binaryDataToData _d)))
txoutEvidence (Mary _) (Shelley.TxOut addr _) =
  (addrCredentials addr, Nothing)
txoutEvidence (Allegra _) (Shelley.TxOut addr _) =
  (addrCredentials addr, Nothing)
txoutEvidence (Shelley _) (Shelley.TxOut addr _) =
  (addrCredentials addr, Nothing)

addrCredentials :: Addr crypto -> [Credential 'Payment crypto]
addrCredentials addr = maybe [] (: []) (paymentCredAddr addr)

paymentCredAddr :: Addr c -> Maybe (Credential 'Payment c)
paymentCredAddr (Addr _ cred _) = Just cred
paymentCredAddr _ = Nothing

stakeCredAddr :: Addr c -> Maybe (Credential 'Staking c)
stakeCredAddr (Addr _ _ (StakeRefBase cred)) = Just cred
stakeCredAddr _ = Nothing

getBody :: Proof era -> Core.Tx era -> Core.TxBody era
getBody (Babbage _) tx = getField @"body" tx
getBody (Alonzo _) tx = getField @"body" tx
getBody (Mary _) tx = getField @"body" tx
getBody (Allegra _) tx = getField @"body" tx
getBody (Shelley _) tx = getField @"body" tx

getInputs :: Proof era -> Core.TxBody era -> Set (TxIn (Crypto era))
getInputs (Babbage _) tx = getField @"inputs" tx
getInputs (Alonzo _) tx = getField @"inputs" tx
getInputs (Mary _) tx = getField @"inputs" tx
getInputs (Allegra _) tx = getField @"inputs" tx
getInputs (Shelley _) tx = getField @"inputs" tx

primaryLanguage :: Proof era -> Maybe Language
primaryLanguage (Babbage _) = Just (PlutusV2)
primaryLanguage (Alonzo _) = Just (PlutusV1)
primaryLanguage _ = Nothing

alwaysTrue :: forall era. Proof era -> Maybe Language -> Natural -> Core.Script era
alwaysTrue (Babbage _) (Just l) n = alwaysSucceeds @era l n
alwaysTrue p@(Babbage _) Nothing _ = allOf [] p
alwaysTrue (Alonzo _) (Just l) n = alwaysSucceeds @era l n
alwaysTrue p@(Alonzo _) Nothing _ = allOf [] p
alwaysTrue p@(Mary _) _ n = always n p
alwaysTrue p@(Allegra _) _ n = always n p
alwaysTrue p@(Shelley _) _ n = always n p

alwaysFalse :: forall era. Proof era -> Maybe Language -> Natural -> Core.Script era
alwaysFalse (Babbage _) (Just l) n = alwaysFails @era l n
alwaysFalse p@(Babbage _) Nothing _ = anyOf [] p
alwaysFalse (Alonzo _) (Just l) n = alwaysFails @era l n
alwaysFalse p@(Alonzo _) Nothing _ = anyOf [] p
alwaysFalse p@(Mary _) _ n = never n p
alwaysFalse p@(Allegra _) _ n = never n p
alwaysFalse p@(Shelley _) _ n = never n p

certs :: Proof era -> Core.Tx era -> [DCert (Crypto era)]
certs (Babbage _) tx = toList $ getField @"certs" (getField @"body" tx)
certs (Alonzo _) tx = toList $ getField @"certs" (getField @"body" tx)
certs (Mary _) tx = toList $ getField @"certs" (getField @"body" tx)
certs (Allegra _) tx = toList $ getField @"certs" (getField @"body" tx)
certs (Shelley _) tx = toList $ getField @"certs" (getField @"body" tx)

languagesUsed ::
  forall era.
  Era era =>
  Proof era ->
  Core.Tx era ->
  UTxO era ->
  Map (ScriptHash (Crypto era), Tag) (IsValid, Core.Script era) ->
  Set Language
languagesUsed proof tx utxo _plutusScripts = case proof of
  (Shelley _) -> Set.empty
  (Allegra _) -> Set.empty
  (Mary _) -> Set.empty
  (Alonzo _) -> Cardano.Ledger.Alonzo.TxInfo.languages tx utxo
  (Babbage _) -> Cardano.Ledger.Alonzo.TxInfo.languages tx utxo
