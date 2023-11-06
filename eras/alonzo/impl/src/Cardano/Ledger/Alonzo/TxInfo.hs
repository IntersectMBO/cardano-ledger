{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

module Cardano.Ledger.Alonzo.TxInfo
  {-# DEPRECATED "In favor of \"Cardano.Ledger.Alonzo.Plutus.TxInfo\", \"Cardano.Ledger.Plutus.TxInfo\" and \"Cardano.Ledger.Alonzo.Plutus.Evaluate\"" #-} (
  -- DEPRECATED
  runPLCScript,
  explainPlutusFailure,
  validPlutusdata,
  getTxOutDatum,
  transShelleyTxCert,
  transStakeCred,
  module Cardano.Ledger.Plutus.Evaluate,
  module Cardano.Ledger.Plutus.TxInfo,
  module Cardano.Ledger.Alonzo.Plutus.TxInfo,
)
where

import Cardano.Crypto.Hash.Class (hashToBytes)
import Cardano.Ledger.Alonzo.Core as Core hiding (TranslationError)
import Cardano.Ledger.Alonzo.Plutus.TxInfo
import Cardano.Ledger.BaseTypes
import Cardano.Ledger.Credential (Credential (KeyHashObj, ScriptHashObj))
import Cardano.Ledger.Keys
import Cardano.Ledger.Plutus.CostModels
import Cardano.Ledger.Plutus.Data
import Cardano.Ledger.Plutus.Evaluate
import Cardano.Ledger.Plutus.ExUnits
import Cardano.Ledger.Plutus.Language
import Cardano.Ledger.Plutus.TxInfo
import Cardano.Ledger.Shelley.TxCert
import qualified Data.ByteString as BS
import qualified Data.ByteString.Short as SBS (ShortByteString)
import Data.Proxy (Proxy)
import Lens.Micro
import qualified PlutusLedgerApi.V1 as PV1

transStakeCred :: Credential kr c -> PV1.Credential
transStakeCred (ScriptHashObj (ScriptHash sh)) =
  PV1.ScriptCredential (PV1.ScriptHash (PV1.toBuiltin (hashToBytes sh)))
transStakeCred (KeyHashObj (KeyHash kh)) =
  PV1.PubKeyCredential (PV1.PubKeyHash (PV1.toBuiltin (hashToBytes kh)))
{-# DEPRECATED transStakeCred "Infavor of identical `transCred`" #-}

transShelleyTxCert ::
  (ShelleyEraTxCert era, ProtVerAtMost era 8, TxCert era ~ ShelleyTxCert era) =>
  ShelleyTxCert era ->
  PV1.DCert
transShelleyTxCert = alonzoTransTxCert
{-# DEPRECATED transShelleyTxCert "In favor of `alonzoTransTxCert`" #-}

getTxOutDatum :: AlonzoEraTxOut era => TxOut era -> Datum era
getTxOutDatum txOut = txOut ^. datumTxOutF
{-# DEPRECATED getTxOutDatum "In favor of `datumTxOutF`" #-}

-- | Run a Plutus Script, given the script and the bounds on resources it is allocated.
runPLCScript ::
  forall era.
  Era era =>
  Proxy era ->
  ProtVer ->
  Language ->
  CostModel ->
  SBS.ShortByteString ->
  ExUnits ->
  [PV1.Data] ->
  ScriptResult
runPLCScript _ pv lang cm scriptBytes units ds =
  runPlutusScript pv $
    PlutusWithContext
      { pwcScript = Plutus lang (PlutusBinary scriptBytes)
      , pwcDatums = map (Data @era) ds
      , pwcExUnits = units
      , pwcCostModel = cm
      }
{-# DEPRECATED runPLCScript "In favor of `runPlutusScript`" #-}

explainPlutusFailure ::
  forall era.
  Era era =>
  Proxy era ->
  ProtVer ->
  Language ->
  SBS.ShortByteString ->
  PV1.EvaluationError ->
  [PV1.Data] ->
  CostModel ->
  ExUnits ->
  ScriptResult
explainPlutusFailure _proxy pv lang scriptbytestring e ds cm eu =
  explainPlutusEvaluationError pv pwc e
  where
    pwc =
      PlutusWithContext
        { pwcScript = Plutus lang (PlutusBinary scriptbytestring)
        , pwcDatums = map (Data @era) ds
        , pwcExUnits = eu
        , pwcCostModel = cm
        }
{-# DEPRECATED explainPlutusFailure "In favor of `explainPlutusEvaluationError`" #-}

{-# DEPRECATED validPlutusdata "Plutus data bytestrings are not restricted to sixty-four bytes." #-}
validPlutusdata :: PV1.Data -> Bool
validPlutusdata (PV1.Constr _n ds) = all validPlutusdata ds
validPlutusdata (PV1.Map ds) =
  all (\(x, y) -> validPlutusdata x && validPlutusdata y) ds
validPlutusdata (PV1.List ds) = all validPlutusdata ds
validPlutusdata (PV1.I _n) = True
validPlutusdata (PV1.B bs) = BS.length bs <= 64
