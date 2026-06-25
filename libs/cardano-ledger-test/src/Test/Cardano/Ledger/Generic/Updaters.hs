{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Test.Cardano.Ledger.Generic.Updaters where

import Cardano.Crypto.DSIGN.Class ()
import Cardano.Ledger.Alonzo.Core (AlonzoEraPParams, AlonzoEraScript, PParams)
import qualified Cardano.Ledger.Alonzo.Core as Alonzo
import Cardano.Ledger.Alonzo.Tx (ScriptIntegrity (..), hashScriptIntegrity)
import qualified Cardano.Ledger.Alonzo.Tx as Alonzo
import Cardano.Ledger.Alonzo.TxWits (Redeemers (..), TxDats (..))
import Cardano.Ledger.BaseTypes (StrictMaybe (..))
import Cardano.Ledger.Plutus.Language (Language (..))
import qualified Data.Set as Set
import Test.Cardano.Ledger.Generic.Proof

alonzoNewScriptIntegrityHash ::
  forall era.
  ( AlonzoEraScript era
  , AlonzoEraPParams era
  ) =>
  PParams era ->
  [Language] ->
  Redeemers era ->
  TxDats era ->
  StrictMaybe Alonzo.ScriptIntegrityHash
alonzoNewScriptIntegrityHash pp ls redeemers@(Redeemers r) dats@(TxDats d)
  | null d
  , null r
  , null ls =
      SNothing
  | otherwise =
      SJust . hashScriptIntegrity @era $
        ScriptIntegrity redeemers dats (Set.map (Alonzo.getLanguageView pp) $ Set.fromList ls)

languages :: Proof era -> [Language]
languages Shelley = []
languages Allegra = []
languages Mary = []
languages Alonzo = [PlutusV1]
languages Babbage = [PlutusV1, PlutusV2]
languages Conway = [PlutusV1, PlutusV2]
