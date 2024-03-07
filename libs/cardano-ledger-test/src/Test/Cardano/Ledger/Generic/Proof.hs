{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}
-- Used for Reflect classes
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE UndecidableSuperClasses #-}

module Test.Cardano.Ledger.Generic.Proof (
  Mock,
  Standard,
  GoodCrypto,
  Proof (..),
  Reflect (..),
  Some (..),
  WitRule (..),
  ruleProof,
  runSTS,
  goSTS,
  preShelley,
  preAllegra,
  preMary,
  preAlonzo,
  preBabbage,
  preConway,
  postShelley,
  postAllegra,
  postMary,
  postAlonzo,
  postBabbage,
  postConway,
  ShelleyEra,
  AllegraEra,
  MaryEra,
  AlonzoEra,
  BabbageEra,
  ConwayEra,
  StandardCrypto,
  C_Crypto,
  specialize,
  unReflect,
  runSTS',
  ValueWit (..),
  TxOutWit (..),
  TxCertWit (..),
  PParamsWit (..),
  UTxOWit (..),
  ScriptWit (..),
  GovStateWit (..),
  whichValue,
  whichTxOut,
  whichTxCert,
  whichPParams,
  whichUTxO,
  whichScript,
  whichGovState,
) where

import Cardano.Ledger.Allegra (AllegraEra)
import Cardano.Ledger.Allegra.Scripts (Timelock)
import Cardano.Ledger.Alonzo (AlonzoEra)
import Cardano.Ledger.Alonzo.Core (AlonzoEraPParams, AlonzoEraTxBody)
import Cardano.Ledger.Alonzo.PParams (AlonzoPParams (..))
import Cardano.Ledger.Alonzo.Scripts (AlonzoEraScript, AlonzoScript (..))
import Cardano.Ledger.Alonzo.TxOut (AlonzoEraTxOut (..), AlonzoTxOut (..))
import Cardano.Ledger.Alonzo.TxWits (AlonzoTxWits (..))
import Cardano.Ledger.Alonzo.UTxO (AlonzoScriptsNeeded)
import Cardano.Ledger.Babbage (BabbageEra)
import Cardano.Ledger.Babbage.Core (BabbageEraPParams)
import Cardano.Ledger.Babbage.PParams (BabbagePParams (..))
import Cardano.Ledger.Babbage.TxOut (BabbageEraTxOut (..), BabbageTxOut (..))
import Cardano.Ledger.BaseTypes (ShelleyBase)
import Cardano.Ledger.Coin (Coin (..))
import Cardano.Ledger.Conway (ConwayEra)
import Cardano.Ledger.Conway.Governance (ConwayGovState (..), RunConwayRatify (..))
import Cardano.Ledger.Conway.PParams (ConwayEraPParams (..), ConwayPParams (..))
import Cardano.Ledger.Conway.TxCert (ConwayEraTxCert, ConwayTxCert (..))
import Cardano.Ledger.Core (
  Era (EraCrypto),
  EraPParams,
  EraRule,
  EraScript,
  EraTx,
  EraTxAuxData,
  EraTxOut,
  PParamsHKD,
  ProtVerAtMost,
  Script,
  TxCert,
  TxOut,
  TxWits,
  Value,
 )
import Cardano.Ledger.Crypto (StandardCrypto)
import Cardano.Ledger.Mary (MaryEra)
import Cardano.Ledger.Mary.Value (MaryValue)
import Cardano.Ledger.Shelley (ShelleyEra)
import Cardano.Ledger.Shelley.Core (EraGov, ShelleyEraTxCert)
import Cardano.Ledger.Shelley.Governance (EraGov (..), ShelleyGovState (..))
import Cardano.Ledger.Shelley.PParams (ShelleyPParams (..))
import Cardano.Ledger.Shelley.Scripts (MultiSig)
import Cardano.Ledger.Shelley.TxCert (ShelleyTxCert)
import Cardano.Ledger.Shelley.TxOut (ShelleyTxOut (..))
import Cardano.Ledger.Shelley.TxWits (ShelleyTxWits (..))
import Cardano.Ledger.Shelley.UTxO (ShelleyScriptsNeeded)
import Cardano.Ledger.UTxO (EraUTxO (..), ScriptsNeeded)
import Control.State.Transition.Extended hiding (Assertion)
import Data.Functor.Identity (Identity)
import Data.Kind (Type)
import Data.List.NonEmpty (NonEmpty)
import Data.Universe (Shape (..), Shaped (..), Singleton (..), Some (..), (:~:) (Refl))
import GHC.TypeLits (Symbol)
import Test.Cardano.Ledger.Shelley.ConcreteCryptoTypes (C_Crypto)
import Test.Cardano.Ledger.Shelley.Utils (applySTSTest, runShelleyBase)

import Cardano.Crypto.DSIGN.Class (Signable)
import Cardano.Crypto.Hash.Class (Hash)
import Cardano.Ledger.Core (EraIndependentTxBody)
import Cardano.Ledger.Crypto (Crypto, DSIGN, HASH)

-- =================================================
-- GADTs for witnessing Crypto and Era

type Mock = C_Crypto

type Standard = StandardCrypto

type GoodCrypto c = (Crypto c, Signable (DSIGN c) (Hash (HASH c) EraIndependentTxBody))

-- ===================================================

-- | Proof of a valid (predefined) era
data Proof era where
  Shelley :: Proof (ShelleyEra StandardCrypto)
  Mary :: Proof (MaryEra StandardCrypto)
  Allegra :: Proof (AllegraEra StandardCrypto)
  Alonzo :: Proof (AlonzoEra StandardCrypto)
  Babbage :: Proof (BabbageEra StandardCrypto)
  Conway :: Proof (ConwayEra StandardCrypto)

instance Show (Proof e) where
  show Shelley = "Shelley"
  show Allegra = "Allegra"
  show Mary = "Mary"
  show Alonzo = "Alonzo"
  show Babbage = "Babbage"
  show Conway = "Conway"

-- ==================================
-- Reflection over Crypto and Era

class
  ( EraGov era
  , EraTx era
  , EraUTxO era
  , EraTxAuxData era
  , ShelleyEraTxCert era
  , --  , GoodCrypto (EraCrypto era)
    EraCrypto era ~ StandardCrypto
  ) =>
  Reflect era
  where
  reify :: Proof era
  lift :: forall a. (Proof era -> a) -> a
  lift f = f (reify @era)

instance Reflect (ConwayEra StandardCrypto) where
  reify = Conway

instance Reflect (BabbageEra StandardCrypto) where
  reify = Babbage

instance Reflect (AlonzoEra StandardCrypto) where
  reify = Alonzo

instance Reflect (MaryEra StandardCrypto) where
  reify = Mary

instance Reflect (AllegraEra StandardCrypto) where
  reify = Allegra

instance Reflect (ShelleyEra StandardCrypto) where
  reify = Shelley

-- ===================================================
-- Tools for building TestTrees for multiple Eras

instance Show (Some Proof) where
  show (Some Shelley) = show Shelley
  show (Some Allegra) = show Allegra
  show (Some Mary) = show Mary
  show (Some Alonzo) = show Alonzo
  show (Some Babbage) = show Babbage
  show (Some Conway) = show Conway

-- ===============================================================
-- Proofs or witnesses to EraRule Tags

data WitRule (s :: Symbol) (e :: Type) where
  UTXO :: Proof era -> WitRule "UTXO" era
  UTXOW :: Proof era -> WitRule "UTXOW" era
  LEDGER :: Proof era -> WitRule "LEDGER" era
  BBODY :: Proof era -> WitRule "BBODY" era
  LEDGERS :: Proof era -> WitRule "LEDGERS" era
  MOCKCHAIN :: Proof era -> WitRule "MOCKCHAIN" era
  RATIFY :: Proof era -> WitRule "RATIFY" era
  ENACT :: Proof era -> WitRule "ENACT" era
  TALLY :: Proof era -> WitRule "TALLY" era
  EPOCH :: Proof era -> WitRule "EPOCH" era

ruleProof :: WitRule s e -> Proof e
ruleProof (UTXO p) = p
ruleProof (UTXOW p) = p
ruleProof (LEDGER p) = p
ruleProof (BBODY p) = p
ruleProof (LEDGERS p) = p
ruleProof (MOCKCHAIN p) = p
ruleProof (RATIFY p) = p
ruleProof (ENACT p) = p
ruleProof (TALLY p) = p
ruleProof (EPOCH p) = p

runSTS ::
  forall s e ans.
  ( BaseM (EraRule s e) ~ ShelleyBase
  , STS (EraRule s e)
  ) =>
  WitRule s e ->
  TRC (EraRule s e) ->
  (Either (NonEmpty (PredicateFailure (EraRule s e))) (State (EraRule s e)) -> ans) ->
  ans
runSTS (UTXO _proof) x cont = cont (runShelleyBase (applySTSTest x))
runSTS (UTXOW _proof) x cont = cont (runShelleyBase (applySTSTest x))
runSTS (LEDGER _proof) x cont = cont (runShelleyBase (applySTSTest x))
runSTS (BBODY _proof) x cont = cont (runShelleyBase (applySTSTest x))
runSTS (LEDGERS _proof) x cont = cont (runShelleyBase (applySTSTest x))
runSTS (MOCKCHAIN _proof) x cont = cont (runShelleyBase (applySTSTest x))
runSTS (RATIFY _proof) x cont = cont (runShelleyBase (applySTSTest x))
runSTS (ENACT _proof) x cont = cont (runShelleyBase (applySTSTest x))
runSTS (TALLY _proof) x cont = cont (runShelleyBase (applySTSTest x))
runSTS (_proof) x cont = cont (runShelleyBase (applySTSTest x))

runSTS' ::
  forall s e.
  ( BaseM (EraRule s e) ~ ShelleyBase
  , STS (EraRule s e)
  ) =>
  WitRule s e ->
  TRC (EraRule s e) ->
  Either (NonEmpty (PredicateFailure (EraRule s e))) (State (EraRule s e))
runSTS' (UTXO _proof) x = runShelleyBase (applySTSTest x)
runSTS' (UTXOW _proof) x = runShelleyBase (applySTSTest x)
runSTS' (LEDGER _proof) x = runShelleyBase (applySTSTest x)
runSTS' (BBODY _proof) x = runShelleyBase (applySTSTest x)
runSTS' (LEDGERS _proof) x = runShelleyBase (applySTSTest x)
runSTS' (MOCKCHAIN _proof) x = runShelleyBase (applySTSTest x)
runSTS' (RATIFY _proof) x = runShelleyBase (applySTSTest x)
runSTS' (ENACT _proof) x = runShelleyBase (applySTSTest x)
runSTS' (TALLY _proof) x = runShelleyBase (applySTSTest x)
runSTS' (EPOCH _proof) x = runShelleyBase (applySTSTest x)

-- | Like runSTS, but makes the components of the TRC triple explicit.
--   in case you can't remember, in ghci type
-- @@@
--   :t goSTS (UTXOW Babbage)
--   goSTS (LEDGER Babbage)
--     :: LedgerEnv (BabbageEra StandardCrypto)
--        -> (UTxOState (BabbageEra StandardCrypto), CertState StandardCrypto)
--        -> Cardano.Ledger.Alonzo.Tx.AlonzoTx (BabbageEra StandardCrypto)
--        -> (Either
--              [LedgerPredicateFailure (BabbageEra StandardCrypto)]
--              (UTxOState (BabbageEra StandardCrypto), CertState StandardCrypto)
--        -> ans)
--        -> ans
-- @@@
--   it will tell you what type 'env' 'state' and 'sig' are for Babbage
goSTS ::
  forall s e ans env state sig.
  ( BaseM (EraRule s e) ~ ShelleyBase
  , STS (EraRule s e)
  , env ~ Environment (EraRule s e)
  , state ~ State (EraRule s e)
  , sig ~ Signal (EraRule s e)
  ) =>
  WitRule s e ->
  env ->
  state ->
  sig ->
  (Either (NonEmpty (PredicateFailure (EraRule s e))) (State (EraRule s e)) -> ans) ->
  ans
goSTS (UTXO _proof) env state sig cont =
  cont (runShelleyBase (applySTSTest (TRC @(EraRule s e) (env, state, sig))))
goSTS (UTXOW _proof) env state sig cont =
  cont (runShelleyBase (applySTSTest (TRC @(EraRule s e) (env, state, sig))))
goSTS (LEDGER _proof) env state sig cont =
  cont (runShelleyBase (applySTSTest (TRC @(EraRule s e) (env, state, sig))))
goSTS (BBODY _proof) env state sig cont =
  cont (runShelleyBase (applySTSTest (TRC @(EraRule s e) (env, state, sig))))
goSTS (LEDGERS _proof) env state sig cont =
  cont (runShelleyBase (applySTSTest (TRC @(EraRule s e) (env, state, sig))))
goSTS (MOCKCHAIN _proof) env state sig cont =
  cont (runShelleyBase (applySTSTest (TRC @(EraRule s e) (env, state, sig))))
goSTS (RATIFY _proof) env state sig cont =
  cont (runShelleyBase (applySTSTest (TRC @(EraRule s e) (env, state, sig))))
goSTS (ENACT _proof) env state sig cont =
  cont (runShelleyBase (applySTSTest (TRC @(EraRule s e) (env, state, sig))))
goSTS (TALLY _proof) env state sig cont =
  cont (runShelleyBase (applySTSTest (TRC @(EraRule s e) (env, state, sig))))
goSTS (EPOCH _proof) env state sig cont =
  cont (runShelleyBase (applySTSTest (TRC @(EraRule s e) (env, state, sig))))

-- ================================================================
-- Crypto agnostic operations on (Proof era) via (Some Proof)

preShelley, preAllegra, preMary, preAlonzo, preBabbage, preConway :: [Some Proof]
preShelley = [Some Shelley]
preAllegra = [Some Allegra, Some Shelley]
preMary = [Some Mary, Some Allegra, Some Shelley]
preAlonzo = [Some Alonzo, Some Mary, Some Allegra, Some Shelley]
preBabbage = [Some Babbage, Some Alonzo, Some Mary, Some Allegra, Some Shelley]
preConway = [Some Conway, Some Babbage, Some Alonzo, Some Mary, Some Allegra, Some Shelley]

postShelley, postAllegra, postMary, postAlonzo, postBabbage, postConway :: [Some Proof]
postShelley =
  [ Some Conway
  , Some Babbage
  , Some Alonzo
  , Some Mary
  , Some Allegra
  , Some Shelley
  ]
postAllegra = [Some Conway, Some Babbage, Some Alonzo, Some Mary, Some Allegra]
postMary = [Some Conway, Some Babbage, Some Alonzo, Some Mary]
postAlonzo = [Some Conway, Some Babbage, Some Alonzo]
postBabbage = [Some Conway, Some Babbage]
postConway = [Some Conway]

-- ============================================

-- | Specialize ('action' :: (constraint era => t)) to all known 'era', because
-- we know (constraint era) holds for all known era. In order for this to work
-- it is best to type apply 'specialize' to a concrete constraint. So a call site
-- looks like '(specialize @EraSegWits proof action). This way the constraint does
-- not percolate upwards, past the call site of 'action'
specialize ::
  forall constraint era t.
  ( constraint (ShelleyEra (EraCrypto era))
  , constraint (AllegraEra (EraCrypto era))
  , constraint (MaryEra (EraCrypto era))
  , constraint (AlonzoEra (EraCrypto era))
  , constraint (BabbageEra (EraCrypto era))
  , constraint (ConwayEra (EraCrypto era))
  ) =>
  Proof era ->
  (constraint era => t) ->
  t
specialize proof action =
  case proof of
    Shelley -> action
    Allegra -> action
    Mary -> action
    Alonzo -> action
    Babbage -> action
    Conway -> action
{-# NOINLINE specialize #-}

-- =================================================

-- | lift a function (Proof era -> a) that has a (Reflect era) constraint
--   to one that does not. This is possible because every inhabited
--   term of type (Proof era) packs a (Reflect era) instance.
--   so instead of writing:           f proof arg1 .. argn
--   one writes:            unReflect f proof arg1 .. argn
--   which will not require a (Reflect era) instance
unReflect :: (Reflect era => Proof era -> a) -> Proof era -> a
unReflect f Shelley = f Shelley
unReflect f Allegra = f Allegra
unReflect f Mary = f Mary
unReflect f Alonzo = f Alonzo
unReflect f Babbage = f Babbage
unReflect f Conway = f Conway

-- ======================================================

instance Singleton Proof where
  testEql Shelley Shelley = Just Refl
  testEql Allegra Allegra = Just Refl
  testEql Mary Mary = Just Refl
  testEql Alonzo Alonzo = Just Refl
  testEql Babbage Babbage = Just Refl
  testEql Conway Conway = Just Refl
  testEql _ _ = Nothing
  cmpIndex x y = compare (shape x) (shape y)

instance Shaped Proof any where
  shape Shelley = Nary 0 []
  shape Allegra = Nary 1 []
  shape Mary = Nary 2 []
  shape Alonzo = Nary 3 []
  shape Babbage = Nary 4 []
  shape Conway = Nary 5 []

-- ======================================================
-- GADT's that witness the special properties that hold
-- for some type families

data TxOutWit era where
  TxOutShelleyToMary :: (TxOut era ~ ShelleyTxOut era, EraTxOut era, ProtVerAtMost era 8) => TxOutWit era
  TxOutAlonzoToAlonzo :: (TxOut era ~ AlonzoTxOut era, AlonzoEraTxOut era, ProtVerAtMost era 8) => TxOutWit era
  TxOutBabbageToConway :: (TxOut era ~ BabbageTxOut era, BabbageEraTxOut era) => TxOutWit era

whichTxOut :: Proof era -> TxOutWit era
whichTxOut Shelley = TxOutShelleyToMary
whichTxOut Allegra = TxOutShelleyToMary
whichTxOut Mary = TxOutShelleyToMary
whichTxOut Alonzo = TxOutAlonzoToAlonzo
whichTxOut Babbage = TxOutBabbageToConway
whichTxOut Conway = TxOutBabbageToConway

data TxCertWit era where
  TxCertShelleyToBabbage :: (TxCert era ~ ShelleyTxCert era, ShelleyEraTxCert era, ProtVerAtMost era 8) => TxCertWit era
  TxCertConwayToConway :: (TxCert era ~ ConwayTxCert era, ConwayEraTxCert era, ConwayEraPParams era) => TxCertWit era

whichTxCert :: Proof era -> TxCertWit era
whichTxCert Shelley = TxCertShelleyToBabbage
whichTxCert Allegra = TxCertShelleyToBabbage
whichTxCert Mary = TxCertShelleyToBabbage
whichTxCert Alonzo = TxCertShelleyToBabbage
whichTxCert Babbage = TxCertShelleyToBabbage
whichTxCert Conway = TxCertConwayToConway

data ValueWit era where
  ValueShelleyToAllegra :: Value era ~ Coin => ValueWit era
  ValueMaryToConway :: Value era ~ MaryValue (EraCrypto era) => ValueWit era

whichValue :: Proof era -> ValueWit era
whichValue Shelley = ValueShelleyToAllegra
whichValue Allegra = ValueShelleyToAllegra
whichValue Mary = ValueMaryToConway
whichValue Alonzo = ValueMaryToConway
whichValue Babbage = ValueMaryToConway
whichValue Conway = ValueMaryToConway

data PParamsWit era where
  PParamsShelleyToMary :: (PParamsHKD Identity era ~ ShelleyPParams Identity era, EraPParams era) => PParamsWit era
  PParamsAlonzoToAlonzo :: (PParamsHKD Identity era ~ AlonzoPParams Identity era, AlonzoEraPParams era) => PParamsWit era
  PParamsBabbageToBabbage :: (PParamsHKD Identity era ~ BabbagePParams Identity era, BabbageEraPParams era) => PParamsWit era
  PParamsConwayToConway :: (PParamsHKD Identity era ~ ConwayPParams Identity era, ConwayEraPParams era) => PParamsWit era

whichPParams :: Proof era -> PParamsWit era
whichPParams Shelley = PParamsShelleyToMary
whichPParams Allegra = PParamsShelleyToMary
whichPParams Mary = PParamsShelleyToMary
whichPParams Alonzo = PParamsAlonzoToAlonzo
whichPParams Babbage = PParamsBabbageToBabbage
whichPParams Conway = PParamsConwayToConway

data UTxOWit era where
  UTxOShelleyToMary ::
    ( EraUTxO era
    , ScriptsNeeded era ~ ShelleyScriptsNeeded era
    , TxWits era ~ ShelleyTxWits era
    ) =>
    UTxOWit era
  UTxOAlonzoToConway ::
    ( EraUTxO era
    , AlonzoEraScript era
    , AlonzoEraTxBody era
    , AlonzoEraPParams era
    , AlonzoEraTxOut era
    , ScriptsNeeded era ~ AlonzoScriptsNeeded era
    , Script era ~ AlonzoScript era
    , TxWits era ~ AlonzoTxWits era
    ) =>
    UTxOWit era

whichUTxO :: Proof era -> UTxOWit era
whichUTxO Shelley = UTxOShelleyToMary
whichUTxO Allegra = UTxOShelleyToMary
whichUTxO Mary = UTxOShelleyToMary
whichUTxO Alonzo = UTxOAlonzoToConway
whichUTxO Babbage = UTxOAlonzoToConway
whichUTxO Conway = UTxOAlonzoToConway

data ScriptWit era where
  ScriptShelleyToShelley :: (Script era ~ MultiSig era, EraScript era) => ScriptWit era
  ScriptAllegraToMary :: (Script era ~ Timelock era, EraScript era) => ScriptWit era
  ScriptAlonzoToConway :: (Script era ~ AlonzoScript era, EraScript era) => ScriptWit era

whichScript :: Proof era -> ScriptWit era
whichScript Shelley = ScriptShelleyToShelley
whichScript Allegra = ScriptAllegraToMary
whichScript Mary = ScriptAllegraToMary
whichScript Alonzo = ScriptAlonzoToConway
whichScript Babbage = ScriptAlonzoToConway
whichScript Conway = ScriptAlonzoToConway

data GovStateWit era where
  GovStateShelleyToBabbage :: (EraGov era, GovState era ~ ShelleyGovState era) => GovStateWit era
  GovStateConwayToConway ::
    ( ConwayEraPParams era
    , RunConwayRatify era
    , EraGov era
    , GovState era ~ ConwayGovState era
    ) =>
    GovStateWit era

whichGovState :: Proof era -> GovStateWit era
whichGovState Shelley = GovStateShelleyToBabbage
whichGovState Allegra = GovStateShelleyToBabbage
whichGovState Mary = GovStateShelleyToBabbage
whichGovState Alonzo = GovStateShelleyToBabbage
whichGovState Babbage = GovStateShelleyToBabbage
whichGovState Conway = GovStateConwayToConway
