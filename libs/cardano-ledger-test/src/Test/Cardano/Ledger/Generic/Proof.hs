{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
-- Used for Reflect classes
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE UndecidableSuperClasses #-}

module Test.Cardano.Ledger.Generic.Proof
  ( Mock,
    Standard,
    Evidence (..),
    Proof (..),
    getCrypto,
    GoodCrypto,
    ReflectC (..),
    Reflect (..),
    Some (..),
    Ranked (..),
    WitRule (..),
    ruleProof,
    runSTS,
    goSTS,
    preShelley,
    preAllegra,
    preMary,
    preAlonzo,
    preBabbage,
    postShelley,
    postAllegra,
    postMary,
    postAlonzo,
    postBabbage,
    ShelleyEra,
    AllegraEra,
    MaryEra,
    AlonzoEra,
    BabbageEra,
    C_Crypto,
  )
where

import Cardano.Crypto.DSIGN.Class (DSIGNAlgorithm)
import qualified Cardano.Crypto.Hash as CH
import Cardano.Ledger.Allegra (AllegraEra)
import Cardano.Ledger.Alonzo (AlonzoEra)
import Cardano.Ledger.Babbage (BabbageEra)
import Cardano.Ledger.BaseTypes (ShelleyBase)
import qualified Cardano.Ledger.Core as Core
import Cardano.Ledger.Crypto (StandardCrypto)
import qualified Cardano.Ledger.Crypto as CC (Crypto, DSIGN, HASH)
import Cardano.Ledger.Era (Era (..), ValidateScript (..))
import Cardano.Ledger.Keys (DSignable)
import Cardano.Ledger.Mary (MaryEra)
import Cardano.Ledger.Shelley (ShelleyEra)
import Cardano.Ledger.Shelley.TxBody (EraIndependentTxBody)
import Cardano.Ledger.Val (Val)
import Control.State.Transition.Extended hiding (Assertion)
import Data.Kind (Type)
import GHC.Natural
import GHC.TypeLits (Symbol)
import Test.Cardano.Ledger.Shelley.ConcreteCryptoTypes (C_Crypto)
import Test.Cardano.Ledger.Shelley.Utils (applySTSTest, runShelleyBase)

-- =================================================
-- GADTs for witnessing Crypto and Era

type Mock = C_Crypto

type Standard = StandardCrypto

data Evidence c where
  Standard :: Evidence Standard
  Mock :: Evidence Mock

-- | Proof of a valid (predefined) era
data Proof era where
  Shelley :: forall c. CC.Crypto c => Evidence c -> Proof (ShelleyEra c)
  Mary :: forall c. CC.Crypto c => Evidence c -> Proof (MaryEra c)
  Allegra :: forall c. CC.Crypto c => Evidence c -> Proof (AllegraEra c)
  Alonzo :: forall c. CC.Crypto c => Evidence c -> Proof (AlonzoEra c)
  Babbage :: forall c. CC.Crypto c => Evidence c -> Proof (BabbageEra c)

instance Show (Proof e) where
  show (Shelley c) = "Shelley " ++ show c
  show (Allegra c) = "Allegra " ++ show c
  show (Mary c) = "Mary " ++ show c
  show (Alonzo c) = "Alonzo " ++ show c
  show (Babbage c) = "Babbage " ++ show c

instance Show (Evidence c) where
  show Mock = "Mock"
  show Standard = "Standard"

getCrypto :: Proof era -> Evidence (Crypto era)
getCrypto (Babbage c) = c
getCrypto (Alonzo c) = c
getCrypto (Mary c) = c
getCrypto (Allegra c) = c
getCrypto (Shelley c) = c

-- ==================================
-- Reflection over Crypto and Era

type GoodCrypto crypto =
  ( CC.Crypto crypto,
    DSignable crypto (CH.Hash (CC.HASH crypto) EraIndependentTxBody),
    DSIGNAlgorithm (CC.DSIGN crypto)
  )

class (GoodCrypto c) => ReflectC c where
  evidence :: Evidence c
  liftC :: forall a. (Evidence c -> a) -> a
  liftC f = f (evidence @c)

instance ReflectC StandardCrypto where
  evidence = Standard

instance ReflectC C_Crypto where
  evidence = Mock

class
  ( Era era,
    ValidateScript era,
    Val (Core.Value era),
    ReflectC (Crypto era)
  ) =>
  Reflect era
  where
  reify :: Proof era
  lift :: forall a. (Proof era -> a) -> a
  lift f = f (reify @era)

instance ReflectC c => Reflect (BabbageEra c) where
  reify = Babbage evidence

instance ReflectC c => Reflect (AlonzoEra c) where
  reify = Alonzo evidence

instance ReflectC c => Reflect (MaryEra c) where
  reify = Mary evidence

instance ReflectC c => Reflect (AllegraEra c) where
  reify = Allegra evidence

instance ReflectC c => Reflect (ShelleyEra c) where
  reify = Shelley evidence

-- ===================================================
-- Tools for building TestTrees for multiple Eras

instance Eq (Some Proof) where
  (Some (Shelley _)) == (Some (Shelley _)) = True
  (Some (Allegra _)) == (Some (Allegra _)) = True
  (Some (Mary _)) == (Some (Mary _)) = True
  (Some (Alonzo _)) == (Some (Alonzo _)) = True
  (Some (Babbage _)) == (Some (Babbage _)) = True
  _ == _ = False

instance Show (Some Proof) where
  show (Some (Shelley c)) = show (Shelley c)
  show (Some (Allegra c)) = show (Allegra c)
  show (Some (Mary c)) = show (Mary c)
  show (Some (Alonzo c)) = show (Alonzo c)
  show (Some (Babbage c)) = show (Babbage c)

-- ===============================================================
-- Proofs or witnesses to Core.EraRule Tags

data WitRule (s :: Symbol) (e :: Type) where
  UTXOW :: Proof era -> WitRule "UTXOW" era
  LEDGER :: Proof era -> WitRule "LEDGER" era
  BBODY :: Proof era -> WitRule "BBODY" era
  LEDGERS :: Proof era -> WitRule "LEDGERS" era
  MOCKCHAIN :: Proof era -> WitRule "MOCKCHAIN" era

ruleProof :: WitRule s e -> Proof e
ruleProof (UTXOW p) = p
ruleProof (LEDGER p) = p
ruleProof (BBODY p) = p
ruleProof (LEDGERS p) = p
ruleProof (MOCKCHAIN p) = p

runSTS ::
  forall s e ans.
  ( BaseM (Core.EraRule s e) ~ ShelleyBase,
    STS (Core.EraRule s e)
  ) =>
  WitRule s e ->
  TRC (Core.EraRule s e) ->
  (Either [PredicateFailure (Core.EraRule s e)] (State (Core.EraRule s e)) -> ans) ->
  ans
runSTS (UTXOW _proof) x cont = cont (runShelleyBase (applySTSTest x))
runSTS (LEDGER _proof) x cont = cont (runShelleyBase (applySTSTest x))
runSTS (BBODY _proof) x cont = cont (runShelleyBase (applySTSTest x))
runSTS (LEDGERS _proof) x cont = cont (runShelleyBase (applySTSTest x))
runSTS (MOCKCHAIN _proof) x cont = cont (runShelleyBase (applySTSTest x))

-- | Like runSTS, but makes the components of the TRC triple explicit.
--   in case you can't remember, in ghci type
-- @@@
--   :t goSTS (UTXOW (Babbage Mock))
--   goSTS (LEDGER (Babbage Mock))
--     :: LedgerEnv (BabbageEra C_Crypto)
--        -> (UTxOState (BabbageEra C_Crypto), DPState C_Crypto)
--        -> Cardano.Ledger.Alonzo.Tx.ValidatedTx (BabbageEra C_Crypto)
--        -> (Either
--              [LedgerPredicateFailure (BabbageEra C_Crypto)]
--              (UTxOState (BabbageEra C_Crypto), DPState C_Crypto)
--        -> ans)
--        -> ans
-- @@@
--   it will tell you what type 'env' 'state' and 'sig' are for Babbage
goSTS ::
  forall s e ans env state sig.
  ( BaseM (Core.EraRule s e) ~ ShelleyBase,
    STS (Core.EraRule s e),
    env ~ Environment (Core.EraRule s e),
    state ~ State (Core.EraRule s e),
    sig ~ Signal (Core.EraRule s e)
  ) =>
  WitRule s e ->
  env ->
  state ->
  sig ->
  (Either [PredicateFailure (Core.EraRule s e)] (State (Core.EraRule s e)) -> ans) ->
  ans
goSTS (UTXOW _proof) env state sig cont =
  cont (runShelleyBase (applySTSTest (TRC @(Core.EraRule s e) (env, state, sig))))
goSTS (LEDGER _proof) env state sig cont =
  cont (runShelleyBase (applySTSTest (TRC @(Core.EraRule s e) (env, state, sig))))
goSTS (BBODY _proof) env state sig cont =
  cont (runShelleyBase (applySTSTest (TRC @(Core.EraRule s e) (env, state, sig))))
goSTS (LEDGERS _proof) env state sig cont =
  cont (runShelleyBase (applySTSTest (TRC @(Core.EraRule s e) (env, state, sig))))
goSTS (MOCKCHAIN _proof) env state sig cont =
  cont (runShelleyBase (applySTSTest (TRC @(Core.EraRule s e) (env, state, sig))))

-- ================================================================
-- Crypto agnostic operations on (Proof era) via (Some Proof)

data Some f where
  Some :: f a -> Some f

class Ranked t where
  rank :: t i -> Natural

instance (Eq (Some t), Ranked t) => Ord (Some t) where
  compare (Some x) (Some y) = compare (rank x) (rank y)

instance Ranked Proof where
  rank (Shelley _) = 0
  rank (Allegra _) = 1
  rank (Mary _) = 2
  rank (Alonzo _) = 3
  rank (Babbage _) = 4

preShelley, preAllegra, preMary, preAlonzo, preBabbage :: [Some Proof]
preShelley = [Some (Shelley Mock)]
preAllegra = [Some (Allegra Mock), Some (Shelley Mock)]
preMary = [Some (Mary Mock), Some (Allegra Mock), Some (Shelley Mock)]
preAlonzo = [Some (Alonzo Mock), Some (Mary Mock), Some (Allegra Mock), Some (Shelley Mock)]
preBabbage = [Some (Babbage Mock), Some (Alonzo Mock), Some (Mary Mock), Some (Allegra Mock), Some (Shelley Mock)]

postShelley, postAllegra, postMary, postAlonzo, postBabbage :: [Some Proof]
postShelley =
  [ Some (Babbage Mock),
    Some (Alonzo Mock),
    Some (Mary Mock),
    Some (Allegra Mock),
    Some (Shelley Mock)
  ]
postAllegra = [Some (Babbage Mock), Some (Alonzo Mock), Some (Mary Mock), Some (Allegra Mock)]
postMary = [Some (Babbage Mock), Some (Alonzo Mock), Some (Mary Mock)]
postAlonzo = [Some (Babbage Mock), Some (Alonzo Mock)]
postBabbage = [Some (Babbage Mock)]
