-- ifCurrentProof
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
-- Used for Refect classes
{-# LANGUAGE UndecidableInstances #-}
-- Used for Refect classes
{-# LANGUAGE UndecidableSuperClasses #-}

module Test.Cardano.Ledger.Generic.Proof where

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
import Control.State.Transition.Extended hiding (Assertion)
import Data.Kind (Type)
import GHC.Natural
import GHC.TypeLits (Symbol)
import Test.Cardano.Ledger.Shelley.ConcreteCryptoTypes (C_Crypto)
import Test.Cardano.Ledger.Shelley.Utils (applySTSTest, runShelleyBase)
import Test.Tasty (TestTree)
import Test.Tasty.QuickCheck (Testable (..), testProperties)

-- =================================================
-- GADTs for witnessing Crypto and Era

type Mock = C_Crypto

type Standard = StandardCrypto

type ShelleyMock = ShelleyEra Mock

type ShelleyReal = ShelleyEra Standard

type AllegraMock = AllegraEra Mock

type AllegraReal = AllegraEra Standard

type MaryMock = MaryEra Mock

type MaryReal = MaryEra Standard

type AlonzoMock = AlonzoEra Mock

type AlonzoReal = AlonzoEra Standard

type BabbageMock = BabbageEra Mock

type BabbageReal = BabbageEra Standard

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

allEra :: Testable p => String -> [Some Proof] -> (forall era. Proof era -> p) -> TestTree
allEra name eras f = testProperties name (map g eras)
  where
    g (Some era) = (show era, property (f era))

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

ruleProof :: WitRule s e -> Proof e
ruleProof (UTXOW p) = p
ruleProof (LEDGER p) = p
ruleProof (BBODY p) = p

runSTS ::
  forall s e ans.
  ( BaseM (Core.EraRule s e) ~ ShelleyBase,
    STS (Core.EraRule s e)
  ) =>
  WitRule s e ->
  RuleContext 'Transition (Core.EraRule s e) ->
  (Either [PredicateFailure (Core.EraRule s e)] (State (Core.EraRule s e)) -> ans) ->
  ans
runSTS (UTXOW _proof) x cont = cont (runShelleyBase (applySTSTest x))
runSTS (LEDGER _proof) x cont = cont (runShelleyBase (applySTSTest x))
runSTS (BBODY _proof) x cont = cont (runShelleyBase (applySTSTest x))

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
