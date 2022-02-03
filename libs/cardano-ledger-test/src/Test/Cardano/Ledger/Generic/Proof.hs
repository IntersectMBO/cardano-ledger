-- ifCurrentProof
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
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
import Cardano.Ledger.Crypto (StandardCrypto)
import qualified Cardano.Ledger.Crypto as CC (Crypto, DSIGN, HASH)
import Cardano.Ledger.Era (Era (..), ValidateScript (..))
import Cardano.Ledger.Keys (DSignable)
import Cardano.Ledger.Mary (MaryEra)
import Cardano.Ledger.Shelley (ShelleyEra)
import Cardano.Ledger.Shelley.TxBody (EraIndependentTxBody)
import Test.Cardano.Ledger.Generic.Parametric (Parametric)
import Test.Cardano.Ledger.Shelley.ConcreteCryptoTypes (C_Crypto)
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
    ReflectC (Crypto era),
    Parametric era
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

data Some f where
  Some :: f a -> Some f

preShelley, preAllegra, preMary, preAlonzo, preBabbage :: CC.Crypto c => Evidence c -> [Some Proof]
preShelley c = [Some (Shelley c)]
preAllegra c = [Some (Allegra c), Some (Shelley c)]
preMary c = [Some (Mary c), Some (Allegra c), Some (Shelley c)]
preAlonzo c = [Some (Alonzo c), Some (Mary c), Some (Allegra c), Some (Shelley c)]
preBabbage c = [Some (Babbage c), Some (Alonzo c), Some (Mary c), Some (Allegra c), Some (Shelley c)]

postShelley, postAllegra, postMary, postAlonzo, postBabbage :: CC.Crypto c => Evidence c -> [Some Proof]
postShelley c = [Some (Babbage c), Some (Alonzo c), Some (Mary c), Some (Allegra c), Some (Shelley c)]
postAllegra c = [Some (Babbage c), Some (Alonzo c), Some (Mary c), Some (Allegra c)]
postMary c = [Some (Babbage c), Some (Alonzo c), Some (Mary c)]
postAlonzo c = [Some (Babbage c), Some (Alonzo c)]
postBabbage c = [Some (Babbage c)]

allEra :: Testable p => String -> [Some Proof] -> (forall era. Proof era -> p) -> TestTree
allEra name eras f = testProperties name (map g eras)
  where
    g (Some era) = (show era, property (f era))

ifProof :: Proof era -> [Some Proof] -> a -> a -> a
ifProof proof ps whentrue whenfalse =
  if elem (Some proof) ps then whentrue else whenfalse

ifCurrentProof :: forall era a. Reflect era => [Some Proof] -> a -> a -> a
ifCurrentProof ps t f = ifProof (reify @era) ps t f

instance Eq (Some Proof) where
  (Some (Shelley _)) == (Some (Shelley _)) = True
  (Some (Allegra _)) == (Some (Allegra _)) = True
  (Some (Mary _)) == (Some (Mary _)) = True
  (Some (Alonzo _)) == (Some (Alonzo _)) = True
  (Some (Babbage _)) == (Some (Babbage _)) = True
  _ == _ = False
