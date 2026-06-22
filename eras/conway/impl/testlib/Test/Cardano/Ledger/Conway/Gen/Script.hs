{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Test.Cardano.Ledger.Conway.Gen.Script (
  genScript,
  genShelleyScript,
  genAllegraScript,
  genAlonzoScript,
  genBabbageScript,
  genConwayScript,
) where

import Cardano.Ledger.Allegra.Scripts (
  AllegraEraScript,
  pattern RequireTimeExpire,
  pattern RequireTimeStart,
 )
import Cardano.Ledger.Alonzo.Plutus.Context (
  EraPlutusContext,
  mkSupportedLanguage,
 )
import Cardano.Ledger.Alonzo.Scripts (fromPlutusScript)
import Cardano.Ledger.Conway.Scripts (ConwayEraScript)
import Cardano.Ledger.Core
import Cardano.Ledger.Plutus.Language (Language (..))
import Cardano.Ledger.Shelley.Scripts
import Control.Monad.State.Strict (MonadState)
import Data.Sequence.Strict (fromList)
import Test.Cardano.Ledger.Alonzo.Arbitrary (genPlutusScript)
import Test.Cardano.Ledger.Imp.Common
import Test.Cardano.Ledger.Shelley.ImpTest (HasKeyPairs, freshKeyHash)

genNativeScript ::
  ( ShelleyEraScript era
  , MonadGen m
  , HasKeyPairs s
  , MonadState s m
  , HasStatefulGen g m
  ) =>
  [m (NativeScript era)] ->
  m (Script era)
genNativeScript allegraExtras = fromNativeScript <$> sized go
  where
    go 0 = RequireSignature <$> freshKeyHash
    go n =
      let children = do
            k <- choose (0, n)
            fromList <$> replicateM k (go (n `div` 2))
       in oneof $
            [ RequireSignature <$> freshKeyHash
            , RequireAllOf <$> children
            , RequireAnyOf <$> children
            , do
                cs <- children
                m <- choose (0, length cs)
                pure (RequireMOf m cs)
            ]
              <> allegraExtras

genShelleyScript ::
  ( ShelleyEraScript era
  , MonadGen m
  , HasKeyPairs s
  , MonadState s m
  , HasStatefulGen g m
  ) =>
  m (Script era)
genShelleyScript = genNativeScript []

genAllegraScript ::
  ( AllegraEraScript era
  , MonadGen m
  , HasKeyPairs s
  , MonadState s m
  , HasStatefulGen g m
  ) =>
  m (Script era)
genAllegraScript =
  genNativeScript
    [ RequireTimeStart <$> arbitrary
    , RequireTimeExpire <$> arbitrary
    ]

genPlutus ::
  forall era m.
  (EraPlutusContext era, MonadGen m) =>
  Language ->
  m (Script era)
genPlutus lang = case mkSupportedLanguage @era lang of
  Just supported -> fromPlutusScript <$> liftGen (genPlutusScript supported)
  Nothing -> error ("Language " ++ show lang ++ " not supported in this era")

genAlonzoScript ::
  (EraPlutusContext era, MonadGen m) =>
  m (Script era)
genAlonzoScript = genPlutus PlutusV1

genBabbageScript ::
  (EraPlutusContext era, MonadGen m) =>
  m (Script era)
genBabbageScript = genPlutus PlutusV2

genConwayScript ::
  (EraPlutusContext era, MonadGen m) =>
  m (Script era)
genConwayScript = genPlutus PlutusV3

genScript ::
  ( ConwayEraScript era
  , EraPlutusContext era
  , MonadGen m
  , HasKeyPairs s
  , MonadState s m
  , HasStatefulGen g m
  ) =>
  m (Script era)
genScript =
  frequency
    [ (40, resize 5 genShelleyScript)
    , (20, resize 5 genAllegraScript)
    , (10, genAlonzoScript)
    , (15, genBabbageScript)
    , (15, genConwayScript)
    ]
