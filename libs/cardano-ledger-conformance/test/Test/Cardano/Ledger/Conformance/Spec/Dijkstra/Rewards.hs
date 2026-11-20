{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Test.Cardano.Ledger.Conformance.Spec.Dijkstra.Rewards (spec) where

import Cardano.Ledger.BaseTypes.NonZero
import Cardano.Ledger.Coin
import Cardano.Ledger.Core
import Cardano.Ledger.Dijkstra (DijkstraEra)
import Cardano.Ledger.Dijkstra.PParams (ppMaxPledgeLeverageL)
import Cardano.Ledger.State (maxPool)
import Data.Either (fromRight)
import Data.Maybe.Strict
import Lens.Micro ((&), (.~))
import Lens.Micro.Extras (view)
import MAlonzo.Code.Ledger.Dijkstra.Foreign.API qualified as Agda
import Test.Cardano.Ledger.Conformance
import Test.Cardano.Ledger.Conformance.SpecTranslate.Dijkstra ()
import Test.Cardano.Ledger.Imp.Common
import Test.Cardano.Ledger.State.SnapShots

spec :: Spec
spec = describe "Rewards" $ do
  describe "maxPool" $ do
    let testMaxPool maxPledgeLeverage = do
          a0 <- arbitrary
          nOpt <- (\nOpt -> unsafeNonZero (unNonZero nOpt + 1)) <$> genNOpt
          r <- arbitrary
          (sigma, pR) <- genSigmaAndRelativePledge nOpt
          let pp :: PParams DijkstraEra
              pp =
                emptyPParams
                  & ppA0L .~ a0
                  & ppNOptL .~ unNonZero nOpt
                  & ppMaxPledgeLeverageL .~ maxPledgeLeverage

              pp' =
                fromRight
                  ( error $
                      unlines
                        [ "Translation error of pparams:"
                        , "a0: " ++ show a0
                        , "nOpt: " ++ show nOpt
                        ]
                  )
                  ( runSpecTransM @DijkstraEra () $
                      toSpecRep $
                        view ppLensHKD pp
                  )
          pure $
            maxPool pp r sigma pR == Coin (Agda.maxPool pp' (unCoin r) sigma pR)

    prop "without leverage cap" $ testMaxPool (MaxPledgeLeverage SNothing)

    prop "with leverage cap" $ do
      l <- MaxPledgeLeverage . SJust <$> genMaxPledgeLeverage
      testMaxPool l
