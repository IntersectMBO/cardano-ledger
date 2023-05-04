{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeApplications #-}

module Test.Cardano.Ledger.Constrained.Preds.PParams (
  pParamsPreds,
  pParamsStage,
  extract,
  mainPParams,
) where

import Cardano.Ledger.Alonzo.Scripts (ExUnits (..))
import qualified Cardano.Ledger.Alonzo.Scripts as Script (Prices (..))
import Cardano.Ledger.BaseTypes (
  NonNegativeInterval,
  boundRational,
 )
import Cardano.Ledger.Coin (Coin (..))
import GHC.Num (Natural)
import Lens.Micro ((^.))
import Test.Cardano.Ledger.Constrained.Ast
import Test.Cardano.Ledger.Constrained.Classes (OrdCond (..))
import Test.Cardano.Ledger.Constrained.Env (Access (..), V (..))
import Test.Cardano.Ledger.Constrained.Rewrite (standardOrderInfo)
import Test.Cardano.Ledger.Constrained.Solver
import Test.Cardano.Ledger.Constrained.TypeRep
import Test.Cardano.Ledger.Constrained.Vars
import Test.Cardano.Ledger.Generic.Fields
import Test.Cardano.Ledger.Generic.Functions (protocolVersion)
import Test.Cardano.Ledger.Generic.Proof
import Test.Cardano.Ledger.Generic.Updaters (defaultCostModels, newPParams)
import Test.Tasty.QuickCheck

extract :: Term era t -> Term era s -> Pred era
extract term@(Var (V _ _ (Yes r1 lens))) record =
  case testEql r1 (termRep record) of
    Just Refl -> term :<-: (Constr "lookup" (\x -> x ^. lens) ^$ record)
    Nothing -> error ("Term " ++ show term ++ " with bad Access in extract2")
extract term _ = error ("Non Var term " ++ show term ++ " in extract2")

-- =====================================================

nonNegativeInterval :: Rational -> NonNegativeInterval
nonNegativeInterval r = case (boundRational @NonNegativeInterval r) of
  Just nn -> nn
  Nothing -> error ("Can't make NonNegativeInterval from: " ++ show r)

genPParams :: Reflect era => Proof era -> Natural -> Natural -> Natural -> Gen (PParamsF era)
genPParams proof tx bb bh = do
  maxTxExUnits2 <- ExUnits <$> (fromIntegral <$> choose (100 :: Int, 10000)) <*> (fromIntegral <$> choose (100 :: Int, 10000))
  maxCollateralInputs <- elements [3 .. 5]
  collateralPercentage2 <- fromIntegral <$> chooseInt (1, 200)
  minfeeA <- Coin <$> choose (0, 100)
  minfeeB <- Coin <$> choose (0, 10)
  pure
    ( PParamsF proof $
        newPParams
          proof
          [ MinfeeA minfeeA
          , MinfeeB minfeeB
          , Prices (Script.Prices (nonNegativeInterval 1.0) (nonNegativeInterval 1.0))
          , defaultCostModels proof
          , MaxValSize 1000
          , MaxTxSize tx
          , MaxBBSize bb
          , MaxBHSize bh
          , MaxTxExUnits maxTxExUnits2
          , MaxCollateralInputs maxCollateralInputs
          , CollateralPercentage collateralPercentage2
          , ProtocolVersion $ protocolVersion proof
          , PoolDeposit $ Coin 5
          , KeyDeposit $ Coin 2
          , EMax 100
          ]
    )

pParamsPreds :: Reflect era => Proof era -> [Pred era]
pParamsPreds p =
  [ GenFrom
      (pparams p)
      ( Constr "genPParams" (genPParams p)
          ^$ (maxTxSize p)
          ^$ (maxBBSize p)
          ^$ (maxBHSize p)
      )
  , extract (protVer p) (pparams p)
  , extract (minFeeA p) (pparams p)
  , extract (minFeeB p) (pparams p)
  , extract (keyDepAmt p) (pparams p)
  , extract (poolDepAmt p) (pparams p)
  , extract (maxEpoch p) (pparams p)
  , Sized (AtLeast 100) (maxBHSize p)
  , Sized (AtLeast 40000) (maxTxSize p)
  , SumsTo (Right 1) (maxBBSize p) LTE [One (maxBHSize p), One (maxTxSize p)]
  , (protVer p) `CanFollow` (prevProtVer p)
  ]
    ++ ( case whichPParams p of
          PParamsShelleyToMary -> []
          PParamsAlonzoToAlonzo ->
            [ extract (maxTxExUnits p) (pparams p)
            , extract (collateralPercentage p) (pparams p)
            ]
          PParamsBabbageToConway ->
            [ extract (maxTxExUnits p) (pparams p)
            , extract (collateralPercentage p) (pparams p)
            ]
       )

pParamsStage ::
  Reflect era =>
  Proof era ->
  Subst era ->
  Gen (Subst era)
pParamsStage proof = toolChainSub proof standardOrderInfo (pParamsPreds proof)

mainPParams :: IO ()
mainPParams = do
  let proof = Babbage Standard
  subst <- generate (pParamsStage proof emptySubst)
  putStrLn "\n"
  putStrLn (show subst)
