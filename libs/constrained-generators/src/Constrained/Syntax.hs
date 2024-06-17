{-# LANGUAGE ImportQualifiedPost #-}

module Constrained.Syntax where

import Language.Haskell.TH qualified as TH
import Language.Haskell.TH.Quote qualified as TH

mkNamed :: String -> TH.Q TH.Pat
mkNamed x =
  pure $
    TH.ViewP (TH.AppE (TH.VarE $ TH.mkName "name") (TH.LitE $ TH.StringL x)) (TH.VarP $ TH.mkName x)

mkNamedExpr :: String -> TH.Q TH.Exp
mkNamedExpr x =
  pure $
    TH.AppE (TH.AppE (TH.VarE $ TH.mkName "name") (TH.LitE $ TH.StringL x)) (TH.VarE $ TH.mkName x)

var :: TH.QuasiQuoter
var =
  TH.QuasiQuoter
    { -- Parses variables e.g. `constrained $ \ [var| x |] [var| y |] -> ...` from the strings " x " and " y "
      -- and replaces them with `name "x" -> x` and `name "y" -> y`
      TH.quotePat = mkNamed . head . words
    , -- Parses variables in expressions like `assert $ [var| x |] + 3 <. 10` and replaces them with `name "x" x`
      TH.quoteExp = mkNamedExpr . head . words -- Parses
    , TH.quoteDec = const $ fail "var should only be used at binding sites and in expressions"
    , TH.quoteType = const $ fail "var should only be used at binding sites and in expressions"
    }
