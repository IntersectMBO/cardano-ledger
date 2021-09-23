{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}

module Byron.Spec.Ledger.Util
  ( mkGoblinGens,
  )
where

import Language.Haskell.TH
import System.FilePath ((<.>), (</>))
import Test.Goblin (loadGoblinDataFromFilePath)

loadGD :: String -> Q Exp
loadGD pfName =
  loadGoblinDataFromFilePath relPath
  where
    -- data files we get at compile time are always relative to the project
    relPath = "src" </> "goblin_genomes" </> pfName <.> "genome"

-- | Take a name (e.g. "DELEG") and a list of `PredicateFailure`s in `renderPF`
-- form (see repo goblins-sts-breeder; STSExtra typeclass) (e.g.
-- ["UTXOW_InsufficientWitnesses"]) and returns `n+1` declarations, where n
-- is the length of the `pfNames` list. There will be 1 decl per element of
-- `pfNames`, which wraps a goblin mutation around the valid `SignalGenerator`.
-- The final decl is a toplevel list of the previously defined `SignalGenerator`s.
mkGoblinGens :: String -> [String] -> Q [Dec]
mkGoblinGens stsNameStr pfNames = do
  b <- isExtEnabled TypeApplications
  if b
    then body
    else error "TypeApplications required"
  where
    body :: Q [Dec]
    body = do
      pairs <- sequence (map (mkGoblinDecls stsNameStr) pfNames)
      let goblinGenNames = map fst pairs
          goblinGenDecs = concatMap snd pairs
      let listName = mkName ("goblinGens" <> stsNameStr)
          listSigDec = SigD listName (AppT ListT sigGenTy)
          listDataDec =
            ValD
              (VarP listName)
              (NormalB (ListE (map VarE goblinGenNames)))
              []
      pure (goblinGenDecs <> [listSigDec, listDataDec])

    stsName = mkName stsNameStr

    sigGenTy :: Type
    sigGenTy =
      AppT
        (ConT (mkName "SignalGenerator"))
        (ConT stsName)

-- | Makes a top-level `Dec` for the GoblinData (loaded at TH evaluation time
-- from a file) and another top-level `Dec` for an invalid generator using the
-- GoblinData declaration.
mkGoblinDecls :: String -> String -> Q (Name, [Dec])
mkGoblinDecls stsNameStr pfNameStr = do
  decs <- mconcat <$> sequence [goblinDataDecs, goblinGenDecs]
  pure (ggName, decs)
  where
    genomeName = stsNameStr <> "_" <> pfNameStr
    gdName = mkName ("goblinData_" <> genomeName)
    ggName = mkName ("goblinGen_" <> genomeName)
    stsName = mkName stsNameStr

    goblinDataDecs = do
      body <- loadGD genomeName
      let sigDec =
            SigD
              gdName
              ( AppT
                  (ConT (mkName "GoblinData"))
                  (ConT (mkName "Bool"))
              )
      let dataDec = ValD (VarP gdName) (NormalB body) []
      pure [sigDec, dataDec]

    goblinGenDecs = do
      body <-
        [|
          tinkerWithSigGen @Bool @ $(pure (ConT stsName))
            $(pure (VarE gdName))
          |]
      let sigDec =
            SigD
              ggName
              ( AppT
                  (ConT (mkName "SignalGenerator"))
                  (ConT stsName)
              )
      let dataDec = ValD (VarP ggName) (NormalB body) []
      pure [sigDec, dataDec]
