{-# LANGUAGE CPP #-}

module GetDataFileName (
  withTestFileProperty,
  (<:<),
)
where

import Control.Monad.IO.Class
import Hedgehog.Internal.Property
import Prelude

#ifdef CARDANO_LEDGER_BYRON_TEST
import Paths_cardano_ledger_byron_test (getDataFileName)
#else
import Paths_cardano_ledger_byron (getDataFileName)
#endif

-- | This contraption is necessary because test modules in the folder are used
-- in two different packages, but the file paths for test files are relative to
-- the package itself, which has to accounted for.
withTestFileProperty :: FilePath -> (FilePath -> Property) -> Property
withTestFileProperty fp mkProperty = Property config $ do
  actualFilePath <- liftIO $ getDataFileName fp
  propertyTest $ mkProperty actualFilePath
  where
    config = propertyConfig $ mkProperty ""

(<:<) :: (FilePath -> Property) -> FilePath -> Property
(<:<) = flip withTestFileProperty
