{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeApplications #-}

module Main where

import CLI
import Cardano.Ledger.Crypto (StandardCrypto)
import Cardano.Ledger.Plutus.Evaluate
import Options.Applicative

main :: IO ()
main = do
  Opts {..} <-
    execParser $
      info
        (optsParser <* abortOption (ShowHelpText Nothing) (long "help"))
        ( header "plutus-debug - A Plutus script debugger"
            <> progDesc
              ( "The purpose of this tool is to troubleshoot failing Plutus scripts. "
                  <> "When you encounter a `PlutusFailure`, you can pass the `Base64-encoded script bytes` "
                  <> "to `plutus-debug` for debugging purposes and override the context of the failed script "
                  <> "and the script itself with the available command line options."
              )
            <> footer
              ( "EXAMPLE: plutus-debug \"hgmCAVksj...\" --script \"5906ab010...\" "
                  <> "Note when rewriting the script with the `--script` option "
                  <> "you will have to provide the hex of the Plutus script as seen in "
                  <> "`Test.Cardano.Ledger.Plutus.Examples`."
              )
        )
  debugPlutus @StandardCrypto optsScriptWithContext optsOverrides >>= print
