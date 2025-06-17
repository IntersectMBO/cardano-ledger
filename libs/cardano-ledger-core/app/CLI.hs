module CLI (
  Opts (..),
  optsParser,
) where

import Cardano.Ledger.Binary (mkVersion64)
import Cardano.Ledger.Plutus.Evaluate
import Options.Applicative
import Text.Read (readMaybe)

data Opts = Opts
  { optsScriptWithContext :: !String
  , optsOverrides :: !PlutusDebugOverrides
  }
  deriving (Show)

overridesParser :: Parser PlutusDebugOverrides
overridesParser =
  PlutusDebugOverrides
    <$> option
      (Just <$> str)
      ( long "script"
          <> value Nothing
          <> help "Plutus script hex without context"
      )
    <*> option
      (mkVersion64 <$> auto)
      ( long "protocol-version"
          <> short 'v'
          <> value Nothing
          <> help "Major protocol version"
      )
    <*> option
      (Just <$> auto)
      ( long "language"
          <> value Nothing
          <> help "Plutus language version"
      )
    <*> option
      (mapM readMaybe . words <$> str)
      ( long "cost-model-values"
          <> value Nothing
          <> help ""
      )
    <*> option
      (Just <$> auto)
      ( long "execution-units-memory"
          <> value Nothing
          <> help ""
      )
    <*> option
      (Just <$> auto)
      ( long "execution-units-steps"
          <> value Nothing
          <> help ""
      )
    <*> option
      (Just <$> auto)
      ( long "timeout"
          <> value Nothing
          <> help
            ( "Timeout in number of milliseconds. Default is 5000000 ms (or 5 seconds). "
                <> "Specifying a negative number will effectively remove the timeout and unbound execution."
            )
      )

optsParser :: Parser Opts
optsParser =
  Opts
    <$> strArgument
      (metavar "SCRIPT_WITH_CONTEXT(BASE64)")
    <*> overridesParser
