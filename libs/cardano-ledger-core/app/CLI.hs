module CLI where

import Cardano.Ledger.Binary (Version, mkVersion64)
import Cardano.Ledger.Plutus.Language (Language)
import Data.ByteString (ByteString)
import Data.Int (Int64)
import Numeric.Natural (Natural)
import Options.Applicative

data Opts = Opts
  { optsScriptWithContext :: !String
  , optsScript :: !(Maybe ByteString)
  , optsProtocolVersion :: !(Maybe Version)
  , optsLanguage :: !(Maybe Language)
  , optsCostModelValues :: !(Maybe [Int64])
  , optsExUnitsMem :: !(Maybe Natural)
  , optsExUnitsSteps :: !(Maybe Natural)
  }
  deriving (Show)

optsParser :: Parser Opts
optsParser =
  Opts
    <$> strArgument
      (metavar "SCRIPT_WITH_CONTEXT(BASE64)")
    <*> option
      (Just <$> str)
      ( long "script"
          <> value Nothing
          <> help "Plutus script"
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
      (str >>= pure . Just . map read . words)
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
