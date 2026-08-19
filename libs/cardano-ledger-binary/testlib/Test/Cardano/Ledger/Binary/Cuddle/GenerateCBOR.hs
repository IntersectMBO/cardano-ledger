{-# LANGUAGE GADTs #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}

module Test.Cardano.Ledger.Binary.Cuddle.GenerateCBOR (
  generateCBORMain,
) where

import Codec.CBOR.Cuddle.CBOR.Gen
import Codec.CBOR.Cuddle.CBOR.Validator
import Codec.CBOR.Cuddle.CBOR.Validator.Trace
import Codec.CBOR.Cuddle.CDDL
import Codec.CBOR.Cuddle.CDDL.Custom.Generator
import qualified Codec.CBOR.Cuddle.Huddle as Cuddle
import Codec.CBOR.Cuddle.IndexMappable
import qualified Codec.CBOR.Term as CBOR
import qualified Codec.CBOR.Write as CBOR
import Control.Monad (forM_, when)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Base16 as Base16
import qualified Data.ByteString.Char8 as BS8
import Data.List (intercalate)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe (fromMaybe)
import qualified Data.Text as T
import Options.Applicative ((<**>))
import qualified Options.Applicative as Opt
import System.Exit (die)
import System.IO
import Test.AntiGen
import Test.Cardano.Ledger.Binary.Cuddle
import Test.QuickCheck (generate)
import Test.QuickCheck.Gen (unGen)
import Test.QuickCheck.Random (mkQCGen)

data GenerateCBOROpts = GenerateCBOROpts
  { gcboEra :: !T.Text
  , gcboRuleNames :: ![T.Text]
  , gcboZap :: !(Maybe Int)
  , gcboCount :: !Int
  , gcboSeed :: !(Maybe Int)
  , gcboBinary :: !Bool
  , gcboTries :: !Int
  , gcboVerbose :: !Bool
  }

readNonNegativeInt :: Opt.ReadM Int
readNonNegativeInt = do
  n <- Opt.auto
  when (n < 0) $ Opt.readerError "Expected a nonnegative number"
  pure n

optsParser :: [String] -> Opt.Parser GenerateCBOROpts
optsParser eras =
  GenerateCBOROpts
    <$> Opt.strOption
      ( Opt.long "era"
          <> Opt.metavar "ERA"
          <> Opt.help ("Era to generate CBOR for. One of: " <> intercalate ", " eras)
      )
    <*> Opt.some
      ( Opt.strArgument $
          Opt.metavar "RULE_NAME..."
            <> Opt.help "CDDL rule names to generate CBOR for"
      )
    <*> Opt.optional
      ( Opt.option readNonNegativeInt $
          Opt.long "zap"
            <> Opt.metavar "N"
            <> Opt.help "Generate corrupted (zapped) CBOR with N mistakes"
      )
    <*> Opt.option
      readNonNegativeInt
      ( Opt.long "count"
          <> Opt.short 'n'
          <> Opt.metavar "N"
          <> Opt.value 1
          <> Opt.showDefault
          <> Opt.help "Number of samples to generate per rule"
      )
    <*> Opt.optional
      ( Opt.option Opt.auto $
          Opt.long "seed"
            <> Opt.metavar "SEED"
            <> Opt.help "Fixed random seed for reproducibility"
      )
    <*> Opt.switch
      ( Opt.long "binary"
          <> Opt.help "Output raw CBOR bytes instead of hex encoding"
      )
    <*> Opt.option
      readNonNegativeInt
      ( Opt.long "tries"
          <> Opt.short 't'
          <> Opt.metavar "N"
          <> Opt.value 50
          <> Opt.showDefault
          <> Opt.help "Number of retries for failed sample generations"
      )
    <*> Opt.switch
      ( Opt.long "verbose"
          <> Opt.help "More verbose output"
      )

generateCBORMain :: Map T.Text Cuddle.Huddle -> IO ()
generateCBORMain eraCDDLs = do
  opts <-
    Opt.execParser $
      Opt.info
        (optsParser eraNames <**> Opt.helper)
        ( Opt.fullDesc
            <> Opt.progDesc "Generate CBOR data from Cardano Ledger CDDL rules"
            <> Opt.header "generate-cbor - CBOR data generator from Cardano Ledger CDDL specifications"
        )
  huddle <- case Map.lookup (gcboEra opts) eraCDDLs of
    Nothing ->
      die $
        "Unknown era: "
          <> T.unpack (gcboEra opts)
          <> "\nSupported eras: "
          <> intercalate ", " eraNames
    Just h -> pure h
  case resolveHuddle huddle of
    Left err -> die $ "Failed to resolve CDDL: " <> err
    Right root -> do
      when (gcboBinary opts) $ hSetBinaryMode stdout True
      let env = HuddleEnv {heTwiddle = True, heRoot = root}
          multipleRules = length (gcboRuleNames opts) > 1
      forM_ (gcboRuleNames opts) $ \ruleName -> do
        when (multipleRules && not (gcboBinary opts)) $
          hPutStrLn stderr $
            "# " <> T.unpack ruleName
        forM_ [0 .. gcboCount opts - 1] $ emitSample 0 opts env ruleName
  where
    eraNames = map T.unpack (Map.keys eraCDDLs)

emitSample :: Int -> GenerateCBOROpts -> HuddleEnv -> T.Text -> Int -> IO ()
emitSample tries opts env ruleName sampleIx
  | tries > gcboTries opts =
      dieWithInfo $ "failed to generate a sample after " <> show (gcboTries opts + 1) <> " attempts"
  | otherwise = do
      let cborGen = runCBORGen (toGenConfig env) (generateFromName (Name ruleName))
          gen = zapAntiGenResult (fromMaybe 0 (gcboZap opts)) cborGen
          retry = emitSample (tries + 1) opts env ruleName sampleIx
      ZapResult {zrValue, zrZapped} <- case gcboSeed opts of
        Nothing -> generate gen
        Just seed -> pure $ unGen gen (mkQCGen (seed + sampleIx + gcboCount opts * tries)) 30
      let
        bs = CBOR.toStrictByteString (CBOR.encodeTerm zrValue)
        outputBinary
          | gcboBinary opts = BS.hPut stdout bs
          | otherwise = BS8.putStrLn (Base16.encode bs)
        validationResult = validateCBOR bs (Name ruleName) (mapIndex (heRoot env))
      case gcboZap opts of
        Just nZaps
          | nZaps == 0 -> outputBinary
          | zrZapped < nZaps ->
              warn "Warning: not enough decision points for the zapper to zap, retrying" >> retry
          | otherwise -> case validationResult of
              Left (RuleDoesNotExist n) -> dieWithInfo $ "Failed to validate because rule does not exist: " <> show n
              Left (LeftoverBytes _) ->
                warn "Warning: leftover bytes after decoding generated CBOR, retrying" >> retry
              Left (DecodingFailed e) ->
                warn ("Warning: failed to decode generated CBOR, retrying (" <> show e <> ")") >> retry
              Right (Evidenced SValid _) ->
                warn "Warning: zapper failed to generate an invalid value, retrying" >> retry
              Right (Evidenced SInvalid _) -> outputBinary
        Nothing -> outputBinary
  where
    extraInfo = "\n(rule: " <> T.unpack ruleName <> ", index: " <> show sampleIx <> ")"
    warn msg
      | gcboVerbose opts =
          hPutStrLn stderr $ msg <> extraInfo
      | otherwise = pure ()
    dieWithInfo msg =
      die $ msg <> extraInfo
