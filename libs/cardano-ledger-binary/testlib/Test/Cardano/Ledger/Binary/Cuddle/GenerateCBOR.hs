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
import qualified Data.ByteString.Base16 as Base16
import qualified Data.ByteString.Char8 as BS8
import Data.Maybe (fromMaybe, isJust)
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
  { gcboRuleNames :: ![T.Text]
  , gcboZap :: !(Maybe Int)
  , gcboSeed :: !(Maybe Int)
  }

optsParser :: Opt.Parser GenerateCBOROpts
optsParser =
  GenerateCBOROpts
    <$> Opt.some
      ( Opt.strArgument $
          Opt.metavar "RULE_NAME..."
            <> Opt.help "CDDL rule names to generate CBOR for"
      )
    <*> Opt.optional
      ( Opt.option Opt.auto $
          Opt.long "zap"
            <> Opt.metavar "N"
            <> Opt.help "Generate corrupted (zapped) CBOR with N mistakes"
      )
    <*> Opt.optional
      ( Opt.option Opt.auto $
          Opt.long "seed"
            <> Opt.metavar "SEED"
            <> Opt.help "Fixed random seed for reproducibility"
      )

generateCBORMain :: Cuddle.Huddle -> IO ()
generateCBORMain huddle = do
  opts <-
    Opt.execParser $
      Opt.info
        (optsParser <**> Opt.helper)
        ( Opt.fullDesc
            <> Opt.progDesc "Generate CBOR data from Cardano Ledger CDDL rules"
            <> Opt.header "generate-cbor - CBOR data generator from Cardano Ledger CDDL specifications"
        )
  case resolveHuddle huddle of
    Left err -> die $ "Failed to resolve CDDL: " <> err
    Right root -> do
      let env = HuddleEnv {heTwiddle = True, heRoot = root}
          multipleRules = length (gcboRuleNames opts) > 1
      forM_ (gcboRuleNames opts) $ \ruleName -> do
        when multipleRules $
          hPutStrLn stderr $
            "# " <> T.unpack ruleName
        emitSample opts env ruleName

emitSample :: GenerateCBOROpts -> HuddleEnv -> T.Text -> IO ()
emitSample opts env ruleName = do
  let cborGen = runCBORGen (toGenConfig env) (generateFromName (Name ruleName))
      gen = zapAntiGenResult (fromMaybe 0 (gcboZap opts)) cborGen
  result <- case gcboSeed opts of
    Nothing -> generate gen
    Just seed -> pure $ unGen gen (mkQCGen seed) 30
  writeSample opts env ruleName result

writeSample :: GenerateCBOROpts -> HuddleEnv -> T.Text -> ZapResult CBOR.Term -> IO ()
writeSample opts env ruleName ZapResult {zrValue, zrZapped}
  | isJust (gcboZap opts) && zrZapped == 0 = warn "produced no corruptions"
  | isJust (gcboZap opts) && isValid validation =
      warn "produced a value that is still valid"
  | otherwise = BS8.putStrLn (Base16.encode bs)
  where
    bs = CBOR.toStrictByteString (CBOR.encodeTerm zrValue)
    validation = validateCBOR bs (Name ruleName) (mapIndex (heRoot env))
    warn reason =
      hPutStrLn stderr $
        "Warning: zap " <> reason <> " for rule " <> T.unpack ruleName
