{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeApplications #-}

module Main where

import CLI
import qualified Cardano.Ledger.Binary.Plain as Plain
import Cardano.Ledger.Crypto (Crypto, StandardCrypto)
import Cardano.Ledger.Plutus.CostModels (
  getCostModelLanguage,
  getCostModelParams,
  getEvaluationContext,
  mkCostModel,
 )
import Cardano.Ledger.Plutus.Evaluate
import Cardano.Ledger.Plutus.ExUnits (ExUnits (..))
import Cardano.Ledger.Plutus.Language (
  Plutus (..),
  PlutusBinary (..),
  PlutusLanguage (..),
 )
import Cardano.Ledger.Plutus.TxInfo
import Control.DeepSeq (force)
import Control.Exception (evaluate)
import Control.Monad (join)
import qualified Data.ByteString.Base16 as B16
import qualified Data.ByteString.Base64 as B64
import qualified Data.ByteString.Char8 as BSC
import qualified Data.ByteString.Short as SBS
import qualified Data.ByteString.UTF8 as BSU
import Data.Either (fromRight)
import Data.Maybe (fromMaybe)
import Options.Applicative
import PlutusLedgerApi.Common as P
import System.Timeout (timeout)

overrideContext :: PlutusWithContext c -> Opts -> PlutusWithContext c
overrideContext pwc@(PlutusWithContext {..}) Opts {..} =
  pwc
    { pwcProtocolVersion = fromMaybe pwcProtocolVersion optsProtocolVersion
    , pwcScript = overrideScript
    , pwcExUnits = overrideExUnits
    , pwcCostModel = overrideCostModel
    , -- TODO: Add support for overriding arguments.
      -- Also note that this is needed in order to make GHC happy.
      -- Due to the `PlutusLanguage l` constraint in `PlutusWithContext`
      -- where `l` is an existential, without the line below GHC won't be able to
      -- tell that the `l` before the record update is the same as the `l` after
      -- the record update.
      pwcArgs
    }
  where
    overrideExUnits =
      ExUnits
        (fromMaybe (exUnitsMem pwcExUnits) optsExUnitsMem)
        (fromMaybe (exUnitsSteps pwcExUnits) optsExUnitsSteps)
    overrideCostModel =
      fromRight pwcCostModel $
        mkCostModel
          (fromMaybe (getCostModelLanguage pwcCostModel) optsLanguage)
          (fromMaybe (getCostModelParams pwcCostModel) optsCostModelValues)
    overrideScript =
      case optsScript of
        Nothing -> pwcScript
        Just script ->
          either error (Left . Plutus . PlutusBinary . SBS.toShort) . B16.decode $ BSC.filter (/= '\n') script

debugPlutus :: Crypto c => Opts -> IO (PlutusDebugInfo c)
debugPlutus opts@Opts {..} =
  case B64.decode (BSU.fromString optsScriptWithContext) of
    Left e -> pure $ DebugBadHex (show e)
    Right bs ->
      case Plain.decodeFull' bs of
        Left e -> pure $ DebugCannotDecode $ show e
        Right pwcOriginal ->
          let pwc = overrideContext pwcOriginal opts
              cm = getEvaluationContext $ pwcCostModel pwc
              eu = transExUnits $ pwcExUnits pwc
              onDecoderError err = pure $ DebugFailure [] err pwc Nothing
           in withRunnablePlutusWithContext pwc onDecoderError $ \plutusRunnable args ->
                let toDebugInfo = \case
                      (logs, Left err@(P.CodecError {})) -> pure $ DebugFailure logs err pwc Nothing
                      (logs, Left err) -> do
                        mExpectedExUnits <-
                          timeout 5_000_000 $ do
                            let res =
                                  evaluatePlutusRunnableBudget (pwcProtocolVersion pwc) P.Verbose cm plutusRunnable args
                            case snd res of
                              Left {} -> pure Nothing
                              Right exUnits -> Just <$> evaluate (force exUnits)
                        pure $ DebugFailure logs err pwc (join mExpectedExUnits)
                      (logs, Right ex) -> pure $ DebugSuccess logs ex
                 in toDebugInfo $
                      evaluatePlutusRunnable (pwcProtocolVersion pwc) P.Verbose cm eu plutusRunnable args

main :: IO ()
main = do
  opts <-
    execParser $
      info
        (optsParser <* abortOption (ShowHelpText Nothing) (long "help"))
        ( header "plutus-debug - A Plutus script debugger"
            <> progDesc
              ( "The purpose of this tool is to troubleshoot failing Plutus scripts. "
                  <> "When you encounter a `PlutusFailure`, you can pass the `Base64-encoded script bytes` "
                  <> "to `plutus-debug` for debugging purposes and override various parts of the failed script "
                  <> "with the available command line options."
              )
            <> footer ""
        )
  debugPlutus @StandardCrypto opts >>= print
