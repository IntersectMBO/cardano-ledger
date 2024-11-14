{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ImplicitParams #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilyDependencies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module Test.ImpSpec.Internal where

import Control.DeepSeq (NFData)
import Control.Monad (void)
import Control.Monad.Reader (MonadReader (..), ReaderT (..), asks)
import Control.Monad.State.Strict (MonadState (..))
import Data.Kind (Type)
import Data.Maybe (fromMaybe)
import Data.Proxy (Proxy (..))
import Data.Text (Text)
import qualified Data.Text.Lazy as TL
import GHC.Stack (CallStack, HasCallStack, SrcLoc (..), getCallStack)
import Prettyprinter (
  Doc,
  Pretty (..),
  annotate,
  defaultLayoutOptions,
  hcat,
  indent,
  layoutPretty,
  line,
  vsep,
 )
import Prettyprinter.Render.Terminal (AnsiStyle, Color (..), color, renderLazy)
import System.Random (randomR, split)
import System.Random.Stateful (IOGenM, applyIOGen, newIOGenM)
import Test.HUnit.Lang (FailureReason (..), HUnitFailure (..))
import Test.Hspec (Spec, SpecWith, beforeAll, beforeAllWith)
import Test.Hspec.Core.Spec (
  Example (..),
  Result (..),
  paramsQuickCheckArgs,
 )
import qualified Test.Hspec.Core.Spec as H
import Test.ImpSpec.Expectations
import Test.ImpSpec.Random
import Test.QuickCheck (Arbitrary, Args (chatty, replay), Testable (..), counterexample, ioProperty)
import Test.QuickCheck.Gen (Gen (..))
import Test.QuickCheck.GenT (MonadGen (..))
import Test.QuickCheck.Random (QCGen (..), integerVariant, mkQCGen)
import UnliftIO (MonadIO (liftIO), MonadUnliftIO (..))
import UnliftIO.Exception (
  Exception (..),
  SomeException (..),
  catchAny,
  catchAnyDeep,
  throwIO,
 )
import UnliftIO.IORef

data ImpState t = ImpState
  { impStateSpecState :: !(ImpSpecState t)
  , impStateLog :: !(Doc AnsiStyle)
  }

data ImpEnv t = ImpEnv
  { impEnvSpecEnv :: !(ImpSpecEnv t)
  , impEnvStateRef :: !(IORef (ImpState t))
  , impEnvQCGenRef :: !(IOGenM QCGen)
  , impEnvQCSize :: !Int
  }

class ImpSpec t where
  type ImpSpecEnv t = (r :: Type) | r -> t
  type ImpSpecEnv t = Proxy t
  type ImpSpecState t = (r :: Type) | r -> t
  type ImpSpecState t = Proxy t

  impInitIO :: QCGen -> IO (ImpInit t)
  default impInitIO :: (ImpSpecEnv t ~ Proxy t, ImpSpecState t ~ Proxy t) => QCGen -> IO (ImpInit t)
  impInitIO _ = pure $ ImpInit Proxy Proxy

  -- | This will be the very first action that will run in all `ImpM` specs.
  impPrepAction :: ImpM t ()
  impPrepAction = pure ()

data ImpInit t = ImpInit
  { impInitEnv :: ImpSpecEnv t
  , impInitState :: ImpSpecState t
  }
deriving instance (Eq (ImpSpecEnv t), Eq (ImpSpecState t)) => Eq (ImpInit t)
deriving instance (Ord (ImpSpecEnv t), Ord (ImpSpecState t)) => Ord (ImpInit t)
deriving instance (Show (ImpSpecEnv t), Show (ImpSpecState t)) => Show (ImpInit t)

-- | Stores extra information about the failure of the unit test
data ImpException = ImpException
  { ieAnnotation :: [Doc AnsiStyle]
  -- ^ Description of the IO action that caused the failure
  , ieThrownException :: SomeException
  -- ^ Exception that caused the test to fail
  }
  deriving (Show)

instance Exception ImpException where
  displayException = ansiDocToString . prettyImpException

prettyImpException :: ImpException -> Doc AnsiStyle
prettyImpException (ImpException ann e) =
  vsep $
    mconcat
      [ ["Annotations:"]
      , zipWith indent [0, 2 ..] ann
      , ["Failed with Exception:", indent 4 $ pretty (displayException e)]
      ]

newtype ImpM t a = ImpM {unImpM :: ReaderT (ImpEnv t) IO a}
  deriving
    ( Functor
    , Applicative
    , Monad
    , MonadIO
    , MonadUnliftIO
    )

instance env ~ ImpSpecEnv t => MonadReader env (ImpM t) where
  ask = impEnvSpecEnv <$> ImpM ask
  local f = ImpM . local (\e -> e {impEnvSpecEnv = f (impEnvSpecEnv e)}) . unImpM

instance MonadFail (ImpM t) where
  fail = liftIO . assertFailure

instance s ~ ImpSpecState t => MonadState s (ImpM t) where
  state f = do
    ImpEnv {impEnvStateRef} <- ImpM ask
    curState <- readIORef impEnvStateRef
    let !(result, !newSpecState) = f $ impStateSpecState curState
    writeIORef impEnvStateRef (curState {impStateSpecState = newSpecState})
    pure result
  get = fmap impStateSpecState . readIORef . impEnvStateRef =<< ImpM ask

instance MonadGen (ImpM t) where
  liftGen (MkGen f) = do
    qcSize <- ImpM $ asks impEnvQCSize
    qcGen <- applyQCGen split
    pure $ f qcGen qcSize
  variant n action = do
    applyQCGen $ \qcGen -> ((), integerVariant (toInteger n) qcGen)
    action
  sized f = ImpM (asks impEnvQCSize) >>= f
  resize n (ImpM f) = ImpM $ local (\env -> env {impEnvQCSize = n}) f
  choose r = applyQCGen (randomR r)

instance HasStatefulGen (IOGenM QCGen) (ImpM t) where
  askStatefulGen = ImpM $ asks impEnvQCGenRef

instance (ImpSpec t, Testable a) => Testable (ImpM t a) where
  property m = property $ MkGen $ \qcGen qcSize ->
    ioProperty $ do
      let (qcGen1, qcGen2) = split qcGen
      impInit <- impInitIO qcGen1
      evalImpM (Just qcGen2) (Just qcSize) impInit m

instance (ImpSpec t, Testable p) => Example (ImpM t p) where
  type Arg (ImpM t p) = ImpInit t

  evaluateExample impTest = evaluateExample (\() -> impTest)

instance (Arbitrary a, Show a, ImpSpec t, Testable p) => Example (a -> ImpM t p) where
  type Arg (a -> ImpM t p) = ImpInit t

  evaluateExample impTest params hook progressCallback = do
    let runImpExample impInit = property $ \x -> do
          let args = paramsQuickCheckArgs params
              mQC = replay (paramsQuickCheckArgs params)

          (r, testable, logs) <- evalImpM (fst <$> mQC) (snd <$> mQC) impInit $ do
            t <- impTest x
            qcSize <- ImpM $ asks impEnvQCSize
            qcGen <- applyQCGen split
            logs <- getLogs
            pure (Just (qcGen, qcSize), t, logs)
          let params' = params {paramsQuickCheckArgs = args {replay = r, chatty = False}}
          res <-
            evaluateExample
              (counterexample (ansiDocToString logs) testable)
              params'
              (\f -> hook (\_st -> f ()))
              progressCallback
          void $ throwIO $ resultStatus res
    evaluateExample runImpExample params hook progressCallback

applyQCGen :: (QCGen -> (b, QCGen)) -> ImpM t b
applyQCGen f = do
  qcGenRef <- ImpM $ asks impEnvQCGenRef
  applyIOGen f qcGenRef

getLogs :: ImpM t (Doc AnsiStyle)
getLogs = do
  ref <- ImpM $ asks impEnvStateRef
  impStateLog <$> readIORef ref

modifyLogs :: (Doc AnsiStyle -> Doc AnsiStyle) -> ImpM t ()
modifyLogs f = do
  ref <- ImpM $ asks impEnvStateRef
  modifyIORef ref $ \s -> s {impStateLog = f (impStateLog s)}

-- | Override the QuickCheck generator using a fixed seed.
impSetSeed :: Int -> ImpM t ()
impSetSeed seed = applyQCGen $ \_ -> ((), mkQCGen seed)

evalImpGenM :: ImpSpec t => ImpInit t -> ImpM t b -> Gen (IO b)
evalImpGenM impInit = fmap (fmap fst) . runImpGenM impInit

evalImpM :: ImpSpec t => Maybe QCGen -> Maybe Int -> ImpInit t -> ImpM t b -> IO b
evalImpM mQCGen mQCSize impInit = fmap fst . runImpM mQCGen mQCSize impInit

execImpGenM :: ImpSpec t => ImpInit t -> ImpM t b -> Gen (IO (ImpState t))
execImpGenM impInit = fmap (fmap snd) . runImpGenM impInit

execImpM ::
  ImpSpec t =>
  Maybe QCGen ->
  Maybe Int ->
  ImpInit t ->
  ImpM t b ->
  IO (ImpState t)
execImpM mQCGen mQCSize impInit = fmap snd . runImpM mQCGen mQCSize impInit

runImpGenM_ :: ImpSpec t => ImpInit t -> ImpM t b -> Gen (IO ())
runImpGenM_ impInit = fmap void . runImpGenM impInit

runImpM_ :: ImpSpec t => Maybe QCGen -> Maybe Int -> ImpInit t -> ImpM t b -> IO ()
runImpM_ mQCGen mQCSize impInit = void . runImpM mQCGen mQCSize impInit

runImpGenM :: ImpSpec t => ImpInit t -> ImpM t b -> Gen (IO (b, ImpState t))
runImpGenM impInit m =
  MkGen $ \qcGen qcSize -> runImpM (Just qcGen) (Just qcSize) impInit m

runImpM ::
  ImpSpec t =>
  Maybe QCGen ->
  Maybe Int ->
  ImpInit t ->
  ImpM t b ->
  IO (b, ImpState t)
runImpM mQCGen mQCSize ImpInit {impInitEnv, impInitState} action = do
  let qcSize = fromMaybe 30 mQCSize
      qcGen = fromMaybe (mkQCGen 2024) mQCGen
  ioRef <-
    newIORef $
      ImpState
        { impStateSpecState = impInitState
        , impStateLog = mempty
        }
  qcGenRef <- newIOGenM qcGen
  let
    env =
      ImpEnv
        { impEnvSpecEnv = impInitEnv
        , impEnvStateRef = ioRef
        , impEnvQCGenRef = qcGenRef
        , impEnvQCSize = qcSize
        }
  res <-
    runReaderT (unImpM (impPrepAction >> action)) env `catchAny` \exc -> do
      logs <- impStateLog <$> readIORef ioRef
      let x <?> my = case my of
            Nothing -> x
            Just y -> x ++ [pretty y]
          uncaughtException header excThrown =
            H.ColorizedReason $
              ansiDocToString $
                vsep $
                  header ++ [pretty $ "Uncaught Exception: " <> displayException excThrown]
          fromHUnitFailure header (HUnitFailure mSrcLoc failReason) =
            case failReason of
              Reason msg ->
                H.Failure (srcLocToLocation <$> mSrcLoc) $
                  H.ColorizedReason $
                    ansiDocToString $
                      vsep $
                        header ++ [annotate (color Red) (pretty msg)]
              ExpectedButGot mMsg expected got ->
                H.Failure (srcLocToLocation <$> mSrcLoc) $
                  H.ExpectedButGot (Just (ansiDocToString $ vsep (header <?> mMsg))) expected got
          adjustFailureReason header = \case
            H.Failure mLoc failureReason ->
              H.Failure mLoc $
                case failureReason of
                  H.NoReason ->
                    H.ColorizedReason $ ansiDocToString $ vsep $ header ++ [annotate (color Red) "NoReason"]
                  H.Reason msg ->
                    H.ColorizedReason $ ansiDocToString $ vsep $ header ++ [annotate (color Red) (pretty msg)]
                  H.ColorizedReason msg ->
                    H.ColorizedReason $ ansiDocToString $ vsep $ header ++ [pretty msg]
                  H.ExpectedButGot mPreface expected actual ->
                    H.ExpectedButGot (Just (ansiDocToString $ vsep (header <?> mPreface))) expected actual
                  H.Error mInfo excThrown -> uncaughtException (header <?> mInfo) excThrown
            result -> result
          newExc
            | Just hUnitExc <- fromException exc = fromHUnitFailure [logs] hUnitExc
            | Just hspecFailure <- fromException exc = adjustFailureReason [logs] hspecFailure
            | Just (ImpException ann excThrown) <- fromException exc =
                let annLen = length ann
                    header =
                      logs
                        : [ let prefix
                                  | annLen <= 1 = "╺╸"
                                  | n <= 0 = "┏╸"
                                  | n + 1 == annLen = indent (n - 1) "┗━╸"
                                  | otherwise = indent (n - 1) "┗┳╸"
                             in annotate (color Red) prefix <> annotate (color Yellow) a
                          | (n, a) <- zip [0 ..] ann
                          ]
                        ++ [""]
                 in case fromException excThrown of
                      Just hUnitExc -> fromHUnitFailure header hUnitExc
                      Nothing ->
                        case fromException excThrown of
                          Just hspecFailure -> adjustFailureReason header hspecFailure
                          Nothing -> H.Failure Nothing $ uncaughtException header excThrown
            | otherwise = H.Failure Nothing $ uncaughtException [logs] exc
      throwIO newExc
  endState <- readIORef ioRef
  pure (res, endState)

ansiDocToString :: Doc AnsiStyle -> String
ansiDocToString = TL.unpack . renderLazy . layoutPretty defaultLayoutOptions

withImpInit :: ImpSpec t => SpecWith (ImpInit t) -> Spec
withImpInit = beforeAll (impInitIO (mkQCGen 2024))

modifyImpInit :: (ImpInit t -> ImpInit t) -> SpecWith (ImpInit t) -> SpecWith (ImpInit t)
modifyImpInit f = beforeAllWith (pure . f)

-- | Annotation for when failure happens. All the logging done within annotation will be
-- discarded if there no failures within the annotation.
impAnn :: NFData a => String -> ImpM t a -> ImpM t a
impAnn msg = impAnnDoc (pretty msg)

impAnnDoc :: NFData a => Doc AnsiStyle -> ImpM t a -> ImpM t a
impAnnDoc msg m = do
  logs <- getLogs
  res <- catchAnyDeep m $ \exc ->
    throwIO $
      case fromException exc of
        Just (ImpException ann origExc) -> ImpException (msg : ann) origExc
        Nothing -> ImpException [msg] exc
  modifyLogs (const logs)
  pure res

-- | Adds a source location and Doc to the log, which are only shown if the test fails
logWithCallStack :: CallStack -> Doc AnsiStyle -> ImpM t ()
logWithCallStack callStack entry =
  modifyLogs (<> stack <> line <> indent 2 entry <> line)
  where
    prettySrcLoc' SrcLoc {srcLocModule, srcLocStartLine} =
      hcat
        [ annotate (color c) d
        | (c, d) <-
            [ (Yellow, "[")
            , (Blue, pretty srcLocModule)
            , (Yellow, ":")
            , (Magenta, pretty srcLocStartLine)
            , (Yellow, "]")
            ]
        ]
    prefix n = if n <= 0 then "" else indent (n - 1) "└"
    stack =
      vsep
        [prefix n <> prettySrcLoc' loc | (n, (_, loc)) <- zip [0, 2 ..] . reverse $ getCallStack callStack]

-- | Adds a Doc to the log, which is only shown if the test fails
logDoc :: HasCallStack => Doc AnsiStyle -> ImpM t ()
logDoc = logWithCallStack ?callStack

-- | Adds a Text to the log, which is only shown if the test fails
logText :: HasCallStack => Text -> ImpM t ()
logText = logWithCallStack ?callStack . pretty

-- | Adds a String to the log, which is only shown if the test fails
logString :: HasCallStack => String -> ImpM t ()
logString = logWithCallStack ?callStack . pretty
