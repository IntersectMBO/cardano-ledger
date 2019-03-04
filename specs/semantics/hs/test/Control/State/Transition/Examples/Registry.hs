-- | Copyright 2017-2019 QuviQ - Courtesy of John Hughes.
--
-- A simple local name service for threads... behaves like the Erlang
-- process registry.
--
-- If you are doing the associated QuickCheck exercises, DO NOT READ
-- THIS CODE!!! The exercise is one in black box testing.

module Control.State.Transition.Examples.Registry where

import Data.IORef
import Control.Monad
import Control.Concurrent
import System.IO.Unsafe
import GHC.Conc

alive :: ThreadId -> IO Bool
alive tid = do
  s <- threadStatus tid
  return $ s /= ThreadFinished && s /= ThreadDied

{-# NOINLINE registry #-}
registry :: IORef [(String,ThreadId)]
registry = unsafePerformIO (newIORef [])

whereis :: String -> IO (Maybe ThreadId)
whereis name = do
  reg <- readRegistry
  return $ lookup name reg

register :: String -> ThreadId -> IO ()
register name tid = do
  ok <- alive tid
  reg <- readRegistry
  if ok && name `notElem` map fst reg && tid `notElem` map snd reg
    then atomicModifyIORef registry $ \reg' ->
           if name `notElem` map fst reg' && tid `notElem` map snd reg'
             then ((name,tid):reg',())
             else (reg',badarg)
    else badarg

unregister :: String -> IO ()
unregister name = do
  reg <- readRegistry
  if name `elem` map fst reg
    then atomicModifyIORef registry $ \reg' ->
           (filter ((/=name).fst) reg',
            ())
    else badarg

readRegistry :: IO [(String, ThreadId)]
readRegistry = do
  reg <- readIORef registry
  garbage <- filterM (fmap not.alive) (map snd reg)
  atomicModifyIORef' registry $ \reg' ->
    let reg'' = filter ((`notElem` garbage).snd) reg' in (reg'',reg'')

badarg :: a
badarg = error "bad argument"
