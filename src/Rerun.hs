{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Rerun where

import           Control.Exception
import           Control.Monad      (when)
import           Prelude
import           Safe               (lastMay)
import           System.Directory   (removeFile)
import           System.Environment
import           System.IO          (IOMode (ReadMode), withFile)
import           System.IO.Strict
import           Test.Hspec.Runner



-- | We wrap this so that we can stash HSPEC_FAILURES in a file (which
--   comes from HSPEC_FAILURES_FILE).

main clientSpec = do
  logPutStrLn <- getPrinter

  let
    logPrint :: Show a => a -> IO ()
    logPrint = logPutStrLn . show
    -- LAZY IO IS THE DEVIL
    safeReadFile :: FilePath -> IO (Maybe String)
    safeReadFile fs = logAndIgnore Nothing $ do
      f <- withFile fs ReadMode (\h -> hGetContents h >>= \x -> seq (lastMay x) (return x))
      logPrint ("saferead got", f)
      return (Just f)

    logAndIgnore def f  =   handle (\(e::IOException) -> logPrint ("saferead failed", e) >> return def) f

  stashFile <- lookupEnv "HSPEC_FAILURES_FILE"
  alreadySet <- lookupEnv "HSPEC_FAILURES"
  logPrint ("startup", stashFile, alreadySet)
  case (stashFile,alreadySet) of
    (Just stash,Just failures) -> do
      -- if there's no stash set, we might as well write it so
      -- we have a starting point. it might get overwritten later,
      -- that's fine
      logPrint ("writing stash optimistically", failures)
      writeFile stash failures
    (Just stash,Nothing) -> do
      logPrint ("reading", stash)
      f <- safeReadFile stash
      logPrint ("read", stash)
      case f of
        Nothing -> logPrint ("couldn't read stash", stash)
        -- this should read all of contents, thereby closing the
        -- filehandle. Lazy IO is the devil.
        Just contents ->  do
          when (contents /= "") $ do
            logPrint ("read stash, setting env", contents)
            setEnv "HSPEC_FAILURES" contents
    _ -> return ()
  clientSpec `finally` do
    logPrint "done with tests!"
    -- we don't remove the file until now, so that if ghcid gets
    -- interrupted, we still have the failures to run first.
    -- small race condition, obviously, but the worst case is that
    -- we run more tests than we should.
    case stashFile of
      Just stash -> logAndIgnore () $ removeFile stash
      Nothing -> return ()

    newSet <- lookupEnv "HSPEC_FAILURES"
    logPrint ("spec failures", newSet,stashFile,alreadySet)
    case (stashFile,alreadySet,newSet) of
      -- only care if there is a stash file, the var wasn't set before,
      -- and there is a new value: here, we write the file
      (Just stash,Nothing,Just newset) ->
        writeFile stash newset
      _ -> return ()

getPrinter :: IO (String -> IO ())
getPrinter = do
    debug <- lookupEnv "HSPEC_STACK_RERUN_DEBUG"
    return $ case debug of
      Nothing -> (\_ -> return ())
      Just _  -> putStrLn
