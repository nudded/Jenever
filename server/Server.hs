module Main where

import Control.Monad.State
import Control.Concurrent
import Control.Applicative ((<$>))
import System.IO
import Network
import System.Directory
import System.FilePath ((</>))

import Control.DeepSeq
import Data.Binary

import Jenever

-- | Application directory to store dumps.
appDirectory :: IO FilePath
appDirectory = getAppUserDataDirectory "jenever"

-- | Current dump file.
currentDump :: IO FilePath
currentDump = (</> "current") <$> appDirectory

-- | Previous dump file.
previousDump :: IO FilePath
previousDump = (</> "previous") <$> appDirectory

-- | Restore the Jenever instance from the last dump, or create a new empty
--   instance if there is no dump.
restore :: IO Jenever
restore = do currentDump' <- currentDump
             previousDump' <- previousDump
             exists <- doesFileExist currentDump'
             if exists then do copyFile currentDump' previousDump'
                               decodeFile previousDump'
                       else return newJenever

store :: Jenever -> IO ()
store jenever = do currentDump' <- currentDump
                   encodeFile currentDump' jenever

main :: IO ()
main = do -- Ensure the directories exist.
          createDirectoryIfMissing True =<< appDirectory
          socket <- listenOn (PortNumber 2625)
          jenever <- restore
          mvar <- newMVar jenever
          forever (listen mvar socket)
  where
    listen mvar socket = do
        (handle, _, _) <- accept socket
        forkIO $ respond mvar handle

    respond mvar handle = do
        message <- hGetLine handle
        let parsed = parseCommand message
        case parsed of
            Nothing -> hPutStrLn handle "FAIL"
            (Just command) -> do jenever <- takeMVar mvar
                                 let (r, j) = applyCommand command jenever
                                 deepseq j $ store j
                                 hPutStrLn handle r
                                 putMVar mvar j
        hClose handle
