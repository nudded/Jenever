module Main where

import Control.Concurrent
import Control.Monad (forever)
import Control.Applicative ((<$>))
import System.IO
import Network
import System.Directory
import System.FilePath ((</>))
import System.Posix.Daemonize
import System.Posix
import System.Posix
import Data.Maybe (fromJust)

import Control.DeepSeq
import Data.Binary

import Jenever

-- | Application directory to store dumps.
dumpDirectory :: FilePath
dumpDirectory = "/var/lib/jenever"

-- | Current dump file.
currentDump :: FilePath
currentDump = dumpDirectory </> "current"

-- | Previous dump file.
previousDump :: FilePath
previousDump = dumpDirectory </> "previous"

-- | Restore the Jenever instance from the last dump, or create a new empty
--   instance if there is no dump.
restore :: IO Jenever
restore = do exists <- doesFileExist currentDump
             if exists then do copyFile currentDump previousDump
                               decodeFile previousDump
                       else return newJenever

store :: Jenever -> IO ()
store jenever = encodeFile currentDump jenever

runJenever :: Logger -> IO ()
runJenever _ = do -- Ensure the directories exist.
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

main :: IO ()
main = do createDirectoryIfMissing True dumpDirectory
          user <- userID <$> getUserEntryForName "daemon"
          group <- groupID <$> getGroupEntryForName "daemon"
          catch (setOwnerAndGroup dumpDirectory user group)
                (\e -> warn)
          serviced runJenever
  where
    warn = putStrLn "Setting permissions failed: are you running as root?"
