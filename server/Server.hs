module Main where

import Control.Monad.State
import Control.Concurrent
import System.IO
import Network
import Jenever
import System.Directory
import Data.Binary
import Control.DeepSeq

restore :: IO Jenever
restore = do exists <- doesFileExist "jenever.previous"
             if exists then do copyFile "jenever.current" "jenever.previous"
                               decodeFile "jenever.previous"
                       else return newJenever

store :: Jenever -> IO ()
store jenever = encodeFile "jenever.current" jenever

main :: IO ()
main = do socket <- listenOn (PortNumber 2625)
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
