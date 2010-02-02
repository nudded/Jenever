module Main where

import Control.Monad.State
import Control.Concurrent
import System.IO
import Network
import Jenever

main :: IO ()
main = do socket <- listenOn (PortNumber 2625)
          count <- newMVar newJenever
          forever (listen count socket)
  where
    listen mvar socket = do
        (handle, _, _) <- accept socket
        message <- hGetLine handle
        let parsed = parseCommand message
        case parsed of
            Nothing -> hPutStrLn handle "FAIL"
            (Just command) -> do jenever <- takeMVar mvar
                                 let (r, j) = applyCommand command jenever
                                 putMVar mvar j
                                 hPutStrLn handle r
        hClose handle
