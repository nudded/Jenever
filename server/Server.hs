module Main where

import Control.Monad.State
import Control.Concurrent
import System.IO
import Network
import Jenever
import System.Directory
import Data.Binary

restore :: IO Jenever
restore = do exists <- doesFileExist "jenever.previous"
             if exists then decodeFile "jenever.previous"
                       else return newJenever

store :: Jenever -> IO ()
store jenever = encodeFile "jenever.current" jenever

main :: IO ()
main = do copyFile "jenever.current" "jenever.previous"
          socket <- listenOn (PortNumber 2625)
          jenever <- restore
          count <- newMVar jenever
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
                                 store j
                                 hPutStrLn handle r
        hClose handle
