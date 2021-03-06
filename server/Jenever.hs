module Jenever
    ( Jenever
    , newJenever
    , parseCommand
    , applyCommand
    ) where

import qualified Data.Map as M
import qualified Data.ByteString.Lazy as B
import Control.Monad.State hiding (put, get)
import Data.Binary
import Control.Monad (liftM)
import Data.Char
import Text.ParserCombinators.Parsec hiding (State)
import Data.Digest.Pure.SHA
import Control.DeepSeq

data Jenever = Jenever { accounts :: M.Map String Int
                       , hashedPassword :: String 
                       } deriving (Show, Eq, Ord)

newJenever :: Jenever
newJenever = Jenever { accounts = M.empty
                     , hashedPassword = hashPassword "gillessuckt"
                     }

hashPassword :: String -> String
hashPassword = showDigest . sha1 . B.pack . c2w
  where
    c2w = map (fromIntegral . fromEnum)

instance Binary Jenever where
    get = do accounts' <- liftM M.fromAscList get
             hashedPassword' <- get
             return Jenever { accounts = accounts'
                            , hashedPassword = hashedPassword'
                            }
    put jenever = do put (M.toAscList $ accounts jenever)
                     put (hashedPassword jenever)

instance NFData Jenever where
    rnf jenever =         M.fold deepseq () (accounts jenever)
                `deepseq` hashedPassword jenever
                `deepseq` ()

data Command = Drink String Int
             | Clean String Int String
             | Status String
             | Table
             | Password String String
             deriving (Show, Eq)

applyCommand :: Command -> State Jenever String
applyCommand = State . applyCommand'

applyCommand' :: Command -> Jenever -> (String, Jenever)
-- Drink command: add to account
applyCommand' (Drink userName quantity) jenever =
    let accounts' = M.insertWith (+) userName quantity (accounts jenever)
    in ("OK", jenever {accounts = accounts'})
-- Status command: show user status.
applyCommand' (Status userName) jenever =
    let account = M.lookup userName (accounts jenever)
    in case account of Nothing -> ("FAIL", jenever)
                       (Just quantity) -> (show quantity, jenever)
-- Clean command: clean drinks.
applyCommand' (Clean userName quantity password) jenever =
    let hash = hashPassword password
        result = M.lookup userName (accounts jenever)
        accounts' original = let newQuantity = original - quantity
                             in M.insert userName newQuantity (accounts jenever)
    in case result of
        Nothing -> ("FAIL", jenever)
        (Just q) -> if hash == hashedPassword jenever
            then ("OK", jenever {accounts = accounts' q})
            else ("FAIL", jenever)
-- Table command: print all users
applyCommand' Table jenever =
    let accounts' = M.toAscList (accounts jenever)
        showAccount userName quantity = userName ++ ": " ++ show quantity
    in (unlines $ map (uncurry showAccount) accounts', jenever)
-- Password command: change password
applyCommand' (Password old new) jenever =
    let hash = hashPassword old
    in if hash == hashedPassword jenever
        then ("OK", jenever {hashedPassword = hashPassword new})
        else ("FAIL", jenever)

parseCommand :: String -> Maybe Command
parseCommand str = let result = runParser parseCommand' () "input" str
                   in case result of (Left _) -> Nothing
                                     (Right c) -> Just c

parseCommand' :: Parser Command
parseCommand' =   drink
              <|> clean
              <|> status
              <|> table
              <|> password
  where
    drink = do command "drink"
               userName <- identifier
               quantity <- number
               return $ Drink userName quantity

    clean = do command "clean"
               userName <- identifier
               quantity <- number
               password <- identifier
               return $ Clean userName quantity password

    status = do command "status"
                userName <- identifier
                return $ Status userName

    table = do command "table"
               return Table

    password = do command "password"
                  old <- identifier
                  new <- identifier
                  return $ Password old new

    command str = string str >> spaces
    identifier = do identifier' <- many1 alphaNum
                    spaces
                    return identifier'

    number = do number' <- many1 digit
                spaces
                return (read number' :: Int)
