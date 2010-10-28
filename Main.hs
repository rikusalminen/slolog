module Main where

import System.Environment (getArgs)
import qualified Data.Map as Map
import Control.Monad

import Text.Parsec

import SloLog.Database
import SloLog.Query
import SloLog.Parser
import SloLog.Pretty

handle :: Database -> Either Query Clause -> IO Database
handle db (Left query) = do
    mapM putStrLn [pprint $ instantiateQ sub query | sub <- qeval db query [Map.empty]]
    return db
handle db (Right clause) = return $ db ++ [clause]

handleLine db line =
    case parse statement "" line of
        Left err -> do
            print err
            return db
        Right stmt ->
            handle db stmt

repl db = do
    line <- getLine
    if null line
        then return ()
        else do
            db <- handleLine db line
            repl db

doFiles args = 
    foldM doFile [] args >>= repl
    where
    doFile db file = do
        source <- readFile file
        case parse (many statement) file source of
            Left err -> do
                print err
                return db
            Right stmts -> do
                foldM handle db stmts


main = do
    args <- getArgs
    if null args then repl [] else doFiles args
