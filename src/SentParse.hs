-- Copyright (c) 2019-2020 China University of Water Resources and Electric Power,
-- All rights reserved.

module SentParse (
    getSentFromDB,     -- Int -> IO String
    getSent,           -- String -> IO [String]
    parseClause,       -- String -> IO ()
    parseSent          -- [String] -> IO ()
    ) where

import Control.Monad
import Database.HDBC
import Database.HDBC.MySQL
import Data.List.Utils
import AssignCate
import Parse
import Output

-- Get a sentence from table raw_corpus. 
getSentFromDB :: Int -> IO String
getSentFromDB sn = do
    conn <- getConn
    sth <- prepare conn ("select cate_sent2 from raw_corpus where serial_num=" ++ show sn)
    executeRaw sth
    rows <- fetchAllRows sth
    return $ fromSql $ (rows!!0)!!0

-- Split a sentence into clauses.
getSent :: String -> IO [String]
getSent sent = return $ split " \65292:" sent    -- \65292 is Chinese comma.
        
-- Parse a clause.
parseClause :: String -> IO ()
parseClause clause = do
    let phraCateClosure1 = parse $ initPhraCate $ getNCate $ words clause
    let phraCateClosure2 = [pc | pc <- phraCateClosure1, caOfCate pc /= []]
    let phraCateClosure = atomizePhraCate phraCateClosure2
    let sp = getNuOfInputCates phraCateClosure - 1
    let roots = findCate (0, sp) phraCateClosure
    let forest = growForest [[t]|t<-roots] phraCateClosure
--  putStrLn "  Parsing Forest is as follows:"
--  showForest forest
--  putStr "\n"
    putStrLn "  Tree Structure is as follows:"
    showForestWithTreeStru forest

-- Parse a sentence.
parseSent :: [String] -> IO ()
parseSent [] = putStrLn "End of Sentence Parsing"
parseSent (c:cs) = do
    putStrLn $ "  Clause No." ++ show (length cs + 1)
    parseClause c
    parseSent cs




