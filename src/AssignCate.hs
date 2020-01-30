-- -# LANGUAGE OverloadedStrings #-

-- Copyright China University of Water Resources and Electric Power (c) 2019-2020
-- All rights reserved.

module AssignCate (
    getConn,            -- IO Connection
    psToCate,           -- IO()
    POS,                -- String
    pos,                -- [POS]
    CateSymb,           -- String
    posCate,            -- [(POS, CateSymb)]
    psToCateInASent,    -- String -> String
    getCateSymbFromPos, -- POS -> [(POS, CateSymb)] -> CateSymb
    copyPS,             -- IO()
    copyCate,           -- IO()

    ) where

import Control.Monad
import Database.HDBC
import Database.HDBC.MySQL
import Data.List.Utils

-- Datetype POS for parts of speech (word classes).
type POS = String

-- Datetype CateSymb for categorial symbols.
type CateSymb = String

-- Parts of speech (word classes) according to GBT20532-2006.
pos :: [POS]
pos = ["n","ng","nt","nd","nl","nh","ns","nn","ni","nz",
       "v","vt","vi","vl","vu","vd",
       "a","aq","as",
       "f",
       "m",
       "q",
       "r",
       "d",
       "p",
       "c",
       "u",
       "e",
       "o",
       "i","in","iv","ia","ic",
       "j","jn","jv","ja",
       "h",
       "k",
       "g","gn","gv","ga",
       "x",
       "w","wp","ws","wu"]

-- Assignments from a part of speech to its syntactic category(ies).
posCate :: [(POS, CateSymb)]
posCate = [("n","np"),
           ("ng","np"),
           ("nt","(s\\.np)/#(s\\.np)|s/*s"),
           ("nd","np\\*np|np\\*(s\\.np)"),
           ("nl","np"),
           ("nh","np"),
           ("ns","np"),
           ("nn","np"),
           ("ni","np"),
           ("nz","np"),
           ("v","s\\.np|(s\\.np)/.np|((s\\.np)/.np)/.np"),
           ("vt","(s\\.np)/.np|((s\\.np)/.np)/.np"),
           ("vi","s\\.np"),
           ("vl","(s\\.np)/.np"),
           ("vu","(s\\.np)/#(s\\.np)"),
           ("vd","(s\\.np)\\x(s\\.np)"),
           ("a","np/.np"),
           ("aq","np/.np|(s\\.np)\\x(s\\.np)"),
           ("as","np/.np"),
           ("f","np/*np"),
           ("m","s/*s"),
           ("q","np\\*(s/*s)|(np/*np)\\*(s/*s)|((s\\.np)\\x(s\\.np))\\*(s/*s)"),
           ("r","np"),
           ("d","(s\\.np)/#(s\\.np)|(np/.np)/*(np/.np)"),
           ("p","((s\\.np)/#(s\\.np))/*np|((s\\.np)\\x(s\\.np))/*np|(s/*s)/*np"),
           ("c","(X\\*X)/*X"),
           ("u","(np/*np)\\*np|(np/*np)\\*(np/.np)|(np/*np)\\*(s\\.np)|((s\\.np)/#(s\\.np))\\*(np/.np)|((s\\.np)\\x(s\\.np))/*(np/.np)"),
           ("e","np|(s\\.np)/#(s\\.np)"),
           ("o","np|(s\\.np)/#(s\\.np)"),
           ("i","np|s\\.np|np/.np|s/*s"),
           ("in","np"),
           ("iv","s\\.np"),
           ("ia","np/.np"),
           ("ic","s/*s"),
           ("j","np|s\\.np|(s\\.np)/.np|np/.np"),
           ("jn","np"),
           ("jv","s\\.np|(s\\.np)/.np"),
           ("ja","np/.np"),
           ("h","np/*np"),
           ("k","np\\*np"),
           ("g","np|s\\.np|(s\\.np)/.np|np/.np"),
           ("gn","np"),
           ("gv","s\\.np|(s\\.np)/.np"),
           ("ga","np/.np"),
           ("x",""),
           ("w",""),
           ("wp","(X/*X)\\*X"),
           ("ws","np"),
           ("wu","")]

-- Get a connection with given database.
getConn :: IO Connection
getConn =  connectMySQL defaultMySQLConnectInfo {
    mysqlHost = "127.0.0.1",
    mysqlUser = "hssc",
    mysqlPassword = "hssc",
    mysqlDatabase = "ccg4c"
    }

-- Initialize the column cate_sent by the column raw_sent.
-- Actually translate each part of speech to its category.
psToCate :: IO()
psToCate = do
    conn <- getConn
    sth <- prepare conn "select raw_sent2,serial_num from raw_corpus"
    executeRaw sth
    rows <- fetchAllRows sth         --Get [[<raw_sent2>, <serial_num>]]
                                     --Select's result must be used.
    putStrLn $ (show $ length rows) ++ " rows has been read."
    putStrLn $ "Data length of cate_sent is " ++ (show $ maxLength $ map (fromSql.head) $ rawToCate rows)

--  colNames <- getColumnNames sth      -- Get names of columns from results.
--  forM_ colNames $ \colName -> (putStrLn $ show colName)

    sth <- prepare conn "update raw_corpus set cate_sent = ? where serial_num = ?"
    executeMany sth $ rawToCate rows    -- Update column cate_sent. 
    commit conn                 -- Commit any pending data to the database.

    putStrLn "Category assignments are finished."

--  sth <- prepare conn "select serial_num,raw_sent,cate_sent from raw_corpus"
--  executeRaw sth
--  rows <- fetchAllRows sth            -- Get [[<serial_num>, <cate_sent>]].
--  forM_ rows $ \row -> (forM_ row $ \fld -> putStrLn $ fromSql fld)
--  forM_ (last rows) $ \fld -> putStrLn $ fromSql fld
    disconnect conn             -- Explicitly close the connection.

-- Calculate the maximum data length of column cate_sent.
maxLength :: [String] -> Int
maxLength [] = 0
maxLength cs = foldr max 0 (map length cs)

-- Prepare [[<cate_sent>, <serial_num>]] from [[<raw_sent>,<serial_num>]]
rawToCate :: [[SqlValue]] -> [[SqlValue]]
rawToCate [] = [] 
rawToCate (row:rows) = ([toSql $ psToCateInASent $ fromSql $ head row]++[last row]):rawToCate rows

-- Translate <word>/<pos> into <word>:<cate> in a String, 
-- here <word> and <pos> are concrete word and its part of speech.
psToCateInASent :: String -> String
psToCateInASent [] = []
psToCateInASent xs = unwords $ map (\w -> (head $ split "/" w) ++ ":" ++ getCateSymbFromPos (last $ split "/" w) posCate) (words xs)
    
-- Get the category symbol from a part of speech, according to the list posCate.
getCateSymbFromPos :: POS -> [(POS, CateSymb)] -> CateSymb
getCateSymbFromPos [] _ = []
getCateSymbFromPos _ [] = []
getCateSymbFromPos pos (c:cs)      --Here, (c:cs) is just the list posCate
    | pos == fst c = snd c
    | otherwise = getCateSymbFromPos pos cs

-- Keep column cate_sent not changed, while column cate_sent2 modified manually.
-- The initial values of column cate_sent2 are copied from column cate_sent.
-- Actually cate_sent2 can be copied again from cate_sent where cate_check = 0.
-- In other words, cate_check will be set 1 after cate_sent is checked by hand.
copyCate :: IO()
copyCate = do
    conn <- getConn
    sth <- prepare conn "select cate_sent,serial_num from raw_corpus where cate_check = 0"
    executeRaw sth
    rows <- fetchAllRows sth         --Get [[<cate_sent>, <serial_num>]]
                                     --Select's result must be used.
    putStrLn $ (show $ length rows) ++ " rows has been read."

    sth <- prepare conn "update raw_corpus set cate_sent2 = ? where serial_num = ?"
    executeMany sth rows    -- Update column cate_sent2 whose cate_check = 0. 
    commit conn             -- Commit any pending data to the database.
    disconnect conn         -- Close the connection.
 
-- Keep column raw_sent not changed, while column raw_sent2 modified manually.
-- The initial values of column raw_sent2 are copied from column raw_sent.
-- Actually raw_sent2 can be copied again from raw_sent where ps_check = 0.
-- In other words, ps_check will be set 1 after raw_sent is checked by hand.
copyPS :: IO()
copyPS = do
    conn <- getConn
    sth <- prepare conn "select raw_sent,serial_num from raw_corpus where ps_check = 0"
    executeRaw sth
    rows <- fetchAllRows sth         --Get [[<raw_sent>, <serial_num>]]
                                     --Select's result must be used.
    putStrLn $ (show $ length rows) ++ " rows has been read."

    sth <- prepare conn "update raw_corpus set raw_sent2 = ? where serial_num = ?"
    executeMany sth rows    -- Update column cate_sent2 whose ps_check = 0. 
    commit conn             -- Commit any pending data to the database.
    disconnect conn         -- Close the connection.
