-- Copyright (c) 2019-2021 China University of Water Resources and Electric Power,
-- All rights reserved.
{-# LANGUAGE OverloadedStrings #-}

module Main (
    main      -- IO()
    ) where

import Control.Monad
import qualified System.IO.Streams as S
import System.IO
import Database.MySQL.Base
import Corpus
import SentParse
import Database

{- This program create syntactic and semantic parsing results for Chinese sentences. Please run MySQL Workbench or other similiar tools, connecting MySQL database 'ccg4c', and querying table 'corpus' for revising parts of speech as well as CCG syntactic types, and the MySQL server is running on a certain compute in Hua-shui campus. The program includes commands for parsing sentences and storing results in database 'ccg4c', as following.
    ?   Display this message.
    1   Get the raw part-of-speech marked sentence (namely column raw_sent) given by value of column 'serial_num'.
    2   Copy column 'raw_sent' to column 'raw_sent2' to be revised for a given row. After executing this command, part-of-speech of every word in this sentence should be revised, then set column 'pos_check' as 1. If the value of column 'pos_check' of this row is 0, this child command will be banned.
    3   Get the part-of-speech marks revised sentence (namely column raw_sent2) given by value of column 'serial_num'.
    4   Create categorial column 'cate_sent' from part-of-speech column 'raw_sent2' for a given row. Before executing this child command, parts of speech of all words in this sentence should be revised, and set column 'pos_check' as 1. If the value of column 'pos_check' of this row is 0, this child command will be banned.
    5   Copy categorial column 'cate_sent' to column 'cate_sent2' to be revised for a given row. After executing this command, CCG-syntactic types of every words in this sentence should be revised, then set column 'cate_check' as 1.
    6   Get the CCG-marked sentence (namely column cate_sent) given by value of column 'serial_num'.
    7   Get the CCG mark-revised sentence (namely column cate_sent2) given by value of column 'serial_num'.
    8   Parse the sentence given by value of column 'serial_num'.
    9   Display parsing result of the sentence given by value of column 'serial_num'.
    10  Quit from this program.
 -}
main :: IO ()
main = do
    hSetBuffering stdout NoBuffering               -- Set buffering mode for stdout as no buffering.
    hSetBuffering stdin LineBuffering              -- Set buffering mode for stdin as line buffering.
    hSetEncoding stdout utf8                       -- Set encoding for stdout as UTF8
    hFlush stdout                                  -- Flush buffered data to assure changing the encoding.

    putStrLn " Chinese CCG parser (build 0.2.4.0)"
    putStrLn " Copyright (c) 2019-2021 China University of Water Resources and Electric Power, All rights reserved."

    (username, ok) <- login 1
    if ok
      then interpreter username
      else return ()

-- The n'th User authentication, returns user name and its authentication result.
login :: Int -> IO (String, Bool)
login 4 = do
    putStrLn " Login failed! input attempts have reached maximal times."
    return ("", False)
login n = do
    putStr " Login: "
    username <- getLine
    putStr " Password: "
    password <- getLine

    conn <- getConnByUserWqj
    stmt <- prepareStmt conn "select * from user where name = ? && password = ?"
    (defs, is) <- queryStmt conn stmt [toMySQLText username, toMySQLText password]   --([ColumnDef], InputStream [MySQLValue])
    row <- S.read is
    let row' = case row of
                 Just x -> x
                 Nothing -> []
    skipToEof is
    if row' == []
      then do
        putStrLn " User or password is error."
        login (n + 1)
      else do
        putStrLn $ " Welcome " ++ fromMySQLText (row'!!1) ++ " :)"    -- The second element is true name of this user.
        return (username, True)

-- Check whether current user is intellectual property creator (IPC) of the row indicated by serial_num.
checkIpc :: Int -> String -> IO Bool
checkIpc sn username = do
    conn <- getConn
    stmt <- prepareStmt conn "select ipc from corpus where serial_num = ? "
    (defs, is) <- queryStmt conn stmt [toMySQLInt32 sn]
    row <- S.read is
    let row' = case row of
                 Just x -> x
                 Nothing -> []
    skipToEof is
    if row' == []
      then do
        putStrLn "checkIpc: No this row."
        return False
      else do
        if (fromMySQLText (row'!!0) == username)
          then return True
          else return False

-- Command interpreter for given user.
interpreter :: String -> IO ()
interpreter username = do
    putStrLn " ? -> Display command list."
    putStrLn " 1 -> Get raw part-of-speech marked sentence indicated by serial_num."
    putStrLn " 2 -> Copy raw sentence indicated by serial_num to column raw_sent2"
    putStrLn " 3 -> Get the revised part-of-speech marked sentence indicated by serial_num."
    putStrLn " 4 -> Create CCG-marked sentence from its revised part-of-speech marks."
    putStrLn " 5 -> Get CCG-marked sentence indicated by serial_num."
    putStrLn " 6 -> Copy CCG-marked sentence indicated by serial_num to column cate_sent2."
    putStrLn " 7 -> Get revised CCG-marked sentence indicated by serial_num."
    putStrLn " 8 -> Parse the sentence indicated by serial_num."
    putStrLn " 9 -> Disply parsing Trees of the sentence indicated by serial_num."
    putStrLn " 0 -> doQuit"
    putStr "Please input command: "
    line <- getLine
    if notElem line ["?","1","2","3","4","5","6","7","8","9","0"]
      then do
             putStrLn "Invalid input."
             interpreter username
      else case line of
             "?" -> interpreter username
             "1" -> doGetRawSentForASent username
             "2" -> doCopyRawSentForASent username
             "3" -> doGetRawSent2ForASent username
             "4" -> doPosToCateForASent username
             "5" -> doGetCateSentForASent username
             "6" -> doCopyCateForASent username
             "7" -> doGetCateSent2ForASent username
             "8" -> doParseSent username
             "9" -> doDisplyTreesForASent username
             "0" -> doQuit

-- 1. Get raw part-of-speech marked sentence indicated by serial_num.
doGetRawSentForASent :: String -> IO ()
doGetRawSentForASent username = do
    putStr "Please input value of 'serial_num': "
    line <- getLine
    let sn = read line :: Int
    getRawSentForASent sn >>= putStrLn
    interpreter username

{- 2. Copy raw sentence indicated by serial_num to column 'raw_sent2', here 'username' MUST
   be the intellectual property creator (ipc) of the row indicated by serial_num.
 -}
doCopyRawSentForASent :: String -> IO ()
doCopyRawSentForASent username = do
    putStr "Please input value of 'serial_num': "
    line <- getLine
    let sn = read line :: Int

    ok <- checkIpc sn username
    if ok
      then do                          -- Pass authentication!
        conn <- getConn
        stmt <- prepareStmt conn "select pos_check from corpus where serial_num = ?"
        (defs, is) <- queryStmt conn stmt [toMySQLInt32 sn]            --([ColumnDef], InputStream [MySQLValue])
        pos_check <- S.read is
        let pos_check' = case pos_check of
                           Just x -> fromMySQLInt8 (head x)
                           Nothing -> error "doCopyRawSentForASent: No pos_check was read."
        skipToEof is                                                   -- Go to the end of the stream.
        if pos_check' == 1
          then do
             putStrLn "Creating column 'raw_sent2' failed because column 'pos_check' is 1."
             interpreter username
          else if pos_check' == 0
                 then do
                   copyRawSentForASent sn
                   interpreter username
                 else do
                   putStrLn "pos_check value is abnormal."
                   interpreter username
      else do
        putStrLn "Copying failed! you are not the intellectual property creator of this sentence."
        interpreter username

-- 3. Get the revised part-of-speech marked sentence indicated by serial_num.
doGetRawSent2ForASent :: String -> IO ()
doGetRawSent2ForASent username = do
    putStr "Please input value of 'serial_num': "
    line <- getLine
    let sn = read line :: Int
    getRawSent2ForASent sn >>= putStrLn
    interpreter username

{- 4. Create CCG-marked sentence from its revised part-of-speech marks, here 'username' MUST
   be the intellectual property creator (ipc) of the row indicated by serial_num.
 -}
doPosToCateForASent :: String -> IO ()
doPosToCateForASent username = do
    putStr "Please input value of 'serial_num': "
    line <- getLine
    let sn = read line :: Int

    ok <- checkIpc sn username
    if ok
      then do                          -- Pass authentication!
        conn <- getConn
        stmt <- prepareStmt conn "select pos_check from corpus where serial_num = ?"
        (defs, is) <- queryStmt conn stmt [toMySQLInt32 sn]            --([ColumnDef], InputStream [MySQLValue])
        pos_check <- S.read is
        let pos_check' = case pos_check of
                           Just x -> fromMySQLInt8 (head x)
                           Nothing -> error "doPosToCateForASent: No pos_check was read."
        skipToEof is                                                   -- Go to the end of the stream.
        if pos_check' == 0
          then do
             putStrLn "Creating column 'cate_sent' failed because column 'pos_check' is 0."
             interpreter username
          else if pos_check' == 1
                 then do
                   posToCateForASent sn
                   interpreter username
                 else do
                   putStrLn "pos_check value is abnormal."
                   interpreter username
      else do
        putStrLn "Creating CCG marks failed! you are not the intellectual property creator of this sentence."
        interpreter username

-- 5. Get CCG-marked sentence indicated by serial_num.
doGetCateSentForASent :: String -> IO ()
doGetCateSentForASent username = do
    putStr "Please input value of 'serial_num': "
    line <- getLine
    let sn = read line :: Int
    getCateSentForASent sn >>= putStrLn
    interpreter username

{- 6. Copy CCG-marked sentence indicated by serial_num to column cate_sent2, here 'username' MUST
   be the intellectual property creator (ipc) of the row indicated by serial_num.
 -}
doCopyCateForASent :: String -> IO ()
doCopyCateForASent username = do
    putStr "Please input value of 'serial_num': "
    line <- getLine
    let sn = read line :: Int

    ok <- checkIpc sn username
    if ok
      then do                          -- Pass authentication!
        conn <- getConn
        stmt <- prepareStmt conn "select cate_check from corpus where serial_num = ?"
        (defs, is) <- queryStmt conn stmt [toMySQLInt32 sn]            --([ColumnDef], InputStream [MySQLValue])
        cate_check <- S.read is
        let cate_check' = case cate_check of
                            Just x -> fromMySQLInt8 (head x)
                            Nothing -> error "doCopyCateForASent: No cate_check was read."
        skipToEof is                                                   -- Go to the end of the stream.
        if cate_check' == 1
          then do
             putStrLn "Creating column 'cate_sent2' failed because column 'cate_check' is 1."
             interpreter username
          else if cate_check' == 0
             then do
               copyCateForASent sn
               interpreter username
             else do
               putStrLn "cate_check value is abnormal."
               interpreter username
      else do
        putStrLn "Copying failed! you are not the intellectual property creator of this sentence."
        interpreter username

-- 7. Get revised CCG-marked sentence indicated by serial_num.".
doGetCateSent2ForASent :: String -> IO ()
doGetCateSent2ForASent username = do
    putStr "Please input value of 'serial_num': "
    line <- getLine
    let sn = read line :: Int
    getCateSent2ForASent sn >>= putStrLn
    interpreter username

{- 8. Parse the sentence indicated by serial_num, here 'username' MUST
   be the intellectual property creator (ipc) of the row indicated by serial_num.
 -}
doParseSent :: String -> IO ()
doParseSent username = do
    putStr "Please input value of 'serial_num': "
    line <- getLine
    let sn = read line :: Int

    ok <- checkIpc sn username
    if ok
      then do                          -- Pass authentication!
        conn <- getConn
        stmt <- prepareStmt conn "select cate_check from corpus where serial_num = ?"
        (defs, is) <- queryStmt conn stmt [toMySQLInt32 sn]            --([ColumnDef], InputStream [MySQLValue])
        cate_check <- S.read is
        let cate_check' = case cate_check of
                            Just x -> fromMySQLInt8 (head x)
                            Nothing -> error "doParseSent: No cate_check was read."
        skipToEof is                                                   -- Go to the end of the stream.
        if cate_check' == 0
          then do
             putStrLn "Parsing sentence failed because column 'cate_check' is 0."
             interpreter username
          else if cate_check' == 1
                 then do
                   getSentFromDB sn >>= getSent >>= parseSent sn
                   interpreter username
                 else do
                   putStrLn "cate_check value is abnormal."
                   interpreter username
      else do
        putStrLn "Parsing failed! you are not the intellectual property creator of this sentence."
        interpreter username

-- 9. Disply parsing Trees of the sentence indicated by serial_num.
doDisplyTreesForASent :: String -> IO ()
doDisplyTreesForASent username = do
    putStr "Please input value of 'serial_num': "
    line <- getLine
    let sn = read line :: Int
    readTree_String sn >>= sentToClauses >>= dispTree
    interpreter username

-- 10. Quit from this program.
doQuit :: IO ()
doQuit = putStrLn "Bye!"
