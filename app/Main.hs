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
    interpreter

-- Child command interpreter
interpreter :: IO ()
interpreter = do
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
    putStrLn " q -> doQuit"
    putStr "Please input command: "
    line <- getLine
    if notElem line ["?","1","2","3","4","5","6","7","8","9","q"]
      then do
             putStrLn "Input invalid."
             interpreter
      else case line of
             "?" -> interpreter
             "1" -> doGetRawSentForASent
             "2" -> doCopyRawSentForASent
             "3" -> doGetRawSent2ForASent
             "4" -> doPosToCateForASent
             "5" -> doGetCateSentForASent
             "6" -> doCopyCateForASent
             "7" -> doGetCateSent2ForASent
             "8" -> doParseSent
             "9" -> doDisplyTreesForASent
             "q" -> doQuit

-- 1. Get raw part-of-speech marked sentence indicated by serial_num.
doGetRawSentForASent :: IO ()
doGetRawSentForASent = do
    putStr "Please input value of 'serial_num': "
    line <- getLine
    let sn = read line :: Int
    getRawSentForASent sn >>= putStrLn
    interpreter

-- 2. Copy raw sentence indicated by serial_num to column 'raw_sent2'.
doCopyRawSentForASent :: IO ()
doCopyRawSentForASent = do
    putStr "Please input value of 'serial_num': "
    line <- getLine
    let sn = read line :: Int

    conn <- getConn
    stmt <- prepareStmt conn "select pos_check from corpus where serial_num = ?"
    (defs, is) <- queryStmt conn stmt [toMySQLInt32 sn]            --([ColumnDef], InputStream [MySQLValue])
    pos_check <- S.read is
    let pos_check' = case pos_check of
                       Just x -> fromMySQLInt8 (head x)
                       Nothing -> error "doCopyRawSentForASent: No pos_check was read."
    skipToEof is                                                   -- Go to the end of the stream.
    if pos_check' == 0
      then do
             putStrLn "Creating column 'raw_sent2' failed because column 'pos_check' is 0."
             interpreter
      else if pos_check' == 1
             then do
                    copyRawSentForASent sn
                    interpreter
             else do
                    putStrLn "pos_check value is abnormal."
                    interpreter

-- 3. Get the revised part-of-speech marked sentence indicated by serial_num.
doGetRawSent2ForASent :: IO ()
doGetRawSent2ForASent = do
    putStr "Please input value of 'serial_num': "
    line <- getLine
    let sn = read line :: Int
    getRawSent2ForASent sn >>= putStrLn
    interpreter

-- 4. Create CCG-marked sentence from its revised part-of-speech marks.
doPosToCateForASent :: IO ()
doPosToCateForASent = do
    putStr "Please input value of 'serial_num': "
    line <- getLine
    let sn = read line :: Int

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
             interpreter
      else if pos_check' == 1
             then do
                    posToCateForASent sn
                    interpreter
             else do
                    putStrLn "pos_check value is abnormal."
                    interpreter

-- 5. Get CCG-marked sentence indicated by serial_num.
doGetCateSentForASent :: IO ()
doGetCateSentForASent = do
    putStr "Please input value of 'serial_num': "
    line <- getLine
    let sn = read line :: Int
    getCateSentForASent sn >>= putStrLn
    interpreter

-- 6. Copy CCG-marked sentence indicated by serial_num to column cate_sent2.
doCopyCateForASent :: IO ()
doCopyCateForASent = do
    putStr "Please input value of 'serial_num': "
    line <- getLine
    let sn = read line :: Int

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
             interpreter
      else if cate_check' == 0
             then do
                    copyCateForASent sn
                    interpreter
             else do
                    putStrLn "cate_check value is abnormal."
                    interpreter

-- 7. Get revised CCG-marked sentence indicated by serial_num.".
doGetCateSent2ForASent :: IO ()
doGetCateSent2ForASent = do
    putStr "Please input value of 'serial_num': "
    line <- getLine
    let sn = read line :: Int
    getCateSent2ForASent sn >>= putStrLn
    interpreter

-- 8. Parse the sentence indicated by serial_num.
doParseSent :: IO ()
doParseSent = do
    putStr "Please input value of 'serial_num': "
    line <- getLine
    let sn = read line :: Int

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
             interpreter
      else if cate_check' == 1
             then do
                    getSentFromDB sn >>= getSent >>= parseSent sn
                    interpreter
             else do
                    putStrLn "cate_check value is abnormal."
                    interpreter

-- 9. Disply parsing Trees of the sentence indicated by serial_num.
doDisplyTreesForASent :: IO ()
doDisplyTreesForASent = do
    putStr "Please input value of 'serial_num': "
    line <- getLine
    let sn = read line :: Int
    readTree_String sn >>= sentToClauses >>= dispTree
    interpreter

-- 10. Quit from this program.
doQuit :: IO ()
doQuit = putStrLn "Bye!"
