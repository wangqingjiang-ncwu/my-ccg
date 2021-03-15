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

main :: IO ()
main = do
    hSetBuffering stdout NoBuffering               -- Set buffering mode for stdout as no buffering.
    hSetBuffering stdin LineBuffering              -- Set buffering mode for stdin as line buffering.
    hSetEncoding stdout utf8                       -- Set encoding for stdout as UTF8
    hFlush stdout                                  -- Flush buffered data to assure changing the encoding.

    putStrLn "Copyright (c) 2019-2021 China University of Water Resources and Electric Power, All rights reserved."
    putStrLn "Version: 0.2.4.0"
    putStrLn "This program create syntactic and semantic parsing results for Chinese sentences. Please run MySQL  Workbench or other similiar tools, connect MySQL database 'ccg4c', and querying table 'corpus' for waiting to complete needed revisions about part of speech as well as CCG syntactic types."
    putStrLn "The available child commands and their functions:"
    putStrLn "\t?\tDisplay this message."
    putStrLn "\t1\tGet the raw part-of-speech marked sentence (namely column raw_sent) given by value of column 'serial_num'."
    putStrLn "\t2\tGet the part-of-speech marks revised sentence (namely column raw_sent2) given by value of column 'serial_num'."
    putStrLn "\t3\tCreate categorial column 'cate_sent' from part-of-speech column 'raw_sent2' for a given row. Before executing this child command, parts of speech of all words in this sentence should be revised, and set column 'pos_check' as 1. If the value of column 'pos_check' of this row is 0, this child command will be banned."
    putStrLn "\t4\tCopy categorial column 'cate_sent' to column 'cate_sent2' to be revised for a given row. After executing this child command, CCG-syntactic types of every words in this sentence should be revised, then set column 'cate_check' as 1."
    putStrLn "\t5\tGet the CCG-marked sentence (namely column cate_sent) given by value of column 'serial_num'."
    putStrLn "\t6\tGet the CCG mark-revised sentence (namely column cate_sent2) given by value of column 'serial_num'."
    putStrLn "\t7\tParse the sentence given by value of column 'serial_num'."
    putStrLn "\t8\tDisplay parsing result of the sentence given by value of column 'serial_num'."
    putStrLn "\t9\tQuit from this program."
    interpreter

-- Child command interpreter
interpreter :: IO ()
interpreter = do
    putStrLn "? -> Display command list."
    putStrLn "1 -> Get raw part-of-speech marked sentence indicated by serial_num."
    putStrLn "2 -> Get the revised part-of-speech marked sentence indicated by serial_num."
    putStrLn "3 -> Create CCG-marked sentence from its revised part-of-speech marks."
    putStrLn "4 -> Get CCG-marked sentence indicated by serial_num."
    putStrLn "5 -> Copy CCG-marked sentence indicated by serial_num to column cate_sent2."
    putStrLn "6 -> Get revised CCG-marked sentence indicated by serial_num."
    putStrLn "7 -> Parse the sentence indicated by serial_num."
    putStrLn "8 -> Disply parsing Trees of the sentence indicated by serial_num."
    putStrLn "9 -> doQuit"
    putStr "Please input command: "
    line <- getLine
    if notElem line ["?","1","2","3","4","5","6","7","8","9"]
      then do
        putStrLn "Input invalid."
        interpreter
      else case line of
             "?" -> interpreter
             "1" -> doGetRawSentForASent
             "2" -> doGetRawSent2ForASent
             "3" -> doPosToCateForASent
             "4" -> doGetCateSentForASent
             "5" -> doCopyCateForASent
             "6" -> doGetCateSent2ForASent
             "7" -> doParseSent
             "8" -> doDisplyTreesForASent
             "9" -> doQuit

-- 1. Get raw part-of-speech marked sentence indicated by serial_num.
doGetRawSentForASent :: IO ()
doGetRawSentForASent = do
    putStr "Please input value of 'serial_num': "
    line <- getLine
    let sn = read line :: Int
    getRawSentForASent sn >>= putStrLn
    interpreter

-- 2. Get the revised part-of-speech marked sentence indicated by serial_num.
doGetRawSent2ForASent :: IO ()
doGetRawSent2ForASent = do
    putStr "Please input value of 'serial_num': "
    line <- getLine
    let sn = read line :: Int
    getRawSent2ForASent sn >>= putStrLn
    interpreter

-- 3. Create CCG-marked sentence from its revised part-of-speech marks.
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

-- 4. Get CCG-marked sentence indicated by serial_num.
doGetCateSentForASent :: IO ()
doGetCateSentForASent = do
    putStr "Please input value of 'serial_num': "
    line <- getLine
    let sn = read line :: Int
    getCateSentForASent sn >>= putStrLn
    interpreter

-- 5. Copy CCG-marked sentence indicated by serial_num to column cate_sent2.
doCopyCateForASent :: IO ()
doCopyCateForASent = do
    putStr "Please input value of 'serial_num': "
    line <- getLine
    let sn = read line :: Int
    copyCateForASent sn
    interpreter

-- 6. Get revised CCG-marked sentence indicated by serial_num.".
doGetCateSent2ForASent :: IO ()
doGetCateSent2ForASent = do
    putStr "Please input value of 'serial_num': "
    line <- getLine
    let sn = read line :: Int
    getCateSent2ForASent sn >>= putStrLn
    interpreter

-- 7. Parse the sentence indicated by serial_num.
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

-- 8. Disply parsing Trees of the sentence indicated by serial_num.
doDisplyTreesForASent :: IO ()
doDisplyTreesForASent = do
    putStr "Please input value of 'serial_num': "
    line <- getLine
    let sn = read line :: Int
    readTree_String sn >>= sentToClauses >>= dispTree
    interpreter

-- 9. Quit from this program.
doQuit :: IO ()
doQuit = putStrLn "Bye!"
