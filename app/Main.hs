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
    putStrLn "Copyright (c) 2019-2021 China University of Water Resources and Electric Power, All rights reserved."
    putStrLn "Version: 0.2.4.0"
    putStrLn "This program create syntactic and semantic parsing results for Chinese sentences. Please run MySQL  Workbench or other similiar tools, connect MySQL database 'ccg4c', and querying table 'corpus' for waiting to complete needed revisions about part of speech as well as CCG syntactic types."
    putStrLn "The available child commands and their functions:"
    putStrLn "\t?\tDisplay this message."
    putStrLn "\t1\tGet marks of part of speech of a sentence (namely column raw_sent) given by value of column 'serial_num'."
    putStrLn "\t2\tCreate categorial column 'cate_sent' from part-of-speech column 'raw_sent2' for a given row. Before executing this child command, parts of speech of all words in this sentence should be revised, and set column 'pos_check' as 1. If the value of column 'pos_check' of this row is 0, this child command will be banned."
    putStrLn "\t3\tCopy categorial column 'cate_sent' to column 'cate_sent2' to be revised for a given row. After executing this child command, CCG-syntactic types of every words in this sentence should be revised, then set column 'cate_check' as 1."
    putStrLn "\t4\tParse a sentence given by value of column 'serial_num'."
    putStrLn "\t5\tDisplay parsing result of a sentence given by value of column 'serial_num'."
    putStrLn "\t6\tQuit from this program."
    interpreter

-- Child command interpreter
interpreter :: IO ()
interpreter = do
    hSetBuffering stdin LineBuffering                  -- Open input buffering
    putStrLn "? -> interpreter"
    putStrLn "1 -> doGetSentFromDB"
    putStrLn "2 -> doPosToCateForASent"
    putStrLn "3 -> doCopyCateForASent"
    putStrLn "4 -> doParseSent"
    putStrLn "5 -> doDisplyTrees"
    putStrLn "6 -> doQuit"
    putStr "Please input child command: "
    line <- getLine
    if notElem line ["?","1","2","3","4","5","6"]
      then do
        putStrLn "Input invalid."
        main
      else case line of
             "?" -> interpreter
             "1" -> doGetSentFromDB
             "2" -> doPosToCateForASent
             "3" -> doCopyCateForASent
             "4" -> doParseSent
             "5" -> doDisplyTrees
             "6" -> doQuit

-- Get a sentence from table corpus.
doGetSentFromDB :: IO ()
doGetSentFromDB = do
    hSetBuffering stdin LineBuffering                  -- Open input buffering
    putStr "Please input value of 'serial_num': "
    line <- getLine
    let sn = read line :: Int
    getSentFromDB sn >>= print
    interpreter

-- Create categorial column 'cate_sent' from part-of-speech column 'raw_sent2' for a given row.
doPosToCateForASent :: IO ()
doPosToCateForASent = do
    hSetBuffering stdin LineBuffering                  -- Open input buffering
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

-- Copy categorial column 'cate_sent' to column 'cate_sent2' to be revised for a given row.
doCopyCateForASent :: IO ()
doCopyCateForASent = do
    hSetBuffering stdin LineBuffering                  -- Open input buffering
    putStr "Please input value of 'serial_num': "
    line <- getLine
    let sn = read line :: Int
    copyCateForASent sn
    interpreter

-- Parse a sentence given by value of column 'serial_num'.
doParseSent :: IO ()
doParseSent = do
    hSetBuffering stdin LineBuffering                  -- Open input buffering
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

-- Display parsing result of a sentence given by value of column 'serial_num'.
doDisplyTrees :: IO ()
doDisplyTrees = do
    hSetBuffering stdin LineBuffering                  -- Open input buffering
    putStr "Please input value of 'serial_num': "
    line <- getLine
    let sn = read line :: Int
    readTree_String sn >>= sentToClauses >>= dispTree
    interpreter

-- Quit from this program.
doQuit :: IO ()
doQuit = putStrLn "Bye!"
