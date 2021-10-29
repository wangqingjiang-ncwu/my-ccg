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
import Statistics

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
    A   Statistically analyze the database table 'corpus' and 'stru_gene'.
    0   Quit from this program.
 -}
main :: IO ()
main = do
    hSetBuffering stdout NoBuffering               -- Set buffering mode for stdout as no buffering.
    hSetBuffering stdin LineBuffering              -- Set buffering mode for stdin as line buffering.
    hSetEncoding stdout utf8                       -- Set encoding for stdout as UTF8
    hFlush stdout                                  -- Flush buffered data to assure changing the encoding.

    putStrLn " Chinese CCG parser (build 0.2.5.0)"
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

    conn <- getConn
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
    putStrLn " 9 -> Display parsing Trees of the sentence indicated by serial_num."
    putStrLn " A -> Statistically analyze the database table corpus and stru_gene"
    putStrLn " 0 -> doQuit"
    putStr "Please input command: "
    line <- getLine
    if notElem line ["?","1","2","3","4","5","6","7","8","9","A","0"]
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
             "9" -> doDisplayTreesForASent username
             "A" -> doStatisticalAnalysis username
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
        stmt <- prepareStmt conn "select cate_check, tree_check from corpus where serial_num = ?"
        (defs, is) <- queryStmt conn stmt [toMySQLInt32 sn]            --([ColumnDef], InputStream [MySQLValue])
        cate_tree_check <- S.read is
        let cate_tree_check' = case cate_tree_check of
                                 Just x -> x
                                 Nothing -> error "doParseSent: No cate_tree_check was read."
        skipToEof is                                                   -- Go to the end of the stream.
        let cate_check = fromMySQLInt8 (cate_tree_check'!!0)
        let tree_check = fromMySQLInt8 (cate_tree_check'!!1)
        if (cate_check == 0 || tree_check == 1)
          then do
             putStrLn $ "Parsing failed because cate_check = " ++ (show cate_check) ++ ", tree_check = " ++ (show tree_check)
             interpreter username
          else if (cate_check == 1 && tree_check == 0)
                 then do
                   getSentFromDB sn >>= getSent >>= parseSent sn
                   interpreter username
                 else do
                   putStrLn "Value of cate_check or tree_check is abnormal."
                   interpreter username
      else do
        putStrLn "Parsing failed! you are not the intellectual property creator of this sentence."
        interpreter username

-- 9. Display parsing Trees of the sentence indicated by serial_num.
doDisplayTreesForASent :: String -> IO ()
doDisplayTreesForASent username = do
    putStr "Please input value of 'serial_num': "
    line <- getLine
    let sn = read line :: Int
    readTree_String sn >>= sentToClauses >>= dispTree
    interpreter username

-- A. Do statistical analysis about Table corpus and stru_gene, and display results.
doStatisticalAnalysis :: String -> IO ()
doStatisticalAnalysis username = do
    putStrLn " ? -> Display command list."
    putStrLn " 1 -> Count in table 'corpus'."
    putStrLn " 2 -> Count in table 'stru_gene'."
    putStrLn " 0 -> Go back to the upper layer."
    putStr "Please input command: "
    line <- getLine
    if notElem line ["?","1","2","0"]
       then do
         putStrLn "Invalid input."
         doStatisticalAnalysis username
    else case line of
         "?" -> doStatisticalAnalysis username
         "1" -> doCountInCorpus username
         "2" -> doCountInStruGene username
         "0" -> interpreter username

-- A1. Do count in corpus and display results. 't' means from field 'tree', and 's' means from 'script'.
doCountInCorpus :: String -> IO ()
doCountInCorpus username = do
    putStrLn " ? -> Display command list."
    putStrLn " t1 -> Get total number of sentences."
    putStrLn " t2 -> Get clausal number in every sentence."
    putStrLn " t3 -> Get averge clausal number per sentence."
    putStrLn " t4 -> Get length of every clause in every sentence."
    putStrLn " t5 -> Get phrase number of every clause in every sentence."
    putStrLn " t6 -> Get clausal number of different clausal lengths."
    putStrLn " t7 -> Get clausal number of different parsing-tree depths."
    putStrLn " t8 -> Get frequency total and normalized frequencies of different CCG tags."
    putStrLn " t9 -> Get frequency total and normalized frequencies of different phrasal structures."
    putStrLn " tA -> Get frequency total and normalized frequencies of different type-tag-stru(s)."
    putStrLn " s1 -> Get transitive times of every clause in all sentences."
    putStrLn " s2 -> Get frequencies of different ransitive times in all clause parsing."
    putStrLn " s3 -> Get conversion-ruled list in parsing every clause."
    putStrLn " s4 -> Get number of abandoned phrases in parsing every clause."
    putStrLn " s5 -> Get number of abandoned not-recognizable phrases in parsing every clause."
    putStrLn " 0 -> Go back to the upper layer."

    putStr "Please input command: "
    line <- getLine
    if notElem line ["?","t1","t2","t3","t4","t5","t6","t7","t8","t9","tA","s1","s2","s3","s4","s5","0"]
       then do
         putStrLn "Invalid input."
         doCountInCorpus username
       else case line of
         "?" -> doCountInCorpus username
         "t1" -> doCountInTree username 1
         "t2" -> doCountInTree username 2
         "t3" -> doCountInTree username 3
         "t4" -> doCountInTree username 4
         "t5" -> doCountInTree username 5
         "t6" -> doCountInTree username 6
         "t7" -> doCountInTree username 7
         "t8" -> doCountInTree username 8
         "t9" -> doCountInTree username 9
         "tA" -> doCountInTree username 10
         "s1" -> doCountInScript username 1
         "s2" -> doCountInScript username 2
         "s3" -> doCountInScript username 3
         "s4" -> doCountInScript username 4
         "s5" -> doCountInScript username 5
         "0" -> interpreter username

-- A1_1. Display statistical results from field 'tree' in table 'corpus'.
doCountInTree :: String -> Int -> IO ()
doCountInTree username funcIndex = do
    putStr "Please input the value of 'serial_num' of start sentence: "
    line <- getLine
    let bottomSn = read line :: Int
    putStr "Please input the value of 'serial_num' of end sentence: "
    line <- getLine
    let topSn = (read line :: Int) + 1
    countInTree bottomSn topSn funcIndex
    doCountInCorpus username

-- A1_2. Display statistical results from field 'script' in table 'corpus'.
doCountInScript :: String -> Int -> IO ()
doCountInScript username funcIndex = do
    putStr "Please input the value of 'serial_num' of start sentence: "
    line <- getLine
    let bottomSn = read line :: Int
    putStr "Please input the value of 'serial_num' of end sentence: "
    line <- getLine
    let topSn = (read line :: Int) + 1
    countInScript bottomSn topSn funcIndex
    doCountInCorpus username

-- A2. Display statistical results from table 'struGene'.
doCountInStruGene :: String  -> IO ()
doCountInStruGene username = do
    putStrLn " ? -> Display command list."
    putStrLn " 1 -> Get total number of structural genes."
    putStrLn " 2 -> Get frequencies of different overlapping types."
    putStrLn " 3 -> Get frequencies of most common phrasal overlapping (LROs) by given common proportion."
    putStrLn " 4 -> Get frequencies of most common phrasal overlapping (LROPs) by given common proportion."
    putStrLn " 5 -> Get clausal number of different clausal lengths."
    putStrLn " 6 -> Get clausal number of different parsing-tree depths."
    putStrLn " 7 -> Get frequency total and normalized frequencies of different CCG tags."
    putStrLn " 8 -> Get frequency total and normalized frequencies of different phrasal structures."
    putStrLn " 9 -> Get frequency total and normalized frequencies of different type-tag-stru(s)."
    putStrLn " 0 -> Go back to the upper layer."

    putStr "Please input command: "
    line <- getLine
    if notElem line ["?","1","2","3","4","5","6","7","8","9","0"]
      then do
        putStrLn "Invalid input."
        doCountInStruGene username
      else case line of
        "?" -> doCountInStruGene username
        "1" -> doCountInStruGene' username 1
        "2" -> doCountInStruGene' username 2
        "3" -> doCountInStruGene' username 3
        "4" -> doCountInStruGene' username 4
        "5" -> doCountInTree username 5
        "6" -> doCountInTree username 6
        "7" -> doCountInTree username 7
        "8" -> doCountInTree username 8
        "9" -> doCountInTree username 9
        "0" -> interpreter username

-- A2_1. Display statistical results from table 'stru_gene'.
doCountInStruGene' :: String -> Int -> IO ()
doCountInStruGene' username funcIndex = do
    countInStruGene funcIndex
    doCountInStruGene username

-- 0. Quit from this program.
doQuit :: IO ()
doQuit = putStrLn "Bye!"
