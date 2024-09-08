-- Copyright (c) 2019-2024 China University of Water Resources and Electric Power,
-- All rights reserved.
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE MultiWayIf #-}

module Main (
    main      -- IO()
    ) where

import Control.Monad
import qualified System.IO.Streams as S
import qualified Data.Map as Map
import System.IO
import Data.Time.Clock
import qualified Data.String as DS
import Database.MySQL.Base
import Corpus
import SentParse
import Database
import Statistics
import Utils
import AmbiResol
import Clustering
import Maintain
import Text.Printf

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
    B   Experiments.
    C   Maintenance tools.
    D   Clustering analysis.
    0   Quit from this program.
 -}
main :: IO ()
main = do
    hSetBuffering stdout NoBuffering               -- Set buffering mode for stdout as no buffering.
    hSetBuffering stdin LineBuffering              -- Set buffering mode for stdin as line buffering.
--  hSetEncoding stdin utf8                        -- Set encoding for stdin as UTF8
    hSetEncoding stdout utf8                       -- Set encoding for stdout as UTF8
    hFlush stdout                                  -- Flush buffered data to assure changing the encoding.

    putStrLn " Chinese CCG parser (build 0.2.7.0)"
    putStrLn " Copyright (c) 2019-2023 China University of Water Resources and Electric Power, All rights reserved."

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
    stmt <- prepareStmt conn "select * from user where name = ? && password = ?"     -- my-ccg-exe user/password
    (defs, is) <- queryStmt conn stmt [toMySQLText username, toMySQLText password]   -- ([ColumnDef], InputStream [MySQLValue])
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
checkIpc :: SentIdx -> String -> IO Bool
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

{- Check whether current user is intellectual property creator (IPC) of the rows indicated by start serial_num and end serial_num.
 - When 'startSn' > 'endSn', return True.
 -}
checkIpc4MultiSent :: SentIdx -> SentIdx -> String -> IO Bool
checkIpc4MultiSent startSn endSn username = do
    if startSn > endSn
      then return True
      else do
        ok <- checkIpc startSn username
        if ok
          then if startSn == endSn
                 then return True
                 else checkIpc4MultiSent (startSn + 1) endSn username >>= return
          else return False

-- Command interpreter for given user.
interpreter :: String -> IO ()
interpreter username = do
    putStrLn " ? -> Display command list"
    putStrLn " 1 -> Get raw part-of-speech marked sentence indicated by serial_num"
    putStrLn " 2 -> Copy raw sentence indicated by serial_num to column raw_sent2"
    putStrLn " 3 -> Get the revised part-of-speech marked sentence indicated by serial_num"
    putStrLn " 4 -> Create CCG-marked sentence from its revised part-of-speech marks"
    putStrLn " 5 -> Get CCG-marked sentence indicated by serial_num"
    putStrLn " 6 -> Copy CCG-marked sentence indicated by serial_num to column cate_sent2"
    putStrLn " 7 -> Get revised CCG-marked sentence indicated by serial_num"
    putStrLn " 8 -> Parse the sentence indicated by serial_num"
    putStrLn " 9 -> Display parsing Trees of the sentence indicated by serial_num"
    putStrLn " A -> Statistically analyze treebank and ambiguity resolution database stru_gene"
    putStrLn " B -> Experiments"
    putStrLn " C -> Maintenance tools"
    putStrLn " D -> Clustering analysis"
    putStrLn " 0 -> doQuit"
    putStr "Please input command: "
    line <- getLine
    if notElem line ["?","1","2","3","4","5","6","7","8","9","A","B","C","D","0"]
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
             "B" -> doExperiments username
             "C" -> doMaintenance username
             "D" -> doClustering username
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
                   putStrLn "doPosToCateForASent: pos_check'==1"
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
 - be the intellectual property creator (ipc) of the row indicated by serial_num.
 -}
doParseSent :: String -> IO ()
doParseSent username = do
    putStrLn " ? -> Display command list"
    putStrLn " 1 -> Do parsing by human mind"
    putStrLn " 2 -> Do parsing by script"
    putStrLn " 0 -> Go back to the upper layer"
    putStr "Please input command: "
    line <- getLine
    if notElem line ["?","1","2","0"]
       then do
         putStrLn "Invalid input."
         doParseSent username
    else case line of
         "?" -> doParseSent username
         "1" -> doParseSentByHumanMind username
         "2" -> doParseSentByScript username
         "0" -> interpreter username

{- 8_1. Human-Machine-interactively Parse the sentence indicated by serial_num, here 'username' MUST
 - be the intellectual property creator (ipc) of the row indicated by serial_num.
 -}
doParseSentByHumanMind :: String -> IO ()
doParseSentByHumanMind username = do
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
             doParseSent username
          else if (cate_check == 1 && tree_check == 0)
                 then do
                   getSentFromDB sn >>= getSent >>= parseSent sn
                   doParseSent username
                 else do
                   putStrLn "Value of cate_check or tree_check is abnormal."
                   doParseSent username
      else do
        putStrLn "Parsing failed! you are not the intellectual property creator of this sentence."
        doParseSent username

{- 8_2. According to the previously created script, parse the sentence indicated by serial_num.
 -}
doParseSentByScript :: String -> IO ()
doParseSentByScript username = do
    putStr "Please input serial_num of start sentence: "
    line1 <- getLine
    let startSn = read line1 :: Int
    putStr "Please input serial_num of end sentence: "
    line2 <- getLine
    let endSn = read line2 :: Int

    if startSn > endSn
      then putStrLn "No sentence is designated."
      else parseSentsByScript startSn endSn

    doParseSent username

-- 9. Display parsing Trees of the sentence indicated by serial_num.
doDisplayTreesForASent :: String -> IO ()
doDisplayTreesForASent username = do
    confInfo <- readFile "Configuration"               -- Read the local configuration file
    let tree_source = getConfProperty "tree_source" confInfo

    putStr "Please input value of 'serial_num': "
    line <- getLine
    let sn = read line :: Int
    readTree_String sn tree_source >>= sentToClauses >>= dispTreeOfSent
    interpreter username

{-- 9. Display parsing Trees of the sentence indicated by serial_num, which is for comparing differences between two kinds of parsing trees.
doDisplayTreesForASent :: String -> IO ()
doDisplayTreesForASent username = do
    confInfo <- readFile "Configuration"
    let tree_source = getConfProperty "tree_source" confInfo
    let ambi_resol_result_tree_source = getConfProperty "ambi_resol_result_tree_source" confInfo

    putStr "Please input value of 'serial_num': "
    line <- getLine
    let sn = read line :: Int
--    readTree_String sn tree_source >>= sentToClauses >>= dispTreeOfSent
    clauses <- (readTree_String sn tree_source >>= sentToClauses)
    clauses' <- (readTree_String sn ambi_resol_result_tree_source >>= sentToClauses)
    dispComparisonTreesOfAmbiResolResult 1 clauses clauses' tree_source ambi_resol_result_tree_source

    interpreter username
-}
-- A. Do statistical analysis about table corpus and stru_gene, and display results.
doStatisticalAnalysis :: String -> IO ()
doStatisticalAnalysis username = do
    putStrLn " ? -> Display command list"
    putStrLn " 1 -> Count in treebank"
    putStrLn " 2 -> Count in table 'stru_gene'"
    putStrLn " 3 -> Search in treebank"
    putStrLn " 0 -> Go back to the upper layer"
    putStr "Please input command: "
    line <- getLine
    if notElem line ["?","1","2","3","0"]
       then do
         putStrLn "Invalid input."
         doStatisticalAnalysis username
    else case line of
         "?" -> doStatisticalAnalysis username
         "1" -> doCountInTreebank username
         "2" -> doCountInStruGene username
         "3" -> doSearchInTreebank username
         "0" -> interpreter username

-- A1. Do count in treebank and display results. 't' means from field 'tree', and 's' means from 'script'.
doCountInTreebank :: String -> IO ()
doCountInTreebank username = do
    putStrLn " ? -> Display command list"
    putStrLn " t1 -> Get total number of sentences"
    putStrLn " t2 -> Get clausal number in every sentence"
    putStrLn " t3 -> Get averge clausal number per sentence"
    putStrLn " t4 -> Get length of every clause in every sentence and average clausal length"
    putStrLn " t5 -> Get phrase number of every clause in every sentence"
    putStrLn " t6 -> Get clausal number of different clausal lengths"
    putStrLn " t7 -> Get parsing-tree depths of every clause in every sentence"
    putStrLn " t8 -> Get frequency total and normalized frequencies of different C2CCG calculus tags"
    putStrLn " t9 -> Get frequency of type conversions used in parsing every clause"
    putStrLn " tA -> Get frequency total and normalized frequencies of different phrasal structures"
    putStrLn " tB -> Get frequency total and normalized frequencies of different type-tag-stru(s)"
    putStrLn " s1 -> Get transitive times of every clause in all sentences"
    putStrLn " s2 -> Get frequencies of different ransitive times in all clause parsing"
    putStrLn " s3 -> Get the list of transitive times for every different clausal length"
    putStrLn " s4 -> Get type-conversional list and type-conversional total in parsing every clause"
    putStrLn " s5 -> Get frequency of using type conversions in transitive computing for every clausal length"
    putStrLn " s6 -> Get number of abandoned phrases in parsing every clause"
    putStrLn " s7 -> Get the minimum, maximum, and mean number of abandoned phrases for every clausal length"
    putStrLn " s8 -> Get number of abandoned not-recognizable phrases in parsing every clause"
    putStrLn " 0 -> Go back to the upper layer"

    putStr "Please input command: "
    line <- getLine
    if notElem line ["?","t1","t2","t3","t4","t5","t6","t7","t8","t9","tA","tB","s1","s2","s3","s4","s5","s6","s7","s8","0"]
       then do
         putStrLn "Invalid input."
         doCountInTreebank username
       else case line of
         "?" -> doCountInTreebank username
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
         "tB" -> doCountInTree username 11
         "s1" -> doCountInScript username 1
         "s2" -> doCountInScript username 2
         "s3" -> doCountInScript username 3
         "s4" -> doCountInScript username 4
         "s5" -> doCountInScript username 5
         "s6" -> doCountInScript username 6
         "s7" -> doCountInScript username 7
         "s8" -> doCountInScript username 8
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
    doCountInTreebank username

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
    doCountInTreebank username

-- A2. Display statistical results from table 'struGene'.
doCountInStruGene :: String  -> IO ()
doCountInStruGene username = do
    putStrLn " ? -> Display command list"
    putStrLn " 1 -> Get total number of structural genes"
    putStrLn " 2 -> Get frequencies of different overlapping types"
    putStrLn " 3 -> Get frequencies of most common phrasal overlapping (LROs) by given common proportion"
    putStrLn " 4 -> Get frequencies of most common phrasal overlapping (LROPs) by given common proportion"
    putStrLn " 0 -> Go back to the upper layer."

    putStr "Please input command: "
    line <- getLine
    if notElem line ["?","1","2","3","4","0"]
      then do
        putStrLn "Invalid input."
        doCountInStruGene username
      else case line of
        "?" -> doCountInStruGene username
        "1" -> doCountInStruGene' username 1
        "2" -> doCountInStruGene' username 2
        "3" -> doCountInStruGene' username 3
        "4" -> doCountInStruGene' username 4
        "0" -> interpreter username

-- A2_1. Display statistical results from table 'stru_gene'.
doCountInStruGene' :: String -> Int -> IO ()
doCountInStruGene' username funcIndex = do
    countInStruGene funcIndex
    doCountInStruGene username

-- A3. Display search result in treebank. 't' means from field 'tree', and 's' means from 'script'.
doSearchInTreebank :: String -> IO ()
doSearchInTreebank username = do
    putStrLn " ? -> Display command list"
    putStrLn " t1 -> Get serial_num list indicating those parsing trees which include given C2CCG calculus tags"
    putStrLn " t2 -> Display parsing trees of all clauses of all sentences."
    putStrLn " t3 -> Display parsing trees in which include given grammatic rule."
    putStrLn " t4 -> To do."
    putStrLn " s1 -> ...."
    putStrLn " s2 -> ...."
    putStrLn " 0 -> Go back to the upper layer"

    putStr "Please input command: "
    line <- getLine
    if notElem line ["?","t1","t2","t3","s1","s2","0"]
      then do
        putStrLn "Invalid input."
        doSearchInTreebank username
      else case line of
        "?" -> doSearchInTreebank username
        "t1" -> doSearchInTree username 1
        "t2" -> doSearchInTree username 2
        "t3" -> doSearchInTree username 3
        "t4" -> doSearchInTree username 4
        "s1" -> doSearchInScript username 1
        "s2" -> doSearchInScript username 2
        "0" -> interpreter username

-- A3_1. Display search results from field 'tree' in table 'corpus'.
doSearchInTree :: String -> Int -> IO ()
doSearchInTree username funcIndex = do
    putStr "Please input the value of 'serial_num' of start sentence: "
    line <- getLine
    let bottomSn = read line :: Int
    putStr "Please input the value of 'serial_num' of end sentence: "
    line <- getLine
    let topSn = (read line :: Int) + 1
    searchInTree bottomSn topSn funcIndex
    doSearchInTreebank username

-- A3_2. Display search results from field 'script' in table 'corpus'.
doSearchInScript :: String -> Int -> IO ()
doSearchInScript username funcIndex = do
    putStr "Please input the value of 'serial_num' of start sentence: "
    line <- getLine
    let bottomSn = read line :: Int
    putStr "Please input the value of 'serial_num' of end sentence: "
    line <- getLine
    let topSn = (read line :: Int) + 1
    searchInScript bottomSn topSn funcIndex
    doSearchInTreebank username

-- B. Do experiments.
doExperiments :: String -> IO ()
doExperiments username = do
    putStrLn " ? -> Display command list"
    putStrLn " 1 -> Parse sentence using all lexcial rules"
    putStrLn " 0 -> Go back to the upper layer"
    putStr "Please input command: "
    line <- getLine
    if notElem line ["?","1","0"]
      then do
        putStrLn "Invalid input."
        doExperiments username
      else case line of
        "?" -> doExperiments username
        "1" -> doParseSentWithAllLexRules username
        "0" -> interpreter username

{- B.1 Parse the sentence indicated by serial_num, here 'username' has not been used.
 -}
doParseSentWithAllLexRules :: String -> IO ()
doParseSentWithAllLexRules username = do
    putStr "Please input value of 'serial_num': "
    line <- getLine
    let sn = read line :: Int

    conn <- getConn
    stmt <- prepareStmt conn "select cate_check from corpus where serial_num = ?"
    (defs, is) <- queryStmt conn stmt [toMySQLInt32 sn]            --([ColumnDef], InputStream [MySQLValue])
    cate_check <- S.read is
    let cate_check' = case cate_check of
                          Just x -> x
                          Nothing -> error "doParseSentWithAllLexRules: No cate_check was read."
    skipToEof is                                                   -- Go to the end of the stream.
    let cate_check = fromMySQLInt8 (cate_check'!!0)
    if cate_check == 0
      then do
             putStrLn $ "Parsing failed because cate_check = " ++ show cate_check
             interpreter username
      else do
             getSentFromDB sn >>= getSent >>= parseSentWithAllLexRules sn
             interpreter username

-- C. Various maintenance tools.
doMaintenance :: String -> IO ()
doMaintenance username = do
    putStrLn " ? -> Display command list"
    putStrLn " 1 -> Rearrange auto-increment values of 'id' in a certain database table"
    putStrLn " 2 -> Sort phrases in corpus field 'tree' and 'script' according to span ascending"
    putStrLn " 3 -> Add phrasal structure Half-juXtaposition to corpus field 'tree' and 'script'"
    putStrLn " 0 -> Go back to the upper layer"
    putStr "Please input command: "
    line <- getLine
    if notElem line ["?","1","2","3","0"]
      then do
        putStrLn "Invalid input."
        doMaintenance username
      else case line of
        "?" -> doMaintenance username
        "1" -> do
                 putStrLn $ "Function " ++ line ++ " has not been implemented."
                 doMaintenance username
        "2" -> do
                 doSortPhraseInTreeAndScript username
                 doMaintenance username
        "3" -> do
                 doAddHX2TreeAndScript username
                 doMaintenance username
        "0" -> interpreter username

-- C_2. Sort phrases in corpus field 'tree' and 'script' according to span ascending.
doSortPhraseInTreeAndScript :: String -> IO ()
doSortPhraseInTreeAndScript username = do
    putStr "Please input serial_num of start sentence: "
    line1 <- getLine
    let startSn = read line1 :: Int
    putStr "Please input serial_num of end sentence: "
    line2 <- getLine
    let endSn = read line2 :: Int

    if startSn > endSn
      then putStrLn "No sentence is designated."
      else checkIpc4MultiSent startSn endSn username >>= \ok -> if ok
               then sortPhraseInTreeAndScript startSn endSn
               else putStrLn "You are not the complete owner of intellectual property of these sentences."

-- C_3. Add phrasal structure Half juXtaposition to parsing result.
doAddHX2TreeAndScript :: String -> IO ()
doAddHX2TreeAndScript username = do
    putStr "Please input serial_num of start sentence: "
    line1 <- getLine
    let startSn = read line1 :: Int
    putStr "Please input serial_num of end sentence: "
    line2 <- getLine
    let endSn = read line2 :: Int

    if startSn > endSn
      then putStrLn "No sentence is designated."
      else checkIpc4MultiSent startSn endSn username >>= \ok -> if ok
               then addHX2TreeAndScript startSn endSn
               else putStrLn "You are not the complete owner of intellectual property of these sentences."

-- D. 测试聚类模块的相关函数.
doClustering :: String -> IO ()
doClustering username = do
    putStrLn " ? -> Display command list"
    putStrLn " 1 -> Test function maxminPoint "
    putStrLn " 2 -> Test function updateCentre4ACluster"
    putStrLn " 3 -> Test function doOnceClustering"
    putStrLn " 4 -> Test function doClustering4DiffKValSNum"
    putStrLn " 0 -> Go back to the upper layer"
    putStr "Please input command: "
    line <- getLine
    if notElem line ["?","1","2","3","4","5","0"]
      then do
        putStrLn "Invalid input."
        doClustering username
    else case line of
        "?" -> doClustering username
        "1" -> doTestfunctionOfMaxminPoint
--        "2" -> doTestfunctionOfUpdateCentre4ACluster
        "3" -> doOnceClustering
        "4" -> doClustering4DiffKValSNum
        "5" -> storeAmbiResolAccuracy4AllClustRes
        "0" -> interpreter username

-- D_1.测试求初始点函数
doTestfunctionOfMaxminPoint :: IO ()
doTestfunctionOfMaxminPoint = do
    putStr "Please input the value of 'K': "
    line <- getLine
    let kVal = read line :: Int
    putStr "Please input start value of 'id' in stru_gene: "
    line <- getLine
    let startId = read line :: Int
    putStr "Please input end value of 'id' in stru_gene: "
    line <- getLine
    let endId = read line :: Int
    confInfo <- readFile "Configuration"
    let wle = read (getConfProperty "wle" confInfo) :: Int
    let wlo = read (getConfProperty "wlo" confInfo) :: Int
    let wro = read (getConfProperty "wro" confInfo) :: Int
    let wre = read (getConfProperty "wre" confInfo) :: Int
    let wot = read (getConfProperty "wot" confInfo) :: Int
    let wpr = read (getConfProperty "wpr" confInfo) :: Int
    let distWeiRatioList = [wle, wlo, wro, wre, wot, wpr]

    conn <- getConn
    stmt <- prepareStmt conn "select id, leftExtend, leftOver, rightOver, rightExtend, overType, prior from stru_gene where id >= ? and id <= ?"
    (defs, is) <- queryStmt conn stmt [toMySQLInt32 startId, toMySQLInt32 endId]
    struGeneSampleList <- readStreamByInt324TextInt8Text [] is
    putStrLn $ "doTestfunctionOfMaxminPoint: " ++ show (length struGeneSampleList)
    let m1 = head struGeneSampleList
    let sgs = tail struGeneSampleList
    let initialKPoints = getKModeByMaxMinPoint sgs [m1] Map.empty kVal distWeiRatioList
    putStrLn $ "The inital k points are " ++ show initialKPoints
    closeStmt conn stmt

{-- D_2.测试求众心函数
doTestfunctionOfUpdateCentre4ACluster :: IO ()
doTestfunctionOfUpdateCentre4ACluster = do
    putStr "Please input start value of 'id' in stru_gene: "
    line <- getLine
    let startId = read line :: Int
    putStr "Please input end value of 'id' in stru_gene: "
    line <- getLine
    let endId = read line :: Int

    conn <- getConn
    stmt <- prepareStmt conn "select leftExtend, leftOver, rightOver, rightExtend, overType, prior from stru_gene where id >= ? and id <= ?"
    (defs, is) <- queryStmt conn stmt [toMySQLInt32 startId, toMySQLInt32 endId]
    struGeneList <- readStreamBy4TextInt8Text [] is
--  putStrLn $ show struGeneList
    let aMode = updateCentre4ACluster struGeneList ([],[],[],[],[],[])
--    putStrLn $ "The mode in rows from " ++ show startId ++ " to " ++ show endId ++ " of the stru_gene is " ++ show aMode
    closeStmt conn stmt
-}

maxValOfInt = maxBound :: Int
--  scml = sample cluster mark list

-- D_3. Given kVal and sNum, do clustering.
doOnceClustering :: IO ()
doOnceClustering = do
    conn <- getConn
    confInfo <- readFile "Configuration"                                        -- Read the local configuration file
    let ambi_resol_model = getConfProperty "ambi_resol_model" confInfo
    let distDef = getConfProperty "distDef" confInfo
    let kVal = read (getConfProperty "kVal" confInfo) :: Int
    let sNum = read (getConfProperty "sNum" confInfo) :: Int
    let wle = read (getConfProperty "wle" confInfo) :: Int
    let wlo = read (getConfProperty "wlo" confInfo) :: Int
    let wro = read (getConfProperty "wro" confInfo) :: Int
    let wre = read (getConfProperty "wre" confInfo) :: Int
    let wot = read (getConfProperty "wot" confInfo) :: Int
    let wpr = read (getConfProperty "wpr" confInfo) :: Int
    let distWeiRatioList = [wle, wlo, wro, wre, wot, wpr]
    let distWeiRatioList' = init distWeiRatioList ++ [maxValOfInt]

    putStrLn $ "The current ambi_resol_model is set as: " ++ ambi_resol_model
               ++ ", distDef = " ++ distDef ++ ", kVal = " ++ show kVal ++ ", sNum = " ++ show sNum
               ++ ", distWeiRatioList = " ++ show distWeiRatioList'
    let arm = if | ambi_resol_model == "stru_gene" -> "SG"
                 | ambi_resol_model == "ambi_resol1" -> "AR1"
                 | otherwise -> "Nothing"
    let df = if | distDef == "arithAdd" -> "AA"
                | distDef == "normArithMean" -> "NAM"
                | otherwise -> "Nothing"

    putStrLn $ "doOnceClustering: arm = " ++ arm ++ ", df = " ++ df
    autoRunClustByChangeKValSNum arm df kVal (kVal, 0, kVal) (sNum, 0, sNum) distWeiRatioList'

{- D_4. From bottomKVal to topKVal, kVal increases every time by deltaKVal.
 - From bottomSNum to topSNum, sNum increases every time by deltaSNum.
 - For every (kVal, sNum), do clustering.
 -}
doClustering4DiffKValSNum :: IO ()
doClustering4DiffKValSNum = do
    conn <- getConn
    confInfo <- readFile "Configuration"                                        -- Read the local configuration file
    let ambi_resol_model = getConfProperty "ambi_resol_model" confInfo
    let distDef = getConfProperty "distDef" confInfo
    let bottomKVal = read (getConfProperty "bottomKVal" confInfo) :: Int
    let deltaKVal = read (getConfProperty "deltaKVal" confInfo) :: Int
    let topKVal = read (getConfProperty "topKVal" confInfo) :: Int
    let bottomSNum = read (getConfProperty "bottomSNum" confInfo) :: Int
    let deltaSNum = read (getConfProperty "deltaSNum" confInfo) :: Int
    let topSNum = read (getConfProperty "topSNum" confInfo) :: Int
    let wle = read (getConfProperty "wle" confInfo) :: Int
    let wlo = read (getConfProperty "wlo" confInfo) :: Int
    let wro = read (getConfProperty "wro" confInfo) :: Int
    let wre = read (getConfProperty "wre" confInfo) :: Int
    let wot = read (getConfProperty "wot" confInfo) :: Int
    let wpr = read (getConfProperty "wpr" confInfo) :: Int
    let distWeiRatioList = [wle, wlo, wro, wre, wot, wpr]
    let distWeiRatioList' = init distWeiRatioList ++ [maxValOfInt]

    putStrLn $ "The current ambi_resol_model is set as: " ++ ambi_resol_model
               ++ ", distDef = " ++ distDef ++ ", bottomKVal = " ++ show bottomKVal ++ ", bottomSNum = " ++ show bottomSNum
               ++ ", deltaKVal = " ++ show deltaKVal ++ ", deltaSNum = " ++ show deltaSNum
               ++ ", topKVal = " ++ show topKVal ++ ", topSNum = " ++ show topSNum
               ++ ", distWeiRatioList = " ++ show distWeiRatioList'

    let arm = if | ambi_resol_model == "stru_gene" -> "SG"
                 | ambi_resol_model == "ambi_resol1" -> "AR1"
                 | otherwise -> "Nothing"
    let df = if | distDef == "arithAdd" -> "AA"
                | distDef == "normArithMean" -> "NAM"
                | otherwise -> "Nothing"

    putStrLn $ "doClustering4DiffKValSNum: arm = " ++ arm ++ ", df = " ++ df
    autoRunClustByChangeKValSNum arm df bottomKVal (bottomKVal, deltaKVal, topKVal) (bottomSNum, deltaSNum, topSNum) distWeiRatioList'
--    autoRunGetAmbiResolAccuracyOfAllClustRes arm df bottomKVal bottomKVal deltaKVal topKVal bottomSNum deltaSNum topSNum []

{- D_5. Store all ambiguity resolution accuracy for all cluster results.
 -}
storeAmbiResolAccuracy4AllClustRes :: IO ()
storeAmbiResolAccuracy4AllClustRes = do
    conn <- getConn
    confInfo <- readFile "Configuration"                                        -- Read the local configuration file
    let ambi_resol_model = getConfProperty "ambi_resol_model" confInfo
    let distDef = getConfProperty "distDef" confInfo
    let bottomKVal = read (getConfProperty "bottomKVal" confInfo) :: Int
    let deltaKVal = read (getConfProperty "deltaKVal" confInfo) :: Int
    let topKVal = read (getConfProperty "topKVal" confInfo) :: Int
    let bottomSNum = read (getConfProperty "bottomSNum" confInfo) :: Int
    let deltaSNum = read (getConfProperty "deltaSNum" confInfo) :: Int
    let topSNum = read (getConfProperty "topSNum" confInfo) :: Int

    putStrLn $ "The current ambi_resol_model is set as: " ++ ambi_resol_model
               ++ ", distDef = " ++ distDef ++ ", bottomKVal = " ++ show bottomKVal ++ ", bottomSNum = " ++ show bottomSNum
               ++ ", deltaKVal = " ++ show deltaKVal ++ ", deltaSNum = " ++ show deltaSNum
               ++ ", topKVal = " ++ show topKVal ++ ", topSNum = " ++ show topSNum

    let arm = if | ambi_resol_model == "stru_gene" -> "SG"
                 | ambi_resol_model == "ambi_resol1" -> "AR1"
                 | otherwise -> "Nothing"
    let df = if | distDef == "arithAdd" -> "AA"
                | distDef == "normArithMean" -> "NAM"
                | otherwise -> "Nothing"


    putStrLn $ "doClustering4DiffKValSNum: arm = " ++ arm ++ ", df = " ++ df
    autoRunGetAmbiResolAccuracyOfAllClustRes arm df bottomKVal bottomKVal deltaKVal topKVal bottomSNum deltaSNum topSNum


-- 0. Quit from this program.
doQuit :: IO ()
doQuit = putStrLn "Bye!"
