-- Copyright (c) 2019-2025 China University of Water Resources and Electric Power
-- All rights reserved.

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE MultiWayIf #-}

module Main (
    main      -- IO()
    ) where

import Control.Monad
import qualified System.IO.Streams as S
import qualified Data.Map as Map
import Data.List (sort)
import Data.Tuple.Utils
import System.IO
import Data.Time.Clock
import qualified Data.String as DS
import Database.MySQL.Base
import Category
import Phrase (sortPhraCateBySpan')
import Corpus
import SentParse
import Database
import Statistics
import Utils
import AmbiResol
import Clustering
import KMedoids
import Maintain
import Output (showSnScript2List)
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
    putStrLn " Copyright (c) 2019-2025 China University of Water Resources and Electric Power, All rights reserved."

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
    line <- getLineUntil "Please input command [RETURN for ?]: " ["?","1","2","3","4","5","6","7","8","9","A","B","C","D","0"] True
    if line == "0"
      then doQuit
      else do
             case line of
               "?" -> putStr ""                           -- Do nothing
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
             interpreter username

-- 1. Get raw part-of-speech marked sentence indicated by serial_num.
doGetRawSentForASent :: String -> IO ()
doGetRawSentForASent username = do
    putStr "Please input value of 'serial_num': "
    line <- getLine
    let sn = read line :: Int
    getRawSentForASent sn >>= putStrLn

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
          then putStrLn "Creating column 'raw_sent2' failed because column 'pos_check' is 1."
          else if pos_check' == 0
                 then do
                   copyRawSentForASent sn
                 else do
                   putStrLn "pos_check value is abnormal."
      else putStrLn "Copying failed! you are not the intellectual property creator of this sentence."

-- 3. Get the revised part-of-speech marked sentence indicated by serial_num.
doGetRawSent2ForASent :: String -> IO ()
doGetRawSent2ForASent username = do
    putStr "Please input value of 'serial_num': "
    line <- getLine
    let sn = read line :: Int
    getRawSent2ForASent sn >>= putStrLn

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
          then putStrLn "Creating column 'cate_sent' failed because column 'pos_check' is 0."

          else if pos_check' == 1
                 then do
                   putStrLn "doPosToCateForASent: pos_check'==1"
                   posToCateForASent sn
                 else putStrLn "pos_check value is abnormal."
      else putStrLn "Creating CCG marks failed! you are not the intellectual property creator of this sentence."

-- 5. Get CCG-marked sentence indicated by serial_num.
doGetCateSentForASent :: String -> IO ()
doGetCateSentForASent username = do
    putStr "Please input value of 'serial_num': "
    line <- getLine
    let sn = read line :: Int
    getCateSentForASent sn >>= putStrLn

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
          then putStrLn "Creating column 'cate_sent2' failed because column 'cate_check' is 1."
          else if cate_check' == 0
             then copyCateForASent sn
             else putStrLn "cate_check value is abnormal."
      else putStrLn "Copying failed! you are not the intellectual property creator of this sentence."

-- 7. Get revised CCG-marked sentence indicated by serial_num.".
doGetCateSent2ForASent :: String -> IO ()
doGetCateSent2ForASent username = do
    putStr "Please input value of 'serial_num': "
    line <- getLine
    let sn = read line :: Int
    getCateSent2ForASent sn >>= putStrLn

{- 8. Parse the sentence indicated by serial_num, here 'username' MUST
 - be the intellectual property creator (ipc) of the row indicated by serial_num.
 -}
doParseSent :: String -> IO ()
doParseSent username = do
    putStrLn " ? -> Display command list"
    putStrLn " 1 -> Do parsing by human mind"
    putStrLn " 2 -> Do parsing by script"
    putStrLn " 3 -> Do parsing by StruGene"
    putStrLn " 4 -> Do parsing by grammar ambiguity resolution"
    putStrLn " 0 -> Go back to the upper layer"
    line <- getLineUntil "Please input command [RETURN for ?]: " ["?","1","2","3","4","0"] True

    if line == "0"
      then putStrLn "Go back to the upper layer."              -- Naturally return to upper layer.
      else do
             case line of
               "?" -> putStr ""                                -- Do nothing
               "1" -> doParseSentByHumanMind username
               "2" -> doParseSentByScript username
               "3" -> doParseSentByStruGene username
               "4" -> doParseSentByGrammarAmbiResol username
             doParseSent username                              -- Rear recursion

{- 8_1. Human-Machine-interactively Parse the sentence indicated by serial_num, here 'username' MUST
 - be the intellectual property creator (ipc) of the row indicated by serial_num.
 -}
doParseSentByHumanMind :: String -> IO ()
doParseSentByHumanMind username = do
    confInfo <- readFile "Configuration"
    let tree_target = getConfProperty "tree_target" confInfo
    let syntax_ambig_resol_model = getConfProperty "syntax_ambig_resol_model" confInfo
    let syntax_ambig_resol_sample_update_switch = getConfProperty "syntax_ambig_resol_sample_update_switch" confInfo

    putStrLn $ " tree_target: " ++ tree_target
    putStrLn $ " syntax_ambig_resol_model: " ++ syntax_ambig_resol_model
    putStrLn $ " syntax_ambig_resol_sample_update_switch: " ++ syntax_ambig_resol_sample_update_switch

    if syntax_ambig_resol_sample_update_switch == "Off"
      then putStrLn "syntax_ambig_resol_sample_update_switch is Off."
      else do
        contOrNot <- getLineUntil ("Continue or not [c/n]? (RETURN for 'n') ") ["c","n"] False
        if contOrNot == "c"
          then do
            contOrNot2 <- getLineUntil (tree_target ++ " will be updated. Please confirm again continuing or not [c/n] (RETURN for 'n'): ") ["c","n"] False
            if contOrNot2 == "c"
              then do
                putStr "Please input value of 'serial_num' [RETURN for cancel]: "
                line <- getLine
                if line /= ""
                  then do
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
                        if (cate_check == 1 && tree_check == 0)
                          then getSentFromDB sn >>= getSent >>= parseSent sn
                          else putStrLn $ "Parsing failed because cate_check = " ++ (show cate_check) ++ ", tree_check = " ++ (show tree_check)
                      else do
                        putStrLn "Parsing failed! you are not the intellectual property creator of this sentence."
                  else putStrLn "Operation was canceled."
              else putStrLn "Operation was canceled."
          else putStrLn "Operation was canceled."

{- 8_2. According to the previously created script, parse the sentence indicated by serial_num.
 - The parsing result will be compared with the result created manually, and then dropped away.
 -}
doParseSentByScript :: String -> IO ()
doParseSentByScript username = do
    confInfo <- readFile "Configuration"
    let script_source = getConfProperty "script_source" confInfo                     -- Script source
    let tree_source = getConfProperty "tree_source" confInfo                         -- Tree source
    let tree_target = getConfProperty "tree_target" confInfo        -- SLR sample target.
    let syntax_ambig_resol_sample_update_switch = getConfProperty "syntax_ambig_resol_sample_update_switch" confInfo
    let category_ambig_resol_sample_update_switch = getConfProperty "category_ambig_resol_sample_update_switch" confInfo

    putStrLn $ " script_source: " ++ script_source
    putStrLn $ " tree_source: " ++ tree_source
    putStrLn $ " tree_target: " ++ tree_target
    putStrLn $ " syntax_ambig_resol_sample_update_switch: " ++ syntax_ambig_resol_sample_update_switch
    putStrLn $ " category_ambig_resol_sample_update_switch: " ++ category_ambig_resol_sample_update_switch

    contOrNot <- getLineUntil ("Continue or not [c/n]? (RETURN for 'n') ") ["c","n"] False
    if contOrNot == "c"
      then do
        contOrNot2 <- getLineUntil ("Targets will be updated. Continuing or not [c/n] (RETURN for 'n'): ") ["c","n"] False
        if contOrNot2 == "c"
          then do
            putStr "Please input serial_num of start sentence: "
            line1 <- getLine
            let startSn = read line1 :: Int
            putStr "Please input serial_num of end sentence: "
            line2 <- getLine
            let endSn = read line2 :: Int

            if startSn > endSn
              then putStrLn "No sentence is designated."
              else parseSentsByScript startSn endSn

          else putStrLn "Operation was canceled."
      else putStrLn "Operation was canceled."

{- 8_3. By resolving syntactic ambiguity according to StruGene samples, parse the sentences indicated by serial_nums.
 - Parsing result including parsing scripts will be stored into database table indicated by 'tree_target'.
 -}
doParseSentByStruGene :: String -> IO ()
doParseSentByStruGene username = do
    putStrLn " ? -> Display command list"
    putStrLn " 1 -> Resolving syntactic ambiguity according to most similar StruGene context"
    putStrLn " 0 -> Go back to the upper layer"
    line <- getLineUntil "Please input command [RETURN for ?]: " ["?","1","0"] True
    if line == "0"
      then putStrLn "Go back to the upper layer."              -- Naturally return to upper layer.
      else do
             case line of
               "?" -> putStr ""                                -- Do nothing
               "1" -> doParseSentByHighestSimilarity username
             doParseSentByStruGene username                    -- Rear recursion

{- 8_3_1. Parse sentence by resolving syntactic ambiguity according to StruGene sample with highest similarity degree,
 - Similarity degree calculated by grammatic attribute being same or not is called identity similarity.
 - Similarity degree calculated by grammatic attribute's concurrent contexts is called embedded similarity.
 -}
doParseSentByHighestSimilarity :: String -> IO ()
doParseSentByHighestSimilarity username = do
    putStrLn " ? -> Display command list"
    putStrLn " 1 -> Similarity degree calculated by grammatic attribute being same or not"
    putStrLn " 2 -> Similarity degree calculated by grammatic attribute-embedded contexts"
    putStrLn " 0 -> Go back to the upper layer"
    line <- getLineUntil "Please input command [RETURN for ?]: " ["?","1","2","0"] True
    if line == "0"
      then putStrLn "Go back to the upper layer."              -- Naturally return to upper layer.
      else do
             case line of
               "?" -> putStr ""                                -- Do nothing
               "1" -> parseSentByStruGeneFromConf "StruGeneIdentity"   -- Identity similarity
               "2" -> parseSentByStruGeneFromConf "StruGeneEmbedded"   -- Embedded similarity
             doParseSentByHighestSimilarity username           -- Rear recursion

{- 8_4. By resolving categorial ambiguity according to SLR samples and resolving syntactic ambiguity according to StruGene samples,
 - parse the sentences indicated by serial_nums.
 - Parsing result including parsing scripts will be stored into database table indicated by 'tree_target'.
 -}
doParseSentByGrammarAmbiResol :: String -> IO ()
doParseSentByGrammarAmbiResol username = do
    putStr "Please input serial_num of start sentence: "
    line1 <- getLine
    let startSn = read line1 :: Int
    putStr "Please input serial_num of end sentence: "
    line2 <- getLine
    let endSn = read line2 :: Int

    if startSn > endSn
      then putStrLn "No sentence is designated."
      else parseSentByGrammarAmbiResol startSn endSn

-- 9. Display parsing Trees of the sentence indicated by serial_num.
doDisplayTreesForASent :: String -> IO ()
doDisplayTreesForASent username = do
    confInfo <- readFile "Configuration"               -- Read the local configuration file
    let tree_source = getConfProperty "tree_source" confInfo
    let tree_field = getConfProperty "tree_field" confInfo
    putStrLn $ " tree_source: " ++ tree_source
    putStrLn $ " tree_field: " ++ tree_field

    contOrNot <- getLineUntil ("Continue or not [c/n]? (RETURN for 'c') ") ["c","n"] True
    if contOrNot == "c"
      then do
        putStr "Please input value of 'serial_num': "
        line <- getLine
        let sn = read line :: Int
        readTree_String sn tree_source tree_field >>= sentToClauses >>= dispTreeOfSent
      else putStrLn "Operation was cancelled."

{-- 9. Display parsing Trees of the sentence indicated by serial_num, which is for comparing differences between two kinds of parsing trees.
doDisplayTreesForASent :: String -> IO ()
doDisplayTreesForASent username = do
    confInfo <- readFile "Configuration"
    let tree_source = getConfProperty "tree_source" confInfo
    let tree_field = getConfProperty "tree_field" confInfo
    let ambi_resol_result_tree_source = getConfProperty "ambi_resol_result_tree_source" confInfo

    putStr "Please input value of 'serial_num': "
    line <- getLine
    let sn = read line :: Int
--    readTree_String sn tree_source tree_field >>= sentToClauses >>= dispTreeOfSent
    clauses <- (readTree_String sn tree_source tree_field >>= sentToClauses)
    clauses' <- (readTree_String sn ambi_resol_result_tree_source tree_field >>= sentToClauses)
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
    putStrLn " 4 -> Evaluate an experimental treebank"
    putStrLn " 0 -> Go back to the upper layer"
    line <- getLineUntil "Please input command [RETURN for ?]: " ["?","1","2","3","4","0"] True
    if line == "0"
      then putStrLn "Go back to the upper layer."              -- Naturally return to upper layer.
      else do
             case line of
               "?" -> putStr ""                                -- Do nothing
               "1" -> doCountInTreebank username
               "2" -> doCountInStruGene username
               "3" -> doSearchInTreebank username
               "4" -> doEvaluateAnExperimentalTreebank username
             doStatisticalAnalysis username                    -- Rear recursion

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
    putStrLn " tC -> Get similarity degree between every pair of categories"
    putStrLn " tD -> Get similarity degree between every pair of grammatic rules"
    putStrLn " tE -> Get similarity degree between every pair of phrasal structures"
    putStrLn " tF -> Get similarity degree between every two phrases in their grammatic features by SVD"
    putStrLn " s1 -> Get transitive times of every clause in all sentences"
    putStrLn " s2 -> Get frequencies of different ransitive times in all clause parsing"
    putStrLn " s3 -> Get the list of transitive times for every different clausal length"
    putStrLn " s4 -> Get type-conversional list and type-conversional total in parsing every clause"
    putStrLn " s5 -> Get frequency of using type conversions in transitive computing for every clausal length"
    putStrLn " s6 -> Get number of abandoned phrases in parsing every clause"
    putStrLn " s7 -> Get the minimum, maximum, and mean number of abandoned phrases for every clausal length"
    putStrLn " s8 -> Get number of abandoned not-recognizable phrases in parsing every clause"
    putStrLn " 0 -> Go back to the upper layer"

    line <- getLineUntil "Please input command [RETURN for ?]: "
        ["?","t1","t2","t3","t4","t5","t6","t7","t8","t9","tA","tB","tC","tD","tE","tF","s1","s2","s3","s4","s5","s6","s7","s8","0"] True
    if line == "0"
      then putStrLn "Go back to the upper layer."              -- Naturally return to upper layer.
      else do
             case line of
               "?" -> putStr ""                                -- Do nothing
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
               "tC" -> doCountInTree username 12
               "tD" -> doCountInTree username 13
               "tE" -> doCountInTree username 14
               "tF" -> doCountInTree username 15
               "s1" -> doCountInScript username 1
               "s2" -> doCountInScript username 2
               "s3" -> doCountInScript username 3
               "s4" -> doCountInScript username 4
               "s5" -> doCountInScript username 5
               "s6" -> doCountInScript username 6
               "s7" -> doCountInScript username 7
               "s8" -> doCountInScript username 8
             doCountInTreebank username                         --Rear recursion

-- A1_1. Display statistical results from field 'tree' in table 'corpus'.
doCountInTree :: String -> Int -> IO ()
doCountInTree username funcIndex = do
    putStr "Please input the value of 'serial_num' of start sentence: "
    line <- getLine
    let bottomSn = read line :: Int
    putStr "Please input the value of 'serial_num' of end sentence: "
    line <- getLine
    let topSn = read line :: Int

    confInfo <- readFile "Configuration"
    let tree_source = getConfProperty "tree_source" confInfo
    let phra_gram_dist_algo = getConfProperty "phra_gram_dist_algo" confInfo
    putStrLn $ " tree_source: " ++ tree_source
    putStrLn $ " phra_gram_dist_algo: " ++ phra_gram_dist_algo

    contOrNot <- getLineUntil ("Continue or not [c/n]? (RETURN for 'n') ") ["c","n"] False
    if contOrNot == "c"
      then do
        countInTree bottomSn topSn funcIndex
      else putStrLn "Operation was cancelled."

-- A1_2. Display statistical results from field 'script' in table 'corpus'.
doCountInScript :: String -> Int -> IO ()
doCountInScript username funcIndex = do
    putStr "Please input the value of 'serial_num' of start sentence: "
    line <- getLine
    let bottomSn = read line :: Int
    putStr "Please input the value of 'serial_num' of end sentence: "
    line <- getLine
    let topSn = read line :: Int

    confInfo <- readFile "Configuration"
    let script_source = getConfProperty "script_source" confInfo
    putStrLn $ " script_source: " ++ script_source
    contOrNot <- getLineUntil ("Continue or not [c/n]? (RETURN for 'n') ") ["c","n"] False
    if contOrNot == "c"
      then do
        countInScript bottomSn topSn funcIndex
      else putStrLn "Operation was cancelled."

-- A2. Display statistical results from table 'struGene'.
doCountInStruGene :: String  -> IO ()
doCountInStruGene username = do
    putStrLn " ? -> Display command list"
    putStrLn " 1 -> Get total number of structural genes"
    putStrLn " 2 -> Get frequencies of different overlapping types"
    putStrLn " 3 -> Get frequencies of most common phrasal overlapping (LROs) by given common proportion"
    putStrLn " 4 -> Get frequencies of most common phrasal overlapping (LROPs) by given common proportion"
    putStrLn " 5 -> Get hit count of different overlapping types, namely [(OverType, sum(LpHitCount + RpHitCount + NothHitCount))]"
    putStrLn " 6 -> Get similarity degree between every pair of categories"
    putStrLn " 7 -> Get similarity degree between every pair of grammatic rules"
    putStrLn " 8 -> Get similarity degree between every pair of phrasal structures"
    putStrLn " 9 -> Get similarity degree between every pair of phrasal spans"
    putStrLn " A -> Get similarity degree between every pair of PhraSyn values"
    putStrLn " 0 -> Go back to the upper layer."

    line <- getLineUntil "Please input command [RETURN for ?]: " ["?","1","2","3","4","5","6","7","8","9","A","0"] True
    if line == "0"
      then putStrLn "Go back to the upper layer."              -- Naturally return to upper layer.
      else do
             case line of
               "?" -> putStr ""                                -- Do nothing
               "1" -> doCountInStruGene' username 1
               "2" -> doCountInStruGene' username 2
               "3" -> doCountInStruGene' username 3
               "4" -> doCountInStruGene' username 4
               "5" -> doCountInStruGene' username 5
               "6" -> doCountInStruGene' username 6
               "7" -> doCountInStruGene' username 7
               "8" -> doCountInStruGene' username 8
               "9" -> doCountInStruGene' username 9
               "A" -> doCountInStruGene' username 10
             doCountInStruGene username                        -- Rear recursion

-- A2_1. Display statistical results from a certain StruGene sample base, such as table 'stru_gene_202501'.
doCountInStruGene' :: String -> Int -> IO ()
doCountInStruGene' username funcIndex = do
    putStr "Please input the value of 'id' of start sample: "
    startIdxStr <- getLine
    let startIdx = read startIdxStr :: Int
    putStr "Please input the value of 'id' of end sample: "
    endIdxStr <- getLine
    let endIdx = read endIdxStr :: Int

    let prompt = " Sample(s) from " ++ startIdxStr ++ " to " ++ endIdxStr ++ " will be searched, are you sure? [y/n] (RETURN for 'y'): "
    answer <- getLineUntil prompt ["y","n"] True
    if answer == "n"
      then putStrLn "doCountInStruGene: cancelled."
      else countInStruGene startIdx endIdx funcIndex

-- A3. Display search result in treebank. 't' means from field 'tree', and 's' means from 'script'.
doSearchInTreebank :: String -> IO ()
doSearchInTreebank username = do
    putStrLn " ? -> Display command list"
    putStrLn " t1 -> Get serial_num list indicating those parsing trees which include given C2CCG calculus tag"
    putStrLn " t2 -> Get serial_num list indicating those parsing trees which include given phrasal structure"
    putStrLn " t3 -> Get serial_num list indicating those parsing trees which include given syntactic category"
    putStrLn " t4 -> Display parsing trees of all clauses of all sentences."
    putStrLn " t5 -> Display parsing trees in which include given grammatic rule."
    putStrLn " t6 -> Display parsing trees in which include given phrasal structure."
    putStrLn " t7 -> Display parsing trees in which include given syntactic category."
    putStrLn " t8 -> To do."
    putStrLn " s1 -> ...."
    putStrLn " s2 -> ...."
    putStrLn " 0 -> Go back to the upper layer"

    line <- getLineUntil "Please input command [RETURN for ?]: " ["?","t1","t2","t3","t4","t5","t6","t7","t8","s1","s2","0"] True
    if line == "0"
      then putStrLn "Go back to the upper layer."              -- Naturally return to upper layer.
      else do
             case line of
               "?" -> putStr ""                                -- Do nothing
               "t1" -> doSearchInTree username 1
               "t2" -> doSearchInTree username 2
               "t3" -> doSearchInTree username 3
               "t4" -> doSearchInTree username 4
               "t5" -> doSearchInTree username 5
               "t6" -> doSearchInTree username 6
               "t7" -> doSearchInTree username 7
               "t8" -> doSearchInTree username 8
               "s1" -> doSearchInScript username 1
               "s2" -> doSearchInScript username 2
             doSearchInTreebank username                       -- Rear recursion

-- A3_1. Display search results from field 'tree' in table 'corpus'.
doSearchInTree :: String -> Int -> IO ()
doSearchInTree username funcIndex = do
    putStr "Please input the value of 'serial_num' of start sentence: "
    bottomSnStr <- getLine
    let bottomSn = read bottomSnStr :: Int
    putStr "Please input the value of 'serial_num' of end sentence: "
    topSnStr <- getLine
    let topSn = (read topSnStr :: Int) + 1

    let prompt = " Sentence(s) from " ++ bottomSnStr ++ " to " ++ topSnStr ++ " will be searched, are you sure? [y/n] (RETURN for 'y'): "
    answer <- getLineUntil prompt ["y","n"] True
    if answer == "n"
      then putStrLn "doSearchInTree: cancelled."
      else searchInTree bottomSn topSn funcIndex
    doSearchInTreebank username

-- A3_2. Display search results from field 'script' in table 'corpus'.
doSearchInScript :: String -> Int -> IO ()
doSearchInScript username funcIndex = do
    putStr "Please input the value of 'serial_num' of start sentence: "
    bottomSnStr <- getLine
    let bottomSn = read bottomSnStr :: Int
    putStr "Please input the value of 'serial_num' of end sentence: "
    topSnStr <- getLine
    let topSn = read topSnStr :: Int

    let prompt = " Sentence(s) from " ++ bottomSnStr ++ " to " ++ topSnStr ++ " will be searched, are you sure? [y/n] (RETURN for 'y'): "
    answer <- getLineUntil prompt ["y","n"] True
    if answer == "n"
      then putStrLn "doSearchInScript: cancelled."
      else searchInScript bottomSn topSn funcIndex
    doSearchInTreebank username

-- A4. Evaluate an experimental treebank against a certain benchmark treebank.
doEvaluateAnExperimentalTreebank :: String -> IO ()
doEvaluateAnExperimentalTreebank username = evaluateExperimentalTreebank

-- B. Do experiments.
doExperiments :: String -> IO ()
doExperiments username = do
    putStrLn " ? -> Display command list"
    putStrLn " 1 -> Parse sentence using all lexcial rules"
    putStrLn " 2 -> Test context similarity degree 1.0 only happens between one StruGene sample and itself"
    putStrLn " 3 -> Test identity between two parsing scripts"
    putStrLn " 4 -> Test the ratio of StruGene2 samples which include duplicate clauTags"
    putStrLn " 0 -> Go back to the upper layer"

    line <- getLineUntil "Please input command [RETURN for ?]: " ["?","1","2","3","4","0"] True
    if line == "0"
      then putStrLn "Go back to the upper layer."              -- Naturally return to upper layer.
      else do
             case line of
               "?" -> putStr ""                                -- Do nothing
               "1" -> doParseSentWithAllLexRules username
               "2" -> doTestSim1HappenBetweenOneStruGeneSampleAndItself username
               "3" -> doTestScriptIdentity username
               "4" -> doTestRatioOfStruGene2SamplesIncludingDuplicateClauTags username
             doExperiments username                            -- Rear recursion

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

{- B.2 Test context similarity degree 1.0 only happens between one StruGene sample and itself.
 -}
doTestSim1HappenBetweenOneStruGeneSampleAndItself :: String -> IO ()
doTestSim1HappenBetweenOneStruGeneSampleAndItself username = do
    startIdx <- getNumUntil "Please input index number of start StruGene sample [Return for 1]: " [0 ..]
    endIdx <- getNumUntil "Please input index number of end StruGene sample [Return for last]: " [0 ..]
    context2ClauTagPriorBase <- getContext2ClauTagPriorBase startIdx endIdx             -- [(ContextOfSG, ClauTagPrior)]
    let startIdx' = case startIdx of
                      0 -> 1
                      _ -> startIdx
    findWhereSim1HappenAmongStruGeneSamples startIdx' 0 context2ClauTagPriorBase

{- B.3 Test identity between two parsing scripts.
 -}
doTestScriptIdentity :: String -> IO ()
doTestScriptIdentity username = do
    confInfo <- readFile "Configuration"
    let benchmark_script = getConfProperty "script_source" confInfo
    let experimental_script = getConfProperty "experimental_script" confInfo
    let defaultStartIdx = getConfProperty "defaultStartIdx" confInfo
    let defaultEndIdx = getConfProperty "defaultEndIdx" confInfo

    putStrLn $ " benchmark_script: " ++ benchmark_script
    putStrLn $ " experimental_script: " ++ experimental_script
    putStrLn $ " defaultStartIdx = " ++ defaultStartIdx
    putStrLn $ " defaultEndIdx = " ++ defaultEndIdx

    contOrNot <- getLineUntil ("Continue or not [c/n]? (RETURN for 'n') ") ["c","n"] False
    if contOrNot == "c"
      then do
        conn <- getConn
        let query = DS.fromString ("select " ++ benchmark_script ++ ".serial_num as serial_num, "
                    ++ benchmark_script ++ ".script as script1, " ++ experimental_script ++ ".script as script2 from "
                    ++ benchmark_script ++ " inner join " ++ experimental_script ++ " where "
                    ++ benchmark_script ++ ".serial_num = " ++ experimental_script ++ ".serial_num && "
                    ++ experimental_script ++ ".serial_num >= ? && "
                    ++ experimental_script ++ ".serial_num <= ?")
        stmt <- prepareStmt conn query
        (defs, is) <- queryStmt conn stmt [toMySQLInt32 (read defaultStartIdx :: Int), toMySQLInt32 (read defaultEndIdx :: Int)]
        issList <- readStreamByInt32TextText [] is   -- [(Int, String, String)]
        let snScript2List = map (\x -> (fst3 x, map (\y -> (fst3 y, snd3 y, map sortPhraCateBySpan' (thd3 y))) (readScripts (snd3 x))
                                              , map (\y -> (fst3 y, snd3 y, map sortPhraCateBySpan' (thd3 y))) (readScripts (thd3 x)))
                                ) issList
                                                                                -- [(SentIdx, [Script], [Script])]
        let checkList = map (\x -> (fst3 x, snd3 x == thd3 x)) snScript2List    -- [(Int, Bool)]
        let oks = filter (\x -> snd x == True) checkList
        putStrLn $ " Serial numbers of sentences with same scripts = " ++ show (map fst oks)
        let bads = filter (\x -> snd x == False) checkList
        putStrLn $ " Serial numbers of sentences with different scripts = " ++ show (map fst bads)
        let badSnScripts = filter (\x -> snd3 x /= thd3 x) snScript2List
        putStrLn $ " Script contrast: "
        showSnScript2List badSnScripts
        close conn
      else putStrLn "doTestScriptIdentity: Test is cancelled."

{- B.4 Test the ratio of StruGene2 samples which include duplicate ClauTag values.
 -}
doTestRatioOfStruGene2SamplesIncludingDuplicateClauTags :: String -> IO ()
doTestRatioOfStruGene2SamplesIncludingDuplicateClauTags username = do
    confInfo <- readFile "Configuration"
    let syntax_ambig_resol_model = getConfProperty "syntax_ambig_resol_model" confInfo
    let defaultStartIdx = getConfProperty "defaultStartIdx" confInfo
    let defaultEndIdx = getConfProperty "defaultEndIdx" confInfo

    putStrLn $ " syntax_ambig_resol_model: " ++ syntax_ambig_resol_model
    putStrLn $ " defaultStartIdx = " ++ defaultStartIdx
    putStrLn $ " defaultEndIdx = " ++ defaultEndIdx

    contOrNot <- getLineUntil ("Continue or not [c/n]? (RETURN for 'n') ") ["c","n"] False
    if contOrNot == "c"
      then do
        case syntax_ambig_resol_model of
          x | elem x ["stru_gene_202501", "stru_gene_202412","stru_gene_202408"] -> do
            conn <- getConn
            let query = DS.fromString ("select id, clauTagPrior from " ++ syntax_ambig_resol_model)
            stmt <- prepareStmt conn query
            (defs, is) <- queryStmt conn stmt []                                --([ColumnDef], InputStream [MySQLValue])

            idCTPsStrList <- readStreamByInt32UText [] is                       -- [(Int, String)]
            let idCTPsList = map (\x -> (fst x, stringToCTPList (snd x))) idCTPsStrList    -- [(SIdx, [ClauTagPrior])]
            let idCTPsDupClauTagDecisionList = map (\x -> (fst x, snd x, hasDupClauTagInCTPList (snd x))) idCTPsList   -- [(SIdx, [ClauTagPrior], Bool)]
            let listOfCTPsHaveDupClauTag = filter (\x -> thd3 x == True) idCTPsDupClauTagDecisionList
            let numOfSamplesHaveDupClauTag = length listOfCTPsHaveDupClauTag
            let ratio = fromIntegral numOfSamplesHaveDupClauTag / fromIntegral (length idCTPsDupClauTagDecisionList)
            putStrLn $ "Ratio = " ++ (printf "%.4f" (ratio :: Double))
            putStrLn $ "(SIdx, [ClauTagPrior], True): " ++ show listOfCTPsHaveDupClauTag
            close conn

          _ -> putStrLn $ "syntax_ambig_resol_model: " ++ syntax_ambig_resol_model ++ "has no ratio definition."
      else putStrLn "doTestRatioOfStruGene2SamplesIncludingDuplicateClauTags: Test is cancelled."

-- C. Various maintenance tools.
doMaintenance :: String -> IO ()
doMaintenance username = do
    putStrLn " ? -> Display command list"
    putStrLn " 1 -> Rearrange auto-increment values of 'id' in a certain database table"
    putStrLn " 2 -> Sort phrases in corpus field 'tree' and 'script' according to span ascending"
    putStrLn " 3 -> Add phrasal structure Half-juXtaposition to corpus field 'tree' and 'script'"
    putStrLn " 4 -> Remove StruGene2 samples using PhraSyn0 definition"
    putStrLn " 5 -> Count clausal ambiguity at StruGene2 samples"
    putStrLn " 6 -> Count Prior ambiguity at StruGene2 samples"
    putStrLn " 0 -> Go back to the upper layer"

    line <- getLineUntil "Please input command [RETURN for ?]: " ["?","1","2","3","4","5","6","0"] True
    if line == "0"
      then putStrLn "Go back to the upper layer."              -- Naturally return to upper layer.
      else do
             case line of
               "?" -> putStr ""                                -- Do nothing
               "1" -> doRearrangeIdinCertainTable username
               "2" -> doSortPhraseInTreeAndScript username
               "3" -> doAddHX2TreeAndScript username
               "4" -> doRemoveStruGene2SamplesUsingPhraSyn0 username
               "5" -> doCountClausalAmbigAtSGSamples username
               "6" -> doCountPriorAmbigAtSGSamples username
             doMaintenance username                            -- Rear recursion

-- C_1. Rearrange index column in a certain table.
doRearrangeIdinCertainTable :: String -> IO ()
doRearrangeIdinCertainTable username =
    if username /= "wqj"
      then putStrLn "doRearrangeIdinCertainTable: Only user 'wqj' can do this operation."
      else do
        putStr "Please input the name of a table: "
        tblName <- getLine
        conn <- getConn
        let sqlstat = DS.fromString $ "create table " ++ tblName ++ "_bak like " ++ tblName      -- Copy table structure to new table.
        stmt' <- prepareStmt conn sqlstat
        executeStmt conn stmt' []
        putStrLn $ "Table " ++ tblName ++ "_bak was created."

        case tblName of
          x | elem x ["stru_gene_202408", "stru_gene_202412", "stru_gene_202501"] -> do
            let sqlstat = DS.fromString $ "insert into " ++ tblName ++ "_bak (leftExtend, leftOver, rightOver, rightExtend, overtype, clauTagPrior, lpHitCount, rpHitCount, nothHitCount) select leftExtend, leftOver, rightOver, rightExtend, overtype, clauTagPrior, lpHitCount, rpHitCount, nothHitCount from " ++ tblName     -- Copy table data to new table.
            stmt' <- prepareStmt conn sqlstat
            ok <- executeStmt conn stmt' []
            putStrLn $ show (getOkAffectedRows ok) ++ " rows were inserted into " ++ tblName ++ "_bak."
            let sqlstat = DS.fromString $ "truncate table " ++ tblName          -- Remove data from old table.
            stmt' <- prepareStmt conn sqlstat
            executeStmt conn stmt' []
            putStrLn $ "Table " ++ tblName ++ " truncated."
            let sqlstat = DS.fromString $ "insert into " ++ tblName ++ " select * from " ++ tblName ++ "_bak"    -- Copy table data to old table.
            stmt' <- prepareStmt conn sqlstat
            ok <- executeStmt conn stmt' []
            putStrLn $ show (getOkAffectedRows ok) ++ " rows were copied into " ++ tblName
            let sqlstat = DS.fromString $ "drop table " ++ tblName ++ "_bak"    -- Drop new table.
            stmt' <- prepareStmt conn sqlstat
            executeStmt conn stmt' []
            putStrLn $ "Table " ++ tblName ++ "_bak was dropped."
            close conn
          _ -> putStrLn $ "Table name was not recognized."

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

-- C_4. Remove StruGene2 samples using PhraSyn0 definition.
doRemoveStruGene2SamplesUsingPhraSyn0 :: String -> IO ()
doRemoveStruGene2SamplesUsingPhraSyn0 username = removeStruGene2SamplesUsingPhraSyn0

-- C_5. Count clausal ambiguity at StruGene2 samples.
doCountClausalAmbigAtSGSamples :: String -> IO ()
doCountClausalAmbigAtSGSamples username = countClausalAmbigAtSGSamples

-- C_6. Count Prior ambiguity at StruGene2 samples.
doCountPriorAmbigAtSGSamples :: String -> IO ()
doCountPriorAmbigAtSGSamples username = countPriorAmbigAtSGSamples

-- D. 测试聚类模块的相关函数.
doClustering :: String -> IO ()
doClustering username = do
    putStrLn " 1 -> Test function maxminPoint "
    putStrLn " 2 -> Test function updateCentre4ACluster"
    putStrLn " 3 -> Test function doOnceClustering"
    putStrLn " 4 -> Test function doClustering4DiffKValSNum"
    putStrLn " 5 -> Test function storeAmbiResolAccuracy4AllClustRes"
    putStrLn " 6 -> Test function getPhraSynSetSim"
    putStrLn " 7 -> Get similarity degrees between any two contexts of overlapping types by Singular Value Decomposition"
    putStrLn " 8 -> Get similarity degrees between any two contexts of overlapping types"
    putStrLn " 9 -> Get similarity degrees between any two overlapping types"
    putStrLn " A -> Get similarity degrees between any two contexts of StruGene samples by Singular Value Decomposition."
    putStrLn " B -> Get similarity degrees between any two contexts of StruGene samples"
    putStrLn " C -> Among StruGene samples, calculate similarity degrees from one to all contexts"
    putStrLn " D -> Get similarity degrees between one ClauTagPrior context to every context of StruGene samples"
    putStrLn " E -> Among StruGene samples, calculate similarity degree between every pair of contexts"
    putStrLn " F -> Run K-Medoids clustering using DB similarity and analyze Prior distribution"
    putStrLn " G -> Batch running of K-Medoids clustering on StruGene2 samples."
    putStrLn " H -> Calculate probability of that two closest samples has same Prior value in StruGene2 samples"
    putStrLn " 0 -> Go back to the upper layer"
    line <- getLineUntil "Please input command [RETURN for ?]: " ["?","1","2","3","4","5","6","7","8","9","A","B","C","D","E","F","G","H","0"] True
    if line == "0"
      then putStrLn "Go back to the upper layer."              -- Naturally return to upper layer.
      else do
             case line of
               "?" -> putStr ""                                -- Do nothing
               "1" -> doTestfunctionOfMaxminPoint
{-       "2" -> do
 -                doTestfunctionOfUpdateCentre4ACluster
 -}
               "3" -> doOnceClustering
               "4" -> doClustering4DiffKValSNum
               "5" -> storeAmbiResolAccuracy4AllClustRes
               "6" -> clusteringAnalysis (read line :: Int)
               "7" -> clusteringAnalysis 7
               "8" -> clusteringAnalysis 8
               "9" -> clusteringAnalysis 9
               "A" -> clusteringAnalysis 10
               "B" -> clusteringAnalysis 11
               "C" -> clusteringAnalysis 12
               "D" -> clusteringAnalysis 13
               "E" -> clusteringAnalysis 14
               "F" -> kMedoidsClusteringOnStruGene2Samples
               "G" -> batchKMedoidsOnStruGene2Samples
               "H" -> doCalProbOfClosestSampleHasSamePrior
             doClustering username                             -- Rear recursion

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
    struGeneSampleList <- readStreamByStruGene [] is
    putStrLn $ "doTestfunctionOfMaxminPoint: " ++ show (length struGeneSampleList)
    let m1 = head struGeneSampleList
    let sgs = tail struGeneSampleList
    let initialKPoints = getKModeByMaxMinPoint sgs [m1] Map.empty kVal distWeiRatioList
    putStrLn $ "The inital k points are " ++ show initialKPoints
    closeStmt conn stmt

{- D_2.测试求众心函数
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
    let syntax_ambig_resol_model = getConfProperty "syntax_ambig_resol_model" confInfo
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

    putStrLn $ "The current syntax_ambig_resol_model is set as: " ++ syntax_ambig_resol_model
               ++ ", distDef = " ++ distDef ++ ", kVal = " ++ show kVal ++ ", sNum = " ++ show sNum
               ++ ", distWeiRatioList = " ++ show distWeiRatioList'
    let arm = if | syntax_ambig_resol_model == "stru_gene" -> "SG"
                 | syntax_ambig_resol_model == "ambi_resol1" -> "AR1"
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
    let syntax_ambig_resol_model = getConfProperty "syntax_ambig_resol_model" confInfo
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

    putStrLn $ "The current syntax_ambig_resol_model is set as: " ++ syntax_ambig_resol_model
               ++ ", distDef = " ++ distDef ++ ", bottomKVal = " ++ show bottomKVal ++ ", bottomSNum = " ++ show bottomSNum
               ++ ", deltaKVal = " ++ show deltaKVal ++ ", deltaSNum = " ++ show deltaSNum
               ++ ", topKVal = " ++ show topKVal ++ ", topSNum = " ++ show topSNum
               ++ ", distWeiRatioList = " ++ show distWeiRatioList'

    let arm = if | syntax_ambig_resol_model == "stru_gene" -> "SG"
                 | syntax_ambig_resol_model == "ambi_resol1" -> "AR1"
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
    let syntax_ambig_resol_model = getConfProperty "syntax_ambig_resol_model" confInfo
    let distDef = getConfProperty "distDef" confInfo
    let bottomKVal = read (getConfProperty "bottomKVal" confInfo) :: Int
    let deltaKVal = read (getConfProperty "deltaKVal" confInfo) :: Int
    let topKVal = read (getConfProperty "topKVal" confInfo) :: Int
    let bottomSNum = read (getConfProperty "bottomSNum" confInfo) :: Int
    let deltaSNum = read (getConfProperty "deltaSNum" confInfo) :: Int
    let topSNum = read (getConfProperty "topSNum" confInfo) :: Int

    putStrLn $ "The current syntax_ambig_resol_model is set as: " ++ syntax_ambig_resol_model
               ++ ", distDef = " ++ distDef ++ ", bottomKVal = " ++ show bottomKVal ++ ", bottomSNum = " ++ show bottomSNum
               ++ ", deltaKVal = " ++ show deltaKVal ++ ", deltaSNum = " ++ show deltaSNum
               ++ ", topKVal = " ++ show topKVal ++ ", topSNum = " ++ show topSNum

    let arm = if | syntax_ambig_resol_model == "stru_gene" -> "SG"
                 | syntax_ambig_resol_model == "ambi_resol1" -> "AR1"
                 | otherwise -> "Nothing"
    let df = if | distDef == "arithAdd" -> "AA"
                | distDef == "normArithMean" -> "NAM"
                | otherwise -> "Nothing"


    putStrLn $ "doClustering4DiffKValSNum: arm = " ++ arm ++ ", df = " ++ df
    autoRunGetAmbiResolAccuracyOfAllClustRes arm df bottomKVal bottomKVal deltaKVal topKVal bottomSNum deltaSNum topSNum

{- D_F. K-Medoids clustering on StruGene2 samples.
 -}
kMedoidsClusteringOnStruGene2Samples :: IO ()
kMedoidsClusteringOnStruGene2Samples = do
    confInfo <- readFile "Configuration"
    let syntax_ambig_resol_model = getConfProperty "syntax_ambig_resol_model" confInfo
    let cluster_logs_dir = getConfProperty "cluster_logs_dir" confInfo
    let clus_res_file = getConfProperty "clus_res_file" confInfo
    let clusters_trace_file = getConfProperty "clusters_trace_file" confInfo
    let mean_sim_trace_file = getConfProperty "mean_sim_trace_file" confInfo
    putStrLn $ "syntax_ambig_resol_model: " ++ syntax_ambig_resol_model
    putStrLn $ "cluster_logs_dir: " ++ cluster_logs_dir
    putStrLn $ "clus_res_file: " ++ clus_res_file
    putStrLn $ "clusters_trace_file: " ++ clusters_trace_file
    putStrLn $ "mean_sim_trace_file: " ++ mean_sim_trace_file

    putStr "Please input K = "
    line <- getLine
    putStrLn $ "K = " ++ line
    let k = read line :: Int

    putStr "Please input: maxIterNum = "
    line <- getLine
    putStrLn $ "maxIterNum = " ++ line
    let maxIterNum = read line :: IterNum

    putStr "Please input start value of 'id' in StruGene2 sample base: "
    line <- getLine
    let startId = read line :: SIdx
    putStr "Please input end value of 'id' in StruGene2 sample base: "
    line <- getLine
    let endId = read line :: SIdx

    cOrN <- getLineUntil "Continue or Not? [y]/n" ["y","n"] True
    if cOrN == "y" || cOrN == ""
      then do
        (_, clus_res) <- kMedoidsClustering k maxIterNum startId endId
        exportClustersToCSV k clus_res
        putStrLn "[INFO] Clustering end."
      else putStrLn "Task D->F was cancelled."

{- D_G. Batch running of K-Medoids clustering on StruGene2 samples.
 -}
batchKMedoidsOnStruGene2Samples :: IO ()
batchKMedoidsOnStruGene2Samples = do
    confInfo <- readFile "Configuration"
    let syntax_ambig_resol_model = getConfProperty "syntax_ambig_resol_model" confInfo
    let cluster_logs_dir = getConfProperty "cluster_logs_dir" confInfo
    let clus_res_file = getConfProperty "clus_res_file" confInfo
    let clusters_trace_file = getConfProperty "clusters_trace_file" confInfo
    let mean_sim_trace_file = getConfProperty "mean_sim_trace_file" confInfo
    putStrLn $ "syntax_ambig_resol_model: " ++ syntax_ambig_resol_model
    putStrLn $ "cluster_logs_dir: " ++ cluster_logs_dir
    putStrLn $ "clus_res_file: " ++ clus_res_file
    putStrLn $ "clusters_trace_file: " ++ clusters_trace_file
    putStrLn $ "mean_sim_trace_file: " ++ mean_sim_trace_file

    putStr "Please input initial K = "
    line <- getLine
    putStrLn $ "Initial K = " ++ line
    let k = read line :: Int

    putStr "Please input: maxIterNum = "
    line <- getLine
    putStrLn $ "maxIterNum = " ++ line
    let maxIterNum = read line :: IterNum

    putStr "Please input start value of 'id' in StruGene2 sample base: "
    line <- getLine
    let startId = read line :: SIdx
    putStr "Please input end value of 'id' in StruGene2 sample base: "
    line <- getLine
    let endId = read line :: SIdx

    cOrN <- getLineUntil "Continue or Not? [y]/n" ["y","n"] True
    if cOrN == "y" || cOrN == ""
      then loop k maxIterNum startId endId
      else putStrLn "Task D->G was cancelled."
    where
      loop :: Int -> IterNum -> SIdx -> SIdx -> IO ()
      loop k maxIterNum startId endId = do
        (flag, clus_res) <- kMedoidsClustering k maxIterNum startId endId
        exportClustersToCSV k clus_res
        if flag == 1                                           -- All cluster purities reach threshhold.
          then putStrLn "[INFO] Batch Clustering ends."
          else if (2*k > endId - startId + 1)
                 then putStrLn "[INFO] K too big."
                 else loop (2*k) maxIterNum startId endId

{- D_H. Calculate probability of that one sample has same major Prior value with its closest sample in StruGene2 database.
 - The closest sample means it has highest similarity degree with the compared sample.
 -}
doCalProbOfClosestSampleHasSamePrior :: IO ()
doCalProbOfClosestSampleHasSamePrior = do
    confInfo <- readFile "Configuration"                                        -- Read the local configuration file
    let syntax_ambig_resol_model = getConfProperty "syntax_ambig_resol_model" confInfo
    let csg_sim_tbl = getConfProperty "csg_sim_tbl" confInfo
    putStrLn $ " syntax_ambig_resol_model: " ++ syntax_ambig_resol_model
    putStrLn $ " csg_sim_tbl: " ++ csg_sim_tbl

    putStr "Please input start value of 'id' in StruGene2 sample base: "
    line <- getLine
    let startId = read line :: SIdx
    putStr "Please input end value of 'id' in StruGene2 sample base: "
    line <- getLine
    let endId = read line :: SIdx

    contOrNot <- getLineUntil ("Continue or not [c/n]? (RETURN for 'n') ") ["c","n"] False
    if contOrNot == "c"
      then do
        prob <- calProbOfClosestSampleHasSamePrior csg_sim_tbl startId endId
        putStrLn $ " Probability: " ++ show prob
      else putStrLn "doCalProbOfClosestSampleHasSamePrior: Operation was cancelled."

-- 0. Quit from this program.
doQuit :: IO ()
doQuit = putStrLn "Bye!"
