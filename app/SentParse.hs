{-# LANGUAGE OverloadedStrings, LambdaCase #-}

-- Copyright (c) 2019-2025 China University of Water Resources and Electric Power
-- All rights reserved.

-- Sentential Parsing by scripts were orignally written by Qian-qian WANG at 2023.

module SentParse (
    getSentFromDB,        -- SentIdx -> IO String
    getSentListFromDB,    -- SentIdx -> SentIdx -> IO [(Int, String)]
    getSent,              -- String -> IO [String]
--    getSentList,              -- String -> IO [String]
    parseSent,            -- SentIdx -> [String] -> IO Bool
    dropParsingResult,    -- SentIdx -> ClauIdx -> ClauIdx -> IO Bool
    goBackTo,             -- SentIdx -> ClauIdx -> IO ()
    parseSent',           -- SentIdx -> [String] -> IO ()
    storeClauseParsing,   -- SentIdx -> ClauIdx -> ([[Rule]],[PhraCate],[BanPCs]) -> IO ()
    parseClause,          -- [[Rule]] -> [PhraCate] -> [PhraCate] -> IO ([[Rule]],[PhraCate],[BanPCs])
    parseSentWithAllLexRules,          -- SentIdx -> [String] -> IO ()
    parseSentWithAllLexRules',         -- SentIdx -> ClauIdx -> [String] -> IO Bool
    parseClauseWithAllLexRules,        -- [PhraCate] -> IO [PhraCate]
    doTrans,              -- OnOff -> [PhraCate] -> [PhraCate] -> IO ([OnOff],[PhraCate],[PhraCate])
    updateStruGene,       -- [PhraCate] -> [OverPair] -> [(PhraCate,PhraCate)] -> IO [OverPair]
    updateStruGene',      -- ([PhraCate],PhraCate,PhraCate,[PhraCate],OverType) -> [OverPair] -> IO [OverPair]
    parseSentsByScript,   -- SentIdx -> SentIdx -> IO ()
    parseSentByScript,    -- SentIdx -> [String] -> IO Bool
    parseSentByScript',   -- SentIdx -> [String] -> [Script] -> IO (Bool, SLROfSent, [Tree], [Script], TagOfClauAnaly)
    parseClauseWithScript,      -- ClauTag -> [[Rule]] -> [PhraCate] -> BanPCs -> [OverPair] -> Script -> SLROfClause -> IO ([[Rule]], [PhraCate], BanPCs, SLROfClause)
    doTransWithScript,          -- ClauTag -> [PhraCate] -> BanPCs -> [OverPair] -> Script -> IO ([Rule], [PhraCate], BanPCs, [OverPair])
    getPhraStruCSFromStruGene2,       -- Int -> Int -> IO ()
    autoverifyEffectOfPhraStruCS,     -- Int -> IO ()
    parseSentByGrammarAmbiResol,      -- SentIdx -> SentIdx -> IO ()
    autoRunCollectPhraSyn,            -- Int -> Int -> IO ()
    parseSentByStruGeneFromConf,      -- SynAmbiResolMethod -> IO ()
    parseSentByStruGene,     -- SynAmbiResolMethod -> SendIdx -> SentIdx -> [(Int, String)] -> String -> String -> IO ()
    parseASentByStruGene,    -- SynAmbiResolMethod -> SentIdx -> [String] -> String -> String -> IO ()
    getStruGene1FromAmbiResol1,       -- Int -> Int -> IO ()
    evaluateExperimentalTreebank,     -- IO ()
    doTransWithManualResol,     -- ClauTag -> [Rule] -> [PhraCate] -> BanPCs -> [OverPair] -> IO ([Rule], [PhraCate], BanPCs, [OverPair])
    syntaxAmbiResolByManualResol,     -- [PhraCate] -> [OverPair] -> [(PhraCate, PhraCate)] -> IO [OverPair]
    syntaxAmbiResolByManualResol',    -- [PhraCate] -> (PhraCate, PhraCate) -> IO OverPair
    updateSyntaxAmbiResolSample,      -- [PhraCate] -> [OverPair] -> IO ()
    updateSyntaxAmbiResolSample',     -- [PhraCate] -> OverPair -> IO ()
    storeClauseParsingToTreebank,     -- SentIdx -> ClauIdx -> ([[Rule]], [PhraCate], BanPCs) -> IO ()
    readTree_String,      -- SentIdx -> String -> String -> IO String
    sentToClauses,        -- String -> IO [String]
    dispTreeOfSent,       -- [String] -> IO ()
    dispTree,             -- [String] -> IO ()
    dispTree',            -- ClauIdx -> [String] -> IO ()
    dispComparisonTreesOfAmbiResolResult,   -- Int -> [String] -> [String] -> String -> String -> IO ()
    getClauPhraCate,      -- String -> [PhraCate]
    parseSentWithoutPruning,     -- [Rule] -> [String] -> IO ()
    ) where

import Control.Monad
import System.Directory
import System.IO
import qualified System.IO.Streams as S
import qualified Data.ByteString.Char8 as C
import qualified Data.ByteString as BS
import qualified Data.String as DS
import Database.MySQL.Base
import Data.List.Utils
import Data.List
import Data.Tuple.Utils
import Data.String.Utils
import Phrase
import Rule
import Corpus
--import AmbiResol (OverType, Prior(..), OverPair, StruGene)
import AmbiResol
import Clustering
import Parse
import Output
import Utils
import Database
import qualified Data.Map as Map
import Text.Printf

-- Get a sentence from table corpus, actually the sentence is content of column cate_sent2.
getSentFromDB :: SentIdx -> IO String
getSentFromDB sn = do
    conn <- getConn
    stmt <- prepareStmt conn "select cate_sent2 from corpus where serial_num = ?"
    (defs, is) <- queryStmt conn stmt [toMySQLInt32 sn]
    S.read is >>= \case
      Just [MySQLText v] -> return $ fromMySQLText (MySQLText v)
      Nothing -> return ""

-- Get some sentences from table corpus, actually the sentences are content of column cate_sent2.
getSentListFromDB :: SentIdx -> SentIdx -> IO [(Int, String)]
getSentListFromDB startSn endSn = do
    confInfo <- readFile "Configuration"
    let sent_source = getConfProperty "sent_source" confInfo

    conn <- getConn
    let sqlstat = DS.fromString $ "select serial_num, cate_sent2 from " ++ sent_source ++ " where serial_num >= ? and serial_num <= ?"
    stmt <- prepareStmt conn sqlstat
    (defs, is) <- queryStmt conn stmt [toMySQLInt32 startSn, toMySQLInt32 endSn]
    sentList <- readStreamByInt32Text [] is
    return sentList

-- Split a sentence into clauses.
getSent :: String -> IO [String]
getSent sent
    | endswith " \12290:" sent'' = return $ split " \65292: " $ replace " \12290:" "" sent''
          -- \12290 is Chinese period.
          -- If Chinese comma has part-of-speech 'c', the comma ',' will be followed by "(X\*X)/*X" instead of "".
          -- So Chinese commas without syntactic types are seperators of clauses.
    | endswith " \65311:" sent'' = return $ split " \65292: " $ replace " \65311:" "" sent''
          -- \65311 is Chinese question mark, which is not considered now.
    | otherwise = return $ split " \65292: " sent''
    where
      sent' =  replace "\12298:" "" $ replace " \12298:" "" $ replace " \12299:" "" $ replace " \":" "" $ replace "\8220:" "" $ replace " \8220:" "" $ replace " \8221:" "" sent
                     -- Remove Chinese book title mark '<<' and '>>', English double quotation mark '"', and Chinese double quotation mark '“' and '”',
                     -- If title mark '<<' or double quotation '"' is the initial symbol of a clause, probably no blank space is before the title mark.
                     -- then strip whitespaces at left and right ends.
      sent'' = lstrip $ rstrip $ replace "\8230:" "" $ replace "\"\":" "" $ replace "\9472\9472" "\65292" $ replace "\65306" "\65292" $ replace "\65307" "\65292" sent'
                     -- Replace ';', ':' and "--" with ',' then remove "...:", and '":'.
                     -- \8230 is ellipsis dots "...", "\9472\9472" is Chinese dash, \65306 is Chinese colon, \65307 is Chinese semicolon, and \65292 is Chinese comma.
                     -- MySQL value "\"\"" stores one Chinese double quotation mark '"', which should be removed.

{- Parse a sentence, here every clause is a String, and parsing can start from a certain clause. The first parameter is the value of 'serial_num' in database Table 'corpus'.
 -}
parseSent :: SentIdx -> [String] -> IO ()
parseSent sn cs = do
    hSetBuffering stdin LineBuffering                  -- Open input buffering

    let clauNum = length cs
    let prompt1 = " There are " ++ show clauNum ++ " clauses in total, from which clause to start? (RETURN for 1): "
    let inputRange1 = [show x | x <- [1 .. clauNum]]
    clauIdxOfStartStr <- getLineUntil prompt1 inputRange1 True
    let clauIdxOfStart = read clauIdxOfStartStr :: Int

    let prompt2 = " to which clause to end? (RETURN for last one): "
    let inputRange2 = [show x | x <- [1 .. clauNum]]
    clauIdxOfEndStr <- getLineUntil prompt2 inputRange2 False
    let clauIdxOfEnd = read clauIdxOfEndStr :: Int

    let prompt3 = " Clause(s) from " ++ clauIdxOfStartStr ++ " to " ++ clauIdxOfEndStr ++ " will begin, are you sure? [y/n] (RETURN for 'y'): "
    answer <- getLineUntil prompt3 ["y","n"] True
    if answer == "n"
      then putStrLn "parseSent: cancelled."
      else do                -- Drop trees and scripts of clause clauIdxOfStart to clause clauIdxOfEnd.
             dropFlag <- dropParsingResult sn clauIdxOfStart clauIdxOfEnd
             if dropFlag
               then do
                 finFlag <- parseSent' sn (clauIdxOfStart - 1) (drop (clauIdxOfStart - 1) (take clauIdxOfEnd cs))      -- Parse clause from clauIdxOfStart to clauIdxOfEnd.
                 if finFlag
                   then putStrLn $ "parseSent: Parsing on sentence No." ++ show sn ++ " was finished successfully."
                   else putStrLn $ "parseSent: Parsing on Sentence No. " ++ show sn ++ " was cancelled midway."
               else putStrLn "parseSent: dropParsingResult returns False."                 -- dropParsingResult failed, return upper layer calling.

{- To prepare for parsing from clause clauIdxOfStart to clause clauIdxOfEnd, parsing trees and scripts of those clauses are removed in treebank.
 - The treebank is designated by property 'tree_target' in file Configuration.
 - For removing success, return True; otherwise return False.
 -}
dropParsingResult :: SentIdx -> ClauIdx -> ClauIdx -> IO Bool
dropParsingResult sn clauIdxOfStart clauIdxOfEnd = do
    hSetBuffering stdin LineBuffering                  -- Open input buffering
    confInfo <- readFile "Configuration"                                        -- Read the local configuration file
    let tree_target = getConfProperty "tree_target" confInfo

    removeClauTagPriorFromSynAmbiResol sn clauIdxOfStart clauIdxOfEnd           -- Remove ClauTagPrior tuples from stru_gene_202408.

    conn <- getConn
    let sqlstat = DS.fromString $ "select tree, script from " ++ tree_target ++ " where serial_num = ?"
    stmt <- prepareStmt conn sqlstat
    (defs, is) <- queryStmt conn stmt [toMySQLInt32 sn]
    row <- S.read is                                   -- Maybe [MySQLText, MySQLText]
    let row' = case row of
                 Just x -> x                           -- [MySQLText, MySQLText]
                 Nothing -> [MySQLText "[]", MySQLText "[]"]                    -- Both "tree" and "script" in treebank will be initialized as '[]'.
    S.skipToEof is                                     -- Consume result set.

    putStrLn $ "dropParsingResult: tree: " ++ (fromMySQLText (head row'))

    let trees = readTrees $ fromMySQLText (row'!!0)
    let scripts = readScripts $ fromMySQLText (row'!!1)

    let clauIdxRange = quickSort4Int [ fst t | t <- trees]
    putStrLn $ "dropParsingResult: The indices of parsed clauses are " ++ show clauIdxRange

    let trees' = [ t | t <- trees, notElem (fst t) [clauIdxOfStart..clauIdxOfEnd]]            -- Element order in [Tree] is kept.
    let scripts' = [ t | t <- scripts, notElem (fst3 t) [clauIdxOfStart..clauIdxOfEnd]]       -- Element order in [Script] is kept.

    if length trees' /= length trees
      then do
        let sqlstat = DS.fromString $ "update " ++ tree_target ++ " set tree = ?, script = ? where serial_num = ?"
        stmt' <- prepareStmt conn sqlstat
        ok <- executeStmt conn stmt' [toMySQLText (nTreeToString trees'), toMySQLText (nScriptToString scripts'), toMySQLInt32 sn]
        close conn
        if (getOkAffectedRows ok == 1)
          then do
            putStrLn $ "dropParsingResult: Parsing result of clauses " ++ show clauIdxOfStart ++ " to clause " ++ show clauIdxOfEnd ++ " were removed from database."
            return True
          else do
            putStrLn "dropParsingResult: Database SQL operation returned an exception."
            return False
      else do
        putStrLn "dropParsingResult: No parsing tree needs to be removed."
        close conn
        return True

{- To be ready for parsing from clause <ci>, parsing trees and scripts of clauses with index bigger than or equal to <ci> are deleted in treebank.
 - The treebank is designated by property 'tree_target' in file Configuration.
 - For skip success, return True; otherwise return False.
 - The function is obsoleted, replaced with dropParsingResult to support parsing the designated range of clauses.
 -}
goBackTo :: SentIdx -> ClauIdx -> IO Bool
goBackTo sn ci = do
    confInfo <- readFile "Configuration"                                        -- Read the local configuration file
    let tree_target = getConfProperty "tree_target" confInfo

    conn <- getConn
    let sqlstat = DS.fromString $ "select tree, script from " ++ tree_target ++ " where serial_num = ?"
    stmt <- prepareStmt conn sqlstat
    (defs, is) <- queryStmt conn stmt [toMySQLInt32 sn]
    row <- S.read is                                   -- Maybe [MySQLText, MySQLText]
    let row' = case row of
                 Just x -> x                           -- [MySQLText, MySQLText]
                 Nothing -> [MySQLText "[]", MySQLText "[]"]                    -- Both "tree" and "script" in treebank will be initialized as '[]'.
    S.skipToEof is                                     -- Consume result set.

--  putStrLn $ "goBackTo: tree: " ++ (fromMySQLText (head row'))

    let trees = readTrees $ fromMySQLText (row'!!0)
    let scripts = readScripts $ fromMySQLText (row'!!1)

    if length trees + 1 < ci
      then do
        putStrLn $ "goBackTo: " ++ show (length trees) ++ " clauses were parsed, skip failed."
        return False
      else if length trees + 1 == ci
             then do
               putStrLn $ "goBackTo: " ++ show (length trees) ++ " clauses were parsed, skip succeeded."
               return True
             else do
               let trees' = take (ci - 1) trees
               let scripts' = take (ci - 1) scripts
               let sqlstat = DS.fromString $ "update " ++ tree_target ++ " set tree = ?, script = ? where serial_num = ?"
               stmt' <- prepareStmt conn sqlstat
               ok <- executeStmt conn stmt' [toMySQLText (nTreeToString trees'), toMySQLText (nScriptToString scripts'), toMySQLInt32 sn]
               if (getOkAffectedRows ok == 1)
               then do
                 putStrLn $ "goBackTo: " ++ show (length trees') ++ " clauses were parsed, skip succeeded."
                 return True
               else do
                 putStrLn $ "goBackTo: skip failed!"
                 return False

{- Parse a sentence, here every clause is a String. Parameter 'sn' is the value of 'serial_num' in database Table 'Corpus', and parameter 'skn' is the number of skipped clauses.
 - If a certain clause is not finished in parsing, return False to skip the remaining clauses.
 -}
parseSent' :: SentIdx -> Int -> [String] -> IO Bool
parseSent' _ _ [] = return True
parseSent' sn skn cs = do
    finFlag <- parseSent' sn skn (take (length cs - 1) cs)
    let clauIdx = skn + length cs
    putStrLn $ "  ===== Clause No." ++ show clauIdx ++ " ====="
    if finFlag                                                                  -- True means sentential parsing has been finished.
      then do
        let nPCs = initPhraCate $ getNCate $ words (last cs)
        putStr "Before parsing: "
        showNPhraCateLn nPCs
        putStr "Word semantic sequence: "
        showNSeman nPCs
        let clauTag = (sn, clauIdx)
        rtbPCs' <- parseClause clauTag [] nPCs []                               -- Parse begins with empty '[[Rule]]' and empty 'banPCs'
        if rtbPCs' == ([],[],[])
          then return False                                                     -- False means the current clause is terminated manually.
          else do
            storeClauseParsing sn clauIdx rtbPCs'
            return True                                                         -- Add the parsing result of this clause into database.
      else do
        putStrLn "  Skip!"
        return False

--  Add the parsing result of a clause into database. Now, parameter <clauIdx> has not been used for checking.
storeClauseParsing :: SentIdx -> ClauIdx -> ([[Rule]], [PhraCate], [[PhraCate]]) -> IO ()
storeClauseParsing sn clauIdx rtbPCs = do
    confInfo <- readFile "Configuration"                                        -- Read configuration file
    let tree_target = getConfProperty "tree_target" confInfo
    let sqlstat = DS.fromString $ "select tree, script from " ++ tree_target ++ " where serial_num = ?"

    conn <- getConn
    stmt <- prepareStmt conn "select tree, script from corpus where serial_num = ?"
    (defs, is) <- queryStmt conn stmt [toMySQLInt32 sn]
    row <- S.read is                                   -- Maybe [MySQLText, MySQLText]
    let row' = case row of
                 Just x -> x                           -- [MySQLText, MySQLText]
                 Nothing -> error "goBackTo: No row was read."
    S.skipToEof is                                     -- Skip to end-of-stream.
    closeStmt conn stmt

    let tree = readTrees $ fromMySQLText (head row')
    let script = readScripts $ fromMySQLText (last row')
    putStrLn $ "storeClauseParsing: tree: " ++ (nTreeToString tree)
    putStrLn $ "storeClauseParsing: script: " ++ (nScriptToString script)

    let tree' = quickSort4Tree $ tree ++ [(clauIdx, snd3 rtbPCs)]
    let script' = quickSort4Script $ script ++ [(clauIdx, fst3 rtbPCs, thd3 rtbPCs)]

    putStrLn $ "storeClauseParsing: tree': " ++ (nTreeToString tree')
    putStrLn $ "storeClauseParsing: script': " ++ (nScriptToString script')

    stmt' <- prepareStmt conn "update corpus set tree = ?, script = ? where serial_num = ?"
    ok <- executeStmt conn stmt' [toMySQLText (nTreeToString tree'), toMySQLText (nScriptToString script'), toMySQLInt32 sn]
    let rn = getOkAffectedRows ok
    close conn
    if (rn /= 0)
      then putStrLn $ "storeClauseParsing: " ++ show rn ++ " row(s) were modified."
      else error "storeClauseParsing: update failed!"

{- Parsing a clause is a human-machine interactive recursive process.
   Input: A sequence of [Rule], a sequence of phrasal categories, and a sequence of banned phrasal categories;
   Algo.:
   (1) Do one trip of transition;
   (2) If creating new phrasal categories, append rules used in this trip to [[Rule]], take resultant phrases and accumulated banned phrase sets as input, go (1);
       Otherwise, return the triple ([[Rule]], resultant tree PCs, accumulated banned phrase sets).
 -}

parseClause :: ClauTag -> [[Rule]] -> [PhraCate] -> [BanPCs] -> IO ([[Rule]],[PhraCate],[BanPCs])
parseClause clauTag rules nPCs banPCSets = do
    rtbPCs <- doTrans clauTag [] nPCs banPCSets           -- Every trip of transition begins with empty rule set.
                                               -- <rtbPCs> ::= ([Rule], resultant tree PCs, accumulated banned phrasal sets)
                                               -- [Rule] is the set of rules used in this trip of transition.
    if rtbPCs == ([],[],[])
      then return ([],[],[])                   -- Return ([],[],[]) as the terminating flag.
      else if nPCs /= (snd3 rtbPCs)
        then parseClause clauTag (rules ++ [fst3 rtbPCs]) (snd3 rtbPCs) (thd3 rtbPCs)    -- Do the next trip of transition
                                               -- with appended rules, resultant PCs, and accumulated banned PC sets.
        else do                                -- Phrasal closure has been formed.
          putStrLn $ "Num. of phrasal categories in closure is " ++ (show $ length nPCs)
          showNPhraCateLn (sortPhraCateBySpan nPCs)
          let spls = divPhraCateBySpan (nPCs)
          putStrLn "  ##### Parsing Tree #####"
          showTreeStru spls spls
          return (rules ++ [fst3 rtbPCs], snd3 rtbPCs, thd3 rtbPCs)

{- Parse a sentence which includes multiple clauses. Parsing can start from a certain clause, and end with a certain clause. The first parameter is the value of 'serial_num' in database Table 'corpus'. The parsing results would not be stored back to database, so the 'sn' is useless now.
 -}
parseSentWithAllLexRules :: SentIdx -> [String] -> IO ()
parseSentWithAllLexRules sn cs = do
    hSetBuffering stdin LineBuffering                  -- Open input buffering
    let clauNum = length cs
    putStr $ " There are " ++ show clauNum ++ " clauses in total, from which clause to start: [RETURN for 1] "
    clauIdxOfStart <- getLine
    putStr $ " With which clause to end: [RETURN for " ++ show clauNum ++ "] "
    clauIdxOfEnd <- getLine
    if clauIdxOfStart /= ""                            -- Not RETURN
      then do
        let cis = read clauIdxOfStart :: Int
        if cis < 1 || cis > clauNum
          then do
            putStrLn $ "The start clause " ++ show cis ++ " does not exist! Input again!"
            parseSentWithAllLexRules sn cs
          else if clauIdxOfEnd /= ""
            then do
              let cie = read clauIdxOfEnd :: Int
              if cie < 1 || cie > clauNum
                then do
                  putStrLn $ "The end clause " ++ show cie ++ " does not exist! Input again!"
                  parseSentWithAllLexRules sn cs
                else if cis > cie
                  then do
                    putStrLn $ "The start clause is after the end clause! Input again!"
                    parseSentWithAllLexRules sn cs
                  else do
                    let cs2 = drop (cis - 1) $ take cie cs
                    parseSentWithAllLexRules' sn cis cs2                  -- Parse clauses from 'cis' to 'cie'.
            else do
              let cie = clauNum
              if cis > cie
                then do
                  putStrLn $ "The start clause is after the end clause! Input again!"
                  parseSentWithAllLexRules sn cs
                else do
                  let cs2 = drop (cis - 1) $ take cie cs
                  parseSentWithAllLexRules' sn cis cs2                    -- Parse clauses from 'cis' to 'cie'.
      else do                                       -- When input 'cis', user presses RETURN.
        let cis = 1                                 -- Here, clausal indices start from 1.
        if clauIdxOfEnd /= ""
          then do
            let cie = read clauIdxOfEnd :: Int
            if cie < 1 || cie > clauNum
              then do
                putStrLn $ "The end clause " ++ show cie ++ " does not exist! Input again!"
                parseSentWithAllLexRules sn cs
              else if cis > cie
                then do
                  putStrLn $ "The start clause is after the end clause! Input again!"
                  parseSentWithAllLexRules sn cs
                else do
                  let cs2 = drop (cis - 1) $ take cie cs
                  parseSentWithAllLexRules' sn cis cs2                    -- Parse clauses from 'cis' to 'cie'.
          else do
            let cie = clauNum                       -- When input 'cie', user presses RETURN
            if cie < 1 || cie > clauNum
              then do
                putStrLn $ "The end clause " ++ show cie ++ " does not exist! Input again!"
                parseSentWithAllLexRules sn cs
              else if cis > cie
                then do
                  putStrLn $ "The start clause is after the end clause! Input again!"
                  parseSentWithAllLexRules sn cs
                else do
                  let cs2 = drop (cis - 1) $ take cie cs
                  parseSentWithAllLexRules' sn cis cs2                    -- Parse clauses from 'cis' to 'cie'.

{- Parse a sentence, here every clause is a String. Parameter 'sn' is the value of 'serial_num' in database Table 'Corpus', but not used now.
 - 'cis' is the clausal index of first clause to parse.
 -}
parseSentWithAllLexRules' :: SentIdx -> ClauIdx -> [String] -> IO ()
parseSentWithAllLexRules' _ _ [] = putStrLn ""
parseSentWithAllLexRules' sn cis cs = do
    parseSentWithAllLexRules' sn cis $ take (length cs - 1) cs
    let clauIdx = cis + length cs - 1
    putStrLn $ "  ===== Clause No." ++ show clauIdx ++ " ====="
    let nPCs = initPhraCate $ getNCate $ words (last cs)
    putStr "Before parsing: "
    showNPhraCateLn nPCs
    putStr "Word semantic sequence: "
    showNSeman nPCs

    pcClo <- parseClauseWithAllLexRules 1 nPCs                      -- pcClo is the closure under LexRule(nPCs).
    putStrLn $ "The forest closure includes " ++ show (length pcClo) ++ " phrasal categoires, which are:"
    showNPhraCateLn pcClo

    let sp = getNuOfInputCates pcClo - 1
    putStrLn $ "Maximal span is " ++ show sp

    let roots = findCate (0, sp) pcClo
    let forest = growForest lexRule [[t]|t<-roots] pcClo
    putStrLn ("        Parsing Tree No.1 ~ No." ++ show (length forest))
    showForest forest
    putStr "\n"
    putStrLn ("        Tree Structure No.1 ~ No." ++ show (length forest))
    showForestWithTreeStru forest

{- Here is a parsing process for a clause using all lexcial rules, which still is a recursive process.
   Input: The transitive index, and a sequence of phrasal categories.
   Algo.:
   (1) Do one trip of transition;
   (2) If creating new phrasal categories, take resultant phrases as input, go (1); Otherwise, return the resultant forest PCs.
 -}
parseClauseWithAllLexRules :: Int -> [PhraCate] -> IO [PhraCate]
parseClauseWithAllLexRules transIdx nPCs = do
    let nPCs2 = sortPhraCateBySpan $ removeDup $ trans lexRule nPCs []          -- Every trip of transition begins with whole rule set.
                                                                    -- <rtbPCs> ::= ([Rule], resultant tree PCs, accumulated banned PCs)
                                                                    -- [Rule] is the set of rules used in this trip of transition.
    putStrLn $ "The result after " ++ show transIdx ++ "th transtion contains " ++ show (length nPCs2) ++ " phrasal categories, which are:"
    showNPhraCateLn nPCs2
    if nPCs2 /= nPCs
      then parseClauseWithAllLexRules (transIdx + 1) nPCs2          -- Do the next trip of transition with resultant PCs.
      else return nPCs

{- Do a trip of transition, insert or update related structural genes in Table stru_gene, and return the category-converted rules in this trip, and the resultant stub tree and accumulated banned phrase sets.
 - If transitive parsing is to terminated, namely selecting 'e' at inquiring rule switches, returnes ([],[],[]) as the terminating flag.
 -}
doTrans :: ClauTag -> [Rule] -> [PhraCate] -> [BanPCs] -> IO ([Rule], Stub, [BanPCs])
doTrans clauTag onOff nPCs banPCSets = do
    showOnOff onOff
    ruleSwitchOk <- getLineUntil "Are rule switches ok? [y/n/e]: ('y' or RETURN for yes, 'n' for no, and 'e' for exit) " ["y","n","e"] True
    case ruleSwitchOk of
      "n" -> do                         -- Press key 'n'
                putStrLn "Enable or disable rules among"
                putStrLn "  S/s, P/s, O/s, A/s, Hn/s, N/s,"
                putStrLn "  S/v, O/v, A/v, Hn/v, D/v, Cn/v, Cv/v, N/v, P/vt, OE/vt, Vt/vi, A/vd,"
                putStrLn "  S/a, P/a, V/a, O/a, D/a, Da/a, Cn/a, Cv/a, Ca/a, Hn/a, N/a,"
                putStrLn "  P/n, V/n, A/n, Cn/n, Cv/n, D/n, Da/n, ADJ/n, S/nd, O/nd, Hn/nd,"
                putStrLn "  S/d, O/d, A/d, Hn/d, Cv/d, N/d, ADJ/d, Da/d, Ds/d, Dx/d, Doe/d,"
                putStrLn "  D/p,"
                putStrLn "  O/oe, Hn/oe, N/oe,"
                putStrLn "  N/pe,"
                putStrLn "  A/q,"
                putStrLn "  Jf/c, Jb/c"
                putStrLn "  and U3d/u3,"
                putStr "  for instance, +O/s, -A/v: (RETURN for skip) "
                ruleSwitchStr <- getLine                    -- Get new onOff from input, such as "+O/s,-A/v"
                let rws = splitAtDeliThrowSpace ',' ruleSwitchStr     -- ["+O/s","-A/v"]
                if [] == [x| x <- rws, notElem (head x) ['+','-'] || notElem (tail x) [
                  "S/s", "P/s", "O/s", "A/s", "Hn/s", "N/s",
                  "S/v", "O/v", "A/v", "Hn/v", "D/v", "Cn/v", "Cv/v", "N/v", "P/vt", "OE/vt", "Vt/vi", "A/vd",
                  "S/a", "O/a", "Hn/a", "N/a", "P/a", "V/a", "D/a", "Da/a", "Cv/a", "Cn/a", "Ca/a",
                  "P/n", "V/n", "A/n", "Cn/n", "Cv/n", "D/n", "Da/n", "ADJ/n", "S/nd", "O/nd", "Hn/nd",
                  "S/d", "O/d", "A/d", "Hn/d", "Cv/d", "N/d", "ADJ/d", "Da/d", "Ds/d", "Dx/d", "Doe/d",
                  "D/p",
                  "O/oe", "Hn/oe", "N/oe",
                  "N/pe",
                  "A/q",
                  "Jf/c", "Jb/c",
                  "U3d/u3"]]
                   then do
                     let newOnOff = updateOnOff onOff rws
                     doTrans clauTag newOnOff nPCs banPCSets           -- Redo this trip of transition by modifying rule switches.
                   else do
                     putStrLn "Rule switch expression error. Consider again!"
                     doTrans clauTag onOff nPCs banPCSets
      "y" -> do                                                 -- Press key 'y' or directly press RETURN
               let nPCs2 = trans onOff nPCs banPCSets           -- Without pruning, get transitive result.
               putStr "Transitive result before pruning: "
               showNPhraCateLn (sortPhraCateBySpan nPCs2)
               putStr "Banned phrases: "
               showNPhraCateListLn (map sortPhraCateBySpan' banPCSets)     -- Can't use <sortPhraCateBySpan> on BanPCs.

               let pcps = getOverlap nPCs2                  -- [(PhraCate, PhraCate)]
               overPairs <- updateStruGene clauTag nPCs2 [] pcps                -- IO [OverPair], namely IO [(PhraCate, PhraCate, Prior)], record overlapping pairs for pruning.
               nbPCs <- transWithPruning onOff nPCs banPCSets overPairs         -- Get transitive result with pruning.

               putStr "New phrases after pruning: "
               showNPhraCateLn [pc | pc <- fst nbPCs, notElem4Phrase pc nPCs]

               putStr "Transitive result after pruning: "
               showNPhraCateLn (sortPhraCateBySpan (fst nbPCs))
               putStr "Banned phrases: "
               showNPhraCateListLn (snd nbPCs)                                  -- Banned-phrasal sets after updated.

               let prompt = "This trip of transition is ok? [y/n/e] (RETURN for 'y', 'e' for exit): "
               transOk <- getLineUntil prompt ["y","n","e"] True
               case transOk of
                 "y" ->  return (onOff,(fst nbPCs),(snd nbPCs))
                 "n" ->  do
                           rollbackStruGene clauTag nPCs2 overPairs             -- Rollback syntax ambiguity resolution samples base.
                           doTrans clauTag onOff nPCs banPCSets                -- Redo this trip of transition.
                 "e" ->  return ([],[],[])                                      -- Return from doTrans, and indicate this is terminating exit.
      "e" -> return ([],[],[])                                                  -- Return from doTrans, and indicate this is terminating exit.

{- Insert or update related structural genes in database, and recursively create overlapping pairs.
   For every pair of overlapping phrases, its overlap type, left- and right-extend phrases are found in a given set
   of phrases.
 -}
updateStruGene :: ClauTag -> [PhraCate] -> [OverPair] -> [(PhraCate,PhraCate)] -> IO [OverPair]
updateStruGene _ _ overPairs [] = do
    putStrLn ""                        -- To make output easy to read.
    return overPairs                   -- Return the collected resolution results.
updateStruGene clauTag nPCs overPairs (pcp:pcps) = do
    newOverPairs <- updateStruGene' clauTag contextOfSG overPairs       -- Update structural gene in Table stru_gene
    updateStruGene clauTag nPCs newOverPairs pcps
    where
      lop = fst pcp                                      -- PhraCate
      rop = snd pcp                                      -- PhraCate
      ot = getOverType nPCs lop rop                      -- Overlapping type
      lopt = findATree lop nPCs                          -- Left overlapping phrase tree :: BiTree PhraCate
      ropt = findATree rop nPCs                          -- Right overlapping phrase tree :: BiTree PhraCate
      leps = getPhraByEnd (stOfCate lop - 1) nPCs        -- All left-extend phrases
      reps = getPhraByStart (enOfCate rop + 1) nPCs      -- All right-entend phrases
      contextOfSG = (leps,lop,rop,reps,ot,lopt,ropt)

{- Update structural genes related with a certain pair of overlapping phrases, add the overlapping pair to the input
 - list of OverPair(s), then return the new OverPair list.
 - Model Stru_Gene :: (LeftExtend, LeftOver, RightOver, RightExtend, Overtype, Prior, HitCount, PriorExCount)
 - Model stru_gene_202408 :: (LeftExtend, LeftOver, RightOver, RightExtend, Overtype, ClauTagPrior, LpHitCount, RpHitCount, NothHitCount)
 - Model stru_gene3_202508 :: (LeftExtend, LeftOverTree, RightOverTree, RightExtend, Overtype, ClauTagPrior, LpHitCount, RpHitCount, NothHitCount)
 - Model stru_gene3a_202508 :: (LeftOverTree, RightOverTree, ClauTagPrior, LpHitCount, RpHitCount, NothHitCount)
 -}
updateStruGene' :: ClauTag -> ([PhraCate],PhraCate,PhraCate,[PhraCate],OverType,BiTree PhraCate,BiTree PhraCate) -> [OverPair] -> IO [OverPair]
updateStruGene' clauTag contextOfSG overPairs = do
    confInfo <- readFile "Configuration"
    let syntax_ambig_resol_model = getConfProperty "syntax_ambig_resol_model" confInfo          -- Find ambiguity resolution model, namely table name in MySQL.

    let leftExtend = fst7 contextOfSG
    let leftOver = snd7 contextOfSG
    let rightOver = thd7 contextOfSG
    let rightExtend = fth7 contextOfSG
    let overType = fif7 contextOfSG
    let leftOverTree = sth7 contextOfSG
    let rightOverTree = svt7 contextOfSG

    putStr "Find structural fragment: "
    showStruFrag leftExtend leftOver rightOver rightExtend overType

    conn <- getConn
    case syntax_ambig_resol_model of
      "stru_gene" -> do
        let le = map ((!!0) . ctpOfCate) leftExtend         -- [(Category,Tag,PhraStru)] of left-extended phrases
        let lo = (ctpOfCate leftOver)!!0                    -- (Category,Tag,PhraStru) of left-overlapping phrase
        let ro = (ctpOfCate rightOver)!!0                   -- (Category,Tag,PhraStru) of right-overlapping phrase
        let re = map ((!!0) . ctpOfCate) rightExtend        -- [(Category,Tag,PhraStru)] of right-extended phrases
        let ot = overType                                   -- Overlap type

        let lev = doubleBackSlash (show le)                 -- Get values to insert them into MySql Table
        let lov = doubleBackSlash (show lo)
        let rov = doubleBackSlash (show ro)
        let rev = doubleBackSlash (show re)
        let otv = show ot
{-
        putStrLn $ "Inquire structural gene: leftExtend = '" ++ show le ++ "' && " ++
                                            "leftOver = '" ++ show lo ++ "' && " ++
                                            "rightOver = '" ++ show ro ++ "' && " ++
                                            "rightExtend = '" ++ show re ++ "' && " ++
                                            "overType = "  ++ show ot
 -}

        let sqlstat = read (show ("select id, prior, hitCount, priorExCount from stru_gene where leftExtend = '" ++ lev ++ "' && " ++ "leftOver = '" ++ lov ++ "' && " ++ "rightOver = '" ++ rov ++ "' && " ++ "rightExtend = '" ++ rev ++ "' && " ++ "overType = " ++ otv)) :: Query
        stmt <- prepareStmt conn sqlstat
        (defs, is) <- queryStmt conn stmt []

        rows <- S.toList is
        if rows /= []
          then
            if length rows > 1
              then do
                close conn                           -- Close MySQL connection.
                error "updateStruGene': Find duplicate structural genes."
              else do
                let id = fromMySQLInt32U ((rows!!0)!!0)
                let prior = fromMySQLText ((rows!!0)!!1)
                let hitCount = fromMySQLInt32U ((rows!!0)!!2)
                let priorExCount = fromMySQLInt16U ((rows!!0)!!3)
                putStrLn $ "updateStruGene': (" ++ show id ++ ") prior: " ++ prior ++ ", hitCount: " ++ show hitCount ++ ", priorExCount: " ++ show priorExCount
                putStr "Is the priority right? [y/n]: (RETURN for 'y') "
                input <- getLine
                if (input == "y" || input == "")       -- Press key 'y' or directly press RETURN.
                  then do
                    resetStmt conn stmt
                    let sqlstat = read (show ("update stru_gene set hitCount = ? where id = '" ++ show id ++ "'")) :: Query
                    stmt <- prepareStmt conn sqlstat
                    executeStmt conn stmt [toMySQLInt32U (hitCount + 1)]            -- Add column 'hitCount' by 1 of structural gene.
                    close conn                       -- Close MySQL connection.
                    return ((leftOver, rightOver, read prior::Prior):overPairs)
                  else do
                    newPriorFlag <- getLineUntil "please input new priority [Lp/Rp/Noth]: ('1' or RETURN for 'Lp', '2' for 'Rp', '3' for 'Noth') " ["1", "2", "3"] True
                    let newPrior = case newPriorFlag of
                                     "1" -> "Lp"
                                     "2" -> "Rp"
                                     "3" -> "Noth"
                    if newPrior /= prior                   -- Ask to modify column 'prior'.
                      then do
                        resetStmt conn stmt
                        let sqlstat = read (show ("update stru_gene set prior = ?, hitCount = ?, priorExCount = ? where id = '" ++ show id ++ "'")) :: Query
                        stmt <- prepareStmt conn sqlstat
                        executeStmt conn stmt [toMySQLText newPrior, toMySQLInt32U 0, toMySQLInt16U (priorExCount + 1)]
                                                                                -- Update columns 'prior', 'hitCount', and 'priorExCount' of structural gene.
                        close conn                                                  -- Close MySQL connection.
                        return ((leftOver, rightOver, read newPrior::Prior):overPairs)
                      else do                              -- Actually, the priority is not asked to change.
                        resetStmt conn stmt
                        let sqlstat = read (show ("update stru_gene set hitCount = ? where id = '" ++ show id ++ "'")) :: Query
                        stmt <- prepareStmt conn sqlstat
                        executeStmt conn stmt [toMySQLInt32U (hitCount + 1)]       -- Add column 'hitCount' by 1 of structural gene.
                        close conn                                                 -- Close MySQL connection.
                        return ((leftOver, rightOver, read newPrior::Prior):overPairs)
          else do
            putStrLn "Inquire failed."
            newPriorFlag <- getLineUntil "please input new priority [Lp/Rp/Noth]: ('1' or RETURN for 'Lp', '2' for 'Rp', '3' for 'Noth') " ["1", "2", "3"] True
            let newPrior = case newPriorFlag of
                             "1" -> "Lp"
                             "2" -> "Rp"
                             "3" -> "Noth"
            let sqlstat = read (show ("insert stru_gene (leftExtend,leftOver,rightOver,rightExtend,overType,prior) values ('" ++ lev ++ "','" ++ lov ++ "','" ++ rov ++ "','" ++ rev ++ "'," ++ otv ++ ",'" ++ newPrior ++ "')")) :: Query
            stmt1 <- prepareStmt conn sqlstat
            ok <- executeStmt conn stmt1 []             -- Insert the described structural gene.
            putStrLn $ "updateStruGene': Last inserted row with ID " ++ show (getOkLastInsertID ok)
            close conn                                   -- Close MySQL connection.
            return ((leftOver, rightOver, read newPrior::Prior):overPairs)

      x | elem x ["stru_gene_202408", "stru_gene_202412", "stru_gene_202501"] -> do       -- Multimodel
        let le = map ((!!0) . ctpsOfCate) leftExtend         -- [(Category,Tag,PhraStru,Span)] of left-extended phrases
        let lo = (ctpsOfCate leftOver)!!0                    -- (Category,Tag,PhraStru,Span) of left-overlapping phrase
        let ro = (ctpsOfCate rightOver)!!0                   -- (Category,Tag,PhraStru,Span) of right-overlapping phrase
        let re = map ((!!0) . ctpsOfCate) rightExtend        -- [(Category,Tag,PhraStru,Span)] of right-extended phrases
        let ot = overType                                    -- Overlap type

        let lev = doubleBackSlash (show le)                 -- Get values to insert them into MySql Table
        let lov = doubleBackSlash (show lo)
        let rov = doubleBackSlash (show ro)
        let rev = doubleBackSlash (show re)
        let otv = show ot

        let sqlstat = read (show ("select id, clauTagPrior, lpHitCount, rpHitCount, nothHitCount from "
                                   ++ syntax_ambig_resol_model
                                   ++ " where leftExtend = '" ++ lev ++ "' && "
                                   ++ "leftOver = '" ++ lov ++ "' && "
                                   ++ "rightOver = '" ++ rov ++ "' && "
                                   ++ "rightExtend = '" ++ rev ++ "' && "
                                   ++ "overType = " ++ otv)) :: Query
        stmt <- prepareStmt conn sqlstat
        (defs, is) <- queryStmt conn stmt []

        rows <- S.toList is
        if rows /= []
          then
            if length rows > 1
              then do
                close conn                           -- Close MySQL connection.
                error "updateStruGene': Find duplicate structural genes."
              else do
                let id = fromMySQLInt32U ((rows!!0)!!0)
                let clauTagPriorListStr = fromMySQLText ((rows!!0)!!1)
                let lpHitCount = fromMySQLInt16U ((rows!!0)!!2)
                let rpHitCount = fromMySQLInt16U ((rows!!0)!!3)
                let nothHitCount = fromMySQLInt16U ((rows!!0)!!4)

                putStrLn $ "updateStruGene': (" ++ show id ++ ") clauTagPrior: " ++ clauTagPriorListStr ++ ", lpHitCount: " ++ show lpHitCount ++ ", rpHitCount: " ++ show rpHitCount ++ ", nothHitCount: " ++ show nothHitCount
                let clauTagPriorList = stringToCTPList clauTagPriorListStr
                let priorList = map snd $ filter ((== clauTag) . fst) clauTagPriorList          -- The selected priorities
                priorFlag <- getLineUntil "please select priority from [Lp/Rp/Noth] ('1' or RETURN for Lp, '2' for Rp, '3' for Noth): " ["1","2","3"] True
                let prior = case priorFlag of
                              "1" -> Lp :: Prior
                              "2" -> Rp :: Prior
                              "3" -> Noth :: Prior

                if notElem prior priorList
                  then do
                    resetStmt conn stmt
                    let sqlstat = case prior of
                                    Lp -> read (show ("update " ++ syntax_ambig_resol_model ++ " set clauTagPrior = ?, lpHitCount = ? where id = '" ++ show id ++ "'")) :: Query
                                    Rp -> read (show ("update " ++ syntax_ambig_resol_model ++ " set clauTagPrior = ?, rpHitCount = ? where id = '" ++ show id ++ "'")) :: Query
                                    Noth -> read (show ("update " ++ syntax_ambig_resol_model ++ " set clauTagPrior = ?, nothHitCount = ? where id = '" ++ show id ++ "'")) :: Query
                    stmt <- prepareStmt conn sqlstat
                    let newClauTagPriorList = (clauTag, prior):clauTagPriorList     -- Add with one ClauTagPrior value.
                    oks <- case prior of
                             Lp -> executeStmt conn stmt [toMySQLText (show newClauTagPriorList), toMySQLInt16U (lpHitCount + 1)]       -- Add 'lpHitCount' by 1.
                             Rp -> executeStmt conn stmt [toMySQLText (show newClauTagPriorList), toMySQLInt16U (rpHitCount + 1)]       -- Add 'rpHitCount' by 1.
                             Noth -> executeStmt conn stmt [toMySQLText (show newClauTagPriorList), toMySQLInt16U (nothHitCount + 1)]   -- Add 'nothHitCount' by 1.
--                  putStrLn $ "updateStruGene': getOkLastInsertID = " ++ show (getOkLastInsertID oks)
                    putStrLn $ "updateStruGene': " ++ show (clauTag, prior) ++ " was inserted into row " ++ show id
                  else putStrLn $ "updateStruGene': " ++ show (clauTag, prior) ++ " was hit in row " ++ show id
                close conn                       -- Close MySQL connection.
                return ((leftOver, rightOver, prior):overPairs)
          else do
            putStrLn "Inquire failed."
            priorFlag <- getLineUntil "please select priority from [Lp/Rp/Noth] ('1' or RETURN for 'Lp', '2' for 'Rp', '3' for 'Noth'): " ["1","2","3"] True
            let prior = case priorFlag of
                          "1" -> Lp :: Prior
                          "2" -> Rp :: Prior
                          "3" -> Noth :: Prior
            let clauTagPrior = (clauTag, prior)
            let sqlstat = case prior of
                            Lp -> read (show ("insert " ++ syntax_ambig_resol_model ++ " (leftExtend,leftOver,rightOver,rightExtend,overType,clauTagPrior,lpHitCount) values ('" ++ lev ++ "','" ++ lov ++ "','" ++ rov ++ "','" ++ rev ++ "'," ++ otv ++ ",'[" ++ show clauTagPrior ++ "]',1)")) :: Query
                            Rp -> read (show ("insert " ++ syntax_ambig_resol_model ++ " (leftExtend,leftOver,rightOver,rightExtend,overType,clauTagPrior,rpHitCount) values ('" ++ lev ++ "','" ++ lov ++ "','" ++ rov ++ "','" ++ rev ++ "'," ++ otv ++ ",'[" ++ show clauTagPrior ++ "]',1)")) :: Query
                            Noth -> read (show ("insert " ++ syntax_ambig_resol_model ++ " (leftExtend,leftOver,rightOver,rightExtend,overType,clauTagPrior,nothHitCount) values ('" ++ lev ++ "','" ++ lov ++ "','" ++ rov ++ "','" ++ rev ++ "'," ++ otv ++ ",'[" ++ show clauTagPrior ++ "]',1)")) :: Query
            stmt1 <- prepareStmt conn sqlstat
            oks <- executeStmt conn stmt1 []             -- Insert the described structural gene.
            putStrLn $ "updateStruGene': Last inserted row with ID " ++ show (getOkLastInsertID oks)
            close conn                                   -- Close MySQL connection.
            return ((leftOver, rightOver, prior):overPairs)

      x | elem x ["stru_gene3_202508"] -> do              -- Multimodel
        let le = map ((!!0) . ctpsOfCate) leftExtend      -- [(Category,Tag,PhraStru,Span)] of left-extended phrases
        let lo = (ctpsOfCate leftOver)!!0                 -- (Category,Tag,PhraStru,Span) of left-overlapping phrase
        let ro = (ctpsOfCate rightOver)!!0                -- (Category,Tag,PhraStru,Span) of right-overlapping phrase
        let re = map ((!!0) . ctpsOfCate) rightExtend     -- [(Category,Tag,PhraStru,Span)] of right-extended phrases
        let ot = overType                                 -- Overlap type
        let lot = phraCateTree2PhraSynTree leftOverTree   -- BiTree PhraSyn
        let rot = phraCateTree2PhraSynTree rightOverTree  -- BiTree PhraSyn

        let lev = doubleBackSlash (show le)               -- Get values to insert them into MySql Table
        let lotv = doubleBackSlash (show lot)
        let rotv = doubleBackSlash (show rot)
        let rev = doubleBackSlash (show re)
        let otv = show ot

        let sqlstat = read (show ("select id, clauTagPrior, lpHitCount, rpHitCount, nothHitCount from "
                                  ++ syntax_ambig_resol_model
                                  ++ " where leftExtend = '" ++ lev ++ "' && "
                                  ++ "leftOverTree = '" ++ lotv ++ "' && "
                                  ++ "rightOverTree = '" ++ rotv ++ "' && "
                                  ++ "rightExtend = '" ++ rev ++ "' && "
                                  ++ "overType = " ++ otv)) :: Query
        stmt <- prepareStmt conn sqlstat
        (defs, is) <- queryStmt conn stmt []

        rows <- S.toList is
        if rows /= []
          then
            if length rows > 1
              then do
                close conn                           -- Close MySQL connection.
                error "updateStruGene': Find duplicate structural genes."
              else do
                let id = fromMySQLInt32U ((rows!!0)!!0)
                let clauTagPriorListStr = fromMySQLText ((rows!!0)!!1)
                let lpHitCount = fromMySQLInt16U ((rows!!0)!!2)
                let rpHitCount = fromMySQLInt16U ((rows!!0)!!3)
                let nothHitCount = fromMySQLInt16U ((rows!!0)!!4)

                putStrLn $ "updateStruGene': (" ++ show id ++ ") clauTagPrior: " ++ clauTagPriorListStr ++ ", lpHitCount: " ++ show lpHitCount ++ ", rpHitCount: " ++ show rpHitCount ++ ", nothHitCount: " ++ show nothHitCount
                let clauTagPriorList = stringToCTPList clauTagPriorListStr
                let priorList = map snd $ filter ((== clauTag) . fst) clauTagPriorList          -- The selected priorities
                priorFlag <- getLineUntil "please select priority from [Lp/Rp/Noth] ('1' or RETURN for Lp, '2' for Rp, '3' for Noth): " ["1","2","3"] True
                let prior = case priorFlag of
                              "1" -> Lp :: Prior
                              "2" -> Rp :: Prior
                              "3" -> Noth :: Prior

                if notElem prior priorList
                  then do
                    resetStmt conn stmt
                    let sqlstat = case prior of
                                    Lp -> read (show ("update " ++ syntax_ambig_resol_model ++ " set clauTagPrior = ?, lpHitCount = ? where id = '" ++ show id ++ "'")) :: Query
                                    Rp -> read (show ("update " ++ syntax_ambig_resol_model ++ " set clauTagPrior = ?, rpHitCount = ? where id = '" ++ show id ++ "'")) :: Query
                                    Noth -> read (show ("update " ++ syntax_ambig_resol_model ++ " set clauTagPrior = ?, nothHitCount = ? where id = '" ++ show id ++ "'")) :: Query
                    stmt <- prepareStmt conn sqlstat
                    let newClauTagPriorList = (clauTag, prior):clauTagPriorList     -- Add with one ClauTagPrior value.
                    oks <- case prior of
                             Lp -> executeStmt conn stmt [toMySQLText (show newClauTagPriorList), toMySQLInt16U (lpHitCount + 1)]       -- Add 'lpHitCount' by 1.
                             Rp -> executeStmt conn stmt [toMySQLText (show newClauTagPriorList), toMySQLInt16U (rpHitCount + 1)]       -- Add 'rpHitCount' by 1.
                             Noth -> executeStmt conn stmt [toMySQLText (show newClauTagPriorList), toMySQLInt16U (nothHitCount + 1)]   -- Add 'nothHitCount' by 1.
--                  putStrLn $ "updateStruGene': getOkLastInsertID = " ++ show (getOkLastInsertID oks)
                    putStrLn $ "updateStruGene': " ++ show (clauTag, prior) ++ " was inserted into row " ++ show id
                  else putStrLn $ "updateStruGene': " ++ show (clauTag, prior) ++ " was hit in row " ++ show id
                close conn                       -- Close MySQL connection.
                return ((leftOver, rightOver, prior):overPairs)
          else do
            putStrLn "Inquire failed."
            priorFlag <- getLineUntil "please select priority from [Lp/Rp/Noth] ('1' or RETURN for 'Lp', '2' for 'Rp', '3' for 'Noth'): " ["1","2","3"] True
            let prior = case priorFlag of
                          "1" -> Lp :: Prior
                          "2" -> Rp :: Prior
                          "3" -> Noth :: Prior
            let clauTagPrior = (clauTag, prior)
            let sqlstat = case prior of
                            Lp -> read (show ("insert " ++ syntax_ambig_resol_model ++ " (leftExtend,leftOverTree,rightOverTree,rightExtend,overType,clauTagPrior,lpHitCount) values ('" ++ lev ++ "','" ++ lotv ++ "','" ++ rotv ++ "','" ++ rev ++ "'," ++ otv ++ ",'[" ++ show clauTagPrior ++ "]',1)")) :: Query
                            Rp -> read (show ("insert " ++ syntax_ambig_resol_model ++ " (leftExtend,leftOverTree,rightOverTree,rightExtend,overType,clauTagPrior,rpHitCount) values ('" ++ lev ++ "','" ++ lotv ++ "','" ++ rotv ++ "','" ++ rev ++ "'," ++ otv ++ ",'[" ++ show clauTagPrior ++ "]',1)")) :: Query
                            Noth -> read (show ("insert " ++ syntax_ambig_resol_model ++ " (leftExtend,leftOverTree,rightOverTree,rightExtend,overType,clauTagPrior,nothHitCount) values ('" ++ lev ++ "','" ++ lotv ++ "','" ++ rotv ++ "','" ++ rev ++ "'," ++ otv ++ ",'[" ++ show clauTagPrior ++ "]',1)")) :: Query
            stmt1 <- prepareStmt conn sqlstat
            oks <- executeStmt conn stmt1 []             -- Insert the described structural gene.
            putStrLn $ "updateStruGene': Last inserted row with ID " ++ show (getOkLastInsertID oks)
            close conn                                   -- Close MySQL connection.
            return ((leftOver, rightOver, prior):overPairs)

      x | elem x ["stru_gene3a_202508"] -> do             -- Multimodel
        let lot = phraCateTree2PhraSynTree leftOverTree   -- BiTree PhraSyn
        let rot = phraCateTree2PhraSynTree rightOverTree  -- BiTree PhraSyn
        let lotv = doubleBackSlash (show lot)
        let rotv = doubleBackSlash (show rot)

        let sqlstat = read (show ("select id, clauTagPrior, lpHitCount, rpHitCount, nothHitCount from "
                                  ++ syntax_ambig_resol_model
                                  ++ " where leftOverTree = '" ++ lotv ++ "' && "
                                  ++ "rightOverTree = '" ++ rotv ++ "'"
                                  )) :: Query
        stmt <- prepareStmt conn sqlstat
        (defs, is) <- queryStmt conn stmt []

        rows <- S.toList is
        if rows /= []
          then
            if length rows > 1
              then do
                close conn                           -- Close MySQL connection.
                error "updateStruGene': Find duplicate structural genes."
              else do
                let id = fromMySQLInt32U ((rows!!0)!!0)
                let clauTagPriorListStr = fromMySQLText ((rows!!0)!!1)
                let lpHitCount = fromMySQLInt16U ((rows!!0)!!2)
                let rpHitCount = fromMySQLInt16U ((rows!!0)!!3)
                let nothHitCount = fromMySQLInt16U ((rows!!0)!!4)

                putStrLn $ "updateStruGene': (" ++ show id ++ ") clauTagPrior: " ++ clauTagPriorListStr ++ ", lpHitCount: " ++ show lpHitCount ++ ", rpHitCount: " ++ show rpHitCount ++ ", nothHitCount: " ++ show nothHitCount
                let clauTagPriorList = stringToCTPList clauTagPriorListStr
                let priorList = map snd $ filter ((== clauTag) . fst) clauTagPriorList          -- The selected priorities
                priorFlag <- getLineUntil "please select priority from [Lp/Rp/Noth] ('1' or RETURN for Lp, '2' for Rp, '3' for Noth): " ["1","2","3"] True
                let prior = case priorFlag of
                              "1" -> Lp :: Prior
                              "2" -> Rp :: Prior
                              "3" -> Noth :: Prior

                if notElem prior priorList
                  then do
                    resetStmt conn stmt
                    let sqlstat = case prior of
                                    Lp -> read (show ("update " ++ syntax_ambig_resol_model ++ " set clauTagPrior = ?, lpHitCount = ? where id = '" ++ show id ++ "'")) :: Query
                                    Rp -> read (show ("update " ++ syntax_ambig_resol_model ++ " set clauTagPrior = ?, rpHitCount = ? where id = '" ++ show id ++ "'")) :: Query
                                    Noth -> read (show ("update " ++ syntax_ambig_resol_model ++ " set clauTagPrior = ?, nothHitCount = ? where id = '" ++ show id ++ "'")) :: Query
                    stmt <- prepareStmt conn sqlstat
                    let newClauTagPriorList = (clauTag, prior):clauTagPriorList     -- Add with one ClauTagPrior value.
                    oks <- case prior of
                             Lp -> executeStmt conn stmt [toMySQLText (show newClauTagPriorList), toMySQLInt16U (lpHitCount + 1)]       -- Add 'lpHitCount' by 1.
                             Rp -> executeStmt conn stmt [toMySQLText (show newClauTagPriorList), toMySQLInt16U (rpHitCount + 1)]       -- Add 'rpHitCount' by 1.
                             Noth -> executeStmt conn stmt [toMySQLText (show newClauTagPriorList), toMySQLInt16U (nothHitCount + 1)]   -- Add 'nothHitCount' by 1.
--                  putStrLn $ "updateStruGene': getOkLastInsertID = " ++ show (getOkLastInsertID oks)
                    putStrLn $ "updateStruGene': " ++ show (clauTag, prior) ++ " was inserted into row " ++ show id
                  else putStrLn $ "updateStruGene': " ++ show (clauTag, prior) ++ " was hit in row " ++ show id
                close conn                       -- Close MySQL connection.
                return ((leftOver, rightOver, prior):overPairs)
          else do
            putStrLn "Inquire failed."
            priorFlag <- getLineUntil "please select priority from [Lp/Rp/Noth] ('1' or RETURN for 'Lp', '2' for 'Rp', '3' for 'Noth'): " ["1","2","3"] True
            let prior = case priorFlag of
                          "1" -> Lp :: Prior
                          "2" -> Rp :: Prior
                          "3" -> Noth :: Prior
            let clauTagPrior = (clauTag, prior)
            let sqlstat = case prior of
                            Lp -> read (show ("insert " ++ syntax_ambig_resol_model ++ " (leftOverTree,rightOverTree,clauTagPrior,lpHitCount) values ('" ++ lotv ++ "','" ++ rotv ++ "','[" ++ show clauTagPrior ++ "]',1)")) :: Query
                            Rp -> read (show ("insert " ++ syntax_ambig_resol_model ++ " (leftOverTree,rightOverTree,clauTagPrior,rpHitCount) values ('" ++ lotv ++ "','" ++ rotv ++ "','[" ++ show clauTagPrior ++ "]',1)")) :: Query
                            Noth -> read (show ("insert " ++ syntax_ambig_resol_model ++ " (leftOverTree,rightOverTree,clauTagPrior,nothHitCount) values ('" ++ lotv ++ "','" ++ rotv ++ "','[" ++ show clauTagPrior ++ "]',1)")) :: Query
            stmt1 <- prepareStmt conn sqlstat
            oks <- executeStmt conn stmt1 []             -- Insert the described structural gene.
            putStrLn $ "updateStruGene': Last inserted row with ID " ++ show (getOkLastInsertID oks)
            close conn                                   -- Close MySQL connection.
            return ((leftOver, rightOver, prior):overPairs)

      _ -> error $ "updateStruGene': syntax_ambig_resol_model " ++ syntax_ambig_resol_model ++ " is undefined."

{- Rollback the operations of function updateStruGene on database, and recursively create overlapping pairs.
 - For every pair of overlapping phrases, its overlap type, left- and right-extend phrases are found in a given set
 - of phrases.
 - Model Stru_Gene does NOT need rollbacks, because its field 'prior' is always overwritten.
 - Model stru_gene_202501 :: (LeftExtend, LeftOver, RightOver, RightExtend, Overtype, ClauTagPrior, LpHitCount, RpHitCount, NothHitCount)
 - Model stru_gene3_202508 :: (LeftExtend, LeftOverTree, RightOverTree, RightExtend, Overtype, ClauTagPrior, LpHitCount, RpHitCount, NothHitCount)
 - Model stru_gene3a_202508 :: (LeftOverTree, RightOverTree, ClauTagPrior, LpHitCount, RpHitCount, NothHitCount)
 -}
rollbackStruGene :: ClauTag -> [PhraCate] -> [OverPair] -> IO ()
rollbackStruGene _ _ [] = putStrLn "rollbackStruGene: End."                     -- To make output easy to read.
rollbackStruGene clauTag nPCs (op:ops) = do
    confInfo <- readFile "Configuration"
    let syntax_ambig_resol_model = getConfProperty "syntax_ambig_resol_model" confInfo    -- Find ambiguity resolution model, namely table name in MySQL.

    let leftOver = fst3 op
    let rightOver = snd3 op
    let prior = thd3 op
    let overType = getOverType nPCs leftOver rightOver                          -- Get overlapping type
    let leftExtend = getPhraByEnd (stOfCate leftOver - 1) nPCs                  -- Get all left-extend phrases
    let rightExtend = getPhraByStart (enOfCate rightOver + 1) nPCs              -- Get all right-entend phrases
    let leftOverTree = findATree leftOver nPCs                                  -- BiTree PhraCate
    let rightOverTree = findATree rightOver nPCs                                -- BiTree PhraCate

    let le = map ((!!0) . ctpsOfCate) leftExtend         -- [(Category,Tag,PhraStru,Span)] of left-extended phrases
    let lo = (ctpsOfCate leftOver)!!0                    -- (Category,Tag,PhraStru,Span) of left-overlapping phrase
    let ro = (ctpsOfCate rightOver)!!0                   -- (Category,Tag,PhraStru,Span) of right-overlapping phrase
    let re = map ((!!0) . ctpsOfCate) rightExtend        -- [(Category,Tag,PhraStru,Span)] of right-extended phrases
    let ot = overType                                    -- Overlap type
    let lot = phraCateTree2PhraSynTree leftOverTree      -- BiTree PhraSyn
    let rot = phraCateTree2PhraSynTree rightOverTree     -- BiTree PhraSyn

    let lev = doubleBackSlash (show le)                 -- Get values to insert them into MySql Table
    let lov = doubleBackSlash (show lo)
    let rov = doubleBackSlash (show ro)
    let rev = doubleBackSlash (show re)
    let otv = show ot
    let lotv = show lot
    let rotv = show rot
{-
    putStrLn $ "Inquire structural gene: leftExtend = '" ++ show le ++ "' && " ++
                                          "leftOver = '" ++ show lo ++ "' && " ++
                                          "rightOver = '" ++ show ro ++ "' && " ++
                                          "rightExtend = '" ++ show re ++ "' && " ++
                                          "overType = "  ++ otv ++ " && " ++
                                          "leftOverTree = '" ++ lotv ++ "' && " ++
                                          "rightOverTree = '" ++ rotv ++ "'"
-}
    case syntax_ambig_resol_model of
      "stru_gene" -> putStrLn "rollbackStruGene: Model StruGene does not need rollbacking."

      x | elem x ["stru_gene_202408", "stru_gene_202412", "stru_gene_202501"] -> do       -- Multimodel
        conn <- getConn
        let sqlstat = read (show ("select id, clauTagPrior, lpHitCount, rpHitCount, nothHitCount from "
                                   ++ syntax_ambig_resol_model ++ " where leftExtend = '" ++ lev ++ "' && "
                                   ++ "leftOver = '" ++ lov ++ "' && "
                                   ++ "rightOver = '" ++ rov ++ "' && "
                                   ++ "rightExtend = '" ++ rev ++ "' && "
                                   ++ "overType = " ++ otv)) :: Query
        stmt <- prepareStmt conn sqlstat
        (defs, is) <- queryStmt conn stmt []

        rows <- S.toList is
        if rows /= []
          then
            if length rows > 1
              then do
                close conn                           -- Close MySQL connection.
                error "rollbackStruGene: Find duplicate structural genes."
              else do
                let id = fromMySQLInt32U ((rows!!0)!!0)
                let clauTagPriorListStr = fromMySQLText ((rows!!0)!!1)
                let lpHitCount = fromMySQLInt16U ((rows!!0)!!2)
                let rpHitCount = fromMySQLInt16U ((rows!!0)!!3)
                let nothHitCount = fromMySQLInt16U ((rows!!0)!!4)

                putStrLn $ "rollbackStruGene: (" ++ show id ++ ") clauTagPrior: " ++ clauTagPriorListStr ++ ", lpHitCount: " ++ show lpHitCount ++ ", rpHitCount: " ++ show rpHitCount ++ ", nothHitCount: " ++ show nothHitCount
                let clauTagPriorList = stringToCTPList clauTagPriorListStr
                let newClauTagPriorList = [x | x <- clauTagPriorList, x /= (clauTag, prior)]     -- Remove one ClauTagPrior value.

                resetStmt conn stmt
                let sqlstat = case prior of
                                Lp -> read (show ("update " ++ syntax_ambig_resol_model ++ " set clauTagPrior = ?, lpHitCount = ? where id = '" ++ show id ++ "'")) :: Query
                                Rp -> read (show ("update " ++ syntax_ambig_resol_model ++ " set clauTagPrior = ?, rpHitCount = ? where id = '" ++ show id ++ "'")) :: Query
                                Noth -> read (show ("update " ++ syntax_ambig_resol_model ++ " set clauTagPrior = ?, nothHitCount = ? where id = '" ++ show id ++ "'")) :: Query
                stmt <- prepareStmt conn sqlstat
                oks <- case prior of
                         Lp -> executeStmt conn stmt [toMySQLText (show newClauTagPriorList), toMySQLInt16U (lpHitCount - 1)]       -- 1 subtracted from 'lpHitCount'
                         Rp -> executeStmt conn stmt [toMySQLText (show newClauTagPriorList), toMySQLInt16U (rpHitCount - 1)]       -- 1 subtracted from 'rpHitCount'
                         Noth -> executeStmt conn stmt [toMySQLText (show newClauTagPriorList), toMySQLInt16U (nothHitCount - 1)]   -- 1 subtracted from 'nothHitCount'
                putStrLn $ "rollbackStruGene: " ++ show (clauTag, prior) ++ " was removed from row " ++ show id
                close conn                       -- Close MySQL connection.
          else do
            putStrLn "rollbackStruGene: Inquire failed. It is impossible!"
            close conn                           -- Close MySQL connection.

      x | elem x ["stru_gene3_202508"] -> do      -- Multimodel
        conn <- getConn
        let sqlstat = read (show ("select id, clauTagPrior, lpHitCount, rpHitCount, nothHitCount from "
                                  ++ syntax_ambig_resol_model ++ " where leftExtend = '" ++ lev ++ "' && "
                                  ++ "leftOverTree = '" ++ lotv ++ "' && "
                                  ++ "rightOverTree = '" ++ rotv ++ "' && "
                                  ++ "rightExtend = '" ++ rev ++ "' && "
                                  ++ "overType = " ++ otv)) :: Query
        stmt <- prepareStmt conn sqlstat
        (defs, is) <- queryStmt conn stmt []

        rows <- S.toList is
        if rows /= []
          then
            if length rows > 1
              then do
                close conn                           -- Close MySQL connection.
                error "rollbackStruGene: Find duplicate structural genes."
              else do
                let id = fromMySQLInt32U ((rows!!0)!!0)
                let clauTagPriorListStr = fromMySQLText ((rows!!0)!!1)
                let lpHitCount = fromMySQLInt16U ((rows!!0)!!2)
                let rpHitCount = fromMySQLInt16U ((rows!!0)!!3)
                let nothHitCount = fromMySQLInt16U ((rows!!0)!!4)

                putStrLn $ "rollbackStruGene: (" ++ show id ++ ") clauTagPrior: " ++ clauTagPriorListStr ++ ", lpHitCount: " ++ show lpHitCount ++ ", rpHitCount: " ++ show rpHitCount ++ ", nothHitCount: " ++ show nothHitCount
                let clauTagPriorList = stringToCTPList clauTagPriorListStr
                let newClauTagPriorList = [x | x <- clauTagPriorList, x /= (clauTag, prior)]     -- Remove one ClauTagPrior value.

                resetStmt conn stmt
                let sqlstat = case prior of
                                Lp -> read (show ("update " ++ syntax_ambig_resol_model ++ " set clauTagPrior = ?, lpHitCount = ? where id = '" ++ show id ++ "'")) :: Query
                                Rp -> read (show ("update " ++ syntax_ambig_resol_model ++ " set clauTagPrior = ?, rpHitCount = ? where id = '" ++ show id ++ "'")) :: Query
                                Noth -> read (show ("update " ++ syntax_ambig_resol_model ++ " set clauTagPrior = ?, nothHitCount = ? where id = '" ++ show id ++ "'")) :: Query
                stmt <- prepareStmt conn sqlstat
                oks <- case prior of
                         Lp -> executeStmt conn stmt [toMySQLText (show newClauTagPriorList), toMySQLInt16U (lpHitCount - 1)]       -- 1 subtracted from 'lpHitCount'
                         Rp -> executeStmt conn stmt [toMySQLText (show newClauTagPriorList), toMySQLInt16U (rpHitCount - 1)]       -- 1 subtracted from 'rpHitCount'
                         Noth -> executeStmt conn stmt [toMySQLText (show newClauTagPriorList), toMySQLInt16U (nothHitCount - 1)]   -- 1 subtracted from 'nothHitCount'
                putStrLn $ "rollbackStruGene: " ++ show (clauTag, prior) ++ " was removed from row " ++ show id
                close conn                       -- Close MySQL connection.
          else do
            putStrLn "rollbackStruGene: Inquire failed. It is impossible!"
            close conn                           -- Close MySQL connection.

      x | elem x ["stru_gene3a_202508"] -> do    -- Multimodel
        conn <- getConn
        let sqlstat = read (show ("select id, clauTagPrior, lpHitCount, rpHitCount, nothHitCount from "
                                  ++ syntax_ambig_resol_model ++ " where leftOverTree = '" ++ lotv ++ "' && "
                                  ++ "rightOverTree = '" ++ rotv ++ "'")) :: Query
        stmt <- prepareStmt conn sqlstat
        (defs, is) <- queryStmt conn stmt []

        rows <- S.toList is
        if rows /= []
          then
            if length rows > 1
              then do
                close conn                           -- Close MySQL connection.
                error "rollbackStruGene: Find duplicate structural genes."
              else do
                let id = fromMySQLInt32U ((rows!!0)!!0)
                let clauTagPriorListStr = fromMySQLText ((rows!!0)!!1)
                let lpHitCount = fromMySQLInt16U ((rows!!0)!!2)
                let rpHitCount = fromMySQLInt16U ((rows!!0)!!3)
                let nothHitCount = fromMySQLInt16U ((rows!!0)!!4)

                putStrLn $ "rollbackStruGene: (" ++ show id ++ ") clauTagPrior: " ++ clauTagPriorListStr ++ ", lpHitCount: " ++ show lpHitCount ++ ", rpHitCount: " ++ show rpHitCount ++ ", nothHitCount: " ++ show nothHitCount
                let clauTagPriorList = stringToCTPList clauTagPriorListStr
                let newClauTagPriorList = [x | x <- clauTagPriorList, x /= (clauTag, prior)]     -- Remove one ClauTagPrior value.

                resetStmt conn stmt
                let sqlstat = case prior of
                                Lp -> read (show ("update " ++ syntax_ambig_resol_model ++ " set clauTagPrior = ?, lpHitCount = ? where id = '" ++ show id ++ "'")) :: Query
                                Rp -> read (show ("update " ++ syntax_ambig_resol_model ++ " set clauTagPrior = ?, rpHitCount = ? where id = '" ++ show id ++ "'")) :: Query
                                Noth -> read (show ("update " ++ syntax_ambig_resol_model ++ " set clauTagPrior = ?, nothHitCount = ? where id = '" ++ show id ++ "'")) :: Query
                stmt <- prepareStmt conn sqlstat
                oks <- case prior of
                         Lp -> executeStmt conn stmt [toMySQLText (show newClauTagPriorList), toMySQLInt16U (lpHitCount - 1)]       -- 1 subtracted from 'lpHitCount'
                         Rp -> executeStmt conn stmt [toMySQLText (show newClauTagPriorList), toMySQLInt16U (rpHitCount - 1)]       -- 1 subtracted from 'rpHitCount'
                         Noth -> executeStmt conn stmt [toMySQLText (show newClauTagPriorList), toMySQLInt16U (nothHitCount - 1)]   -- 1 subtracted from 'nothHitCount'
                putStrLn $ "rollbackStruGene: " ++ show (clauTag, prior) ++ " was removed from row " ++ show id
                close conn                       -- Close MySQL connection.
          else do
            putStrLn "rollbackStruGene: Inquire failed. It is impossible!"
            close conn                       -- Close MySQL connection.

      _ -> error $ "rollbackStruGene: syntax_ambig_resol_model " ++ syntax_ambig_resol_model ++ " is undefined."

    rollbackStruGene clauTag nPCs ops

-- Recursively parse sentences according scripts, such that samples of category-conversional ambiguity resolution model SLR can be created.
parseSentsByScript :: SentIdx -> SentIdx -> IO ()
parseSentsByScript startSn endSn = do
    putStrLn $ "Sentence No." ++ show startSn ++ " start to create SLR samples."
    let startSn' = startSn +1
    getSentFromDB startSn >>= getSent >>= parseSentByScript startSn >>= \cOrS ->
      if cOrS
        then do                  -- Continue parsing
          if startSn' <= endSn
            then parseSentsByScript startSn' endSn
            else putStrLn "parseSentsByScript: End."
        else do                  -- Skip the following parsing.
          if startSn' <= endSn
            then putStrLn $ "parseSentsByScript: Skip sentence No. " ++ show [startSn' .. endSn]
            else putStrLn "parseSentsByScript: End."

type NumOfClauEqual = Int                                         -- Num. of clauses with same parsing trees.
type ClauIdxOfClauUnequal = [Int]                                 -- Indices List of clauses with different trees.
type TagOfClauAnaly = (NumOfClauEqual, ClauIdxOfClauUnequal)      -- Comparing results between original and new parsing.

{- Re-parse a sentence according the previously created parsing script.
 - Here every clause is a String, and parsing starts from the first clause.
 - The first parameter is the value of 'serial_num' in database Table 'corpus'.
 - This function is used to create some side-products, such as samlpes of category conversion ambiguity resolution model SLR.
 - To create other side-products, the below codes need be extended.
 - If parsing result by script is different with original trees, and want cancel the parsing of the following sentences, return False.
 -}
parseSentByScript :: SentIdx -> [String] -> IO Bool
parseSentByScript sn cs = do
    hSetBuffering stdin LineBuffering                  -- Open input buffering
    confInfo <- readFile "Configuration"
    let script_source = getConfProperty "script_source" confInfo                -- Script source
    let tree_source = getConfProperty "tree_source" confInfo                    -- Tree source
    let tree_target = getConfProperty "tree_target" confInfo                    -- auto sample target
    let syntax_ambig_resol_sample_update_switch = getConfProperty "syntax_ambig_resol_sample_update_switch" confInfo

    conn <- getConn
    let sqlstat = DS.fromString $ "create table if not exists " ++ tree_target ++ " (serial_num int primary key auto_increment,tree mediumtext, script mediumtext, tree_check tinyint(1), SLR mediumtext, tagClause varchar(50))"
    stmt <- prepareStmt conn sqlstat
    executeStmt conn stmt []

    let query = DS.fromString ("select script from " ++ script_source ++ " where serial_num = ?")       -- Query is instance of IsString.
    stmt <- prepareStmt conn query
    (defs, is) <- queryStmt conn stmt [toMySQLInt32 sn]                         --([ColumnDef], InputStream [MySQLValue])
    record <- S.read is
    let scriptStr = case record of
                      Just x -> x
                      Nothing -> [MySQLText "[]"]
    let script = fromMySQLText (scriptStr!!0)
    skipToEof is                                                                -- Go to the end of the stream.
    closeStmt conn stmt

    let script' = readScripts script
    putStr "Parsing script: "
    showScripts script'

    let clauNum = length cs
    putStrLn $ " There are " ++ show clauNum ++ " clauses in total."

    if syntax_ambig_resol_sample_update_switch == "On"
      then do                                                      -- Update syntax ambiguity resolution samples in database
        hasCTPSample <- hasSentSampleInSynAmbiResol sn 1 clauNum
        if hasCTPSample
          then removeClauTagPriorFromSynAmbiResol sn 1 clauNum     -- Remove all samples of this sentence in syntax ambiguity resolution database.
          else putStrLn "parseSentByScript: No syntax ambiguity resolution sample."
      else putStr ""                                               -- Do NOT update sample base

    finFlagAndSLRAndTree <- parseSentByScript' sn cs script'
    if (fst5 finFlagAndSLRAndTree)
      then do
        let query = DS.fromString ("select tree from " ++ tree_source ++ " where serial_num = ?")
        stmt <- prepareStmt conn query
        (defs, is) <- queryStmt conn stmt [toMySQLInt32 sn]
        rows <- S.toList is
        let origTrees = readTrees $ fromMySQLText ((rows!!0)!!0)
        let origTrees' = map (\t -> (fst t, (sortPhraCateBySpan . snd) t)) origTrees    -- PhraCates are ordered by span for every clause.
        let newTree = thd5 finFlagAndSLRAndTree                                 -- [Tree]
        let newScript = fth5 finFlagAndSLRAndTree                               -- [Script]

        putStrLn "parseSentByScript: Finished parsing."
        let tagOfClauAnaly = (length origTrees, (fst . fif5) finFlagAndSLRAndTree, (snd . fif5) finFlagAndSLRAndTree)   -- Compare original tree with new tree.

        let query = DS.fromString ("select serial_num from " ++ tree_target ++ " where serial_num = ?")
        stmt <- prepareStmt conn query
        (defs, is) <- queryStmt conn stmt [toMySQLInt32 sn]
        rows <- S.toList is
        if rows == []
          then do
            let query = DS.fromString ("insert into " ++ tree_target ++ " set serial_num = ?, tree = ?, script = ?, SLR = ?, tagClause = ?")
            stmt <- prepareStmt conn query
            ok <- executeStmt conn stmt [toMySQLInt32 sn, toMySQLText (nTreeToString newTree), toMySQLText (nScriptToString newScript), toMySQLText (nClauseSLRToString (snd5 finFlagAndSLRAndTree)), toMySQLText (show tagOfClauAnaly)]
            let rn = getOkAffectedRows ok
            putStrLn $ "parseSentByScript: " ++ show rn ++ " row(s) were inserted in " ++ tree_target ++ "."
            close conn
          else do
            let sqlstat = DS.fromString $ "update " ++ tree_target ++ " set tree = ?, script = ?, SLR = ?, tagClause = ? where serial_num = ?"
            stmt <- prepareStmt conn sqlstat
            ok <- executeStmt conn stmt [toMySQLText (nTreeToString newTree), toMySQLText (nScriptToString newScript), toMySQLText (nClauseSLRToString (snd5 finFlagAndSLRAndTree)), toMySQLText (show tagOfClauAnaly), toMySQLInt32 sn]
            let rn = getOkAffectedRows ok
            putStrLn $ "parseSentByScript: " ++ show rn ++ " row(s) were updated in " ++ tree_target ++ "."
            close conn

        putStrLn $ "tagOfClauAnaly = " ++ show tagOfClauAnaly
        putStrLn $ "Sentence No."++ show sn ++" has "++ show (length origTrees) ++" clauses, in which "++ show ((fst . fif5) finFlagAndSLRAndTree) ++ " clauses are the same with the origial clauses."
        if fst3 tagOfClauAnaly /= snd3 tagOfClauAnaly
          then do          -- Let user know sentential parsing difference happens, and decide whether to parse the following sentences.
            cOrS <- getLineUntil "Press 'c' or RETURN to continue, 's' to skip the remaining parsing: " ["c","s"] True
            case cOrS of
              "c" -> return True
              "s" -> return False
              _ -> error "parseSentByScript: Impossible."
          else return True
      else do
        putStrLn $ "parseSentByScript: Sentence " ++ show sn ++ " was NOT finished in parsing."
        return True               -- Skipping clauses ONLY happens in manual parsing. Here is just for satisfying type requirements.

{- Re-parse a sentence according the previously created parsing script.
 - Here every clause is a String.
 - Parameter 'sn' is the value of 'serial_num' in database Table 'corpus'.
 - Table 'corpus', 'treebank1', and some other tables are associated with field 'serial_num'.
 - 'cs' is clausal strings to be parsed.
 - 'script' is parsing scripts for these clauses.
 - If a certain clause is not finished in parsing, return False to skip the remaining clauses.
 - SLROfSent :: [SLROfClause] is the category conversion ambiguity resolution of this sentence.
 - Tree ::= (ClauIdx, [PhraCate]) is parsing result of one clause.
 - TagOfClauAnaly ::= (NumOfClauEqual, ClauIdxOfClauUnequal)      -- 相等小句的个数，不等小句的编号
 -}
parseSentByScript' :: SentIdx -> [String] -> [Script] -> IO (Bool, SLROfSent, [Tree], [Script], TagOfClauAnaly)
parseSentByScript' _ [] _ = return (True, [], [], [], (0,[]))
parseSentByScript' sn cs scripts = do
    finFlagAndSLRAndTree <- parseSentByScript' sn (take (length cs - 1) cs) (take (length cs - 1) scripts)     -- 前递归，即先分析第一个小句。
    let numOfClauEqual = fst $ fif5 finFlagAndSLRAndTree                        -- Num. of clauses with same parsing trees.
    let clauIdxOfClauUnequal = snd $ fif5 finFlagAndSLRAndTree                  -- [ClauIdx]
    let clauIdx = length cs
    putStrLn $ "  ===== Clause No." ++ show clauIdx ++ " ====="
    if (fst5 finFlagAndSLRAndTree)                                              -- True means the previous clauses has been finished parsing.
      then do
        let nPCs = initPhraCate $ getNCate $ words (last cs)
        putStr "Before parsing: "
        showNPhraCateLn nPCs
        putStr "Word semantic sequence: "
        showNSeman nPCs

        let lastScript = case scripts of                                        -- It's possible of no script to use.
                           [] -> (clauIdx, [], [])                              -- Null script for clause 'clauIdx'
                           ss -> last ss

        putStr "Script (ClauIdx, [[Rule]], [BanPCs]): "
        showScripts' [lastScript]
        putStrLn ""

        confInfo <- readFile "Configuration"
        let tree_source = getConfProperty "tree_source" confInfo                -- Tree source is for retrieving correct parsing trees.
        let syntax_ambig_resol_model = getConfProperty "syntax_ambig_resol_model" confInfo      -- Syntax Ambiguity Resolution Model

        conn <- getConn
        let query = DS.fromString ("select tree from " ++ tree_source ++ " where serial_num = ?")       -- Prepare to read Tree field.
        stmt <- prepareStmt conn query
        (defs, is) <- queryStmt conn stmt [toMySQLInt32 sn]
        rows <- S.toList is
        let origTrees = readTrees $ fromMySQLText ((rows!!0)!!0)
        let origClauIdxTree = origTrees!!(clauIdx-1)                            -- (ClauIdx, [PhraCate]) of the last clause.
        close conn

        rtbPCs <- parseClauseWithScript (sn, clauIdx) [] nPCs [] [] lastScript []        -- Parse begins with empty '[[Rule]]', empty '[BanPCs]' and empty SLROfClause value.
        let newClauIdxTree = snd4 rtbPCs
--        let judgeClauIdxTree = quickSort4Phrase (snd origClauIdxTree) == quickSort4Phrase newClauIdxTree   -- Bool
        let judgeClauIdxTree = equalSortedPhraList' (quickSort4Phrase (snd origClauIdxTree)) (quickSort4Phrase newClauIdxTree)     -- Ignoring semantic item.
        let numOfClauEqual' = case judgeClauIdxTree of
                                True -> numOfClauEqual + 1
                                False -> numOfClauEqual

        let newSLROfSent = snd5 finFlagAndSLRAndTree ++ [fth4 rtbPCs]                    -- [SLROfClause]
        let newTreeOfSent = thd5 finFlagAndSLRAndTree ++ [(clauIdx, snd4 rtbPCs)]        -- [Tree]
        let newScriptOfSent = fth5 finFlagAndSLRAndTree ++ [(clauIdx, fst4 rtbPCs, thd4 rtbPCs)]     -- [Script]
        let rtbPCs' = (fst4 rtbPCs, snd4 rtbPCs, thd4 rtbPCs)
        if rtbPCs' == ([],[],[])         -- False means the current clause is terminated manually.
          then return (False, newSLROfSent, newTreeOfSent, newScriptOfSent, (numOfClauEqual',clauIdxOfClauUnequal))
          else if judgeClauIdxTree
               then return (True, newSLROfSent, newTreeOfSent, newScriptOfSent, (numOfClauEqual',clauIdxOfClauUnequal))
               else do
                 let clauIdxOfClauUnequal' = clauIdxOfClauUnequal ++ [clauIdx]
                 putStrLn "origClauTree:"
                 showNPhraCateLn (sortPhraCateBySpan (snd origClauIdxTree))
                 putStrLn "newClauTree:"
                 showNPhraCateLn newClauIdxTree
                 return (True, newSLROfSent, newTreeOfSent, newScriptOfSent, (numOfClauEqual', clauIdxOfClauUnequal'))

      else do
        putStrLn $ "Skip clause " ++ show clauIdx
        return (False, snd5 finFlagAndSLRAndTree, thd5 finFlagAndSLRAndTree, fth5 finFlagAndSLRAndTree, fif5 finFlagAndSLRAndTree)

{- Parsing a clause is a recursive transition process.
 - Input: A sequence of [Rule], a sequence of phrasal categories, a sequence of banned phrasal categories, and a parsing script;
 - Algo.:
 - (1) Do one trip of transition;
 - (2) If creating new phrasal categories, append rules used in this trip to [[Rule]], take resultant phrases, accumulated banned
 -     phrases and accumulated overlapping phrases as input, go (1);
 -     Otherwise, return the quadruple ([[Rule]], resultant tree PCs, accumulated banned PCs, SLR List Of This Clause).
 - 'overPairs' records the overlapping phrases and their ambiguity resolution in previous rounds of transitions. Before the first round, it should be empty.
 -}
parseClauseWithScript :: ClauTag -> [[Rule]] -> [PhraCate] -> [BanPCs] -> [OverPair] -> Script -> SLROfClause -> IO ([[Rule]], [PhraCate], [BanPCs], SLROfClause)
parseClauseWithScript clauTag rules nPCs banPCSets overPairs script origSLROfClause = do
    rtboPCs <- doTransWithScript clauTag nPCs banPCSets overPairs script
                                               -- Tree = (ClauIdx, [PhraCate])
                                               -- <rtboPCs> ::= ([Rule], resultant tree PCs, accumulated banned phrase sets, accumulated overlapping phrases)
                                               -- [Rule] is the set of rules used in this trip of transition.

    putStrLn $ "parseClauseWithScript: fst4 rtboPCs (Rules): " ++ show (fst4 rtboPCs)     -- [Rule] used in last trip of transition
    putStr "parseClauseWithScript: snd4 rtboPCs (nPCs): "                                 -- Stub tree formed in last trip of transition
    showNPhraCateLn (snd4 rtboPCs)
    putStr "parseClauseWithScript: thd4 rtboPCs (banPCSets): "                            -- [BanPCs] formed in last trip of transition
    showNPhraCateListLn (thd4 rtboPCs)
    putStr $ "parseClauseWithScript: fth4 rtboPCs (OverPairs): "     -- Overlap pairs found after last trip of transition
    showNOverPairLn (fth4 rtboPCs)

    let ruleOfATrans = fst4 rtboPCs                                             -- [Rule]
    let sLROfATrans = (clauTag, (nPCs, ruleOfATrans))                           -- SLROfATrans
    let newSLROfClause = origSLROfClause ++ [sLROfATrans]                       -- [SLROfATrans]

    if rtboPCs == ([],[],[],[])
      then return ([],[],[],origSLROfClause)                   -- Terminating flag.
      else if nPCs /= (snd4 rtboPCs)
        then do
          let scriptTail = case script of
                             (clauIdx, (x:xs), bPCSets) -> (clauIdx, xs, tail bPCSets)  -- Remove the head element of OnOff and BanPCs list.
                             _ -> error "parseClauseWithScript: Both [[Rule]] and [BanPCs] are []."

          putStr $ "scriptTail: "
          showScripts [scriptTail]
          parseClauseWithScript clauTag (rules ++ [fst4 rtboPCs]) (snd4 rtboPCs) (thd4 rtboPCs) (fth4 rtboPCs) scriptTail newSLROfClause        -- Do the next trip of transition
                                               -- with appended rules, resultant PCs, accumulated banned PCs, and accumulate overlapping phrasal pairs.
        else do                                -- Phrasal closure has been formed.
--          putStrLn $ "Num. of phrasal categories in closure is " ++ (show $ length nPCs)-
--          showNPhraCateLn (sortPhraCateBySpan nPCs)
          let spls = divPhraCateBySpan nPCs
          putStrLn "  ##### Parsing Tree #####"
          showTreeStru spls spls
--          putStrLn $ "SLR ="
--          showSLROfClause newSLRSample     -- Last round of transition does not create SLR.
          return (rules ++ [fst4 rtboPCs], (snd4 rtboPCs), thd4 rtboPCs, newSLROfClause)

{- Do a trip of transition, insert or update related ambiguity resolution samples in Table <syntax_ambig_resol_model>, and return the category-converted
 - rules used in this trip, the resultant phrases, and the banned phrases.
 - If transitive parsing is to terminated, namely selecting 'e' at inquiring rule switches, returnes ([],[],[],[]) as the terminating flag.
 - 'prevOverPairs' is so far known overlapping phrasal pairs and their ambiguity resolution.
 -}
doTransWithScript :: ClauTag -> [PhraCate] -> [BanPCs] -> [OverPair] -> Script -> IO ([Rule], [PhraCate], [BanPCs], [OverPair])
doTransWithScript clauTag nPCs banPCSets prevOverPairs script = do
    let onOffs = snd3 script
    let onOff = case onOffs of                      -- Get rule switches of this trip of transition
                      [] -> [] :: OnOff             -- No script of rule switches to use
                      (x:_) -> x :: OnOff           -- There is a script of rule switches to use

    putStr "Rule switches: "
    showOnOff onOff                                 -- Display rule switches

    let nPCs2 = trans onOff nPCs banPCSets         -- Without pruning, get transitive result.

--    putStr "Transitive result before pruning: "
--    showNPhraCateLn (sortPhraCateBySpan nPCs2)
    putStr "New phrases before pruning: "
    showNPhraCateLn [pc | pc <- nPCs2, notElem4Phrase pc nPCs]
    putStr "Banned phrases: "
    showNPhraCateListLn (map sortPhraCateBySpan' banPCSets)                    -- Can't use <sortPhraCateBySpan> on elements of <banPCSets>.

    let pcps = getOverlap nPCs2                     -- [(PhraCate, PhraCate)]

--    if (hasDup4OverPair prevOverPairs)
--      then putStrLn "doTransWithScript: prevOverPairs includes duplicate elements."
--      else putStrLn "doTransWithScript: prevOverPairs does NOT include duplicate elements."

    let overPairsByPrev = ambiResolByPrevOverPair [] pcps prevOverPairs         -- [OverPair], phrasal pairs appearing in the previous rounds of transitions.
--    putStr "doTransWithScript: overPairsByPrev: "
--    showNOverPairLn overPairsByPrev

    let pcpsWithPrior1 = map (\x-> (fst3 x, snd3 x)) overPairsByPrev
    let pcps1 = [pcp | pcp <- pcps, notElem pcp pcpsWithPrior1]
    let overPairsByScript = ambiResolByScript nPCs2 [] pcps1 script             -- [OverPair], phrasal pairs are resoluted by script.
--    putStr "doTransWithScript: script: "
--    showScripts [script]
    putStr "doTransWithScript: overPairsByScript: "
    showNOverPairLn overPairsByScript

    let pcpsWithPrior2 = map (\x->(fst3 x, snd3 x)) overPairsByScript
    let pcps2 = [pcp | pcp <- pcps1, notElem pcp pcpsWithPrior2]                -- [(PhraCate, PhraCate)] not resolved by script.
    if pcps2 /= []
      then putStrLn "syntaxAmbiResolByManualResol: Begin manual resolution ..."
      else putStr ""
    overPairsByManual <- syntaxAmbiResolByManualResol nPCs2 [] pcps2            -- [OverPair], phrasal pairs are resoluted manually.
    putStr "syntaxAmbiResolByManualResol: overPairsByManual: "
    showNOverPairLn overPairsByManual

    let overPairs = overPairsByPrev ++ overPairsByScript ++ overPairsByManual   -- All phrasal pairs with their ambiguity resolution policies in this round of transition.
    nbPCs <- transWithPruning onOff nPCs banPCSets overPairs                    -- Get transitive result with pruning. ([PhraCate],[BanPCs])

    let nbPCs' = cleanupNbPCs nbPCs (thd3 script)     -- According to banned phrases in script, move unwanted phrases from stub Tree to BanPCs. ([PhraCate],[BanPCs])
                                                      -- Implement consistency between benchmark script and experimental script.

    let pcs1 = quickSort4Phrase  [x | x <- fst nbPCs, notElem4Phrase x (fst nbPCs')]    -- The phrases moved away from original parsing tree.
    let pcs2 = quickSort4Phrase  [x | x <- last (snd nbPCs'), notElem4Phrase x (last (snd nbPCs))]      -- The phrases newly banned in original parsing tree.
    if pcs1 == pcs2
      then do
        putStr "syntaxAmbiResolByManualResol: cleanupNbPCs: Phrases moved from parsing tree to banned phrasal set in this transition: "
        showNPhraCateLn pcs1
      else putStrLn "syntaxAmbiResolByManualResol: cleanupNbPCs: Not identical!"

--    putStr "Transitive result after pruning: "
--    showNPhraCateLn (sortPhraCateBySpan (fst nbPCs'))
    putStr "New phrases after pruning: "
    showNPhraCateLn [pc | pc <- fst nbPCs', notElem4Phrase pc nPCs]
    putStr "Banned phrases: "
    showNPhraCateListLn (snd nbPCs')         -- The banned phrases after updated, which can NOT be sorted by spans!

--    transOk <- getLineUntil "This trip of transition is ok? [y/n/e]: (RETURN for 'y') " ["y","n","e"] True   -- Get user decision of whether to do next transition
    let transOk = "y"                       -- If hoping manually decide whether transitive result is accepted, annotate this line.
    case transOk of
      "y" -> do                             -- Press key 'y' or directly press RETURN
               confInfo <- readFile "Configuration"
               let syntax_ambig_resol_sample_update_switch = getConfProperty "syntax_ambig_resol_sample_update_switch" confInfo
               if syntax_ambig_resol_sample_update_switch == "On"
                 then updateSyntaxAmbiResolSample clauTag (fst nbPCs') (overPairsByScript ++ overPairsByManual)    -- Record new ambiguity resolution fragments.
                 else putStr ""             -- Do nothing
               return (onOff, sortPhraCateBySpan (fst nbPCs'), snd nbPCs', removeDup4OverPair (prevOverPairs ++ overPairsByScript ++ overPairsByManual))
      "n" -> doTransWithManualResol clauTag onOff nPCs banPCSets prevOverPairs        -- do this trip of transition by manually resolving ambiguities.
      "e" -> return ([],[],[],[])      -- Return from doTrans, and indicate this is terminating exit.

{- Resolve ambiguities by the known ambiguity resolution pairs.
 - The overlapping phrasal pairs appeared and were resolved in the previous rounds of transition, identified as prevOverPairs.
 - The overlapping phrasal pairs which have been resolved in this round of transition are identified as currentOverPairs.
 - When no overlapping phrasal pairs need be resolued, return all phrasal pairs which have their resolution policies.
 -}
ambiResolByPrevOverPair :: [OverPair] -> [(PhraCate, PhraCate)] -> [OverPair] -> [OverPair]
ambiResolByPrevOverPair currentOverPairs [] _ = currentOverPairs
ambiResolByPrevOverPair currentOverPairs (pcp:pcps) prevOverPairs
    | priorStr == "Nothing" = ambiResolByPrevOverPair currentOverPairs pcps prevOverPairs
    | otherwise = ambiResolByPrevOverPair ((lp, rp, (read priorStr :: Prior)):currentOverPairs) pcps prevOverPairs
    where
      (lp, rp) = (fst pcp, snd pcp)
      priorStr = case (getPrior' prevOverPairs lp rp) of
                   Nothing -> "Nothing"
                   Just x -> show x

{- Resolve ambiguities by parsing script. Let banPCs be the set of banned phrases, which can be obtained from parsing script. For (lp, rp),
 - if lp belongs to banPCs but rp does not, then we get (lp, rp, Rp).
 - if rp belongs to banPCs but lp does not, then we get (lp, rp, Lp).
 - if both lp and rp belong to banPCs, then we get (lp, rp, Noth).
 - if both lp and rp do not belong to banPCs, then the ambiguity resolution for this pair of phrases is skipped.
 -}
ambiResolByScript :: [PhraCate] -> [OverPair] -> [(PhraCate, PhraCate)] -> Script -> [OverPair]
ambiResolByScript _ overPairs [] _ = overPairs
ambiResolByScript nPCs overPairs (pcp:pcps) script
    | notElem4Phrase' lp banPCs && elem4Phrase' rp banPCs = ambiResolByScript nPCs ((lp, rp, Lp):overPairs) pcps script
    | elem4Phrase' lp banPCs && notElem4Phrase' rp banPCs = ambiResolByScript nPCs ((lp, rp, Rp):overPairs) pcps script
    | elem4Phrase' lp banPCs && elem4Phrase' rp banPCs = ambiResolByScript nPCs ((lp, rp, Noth):overPairs) pcps script
    | otherwise = ambiResolByScript nPCs overPairs pcps script
    where
    banPCs = head (thd3 script)
    lp = fst pcp
    rp = snd pcp

{- When parsing by scripts, every pair of phrases get its ambiguity resolution policy, 'Lp', 'Rp', or 'Noth'.
 - Then 'transWithPruning' is called to create new phrases, and resolves syntactic ambiguities incurred by new phrases.
 - Originally, 'transWithPruning' works until no ambiguity exists, which might temperarily retain some phrases not belonging
 - to final parsing tree. For one example, AB BC CD inculde two pairs of overlapping phrases. If BC is removed, AB and CD can be retained.
 - If CD is removed, then either AB or BC should be removed such that no ambiguity exists. which phrase to be removed depends
 - on function findPhraWithLowestPrio.
 - Now, 'transWithPruning' remove all phrases not belonging to final tree, not using function findPhraWithLowestPrio every time
 - to find one phrasal pair first considered to resolve its ambiguity.
 - This function was originally used to cleanup parsing result such that the residual unwanted phrases are moved into banned phrase set.
 - Now it operates on new Script definition, and only BanPCs value of current round transition is considered.
 - If Script value is correct, then there is no unwanted phrases in stub tree after parsing by scripts, and
 - this function just report this situation.
 - Original definition of Script :: (ClauIdx, [[Rule]], BanPCs)
 - Current definition of Script :: (ClauIdx, [[Rule]], [BanPCs])
 -}
cleanupNbPCs :: ([PhraCate], [BanPCs]) -> [BanPCs] -> ([PhraCate], [BanPCs])
cleanupNbPCs nbPCs banPCSetsFromScript = (nPCs', banPCSets')
    where
    nPCs = fst nbPCs                                                            -- Stub tree, and its corresponding
    banPCSets = snd nbPCs                                                       -- [BanPCs]
    nPCs' = updateAct [x | x <- nPCs, notElem4Phrase x (head banPCSetsFromScript)]        -- New stub tree
                                          -- Change attribute Act of some phrases as True, because their children phrases were just moved to BanPCs.
    banPCs = [x | x <- nPCs, notElem4Phrase x nPCs']                            -- BanPCs
    banPCSets' = case banPCSets of
                   [] -> error "cleanupNbPCs: transWithPruning did not return information about banned phrases."
                   [x] -> [x ++ banPCs]                                         -- Add new banned phrases discovered in cleanup stage.
                   xs -> init xs ++ [last xs ++ banPCs]

{-
lexAmbiResol :: [PhraCate] -> IO [Rule]
lexAmbiResol nPCs = do
    conn <- getConn
    let query = DS.fromString ("select SLR from treebank1 where serial_num >= ? and serial_num <= ?")
    stmt <- prepareStmt conn query
    (defs, is) <- queryStmt conn stmt [toMySQLInt32 1, toMySQLInt32 200]
--    rows <- S.toList is
--    let sLRListOfSent = map (\x -> readSLROfSent (fromMySQLText (x!!0))) rows
    sLRStrListOfSent <- readStreamByText [] is      -- [String], each string is the string of SLRs of one sentence.
    let sLRListOfSent = map readSLROfSent sLRStrListOfSent                      -- [[[(ClauTag, (Stub, [Rule]))]]]
    let sLRListOfClause = foldl (++) [] sLRListOfSent                           -- [[(ClauTag, (Stub, [Rule]))]]
    let sLRListOfTrans = foldl (++) [] sLRListOfClause                          -- [(ClauTag, (Stub, [Rule]))]
    close conn

    let ctpOfnPCs = ctpOfCateList' nPCs                                         -- [PhraSyn] of current stub tree
    let ctpOfsLR = map (\x -> ((fst x, (ctpOfCateList' ((fst . snd) x), (snd . snd) x))) sLRListOfTrans    -- [(ClauTag, ([PhraSyn], [Rule]))] of SLR samples
    let distRuleList = map (\x -> (distPhraSynSetByIdentity ctpOfnPCs ((fst . snd) x), (snd . snd) x)) ctpOfsLR
    let rule = snd $ Map.findMin $ Map.fromList distRuleList
    return rule
-}

-- 从StruGene2表中获得左右重叠短语的PhraStru,overType,prior,并计算Lp/Rp的次数,存到stru_gene_PhraStruCS表中
-- 下面的函数还要对Tag/Category做

getPhraStruCSFromStruGene2 :: Int -> Int -> IO ()
getPhraStruCSFromStruGene2 startId endId = do
    if startId <= endId
     then do
       getPhraStruCSFromStruGene2' startId endId
     else putStrLn "getPhraStruCSFromStruGene2: End."

getPhraStruCSFromStruGene2' :: Int -> Int -> IO ()
getPhraStruCSFromStruGene2' currentId endId = do
    conn <- getConn
    let currentId' = currentId + 1
    let sqlstat = DS.fromString $ "create table if not exists stru_gene_PhraStruCS (id int primary key auto_increment, PhraStruOflo varchar(200), PhraStruOfro varchar(200), overType tinyint(1), numOfLp int, numOfRp int)"
    stmt <- prepareStmt conn sqlstat
    executeStmt conn stmt []

    let query = DS.fromString ("select leftOver, rightOver, overType, prior from stru_gene2 where id = ? ")       -- Query is instance of IsString.
    stmt <- prepareStmt conn query
    (defs, is) <- queryStmt conn stmt [toMySQLInt32 currentId]
    rows <- S.toList is

    let phraStruOflo = thd4 $ readPhraSynFromStr (fromMySQLText ((rows!!0)!!0))
    let phraStruOfro = thd4 $ readPhraSynFromStr (fromMySQLText ((rows!!0)!!1))
    let ot = fromMySQLInt8 ((rows!!0)!!2)
    let prior = readPriorFromStr (fromMySQLText ((rows!!0)!!3))
    let prior' = show prior
    let priorTag = case prior' == "Lp" of
                    True -> (1,0)
                    False -> (0,1)

    if phraStruOflo /= phraStruOfro
      then do
        let query = DS.fromString ("select id, numOfLp, numOfRp from stru_gene_PhraStruCS where PhraStruOflo = ? and PhraStruOfro = ? and overType = ?")       -- Query is instance of IsString.
        stmt <- prepareStmt conn query
        (defs, is) <- queryStmt conn stmt [toMySQLText phraStruOflo, toMySQLText phraStruOfro, toMySQLInt8 ot]
--        (defs, is) <- queryStmt conn stmt [toMySQLText (show cateOflo), toMySQLText (show cateOfro), toMySQLInt8 ot]
        rows <- S.toList is

        if rows == []
          then do
            putStrLn $ "id = "++ show currentId ++ " is new."
            let numOfLp' = fst priorTag
            let numOfRp' = snd priorTag

            let query = DS.fromString ("insert into stru_gene_PhraStruCS set PhraStruOflo = ?, PhraStruOfro = ?, overType = ?, numOfLp = ?, numOfRp = ?")
            stmt <- prepareStmt conn query
            ok <- executeStmt conn stmt [toMySQLText phraStruOflo, toMySQLText phraStruOfro, toMySQLInt8 ot, toMySQLInt32 numOfLp', toMySQLInt32 numOfRp']
            let rn = getOkAffectedRows ok
            close conn
          else do
            putStrLn $ "id = "++ show currentId ++ " is hitcount."
            let idOfPhraStruCS = fromMySQLInt32 ((rows!!0)!!0)
            let numOfLp = fromMySQLInt32 ((rows!!0)!!1)
            let numOfRp = fromMySQLInt32 ((rows!!0)!!2)
            let numOfLp' = numOfLp + (fst priorTag)
            let numOfRp' = numOfRp + (snd priorTag)

            let query = DS.fromString ("update stru_gene_PhraStruCS set numOfLp = ?, numOfRp = ? where id = ?")
            stmt <- prepareStmt conn query
            ok <- executeStmt conn stmt [toMySQLInt32 numOfLp', toMySQLInt32 numOfRp', toMySQLInt32 idOfPhraStruCS]
            let rn = getOkAffectedRows ok
            close conn
      else do
        putStrLn $ "id = "++ show currentId ++ " phraStruOflo == phraStruOfro."
        close conn

    getPhraStruCSFromStruGene2 currentId' endId

-- 设置不同的proportion，来看verifyEffectOfTagCS 的结果,在运行时no=0
autoverifyEffectOfPhraStruCS :: Int -> IO ()
autoverifyEffectOfPhraStruCS no = do
    let proportion = [0.6, 0.65, 0.7, 0.75, 0.8, 0.85, 0.9, 0.95]
    if no < length proportion
      then do
        let proportion' = proportion!!no
        verifyEffectOfPhraStruCS proportion' 1 11013 0 0 0
        autoverifyEffectOfPhraStruCS (no + 1)
      else putStrLn "autoverifyEffectOfPhraStruCS : End."

{- 对上面得到的stru_gene_PhraStruCS表,反过来验证stru_gene2
   1.取stru_gene2的一条数据,得到左右重叠短语的PhraStru,overType,prior
   2.若左右重叠短语的PhraStru相等,取下一条stru_gene2数据
   3.左右重叠短语的PhraStru不相等,找到stru_gene_PhraStruCS匹配的左右重叠短语的PhraStru,overType,计算numOfLp/num和numOfRp/num
   4.给定一个proportion,若numOfLp/num和numOfRp/num 高于这个值，就进行预测，若该条数据的实际prior = 预测prior，说明预测正确，否则预测错误
                       若numOfLp/num和numOfRp/num 低于这个值，就说该条数据模糊
   5.计算11013条数据的总的ambiguNum numOfIncorrPred (numOfCorrPred / totalNumOfPred)
-}
verifyEffectOfPhraStruCS :: Float -> Int -> Int -> Int -> Int -> Int -> IO ()
verifyEffectOfPhraStruCS proportion currentId endId numOfCorrPred ambiguNum totalNumOfPred = do
    conn <- getConn
    if currentId <= endId
      then do
        let query = DS.fromString ("select leftOver, rightOver, overType, prior from stru_gene2 where id = ? ")       -- Query is instance of IsString.
        stmt <- prepareStmt conn query
        (defs, is) <- queryStmt conn stmt [toMySQLInt32 currentId]
        rows <- S.toList is

        let phraStruOflo = thd4 $ readPhraSynFromStr (fromMySQLText ((rows!!0)!!0))
        let phraStruOfro = thd4 $ readPhraSynFromStr (fromMySQLText ((rows!!0)!!1))
        let ot = fromMySQLInt8 ((rows!!0)!!2)
        let prior = readPriorFromStr (fromMySQLText ((rows!!0)!!3))
        let prior' = show prior
        let currentId' = currentId + 1
        if phraStruOflo /= phraStruOfro
          then do
            let query = DS.fromString ("select numOfLp, numOfRp from stru_gene_PhraStruCS where PhraStruOflo = ? and PhraStruOfro = ? and overType = ?")       -- Query is instance of IsString.
            stmt <- prepareStmt conn query
            (defs, is) <- queryStmt conn stmt [toMySQLText phraStruOflo, toMySQLText phraStruOfro, toMySQLInt8 ot]
            rows <- S.toList is

            if rows /= []
              then do
                let numOfLp = fromMySQLInt32 ((rows!!0)!!0)
                let numOfRp = fromMySQLInt32 ((rows!!0)!!1)
                let proportionOfLp = fromIntegral numOfLp / fromIntegral (numOfLp + numOfRp)
                let proportionOfRp = fromIntegral numOfRp / fromIntegral (numOfLp + numOfRp)
                let totalNumOfPred' = totalNumOfPred + 1
                if proportionOfLp >= proportion
                  then do
--                    putStrLn $ "id = " ++ show currentId ++ "'s proportionOfLp >= 0.7"
                    let numOfCorrPred' = case prior'== "Lp" of
                                           True -> numOfCorrPred + 1
                                           False -> numOfCorrPred
                    close conn
                    verifyEffectOfPhraStruCS proportion currentId' endId numOfCorrPred' ambiguNum totalNumOfPred'
                  else if proportionOfRp >= proportion
                         then do
--                           putStrLn $ "id = " ++ show currentId ++ "'s proportionOfRp >= 0.8"
                           let numOfCorrPred' = case prior'== "Rp" of
                                                  True -> numOfCorrPred + 1
                                                  False -> numOfCorrPred
                           close conn
                           verifyEffectOfPhraStruCS proportion currentId' endId numOfCorrPred' ambiguNum totalNumOfPred'
                         else do
                           let ambiguNum' = ambiguNum + 1
--                           putStrLn $ "id = " ++ show currentId ++ "'s prior is ambiguous.(proportionOfLp or proportionOfRp < 0.8)"
                           close conn
                           verifyEffectOfPhraStruCS proportion currentId' endId numOfCorrPred ambiguNum' totalNumOfPred'
              else error "verifyEffectOfPhraStruCS: rows is impossible null!"
          else do
            close conn
            verifyEffectOfPhraStruCS proportion currentId' endId numOfCorrPred ambiguNum totalNumOfPred
      else do
        let accuracy = fromIntegral numOfCorrPred / fromIntegral totalNumOfPred
        let numOfIncorrPred = totalNumOfPred - numOfCorrPred - ambiguNum
        let accuracy' = fromIntegral numOfCorrPred / fromIntegral (totalNumOfPred - ambiguNum)
        putStrLn $ "proportion = "++ show proportion ++ ", ambiguNum = " ++ show ambiguNum ++ ", numOfIncorrPred = " ++ show numOfIncorrPred ++ ", numOfCorrPred / totalNumOfPred = " ++ show numOfCorrPred ++ " / " ++ show totalNumOfPred ++ "=" ++ show accuracy ++ "/"++ show accuracy'

{- Parse sentences by grammar ambiguity resolution.
 - On category ambiguity, use model SLR. On syntax ambiguity, use model StruGene2 under identity similarity.
 -}
parseSentByGrammarAmbiResol :: SentIdx -> SentIdx -> IO ()
parseSentByGrammarAmbiResol startSn endSn = do
    confInfo <- readFile "Configuration"
    let sent_source = getConfProperty "sent_source" confInfo
    let tree_target = getConfProperty "tree_target" confInfo
    let cate_ambig_resol_source = getConfProperty "cate_ambig_resol_source" confInfo
    let cate_resol_sample_startsn = getConfProperty "cate_resol_sample_startsn" confInfo
    let cate_resol_sample_endsn = getConfProperty "cate_resol_sample_endsn" confInfo
    let category_ambig_resol_sample_update_switch = getConfProperty "category_ambig_resol_sample_update_switch" confInfo
    let syntax_ambig_resol_model = getConfProperty "syntax_ambig_resol_model" confInfo
    let syntax_resol_sample_startsn = getConfProperty "syntax_resol_sample_startsn" confInfo
    let syntax_resol_sample_endsn = getConfProperty "syntax_resol_sample_endsn" confInfo
    let syntax_ambig_resol_sample_update_switch = getConfProperty "syntax_ambig_resol_sample_update_switch" confInfo

    putStrLn $ " sent_source: " ++ sent_source
    putStrLn $ " tree_target: " ++ tree_target
    putStrLn $ " cate_ambig_resol_source: " ++ cate_ambig_resol_source
    putStrLn $ "   startSn = " ++ cate_resol_sample_startsn ++ ", endSn = " ++ cate_resol_sample_endsn
    putStrLn $ "   category_ambig_resol_sample_update_switch: " ++ category_ambig_resol_sample_update_switch
    putStrLn $ " syntax_ambig_resol_model: " ++ syntax_ambig_resol_model
    putStrLn $ "   startSn = " ++ syntax_resol_sample_startsn ++ ", endSn = " ++ syntax_resol_sample_endsn
    putStrLn $ "   syntax_ambig_resol_sample_update_switch: " ++ syntax_ambig_resol_sample_update_switch

    contOrNot <- getLineUntil ("Continue or not [c/n]? (RETURN for 'n') ") ["c","n"] False
    if contOrNot == "c"
      then do
        contOrNot2 <- getLineUntil ("Please confirm again continuing or not [c/n] (RETURN for 'n'): ") ["c","n"] False
        if contOrNot2 == "c"
          then do
            conn <- getConn
            let query = DS.fromString ("select SLR from " ++ cate_ambig_resol_source ++ " where serial_num >= ? and serial_num <= ?")
            stmt <- prepareStmt conn query
            (defs, is) <- queryStmt conn stmt [toMySQLInt32 (read cate_resol_sample_startsn :: Int), toMySQLInt32 (read cate_resol_sample_endsn :: Int)]

--    rows <- S.toList is
--    let sLRListOfSent = map (\x -> readSLROfSent (fromMySQLText (x!!0))) rows
            sLRStrListOfSent <- readStreamByText [] is                          -- [String], each string is the string of SLRs of one sentence.
            let sLRListOfSent = map readSLROfSent sLRStrListOfSent              -- [SLROfSent], namely [[SLROfClause]]
            let sLRListOfClause = foldl (++) [] sLRListOfSent                   -- [SLROfClause], namely [[SLROfATrans]]
            let sLRListOfTrans = foldl (++) [] sLRListOfClause                  -- [SLROfATrans]

            sentList <- getSentListFromDB startSn endSn
            parseSentByGrammarAmbiResol' startSn endSn sLRListOfTrans sentList tree_target
            close conn
          else putStrLn "Operation was canceled."
      else putStrLn "Operation was canceled."

parseSentByGrammarAmbiResol' :: SentIdx -> SentIdx -> SLROfClause -> [(SentIdx, String)] -> String -> IO ()
parseSentByGrammarAmbiResol' sn endSn sLR sentList tree_target = do
    putStrLn $ "The sentence of serial_num = " ++ show sn ++ " will be analyzed by grammar ambiguity resolution. "
    let sent = snd $ sentList!!0
    cs <- getSent sent
    let sn' = sn + 1
    let sentList' = tail sentList

    parseASentByGrammarAmbiResol sn cs sLR tree_target

    if sn' > endSn
      then putStrLn "parseSentByGrammarAmbiResol': End"
      else parseSentByGrammarAmbiResol' sn' endSn sLR sentList' tree_target

parseASentByGrammarAmbiResol :: SentIdx -> [String] -> SLROfClause -> String -> IO ()
parseASentByGrammarAmbiResol sn cs sLR tree_target = do
    conn <- getConn
    putStrLn $ " Num. of SLR samples = " ++ show (length sLR)
    let sqlstat = DS.fromString $ "create table if not exists " ++ tree_target ++ " (serial_num int primary key, tree mediumtext, script mediumtext, tree_check tinyint default 0)"
    stmt <- prepareStmt conn sqlstat
    executeStmt conn stmt []                          -- Create a new MySQL table for storing parsing result.
    let sqlstat = DS.fromString $ "select tree from " ++ tree_target ++ " where serial_num = ?"
    stmt <- prepareStmt conn sqlstat
    (defs, is) <- queryStmt conn stmt [toMySQLInt32 sn]           -- read a row with given serial_num.

    rows <- S.toList is                                                         -- [[MySQLText, MySQLText]]
    if rows == []
      then do
        let query' = DS.fromString ("insert into " ++ tree_target ++ " set tree = ?, script = ?, serial_num = ?")     -- Query is instance of IsString.
        stmt' <- prepareStmt conn query'
        ok <- executeStmt conn stmt' [toMySQLText "[]", toMySQLText "[]", toMySQLInt32 sn]
        let rn = getOkAffectedRows ok
        close conn
        case rn of
          1 -> putStrLn $ "parseASentByGrammarAmbiResol: " ++ show rn ++ " row(s) were inserted."
          0 -> putStrLn "parseASentByGrammarAmbiResol: No rows were inserted."
          _ -> putStrLn "parseASentByGrammarAmbiResol: Insert failed!"
      else do
        let query' = DS.fromString ("update " ++ tree_target ++ " set tree = ?, script = ? where serial_num = ?")   -- Query is instance of IsString.
        stmt' <- prepareStmt conn query'
        ok <- executeStmt conn stmt' [toMySQLText "[]", toMySQLText "[]", toMySQLInt32 sn]
        let rn = getOkAffectedRows ok
        close conn
        case rn of
          1 -> putStrLn $ "parseASentByGrammarAmbiResol: " ++ show rn ++ " row was initialized."
          0 -> putStrLn $ "parseASentByGrammarAmbiResol: No rows were changed."
          _ -> error "parseASentByGrammarAmbiResol: update failed."

    struGene2Samples <- getStruGene2Samples                                     -- [StruGene2Sample]
    if struGene2Samples /= []
      then do
        putStrLn $ " There are " ++ show (length cs) ++ " clauses in total."
--        let struGene2s = map (\x -> (snd7 x, thd7 x, fth7 x, fif7 x, sth7 x, svt7 x)) struGene2Samples       -- [StruGene2]
        let struGene2s = map (\x -> (fst7 x, snd7 x, thd7 x, fth7 x, fif7 x, sth7 x, svt7 x)) struGene2Samples -- [StruGene2Sample]
        parseASentByGrammarAmbiResol' sn cs sLR struGene2s
        putStrLn "parseASentByGrammarAmbiResol: Finished parsing."
      else error "parseASentByGrammarAmbiResol: struGene2Samples is Null."

parseASentByGrammarAmbiResol' :: SentIdx -> [String] -> SLROfClause -> [StruGene2Sample] -> IO ()
parseASentByGrammarAmbiResol' _ [] _ _ = return ()
parseASentByGrammarAmbiResol' sn cs sLR struGene2s = do
    parseASentByGrammarAmbiResol' sn (take (length cs - 1) cs) sLR struGene2s

    let clauIdx = length cs
    putStrLn $ "  ===== Clause No." ++ show clauIdx ++ " ====="
    let nPCs = initPhraCate $ getNCate $ words (last cs)
    putStr "Before parsing: "
    showNPhraCateLn nPCs
    putStr "Word semantic sequence: "
    showNSeman nPCs

    rtbPCs <- parseClauseWithGrammarAmbiResol (sn, clauIdx) [] nPCs [] sLR (length nPCs) struGene2s
                                             -- Parse begins with empty '[[Rule]]' and empty 'banPCs'
    storeClauseParsingToTreebank sn clauIdx rtbPCs                      -- Add the parsing result of this clause into database.

parseClauseWithGrammarAmbiResol :: ClauTag -> [[Rule]] -> [PhraCate] -> [BanPCs] -> SLROfClause -> Int -> [StruGene2Sample] -> IO ([[Rule]],[PhraCate],[BanPCs])
parseClauseWithGrammarAmbiResol clauTag rules nPCs banPCSets sLR lengthOfClause struGene2s = do
    rtbPCs <- doTransWithGrammarAmbiResol clauTag nPCs banPCSets sLR lengthOfClause struGene2s
                                               -- <rtbPCs> ::= ([Rule], resultant tree PCs, accumulated banned PCs)
                                               -- [Rule] is the set of rules used in this trip of transition.
    if rtbPCs == ([],[],[])
      then return ([],[],[])                   -- Return ([],[],[]) as the terminating flag.
      else if nPCs /= (snd3 rtbPCs)
        then
          parseClauseWithGrammarAmbiResol clauTag (rules ++ [fst3 rtbPCs]) (snd3 rtbPCs) (thd3 rtbPCs) sLR lengthOfClause struGene2s
                                               -- Do the next trip of transition
                                               -- with appended rules, resultant PCs, and accumulated banned PCs.
        else do                                -- Phrasal closure has been formed.
          putStrLn $ "Num. of phrasal categories in closure is " ++ (show $ length nPCs)
          showNPhraCateLn (sortPhraCateBySpan nPCs)
          let spls = divPhraCateBySpan (nPCs)
          putStrLn "  ##### Parsing Tree #####"
          showTreeStru spls spls
          return (rules ++ [fst3 rtbPCs], snd3 rtbPCs, thd3 rtbPCs)

-- Local definition of similarity degree.
type SimDeg = Double

{- Return merged Rule lists which have minimal distance. Before merging, rule lists are ordered by ascending similarity degrees.
 - Additionally, return the remaining (Dist, (ClauTag, [Rule])) value list.
 - For [(0.1,((1,2),[A/n]))], return ([A/n], []).
 - For [(0.1,((1,2),[A/n])),(0.1,((2,3),[S/v])),(0.2,((3,4),[Hn/a])),(0.4,((4,5),[O/n]))], return ([A/n, S/v], [(0.2,((3,4),[Hn/a])),(0.4,((4,5),[O/n])))]).
 - Do not merge rules when clausal tag was repeatedly hit.
 -   For [(-1.0,((30,1),[])),(-1.0,((30,1),[S/v])),(0.2,((3,4),[Hn/a]))], return [[],[(-1.0,((30,1),[S/v])),(0.2,((3,4),[Hn/a]))].
 -}
--getRuleListOfMinDist :: [(Double, (ClauTag, [Rule]))] -> ([Rule],[(Double,(ClauTag, [Rule]))])
getRuleListOfMinDist :: [(Double, (ClauTag, ([PhraSyn],[Rule])))] -> ([Rule],[(Double,(ClauTag, ([PhraSyn],[Rule])))])
getRuleListOfMinDist [] =  ([], [])
getRuleListOfMinDist [(_, (_, (_, rules)))] = (rules, [])
getRuleListOfMinDist (x:(y:zs))
    | fst x == -1.0 && fst y == -1.0 = ((snd. snd . snd) x, (y:zs))                  -- Do not merge rules when clausal tag was repeatedly hit.
--    | abs (fst x - fst y) < 1.0e-5 = getRuleListOfMinDist ((fst x, ((0, 0), (snd . snd) x ++ (snd . snd) y)):zs)
    | otherwise = ((snd. snd . snd) x, (y:zs))

{- Set distance below zero for every [(Dist, (ClauTag, [Rule]))] element which has distance 0.0 and given ClauTag value.
 - minDist: Minimal distance known to now. The initial value should be zero.
 - From the last to the head, check every sample.
 - If hitting given clausal tag, let its distance (minDist - 1e-4) and let the distance the new minimal distance.
 - Actually, there is always new generated phrases for every time of transition, namely stub trees always changes as transition happens.
 - For a clause, different transitions have different stub trees, and it is impossible of hitting a ClauTag value while having distance 0.0.
 -}
--setMinDist4ZeroDistAndClauTagHit :: Double -> ClauTag -> [(Double, (ClauTag, [Rule]))] -> [(Double, (ClauTag, [Rule]))]
setMinDist4ZeroDistAndClauTagHit :: Double -> ClauTag -> [(Double, (ClauTag, ([PhraSyn],[Rule])))] -> [(Double, (ClauTag, ([PhraSyn],[Rule])))]
setMinDist4ZeroDistAndClauTagHit _ _ [] = []
setMinDist4ZeroDistAndClauTagHit minDist clauTag xs = setMinDist4ZeroDistAndClauTagHit newMinDist clauTag (init xs) ++ [newX]
    where
    x = last xs
    newX = case (fst x < 1e-6 && (fst . snd) x == clauTag) of                   -- Hit
             True -> (minDist - 1e-4, snd x)                                    -- Distance = minDist - 1e-4
             False -> x
    newMinDist = case (fst newX < minDist) of
                    True -> fst newX
                    False -> minDist

{- One transition of category combinations, in which category ambiguity resolution is done based upon model SLR,
 - and syntax ambiguity resolution is done based upon model StruGene2.
 - SLROfClause :: [(ClauTag, (Stub, [Rule])]
 - Stub :: [PhraSyn]
 -}
doTransWithGrammarAmbiResol :: ClauTag -> [PhraCate] -> [BanPCs] -> SLROfClause -> Int -> [StruGene2Sample] -> IO ([Rule], [PhraCate], [BanPCs])
doTransWithGrammarAmbiResol clauTag nPCs banPCSets sLR lengthOfClause struGene2s = do
--    putStrLn "nPCs="
--    showNPhraCateLn nPCs
    let sStub = ctpsOfCateList' nPCs                                            -- [PhraSyn] of stub tree
--    putStrLn "sStub="
--    showNPhraSynLn sStub
    let sSLRBase = map (\x -> (fst x, (ctpsOfCateList' ((fst . snd) x), (snd . snd) x))) sLR    -- [(ClauTag, ([PhraSyn],[Rule]))]
--    let distRuleListWithoutSort =  map (\x -> (distPhraSynSetByIdentity sStub ((fst . snd) x), (fst x, (snd . snd) x))) sSLRBase  -- [(Dist, (ClauTag, [Rule]))]
    let distRuleListWithoutSort =  map (\x -> (distPhraSynSetByIdentity sStub ((fst . snd) x), x)) sSLRBase  -- [(Dist, (ClauTag, ([PhraSyn], [Rule])))]
--    putStrLn $ "distRuleListWithoutSort = " ++ show (formatDoubleAList (take 10 distRuleListWithoutSort) 4)

    let distRuleListHitFirst = sortOn fst $ setMinDist4ZeroDistAndClauTagHit 0.0 clauTag distRuleListWithoutSort    -- Set negative distances for hit SLR samples and sort
--    putStrLn $ "distRuleListHitFirst = " ++ show (formatDoubleAList (take 10 distRuleListHitFirst) 4)

    let distRuleList = nubBy (\x y -> (snd . snd) x == (snd . snd) y) $ distRuleListHitFirst    -- Ascending SimDegs and no duplicate [Rule]
    putStrLn $ "distRuleList = " ++ show (formatDoubleAList (take 10 (map (\x -> (fst x, (((fst . snd) x, ((snd . snd . snd) x))))) distRuleList)) 4)    -- [(Dist, (ClauTag, ([PhraCate], [Rule])))]

    let ruleListAndDistRuleList = getRuleListOfMinDist distRuleList             -- ([Rule], [(Double, (ClauTag, ([PhraCate], [Rule])))])
    let distRuleList' = snd ruleListAndDistRuleList                             -- Remaining distance rule list
    let rules = fst ruleListAndDistRuleList                                     -- [Rule]

    putStr "Rule switches: "
    showOnOff rules                                  -- Display rule switches
    let nPCs2 = trans rules nPCs banPCSets           -- Without pruning, get transitive result.

    putStr "New phrases before pruning: "
    let pcs = [pc | pc <- nPCs2, notElem4Phrase pc nPCs]                        -- [PhraCate] of NEW phrases
    showNPhraCateLn pcs
    putStr "Banned phrases: "
    showNPhraCateListLn banPCSets                                               -- The banned phrases before updated.

    let spanList = map spOfCate nPCs                                            -- [Span]
    if pcs == [] && (maximum spanList) /= lengthOfClause - 1                    -- No new phrases was created, and parsing tree does NOT yet formed.
      then if distRuleList' /= []
             then doTransWithGrammarAmbiResol' clauTag nPCs banPCSets sLR distRuleList' lengthOfClause struGene2s     -- -- There are SLR samples available.
             else return (rules, nPCs2, banPCSets ++ [[]])                      -- No SLR sample is availble.
      else if pcs /= []
             then do
               let pcps = getOverlap nPCs2                    -- [(PhraCate, PhraCate)]
               overPairs <- ambiResolByStruGene2 "StruGeneIdentity" clauTag nPCs2 [] pcps struGene2s    -- [OverPair], namely [(PhraCate, PhraCate, Prior)]
               putStr "doTransWithGrammarAmbiResol: overPairs: "
               showNOverPairLn overPairs

               nbPCs <- transWithPruning rules nPCs banPCSets overPairs         -- Get transitive result with pruning.
               putStr "New phrases after pruning: "
               showNPhraCateLn [pc | pc <- fst nbPCs, notElem4Phrase pc nPCs]
               putStr "Banned phrases: "
               showNPhraCateListLn (snd nbPCs)                                  -- The banned phrases after updated.
               return (rules,(fst nbPCs),(snd nbPCs))
             else return (rules, nPCs2, banPCSets ++ [[]])                      -- No new phrases were created.

{- This function is called when one time of transition creates no new phrase before pruning and parsing tree is NOT formed.
 - Recursively use [Rule] with next miminal distance to do category conversions.
 -}
doTransWithGrammarAmbiResol' :: ClauTag -> [PhraCate] -> [BanPCs] -> SLROfClause -> [(Double,(ClauTag, ([PhraSyn],[Rule])))] -> Int -> [StruGene2Sample] -> IO ([Rule], [PhraCate], [BanPCs])
doTransWithGrammarAmbiResol' clauTag nPCs banPCSets sLR distRuleList lengthOfClause struGene2s = do
--    putStrLn $ "doTransWithGrammarAmbiResol' distRuleList = " ++ show (formatDoubleAList (take 10 distRuleList) 4)    -- [(Dist, (ClauTag, ([PhraCate], [Rule])))]

    let ruleListAndDistRuleList = getRuleListOfMinDist distRuleList             -- ([Rule], [(Double, (ClauTag, ([PhraCate], [Rule])))])
    let distRuleList' = snd ruleListAndDistRuleList                             -- [(Double, (ClauTag, ([PhraCate], [Rule])))], the remaining distance rules list
    let rules = fst ruleListAndDistRuleList                                     -- [Rule]

    putStr "Rule switches: "
    showOnOff rules                              -- Display rule switches
    let nPCs2 = trans rules nPCs banPCSets      -- Without pruning, get transitive result.

    putStr "New phrases before pruning: "
    let pcs = [pc | pc <- nPCs2, notElem4Phrase pc nPCs]
    showNPhraCateLn pcs
    let spanList = map (\x -> spOfCate x) nPCs
    if pcs == [] && (maximum spanList) /= lengthOfClause - 1
      then if distRuleList' /= []
             then doTransWithGrammarAmbiResol' clauTag nPCs banPCSets sLR distRuleList' lengthOfClause struGene2s
             else return (rules, nPCs2, banPCSets ++ [[]])                      -- No SLR sample is available.
      else if pcs /= []
             then do
               let pcps = getOverlap nPCs2                    -- [(PhraCate, PhraCate)]
               overPairs <- ambiResolByStruGene2 "StruGeneIdentity" clauTag nPCs2 [] pcps struGene2s     -- [OverPair], namely [(PhraCate, PhraCate, Prior)]
               putStr "doTransWithGrammarAmbiResol: overPairs: "
               showNOverPairLn overPairs

               nbPCs <- transWithPruning rules nPCs banPCSets overPairs         -- Get transitive result with pruning.
               putStr "New phrases after pruning: "
               showNPhraCateLn [pc | pc <- fst nbPCs, notElem4Phrase pc nPCs]
               putStr "Banned phrases: "
               showNPhraCateListLn (snd nbPCs)                                  -- The banned phrases after updated.
               return (rules,(fst nbPCs),(snd nbPCs))
             else return (rules, nPCs2, banPCSets ++ [[]])

{- This function is obsoleted, and has been replaced with ambiResolByStruGene2.
 -}
ambiResolByGrammarAmbiResol :: ClauTag -> [PhraCate] -> [OverPairid] -> [(PhraCate, PhraCate)] -> [StruGene2Sample] -> DistWeiRatioList -> [OverPairid]
ambiResolByGrammarAmbiResol _ _ overPairs [] _ _ = overPairs
ambiResolByGrammarAmbiResol clauTag nPCs overPairs (pcp:pcps) struGene2Samples distWeiRatioList
    = ambiResolByGrammarAmbiResol clauTag nPCs overPairs' pcps struGene2Samples distWeiRatioList
    where
    lop = fst pcp
    rop = snd pcp
    ot = getOverType nPCs lop rop                      -- Get overlapping type
    leps = getPhraByEnd (stOfCate lop - 1) nPCs        -- Get all left-extend phrases
    reps = getPhraByStart (enOfCate rop + 1) nPCs      -- Get all right-entend phrases

    le = map ((!!0) . ctpsOfCate) leps         -- [(Category,Tag,PhraStru,Span)] of left-extended phrases
    lo = (ctpsOfCate lop)!!0                    -- (Category,Tag,PhraStru,Span) of left-overlapping phrase
    ro = (ctpsOfCate rop)!!0                    -- (Category,Tag,PhraStru,Span) of right-overlapping phrase
    re = map ((!!0) . ctpsOfCate) reps          -- [(Category,Tag,PhraStru,Span)] of right-extended phrases
                                                 -- The value is not used, just acted as place holder.
    contextOfSG = (le,lo,ro,re,ot)
    distList = map (\x -> (dist4ContextOfSGByWeightSum contextOfSG (snd7 x, thd7 x, fth7 x, fif7 x, sth7 x) distWeiRatioList, x)) struGene2Samples
    distList' = sortOn fst distList                                             -- [(Double, StruGene2Sample)]
--    minDist = minimum distList
--    idx = elemIndex minDist distList
--    idx' = case idx of
--             Just x -> x
--             Nothing -> -1                                                    -- Impossible position
    sampleWithMinDist = snd (head distList')                                    -- StruGene2Sample
    clauTagPriorList = svt7 sampleWithMinDist                                   -- [ClauTagPrior]
    hitClauTagPriorList = filterInCTPListByClauTag clauTag clauTagPriorList     -- [ClauTagPrior]

    prior = case hitClauTagPriorList of
              [] -> (fromMaybePrior . priorWithHighestFreq) clauTagPriorList    -- Prior
              _  -> snd (hitClauTagPriorList!!0)                                -- Prior

    id = fst7 sampleWithMinDist                                                 -- SIdx
    overPairs' = (lop, rop, prior, id):overPairs

autoRunCollectPhraSyn :: SentIdx -> SentIdx -> IO ()
autoRunCollectPhraSyn startSn endSn = do
    collectPhraSynFromTreebank1 startSn
    let startSn' = startSn +1
    if startSn' <= endSn
      then autoRunCollectPhraSyn startSn' endSn
      else putStrLn "autoRunParseSentByScript: End."

-- 为了方便观察(Category, Tag, PhraStru)中各元素的关系
-- 从Treebank1的tree字段收集所有短语的(Category, Tag, PhraStru)，不包括词
collectPhraSynFromTreebank1 :: Int -> IO ()
collectPhraSynFromTreebank1 sn = do
    conn <- getConn
    let sqlstat = DS.fromString $ "create table if not exists PhraSynFromTree (id int primary key auto_increment, PhraSyn varchar(200))"
    stmt <- prepareStmt conn sqlstat
    executeStmt conn stmt []

    let query = DS.fromString ("select tree from treebank1 where serial_num = ?")       -- Query is instance of IsString.
    stmt <- prepareStmt conn query
    (defs, is) <- queryStmt conn stmt [toMySQLInt32 sn]
    rows <- S.toList is
    let origTrees = readTrees $ fromMySQLText ((rows!!0)!!0)
    let origTrees' = foldl (++) [] (map (\t -> snd t) origTrees)
    let phraseOfSpan0 = getPhraBySpan 0 origTrees'
    let phraseWithoutSpan0 = [x| x <- origTrees', notElem x phraseOfSpan0]
    let phraSynsOfASent = ctpsOfCateList phraseWithoutSpan0 []
    storeAPhraSynToDS phraSynsOfASent sn
    close conn

storeAPhraSynToDS :: [PhraSyn] -> Int -> IO ()
storeAPhraSynToDS (x:xs) sn = do
    conn <- getConn
    let query = DS.fromString ("insert into PhraSynFromTree set PhraSyn = ?")
    stmt <- prepareStmt conn query
    ok <- executeStmt conn stmt [toMySQLText (phraSynToString x)]
    let rn = getOkAffectedRows ok
    close conn
    if xs /= []
      then storeAPhraSynToDS xs sn
      else putStrLn $ "Sentence No." ++ show sn ++ "'s phraSyns store end."

{- Parsing sentences by automatically resolving syntactic ambiguity.
 - Resolving syntactic ambiguity follows the StruGene sample with highest context similarity degree.
 - Similarity degrees are calculated based on comparing grammatic attributes between two phrases.
 - "StruGeneIdentity": One comparison method which only considers attribute to be identical or not.
 - "StruGeneEmbedded": One comparison method which measures differences of concurrent grammatic attributes by embedded grammatic environment.
 -}
parseSentByStruGeneFromConf :: SynAmbiResolMethod -> IO ()
parseSentByStruGeneFromConf resolMethod = do
    confInfo <- readFile "Configuration"
    let script_source = getConfProperty "script_source" confInfo
    let tree_target = getConfProperty "tree_target" confInfo
    let phra_gram_dist_algo = getConfProperty "phra_gram_dist_algo" confInfo
    let overlap_type_dist_algo = getConfProperty "overlap_type_dist_algo" confInfo
    let strugene_context_dist_algo = getConfProperty "strugene_context_dist_algo" confInfo
    let syntax_resol_sample_startsn = getConfProperty "syntax_resol_sample_startsn" confInfo
    let syntax_resol_sample_endsn = getConfProperty "syntax_resol_sample_endsn" confInfo
    let syntax_ambig_resol_sample_update_switch = getConfProperty "syntax_ambig_resol_sample_update_switch" confInfo
    let category_ambig_resol_sample_update_switch = getConfProperty "category_ambig_resol_sample_update_switch" confInfo

    putStrLn $ " tree_target: " ++ tree_target
    putStrLn $ " phra_gram_dist_algo: " ++ phra_gram_dist_algo
    putStrLn $ " overlap_type_dist_algo: " ++ overlap_type_dist_algo
    putStrLn $ " strugene_context_dist_algo: " ++ strugene_context_dist_algo
    putStrLn $ " syntax_resol_sample_startsn: " ++ syntax_resol_sample_startsn
    putStrLn $ " syntax_resol_sample_endsn: " ++ syntax_resol_sample_endsn
    putStrLn $ " syntax_ambig_resol_sample_update_switch: " ++ syntax_ambig_resol_sample_update_switch
    putStrLn $ " category_ambig_resol_sample_update_switch: " ++ category_ambig_resol_sample_update_switch

    contOrNot <- getLineUntil ("Continue or not [c/n]? (RETURN for 'n') ") ["c","n"] False
    if contOrNot == "c"
      then do
        contOrNot2 <- getLineUntil ("Please confirm again continuing or not [c/n] (RETURN for 'n'): ") ["c","n"] False
        if contOrNot2 == "c"
          then do
            let startIdx = getConfProperty "defaultStartIdx" confInfo
            let startSn = read startIdx :: Int
            let endIdx = getConfProperty "defaultEndIdx" confInfo
            let endSn = read endIdx :: Int

            startSn <- getNumUntil ("Please input which sentence to start [" ++ show startSn ++ " .. " ++ show endSn ++ "]: ")  [startSn .. endSn]
            endSn <- getNumUntil ("Please input which sentence to end [" ++ show startSn ++ " .. " ++ show endSn ++ "]: ") [startSn .. endSn]
            putStrLn $ "startSn = " ++ show startSn ++ ", endSn = " ++ show endSn
            sentList <- getSentListFromDB startSn endSn
            parseSentByStruGene resolMethod startSn endSn sentList script_source tree_target

          else putStrLn "Operation was canceled."
      else putStrLn "Operation was canceled."

{- Recursively parse all sentences and resolve syntactic ambiguity by StruGene sample which has highest similarity degree.
 -}
parseSentByStruGene :: SynAmbiResolMethod -> SentIdx -> SentIdx -> [(Int, String)] -> String -> String -> IO ()
parseSentByStruGene resolMethod startIdx endIdx sentList script_source tree_target = do
    putStrLn $ "Parsing sentence No." ++ show startIdx
    let sent = snd $ sentList!!0                    -- String of a sentence
    cs <- getSent sent                              -- [String] of clauses of a sentence
    let startIdx' = startIdx + 1
    let sentList' = tail sentList

    parseASentByStruGene resolMethod startIdx cs script_source tree_target
    if startIdx' > endIdx
      then putStrLn "parseSentByStruGene: End."
      else parseSentByStruGene resolMethod startIdx' endIdx sentList' script_source tree_target

{- Parse a sentence, in which lexical ambiguities are resolved according the previously created parsing script,
 - and syntactic ambiguities are resolved by StruGene sample with highest similarity degree context,
 - Every clause is a String, and parsing starts from the first clause.
 - startIdx: The value of 'serial_num' in database Table 'corpus'.
 - cs: The list of clausal strings.
 -}
parseASentByStruGene :: SynAmbiResolMethod -> SentIdx -> [String] -> String -> String -> IO ()
parseASentByStruGene resolMethod sn cs script_source tree_target = do
    conn <- getConn
    let query = DS.fromString ("select script from " ++ script_source ++ " where serial_num = ?")    -- Query is instance of IsString.
    stmt <- prepareStmt conn query
    (defs, is) <- queryStmt conn stmt [toMySQLInt32 sn]                     --([ColumnDef], InputStream [MySQLValue])
    record <- S.read is
    let record' = case record of
                    Just x -> x
                    Nothing -> [MySQLText "[]"]                             -- No record

    let script = fromMySQLText (record'!!0)
    skipToEof is                                                            -- Go to the end of the stream.
    closeStmt conn stmt

    let script' = readScripts $ script                                      -- [Script]
--    putStr "Parsing script: "
--    showScripts script'

    let sqlstat = DS.fromString $ "create table if not exists " ++ tree_target ++ " (serial_num int primary key, tree mediumtext, script mediumtext, accuracy float)"
    stmt <- prepareStmt conn sqlstat
    executeStmt conn stmt []                          -- Create a new MySQL table for storing parsing result.

    let sqlstat = DS.fromString $ "select tree from " ++ tree_target ++ " where serial_num = ?"
    stmt <- prepareStmt conn sqlstat
    (defs, is) <- queryStmt conn stmt [toMySQLInt32 sn]           -- read a row with given serial_num.

    rows <- S.toList is                                                         -- [[MySQLText, MySQLText]]
    if rows == []
      then do
        let query' = DS.fromString ("insert into " ++ tree_target ++ " set tree = ?, script = ?, serial_num = ?")     -- Query is instance of IsString.
        stmt' <- prepareStmt conn query'
        ok <- executeStmt conn stmt' [toMySQLText "[]", toMySQLText "[]", toMySQLInt32 sn]
        let rn = getOkAffectedRows ok
        close conn
        if (rn /= 0)
          then putStrLn $ "parseASentByStruGeneIdentitySim: " ++ show rn ++ " row(s) were inserted."
          else error "parseASentByStruGeneIdentitySim: Insert failed!"
      else do
        let query' = DS.fromString ("update " ++ tree_target ++ " set tree = ?, script = ? where serial_num = ?")
        stmt' <- prepareStmt conn query'
        ok <- executeStmt conn stmt' [toMySQLText "[]", toMySQLText "[]", toMySQLInt32 sn]
        let rn = getOkAffectedRows ok
        close conn
        putStrLn $ "parseASentByStruGene: " ++ show rn ++ " row(s) were initialized."

    confInfo <- readFile "Configuration"
    let syntax_ambig_resol_model = getConfProperty "syntax_ambig_resol_model" confInfo
    case syntax_ambig_resol_model of
      "stru_gene" -> do
        struGeneSamples <- getStruGeneSamples                                   -- [StruGeneSample]
        if struGeneSamples /= []
          then do
            let struGenes = map (\x -> (snd7 x, thd7 x, fth7 x, fif7 x, sth7 x, svt7 x)) struGeneSamples      -- [StruGene]
            putStrLn $ " There are " ++ show (length cs) ++ " clauses in total."
            parseASentByStruGene' resolMethod sn cs script' struGenes
            putStrLn "parseASentByStruGene: Finished parsing."
          else error "parseASentByStruGene: struGeneSamples is Null."
      x | elem x ["stru_gene_202408", "stru_gene_202412", "stru_gene_202501"] -> do        -- Multimodel
        struGene2Samples <- getStruGene2Samples                                 -- [StruGene2Sample]
        if struGene2Samples /= []
          then do
--            let struGene2s = map (\x -> (snd7 x, thd7 x, fth7 x, fif7 x, sth7 x, svt7 x)) struGene2Samples    -- [StruGene2]
            let struGene2s = map (\x -> (fst7 x, snd7 x, thd7 x, fth7 x, fif7 x, sth7 x, svt7 x)) struGene2Samples    -- [StruGene2]
            putStrLn $ " There are " ++ show (length cs) ++ " clauses in total."
            parseASentByStruGene2' resolMethod sn cs script' struGene2s
            putStrLn "parseASentByStruGene2: Finished parsing."
          else error "parseASentByStruGene2: struGene2Samples is Null."

{- Parse a sentence, in which lexical ambiguities are resolved according the previously created parsing script,
 - and syntactic ambiguities are resolved by StruGene samples or their clustering result.
 - Here every clause is a String.
 - Parameter 'resolMethod' is the name of one kind of syntactic ambiguity resolution method.
 - Parameter 'sentIdx' is the value of 'serial_num' in database Table 'corpus'.
 - Table 'corpus', 'treebank1', and some other tables are associated with field 'serial_num'.
 - 'cs' is clausal strings to be parsed.
 - 'scripts' is parsing scripts for these clauses.
 - 'struGenes' is a list of modes or StruGene samples.
 - If a certain clause is not finished in parsing, return False to skip the remaining clauses.
 -}
parseASentByStruGene' :: SynAmbiResolMethod -> SentIdx -> [String] -> [Script] -> [StruGene] -> IO ()
parseASentByStruGene' _ _ [] _ _ = return ()
parseASentByStruGene' resolMethod sentIdx cs scripts struGenes = do
    let clauIdx = length cs
    parseASentByStruGene' resolMethod sentIdx (take (length cs - 1) cs) (take (length cs - 1) scripts) struGenes   -- Recursively

    putStrLn $ "  ===== Clause No." ++ show clauIdx ++ " ====="
    let nPCs = initPhraCate $ getNCate $ words (last cs)
    putStr "Before parsing: "
    showNPhraCateLn nPCs
    putStr "Word semantic sequence: "
    showNSeman nPCs

    let lastScript = case scripts of                                        -- It's possible of no script to use.
                       [] -> (clauIdx, [], [])                              -- Null script for clause 'clauIdx'
                       ss -> last ss

    rtbPCs <- parseClauseWithStruGene resolMethod (sentIdx, clauIdx) [] nPCs [] lastScript struGenes
                                             -- Parse begins with empty '[[Rule]]' and empty 'banPCs'
    storeClauseParsingToTreebank sentIdx clauIdx rtbPCs                     -- Add the parsing result of this clause into database.

{- Parse a sentence, in which lexical ambiguities are resolved according the previously created parsing script,
 - and syntactic ambiguities are resolved by StruGene2 samples or their clustering result.
 - Here every clause is a String.
 - Parameter 'resolMethod' is the name of one kind of syntactic ambiguity resolution method.
 - Parameter 'sentIdx' is the value of 'serial_num' in database Table 'corpus'.
 - Table 'corpus', 'treebank1', and some other tables are associated with field 'serial_num'.
 - 'cs' is clausal strings to be parsed.
 - 'scripts' is parsing scripts for these clauses.
 - 'struGene2s' is a list of modes or StruGene2 samples.
 - If a certain clause is not finished in parsing, return False to skip the remaining clauses.
 -}
parseASentByStruGene2' :: SynAmbiResolMethod -> SentIdx -> [String] -> [Script] -> [StruGene2Sample] -> IO ()
parseASentByStruGene2' _ _ [] _ _ = return ()
parseASentByStruGene2' resolMethod sentIdx cs scripts struGene2s = do
    let clauIdx = length cs
    parseASentByStruGene2' resolMethod sentIdx (take (length cs - 1) cs) (take (length cs - 1) scripts) struGene2s   -- Recursively

    putStrLn $ "  ===== Clause No." ++ show clauIdx ++ " ====="
    let nPCs = initPhraCate $ getNCate $ words (last cs)
    putStr "Before parsing: "
    showNPhraCateLn nPCs
    putStr "Word semantic sequence: "
    showNSeman nPCs

    let lastScript = case scripts of                                        -- It's possible of no script to use.
                       [] -> (clauIdx, [], [])                              -- Null script for clause 'clauIdx'
                       ss -> last ss

    rtbPCs <- parseClauseWithStruGene2 resolMethod (sentIdx, clauIdx) [] nPCs [] lastScript struGene2s
                                             -- Parse begins with empty '[[Rule]]' and empty 'banPCs'
    storeClauseParsingToTreebank sentIdx clauIdx rtbPCs                     -- Add the parsing result of this clause into database.

{- This function is for 1st generation of model StruGene, and now is obsoleted.
 - Parsing a clause is a recursive transition process.
 - Input: A sequence of [Rule], a sequence of phrasal categories, a sequence of banned phrasal categories, a parsing script of this clause,
          a sequence of modes and a sequence of distance weights.
 - Algo.:
 - (1) Do one trip of transition;
 - (2) If creating new phrasal categories, append rules used in this trip to [[Rule]], take resultant phrases and accumulated banned
 -     phrases as input, go (1); Otherwise, return the triple ([[Rule]], resultant tree PCs, accumulated banned PCs).
 - Syntax ambiguity resolution is done by machine.
 -}
parseClauseWithStruGene :: SynAmbiResolMethod -> ClauTag -> [[Rule]] -> [PhraCate] -> [BanPCs] -> Script -> [StruGene] -> IO ([[Rule]],[PhraCate],[BanPCs])
parseClauseWithStruGene resolMethod clauTag rules nPCs banPCSets script struGenes = do
    rtbPCs <- doTransWithStruGene resolMethod clauTag nPCs banPCSets script struGenes
                                               -- <rtbPCs> ::= ([Rule], resultant tree PCs, [[PhraCate]])
                                               -- [Rule] is the set of rules used in this trip of transition.
                                               -- [[PhraCate]] is banned phrases in all rounds of transition, and one [PhraCate] per transition.
    if rtbPCs == ([],[],[])
      then return ([],[],[])                   -- Return ([],[],[]) as the terminating flag.
      else if nPCs /= (snd3 rtbPCs)
        then do
          let scriptTail = case script of
                              (clauIdx, [], bPCSets) -> (clauIdx, [], bPCSets)        -- Null script
                              (clauIdx, [x], bPCSets) -> (clauIdx, [], tail bPCSets)  -- Remove the head elements of OnOff and [BanPCs] lists in parsing script.
                              (clauIdx, (x:xs), bPCSets) -> (clauIdx, xs, tail bPCSets)

          parseClauseWithStruGene resolMethod clauTag (rules ++ [fst3 rtbPCs]) (snd3 rtbPCs) (thd3 rtbPCs) scriptTail struGenes
                                               -- Do the next trip of transition
                                               -- with appended rules, resultant PCs, and accumulated banned PCs.
        else do                                -- Phrasal closure has been formed.
          putStrLn $ "Num. of phrasal categories in closure is " ++ (show $ length nPCs)
          showNPhraCateLn (sortPhraCateBySpan nPCs)
          let spls = divPhraCateBySpan (nPCs)
          putStrLn "  ##### Parsing Tree #####"
          showTreeStru spls spls
          return (rules ++ [fst3 rtbPCs], snd3 rtbPCs, thd3 rtbPCs)

{- Parsing a clause is a recursive transition process.
 - Input: A sequence of [Rule], a sequence of phrasal categories, a sequence of banned phrasal categories, a parsing script of this clause,
          a sequence of modes and a sequence of distance weights.
 - Algo.:
 - (1) Do one trip of transition;
 - (2) If creating new phrasal categories, append rules used in this trip to [[Rule]], take resultant phrases and accumulated banned
 -     phrases as input, go (1); Otherwise, return the triple ([[Rule]], resultant tree PCs, accumulated banned PCs).
 - Syntax ambiguity resolution is done by machine.
 -}
parseClauseWithStruGene2 :: SynAmbiResolMethod -> ClauTag -> [[Rule]] -> [PhraCate] -> [BanPCs] -> Script -> [StruGene2Sample] -> IO ([[Rule]],[PhraCate],[BanPCs])
parseClauseWithStruGene2 resolMethod clauTag rules nPCs banPCSets script struGene2s = do
    rtbPCs <- doTransWithStruGene2 resolMethod clauTag nPCs banPCSets script struGene2s
                                               -- <rtbPCs> ::= ([Rule], resultant tree PCs, accumulated banned PCs [BanPCs])
                                               -- [Rule] is the set of rules used in this trip of transition.
    if rtbPCs == ([],[],[])
      then return ([],[],[])                   -- Return ([],[],[]) as the terminating flag.
      else if nPCs /= (snd3 rtbPCs)
        then do
          let scriptTail = case script of
                              (clauIdx, [], []) -> error "parseClauseWithStruGene2: (clauIdx, [], [])"       -- Has not been parsed.
                              (clauIdx, [x], _) -> (clauIdx, [[]], [[]])        -- Add script for one transition with no category conversion and no banned phrases.
                              (clauIdx, (x:xs), (b:bs)) -> (clauIdx, xs, bs)    -- Remove the head elements of OnOff and [BanPCs] list in parsing script.

          parseClauseWithStruGene2 resolMethod clauTag (rules ++ [fst3 rtbPCs]) (snd3 rtbPCs) (thd3 rtbPCs) scriptTail struGene2s
                                               -- Do the next trip of transition
                                               -- with appended rules, resultant PCs, and accumulated banned PCs.
        else do                                -- Phrasal closure has been formed.
          putStrLn $ "Num. of phrasal categories in closure is " ++ (show $ length nPCs)
          showNPhraCateLn (sortPhraCateBySpan nPCs)
          let spls = divPhraCateBySpan (nPCs)
          putStrLn "  ##### Parsing Tree #####"
          showTreeStru spls spls
          return (rules ++ [fst3 rtbPCs], snd3 rtbPCs, thd3 rtbPCs)

{- This function is for 1st generation of model StruGene, and now is obsoleted.
 - Do a trip of transition, and return the category-converted rules used in this trip, the resultant phrases, and the banned phrases.
 - resolMethod: One kind of syntactic ambiguity resolution method
 - clauTag: (SentIdx, ClauIdx)
 - nPCs: The current phrase set
 - banPCSets: The list of banned phrase sets
 - script: The parsing script of this clause
 - struGenes: The list of modes or StruGene values
 - onOff: The category conversion list for this transition
 - nbPCs: The tuple (phrase set, banned set) after this transition
 - (onOff,(fst nbPCs),(snd nbPCs)): The returned overlap phrase pair set
 -}
doTransWithStruGene :: SynAmbiResolMethod -> ClauTag -> [PhraCate] -> [BanPCs] -> Script -> [StruGene] -> IO ([Rule], [PhraCate], [BanPCs])
doTransWithStruGene resolMethod clauTag nPCs banPCSets script struGenes = do
    let onOffs = snd3 script
    let onOff = case onOffs of                      -- Get rule switches of this trip of transition
                      [] -> [] :: OnOff             -- No script of rule switches to use
                      (x:_) -> x :: OnOff           -- There is a script of rule switches to use

    putStr "Rule switches: "
    showOnOff onOff                              -- Display rule switches
    let nPCs2 = trans onOff nPCs banPCSets      -- Without pruning, get transitive result.

    putStr "New phrases before pruning: "
    showNPhraCateLn [pc | pc <- nPCs2, notElem4Phrase pc nPCs]

    let pcps = getOverlap nPCs2                    -- [(PhraCate, PhraCate)]
    overPairs <- ambiResolByStruGene resolMethod clauTag nPCs2 [] pcps struGenes     -- [OverPair], namely [(PhraCate, PhraCate, Prior)]
    putStr "doTransWithStruGene: overPairs: "
    showNOverPairLn overPairs

    nbPCs <- transWithPruning onOff nPCs banPCSets overPairs                        -- Get transitive result with pruning.

    putStr "New phrases after pruning: "
    showNPhraCateLn [pc | pc <- fst nbPCs, notElem4Phrase pc nPCs]
    putStr "Banned phrases: "
    showNPhraCateListLn (snd nbPCs)                    -- The banned phrases after updated.

    return (onOff,(fst nbPCs),(snd nbPCs))

{- Do a trip of transition, and return the category-converted rules used in this trip, the resultant phrases, and the banned phrases.
 - resolMethod: One kind of syntactic ambiguity resolution method
 - clauTag: (SentIdx, ClauIdx)
 - nPCs: The current phrase set
 - banPCSets: The list of banned phrase sets
 - script: The parsing script of this clause
 - struGene2s: The list of modes or StruGene2 values
 - onOff: The category conversion list for this transition
 - nbPCs: The tuple (phrase set, banned phrase set list) after this transition
 - (onOff,(fst nbPCs),(snd nbPCs)): The returned overlap phrase pair set
 -}
doTransWithStruGene2 :: SynAmbiResolMethod -> ClauTag -> [PhraCate] -> [BanPCs] -> Script -> [StruGene2Sample] -> IO ([Rule], [PhraCate], [BanPCs])
doTransWithStruGene2 resolMethod clauTag nPCs banPCSets script struGene2s = do
    let onOffs = snd3 script
    let onOff = case onOffs of                      -- Get rule switches of this trip of transition
                      [] -> [] :: OnOff             -- No script of rule switches to use
                      (x:_) -> x :: OnOff           -- There is a script of rule switches to use

    putStr "Rule switches: "
    showOnOff onOff                              -- Display rule switches
    let nPCs2 = trans onOff nPCs banPCSets      -- Without pruning, get transitive result.

    putStr "New phrases before pruning: "
    showNPhraCateLn [pc | pc <- nPCs2, notElem4Phrase pc nPCs]

    let pcps = getOverlap nPCs2                    -- [(PhraCate, PhraCate)]
    overPairs <- ambiResolByStruGene2 resolMethod clauTag nPCs2 [] pcps struGene2s    -- [OverPair], namely [(PhraCate, PhraCate, Prior)]
    putStr "doTransWithStruGene2: overPairs: "
    showNOverPairLn overPairs

    let banPCsFromScript = head (thd3 script)
    putStr "doTransWithStruGene2: Phrases banned by script: "
    showNPhraCateLn banPCsFromScript

    let banPCsByOverPairs = getBanPCsByOverPairs overPairs                      -- BanPCs
    if (sortPhraCateBySpan' banPCsFromScript == sortPhraCateBySpan' banPCsByOverPairs)
      then putStrLn "Phrases banned by StruGene2 and by script ARE same."
      else putStrLn "Phrases banned by StruGene2 and by script are NOT same."

    nbPCs <- transWithPruning onOff nPCs banPCSets overPairs                          -- Get transitive result with pruning.

    putStr "New phrases after pruning: "
    showNPhraCateLn [pc | pc <- fst nbPCs, notElem4Phrase pc nPCs]
    putStr "Banned phrases: "
    showNPhraCateListLn (snd nbPCs)                    -- The banned phrases after updated.

    return (onOff,(fst nbPCs),(snd nbPCs))

{- Resolve ambiguities by StruGene samples. This function is obsoleted.
 - resolMethod: One kind of syntactic ambiguity resolution method
 - clauTag: (SentIdx, ClauIdx)
 - nPCs: The current phrase set
 - overPairs: The overlap phrase pair set
 - (pcp:pcps): The overlap phrase pair set
 - struGenes: The list of modes or StruGene values
 - Algo.:
 -   (1) get the first overlap phrase pair, create the ambiguous context for the phrasal pair,
 -       that is, a StruGene value in which component 'prior' is invalid. The StruGene value is noted 'sgv';
 -   (2) find the StruGene sample or clustering mode closest to this the ambiguous context, set the resolution policy
 -       of 'sgv' as that of the StruGene sample or clustering mode;
 -   (3) if there remain overlap phrase pairs, go (1); otherwise, return phrase pairs with their resolution policies.
 - Note: Except of clustering mode, StruGene sample with context being highest similar to the ambiguous context will be selected.
 -}
ambiResolByStruGene :: SynAmbiResolMethod -> ClauTag -> [PhraCate] -> [OverPair] -> [(PhraCate, PhraCate)] -> [StruGene] -> IO [OverPair]
ambiResolByStruGene _ _ _ overPairs [] _ = return overPairs
ambiResolByStruGene resolMethod clauTag nPCs overPairs (pcp:pcps) struGenes = do
    let lop = fst pcp
    let rop = snd pcp
    let ot = getOverType nPCs lop rop                      -- Get overlapping type
    let leps = getPhraByEnd (stOfCate lop - 1) nPCs        -- Get all left-extend phrases
    let reps = getPhraByStart (enOfCate rop + 1) nPCs      -- Get all right-entend phrases
    let pri = Noth                                         -- Default value which is not used, just acted as place holder.

    let le = map ((!!0) . ctpsOfCate) leps          -- [(Category,Tag,PhraStru,Span)] of left-extended phrases
    let lo = (ctpsOfCate lop)!!0                    -- (Category,Tag,PhraStru,Span) of left-overlapping phrase
    let ro = (ctpsOfCate rop)!!0                    -- (Category,Tag,PhraStru,Span) of right-overlapping phrase
    let re = map ((!!0) . ctpsOfCate) reps          -- [(Category,Tag,PhraStru,Span)] of right-extended phrases

    let contextOfSG = (le,lo,ro,re,ot)

    if resolMethod == "StruGeneIdentity"
      then do
        confInfo <- readFile "Configuration"
        let wle = read (getConfProperty "wle" confInfo) :: Int
        let wlo = read (getConfProperty "wlo" confInfo) :: Int
        let wro = read (getConfProperty "wro" confInfo) :: Int
        let wre = read (getConfProperty "wre" confInfo) :: Int
        let wot = read (getConfProperty "wot" confInfo) :: Int
        let distWeiRatioList = [wle, wlo, wro, wre, wot]
        let contextOfSGs = map (\x -> (fst6 x, snd6 x, thd6 x, fth6 x, fif6 x)) struGenes
        let distList = map (\x -> dist4ContextOfSGByWeightSum contextOfSG x distWeiRatioList) contextOfSGs
        let minDist = minimum distList
        let idx = elemIndex minDist distList
        let idx' = case idx of
                     Just x -> x
                     Nothing -> -1                     -- Impossible position
        let pri' = sth6 (struGenes!!idx')
        let overPairs' = (lop, rop, pri'):overPairs
        ambiResolByStruGene resolMethod clauTag nPCs overPairs' pcps struGenes
      else error "ambiResolByStruGene: Syntactic ambiguity resolution was NOT done."

{- Resolve ambiguities by StruGene2 samples.
 - resolMethod: One kind of syntactic ambiguity resolution method
 - clauTag: (SentIdx, ClauIdx)
 - nPCs: The current phrase set
 - overPairs: The overlap phrase pair set
 - (pcp:pcps): The overlap phrase pair set
 - struGene2s: The list of StruGene2 values
 - Algo.:
 -   (1) get the first overlap phrase pair, create the ambiguous context for the phrasal pair,
 -       that is, a StruGene value in which component 'prior' is invalid. The StruGene value is noted 'sgv';
 -   (2) find the StruGene sample or clustering mode closest to this the ambiguous context, set the resolution policy
 -       of 'sgv' as that of the StruGene sample or clustering mode;
 -   (3) if there remain overlap phrase pairs, go (1); otherwise, return phrase pairs with their resolution policies.
 - Note: StruGene sample with context being highest similar to the ambiguous context will be selected.
 -}
ambiResolByStruGene2 :: SynAmbiResolMethod -> ClauTag -> [PhraCate] -> [OverPair] -> [(PhraCate, PhraCate)] -> [StruGene2Sample] -> IO [OverPair]
ambiResolByStruGene2 _ _ _ overPairs [] _ = return overPairs
ambiResolByStruGene2 resolMethod clauTag nPCs overPairs (pcp:pcps) struGene2s = do
    let lop = fst pcp
    let rop = snd pcp
    let ot = getOverType nPCs lop rop                      -- Get overlapping type
    let leps = getPhraByEnd (stOfCate lop - 1) nPCs        -- Get all left-extend phrases
    let reps = getPhraByStart (enOfCate rop + 1) nPCs      -- Get all right-entend phrases

    let le = map ((!!0) . ctpsOfCate) leps          -- [(Category,Tag,PhraStru,Span)] of left-extended phrases
    let lo = (ctpsOfCate lop)!!0                    -- (Category,Tag,PhraStru,Span) of left-overlapping phrase
    let ro = (ctpsOfCate rop)!!0                    -- (Category,Tag,PhraStru,Span) of right-overlapping phrase
    let re = map ((!!0) . ctpsOfCate) reps          -- [(Category,Tag,PhraStru,Span)] of right-extended phrases

    let contextOfSG = (le,lo,ro,re,ot)

    let idContextOfSGPairList = zip (map fst7 struGene2s) (map getContextFromStruGene2Sample struGene2s)  -- [(SIdx, ContextOfSG)]
    let idClauTagPriorPairList = zip (map fst7 struGene2s) (map svt7 struGene2s)          -- [(SIdx,[ClauTagPrior])]

    let idClauTagPriorHitList = filter (hasClauTagInCTPList clauTag . snd) idClauTagPriorPairList  -- [(SIdx, [ClauTagPrior])]
--  putStrLn $ "ambiResolByStruGene2: idClauTagPriorPairList: " ++ show idClauTagPriorPairList
--  putStrLn $ "ambiResolByStruGene2: idClauTagPriorPairs hitting: " ++ show clauTag ++ ": " ++ show idClauTagPriorHitList

    case resolMethod of
      "StruGeneIdentity" -> do
        confInfo <- readFile "Configuration"
        let wle = read (getConfProperty "wle" confInfo) :: Int
        let wlo = read (getConfProperty "wlo" confInfo) :: Int
        let wro = read (getConfProperty "wro" confInfo) :: Int
        let wre = read (getConfProperty "wre" confInfo) :: Int
        let wot = read (getConfProperty "wot" confInfo) :: Int

        let distWeiRatioList = [wle, wlo, wro, wre, wot]
        let distList = map (\x -> dist4ContextOfSGByWeightSum contextOfSG x distWeiRatioList) (map snd idContextOfSGPairList)

        let minDist = minimum distList
        let idx = case (elemIndex minDist distList) of
                    Just x -> x
                    Nothing -> -1                     -- Impossible position
        let idClauTagPriors = (fst7 (struGene2s!!idx), svt7 (struGene2s!!idx))              -- (SIdx, [ClauTagPrior])

        putStrLn $ "(minDist, (id, clauTagPrior)) " ++ show (minDist, idClauTagPriors)
        let idCTPOfSamplesWithDist0 = map snd $ filter (\x -> fst x < 1e-6) $ zip distList idClauTagPriorPairList  -- [(SIdx, [ClauTagPrior])]
        putStrLn $ "ambiResolByStruGene2: [(id, clauTagPrior)] of samples with ContextOfSG distance 0.0: " ++ show idCTPOfSamplesWithDist0

        let hitClauTagPriorList = filterInCTPListByClauTag clauTag (snd idClauTagPriors)    -- [ClauTagPrior]
        let prior = case hitClauTagPriorList of
                      [] -> (fromMaybePrior . priorWithHighestFreq) (snd idClauTagPriors)   -- Prior
                      _  -> snd (hitClauTagPriorList!!0)                                    -- Prior

        if hitClauTagPriorList /= []
          then putStrLn $ "ambiResolByStruGene2: Hit (id, clauTagPrior) = " ++ show (fst idClauTagPriors, hitClauTagPriorList!!0)
          else putStrLn $ "ambiResolByStruGene2: Miss"

        let overPairs' = (lop, rop, prior):overPairs
        ambiResolByStruGene2 resolMethod clauTag nPCs overPairs' pcps struGene2s

      "StruGeneEmbedded" -> do
        let context2ClauTagPriorBase = map (\x -> ((snd7 x, thd7 x, fth7 x, fif7 x, sth7 x), svt7 x)) struGene2s   -- [(ContextOfSG, [ClauTagPrior])]
        context2ClauTagPriorTuple <- findStruGeneSampleByMaxContextSim contextOfSG context2ClauTagPriorBase
                                                                                   -- (SIdx, SimDeg, Context2ClauTagPrior)
        let clauTagPriorList = (snd . thd3) context2ClauTagPriorTuple           -- [ClauTagPrior]
        let hitClauTagPriorList = filterInCTPListByClauTag clauTag clauTagPriorList        -- [ClauTagPrior]

        let prior = case hitClauTagPriorList of
                      [] -> (fromMaybePrior . priorWithHighestFreq) clauTagPriorList
                      _  -> snd (hitClauTagPriorList!!0)                        -- Prior

        let overPairs' = (lop, rop, prior):overPairs
        ambiResolByStruGene2 resolMethod clauTag nPCs overPairs' pcps struGene2s

      _ -> error "ambiResolByStruGene2: Syntactic ambiguity resolution was NOT done."

getStruGene1FromAmbiResol1 :: Int -> Int -> IO ()
getStruGene1FromAmbiResol1 startId endId = do
    if startId <= endId
     then do
       getStruGene1FromAmbiResol1' startId
       getStruGene1FromAmbiResol1 (startId + 1) endId
     else putStrLn "getStruGene1FromAmbiResol1: End."

getStruGene1FromAmbiResol1' :: Int -> IO ()
getStruGene1FromAmbiResol1' id = do
    conn <- getConn
    let sqlstat = DS.fromString $ "create table if not exists stru_gene2 (id int primary key auto_increment, origId int, leftExtend varchar(200), leftOver varchar(200), rightOver varchar(70), rightExtend varchar(200), overType tinyint(1), prior varchar(4), hitCount Int)"
--    let sqlstat = DS.fromString $ "create table if not exists stru_gene2 (id int primary key, leftExtend varchar(200), leftOver varchar(200), rightOver varchar(70), rightExtend varchar(200), overType tinyint(1), prior varchar(4), hitCount Int)"
    stmt <- prepareStmt conn sqlstat
    executeStmt conn stmt []
    let query = DS.fromString ("select id, leftPhrase, rightPhrase, context, overType, prior from ambi_resol1 where id = ? ")       -- Query is instance of IsString.
    stmt <- prepareStmt conn query
    (defs, is) <- queryStmt conn stmt [toMySQLInt32U id]
    ambiResol1List <- readStreamByInt32U3TextInt8Text [] is
    let ambiResol1 = ambiResol1List!!0
    let prior = show $ sth6 ambiResol1

    if prior /= "Noth"
      then do
        let leftPhrase = snd6 ambiResol1
        let rightPhrase = thd6 ambiResol1
        let context = fth6 ambiResol1
        let nPCs = context ++ [leftPhrase] ++ [rightPhrase]
        let leps = getPhraByEnd (stOfCate leftPhrase - 1) context        -- Get all left-extend phrases
        let reps = getPhraByStart (enOfCate rightPhrase + 1) context
        let id = fst6 ambiResol1
        let le = map ((!!0) . ctpsOfCate) leps
        let lo = (ctpsOfCate leftPhrase)!!0
        let ro = (ctpsOfCate rightPhrase)!!0
        let re = map ((!!0) . ctpsOfCate) reps
--        let ot = getOverType nPCs leftPhrase rightPhrase
        let ot = fif6 ambiResol1

        let sqlstat = DS.fromString $ "replace into stru_gene2 set origId = ?, leftExtend = ?, leftOver = ?, rightOver = ?, rightExtend = ?, overType = ?, prior = ?"
        stmt <- prepareStmt conn sqlstat
--        executeStmt conn stmt [toMySQLInt32 1, toMySQLText (nPhraSynToString le), toMySQLText (phraSynToString lo), toMySQLText (phraSynToString ro), toMySQLText (nPhraSynToString re), toMySQLInt32U ot, toMySQLText prior]
        executeStmt conn stmt [toMySQLInt32 id, toMySQLText (nPhraSynToString le), toMySQLText (phraSynToString lo), toMySQLText (phraSynToString ro), toMySQLText (nPhraSynToString re), toMySQLInt32U ot, toMySQLText prior]
        close conn                                   -- Close MySQL connection.

        else do
          putStrLn $ "The data of id = " ++ show id ++ " from ambi_resol1 table has prior = Noth, this data is not processed."
          close conn

{- According to a certain benchmark treebank, evaluate a certain experimental treebank usually obtained by machine resolving ambuiguities.
 - They are designated in file Configuration.
 - These treebanks, namely database tables,  all include attributes 'serial_num', 'tree', 'script'.
 -}
evaluateExperimentalTreebank :: IO ()
evaluateExperimentalTreebank = do
    conn <- getConn
    confInfo <- readFile "Configuration"                                        -- Read the local configuration file
    let benchmark_treebank = getConfProperty "benchmark_treebank" confInfo
    let experimental_treebank = getConfProperty "experimental_treebank" confInfo
    let startSn = getConfProperty "defaultStartIdx" confInfo
    let endSn = getConfProperty "defaultEndIdx" confInfo

    putStrLn $ " benchmark_treebank: " ++ benchmark_treebank
    putStrLn $ " experimental_treebank: " ++ experimental_treebank
    putStrLn $ " startSn = " ++ startSn ++ ", endSn = " ++ endSn

    contOrNot <- getLineUntil (" Continue or not [c/n]? (RETURN for 'n') ") ["c","n"] False
    if contOrNot == "c"
      then do
        let query = DS.fromString ("select serial_num, tree, script from " ++ experimental_treebank ++ " where serial_num >= ? and serial_num <= ?")
        stmt <- prepareStmt conn query
        (defs, is) <- queryStmt conn stmt [toMySQLInt32 (read startSn :: Int), toMySQLInt32 (read endSn :: Int)]
        experimentalTreebank <- readStreamByInt32TextText [] is
        let startSnOfET = fst3 (head experimentalTreebank)
        let endSnOfET = fst3 (last experimentalTreebank)
        putStrLn $ "  startSnOfET = " ++ show startSnOfET ++ ", endSnOfET = " ++ show endSnOfET

        let query = DS.fromString ("select serial_num, tree, script from " ++ benchmark_treebank ++ " where serial_num >= ? and serial_num <= ?")
        stmt <- prepareStmt conn query
        (defs, is) <- queryStmt conn stmt [toMySQLInt32 startSnOfET, toMySQLInt32 endSnOfET]
        benchmarkTreebank <- readStreamByInt32TextText [] is
        let startSnOfBT = fst3 (head benchmarkTreebank)
        let endSnOfBT = fst3 (last benchmarkTreebank)
        putStrLn $ "  startSnOfBT = " ++ show startSnOfBT ++ ", endSnOfBT = " ++ show endSnOfBT

        if startSnOfET == startSnOfBT && endSnOfET == endSnOfBT
          then do
            finalMachAmbiResolRes <- findMachAmbiResolRes startSnOfET endSnOfET benchmarkTreebank experimentalTreebank ((0, 0), (0, 0), (0, 0, 0, 0))
            putStrLn $ "finalMachAmbiResolRes: Benchmark (Positives, Negatives) = " ++ show (fst3 finalMachAmbiResolRes)
            putStrLn $ "                       Experimental (Positives, Negatives) = " ++ show (snd3 finalMachAmbiResolRes)
            putStrLn $ "                       (TP, TN, Semantic TP, Completed Num. of Multi-words clauses) = " ++ show (thd3 finalMachAmbiResolRes)

            let precision = fromIntegral ((fst4 . thd3) finalMachAmbiResolRes) / fromIntegral ((fst . snd3) finalMachAmbiResolRes)
            putStrLn $ "Precision = " ++ (printf "%.4f" (precision :: Double))            -- Precision = TP / (TP + FP)

            let recall = fromIntegral ((fst4 . thd3) finalMachAmbiResolRes) / fromIntegral ((fst . fst3) finalMachAmbiResolRes)
            putStrLn $ "Recall = " ++ (printf "%.4f" (recall :: Double))                  -- Recall = TP / (TP + FN)

            let f1Score = 2.0 * (precision * recall) / (precision + recall)
            putStrLn $ "F1Score = " ++ (printf "%.4f" (f1Score :: Double))                -- F1Score
          else putStrLn "Benchmark and experimental treebanks have different serial_num ranges."
      else putStrLn "Operation was canceled."

-- According to a benchmark treebank, there are statistical numbers of a certain machine-building treebank.
type NumOfManPositPhrase = Int         -- Num. of phrases in a manually built tree.
type NumOfManNegPhrase = Int           -- Num. of phrases in a manually built banned phrases set, namely banPCSets.
type NumOfMachPositPhrase = Int        -- Num. of phrases in a machine-building tree, namely TP + FP
type NumOfMachNegPhrase = Int          -- Num. of phrases in a machine-building banned phrases set, namely banPCSets
type NumOfCommonPositPhrase = Int      -- True Positive, namely TP
type NumOfCommonNegPhrase = Int        -- True Negative, namely TN. Actually, it is pseudo TN because it is difficult to get ALL negative instances.
type NumOfCommonPhraseOnlySeman = Int  -- TP of attribute Seman
type NumOfCommonResClause = Int        -- Num. of experimental clauses same with benchmark clauses
type MachAmbiResolRes = ((NumOfManPositPhrase, NumOfManNegPhrase)
                       , (NumOfMachPositPhrase, NumOfMachNegPhrase)
                       , (NumOfCommonPositPhrase, NumOfCommonNegPhrase, NumOfCommonPhraseOnlySeman, NumOfCommonResClause)
                        )

{- Calculate precisions, recall rates, and F1 scores of machine-building parsing trees.
 - benchmarkTreebank: Benchmark treebank
 - experimentalTreebank: Experimental treebank in which trees were obtained by machine resolving ambuiguities.
 - origMachAmbiResolRes: Existing result of evaluating experimentalTreebank against benchmarkTreebank.
 -}
findMachAmbiResolRes :: SentIdx -> SentIdx -> [(SentIdx, String, String)] -> [(SentIdx, String, String)] -> MachAmbiResolRes -> IO (MachAmbiResolRes)
findMachAmbiResolRes startSn endSn benchmarkTreebank experimentalTreebank origMachAmbiResolRes = do
    let treeOfStartSn = stringToList $ snd3 . head  $ filter (\x -> fst3 x == startSn) benchmarkTreebank        -- [ClauseStr]
    let treeOfStartSn' = stringToList $ snd3 . head  $ filter (\x -> fst3 x == startSn) experimentalTreebank
    let scriptOfStartSn = stringToList $ thd3 . head  $ filter (\x -> fst3 x == startSn) benchmarkTreebank      -- [ScriptStr]
    let scriptOfStartSn' = stringToList $ thd3 . head  $ filter (\x -> fst3 x == startSn) experimentalTreebank

    putStrLn $ "Sentence No.:" ++ show startSn
    currentMachAmbiResolRes <- findMachAmbiResolResOfASent 1 treeOfStartSn treeOfStartSn' scriptOfStartSn scriptOfStartSn' origMachAmbiResolRes
    let startSn' = startSn + 1
    if startSn' <= endSn
      then findMachAmbiResolRes startSn' endSn benchmarkTreebank experimentalTreebank currentMachAmbiResolRes
      else return currentMachAmbiResolRes

{- Evaluate ONE sentence's experimental parsing against benchmark parsing.
 - clauseNo: Clausal index number, which begins from 1.
 - (s:cs): List of clausal strings of benchmark parsing.
 - (s':cs'): List of clausal strings of experimental parsing.
 - script: String of script of benchmark parsing.
 - script': String of script of experimental parsing.
 - origMachAmbiResolRes: Existing result of evaluating ambiResolTreebank against accurateTreebank.
 -}
findMachAmbiResolResOfASent :: Int -> [String] -> [String] -> [String] -> [String] -> MachAmbiResolRes -> IO (MachAmbiResolRes)
findMachAmbiResolResOfASent clauseNo (s:cs) (s':cs') script script' origMachAmbiResolRes = do
    let index = clauseNo - 1                               -- List index
    let pcs = getClauPhraCate s                            -- [PhraCate] of benchmark clause
    let pcs' = getClauPhraCate s'                          -- [PhraCate] of experimental clause
    let phraseOfSpan0 = getPhraBySpan 0 pcs                -- Words of benchmark clause
    let phraseOfSpan0' = getPhraBySpan 0 pcs'              -- Words of experimental clause
    let phraseWithoutSpan0 = [x| x <- pcs, notElem x phraseOfSpan0]             -- [PhraCate] of benchmark phrases with at least two words
    let phraseWithoutSpan0' = [x| x <- pcs', notElem x phraseOfSpan0']          -- [PhraCate] of experimental phrases with at least two words
    let npcWithoutAct = nPhraCateWithoutAct phraseWithoutSpan0 []               -- Eliminating attribute 'Act'
    let npcWithoutAct' = nPhraCateWithoutAct phraseWithoutSpan0' []             -- Eliminating attribute 'Act'
    let npcOnlySeman = nseOfCate phraseWithoutSpan0 []                          -- [Seman] of generated phrases in benchmark tree
    let npcOnlySeman' = nseOfCate phraseWithoutSpan0' []                        -- [Seman] of generated phrases in experimental tree
    let commonPositPhrase = [x| x <- phraseWithoutSpan0', elem (phraCateWithoutAct x) npcWithoutAct]   -- True Positive (TP)
    let banPCs = getClauBanPCs (script!!index)                                  -- [PhraCate] dropped from benchmark parsing tree
    let banPCs' = getClauBanPCs (script'!!index)                                -- [PhraCate] droppped from experimental parsing tree
    let banPCsWithoutAct = nPhraCateWithoutAct banPCs []                        -- Eliminating attribute 'Act'
    let banPCsWithoutAct' = nPhraCateWithoutAct banPCs' []                      -- Eliminating attribute 'Act'
    let commonNegPhrase = [x| x <- banPCs', elem (phraCateWithoutAct x) banPCsWithoutAct]    -- PSEUDO True Negative (TN)
    let commonPhraseOnlySeman = [x| x <- npcOnlySeman, elem x npcOnlySeman']    -- TP in experimental [Seman] against benchmark [Seman]
    let phraseWithLongestSp = last pcs                                          -- Suppose phrases be sorted by spans from 0 to longest
    let commonResClause = case elem phraseWithLongestSp phraseWithoutSpan0' of  -- Adds 1 if parsing was completed, here one-word clauses were NOT taken into account。
                               True -> 1                                        -- For one-word clauses,  phraseWithoutSpan0' = [], elem returns False.
                               False -> 0

    let numOfClausePhrase = length phraseWithoutSpan0                           -- Num. of generated phrases in benchmark tree
    let numOfBanPCs = length banPCs                                             -- Num. of dropped phrases from benchmark tree
    let numOfClausePhrase' = length phraseWithoutSpan0'                         -- Num. of generated phrases in experimental tree
    let numOfBanPCs' = length banPCs'                                           -- Num. of dropped phrases from experimental tree

    let newNumOfPhrase = (fst . fst3) origMachAmbiResolRes + numOfClausePhrase       -- Accumulate num. of benchmark phrases
    let newNumOfBanPCs = (snd . fst3) origMachAmbiResolRes + numOfBanPCs             -- Accumulate num. of benchmark-banned phrases
    let newNumOfPhrase' = (fst . snd3) origMachAmbiResolRes + numOfClausePhrase'     -- Accumulate num. of experimental phrases
    let newNumOfBanPCs' = (snd . snd3) origMachAmbiResolRes + numOfBanPCs'           -- Accumulate num. of experiment-banned phrases
    let newNumOfCommonPositPhrase = (fst4 . thd3) origMachAmbiResolRes + length commonPositPhrase    -- Accumulate num. of TP phrases
    let newNumOfCommonNegPhrase = (snd4 . thd3) origMachAmbiResolRes + length commonNegPhrase        -- Accumulate num. of pseudo TN phrases
    let newNumOfCommonPhraseOnlySeman = (thd4 . thd3) origMachAmbiResolRes + length commonPhraseOnlySeman   -- Accumulate num. of Seman TP
    let newNumOfCommonResClause = (fth4 . thd3) origMachAmbiResolRes + commonResClause               -- Accumulate num. of accuracy clause

    let clauseResMark = ((numOfClausePhrase, numOfBanPCs), (numOfClausePhrase', numOfBanPCs'), (length commonPositPhrase, length commonNegPhrase, length commonPhraseOnlySeman,commonResClause))
    let newAmbiResolResMark = ((newNumOfPhrase, newNumOfBanPCs), (newNumOfPhrase', newNumOfBanPCs'), (newNumOfCommonPositPhrase, newNumOfCommonNegPhrase, newNumOfCommonPhraseOnlySeman,newNumOfCommonResClause))
{-
    let spls = divPhraCateBySpan pcs         -- Span lines
    let spls' = divPhraCateBySpan pcs'       -- Span lines
    putStrLn $ "Clause No.: " ++ show clauseNo
    putStrLn $ "Benchmark tree:"
    showTreeStru spls spls
    putStrLn $ "banPCs = "
    showNPhraCateLn banPCs
    putStrLn $ "Experimental tree:"
    showTreeStru spls' spls'
    putStrLn $ "banPCs' = "
    showNPhraCateLn banPCs'
    putStrLn $ "commonPhraseOnlySeman=" ++ show commonPhraseOnlySeman

    putStr $ "phraseWithLongestSp = "
    showPhraCate phraseWithLongestSp
    putStrLn ""

    putStrLn $ "The ambiguity-resolution-successful phrases without span = 0 are: "
    putStr "True Positive instances: "
    showNPhraCateLn commonPositPhrase
    putStr "True Negative instances: "
    showNPhraCateLn commonNegPhrase

    putStrLn $ "clauseResMark = " ++ show clauseResMark
    putStrLn $ "ambiResolResMark = " ++ show newAmbiResolResMark
    putStrLn ""
 -}
    if cs /= [] && cs' /= []                                                    -- Lengths of cs and cs' should be equal.
      then findMachAmbiResolResOfASent (clauseNo + 1) cs cs' script script' newAmbiResolResMark
      else return newAmbiResolResMark

{- Do a trip of transition, insert or update related ambiguity resolution samples in Table <syntax_ambig_resol_model>, and return the category-converted
 - rules used in this trip, the resultant phrases, and the banned phrases.
 - If transitive parsing is to terminated, namely selecting 'e' at inquiring rule switches, returnes ([],[],[]) as the terminating flag.
 -}
doTransWithManualResol :: ClauTag -> [Rule] -> [PhraCate] -> [BanPCs] -> [OverPair] -> IO ([Rule], [PhraCate], [BanPCs], [OverPair])
doTransWithManualResol clauTag onOff nPCs banPCSets prevOverPairs = do
    putStr "Rule switches: "
    showOnOff onOff
    ruleSwitchOk <- getLineUntil "Are rule switches ok? [y/n/e]: ('y' or RETURN for yes, 'n' for no, and 'e' for exit) " ["y","n","e"] True
    if ruleSwitchOk == "n"                          -- Press key 'n'
      then do
        putStrLn "Enable or disable rules among"
        putStrLn "  S/s, P/s, O/s, A/s, Hn/s, N/s,"
        putStrLn "  S/v, O/v, A/v, Hn/v, D/v, Cn/v, Cv/v, N/v, P/vt, OE/vt, Vt/vi, A/vd,"
        putStrLn "  S/a, P/a, V/a, O/a, D/a, Da/a, Cn/a, Cv/a, Ca/a, Hn/a, N/a,"
        putStrLn "  P/n, V/n, A/n, Cn/n, Cv/n, D/n, Da/n, ADJ/n, S/nd, O/nd, Hn/nd,"
        putStrLn "  S/d, O/d, A/d, Hn/d, Cv/d, N/d, ADJ/d, Da/d, Ds/d, Dx/d, Doe/d,"
        putStrLn "  D/p,"
        putStrLn "  O/oe, Hn/oe, N/oe,"
        putStrLn "  N/pe,"
        putStrLn "  A/q,"
        putStrLn "  Jf/c, Jb/c"
        putStrLn "  and U3d/u3,"
        putStr "  for instance, +O/s, -A/v: (RETURN for skip) "
        ruleSwitchStr <- getLine                              -- Get new onOff from input, such as "+O/s,-A/v"
        let rws = splitAtDeliThrowSpace ',' ruleSwitchStr     -- ["+O/s","-A/v"]
        if [] == [x| x <- rws, notElem (head x) ['+','-'] || notElem (tail x) [
          "S/s", "P/s", "O/s", "A/s", "Hn/s", "N/s",
          "S/v", "O/v", "A/v", "Hn/v", "D/v", "Cn/v", "Cv/v", "N/v", "P/vt", "OE/vt", "Vt/vi", "A/vd",
          "S/a", "O/a", "Hn/a", "N/a", "P/a", "V/a", "D/a", "Da/a", "Cv/a", "Cn/a", "Ca/a",
          "P/n", "V/n", "A/n", "Cn/n", "Cv/n", "D/n", "Da/n", "ADJ/n", "S/nd", "O/nd", "Hn/nd",
          "S/d", "O/d", "A/d", "Hn/d", "Cv/d", "N/d", "ADJ/d", "Da/d", "Ds/d", "Dx/d", "Doe/d",
          "D/p",
          "O/oe", "Hn/oe", "N/oe",
          "N/pe",
          "A/q",
          "Jf/c", "Jb/c",
          "U3d/u3"]]
           then do
             let newOnOff = updateOnOff onOff rws
             doTransWithManualResol clauTag newOnOff nPCs banPCSets prevOverPairs          -- Redo this trip of transition by modifying rule switches.
           else do
             putStrLn "Rule switch expression error. Consider again!"
             doTransWithManualResol clauTag onOff nPCs banPCSets prevOverPairs
      else if ruleSwitchOk == "y" || ruleSwitchOk == ""         -- Press key 'y' or directly press RETURN
             then do
               let nPCs2 = trans onOff nPCs banPCSets          -- Without pruning, get transitive result.
--               putStr "Transitive result before pruning: "
--               showNPhraCateLn (sortPhraCateBySpan nPCs2)
               putStr "New phrases before pruning: "
               showNPhraCateLn [pc | pc <- nPCs2, notElem4Phrase pc nPCs]
--               putStr "Banned phrases: "
--               showNPhraCateListLn banPCSets                  -- Can't use <sortPhraCateBySpan> on <banPCs>.

               let pcps = getOverlap nPCs2                  -- [(PhraCate, PhraCate)]
               let overPairsByPrev = ambiResolByPrevOverPair [] pcps prevOverPairs            -- [OverPair], phrasal pairs appearing in the previous rounds of transitions.
               putStr "doTransWithManualResol: overPairsByPrev: "
               showNOverPairLn overPairsByPrev

               let pcpsWithPrior = map (\x->(fst3 x, snd3 x)) overPairsByPrev
               let pcps' = [pcp | pcp <- pcps, notElem pcp pcpsWithPrior]

               overPairsByManual <- syntaxAmbiResolByManualResol nPCs2 [] pcps'               -- [OverPair], record overlapping pairs for pruning.
               let overPairs = overPairsByPrev ++ overPairsByManual

               nbPCs <- transWithPruning onOff nPCs banPCSets overPairs                      -- Get transitive result with pruning.
--               putStr "Transitive result after pruning: "
--               showNPhraCateLn (sortPhraCateBySpan (fst nbPCs))
               putStr "New phrases after pruning: "
               showNPhraCateLn [pc | pc <- fst nbPCs, notElem4Phrase pc nPCs]
               putStr "Banned phrases: "
               showNPhraCateListLn (snd nbPCs)                                  -- The banned phrases after updated.

               transOk <- getLineUntil "This trip of transition is ok? [y/n/e]: (RETURN for 'y') " ["y","n","e"] True  -- Get user decision of whether to do next transition
               if transOk == "y" || transOk == ""           -- Press key 'y' or directly press RETURN
                 then do
                     updateSyntaxAmbiResolSample clauTag (fst nbPCs) overPairsByManual                   -- Record new ambiguity resolution fragments.
                     return (onOff, (fst nbPCs), (snd nbPCs), removeDup4OverPair (prevOverPairs ++ overPairsByManual))
                 else if transOk == "n"
                        then doTransWithManualResol clauTag onOff nPCs banPCSets prevOverPairs          -- Redo this trip of transition.
                        else if transOk == "e"
                               then return ([],[],[],[])                        -- Return from doTrans, and indicate this is terminating exit.
                               else error "doTransWithManualResol: Impossible input error!"
             else if ruleSwitchOk == "e"
                    then return ([],[],[],[])                  -- Return from doTrans, and indicate this is terminating exit.
                    else error "doTransWithManualResol: Impossible input error!"

{- Mannually resolve a set of overlapping phrases, and return the list of OverPair(s).
 - 'nPCs': Phrase set.
 - 'overPairs': [(PhraCate, PhraCate, Prior)], phrasal pairs whose ambiguity resolution has been done to now.
 - 'pcps': Phrasal pairs waiting to obtain ambiguity resolution policy.
  -}
syntaxAmbiResolByManualResol :: [PhraCate] -> [OverPair] -> [(PhraCate, PhraCate)] -> IO [OverPair]
syntaxAmbiResolByManualResol _ overPairs [] = return overPairs
syntaxAmbiResolByManualResol nPCs overPairs (pcp:pcps) = do
    op <- syntaxAmbiResolByManualResol' nPCs pcp
    syntaxAmbiResolByManualResol nPCs (op:overPairs) pcps

{- Mannually resolve a pair of overlapping phrases, and return a OverPair.
 -}
syntaxAmbiResolByManualResol' :: [PhraCate] -> (PhraCate, PhraCate) -> IO OverPair
syntaxAmbiResolByManualResol' nPCs (lp, rp) = do
    confInfo <- readFile "Configuration"                                        -- Read the local configuration file
    let syntax_ambig_resol_model = getConfProperty "syntax_ambig_resol_model" confInfo
    let ot = getOverType nPCs lp rp                        -- Get overlapping type
    case syntax_ambig_resol_model of
      "ambi_resol1" -> do
        let context = [x | x <- nPCs, x /= lp, x /= rp]        -- Get context of the pair of overlapping phrases
        putStr "Find a fragment of No.1 ambiguity model: "
        showAmbiModel1Frag lp rp context ot
      "stru_gene" -> do
        let leps = getPhraByEnd (stOfCate lp - 1) nPCs        -- Get all left-extend phrases
        let reps = getPhraByStart (enOfCate rp + 1) nPCs      -- Get all right-entend phrases
        putStr "Find structural fragment: "
        showStruFrag leps lp rp reps ot
      x | elem x ["stru_gene_202408", "stru_gene_202412", "stru_gene_202501"] -> do        -- Multimodel
        let leps = getPhraByEnd (stOfCate lp - 1) nPCs        -- Get all left-extend phrases
        let reps = getPhraByStart (enOfCate rp + 1) nPCs      -- Get all right-entend phrases
        putStr "Find structural fragment: "
        showStruFrag leps lp rp reps ot
      _ -> error "syntaxAmbiResolByManualResol': syntax_ambig_resol_model is set wrongly."

    priorFlag <- getLineUntil "please input new priority [Lp/Rp/Noth]: ('1' or RETURN for 'Lp', '2' for 'Rp', '3' for 'Noth') " ["1", "2", "3"] True
    case priorFlag of
      "1" -> return (lp, rp, (read "Lp"::Prior))
      "2" -> return (lp, rp, (read "Rp"::Prior))
      "3" -> return (lp, rp, (read "Noth"::Prior))

{- Insert new or update old syntax ambiguity resolution samples in database.
 - The input phrase set <nPCs> is used to create ambiguity resolution context for every overlapping phrases.
 -}
updateSyntaxAmbiResolSample :: ClauTag -> [PhraCate] -> [OverPair] -> IO ()
updateSyntaxAmbiResolSample _ _ [] = do
    putStrLn "updateSyntaxAmbiResolSample: Update finished."              -- To make output easy to read.
updateSyntaxAmbiResolSample clauTag nPCs (op:ops) = do
    updateSyntaxAmbiResolSample' clauTag nPCs op
    updateSyntaxAmbiResolSample clauTag nPCs ops

{- Insert or update one ambiguity resolution fragment in MySQL tables storing ambiguity resolution samples.
 - Ambiguity resolution model also is the name of MySQL table storing the samples of that model.
 - To now, models include 'stru_gene', 'stru_gene2', 'stru_gene3', 'stru_gene3a' and 'ambi_resol1'.
 -}
updateSyntaxAmbiResolSample' :: ClauTag -> [PhraCate] -> OverPair -> IO ()
updateSyntaxAmbiResolSample' clauTag nPCs overPair = do
    confInfo <- readFile "Configuration"                                        -- Read the local configuration file
    let syntax_ambig_resol_model = getConfProperty "syntax_ambig_resol_model" confInfo

    let leftOver = fst3 overPair                                    -- Get left overlapping phrase.
    let rightOver = snd3 overPair                                   -- Get right overlapping phrase.
    let overType = getOverType nPCs leftOver rightOver              -- Get overlapping type
    let prior = thd3 overPair                                       -- Get prior selection of the two overlapping phrases.
    let leftOverTree = findATree leftOver nPCs                      -- BiTree PhraCate
    let rightOverTree = findATree rightOver nPCs                    -- BiTree PhraCate

    case syntax_ambig_resol_model of
      x | elem x ["stru_gene3a_phrasyn0_202509"] -> do                -- Multimodel

        let lot = phraCateTree2PhraSyn0Tree leftOverTree      -- BiTree PhraSyn0
        let rot = phraCateTree2PhraSyn0Tree rightOverTree     -- BiTree PhraSyn0
        let lotv = doubleBackSlash (show lot)                 -- Convert BiTree Syntax0 into String, and double backward slashes.
        let rotv = doubleBackSlash (show rot)                 -- Convert BiTree Syntax0 into String, and double backward slashes.

        conn <- getConn
        let sqlstat = read (show ("select id, clauTagPrior, lpHitCount, rpHitCount, nothHitCount from "
                                   ++ syntax_ambig_resol_model
                                   ++ " where leftOverTree = '" ++ lotv ++ "' && "
                                   ++ "rightOverTree = '" ++ rotv ++ "'")) :: Query
        stmt <- prepareStmt conn sqlstat
        (defs, is) <- queryStmt conn stmt []

        rows <- S.toList is
        if rows /= []
          then
            if length rows > 1
              then do
                closeStmt conn stmt
                close conn                           -- Close MySQL connection.
                error "updateSyntaxAmbiResolSample': Find duplicate structural genes."
              else do
                let id = fromMySQLInt32U ((rows!!0)!!0)
                let clauTagPriorListStr = fromMySQLText ((rows!!0)!!1)
                let lpHitCount = fromMySQLInt16U ((rows!!0)!!2)
                let rpHitCount = fromMySQLInt16U ((rows!!0)!!3)
                let nothHitCount = fromMySQLInt16U ((rows!!0)!!4)

                putStrLn $ "updateSyntaxAmbiResolSample': (" ++ show id ++ ") clauTagPrior: " ++ clauTagPriorListStr ++ ", lpHitCount: " ++ show lpHitCount ++ ", rpHitCount: " ++ show rpHitCount ++ ", nothHitCount: " ++ show nothHitCount
                let clauTagPriorList = stringToCTPList clauTagPriorListStr
                let priorList = map snd $ filter ((== clauTag) . fst) clauTagPriorList          -- The selected priorities

                if elem prior priorList
                  then putStrLn $ "updateSyntaxAmbiResolSample': " ++ show (clauTag, prior) ++ " was hit in row " ++ show id
                  else do
                    resetStmt conn stmt
                    let sqlstat = case prior of
                                    Lp -> read (show ("update " ++ syntax_ambig_resol_model ++ " set clauTagPrior = ?, lpHitCount = ? where id = '" ++ show id ++ "'")) :: Query
                                    Rp -> read (show ("update " ++ syntax_ambig_resol_model ++ " set clauTagPrior = ?, rpHitCount = ? where id = '" ++ show id ++ "'")) :: Query
                                    Noth -> read (show ("update " ++ syntax_ambig_resol_model ++ " set clauTagPrior = ?, nothHitCount = ? where id = '" ++ show id ++ "'")) :: Query
                    stmt <- prepareStmt conn sqlstat
                    let newClauTagPriorList = (clauTag, prior):clauTagPriorList     -- Add with one ClauTagPrior value.
                    case prior of
                      Lp -> executeStmt conn stmt [toMySQLText (show newClauTagPriorList), toMySQLInt16U (lpHitCount + 1)]    -- Add 'lpHitCount' by 1.
                      Rp -> executeStmt conn stmt [toMySQLText (show newClauTagPriorList), toMySQLInt16U (rpHitCount + 1)]    -- Add 'rpHitCount' by 1.
                      Noth -> executeStmt conn stmt [toMySQLText (show newClauTagPriorList), toMySQLInt16U (nothHitCount + 1)]   -- Add 'nothHitCount' by 1.
                    putStrLn $ "updateSyntaxAmbiResolSample': " ++ show (clauTag, prior) ++ " was inserted into row " ++ show id
                closeStmt conn stmt
                close conn                       -- Close MySQL connection.

          else do
            putStrLn "Inquire failed."
            let clauTagPrior = (clauTag, prior)
            let sqlstat = case prior of
                            Lp -> read (show ("insert " ++ syntax_ambig_resol_model ++ " (leftOverTree,rightOverTree,clauTagPrior,lpHitCount) values ('" ++ lotv ++ "','" ++ rotv ++ "','[" ++ show clauTagPrior ++ "]',1)")) :: Query
                            Rp -> read (show ("insert " ++ syntax_ambig_resol_model ++ " (leftOverTree,rightOverTree,clauTagPrior,rpHitCount) values ('" ++ lotv ++ "','" ++ rotv ++ "','[" ++ show clauTagPrior ++ "]',1)")) :: Query
                            Noth -> read (show ("insert " ++ syntax_ambig_resol_model ++ " (leftOverTree,rightOverTree,clauTagPrior,nothHitCount) values ('" ++ lotv ++ "','" ++ rotv ++ "','[" ++ show clauTagPrior ++ "]',1)")) :: Query
            stmt1 <- prepareStmt conn sqlstat
            oks <- executeStmt conn stmt1 []             -- Insert the described structural gene.
            putStrLn $ "updateSyntaxAmbiResolSample': Last inserted row with ID " ++ show (getOkLastInsertID oks)
            closeStmt conn stmt1
            close conn

      x | elem x ["stru_gene3a_202508"] -> do                 -- Multimodel

        let lot = phraCateTree2PhraSynTree leftOverTree      -- BiTree PhraSyn
        let rot = phraCateTree2PhraSynTree rightOverTree     -- BiTree PhraSyn
        let lotv = doubleBackSlash (show lot)                -- Convert BiTree Syntax into String, and double backward slashes.
        let rotv = doubleBackSlash (show rot)                -- Convert BiTree Syntax into String, and double backward slashes.

        conn <- getConn
        let sqlstat = read (show ("select id, clauTagPrior, lpHitCount, rpHitCount, nothHitCount from "
                                   ++ syntax_ambig_resol_model
                                   ++ " where leftOverTree = '" ++ lotv ++ "' && "
                                   ++ "rightOverTree = '" ++ rotv ++ "'")) :: Query
        stmt <- prepareStmt conn sqlstat
        (defs, is) <- queryStmt conn stmt []

        rows <- S.toList is
        if rows /= []
          then
            if length rows > 1
              then do
                closeStmt conn stmt
                close conn                           -- Close MySQL connection.
                error "updateSyntaxAmbiResolSample': Find duplicate structural genes."
              else do
                let id = fromMySQLInt32U ((rows!!0)!!0)
                let clauTagPriorListStr = fromMySQLText ((rows!!0)!!1)
                let lpHitCount = fromMySQLInt16U ((rows!!0)!!2)
                let rpHitCount = fromMySQLInt16U ((rows!!0)!!3)
                let nothHitCount = fromMySQLInt16U ((rows!!0)!!4)

                putStrLn $ "updateSyntaxAmbiResolSample': (" ++ show id ++ ") clauTagPrior: " ++ clauTagPriorListStr ++ ", lpHitCount: " ++ show lpHitCount ++ ", rpHitCount: " ++ show rpHitCount ++ ", nothHitCount: " ++ show nothHitCount
                let clauTagPriorList = stringToCTPList clauTagPriorListStr
                let priorList = map snd $ filter ((== clauTag) . fst) clauTagPriorList          -- The selected priorities

                if elem prior priorList
                  then putStrLn $ "updateSyntaxAmbiResolSample': " ++ show (clauTag, prior) ++ " was hit in row " ++ show id
                  else do
                    resetStmt conn stmt
                    let sqlstat = case prior of
                                    Lp -> read (show ("update " ++ syntax_ambig_resol_model ++ " set clauTagPrior = ?, lpHitCount = ? where id = '" ++ show id ++ "'")) :: Query
                                    Rp -> read (show ("update " ++ syntax_ambig_resol_model ++ " set clauTagPrior = ?, rpHitCount = ? where id = '" ++ show id ++ "'")) :: Query
                                    Noth -> read (show ("update " ++ syntax_ambig_resol_model ++ " set clauTagPrior = ?, nothHitCount = ? where id = '" ++ show id ++ "'")) :: Query
                    stmt <- prepareStmt conn sqlstat
                    let newClauTagPriorList = (clauTag, prior):clauTagPriorList     -- Add with one ClauTagPrior value.
                    case prior of
                      Lp -> executeStmt conn stmt [toMySQLText (show newClauTagPriorList), toMySQLInt16U (lpHitCount + 1)]    -- Add 'lpHitCount' by 1.
                      Rp -> executeStmt conn stmt [toMySQLText (show newClauTagPriorList), toMySQLInt16U (rpHitCount + 1)]    -- Add 'rpHitCount' by 1.
                      Noth -> executeStmt conn stmt [toMySQLText (show newClauTagPriorList), toMySQLInt16U (nothHitCount + 1)]   -- Add 'nothHitCount' by 1.
                    putStrLn $ "updateSyntaxAmbiResolSample': " ++ show (clauTag, prior) ++ " was inserted into row " ++ show id
                closeStmt conn stmt
                close conn                       -- Close MySQL connection.

          else do
            putStrLn "Inquire failed."
            let clauTagPrior = (clauTag, prior)
            let sqlstat = case prior of
                            Lp -> read (show ("insert " ++ syntax_ambig_resol_model ++ " (leftOverTree,rightOverTree,clauTagPrior,lpHitCount) values ('" ++ lotv ++ "','" ++ rotv ++ "','[" ++ show clauTagPrior ++ "]',1)")) :: Query
                            Rp -> read (show ("insert " ++ syntax_ambig_resol_model ++ " (leftOverTree,rightOverTree,clauTagPrior,rpHitCount) values ('" ++ lotv ++ "','" ++ rotv ++ "','[" ++ show clauTagPrior ++ "]',1)")) :: Query
                            Noth -> read (show ("insert " ++ syntax_ambig_resol_model ++ " (leftOverTree,rightOverTree,clauTagPrior,nothHitCount) values ('" ++ lotv ++ "','" ++ rotv ++ "','[" ++ show clauTagPrior ++ "]',1)")) :: Query
            stmt1 <- prepareStmt conn sqlstat
            oks <- executeStmt conn stmt1 []             -- Insert the described structural gene.
            putStrLn $ "updateSyntaxAmbiResolSample': Last inserted row with ID " ++ show (getOkLastInsertID oks)
            closeStmt conn stmt1
            close conn

      x | elem x ["stru_gene3_202508"] -> do                 -- Multimodel
        let leftExtend = getPhraByEnd (stOfCate leftOver - 1) nPCs          -- Get all left-extend phrases
        let rightExtend = getPhraByStart (enOfCate rightOver + 1) nPCs      -- Get all right-entend phrases

        let le = map ((!!0) . ctpsOfCate) leftExtend         -- [(Category,Tag,PhraStru,Span)] of left-extended phrases
        let lo = (ctpsOfCate leftOver)!!0                    -- (Category,Tag,PhraStru,Span) of left-overlapping phrase
        let ro = (ctpsOfCate rightOver)!!0                   -- (Category,Tag,PhraStru,Span) of right-overlapping phrase
        let re = map ((!!0) . ctpsOfCate) rightExtend        -- [(Category,Tag,PhraStru,Span)] of right-extended phrases
        let ot = overType                                    -- Overlap type
        let lot = phraCateTree2PhraSynTree leftOverTree      -- BiTree PhraSyn
        let rot = phraCateTree2PhraSynTree rightOverTree     -- BiTree PhraSyn

        let lev = doubleBackSlash (show le)                 -- Get values to insert them into MySql Table
        let lov = doubleBackSlash (show lo)
        let rov = doubleBackSlash (show ro)
        let rev = doubleBackSlash (show re)
        let otv = show overType
        let lotv = doubleBackSlash (show lot)
        let rotv = doubleBackSlash (show rot)

        conn <- getConn
        let sqlstat = read (show ("select id, clauTagPrior, lpHitCount, rpHitCount, nothHitCount from "
                                   ++ syntax_ambig_resol_model
                                   ++ " where leftExtend = '" ++ lev ++ "' && "
                                   ++ "leftOverTree = '" ++ lotv ++ "' && "
                                   ++ "rightOverTree = '" ++ rotv ++ "' && "
                                   ++ "rightExtend = '" ++ rev ++ "' && "
                                   ++ "overType = " ++ otv)) :: Query
        stmt <- prepareStmt conn sqlstat
        (defs, is) <- queryStmt conn stmt []

        rows <- S.toList is
        if rows /= []
          then
            if length rows > 1
              then do
                closeStmt conn stmt
                close conn                           -- Close MySQL connection.
                error "updateSyntaxAmbiResolSample': Find duplicate structural genes."
              else do
                let id = fromMySQLInt32U ((rows!!0)!!0)
                let clauTagPriorListStr = fromMySQLText ((rows!!0)!!1)
                let lpHitCount = fromMySQLInt16U ((rows!!0)!!2)
                let rpHitCount = fromMySQLInt16U ((rows!!0)!!3)
                let nothHitCount = fromMySQLInt16U ((rows!!0)!!4)

                putStrLn $ "updateSyntaxAmbiResolSample': (" ++ show id ++ ") clauTagPrior: " ++ clauTagPriorListStr ++ ", lpHitCount: " ++ show lpHitCount ++ ", rpHitCount: " ++ show rpHitCount ++ ", nothHitCount: " ++ show nothHitCount
                let clauTagPriorList = stringToCTPList clauTagPriorListStr
                let priorList = map snd $ filter ((== clauTag) . fst) clauTagPriorList          -- The selected priorities

                if elem prior priorList
                  then putStrLn $ "updateSyntaxAmbiResolSample': " ++ show (clauTag, prior) ++ " was hit in row " ++ show id
                  else do
                    resetStmt conn stmt
                    let sqlstat = case prior of
                                    Lp -> read (show ("update " ++ syntax_ambig_resol_model ++ " set clauTagPrior = ?, lpHitCount = ? where id = '" ++ show id ++ "'")) :: Query
                                    Rp -> read (show ("update " ++ syntax_ambig_resol_model ++ " set clauTagPrior = ?, rpHitCount = ? where id = '" ++ show id ++ "'")) :: Query
                                    Noth -> read (show ("update " ++ syntax_ambig_resol_model ++ " set clauTagPrior = ?, nothHitCount = ? where id = '" ++ show id ++ "'")) :: Query
                    stmt <- prepareStmt conn sqlstat
                    let newClauTagPriorList = (clauTag, prior):clauTagPriorList     -- Add with one ClauTagPrior value.
                    case prior of
                      Lp -> executeStmt conn stmt [toMySQLText (show newClauTagPriorList), toMySQLInt16U (lpHitCount + 1)]    -- Add 'lpHitCount' by 1.
                      Rp -> executeStmt conn stmt [toMySQLText (show newClauTagPriorList), toMySQLInt16U (rpHitCount + 1)]    -- Add 'rpHitCount' by 1.
                      Noth -> executeStmt conn stmt [toMySQLText (show newClauTagPriorList), toMySQLInt16U (nothHitCount + 1)]   -- Add 'nothHitCount' by 1.
                    putStrLn $ "updateSyntaxAmbiResolSample': " ++ show (clauTag, prior) ++ " was inserted into row " ++ show id
                closeStmt conn stmt
                close conn                       -- Close MySQL connection.

          else do
            putStrLn "Inquire failed."
            let clauTagPrior = (clauTag, prior)
            let sqlstat = case prior of
                            Lp -> read (show ("insert " ++ syntax_ambig_resol_model ++ " (leftExtend,leftOverTree,rightOverTree,rightExtend,overType,clauTagPrior,lpHitCount) values ('" ++ lev ++ "','" ++ lotv ++ "','" ++ rotv ++ "','" ++ rev ++ "'," ++ otv ++ ",'[" ++ show clauTagPrior ++ "]',1)")) :: Query
                            Rp -> read (show ("insert " ++ syntax_ambig_resol_model ++ " (leftExtend,leftOverTree,rightOverTree,rightExtend,overType,clauTagPrior,rpHitCount) values ('" ++ lev ++ "','" ++ lotv ++ "','" ++ rotv ++ "','" ++ rev ++ "'," ++ otv ++ ",'[" ++ show clauTagPrior ++ "]',1)")) :: Query
                            Noth -> read (show ("insert " ++ syntax_ambig_resol_model ++ " (leftExtend,leftOverTree,rightOverTree,rightExtend,overType,clauTagPrior,nothHitCount) values ('" ++ lev ++ "','" ++ lotv ++ "','" ++ rotv ++ "','" ++ rev ++ "'," ++ otv ++ ",'[" ++ show clauTagPrior ++ "]',1)")) :: Query
            stmt1 <- prepareStmt conn sqlstat
            oks <- executeStmt conn stmt1 []             -- Insert the described structural gene.
            putStrLn $ "updateSyntaxAmbiResolSample': Last inserted row with ID " ++ show (getOkLastInsertID oks)
            closeStmt conn stmt1
            close conn

      x | elem x ["stru_gene_202408" , "stru_gene_202412", "stru_gene_202501"] -> do      -- Multimodel
        let leftExtend = getPhraByEnd (stOfCate leftOver - 1) nPCs          -- Get all left-extend phrases
        let rightExtend = getPhraByStart (enOfCate rightOver + 1) nPCs      -- Get all right-entend phrases

        let le = map ((!!0) . ctpsOfCate) leftExtend         -- [(Category,Tag,PhraStru,Span)] of left-extended phrases
        let lo = (ctpsOfCate leftOver)!!0                    -- (Category,Tag,PhraStru,Span) of left-overlapping phrase
        let ro = (ctpsOfCate rightOver)!!0                   -- (Category,Tag,PhraStru,Span) of right-overlapping phrase
        let re = map ((!!0) . ctpsOfCate) rightExtend        -- [(Category,Tag,PhraStru,Span)] of right-extended phrases
        let ot = overType                                   -- Overlap type

        let lev = doubleBackSlash (show le)                 -- Get values to insert them into MySql Table
        let lov = doubleBackSlash (show lo)
        let rov = doubleBackSlash (show ro)
        let rev = doubleBackSlash (show re)
        let otv = show overType

        conn <- getConn
        let sqlstat = read (show ("select id, clauTagPrior, lpHitCount, rpHitCount, nothHitCount from "
                                   ++ syntax_ambig_resol_model
                                   ++ " where leftExtend = '" ++ lev ++ "' && "
                                   ++ "leftOver = '" ++ lov ++ "' && "
                                   ++ "rightOver = '" ++ rov ++ "' && "
                                   ++ "rightExtend = '" ++ rev ++ "' && "
                                   ++ "overType = " ++ otv)) :: Query
        stmt <- prepareStmt conn sqlstat
        (defs, is) <- queryStmt conn stmt []

        rows <- S.toList is
        if rows /= []
          then
            if length rows > 1
              then do
                closeStmt conn stmt
                close conn                           -- Close MySQL connection.
                error "updateSyntaxAmbiResolSample': Find duplicate structural genes."
              else do
                let id = fromMySQLInt32U ((rows!!0)!!0)
                let clauTagPriorListStr = fromMySQLText ((rows!!0)!!1)
                let lpHitCount = fromMySQLInt16U ((rows!!0)!!2)
                let rpHitCount = fromMySQLInt16U ((rows!!0)!!3)
                let nothHitCount = fromMySQLInt16U ((rows!!0)!!4)

                putStrLn $ "updateSyntaxAmbiResolSample': (" ++ show id ++ ") clauTagPrior: " ++ clauTagPriorListStr ++ ", lpHitCount: " ++ show lpHitCount ++ ", rpHitCount: " ++ show rpHitCount ++ ", nothHitCount: " ++ show nothHitCount
                let clauTagPriorList = stringToCTPList clauTagPriorListStr
                let priorList = map snd $ filter ((== clauTag) . fst) clauTagPriorList          -- The selected priorities

                if elem prior priorList
                  then putStrLn $ "updateSyntaxAmbiResolSample': " ++ show (clauTag, prior) ++ " was hit in row " ++ show id
                  else do
                    resetStmt conn stmt
                    let sqlstat = case prior of
                                    Lp -> read (show ("update " ++ syntax_ambig_resol_model ++ " set clauTagPrior = ?, lpHitCount = ? where id = '" ++ show id ++ "'")) :: Query
                                    Rp -> read (show ("update " ++ syntax_ambig_resol_model ++ " set clauTagPrior = ?, rpHitCount = ? where id = '" ++ show id ++ "'")) :: Query
                                    Noth -> read (show ("update " ++ syntax_ambig_resol_model ++ " set clauTagPrior = ?, nothHitCount = ? where id = '" ++ show id ++ "'")) :: Query
                    stmt <- prepareStmt conn sqlstat
                    let newClauTagPriorList = (clauTag, prior):clauTagPriorList     -- Add with one ClauTagPrior value.
                    case prior of
                      Lp -> executeStmt conn stmt [toMySQLText (show newClauTagPriorList), toMySQLInt16U (lpHitCount + 1)]    -- Add 'lpHitCount' by 1.
                      Rp -> executeStmt conn stmt [toMySQLText (show newClauTagPriorList), toMySQLInt16U (rpHitCount + 1)]    -- Add 'rpHitCount' by 1.
                      Noth -> executeStmt conn stmt [toMySQLText (show newClauTagPriorList), toMySQLInt16U (nothHitCount + 1)]   -- Add 'nothHitCount' by 1.
                    putStrLn $ "updateSyntaxAmbiResolSample': " ++ show (clauTag, prior) ++ " was inserted into row " ++ show id
                closeStmt conn stmt
                close conn                       -- Close MySQL connection.

          else do
            putStrLn "Inquire failed."
            let clauTagPrior = (clauTag, prior)
            let sqlstat = case prior of
                            Lp -> read (show ("insert " ++ syntax_ambig_resol_model ++ " (leftExtend,leftOver,rightOver,rightExtend,overType,clauTagPrior,lpHitCount) values ('" ++ lev ++ "','" ++ lov ++ "','" ++ rov ++ "','" ++ rev ++ "'," ++ otv ++ ",'[" ++ show clauTagPrior ++ "]',1)")) :: Query
                            Rp -> read (show ("insert " ++ syntax_ambig_resol_model ++ " (leftExtend,leftOver,rightOver,rightExtend,overType,clauTagPrior,rpHitCount) values ('" ++ lev ++ "','" ++ lov ++ "','" ++ rov ++ "','" ++ rev ++ "'," ++ otv ++ ",'[" ++ show clauTagPrior ++ "]',1)")) :: Query
                            Noth -> read (show ("insert " ++ syntax_ambig_resol_model ++ " (leftExtend,leftOver,rightOver,rightExtend,overType,clauTagPrior,nothHitCount) values ('" ++ lev ++ "','" ++ lov ++ "','" ++ rov ++ "','" ++ rev ++ "'," ++ otv ++ ",'[" ++ show clauTagPrior ++ "]',1)")) :: Query
            stmt1 <- prepareStmt conn sqlstat
            oks <- executeStmt conn stmt1 []             -- Insert the described structural gene.
            putStrLn $ "updateSyntaxAmbiResolSample': Last inserted row with ID " ++ show (getOkLastInsertID oks)
            closeStmt conn stmt1
            close conn                                   -- Close MySQL connection.

      "stru_gene" -> do
        let leftExtend = getPhraByEnd (stOfCate leftOver - 1) nPCs          -- Get all left-extend phrases
        let rightExtend = getPhraByStart (enOfCate rightOver + 1) nPCs      -- Get all right-entend phrases

        let le = map ((!!0) . ctpOfCate) leftExtend         -- [(Category,Tag,PhraStru)] of left-extended phrases
        let lo = (ctpOfCate leftOver)!!0                    -- (Category,Tag,PhraStru) of left-overlapping phrase
        let ro = (ctpOfCate rightOver)!!0                   -- (Category,Tag,PhraStru) of right-overlapping phrase
        let re = map ((!!0) . ctpOfCate) rightExtend        -- [(Category,Tag,PhraStru)] of right-extended phrases
        let ot = overType                                   -- Overlap type

        let lev = doubleBackSlash (show le)                 -- Get values to insert them into MySql Table
        let lov = doubleBackSlash (show lo)
        let rov = doubleBackSlash (show ro)
        let rev = doubleBackSlash (show re)
        let otv = show overType

        conn <- getConn
        let sqlstat = read (show ("select id, prior, hitCount, priorExCount from stru_gene where leftExtend = '" ++ lev ++ "' && " ++ "leftOver = '" ++ lov ++ "' && " ++ "rightOver = '" ++ rov ++ "' && " ++ "rightExtend = '" ++ rev ++ "' && " ++ "overType = " ++ otv)) :: Query
        stmt <- prepareStmt conn sqlstat
        (defs, is) <- queryStmt conn stmt []

        rows <- S.toList is
        if rows /= []
          then
            if length rows > 1
              then do
                closeStmt conn stmt
                close conn                           -- Close MySQL connection.
                error "updateStruGene': Find duplicate structural genes."
              else do
                let id = fromMySQLInt32U ((rows!!0)!!0)
                let origPrior = fromMySQLText ((rows!!0)!!1)
                let hitCount = fromMySQLInt32U ((rows!!0)!!2)
                let priorExCount = fromMySQLInt16U ((rows!!0)!!3)
                putStrLn $ "updateStruGene': (" ++ show id ++ ") origPrior: " ++ origPrior ++ ", hitCount: " ++ show hitCount ++ ", priorExCount: " ++ show priorExCount

                let newPrior = show prior
                if newPrior == origPrior
                  then do
                    resetStmt conn stmt
                    let sqlstat = read (show ("update stru_gene set hitCount = ? where id = '" ++ show id ++ "'")) :: Query
                    stmt <- prepareStmt conn sqlstat
                    executeStmt conn stmt [toMySQLInt32U (hitCount + 1)]            -- Add column 'hitCount' by 1 of structural gene.
                    closeStmt conn stmt
                    close conn                       -- Close MySQL connection.
                  else do
                    resetStmt conn stmt
                    let sqlstat = read (show ("update stru_gene set prior = ?, hitCount = ?, priorExCount = ? where id = '" ++ show id ++ "'")) :: Query
                    stmt <- prepareStmt conn sqlstat
                    executeStmt conn stmt [toMySQLText newPrior, toMySQLInt32U 0, toMySQLInt16U (priorExCount + 1)]
                    closeStmt conn stmt                                         -- Update columns 'prior', 'hitCount', and 'priorExCount' of structural gene.
                    close conn                                                  -- Close MySQL connection.
          else do
            putStrLn "Inquire failed."
            let sqlstat = read (show ("insert stru_gene (leftExtend,leftOver,rightOver,rightExtend,overType,prior) values ('" ++ lev ++ "','" ++ lov ++ "','" ++ rov ++ "','" ++ rev ++ "'," ++ otv ++ ",'" ++ show prior ++ "')")) :: Query
            stmt1 <- prepareStmt conn sqlstat
            oks <- executeStmt conn stmt1 []             -- Insert the described structural gene.
            putStrLn $ "updateStruGene': Last inserted row with ID " ++ show (getOkLastInsertID oks)
            closeStmt conn stmt1
            close conn                                   -- Close MySQL connection.

      "ambi_resol1" -> do
        let context =  sortPhraCateBySpan [x | x <- nPCs, x /= leftOver, x /= rightOver]     -- Get context for ambiguity resolution, which is sorted by increasing phrasal spans.
{-
        putStr "updateSyntaxAmbiResolSample': Inquire ambiguity fragment: leftPhrase = "
        showPhraCate leftOver
        putStr ", rightPhrase = "
        showPhraCate rightOver
--        putStr ", context = "
--        showNPhraCate context
--        putStr ", overType = "
--        putStr $ show overType
        putStrLn ""
 -}
        let lpv = replace "'" "''" $ doubleBackSlash (getPhraCate_String leftOver)
        let rpv = replace "'" "''" $ doubleBackSlash (getPhraCate_String rightOver)
        let contextv = replace "'" "''" $ doubleBackSlash (getNPhraCate_String context)
        let otv = show overType
        let priorv = show prior

        conn <- getConn

--        let sqlstat = read (show ("select id, prior from ambi_resol1 where leftPhrase = _utf8mb4'" ++ lpv ++ "' && " ++ "rightPhrase = _utf8mb4'" ++ rpv ++ "' && " ++ "context = _utf8mb4'" ++ contextv ++ "' && " ++ "overType = '" ++ otv ++ "'")) :: Query
        let sqlstat = DS.fromString $ "select id, prior from ambi_resol1 where leftPhrase = '" ++ lpv ++ "' && " ++ "rightPhrase = '" ++ rpv ++ "' && " ++ "context = '" ++ contextv ++ "' && " ++ "overType = '" ++ otv ++ "'"
        stmt <- prepareStmt conn sqlstat
        (defs, is) <- queryStmt conn stmt []

        rows <- S.toList is
        if rows /= []
          then
            if length rows > 1
              then do
                closeStmt conn stmt
                close conn                               -- Close MySQL connection.
                error "updateSyntaxAmbiResolSample': Find duplicate ambiguity fragments, which is impossible."
              else do
                let id = fromMySQLInt32U ((rows!!0)!!0)
                let priorOrig = fromMySQLText ((rows!!0)!!1)
                putStr $ "updateSyntaxAmbiResolSample': (" ++ show id ++ ") original prior: " ++ priorOrig ++ ", new prior: " ++ priorv
                if priorOrig /= priorv
                  then do
                    resetStmt conn stmt
                    let sqlstat = DS.fromString $ "update ambi_resol1 set prior = ? where id = " ++ show id
                    stmt <- prepareStmt conn sqlstat
                    executeStmt conn stmt [toMySQLText priorv]
                    putStrLn ", modification is done."
                  else do
                    putStrLn ", no modification is to do."
          else do
            putStr "Inquire failed. Insert the ambiguity resolution fragment ..."
--            let sqlstat = read (show ("insert ambi_resol1 (leftPhrase, rightPhrase, context, overType, prior) values (_utf8mb4'" ++ lpv ++ "', _utf8mb4'" ++ rpv ++ "', _utf8mb4'" ++ contextv ++ "'," ++ otv ++ ",'" ++ priorv ++ "')")) :: Query
            let sqlstat = DS.fromString $ "insert ambi_resol1 (leftPhrase, rightPhrase, context, overType, prior) values ('" ++ lpv ++ "', '" ++ rpv ++ "', '" ++ contextv ++ "'," ++ otv ++ ",'" ++ priorv ++ "')"
            stmt <- prepareStmt conn sqlstat
            oks <- executeStmt conn stmt []                  -- Insert the described structural gene.
            putStrLn $ " [OK], and its id is " ++ show (getOkLastInsertID oks)
        closeStmt conn stmt
        close conn                                       -- Close MySQL connection.

      _ -> error $ "updateSyntaxAmbiResolSample': syntax_ambig_resol_model " ++ syntax_ambig_resol_model ++ " is undefined."

{- Add the parsing result of a clause into treebank designated by <Configuration>.
 - Now, parameter <clauIdx> has not been used for checking.
 -}
storeClauseParsingToTreebank :: SentIdx -> ClauIdx -> ([[Rule]], [PhraCate], [BanPCs]) -> IO ()
storeClauseParsingToTreebank sn clauIdx rtbPCs = do
    confInfo <- readFile "Configuration"
    let tree_target = getConfProperty "tree_target" confInfo

    conn <- getConn
    let sqlstat = DS.fromString $ "create table if not exists " ++ tree_target ++ " (serial_num int primary key, tree mediumtext, script mediumtext, accuracy float)"
    stmt <- prepareStmt conn sqlstat
    executeStmt conn stmt []

    let query = DS.fromString ("select tree, script from " ++ tree_target ++ " where serial_num = ?")       -- Query is instance of IsString.
    stmt <- prepareStmt conn query
    (defs, is) <- queryStmt conn stmt [toMySQLInt32 sn]
    rows <- S.toList is                                                         -- [[MySQLText, MySQLText]]
    closeStmt conn stmt

    if rows == []
      then do
        let trees' = [(clauIdx, snd3 rtbPCs)]
        let scripts' = [(clauIdx, fst3 rtbPCs, thd3 rtbPCs)]

        putStrLn $ "storeClauseParsingToTreebank: trees': " ++ (nTreeToString trees')
        putStrLn $ "storeClauseParsingToTreebank: scripts': " ++ (nScriptToString scripts')

        let query' = DS.fromString ("insert into " ++ tree_target ++ " set tree = ?, script = ?, serial_num = ?")     -- Query is instance of IsString.
        stmt' <- prepareStmt conn query'
        ok <- executeStmt conn stmt' [toMySQLText (nTreeToString trees'), toMySQLText (nScriptToString scripts'), toMySQLInt32 sn]
        let rn = getOkAffectedRows ok
        closeStmt conn stmt'
        close conn
        if (rn /= 0)
          then putStrLn $ "storeClauseParsingToTreebank: " ++ show rn ++ " row(s) were modified."
          else error "storeClauseParsingToTreebank: update failed!"
      else do
        let trees = readTrees $ fromMySQLText ((rows!!0)!!0)
        let scripts = readScripts $ fromMySQLText ((rows!!0)!!1)
        putStrLn $ "storeClauseParsingToTreebank: trees: " ++ (nTreeToString trees)
        putStrLn $ "storeClauseParsingToTreebank: scripts: " ++ (nScriptToString scripts)

        let trees' = trees ++ [(clauIdx, snd3 rtbPCs)]
        let scripts' = scripts ++ [(clauIdx, fst3 rtbPCs, thd3 rtbPCs)]

        putStrLn $ "storeClauseParsingToTreebank: trees': " ++ (nTreeToString trees')
        putStrLn $ "storeClauseParsingToTreebank: scripts': " ++ (nScriptToString scripts')

        let query' = DS.fromString ("update " ++ tree_target ++ " set tree = ?, script = ? where serial_num = ?")   -- Query is instance of IsString.
        stmt' <- prepareStmt conn query'
        ok <- executeStmt conn stmt' [toMySQLText (nTreeToString trees'), toMySQLText (nScriptToString scripts'), toMySQLInt32 sn]
        let rn = getOkAffectedRows ok
        closeStmt conn stmt'
        close conn
        if (rn /= 0)
          then putStrLn $ "storeClauseParsingToTreebank: " ++ show rn ++ " row(s) were modified."
          else error "storeClauseParsingToTreebank: update failed!"

{- Read the tree String of designated sentence in treebank.
 - There are several treebanks stored in database, their table names are corpus, treebank1, and so on.
 - tree_source: Source of trees.
 - tree_field: Field storing parsing tree.
 -}
readTree_String :: SentIdx -> String -> String -> IO String
readTree_String sn tree_source tree_field = do
    conn <- getConn
    let sqlstat = DS.fromString $ "select " ++ tree_field ++" from " ++ tree_source ++ " where serial_num = ?"
    stmt <- prepareStmt conn sqlstat
    (defs, is) <- queryStmt conn stmt [toMySQLInt32 sn]
    row <- S.read is                                --Get [<tree>]
    let row' = case row of
                 Just x -> x
                 Nothing -> error "readTree_String: No tree."
    S.skipToEof is

    return (fromMySQLText (row'!!0))

-- Get the list of clause strings from the tree string. Tree string actually is the String of [[PhraCate]].
sentToClauses :: String -> IO [String]
sentToClauses cs = return $ stringToList cs

{- Display trees' structure of clauses of a sentence. For every clause, the input is a string of (ClauIdx, [PhraCate]).
 -}
dispTreeOfSent :: [String] -> IO ()
dispTreeOfSent [] = putStr ""                          -- Nothing to display.
dispTreeOfSent (s:cs) = do
    let clauTreeStr = stringToTuple s
    putStrLn $ "Clause No.: " ++ (fst clauTreeStr)
    let pcs = getClauPhraCate s
    let spls = divPhraCateBySpan pcs                   -- Span lines
    showTreeStru spls spls
    dispTreeOfSent cs

{- Display trees' structure of clauses of a sentence, one tree per clause.
 - This function is obsoleted.
 -}
dispTree :: [String] -> IO ()
dispTree [] = putStr ""                      -- Nothing to display.
dispTree (s:cs) = do
    dispTree cs
    putStrLn $ "Clause No.: " ++ show (length cs + 1)
    showTreeStru spls spls
    where
      pcs = getClauPhraCate s
      spls = divPhraCateBySpan pcs      -- Span lines

{- Display trees' structure of remaining clauses of a sentence, one tree per clause, and the 'idx' is ordered number of first clause among remaining clauses.
 - The function is obsoleted.
 -}
dispTree' :: ClauIdx -> [String] -> IO ()
dispTree' _ [] = putStr ""                      -- Nothing to display.
dispTree' idx (s:cs) = do
    putStrLn $ "Clause No.: " ++ show idx
    showTreeStru spls spls
    dispTree' (idx + 1) cs
    where
      pcs = getClauPhraCate s
      spls = divPhraCateBySpan pcs      -- Span lines

{- Display Comparison trees' structure of ambiguity resolution result.
 - every clause has one accuracy tree and one ambiguity resolution tree.
 - and the 'idx' is ordered number of first clause among remaining clauses.
 - (s:cs):
 - (x:xs):
 - tree_source1:
 - tree_source2:
 -}
dispComparisonTreesOfAmbiResolResult :: Int -> [String] -> [String] -> String -> String -> IO ()
dispComparisonTreesOfAmbiResolResult _ [] [] _ _ = putStr ""                      -- Nothing to display.
dispComparisonTreesOfAmbiResolResult clauIdx (s:cs) (x:xs) tree_source1 tree_source2 = do
    putStrLn $ "Clause No.: " ++ show clauIdx
    putStrLn $ "Accuracy clause analysis tree from tree_source = " ++ show tree_source1 ++ " is:"
    showTreeStru spls spls
    putStrLn $ "The clause analysis tree obtained by disambiguation which is from ambi_resol_result_tree_source = " ++ show tree_source2 ++ " is:"
    showTreeStru splx splx
    putStr "\n"
    dispComparisonTreesOfAmbiResolResult (clauIdx + 1) cs xs tree_source1 tree_source2
    where
      pcs = getClauPhraCate s
      spls = divPhraCateBySpan pcs      -- Span lines
      pcx = getClauPhraCate x
      splx = divPhraCateBySpan pcx      -- Span lines

{- Get a clause's [PhraCate] from its string value of type Tree, where Tree ::= (ClauIdx, [PhraCate]).
 -}
getClauPhraCate :: String -> [PhraCate]
getClauPhraCate "" = []
getClauPhraCate str = map getPhraCateFromString (stringToList' phraCateListStr)
    where
      clauTreeStrTuple = stringToTuple str                                      -- ("ClauIdx", "[PhraCate]")
      phraCateListStr = snd clauTreeStrTuple                                    -- "[PhraCate]"

-- Get a clause's BanPCs value from string value of (clauIdx, [[Rule]], [BanPCs]).
getClauBanPCs :: String -> BanPCs
getClauBanPCs "" = []
getClauBanPCs str = concat $ map getPhraCateListFromString $ stringToList $ thd3 (stringToTriple str)

{- Parse a sentence. The first input parameter is [Rule] value, and the second input parameter is the clause string of a sentence.
 -}

parseSentWithoutPruning :: Int -> [Rule] -> [String] -> IO ()
parseSentWithoutPruning _ _ [] = putStrLn ""
parseSentWithoutPruning sn rules cs = do
    parseSentWithoutPruning sn rules (take (length cs - 1) cs)
    putStrLn $ "  ===== Clause No." ++ show (length cs) ++ " ====="

    conn <- getConn
    stmt <- prepareStmt conn "update corpus set closure = '[]', forest = '[]' where serial_num = ?"
    ok <- executeStmt conn stmt [toMySQLInt32 sn]
    if (getOkAffectedRows ok == 1)
    then putStrLn "parseSentWithoutPruning: update succeeded."
    else error "parseSentWithoutPruning: update failed!"

    let ws = words (last cs)
    putStrLn $ "Num. of initial phrasal categories = " ++ show (length ws)
                                    -- rules is subset of [Ss,Os,As,Sv,Ov,Av,Hnv,Dv,Sa,Oa,Pa,Cva,Cna,An]
    parseClauseWithoutPruning sn 1 rules $ initPhraCate $ getNCate ws

{- Parse a clause. This is a recursive process, and terminates when no new phrasal category is created. The first
   parameter is the serial number of sentence which the clause is affiliated with, the second parameter is which trip
   of transition to be executed, the third parameter is [Rule] value, where
   Rule::= Ss | Ps | Os | As | Hns | Ns
        | Sv | Ov | Av | Hnv | Dv | Cnv | Cvv | Nv | Pvt | OEvt | Vtvi | Avd
        | Sa | Pa | Va | Oa | Da | Daa | Cna | Cva | Caa | Hna | Na
        | Pn | Vn | An | Cnn | Cvn | Dn | Dan | ADJn | Snd | Ond | Hnnd
        | Sd | Od | Ad | Hnd | Cvd | Nd | ADJd | Dad | Dsd | Dxd | Doed
        | Dp
        | Ooe | Hnoe | Noe
        | Npe
        | Aq
        | Jfc | Jbc
        | U3du3.
   The fourth parameter is word-category string of this clause.
 -}

parseClauseWithoutPruning :: Int -> Int -> [Rule] -> [PhraCate] -> IO ()
parseClauseWithoutPruning sn transIdx rules nPCs = do
    let nPCs2 = trans rules nPCs []
    putStrLn $ "After " ++ show transIdx ++ "th transition, num. of phrasal categories = " ++ show (length nPCs2)
--  showNPhraCateLn (sortPhraCateBySpan nPCs2)
    if ([pc| pc <- nPCs, notElem4Phrase pc nPCs2] /= [])||([pc| pc <- nPCs2, notElem4Phrase pc nPCs] /= [])
--  if (equalSortedPhraList (quickSort4Phrase  nPCs) (quickSort4Phrase  nPCs2))
      then parseClauseWithoutPruning sn (transIdx+1) rules nPCs2
      else do
        putStrLn $ "Num. of phrasal categories in closure is " ++ show (length nPCs)
        let sp = getNuOfInputCates nPCs - 1
        putStrLn $ "Maximal span is " ++ (show sp)

        let roots = getPhraBySS (0, sp) nPCs
        let forest = growForest rules [[t]|t<-roots] nPCs
        putStrLn ("        Parsing Tree No.1 ~ No." ++ show (length forest))
        showForest forest
        putStr "\n"
        putStrLn ("        Tree Structure No.1 ~ No." ++ show (length forest))
        showForestWithTreeStru forest

        storeClauseParsingWithoutPruning sn (nPCs, forest)

--  Add the no-pruning parsing result of a clause into database.
storeClauseParsingWithoutPruning :: Int -> ([PhraCate], [[PhraCate]]) -> IO ()
storeClauseParsingWithoutPruning sn (nPCs, forest) = do
    conn <- getConn
    stmt <- prepareStmt conn "select closure, forest from corpus where serial_num = ?"
    (defs, is) <- queryStmt conn stmt [toMySQLInt32 sn]
    row <- S.read is                                   -- Maybe [MySQLText, MySQLText]
    let row' = case row of
                 Just x -> x                           -- [MySQLText, MySQLText]
                 Nothing -> error "storeClauseParsingWithoutPruning: No row was read."
    S.skipToEof is        -- Use query result before new SQL operation.

    let nClosure = readClosures (fromMySQLText (row'!!0))
--  putStrLn $ "readClosures: nClosure: " ++ (nClosureToString nClosure)

    let nForest = readForests (fromMySQLText (row'!!1))
--  putStrLn $ "readForests: nForest: " ++ (nForestToString nForest)

    let nClosure' = nClosure ++ [nPCs]
    let nForest' = nForest ++ [forest]

--  putStrLn $ "storeClauseParsingWithoutPruning: nClosure': " ++ (nClosureToString nClosure')
--  putStrLn $ "storeClauseParsingWithoutPruning: nForest': " ++ (nForestToString nForest')

    stmt' <- prepareStmt conn "update corpus set closure = ?, forest = ? where serial_num = ?"
    ok <- executeStmt conn stmt' [toMySQLText (nClosureToString nClosure'), toMySQLText (nForestToString nForest'), toMySQLInt32 sn]
    let rn = getOkAffectedRows ok
    if (rn == 1)
      then do
        putStrLn $ "storeClauseParsingWithoutPruning: " ++ show rn ++ " row(s) were modified."
        close conn
      else do
        close conn
        error "storeClauseParsingWithoutPruning: update failed!"

{- Get statistics on a phrasal set, including
   (1) Total number of atomized phrasal categories;
   (2) Times of using various category-converted rules respectively and in total;
   (3) Number of clauses;
   (4) Length, number of parsing trees, and processing time of each clause.
 -}
