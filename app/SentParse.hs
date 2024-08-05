{-# LANGUAGE OverloadedStrings, LambdaCase #-}

-- Copyright (c) 2019-2024 China University of Water Resources and Electric Power,
-- All rights reserved.

-- Sentential Parsing by scripts were orignally written by Qian-qian WANG at 2023, based on which modification was done by Qing-jiang WANG at 2024.

module SentParse (
    getSentFromDB,        -- Int -> IO String
    getSentListFromDB,    -- Int -> Int -> IO [(Int, String)]
    getSent,              -- String -> IO [String]
--    getSentList,              -- String -> IO [String]
    parseSent,            -- Int -> Int -> [String] -> IO Bool
    dropParsingResult,    -- Int -> Int -> Int -> IO Bool
    goBackTo,             -- Int -> Int -> IO ()
    parseSent',           -- Int -> [String] -> IO ()
    storeClauseParsing,   -- Int -> Int -> ([[Rule]],[PhraCate],[PhraCate]) -> IO ()
    parseClause,          -- [[Rule]] -> [PhraCate] -> [PhraCate] -> IO ([[Rule]],[PhraCate],[PhraCate])
    parseSentWithAllLexRules,          -- Int -> [String] -> IO ()
    parseSentWithAllLexRules',         -- Int -> Int -> [String] -> IO Bool
    parseClauseWithAllLexRules,        -- [PhraCate] -> IO [PhraCate]
    doTrans,              -- OnOff -> [PhraCate] -> [PhraCate] -> IO ([OnOff],[PhraCate],[PhraCate])
    updateStruGene,       -- [PhraCate] -> [OverPair] -> [(PhraCate,PhraCate)] -> IO [OverPair]
    updateStruGene',      -- ([PhraCate],PhraCate,PhraCate,[PhraCate],OverType) -> [OverPair] -> IO [OverPair]
    autoRunParseSentByScript, -- Int -> Int -> IO ()
    parseSentByScript,    -- Int -> [String] -> IO ()
    parseSentByScript',   -- Int -> [String] -> [Script] -> IO Bool
    parseClauseWithScript,            -- [[Rule]] -> [PhraCate] -> [PhraCate] -> Script -> IO ([[Rule]],[PhraCate],[PhraCate])
    doTransWithScript,    -- [PhraCate] -> [PhraCate] -> Script -> IO ([Rule], [PhraCate], [PhraCate])
    getPhraStruCSFromStruGene2,       -- Int -> Int -> IO ()
    autoverifyEffectOfPhraStruCS,     -- Int -> IO ()
    autoRunGrammarAmbiResol,          -- Int -> Int -> IO ()
    autoRunCollectPhraSyn,            -- Int -> Int -> IO ()
    autoRunAmbiResolByStruGene,       -- Int -> Int -> IO ()
    parseSentByStruGene,              -- Int -> [String] -> IO ()
    getStruGene1FromAmbiResol1,       -- Int -> Int -> IO ()
    getAccuracyOfAmbiResol,           -- IO ()
    doTransWithManualResol,           -- [Rule] -> [PhraCate] -> [PhraCate] -> Script -> IO ([Rule], [PhraCate], [PhraCate])
    ambiResolByManualResol,           -- [PhraCate] -> [OverPair] -> [(PhraCate, PhraCate)] -> IO [OverPair]
    ambiResolByManualResol',          -- [PhraCate] -> (PhraCate, PhraCate) -> IO OverPair
    updateAmbiResol,      -- [PhraCate] -> [OverPair] -> IO ()
    updateAmbiResol',     -- [PhraCate] -> OverPair -> IO ()
    storeClauseParsingToTreebank,     -- Int -> Int -> ([[Rule]], [PhraCate], [PhraCate]) -> IO ()
    storeTree,            -- Int -> String -> IO ()
    readTree_String,      -- Int -> IO String
    sentToClauses,        -- String -> IO [String]
    dispTreeOfSent,       -- [String] -> IO ()
    dispTree,             -- [String] -> IO ()
    dispTree',            -- Int -> [String] -> IO ()
    dispComparisonTreesOfAmbiResolResult,   -- Int -> [String] -> [String] -> String -> String -> IO ()
    getClauPhraCate,      -- String -> [PhraCate]
    parseSentWithoutPruning      -- [Rule] -> [String] -> IO ()
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

-- Get a sentence from table corpus, actually the sentence is content of column cate_sent2.
getSentFromDB :: Int -> IO String
getSentFromDB sn = do
    conn <- getConn
    stmt <- prepareStmt conn "select cate_sent2 from corpus where serial_num = ?"
    (defs, is) <- queryStmt conn stmt [toMySQLInt32 sn]
    S.read is >>= \case
      Just [MySQLText v] -> return $ fromMySQLText (MySQLText v)
      Nothing -> return ""

-- Get some sentences from table corpus, actually the sentences are content of column cate_sent2.
getSentListFromDB :: Int -> Int -> [(Int, String)] -> IO [(Int, String)]
getSentListFromDB startSn endSn sentList = do
    conn <- getConn
    stmt <- prepareStmt conn "select serial_num, cate_sent2 from corpus where serial_num >= ? and serial_num <= ?"
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
parseSent :: Int -> [String] -> IO ()
parseSent sn cs = do
    hSetBuffering stdin LineBuffering                  -- Open input buffering

    let prompt1 = " There are " ++ show (length cs) ++ " clauses in total, from which clause to start? [RETURN for 1] "
    let inputRange1 = [show x | x <- [1 .. length cs]]
    clauIdxOfStartStr <- getLineUntil prompt1 inputRange1 True
    let clauIdxOfStart = read clauIdxOfStartStr :: Int

    let prompt2 = " to which clause to end? [RETURN for last one] "
    let inputRange2 = [show x | x <- [1 .. length cs]]
    clauIdxOfEndStr <- getLineUntil prompt2 inputRange2 False

    if clauIdxOfEndStr == "ZeroRange"
      then putStrLn "parseSent: Clausal range is zero, and no clause is parsed."               -- Impossible.
      else do               -- Drop trees and scripts of clause clauIdxOfStart to clause clauIdxOfEnd.
             let clauIdxOfEnd = read clauIdxOfEndStr :: Int
             putStrLn $ "Clause from " ++ show clauIdxOfStart ++ " to " ++ show clauIdxOfEnd ++ " will be parsed."
             dropFlag <- dropParsingResult sn clauIdxOfStart clauIdxOfEnd
             if dropFlag
               then do
                 finFlag <- parseSent' sn (clauIdxOfStart - 1) (drop (clauIdxOfStart - 1) (take clauIdxOfEnd cs))      -- Parse clause from clauIdxOfStart to clauIdxOfEnd.
                 if finFlag
                   then putStrLn "parseSent: Finished parsing."
                   else putStrLn "parseSent: Not finished parsing."
               else putStrLn "parseSent: Parsing was cancelled."                 -- dropParsingResult failed, return upper layer calling.

{- To prepare for parsing from clause clauIdxOfStart to clause clauIdxOfEnd, parsing trees and scripts of those clauses are removed in treebank.
 - The treebank is designated by property 'tree_target' in file Configuration.
 - For removing success, return True; otherwise return False.
 -}
dropParsingResult :: Int -> Int -> Int -> IO Bool
dropParsingResult sn clauIdxOfStart clauIdxOfEnd = do
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

    putStrLn $ "dropParsingResult: tree: " ++ (fromMySQLText (head row'))

    let trees = readTrees $ fromMySQLText (row'!!0)
    let scripts = readScripts $ fromMySQLText (row'!!1)

    let clauIdxRange = quickSortForInt [ fst t | t <- trees]
    putStrLn $ "dropParsingResult: The indices of parsed clauses are " ++ show clauIdxRange

    let trees' = [ t | t <- trees, notElem (fst t) [clauIdxOfStart..clauIdxOfEnd]]            -- Element order in [Tree] is kept.
    let scripts' = [ t | t <- scripts, notElem (fst3 t) [clauIdxOfStart..clauIdxOfEnd]]       -- Element order in [Script] is kept.

    if length trees' /= length trees
      then do
        let sqlstat = DS.fromString $ "update " ++ tree_target ++ " set tree = ?, script = ? where serial_num = ?"
        stmt' <- prepareStmt conn sqlstat
        ok <- executeStmt conn stmt' [toMySQLText (nTreeToString trees'), toMySQLText (nScriptToString scripts'), toMySQLInt32 sn]
        if (getOkAffectedRows ok == 1)
          then do
            putStrLn $ "dropParsingResult: Parsing result of clause " ++ show clauIdxOfStart ++ " to clause " ++ show clauIdxOfEnd ++ " has been removed from database."
            return True
          else do
            putStrLn "dropParsingResult: Parsing result failed to remove!"
            return False
      else do
        putStrLn "dropParsingResult: No parsing result needs to be removed."
        return True

{- To be ready for parsing from clause <ci>, parsing trees and scripts of clauses with index bigger than or equal to <ci> are deleted in treebank.
 - The treebank is designated by property 'tree_target' in file Configuration.
 - For skip success, return True; otherwise return False.
 - The function is obsoleted, replaced with dropParsingResult to support parsing the designated range of clauses.
 -}
goBackTo :: Int -> Int -> IO Bool
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
parseSent' :: Int -> Int -> [String] -> IO Bool
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
        rtbPCs' <- parseClause [] nPCs []                                       -- Parse begins with empty '[[Rule]]' and empty 'banPCs'
        if rtbPCs' == ([],[],[])
          then return False                                                     -- False means the current clause is terminated manually.
          else do
            storeClauseParsing sn clauIdx rtbPCs'
            return True                                                         -- Add the parsing result of this clause into database.
      else do
        putStrLn "  Skip!"
        return False

--  Add the parsing result of a clause into database. Now, parameter <clauIdx> has not been used for checking.
storeClauseParsing :: Int -> Int -> ([[Rule]], [PhraCate], [PhraCate]) -> IO ()
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
--    putStrLn $ "storeClauseParsing: tree: " ++ (nTreeToString tree)
--    putStrLn $ "storeClauseParsing: script: " ++ (nScriptToString script)

    let tree' = quickSortForTree $ tree ++ [(clauIdx, snd3 rtbPCs)]
    let script' = quickSortForScript $ script ++ [(clauIdx, fst3 rtbPCs, thd3 rtbPCs)]

--    putStrLn $ "storeClauseParsing: tree': " ++ (nTreeToString tree')
--    putStrLn $ "storeClauseParsing: script': " ++ (nScriptToString script')

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
   (2) If creating new phrasal categories, append rules used in this trip to [[Rule]], take resultant phrases and accumulated banned phrases as input, go (1); Otherwise, return the triple ([[Rule]], resultant tree PCs, accumulated banned PCs).
 -}

parseClause :: [[Rule]] -> [PhraCate] -> [PhraCate] -> IO ([[Rule]],[PhraCate],[PhraCate])
parseClause rules nPCs banPCs = do
    rtbPCs <- doTrans [] nPCs banPCs           -- Every trip of transition begins with empty rule set.
                                               -- <rtbPCs> ::= ([Rule], resultant tree PCs, accumulated banned PCs)
                                               -- [Rule] is the set of rules used in this trip of transition.
    if rtbPCs == ([],[],[])
      then return ([],[],[])                   -- Return ([],[],[]) as the terminating flag.
      else if nPCs /= (snd3 rtbPCs)
        then parseClause (rules ++ [fst3 rtbPCs]) (snd3 rtbPCs) (thd3 rtbPCs)    -- Do the next trip of transition
                                               -- with appended rules, resultant PCs, and accumulated banned PCs.
        else do                                -- Phrasal closure has been formed.
          putStrLn $ "Num. of phrasal categories in closure is " ++ (show $ length nPCs)
          showNPhraCateLn (sortPhraCateBySpan nPCs)
          let spls = divPhraCateBySpan (nPCs)
          putStrLn "  ##### Parsing Tree #####"
          showTreeStru spls spls
          return (rules ++ [fst3 rtbPCs], snd3 rtbPCs, thd3 rtbPCs)

{- Parse a sentence which includes multiple clauses. Parsing can start from a certain clause, and end with a certain clause. The first parameter is the value of 'serial_num' in database Table 'corpus'. The parsing results would not be stored back to database, so the 'sn' is useless now.
 -}
parseSentWithAllLexRules :: Int -> [String] -> IO ()
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
parseSentWithAllLexRules' :: Int -> Int -> [String] -> IO ()
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

{- Do a trip of transition, insert or update related structural genes in Table stru_gene, and return the category-converted rules used in this trip, the resultant phrases, and the banned phrases.
 - If transitive parsing is to terminated, namely selecting 'e' at inquiring rule switches, returnes ([],[],[]) as the terminating flag.
 -}
doTrans :: [Rule] -> [PhraCate] -> [PhraCate] -> IO ([Rule], [PhraCate], [PhraCate])
doTrans onOff nPCs banPCs = do
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
             doTrans newOnOff nPCs banPCs                -- Redo this trip of transition by modifying rule switches.
           else do
             putStrLn "Rule switch expression error. Consider again!"
             doTrans onOff nPCs banPCs
      else if ruleSwitchOk == "y" || ruleSwitchOk == ""     -- Press key 'y' or directly press RETURN
             then do
               let nPCs2 = trans onOff nPCs banPCs          -- Without pruning, get transitive result.
               putStr "Transitive result before pruning: "
               showNPhraCateLn (sortPhraCateBySpan nPCs2)
               putStr "Banned phrases: "
               showNPhraCateLn (banPCs)                       -- Can't use <sortPhraCateBySpan> on <banPCs>.

               let pcps = getOverlap nPCs2                  -- [(PhraCate, PhraCate)]
               overPairs <- updateStruGene nPCs2 [] pcps    -- IO [OverPair], namely IO [(PhraCate, PhraCate, Prior)], record overlapping pairs for pruning.
               nbPCs <- transWithPruning onOff nPCs banPCs overPairs     -- Get transitive result with pruning.
               putStr "Transitive result after pruning: "
               showNPhraCateLn (sortPhraCateBySpan (fst nbPCs))
               putStr "Banned phrases: "
               showNPhraCateLn (snd nbPCs)                  -- The banned phrases after updated.

               putStr "This trip of transition is ok? [y/n/e]: (RETURN for 'y')"
               transOk <- getLine                           -- Get user decision of whether to do next transition
               if transOk == "y" || transOk == ""           -- Press key 'y' or directly press RETURN
                 then return (onOff,(fst nbPCs),(snd nbPCs))
                 else if transOk == "n"
                        then doTrans onOff nPCs banPCs      -- Redo this trip of transition.
                        else if transOk == "e"
                               then return ([],[],[])       -- Return from doTrans, and indicate this is terminating exit.
                               else do
                                 putStrLn "Please input 'y', 'n', or 'e'!"
                                 doTrans onOff nPCs banPCs
             else if ruleSwitchOk == "e"
                    then return ([],[],[])                  -- Return from doTrans, and indicate this is terminating exit.
                    else error "doTrans: Impossible input error!"

{- Insert or update related structural genes in Table stru_gene, and recursively create overlapping pairs.
   For every pair of overlapping phrases, its overlap type, left- and right-extend phrases are found in a given set
   of phrases.
 -}
updateStruGene :: [PhraCate] -> [OverPair] -> [(PhraCate,PhraCate)] -> IO [OverPair]
updateStruGene _ overPairs [] = do
--    putStrLn "updateStruGene: End"
    putStrLn ""                        -- To make output easy to read.
    return overPairs
updateStruGene nPCs overPairs (pcp:pcps) = do
    newOverPairs <- updateStruGene' struGene overPairs       -- Update structural gene in Table stru_gene
    updateStruGene nPCs newOverPairs pcps
    where
      lop = fst pcp
      rop = snd pcp
      ot = getOverType nPCs lop rop                      -- Get overlapping type
      leps = getPhraByEnd (stOfCate lop - 1) nPCs        -- Get all left-extend phrases
      reps = getPhraByStart (enOfCate rop + 1) nPCs      -- Get all right-entend phrases
      struGene = (leps,lop,rop,reps,ot)

{- Update structural genes related with a certain pair of overlapping phrases, add the overlapping pair to the input
   list of OverPair(s), then return the new OverPair list.
 -}
updateStruGene' :: ([PhraCate],PhraCate,PhraCate,[PhraCate],OverType) -> [OverPair] -> IO [OverPair]
updateStruGene' gene overPairs = do
    let leftExtend = fst5 gene
    let leftOver = snd5 gene
    let rightOver = thd5 gene
    let rightExtend = fth5 gene
    let overType = fif5 gene

    putStr "Find structural fragment: "
    showStruFrag leftExtend leftOver rightOver rightExtend overType

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

    putStrLn $ "Inquire structural gene: leftExtend = '" ++ show le ++ "' && " ++
                                          "leftOver = '" ++ show lo ++ "' && " ++
                                         "rightOver = '" ++ show ro ++ "' && " ++
                                       "rightExtend = '" ++ show re ++ "' && " ++
                                          "overType = "  ++ show ot
    conn <- getConn
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
            if input == "y" || input == ""       -- Press key 'y' or directly press RETURN.
              then do
                resetStmt conn stmt
                let sqlstat = read (show ("update stru_gene set hitCount = ? where id = '" ++ show id ++ "'")) :: Query
                stmt <- prepareStmt conn sqlstat
                executeStmt conn stmt [toMySQLInt32U (hitCount + 1)]            -- Add column 'hitCount' by 1 of structural gene.
                close conn                       -- Close MySQL connection.
                return ((snd5 gene, thd5 gene, read prior::Prior):overPairs)
              else do
                putStr "please input new priority [Lp/Rp]: (RETURN for 'Lp') "
                newPrior <- getLine
                if (newPrior == "Lp" || newPrior == "Rp") && newPrior /= prior                   -- Ask to modify column 'prior'.
                  then do
                    resetStmt conn stmt
--                    let sqlstat = read (show ("update stru_gene set prior = ? where leftExtend = '" ++ lev ++ "' && " ++ "leftOver = '" ++ lov ++ "' && " ++ "rightOver = '" ++ rov ++ "' && " ++ "rightExtend = '" ++ rev ++ "' && " ++ "overType = "  ++ otv)) :: Query
                    let sqlstat = read (show ("update stru_gene set prior = ?, hitCount = ?, priorExCount = ? where id = '" ++ show id ++ "'")) :: Query
                    stmt <- prepareStmt conn sqlstat
                    executeStmt conn stmt [toMySQLText newPrior, toMySQLInt32U 0, toMySQLInt16U (priorExCount + 1)]
                                                                                -- Update columns 'prior', 'hitCount', and 'priorExCount' of structural gene.
                    close conn                                                  -- Close MySQL connection.
                    return ((snd5 gene, thd5 gene, read newPrior::Prior):overPairs)
                  else if newPrior == prior || (newPrior == "" && prior == "Lp")            -- Actually, the priority is not asked to change.
                         then do
                           resetStmt conn stmt
                           let sqlstat = read (show ("update stru_gene set hitCount = ? where id = '" ++ show id ++ "'")) :: Query
                           stmt <- prepareStmt conn sqlstat
                           executeStmt conn stmt [toMySQLInt32U (hitCount + 1)]       -- Add column 'hitCount' by 1 of structural gene.
                           close conn                                                 -- Close MySQL connection.
                           return ((snd5 gene, thd5 gene, read "Lp"::Prior):overPairs)
                         else if newPrior == "" && prior /= "Lp"                      -- The priority changes from 'Rp' to 'Lp' when pressing Key RETURN.
                                then do
                                  resetStmt conn stmt
                                  let sqlstat = read (show ("update stru_gene set prior = ?, hitCount = ?, priorExCount = ? where id = '" ++ show id ++ "'")) :: Query
                                  stmt <- prepareStmt conn sqlstat
                                  executeStmt conn stmt [toMySQLText "Lp", toMySQLInt32U 0, toMySQLInt16U (priorExCount + 1)]
                                                                                -- Update 'prior', 'hitCount', and 'priorExCount' of the gene.
                                  close conn                                    -- Close MySQL connection.
                                  return ((snd5 gene, thd5 gene, read "Lp"::Prior):overPairs)
                                else do
                                  putStrLn "updateStruGene': Illegal priority"
                                  updateStruGene' gene overPairs        -- Calling the function itself again.
      else do
        putStr "Inquire failed, skip? [y/n]: (RETURN for 'y') "
        input <- getLine
        if input == "y" || input == ""     -- Press key 'y' or directly press RETURN.
          then
            updateStruGene' gene overPairs      -- To now, don't allow skipping.
          else do
            putStr "please input new priority [Lp/Rp]: (RETURN for 'Lp') "
            newPrior <- getLine
            if newPrior == "Lp" || newPrior == "Rp"
              then do
                let sqlstat = read (show ("insert stru_gene (leftExtend,leftOver,rightOver,rightExtend,overType,prior) values ('" ++ lev ++ "','" ++ lov ++ "','" ++ rov ++ "','" ++ rev ++ "'," ++ otv ++ ",'" ++ newPrior ++ "')")) :: Query
                stmt1 <- prepareStmt conn sqlstat
                oks <- executeStmt conn stmt1 []             -- Insert the described structural gene.
                putStrLn $ "updateStruGene': Last inserted row with ID " ++ show (getOkLastInsertID oks)
                close conn                                   -- Close MySQL connection.
                return ((snd5 gene, thd5 gene, read newPrior::Prior):overPairs)
              else if newPrior == ""
                then do
                  let sqlstat = read (show ("insert stru_gene (leftExtend,leftOver,rightOver,rightExtend,overType,prior) values ('" ++ lev ++ "','" ++ lov ++ "','" ++ rov ++ "','" ++ rev ++ "'," ++ otv ++ ",'Lp')")) :: Query
                  stmt2 <- prepareStmt conn sqlstat
                  oks <- executeStmt conn stmt2 []           -- Insert the described structural gene.
                  putStrLn $ "updateStruGene': Last inserted row with ID " ++ show (getOkLastInsertID oks)
                  close conn                                                    -- Close MySQL connection.
                  return ((snd5 gene, thd5 gene, read "Lp"::Prior):overPairs)
                else do
                  putStrLn "updateStruGene': Illegal priority"
                  updateStruGene' gene overPairs   -- Calling the function itself again.

-- Recursively parse sentences according scripts, such that samples of category-conversional ambiguity resolution model SLR can be created.
autoRunParseSentByScript :: Int -> Int -> IO ()
autoRunParseSentByScript startSn endSn = do
    putStrLn $ "Sentence No." ++ show startSn ++ " start to create SLR samples."
    getSentFromDB startSn >>= getSent >>= parseSentByScript startSn
    let startSn' = startSn +1
    if startSn' <= endSn
      then autoRunParseSentByScript startSn' endSn
      else putStrLn "autoRunParseSentByScript: End."

type NumOfClauEqual = Int
type ClauIdxOfClauUnequal = [Int]                                 -- 不等小句的编号的列表
type TagOfClauAnaly = (NumOfClauEqual, ClauIdxOfClauUnequal)      -- 相等小句的个数，不等小句的编号

{- Re-parse a sentence according the previously created parsing script.
 - Here every clause is a String, and parsing starts from the first clause.
 - The first parameter is the value of 'serial_num' in database Table 'corpus'.
 - This function is used to create some side-products, such as samlpes of category conversion ambiguity resolution model SLR.
 -}
parseSentByScript :: Int -> [String] -> IO ()
parseSentByScript sn cs = do
    hSetBuffering stdin LineBuffering                  -- Open input buffering
    confInfo <- readFile "Configuration"
    let script_source = getConfProperty "cate_conv_ambig_resol_source" confInfo                       -- Script source
    let tree_source = getConfProperty "cate_conv_ambig_resol_source" confInfo                         -- Tree source
    let cate_conv_ambig_resol_target = getConfProperty "cate_conv_ambig_resol_target" confInfo        -- SLR sample target.

    conn <- getConn
    let query = DS.fromString ("select script from " ++ script_source ++ " where serial_num = ?")       -- Query is instance of IsString.
    stmt <- prepareStmt conn query
    (defs, is) <- queryStmt conn stmt [toMySQLInt32 sn]                     --([ColumnDef], InputStream [MySQLValue])
    record <- S.read is
    let scriptStr = case record of
                    Just x -> x
                    Nothing -> [MySQLText "[]"]
    let script = fromMySQLText (scriptStr!!0)
    skipToEof is                                                            -- Go to the end of the stream.
    closeStmt conn stmt

    let script' = readScripts $ script
    putStr "Parsing script: "
    showScript script'

    putStr $ " There are " ++ show (length cs) ++ " clauses in total."

    finFlagAndSLRAndTree <- parseSentByScript' sn cs script'
    if (fst4 finFlagAndSLRAndTree)
      then do
        let query = DS.fromString ("select tree from " ++ tree_source ++ " where serial_num = ?")       -- Query is instance of IsString.
        stmt <- prepareStmt conn query
        (defs, is) <- queryStmt conn stmt [toMySQLInt32 sn]
        rows <- S.toList is
        let origTrees = readTrees $ fromMySQLText ((rows!!0)!!0)
        let origTrees' = map (\t -> (fst t, (sortPhraCateBySpan . snd) t)) origTrees         -- PhraCates are ordered by span for every clause.
        let newTree = thd4 finFlagAndSLRAndTree

        putStrLn "origTrees: "
        showTrees origTrees'
        putStrLn ""
        putStrLn "newTree: "
        showTrees newTree
        putStrLn "parseSentByScript: Finished parsing."

        let tagOfClauAnaly = (length origTrees, (fst . fth4) finFlagAndSLRAndTree, (snd . fth4) finFlagAndSLRAndTree)   -- Compare original tree with new tree.

        let sqlstat = DS.fromString $ "update " ++ cate_conv_ambig_resol_target ++ " set SLR = ?, tagClause = ? where serial_num = ?"
        stmt <- prepareStmt conn sqlstat
        ok <- executeStmt conn stmt [toMySQLText (nClauseSLRToString (snd4 finFlagAndSLRAndTree)), toMySQLText (show tagOfClauAnaly), toMySQLInt32 sn]
        close conn

        putStrLn $ "tagOfClauAnaly = " ++ show tagOfClauAnaly
        putStrLn $ "Sentence No."++ show sn ++" has "++ show (length origTrees) ++" clauses, in which "++ show ((fst . fth4) finFlagAndSLRAndTree) ++ " clauses are the same with the origial clauses."
      else putStrLn "parseSentByScript: Not finished parsing."

{- Re-parse a sentence according the previously created parsing script.
 - Here every clause is a String.
 - Parameter 'sn' is the value of 'serial_num' in database Table 'corpus'.
 - Table 'corpus', 'treebank1', and some other tables are associated with field 'serial_num'.
 - 'cs' is clausal strings to be parsed.
 - 'script' is parsing scripts for these clauses.
 - If a certain clause is not finished in parsing, return False to skip the remaining clauses.
 - 'SLROfSent' is the category conversion ambiguity resolution of this sentence.
 - Tree ::= (ClauIdx, [PhraCate]) is parsing result of one clause.
 - TagOfClauAnaly ::= (NumOfClauEqual, ClauIdxOfClauUnequal)      -- 相等小句的个数，不等小句的编号
 -}
parseSentByScript' :: Int -> [String] -> [Script] -> IO (Bool, SLROfSent, [Tree], TagOfClauAnaly)
parseSentByScript' _ [] _ = return (True, [], [], (0,[]))
parseSentByScript' sn cs scripts = do
    finFlagAndSLRAndTree <- parseSentByScript' sn (take (length cs - 1) cs) (take (length cs - 1) scripts)     -- 前递归，即先分析第一个小句。
    let numOfClauEqual = fst $ fth4 finFlagAndSLRAndTree
    let clauIdxOfClauUnequal = snd $ fth4 finFlagAndSLRAndTree
    let clauIdx = length cs
    putStrLn $ "  ===== Clause No." ++ show clauIdx ++ " ====="
    if (fst4 finFlagAndSLRAndTree)                                              -- True means the previous clauses has been finished parsing.
      then do
        let nPCs = initPhraCate $ getNCate $ words (last cs)
        putStr "Before parsing: "
        showNPhraCateLn nPCs
        putStr "Word semantic sequence: "
        showNSeman nPCs

        let lastScript = case scripts of                                        -- It's possible of no script to use.
                           [] -> (clauIdx, [], [])                              -- Null script for clause 'clauIdx'
                           [x] -> x
                           (x:xs) -> last xs

        confInfo <- readFile "Configuration"
        let tree_source = getConfProperty "cate_conv_ambig_resol_source" confInfo           -- Tree source is for retrieving correct parsing trees.

        conn <- getConn
        let query = DS.fromString ("select tree from " ++ tree_source ++ " where serial_num = ?")       -- Prepare to read Tree field.
        stmt <- prepareStmt conn query
        (defs, is) <- queryStmt conn stmt [toMySQLInt32 sn]
        rows <- S.toList is
        let origTrees = readTrees $ fromMySQLText ((rows!!0)!!0)
        let origClauIdxTree = origTrees!!(clauIdx-1)                            -- (ClauIdx, [PhraCate]) of the last clause.
        close conn

        rtbPCs <- parseClauseWithScript [] nPCs [] lastScript []                -- Parse begins with empty '[[Rule]]' and empty 'banPCs'
        let newClauIdxTree = snd4 rtbPCs
        let judgeClauIdxTree = [x | x <- (snd origClauIdxTree), elem x newClauIdxTree]     -- [PhraCate]
        let numOfClauEqual' = case (length judgeClauIdxTree == length (snd origClauIdxTree)) of
                                True -> numOfClauEqual + 1
                                False -> numOfClauEqual

        let newSLRSampleOfSent = (snd4 finFlagAndSLRAndTree) ++ [fth4 rtbPCs]
        let newTreeOfSent = (thd4 finFlagAndSLRAndTree) ++ [(clauIdx, snd4 rtbPCs)]
        let rtbPCs' = (fst4 rtbPCs, snd4 rtbPCs, thd4 rtbPCs)
        if rtbPCs' == ([],[],[])
          then return (False, newSLRSampleOfSent, newTreeOfSent, (numOfClauEqual',clauIdxOfClauUnequal))
          else if (length judgeClauIdxTree == length (snd origClauIdxTree))
               then return (True, newSLRSampleOfSent, newTreeOfSent, (numOfClauEqual',clauIdxOfClauUnequal))    -- False means the current clause is terminated manually.
               else do
                 let clauIdxOfClauUnequal' = clauIdxOfClauUnequal ++ [clauIdx]
                 putStrLn "origClauTree:"
                 showNPhraCateLn (sortPhraCateBySpan (snd origClauIdxTree))
                 putStrLn "newClauTree:"
                 showNPhraCateLn newClauIdxTree
                 return (True, newSLRSampleOfSent, newTreeOfSent, (numOfClauEqual', clauIdxOfClauUnequal'))

      else do
        putStrLn $ "Skip clause " ++ show clauIdx
--        return (False, snd4 finFlagAndSLRAndTree, thd4 finFlagAndSLRAndTree, numOfClauEqual)
        return (False, snd4 finFlagAndSLRAndTree, thd4 finFlagAndSLRAndTree, fth4 finFlagAndSLRAndTree)

type Stub = [PhraCate]
type SLROfATrans = (Stub, [Rule])
type SLROfClause = [SLROfATrans]
type SLROfSent = [SLROfClause]

{- Parsing a clause is a recursive transition process.
 - Input: A sequence of [Rule], a sequence of phrasal categories, a sequence of banned phrasal categories, and a parsing script;
 - Algo.:
 - (1) Do one trip of transition;
 - (2) If creating new phrasal categories, append rules used in this trip to [[Rule]], take resultant phrases and accumulated banned
 -     phrases as input, go (1); Otherwise, return the quadruple ([[Rule]], resultant tree PCs, accumulated banned PCs, SLR List Of This Clause).
 -}
parseClauseWithScript :: [[Rule]] -> [PhraCate] -> [PhraCate] -> Script -> SLROfClause -> IO ([[Rule]], [PhraCate], [PhraCate], SLROfClause)
parseClauseWithScript rules nPCs banPCs script origSLRSample = do
    rtbPCs <- doTransWithScript nPCs banPCs script
                                               -- Tree = (ClauIdx, [PhraCate])
                                               -- <rtbPCs> ::= ([Rule], resultant tree PCs, accumulated banned PCs)
                                               -- [Rule] is the set of rules used in this trip of transition.
    let ruleOfATrans = fst3 rtbPCs
    let sLROfATrans = (nPCs, ruleOfATrans)
    let newSLRSample = origSLRSample ++ [sLROfATrans]

    if rtbPCs == ([],[],[])
      then return ([],[],[],origSLRSample)                   -- Terminating flag.
      else if nPCs /= (snd3 rtbPCs)
        then do
          let scriptTail = case script of
                              (clauIdx, [], bPCs) -> (clauIdx, [], bPCs)        -- Null script
                              (clauIdx, [x], bPCs) -> (clauIdx, [], bPCs)       -- Remove the head element of OnOff list in parsing script.
                              (clauIdx, (x:xs), bPCs) -> (clauIdx, xs, bPCs)

          parseClauseWithScript (rules ++ [fst3 rtbPCs]) (snd3 rtbPCs) (thd3 rtbPCs) scriptTail newSLRSample        -- Do the next trip of transition
                                               -- with appended rules, resultant PCs, and accumulated banned PCs.
        else do                                -- Phrasal closure has been formed.
          putStrLn $ "Num. of phrasal categories in closure is " ++ (show $ length nPCs)
          showNPhraCateLn (sortPhraCateBySpan nPCs)
          let spls = divPhraCateBySpan nPCs
          putStrLn "  ##### Parsing Tree #####"
          showTreeStru spls spls
          putStrLn $ "SLR ="
          showSLROfClause newSLRSample     -- Last round of transition does not create SLR.
          return (rules ++ [fst3 rtbPCs], (snd3 rtbPCs), thd3 rtbPCs, newSLRSample)

{- Do a trip of transition, insert or update related ambiguity resolution samples in Table <ambi_resol_model>, and return the category-converted
 - rules used in this trip, the resultant phrases, and the banned phrases.
 - If transitive parsing is to terminated, namely selecting 'e' at inquiring rule switches, returnes ([],[],[]) as the terminating flag.
 -}
doTransWithScript :: [PhraCate] -> [PhraCate] -> Script -> IO ([Rule], [PhraCate], [PhraCate])
doTransWithScript nPCs banPCs script = do
    let onOffs = snd3 script
    let onOff = case onOffs of                      -- Get rule switches of this trip of transition
                      [] -> [] :: OnOff             -- No script of rule switches to use
                      (x:_) -> x :: OnOff           -- There is a script of rule switches to use

    putStr "Rule switches: "
    showOnOff onOff                                 -- Display rule switches

    let nPCs2 = trans onOff nPCs banPCs             -- Without pruning, get transitive result.

--    putStr "Transitive result before pruning: "
--    showNPhraCateLn (sortPhraCateBySpan nPCs2)
    putStr "New phrases before pruning: "
    showNPhraCateLn [pc | pc <- nPCs2, notElem' pc nPCs]
--    putStr "Banned phrases: "
--    showNPhraCateLn (banPCs)                      -- Can't use <sortPhraCateBySpan> on <banPCs>.

    let pcps = getOverlap nPCs2                     -- [(PhraCate, PhraCate)]
    let overPairsByScript = ambiResolByScript nPCs2 [] pcps script    -- [OverPair], namely [(PhraCate, PhraCate, Prior)], record overlapping pairs for pruning.
    putStr "ambiResolByScript: overPairsByScript: "
    showNOverPair overPairsByScript

    result <- acceptOrNot overPairsByScript "Is the script-resolving result OK? [y/n]: (RETURN for 'y')"
    let overPairs = case result of
                      Just x -> x
                      Nothing -> [] :: [OverPair]

    let pcpsWithPrior = map (\x->(fst3 x, snd3 x)) overPairs
    let pcps' = [pcp | pcp <- pcps, notElem pcp pcpsWithPrior]                  -- [(PhraCate, PhraCate)] not resolved by script.
    putStr "doTransWithScript: Manually resolve: "
    showNPhraCatePair pcps'

    overPairs' <- ambiResolByManualResol nPCs2 [] pcps'       -- [OverPair], namely [(PhraCate, PhraCate, Prior)], record overlapping pairs for pruning.
    putStr "ambiResolByManualResol: overPairs': "
    showNOverPair overPairs'

    let overPairs'' = overPairs ++ overPairs'
    nbPCs <- transWithPruning onOff nPCs banPCs overPairs''                     -- Get transitive result with pruning.

--    putStr "Transitive result after pruning: "
--    showNPhraCateLn (sortPhraCateBySpan (fst nbPCs))
    putStr "New phrases after pruning: "
    showNPhraCateLn [pc | pc <- fst nbPCs, notElem' pc nPCs]
    putStr "Banned phrases: "
    showNPhraCateLn (snd nbPCs)                    -- The banned phrases after updated.

    transOk <- getLineUntil "This trip of transition is ok? [y/n/e]: (RETURN for 'y')" ["y","n","e"] True   -- Get user decision of whether to do next transition
    if transOk == "y" || transOk == ""             -- Press key 'y' or directly press RETURN
      then do
          updateAmbiResol (fst nbPCs) overPairs''                               -- Record ambiguity resolution fragments.
          return (onOff,sortPhraCateBySpan (fst nbPCs),sortPhraCateBySpan (snd nbPCs))
      else if transOk == "n"
             then doTransWithManualResol onOff nPCs banPCs      -- do this trip of transition by manually resolving ambiguities.
             else if transOk == "e"
                    then return ([],[],[])       -- Return from doTrans, and indicate this is terminating exit.
                    else error "doTransWithScript: Impossible input error!"

{- Resolve ambiguities by parsing script. Let banPCs be the set of banned phrases, which can be obtained from parsing script. For (lp, rp),
 - if lp belongs to banPCs but rp does not, then we get (lp, rp, Rp).
 - if rp belongs to banPCs but lp does not, then we get (lp, rp, Lp).
 - if both lp and rp belong to banPCs, then we get (lp, rp, Noth).
 - if both lp and rp do not belong to banPCs, then the ambiguity resolution for this pair of phrases is skipped.
 -}
ambiResolByScript :: [PhraCate] -> [OverPair] -> [(PhraCate, PhraCate)] -> Script -> [OverPair]
ambiResolByScript _ overPairs [] _ = overPairs
ambiResolByScript nPCs overPairs (pcp:pcps) script
    | notElem lp banPCs && elem rp banPCs = ambiResolByScript nPCs ((lp, rp, Lp):overPairs) pcps script
    | elem lp banPCs && notElem rp banPCs = ambiResolByScript nPCs ((lp, rp, Rp):overPairs) pcps script
    | elem lp banPCs && elem rp banPCs = ambiResolByScript nPCs ((lp, rp, Noth):overPairs) pcps script
    | otherwise = ambiResolByScript nPCs overPairs pcps script
    where
    banPCs = thd3 script
    lp = fst pcp
    rp = snd pcp

{-
lexAmbiResol :: [PhraCate] -> IO [Rule]
lexAmbiResol nPCs = do
    conn <- getConn
    let query = DS.fromString ("select SLR from treebank1 where serial_num >= ? and serial_num <= ?")       -- Query is instance of IsString.
    stmt <- prepareStmt conn query
    (defs, is) <- queryStmt conn stmt [toMySQLInt32 1, toMySQLInt32 200]
--    rows <- S.toList is
--    let sLRListOfSent = map (\x -> readSLROfSent (fromMySQLText (x!!0))) rows
    sLRStrListOfSent <- readStreamByText [] is      -- [String], each string is the string of SLRs of one sentence.
    let sLRListOfSent = map (\x -> readSLROfSent x) sLRStrListOfSent
    let sLRListOfClause = foldl (++) [] sLRListOfSent
    let sLRListOfTrans = foldl (++) [] sLRListOfClause
    close conn

    let ctpOfnPCs = ctpOfCateList nPCs
    let ctpOfsLR = map (\x -> (ctpOfCateList fst x, snd x)) sLRListOfTrans
    let distRuleList = map (\x -> (distPhraSynSet ctpOfnPCs (fst x), snd x)) sLRListOfTrans
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

    let phraStruOflo = thd3 $ readPhraSynFromStr (fromMySQLText ((rows!!0)!!0))
    let phraStruOfro = thd3 $ readPhraSynFromStr (fromMySQLText ((rows!!0)!!1))
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

        let phraStruOflo = thd3 $ readPhraSynFromStr (fromMySQLText ((rows!!0)!!0))
        let phraStruOfro = thd3 $ readPhraSynFromStr (fromMySQLText ((rows!!0)!!1))
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

autoRunGrammarAmbiResol :: Int -> Int -> IO ()
autoRunGrammarAmbiResol startSn endSn = do
    confInfo <- readFile "Configuration"
    let tree_target = getConfProperty "tree_target" confInfo
    let wle = read (getConfProperty "wle" confInfo) :: Int
    let wlo = read (getConfProperty "wlo" confInfo) :: Int
    let wro = read (getConfProperty "wro" confInfo) :: Int
    let wre = read (getConfProperty "wre" confInfo) :: Int
    let wot = read (getConfProperty "wot" confInfo) :: Int
    let wpr = read (getConfProperty "wpr" confInfo) :: Int
    let distWeiRatioList = [wle, wlo, wro, wre, wot, wpr]

    conn <- getConn
    let query = DS.fromString ("select SLR from treebank1 where serial_num >= ? and serial_num <= ?")       -- Query is instance of IsString.
    stmt <- prepareStmt conn query
    (defs, is) <- queryStmt conn stmt [toMySQLInt32 1, toMySQLInt32 200]

--    rows <- S.toList is
--    let sLRListOfSent = map (\x -> readSLROfSent (fromMySQLText (x!!0))) rows
    sLRStrListOfSent <- readStreamByText [] is      -- [String], each string is the string of SLRs of one sentence.
    let sLRListOfSent = map (\x -> readSLROfSent x) sLRStrListOfSent
    let sLRListOfClause = foldl (++) [] sLRListOfSent
    let sLRListOfTrans = foldl (++) [] sLRListOfClause

    sentList <- getSentListFromDB startSn endSn []
    autoRunGrammarAmbiResol' startSn endSn sLRListOfTrans sentList tree_target distWeiRatioList
    close conn

autoRunGrammarAmbiResol' :: Int -> Int -> SLROfClause -> [(Int, String)] -> String -> DistWeiRatioList -> IO ()
autoRunGrammarAmbiResol' sn endSn sLR sentList tree_target distWeiRatioList = do
    putStrLn $ "The sentence of serial_num = " ++ show sn ++ " begins analysing automatically for grammar ambiguity resolution. "
    let sent = snd $ sentList!!0
    cs <- getSent sent
    let sn' = sn + 1
    let sentList' = tail sentList

    parseSentByGrammarAmbiResol sn cs sLR tree_target distWeiRatioList
    putStrLn " After parseSentByGrammarAmbiResol"
    if sn' > endSn
      then putStrLn "autoRunGrammarAmbiResol: End."
      else do
      putStrLn " sn' > endSn"
      autoRunGrammarAmbiResol' sn' endSn sLR sentList' tree_target distWeiRatioList

parseSentByGrammarAmbiResol :: Int -> [String] -> SLROfClause -> String -> DistWeiRatioList -> IO ()
parseSentByGrammarAmbiResol sn cs sLR tree_target distWeiRatioList = do
    conn <- getConn
    putStrLn $ "!!! Number of SLR samples = " ++ show (length sLR)
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
        if (rn /= 0)
          then putStrLn $ "parseSentByGrammarAmbiResol: " ++ show rn ++ " row(s) were inserted."
          else error "parseSentByGrammarAmbiResol: insert failed!"
      else do
        let query' = DS.fromString ("update " ++ tree_target ++ " set tree = ?, script = ? where serial_num = ?")   -- Query is instance of IsString.
        stmt' <- prepareStmt conn query'
        ok <- executeStmt conn stmt' [toMySQLText "[]", toMySQLText "[]", toMySQLInt32 sn]
        let rn = getOkAffectedRows ok
        close conn
        if (rn /= 0)
          then putStrLn $ "parseSentByGrammarAmbiResol: " ++ show rn ++ " row(s) were initialized."
          else error "parseSentByGrammarAmbiResol: update failed!"

    struGeneSamples <- getAmbiResolSamples
    if struGeneSamples /= []
      then do
--        let struGenes = map (\x -> (snd7 x, thd7 x, fth7 x, fif7 x, sth7 x, svt7 x)) struGeneSamples
        putStrLn $ " There are " ++ show (length cs) ++ " clauses in total."
        parseSentByGrammarAmbiResol' sn cs sLR struGeneSamples distWeiRatioList
        putStrLn "parseSentByGrammarAmbiResol: Finished parsing."
      else error "parseSentByGrammarAmbiResol: struGeneSamples is Null."

parseSentByGrammarAmbiResol' :: Int -> [String] -> SLROfClause -> [StruGeneSample] -> DistWeiRatioList -> IO ()
parseSentByGrammarAmbiResol' _ [] _ _ _ = return ()
parseSentByGrammarAmbiResol' sn cs sLR struGeneSamples distWeiRatioList = do
    parseSentByGrammarAmbiResol' sn (take (length cs - 1) cs) sLR struGeneSamples distWeiRatioList

    let clauIdx = length cs
    putStrLn $ "  ===== Clause No." ++ show clauIdx ++ " ====="
    let nPCs = initPhraCate $ getNCate $ words (last cs)
    putStr "Before parsing: "
    showNPhraCateLn nPCs
    putStr "Word semantic sequence: "
    showNSeman nPCs

    rtbPCs <- parseClauseWithGrammarAmbiResol [] nPCs [] sLR (length nPCs) struGeneSamples distWeiRatioList
                                             -- Parse begins with empty '[[Rule]]' and empty 'banPCs'
    storeClauseParsingToTreebank sn clauIdx rtbPCs                      -- Add the parsing result of this clause into database.

parseClauseWithGrammarAmbiResol :: [[Rule]] -> [PhraCate] -> [PhraCate] -> SLROfClause -> Int -> [StruGeneSample] -> DistWeiRatioList -> IO ([[Rule]],[PhraCate],[PhraCate])
parseClauseWithGrammarAmbiResol rules nPCs banPCs sLR lengthOfClause struGeneSamples distWeiRatioList = do
    rtbPCs <- doTransWithGrammarAmbiResol nPCs banPCs sLR lengthOfClause struGeneSamples distWeiRatioList
                                               -- <rtbPCs> ::= ([Rule], resultant tree PCs, accumulated banned PCs)
                                               -- [Rule] is the set of rules used in this trip of transition.
    if rtbPCs == ([],[],[])
      then return ([],[],[])                   -- Return ([],[],[]) as the terminating flag.
      else if nPCs /= (snd3 rtbPCs)
        then do
          parseClauseWithGrammarAmbiResol (rules ++ [fst3 rtbPCs]) (snd3 rtbPCs) (thd3 rtbPCs) sLR lengthOfClause struGeneSamples distWeiRatioList
                                               -- Do the next trip of transition
                                               -- with appended rules, resultant PCs, and accumulated banned PCs.
        else do                                -- Phrasal closure has been formed.
          putStrLn $ "Num. of phrasal categories in closure is " ++ (show $ length nPCs)
          showNPhraCateLn (sortPhraCateBySpan nPCs)
          let spls = divPhraCateBySpan (nPCs)
          putStrLn "  ##### Parsing Tree #####"
          showTreeStru spls spls
          return (rules ++ [fst3 rtbPCs], snd3 rtbPCs, thd3 rtbPCs)

getRuleListOfMinDist :: [(Float,[Rule])] -> [Rule] -> IO ([Rule],[(Float,[Rule])])
getRuleListOfMinDist distRuleList ruleList = do
    let distRuleList' = tail distRuleList
    if fst (head distRuleList) == fst (head distRuleList')
      then do
        let ruleList' = ruleList ++ snd (head distRuleList')
        getRuleListOfMinDist distRuleList' ruleList'
      else return (ruleList,distRuleList')

doTransWithGrammarAmbiResol :: [PhraCate] -> [PhraCate] ->  SLROfClause -> Int -> [StruGeneSample] -> DistWeiRatioList -> IO ([Rule], [PhraCate], [PhraCate])
doTransWithGrammarAmbiResol nPCs banPCs sLR lengthOfClause struGeneSamples distWeiRatioList = do
    putStrLn "nPCs="
    showNPhraCateLn nPCs
    let ctpOfnPCs = ctpOfCateList nPCs []
    putStrLn "ctpOfnPCs="
    showNPhraSynLn ctpOfnPCs
    let ctpOfsLR = map (\x -> (ctpOfCateList (fst x) [], snd x)) sLR
    let distRuleListWithoutSort =  map (\x -> (distPhraSynSet' ctpOfnPCs (fst x), snd x)) ctpOfsLR
    putStrLn $ "distRuleListWithoutSort=" ++ show (take 30 distRuleListWithoutSort)
    let distRuleList = sortOn fst distRuleListWithoutSort
    putStrLn $ "distRuleList=" ++ show (take 30 distRuleList)

    let ruleList = snd (head distRuleList)
    ruleListAndDistRuleList <- getRuleListOfMinDist distRuleList ruleList
    let distRuleList' = snd ruleListAndDistRuleList
    let ruleList' = removeDup' (fst ruleListAndDistRuleList) []
    let rules = take (length ruleList') ruleList'      -- get all elems of ruleList

    putStr "Rule switches: "
    showOnOff rules                              -- Display rule switches
    let nPCs2 = trans rules nPCs banPCs          -- Without pruning, get transitive result.

    putStr "New phrases before pruning: "
--    showNPhraCateLn [pc | pc <- nPCs2, notElem' pc nPCs]
    let pcs = [pc | pc <- nPCs2, notElem' pc nPCs]
    showNPhraCateLn pcs
    let spanList = map (\x -> spOfCate x) nPCs
    if pcs == [] && (maximum spanList) /= lengthOfClause - 1
      then doTransWithGrammarAmbiResol' nPCs banPCs sLR distRuleList' lengthOfClause struGeneSamples distWeiRatioList
      else do
        let pcps = getOverlap nPCs2                    -- [(PhraCate, PhraCate)]
        let overPairs = ambiResolByGrammarAmbiResol nPCs2 [] pcps struGeneSamples distWeiRatioList    -- [OverPair], namely [(PhraCate, PhraCate, Prior)], record overlapping pairs for pruning.
        putStr "doTransWithGrammarAmbiResol: overPairs: "
        showNOverPairid overPairs

        let overPairs' = map (\x -> (fst4 x, snd4 x, thd4 x)) overPairs
        nbPCs <- transWithPruning rules nPCs banPCs overPairs'                     -- Get transitive result with pruning.

        putStr "New phrases after pruning: "
        showNPhraCateLn [pc | pc <- fst nbPCs, notElem' pc nPCs]
        putStr "Banned phrases: "
        showNPhraCateLn (snd nbPCs)                    -- The banned phrases after updated.
        return (rules,(fst nbPCs),(snd nbPCs))

{-
  当传递没有产生新短语(剪枝前)且没有形成最终的句子时，调用该函数
-}
doTransWithGrammarAmbiResol' :: [PhraCate] -> [PhraCate] -> SLROfClause -> [(Float,[Rule])] -> Int -> [StruGeneSample] -> DistWeiRatioList -> IO ([Rule], [PhraCate], [PhraCate])
doTransWithGrammarAmbiResol' nPCs banPCs sLR distRuleList lengthOfClause struGeneSamples distWeiRatioList = do
    putStrLn $ "doTransWithGrammarAmbiResol' distRuleList=" ++ show (take 30 distRuleList)

    let ruleList = snd (head distRuleList)
    ruleListAndDistRuleList <- getRuleListOfMinDist distRuleList ruleList
    let distRuleList' = snd ruleListAndDistRuleList
    let ruleList' = removeDup' (fst ruleListAndDistRuleList) []
    let rules = take (length ruleList') ruleList'      -- get all elems of ruleList

    putStr "Rule switches: "
    showOnOff rules                              -- Display rule switches
    let nPCs2 = trans rules nPCs banPCs          -- Without pruning, get transitive result.

    putStr "New phrases before pruning: "
--    showNPhraCateLn [pc | pc <- nPCs2, notElem' pc nPCs]
    let pcs = [pc | pc <- nPCs2, notElem' pc nPCs]
    showNPhraCateLn pcs
    let spanList = map (\x -> spOfCate x) nPCs
    if pcs == [] && (maximum spanList) /= lengthOfClause - 1
      then doTransWithGrammarAmbiResol' nPCs banPCs sLR distRuleList' lengthOfClause struGeneSamples distWeiRatioList
      else do
        let pcps = getOverlap nPCs2                    -- [(PhraCate, PhraCate)]
        let overPairs = ambiResolByGrammarAmbiResol nPCs2 [] pcps struGeneSamples distWeiRatioList    -- [OverPair], namely [(PhraCate, PhraCate, Prior)], record overlapping pairs for pruning.
        putStr "doTransWithGrammarAmbiResol: overPairs: "
        showNOverPairid overPairs

        let overPairs' = map (\x -> (fst4 x, snd4 x, thd4 x)) overPairs
        nbPCs <- transWithPruning rules nPCs banPCs overPairs'                     -- Get transitive result with pruning.

        putStr "New phrases after pruning: "
        showNPhraCateLn [pc | pc <- fst nbPCs, notElem' pc nPCs]
        putStr "Banned phrases: "
        showNPhraCateLn (snd nbPCs)                    -- The banned phrases after updated.
        return (rules,(fst nbPCs),(snd nbPCs))

ambiResolByGrammarAmbiResol :: [PhraCate] -> [OverPairid] -> [(PhraCate, PhraCate)] -> [StruGeneSample] -> DistWeiRatioList -> [OverPairid]
ambiResolByGrammarAmbiResol _ overPairs [] _ _ = overPairs
ambiResolByGrammarAmbiResol nPCs overPairs (pcp:pcps) struGeneSamples distWeiRatioList
    = ambiResolByGrammarAmbiResol nPCs overPairs' pcps struGeneSamples distWeiRatioList
    where
    lop = fst pcp
    rop = snd pcp
    ot = getOverType nPCs lop rop                      -- Get overlapping type
    leps = getPhraByEnd (stOfCate lop - 1) nPCs        -- Get all left-extend phrases
    reps = getPhraByStart (enOfCate rop + 1) nPCs      -- Get all right-entend phrases
    pri = Noth

    le = map ((!!0) . ctpOfCate) leps          -- [(Category,Tag,PhraStru)] of left-extended phrases
    lo = (ctpOfCate lop)!!0                    -- (Category,Tag,PhraStru) of left-overlapping phrase
    ro = (ctpOfCate rop)!!0                    -- (Category,Tag,PhraStru) of right-overlapping phrase
    re = map ((!!0) . ctpOfCate) reps          -- [(Category,Tag,PhraStru)] of right-extended phrases
                                                 -- The value is not used, just acted as place holder.
    struGene = (le,lo,ro,re,ot,pri)
    distWeiRatioList' = init distWeiRatioList ++ [0]
    distList = map (\x -> (dist4StruGeneByArithAdd struGene (snd7 x, thd7 x, fth7 x, fif7 x, sth7 x, svt7 x) distWeiRatioList', x)) struGeneSamples
    distList' = sortOn fst distList
--    minDist = minimum distList
--    idx = elemIndex minDist distList
--    idx' = case idx of
--             Just x -> x
--             Nothing -> -1                     -- Impossible position
    pcOfMinDist = snd (head distList')
    pri' = svt7 pcOfMinDist
    id = fst7 pcOfMinDist
    overPairs' = (lop, rop, pri',id):overPairs

autoRunCollectPhraSyn :: Int -> Int -> IO ()
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
    let phraSynsOfASent = ctpOfCateList phraseWithoutSpan0 []
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

autoRunAmbiResolByStruGene :: Int -> Int -> IO ()
autoRunAmbiResolByStruGene startSn endSn = do
    confInfo <- readFile "Configuration"
    let script_source = getConfProperty "script_source" confInfo
    let tree_target = getConfProperty "tree_target" confInfo
    let wle = read (getConfProperty "wle" confInfo) :: Int
    let wlo = read (getConfProperty "wlo" confInfo) :: Int
    let wro = read (getConfProperty "wro" confInfo) :: Int
    let wre = read (getConfProperty "wre" confInfo) :: Int
    let wot = read (getConfProperty "wot" confInfo) :: Int
    let wpr = read (getConfProperty "wpr" confInfo) :: Int
    let distWeiRatioList = [wle, wlo, wro, wre, wot, wpr]
{-
    putStrLn $ "The sentence of serial_num = " ++ show startSn ++ " begins analysing automatically for ambiguity resolution. "
--    getSentFromDB startSn >>= getSent >>= parseSentByStruGene startSn
    cs <- getSentFromDB startSn >>= getSent
    parseSentByStruGene startSn cs script_source tree_target distWeiRatioList
    let startSn' = startSn + 1
    if startSn' > endSn
      then putStrLn "autoRunAmbiResolByStruGene: End."
      else autoRunAmbiResolByStruGene startSn' endSn
-}
    sentList <- getSentListFromDB startSn endSn []
    autoRunAmbiResolByStruGene' startSn endSn sentList script_source tree_target distWeiRatioList

autoRunAmbiResolByStruGene' :: Int -> Int -> [(Int, String)] -> String -> String -> DistWeiRatioList -> IO ()
autoRunAmbiResolByStruGene' sn endSn sentList script_source tree_target distWeiRatioList = do
    putStrLn $ "The sentence of serial_num = " ++ show sn ++ " begins analysing automatically for grammar ambiguity resolution. "
    let sent = snd $ sentList!!0
    cs <- getSent sent
    let sn' = sn + 1
    let sentList' = tail sentList

    parseSentByStruGene sn cs script_source tree_target distWeiRatioList
    if sn' > endSn
      then putStrLn "autoRunAmbiResolByStruGene: End."
      else autoRunAmbiResolByStruGene' sn' endSn sentList' script_source tree_target distWeiRatioList

{- Re-parse a sentence, in which lexical ambiguities are resolved according the previously created parsing script,
 - and syntactic ambiguities are resolved by clustering result.
 - Here every clause is a String, and parsing starts from the first clause.
 - sn: The value of 'serial_num' in database Table 'corpus'.
 - cs: The list of clausal strings.
 -}
parseSentByStruGene :: Int -> [String] -> String -> String -> DistWeiRatioList -> IO ()
parseSentByStruGene sn cs script_source tree_target distWeiRatioList = do
    conn <- getConn
    let query = DS.fromString ("select script from " ++ script_source ++ " where serial_num = ?")       -- Query is instance of IsString.
    stmt <- prepareStmt conn query
    (defs, is) <- queryStmt conn stmt [toMySQLInt32 sn]                     --([ColumnDef], InputStream [MySQLValue])
    record <- S.read is
    let record' = case record of
                    Just x -> x
                    Nothing -> [MySQLText "[]"]
    let script = fromMySQLText (record'!!0)
    skipToEof is                                                            -- Go to the end of the stream.
    closeStmt conn stmt

    let script' = readScripts $ script
    putStr "Parsing script: "
    showScript script'

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
        if (rn /= 0)
          then putStrLn $ "parseSentByStruGene: " ++ show rn ++ " row(s) were inserted."
          else error "parseSentByStruGene: insert failed!"
      else do
        let query' = DS.fromString ("update " ++ tree_target ++ " set tree = ?, script = ? where serial_num = ?")   -- Query is instance of IsString.
        stmt' <- prepareStmt conn query'
        ok <- executeStmt conn stmt' [toMySQLText "[]", toMySQLText "[]", toMySQLInt32 sn]
        let rn = getOkAffectedRows ok
        close conn
        if (rn /= 0)
          then putStrLn $ "parseSentByStruGene: " ++ show rn ++ " row(s) were initialized."
          else error "parseSentByStruGene: update failed!"

    struGeneSamples <- getAmbiResolSamples
    if struGeneSamples /= []
      then do
        let struGenes = map (\x -> (snd7 x, thd7 x, fth7 x, fif7 x, sth7 x, svt7 x)) struGeneSamples

        putStrLn $ " There are " ++ show (length cs) ++ " clauses in total."
        parseSentByStruGene' sn cs script' struGenes distWeiRatioList
        putStrLn "parseSentByStruGene: Finished parsing."
        close conn
      else error "parseSentByStruGene: struGeneSamples is Null."

{- Re-parse a sentence, in which lexical ambiguities are resolved according the previously created parsing script,
 - and syntactic ambiguities are resolved by clustering result.
 - Here every clause is a String.
 - Parameter 'sn' is the value of 'serial_num' in database Table 'corpus'.
 - Table 'corpus', 'treebank1', and some other tables are associated with field 'serial_num'.
 - 'cs' is clausal strings to be parsed.
 - 'scripts' is parsing scripts for these clauses.
 - 'struGenes' is a list of modes or StruGene samples.
 - 'distWeiRatioList' is a list of distance weigth ratios.
 - If a certain clause is not finished in parsing, return False to skip the remaining clauses.
 -}
parseSentByStruGene' :: Int -> [String] -> [Script] -> [StruGene] -> DistWeiRatioList -> IO ()
parseSentByStruGene' _ [] _ _ _ = return ()
parseSentByStruGene' sn cs scripts struGenes distWeiRatioList = do
    parseSentByStruGene' sn (take (length cs - 1) cs) (take (length cs - 1) scripts) struGenes distWeiRatioList

    let clauIdx = length cs
    putStrLn $ "  ===== Clause No." ++ show clauIdx ++ " ====="
    let nPCs = initPhraCate $ getNCate $ words (last cs)
    putStr "Before parsing: "
    showNPhraCateLn nPCs
    putStr "Word semantic sequence: "
    showNSeman nPCs

    let lastScript = case scripts of                                        -- It's possible of no script to use.
                       [] -> (clauIdx, [], [])                              -- Null script for clause 'clauIdx'
                       [x] -> x
                       (x:xs) -> last xs
    rtbPCs <- parseClauseWithStruGene [] nPCs [] lastScript struGenes distWeiRatioList
                                             -- Parse begins with empty '[[Rule]]' and empty 'banPCs'
    storeClauseParsingToTreebank sn clauIdx rtbPCs                      -- Add the parsing result of this clause into database.

{- Parsing a clause is a recursive transition process.
 - Input: A sequence of [Rule], a sequence of phrasal categories, a sequence of banned phrasal categories, a parsing script of this clause,
          a sequence of modes and a sequence of distance weigths.
 - Algo.:
 - (1) Do one trip of transition;
 - (2) If creating new phrasal categories, append rules used in this trip to [[Rule]], take resultant phrases and accumulated banned
 -     phrases as input, go (1); Otherwise, return the triple ([[Rule]], resultant tree PCs, accumulated banned PCs).
 - Syntax ambiguity resolution is done by machine.
 -}
parseClauseWithStruGene :: [[Rule]] -> [PhraCate] -> [PhraCate] -> Script -> [StruGene] -> DistWeiRatioList -> IO ([[Rule]],[PhraCate],[PhraCate])
parseClauseWithStruGene rules nPCs banPCs script struGenes distWeiRatioList = do
    rtbPCs <- doTransWithStruGene nPCs banPCs script struGenes distWeiRatioList
                                               -- <rtbPCs> ::= ([Rule], resultant tree PCs, accumulated banned PCs)
                                               -- [Rule] is the set of rules used in this trip of transition.
    if rtbPCs == ([],[],[])
      then return ([],[],[])                   -- Return ([],[],[]) as the terminating flag.
      else if nPCs /= (snd3 rtbPCs)
        then do
          let scriptTail = case script of
                              (clauIdx, [], bPCs) -> (clauIdx, [], bPCs)        -- Null script
                              (clauIdx, [x], bPCs) -> (clauIdx, [], bPCs)       -- Remove the head element of OnOff list in parsing script.
                              (clauIdx, (x:xs), bPCs) -> (clauIdx, xs, bPCs)

          parseClauseWithStruGene (rules ++ [fst3 rtbPCs]) (snd3 rtbPCs) (thd3 rtbPCs) scriptTail struGenes distWeiRatioList
                                               -- Do the next trip of transition
                                               -- with appended rules, resultant PCs, and accumulated banned PCs.
        else do                                -- Phrasal closure has been formed.
          putStrLn $ "Num. of phrasal categories in closure is " ++ (show $ length nPCs)
          showNPhraCateLn (sortPhraCateBySpan nPCs)
          let spls = divPhraCateBySpan (nPCs)
          putStrLn "  ##### Parsing Tree #####"
          showTreeStru spls spls
          return (rules ++ [fst3 rtbPCs], snd3 rtbPCs, thd3 rtbPCs)

{- Do a trip of transition, and return the category-converted rules used in this trip, the resultant phrases, and the banned phrases.
 - nPCs: The current phrase set
 - banPCs: The set of banned phrases
 - script: The parsing script of this clause
 - struGenes: The list of modes or StruGene values
 - distWeiRatioList: The distance weigth ratio list [wle, wlo, wro, wre, wot, wpr]
 - onOff: The category conversion list for this transition
 - nbPCs: The tuple (phrase set, banned set) after this transition
 - (onOff,(fst nbPCs),(snd nbPCs)): The returned overlap phrase pair set
 -}
doTransWithStruGene :: [PhraCate] -> [PhraCate] -> Script -> [StruGene] -> DistWeiRatioList -> IO ([Rule], [PhraCate], [PhraCate])
doTransWithStruGene nPCs banPCs script struGenes distWeiRatioList = do
    let onOffs = snd3 script
    let onOff = case onOffs of                      -- Get rule switches of this trip of transition
                      [] -> [] :: OnOff             -- No script of rule switches to use
                      (x:_) -> x :: OnOff           -- There is a script of rule switches to use

    putStr "Rule switches: "
    showOnOff onOff                              -- Display rule switches
    let nPCs2 = trans onOff nPCs banPCs          -- Without pruning, get transitive result.

    putStr "New phrases before pruning: "
    showNPhraCateLn [pc | pc <- nPCs2, notElem' pc nPCs]

    let pcps = getOverlap nPCs2                    -- [(PhraCate, PhraCate)]
    let overPairs = ambiResolByStruGene nPCs2 [] pcps struGenes distWeiRatioList    -- [OverPair], namely [(PhraCate, PhraCate, Prior)], record overlapping pairs for pruning.
    putStr "doTransWithStruGene: overPairs: "
    showNOverPair overPairs

    nbPCs <- transWithPruning onOff nPCs banPCs overPairs                     -- Get transitive result with pruning.

    putStr "New phrases after pruning: "
    showNPhraCateLn [pc | pc <- fst nbPCs, notElem' pc nPCs]
    putStr "Banned phrases: "
    showNPhraCateLn (snd nbPCs)                    -- The banned phrases after updated.

    return (onOff,(fst nbPCs),(snd nbPCs))

{- Resolve ambiguities by StruGene samples.
 - nPCs: The current phrase set
 - overPairs: The overlap phrase pair set
 - (pcp:pcps): The overlap phrase pair set
 - struGenes: The list of modes or StruGene values
 - distWeiRatioList: The distance weigth ratio list [wle, wlo, wro, wre, wot, wpr]
 - overPairs: The returned overlap phrase pair set
 - Algo.:
 -   (1) get the first overlap phrase pair, create the ambiguous context for the phrasal pair,
 -       that is, a StruGene value in which component 'prior' is invalid. The StruGene value is noted 'sgv';
 -   (2) find the clustering mode closest to this the ambiguous context, set the resolution policy
 -       of 'sgv' as that of the clustering mode;
 -   (3) if there remain overlap phrase pairs, go (1); otherwise, return phrase pairs with their resolution policies.
 -}
ambiResolByStruGene :: [PhraCate] -> [OverPair] -> [(PhraCate, PhraCate)] -> [StruGene] -> DistWeiRatioList -> [OverPair]
ambiResolByStruGene _ overPairs [] _ _ = overPairs
ambiResolByStruGene nPCs overPairs (pcp:pcps) struGenes distWeiRatioList
    = ambiResolByStruGene nPCs overPairs' pcps struGenes distWeiRatioList
    where
    lop = fst pcp
    rop = snd pcp
    ot = getOverType nPCs lop rop                      -- Get overlapping type
    leps = getPhraByEnd (stOfCate lop - 1) nPCs        -- Get all left-extend phrases
    reps = getPhraByStart (enOfCate rop + 1) nPCs      -- Get all right-entend phrases
    pri = Noth

    le = map ((!!0) . ctpOfCate) leps          -- [(Category,Tag,PhraStru)] of left-extended phrases
    lo = (ctpOfCate lop)!!0                    -- (Category,Tag,PhraStru) of left-overlapping phrase
    ro = (ctpOfCate rop)!!0                    -- (Category,Tag,PhraStru) of right-overlapping phrase
    re = map ((!!0) . ctpOfCate) reps          -- [(Category,Tag,PhraStru)] of right-extended phrases
                                                 -- The value is not used, just acted as place holder.
    struGene = (le,lo,ro,re,ot,pri)
    distWeiRatioList' = init distWeiRatioList ++ [0]
    distList = map (\x -> dist4StruGeneByArithAdd struGene x distWeiRatioList') struGenes
    minDist = minimum distList
    idx = elemIndex minDist distList
    idx' = case idx of
             Just x -> x
             Nothing -> -1                     -- Impossible position
    pri' = sth6 (struGenes!!idx')
    overPairs' = (lop, rop, pri'):overPairs

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
        let le = map ((!!0) . ctpOfCate) leps
        let lo = (ctpOfCate leftPhrase)!!0
        let ro = (ctpOfCate rightPhrase)!!0
        let re = map ((!!0) . ctpOfCate) reps
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

getAccuracyOfAmbiResol :: IO ()
getAccuracyOfAmbiResol = do
    conn <- getConn
    confInfo <- readFile "Configuration"                                        -- Read the local configuration file
    let accurate_tree_source = getConfProperty "tree_source" confInfo
    let ambi_resol_result_tree_source = getConfProperty "ambi_resol_result_tree_source" confInfo
    let query = DS.fromString ("select serial_num, tree, script from " ++ accurate_tree_source )       -- Query is instance of IsString.
    stmt <- prepareStmt conn query
    (defs, is) <- queryStmt conn stmt []
    accurateTreebank <- readStreamByInt32TextText [] is

    let query = DS.fromString ("select serial_num, tree, script from " ++ ambi_resol_result_tree_source )       -- Query is instance of IsString.
    stmt <- prepareStmt conn query
    (defs, is) <- queryStmt conn stmt []
    ambiResolTreebank <- readStreamByInt32TextText [] is
    let finalSn = fst3 (last ambiResolTreebank)
    putStrLn $ "finalSn = " ++ show finalSn

    finalMachAmbiResolRes <- findMachAmbiResolRes 1 1 accurateTreebank ambiResolTreebank ((0, 0), (0, 0), (0, 0, 0, 0))
--    let accuracy = fromIntegral ((fst . thd3) finalMachAmbiResolRes) / fromIntegral ((snd . thd3) finalMachAmbiResolRes)
    putStrLn $ "finalMachAmbiResolRes = " ++ show finalMachAmbiResolRes
--    putStrLn $ "Accuracy = " ++ show accuracy

type NumOfManPositPhrase = Int
type NumOfManNegPhrase = Int
type NumOfMachPositPhrase = Int
type NumOfMachNegPhrase = Int
type NumOfCommonPositPhrase = Int
type NumOfCommonNegPhrase = Int
type NumOfCommonPhraseOnlySeman = Int
type NumOfCommonResClause = Int
--type NumOfPositUnionPhrase = Int
type MachAmbiResolRes = ((NumOfManPositPhrase, NumOfManNegPhrase), (NumOfMachPositPhrase, NumOfMachNegPhrase), (NumOfCommonPositPhrase, NumOfCommonNegPhrase,NumOfCommonPhraseOnlySeman,NumOfCommonResClause))

findMachAmbiResolRes :: Int -> Int -> [(Int, String, String)] -> [(Int, String, String)] -> MachAmbiResolRes -> IO (MachAmbiResolRes)
findMachAmbiResolRes startSn endSn accurateTreebank ambiResolTreebank origMachAmbiResolRes = do
    let index = startSn - 1
    let treeOfStartSn = stringToList $ snd3 (accurateTreebank!!index)   -- [clausestr]
    let treeOfStartSn' = stringToList $ snd3 (ambiResolTreebank!!index)
    let scriptOfStartSn = stringToList $ thd3 (accurateTreebank!!index)
    let scriptOfStartSn' = stringToList $ thd3 (ambiResolTreebank!!index)

    putStrLn $ "Sentence No.:" ++ show startSn
    currentMachAmbiResolRes <- findMachAmbiResolRes' 1 treeOfStartSn treeOfStartSn' scriptOfStartSn scriptOfStartSn' origMachAmbiResolRes
    let startSn' = startSn + 1
    if startSn' <= endSn
      then findMachAmbiResolRes startSn' endSn accurateTreebank ambiResolTreebank currentMachAmbiResolRes
      else return currentMachAmbiResolRes

findMachAmbiResolRes' :: Int -> [String] -> [String] -> [String] -> [String] -> MachAmbiResolRes -> IO (MachAmbiResolRes)
findMachAmbiResolRes' clauseNo (s:cs) (s':cs') script script' origMachAmbiResolRes = do
    let index = clauseNo - 1
    let pcs = getClauPhraCate s
    let pcs' = getClauPhraCate s'
    let phraseOfSpan0 = getPhraBySpan 0 pcs
    let phraseOfSpan0' = getPhraBySpan 0 pcs'
    let phraseWithoutSpan0 = [x| x <- pcs, notElem x phraseOfSpan0]    -- not includes word, only phrase with at least two words
    let phraseWithoutSpan0' = [x| x <- pcs', notElem x phraseOfSpan0']
    let npcWithoutAct = nPhraCateWithoutAct phraseWithoutSpan0 []
    let npcWithoutAct' = nPhraCateWithoutAct phraseWithoutSpan0' []
    let npcOnlyAct = nseOfCate phraseWithoutSpan0 []
    let npcOnlyAct' = nseOfCate phraseWithoutSpan0' []
    let commonPositPhrase = [x| x <- phraseWithoutSpan0', elem (phraCateWithoutAct x) npcWithoutAct]
    let banPCs = getClauBanPCs (script!!index)
    let banPCs' = getClauBanPCs (script'!!index)
    let banPCsWithoutAct = nPhraCateWithoutAct banPCs []
    let banPCsWithoutAct' = nPhraCateWithoutAct banPCs' []
    let commonNegPhrase = [x| x <- banPCs', elem (phraCateWithoutAct x) banPCsWithoutAct]
--    let positUnionPhrase = [x| x <- phraseWithoutSpan0 ++ phraseWithoutSpan0', ]
--    let positUnionPhraseWithoutAct = union npcWithoutAct npcWithoutAct'
    let commonPhraseOnlySeman = [x| x <- npcOnlyAct, elem x npcOnlyAct']
    let phraseWithLongestSp = last pcs
    let commonResClause = case elem phraseWithLongestSp phraseWithoutSpan0' of   -- 若小句只要一个词，想要得到0，phraseWithoutSpan0'代替pcs',可以实现 即elem phraseWithLongestSp [] = False
                               True -> 1                                         -- 注意按小句算accuracy时，要减去只要一个词的小句。
                               False -> 0

    let numOfClausePhrase = length phraseWithoutSpan0
    let banPCs = getClauBanPCs (script!!index)
    let numOfBanPCs = length $ getClauBanPCs (script!!index)
    let numOfClausePhrase' = length phraseWithoutSpan0'
    let banPCs' = getClauBanPCs (script'!!index)
    let numOfBanPCs' = length $ getClauBanPCs (script'!!index)

    let newNumOfPhrase = (fst . fst3) origMachAmbiResolRes + numOfClausePhrase
    let newNumOfBanPCs = (snd . fst3) origMachAmbiResolRes + numOfBanPCs
    let newNumOfPhrase' = (fst . snd3) origMachAmbiResolRes + numOfClausePhrase'
    let newNumOfBanPCs' = (snd . snd3) origMachAmbiResolRes + numOfBanPCs'
    let newNumOfCommonPositPhrase = (fst4 . thd3) origMachAmbiResolRes + length commonPositPhrase
    let newNumOfCommonNegPhrase = (snd4 . thd3) origMachAmbiResolRes + length commonNegPhrase
    let newNumOfCommonPhraseOnlySeman = (thd4 . thd3) origMachAmbiResolRes + length commonPhraseOnlySeman
    let newNumOfCommonResClause = (fth4 . thd3) origMachAmbiResolRes + commonResClause

--    let newNumOfPositUnionPhrase = (snd . thd3) origMachAmbiResolRes + (numOfClausePhrase + numOfClausePhrase' - length commonPositPhrase)
    let clauseResMark = ((numOfClausePhrase, numOfBanPCs), (numOfClausePhrase', numOfBanPCs'), (length commonPositPhrase, length commonNegPhrase, length commonPhraseOnlySeman,commonResClause))
    let newAmbiResolResMark = ((newNumOfPhrase, newNumOfBanPCs), (newNumOfPhrase', newNumOfBanPCs'), (newNumOfCommonPositPhrase, newNumOfCommonNegPhrase, newNumOfCommonPhraseOnlySeman,newNumOfCommonResClause))

    let spls = divPhraCateBySpan pcs       -- Span lines
    let spls' = divPhraCateBySpan pcs'       -- Span lines
    putStrLn $ "Clause No.: " ++ show clauseNo
    putStrLn $ "Accuracy clause analysis tree is:"
    showTreeStru spls spls
    putStrLn $ "banPCs = "
    showNPhraCateLn banPCs
    putStrLn $ "The clause analysis tree obtained by disambiguation is:"
    showTreeStru spls' spls'
    putStrLn $ "banPCs' = "
    showNPhraCateLn banPCs'
    putStrLn $ "commonPhraseOnlySeman=" ++ show commonPhraseOnlySeman
    putStrLn $ "phraseWithLongestSp = "
    showPhraCate phraseWithLongestSp
    putStrLn $ "The ambiguity resolution successful phrases without span = 0 are:"
    showNPhraCateLn commonPositPhrase
    showNPhraCateLn commonNegPhrase
    putStrLn $ "clauseResMark = " ++ show clauseResMark
    putStrLn $ "ambiResolResMark = " ++ show newAmbiResolResMark
    putStr "\n"

    if cs /= []
      then findMachAmbiResolRes' (clauseNo + 1) cs cs' script script' newAmbiResolResMark
      else return newAmbiResolResMark

{- Do a trip of transition, insert or update related ambiguity resolution samples in Table <ambi_resol_model>, and return the category-converted
 - rules used in this trip, the resultant phrases, and the banned phrases.
 - If transitive parsing is to terminated, namely selecting 'e' at inquiring rule switches, returnes ([],[],[]) as the terminating flag.
 -}
doTransWithManualResol :: [Rule] -> [PhraCate] -> [PhraCate] -> IO ([Rule], [PhraCate], [PhraCate])
doTransWithManualResol onOff nPCs banPCs = do
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
             doTransWithManualResol newOnOff nPCs banPCs                        -- Redo this trip of transition by modifying rule switches.
           else do
             putStrLn "Rule switch expression error. Consider again!"
             doTransWithManualResol onOff nPCs banPCs
      else if ruleSwitchOk == "y" || ruleSwitchOk == ""     -- Press key 'y' or directly press RETURN
             then do
               let nPCs2 = trans onOff nPCs banPCs          -- Without pruning, get transitive result.
--               putStr "Transitive result before pruning: "
--               showNPhraCateLn (sortPhraCateBySpan nPCs2)
               putStr "New phrases before pruning: "
               showNPhraCateLn [pc | pc <- nPCs2, notElem' pc nPCs]
--               putStr "Banned phrases: "
--               showNPhraCateLn (banPCs)                   -- Can't use <sortPhraCateBySpan> on <banPCs>.

               let pcps = getOverlap nPCs2                  -- [(PhraCate, PhraCate)]
               overPairs <- ambiResolByManualResol nPCs2 [] pcps                -- [OverPair], record overlapping pairs for pruning.
               nbPCs <- transWithPruning onOff nPCs banPCs overPairs            -- Get transitive result with pruning.
--               putStr "Transitive result after pruning: "
--               showNPhraCateLn (sortPhraCateBySpan (fst nbPCs))
               putStr "New phrases after pruning: "
               showNPhraCateLn [pc | pc <- fst nbPCs, notElem' pc nPCs]
               putStr "Banned phrases: "
               showNPhraCateLn (snd nbPCs)                  -- The banned phrases after updated.

               transOk <- getLineUntil "This trip of transition is ok? [y/n/e]: (RETURN for 'y')" ["y","n","e"] True  -- Get user decision of whether to do next transition
               if transOk == "y" || transOk == ""           -- Press key 'y' or directly press RETURN
                 then do
                     updateAmbiResol (fst nbPCs) overPairs                      -- Record ambiguity resolution fragments.
                     return (onOff,(fst nbPCs),(snd nbPCs))
                 else if transOk == "n"
                        then doTransWithManualResol onOff nPCs banPCs           -- Redo this trip of transition.
                        else if transOk == "e"
                               then return ([],[],[])       -- Return from doTrans, and indicate this is terminating exit.
                               else error "doTransWithManualResol: Impossible input error!"
             else if ruleSwitchOk == "e"
                    then return ([],[],[])                  -- Return from doTrans, and indicate this is terminating exit.
                    else error "doTransWithManualResol: Impossible input error!"

{- Mannually resolve a set of overlapping phrases, and return the list of OverPair(s).
 -}
ambiResolByManualResol :: [PhraCate] -> [OverPair] -> [(PhraCate, PhraCate)] -> IO [OverPair]
ambiResolByManualResol _ overPairs [] = return overPairs
ambiResolByManualResol nPCs overPairs (pcp:pcps) = do
    op <- ambiResolByManualResol' nPCs pcp
    ambiResolByManualResol nPCs (op:overPairs) pcps

{- Mannually resolve a pair of overlapping phrases, and return a OverPair.
 -}
ambiResolByManualResol' :: [PhraCate] -> (PhraCate, PhraCate) -> IO OverPair
ambiResolByManualResol' nPCs (lp, rp) = do
    confInfo <- readFile "Configuration"                                        -- Read the local configuration file
    let ambi_resol_model = getConfProperty "ambi_resol_model" confInfo
    let ot = getOverType nPCs lp rp                        -- Get overlapping type
    case ambi_resol_model of
      "ambi_resol1" -> do
        let context = [x | x <- nPCs, x /= lp, x /= rp]        -- Get context of the pair of overlapping phrases
        putStr "Find a fragment of No.1 ambiguity model: "
        showAmbiModel1Frag lp rp context ot
      "stru_gene" -> do
        let leps = getPhraByEnd (stOfCate lp - 1) nPCs        -- Get all left-extend phrases
        let reps = getPhraByStart (enOfCate rp + 1) nPCs      -- Get all right-entend phrases
        putStr "Find structural fragment: "
        showStruFrag leps lp rp reps ot
      _ -> error "ambiResolByManualResol': ambi_resol_model is set wrongly."

    prior <- getLineUntil "Please input priority [Lp/Rp/Noth]: (RETURN for 'Lp') " ["Lp","Rp","Noth"] True
    if (prior == "" || prior == "Lp")                  -- Set prior as "Lp".
      then return (lp, rp, (read "Lp"::Prior))
      else if prior == "Rp"                            -- Set prior as "Rp".
        then return (lp, rp, (read "Rp"::Prior))
        else if prior == "Noth"
           then return (lp, rp, (read "Noth"::Prior))
           else error "ambiResolByManualResol': Impossible input!"

{- Insert new or update old ambiguity resolution fragments in ambiguity resolution database.
 - The input phrase set <nPCs> is used to create ambiguity resolution context for every overlapping phrases.
 -}
updateAmbiResol :: [PhraCate] -> [OverPair] -> IO ()
updateAmbiResol _ [] = do
    putStrLn "updateAmbiResol: Update finshed."              -- To make output easy to read.
updateAmbiResol nPCs (op:ops) = do
    updateAmbiResol' nPCs op
    updateAmbiResol nPCs ops

{- Insert or update one ambiguity resolution fragment in MySQL tables storing ambiguity resolution samples.
 - Ambiguity resolution model also is the name of MySQL table storing the samples of that model.
 - To now, models include 'stru_gene' and 'ambi_resol1'.
 -}
updateAmbiResol' :: [PhraCate] -> OverPair -> IO ()
updateAmbiResol' nPCs overPair = do
    confInfo <- readFile "Configuration"                                        -- Read the local configuration file
    let ambi_resol_model = getConfProperty "ambi_resol_model" confInfo

    let lp = fst3 overPair                                   -- Get left overlapping phrase.
    let rp = snd3 overPair                                   -- Get right overlapping phrase.
    let ot = getOverType nPCs lp rp                          -- Get overlapping type
    let prior = thd3 overPair                                -- Get prior selection of the two overlapping phrases.

    let lpv = replace "'" "''" $ doubleBackSlash (getPhraCate_String lp)           -- Get values to insert them into MySql Table
    let rpv = replace "'" "''" $ doubleBackSlash (getPhraCate_String rp)
    let otv = show ot
    let priorv = show prior

    conn <- getConn

    case ambi_resol_model of
      "stru_gene" -> do
        let le = getPhraByEnd (stOfCate lp - 1) nPCs        -- Get all left-extend phrases
        let re = getPhraByStart (enOfCate rp + 1) nPCs      -- Get all right-entend phrases

        putStr "Find structural fragment: "
        showStruFrag le lp rp re ot

        let lev = doubleBackSlash (show le)                 -- Get values to insert them into MySql Table
        let lpv = doubleBackSlash (show lp)
        let rpv = doubleBackSlash (show rp)
        let rev = doubleBackSlash (show re)
        let otv = show ot

        putStrLn $ "Inquire structural gene: leftExtend = '" ++ show le ++ "' && " ++
                                              "leftOver = '" ++ show lp ++ "' && " ++
                                             "rightOver = '" ++ show rp ++ "' && " ++
                                           "rightExtend = '" ++ show re ++ "' && " ++
                                              "overType = "  ++ show ot
        conn <- getConn
        let sqlstat = read (show ("select id, prior, hitCount, priorExCount from stru_gene where leftExtend = '" ++ lev ++ "' && " ++ "leftOver = '" ++ lpv ++ "' && " ++ "rightOver = '" ++ rpv ++ "' && " ++ "rightExtend = '" ++ rev ++ "' && " ++ "overType = " ++ otv)) :: Query
        stmt <- prepareStmt conn sqlstat
        (defs, is) <- queryStmt conn stmt []

        rows <- S.toList is
        if rows /= []
          then
            if length rows > 1
              then do
                close conn                           -- Close MySQL connection.
                error "updateAmbiResol': Find duplicate structural genes."
              else do
                let id = fromMySQLInt32U ((rows!!0)!!0)
                let prior = fromMySQLText ((rows!!0)!!1)
                let hitCount = fromMySQLInt32U ((rows!!0)!!2)
                let priorExCount = fromMySQLInt16U ((rows!!0)!!3)
                putStrLn $ "updateAmbiResol': (" ++ show id ++ ") prior: " ++ prior ++ ", hitCount: " ++ show hitCount ++ ", priorExCount: " ++ show priorExCount
                putStr "Is the priority right? [y/n]: (RETURN for 'y') "
                input <- getLine
                if input == "y" || input == ""       -- Press key 'y' or directly press RETURN.
                  then do
                    resetStmt conn stmt
                    let sqlstat = read (show ("update stru_gene set hitCount = ? where id = '" ++ show id ++ "'")) :: Query
                    stmt <- prepareStmt conn sqlstat
                    executeStmt conn stmt [toMySQLInt32U (hitCount + 1)]            -- Add column 'hitCount' by 1 of structural gene.
                    close conn                           -- Close MySQL connection.
                  else do
                    putStr "please input new priority [Lp/Rp]: (RETURN for 'Lp') "
                    newPrior <- getLine
                    if (newPrior == "Lp" || newPrior == "Rp") && newPrior /= prior                   -- Ask to modify column 'prior'.
                      then do
                        resetStmt conn stmt
                        let sqlstat = read (show ("update stru_gene set prior = ?, hitCount = ?, priorExCount = ? where id = '" ++ show id ++ "'")) :: Query
                        stmt <- prepareStmt conn sqlstat
                        executeStmt conn stmt [toMySQLText newPrior, toMySQLInt32U 0, toMySQLInt16U (priorExCount + 1)]
                                                                                  -- Update columns 'prior', 'hitCount', and 'priorExCount' of structural gene.
                        close conn                           -- Close MySQL connection.
                      else if newPrior == prior || (newPrior == "" && prior == "Lp")            -- Actually, the priority is not asked to change.
                             then do
                               resetStmt conn stmt
                               let sqlstat = read (show ("update stru_gene set hitCount = ? where id = '" ++ show id ++ "'")) :: Query
                               stmt <- prepareStmt conn sqlstat
                               executeStmt conn stmt [toMySQLInt32U (hitCount + 1)]       -- Add column 'hitCount' by 1 of structural gene.
                               close conn                           -- Close MySQL connection.
                             else if newPrior == "" && prior /= "Lp"                      -- The priority changes from 'Rp' to 'Lp' when pressing Key RETURN.
                                    then do
                                      resetStmt conn stmt
                                      let sqlstat = read (show ("update stru_gene set prior = ?, hitCount = ?, priorExCount = ? where id = '" ++ show id ++ "'")) :: Query
                                      stmt <- prepareStmt conn sqlstat
                                      executeStmt conn stmt [toMySQLText "Lp", toMySQLInt32U 0, toMySQLInt16U (priorExCount + 1)]
                                                                                -- Update 'prior', 'hitCount', and 'priorExCount' of the gene.
                                      close conn                                -- Close MySQL connection.
                                    else do
                                      putStrLn "updateAmbiResol': Illegal priority"
                                      updateAmbiResol' nPCs overPair            -- Calling the function itself again.
        else do
          putStr "Inquire failed, skip? [y/n]: (RETURN for 'y') "
          input <- getLine
          if input == "y" || input == ""              -- Press key 'y' or directly press RETURN.
            then
              updateAmbiResol' nPCs overPair          -- To now, don't allow skipping.
            else do
              putStr "please input new priority [Lp/Rp]: (RETURN for 'Lp') "
              newPrior <- getLine
              if newPrior == "Lp" || newPrior == "Rp"
                then do
                  let sqlstat = read (show ("insert stru_gene (leftExtend,leftOver,rightOver,rightExtend,overType,prior) values ('" ++ lev ++ "','" ++ lpv ++ "','" ++ rpv ++ "','" ++ rev ++ "'," ++ otv ++ ",'" ++ newPrior ++ "')")) :: Query
                  stmt1 <- prepareStmt conn sqlstat
                  oks <- executeStmt conn stmt1 []             -- Insert the described structural gene.
                  putStrLn $ "updateAmbiResol': Last inserted row with ID " ++ show (getOkLastInsertID oks)
                else if newPrior == ""
                  then do
                    let sqlstat = read (show ("insert stru_gene (leftExtend,leftOver,rightOver,rightExtend,overType,prior) values ('" ++ lev ++ "','" ++ lpv ++ "','" ++ rpv ++ "','" ++ rev ++ "'," ++ otv ++ ",'Lp')")) :: Query
                    stmt2 <- prepareStmt conn sqlstat
                    oks <- executeStmt conn stmt2 []           -- Insert the described structural gene.
                    putStrLn $ "updateAmbiResol': Last inserted row with ID " ++ show (getOkLastInsertID oks)
                  else do
                    putStrLn "updateAmbiResol': Illegal priority"
                    updateAmbiResol' nPCs overPair             -- Calling the function itself again.
        close conn                       -- Close MySQL connection.
      "ambi_resol1" -> do
        let context =  sortPhraCateBySpan [x | x <- nPCs, x /= lp, x /= rp]        -- Get context for ambiguity resolution, which is sorted by increasing phrasal spans.

        putStr "updateAmbiResol': Inquire ambiguity fragment: leftPhrase = "
        showPhraCate lp
        putStr ", rightPhrase = "
        showPhraCate rp
--        putStr ", context = "
--        showNPhraCate context
--        putStr ", overType = "
--        putStr $ show ot
        putStrLn ""

        let contextv = replace "'" "''" $ doubleBackSlash (getNPhraCate_String context)

--        let sqlstat = read (show ("select id, prior from ambi_resol1 where leftPhrase = _utf8mb4'" ++ lpv ++ "' && " ++ "rightPhrase = _utf8mb4'" ++ rpv ++ "' && " ++ "context = _utf8mb4'" ++ contextv ++ "' && " ++ "overType = '" ++ otv ++ "'")) :: Query
        let sqlstat = DS.fromString $ "select id, prior from ambi_resol1 where leftPhrase = '" ++ lpv ++ "' && " ++ "rightPhrase = '" ++ rpv ++ "' && " ++ "context = '" ++ contextv ++ "' && " ++ "overType = '" ++ otv ++ "'"
        stmt <- prepareStmt conn sqlstat
        (defs, is) <- queryStmt conn stmt []

        rows <- S.toList is
        if rows /= []
          then
            if length rows > 1
              then do
                close conn                               -- Close MySQL connection.
                error "updateAmbiResol': Find duplicate ambiguity fragments, which is impossible."
              else do
                let id = fromMySQLInt32U ((rows!!0)!!0)
                let priorOrig = fromMySQLText ((rows!!0)!!1)
                putStr $ "updateAmbiResol': (" ++ show id ++ ") original prior: " ++ priorOrig ++ ", new prior: " ++ priorv
                if priorOrig /= priorv
                  then do
                    resetStmt conn stmt
                    let sqlstat = DS.fromString $ "update ambi_resol1 set prior = ? where id = " ++ show id
                    stmt <- prepareStmt conn sqlstat
                    executeStmt conn stmt [toMySQLText priorv]                      -- Add column 'hitCount' by 1.
                    putStrLn ", modification is done."
                  else putStrLn ", no modification is to do."
          else do
            putStr "Inquire failed. Insert the ambiguity resolution fragment ..."
--            let sqlstat = read (show ("insert ambi_resol1 (leftPhrase, rightPhrase, context, overType, prior) values (_utf8mb4'" ++ lpv ++ "', _utf8mb4'" ++ rpv ++ "', _utf8mb4'" ++ contextv ++ "'," ++ otv ++ ",'" ++ priorv ++ "')")) :: Query
            let sqlstat = DS.fromString $ "insert ambi_resol1 (leftPhrase, rightPhrase, context, overType, prior) values ('" ++ lpv ++ "', '" ++ rpv ++ "', '" ++ contextv ++ "'," ++ otv ++ ",'" ++ priorv ++ "')"
            stmt1 <- prepareStmt conn sqlstat
            oks <- executeStmt conn stmt1 []             -- Insert the described structural gene.
            putStrLn $ " [OK], and its id is " ++ show (getOkLastInsertID oks)
        close conn                                       -- Close MySQL connection.

{- Add the parsing result of a clause into treebank designated by <Configuration>.
 - Now, parameter <clauIdx> has not been used for checking.
 -}
storeClauseParsingToTreebank :: Int -> Int -> ([[Rule]], [PhraCate], [PhraCate]) -> IO ()
storeClauseParsingToTreebank sn clauIdx rtbPCs = do
    confInfo <- readFile "Configuration"
    let tree_target = getConfProperty "tree_target" confInfo

    conn <- getConn
    let query = DS.fromString ("select tree, script from " ++ tree_target ++ " where serial_num = ?")       -- Query is instance of IsString.
    stmt <- prepareStmt conn query
    (defs, is) <- queryStmt conn stmt [toMySQLInt32 sn]
    rows <- S.toList is                                                         -- [[MySQLText, MySQLText]]
--    closeStmt conn stmt

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
        close conn
        if (rn /= 0)
          then putStrLn $ "storeClauseParsingToTreebank: " ++ show rn ++ " row(s) were modified."
          else error "storeClauseParsingToTreebank: update failed!"

{- Store the parsing tree of a clause into database. Actually, here is an append operation.
   The function is obsoleted, and replaced with Function storeClauseParsing.
 -}
storeTree :: Int -> String -> IO ()
storeTree sn treeStr = do
    confInfo <- readFile "Configuration"               -- Read the local configuration file
    let tree_source = getConfProperty "ambi_resol_result_tree_source" confInfo
    origTree <- readTree_String sn tree_source
    let newTree = origTree ++ ";" ++ treeStr                 -- Use ';' to seperate clause trees
    conn <- getConn
    stmt <- prepareStmt conn "update corpus set tree = ? where serial_num = ?"
    ok <- executeStmt conn stmt [toMySQLText newTree, toMySQLInt32 sn]
    close conn
    if (getOkAffectedRows ok == 1)
      then putStrLn "storeTree: succeeded."
      else error "storeTree: failed!"

{- Read the tree String of designated sentence in treebank.
 - There are several treebanks stored in database, their table names are corpus, treebank1, and so on.
 -}
readTree_String :: Int -> String -> IO String
readTree_String sn tree_source = do
--    dfe <- doesFileExist "Configuration"
--    if dfe
--      then putStrLn $ "readTree_String: Configuration exists."
--      else putStrLn $ "readTree_String: Configuration does not exist."
--    confInfo <- readFile "Configuration"               -- Read the local configuration file
--    let tree_source = getConfProperty "tree_source" confInfo

    conn <- getConn
    let sqlstat = DS.fromString $ "select tree from " ++ tree_source ++ " where serial_num = ?"
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
    let pcs = getClauPhraCate (snd clauTreeStr)
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
dispTree' :: Int -> [String] -> IO ()
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

-- Get a clause's [PhraCate] from its string value.
getClauPhraCate :: String -> [PhraCate]
getClauPhraCate "" = []
getClauPhraCate str = map getPhraCateFromString (stringToList' str)

-- Get a clause's [PhraCate] from string value of (clauIdx, [[Rule]], [PhraCate]).
getClauBanPCs :: String -> [PhraCate]
getClauBanPCs "" = []
getClauBanPCs str = map getPhraCateFromString $ stringToList $ thd3 (stringToTriple str)

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
    if ([pc| pc <- nPCs, notElem' pc nPCs2] /= [])||([pc| pc <- nPCs2, notElem' pc nPCs] /= [])
--  if (equalSortedPhraList (quickSort nPCs) (quickSort nPCs2))
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
