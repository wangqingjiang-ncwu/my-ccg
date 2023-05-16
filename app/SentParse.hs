{-# LANGUAGE OverloadedStrings, LambdaCase #-}

-- Copyright (c) 2019-2023 China University of Water Resources and Electric Power,
-- All rights reserved.

module SentParse (
    getSentFromDB,        -- Int -> IO String
    getSent,              -- String -> IO [String]
    parseSent,            -- Int -> Int -> [String] -> IO Bool
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
    parseSentByScript,    -- Int -> [String] -> IO ()
    parseSentByScript',   -- Int -> [String] -> IO Bool
    parseClauseWithScript,            -- [[Rule]] -> [PhraCate] -> [PhraCate] -> Script -> IO ([[Rule]],[PhraCate],[PhraCate])
    doTransWithScript,    -- [Rule] -> [PhraCate] -> [PhraCate] -> Script -> IO ([Rule], [PhraCate], [PhraCate])
    doTransWithManualResol,           -- [Rule] -> [PhraCate] -> [PhraCate] -> Script -> IO ([Rule], [PhraCate], [PhraCate])
    ambiResolByManualResol,           -- [PhraCate] -> [OverPair] -> [(PhraCate,PhraCate)] -> IO (OverPair)
    ambiResolByManualResol',          -- (PhraCate, PhraCate, [PhraCate], OverType) -> [OverPair] -> IO [OverPair]
    updateAmbiResol,      -- [PhraCate] -> [OverPair] -> IO ()
    updateAmbiResol',     -- [PhraCate] -> OverPair -> IO ()
    storeClauseParsingToTreebank,     -- Int -> Int -> ([[Rule]], [PhraCate], [PhraCate]) -> IO ()
    storeTree,            -- Int -> String -> IO ()
    readTree_String,      -- Int -> IO String
    sentToClauses,        -- String -> IO [String]
    dispTree,             -- [String] -> IO ()
    dispTree',            -- Int -> [String] -> IO ()
    getClauPhraCate,      -- String -> [PhraCate]
    parseSentWithoutPruning      -- [Rule] -> [String] -> IO ()

    ) where

import Control.Monad
import System.IO
import qualified System.IO.Streams as S
import qualified Data.ByteString.Char8 as C
import qualified Data.ByteString as BS
import Database.MySQL.Base
import Data.List.Utils
import Data.List
import Data.Tuple.Utils
import Data.String.Utils
import Phrase
import Rule
import Corpus
import AmbiResol (OverType,Prior(..),OverPair)
import Parse
import Output
import Utils
import Database

-- Get a sentence from table corpus, actually the sentence is content of column cate_sent2.
getSentFromDB :: Int -> IO String
getSentFromDB sn = do
    conn <- getConn
    stmt <- prepareStmt conn "select cate_sent2 from corpus where serial_num = ?"
    (defs, is) <- queryStmt conn stmt [toMySQLInt32 sn]
    S.read is >>= \case
      Just [MySQLText v] -> return $ fromMySQLText (MySQLText v)
      Nothing -> return ""

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
    putStr $ " There are " ++ show (length cs) ++ " clauses in total, from which clause to start: [RETURN for 1] "
    clauIdx <- getLine
    if clauIdx /= ""                                   -- Not RETURN
      then do
        let ci = read clauIdx :: Int
        if ci < 1 || ci > length cs
          then putStrLn $ "Clause " ++ show ci ++ " does not exist!"
          else do
            gbtFlag <- goBackTo sn ci                             -- Drop trees and scripts of clause <ci> and its subsequents.
            if gbtFlag
              then do
                finFlag <- parseSent' sn (ci - 1) (drop (ci - 1) cs)            -- Skip some clauses
                if finFlag
                  then putStrLn "parseSent: Finished parsing."
                  else putStrLn "parseSent: Not finished parsing."
              else putStrLn "parseSent: Parsing was cancelled."                 -- goBackTo failed, return upper layer calling.
      else do
        gbtFlag <- goBackTo sn 1
        if gbtFlag
          then do
            finFlag <- parseSent' sn 0 cs
            if finFlag
              then putStrLn "parseSent: Finished parsing."
              else putStrLn "parseSent: Not finished parsing."
          else putStrLn "parseSent: Parsing was cancelled."                     -- goBackTo failed, return upper layer calling.

-- To be ready for parsing from clause <ci>, modify attribute <tree> and <script> of entry <sn> in Table corpus.
goBackTo :: Int -> Int -> IO Bool
goBackTo sn ci = do
    conn <- getConn
    stmt <- prepareStmt conn "select tree, script from corpus where serial_num = ?"
    (defs, is) <- queryStmt conn stmt [toMySQLInt32 sn]
    row <- S.read is                                   -- Maybe [MySQLText, MySQLText]
    let row' = case row of
                 Just x -> x                           -- [MySQLText, MySQLText]
                 Nothing -> error "goBackTo: No row was read."
    S.skipToEof is                                     -- Consume result set.

--  putStrLn $ "goBackTo: tree: " ++ (fromMySQLText (head row'))

    let trees = readTrees $ fromMySQLText (head row')
    let scripts = readScripts $ fromMySQLText (last row')

    if length trees + 1 < ci
      then do
        putStrLn $ "goBackTo: " ++ show (length trees) ++ " clause(s) was(were) parsed, skip failed."
        return False
      else if length trees + 1 == ci
             then do
               putStrLn $ "goBackTo: " ++ show (length trees) ++ " clause(s) was(were) parsed, skip succeeded."
               return True
             else do
               let trees' = take (ci - 1) trees
               let scripts' = take (ci - 1) scripts
               stmt' <- prepareStmt conn "update corpus set tree = ?, script = ? where serial_num = ?"
               ok <- executeStmt conn stmt' [toMySQLText (nTreeToString trees'), toMySQLText (nScriptToString scripts'), toMySQLInt32 sn]
               if (getOkAffectedRows ok == 1)
               then do
                 putStrLn $ "goBackTo: " ++ show (length trees) ++ " clause(s) was(were) parsed, skip succeeded."
                 return True
               else do
                 putStrLn $ "goBackTo: skip failed!"
                 return False

{- Parse a sentence, here every clause is a String. Parameter 'sn' is the value of 'serial_num' in database Table 'Corpus', and parameter 'skn' is the number of skipped clauses.
 - If the last clause is not finished in parsing process, return False to skip the remaining clauses.
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
        showNPhraCate nPCs
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
    conn <- getConn
    stmt <- prepareStmt conn "select tree, script from corpus where serial_num = ?"
    (defs, is) <- queryStmt conn stmt [toMySQLInt32 sn]
    row <- S.read is                                   -- Maybe [MySQLText, MySQLText]
    let row' = case row of
                 Just x -> x                           -- [MySQLText, MySQLText]
                 Nothing -> error "goBackTo: No row was read."
    S.skipToEof is                                     -- Skip to end-of-stream.
    closeStmt conn stmt

    let trees = readTrees $ fromMySQLText (head row')
    let scripts = readScripts $ fromMySQLText (last row')
    putStrLn $ "storeClauseParsing: trees: " ++ (show trees)
    putStrLn $ "storeClauseParsing: scripts: " ++ (show scripts)

    let trees' = trees ++ [snd3 rtbPCs]
    let scripts' = scripts ++ [(clauIdx, fst3 rtbPCs, thd3 rtbPCs)]

    putStrLn $ "storeClauseParsing: trees': " ++ (nTreeToString trees')
    putStrLn $ "storeClauseParsing: scripts': " ++ (nScriptToString scripts')

    stmt' <- prepareStmt conn "update corpus set tree = ?, script = ? where serial_num = ?"
    ok <- executeStmt conn stmt' [toMySQLText (nTreeToString trees'), toMySQLText (nScriptToString scripts'), toMySQLInt32 sn]
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
          showNPhraCate (sortPhraCateBySpan nPCs)
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
    showNPhraCate nPCs
    putStr "Word semantic sequence: "
    showNSeman nPCs

    pcClo <- parseClauseWithAllLexRules 1 nPCs                      -- pcClo is the closure under LexRule(nPCs).
    putStrLn $ "The forest closure includes " ++ show (length pcClo) ++ " phrasal categoires, which are:"
    showNPhraCate pcClo

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
    showNPhraCate nPCs2
    if nPCs2 /= nPCs
      then parseClauseWithAllLexRules (transIdx + 1) nPCs2          -- Do the next trip of transition with resultant PCs.
      else return nPCs

{- Do a trip of transition, insert or update related structural genes in Table stru_gene, and return the category-converted rules used in this trip, the resultant phrases, and the banned phrases.
 - If transitive parsing is to terminated, namely selecting 'e' at inquiring rule switches, returnes ([],[],[]) as the terminating flag.
 -}
doTrans :: [Rule] -> [PhraCate] -> [PhraCate] -> IO ([Rule], [PhraCate], [PhraCate])
doTrans onOff nPCs banPCs = do
    showOnOff onOff
    putStr "Are rule switches ok? [y/n/e]: ('y' or RETURN for yes, 'n' for no, and 'e' for exit) "
    ruleSwitchOk <- getLine
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
               showNPhraCate (sortPhraCateBySpan nPCs2)
               putStr "Banned phrases: "
               showNPhraCate (banPCs)                       -- Can't use <sortPhraCateBySpan> on <banPCs>.

               let pcps = getOverlap nPCs2                  -- [(PhraCate, PhraCate)]
               overPairs <- updateStruGene nPCs2 [] pcps    -- IO [OverPair], namely IO [(PhraCate, PhraCate, Prior)], record overlapping pairs for pruning.
               nbPCs <- transWithPruning onOff nPCs banPCs overPairs     -- Get transitive result with pruning.
               putStr "Transitive result after pruning: "
               showNPhraCate (sortPhraCateBySpan (fst nbPCs))
               putStr "Banned phrases: "
               showNPhraCate (snd nbPCs)                    -- The banned phrases after updated.

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
                    else do
                      putStrLn "Please input 'y', 'n', or 'e'!"
                      doTrans onOff nPCs banPCs

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

{- Re-parse a sentence according the previously created parsing script.
 - Here every clause is a String, and parsing starts from the first clause.
 - The first parameter is the value of 'serial_num' in database Table 'corpus'.
 -}
parseSentByScript :: Int -> [String] -> IO ()
parseSentByScript sn cs = do
    hSetBuffering stdin LineBuffering                  -- Open input buffering
    putStr $ " There are " ++ show (length cs) ++ " clauses in total. Press Y to start, press other key to cancel parsing:"
    clauIdx <- getLine
    if clauIdx == "Y"                                  -- Press key 'Y'.
      then do
        let finFlag = parseSentByScript' sn cs
        if finFlag
          then putStrLn "parseSentByScript: Finished parsing."
          else putStrLn "parseSentByScript: Not finished parsing."
      else putStrLn "parseSent: Parsing was cancelled."              -- Press other key.

{- Re-parse a sentence according the previously created parsing script.
 - Here every clause is a String. Parameter 'sn' is the value of 'serial_num' in database Table 'corpus'.
 - If the last clause is not finished in parsing process, return False to skip the remaining clauses.
 -}
parseSentByScript' :: Int -> [String] -> IO Bool
parseSentByScript' _ [] = return True
parseSentByScript' sn cs = do
    confInfo <- readFile "Configuration"
    let script_source = getConfProperty "script_source" confInfo
    conn <- getConn
    let query = DS.fromString ("select script from " ++ script_source ++ " where serial_num = ?")       -- Query is instance of IsString.
    stmt <- prepareStmt conn query
    (defs, is) <- queryStmt conn stmt [toMySQLInt32 sn]                         --([ColumnDef], InputStream [MySQLValue])
    record <- S.read is
    let record' = case record of
                    Just x -> x
                    Nothing -> [MySQLText "NULL"]
    skipToEof is                                                                -- Go to the end of the stream.
    let scripts = fromMySQLText (record'!!0)
    if (scripts == "NULL")
      then error "parseSentByScript': No script was read."
      else putStrLn $ show scripts
    closeStmt conn stmt

    let scripts' = readScripts $ fromMySQLText scripts                          -- [Script]
    finFlag <- parseSentByScript' sn (take (length cs - 1) cs) (take (length cs - 1) scripts')
    let clauIdx = length cs
    putStrLn $ "  ===== Clause No." ++ show clauIdx ++ " ====="
    if finFlag                                                                  -- True means sentential parsing has been finished.
      then do
        let nPCs = initPhraCate $ getNCate $ words (last cs)
        putStr "Before parsing: "
        showNPhraCate nPCs
        putStr "Word semantic sequence: "
        showNSeman nPCs

        rtbPCs' <- parseClauseWithScript [] nPCs [] (last scripts)              -- Parse begins with empty '[[Rule]]' and empty 'banPCs'
        if rtbPCs' == ([],[],[])
          then return False                                                     -- False means the current clause is terminated manually.
          else do
            storeClauseParsingToTreebank sn clauIdx rtbPCs'
            return True

{- Parsing a clause is a recursive transition process.
   Input: A sequence of [Rule], a sequence of phrasal categories, a sequence of banned phrasal categories, and a parsing script;
   Algo.:
   (1) Do one trip of transition;
   (2) If creating new phrasal categories, append rules used in this trip to [[Rule]], take resultant phrases and accumulated banned phrases as input, go (1); Otherwise, return the triple ([[Rule]], resultant tree PCs, accumulated banned PCs).
 -}
parseClauseWithScript :: [[Rule]] -> [PhraCate] -> [PhraCate] -> Script -> IO ([[Rule]],[PhraCate],[PhraCate])
parseClauseWithScript rules nPCs banPCs script = do
    rtbPCs <- doTransWithScript [] nPCs banPCs script                           -- Every trip of transition begins with empty rule set.
                                               -- <rtbPCs> ::= ([Rule], resultant tree PCs, accumulated banned PCs)
                                               -- [Rule] is the set of rules used in this trip of transition.
    if rtbPCs == ([],[],[])
      then return ([],[],[])                   -- Return ([],[],[]) as the terminating flag.
      else if nPCs /= (snd3 rtbPCs)
        then do
          let scriptTail = (fst3 script, tail (snd3 script), thd3 script)       -- Remove the head element of OnOff list in parsing script.
          parseClauseWithScript (rules ++ [fst3 rtbPCs]) (snd3 rtbPCs) (thd3 rtbPCs) scriptTail         -- Do the next trip of transition
                                               -- with appended rules, resultant PCs, and accumulated banned PCs.
        else do                                -- Phrasal closure has been formed.
          putStrLn $ "Num. of phrasal categories in closure is " ++ (show $ length nPCs)
          showNPhraCate (sortPhraCateBySpan nPCs)
          let spls = divPhraCateBySpan (nPCs)
          putStrLn "  ##### Parsing Tree #####"
          showTreeStru spls spls
          return (rules ++ [fst3 rtbPCs], snd3 rtbPCs, thd3 rtbPCs)

{- Do a trip of transition, insert or update related ambiguity resolution samples in Table <ambi_resol_model>, and return the category-converted
 - rules used in this trip, the resultant phrases, and the banned phrases.
 - If transitive parsing is to terminated, namely selecting 'e' at inquiring rule switches, returnes ([],[],[]) as the terminating flag.
 -}
doTransWithScript :: [Rule] -> [PhraCate] -> [PhraCate] -> Script -> IO ([Rule], [PhraCate], [PhraCate])
doTransWithScript onOff nPCs banPCs script = do
    let rws = map ("+" ++) $ head $ snd3 script                                 -- Get rule switch series
    let updateOnOff [] rws                                                      -- Set [Rule] for this trip of transition

    let nPCs2 = trans onOff nPCs banPCs          -- Without pruning, get transitive result.
    let pcps = getOverlap nPCs2                  -- [(PhraCate, PhraCate)]
    overPairs <- ambiResolByScript nPCs2 [] pcps script    -- IO [OverPair], namely IO [(PhraCate, PhraCate, Prior)], record overlapping pairs for pruning.
    let pcpsWithPrior = map (\x->(fst3 x, snd3 x)) overPairs
    let pcps' = [pcp | pcp <- pcps, notElem pcp pcpsWithPrior]                  -- [(PhraCate, PhraCate)] not resolved by script.

    if pcps' /= []
      then do
        putStrLn "Ambiguities are resolved by script ... [Failed]"
        let overPairs' = ambiResolByManualResol nPCs2 overPairs pcps'           -- Continue resolving ambiguities but by human mind.
      else
        putStrLn "Ambiguities are resolved by script ... [OK]"
        let overPairs' = overPairs

    nbPCs <- transWithPruning onOff nPCs banPCs overPairs'    -- Get transitive result with pruning.
    putStr "Transitive result after pruning: "
    showNPhraCate (sortPhraCateBySpan (fst nbPCs))
    putStr "Banned phrases: "
    showNPhraCate (snd nbPCs)                    -- The banned phrases after updated.

    putStr "This trip of transition is ok? [y/n/e]: (RETURN for 'y')"
    transOk <- getLine                           -- Get user decision of whether to do next transition
    if transOk == "y" || transOk == ""           -- Press key 'y' or directly press RETURN
      then do
          updateAmbiResol (fst nbPCs) overPairs'                                -- Record ambiguity resolution fragments.
          return (onOff,(fst nbPCs),(snd nbPCs))
      else if transOk == "n"
             then doTransWithManualResol onOff nPCs banPCs      -- do this trip of transition by manually resolving ambiguities.
             else if transOk == "e"
                    then return ([],[],[])       -- Return from doTrans, and indicate this is terminating exit.
                    else do
                      putStrLn "Please input 'y', 'n', or 'e'!"
                      doTrans onOff nPCs banPCs
  else if ruleSwitchOk == "e"
         then return ([],[],[])                  -- Return from doTrans, and indicate this is terminating exit.
         else do
           putStrLn "Please input 'y', 'n', or 'e'!"
           doTrans onOff nPCs banPCs

{- Resolve ambiguities by parsing script. Let banPCs be the set of banned phrases, which can be obtained from parsing script. For (lp, rp),
 - if lp belongs to banPCs but rp does not, then we get (lp, rp, Rp).
 - if rp belongs to banPCs but lp does not, then we get (lp, rp, Lp).
 - if both lp and rp belong to banPCs, then we get (lp, rp, Noth).
 - if both lp and rp do not belong to banPCs, then the ambiguity resolution for this pair of phrases is skipped.
 -}
ambiResolByScript :: [PhraCate] -> [OverPair] -> [(PhraCate, PhraCate)] -> Script -> [OverPair]
ambiResolByScript _ overPairs [] _ = overPairs
ambiResolByScript nPCs overPairs (pcp:pcps) script
    | notElem lp banPCs && elem rp banPcs = ambiResolByScript nPCs ((lp, rp, Lp):overPairs) pcps script
    | elem lp banPCs && notElem rp banPcs = ambiResolByScript nPCs ((lp, rp, Rp):overPairs) pcps script
    | elem lp banPCs && elem rp banPCs = ambiResolByScript nPCs ((lp, rp, Noth):overPairs) pcps script
    | otherwise = ambiResolByScript nPCs overPairs pcps script
    where
    banPCs = thd3 script
    lp = fst pcp
    rp = snd pcp

{- Do a trip of transition, insert or update related ambiguity resolution samples in Table <ambi_resol_model>, and return the category-converted
 - rules used in this trip, the resultant phrases, and the banned phrases.
 - If transitive parsing is to terminated, namely selecting 'e' at inquiring rule switches, returnes ([],[],[]) as the terminating flag.
 -}
doTransWithManualResol :: [Rule] -> [PhraCate] -> [PhraCate] -> IO ([Rule], [PhraCate], [PhraCate])
    showOnOff onOff
    putStr "Are rule switches ok? [y/n/e]: ('y' or RETURN for yes, 'n' for no, and 'e' for exit) "
    ruleSwitchOk <- getLine
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
               putStr "Transitive result before pruning: "
               showNPhraCate (sortPhraCateBySpan nPCs2)
               putStr "Banned phrases: "
               showNPhraCate (banPCs)                       -- Can't use <sortPhraCateBySpan> on <banPCs>.

               let pcps = getOverlap nPCs2                  -- [(PhraCate, PhraCate)]
               overPairs <- ambiResolByManualResol nPCs2 [] pcps                -- [OverPair], record overlapping pairs for pruning.
               nbPCs <- transWithPruning onOff nPCs banPCs overPairs            -- Get transitive result with pruning.
               putStr "Transitive result after pruning: "
               showNPhraCate (sortPhraCateBySpan (fst nbPCs))
               putStr "Banned phrases: "
               showNPhraCate (snd nbPCs)                    -- The banned phrases after updated.

               putStr "This trip of transition is ok? [y/n/e]: (RETURN for 'y')"
               transOk <- getLine                           -- Get user decision of whether to do next transition
               if transOk == "y" || transOk == ""           -- Press key 'y' or directly press RETURN
                 then do
                     updateAmbiResol (fst nbPCs) overPairs        -- Record ambiguity resolution fragments.
                     return (onOff,(fst nbPCs),(snd nbPCs))
                 else if transOk == "n"
                        then doTransWithManualResol onOff nPCs banPCs           -- Redo this trip of transition.
                        else if transOk == "e"
                               then return ([],[],[])       -- Return from doTrans, and indicate this is terminating exit.
                               else do
                                 putStrLn "Please input 'y', 'n', or 'e'!"
                                 doTrans onOff nPCs banPCs
             else if ruleSwitchOk == "e"
                    then return ([],[],[])                  -- Return from doTrans, and indicate this is terminating exit.
                    else do
                      putStrLn "Please input 'y', 'n', or 'e'!"
                      doTransWithManualResol onOff nPCs banPCs

{- Manually resolve ambiguities but not insert or update ambiguity resolution records.
 - MySQL table storing ambiguity resolution fragments is dictated by configuration file 'Configuration'.
 -}
ambiResolByManualResol [PhraCate] -> [OverPair] -> [(PhraCate, PhraCate)] -> IO [OverPair]
ambiResolByManualResol _ overPairs [] = return overPairs                        -- No overlapping phrases need to be resolved.
ambiResolByManualResol nPCs overPairs (pcp:pcps) = do
    newOverPairs <- ambiResolByManualResol' nPCs overPairs pcp                  -- Update structural gene in Table stru_gene
    ambiResolByManualResol nPCs newOverPairs pcps

{- Mannually resolve a pair of overlapping phrases, and add the resolving result into the input list of OverPair(s), then return the new OverPair list.
 -}
ambiResolByManualResol' :: [PhraCate] -> [OverPair] -> (PhraCate, PhraCate) -> IO [OverPair]
ambiResolByManualResol' nPCs overPairs (lp, rp) = do
    confInfo <- readFile "Configuration"                                        -- Read the local configuration file
    let ambi_resol_model = getConfProperty "ambi_resol_model" confInfo
    if (ambi_resol_model == "ambi_resol1")
      then do
        context = [x | x <- nPCs, x /= lp, x /= rp]        -- Get context of the pair of overlapping phrases
        ot = getOverType nPCs lp rp                        -- Get overlapping type

        putStr "Find a fragment of No.1 ambiguity model: "
        showAmbiModel1Frag lp rp context ot

        putStr "please input priority [Lp/Rp]: (RETURN for 'Lp') "
        prior <- getLine
        if (prior == "" || prior == "Rp")                  -- Set prior as "Rp".
          then return ((lp, rp, read "Rp"::Prior):overPairs)
          else if prior == "Lp"                            -- Set prior as "Lp".
            then return ((lp, rp, read "Rp"::Prior):overPairs)
            else do
              putStrLn "ambiResolByManualResol': Illegal priority"
              ambiResolByManualResol' nPCs overPairs (lp, rp)                   -- Calling the function itself again.
      else do
        putStrLn "ambiResolByManualResol': Manual resolution of other models."
        return []

{- Insert new ambiguity resolution fragments or update old ambiguity resolution fragments in databse table.
 - The input phrase set <nPCs> is used to create ambiguity resolution context for every overlapping phrases.
 - Ambiguity resolution model, such as stru_gene and ambi_resol1, also is the name of MySQL table storing the samples of that model.
 -}
updateAmbiResol :: [PhraCate] -> [OverPair] -> IO ()
updateAmbiResol _ [] = do
    putStrLn "updateAmbiResol: Update finshed."              -- To make output easy to read.
updateAmbiResol nPCs (op:ops) = do
    updateAmbiResol' nPCs op
    updateAmbiResol nPCs ops

{- Insert or update an ambiguity resolution fragment in MySQL table storing ambiguity resolution samples.
 -}
updateAmbiResol' :: [PhraCate] -> OverPair -> IO ()
updateAmbiResol' nPCs overPair = do
    confInfo <- readFile "Configuration"                                        -- Read the local configuration file
    let ambi_resol_model = getConfProperty "ambi_resol_model" confInfo
    if (ambi_resol_model == "ambi_resol1")
      then
        let lp = fst3 overPair                                   -- Get left overlapping phrase.
        let rp = snd3 overPair                                   -- Get right overlapping phrase.
        let context =  sortPhraCateBySpan [x | x <- nPCs, x /= leftPhrase, x/= rightPhrase]        -- Get context for ambiguity resolution, which is sorted by increasing phrasal spans.
        let ot = getOverType nPCs leftPhrase rightPhrase         -- Get overlapping type
        let prior = thd3 overPair                                -- Get prior selection of the two overlapping phrases.

        putStrLn $ "updateAmbiResol1': Inquire ambiguity resolution fragment: leftPhrase = '" ++ show lp ++ "' && " ++
                                          "rightPhrase = '" ++ show rp ++ "' && " ++
                                          "context = '" ++ show context ++ "' && " ++
                                          "overType = " ++ show ot ++ " && " ++
                                          "prior = '" ++ show prior ++ "'"

        let lpv = doubleBackSlash (show lp)                     -- Get values to insert them into MySql Table
        let rpv = doubleBackSlash (show rp)
        let contextv = doubleBackSlash (show context)
        let otv = show ot
        let priorv = show prior

        conn <- getConn
        let sqlstat = read (show ("select id, hitCount from ambi_resol1 where leftOver = '" ++ lpv ++ "' && " ++ "rightOver = '" ++ rpv ++ "' && " ++ "context = '" ++ contextv ++ "' && " ++ "overType = '" ++ otv ++ "' && " ++ "prior = " ++ priorv)) :: Query
        stmt <- prepareStmt conn sqlstat
        (defs, is) <- queryStmt conn stmt []

        rows <- S.toList is
        if rows /= []
          then
            if length rows > 1
              then do
                close conn                              -- Close MySQL connection.
                error "updateAmbiResol': Find duplicate ambiguity resolution fragments, which is impossible."
              else do
                let id = fromMySQLInt32U ((rows!!0)!!0)
                let hitCount = fromMySQLInt32U ((rows!!0)!!1)
                putStrLn $ "updateAmbiResol': (" ++ show id ++ ") hitCount: " ++ show hitCount
                resetStmt conn stmt
                let sqlstat = read (show ("update ambi_resol1 set hitCount = ? where id = '" ++ show id ++ "'")) :: Query
                stmt <- prepareStmt conn sqlstat
                executeStmt conn stmt [toMySQLInt32U (hitCount + 1)]                -- Add column 'hitCount' by 1 of structural gene.
                close conn                               -- Close MySQL connection.
          else do
            putStr "Inquire failed. Insert the ambiguity resolution fragment ..."
            let sqlstat = read (show ("insert ambi_resol1 (leftPhrase,rightRight,context,overType,prior) values ('" ++ lpv ++ "','" ++ rpv ++ "','" ++ contextv ++ "'," ++ otv ++ ",'" ++ priorv ++ "')")) :: Query
            stmt1 <- prepareStmt conn sqlstat
            oks <- executeStmt conn stmt1 []             -- Insert the described structural gene.
            putStrLn $ " [OK], and its id is " ++ show (getOkLastInsertID oks)
            close conn                                   -- Close MySQL connection.
      else putStrLn "updateAmbiResol': Processing for other model is not done."

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
    row <- S.read is                                                            -- Maybe [MySQLText, MySQLText]
    let row' = case row of
                 Just x -> x                                                    -- [MySQLText, MySQLText]
                 Nothing -> error "storeClauseParsingToTreebank: No row was read."
    S.skipToEof is                                                              -- Skip to end-of-stream.
    closeStmt conn stmt

    let trees = readTrees $ fromMySQLText (head row')
    let scripts = readScripts $ fromMySQLText (last row')
    putStrLn $ "storeClauseParsing: trees: " ++ (show trees)
    putStrLn $ "storeClauseParsing: scripts: " ++ (show scripts)

    let trees' = trees ++ [snd3 rtbPCs]
    let scripts' = scripts ++ [(clauIdx, fst3 rtbPCs, thd3 rtbPCs)]

    putStrLn $ "storeClauseParsing: trees': " ++ (nTreeToString trees')
    putStrLn $ "storeClauseParsing: scripts': " ++ (nScriptToString scripts')

    let query' = DS.fromString ("update " ++ tree_target ++ " set tree = ?, script = ? where serial_num = ?")     -- Query is instance of IsString.
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
    origTree <- readTree_String sn
    let newTree = origTree ++ ";" ++ treeStr                 -- Use ';' to seperate clause trees
    conn <- getConn
    stmt <- prepareStmt conn "update corpus set tree = ? where serial_num = ?"
    ok <- executeStmt conn stmt [toMySQLText newTree, toMySQLInt32 sn]
    close conn
    if (getOkAffectedRows ok == 1)
      then putStrLn "storeTree: succeeded."
      else error "storeTree: failed!"

-- Read the tree String of designated sentence in Table corpus.
readTree_String :: Int -> IO String
readTree_String sn = do
    conn <- getConn
    stmt <- prepareStmt conn "select tree from corpus where serial_num = ?"
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

-- Display trees' structure of clauses of a sentence, one tree per clause.
dispTree :: [String] -> IO ()
dispTree [] = putStr ""                      -- Nothing to display.
dispTree (s:cs) = do
    dispTree cs
    putStrLn $ "Clause No.: " ++ show (length cs + 1)
    showTreeStru spls spls
    where
      pcs = getClauPhraCate s
      spls = divPhraCateBySpan pcs      -- Span lines

-- Display trees' structure of remaining clauses of a sentence, one tree per clause, and the 'idx' is ordered number of first clause among remaining clauses.
dispTree' :: Int -> [String] -> IO ()
dispTree' _ [] = putStr ""                      -- Nothing to display.
dispTree' idx (s:cs) = do
    putStrLn $ "Clause No.: " ++ show idx
    showTreeStru spls spls
    dispTree' (idx + 1) cs
    where
      pcs = getClauPhraCate s
      spls = divPhraCateBySpan pcs      -- Span lines

-- Get a clause's [PhraCate] from its string value.
getClauPhraCate :: String -> [PhraCate]
getClauPhraCate "" = []
getClauPhraCate str = map getPhraCateFromString (stringToList str)

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
--  showNPhraCate (sortPhraCateBySpan nPCs2)
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
