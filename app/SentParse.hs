{-# LANGUAGE OverloadedStrings, LambdaCase #-}

-- Copyright (c) 2019-2021 China University of Water Resources and Electric Power,
-- All rights reserved.

module SentParse (
    getSentFromDB,        -- Int -> IO String
    getSent,              -- String -> IO [String]
    parseSent,            -- Int -> [String] -> IO ()
    goBackTo,             -- Int -> Int -> IO ()
    parseSent',           -- Int -> [String] -> IO ()
    storeClauseParsing,   -- Int -> Int -> ([[Rule]],[PhraCate],[PhraCate]) -> IO ()
    parseClause,          -- [[Rule]] -> [PhraCate] -> [PhraCate] -> IO ([[Rule]],[PhraCate],[PhraCate])
    doTrans,              -- OnOff -> [PhraCate] -> [PhraCate] -> IO ([OnOff],[PhraCate],[PhraCate])
    updateStruGene,       -- [PhraCate] -> [OverPair] -> [(PhraCate,PhraCate)] -> IO [OverPair]
    updateStruGene',      -- ([PhraCate],PhraCate,PhraCate,[PhraCate],OverType) -> [OverPair] -> IO [OverPair]
    storeTree,            -- Int -> String -> IO ()
    readTree_String,      -- Int -> IO String
    sentToClauses,        -- String -> IO [String]
    dispTree,             -- [String] -> IO ()
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
    | endswith " \12290:" sent' = return $ split " \65292:" $ replace " \12290:" "" sent'
                                             -- \12290 is Chinese period, and \65292 is Chinese comma.
    | otherwise = return $ split " \65292:" sent'
    where
      sent' = lstrip $ rstrip sent           -- Strip whitespaces at left and right ends.

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
          then error $ "Clause " ++ show ci ++ " does not exist!"
          else do
            goBackTo sn ci                             -- Drop trees and scripts of clause <ci> and its subsequents.
            parseSent' sn (ci - 1) (drop (ci - 1) cs)  -- Skip some clauses
      else do
        goBackTo sn 1
        parseSent' sn 0 cs

-- To be ready for parsing from clause <ci>, modify attribute <tree> and <script> of entry <sn> in Table corpus.
goBackTo :: Int -> Int -> IO ()
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
      then error $ "goBackTo: " ++ show (length trees) ++ " clause(s) was(were) parsed, skip failed."
      else if length trees + 1 == ci
             then putStrLn $ "goBackTo: " ++ show (length trees) ++ " clause(s) was(were) parsed, skip succeeded."
             else do
               let trees' = take (ci - 1) trees
               let scripts' = take (ci - 1) scripts
               stmt' <- prepareStmt conn "update corpus set tree = ?, script = ? where serial_num = ?"
               ok <- executeStmt conn stmt' [toMySQLText (show (nTreeToString trees')), toMySQLText (show (nScriptToString scripts')), toMySQLInt32 sn]
               if (getOkAffectedRows ok == 1)
               then putStrLn $ "goBackTo: " ++ show (length trees) ++ " clause(s) was(were) parsed, skip succeeded."
               else error $ "goBackTo: skip failed!"

{- Parse a sentence, here every clause is a String. Parameter 'sn' is the value of 'serial_num' in database Table 'Corpus', and parameter 'skn' is the number of skipped clauses.
 -}
parseSent' :: Int -> Int -> [String] -> IO ()
parseSent' _ _ [] = putStrLn ""
parseSent' sn skn cs = do
    parseSent' sn skn (take (length cs - 1) cs)
    let clauIdx = skn + length cs
    putStrLn $ "  ===== Clause No." ++ show clauIdx ++ " ====="
    let nPCs = initPhraCate $ getNCate $ words (last cs)
    putStr "Before parsing: "
    showNPhraCate nPCs
    putStr "Word semantic sequence: "
    showNSeman nPCs
    rtbPCs' <- parseClause [] nPCs []           -- Parse begins with empty '[[Rule]]' and empty 'banPCs'
    storeClauseParsing sn clauIdx rtbPCs'       -- Add the parsing result of this clause into database.

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
    if nPCs /= (snd3 rtbPCs)
      then parseClause (rules ++ [fst3 rtbPCs]) (snd3 rtbPCs) (thd3 rtbPCs)    -- Do the next trip of transition
                                               -- with appended rules, resultant PCs, and accumulated banned PCs.
      else do                                -- Phrasal closure has been formed.
        putStrLn $ "Num. of phrasal categories in closure is '" ++ (show $ length nPCs)
        showNPhraCate (sortPhraCateBySpan nPCs)
        let spls = divPhraCateBySpan (nPCs)
        putStrLn "  ##### Parsing Tree #####"
        showTreeStru spls spls
        return (rules ++ [fst3 rtbPCs], snd3 rtbPCs, thd3 rtbPCs)

{- Do a trip of transition, insert or update related structural genes in Table stru_gene, and return the category-converted rules used in this trip, the resultant phrases, and the banned phrases.
 -}
doTrans :: [Rule] -> [PhraCate] -> [PhraCate] -> IO ([Rule], [PhraCate], [PhraCate])
doTrans onOff nPCs banPCs = do
    showOnOff onOff
    putStr "Are rule switches ok? [y/n/e]: ('y' or RETURN for yes, 'n' for no, and 'e' for exit)"
    ruleSwitchOk <- getLine
    if ruleSwitchOk == "n"                          -- Press key 'n'
      then do
        putStr "Enable or disable rules among \"S/s\", \"O/s\", \"A/s\", \"S/v\", \"O/v\", \"A/v\", \"Hn/v\", \"D/v\", \"S/a\", \"O/a\", \"Hn/a\", \"P/a\", \"Cv/a\", \"Cn/a\", and \"A/n\", for instance, \"+O/s, -A/v\": (RETURN for skip) "
        ruleSwitchStr <- getLine                    -- Get new onOff from input, such as "+O/s,-A/v"
        let rws = splitAtDeliThrowSpace ',' ruleSwitchStr     -- ["+O/s","-A/v"]
        let newOnOff = updateOnOff onOff rws
        doTrans newOnOff nPCs banPCs                -- Redo this trip of transition by modifying rule switches.
      else if ruleSwitchOk == "y" || ruleSwitchOk == ""     -- Press key 'y' or directly press RETURN
             then do
               let nPCs2 = trans onOff nPCs banPCs          -- Without pruning, get transitive result
               putStr "Transitive result before pruning: "
               showNPhraCate (sortPhraCateBySpan nPCs2)
               putStr "Banned phrases: "
               showNPhraCate (banPCs)                       -- Can't use <sortPhraCateBySpan> on <banPCs>.

               overPairs <- updateStruGene nPCs2 [] $ getOverlap nPCs2   -- Record overlapping pairs for pruning.
               nbPCs <- transWithPruning onOff nPCs banPCs overPairs     -- Get transitive result with pruning.
               putStr "Transitive result after pruning: "
               showNPhraCate (sortPhraCateBySpan (fst nbPCs))
               putStr "Banned phrases: "
               showNPhraCate (snd nbPCs)                    -- If there is any phrase removed, here is not empty.

               putStr "This trip of transition is ok? [y/n/e]: (RETURN for 'y')"
               transOk <- getLine                           -- Get user decision of whether to do next transition
               if transOk == "y" || transOk == ""           -- Press key 'y' or directly press RETURN
                 then return (onOff,(fst nbPCs),(snd nbPCs))
                 else if transOk == "n"
                        then doTrans onOff nPCs banPCs      -- Redo this trip of transition.
                        else if transOk == "e"
                               then error "doTrans: Exit."
                               else do
                                 putStrLn "Please input 'y', 'n', or 'e'!"
                                 doTrans onOff nPCs banPCs
             else if ruleSwitchOk == "e"
                    then error "doTrans: Exit."
                    else do
                      putStrLn "Please input 'y', 'n', or 'e'!"
                      doTrans onOff nPCs banPCs

{- Insert or update related structural genes in Table stru_gene, and recursively create overlapping pairs.
   For every pair of overlapping phrases, its overlap type, left- and right-extend phrases are found in a given set
   of phrases.
 -}
updateStruGene :: [PhraCate] -> [OverPair] -> [(PhraCate,PhraCate)] -> IO [OverPair]
updateStruGene _ overPairs [] = do
    putStrLn "updateStruGene: End"
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

    putStr "Find new structural fragment: "
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
    let sqlstat = read (show ("select id, prior from stru_gene where leftExtend = '" ++ lev ++ "' && " ++ "leftOver = '" ++ lov ++ "' && " ++ "rightOver = '" ++ rov ++ "' && " ++ "rightExtend = '" ++ rev ++ "' && " ++ "overType = " ++ otv)) :: Query
    stmt <- prepareStmt conn sqlstat
    (defs, is) <- queryStmt conn stmt []

    rows <- S.toList is
    if rows /= []
      then
        if length rows > 1
          then error "updateStruGene': Find duplicate structural genes."
          else do
            let id = fromMySQLInt32U ((rows!!0)!!0)
            let prior = fromMySQLText ((rows!!0)!!1)
            putStrLn $ "updateStruGene': (" ++ show id ++ ")Prior: " ++ prior
            putStr "Is the priority right? [y/n]: (RETURN for 'y') "
            input <- getLine
            if input == "y" || input == ""     -- Press key 'y' or directly press RETURN.
              then
                return ((snd5 gene, thd5 gene, read prior::Prior):overPairs)
              else do
                putStr "please input new priority [Lp/Rp]: (RETURN for 'Lp') "
                newPrior <- getLine
                if newPrior == "Lp" || newPrior == "Rp"
                  then do
                    resetStmt conn stmt
                    let sqlstat = read (show ("update stru_gene set prior = ? where leftExtend = '" ++ lev ++ "' && " ++ "leftOver = '" ++ lov ++ "' && " ++ "rightOver = '" ++ rov ++ "' && " ++ "rightExtend = '" ++ rev ++ "' && " ++ "overType = "  ++ otv)) :: Query
                    stmt <- prepareStmt conn sqlstat
                    executeStmt conn stmt [toMySQLText newPrior]   -- Update column 'prior' of structural gene.
                    return ((snd5 gene, thd5 gene, read newPrior::Prior):overPairs)
                  else if newPrior == ""
                         then do
                           resetStmt conn stmt

                           let sqlstat = read (show ("update stru_gene set prior = ? where " ++ "leftExtend = '" ++ lev ++ "' && " ++ "leftOver = '" ++ lov ++ "' && " ++ "rightOver = '" ++ rov ++ "' && " ++ "rightExtend = '" ++ rev ++ "' && " ++ "overType = "  ++ otv)) :: Query
                           stmt <- prepareStmt conn sqlstat
                           executeStmt conn stmt [toMySQLText "Lp"]       -- Update column 'prior' of structural gene.

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
                return ((snd5 gene, thd5 gene, read newPrior::Prior):overPairs)
              else if newPrior == ""
                then do
                  let sqlstat = read (show ("insert stru_gene (leftExtend,leftOver,rightOver,rightExtend,overType,prior) values ('" ++ lev ++ "','" ++ lov ++ "','" ++ rov ++ "','" ++ rev ++ "'," ++ otv ++ ",'Lp')")) :: Query
                  stmt2 <- prepareStmt conn sqlstat
                  oks <- executeStmt conn stmt2 []           -- Insert the described structural gene.
                  putStrLn $ "updateStruGene': Last inserted row with ID " ++ show (getOkLastInsertID oks)
                  return ((snd5 gene, thd5 gene, read "Lp"::Prior):overPairs)
                else do
                  putStrLn "updateStruGene': Illegal priority"
                  updateStruGene' gene overPairs   -- Calling the function itself again.

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
dispTree [] = putStrLn "End of Forest."
dispTree (s:cs) = do
    showTreeStru spls spls
    dispTree cs
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
   of transition to be executed, the third parameter is [Rule] value, where Rule::= Ss | Os | As | Sv | Ov | Av | Hnv
   | Dv | Sa | Oa | Pa | Cva | Cna | An. The fourth parameter is word-category string of this clause.
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
      then putStrLn $ "storeClauseParsingWithoutPruning: " ++ show rn ++ " row(s) were modified."
      else error "storeClauseParsingWithoutPruning: update failed!"

{- Get statistics on a phrasal set, including
   (1) Total number of atomized phrasal categories;
   (2) Times of using various category-converted rules respectively and in total;
   (3) Number of clauses;
   (4) Length, number of parsing trees, and processing time of each clause.
 -}