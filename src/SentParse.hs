-- Copyright (c) 2019-2020 China University of Water Resources and Electric Power,
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
    getClauPhraCate       -- String -> [PhraCate]

    ) where

import Control.Monad
import System.IO
import Database.HDBC
import Database.HDBC.MySQL
import Data.List.Utils
import Data.List
import Data.Tuple.Utils
import Phrase
import Parse
import Corpus
import Output
import Script
import Utils

-- Get a sentence from table corpus. 
getSentFromDB :: Int -> IO String
getSentFromDB sn = do
    conn <- getConn
    sth <- prepare conn ("select cate_sent2 from corpus where serial_num=" ++ show sn)
    executeRaw sth
    rows <- fetchAllRows sth
    return $ fromSql $ (rows!!0)!!0

-- Split a sentence into clauses.
getSent :: String -> IO [String]
getSent sent = return $ split " \65292:" sent    -- \65292 is Chinese comma.

{- Parse a sentence, here every clause is a String, and parsing can start from a certain clause. The first parameter
   is the value of 'serial_num' in database Table 'corpus'.
 -}
parseSent :: Int -> [String] -> IO ()
parseSent sn cs = do
    hSetBuffering stdin LineBuffering                  -- Open input buffering
    putStrLn $ " There are " ++ show (length cs) ++ " clauses in total, from which clause to start: [RETURN for 1] "
    clauIdx <- getLine
    if clauIdx /= ""                                   -- Not RETURN
      then do
        let ci = read clauIdx :: Int
        if ci < 1 || ci > length cs
          then error $ "Clause " ++ show ci ++ " does not exist!"
          else do
            goBackTo sn ci                               -- Drop trees and scripts of clause <ci> and its subsequents.
            parseSent' sn (ci - 1) (drop (ci - 1) cs)    -- Skip some clauses
      else parseSent' sn 0 cs                          

-- To be ready for parsing from clause <ci>, modify attribute <tree> and <script> of entry <sn> in Table corpus.
goBackTo :: Int -> Int -> IO ()
goBackTo sn ci = do
    conn <- getConn
    sth <- prepare conn ("select tree, script from corpus where serial_num = " ++ show sn)
    executeRaw sth
    rows <- fetchAllRows sth
    let trees = readPCList (fromSql ((rows!!0)!!0))
    let scripts = readScripts (fromSql ((rows!!0)!!1))
    if length trees + 1 < ci
      then error $ "goBackTo: " ++ show (length trees) ++ " clause(s) was(were) parsed, skip failed."
      else if length trees + 1 == ci
             then putStrLn $ "goBackTo: " ++ show (length trees) ++ " clause(s) was(were) parsed, skip succeeded."
             else do
               let trees' = take (ci - 1) trees
               let scripts' = take (ci - 1) scripts  
               sth' <- prepare conn ("update corpus set tree = ?, script = ? where serial_num = " ++ show sn)
               execute sth [toSql (nPhraCateToString trees'), toSql (nScriptToString scripts')]
               commit conn                    -- Commit any pending data to the database.
               disconnect conn                -- Explicitly close the connection.
               putStrLn $ "goBackTo: " ++ show (length trees) ++ " clause(s) was(were) parsed, skip succeeded."

{- Parse a sentence, here every clause is a String. Parameter 'sn' is the value of 'serial_num' in database Table
   'Corpus', and parameter 'skn' is the number of skipped clauses.
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

--  Add the parsing result of a clause into database.
storeClauseParsing :: Int -> Int -> ([[Rule]], [PhraCate], [PhraCate]) -> IO ()
storeClauseParsing sn clauIdx rtbPCs = do
    conn <- getConn
    sth <- prepare conn ("select tree, script from corpus where serial_num = " ++ show sn)
    executeRaw sth
    rows <- fetchAllRows sth
    let trees = readPCList (fromSql ((rows!!0)!!0))
    let scripts = readScripts (fromSql ((rows!!0)!!1))
    let trees' = trees ++ (snd3 rtbPCs)
    let scripts' = scripts ++ [(clauIdx, fst3 rtbPCs, thd3 rtbPCs)]
    sth' <- prepare conn ("update corpus set tree = ?, script = ? where serial_num = " ++ show sn)
    execute sth [toSql (nPhraCateToString trees'), toSql (nScriptToString scripts')]
 
    commit conn                    -- Commit any pending data to the database.
    disconnect conn                -- Explicitly close the connection.

{- Parsing a clause is a human-machine interactive recursive process. 
   Input: A sequence of [Rule], a sequence of phrasal categories, and a sequence of banned phrasal categories;
   Algo.: 
   (1) Do one trip of transition;
   (2) If creating new phrasal categories, append rules used in this trip to [[Rule]], take resultant phrases and
       accumulated banned phrases as input, go (1);
       Otherwise, return the triple ([[Rule]], resultant tree PCs, accumulated banned PCs).
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

{- Do a trip of transition, insert or update related structural genes in table stru_gene, and return the category-
   converted rules used in this trip, the resultant phrases, and the banned phrases.
 -}
doTrans :: OnOff -> [PhraCate] -> [PhraCate] -> IO ([Rule], [PhraCate], [PhraCate])
doTrans onOff nPCs banPCs = do
    showOnOff onOff
    putStr "Are rule switches ok? [y/n]: (RETURN for 'y') "
    ruleSwitchOk <- getLine
    if ruleSwitchOk /= "y" && ruleSwitchOk /= ""    -- Press key 'n' or other char but not 'y' or RETURN.
      then do
        putStr "Enable or disable rules among \"S/s\", \"O/s\", \"A/s\", \"S/v\", \"O/v\", \"A/v\", \"Hn/v\", \"D/v\", \"S/a\", \"O/a\", \"P/a\", \"Cv/a\", \"Cn/a\", and \"A/n\", for instance, \"+O/s, -A/v\": (RETURN for skip) "
        ruleSwitchStr <- getLine                    -- Get new onOff from input, such as "+O/s,-A/v"
        let rws = splitAtDeli ',' ruleSwitchStr     -- ["+O/s","-A/v"]
        let newOnOff = updateOnOff onOff rws
        doTrans newOnOff nPCs banPCs                -- Redo this trip of transition by modifying rule switches.
      else do                                       -- Press key 'y' or directly press RETURN
        let nPCs2 = trans onOff nPCs banPCs         -- Without pruning, get transitive result
        putStr "Transitive result before pruning: "
        showNPhraCate (sortPhraCateBySpan nPCs2)
        putStr "Banned phrases: "
        showNPhraCate (banPCs)                      -- Can't use <sortPhraCateBySpan> on <banPCs>.

        overPairs <- updateStruGene nPCs2 [] $ getOverlap nPCs2   -- Record overlapping pairs for future pruning.
        nbPCs <- transWithPruning onOff nPCs banPCs overPairs     -- Get transitive result again, and using pruning
        putStr "Transitive result after pruning: "
        showNPhraCate (sortPhraCateBySpan (fst nbPCs))
        putStr "Banned phrases: " 
        showNPhraCate (snd nbPCs)                   -- If there is any phrase removed, here is not empty.

        putStr "This trip of transition is ok? [y/n]: (RETURN for 'y') "
        transOk <- getLine                          -- Get user decision of whether to do next transition
        if transOk == "y" || transOk == ""          -- Press key 'y' or directly press RETURN 
          then return (onOff,(fst nbPCs),(snd nbPCs))
          else doTrans onOff nPCs banPCs            -- Redo this trip of transition by modifying structural genes.
      
-- Insert or update related structural genes in table stru_gene, and recursively create overlapping pairs.
updateStruGene :: [PhraCate] -> [OverPair] -> [(PhraCate,PhraCate)] -> IO [OverPair]
updateStruGene _ overPairs [] = do
    putStrLn "updateStruGene: End"
    return overPairs
updateStruGene nPCs overPairs (pcp:pcps) = do
    newOverPairs <- updateStruGene' struGene overPairs       -- Update structural gene in table stru_gene
    updateStruGene nPCs newOverPairs pcps
    where
      lop = fst pcp
      rop = snd pcp
      ot = getOverType nPCs lop rop                      -- Get overlapping type
      leps = getPhraByEnd (stOfCate lop - 1) nPCs        -- Get all left-extend phrases
      reps = getPhraByStart (enOfCate rop + 1) nPCs      -- Get all right-entend phrases
      struGene = (leps,lop,rop,reps,ot) 

{- Update structural genes related with a certain pair of overlapping phrases, and add the overlapping pair into
   a list of OverPair.
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
    sth <- prepare conn ("select id, prior from stru_gene where leftExtend = '" ++ lev ++ "' && " ++ 
                                                                 "leftOver = '" ++ lov ++ "' && " ++
                                                                "rightOver = '" ++ rov ++ "' && " ++
                                                              "rightExtend = '" ++ rev ++ "' && " ++
                                                                 "overType = "  ++ otv)
    executeRaw sth
    rows <- fetchAllRows sth
    if rows /= []
      then 
        if length rows > 1
          then error "updateStruGene': Find duplicate structural genes."
          else do
            let id = read (fromSql ((rows!!0)!!0))::Int
            let prior = fromSql ((rows!!0)!!1) 
            putStrLn $ "updateStruGene': (" ++ show id ++ ")Prior: " ++ prior
            putStr "Is the priority right? [y/n]: (RETURN for 'y') "
            input <- getLine
            if input == "y" || input == ""     -- Press key 'y' or directly press RETURN.
              then do
                disconnect conn             -- Explicitly close this MySQL connection.
                return ((snd5 gene, thd5 gene, read prior::Prior):overPairs)
              else do 
                putStr "please input new priority [Lp/Rp]: (RETURN for 'Lp') "
                newPrior <- getLine
                if newPrior == "Lp" || newPrior == "Rp"
                  then do
                    sth <- prepare conn ("update stru_gene set prior = ? where leftExtend = '" ++ lev ++ "' && "
                                                                             ++ "leftOver = '" ++ lov ++ "' && "
                                                                            ++ "rightOver = '" ++ rov ++ "' && "
                                                                          ++ "rightExtend = '" ++ rev ++ "' && "
                                                                             ++ "overType = "  ++ otv)
                    execute sth [toSql newPrior]   -- Update column 'prior' of structural gene.
                    commit conn                    -- Commit any pending data to the database.
                    disconnect conn                -- Explicitly close the connection.
                    return ((snd5 gene, thd5 gene, read newPrior::Prior):overPairs)
                  else if newPrior == ""
                         then do
                           sth <- prepare conn ("update stru_gene set prior = ? where " 
                                                                  ++ "leftExtend = '" ++ lev ++ "' && " 
                                                                    ++ "leftOver = '" ++ lov ++ "' && " 
                                                                   ++ "rightOver = '" ++ rov ++ "' && " 
                                                                 ++ "rightExtend = '" ++ rev ++ "' && " 
                                                                    ++ "overType = "  ++ otv)
                           execute sth [toSql "Lp"]       -- Update column 'prior' of structural gene.
                           commit conn                    -- Commit any pending data to the database.
                           disconnect conn                -- Explicitly close the connection.
                           return ((snd5 gene, thd5 gene, read "Lp"::Prior):overPairs)
                         else do
                           putStrLn "updateStruGene': Illegal priority"
                           updateStruGene' gene overPairs        -- Calling the function itself again.
      else do
        putStr "Inquire failed, skip? [y/n]: (RETURN for 'y') " 
        input <- getLine 
        if input == "y" || input == ""     -- Press key 'y' or directly press RETURN. 
          then do
            disconnect conn                     -- Explicitly close this MySQL connection.
            updateStruGene' gene overPairs      -- To now, don't allow skipping.
          else do 
            putStr "please input new priority [Lp/Rp]: (RETURN for 'Lp') "
            newPrior <- getLine
            if newPrior == "Lp" || newPrior == "Rp"
              then do
                sth <- prepare conn ("insert stru_gene (leftExtend,leftOver,rightOver,rightExtend,overType,prior) values ('" ++ lev ++ "','" ++ lov ++ "','" ++ rov ++ "','" ++ rev ++ "'," ++ otv ++ ",'" ++ newPrior ++ "')")
                executeRaw sth          -- Insert the described structural gene.
                commit conn             -- Commit any pending data to the database.
                disconnect conn         -- Explicitly close the connection.
                return ((snd5 gene, thd5 gene, read newPrior::Prior):overPairs)
              else if newPrior == ""
                then do
                  sth <- prepare conn ("insert stru_gene (leftExtend,leftOver,rightOver,rightExtend,overType,prior) values ('" ++ lev ++ "','" ++ lov ++ "','" ++ rov ++ "','" ++ rev ++ "'," ++ otv ++ ",'Lp')")
                  executeRaw sth          -- Insert the described structural gene.
                  commit conn             -- Commit any pending data to the database.
                  disconnect conn         -- Explicitly close the connection.
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
    sth <- prepare conn ("update corpus set tree = ? where serial_num = " ++ show sn)
    execute sth [toSql newTree]
    commit conn
    disconnect conn

-- Read the tree String of designated sentence in Table corpus.
readTree_String :: Int -> IO String
readTree_String sn = do
    conn <- getConn
    sth <- prepare conn ("select tree from corpus where serial_num = " ++ show sn)
    executeRaw sth
    rows <- fetchAllRows sth          --Get [[<tree>]]
--  disconnect conn
    return (fromSql ((rows!!0)!!0))

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

