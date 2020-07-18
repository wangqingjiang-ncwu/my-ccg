-- Copyright (c) 2019-2020 China University of Water Resources and Electric Power,
-- All rights reserved.

module Test (
  parseSentWithoutPruning      -- [Rule] -> [String] -> IO ()
  
  ) where

import Control.Monad
import System.IO
import Database.HDBC
import Database.HDBC.MySQL
import Data.List.Utils
import Data.List
import Data.Tuple.Utils
import SentParse
import Parse
import Phrase
import Corpus
import Rule
import Output
import Utils

{- Parse a sentence. The first input parameter is [Rule] value, and the second input parameter is the clause string
   of a sentence.
 -}

parseSentWithoutPruning :: Int -> [Rule] -> [String] -> IO ()
parseSentWithoutPruning _ _ [] = putStrLn ""
parseSentWithoutPruning sn rules cs = do
    parseSentWithoutPruning sn rules (take (length cs - 1) cs)
    putStrLn $ "  ===== Clause No." ++ show (length cs) ++ " ====="

    conn <- getConn
    sth <- prepare conn ("update corpus set closure = '[]', forest = '[]' where serial_num = " ++ show sn)
    executeRaw sth
    commit conn
    disconnect conn

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
    sth <- prepare conn ("select closure, forest from corpus where serial_num = " ++ show sn)
    executeRaw sth
    rows <- fetchAllRows sth
--  putStr "MySQL closure and forest: "
--  forM_ rows $ \row -> forM_ row $ \col -> putStr (fromSql (col))   -- Use query result before new SQL operation.
--  putStrLn ""
    forM_ rows $ \row -> forM_ row $ \col -> putStr ""   -- Use query result before new SQL operation.

    let nClosure = readClosures (fromSql ((rows!!0)!!0))
--  putStrLn $ "readClosures: nClosure: " ++ (nClosureToString nClosure)
    
    let nForest = readForests (fromSql ((rows!!0)!!1))
--  putStrLn $ "readForests: nForest: " ++ (nForestToString nForest)

    let nClosure' = nClosure ++ [nPCs]
    let nForest' = nForest ++ [forest]

--  putStrLn $ "storeClauseParsingWithoutPruning: nClosure': " ++ (nClosureToString nClosure')
--  putStrLn $ "storeClauseParsingWithoutPruning: nForest': " ++ (nForestToString nForest')

    sth' <- prepare conn ("update corpus set closure = ?, forest = ? where serial_num = " ++ show sn)
    res <- execute sth' [toSql (nClosureToString nClosure'), toSql (nForestToString nForest')]
    putStrLn $ "storeClauseParsingWithoutPruning: " ++ show res ++ " row(s) were modified."

    commit conn                    -- Commit any pending data to the database.
    disconnect conn                -- Explicitly close the connection.

{- Get statistics on a phrasal set, including 
   (1) Total number of atomized phrasal categories;
   (2) Times of using various category-converted rules respectively and in total;
   (3) Number of clauses;
   (4) Length, number of parsing trees, and processing time of each clause.
 -}


