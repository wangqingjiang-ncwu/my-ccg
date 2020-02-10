-- Copyright (c) 2019-2020 China University of Water Resources and Electric Power,
-- All rights reserved.

module SentParse (
    getSentFromDB,        -- Int -> IO String
    getSent,              -- String -> IO [String]
    parseClause,          -- OnOff -> String -> IO ()
    parseSent,            -- OnOff -> [String] -> IO ()
    parseClause2,         -- OnOff -> Int -> String -> IO ()
    treeSelect,           -- OnOff -> [Int] -> [String] -> IO () 
    getNPhraCate_String,  -- [PhraCate] -> String
    parseClause2_String,  -- Int -> String -> String
    getTree_String,       -- OnOff -> [Int] -> [String] -> String
    storeTree,            -- OnOff -> [Int] -> Int -> [String] -> IO()
    readTree_String,      -- Int -> IO String
    splitAtDeli,          -- String -> [String]
    clauses,              -- String -> [[String]]
    splitClauStr,         -- String -> [String]
    getClauPhraCate,      -- [String] -> [PhraCate]
    dispTree              -- String -> IO ()
    ) where

import Control.Monad
import Database.HDBC
import Database.HDBC.MySQL
import Data.List.Utils
import Data.List
import AssignCate
import Parse
import Output
import Utils

-- Get a sentence from table raw_corpus. 
getSentFromDB :: Int -> IO String
getSentFromDB sn = do
    conn <- getConn
    sth <- prepare conn ("select cate_sent2 from raw_corpus where serial_num=" ++ show sn)
    executeRaw sth
    rows <- fetchAllRows sth
    return $ fromSql $ (rows!!0)!!0

-- Split a sentence into clauses.
getSent :: String -> IO [String]
getSent sent = return $ split " \65292:" sent    -- \65292 is Chinese comma.
        
-- Parse a clause.
-- The first input parameter is On/Off string, for turning on/off Np/s-, Np/v-, Np/a-, P/a-, and A/n- rules.
-- For examples, "+-++-" means using Np/s-, Np/a-, and P/a- rules, but denying Np/v- and A/n- rules.

parseClause :: OnOff -> String -> IO ()
parseClause onOff clause = do
{-  let nPCs0 = initPhraCate $ getNCate $ words clause
    putStrLn $ "Initial phrasal categories: " ++ show (length nPCs0)
    showNPhraCate nPCs0
    let nPCs1 = newSpanPCs onOff nPCs0
    putStrLn $ "New span (>0) phrasal categories: " ++ show (length nPCs1)
    showNPhraCate (sortBySpan nPCs1)
    let nPCs2 = newSpanPCs onOff nPCs1
    putStrLn $ "New span (>1) phrasal categories: " ++ show (length nPCs2)
    showNPhraCate (sortBySpan nPCs2)
    let nPCs3 = newSpanPCs onOff nPCs2
    putStrLn $ "New span (>2) phrasal categories: " ++ show (length nPCs3)
    showNPhraCate (sortBySpan nPCs3)
    let nPCs4 = newSpanPCs onOff nPCs3
    putStrLn $ "New span (>3) phrasal categories: " ++ show (length nPCs4)
    showNPhraCate (sortBySpan nPCs4)
    let nPCs5 = newSpanPCs onOff nPCs4
    putStrLn $ "New span (>4) phrasal categories: " ++ show (length nPCs5)
    showNPhraCate (sortBySpan nPCs5)
    let nPCs6 = newSpanPCs onOff nPCs5 
    putStrLn $ "New span (>5) phrasal categories: " ++ show (length nPCs6)
    showNPhraCate (sortBySpan nPCs6)
    let nPCs7 = newSpanPCs onOff nPCs6
    putStrLn $ "New span (>6) phrasal categories: " ++ show (length nPCs7)
    showNPhraCate (sortBySpan nPCs7)
    let nPCs8 = newSpanPCs onOff nPCs7
    putStrLn $ "New span (>7) phrasal categories: " ++ show (length nPCs8)
    showNPhraCate (sortBySpan nPCs8)
    let nPCs9 = newSpanPCs onOff nPCs8
    putStrLn $ "New span (>8) phrasal categories: " ++ show (length nPCs9)
    showNPhraCate (sortBySpan nPCs9)
    let nPCs10 = newSpanPCs onOff nPCs9
    putStrLn $ "New span (>9) phrasal categories: " ++ show (length nPCs10)
    showNPhraCate (sortBySpan nPCs10)
    let nPCs11 = newSpanPCs onOff nPCs10
    putStrLn $ "New span (>10) phrasal categories: " ++ show (length nPCs11)
    showNPhraCate (sortBySpan nPCs11)
    let nPCs12 = newSpanPCs onOff nPCs11
    putStrLn $ "New span (>11) phrasal categories: " ++ show (length nPCs12)
    showNPhraCate (sortBySpan nPCs12)
-}
    let phraCateClosure = parse onOff $ initPhraCate $ getNCate $ words clause
{-  putStrLn $ "Num. of phrasal categories in closure is " ++ (show $ length phraCateClosure)
    showNPhraCate (sortBySpan phraCateClosure)
    let phraCateClosure2 = [pc | pc <- phraCateClosure1, caOfCate pc /= []]
    putStrLn $ "Num. of phrasal categories in closure 2 is " ++ (show $ length phraCateClosure1)
    showNPhraCate (sortBySpan phraCateClosure2)
    let phraCateClosure = atomizePhraCate phraCateClosure2
    putStrLn $ "Num. of phrasal categories in atomized closure is " ++ (show $ length phraCateClosure)
    putStrLn "After atomized, phraCateClosure :"
    showNPhraCate (sortBySpan phraCateClosure)
-}
    let sp = getNuOfInputCates phraCateClosure - 1
    putStrLn $ "Maximal span is " ++ (show sp)

    let roots = findCate (0, sp) phraCateClosure
    let forest = growForest onOff [[t]|t<-roots] phraCateClosure
    putStrLn ("        Parsing Tree No.1 ~ No." ++ show (length forest))
    showForest forest
    putStr "\n"
    putStrLn ("        Tree Structure No.1 ~ No." ++ show (length forest))
    showForestWithTreeStru forest

-- Parse a sentence.
-- The first input parameter is On/Off string, for turning on/off Np/s-, Np/v-, Np/a-, P/a-, and A/n- rules.
-- For examples, "+-++-" means using Np/s-, Np/a-, and P/a- rules, but denying Np/v- and A/n- rules.

parseSent :: OnOff -> [String] -> IO ()
parseSent _ [] = putStrLn ""
parseSent onOff cs = do
    parseSent onOff (take (length cs - 1) cs)
    putStrLn $ "  ===== Clause No." ++ show (length cs)
    parseClause onOff (last cs)

-- Parse a clause, give out the designed tree by index.
-- The first input parameter is On/Off string, for turning on/off Np/s-, Np/v-, Np/a-, P/a-, and A/n- rules.
-- For examples, "+-++-" means using Np/s-, Np/a-, and P/a- rules, but denying Np/v- and A/n- rules.
 
parseClause2 :: OnOff -> Int -> String -> IO ()
parseClause2 onOff ind clause = do
    let phraCateClosure1 = parse onOff $ initPhraCate $ getNCate $ words clause
    let phraCateClosure2 = [pc | pc <- phraCateClosure1, caOfCate pc /= []]
    let phraCateClosure = atomizePhraCate phraCateClosure2
    let sp = getNuOfInputCates phraCateClosure - 1
    let roots = findCate (0, sp) phraCateClosure
    let forest = growForest onOff [[t]|t<-roots] phraCateClosure
    putStrLn $ "        Parsing Tree No." ++ show ind
    showATree ind forest
    putStr "\n"
    putStrLn $ "        Tree Structure No." ++ show ind
    let spls = divPhraCateBySpan (forest!!(ind-1)) 
    showTreeStru spls spls

-- Output the selected tree of each clause. 
-- Interactively input an [Int] giving tree indices for all clauses. 
-- The first input parameter is On/Off string, for turning on/off Np/s-, Np/v-, Np/a-, P/a-, and A/n- rules.
-- For examples, "+-++-" means using Np/s-, Np/a-, and P/a- rules, but denying Np/v- and A/n- rules.

treeSelect :: OnOff -> [Int] -> [String] -> IO()
treeSelect _ [] _ = putStrLn ""
treeSelect onOff ids cs = do
    treeSelect onOff (take (length ids - 1) ids) (take (length ids - 1) cs)
    putStrLn $ "  ===== Clause No." ++ show (length cs)
    parseClause2 onOff (last ids) (last cs)

-- Get the string of a phrasal category.
getPhraCate_String :: PhraCate -> String
getPhraCate_String pc = "((" ++ show st ++ "," ++ show sp ++ "),[(" ++ show (ca!!0) ++ "," ++ (ta!!0) ++ "," ++ (se!!0) ++ "," ++ (cn!!0) ++ "," ++ show (ac!!0) ++ ")]," ++ show ss ++ ")"
    where
    st = stOfCate pc
    sp = spOfCate pc
    ca = caOfCate pc
    ta = taOfCate pc
    se = seOfCate pc
    cn = cnOfCate pc
    ac = acOfCate pc
    ss = ssOfCate pc

-- Get the string of [PhraCate], using delimiter '&'.
getNPhraCate_String :: [PhraCate] -> String
getNPhraCate_String [] = ""
getNPhraCate_String [pc] = getPhraCate_String pc
getNPhraCate_String (pc:pcs) = getPhraCate_String pc ++ "&" ++ getNPhraCate_String pcs

-- Parse a clause, give out the String value of the designed tree by index.
-- The first input parameter is On/Off string, for turning on/off Np/s-, Np/v-, Np/a-, P/a-, and A/n- rules.
-- For examples, "+-++-" means using Np/s-, Np/a-, and P/a- rules, but denying Np/v- and A/n- rules.

parseClause2_String :: OnOff -> Int -> String -> String
parseClause2_String onOff ind clause = "[" ++ getNPhraCate_String (forest!!(ind-1)) ++ "]"
    where
      phraCateClosure1 = parse onOff $ initPhraCate $ getNCate $ words clause
      phraCateClosure2 = [pc | pc <- phraCateClosure1, caOfCate pc /= []]
      phraCateClosure = atomizePhraCate phraCateClosure2
      sp = getNuOfInputCates phraCateClosure - 1
      roots = findCate (0, sp) phraCateClosure
      forest = growForest onOff [[t]|t<-roots] phraCateClosure

-- Get the String of parsing trees of all clauses for given sentence.
-- The first input parameter is On/Off string, for turning on/off Np/s-, Np/v-, Np/a-, P/a-, and A/n- rules.
-- For examples, "+-++-" means using Np/s-, Np/a-, and P/a- rules, but denying Np/v- and A/n- rules.

getTree_String :: OnOff -> [Int] -> [String] -> String
getTree_String _ [] _ = ""
getTree_String onOff ids cs = getTree_String onOff (take (length ids - 1) ids) (take (length ids - 1) cs) ++ ";" ++ parseClause2_String onOff (last ids) (last cs)

-- Input the selected tree of each clause into database.
-- Input an [Int] to give tree indices for all clauses, a Int to indicate which sentence will be updated, and a Monad Input String which will be parsed.
-- The first input parameter is On/Off string, for turning on/off Np/s-, Np/v-, Np/a-, P/a-, and A/n- rules.
-- For examples, "+-++-" means using Np/s-, Np/a-, and P/a- rules, but denying Np/v- and A/n- rules.

storeTree :: OnOff -> [Int] -> Int -> [String] -> IO()
storeTree _ [] _ _ = putStrLn "No tree is selected!"
storeTree onOff ids sn cs = do
    conn <- getConn
    sth <- prepare conn ("update raw_corpus set tree = ?, onOff = ? where serial_num = " ++ show sn)
    execute sth [toSql $ getTree_String onOff ids cs, toSql onOff]
    commit conn
    disconnect conn

-- Read the tree String of designated sentence in table raw_corpus.
readTree_String :: Int -> IO String
readTree_String sn = do
    conn <- getConn
    sth <- prepare conn ("select tree from raw_corpus where serial_num = " ++ show sn)
    executeRaw sth
    rows <- fetchAllRows sth         --Get [[<tree>]]
    putStrLn $ "Got " ++ show (length rows) ++ " sentence(s)."
    commit conn
    disconnect conn
    return (fromSql ((rows!!0)!!0))

-- Get the list of clause strings from the tree string.
clauses :: String -> IO [String]
clauses cs = return $ [x | x <- splitAtDeli cs ';', x /= ""]
                                  -- Remove "" from [String]

-- Split clause's string into a list of substrings, any one of which is of a phrasal category.
splitClauStr :: String -> [String]
splitClauStr cs = splitAtDeli (init (tail cs)) '&'

-- Get a clause's [PhraCate] from its string value.
getClauPhraCate :: String -> [PhraCate]
getClauPhraCate "" = []
getClauPhraCate str = map getPhraCateFromString (splitClauStr str)

-- Display trees' structure of clauses of a sentence, one tree per clause.
dispTree :: [String] -> IO ()
dispTree [] = putStrLn "End of Forest."
dispTree (s:cs) = do
    showTreeStru spls spls
    dispTree cs
    where
      pcs = getClauPhraCate s 
      spls = divPhraCateBySpan pcs      -- Span lines


