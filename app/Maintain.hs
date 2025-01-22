{-# LANGUAGE OverloadedStrings, LambdaCase #-}

-- Copyright (c) 2019-2025 China University of Water Resources and Electric Power
-- All rights reserved.
-- This mudule collects various maintainence tools, such as updating corpus, which are not necessary to be executed so much often that they appear in main menu.

module Maintain (
    setIpcOfRows,        -- Int -> Int -> String -> IO ()
    removeLineFeed,      -- IO ()
    removeLineFeedForOneRow,    -- [MySQLValue, MySQLValue, MySQLValue] -> [MySQLValue, MySQLValue, MySQLValue]
    addClauIdxToTreeField,      -- SentIdx -> SentIdx -> IO ()
    sortPhraseInTreeAndScript,  -- SentIdx -> SentIdx -> IO ()
    sortPhraseInTree4ASent,     -- [Tree] -> [Tree]
    sortPhraseInScript4ASent,   -- [Script] -> [Script]
    addHX2TreeAndScript,        -- SentIdx -> SentIdx -> IO ()
    addHX2Tree4ASent,    -- [Tree] -> [Tree]
    addHX2Script4ASent,  -- [Script] -> [Script]
    addHX2Phrase,        -- PhraCate -> PhraCate
    ) where

import Control.Monad
import qualified System.IO.Streams as S
import qualified Data.String as DS
import Database.MySQL.Base
import Data.List.Utils
import Data.Tuple.Utils
import Data.List
import Database
import Output
import Corpus
import Phrase
import Utils

{- Set attribue 'ipc' of certain rows as an user of this software. In table corpus,
   column 'ipc' records the intellectual property creator (IPC), who will complete or have
   completed the parsing of one row (namely sentence).
   ONLY user 'wqj' can execute this function to designate a certain software user as IPC of certain rows,
   but this access control has not implemented.
 -}
setIpcOfRows :: SentIdx -> SentIdx -> String -> IO ()
setIpcOfRows startSn endSn username = do
    conn <- getConnByUserWqj
    stmt <- prepareStmt conn "select * from user where name = ?"
    (defs, is) <- queryStmt conn stmt [toMySQLText username]
    row <- S.read is                -- Get Just [MySQLText]
    let username' = case row of
                      Just x -> fromMySQLText (x!!0)     -- The first element of x is MySQLText 'name'
                      Nothing -> error "setIpcOfRows: No this user."
    skipToEof is        -- Go to the end of the stream, consuming result set before executing next SQL statement.
    closeStmt conn stmt
    if (username' /= username)
      then error "setIpcOfRows: Unexpected failure."
      else do
        stmt1 <- prepareStmt conn "update corpus set ipc = ? where serial_num >= ? && serial_num <= ?"
        ok <- executeStmt conn stmt1 [toMySQLText username, toMySQLInt32 startSn, toMySQLInt32 endSn]      -- Update column ipc.
        putStrLn $ show (getOkAffectedRows ok) ++ " rows have been updated."      -- Only rows with their values changed are affected rows.
        closeStmt conn stmt1
    close conn                       -- Close the connection.

{- There might be line feed character '\r', '\n', or "\r\n" in field raw_sent and raw_sent2, which makes the csv file
   exported from table corpus can't be used directly for importing. The function removeLineFeed removes these line-
   terminated characters.
   ONLY user 'wqj' can execute this function to designate a certain software user as IPC of certain rows,
   but this access control has not implemented.
  -}
removeLineFeed :: IO ()
removeLineFeed = do
    conn <- getConnByUserWqj
    stmt <- prepareStmt conn "select raw_sent, raw_sent2, serial_num from corpus"
    (defs, is) <- queryStmt conn stmt []
    rows <- S.toList is
    let rows' = map removeLineFeedForOneRow rows
    let stmt1 = "update corpus set raw_sent = ?, raw_sent2 = ? where serial_num = ?"
    oks <- executeMany conn stmt1 rows'
    putStrLn $ show (length oks) ++ " rows have been updated."      -- Only rows with their values changed are affected rows.
    closeStmt conn stmt
    close conn                       -- Close the connection.

{- Remove line-terminated characters '\r', '\n', or "\r\n" from MySQL values.
 - The input is raw_sent, raw_sent2, and serial_num for one row in Table corpus.
 -}
removeLineFeedForOneRow :: [MySQLValue] -> [MySQLValue]
removeLineFeedForOneRow vs = [rs', rs2', vs!!2]
    where
    rsStr = fromMySQLText (vs!!0)                      -- The field raw_sent
    rs2Str = fromMySQLText (vs!!1)                     -- The field raw_sent2
    rsStrNoLF = replace "\n" "" $ replace "\r" "" rsStr
    rs2StrNoLF = replace "\n" "" $ replace "\r" "" rs2Str
    rs' = toMySQLText rsStrNoLF
    rs2' = toMySQLText rs2StrNoLF

{- Add clause index to field tree in treebank. Treebank is selected by property 'tree_target' in configuration file.
 - The serial number range of sentences are designated by input parameter 'sentSnOfStart' and 'sentSnOfEnd'.
 - Clause indices make removing and paring again desiganated clauses feasible.
 - This function should be run under ghci.
 -}
addClauIdxToTreeField :: SentIdx -> SentIdx -> IO ()
addClauIdxToTreeField sentSnOfStart sentSnOfEnd = do
    confInfo <- readFile "Configuration"                                        -- Read the local configuration file
    let tree_target = getConfProperty "tree_target" confInfo

    conn <- getConn
    let sqlstat = DS.fromString $ "select tree, serial_num from " ++ tree_target ++ " where serial_num >= ? && serial_num <= ?"
    stmt <- prepareStmt conn sqlstat
    (defs, is) <- queryStmt conn stmt [toMySQLInt32 sentSnOfStart, toMySQLInt32 sentSnOfEnd]           -- read rows whose serial_nums are in designated range.
    rows <- S.toList is

    if (rows == [])
      then
        putStrLn "addClauIdxToTreeField: No sentence is asked to do this operation."
      else do
        let bracketSnList = map (\row -> (((!!1) . fromMySQLText . (!!0)) row, (fromMySQLInt32 . (!!1)) row)) $ filter (\row -> (fromMySQLText . (!!0)) row /= "[]") rows
                                             -- [(Char, Int)], the 2nd char i.e. bracket and serial_num
                                             -- The rows with 'tree' value "[]" are filtered out.
--        putStrLn $ "addClauIdxToTreeField: bracketSnList: " ++ show bracketSnList
        prevRes <- someFinished False bracketSnList
        if prevRes
          then putStrLn "addClauIdxToTreeField: This operation was cancelled."
          else do
            let rows' = map addClauIdxForOneRow rows                        -- [[MySQLValue]]
            let sqlstat1 = DS.fromString $ "update " ++ tree_target ++ " set tree = ? where serial_num = ?"
            oks <- executeMany conn sqlstat1 rows'
            putStrLn $ show (length oks) ++ " rows have been updated."             -- Only rows with their values changed are affected rows.

    closeStmt conn stmt
    close conn                       -- Close the connection.

{- Check the first element of every tuple.
 - If the first element is '(', then the sentence indicated by the second element might have finished operation 'addClauIdxToTreeField'; Otherwise, it might not.
 - If some sentences might finish the operation, return True; Otherwise, return False.
 - From the previous transition, 'prevRes' is True for some previous sentences finishing this operation, and False for no previous sentences finishes this operation.
 - So at the beginning, 'prevRes' should be False.
 -}
someFinished :: Bool -> [(Char, Int)] -> IO Bool
someFinished prevRes [] = return prevRes
someFinished prevRes (ci:cis)
    | fst ci == '[' = someFinished prevRes cis
    | fst ci == '(' = do
                        putStrLn $ "someFinished: Sentence " ++ show (snd ci) ++ " have finished operation 'addClauIdxToTreeField'."
                        someFinished True cis
    | otherwise = error "someFinished: Exception."

{- For [MySQLText tree, MySQLInt32 serial_num] in querying result from treebank, 'tree' is transformed into [[PhraCate]], then introduce clausal indices to
 - form [(Int, [PhraCate])]. 'serial_num' remain same.
 -}
addClauIdxForOneRow :: [MySQLValue] -> [MySQLValue]
addClauIdxForOneRow vs = [tree'', vs!!1]
    where
    treeStr = fromMySQLText (vs!!0)                            -- The field tree
    tree = map readPCList $ stringToList treeStr               -- [[PhraCate]]
    tree' = zip ([1..] :: [ClauIdx]) tree                      -- [(ClauIdx, [PhraCate])], namely [Tree]
    tree'' = (toMySQLText . listToString) $ map treeToString tree'

{- Sort phrases in corpus field 'tree' and 'script' such that they are in span ascending order.
 -}
sortPhraseInTreeAndScript :: SentIdx -> SentIdx -> IO ()
sortPhraseInTreeAndScript startSn endSn = do
    confInfo <- readFile "Configuration"                                        -- Read the local configuration file
    let tree_target = getConfProperty "tree_target" confInfo

    conn <- getConn
    let sqlstat = DS.fromString $ "select serial_num, tree, script from " ++ tree_target ++ " where serial_num >= ? && serial_num <= ?"
    stmt <- prepareStmt conn sqlstat
    (_, is) <- queryStmt conn stmt [toMySQLInt32 startSn, toMySQLInt32 endSn]           -- read rows whose serial_nums are in designated range.
    rows <- readStreamByInt32TextText [] is                                     -- [(SendIdx, TreesStr, ScriptsStr)]
    let rowsParsed = [x | x <- rows, snd3 x /= "[]"]                            -- After parsed, a row has non-empty Field 'tree'.

    if (rowsParsed == [])
      then putStrLn "addHX2TreeAndScript: No sentence is asked to do this operation."
      else do
          let rows2Memory = map (\row -> (fst3 row, (readTrees . snd3) row, (readScripts . thd3) row)) rowsParsed       -- [(SendIdx, [Tree], [Script])]
--          mapM_ (\row -> (putStr "(" >>= \_ -> putStr (show (fst3 row)) >>= \_ -> putStr "," >>= \_ -> showTrees (snd3 row) >>= \_ -> putStr "," >>= \_ -> showScript (thd3 row) >>= \_ -> putStrLn ")")) rows2Memory

          let rows2Memory' = map (\row -> (fst3 row, (sortPhraseInTree4ASent . snd3) row, (sortPhraseInScript4ASent . thd3) row)) rows2Memory
--          mapM_ (\row -> (putStr "(" >>= \_ -> putStr (show (fst3 row)) >>= \_ -> putStr "," >>= \_ -> showTrees (snd3 row) >>= \_ -> putStr "," >>= \_ -> showScript (thd3 row) >>= \_ -> putStrLn ")")) rows2Memory'

          let rows' = map (\row -> [(toMySQLText . nTreeToString . snd3) row, (toMySQLText . nScriptToString . thd3) row, (toMySQLInt32 . fst3) row]) rows2Memory'
          let sqlstat1 = DS.fromString $ "update " ++ tree_target ++ " set tree = ?, script = ? where serial_num = ?"
          oks <- executeMany conn sqlstat1 rows'
          putStrLn $ show (length oks) ++ " rows have been updated."             -- Only rows with their values changed are affected rows.

    close conn                       -- Close the connection.

{- For a sentence, parsing result is [Tree], namely [(ClauIdx, [PhraCate])].
 - This function is sort phrases such that they are in span ascending order.
 -}
sortPhraseInTree4ASent :: [Tree] -> [Tree]
sortPhraseInTree4ASent [] = []
sortPhraseInTree4ASent [(clauIdx, pcs)] = [(clauIdx, sortPhraCateBySpan pcs)]
sortPhraseInTree4ASent (t:ts) = sortPhraseInTree4ASent [t] ++ sortPhraseInTree4ASent ts

{- For a sentence, parsing script is [Script], namely [(ClauIdx, [[Rule]], [BanPCs])].
 - This function is sort phrases such that they are in span ascending order.
 -}
sortPhraseInScript4ASent :: [Script] -> [Script]
sortPhraseInScript4ASent [] = []
sortPhraseInScript4ASent [(clauIdx, ruleSets, banPCSets)] = [(clauIdx, ruleSets, map sortPhraCateBySpan' banPCSets)]
sortPhraseInScript4ASent (s:ss) = sortPhraseInScript4ASent [s] ++ sortPhraseInScript4ASent ss

{- Update Field tree in treebank to change the symbol for phrasal structure Half juXtaposition from 'XX' to 'HX'.
 - Half-juXtaposition phrases have semantic expressions starting with '、', '和', '与', '及', '以及' and other conjunctions.
 - This function modify these phrases such that they have structure symbol HX.
 -}
addHX2TreeAndScript :: SentIdx -> SentIdx -> IO ()
addHX2TreeAndScript startSn endSn = do
    confInfo <- readFile "Configuration"                                        -- Read the local configuration file
    let tree_target = getConfProperty "tree_target" confInfo

    conn <- getConn
    let sqlstat = DS.fromString $ "select serial_num, tree, script from " ++ tree_target ++ " where serial_num >= ? && serial_num <= ?"
    stmt <- prepareStmt conn sqlstat
    (_, is) <- queryStmt conn stmt [toMySQLInt32 startSn, toMySQLInt32 endSn]           -- read rows whose serial_nums are in designated range.
    rows <- readStreamByInt32TextText [] is                                     -- [(SendIdx, TreesStr, ScriptsStr)]
    let rowsParsed = [x | x <- rows, snd3 x /= "[]"]                            -- After parsed, a row has non-empty Field 'tree'.

    if (rowsParsed == [])
      then putStrLn "addHX2TreeAndScript: No sentence is asked to do this operation."
      else do
          let rows2Memory = map (\row -> (fst3 row, (readTrees . snd3) row, (readScripts . thd3) row)) rowsParsed       -- [(SendIdx, [Tree], [Script])]
--          putStrLn "addHX2TreeAndScript: Original trees and scripts: "
--          mapM_ (\row -> (putStr "(" >>= \_ -> putStr (show (fst3 row)) >>= \_ -> putStr "," >>= \_ -> showTrees (snd3 row) >>= \_ -> putStr "," >>= \_ -> showScript (thd3 row) >>= \_ -> putStrLn ")")) rows2Memory

          let rows2Memory' = map (\row -> (fst3 row, (addHX2Tree4ASent . snd3) row, (addHX2Script4ASent . thd3) row)) rows2Memory
--          putStrLn "addHX2TreeAndScript: Structure HX-added trees and scripts: "
--          mapM_ (\row -> (putStr "(" >>= \_ -> putStr (show (fst3 row)) >>= \_ -> putStr "," >>= \_ -> showTrees (snd3 row) >>= \_ -> putStr "," >>= \_ -> showScript (thd3 row) >>= \_ -> putStrLn ")")) rows2Memory'

          let rows' = map (\row -> [(toMySQLText . nTreeToString . snd3) row, (toMySQLText . nScriptToString . thd3) row, (toMySQLInt32 . fst3) row]) rows2Memory'
          let sqlstat1 = DS.fromString $ "update " ++ tree_target ++ " set tree = ?, script = ? where serial_num = ?"
          oks <- executeMany conn sqlstat1 rows'
          putStrLn $ show (length oks) ++ " rows have been updated."             -- Only rows with their values changed are affected rows.

    close conn                       -- Close the connection.

{- For a sentence, parsing result is [Tree], namely [(ClauIdx, [PhraCate])].
 - This function is add mark symbol 'HX' for structure Half-juXtaposition.
 -}
addHX2Tree4ASent :: [Tree] -> [Tree]
addHX2Tree4ASent [] = []
addHX2Tree4ASent [(clauIdx, pcs)] = [(clauIdx, map addHX2Phrase pcs)]
addHX2Tree4ASent (t:ts) = addHX2Tree4ASent [t] ++ addHX2Tree4ASent ts

{- For a sentence, parsing script is [Script], namely [(ClauIdx, [[Rule]], [BanPCs])].
 - This function is add mark symbol 'HX' for structure Half-juXtaposition.
 -}
addHX2Script4ASent :: [Script] -> [Script]
addHX2Script4ASent [] = []
addHX2Script4ASent [(clauIdx, ruleSets, banPCSets)] = [(clauIdx, ruleSets, map (map addHX2Phrase) banPCSets)]
addHX2Script4ASent (s:ss) = addHX2Script4ASent [s] ++ addHX2Script4ASent ss

{- For a phrase, it is represented as ((Start, Span), [(Category, Tag, Seman, PhraStru, Act)], SecStart), here CTSPA has only one member.
 - This function is to check Field Seman. When Seman value starts with 、, 和, 与, 及 and 以及, and PhraStru value is "XX", then
 - change PhraStru value into 'HX'. Such phrases are Half-juXtaposition.
 -}
addHX2Phrase :: PhraCate -> PhraCate
addHX2Phrase ((start, span), [(cate, tag, seman, phraStru, act)], secStart)
--    | elem True (map (\x -> isPrefixOf x seman) ["、","，","和","与","并","或","及","以及","也","而","还是","即使","则","却"，"而且","如果","无论"]) && phraStru == "XX" = ((start, span), [(cate, tag, seman, "HX", act)], secStart)
    | (seman!!0) /= '(' && phraStru == "XX" = ((start, span), [(cate, tag, seman, "HX", act)], secStart)
    | otherwise = ((start, span), [(cate, tag, seman, phraStru, act)], secStart)
