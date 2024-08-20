{-# LANGUAGE OverloadedStrings, LambdaCase #-}

-- Copyright (c) 2019-2024 China University of Water Resources and Electric Power,
-- All rights reserved.

-- To alleviate the burden of Module SentParse.hs, some basic types and functions are moved to this module.

module SentParse_Base (
    ClauTag,                     -- (SentIdx, ClauIdx)
    ClauTagPrior,                -- (Prior, ClauTag)
    fromClauTagPriorListStr,     -- String -> [PriorClauTag]
    removeClauTagPriorFromSynAmbiResol,     -- SentIdx -> ClauIdx -> ClauIdx -> IO ()
    removeFromCTPListByClauTag,  -- ClauTag -> [ClauTagPrior] -> [ClauTagPrior]
    stringToCTPListList,         -- String -> [[ClauTagPrior]]
    stringToCTPList,             -- String -> [ClauTagPrior]
    stringToClauTagPrior,        -- String -> ClauTagPrior
    countPriorInCTPList,         -- Prior -> [ClauTagPrior] -> Int
    hasSentSampleInSynAmbiResol, -- SentIdx -> ClauIdx -> ClauIdx -> IO Bool
    hasClauTagInSynAmbiResol,    -- ClauTag -> IO Bool
    hasClauTagInCTPList,         -- ClauTag -> [ClauTagPrior] -> Bool
    ) where

import Corpus (SentIdx, ClauIdx)
import AmbiResol
import Database
import Utils
import Database.MySQL.Base
import qualified Data.String as DS

{- Record which clauses select the ambiguity resolution policy (prior) under a certain stru_gene model context.
 - For a given context of stru-gene model sample, different clauses might select different resolution policies.
 - The type is used for storage of stru_gene samples.
 -}
type ClauTag = (SentIdx, ClauIdx)
type ClauTagPrior = (ClauTag, Prior)

-- Get the list of ClauTagPrior values from its String.
fromClauTagPriorListStr :: String -> [ClauTagPrior]
fromClauTagPriorListStr "[]" = []
fromClauTagPriorListStr str = map (\e -> ((read (fst (fst e)) :: Int, read (snd (fst e)) :: Int), snd e)) tmp
    where
    tmp = map (\x -> (stringToTuple (fst x), read (snd x) :: Prior))$ map stringToTuple $ stringToList str

-- Remove ClauTagPrior tuples from syntax ambiguity resolution samples database.
removeClauTagPriorFromSynAmbiResol :: SentIdx -> ClauIdx -> ClauIdx -> IO ()
removeClauTagPriorFromSynAmbiResol sn clauIdxOfStart clauIdxOfEnd = do
    let clauIdxRange = [clauIdxOfStart .. clauIdxOfEnd]
    if clauIdxRange == []
      then putStrLn "removeClauTagPriorFromSynAmbiResol: Finished."
      else do
        putStrLn $ "removeClauTagPriorFromSynAmbiResol: Removing sample tags of clause " ++ show clauIdxOfStart ++ " begins ..."
        removeClauTagPriorFromSynAmbiResol' (sn, clauIdxOfStart)
        removeClauTagPriorFromSynAmbiResol sn (clauIdxOfStart + 1) clauIdxOfEnd

-- Remove one ClauTagPrior tuple from syntax ambiguity resolution samples database.
removeClauTagPriorFromSynAmbiResol' :: ClauTag -> IO ()
removeClauTagPriorFromSynAmbiResol' clauTag = do
    confInfo <- readFile "Configuration"                                        -- Read the local configuration file
    let ambi_resol_model = getConfProperty "ambi_resol_model" confInfo          -- Syntax Ambiguity Resolution Model

    conn <- getConn
    let sqlstat = DS.fromString $ "select id, clauTagPrior from " ++ ambi_resol_model ++ " where clauTagPrior like '%" ++ show clauTag ++ "%';"
    stmt <- prepareStmt conn sqlstat
    (defs, is) <- queryStmt conn stmt []
    idAndCTPStrList <- readStreamByInt32UText [] is                             -- [(Int, ClauTagPriorStr)]
--    putStrLn $"removeClauTagPriorFromSynAmbiResol': idAndCTPStrList: " ++ show idAndCTPStrList

    let cTPList2 = map (removeFromCTPListByClauTag clauTag) $ map stringToCTPList $ map snd idAndCTPStrList       -- [[ClauTagPrior]]
--    putStrLn $ "removeClauTagPriorFromSynAmbiResol': cTPList2: " ++ show cTPList2

    let lpHitCounts = map toMySQLInt16U $ map (countPriorInCTPList Lp) cTPList2
    let rpHitCounts = map toMySQLInt16U $ map (countPriorInCTPList Rp) cTPList2
    let nothHitCounts = map toMySQLInt16U $ map (countPriorInCTPList Noth) cTPList2

    let cTPList2' = map (toMySQLText . show) cTPList2                           -- [MySqlText]
    let ids = map toMySQLInt32U $ map fst idAndCTPStrList                       -- [MySQLInt32U]
    let rows = compFiveLists cTPList2' lpHitCounts rpHitCounts nothHitCounts ids               -- [[MySQLText, MySqlInt32U]]
--    putStrLn $ "removeClauTagPriorFromSynAmbiResol': rows: " ++ show rows
    let sqlstat' = DS.fromString $ "update " ++ ambi_resol_model ++ " set clauTagPrior = ?, lpHitCount = ?, rpHitCount = ?, nothHitCount = ? where id = ?"
    oks <- executeMany conn sqlstat' rows
    putStrLn $ "removeClauTagPriorFromSynAmbiResol': " ++ show (length oks) ++ " rows have been updated."

    close conn

-- Remove ClauTagPrior values whose ClauTag member equals to a given ClatTag value.
removeFromCTPListByClauTag :: ClauTag -> [ClauTagPrior] -> [ClauTagPrior]
removeFromCTPListByClauTag clauTag cTPList = filter (\ctp -> fst ctp /= clauTag) cTPList

-- Convert a [[String]] value to its corresponding [[ClauTagPrior]] value.
stringToCTPListList :: String -> [[ClauTagPrior]]
stringToCTPListList str = map stringToCTPList $ stringToList str

-- Convert a string list to its corresponding list of ClauTagPrior values.
stringToCTPList :: String -> [ClauTagPrior]
stringToCTPList str = map stringToClauTagPrior $ stringToList str

-- Convert a string to its corresponding ClauTag value.
stringToClauTagPrior :: String -> ClauTagPrior
stringToClauTagPrior str = (\x -> (stringToIntTuple (fst x), read (snd x) :: Prior)) $ stringToTuple str

-- count Prior values in [ClauTagPrior].
countPriorInCTPList :: Prior -> [ClauTagPrior] -> Int
countPriorInCTPList _ [] = 0
countPriorInCTPList prior (ctp:ctps)
   | prior == snd ctp = 1 + countPriorInCTPList prior ctps
   | otherwise = countPriorInCTPList prior ctps

-- Decide whether there is any sample for given SentIdx value and ClauIdx range in syntax ambiguity resolution samples database.
hasSentSampleInSynAmbiResol :: SentIdx -> ClauIdx -> ClauIdx -> IO Bool
hasSentSampleInSynAmbiResol sentIdx clauIdxOfStart clauIdxOfEnd =
    if clauIdxOfStart <= clauIdxOfEnd
      then do
        hasInFstClause <- hasClauTagInSynAmbiResol (sentIdx, clauIdxOfStart)
        if hasInFstClause
          then return True
          else hasSentSampleInSynAmbiResol sentIdx (clauIdxOfStart + 1) clauIdxOfEnd
      else return False

-- Decide whether there is any sample for given ClauTag value in syntax ambiguity resolution samples database.
hasClauTagInSynAmbiResol :: ClauTag -> IO Bool
hasClauTagInSynAmbiResol clauTag = do
    confInfo <- readFile "Configuration"                                        -- Read the local configuration file
    let ambi_resol_model = getConfProperty "ambi_resol_model" confInfo          -- Syntax Ambiguity Resolution Model

    conn <- getConn
    let sqlstat = DS.fromString $ "select id, clauTagPrior from " ++ ambi_resol_model ++ " where clauTagPrior like '%" ++ show clauTag ++ "%';"
    stmt <- prepareStmt conn sqlstat
    (defs, is) <- queryStmt conn stmt []
    idAndCTPStrList <- readStreamByInt32UText [] is                             -- [(Int, ClauTagPriorStr)]
    close conn

    return $ elem True $ map (hasClauTagInCTPList clauTag) $ map stringToCTPList $ map snd idAndCTPStrList       -- [[ClauTagPrior]]

-- Decide whether there is any ClauTagPrior value with given ClauTag value in a [ClauTagPrior] value.
hasClauTagInCTPList :: ClauTag -> [ClauTagPrior] -> Bool
hasClauTagInCTPList _ [] = False
hasClauTagInCTPList clauTag (x:xs)
    | clauTag == fst x = True
    | otherwise = hasClauTagInCTPList clauTag xs
