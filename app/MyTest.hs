{-# LANGUAGE OverloadedStrings #-}

module MyTest (
  testCase,
  test1,
  testMySQLRead,
  testClockTime
) where

import Prelude hiding (lookup)
import Data.Map hiding (map)
import Data.List
import Numeric.LinearAlgebra as LA
import qualified System.IO.Streams as S
import Data.Time.Clock
import Data.CSV as CSV
import Data.Time.Clock
import Database
import Database.MySQL.Base
import qualified Data.String as DS

testCase :: Int -> IO ()
testCase i = case i of
               1 -> putStrLn "1"
               2 -> putStrLn "2"
               _ -> putStrLn "Other"

test1 :: Int -> Int -> IO ()
test1 j max = do
    let loop i
          | i > max = putStrLn $ "[Done]"
          | otherwise = do
              putStrLn $ show i
              loop (i + 1)
    loop j

testClockTime :: IO ()
testClockTime = do
    ct1 <- getCurrentTime
    test1 1 1000
    ct2 <- getCurrentTime
    let time = diffUTCTime ct2 ct1
    putStrLn $ "ct1: " ++ show ct1
    putStrLn $ "ct2: " ++ show ct2
    putStrLn $ "ct1 to ct2: " ++ show time
    putStrLn $ show (read (init (show time)) :: Double)

testMySQLRead :: Int -> Int -> Int -> Int -> IO Int
testMySQLRead idx1min idx1max idx2min idx2max = do
  conn <- getConn
  putStrLn $ "  idx1 <- [" ++ show idx1min ++ ", " ++ show idx1max ++ "], idx2 <- [" ++ show idx2min ++ ", " ++ show idx2max ++ "]"
  putStrLn $ "  Many times of queryStmt ..."
  let sqlstat = DS.fromString $ "Select sim from csg_sim_202507" ++ " where contextofsg1idx = ? and contextofsg2idx = ?"
  stmt <- prepareStmt conn sqlstat
  let idPairList = [(idx1, idx2) | idx1 <- [idx1min .. idx1max], idx2 <- [idx2min .. idx2max]]     -- idx1 is primary order index
  let vList = map (\(idx1, idx2) -> (toMySQLInt16U idx1, toMySQLInt16U idx2)) idPairList  -- [(MySQLValue, MySQLValue)]

  t1 <- getCurrentTime                -- UTCTime
  mapM (\(idx1, idx2) -> do
    (_, is) <- queryStmt conn stmt [idx1, idx2]
    skipToEof is
    ) vList
  t2 <- getCurrentTime                                    -- UTCTime
  putStrLn $ "  Complete time = " ++ show (diffUTCTime t2 t1)

  putStrLn $ "  One time of queryStmt ..."
  let sqlstat = DS.fromString $ "Select sim from csg_sim_202507" ++ " where contextofsg1idx >= ? and contextofsg1idx <= ? and contextofsg2idx >= ? and contextofsg2idx <= ?"
  stmt <- prepareStmt conn sqlstat

  t1 <- getCurrentTime                                    -- UTCTime
  (_, is) <- queryStmt conn stmt [toMySQLInt16U idx1min, toMySQLInt16U idx1max, toMySQLInt16U idx2min, toMySQLInt16U idx2max]
  rows <- S.toList is
  t2 <- getCurrentTime                                    -- UTCTime
  putStrLn $ "  Complete time = " ++ show (diffUTCTime t2 t1)

  close conn
  return 0
