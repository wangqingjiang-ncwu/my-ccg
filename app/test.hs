module Test (
  testCase,
  test1,
  testClockTime
) where

import Prelude hiding (lookup)
import Data.Map
import Data.List
import Numeric.LinearAlgebra as LA
import Data.Time.Clock
import Data.CSV as CSV

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
    putStrLn $ "ct1: " ++ show ct1
    putStrLn $ "ct2: " ++ show ct2
    putStrLn $ "ct1 to ct2: " ++ show (diffUTCTime ct2 ct1)
