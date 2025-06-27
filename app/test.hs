module Test (
  testCase,
  test1
) where

import Prelude hiding (lookup)
import Data.Map
import Data.List
import Numeric.LinearAlgebra as LA

testCase :: Int -> IO ()
testCase i = case i of
               1 -> putStrLn "1"
               2 -> putStrLn "2"
               _ -> putStrLn "Other"

test1 :: Int -> IO ()
test1 arg = do
    let loop i
          | i > 10 = putStrLn "[Done] i > 10"
          | otherwise = do
              putStrLn $ show i
              loop (i + 1)
    loop arg
