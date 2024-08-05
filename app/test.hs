module Test (
  testCase
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
