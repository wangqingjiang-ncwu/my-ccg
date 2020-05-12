-- Copyright (c) 2019-2020 China University of Water Resources and Electric Power,
-- All rights reserved.

module Main (
    main      -- IO()
    ) where

import Category
import Rule
import Parse
import Output
import Utils

main :: IO ()
main = cateAnalyze

cateAnalyze :: IO()
cateAnalyze
  = do line <- getLine
       let cateStrInput = words line
       putStr "You have inputed: "
       showNStr cateStrInput
       let cateInput = getNCate cateStrInput
       putStr "Accepted category sequence: "
       showNCate cateInput
       putStr "\n"
       let phraCateInput = initPhraCate cateInput
       putStrLn "Initial phrase categories: "
       showNPhraCate phraCateInput   

