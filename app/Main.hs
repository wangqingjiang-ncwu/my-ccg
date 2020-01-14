-- Copyright (c) 2019-2020 China University of Water Resources and Electric Power,
-- All rights reserved.

module Main (
    main      -- IO()
    ) where

import Category
import Rule
import Parse
import Output

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
       let phraCateClosure1 = parse phraCateInput
       putStrLn "Parsing result: "
       showNPhraCate phraCateClosure1
       let phraCateClosure2 = [pc | pc <- phraCateClosure1, caOfCate pc /= []]
       putStrLn "After deleting category [], parsing result: "
       showNPhraCate phraCateClosure2
       let phraCateClosure = atomizePhraCate phraCateClosure2
       putStrLn "After unpacking phrasal categories into atomic phrasal categories, result:"
       showNPhraCate phraCateClosure
       let sp = getNuOfInputCates phraCateClosure - 1
       let roots = findCate (0, sp) phraCateClosure
       putStr "Primitive forest: "
       showNPhraCate roots
       let forest = growForest [[t]|t<-roots] phraCateClosure
       putStrLn "Forests that no longer grow:"
       showForest forest
       showForestCateStartPos forest
       putStr "\n" 
       showForestWithTreeStru forest 

