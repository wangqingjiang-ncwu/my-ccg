module Output (
    showNStr,         -- [String] -> IO()
    showPhraCate,     -- PhraCate a -> IO()
    showNCate,        -- [Category a] -> IO()
    showNPhraCate,    -- [PhraCate a] -> IO()
    showNSplitCate,   -- [(PhraCate a, PhraCate a)] -> IO()
    showAllSplitCate, -- [[(PhraCate a, PhraCate a)]] -> IO()
    showForest,       -- [[PhraCate a]] -> IO()
    showForestWithTreeStru,       -- [[PhraCate a]] -> IO()
    showTree,         -- [[PhraCate a]] -> IO()
    showTreeStru,     -- [[PhraCate a]] -> [[PhraCate a]] -> IO()
    showNCateLine,    -- Bool -> [PhraCate a] -> [[PhraCate a]] -> IO()
    getCateWidth,     -- PhraCate a -> [[PhraCate a]] -> Int
    showNCateSymb,    -- Bool -> [PhraCate a] -> [[PhraCate a]] -> IO()
    getCateStartPos,  -- PhraCate a -> [[PhraCate a]] -> Int
    showForestCateStartPos,      -- [[PhraCate a]] -> IO()
    showCateStartPos,            -- [[PhraCate a]] -> [[PhraCate a]] -> IO()
    drawLine,         -- Int -> IO()
    nSpace            -- Int -> IO()
    ) where

import Category
import Parse

showNStr :: [String] -> IO()
showNStr [] = putStrLn ""
showNStr xs = do
    (putStr . (++ " ") . show) (head xs)
    showNStr (tail xs)

showPhraCate :: PhraCate a -> IO()
showPhraCate pc = do 
    putStr (show pc)
    putStrLn ""

showNCate :: [Category a] -> IO()
showNCate [] = return ()
showNCate [x] = (putStr . show) x
showNCate (x:xs) = do 
    (putStr . show) x
    putStr ", "
    showNCate xs

showNPhraCate :: [PhraCate a] -> IO()
showNPhraCate [] = putStrLn ""
showNPhraCate xs = do
    (putStr . (++ " ") . show) (head xs)
    showNPhraCate (tail xs)   

showNSplitCate :: [(PhraCate a, PhraCate a)] -> IO()
showNSplitCate [] = putStrLn ""
showNSplitCate xs = do
    putStr "("
    putStr (show (fst (head xs)))
    putStr ","
    putStr (show (snd (head xs)))
    putStr ") "
    showNSplitCate (tail xs) 

showAllSplitCate :: [[(PhraCate a, PhraCate a)]] -> IO()
showAllSplitCate [] = putStrLn ""
showAllSplitCate xs = do
    showNSplitCate (head xs)
    showAllSplitCate (tail xs)

showForest :: [[PhraCate a]] -> IO()
showForest [] = putStrLn ""
showForest ts = do
    showNPhraCate (head ts)
    showForest (tail ts)

-- Show a forest with tree structures
showForestWithTreeStru :: [[PhraCate a]] -> IO()
showForestWithTreeStru [] = putStrLn ""
showForestWithTreeStru ts = do
--    showTree spls                           -- Just show in span lines.
    showTreeStru spls spls       
    showForestWithTreeStru (tail ts)
    where
    spls = divPhraCateBySpan (head ts)        -- Span lines

-- Show a tree by printing all lines of phrasal categories in ascending order of spans. 
showTree :: [[PhraCate a]] -> IO()
showTree [] = putStrLn ""
showTree spls = do
    showNPhraCate (head spls)
    showTree (tail spls)

-- Show the structure of a tree with the purpose of human's reading easily.
-- The input is the categorial lines with span from 0 to (getNuOfInputCates - 1).
-- Use brackets to represent the structure of a tree.
-- Besides, the original result 'ospls' of divPhraCateBySpan should be inputed.
-- Here 'ospls' means original span lines.
-- Bool value 'False' means the first category has not printed.

showTreeStru :: [[PhraCate a]] -> [[PhraCate a]] -> IO()
showTreeStru [] _ = putStrLn ""                  -- No any span!
showTreeStru spls ospls = do
    showNCateLine 0 (head spls) ospls
    putStrLn ""                                  -- Line feed
    showNCateSymb 0 (head spls) ospls
    putStrLn ""                                  -- Line feed
    showTreeStru (tail spls) ospls

-- Show a horizontal line for each phrasal category. 
-- The input is phrasal categories with same span and in order of ascending of Start.
-- Here, the original result 'ospls' of divPhraCateBySpan is needed.
-- The first category start at its Start position, followed by other categories.
-- Int value 'curPos' means the position where a dash line begin to be printed.

showNCateLine :: Int -> [PhraCate a] -> [[PhraCate a]] -> IO()
showNCateLine _ [] _ = return ()             -- No phrasal category to display.
showNCateLine _ [((_,_),[],_)] _ = return () -- No category to derive."
showNCateLine curPos (x:xs) ospls = do
    nSpace (catPos - curPos)
    drawLine (getCateWidth x ospls)
    putStr " "                   -- Interval between two adjacent categories
    showNCateLine newPos xs ospls
    where
    catPos = getCateStartPos x ospls
    catWid = getCateWidth x ospls
    newPos = catPos + catWid + 1

-- Compute the width of a phrasal category with letter number as unit.
-- For every initial phrasal (word) category, 
-- its width = upper rounding of (category string length / 8) * 8 - 1. 
-- For other categories, width = (sum of two parent categories) + 1
-- Here the original result 'ospls' of divPhraCateBySpan is needed.

getCateWidth :: PhraCate a -> [[PhraCate a]] -> Int
getCateWidth x ospls
    | sp == 0 = (div (length (show (ca!!0))) 8 + 1) * 8 - 1
    | otherwise = (getCateWidth pc1 ospls) + (getCateWidth pc2 ospls) + 1
        where
        st = stOfCate x
        sp = spOfCate x
        ca = caOfCate x
        ss = ssOfCate x
        pst1 = st
        pst2 = ss
        psp1 = pst2 - pst1 - 1
        psp2 = sp - psp1 - 1
        pc1 = (findCate (pst1, psp1) (ospls!!psp1))!!0     -- In a tree, only one category
        pc2 = (findCate (pst2, psp2) (ospls!!psp2))!!0     -- In a tree, only one category

-- Show symbol string of each phrasal category. 
-- The input is phrasal categories with same span and in order of ascending of Start.
-- Here, the original result 'ospls' of divPhraCateBySpan is needed.
-- The first category start at its Start position, followed by other categories.
-- Int value 'curPos' means the position where the remaining categories will be printed.

showNCateSymb :: Int -> [PhraCate a] -> [[PhraCate a]] -> IO()
showNCateSymb _ [] _ = return ()                -- No category to display.
showNCateSymb _ [((_,_),[],_)] _ = putStrLn "Here, fail to derive category."
showNCateSymb curPos (x:xs) ospls = do
    nSpace (catPos - curPos)
    showNCate (caOfCate x)            -- Usually onle one category. 
    nSpace (catWid - ((length.show) ((caOfCate x)!!0)))
    showNCateSymb newPos xs ospls
    where
    catPos = getCateStartPos x ospls
    catWid = getCateWidth x ospls
    newPos = catPos + catWid + 1

-- Compute the start position of the first category of a certain span line.
-- For initial phrasal (word) categories, their start positions are also computed.
-- For other span lines, the start position of the first category is computed.
-- For other categories, this compution is meaningless.
-- Here the original result 'ospls' of divPhraCateBySpan is needed.

getCateStartPos :: PhraCate a -> [[PhraCate a]] -> Int
getCateStartPos x ospls
    | sp == 0 = foldr (+) 0 [(getCateWidth pc ospls) + 1 | pc <- (ospls!!0), stOfCate pc < st]
                                    -- Initial phrasal (word) category
--    | x /= (ospls!!sp)!!0 = -1      -- Not a first category
    | otherwise = getCateStartPos pc1 ospls  -- Equal to the start position of its first parent.
        where
        st = stOfCate x
        sp = spOfCate x
        ss = ssOfCate x
        pst1 = st
        psp1 = ss - pst1 - 1
        pc1 = (findCate (pst1, psp1) (ospls!!psp1))!!0     -- In a tree, only one category

-- For every tree in a forest, show Start positions for all phrasal categories.
showForestCateStartPos :: [[PhraCate a]] -> IO()
showForestCateStartPos [] = return ()
showForestCateStartPos [t] =
    showCateStartPos spls spls
    where 
    spls = divPhraCateBySpan t
showForestCateStartPos (t:ts) = do
    showCateStartPos spls spls
    showForestCateStartPos ts
    where
    spls = divPhraCateBySpan t

-- Show Start positions for all phrasal categories.
-- Here the original result 'ospls' of divPhraCateBySpan is needed.
showCateStartPos :: [[PhraCate a]] -> [[PhraCate a]] -> IO()
showCateStartPos [] _ = putStrLn ""
showCateStartPos [pcs] ospls 
    | pcs == [] = putStrLn ""
    | otherwise = do
        putStr " ("
        (showNCate . caOfCate . head) pcs
        putStr ", "
        (putStr . show) (getCateStartPos (head pcs) ospls) 
        putStr ")"
        showCateStartPos [tail pcs] ospls
showCateStartPos (l:ls) ospls = do
    showCateStartPos [l] ospls
    showCateStartPos ls ospls

-- Draw a horizontal line of width 'w'
drawLine :: Int -> IO()
drawLine w
    | w == 0 = return ()
    | otherwise = do
        putStr "-"
        drawLine (w-1)
         
-- Show 'w' spaces
nSpace :: Int -> IO()
nSpace w
    | w == 0 = return ()
    | otherwise = do
        putStr " "
        nSpace (w-1)        
        
    
