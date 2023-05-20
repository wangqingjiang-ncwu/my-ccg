-- Copyright (c) 2019-2023 China University of Water Resources and Electric Power,
-- All rights reserved.

module Output (
    getSemStr,        -- String -> String
    getCateStr,       -- String -> String
    getNCate,         -- [String] -> [(Category, Seman)]
    showNCate,        -- [(Category, Seman)] -> IO ()
    showNCate2,       -- [(Category, Seman)] -> IO ()
    showNSeman,       -- [PhraCate] -> IO ()
    showNSeman2,      -- [(Category, Seman)] -> IO ()
    showPhraCate,     -- PhraCate -> IO ()
    putNCtsca,        -- [(Category, Tag, Seman, PhraStru, Act)] -> IO ()
    getNCtsca_String, -- [(Category, Tag, Seman, PhraStru, Act)] -> String
    getPhraCate_String,  -- PharCate -> string
    showNPhraCateLn,     -- [PhraCate] -> IO ()
    showNPhraCate,       -- [PhraCate] -> IO ()
    showNPhraCateWithoutNewLine,    -- [PhraCate] -> IO ()
    putNPC,           -- [PhraCate] -> IO ()
    showStruFrag,     -- [PhraCate] -> PhraCate -> PhraCate -> [PhraCate] -> OverType -> IO ()
    showAmbiModel1Frag,    -- PhraCate -> PhraCate -> [PhraCate] -> OverType -> IO ()
    getNPhraCate_String,   -- [PhraCate] -> String
    showNPhraCatePair,     -- [(PhraCate, PhraCate)] -> IO ()
    showNPhraCatePairList, -- [[(PhraCate, PhraCate)]] -> IO ()
    showOverPair,     -- OverPair -> IO ()
    showNOverPair,    -- [OverPair] -> IO ()
    showScript,       -- [(ClauIdx, [[Rule]], BanPCs))] -> IO ()
    showForest,       -- [[PhraCate]] -> IO ()
    showTree,         -- [[PhraCate]] -> IO ()
    showATree,        -- Int -> [[PhraCate]] -> IO ()
    showForestWithTreeStru,      -- [[PhraCate]] -> IO ()
    showTreeStru,     -- [[PhraCate]] -> [[PhraCate]] -> IO ()
    showNCateLine,    -- Bool -> [PhraCate] -> [[PhraCate]] -> IO ()
    dispWidth,        -- String -> Int
    dispWidth2,       -- Category -> Seman -> Int
    getCateWidth,     -- PhraCate -> [[PhraCate]] -> Int
    showNCateSymb,    -- Bool -> [PhraCate] -> [[PhraCate]] -> IO ()
    showNSemanSymb,   -- Int -> [PhraCate] -> [[PhraCate]] -> IO ()
    findPhraStartPos, -- PhraCate -> [[PhraCate]] -> Int
    showForestCateStartPos,      -- [[PhraCate]] -> IO ()
    showCateStartPos,            -- [[PhraCate]] -> [[PhraCate]] -> IO ()
    drawLine,         -- Int -> IO ()
    nSpace            -- Int -> IO ()
    ) where

import Category
import Rule
import Phrase
import Parse
import Corpus
import AmbiResol (OverPair, OverType, Prior)
import Utils
import Data.Char
import Data.List
import Data.Tuple.Utils

getSemStr :: String -> String
getSemStr [] = []
getSemStr (x:xs)
    | x == ':' = "'"
    | otherwise = x : getSemStr xs

getCateStr :: String -> String
getCateStr [] = []
getCateStr (x:xs)
    | x == ':' = xs
    | otherwise = getCateStr xs

getNCate :: [String] -> [(Category, Seman)]
getNCate [] = []
getNCate (w:ws) = (getCateFromString (getCateStr w), getSemStr w) : getNCate ws

-- Show the list of tuple (Category, Seman).
showNCate :: [(Category, Seman)] -> IO ()
showNCate [] = return ()
showNCate [x] = do
    (putStr . show) (fst x)
    putStr ":"
    putStr (snd x)
showNCate (x:xs) = do
    (putStr . show) (fst x)
    putStr ":"
    putStr (snd x)
    putStr ", "
    showNCate xs

-- Show the list of Category symbols.
showNCate2 :: [(Category, Seman)] -> IO ()
showNCate2 [] = return ()
showNCate2 [x] = do
    (putStr . show) (fst x)
showNCate2 (x:xs) = do
    (putStr . show) (fst x)
    putStr ", "
    showNCate2 xs

-- Show the list of (Start, Seman) of a phrasal sequence.
showNSeman :: [PhraCate] -> IO ()
showNSeman [] = return ()
showNSeman pcs = do
    putNStr $ map (\x -> "(" ++ show (fst x) ++ ", " ++ snd x ++ ")") $ zip (map stOfCate pcs) (map ((!!0) . seOfCate) pcs)

-- Show the list of Semantic symbols.
showNSeman2 :: [(Category, Seman)] -> IO ()
showNSeman2 [] = return ()
showNSeman2 [x] = do
    putStr (snd x)
showNSeman2 (x:xs) = do
    putStr (snd x)
    putStr ", "
    showNSeman2 xs

showPhraCate :: PhraCate -> IO ()
showPhraCate pc = do
--  putStr (show pc)       -- Function 'show' converts Chinese characters to [char].
    putStr $ "((" ++ show (stOfCate pc) ++ "," ++ show (spOfCate pc) ++ "),["
    putNCtsca (ctspaOfCate pc)
    putStr $ "]," ++ show (ssOfCate pc) ++ ")"

putNCtsca :: [(Category,Tag,Seman,PhraStru,Act)] -> IO ()
putNCtsca [] = putStr ""
putNCtsca [x] = putStr $ "(" ++ show (fst5 x) ++ "," ++ (snd5 x) ++ "," ++ (thd5 x) ++ "," ++ (fth5 x) ++ "," ++ show (fif5 x) ++ ")"
putNCtsca (x:xs) = do
    putStr $ "(" ++ show (fst5 x) ++ "," ++ (snd5 x) ++ "," ++ (thd5 x) ++ "," ++ (fth5 x) ++ "," ++ show (fif5 x) ++ "),"
    putNCtsca xs

getNCtsca_String :: [(Category, Tag, Seman, PhraStru, Act)] -> String
getNCtsca_String ctsca_list = "[" ++ getNCtsca_String' ctsca_list ++ "]"

getNCtsca_String' :: [(Category, Tag, Seman, PhraStru, Act)] -> String
getNCtsca_String' [] = ""
getNCtsca_String' [x] = "(" ++ show (fst5 x) ++ "," ++ (snd5 x) ++ "," ++ (thd5 x) ++ "," ++ (fth5 x) ++ "," ++ show (fif5 x) ++ ")"
getNCtsca_String' (x:xs) = getNCtsca_String' [x] ++ ", " ++ getNCtsca_String xs

getPhraCate_String :: PhraCate -> String
getPhraCate_String ((st, sp), nCtsca, ss) = "((" ++ show st ++ ", " ++ show sp ++ "), " ++ getNCtsca_String nCtsca ++ ", " ++ show ss ++ ")"

showNPhraCateLn :: [PhraCate] -> IO ()
showNPhraCateLn [] = putStrLn "[]"
showNPhraCateLn [x] = do
    putStr "["
    showPhraCate x
    putStrLn "]"
showNPhraCateLn (x:xs) = do
    putStr "["
    putNPC (x:xs)
    putStrLn "]"

showNPhraCate :: [PhraCate] -> IO ()
showNPhraCate [] = putStr "[]"
showNPhraCate [x] = do
    putStr "["
    showPhraCate x
    putStr "]"
showNPhraCate (x:xs) = do
    putStr "["
    putNPC (x:xs)
    putStr "]"

showNPhraCateWithoutNewLine :: [PhraCate] -> IO ()
showNPhraCateWithoutNewLine [] = putStr "[]"
showNPhraCateWithoutNewLine [x] = do
    putStr "["
    showPhraCate x
    putStr "]"
showNPhraCateWithoutNewLine (x:xs) = do
    putStr "["
    putNPC (x:xs)
    putStr "]"

putNPC :: [PhraCate] -> IO ()
putNPC [] = putStr ""
putNPC [x] = showPhraCate x
putNPC (x:xs) = do
    showPhraCate x
    putStr ","
    putNPC xs

showStruFrag :: [PhraCate] -> PhraCate -> PhraCate -> [PhraCate] -> OverType -> IO ()
showStruFrag leftExtend leftOver rightOver rightExtend overType = do
    putStr "leftExtend = "
    showNPhraCateWithoutNewLine leftExtend
    putStr ", leftOver = "
    showPhraCate leftOver
    putStr ", rightOver = "
    showPhraCate rightOver
    putStr ", rightExtend = "
    showNPhraCateWithoutNewLine rightExtend
    putStrLn $ ", overType = " ++ show overType

showAmbiModel1Frag :: PhraCate -> PhraCate -> [PhraCate] -> OverType -> IO ()
showAmbiModel1Frag leftPhrase rightPhrase context overType = do
    putStr "leftPhrase = "
    showPhraCate leftPhrase
    putStr ", rightPhrase = "
    showPhraCate rightPhrase
    putStr ", context = "
    showNPhraCateWithoutNewLine context
    putStrLn $ ", overType = " ++ show overType

getNPhraCate_String :: [PhraCate] -> String
getNPhraCate_String xs = "[" ++ getNPhraCate_String' xs ++ "]"

getNPhraCate_String' :: [PhraCate] -> String
getNPhraCate_String' [] = ""
getNPhraCate_String' [x] = getPhraCate_String x
getNPhraCate_String' (x:xs) = getNPhraCate_String' [x] ++ ", " ++ getNPhraCate_String' xs

showNPhraCatePair :: [(PhraCate, PhraCate)] -> IO ()
showNPhraCatePair pcps = do
    putStr "["
    showNPhraCatePair' pcps
    putStrLn "]"

showNPhraCatePair' :: [(PhraCate, PhraCate)] -> IO ()
showNPhraCatePair' [] = putStr ""
showNPhraCatePair' [pcp] = do
    putStr "("
    showPhraCate (fst pcp)
    putStr ","
    showPhraCate (snd pcp)
    putStr ")"
showNPhraCatePair' (pcp:pcps) = do
    showNPhraCatePair' [pcp]
    putStr ","
    showNPhraCatePair' pcps

showNPhraCatePairList :: [[(PhraCate, PhraCate)]] -> IO ()
showNPhraCatePairList pcpsl = do
    putStr "["
    showNPhraCatePairList' pcpsl
    putStrLn "]"

showNPhraCatePairList' :: [[(PhraCate, PhraCate)]] -> IO ()
showNPhraCatePairList' [] = putStr ""
showNPhraCatePairList' [pcps] = showNPhraCatePair pcps
showNPhraCatePairList' (pcps:pcpsl) = do
    showNPhraCatePair pcps
    putStr ","
    showNPhraCatePairList' pcpsl

showOverPair :: OverPair -> IO ()
showOverPair (lp, rp, prior) = do
    putStr "("
    showPhraCate lp
    putStr ", "
    showPhraCate rp
    putStr ", "
    putStr $ show prior
    putStr ")"

showNOverPair :: [OverPair] -> IO ()
showNOverPair ops = do
    putStr "["
    showNOverPair' ops
    putStrLn "]"

showNOverPair' :: [OverPair] -> IO ()
showNOverPair' [] = putStr ""
showNOverPair' [op] = showOverPair op
showNOverPair' (op:ops) = do
    showOverPair op
    putStr ", "
    showNOverPair' ops

showScript :: [(ClauIdx, [[Rule]], BanPCs)] -> IO ()
showScript [] = putStrLn ""
showScript (s:ss) = do
    putStr "["
    showScript' (s:ss)
    putStrLn "]"

showScript' :: [(ClauIdx, [[Rule]], BanPCs)] -> IO ()
showScript' [] = putStr ""
showScript' [s] = do
    putStr "("
    putStr $ show (fst3 s)
    putStr ","
    putStr $ show (snd3 s)
    putStr ",["
    putNPC (thd3 s)
    putStr "])"
showScript' (s:ss) = do
    showScript' [s]
    putStr ","
    showScript' ss

showForest :: [[PhraCate]] -> IO ()
showForest [] = putStrLn ""
showForest ts = do
    showForest (take (length ts - 1) ts)
    putStrLn $ "  ##### Parsing Tree No." ++ show (length ts)
    showNPhraCate (last ts)

-- The following definition is same as the above, used to show a tree by printing all lines of phrasal categories in ascending order of spans.
showTree :: [[PhraCate]] -> IO ()
showTree [] = putStrLn ""
showTree spls = do
    showNPhraCate (head spls)
    showTree (tail spls)

showATree :: Int -> [[PhraCate]] -> IO ()
showATree ind pcss
    | notElem ind [1..length pcss] = error "Tree index is out of range."
    | otherwise = do
        putStrLn $ "  ##### Parsing Tree No." ++ show ind
        showNPhraCate (pcss!!(ind-1))

-- Draw a horizontal line of width 'w'
drawLine :: Int -> IO ()
drawLine w
    | w == 0 = return ()
    | otherwise = do
        putStr "-"
        drawLine (w-1)

-- Show 'w' spaces
nSpace :: Int -> IO ()
nSpace w
    | w == 0 = return ()
    | otherwise = do
        putStr " "
        nSpace (w-1)

-- Get the display width of a string, one-character width for each ASCII character, and two-characters width for each Chinese character.
dispWidth :: String -> Int
dispWidth [] = 0
dispWidth (c:cs)
    | isAscii c = 1 + dispWidth cs
    | otherwise = 2 + dispWidth cs

-- Get the display width of a phrasal category.
-- The category and its corresponding semantics on different lines.
dispWidth2 :: Category -> Seman -> Int
dispWidth2 cate sem
    | lc >= ls = lc
    | otherwise = ls
    where
      lc = length $ show cate
      ls = dispWidth sem + 1       -- Adding 1, is for colon symbol.

-- Compute the width of a phrasal category with letter number as unit.
-- For the initial phrasal (word) category in each line,
-- its width = upper rounding of ((category string length / 8) + 1) * 8 - 1.
-- For other categories, width = (sum of two parent categories) + 1
-- Here the original result 'ospls' of divPhraCateBySpan is needed.

getCateWidth :: PhraCate -> [[PhraCate]] -> Int
getCateWidth x ospls
    | sp == 0 = (div (dispWidth2 (ca!!0) (se!!0)) 8 + 1) * 8 - 1
    | otherwise = (getCateWidth pc1 ospls) + (getCateWidth pc2 ospls) + 1
        where
        st = stOfCate x
        sp = spOfCate x
        ca = caOfCate x
        se = seOfCate x
        ss = ssOfCate x
        pst1 = st
        pst2 = ss
        psp1 = pst2 - pst1 - 1
        psp2 = sp - psp1 - 1
        pc1 = (getPhraBySS (pst1, psp1) (ospls!!psp1))!!0     -- In a tree, only one category
        pc2 = (getPhraBySS (pst2, psp2) (ospls!!psp2))!!0     -- In a tree, only one category

-- Compute the start position of given category before printing tree structure.
-- For span 0, the start position of given category is computed.
-- For other spans, the start position of only first category is meaningful.
-- Here the original result 'ospls' of divPhraCateBySpan is needed.

findPhraStartPos :: PhraCate -> [[PhraCate]] -> Int
findPhraStartPos x ospls
    | sp == 0 = foldr (+) 0 [(getCateWidth pc ospls) + 1 | pc <- (ospls!!0), stOfCate pc < st]
                                    -- Initial phrasal (word) category
--    | x /= (ospls!!sp)!!0 = -1      -- Not a first category
    | otherwise = findPhraStartPos pc1 ospls  -- Equal to the start position of its first parent.
        where
        st = stOfCate x
        sp = spOfCate x
        ss = ssOfCate x
        pst1 = st
        psp1 = ss - pst1 - 1
        pc1 = (getPhraBySS (pst1, psp1) (ospls!!psp1))!!0     -- In a tree, only one category

-- Given a series of phrasal categories, show corresponding horizontal lines.
-- These categories have same span and are ordered in ascending of Start.
-- Here, the original result 'ospls' of divPhraCateBySpan is needed.
-- The first category start at its Start position, followed by other categories.
-- Int value 'curPos' means the position where a dash line begin to be printed.

showNCateLine :: Int -> [PhraCate] -> [[PhraCate]] -> IO ()
showNCateLine _ [] _ = return ()             -- No phrasal category to display.
showNCateLine _ [((_,_),[],_)] _ = return () -- No category to derive."
showNCateLine curPos (x:xs) ospls = do
    nSpace (catPos - curPos)
    drawLine ((getCateWidth x ospls) - (length (catTag!!0)))
                                 -- leave space for rule tag.
    putStr (catTag!!0)
    putStr " "                   -- Interval between two adjacent categories
    showNCateLine newPos xs ospls
    where
    catPos = findPhraStartPos x ospls
    catWid = getCateWidth x ospls
    catTag = taOfCate x
    newPos = catPos + catWid + 1

-- Show symbol strings of phrasal categories in a certain span line.
-- The input is phrasal categories with same span and in order of ascending of Start.
-- Here, the original result 'ospls' of divPhraCateBySpan is needed.
-- The first category start at its Start position, followed by other categories.
-- Int value 'curPos' means the position where the remaining categories will be printed.

showNCateSymb :: Int -> [PhraCate] -> [[PhraCate]] -> IO ()
showNCateSymb _ [] _ = return ()                -- No category to display.
showNCateSymb _ [((_,_),[],_)] _ = putStrLn "Here, fail to derive category."
showNCateSymb curPos (x:xs) ospls = do
    nSpace (catPos - curPos)
    showNCate2 cs            -- Usually onle one category.
    nSpace (catWid - length (show (fst (cs!!0))) + 1)
    showNCateSymb newPos xs ospls
    where
    catPos = findPhraStartPos x ospls
    catWid = getCateWidth x ospls
    newPos = catPos + catWid + 1
    cs = csOfCate x         -- [(category, seman)]

-- Show symbol strings of phrasal categories' semantics in a certain span line.
-- The input is phrasal categories with same span and in order of ascending of Start.
-- Here, the original result 'ospls' of divPhraCateBySpan is needed.
-- The first category start at its Start position, followed by other categories.
-- Int value 'curPos' means the position where the remaining categories will be printed.

showNSemanSymb :: Int -> [PhraCate] -> [[PhraCate]] -> IO ()
showNSemanSymb _ [] _ = return ()           -- No phrasal category to display.
showNSemanSymb _ [((_,_),[],_)] _ = putStrLn "Here, fail to derive category."
showNSemanSymb curPos (x:xs) ospls = do
    nSpace (catPos - curPos)
    putStr ":"                -- The leading flag for semantic expression.
    showNSeman2 cs            -- Usually onle one semantic symbol.
    nSpace (catWid - dispWidth (snd (cs!!0)))
                              -- The inter-category space offsets position occupying of leading colon.
    showNSemanSymb newPos xs ospls
    where
      catPos = findPhraStartPos x ospls
      catWid = getCateWidth x ospls
      newPos = catPos + catWid + 1
      cs = csOfCate x         -- [(category, seman)]

-- Show the structure of a tree with the purpose of human's reading easily.
-- The input is categorial lines with span from 0 to (getNuOfInputCates - 1).
-- Use brackets to represent the structure of a tree.
-- Besides, the original result 'ospls' of divPhraCateBySpan should be inputed.
-- Here 'ospls' means original span lines.
-- Bool value 'False' means the first category has not printed.

showTreeStru :: [[PhraCate]] -> [[PhraCate]] -> IO ()
showTreeStru [] _ = putStrLn ""                  -- No any span!
showTreeStru spls ospls = do
    if (head spls /= [])                         -- Only output not-empty spans.
      then do
        showNCateLine 0 (head spls) ospls
        putStrLn ""                                  -- Line feed
        showNCateSymb 0 (head spls) ospls
        putStrLn ""                                  -- Line feed
        showNSemanSymb 0 (head spls) ospls
        putStrLn ""                                  -- Line feed
      else
        putStr ""                                    -- Do nothing
    showTreeStru (tail spls) ospls

-- Show a forest with tree structures
showForestWithTreeStru :: [[PhraCate]] -> IO ()
showForestWithTreeStru [] = putStrLn ""
showForestWithTreeStru ts = do
    showForestWithTreeStru (take (length ts - 1) ts)
    putStrLn $ "  ##### Parsing Tree No." ++ show (length ts)
    showTreeStru spls spls
    where
      spls = divPhraCateBySpan (last ts)         -- Span lines

-- Show Start positions for all phrasal categories.
-- Here the original result 'ospls' of divPhraCateBySpan is needed.
showCateStartPos :: [[PhraCate]] -> [[PhraCate]] -> IO ()
showCateStartPos [] _ = putStrLn ""
showCateStartPos [pcs] ospls
    | pcs == [] = putStrLn ""
    | otherwise = do
        putStr " ("
        (showNCate . csOfCate . head) pcs
        putStr ", "
        (putStr . show) (findPhraStartPos (head pcs) ospls)
        putStr ")"
        showCateStartPos [tail pcs] ospls
showCateStartPos (l:ls) ospls = do
    showCateStartPos [l] ospls
    showCateStartPos ls ospls

-- For every tree in a forest, show Start positions for all phrasal categories.
showForestCateStartPos :: [[PhraCate]] -> IO ()
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
