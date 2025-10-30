-- Copyright (c) 2019-2025 China University of Water Resources and Electric Power
-- All rights reserved.

module Utils (
    fst4,          -- (a,b,c,d) -> a
    snd4,          -- (a,b,c,d) -> b
    thd4,          -- (a,b,c,d) -> c
    fth4,          -- (a,b,c,d) -> d
    fst5,          -- (a,b,c,d,e) -> a
    snd5,          -- (a,b,c,d,e) -> b
    thd5,          -- (a,b,c,d,e) -> c
    fth5,          -- (a,b,c,d,e) -> d
    fif5,          -- (a,b,c,d,e) -> e
    fst6,          -- (a,b,c,d,e,f) -> a
    snd6,          -- (a,b,c,d,e,f) -> b
    thd6,          -- (a,b,c,d,e,f) -> c
    fth6,          -- (a,b,c,d,e,f) -> d
    fif6,          -- (a,b,c,d,e,f) -> e
    sth6,          -- (a,b,c,d,e,f) -> f
    fst7,          -- (a,b,c,d,e,f,g) -> a
    snd7,          -- (a,b,c,d,e,f,g) -> b
    thd7,          -- (a,b,c,d,e,f,g) -> c
    fth7,          -- (a,b,c,d,e,f,g) -> d
    fif7,          -- (a,b,c,d,e,f,g) -> e
    sth7,          -- (a,b,c,d,e,f,g) -> f
    svt7,          -- (a,b,c,d,e,f,g) -> g
    fst8,          -- (a,b,c,d,e,f,g,h) -> a
    snd8,          -- (a,b,c,d,e,f,g,h) -> b
    thd8,          -- (a,b,c,d,e,f,g,h) -> c
    fth8,          -- (a,b,c,d,e,f,g,h) -> d
    fif8,          -- (a,b,c,d,e,f,g,h) -> e
    sth8,          -- (a,b,c,d,e,f,g,h) -> f
    svt8,          -- (a,b,c,d,e,f,g,h) -> g
    eth8,          -- (a,b,c,d,e,f,g,h) -> g
    removeDup,     -- Eq a => [a] -> [a]
    removeDup',    -- Eq a => [a] -> [a] -> [a]
    removeTuple,   -- Eq a => [(a,a)] -> [(a,a)]
    tupToList,     -- Eq a => [(a,a)] -> [a]
    throwBrac,     -- String -> String
    throwFstBrac,  -- String -> String
    throwLastBrac, -- String -> String
    splitAtDeli,   -- Char -> String -> [String]
    splitAtDeliThrowSpace,       -- Char -> String -> [String]
    splitAtDeliAtFP,             -- Char -> String -> [String]
    splitTagAsConvOrCal,         -- String -> [String]
    maxStrLen,     -- [String] -> Int
    doubleBackSlash,   -- String -> String
    putNStr,       -- [String] -> IO ()
    throwHTSpace,  -- String -> String
--    lstrip,        -- String -> String
--    rstrip,        -- String -> String
    throwHTSpaceInList,      -- String -> String
    listHead,      -- String -> String
    listHead',     -- String -> String
    listLast,      -- String -> String
    listLast',     -- String -> String
    listTake,      -- Int -> String -> String
    listTake',     -- Int -> String -> String
    takeNElem,     -- Int -> String -> String
    listDrop,      -- Int -> String -> String
    listDrop',     -- Int -> String -> String
    dropNElem,     -- Int -> String -> String
    listLength,    -- String -> Int
    stringToList,      -- String -> [String]
    stringToList',     -- String -> [String]
    listToString,      -- [String] -> String
    stringToTuple,     -- String -> (String,String)
    stringToIntTuple,  -- String -> (Int, Int)
    stringToTriple,    -- String -> (String,String,String)
    stringToQuadruple, -- String -> (String,String,String,String)
    stringToFiveTuple,  -- String -> (String,String,String,String,String)
    stringToSixTuple,  -- String -> (String,String,String,String,String,String)
    indexOfDelimiter,  -- Int -> Int -> Int -> String -> Int
    rewriteBackSlash,  -- String -> string
    quickSort4Int,   -- [Int] -> [Int]
    toDescListOfMapByValue, -- EQ k => [(k,Int)] -> [(k,Int)]
    toAscListOfMapByValue,  -- [(String,Int)] -> [(String,Int)]
    isSubstr,          -- String -> String -> String -> String -> Bool
    txt2csv4WordEmbed, -- String -> IO ()
    getConfProperty,   -- String -> String -> String
    getLineUntil,      -- String -> [String] -> Bool -> IO String
    getNumUntil,       -- Int -> [Int] -> IO Int
    acceptOrNot,       -- a -> String -> IO Maybe a
    compFiveLists,     -- [a] -> [a] -> [a] -> [a] -> [a] -> [[a]]
    dispList,          --  (Eq a, Show a) => Int -> [a] -> IO ()
    RowIdx,        -- Int
    ColIdx,        -- Int
    getMatchedElemPair2SimList,   -- (Eq a, Ord b, Num b) => [((RowIdx, ColIdx), ((a, a), b))] -> [((RowIdx, ColIdx), ((a, a), b))] -> [((RowIdx, ColIdx), ((a, a), b))]
    formatDoubleAList,            -- Show a => [(Double, a)] -> Int -> [(String, a)]
    chunk,         -- Int -> [a] -> [[a]]
    indexOfMin',   -- Ord a => [a] -> Maybe Int
    indexOfMax',   -- Ord a => [a] -> Maybe Int
    fromMaybe',    -- Maybe a -> a
    var,           -- Integral a => [a] -> Float
    BiTree(..),    -- BiTree and its all Constructors
    emptyBiTree,   -- BiTree a
    isEmptyBiTree, -- BiTree a -> Bool
    isNodeBiTree,  -- BiTree a -> Bool
    getLeftSub,    -- BiTree a -> BiTree a
    getRightSub,   -- BiTree a -> BiTree a
    getRoot,       -- BiTree a -> a
    setLeftSub,    -- BiTree a -> BiTree a -> BiTree a
    setRightSub,   -- BiTree a -> BiTree a -> BiTree a
    setRoot,       -- a -> BiTree a -> BiTree a
    traverseBiTree,     -- BiTree a -> [a]
    nodePairsBetwTwOBiTree,   -- BiTree a -> BiTree a -> [(a,a)]
--    stringToBiTree,     -- (Read a) => String -> BiTree a
    forest2BiTree,      -- [BiTree a] -> BiTree a
    jaccardSimIndex,    -- Eq a => [a] -> [a] -> Double
    jaccardSimIndex',   -- Eq a => [[a]] -> Double
    findSubstringIndex, -- Eq a => [a] -> [a] -> Int
    ) where

import Data.Tuple
import Data.List (elemIndex, intersect, union, isPrefixOf, tails)
import Data.Map (Map)
import qualified Data.Map as Map
import qualified Data.String as DS
import Text.Printf
import Data.Maybe

-- Functions on four tuple.

fst4 :: (a,b,c,d) -> a
fst4 (a,_,_,_) = a

snd4 :: (a,b,c,d) -> b
snd4 (_,b,_,_) = b

thd4 :: (a,b,c,d) -> c
thd4 (_,_,c,_) = c

fth4 :: (a,b,c,d) -> d
fth4 (_,_,_,d) = d

-- Functions on five tuple.

fst5 :: (a,b,c,d,e) -> a
fst5 (a,_,_,_,_) = a

snd5 :: (a,b,c,d,e) -> b
snd5 (_,b,_,_,_) = b

thd5 :: (a,b,c,d,e) -> c
thd5 (_,_,c,_,_) = c

fth5 :: (a,b,c,d,e) -> d
fth5 (_,_,_,d,_) = d

fif5 :: (a,b,c,d,e) -> e
fif5 (_,_,_,_,e) = e

-- Functions on six tuple.

fst6 :: (a,b,c,d,e,f) -> a
fst6 (a,_,_,_,_,_) = a

snd6 :: (a,b,c,d,e,f) -> b
snd6 (_,b,_,_,_,_) = b

thd6 :: (a,b,c,d,e,f) -> c
thd6 (_,_,c,_,_,_) = c

fth6 :: (a,b,c,d,e,f) -> d
fth6 (_,_,_,d,_,_) = d

fif6 :: (a,b,c,d,e,f) -> e
fif6 (_,_,_,_,e,_) = e

sth6 :: (a,b,c,d,e,f) -> f
sth6 (_,_,_,_,_,f) = f

-- Functions on seven tuple.

fst7 :: (a,b,c,d,e,f,g) -> a
fst7 (a,_,_,_,_,_,_) = a

snd7 :: (a,b,c,d,e,f,g) -> b
snd7 (_,b,_,_,_,_,_) = b

thd7 :: (a,b,c,d,e,f,g) -> c
thd7 (_,_,c,_,_,_,_) = c

fth7 :: (a,b,c,d,e,f,g) -> d
fth7 (_,_,_,d,_,_,_) = d

fif7 :: (a,b,c,d,e,f,g) -> e
fif7 (_,_,_,_,e,_,_) = e

sth7 :: (a,b,c,d,e,f,g) -> f
sth7 (_,_,_,_,_,f,_) = f

svt7 :: (a,b,c,d,e,f,g) -> g
svt7 (_,_,_,_,_,_,g) = g

-- Functions on eight tuple.

fst8 :: (a,b,c,d,e,f,g,h) -> a
fst8 (a,_,_,_,_,_,_,_) = a

snd8 :: (a,b,c,d,e,f,g,h) -> b
snd8 (_,b,_,_,_,_,_,_) = b

thd8 :: (a,b,c,d,e,f,g,h) -> c
thd8 (_,_,c,_,_,_,_,_) = c

fth8 :: (a,b,c,d,e,f,g,h) -> d
fth8 (_,_,_,d,_,_,_,_) = d

fif8 :: (a,b,c,d,e,f,g,h) -> e
fif8 (_,_,_,_,e,_,_,_) = e

sth8 :: (a,b,c,d,e,f,g,h) -> f
sth8 (_,_,_,_,_,f,_,_) = f

svt8 :: (a,b,c,d,e,f,g,h) -> g
svt8 (_,_,_,_,_,_,g,_) = g

eth8 :: (a,b,c,d,e,f,g,h) -> h
eth8 (_,_,_,_,_,_,_,h) = h

{- Remove duplicate elements in a list.
 - Actually, Data.List.nub is better than the following removeDup series of functions.
 -}

removeDup :: Eq a => [a] -> [a]
removeDup [] = []
removeDup [x] = [x]
removeDup (x:xs)
    | elem x xs = removeDup xs
    | otherwise = x:(removeDup xs)

removeDup' :: Eq a => [a] -> [a] -> [a]
removeDup' [] newList = newList
removeDup' (x:xs) newList = removeDup' xs newList'
    where
      newList' = case notElem x newList of
                   True -> newList ++ [x]
                   False -> newList

-- Remove duplicate 2-tuples. For (t1,t2) and (t2,t1), only the first is remained.
removeTuple :: Eq a => [(a,a)] -> [(a,a)]
removeTuple [] = []
removeTuple [t] = [t]
removeTuple ts
    | elem lt it = removeTuple it
    | elem (swap lt) it = removeTuple it
    | otherwise = (removeTuple it) ++ [lt]
    where
      lt = last ts
      it = init ts

-- Convert a list of tuples into a list of elements.
tupToList :: [(a,a)] -> [a]
tupToList tupList = (fst tup) ++ (snd tup)
    where
      tup = unzip tupList

-- Throw out brackets.
throwBrac :: String -> String
throwBrac [] = []
throwBrac (c:cs)
    | elem c ['(',')','[',']'] = throwBrac cs
    | otherwise = c:(throwBrac cs)

-- Throw out the first <n> bracket, no mather which are '(', ')', '[', or ']'.
throwFstBrac :: Int -> String -> String
throwFstBrac _ [] = []
throwFstBrac 0 cs = cs
throwFstBrac n (c:cs)
    | elem c ['(',')','[',']'] = throwFstBrac (n-1) cs
    | otherwise = c:(throwFstBrac n cs)

-- Throw out the Last <n> bracket, no mather which are '(', ')', '[', or ']'.
throwLastBrac :: Int -> String -> String
throwLastBrac _ [] = []
throwLastBrac 0 cs = cs
throwLastBrac n cs
    | elem (last cs) ['(',')','[',']'] = throwLastBrac (n-1) (init cs)
    | otherwise = throwLastBrac n (init cs) ++ [last cs]

-- Split a string with designated delimiter.
splitAtDeli :: Char -> String -> [String]
splitAtDeli _ "" = []
splitAtDeli c cs
    | i /= -1 = (take i cs) : splitAtDeli c (drop (i+1) cs)
    | otherwise = [cs]
    where
      ind = elemIndex c cs
      i = maybe (-1) (0+) ind     -- Result -1 for no delimiter.

-- Split a string with designated delimiter, and every substring throws out its left and right whitespaces.
splitAtDeliThrowSpace :: Char -> String -> [String]
splitAtDeliThrowSpace _ "" = []
splitAtDeliThrowSpace c cs
    | i /= -1 = (throwHTSpace (take i cs)) : splitAtDeli c (drop (i+1) cs)
    | otherwise = [cs]
    where
      ind = elemIndex c cs
      i = maybe (-1) (0+) ind     -- Result -1 for no delimiter.

-- Split a string with designated delimiter at its first place.
splitAtDeliAtFP :: Char -> String -> [String]
splitAtDeliAtFP _ "" = []
splitAtDeliAtFP c cs
    | i == -1 = [cs]
    | i == 0 = [drop 1 cs]
    | i == length cs - 1 = [take i cs]
    | otherwise = [take i cs, drop (i+1) cs]
    where
      ind = elemIndex c cs
      i = maybe (-1) (0+) ind     -- Result -1 for no delimiter.

{- Split a C2CCG Tag as type-conversional and CCG calculus tags.
 - For ">", only calculus tag ">" is there.
 - For "S/v->", the conversional tag is "S/v", and the calculus tag is ">".
 - For "S/v-P/a-<", the conversional tag is "S/v", "P/a", and the calculus tag is "<", here conversional tag "S/v-P/a" is omited.
 -}
splitTagAsConvOrCal :: String -> [String]
splitTagAsConvOrCal "" = []
splitTagAsConvOrCal tag
    | tag!!0 /= '<' && tag!!0 /= '>' = (splitAtDeliAtFP '-' tag)!!0 : (splitTagAsConvOrCal ((splitAtDeliAtFP '-' tag)!!1))
    | otherwise = [tag]

-- Calculate the string-length maximum in a String list.
maxStrLen :: [String] -> Int
maxStrLen [] = 0
maxStrLen cs = foldr max 0 (map length cs)

-- Double every back slash in a string to adapt inserting it as value into MySQL table.
doubleBackSlash :: String -> String
doubleBackSlash "" = ""
doubleBackSlash (c:cs)
    | c == '\\' = "\\\\" ++ doubleBackSlash cs
    | otherwise = c : doubleBackSlash cs

putNStr :: [String] -> IO ()
putNStr [] = putStrLn ""
putNStr (s:ss) = do
    putStr s
    if ss /= []
      then do
        putStr ", "
        putNStr ss
      else putNStr ss

-- Throw away head spaces and tail spaces in a string.
throwHTSpace :: String -> String
throwHTSpace str = rstrip (lstrip str)

-- Implement of function Data.String.Utils.lstrip
lstrip :: String -> String
lstrip [] = []
lstrip (x:xs)
    | x == ' ' = lstrip xs
    | otherwise = x : xs

-- Implement of function Data.String.Utils.rstrip
rstrip :: String -> String
rstrip [] = []
rstrip xs
    | last xs == ' ' = rstrip $ init xs
    | otherwise = xs

{- Throw away head spaces and tail spaces in each string element as well as head spaces and tail spaces of the whole
   string list, here the string list is converted from a list.
   For an example, " [ I , am, a , student ]  " will become "[I,am,a,student]"
 -}
throwHTSpaceInList :: String -> String
throwHTSpaceInList listStr = listToString $ map throwHTSpace (stringToList listStr)

{- The following functions are named <list****>, which handle a string as "[x,y,z]", here 'x', 'y', and 'z' may be
   some certain types in this application.
   Function listHead to get the String of list head from the String of this list, which wraps Function listHead' to
   throw off head and tail spaces.
 -}
listHead :: String -> String
listHead str
    | headElem == "" = error "listHead: Empty list."
    | otherwise = headElem
    where
      headElem = throwHTSpace $ listHead' str

-- Function listHead' to get the String of list head from the String of a list.
listHead' :: String -> String
listHead' listStr
    | listStr' == "" = error "listHead': This is a empty string."
    | head listStr' /= '[' || last listStr' /= ']' = error "listHead': This is not the string of a list."
    | idx == -1 = listStr''
    | otherwise = take idx listStr''
    where
      listStr' = throwHTSpace listStr
      listStr'' = init (tail listStr')
      idx = indexOfDelimiter 0 0 0 ',' listStr''

{- Function listLast to get the String of last element from the String of this list, which wraps Function listLast'
   to throw off head and tail spaces.
 -}
listLast :: String -> String
listLast str
    | lastElem == "" = error "listLast: Empty last."
    | otherwise = lastElem
    where
      lastElem = throwHTSpace $ listLast' str

-- Function listLast' to get the String of last element from the String of this list,.
listLast' :: String -> String
listLast' listStr
    | listStr' == "" = error "listLast: This is a empty string."
    | head listStr' /= '[' || last listStr' /= ']' = error "listLast': This is not the string of a list."
    | idx == -1 = listStr''
    | otherwise = listLast' ("[" ++ drop (idx + 1) listStr'' ++ "]")
    where
      listStr' = throwHTSpace listStr
      listStr'' = init (tail listStr')
      idx = indexOfDelimiter 0 0 0 ',' listStr''

{- Function listTake to get the first n elements from the String of a list, which wraps Function listTake' to throw
   off head and tail spaces in each element.
 -}
listTake :: Int -> String -> String
listTake n listStr = throwHTSpaceInList (listTake' n listStr)

-- The wrapper of Function takeNElem, using '[' and ']' to bracket the result of Function takeNElem.
listTake' :: Int -> String -> String
listTake' n listStr
    | head listStr' /= '[' || last listStr' /= ']' = error "listTake': This is not a string of a list."
    | otherwise = "[" ++ takeNElem n (tail (init listStr')) ++ "]"
    where
      listStr' = throwHTSpace listStr

-- Take the String of first n elements from the String of a comma-seperated element sequence.
takeNElem :: Int -> String -> String
takeNElem n listStr
    | n == 0 || listStr == "" = ""
    | n == 1 && idx /= -1 = take idx listStr
    | n == 1 = listStr                               -- Only one element.
    | n /= 1 && idx /= -1 = take idx listStr ++ "," ++ (takeNElem (n - 1) (drop (idx + 1) listStr))
    | otherwise = listStr
    where
      idx = indexOfDelimiter 0 0 0 ',' listStr

{- Function listDrop to remove the substring of first n elements from the String of a list, which wraps Function
   listDrop' to throw off head and tail spaces in each element.
 -}
listDrop :: Int -> String -> String
listDrop n listStr = throwHTSpaceInList (listDrop' n listStr)

{- The wrapper of Function dropNElem, using '[' and ']' to bracket the result of Function dropNElem. This function
   drops n elements from the beginning of a list.
 -}
listDrop' :: Int -> String -> String
listDrop' n listStr
    | head listStr' /= '[' || last listStr' /= ']' = error "listDrop': This is not a string of a list."
    | otherwise = "[" ++ dropNElem n (tail (init listStr')) ++ "]"
    where
      listStr' = throwHTSpace listStr

-- Drop the substring of first n elements from the String of a comma-seperated element sequence.
dropNElem :: Int -> String -> String
dropNElem n listStr
    | n == 0 = listStr
    | listStr == "" = ""
    | idx == -1 = ""
    | otherwise = dropNElem (n - 1) (drop (idx + 1) listStr)
    where
      idx = indexOfDelimiter 0 0 0 ',' listStr

-- Get the length of a list which is getten from a string of a list.
listLength :: String -> Int
listLength str = length (stringToList str)

-- Get [String] from a String of element list.
stringToList :: String -> [String]
stringToList str
    | head str' /= '[' || last str' /= ']' = error "stringToList: This is not the string of a list."
    | str'' == "" = []
    | idx == -1 = [str'']
    | otherwise = throwHTSpace (take idx str'') : (stringToList ("[" ++ drop (idx + 1) str'' ++ "]"))
    where
      str' = throwHTSpace str                   -- Throw away head spaces and tail spaces.
      str'' = throwHTSpace $ init (tail str')   -- Throw away '[' and ']', then remove head and tail spaces.
      idx = indexOfDelimiter 0 0 0 ',' str''

{- Without considering to throw away the head and tail whitespaces of every element, directly get [String] from a String of element list.
 - This function is used to read string list where no redundant spaces exist.
 -}
stringToList' :: String -> [String]
stringToList' str
    | head str /= '[' || last str /= ']' = error "stringToList: This is not the string of a list."
    | str' == "" = []
    | idx == -1 = [str']
    | otherwise =  (take idx str') : (stringToList' ("[" ++ drop (idx + 1) str' ++ "]"))
    where
      str' = init (tail str)   -- Throw away '[' and ']'
      idx = indexOfDelimiter 0 0 0 ',' str'

-- Get the String from a [String].
listToString :: [String] -> String
listToString [] = "[]"
listToString [s] = "[" ++ s ++ "]"
listToString (s:ss) = "[" ++ s ++ "," ++ init (tail (listToString ss)) ++ "]"

-- Get (String, String) from the String of a tuple.
stringToTuple :: String -> (String, String)
stringToTuple str = (first, second)
    where
      str' = "[" ++ init (tail (throwHTSpace str)) ++ "]"
      first = listHead str'
      second = listLast str'

-- Get (Int, Int) from its string.
stringToIntTuple :: String -> (Int, Int)
stringToIntTuple str = (read first :: Int, read second :: Int)
    where
    (first, second) = stringToTuple str

-- Get (String, String, String) from the String of a triple.
stringToTriple :: String -> (String, String, String)
stringToTriple str = (first, second, third)
    where
      str' = "[" ++ init (tail (throwHTSpace str)) ++ "]"
      first = listHead str'
      second = listHead (listDrop 1 str')
      third = listLast str'

-- Get (String, String, String, String) from the String of a quadruple.
stringToQuadruple :: String -> (String, String, String, String)
stringToQuadruple str = (first, second, third, fourth)
    where
      str' = "[" ++ init (tail (throwHTSpace str)) ++ "]"
      first = listHead str'
      second = listHead (listDrop 1 str')
      third = listHead (listDrop 2 str')
      fourth = listLast str'

-- Get (String, String, String, String, String) from the String of a five-tuple.
stringToFiveTuple :: String -> (String, String, String, String, String)
stringToFiveTuple str = (first, second, third, fourth, fifth)
    where
      str' = "[" ++ init (tail (throwHTSpace str)) ++ "]"
      first = listHead str'
      second = listHead (listDrop 1 str')
      third = listHead (listDrop 2 str')
      fourth = listHead (listDrop 3 str')
      fifth = listLast str'

-- Get (String, String, String, String, String, String) from the String of a six-tuple.
stringToSixTuple :: String -> (String, String, String, String, String, String)
stringToSixTuple str = (first, second, third, fourth, fifth, sixth)
    where
      str' = "[" ++ init (tail (throwHTSpace str)) ++ "]"
      first = listHead str'
      second = listHead (listDrop 1 str')
      third = listHead (listDrop 2 str')
      fourth = listHead (listDrop 3 str')
      fifth = listHead (listDrop 4 str')
      sixth = listLast str'

{- Get the index of first toppest delimiter such as ',' or ';'. Toppest delimiters are those which don't be embedded
   in any list element. The index is initialized as 0, and will be -1 when meeting an empty list. To remember how
   many left parentheses '(' and square brackets '[' have been met, the integers 'nlp' and 'nlb' are needed. The
   index is initialized as 0.
 -}
indexOfDelimiter :: Int -> Int -> Int -> Char -> String -> Int
indexOfDelimiter nlp nlb i de str
    | i == length str = -1
    | x == '(' = indexOfDelimiter (nlp + 1) nlb (i+1) de str
    | x == ')' = indexOfDelimiter (nlp - 1) nlb (i+1) de str
    | x == '[' = indexOfDelimiter nlp (nlb + 1) (i+1) de str
    | x == ']' = indexOfDelimiter nlp (nlb - 1) (i+1) de str
    | x == de && nlp == 0 && nlb == 0 = i
    | otherwise = indexOfDelimiter nlp nlb (i+1) de str
    where
      x = str!!i

-- Rewrite twice for every back slash in a string.
rewriteBackSlash :: String -> String
rewriteBackSlash ('\\':xs) = '\\' : ('\\' : rewriteBackSlash xs)
rewriteBackSlash (x:xs) = x : rewriteBackSlash xs
rewriteBackSlash ""           = ""

-- Quick sort for Integers.
quickSort4Int :: [Int] -> [Int]
quickSort4Int [] = []
quickSort4Int (i:is) = (quickSort4Int [x|x<-is, x<i]) ++ [i] ++ (quickSort4Int [x|x<-is, x>=i])

-- Get ascending list of a Map by its values.
toAscListOfMapByValue :: Eq k => [(k, Int)] -> [(k, Int)]
toAscListOfMapByValue [] = []
toAscListOfMapByValue [x] = [x]
toAscListOfMapByValue (t:ts) = (toAscListOfMapByValue [x | x <- ts, snd x < snd t]) ++ [t] ++ (toAscListOfMapByValue [x | x <- ts, snd x >= snd t])

-- Get descending list of a Map by its values, where 'k' can be String, Int, or any type only if it is equality-decidable.
toDescListOfMapByValue :: Eq k => [(k, Int)] -> [(k, Int)]
toDescListOfMapByValue [] = []
toDescListOfMapByValue [x] = [x]
toDescListOfMapByValue (t:ts) = (toDescListOfMapByValue [x | x <- ts, snd x > snd t]) ++ [t] ++ (toDescListOfMapByValue [x | x <- ts, snd x <= snd t])

-- Decide the first string is a substring of the second, here the third and the fourth strings are origial first string and second string.
isSubstr :: String -> String -> String -> String -> Bool
isSubstr [] _ _ _ = True         -- Including [] is a substring of []
isSubstr _ [] _ _ = False
isSubstr str1 str2 str3 str4
    | (str1!!0) == (str2!!0) = isSubstr (tail str1) (tail str2) str3 str4
    | otherwise = isSubstr str3 (tail str4) str3 (tail str4)

{- Get csv file from txt file which includes vector semantics of Chinese words and phrases.
 - Format of txt file:
 -    k lines of meta data
 -    word1 float1 float2 ... floatn
 -    word2 float1 float2 ... floatn
 -    ...
 - Format of csv file:
 -    k,lines,of,meta,data
 -    word1,float1,float2,...,floatn
 -    word2,float1,float2,...,floatn
 -    ...
 -}
txt2csv4WordEmbed :: String -> String -> IO ()
txt2csv4WordEmbed txtFile csvFile = do
    txtInfo <- readFile txtFile
    let txtLines = lines txtInfo
    let csvInfo = unlines $ map replaceBlankByComma txtLines
    writeFile csvFile csvInfo

-- Replace all blanks with comma.
replaceBlankByComma :: String -> String
replaceBlankByComma [] = []
replaceBlankByComma (x:xs)
    | x == ' ' = ',' : replaceBlankByComma xs
    | otherwise = x : replaceBlankByComma xs

-- Get a property value by a given property name in configuration text.
getConfProperty :: String -> String -> String
getConfProperty propName confInfo = case propValue of
    Just x -> x
    Nothing -> ""
    where
      confInfo' = [kv | kv <- DS.lines confInfo, length kv /= 0]                -- Remove blank lines
      validConfInfo = [kv | kv <- confInfo', kv!!0 /= '#']                      -- Remove the lines starting from '#'
      confTupleSeq = [(head keyValue, last keyValue)| keyValue <- map (splitAtDeli ':') validConfInfo]
      kvMap = kvListToMap confTupleSeq Map.empty
      propValue = Map.lookup propName kvMap

-- Store key-value pair into Map String String.
kvListToMap :: [(String, String)] -> Map String String -> Map String String
kvListToMap [] m = m
kvListToMap (s:ss) m = kvListToMap ss (Map.insert (fst s) (snd s) m)

{- Read input repeatedly until the input string is in designated string set.
 - Suppose the string set is not empty.
 - If the input string is empty string, namely pressing RETURN, and
 -   (1) the default flag is True, return the first element of the string set;
 -   (2) the default flag is False, return the last element of the string set.
 -}
getLineUntil :: String -> [String] -> Bool -> IO String
getLineUntil _ [] _ = return "getLineUntil: ZeroRange"
getLineUntil prompt cs flag = do
    putStr prompt
    input <- getLine
    if elem input cs
      then return input
      else if input == ""
             then if flag == True
                    then return (head cs)
                    else return (last cs)
             else getLineUntil prompt cs flag

{- Read input number repeatedly until it is in designated number set.
 - Suppose the number set is not empty.
 - If the input number is null, namely pressing RETURN, return the first element of the number set;
 -}
getNumUntil :: String -> [Int] -> IO Int
getNumUntil _ [] = return 0       -- '0' denote exception.
getNumUntil prompt is = do
    putStr prompt
    inputStr <- getLine
    if inputStr == ""
      then return (head is)
      else do
        let input = read inputStr :: Int
        if elem input is
          then return input
          else getNumUntil prompt is

-- According to user opinion, accept or deny the input.
acceptOrNot :: a -> String -> IO (Maybe a)
acceptOrNot a prompt = do
    yn <- getLineUntil prompt ["y","n"] True
    if yn == "y"
      then return (Just a)
      else return Nothing

-- Composite corresponding elements from five lists to form a list of lists.
compFiveLists :: [a] -> [a] -> [a] -> [a] -> [a] -> [[a]]
compFiveLists [] _ _ _ _ = []
compFiveLists _ [] _ _ _ = []
compFiveLists _ _ [] _ _ = []
compFiveLists _ _ _ [] _ = []
compFiveLists _ _ _ _ [] = []
compFiveLists (a:as) (b:bs) (c:cs) (d:ds) (e:es) = [a,b,c,d,e] : compFiveLists as bs cs ds es

-- Display list according given element number per line. Element type is supposed not String and can be shown.
dispList :: (Eq a, Show a) => Int -> [a] -> IO ()
dispList numOfElem xs
    | numOfElem < 1 = error "dispList: numOfElem < 1"
    | length xs <= numOfElem = dispELemList xs
    | otherwise = do
                    dispELemList (take numOfElem xs)
                    dispList numOfElem (drop numOfElem xs)

-- Display all elements in a list, using commas to seperate them.
dispELemList :: (Eq a, Show a) => [a] -> IO ()
dispELemList [] = putStrLn ""
dispELemList (x:xs) = do
    putStr (show x)
    if xs /= []
      then do
        putStr ", "
        dispELemList xs
      else
        putStrLn ""

{- Match element pairs which have similarity degrees from high to low.
 - (1) Suppose similarity degree matrix M = [((xIdx, yIdx), ((a,b),sim(a,b)))], a and b are phrases, and sim(a,b) is the similarity degree of (a,b);
 -     Here, xIdx :: [1 .. m], yIdx :: [1 .. n], so the matrix size is (m >< n).
 - (2) If ((x, y), ((c,d),sim(c,d))) is the maximum in M, Move ((c,d), sim(c,d)) from M to L, L stores matching result, while M is modified as
 -     [e | e <-M, (fst . fst) e /= x, (snd . fst) e /= y];
 -     Repeat (1)(2) until M is empty.
 - (3) return L.
 -}

type RowIdx = Int
type ColIdx = Int

getMatchedElemPair2SimList :: (Eq a, Ord b, Num b) => [((RowIdx, ColIdx), ((a, a), b))] -> [((RowIdx, ColIdx), ((a, a), b))] -> [((RowIdx, ColIdx), ((a, a), b))]
getMatchedElemPair2SimList matchedElemPair2SimList [] = matchedElemPair2SimList
getMatchedElemPair2SimList matchedElemPair2SimList elemPair2SimMatrix = getMatchedElemPair2SimList matchedElemPair2SimList' elemPair2SimMatrix'
    where
    simList = map (snd . snd) elemPair2SimMatrix                                -- [SimDeg]
    maxSim = maximum simList
    idx = case (elemIndex maxSim simList) of
            Just x -> x
            Nothing -> error "getMatchedElemPair2SimList: Impossible."
    elemWithMaxSim = elemPair2SimMatrix!!idx
    (x, y) = fst elemWithMaxSim                                                 -- (xIdx, yIdx)
    matchedElemPair2SimList' = matchedElemPair2SimList ++ [elemWithMaxSim]
    elemPair2SimMatrix' = [e | e <- elemPair2SimMatrix, (fst . fst) e /= x, (snd . fst) e /= y]        -- Remove elements at row x and column y.

{- Get the format print of [(Double, a)] with given decimal places to represent Double values.
 -}
formatDoubleAList :: Show a => [(Double, a)] -> Int -> [(String, a)]       -- 'a' may be any showable type, Polymorphism!
formatDoubleAList doubleAList n = map (\x -> (printf ("%." ++ show n ++"f") (fst x), snd x)) doubleAList

-- Divide a list into some sublists with same number of elements.
chunk :: Int -> [a] -> [[a]]
chunk _ [] = []
chunk n xs = let (group, rest) = splitAt n xs
             in group : chunk n rest

-- One efficient function to find the index of one element which is the smallest in a list.
indexOfMin' :: Ord a => [a] -> Maybe Int
indexOfMin' [] = Nothing
indexOfMin' xs = Just (snd (foldl1 comparePair (zip xs [0..])))
  where
    comparePair (x1, i1) (x2, i2)
      | x1 < x2    = (x1, i1)
      | otherwise  = (x2, i2)

-- One efficient function to find the index of one element which is the bigest in a list.
indexOfMax' :: Ord a => [a] -> Maybe Int
indexOfMax' [] = Nothing
indexOfMax' xs = Just (snd (foldl1 comparePair (zip xs [0..])))
  where
    comparePair (x1, i1) (x2, i2)
      | x1 < x2    = (x2, i2)
      | otherwise  = (x1, i1)

-- Extract the inner value from a Maybe box.
fromMaybe' :: Maybe a -> a
fromMaybe' x = case x of
                 Just v -> v
                 Nothing -> error "fromMaybe': Exception."

-- Population variance of integer samples.
var :: [Int] -> Float
var as = variance
  where
    num = length as
    mean = fromIntegral (sum as) / fromIntegral num :: Float
    variance = sum (map (\x -> (fromIntegral x - mean) ^ 2) as) / fromIntegral num

-- Binary tree
data BiTree a = Empty | Node a (BiTree a) (BiTree a) deriving (Eq)

-- Empty binary tree
emptyBiTree :: BiTree a
emptyBiTree = Empty

-- isEmptyBiTree :: BiTree a -> Bool
isEmptyBiTree Empty = True
isEmptyBiTree _ = False

-- isNodeBiTree :: BiTree a -> Bool
isNodeBiTree Empty = False
isNodeBiTree _ = True

-- Get left subtree of a binary tree.
getLeftSub :: BiTree a -> BiTree a
getLeftSub Empty = error "getLeftSub: Empty"
getLeftSub (Node _ t1 _) = t1

-- Get right subtree of a binary tree.
getRightSub :: BiTree a -> BiTree a
getRightSub Empty = error "getRightSub: Empty"
getRightSub (Node _ _ t2) = t2

-- Get root value of a binary tree.
getRoot :: BiTree a -> a
getRoot Empty = error "getRoot: Empty"
getRoot (Node r _ _) = r

-- Set left subtree of a binary tree.
setLeftSub :: BiTree a -> BiTree a -> BiTree a
setLeftSub _ Empty = error "setLeftSub: Empty"
setLeftSub t1 (Node r _ t2) = (Node r t1 t2)

-- Set right subtree of a binary tree.
setRightSub :: BiTree a -> BiTree a -> BiTree a
setRightSub _ Empty = error "setRightSub: Empty"
setRightSub t2 (Node r t1 _) = (Node r t1 t2)

-- Set root value of a binary tree.
setRoot :: a -> BiTree a -> BiTree a
setRoot _ Empty = error "setRoot: Empty"
setRoot r (Node _ t1 t2) = (Node r t1 t2)

-- Define relation Ord between two BiTree values.
instance Ord a => Ord (BiTree a) where
    compare Empty Empty = EQ
    compare Empty _ = LT
    compare _ Empty = GT
    compare (Node r1 t11 t12) (Node r2 t21 t22) =
      case compare r1 r2 of
        EQ -> case compare t11 t21 of
                EQ -> case compare t12 t22 of
                        EQ -> EQ
                        LT -> LT
                        GT -> GT
                LT -> LT
                GT -> GT
        LT -> LT
        GT -> GT

-- Define how a BiTree value shows.
instance Show a => Show (BiTree a) where
    show Empty = "()"
    show (Node r t1 t2) = "(" ++ show r ++ "," ++ show t1 ++ "," ++ show t2 ++ ")"

-- Traverse a BiTree instance to get node list.
traverseBiTree :: BiTree a -> [a]
traverseBiTree Empty = []
traverseBiTree (Node root leftSub rightSub) = root : traverseBiTree leftSub ++ traverseBiTree rightSub

-- Non-recursive algorithm for geting node pairs between two BiTree instances.
nodePairsBetwTwOBiTree :: BiTree a -> BiTree a -> [(a,a)]
nodePairsBetwTwOBiTree t1 t2 = go [(t1, t2)]
    where
      go [] = []
      go ((Empty,Empty) : rest) = go rest
      go ((Empty,_) : rest) = go rest
      go ((_,Empty) : rest) = go rest
      go ((Node r1 lst1 rst1, Node r2 lst2 rst2) : rest) = (r1,r2) : go ((lst1,lst2) : (rst1,rst2) : rest)

{- Create a binary tree from a string.
 - Has NOT debugged.
stringToBiTree :: (Read a) => String -> BiTree a
stringToBiTree "()" = Empty
stringToBiTree str = Node (read rStr :: a) (stringToBiTree lstStr) (stringToBiTree rstStr)
    where
    (rStr, lstStr, rstStr) = stringToTriple str
 -}

{- Create a binary tree from a forest of binary trees.
 - Binary trees for Categorial Grammar are those not including 1-degree nodes, so left-child right-sibling algorithm is used.
 -}
forest2BiTree :: [BiTree a] -> BiTree a
forest2BiTree [] = Empty
forest2BiTree ((Node r Empty Empty):ts) = Node r Empty (forest2BiTree ts)
forest2BiTree ((Node r lst rst):ts) = Node r (forest2BiTree [lst, rst]) (forest2BiTree ts)
forest2BiTree _ = error "forest2BiTree: A binary tree includes 1-degree nodes."

{- Jaccard Similarity Index is one kind of similarity metric between two sets.
 - The index equals to the ratio of cardinality of intersection to cardinality of union between two sets.
 -}
jaccardSimIndex :: (Eq a) => [a] -> [a] -> Double
jaccardSimIndex [] [] = 1.0
jaccardSimIndex [] _ = 0.0
jaccardSimIndex _ [] = 0.0
jaccardSimIndex s1 s2 = (fromIntegral (length (intersect s1 s2))) / (fromIntegral (length (union s1 s2)))

{- Another version of Jaccard Similarity Index is used as the similarity between one set and more than one set.
 - The index equals to the average value of similarity between the set and every one in a collection of sets.
 -}
jaccardSimIndex' :: (Eq a) => [a] -> [[a]] -> Double
jaccardSimIndex' s1 ss = foldl (+) 0.0 indices / ((fromIntegral . length) indices)
    where
    indices = map (jaccardSimIndex s1) ss

-- Find index where a substring starts in a list. If the substring is NOT in the list, return -1.
findSubstringIndex :: Eq a => [a] -> [a] -> Int
findSubstringIndex sub str = fromMaybe (-1) $ elemIndex True $ map (isPrefixOf sub) (tails str)
