-- Copyright (c) 2019-2023 China University of Water Resources and Electric Power,
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
    removeDup,     -- Eq a => [a] -> [a]
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
    listToString,      -- [String] -> String
    stringToTriple,    -- String -> (String,String,String)
    indexOfDelimiter,  -- Int -> Int -> Int -> String -> Int
    rewriteBackSlash,  -- String -> string
    quickSortForInt,   -- [Int] -> [Int]
    toDescListOfMapByValue, -- [(String,Int)] -> [(String,Int)]
    toAscListOfMapByValue,  -- [(String,Int)] -> [(String,Int)]
    isSubstr,          -- String -> String -> String -> String -> Bool
    txt2csv4WordEmbed, -- String -> IO ()
    getConfProperty,   -- String -> String -> String
    getLineUntil,      -- [String] -> IO String
    acceptOrNot        -- a -> String -> IO Maybe a
    ) where

import Data.Tuple
import Data.List
import Data.String.Utils
import Data.Map (Map)
import qualified Data.Map as Map
import qualified Data.String as DS

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

-- Remove duplicate elements in a list.

removeDup :: Eq a => [a] -> [a]
removeDup [] = []
removeDup [x] = [x]
removeDup (x:xs)
    | elem x xs = removeDup xs
    | otherwise = x:(removeDup xs)

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
tupToList :: Eq a => [(a,a)] -> [a]
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
    | i /= -1 = (lstrip (rstrip (take i cs))) : splitAtDeli c (drop (i+1) cs)
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

-- Throw away head spaces and tail spaces from a character string.
throwHTSpace :: String -> String
throwHTSpace cs = lstrip $ rstrip cs

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

-- Get the String from a [String].
listToString :: [String] -> String
listToString [] = "[]"
listToString [s] = "[" ++ s ++ "]"
listToString (s:ss) = "[" ++ s ++ "," ++ init (tail (listToString ss)) ++ "]"

-- Get (String, String, String) from the String of a triple.
stringToTriple :: String -> (String, String, String)
stringToTriple str = (first, second, third)
    where
      str' = "[" ++ init (tail (throwHTSpace str)) ++ "]"
      first = listHead str'
      second = listHead (listDrop 1 str')
      third = listLast str'

{- Get the index of first toppest delimiter such ',' or ';'. Toppest delimiters are those which don't be embedded
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
    | x == ',' && nlp == 0 && nlb == 0 = i
    | otherwise = indexOfDelimiter nlp nlb (i+1) de str
    where
      x = str!!i

-- Rewrite twice for every back slash in a string.
rewriteBackSlash :: String -> String
rewriteBackSlash ('\\':xs) = '\\' : ('\\' : rewriteBackSlash xs)
rewriteBackSlash (x:xs) = x : rewriteBackSlash xs
rewriteBackSlash ""           = ""

-- Quick sort for Integers.
quickSortForInt :: [Int] -> [Int]
quickSortForInt [] = []
quickSortForInt (i:is) = (quickSortForInt [x|x<-is, x<i]) ++ [i] ++ (quickSortForInt [x|x<-is, x>=i])

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

-- Replace the first blank by comma. If FoundFirstBlank is true, that represents the first blank has been found and replaced.
replaceFirstBlankByComma :: Bool -> String -> String
replaceFirstBlankByComma _ [] = []
replaceFirstBlankByComma True xs = xs
replaceFirstBlankByComma False (x:xs)
    | x == ' ' = ',' : xs
    | otherwise = x : replaceFirstBlankByComma False xs

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

-- Read input repeatedly until the input string is in designated string set.
getLineUntil :: String -> [String] -> IO String
getLineUntil prompt cs = do
    putStr prompt
    read <- getLine
    if elem read cs
      then return read
      else getLineUntil prompt cs

-- According to user opinion, accept or deny the input.
acceptOrNot :: a -> String -> IO (Maybe a)
acceptOrNot a prompt = do
    yn <- getLineUntil prompt ["y","n",""]
    if yn == "y" || yn == ""
      then return (Just a)
      else return Nothing
