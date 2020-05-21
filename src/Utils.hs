-- Copyright (c) 2019-2020 China University of Water Resources and Electric Power,
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
    splitAtDeli,   -- Char -> String -> [String]
    maxStrLen,     -- [String] -> Int
    doubleBackSlash,   -- String -> String
    putNStr   
    ) where

import Data.Tuple
import Data.List

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

-- Split a string with designated delimiter.
splitAtDeli :: Char -> String -> [String]
splitAtDeli _ "" = []
splitAtDeli c cs
    | i /= -1 = (take i cs) : splitAtDeli c (drop (i+1) cs)
    | otherwise = [cs]
    where
      ind = elemIndex c cs
      i = maybe (-1) (0+) ind     -- Result -1 for no delimiter.

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

