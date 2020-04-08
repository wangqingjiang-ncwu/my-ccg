-- Copyright (c) 2019-2020 China University of Water Resources and Electric Power,
-- All rights reserved.

module Utils (
    Start,         -- Int
    Span,          -- Int
    Tag,           -- String
    Seman,         -- String
    PhraStru,      -- String
    Act,           -- Bool
    SecStart,      -- Int
    OnOff,         -- String
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
    PhraCate,      -- ((Start, Span), [(Category, Tag, Seman, PhraStru, Act)], SecStart)
    pcBelong,      -- PhraCate -> PhraCate -> Bool
    pcBelong',     -- PhraCate -> PhraCate -> Bool
    stOfCate,      -- PhraCate -> Start
    spOfCate,      -- PhraCate -> Span
    ctscaOfCate,    -- PhraCate -> [(Category, Tag, Seman, PhraStru, Act)]
    ctscaOfActCate, -- PhraCate -> [(Category, Tag, Seman, PhraStru, Act)]
    ctscOfCate,    -- PhraCate -> [(Category, Tag, Seman, PhraStru)]
    ctscOfActCate, -- PhraCate -> [(Category, Tag, Seman, PhraStru)]
    cscOfCate,     -- PhraCate -> [(Category, Seman, PhraStru)]
    cscOfActCate,  -- PhraCate -> [(Category, Seman, PhraStru)]
    caOfCate,      -- PhraCate -> [Category]
    caOfActCate,   -- PhraCate -> [Category]
    taOfCate,      -- PhraCate -> [Tag]
    taOfActCate,   -- PhraCate -> [Tag]
    seOfCate,      -- PhraCate -> [Seman]
    seOfActCate,   -- PhraCate -> [Seman]
    cnOfCate,      -- PhraCate -> [PhraStru]
    cnOfActCate,   -- PhraCate -> [PhraStru]
    acOfCate,      -- PhraCate -> [Act]
    acOfActCate,   -- PhraCate -> [Act]
    csOfCate,      -- PhraCate -> [(Category, Seman)]
    csOfActCate,   -- PhraCate -> [(Category, Seman)]
    ssOfCate,      -- PhraCate -> SecStart
    removeDup,     -- Eq a => [a] -> [a]
    removeTuple,   -- Eq a => [(a,a)] -> [(a,a)]
    tupToList,     -- Eq a => [(a,a)] -> [a]


    ) where

import Data.Tuple
import Category

type Start = Int      -- The start position of a phrase (category) in sentences.
type Span = Int       -- The span distance of a phrase (category) in sentences.
type Tag = String        -- The tag of rule used for creating this category.
type Seman = String      -- The semantic component of this phrasal category.
type PhraStru = String   -- The name of phrasal structure.
type Act = Bool          -- The activity of a category, True for active, and False for inactive.
type SecStart = Int      -- The position of spliting a phrase (category).
type OnOff = String   -- The char string to represent turning on/off certain category-converting rules

-- Four tuple functions.
fst4 :: (a,b,c,d) -> a
fst4 (a,_,_,_) = a

snd4 :: (a,b,c,d) -> b
snd4 (_,b,_,_) = b

thd4 :: (a,b,c,d) -> c
thd4 (_,_,c,_) = c

fth4 :: (a,b,c,d) -> d
fth4 (_,_,_,d) = d

-- Five tuple functions.
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

-- Six tuple functions.
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

-- When combining two phrase categories, there might be more than one rule available, resulting in multiple categories (Usually the resultant categories are same).

type PhraCate = ((Start, Span), [(Category, Tag, Seman, PhraStru, Act)], SecStart)

-- Define relation 'belong to' between two phrasal categories. They have same Start, Span, and SecStart, except that the component [(Category,Tag,Seman)] of first phrasal category is subset of that of the second one.
pcBelong :: PhraCate -> PhraCate -> Bool
pcBelong x y = (stx == sty) && (spx == spy) && (ssx == ssy) && belong
    where
    stx = stOfCate x
    sty = stOfCate y
    spx = spOfCate x
    spy = spOfCate y
    ssx = ssOfCate x
    ssy = ssOfCate y
    ctscax = ctscaOfCate x
    ctscay = ctscaOfCate y
    belong = foldr (&&) True (map (\x -> elem x ctscay) ctscax)

-- Another version pcBelong without considering Act attribute.
pcBelong' :: PhraCate -> PhraCate -> Bool
pcBelong' x y = (stx == sty) && (spx == spy) && (ssx == ssy) && belong
    where
    stx = stOfCate x
    sty = stOfCate y
    spx = spOfCate x
    spy = spOfCate y
    ssx = ssOfCate x
    ssy = ssOfCate y
    ctscx = ctscOfCate x
    ctscy = ctscOfCate y
    belong = foldr (&&) True (map (\x -> elem x ctscy) ctscx)

-- The following functions are used to select an element from tuple PhraCate.
stOfCate :: PhraCate -> Start
stOfCate (s, _, _) = fst s

spOfCate :: PhraCate -> Span
spOfCate (s, _, _) = snd s

ctscaOfCate :: PhraCate -> [(Category, Tag, Seman, PhraStru, Act)]
ctscaOfCate (_, ctsca, _) = ctsca

ctscaOfActCate :: PhraCate -> [(Category, Tag, Seman, PhraStru, Act)]
ctscaOfActCate (_, ctsca, _) = [x| x <- ctsca, fif5 x]

ctscOfCate :: PhraCate -> [(Category, Tag, Seman, PhraStru)]
ctscOfCate (_, ctsca, _) = map (\x -> (fst5 x, snd5 x, thd5 x, fth5 x)) ctsca

ctscOfActCate :: PhraCate -> [(Category, Tag, Seman, PhraStru)]
ctscOfActCate (_, ctsca, _) = map (\x -> (fst5 x, snd5 x, thd5 x, fth5 x)) [y| y <- ctsca, fif5 y]

cscOfCate :: PhraCate -> [(Category, Seman, PhraStru)]
cscOfCate (_, ctsca, _) = map (\x -> (fst5 x, thd5 x, fth5 x)) ctsca

cscOfActCate :: PhraCate -> [(Category, Seman, PhraStru)]
cscOfActCate (_, ctsca, _) = map (\x -> (fst5 x, thd5 x, fth5 x)) [y| y <- ctsca, fif5 y]

caOfCate :: PhraCate -> [Category]
caOfCate pc = [fst5 c | c <- ctsca]
    where
    ctsca = ctscaOfCate pc

caOfActCate :: PhraCate -> [Category]
caOfActCate pc = [fst5 c | c <- ctsca, fif5 c == True]
    where
    ctsca = ctscaOfCate pc

taOfCate :: PhraCate -> [Tag]
taOfCate pc = [snd5 c | c <- ctsca]
    where
    ctsca = ctscaOfCate pc

taOfActCate :: PhraCate -> [Tag]
taOfActCate pc = [snd5 c | c <- ctsca, fif5 c == True]
    where
    ctsca = ctscaOfCate pc

seOfCate :: PhraCate -> [Seman]
seOfCate pc = [thd5 c | c <- ctsca]
    where
    ctsca = ctscaOfCate pc

seOfActCate :: PhraCate -> [Seman]
seOfActCate pc = [thd5 c | c <- ctsca, fif5 c == True]
    where
    ctsca = ctscaOfCate pc

cnOfCate :: PhraCate -> [PhraStru]
cnOfCate pc = [fth5 c | c <- ctsca]
    where
    ctsca = ctscaOfCate pc

cnOfActCate :: PhraCate -> [PhraStru]
cnOfActCate pc = [fth5 c | c <- ctsca, fif5 c == True]
    where
    ctsca = ctscaOfCate pc

acOfCate :: PhraCate -> [Act]
acOfCate pc = [fif5 c | c <- ctsca]
    where
    ctsca = ctscaOfCate pc

acOfActCate :: PhraCate -> [Act]
acOfActCate pc = [fif5 c | c <- ctsca, fif5 c == True]
    where
    ctsca = ctscaOfCate pc

csOfCate :: PhraCate -> [(Category, Seman)]
csOfCate pc = zip (caOfCate pc) (seOfCate pc)

csOfActCate :: PhraCate -> [(Category, Seman)]
csOfActCate pc = zip (caOfActCate pc) (seOfActCate pc)

ssOfCate :: PhraCate -> SecStart
ssOfCate (_, _, s) = s

-- Rremove duplicate elements in a list.
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
