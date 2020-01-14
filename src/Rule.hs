-- Copyright (c) 2019-2020 China University of Water Resources and Electric Power,
-- All rights reserved.

module Rule (
    Tag,      -- String
    Seman,    -- String
    rules,    -- [(Category, Seman) -> (Category, Seman) -> (Category, Tag, Seman)]
    ruleTags, -- [String]
    semComb,  -- Seman -> Seman -> Seman
    appF,     -- (Category, Seman) -> (Category, Seman) -> (Category, Tag, Seman)
    appB,     -- (Category, Seman) -> (Category, Seman) -> (Category, Tag, Seman)
    comFh,    -- (Category, Seman) -> (Category, Seman) -> (Category, Tag, Seman)
    comFh2,   -- (Category, Seman) -> (Category, Seman) -> (Category, Tag, Seman)
    comBh,    -- (Category, Seman) -> (Category, Seman) -> (Category, Tag, Seman)
    comFc,    -- (Category, Seman) -> (Category, Seman) -> (Category, Tag, Seman)
    comBc,    -- (Category, Seman) -> (Category, Seman) -> (Category, Tag, Seman)
    raiFh,    -- (Category, Seman) -> (Category, Seman) -> (Category, Tag, Seman)
    raiFc,    -- (Category, Seman) -> (Category, Seman) -> (Category, Tag, Seman)
    raiBh,    -- (Category, Seman) -> (Category, Seman) -> (Category, Tag, Seman)
    raiBc,    -- (Category, Seman) -> (Category, Seman) -> (Category, Tag, Seman)
    ) where

import Data.List
import Category 

type Tag = String        -- The tag of rule used for creating a category.
type Seman = String      -- The semantic component of a category

-- CCG rules constitute a functional list.
rules :: [(Category, Seman) -> (Category, Seman) -> (Category, Tag, Seman)]
rules = [appF, appB, comFh, comFh2, comBh, comFc, comBc, raiFh, raiFc, raiBh, raiBc] 

-- In parsing trees, every combination should print its corresponding rule tag.
ruleTags :: [String]
ruleTags = [">","<",">B",">B2","<B",">Bx","<Bx",">T->B",">T->Bx","<T-<B","<T-<Bx"]

-- To create a predicate-argument structure from two semantic components.
-- For semantic components themselves as predicate-argument structure, they should be bracketed.
semComb :: Seman -> Seman -> Seman
semComb se1 se2
    | elem ' ' se1 == False && elem ' ' se2 == False = se1 ++ " " ++ se2
    | elem ' ' se1 == True  && elem ' ' se2 == False = "(" ++ se1 ++ ")" ++ " " ++ se2
    | elem ' ' se1 == False && elem ' ' se2 == True  = se1 ++ " " ++ "(" ++ se2 ++ ")"
    | otherwise = "(" ++ se1 ++ ")" ++ " " ++ "(" ++ se2 ++ ")" 

-- CCG forward application
appF :: (Category, Seman) -> (Category, Seman) -> (Category, Tag, Seman)
appF cate1 cate2
    | isPrimitive ca1 = (nilCate, ">", "")
    | ca1 == getCateFromString "(X\\*X)/*X" = (derivate ca2 "\\*" ca2, ">", semComb se1 se2)
    | ca2 == getCateFromString "(X\\*X)/*X" = (nilCate, ">", "")
    | head (midSlash ca1) == '/' && rightCate ca1 == ca2 = (leftCate ca1, ">", semComb se1 se2)
    | otherwise = (nilCate, ">", "")
    where
    ca1 = fst cate1
    se1 = snd cate1
    ca2 = fst cate2
    se2 = snd cate2

-- CCG backward application, here using nonstrict equality.
appB :: (Category, Seman) -> (Category, Seman) -> (Category, Tag, Seman)
appB cate1 cate2
    | isPrimitive ca2 = (nilCate, "<", "")
    | head (midSlash ca2) == '\\' && rightCate ca2 == ca1 = (leftCate ca2, "<", semComb se2 se1)
    | otherwise = (nilCate, "<", "")
    where
    ca1 = fst cate1
    se1 = snd cate1
    ca2 = fst cate2
    se2 = snd cate2

-- CCG forward harmonic composition
comFh :: (Category, Seman) -> (Category, Seman) -> (Category, Tag, Seman)
comFh cate1 cate2
    | isPrimitive ca1 || isPrimitive ca2 = (nilCate, ">B", "")
    | (midSlash ca1 == "/#" || midSlash ca1 == "/.") && (midSlash ca2 == "/#" || midSlash ca2 == "/.") && rightCate ca1 == leftCate ca2 = (derivate (leftCate ca1) (midSlash ca2) (rightCate ca2), ">B", semComb se1 se2)
    | otherwise = (nilCate, ">B", "")
    where
    ca1 = fst cate1
    se1 = snd cate1
    ca2 = fst cate2
    se2 = snd cate2
        
-- CCG forward harmonic composition^2
comFh2 :: (Category, Seman) -> (Category, Seman) -> (Category, Tag, Seman)
comFh2 cate1 cate2
    | isPrimitive ca1 || isPrimitive ca2 = (nilCate, ">B2", "")
    | isPrimitive (leftCate ca2) = (nilCate, ">B2", "")
    | (midSlash ca1 == "/#" || midSlash ca1 == "/.") && (midSlash lCate2 == "/#" || midSlash lCate2 == "/.") && rightCate ca1 == leftCate lCate2 = (derivate (derivate (leftCate ca1) (midSlash lCate2) (rightCate lCate2)) (midSlash ca2) (rightCate ca2), ">B2", semComb se1 se2)
    | otherwise = (nilCate, ">B2", "")
    where
    ca1 = fst cate1
    se1 = snd cate1
    ca2 = fst cate2
    se2 = snd cate2
    lCate2 = leftCate ca2

-- CCG backward harmonic composition
comBh :: (Category, Seman) -> (Category, Seman) -> (Category, Tag, Seman)
comBh cate1 cate2
    | isPrimitive ca1 || isPrimitive ca2 = (nilCate, "<B", "")
    | (midSlash ca1 == "\\#" || midSlash ca1 == "\\.") && (midSlash ca2 == "\\#" || midSlash ca2 == "\\.") && rightCate ca2 == leftCate ca1 = (derivate (leftCate ca2) (midSlash ca1) (rightCate ca1), "<B", semComb se2 se1)
    | otherwise = (nilCate, "<B", "")
    where
    ca1 = fst cate1
    se1 = snd cate1
    ca2 = fst cate2
    se2 = snd cate2

-- CCG forward crossing composition
comFc :: (Category, Seman) -> (Category, Seman) -> (Category, Tag, Seman)
comFc cate1 cate2
    | isPrimitive ca1 || isPrimitive ca2 = (nilCate, ">Bx", "")
    | (midSlash ca1 == "/x" || midSlash ca1 == "/.") && (midSlash ca2 == "\\x" || midSlash ca2 == "\\.") && rightCate ca1 == leftCate ca2 = (derivate (leftCate ca1) (midSlash ca2) (rightCate ca2), ">Bx", semComb se1 se2)
    | otherwise = (nilCate, ">Bx", "")
    where
    ca1 = fst cate1
    se1 = snd cate1
    ca2 = fst cate2
    se2 = snd cate2

-- CCG backward crossing composition
comBc :: (Category, Seman) -> (Category, Seman) -> (Category, Tag, Seman)
comBc cate1 cate2
    | isPrimitive ca1 || isPrimitive ca2 = (nilCate, "<Bx", "")
    | (midSlash ca1 == "/x" || midSlash ca1 == "/.") && (midSlash ca2 == "\\x" || midSlash ca2 == "\\.") && leftCate ca1 == rightCate ca2 = (derivate (leftCate ca2) (midSlash ca1) (rightCate ca1), "<Bx", semComb se2 se1)
    | otherwise = (nilCate, "<Bx", "")
    where
    ca1 = fst cate1
    se1 = snd cate1
    ca2 = fst cate2
    se2 = snd cate2

-- CCG Forward type raising and harmonic composition: X (Y\X)/Z -> Y/(Y\X) (Y\X)/Z -> Y/Z
raiFh :: (Category, Seman) -> (Category, Seman) -> (Category, Tag, Seman)
raiFh cate1 cate2
    | isPrimitive ca2 || isPrimitive lcate2 = (nilCate, ">T->B", "")
    | head (midSlash lcate2) /= '\\' || (midSlash ca2 /= "/#" && midSlash ca2 /= "/.") = (nilCate, ">T->B", "")
    | ca1 == rightCate lcate2 = (derivate (leftCate lcate2) (midSlash ca2) (rightCate ca2), ">T->B", semComb se1 se2)
    | otherwise = (nilCate, ">T->B", "")
    where
    ca1 = fst cate1
    se1 = snd cate1
    ca2 = fst cate2
    se2 = snd cate2
    lcate2 = leftCate ca2

-- Forward type raising and crossing composition: X (Y\X)\Z -> Y/(Y\X) (Y\X)\Z -> Y\Z
raiFc :: (Category, Seman) -> (Category, Seman) -> (Category, Tag, Seman)
raiFc cate1 cate2
    | isPrimitive ca2 || isPrimitive lcate2 = (nilCate, ">T->Bx", "")
    | head (midSlash lcate2) /= '\\' || (midSlash ca2 /= "\\x" && midSlash ca2 /= "\\.") = (nilCate, ">T->Bx", "")
    | ca1 == rightCate lcate2 = (derivate (leftCate lcate2) (midSlash ca2) (rightCate ca2), ">T->Bx", semComb se1 se2)
    | otherwise = (nilCate, ">T->Bx", "")
    where
    ca1 = fst cate1
    se1 = snd cate1
    ca2 = fst cate2
    se2 = snd cate2
    lcate2 = leftCate ca2

-- Backward type raising and harmonic composition: (Y/X)\Z X -> (Y/X)\Z Y\(Y/X)-> Y\Z
raiBh :: (Category, Seman) -> (Category, Seman) -> (Category, Tag, Seman)
raiBh cate1 cate2
    | isPrimitive ca1 || isPrimitive lcate1 = (nilCate, "<T-<B", "")
    | head (midSlash lcate1) /= '/' || (midSlash ca1 /= "\\#" && midSlash ca1 /= "\\.") = (nilCate, "<T-<B", "")
    | rightCate lcate1 == ca2 = (derivate (leftCate lcate1) (midSlash ca1) (rightCate ca1), "<T-<B", semComb se2 se1)
    | otherwise = (nilCate, "<T-<B", "")
    where
    ca1 = fst cate1
    se1 = snd cate1
    ca2 = fst cate2
    se2 = snd cate2
    lcate1 = leftCate ca1

-- Backward type raising and crossing composition: (Y/X)/Z X -> (Y/X)/Z Y\(Y/X)-> Y/Z
raiBc :: (Category, Seman) -> (Category, Seman) -> (Category, Tag, Seman)
raiBc cate1 cate2
    | isPrimitive ca1 || isPrimitive lcate1 = (nilCate, "<T-<Bx", "")
    | head (midSlash lcate1) /= '/' || (midSlash ca1 /= "/x" && midSlash ca1 /= "/.") = (nilCate, "<T-<Bx", "")
    | rightCate lcate1 == ca2 = (derivate (leftCate lcate1) (midSlash ca1) (rightCate ca1), "<T-<Bx", semComb se2 se1)
    | otherwise = (nilCate, "<T-<Bx", "")
    where
    ca1 = fst cate1
    se1 = snd cate1
    ca2 = fst cate2
    se2 = snd cate2
    lcate1 = leftCate ca1


