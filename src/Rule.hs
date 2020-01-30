-- Copyright (c) 2019-2020 China University of Water Resources and Electric Power,
-- All rights reserved.

module Rule (
    Tag,      -- String
    Seman,    -- String
    Act,      -- Bool
    rules,    -- [(Category, Seman) -> (Category, Seman) -> (Category, Tag, Seman, Act)]
    ruleTags, -- [String]
    semComb,  -- Seman -> Seman -> Seman
    appF,     -- (Category, Seman) -> (Category, Seman) -> (Category, Tag, Seman, Act)
    appB,     -- (Category, Seman) -> (Category, Seman) -> (Category, Tag, Seman, Act)
    comFh,    -- (Category, Seman) -> (Category, Seman) -> (Category, Tag, Seman, Act)
    comFh2,   -- (Category, Seman) -> (Category, Seman) -> (Category, Tag, Seman, Act)
    comBh,    -- (Category, Seman) -> (Category, Seman) -> (Category, Tag, Seman, Act)
    comFc,    -- (Category, Seman) -> (Category, Seman) -> (Category, Tag, Seman, Act)
    comBc,    -- (Category, Seman) -> (Category, Seman) -> (Category, Tag, Seman, Act)
    raiFh,    -- (Category, Seman) -> (Category, Seman) -> (Category, Tag, Seman, Act)
    raiFc,    -- (Category, Seman) -> (Category, Seman) -> (Category, Tag, Seman, Act)
    raiBh,    -- (Category, Seman) -> (Category, Seman) -> (Category, Tag, Seman, Act)
    raiBc,    -- (Category, Seman) -> (Category, Seman) -> (Category, Tag, Seman, Act)
    ) where

import Data.List
import Category 

type Tag = String        -- The tag of rule used for creating a category.
type Seman = String      -- The semantic component of a category
type Act = Bool          -- The activity of a category, True for active, and False for inactive.

-- CCG rules constitute a functional list.
rules :: [(Category, Seman) -> (Category, Seman) -> (Category, Tag, Seman, Act)]
-- rules = [appF, appB, comFh, comFh2, comBh, comFc, comBc, raiFh, raiFc, raiBh, raiBc] 
rules = [appF, appB, comFh, comFh2, comBh, comFc, comBc, raiFh]

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
appF :: (Category, Seman) -> (Category, Seman) -> (Category, Tag, Seman, Act)
appF cate1 cate2
    | isPrimitive ca1 = (nilCate, ">", "", False)
    | ca1 == getCateFromString "(X\\*X)/*X" = (derivate ca2 "\\*" ca2, ">", semComb se1 se2, True)
    | ca2 == getCateFromString "(X\\*X)/*X" = (nilCate, ">", "", False)
    | head (midSlash ca1) == '/' && rightCate ca1 == ca2 = (leftCate ca1, ">", semComb se1 se2, True)
    | otherwise = (nilCate, ">", "", False)
    where
    ca1 = fst cate1
    se1 = snd cate1
    ca2 = fst cate2
    se2 = snd cate2

-- CCG backward application, here using nonstrict equality.
appB :: (Category, Seman) -> (Category, Seman) -> (Category, Tag, Seman, Act)
appB cate1 cate2
    | isPrimitive ca2 = (nilCate, "<", "", False)
    | head (midSlash ca2) == '\\' && rightCate ca2 == ca1 = (leftCate ca2, "<", semComb se2 se1, True)
    | otherwise = (nilCate, "<", "", False)
    where
    ca1 = fst cate1
    se1 = snd cate1
    ca2 = fst cate2
    se2 = snd cate2

-- CCG forward harmonic composition
-- To now, the rule is only used for "adverbal + transitive verb" structure.
comFh :: (Category, Seman) -> (Category, Seman) -> (Category, Tag, Seman, Act)
comFh cate1 cate2
    | isPrimitive ca1 || isPrimitive ca2 = (nilCate, ">B", "", False)
    | ca2 /= getCateFromString "(s\\.np)/.np" = (nilCate, ">B", "", False)
    | (midSlash ca1 == "/#" || midSlash ca1 == "/.") && (midSlash ca2 == "/#" || midSlash ca2 == "/.") && rightCate ca1 == leftCate ca2 = (derivate (leftCate ca1) (midSlash ca2) (rightCate ca2), ">B", semComb se1 se2, True)
    | otherwise = (nilCate, ">B", "", False)
    where
    ca1 = fst cate1
    se1 = snd cate1
    ca2 = fst cate2
    se2 = snd cate2
        
-- CCG forward harmonic composition^2
-- To now, the rule is only used for "adverbal + double objects-transitive verb" structure.
comFh2 :: (Category, Seman) -> (Category, Seman) -> (Category, Tag, Seman, Act)
comFh2 cate1 cate2
    | isPrimitive ca1 || isPrimitive ca2 = (nilCate, ">B2", "", False)
    | isPrimitive (leftCate ca2) = (nilCate, ">B2", "", False)
    | ca2 /= getCateFromString "((s\\.np)/.np)/.np" = (nilCate, ">B2", "", False)
    | (midSlash ca1 == "/#" || midSlash ca1 == "/.") && (midSlash lCate2 == "/#" || midSlash lCate2 == "/.") && rightCate ca1 == leftCate lCate2 = (derivate (derivate (leftCate ca1) (midSlash lCate2) (rightCate lCate2)) (midSlash ca2) (rightCate ca2), ">B2", semComb se1 se2, True)
    | otherwise = (nilCate, ">B2", "", False)
    where
    ca1 = fst cate1
    se1 = snd cate1
    ca2 = fst cate2
    se2 = snd cate2
    lCate2 = leftCate ca2

-- CCG backward harmonic composition
comBh :: (Category, Seman) -> (Category, Seman) -> (Category, Tag, Seman, Act)
comBh cate1 cate2
    | isPrimitive ca1 || isPrimitive ca2 = (nilCate, "<B", "", False)
    | (midSlash ca1 == "\\#" || midSlash ca1 == "\\.") && (midSlash ca2 == "\\#" || midSlash ca2 == "\\.") && rightCate ca2 == leftCate ca1 = (derivate (leftCate ca2) (midSlash ca1) (rightCate ca1), "<B", semComb se2 se1, True)
    | otherwise = (nilCate, "<B", "", False)
    where
    ca1 = fst cate1
    se1 = snd cate1
    ca2 = fst cate2
    se2 = snd cate2

-- CCG forward crossing composition
comFc :: (Category, Seman) -> (Category, Seman) -> (Category, Tag, Seman, Act)
comFc cate1 cate2
    | isPrimitive ca1 || isPrimitive ca2 = (nilCate, ">Bx", "", False)
    | (midSlash ca1 == "/x" || midSlash ca1 == "/.") && (midSlash ca2 == "\\x" || midSlash ca2 == "\\.") && rightCate ca1 == leftCate ca2 = (derivate (leftCate ca1) (midSlash ca2) (rightCate ca2), ">Bx", semComb se1 se2, True)
    | otherwise = (nilCate, ">Bx", "", False)
    where
    ca1 = fst cate1
    se1 = snd cate1
    ca2 = fst cate2
    se2 = snd cate2

-- CCG backward crossing composition
comBc :: (Category, Seman) -> (Category, Seman) -> (Category, Tag, Seman, Act)
comBc cate1 cate2
    | isPrimitive ca1 || isPrimitive ca2 = (nilCate, "<Bx", "", False)
    | (midSlash ca1 == "/x" || midSlash ca1 == "/.") && (midSlash ca2 == "\\x" || midSlash ca2 == "\\.") && leftCate ca1 == rightCate ca2 = (derivate (leftCate ca2) (midSlash ca1) (rightCate ca1), "<Bx", semComb se2 se1, True)
    | otherwise = (nilCate, "<Bx", "", False)
    where
    ca1 = fst cate1
    se1 = snd cate1
    ca2 = fst cate2
    se2 = snd cate2

-- CCG Forward type raising and harmonic composition: X (Y\X)/Z -> Y/(Y\X) (Y\X)/Z -> Y/Z
-- To now, the rule is only used for objective extraction.
raiFh :: (Category, Seman) -> (Category, Seman) -> (Category, Tag, Seman, Act)
raiFh cate1 cate2
    | isPrimitive ca2 || isPrimitive lcate2 = (nilCate, ">T->B", "", False)
    | ca2 /= getCateFromString "(s\\.np)/.np" && ca2 /= getCateFromString "((s\\.np)/.np)/.np" = (nilCate, ">T->B", "", False)
    | head (midSlash lcate2) /= '\\' || (midSlash ca2 /= "/#" && midSlash ca2 /= "/.") = (nilCate, ">T->B", "", False)
    | ca1 == rightCate lcate2 = (derivate (leftCate lcate2) (midSlash ca2) (rightCate ca2), ">T->B", semComb se1 se2, True)
    | otherwise = (nilCate, ">T->B", "", False)
    where
    ca1 = fst cate1
    se1 = snd cate1
    ca2 = fst cate2
    se2 = snd cate2
    lcate2 = leftCate ca2

-- Forward type raising and crossing composition: X (Y\X)\Z -> Y/(Y\X) (Y\X)\Z -> Y\Z
raiFc :: (Category, Seman) -> (Category, Seman) -> (Category, Tag, Seman, Act)
raiFc cate1 cate2
    | isPrimitive ca2 || isPrimitive lcate2 = (nilCate, ">T->Bx", "", False)
    | head (midSlash lcate2) /= '\\' || (midSlash ca2 /= "\\x" && midSlash ca2 /= "\\.") = (nilCate, ">T->Bx", "", False)
    | ca1 == rightCate lcate2 = (derivate (leftCate lcate2) (midSlash ca2) (rightCate ca2), ">T->Bx", semComb se1 se2, True)
    | otherwise = (nilCate, ">T->Bx", "", False)
    where
    ca1 = fst cate1
    se1 = snd cate1
    ca2 = fst cate2
    se2 = snd cate2
    lcate2 = leftCate ca2

-- Backward type raising and harmonic composition: (Y/X)\Z X -> (Y/X)\Z Y\(Y/X)-> Y\Z
raiBh :: (Category, Seman) -> (Category, Seman) -> (Category, Tag, Seman, Act)
raiBh cate1 cate2
    | isPrimitive ca1 || isPrimitive lcate1 = (nilCate, "<T-<B", "", False)
    | head (midSlash lcate1) /= '/' || (midSlash ca1 /= "\\#" && midSlash ca1 /= "\\.") = (nilCate, "<T-<B", "", False)
    | rightCate lcate1 == ca2 = (derivate (leftCate lcate1) (midSlash ca1) (rightCate ca1), "<T-<B", semComb se2 se1, True)
    | otherwise = (nilCate, "<T-<B", "", False)
    where
    ca1 = fst cate1
    se1 = snd cate1
    ca2 = fst cate2
    se2 = snd cate2
    lcate1 = leftCate ca1

-- Backward type raising and crossing composition: (Y/X)/Z X -> (Y/X)/Z Y\(Y/X)-> Y/Z
raiBc :: (Category, Seman) -> (Category, Seman) -> (Category, Tag, Seman, Act)
raiBc cate1 cate2
    | isPrimitive ca1 || isPrimitive lcate1 = (nilCate, "<T-<Bx", "", False)
    | head (midSlash lcate1) /= '/' || (midSlash ca1 /= "/x" && midSlash ca1 /= "/.") = (nilCate, "<T-<Bx", "", False)
    | rightCate lcate1 == ca2 = (derivate (leftCate lcate1) (midSlash ca1) (rightCate ca1), "<T-<Bx", semComb se2 se1, True)
    | otherwise = (nilCate, "<T-<Bx", "", False)
    where
    ca1 = fst cate1
    se1 = snd cate1
    ca2 = fst cate2
    se2 = snd cate2
    lcate1 = leftCate ca1


