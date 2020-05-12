-- Copyright (c) 2019-2020 China University of Water Resources and Electric Power
-- All rights reserved.

module Rule (
    rules,    -- [(Category,Seman,PhraStru) -> (Category,Seman,PhraStru) -> (Category, Tag, Seman, PhraStru, Act)]
    ruleTags, -- [Tag]
    semComb,  -- Seman -> Seman -> Seman
    appF,     -- (Category,Seman,PhraStru) -> (Category,Seman,PhraStru) -> (Category, Tag, Seman, PhraStru, Act)
    appB,     -- (Category,Seman,PhraStru) -> (Category,Seman,PhraStru) -> (Category, Tag, Seman, PhraStru, Act)
    comFh,    -- (Category,Seman,PhraStru) -> (Category,Seman,PhraStru) -> (Category, Tag, Seman, PhraStru, Act)
    comFh2,   -- (Category,Seman,PhraStru) -> (Category,Seman,PhraStru) -> (Category, Tag, Seman, PhraStru, Act)
    comBh,    -- (Category,Seman,PhraStru) -> (Category,Seman,PhraStru) -> (Category, Tag, Seman, PhraStru, Act)
    comFc,    -- (Category,Seman,PhraStru) -> (Category,Seman,PhraStru) -> (Category, Tag, Seman, PhraStru, Act)
    comBc,    -- (Category,Seman,PhraStru) -> (Category,Seman,PhraStru) -> (Category, Tag, Seman, PhraStru, Act)
    raiFh,    -- (Category,Seman,PhraStru) -> (Category,Seman,PhraStru) -> (Category, Tag, Seman, PhraStru, Act)
    raiFc,    -- (Category,Seman,PhraStru) -> (Category,Seman,PhraStru) -> (Category, Tag, Seman, PhraStru, Act)
    raiBh,    -- (Category,Seman,PhraStru) -> (Category,Seman,PhraStru) -> (Category, Tag, Seman, PhraStru, Act)
    raiBc,    -- (Category,Seman,PhraStru) -> (Category,Seman,PhraStru) -> (Category, Tag, Seman, PhraStru, Act)
    ) where

import Data.List
import Data.List.Utils
import Data.Tuple.Utils
import Category
import Phrase
import Utils

-- CCG rules constitute a functional list.
rules :: [(Category,Seman,PhraStru) -> (Category,Seman,PhraStru) -> (Category, Tag, Seman, PhraStru, Act)]

-- rules = [appF, appB, comFh, comFh2, comBh, comFc, comBc, raiFh, raiFc, raiBh, raiBc] 
rules = [appF, appB, comFh, comFh2, comBh, comFc, comBc, raiFh]

-- In parsing trees, every combination should print its corresponding rule tag.
ruleTags :: [Tag]
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
appF :: (Category,Seman,PhraStru) -> (Category,Seman,PhraStru) -> (Category, Tag, Seman, PhraStru, Act)
appF cate1 cate2
    | isPrimitive ca1 = (nilCate, ">", "", "", False)
    | ca1 == getCateFromString "(X\\*X)/*X" = (derivate ca2 "\\*" ca2, ">", semComb se1 se2, "XX", True)
    | ca1 == getCateFromString "X/*X" = (ca2, ">", semComb se1 se2, "CC", True)
    | isAvail && cateEqual ca1 (getCateFromString "np/.np") = (leftCate ca1, ">", semComb se1 se2, "AHn", True)
    | isAvail && cateEqual ca1 (getCateFromString "(s\\.np)/.np") = (leftCate ca1, ">", semComb se1 se2, "VO", True)
    | isAvail && cateEqual ca1 (getCateFromString "((s\\.np)/.np)/.np") = (leftCate ca1, ">", semComb se1 se2, "VO", True)
    | isAvail && cateEqual ca1 (getCateFromString "(s\\.np)/#(s\\.np)") = (leftCate ca1, ">", semComb se1 se2, "DHv", True)
    | isAvail && (cateEqual ca1 (getCateFromString "((s\\.np)\\x(s\\.np))/*np") || cateEqual ca1 (getCateFromString "((s\\.np)/#(s\\.np))/*np") || cateEqual ca1 (getCateFromString "(s/*s)/*np")) = (leftCate ca1, ">", semComb se1 se2, "PO", True)
    | isAvail && cateEqual ca1 (getCateFromString "(np/.np)/.(np/.np)") = (leftCate ca1, ">", semComb se1 se2, "DHa", True)
    | isAvail && cateEqual ca1 (getCateFromString "((s\\.np)\\x(s\\.np))/*(np/.np)") = (leftCate ca1, ">", semComb se1 se2, "U3P", True)
    | isAvail =  (leftCate ca1, ">", semComb se1 se2, "NR", True)
    | otherwise = (nilCate, ">", "", "", False)
    where
    ca1 = fst3 cate1
    se1 = snd3 cate1
    ca2 = fst3 cate2
    se2 = snd3 cate2
    isAvail = head (midSlash ca1) == '/' && rightCate ca1 == ca2

-- CCG backward application, here using nonstrict equality.
appB :: (Category,Seman,PhraStru) -> (Category,Seman,PhraStru) -> (Category, Tag, Seman, PhraStru, Act)
appB cate1 cate2
    | isPrimitive ca2 = (nilCate, "<", "", "", False)
    | ca2 == getCateFromString "X\\*X" = (ca1, "<", semComb se2 se1, "CC", True)
    | isAvail && cn2 == "XX" = (leftCate ca2, "<", semComb se2 se1, "XX", True)
    | isAvail && ca1 == getCateFromString "np/.np" && ca2 == getCateFromString "(np/.np)\\*(np/.np)" = (leftCate ca2, "<", semComb se2 se1, "MQ", True)
    | isAvail && cateEqual ca2 (getCateFromString "s\\.np") = (leftCate ca2, "<", semComb se2 se1, "SP", True)
    | isAvail && ca1 == npCate && cateEqual ca2 (getCateFromString "np\\.np") = (leftCate ca2, "<", semComb se2 se1, "HnC", True)
    | isAvail && cateEqual ca2 (getCateFromString "(s\\.np)\\x(s\\.np)") = (leftCate ca2, "<", semComb se2 se1, "HvC", True)
    | isAvail && (ca2 == getCateFromString "(np/*np)\\*np" || ca2 == getCateFromString "(np/*np)\\*(np/.np)" || ca2 == getCateFromString "(np/*np)\\*(s/.np)" || ca2 == getCateFromString "(np/*np)\\*(s\\.np)") = (leftCate ca2, "<", semComb se2 se1, "U1P", True)
    | isAvail && ca2 == getCateFromString "((s\\.np)/#(s\\.np))\\*(np/.np)" = (leftCate ca2, "<", semComb se2 se1, "U2P", True)
    | isAvail && cateEqual ca2 (getCateFromString "s\\*s") =  (leftCate ca2, "<", semComb se2 se1, "EM", True)
    | isAvail =  (leftCate ca2, "<", semComb se2 se1, "NR", True)
    | otherwise = (nilCate, "<", "", "", False)
    where
    ca1 = fst3 cate1
    se1 = snd3 cate1
    ca2 = fst3 cate2
    se2 = snd3 cate2
    cn2 = thd3 cate2
    isAvail = head (midSlash ca2) == '\\' && rightCate ca2 == ca1

-- CCG forward harmonic composition, like X/Y Y/Z -> X/Z.
-- Obsoletely, the rule is only used for "adverbal + transitive verb" structure.
comFh :: (Category,Seman,PhraStru) -> (Category,Seman,PhraStru) -> (Category, Tag, Seman, PhraStru, Act)
comFh cate1 cate2
    | isPrimitive ca1 || isPrimitive ca2 = (nilCate, ">B", "", "", False)
    | isAvail && cateEqual ca2 (getCateFromString "(s\\.np)/.np") = (derivate (leftCate ca1) (midSlash ca2) (rightCate ca2), ">B", semComb se1 se2, "DHv", True)
    | isAvail = (derivate (leftCate ca1) (midSlash ca2) (rightCate ca2), ">B", semComb se1 se2, "NR", True)
    | otherwise = (nilCate, ">B", "", "", False)
    where
    ca1 = fst3 cate1
    se1 = snd3 cate1
    ca2 = fst3 cate2
    se2 = snd3 cate2
    isAvail = (midSlash ca1 == "/#" || midSlash ca1 == "/.") && (midSlash ca2 == "/#" || midSlash ca2 == "/.") && rightCate ca1 == leftCate ca2
        
-- CCG forward harmonic composition^2, like X/Y (Y/Z)/W -> (X/Z)/W.
-- To now, the rule is only used for "adverbal + double objects-transitive verb" structure.
comFh2 :: (Category,Seman,PhraStru) -> (Category,Seman,PhraStru) -> (Category, Tag, Seman, PhraStru, Act)
comFh2 cate1 cate2
    | isPrimitive ca1 || isPrimitive ca2 = (nilCate, ">B2", "", "", False)
    | isPrimitive (leftCate ca2) = (nilCate, ">B2", "", "", False)
    | isAvail && cateEqual ca2 (getCateFromString "((s\\.np)/.np)/.np") = (derivate (derivate (leftCate ca1) (midSlash lCate2) (rightCate lCate2)) (midSlash ca2) (rightCate ca2), ">B2", semComb se1 se2, "DHv", True)
    | isAvail =  (derivate (derivate (leftCate ca1) (midSlash lCate2) (rightCate lCate2)) (midSlash ca2) (rightCate ca2), ">B2", semComb se1 se2, "NR", True)
    | otherwise = (nilCate, ">B2", "", "", False)
    where
    ca1 = fst3 cate1
    se1 = snd3 cate1
    ca2 = fst3 cate2
    se2 = snd3 cate2
    lCate2 = leftCate ca2
    isAvail = (midSlash ca1 == "/#" || midSlash ca1 == "/.") && (midSlash ca2 == "/#" || midSlash ca2 == "/.") && (midSlash lCate2 == "/#" || midSlash lCate2 == "/.") && rightCate ca1 == leftCate lCate2

-- CCG backward harmonic composition, like X\Y Y\Z -> X\Z.
comBh :: (Category,Seman,PhraStru) -> (Category,Seman,PhraStru) -> (Category, Tag, Seman, PhraStru, Act)
comBh cate1 cate2
    | isPrimitive ca1 || isPrimitive ca2 = (nilCate, "<B", "", "", False)
    | isAvail = (derivate (leftCate ca2) (midSlash ca1) (rightCate ca1), "<B", semComb se2 se1, "NR", True)
    | otherwise = (nilCate, "<B", "", "", False)
    where
    ca1 = fst3 cate1
    se1 = snd3 cate1
    ca2 = fst3 cate2
    se2 = snd3 cate2
    isAvail = (midSlash ca1 == "\\#" || midSlash ca1 == "\\.") && (midSlash ca2 == "\\#" || midSlash ca2 == "\\.") && rightCate ca2 == leftCate ca1

-- CCG forward crossing composition, like X/Y Y\Z -> X\Z.
comFc :: (Category,Seman,PhraStru) -> (Category,Seman,PhraStru) -> (Category, Tag, Seman, PhraStru, Act)
comFc cate1 cate2
    | isPrimitive ca1 || isPrimitive ca2 = (nilCate, ">Bx", "", "", False)
    | isAvail = (derivate (leftCate ca1) (midSlash ca2) (rightCate ca2), ">Bx", semComb se1 se2, "NR", True)
    | otherwise = (nilCate, ">Bx", "", "", False)
    where
    ca1 = fst3 cate1
    se1 = snd3 cate1
    ca2 = fst3 cate2
    se2 = snd3 cate2
    isAvail = (midSlash ca1 == "/x" || midSlash ca1 == "/.") && (midSlash ca2 == "\\x" || midSlash ca2 == "\\.") && rightCate ca1 == leftCate ca2

-- CCG backward crossing composition, like Y/Z X\Y ->X\Z.
comBc :: (Category,Seman,PhraStru) -> (Category,Seman,PhraStru) -> (Category, Tag, Seman, PhraStru, Act)
comBc cate1 cate2
    | isPrimitive ca1 || isPrimitive ca2 = (nilCate, "<Bx", "", "", False)
    | isAvail && cateEqual ca2 (getCateFromString "(s\\.np)\\x(s\\.np)") = (derivate (leftCate ca2) (midSlash ca1) (rightCate ca1), "<Bx", semComb se2 se1, "HvC", True)
    | isAvail && cateEqual ca1 (getCateFromString "np/.np") && cateEqual ca2 (getCateFromString "np\\.np") = (derivate (leftCate ca2) (midSlash ca1) (rightCate ca1), "<Bx", semComb se2 se1, "HaC", True)
    | isAvail = (derivate (leftCate ca2) (midSlash ca1) (rightCate ca1), "<Bx", semComb se2 se1, "NR", True)
    | otherwise = (nilCate, "<Bx", "", "", False)
    where
    ca1 = fst3 cate1
    se1 = snd3 cate1
    ca2 = fst3 cate2
    se2 = snd3 cate2
    isAvail = (midSlash ca1 == "/x" || midSlash ca1 == "/.") && (midSlash ca2 == "\\x" || midSlash ca2 == "\\.") && leftCate ca1 == rightCate ca2

-- CCG Forward type raising and harmonic composition: X (Y\X)/Z -> Y/(Y\X) (Y\X)/Z -> Y/Z
-- To now, the rule is only used for objective extraction.
raiFh :: (Category,Seman,PhraStru) -> (Category,Seman,PhraStru) -> (Category, Tag, Seman, PhraStru, Act)
raiFh cate1 cate2
    | isPrimitive ca2 || isPrimitive lcate2 || isX lcate2 = (nilCate, ">T->B", "", "", False)
    | isAvail && (ca2 == getCateFromString "(s\\.np)/.np" || ca2 == getCateFromString "((s\\.np)/.np)/.np") = (derivate (leftCate lcate2) (midSlash ca2) (rightCate ca2), ">T->B", semComb se1 se2, "OE", True)
    | isAvail = (derivate (leftCate lcate2) (midSlash ca2) (rightCate ca2), ">T->B", semComb se1 se2, "NR", True)
    | otherwise = (nilCate, ">T->B", "", "", False)
    where
    ca1 = fst3 cate1
    se1 = snd3 cate1
    ca2 = fst3 cate2
    se2 = snd3 cate2
    lcate2 = leftCate ca2
    isAvail = (ca1 == rightCate lcate2) && (head (midSlash lcate2) == '\\') && (midSlash ca2 == "/#" || midSlash ca2 == "/.")

-- Forward type raising and crossing composition: X (Y\X)\Z -> Y/(Y\X) (Y\X)\Z -> Y\Z
raiFc :: (Category,Seman,PhraStru) -> (Category,Seman,PhraStru) -> (Category, Tag, Seman, PhraStru, Act)
raiFc cate1 cate2
    | isPrimitive ca2 || isPrimitive lcate2 || isX lcate2 = (nilCate, ">T->Bx", "", "", False)
    | isAvail = (derivate (leftCate lcate2) (midSlash ca2) (rightCate ca2), ">T->Bx", semComb se1 se2, "NR", True)
    | otherwise = (nilCate, ">T->Bx", "", "", False)
    where
    ca1 = fst3 cate1
    se1 = snd3 cate1
    ca2 = fst3 cate2
    se2 = snd3 cate2
    lcate2 = leftCate ca2
    isAvail = (ca1 == rightCate lcate2) && (head (midSlash lcate2) == '\\') && (midSlash ca2 == "\\x" || midSlash ca2 == "\\.")

-- Backward type raising and harmonic composition: (Y/X)\Z X -> (Y/X)\Z Y\(Y/X)-> Y\Z
raiBh :: (Category,Seman,PhraStru) -> (Category,Seman,PhraStru) -> (Category, Tag, Seman, PhraStru, Act)
raiBh cate1 cate2
    | isPrimitive ca1 || isPrimitive lcate1 || isX lcate1 = (nilCate, "<T-<B", "", "", False)
    | isAvail = (derivate (leftCate lcate1) (midSlash ca1) (rightCate ca1), "<T-<B", semComb se2 se1, "NR", True)
    | otherwise = (nilCate, "<T-<B", "", "", False)
    where
    ca1 = fst3 cate1
    se1 = snd3 cate1
    ca2 = fst3 cate2
    se2 = snd3 cate2
    lcate1 = leftCate ca1
    isAvail = (rightCate lcate1 == ca2) && (head (midSlash lcate1) == '/') && (midSlash ca1 == "\\#" || midSlash ca1 == "\\.")

-- Backward type raising and crossing composition: (Y/X)/Z X -> (Y/X)/Z Y\(Y/X)-> Y/Z
raiBc :: (Category,Seman,PhraStru) -> (Category,Seman,PhraStru) -> (Category, Tag, Seman, PhraStru, Act)
raiBc cate1 cate2
    | isPrimitive ca1 || isPrimitive lcate1 || isX lcate1 = (nilCate, "<T-<Bx", "", "", False)
    | isAvail = (derivate (leftCate lcate1) (midSlash ca1) (rightCate ca1), "<T-<Bx", semComb se2 se1, "NR", True)
    | otherwise = (nilCate, "<T-<Bx", "", "", False)
    where
    ca1 = fst3 cate1
    se1 = snd3 cate1
    ca2 = fst3 cate2
    se2 = snd3 cate2
    lcate1 = leftCate ca1
    isAvail = (rightCate lcate1 == ca2) && (head (midSlash lcate1) == '/') && (midSlash ca1 == "/x" || midSlash ca1 == "/.")

