-- Copyright (c) 2019-2021 China University of Water Resources and Electric Power
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
    Rule(..),         -- Enumerated type for the tags of category-converted rules
    ccTags,           -- Tags of category-converted rules
    OnOff,            -- [Rule], Rule used is the one in this module
    ruleOn,           -- Rule -> OnOff -> OnOff
    ruleOff,          -- Rule -> OnOff -> OnOff
    updateOnOff,      -- [Rule] -> [String] -> [Rule]
    showOnOff         -- [Rule] -> IO ()
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
    | ca1 == aux6Cate = (lca, ">", semComb se1 se2, "U6P", True)
    | ca1 == conjCate1 = (derivate ca2 "\\*" ca2, ">", semComb se1 se2, "XX", True)
    | ca1 == conjCate3 = (ca2, ">", semComb se1 se2, "CC", True)
    | isAvail && cateEqual ca1 adjCate = (leftCate ca1, ">", semComb se1 se2, "AHn", True)
    | isAvail && cateEqual ca1 (getCateFromString "(s\\.np)/.np") = (lca, ">", semComb se1 se2, "VO", True)
    | isAvail && cateEqual ca1 (getCateFromString "((s\\.np)/.np)/.np") = (lca, ">", semComb se1 se2, "VO", True)
    | isAvail && cateEqual ca1 (getCateFromString "(s\\.np)/#(s\\.np)") = (lca, ">", semComb se1 se2, "DHv", True)
    | isAvail && (cateEqual ca1 (getCateFromString "((s\\.np)\\x(s\\.np))/*np") || cateEqual ca1 (getCateFromString "((s\\.np)/#(s\\.np))/*np") || cateEqual ca1 (getCateFromString "(s/*s)/*np")) = (leftCate ca1, ">", semComb se1 se2, "PO", True)
    | isAvail && cateEqual ca1 (getCateFromString "(np/.np)/.(np/.np)") = (lca, ">", semComb se1 se2, "DHa", True)
    | isAvail && cateEqual ca1 aux3Cate = (leftCate ca1, ">", semComb se1 se2, "U3P", True)
    | isAvail && cateEqual ca1 (getCateFromString "s/*s") = (sCate, ">", semComb se1 se2, "DHs", True)
    | isAvail && cateEqual ca1 (getCateFromString "s/.np") = (npCate, ">", semComb se1 se2, "AHn", True)
    | isAvail =  (leftCate ca1, ">", semComb se1 se2, "NR", True)
    | otherwise = (nilCate, ">", "", "", False)
    where
    ca1 = fst3 cate1
    se1 = snd3 cate1
    ca2 = fst3 cate2
    se2 = snd3 cate2
    lca = leftCate ca1
    isAvail = head (midSlash ca1) == '/' && (rightCate ca1 == ca2 || rightCate ca1 == xCate)

-- CCG backward application, here using nonstrict equality.
appB :: (Category,Seman,PhraStru) -> (Category,Seman,PhraStru) -> (Category, Tag, Seman, PhraStru, Act)
appB cate1 cate2
    | isPrimitive ca2 = (nilCate, "<", "", "", False)
    | ca2 == getCateFromString "X\\*X" && ps1 == "XX" = (ca1, "<", semComb se2 se1, "U5P", True)
    | ca2 == getCateFromString "X\\*X" =  (ca1, "<", semComb se2 se1, "TP", True)
    | isAvail && ps2 == "XX" = (leftCate ca2, "<", semComb se2 se1, "XX", True)
    | isAvail && (ca1 == quantityCate || ca1 == adjCate) = (leftCate ca2, "<", semComb se2 se1, "MQ", True)
    | isAvail && ca2 == aux1Cate = (leftCate ca2, "<", semComb se2 se1, "U1P", True)
    | isAvail && ca2 == getCateFromString "(np/*np)\\*X" = (leftCate ca2, "<", semComb se2 se1, "U1P", True)          -- Obsolted!
    | isAvail && cateEqual ca2 (getCateFromString "s\\.np") = (leftCate ca2, "<", semComb se2 se1, "SP", True)
    | isAvail && ca1 == npCate && cateEqual ca2 (getCateFromString "np\\.np") = (leftCate ca2, "<", semComb se2 se1, "HnC", True)
    | isAvail && cateEqual ca2 (getCateFromString "(s\\.np)\\x(s\\.np)") = (leftCate ca2, "<", semComb se2 se1, "HvC", True)
    | isAvail && ca2 == aux2Cate = (leftCate ca2, "<", semComb se2 se1, "U2P", True)
    | isAvail =  (leftCate ca2, "<", semComb se2 se1, "NR", True)
    | otherwise = (nilCate, "<", "", "", False)
    where
    ca1 = fst3 cate1
    se1 = snd3 cate1
    ps1 = thd3 cate1
    ca2 = fst3 cate2
    se2 = snd3 cate2
    ps2 = thd3 cate2
    isAvail = head (midSlash ca2) == '\\' && (rightCate ca2 == ca1 || rightCate ca2 == xCate)

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
    | isAvail && cateEqual ca1 adjCate && cateEqual ca2 (getCateFromString "np\\.np") = (derivate (leftCate ca2) (midSlash ca1) (rightCate ca1), "<Bx", semComb se2 se1, "HaC", True)
    | isAvail = (derivate (leftCate ca2) (midSlash ca1) (rightCate ca1), "<Bx", semComb se2 se1, "NR", True)
    | otherwise = (nilCate, "<Bx", "", "", False)
    where
    ca1 = fst3 cate1
    se1 = snd3 cate1
    ca2 = fst3 cate2
    se2 = snd3 cate2
    isAvail = (midSlash ca1 == "/x" || midSlash ca1 == "/.") && (midSlash ca2 == "\\x" || midSlash ca2 == "\\.") && leftCate ca1 == rightCate ca2

-- CCG Forward type raising and harmonic composition: X (Y\X)/Z -> Y/(Y\X) (Y\X)/Z -> Y/Z
-- To now, the rule is only used for objective extraction and predicate extraction.
raiFh :: (Category,Seman,PhraStru) -> (Category,Seman,PhraStru) -> (Category, Tag, Seman, PhraStru, Act)
raiFh cate1 cate2
    | isPrimitive ca2 || isPrimitive lcate2 || isX lcate2 = (nilCate, ">T->B", "", "", False)
    | isAvail && (ca2 == verbCate || ca2 == verbCate2) = (derivate (leftCate lcate2) (midSlash ca2) (rightCate ca2), ">T->B", semComb se1 se2, "OE", True)
    | isAvail && ca2 == adverbalCate = (derivate (leftCate lcate2) (midSlash ca2) (rightCate ca2), ">T->B", semComb se1 se2, "PE", True)
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

{- All tags of context-sensitive category-converted rules:
   (1)S/s, (2)P/s, (3)O/s, (4)N/s, (5)A/s, (6)S/v, (7)O/v, (8)A/v, (9)Hn/v, (10)N/v, (11)D/v, (12)S/a,
   (13)O/a, (14)Hn/a, (15)N/a, (16)P/a, (17)D/a, (18)Cv/a, (19)Cn/a, (20)A/n, (21)P/n, (22)V/n, (23)Cn/n,
   (24)D/p, (25)N/oe.
 -}

ccTags = ["S/s","P/s","O/s","N/s","A/s","S/v","O/v","A/v","Hn/v","N/v","D/v","S/a","O/a","Hn/a","N/a","P/a","D/a","Cv/a","Cn/a","A/n","P/n","V/n","Cn/n","D/p","N/oe"]

{- The enumerated type Rule is for the tags of category-converted rules. Rule value throws away '/' because enumerated
   value can't include '/'.
 -}

data Rule = Ss | Ps | Os | Ns | As | Sv | Ov | Av | Hnv | Nv | Dv | Sa | Oa | Hna | Na | Pa | Va | Da | Cva | Cna | An | Pn | Vn | Cnn | Dp | Noe deriving (Eq)

-- Define how the tag of a category-converted rule shows as a letter string.
instance Show Rule where
    show Ss = "S/s"
    show Ps = "P/s"
    show Os = "O/s"
    show Ns = "N/s"
    show As = "A/s"
    show Sv = "S/v"
    show Ov = "O/v"
    show Av = "A/v"
    show Hnv = "Hn/v"
    show Nv = "N/v"
    show Dv = "D/v"
    show Sa = "S/a"
    show Oa = "O/a"
    show Hna = "Hn/a"
    show Na = "N/a"
    show Pa = "P/a"
    show Va = "V/a"
    show Da = "D/a"
    show Cva = "Cv/a"
    show Cna = "Cn/a"
    show An = "A/n"
    show Pn = "P/n"
    show Vn = "V/n"
    show Cnn = "Cn/n"
    show Dp = "D/p"
    show Noe = "N/oe"

-- OnOff is the list of Rule members
type OnOff = [Rule]

-- Turn On a Rule
ruleOn :: Rule -> OnOff -> OnOff
ruleOn ru onOff
    | elem ru onOff = onOff
    | otherwise = ru:onOff

-- Turn Off a Rule
ruleOff :: Rule -> OnOff -> OnOff
ruleOff ru onOff
    | notElem ru onOff = onOff
    | otherwise = [x| x <- onOff, x /= ru]

-- Update rule switches. For "+S/s", turn on Ss; For "-P/a", turn off Pa.
updateOnOff :: [Rule] -> [String] -> [Rule]
updateOnOff onOff rws
    | rws == [] = onOff
    | rw1 == "+S/s" = updateOnOff (ruleOn Ss onOff) rwt
    | rw1 == "-S/s" = updateOnOff (ruleOff Ss onOff) rwt
    | rw1 == "+P/s" = updateOnOff (ruleOn Ps onOff) rwt
    | rw1 == "-P/s" = updateOnOff (ruleOff Ps onOff) rwt
    | rw1 == "+O/s" = updateOnOff (ruleOn Os onOff) rwt
    | rw1 == "-O/s" = updateOnOff (ruleOff Os onOff) rwt
    | rw1 == "+N/s" = updateOnOff (ruleOn Ns onOff) rwt
    | rw1 == "-N/s" = updateOnOff (ruleOff Ns onOff) rwt
    | rw1 == "+A/s" = updateOnOff (ruleOn As onOff) rwt
    | rw1 == "-A/s" = updateOnOff (ruleOff As onOff) rwt
    | rw1 == "+S/v" = updateOnOff (ruleOn Sv onOff) rwt
    | rw1 == "-S/v" = updateOnOff (ruleOff Sv onOff) rwt
    | rw1 == "+O/v" = updateOnOff (ruleOn Ov onOff) rwt
    | rw1 == "-O/v" = updateOnOff (ruleOff Ov onOff) rwt
    | rw1 == "+A/v" = updateOnOff (ruleOn Av onOff) rwt
    | rw1 == "-A/v" = updateOnOff (ruleOff Av onOff) rwt
    | rw1 == "+Hn/v" = updateOnOff (ruleOn Hnv onOff) rwt
    | rw1 == "-Hn/v" = updateOnOff (ruleOff Hnv onOff) rwt
    | rw1 == "+N/v" = updateOnOff (ruleOn Nv onOff) rwt
    | rw1 == "-N/v" = updateOnOff (ruleOff Nv onOff) rwt
    | rw1 == "+D/v" = updateOnOff (ruleOn Dv onOff) rwt
    | rw1 == "-D/v" = updateOnOff (ruleOff Dv onOff) rwt
    | rw1 == "+S/a" = updateOnOff (ruleOn Sa onOff) rwt
    | rw1 == "-S/a" = updateOnOff (ruleOff Sa onOff) rwt
    | rw1 == "+O/a" = updateOnOff (ruleOn Oa onOff) rwt
    | rw1 == "-O/a" = updateOnOff (ruleOff Oa onOff) rwt
    | rw1 == "+Hn/a" = updateOnOff (ruleOn Hna onOff) rwt
    | rw1 == "-Hn/a" = updateOnOff (ruleOff Hna onOff) rwt
    | rw1 == "+N/a" = updateOnOff (ruleOn Na onOff) rwt
    | rw1 == "-N/a" = updateOnOff (ruleOff Na onOff) rwt
    | rw1 == "+P/a" = updateOnOff (ruleOn Pa onOff) rwt
    | rw1 == "-P/a" = updateOnOff (ruleOff Pa onOff) rwt
    | rw1 == "+V/a" = updateOnOff (ruleOn Va onOff) rwt
    | rw1 == "-V/a" = updateOnOff (ruleOff Va onOff) rwt
    | rw1 == "+D/a" = updateOnOff (ruleOn Da onOff) rwt
    | rw1 == "-D/a" = updateOnOff (ruleOff Da onOff) rwt
    | rw1 == "+Cv/a" = updateOnOff (ruleOn Cva onOff) rwt
    | rw1 == "-Cv/a" = updateOnOff (ruleOff Cva onOff) rwt
    | rw1 == "+Cn/a" = updateOnOff (ruleOn Cna onOff) rwt
    | rw1 == "-Cn/a" = updateOnOff (ruleOff Cna onOff) rwt
    | rw1 == "+A/n" = updateOnOff (ruleOn An onOff) rwt
    | rw1 == "-A/n" = updateOnOff (ruleOff An onOff) rwt
    | rw1 == "+P/n" = updateOnOff (ruleOn Pn onOff) rwt
    | rw1 == "-P/n" = updateOnOff (ruleOff Pn onOff) rwt
    | rw1 == "+V/n" = updateOnOff (ruleOn Vn onOff) rwt
    | rw1 == "-V/n" = updateOnOff (ruleOff Vn onOff) rwt
    | rw1 == "+Cn/n" = updateOnOff (ruleOn Cnn onOff) rwt
    | rw1 == "-Cn/n" = updateOnOff (ruleOff Cnn onOff) rwt
    | rw1 == "+D/p" = updateOnOff (ruleOn Dp onOff) rwt
    | rw1 == "-D/p" = updateOnOff (ruleOff Dp onOff) rwt
    | rw1 == "+N/oe" = updateOnOff (ruleOn Noe onOff) rwt
    | rw1 == "-N/oe" = updateOnOff (ruleOff Noe onOff) rwt
    | otherwise = error $ "updateOnOff: Rule switch " ++ rw1 ++ " is not cognizable."
      where
        rw1 = head rws
        rwt = tail rws

-- Output [Rule] on console
showOnOff :: [Rule] -> IO ()
showOnOff [] = putStrLn ":: OnOff"
showOnOff (r:rs) = do
    putStr (show r)
    putStr " "
    showOnOff rs
