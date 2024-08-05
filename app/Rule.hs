-- Copyright (c) 2019-2024 China University of Water Resources and Electric Power
-- All rights reserved.

module Rule (
    rules,    -- [(Category,Seman,PhraStru) -> (Category,Seman,PhraStru) -> (Category, Tag, Seman, PhraStru, Act)]
    ruleTags, -- [Tag], List of CCG rule tags
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
    raiBh2,   -- (Category,Seman,PhraStru) -> (Category,Seman,PhraStru) -> (Category, Tag, Seman, PhraStru, Act)
    raiBc,    -- (Category,Seman,PhraStru) -> (Category,Seman,PhraStru) -> (Category, Tag, Seman, PhraStru, Act)
    Rule(..),         -- Enumerated type for the tags of category-converted rules
    ccTags,           -- [String], List of category type-conversional tags
    lexRule,          -- [Rule]
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

-- rules = [appF, appB, comFh, comFh2, comBh, comFc, comBc, raiFh, raiFc, raiBh, raiBh2, raiBc]
rules = [appF, appB, comFh, comFh2, comBh, comFc, comBc, raiFh, raiBh, raiBh2]

{- In parsing trees, every combination should print its corresponding rule tag.
 - To now, the rules used for combining two phrases are not limited in the following.
 -}
ruleTags :: [Tag]
ruleTags = [">","<",">B",">B2","<B",">Bx","<Bx",">T->B",">T->Bx","<T-<B","<T-<B2","<T-<Bx"]

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
    | ca1 == conjCate = (derivate ca2 "\\*" ca2, ">", semComb se1 se2, "XX", True)
    | ca1 == conjCate4Forward = (ca2, ">", semComb se1 se2, "CC", True)
    | isAvail && cateEqual ca1 adjCate = (leftCate ca1, ">", semComb se1 se2, "AHn", True)
    | isAvail && cateEqual ca1 verbCate = (lca, ">", semComb se1 se2, "VO", True)
    | isAvail && cateEqual ca1 verbCate2 = (lca, ">", semComb se1 se2, "VO", True)
    | isAvail && (ca1 == advCate || ca1 == baPhraseCate) = (lca, ">", semComb se1 se2, "DHv", True)
    | isAvail && ca1 == baPhraseCate = (lca, ">", semComb se1 se2, "DHv", True)
    | isAvail && ca1 == prep2AdvCate = (leftCate ca1, ">", semComb se1 se2, "PO", True)
    | isAvail' && ca1 == advCate4Adj = (lca, ">", semComb se1 se2, "DHa", True)
    | isAvail && (ca1 == aux3Cate || ca1 == aux3dCate) = (leftCate ca1, ">", semComb se1 se2, "U3P", True)
    | isAvail && ca1 == advCate4Sent = (sCate, ">", semComb se1 se2, "DHs", True)
    | isAvail && ca1 == advCate4OE = (ca2, ">", semComb se1 se2, "DHv", True)
    | isAvail && ca1 == prefixCate = (lca, ">", semComb se1 se2, "HP", True)
    | isAvail && ca1 == advCate4OE = (lca, ">", semComb se1 se2, "DHoe", True)
    | isAvail && cateEqual ca1 (getCateFromString "s/.np") = (sCate, ">", semComb se1 se2, "NR", True)
    | isAvail && ca1 == prep2CompCate = (lca, ">", semComb se1 se2, "PO", True)
    | isAvail =  (leftCate ca1, ">", semComb se1 se2, "NR", True)
    | otherwise = (nilCate, ">", "", "", False)
    where
    ca1 = fst3 cate1
    se1 = snd3 cate1
    ca2 = fst3 cate2
    se2 = snd3 cate2
    lca = leftCate ca1
    isAvail = head (midSlash ca1) == '/' && (rightCate ca1 == ca2 || rightCate ca1 == xCate)
    isAvail' = head (midSlash ca1) == '/' && cateEqual (rightCate ca1) ca2

-- CCG backward application, here using nonstrict equality.
appB :: (Category,Seman,PhraStru) -> (Category,Seman,PhraStru) -> (Category, Tag, Seman, PhraStru, Act)
appB cate1 cate2
    | isPrimitive ca2 = (nilCate, "<", "", "", False)
    | ca2 == aux5Cate =  (ca1, "<", semComb se2 se1, "U5P", True)
    | ca2 == toneCate = (ca1, "<", semComb se2 se1, "TP", True)
    | ca2 == conjCate4Backward = (ca1, "<", semComb se2 se1, "CC", True)
    | isAvail && ca1 == numeralCate && ca2 == quantifierCate && ps2 /= "XX" = (leftCate ca2, "<", semComb se2 se1, "MQ", True)
    | isAvail' && ca1 == adjCate && ca2 == quantifierCate && ps2 /= "XX" = (leftCate ca2, "<", semComb se2 se1, "PQ", True)
    | isAvail' && ca1 == numeralCate && ca2 == adjCompCate && ps2 /= "XX" = (ca1, "<", semComb se2 se1, "HmC", True)
    | isAvail' && ca1 == pronCate4Numeral && ca2 == quantifierCate = (leftCate ca2, "<", semComb se2 se1, "PQ", True)
    | isAvail && ca2 == aux2Cate = (leftCate ca2, "<", semComb se2 se1, "U2P", True)
    | isAvail && ca1 == adjCate && ps2 /= "XX" = (leftCate ca2, "<", semComb se2 se1, "HaC", True)
    | isAvail && ca2 == aux1Cate = (leftCate ca2, "<", semComb se2 se1, "U1P", True)
    | isAvail && ca2 == getCateFromString "(np/*np)\\*X" = (leftCate ca2, "<", semComb se2 se1, "U1P", True)          -- Obsolted!
    | isAvail && cateEqual ca2 predCate = (leftCate ca2, "<", semComb se2 se1, "SP", True)
    | isAvail && ca1 == npCate && ca2 == nounCompCate && ps2 /= "XX" = (leftCate ca2, "<", semComb se2 se1, "HnC", True)
    | isAvail' && ca2 == verbCompCate = (leftCate ca2, "<", semComb se2 se1, "HvC", True)                             -- For '把' phrase, isAvail' is used.
    | isAvail && ca2 == postfixCate = (npCate, "<", semComb se2 se1, "KP", True)
    | isAvail && ca2 == prep4BeiCate = (leftCate ca2, "<", semComb se2 se1, "MOs", True)
    | (isAvail || isAvail') && ps2 == "XX" = (leftCate ca2, "<", semComb se2 se1, "XX", True)
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
    isAvail' = head (midSlash ca2) == '\\' && (cateEqual (rightCate ca2) ca1)

-- CCG forward harmonic composition, like X/Y Y/Z -> X/Z.
-- The rule is usually used for "adverbal + transitive verb" structure, and sometimes used for "PO+PO" structure, such as "从p 小学n 到p 中学n".
comFh :: (Category,Seman,PhraStru) -> (Category,Seman,PhraStru) -> (Category, Tag, Seman, PhraStru, Act)
comFh cate1 cate2
    | isPrimitive ca1 || isPrimitive ca2 = (nilCate, ">B", "", "", False)
    | isAvail && (cateEqual ca2 verbCate || cateEqual ca2 verbCate2 || ca1 == baPhraseCate) = (derivate (leftCate ca1) (midSlash ca2) (rightCate ca2), ">B", semComb se1 se2, "DHv", True)
    | isAvail && ca1 == advCate && ca2 == advCate && ps1 == "PO" && ps2 == "PO" = (advCate, ">B", semComb se1 se2, "PO", True)
    | isAvail && ca1 == advCate && ca2 == baPhraseCate = (baPhraseCate, ">B", semComb se1 se2, "DHas", True)
    | isAvail && ca1 == advCate && ca2 == advCate = (advCate, ">B", semComb se1 se2, "DHd", True)     -- 例，今天nt 下午nt
    | isAvail = (derivate (leftCate ca1) (midSlash ca2) (rightCate ca2), ">B", semComb se1 se2, "NR", True)
    | otherwise = (nilCate, ">B", "", "", False)
    where
    ca1 = fst3 cate1
    se1 = snd3 cate1
    ps1 = thd3 cate1
    ca2 = fst3 cate2
    se2 = snd3 cate2
    ps2 = thd3 cate2
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

{- CCG backward harmonic composition, like Y\Z X\Y -> X\Z.
 -}
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

-- CCG forward crossing composition, like X/xY Y\xZ -> X\xZ.
comFc :: (Category,Seman,PhraStru) -> (Category,Seman,PhraStru) -> (Category, Tag, Seman, PhraStru, Act)
comFc cate1 cate2
    | isPrimitive ca1 || isPrimitive ca2 = (nilCate, ">Bx", "", "", False)
    | isAvail && ca1 == advCate4DirecVerb && ca2 == verbCompCate = (verbCompCate, ">Bx", semComb se1 se2, "DHx", True)
    | isAvail = (derivate (leftCate ca1) (midSlash ca2) (rightCate ca2), ">Bx", semComb se1 se2, "NR", True)
    | otherwise = (nilCate, ">Bx", "", "", False)
    where
    ca1 = fst3 cate1
    se1 = snd3 cate1
    ca2 = fst3 cate2
    se2 = snd3 cate2
    isAvail = (midSlash ca1 == "/x" || midSlash ca1 == "/.") && (midSlash ca2 == "\\x" || midSlash ca2 == "\\.") && rightCate ca1 == leftCate ca2

-- CCG backward crossing composition, like Y/xZ X\xY ->X/xZ.
comBc :: (Category,Seman,PhraStru) -> (Category,Seman,PhraStru) -> (Category, Tag, Seman, PhraStru, Act)
comBc cate1 cate2
    | isPrimitive ca1 || isPrimitive ca2 = (nilCate, "<Bx", "", "", False)
    | isAvail && (ca1 == predCate || ca1 == verbCate || ca1 == verbCate2) && ca2 == verbCompCate = (ca1, "<Bx", semComb se2 se1, "HvC", True)
    | isAvail && (ca1 == advCate4DirecVerb && ca2 == verbCompCate) = (ca1, "<Bx", semComb se2 se1, "NR", True)     -- 没有形成短语DHx
--  | isAvail && cateEqual ca1 adjCate && cateEqual ca2 (getCateFromString "np\\.np") = (derivate (leftCate ca2) (midSlash ca1) (rightCate ca1), "<Bx", semComb se2 se1, "HaC", True)
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
    | isAvail && (ca2 == verbCate || ca2 == verbCate2) = (derivate (leftCate lcate2) (midSlash ca2) (rightCate ca2), ">T->B", semComb se2 se1, "OE", True)
    | isAvail && (ca2 == advCate || ca2 == baPhraseCate) = (derivate (leftCate lcate2) (midSlash ca2) (rightCate ca2), ">T->B", semComb se2 se1, "PE", True)
    | isAvail = (derivate (leftCate lcate2) (midSlash ca2) (rightCate ca2), ">T->B", semComb se2 se1, "NR", True)
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
    | isAvail = (derivate (leftCate lcate2) (midSlash ca2) (rightCate ca2), ">T->Bx", semComb se2 se1, "NR", True)
    | otherwise = (nilCate, ">T->Bx", "", "", False)
    where
    ca1 = fst3 cate1
    se1 = snd3 cate1
    ca2 = fst3 cate2
    se2 = snd3 cate2
    lcate2 = leftCate ca2
    isAvail = (ca1 == rightCate lcate2) && (head (midSlash lcate2) == '\\') && (midSlash ca2 == "\\x" || midSlash ca2 == "\\.")

{- Backward type raising and harmonic composition: (Y/X)\Z X -> (Y/X)\Z Y\(Y/X)-> Y\Z
 - '被'字结构 MOs
 - 被pb 他r 打vt => 被pb (他r 打vt)vi => (s/#(s/.np))\#np np (s\.np)/.np => (s/#(s/.np))\#np s/.np => (s/#(s/.np))\#np s\#(s/#(s/.np)) => s\#np, namely raiBh.
 - Seemingly, the functional name "raiBh" should be "raiBB", because 'h' is not necessary for representing harmonic，while backward direction in combination should be reflected.
 -}
raiBh :: (Category,Seman,PhraStru) -> (Category,Seman,PhraStru) -> (Category, Tag, Seman, PhraStru, Act)
raiBh cate1 cate2
    | isPrimitive ca1 || isPrimitive lcate1 || isX lcate1 = (nilCate, "<T-<B", "", "", False)
    | isAvail && ca1 == prep4BeiCate = (derivate (leftCate lcate1) (midSlash ca1) (rightCate ca1), "<T-<B", semComb se1 se2, "MOs", True)
    | isAvail = (derivate (leftCate lcate1) (midSlash ca1) (rightCate ca1), "<T-<B", semComb se1 se2, "NR", True)
    | otherwise = (nilCate, "<T-<B", "", "", False)
    where
    ca1 = fst3 cate1
    se1 = snd3 cate1
    ca2 = fst3 cate2
    se2 = snd3 cate2
    lcate1 = leftCate ca1
    isAvail = (rightCate lcate1 == ca2) && (head (midSlash lcate1) == '/') && (midSlash ca1 == "\\#" || midSlash ca1 == "\\.")

{- CCG backward type raising and backward harmonic composition^2:  ((Y/X)\Z)/W X-> ((Y/X)\Z)/W Y\(Y/X)-> (Y\Z)/W
 - Here, backward harmonic composition^2 is not pure, because the two parameters are not all backward.
 - To now, the rule is only used for '把' phrase.
 - For examples, ((s/.np)\#np)/#((s\.np)/.np) np => ((s/.np)\#np)/#((s\.np)/.np) s\.(s/.np) => (s\#np)/#((s\.np)/.np)
 - Similarly, the functional name should be "raiBB2".
 -}
raiBh2 :: (Category,Seman,PhraStru) -> (Category,Seman,PhraStru) -> (Category, Tag, Seman, PhraStru, Act)
raiBh2 cate1 cate2
    | isPrimitive ca1 || isPrimitive lcate1 || isX lcate1 || isPrimitive llcate1 || isX llcate1 = (nilCate, "<T-<B2", "", "", False)
    | isAvail && ca1 == prep4BaCate && ca2 == npCate = (derivate (derivate (leftCate llcate1) (midSlash lcate1) (rightCate lcate1)) (midSlash ca1) (rightCate ca1), "<T-<B2", semComb se1 se2, "MOv", True)
    | isAvail = (derivate (derivate (leftCate llcate1) (midSlash lcate1) (rightCate lcate1)) (midSlash ca1) (rightCate ca1), "<T-<B2", semComb se1 se2, "NR", True)
    | otherwise = (nilCate, "<T-<B2", "", "", False)
    where
    ca1 = fst3 cate1
    se1 = snd3 cate1
    ca2 = fst3 cate2
    se2 = snd3 cate2
    lcate1 = leftCate ca1
    llcate1 = leftCate lcate1
    isAvail = (ca2 == rightCate llcate1) && (head (midSlash llcate1) == '/') && (midSlash lcate1 == "\\#" || midSlash lcate1 == "\\.") && (midSlash ca1 == "/#" || midSlash ca1 == "/.")

-- Backward type raising and crossing composition: (Y/X)/Z X -> (Y/X)/Z Y\(Y/X)-> Y/Z
raiBc :: (Category,Seman,PhraStru) -> (Category,Seman,PhraStru) -> (Category, Tag, Seman, PhraStru, Act)
raiBc cate1 cate2
    | isPrimitive ca1 || isPrimitive lcate1 || isX lcate1 = (nilCate, "<T-<Bx", "", "", False)
    | isAvail = (derivate (leftCate lcate1) (midSlash ca1) (rightCate ca1), "<T-<Bx", semComb se1 se2, "NR", True)
    | otherwise = (nilCate, "<T-<Bx", "", "", False)
    where
    ca1 = fst3 cate1
    se1 = snd3 cate1
    ca2 = fst3 cate2
    se2 = snd3 cate2
    lcate1 = leftCate ca1
    isAvail = (rightCate lcate1 == ca2) && (head (midSlash lcate1) == '/') && (midSlash ca1 == "/x" || midSlash ca1 == "/.")

{- All tags of context-sensitive category-converted rules:
   (s1)S/s, (s2)P/s, (s3)O/s, (s4)A/s, (s5)Hn/s, (s6)N/s,
   (v1)S/v, (v2)O/v, (v3)A/v, (v4)Hn/v, (v5)D/v, (v6)Cn/v, (v7)Cv/v, (v8)N/v, (v9)P/vt, (v10)OE/vt, (v11)Vt/vi, (v12)A/vd,
   (a1)S/a, (a2)P/a, (a3)V/a, (a4)O/a, (a5)D/a, (a6)Da/a, (a7)Cn/a, (a8)Cv/a, (a9)Ca/a, (a10)Hn/a, (a11)N/a,
   (n1)P/n, (n2)V/n, (n3)A/n, (n4)Cn/n, (n4)Cv/n, (n6)D/n, (n7)Da/n, (n8)ADJ/n, (n9)S/nd, (n10)O/nd, (n11)Hn/nd,
   (d1)S/d, (d2)O/d, (d3)A/d, (d4)Hn/d, (d5)Cv/d, (d6)N/d, (d7)ADJ/d, (d8)Da/d, (d9)Ds/d, (d10)Dx/d, (d11)Doe/d,
   (p1)D/p,
   (oe1)O/oe, (oe2)Hn/oe, (oe3)N/oe,
   (pe1)N/pe,
   (q1)A/q,
   (c1)Jf/c, (c2)Jb/c,
   (au1)U3d/u3
 -}

ccTags = ["S/s","P/s","O/s","A/s","Hn/s","N/s",
          "S/v","O/v","A/v","Hn/v","D/v","Cn/v","Cv/v","N/v","P/vt","OE/vt","Vt/vi","A/vd",
          "S/a","P/a","V/a","O/a","D/a","Da/a","Cn/a","Cv/a","Ca/a","Hn/a","N/a",
          "P/n","V/n","A/n","Cn/n","Cv/n","D/n","Da/n","ADJ/n","S/nd","O/nd","Hn/nd",
          "S/d","O/d","A/d","Hn/d","Cv/d","N/d","ADJ/d","Da/d","Ds/d","Dx/d","Doe/d",
          "D/p",
          "O/oe","Hn/oe","N/oe",
          "N/pe",
          "A/q",
          "Jf/c","Jb/c",
          "U3d/u3"]

{- The enumerated type Rule is for the tags of category-converted rules. Rule value throws away '/' because enumerated
   value can't include '/'.
 -}

data Rule = Ss | Ps | Os | As | Hns | Ns
          | Sv | Ov | Av | Hnv | Dv | Cnv | Cvv | Nv | Pvt | OEvt | Vtvi | Avd
          | Sa | Pa | Va | Oa | Da | Daa | Cna | Cva | Caa | Hna | Na
          | Pn | Vn | An | Cnn | Cvn | Dn | Dan | ADJn | Snd | Ond | Hnnd
          | Sd | Od | Ad | Hnd | Cvd | Nd | ADJd | Dad | Dsd | Dxd | Doed
          | Dp
          | Ooe | Hnoe | Noe
          | Npe
          | Aq
          | Jfc | Jbc
          | U3du3 deriving (Eq)

lexRule :: [Rule]
lexRule = [Ss, Ps, Os, As, Hns, Ns, Sv, Ov, Av, Hnv, Dv, Cnv, Cvv, Nv, Pvt, OEvt, Vtvi, Avd, Sa, Oa, Hna, Na, Pa, Va, Da, Daa, Cva, Cna, Caa, Pn, Vn, An, Cnn, Cvn, Dn, Dan, ADJn, Snd, Ond, Hnnd, Sd, Od, Ad, Hnd, Cvd, Nd, ADJd, Dad, Dsd, Dxd, Doed, Dp, Ooe, Hnoe, Noe, Npe, Aq, Jfc, Jbc, U3du3]

-- Define how the tag of a category-converted rule shows as a letter string.
instance Show Rule where
    show Ss = "S/s"
    show Ps = "P/s"
    show Os = "O/s"
    show As = "A/s"
    show Hns = "Hn/s"
    show Ns = "N/s"
    show Sv = "S/v"
    show Ov = "O/v"
    show Av = "A/v"
    show Hnv = "Hn/v"
    show Dv = "D/v"
    show Cnv = "Cn/v"
    show Cvv = "Cv/v"
    show Nv = "N/v"
    show Pvt = "P/vt"
    show OEvt = "OE/vt"
    show Vtvi = "Vt/vi"
    show Avd = "A/vd"
    show Sa = "S/a"
    show Oa = "O/a"
    show Pa = "P/a"
    show Va = "V/a"
    show Da = "D/a"
    show Daa = "Da/a"
    show Cna = "Cn/a"
    show Cva = "Cv/a"
    show Caa = "Ca/a"
    show Hna = "Hn/a"
    show Na = "N/a"
    show Pn = "P/n"
    show Vn = "V/n"
    show An = "A/n"
    show Cnn = "Cn/n"
    show Cvn = "Cv/n"
    show Dn = "D/n"
    show Dan = "Da/n"
    show ADJn = "ADJ/n"
    show Snd = "S/nd"
    show Ond = "O/nd"
    show Hnnd = "Hn/nd"
    show Sd = "S/d"
    show Od = "O/d"
    show Ad = "A/d"
    show Hnd = "Hn/d"
    show Cvd = "Cv/d"
    show Nd = "N/d"
    show ADJd = "ADJ/d"
    show Dad = "Da/d"
    show Dsd = "Ds/d"
    show Dxd = "Dx/d"
    show Doed = "Doe/d"
    show Dp = "D/p"
    show Ooe = "O/oe"
    show Hnoe = "Hn/oe"
    show Noe = "N/oe"
    show Npe = "N/pe"
    show Aq = "A/q"
    show Jfc = "Jf/c"
    show Jbc = "Jb/c"
    show U3du3 = "U3d/u3"

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

-- Update rule switches. For "+N/s", turn on Ns; For "-P/a", turn off Pa.
updateOnOff :: [Rule] -> [String] -> [Rule]
updateOnOff onOff rws
    | rws == [] = onOff
    | rw1 == "+S/s" = updateOnOff (ruleOn Ss onOff) rwt
    | rw1 == "-S/s" = updateOnOff (ruleOff Ss onOff) rwt
    | rw1 == "+P/s" = updateOnOff (ruleOn Ps onOff) rwt
    | rw1 == "-P/s" = updateOnOff (ruleOff Ps onOff) rwt
    | rw1 == "+O/s" = updateOnOff (ruleOn Os onOff) rwt
    | rw1 == "-O/s" = updateOnOff (ruleOff Os onOff) rwt
    | rw1 == "+A/s" = updateOnOff (ruleOn As onOff) rwt
    | rw1 == "-A/s" = updateOnOff (ruleOff As onOff) rwt
    | rw1 == "+Hn/s" = updateOnOff (ruleOn Hns onOff) rwt
    | rw1 == "-Hn/s" = updateOnOff (ruleOff Hns onOff) rwt
    | rw1 == "+N/s" = updateOnOff (ruleOn Ns onOff) rwt
    | rw1 == "-N/s" = updateOnOff (ruleOff Ns onOff) rwt
    | rw1 == "+S/v" = updateOnOff (ruleOn Sv onOff) rwt
    | rw1 == "-S/v" = updateOnOff (ruleOff Sv onOff) rwt
    | rw1 == "+O/v" = updateOnOff (ruleOn Ov onOff) rwt
    | rw1 == "-O/v" = updateOnOff (ruleOff Ov onOff) rwt
    | rw1 == "+A/v" = updateOnOff (ruleOn Av onOff) rwt
    | rw1 == "-A/v" = updateOnOff (ruleOff Av onOff) rwt
    | rw1 == "+Hn/v" = updateOnOff (ruleOn Hnv onOff) rwt
    | rw1 == "-Hn/v" = updateOnOff (ruleOff Hnv onOff) rwt
    | rw1 == "+D/v" = updateOnOff (ruleOn Dv onOff) rwt
    | rw1 == "-D/v" = updateOnOff (ruleOff Dv onOff) rwt
    | rw1 == "+Cn/v" = updateOnOff (ruleOn Cnv onOff) rwt
    | rw1 == "-Cn/v" = updateOnOff (ruleOff Cnv onOff) rwt
    | rw1 == "+Cv/v" = updateOnOff (ruleOn Cvv onOff) rwt
    | rw1 == "-Cv/v" = updateOnOff (ruleOff Cvv onOff) rwt
    | rw1 == "+N/v" = updateOnOff (ruleOn Nv onOff) rwt
    | rw1 == "-N/v" = updateOnOff (ruleOff Nv onOff) rwt
    | rw1 == "+P/vt" = updateOnOff (ruleOn Pvt onOff) rwt
    | rw1 == "-P/vt" = updateOnOff (ruleOff Pvt onOff) rwt
    | rw1 == "+OE/vt" = updateOnOff (ruleOn OEvt onOff) rwt
    | rw1 == "-OE/vt" = updateOnOff (ruleOff OEvt onOff) rwt
    | rw1 == "+Vt/vi" = updateOnOff (ruleOn Vtvi onOff) rwt
    | rw1 == "-Vt/vi" = updateOnOff (ruleOff Vtvi onOff) rwt
    | rw1 == "+A/vd" = updateOnOff (ruleOn Avd onOff) rwt
    | rw1 == "-A/vd" = updateOnOff (ruleOff Avd onOff) rwt
    | rw1 == "+S/a" = updateOnOff (ruleOn Sa onOff) rwt
    | rw1 == "-S/a" = updateOnOff (ruleOff Sa onOff) rwt
    | rw1 == "+P/a" = updateOnOff (ruleOn Pa onOff) rwt
    | rw1 == "-P/a" = updateOnOff (ruleOff Pa onOff) rwt
    | rw1 == "+V/a" = updateOnOff (ruleOn Va onOff) rwt
    | rw1 == "-V/a" = updateOnOff (ruleOff Va onOff) rwt
    | rw1 == "+O/a" = updateOnOff (ruleOn Oa onOff) rwt
    | rw1 == "-O/a" = updateOnOff (ruleOff Oa onOff) rwt
    | rw1 == "+D/a" = updateOnOff (ruleOn Da onOff) rwt
    | rw1 == "-D/a" = updateOnOff (ruleOff Da onOff) rwt
    | rw1 == "+Da/a" = updateOnOff (ruleOn Daa onOff) rwt
    | rw1 == "-Da/a" = updateOnOff (ruleOff Daa onOff) rwt
    | rw1 == "+Cn/a" = updateOnOff (ruleOn Cna onOff) rwt
    | rw1 == "-Cn/a" = updateOnOff (ruleOff Cna onOff) rwt
    | rw1 == "+Cv/a" = updateOnOff (ruleOn Cva onOff) rwt
    | rw1 == "-Cv/a" = updateOnOff (ruleOff Cva onOff) rwt
    | rw1 == "+Ca/a" = updateOnOff (ruleOn Caa onOff) rwt
    | rw1 == "-Ca/a" = updateOnOff (ruleOff Caa onOff) rwt
    | rw1 == "+Hn/a" = updateOnOff (ruleOn Hna onOff) rwt
    | rw1 == "-Hn/a" = updateOnOff (ruleOff Hna onOff) rwt
    | rw1 == "+N/a" = updateOnOff (ruleOn Na onOff) rwt
    | rw1 == "-N/a" = updateOnOff (ruleOff Na onOff) rwt
    | rw1 == "+P/n" = updateOnOff (ruleOn Pn onOff) rwt
    | rw1 == "-P/n" = updateOnOff (ruleOff Pn onOff) rwt
    | rw1 == "+V/n" = updateOnOff (ruleOn Vn onOff) rwt
    | rw1 == "-V/n" = updateOnOff (ruleOff Vn onOff) rwt
    | rw1 == "+A/n" = updateOnOff (ruleOn An onOff) rwt
    | rw1 == "-A/n" = updateOnOff (ruleOff An onOff) rwt
    | rw1 == "+Cn/n" = updateOnOff (ruleOn Cnn onOff) rwt
    | rw1 == "-Cn/n" = updateOnOff (ruleOff Cnn onOff) rwt
    | rw1 == "+Cv/n" = updateOnOff (ruleOn Cvn onOff) rwt
    | rw1 == "-Cv/n" = updateOnOff (ruleOff Cvn onOff) rwt
    | rw1 == "+D/n" = updateOnOff (ruleOn Dn onOff) rwt
    | rw1 == "-D/n" = updateOnOff (ruleOff Dn onOff) rwt
    | rw1 == "+Da/n" = updateOnOff (ruleOn Dan onOff) rwt
    | rw1 == "-Da/n" = updateOnOff (ruleOff Dan onOff) rwt
    | rw1 == "+ADJ/n" = updateOnOff (ruleOn ADJn onOff) rwt
    | rw1 == "-ADJ/n" = updateOnOff (ruleOff ADJn onOff) rwt
    | rw1 == "+S/nd" = updateOnOff (ruleOn Snd onOff) rwt
    | rw1 == "-S/nd" = updateOnOff (ruleOff Snd onOff) rwt
    | rw1 == "+O/nd" = updateOnOff (ruleOn Ond onOff) rwt
    | rw1 == "-O/nd" = updateOnOff (ruleOff Ond onOff) rwt
    | rw1 == "+Hn/nd" = updateOnOff (ruleOn Hnnd onOff) rwt
    | rw1 == "-Hn/nd" = updateOnOff (ruleOff Hnnd onOff) rwt
    | rw1 == "+D/p" = updateOnOff (ruleOn Dp onOff) rwt
    | rw1 == "-D/p" = updateOnOff (ruleOff Dp onOff) rwt
    | rw1 == "+O/oe" = updateOnOff (ruleOn Ooe onOff) rwt
    | rw1 == "-O/oe" = updateOnOff (ruleOff Ooe onOff) rwt
    | rw1 == "+Hn/oe" = updateOnOff (ruleOn Hnoe onOff) rwt
    | rw1 == "-Hn/oe" = updateOnOff (ruleOff Hnoe onOff) rwt
    | rw1 == "+N/oe" = updateOnOff (ruleOn Noe onOff) rwt
    | rw1 == "-N/oe" = updateOnOff (ruleOff Noe onOff) rwt
    | rw1 == "+N/pe" = updateOnOff (ruleOn Npe onOff) rwt
    | rw1 == "-N/pe" = updateOnOff (ruleOff Npe onOff) rwt
    | rw1 == "+A/q" = updateOnOff (ruleOn Aq onOff) rwt
    | rw1 == "-A/q" = updateOnOff (ruleOff Aq onOff) rwt
    | rw1 == "+S/d" = updateOnOff (ruleOn Sd onOff) rwt
    | rw1 == "-S/d" = updateOnOff (ruleOff Sd onOff) rwt
    | rw1 == "+O/d" = updateOnOff (ruleOn Od onOff) rwt
    | rw1 == "-O/d" = updateOnOff (ruleOff Od onOff) rwt
    | rw1 == "+A/d" = updateOnOff (ruleOn Ad onOff) rwt
    | rw1 == "-A/d" = updateOnOff (ruleOff Ad onOff) rwt
    | rw1 == "+Hn/d" = updateOnOff (ruleOn Hnd onOff) rwt
    | rw1 == "-Hn/d" = updateOnOff (ruleOff Hnd onOff) rwt
    | rw1 == "+Cv/d" = updateOnOff (ruleOn Cvd onOff) rwt
    | rw1 == "-Cv/d" = updateOnOff (ruleOff Cvd onOff) rwt
    | rw1 == "+N/d" = updateOnOff (ruleOn Nd onOff) rwt
    | rw1 == "-N/d" = updateOnOff (ruleOff Nd onOff) rwt
    | rw1 == "+ADJ/d" = updateOnOff (ruleOn ADJd onOff) rwt
    | rw1 == "-ADJ/d" = updateOnOff (ruleOff ADJd onOff) rwt
    | rw1 == "+Da/d" = updateOnOff (ruleOn Dad onOff) rwt
    | rw1 == "-Da/d" = updateOnOff (ruleOff Dad onOff) rwt
    | rw1 == "+Ds/d" = updateOnOff (ruleOn Dsd onOff) rwt
    | rw1 == "-Ds/d" = updateOnOff (ruleOff Dsd onOff) rwt
    | rw1 == "+Dx/d" = updateOnOff (ruleOn Dxd onOff) rwt
    | rw1 == "-Dx/d" = updateOnOff (ruleOff Dxd onOff) rwt
    | rw1 == "+Doe/d" = updateOnOff (ruleOn Doed onOff) rwt
    | rw1 == "-Doe/d" = updateOnOff (ruleOff Doed onOff) rwt
    | rw1 == "+Jf/c" = updateOnOff (ruleOn Jfc onOff) rwt
    | rw1 == "-Jf/c" = updateOnOff (ruleOff Jfc onOff) rwt
    | rw1 == "+Jb/c" = updateOnOff (ruleOn Jbc onOff) rwt
    | rw1 == "-Jb/c" = updateOnOff (ruleOff Jbc onOff) rwt
    | rw1 == "+U3d/u3" = updateOnOff (ruleOn U3du3 onOff) rwt
    | rw1 == "-U3d/u3" = updateOnOff (ruleOff U3du3 onOff) rwt
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
