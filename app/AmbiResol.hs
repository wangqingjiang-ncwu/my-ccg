-- Copyright (c) 2019-2024 China University of Water Resources and Electric Power,
-- All rights reserved.

module AmbiResol (
    SIdx,                -- Int
    LeftExtend,          -- [(Category,Tag,PhraStru)]
    LeftOver,            -- (Category,Tag,PhraStru)
    RightOver,           -- (Category,Tag,PhraStru)
    RightExtend,         -- [(Category,Tag,PhraStru)]
    OverType,            -- Int
    Prior(..),           -- Prior and its all Constructors
    PhraSyn,             -- (Category, Tag, PhraStru)
    nullPhraSyn,         -- (Nil, "", "")
    StruGene,            -- (LeftExtend, LeftOver, RightOver, RightExtend, OverType, Prior)
    StruGeneSample,      -- (SIdx, LeftExtend, LeftOver, RightOver, RightExtend, OverType, Prior)
    nullStruGeneSample,  -- StruGeneSample
    nullStruGene,        -- StruGene
    OverPair,            -- (PhraCate, PhraCate, Prior)
    OverPairid,          -- (PhraCate, PhraCate, Prior, SIdx)
    LeftPhra,            -- PhraCate
    RightPhra,           -- PhraCate
    Context,             -- [PhraCate]
    AmbiResol1,          -- (LeftPhra, RightPhra, Context, OverType, Prior)
    AmbiResol1Sample,    -- (SIdx, LeftPhra, RightPhra, Context, OverType, Prior)
    readPriorFromStr,    -- String -> Prior
    readAmbiResol1FromStr,   -- String -> AmbiResol1
    readPhraSynFromStr,      -- String -> PhraSyn
    readPhraSynListFromStr,       -- String -> [PhraSyn]
    readStruGeneFromStr,          -- String -> StruGene
    readStruGeneListFromStr,      -- String -> [StruGene]
    scdToString,         -- Scd -> String
    nScdToString,        -- [Scd] -> String
    phraSynToString,     -- PhraSyn -> String
    nPhraSynToString,    -- [PhraSyn] -> String
    struGeneToString,    -- StruGene -> String
    nStruGeneToString,   -- [StruGene] -> String
    removeDup4OverPair,  -- [OverPair] -> [OverPair]
    hasDup4OverPair,     -- [OverPair] -> [OverPair]
    equal4OverPair,      -- OverPair -> OverPair -> Bool
    elem4OverPair,       -- OverPair -> [OverPair] -> Bool
    ) where

import Category
import Phrase (Tag, PhraStru, PhraCate, getPhraCateFromString, getPhraCateListFromString, equalPhra)
import Utils
import Data.Tuple.Utils
import Text.Printf

-- Syntactic attribues of a phrase, including its syntactic category, tag of rule by which the phrase is obtained, and structural type of the phrase.
-- In Chinese, sentential parsing includes morphological parsing, so rule 'A/n->' contains the two aspects.
type PhraSyn = (Category, Tag, PhraStru)

-- Null phrasal syntax.
nullPhraSyn :: PhraSyn
nullPhraSyn = (Nil, "", "")

{- To indicate which phrasal structure is more prior in an overlapping pair, the left-adjacent phrases and the right-
   adjacent phrases should be considered. As basic fragments, such quadruples would exist in many
   sentences, and act like human body genes.
   The structural gene is a 6-tuple (<leftExtend>, <leftOver>, <rightOver>, <rightExtend>, <overType>, <prior>), here
   <leftExtend> is the left adjacent phrases of <leftOver>, <rightExtend> is the right adjacent phrases of <rightOver>, and
   <leftOver> and <rightOver> are the left-to-right overlapping phrases, with <overType> to indicate overlapping type,
   and with <prior> to indicate which is prior to exist. Besides, <leftOver> and <rightOver> are at least one active.
 -}

type LeftExtend = [PhraSyn]     -- Left neighbors
type LeftOver = PhraSyn         -- Overlapping left phrase
type RightOver = PhraSyn        -- Overlapping right phrase
type RightExtend = [PhraSyn]    -- Right neighbors
type OverType = Int                             -- Overlapping type
data Prior = Lp | Rp | Noth deriving (Eq, Read)    -- Lp means left prior, Rp means right prior, Noth means nothing or both not.

instance Show Prior where
    show Lp = "Lp"
    show Rp = "Rp"
    show Noth = "Noth"

-- Define relation Ord between two values with type Prior such that two Prior values also can be compared.
instance Ord Prior where
    Lp < Lp = False
    Rp < Rp = False
    Noth < Noth = False
    Lp < Rp = True
    Lp < Noth = True
    Rp < Noth = True
    Rp < Lp = False
    Noth < Lp = False
    Noth < Rp = False
    Lp <= _ = True
    Rp <= Lp = False
    Rp <= Rp = True
    Rp <= Noth = True
    Noth <= Noth = True
    Noth <= Lp = False
    Noth <= Rp = False


-- The following defines No.0 ambiguity resolution model, and the original samples of which are stored in table 'stru_gene' of database 'ccg4c'.
type StruGene = (LeftExtend, LeftOver, RightOver, RightExtend, OverType, Prior)

-- Null struture gene is just used as a place holder.
nullStruGene :: StruGene
nullStruGene = ([], nullPhraSyn, nullPhraSyn, [], -1, Noth)

-- Sample index, used in multiple sample bases.
type SIdx = Int

-- Sample model of Syntax-Structural Genes
type StruGeneSample = (SIdx, LeftExtend, LeftOver, RightOver, RightExtend, OverType, Prior)

nullStruGeneSample :: StruGeneSample
nullStruGeneSample = (-1, [], nullPhraSyn, nullPhraSyn, [], -1, Noth)

-- An overlapping pair of phrasal categories, including its priority assignment, used in clause parsing.
type OverPair = (PhraCate, PhraCate, Prior)
type OverPairid = (PhraCate, PhraCate, Prior, SIdx)

-- The following defines No.1 ambiguity resolution model.

type LeftPhra = PhraCate                       -- Overlapping left phrase
type RightPhra = PhraCate                      -- Overlapping right phrase
type Context = [PhraCate]                      -- All phrases created to now except LeftPhra and RightPhra.
type AmbiResol1 = (LeftPhra, RightPhra, Context, OverType, Prior)      -- Ambiguity resolution model
type AmbiResol1Sample = (SIdx, LeftPhra, RightPhra, Context, OverType, Prior)

readPriorFromStr :: String -> Prior
readPriorFromStr str
    | str == "Lp" = Lp
    | str == "Rp" = Rp
    | str == "Noth" = Noth
    | otherwise = error "readPriorFromStr: String is invalid."

-- Get a sample of No.1 ambiguity resolution model from a string.
readAmbiResol1FromStr :: String -> AmbiResol1
readAmbiResol1FromStr str = (lp, rp, ct, ot, pr)
    where
    fiveTupleStr = stringToFiveTuple str
    lp = getPhraCateFromString (fst5 fiveTupleStr)
    rp = getPhraCateFromString (snd5 fiveTupleStr)
    ct = getPhraCateListFromString (thd5 fiveTupleStr)
    ot = read (fth5 fiveTupleStr) :: Int
    pr = readPriorFromStr (fif5 fiveTupleStr)

readPhraSynFromStr :: String -> PhraSyn
readPhraSynFromStr str = (ca, ta, ps)
    where
    tripleStr  = stringToTriple str
    ca = getCateFromString (fst3 tripleStr)
    ta = snd3 tripleStr
    ps = thd3 tripleStr

readPhraSynListFromStr :: String -> [PhraSyn]
readPhraSynListFromStr "[]" = []
readPhraSynListFromStr str = readPhraSynListFromStrList (stringToList str)

readPhraSynListFromStrList :: [String] -> [PhraSyn]
readPhraSynListFromStrList [] = []
readPhraSynListFromStrList (s:ss) = readPhraSynFromStr s : (readPhraSynListFromStrList ss)

{- StruGene :: (LeftExtend, LeftOver, RightOver, RightExtend, OverType, Prior)
 - LeftExtend :: [PhraSyn]
 - LeftOver :: PhraSyn
 -}
readStruGeneFromStr :: String -> StruGene
readStruGeneFromStr str = (le, lo, ro, re, ot, pr)
    where
    sixTupleStr = stringToSixTuple str
    le = readPhraSynListFromStr (fst6 sixTupleStr)
    lo = readPhraSynFromStr (snd6 sixTupleStr)
    ro = readPhraSynFromStr (thd6 sixTupleStr)
    re = readPhraSynListFromStr (fth6 sixTupleStr)
    ot = read (fif6 sixTupleStr) :: Int
    pr = readPriorFromStr (sth6 sixTupleStr)

readStruGeneListFromStr :: String -> [StruGene]
readStruGeneListFromStr str = map readStruGeneFromStr (stringToList str)

type CIdx = Int                     -- Cluster index
type DMin = Float                   -- Minimal distance between a sample and all cluster members
type Scd = (SIdx, CIdx, DMin)

-- Get the string of a Scd value.
scdToString :: Scd -> String
scdToString scd = "(" ++ sIdx ++ "," ++ cIdx ++ "," ++ dMin ++")"
    where
      sIdx = show (fst3 scd)
      cIdx = show (snd3 scd)
      dMin = printf "%.02f" (thd3 scd)

-- Get the string of [scd]
nScdToString :: [Scd] -> String
nScdToString scds = listToString (map scdToString scds)

-- Get the string of a PhraSyn
phraSynToString :: PhraSyn -> String
phraSynToString phs = "(" ++ ca ++ "," ++ ta ++ "," ++ ps ++")"
    where
      ca = show (fst3 phs)
      ta = snd3 phs
      ps = thd3 phs

-- Get the string of [PhraSyn]
nPhraSynToString :: [PhraSyn] -> String
nPhraSynToString nps = listToString (map phraSynToString nps)

-- Get the string of a StruGene
struGeneToString :: StruGene -> String
struGeneToString sg = "(" ++ le ++ "," ++ lo ++ "," ++ re ++ "," ++ ro ++ "," ++ ot ++ "," ++ pr ++ ")"
    where
      le = nPhraSynToString (fst6 sg)
      lo = phraSynToString (snd6 sg)
      re = phraSynToString (thd6 sg)
      ro = nPhraSynToString (fth6 sg)
      ot = show (fif6 sg)
      pr = show (sth6 sg)

-- Get the string of [StruGene]
nStruGeneToString :: [StruGene] -> String
nStruGeneToString sgs = listToString (map struGeneToString sgs)

-- Remove duplicate OverPair values in a list. When comparing two PhraCate values, attribue Act is NOT considered.
removeDup4OverPair :: [OverPair] -> [OverPair]
removeDup4OverPair [] = []
removeDup4OverPair [x] = [x]
removeDup4OverPair (x:xs)
    | elem4OverPair x xs = removeDup4OverPair xs
    | otherwise = x : removeDup4OverPair xs

-- Decide whether there are duplicate OverPair values in a list.
hasDup4OverPair :: [OverPair] -> Bool
hasDup4OverPair [] = False
hasDup4OverPair [x] = False
hasDup4OverPair (x:xs)
    | elem4OverPair x xs = True
    | otherwise = hasDup4OverPair xs

-- Without considering attribute Act in PhraCate, decide whether two OverPair values are equal or not.
equal4OverPair :: OverPair -> OverPair -> Bool
equal4OverPair x y = equalPhra (fst3 x) (fst3 y) && equalPhra (snd3 x) (snd3 y) && thd3 x == thd3 y

-- Without considering attribute Act in PhraCate, decide whether a OverPair value is in a given OverPair list.
elem4OverPair :: OverPair -> [OverPair] -> Bool
elem4OverPair _ [] = False
elem4OverPair op (op1:ops) = case (equal4OverPair op op1) of
                               True -> True
                               False -> elem4OverPair op ops
