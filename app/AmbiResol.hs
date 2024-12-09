-- Copyright (c) 2019-2024 China University of Water Resources and Electric Power,
-- All rights reserved.

module AmbiResol (
    SIdx,                -- Int
    LeftExtend,          -- [(Category,Tag,PhraStru)]
    LeftOver,            -- (Category,Tag,PhraStru)
    RightOver,           -- (Category,Tag,PhraStru)
    RightExtend,         -- [(Category,Tag,PhraStru)]
    ContextOfOT,         -- (LeftExtend, LeftOver, RightOver, RightExtend)
    Context2OverType,    -- (ContextOfOT, OverType)
    Context2OverTypeBase, -- [Context2OverType]
    nullContextOfOT,     -- ContextOfOT
    OverType,            -- Int
    Prior(..),           -- Prior and its all Constructors
    ContextOfSG,         -- (LeftExtend, LeftOver, RightOver, RightExtend, OverType)
    ClauTag,             -- (Int, Int), actually (SentIdx, ClauIdx)
    ClauTagPrior,        -- (ClauTag, Prior)
    stringToCTPListList,        -- String -> [[ClauTagPrior]]
    stringToCTPList,            -- String -> [ClauTagPrior]
    stringToClauTagPrior,       -- String -> ClauTagPrior
    removeFromCTPListByClauTag,    -- ClauTag -> [ClauTagPrior] -> [ClauTagPrior]
    hasClauTagInCTPList,        -- ClauTag -> [ClauTagPrior] -> Bool
    countPriorInCTPList,        -- Prior -> [ClauTagPrior] -> Int
    hasClauTagInSynAmbiResol,   -- ClauTag -> IO Bool
    hasSentSampleInSynAmbiResol,    -- SentIdx -> ClauIdx -> ClauIdx -> IO Bool
    removeClauTagPriorFromSynAmbiResol,     -- SentIdx -> ClauIdx -> ClauIdx -> IO ()
    Context2ClauTagPrior,       -- (ContextOfSG, [ClauTagPrior])
    Context2ClauTagPriorBase,   -- [Context2ClauTagPrior]
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
    readContextOfSGFromStr,       -- String -> ContextOfSG
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
    readStreamByContext2OverType,      -- [Context2OverType] -> S.InputStream [MySQLValue] -> IO [Context2OverType]
    readStreamByContext2ClauTagPrior,  -- [Context2ClauTagPrior] -> S.InputStream [MySQLValue] -> IO [Context2ClauTagPrior]
    readStreamByInt32U3TextInt8Text, -- [AmbiResol1Sample] -> S.InputStream [MySQLValue] -> IO [AmbiResol1Sample]
    ) where

import Category
import Phrase (Tag, PhraStru, PhraCate, getPhraCateFromString, getPhraCateListFromString, equalPhra)
import Utils
import Data.Tuple.Utils
import Text.Printf
import Corpus (SentIdx, ClauIdx)
import Database
import Database.MySQL.Base
import qualified Data.String as DS
import qualified System.IO.Streams as S

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
type OverType = Int                                -- Overlapping type, [0 .. 5]
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

-- Overtype context 'ContextOfOT', overtype and its context 'Context2OverType', and sample base of 'Context2OverType'.
type ContextOfOT = (LeftExtend, LeftOver, RightOver, RightExtend)
type Context2OverType = (ContextOfOT, OverType)
type Context2OverTypeBase = [Context2OverType]

{- Record which clauses select the ambiguity resolution policy (prior) under a certain stru_gene model context.
 - For a given context of stru-gene model sample, different clauses might select different resolution policies.
 - The type is used for storage of stru_gene samples.
 -}
type ClauTag = (Int, Int)                    -- Actually (SentIdx, ClauIdx)
type ClauTagPrior = (ClauTag, Prior)

-- Convert a [[String]] value to its corresponding [[ClauTagPrior]] value.
stringToCTPListList :: String -> [[ClauTagPrior]]
stringToCTPListList str = map stringToCTPList $ stringToList str

-- Convert a string list to its corresponding list of ClauTagPrior values.
stringToCTPList :: String -> [ClauTagPrior]
stringToCTPList str = map stringToClauTagPrior $ stringToList str

-- Convert a string to its corresponding ClauTag value.
stringToClauTagPrior :: String -> ClauTagPrior
stringToClauTagPrior str = (\x -> (stringToIntTuple (fst x), read (snd x) :: Prior)) $ stringToTuple str

-- Remove ClauTagPrior values whose ClauTag member equals to a given ClatTag value.
removeFromCTPListByClauTag :: ClauTag -> [ClauTagPrior] -> [ClauTagPrior]
removeFromCTPListByClauTag clauTag cTPList = filter (\ctp -> fst ctp /= clauTag) cTPList

-- count Prior values in [ClauTagPrior].
countPriorInCTPList :: Prior -> [ClauTagPrior] -> Int
countPriorInCTPList _ [] = 0
countPriorInCTPList prior (ctp:ctps)
   | prior == snd ctp = 1 + countPriorInCTPList prior ctps
   | otherwise = countPriorInCTPList prior ctps

-- Decide whether there is any sample for given ClauTag value in syntax ambiguity resolution samples database.
hasClauTagInSynAmbiResol :: ClauTag -> IO Bool
hasClauTagInSynAmbiResol clauTag = do
    confInfo <- readFile "Configuration"                                        -- Read the local configuration file
    let ambi_resol_model = getConfProperty "ambi_resol_model" confInfo          -- Syntax Ambiguity Resolution Model

    conn <- getConn
    let sqlstat = DS.fromString $ "select id, clauTagPrior from " ++ ambi_resol_model ++ " where clauTagPrior like '%" ++ show clauTag ++ "%';"
    stmt <- prepareStmt conn sqlstat
    (defs, is) <- queryStmt conn stmt []
    idAndCTPStrList <- readStreamByInt32UText [] is                             -- [(Int, ClauTagPriorStr)]
    close conn
    return $ elem True $ map (hasClauTagInCTPList clauTag) $ map stringToCTPList $ map snd idAndCTPStrList       -- [[ClauTagPrior]]

-- Remove ClauTagPrior tuples from syntax ambiguity resolution samples database.
removeClauTagPriorFromSynAmbiResol :: SentIdx -> ClauIdx -> ClauIdx -> IO ()
removeClauTagPriorFromSynAmbiResol sn clauIdxOfStart clauIdxOfEnd = do
    let clauIdxRange = [clauIdxOfStart .. clauIdxOfEnd]
    if clauIdxRange == []
      then putStrLn "removeClauTagPriorFromSynAmbiResol: Finished."
      else do
        putStrLn $ "removeClauTagPriorFromSynAmbiResol: Removing sample tags of clause " ++ show clauIdxOfStart ++ " begins ..."
        removeClauTagPriorFromSynAmbiResol' (sn, clauIdxOfStart)
        removeClauTagPriorFromSynAmbiResol sn (clauIdxOfStart + 1) clauIdxOfEnd

-- Remove one ClauTagPrior tuple from syntax ambiguity resolution samples database.
removeClauTagPriorFromSynAmbiResol' :: ClauTag -> IO ()
removeClauTagPriorFromSynAmbiResol' clauTag = do
    confInfo <- readFile "Configuration"                                        -- Read the local configuration file
    let ambi_resol_model = getConfProperty "ambi_resol_model" confInfo          -- Syntax Ambiguity Resolution Model

    conn <- getConn
    let sqlstat = DS.fromString $ "select id, clauTagPrior from " ++ ambi_resol_model ++ " where clauTagPrior like '%" ++ show clauTag ++ "%';"
    stmt <- prepareStmt conn sqlstat
    (defs, is) <- queryStmt conn stmt []
    idAndCTPStrList <- readStreamByInt32UText [] is                             -- [(Int, ClauTagPriorStr)]
--    putStrLn $"removeClauTagPriorFromSynAmbiResol': idAndCTPStrList: " ++ show idAndCTPStrList

    let cTPList2 = map (removeFromCTPListByClauTag clauTag) $ map stringToCTPList $ map snd idAndCTPStrList       -- [[ClauTagPrior]]
--    putStrLn $ "removeClauTagPriorFromSynAmbiResol': cTPList2: " ++ show cTPList2

    let lpHitCounts = map toMySQLInt16U $ map (countPriorInCTPList Lp) cTPList2
    let rpHitCounts = map toMySQLInt16U $ map (countPriorInCTPList Rp) cTPList2
    let nothHitCounts = map toMySQLInt16U $ map (countPriorInCTPList Noth) cTPList2

    let cTPList2' = map (toMySQLText . show) cTPList2                           -- [MySqlText]
    let ids = map toMySQLInt32U $ map fst idAndCTPStrList                       -- [MySQLInt32U]
    let rows = compFiveLists cTPList2' lpHitCounts rpHitCounts nothHitCounts ids               -- [[MySQLText, MySqlInt32U]]
--    putStrLn $ "removeClauTagPriorFromSynAmbiResol': rows: " ++ show rows
    let sqlstat' = DS.fromString $ "update " ++ ambi_resol_model ++ " set clauTagPrior = ?, lpHitCount = ?, rpHitCount = ?, nothHitCount = ? where id = ?"
    oks <- executeMany conn sqlstat' rows
    putStrLn $ "removeClauTagPriorFromSynAmbiResol': " ++ show (length oks) ++ " rows have been updated."
    close conn

-- Decide whether there is any sample for given SentIdx value and ClauIdx range in syntax ambiguity resolution samples database.
hasSentSampleInSynAmbiResol :: SentIdx -> ClauIdx -> ClauIdx -> IO Bool
hasSentSampleInSynAmbiResol sentIdx clauIdxOfStart clauIdxOfEnd =
    if clauIdxOfStart <= clauIdxOfEnd
      then do
        hasInFstClause <- hasClauTagInSynAmbiResol (sentIdx, clauIdxOfStart)
        if hasInFstClause
          then return True
          else hasSentSampleInSynAmbiResol sentIdx (clauIdxOfStart + 1) clauIdxOfEnd
      else return False

-- Decide whether there is any ClauTagPrior value with given ClauTag value in a [ClauTagPrior] value.
hasClauTagInCTPList :: ClauTag -> [ClauTagPrior] -> Bool
hasClauTagInCTPList _ [] = False
hasClauTagInCTPList clauTag (x:xs)
    | clauTag == fst x = True
    | otherwise = hasClauTagInCTPList clauTag xs

-- Overtype context 'ContextOfSG', clause-tagged prior and its context 'Context2ClauTagPrior', and sample base of 'Context2ClauTagPrior'.
type ContextOfSG = (LeftExtend, LeftOver, RightOver, RightExtend, OverType)
type Context2ClauTagPrior = (ContextOfSG, [ClauTagPrior])
type Context2ClauTagPriorBase = [Context2ClauTagPrior]

{- Null value of ContextOfOT, used for calculating similarity between ContextOfOTs.
 - Similarity is zero between nullPhraSyn and any other PhraSyn value.
 -}
nullContextOfOT :: ContextOfOT
nullContextOfOT = ([nullPhraSyn], nullPhraSyn, nullPhraSyn, [nullPhraSyn])

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
    (caStr, ta, ps)  = stringToTriple str
    ca = getCateFromString caStr

readPhraSynListFromStr :: String -> [PhraSyn]
readPhraSynListFromStr "[]" = []
readPhraSynListFromStr str = readPhraSynListFromStrList (stringToList str)

readPhraSynListFromStrList :: [String] -> [PhraSyn]
readPhraSynListFromStrList [] = []
readPhraSynListFromStrList (s:ss) = readPhraSynFromStr s : (readPhraSynListFromStrList ss)

{- ContextOfSG :: (LeftExtend, LeftOver, RightOver, RightExtend, OverType)
 - LeftExtend :: [PhraSyn]
 - LeftOver :: PhraSyn
 -}
readContextOfSGFromStr :: String -> ContextOfSG
readContextOfSGFromStr str = (le, lo, ro, re, ot)
    where
    quinTupleStr = stringToFiveTuple str
    le = readPhraSynListFromStr (fst5 quinTupleStr)
    lo = readPhraSynFromStr (snd5 quinTupleStr)
    ro = readPhraSynFromStr (thd5 quinTupleStr)
    re = readPhraSynListFromStr (fth5 quinTupleStr)
    ot = read (fif5 quinTupleStr) :: Int

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

{- Read a value from input stream [MySQLValue], change it into a Context2OverType value, append it
 - to existed Context2OverType list, then read the next until read Nothing.
 - Here [MySQLValue] is [MySQLText, MySQLText, MySQLText, MySQLText, MySQLInt8],
 - and Context2OverType is ((LeftExtend, LeftOver, RightOver, RightExtend), OverType).
 -}
readStreamByContext2OverType :: [Context2OverType] -> S.InputStream [MySQLValue] -> IO [Context2OverType]
readStreamByContext2OverType es is = do
    S.read is >>= \x -> case x of                                        -- Dumb element 'case' is an array with type [MySQLValue]
        Just x -> readStreamByContext2OverType (es ++ [((readPhraSynListFromStr (fromMySQLText (x!!0)),
                                                         readPhraSynFromStr (fromMySQLText (x!!1)),
                                                         readPhraSynFromStr (fromMySQLText (x!!2)),
                                                         readPhraSynListFromStr (fromMySQLText (x!!3))),
                                                         fromMySQLInt8 (x!!4))
                                                       ]) is
        Nothing -> return es

{- Read a value from input stream [MySQLValue], change it into a Context2ClauTagPrior value, append it
 - to existed Context2ClauTagPrior list, then read the next until read Nothing.
 - Here [MySQLValue] is [MySQLText, MySQLText, MySQLText, MySQLText, MySQLInt8, MySQLText],
 - and Context2ClauTagPrior is ((LeftExtend, LeftOver, RightOver, RightExtend, OverType), ClauTagPrior).
 -}
readStreamByContext2ClauTagPrior :: [Context2ClauTagPrior] -> S.InputStream [MySQLValue] -> IO [Context2ClauTagPrior]
readStreamByContext2ClauTagPrior es is = do
    S.read is >>= \x -> case x of                                        -- Dumb element 'case' is an array with type [MySQLValue]
        Just x -> readStreamByContext2ClauTagPrior (es ++ [((readPhraSynListFromStr (fromMySQLText (x!!0)),
                                                      readPhraSynFromStr (fromMySQLText (x!!1)),
                                                      readPhraSynFromStr (fromMySQLText (x!!2)),
                                                      readPhraSynListFromStr (fromMySQLText (x!!3)),
                                                      fromMySQLInt8 (x!!4)),
                                                      stringToCTPList (fromMySQLText (x!!5)))
                                                    ]) is
        Nothing -> return es

{- Read a value from input stream [MySQLValue], change it into a StruGeneSample value, append it
 - to existed StruGeneSample list, then read the next until read Nothing.
 - Here [MySQLValue] is [MySQLInt32,MySQLText, MySQLText, MySQLText, MySQLInt8, MySQLText].
 -}
readStreamByInt32U3TextInt8Text :: [AmbiResol1Sample] -> S.InputStream [MySQLValue] -> IO [AmbiResol1Sample]
readStreamByInt32U3TextInt8Text es is = do
    S.read is >>= \x -> case x of                                          -- Dumb element 'case' is an array with type [MySQLValue]
        Just x -> readStreamByInt32U3TextInt8Text (es ++ [(fromMySQLInt32U (x!!0),
                                                    getPhraCateFromString (fromMySQLText (x!!1)),
                                                    getPhraCateFromString (fromMySQLText (x!!2)),
                                                    getPhraCateListFromString (fromMySQLText (x!!3)),
                                                    fromMySQLInt8 (x!!4),
                                                    readPriorFromStr (fromMySQLText (x!!5)))]) is
        Nothing -> return es
