-- Copyright (c) 2019-2025 China University of Water Resources and Electric Power
-- All rights reserved.

module AmbiResol (
    PhraSyn0,            -- (Category, Tag, PhraStru)
    PhraSyn,             -- (Category, Tag, PhraStru, Span)
    nullPhraSyn0,         -- (Nil, "", "")
    nullPhraSyn,         -- (Nil, "", "", -1)
    SIdx,                -- Int
    LeftExtend,          -- [(Category, Tag, PhraStru, Span)]
    LeftOver,            -- (Category, Tag, PhraStru, Span)
    RightOver,           -- (Category, Tag, PhraStru, Span)
    RightExtend,         -- [(Category, Tag, PhraStru, Span)]
    ContextOfOT,         -- (LeftExtend, LeftOver, RightOver, RightExtend)
    Context2OverType,    -- (ContextOfOT, OverType)
    Context2OverTypeBase,       -- [Context2OverType]
    nullContextOfOT,     -- ContextOfOT
    OverType,            -- Int
    Prior(..),           -- Prior and its all Constructors
    SIdxPrior,           -- (SIdx, Prior)
    ContextOfSG,         -- (LeftExtend, LeftOver, RightOver, RightExtend, OverType)
    ClauTag,             -- (SentIdx, ClauIdx)
    ClauTagPrior,        -- (ClauTag, Prior)
    stringToCTPListList,        -- String -> [[ClauTagPrior]]
    stringToCTPList,            -- String -> [ClauTagPrior]
    stringToClauTagPrior,       -- String -> ClauTagPrior
    removeFromCTPListByClauTag,        -- ClauTag -> [ClauTagPrior] -> [ClauTagPrior]
    removeFromCTPListBySentIdxRange,   -- [ClauTagPrior] -> SentIdx -> SentIdx -> [ClauTagPrior]
    hasClauTagInCTPList,        -- ClauTag -> [ClauTagPrior] -> Bool
    hasDupClauTagInCTPList,     -- [ClauTagPrior] -> Bool
    filterInCTPListByClauTag,   -- ClauTag -> [ClauTagPrior] -> [ClauTagPrior]
    hasSentIdxInCTPList,        -- SentIdx -> [ClauTagPrior] -> Bool

    countPriorInCTPList,        -- Prior -> [ClauTagPrior] -> Int
    priorWithHighestFreq,       -- [ClauTagPrior] -> Prior
    isPriorAmbiInClauTagPriorList,  -- [ClauTagPrior] -> Bool
    fromMaybePrior,             -- Maybe Prior -> Prior
    hasClauTagInSynAmbiResol,   -- ClauTag -> IO Bool
    Purity,                     -- Float
    purityOfMajorPrior,         -- [Prior] -> (Prior, Purity)

    getSentRangeByStruGeneSamples,  -- [StruGene2Sample] -> (SentIdx, SentIdx) -> (SentIdx, SentIdx)
    hasSentSampleInSynAmbiResol,    -- SentIdx -> ClauIdx -> ClauIdx -> IO Bool
    removeClauTagPriorFromSynAmbiResol,     -- SentIdx -> ClauIdx -> ClauIdx -> IO ()
    Context2ClauTagPrior,       -- (ContextOfSG, [ClauTagPrior])
    Context2ClauTagPriorBase,   -- [Context2ClauTagPrior]
    StruGene,            -- (LeftExtend, LeftOver, RightOver, RightExtend, OverType, Prior)
    getContextFromStruGene,     -- StruGene -> ContextOfSG
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
    readPhraSyn0FromStr,     -- String -> PhraSyn0
    readPhraSynFromStr,      -- String -> PhraSyn
    readPhraSyn0ListFromStr,      -- String -> [PhraSyn0]
    readPhraSynListFromStr,       -- String -> [PhraSyn]
    readBiTreePhraSyn0FromStr,     -- String -> BiTree PhraSyn0
    readBiTreePhraSynFromStr,     -- String -> BiTree PhraSyn
    readContextOfSGFromStr,       -- String -> ContextOfSG
    readContextOfSG3a0FromStr,    -- String -> ContextOfSG3a0
    readStruGeneFromStr,          -- String -> StruGene
    readStruGeneListFromStr,      -- String -> [StruGene]
    Scd,                 -- (SIdx, CIdx, DMin)
    scdToString,         -- Scd -> String
    nScdToString,        -- [Scd] -> String
    phraSyn0ToString,    -- PhraSyn0 -> String
    phraSynToString,     -- PhraSyn -> String
    nPhraSyn0ToString,   -- [PhraSyn0] -> String
    nPhraSynToString,    -- [PhraSyn] -> String
    contextOfSGToString, -- ContextOfSG -> String
    struGeneToString,    -- StruGene -> String
    nStruGeneToString,   -- [StruGene] -> String
    removeDup4OverPair,  -- [OverPair] -> [OverPair]
    hasDup4OverPair,     -- [OverPair] -> [OverPair]
    equal4OverPair,      -- OverPair -> OverPair -> Bool
    elem4OverPair,       -- OverPair -> [OverPair] -> Bool
    getBanPCsByOverPairs,         -- [OverPair] -> BanPCs
    readStreamByContext2OverType,      -- [Context2OverType] -> S.InputStream [MySQLValue] -> IO [Context2OverType]
    StruGene2,           -- (LeftExtend, LeftOver, RightOver, RightExtend, OverType, [ClauTagPrior])
    StruGene2Sample,     -- (SIdx, LeftExtend, LeftOver, RightOver, RightExtend, OverType, [ClauTagPrior])
    getContextFromStruGene2,           -- StruGene2 -> ContextOfSG
    getContextFromStruGene2Sample,     -- StruGene2Sample -> ContextOfSG
    fromStruGene2ByHighestFreqPrior,   -- StruGene2 -> StruGene
    readStreamByContext2ClauTagPrior,  -- [Context2ClauTagPrior] -> S.InputStream [MySQLValue] -> IO [Context2ClauTagPrior]
    readStreamByContextOfSG3a02ClauTagPrior,  -- [ContextOfSG3a02ClauTagPrior] -> S.InputStream [MySQLValue] -> IO [ContextOfSG3a02ClauTagPrior]
    readStreamByContextOfSG3a2ClauTagPrior,   -- [ContextOfSG3a2ClauTagPrior] -> S.InputStream [MySQLValue] -> IO [ContextOfSG3a2ClauTagPrior]
    readStreamByStruGene2Sample,       -- [StruGene2Sample] -> S.InputStream [MySQLValue] -> IO [StruGene2Sample]
    readStreamByInt32U3TextInt8Text,   -- [AmbiResol1Sample] -> S.InputStream [MySQLValue] -> IO [AmbiResol1Sample]
    readStreamByInt16UInt16UFloat,     -- [((SIdx,SIdx),Float)] -> S.InputStream [MySQLValue] -> IO [((SIdx,SIdx),Float)]
    readStreamByInt16UInt16UDouble,     -- [((SIdx,SIdx),Double)] -> S.InputStream [MySQLValue] -> IO [((SIdx,SIdx),Double)]
    SynAmbiResolMethod,  -- String
    rmNullCTPRecordsFromDB,       -- IO ()

    LeftOverTree,        -- BiTree PhraSyn
    RightOverTree,       -- BiTree PhraSyn
    StruGene3,           -- (LeftExtend, LeftOverTree, RightOverTree, RightExtend, OverType, [ClauTagPrior])
    StruGene3Sample,     -- (SIdx, LeftExtend, LeftOverTree, RightOverTree, RightExtend, OverType, [ClauTagPrior])
    ContextOfOT3a,       -- (LeftExtend, LeftOverTree, RightOverTree, RightExtend)
    StruGene3a,          -- (LeftOverTree, RightOverTree, [ClauTagPrior])
    StruGene3aSample,    -- (SIdx, LeftOverTree, RightOverTree, [ClauTagPrior])
    ContextOfSG3a,       -- (LeftOverTree, RightOverTree)

    LeftOverTree0,       -- BiTree PhraSyn0
    RightOverTree0,      -- BiTree PhraSyn0
    StruGene3a0,         -- (LeftOverTree0, RightOverTree0, [ClauTagPrior])
    StruGene3a0Sample,   -- (SIdx, LeftOverTree0, RightOverTree0, [ClauTagPrior])
    ContextOfSG3a0,      -- (LeftOverTree0, RightOverTree0)

    ContextOfSG3a2ClauTagPrior,       -- (ContextOfSG3a, [ClauTagPrior])
    ContextOfSG3a2ClauTagPriorBase,   -- [ContextOfSG3a2ClauTagPrior]
    ContextOfSG3a02ClauTagPrior,       -- (ContextOfSG3a0, [ClauTagPrior])
    ContextOfSG3a02ClauTagPriorBase,   -- [ContextOfSG3a02ClauTagPrior]
    phraCateTree2PhraSynTree,     -- BiTree PhraCate -> BiTree PhraSyn
    phraCateTree2PhraSyn0Tree,    -- BiTree PhraCate -> BiTree PhraSyn0

    phraCate2PhraSyn0,   -- PhraCate -> PhraSyn0
    phraCate2PhraSyn,    -- PhraCate -> PhraSyn
    biTreePhraCate2BiTreePhraSyn,   -- BiTree PhraCate -> BiTree PhraSyn
    biTreePhraCate2BiTreePhraSyn0,  -- BiTree PhraCate -> BiTree PhraSyn0

    phraSyns2RulesToString,    -- ([PhraSyn],[Rule]) -> String
    phraSyn0s2RulesToString,   -- ([PhraSyn0],[Rule]) -> String
    biTreePhraSynToString,     -- BiTree PhraSyn -> String
    biTreePhraSyn0ToString,    -- BiTree PhraSyn0 -> String
    biTreePhraSyn2RulesToString,    -- (BiTree PhraSyn,[Rule]) -> String
    biTreePhraSyn02RulesToString,   -- (BiTree PhraSyn0,[Rule]) -> String
    stringToPhraSyns2Rules,     -- String -> ([PhraSyn],[Rule])
    stringToPhraSyn0s2Rules,    -- String -> ([PhraSyn0],[Rule])
    stringToBiTreePhraSyn,      -- String -> BiTree PhraSyn
    stringToBiTreePhraSyn0,     -- String -> BiTree PhraSyn0
    stringToBiTreePhraSyn2Rules,    -- String -> (BiTree PhraSyn, [Rule])
    stringToBiTreePhraSyn02Rules,   -- String -> (BiTree PhraSyn0, [Rule])

    ContextOfCC1,        -- [PhraSyn]
    ContextOfCC2,        -- [PhraSyn0]
    ContextOfCC3,        -- [BiTree PhraSyn]
    ContextOfCC4,        -- [BiTree PhraSyn0]
    SIdxRules,           -- (SIdx,[Rule])
    meanRulesSimOnOneSample,   -- M.Map (SIdx,SIdx) Double -> [(SIdx,[Rule])] -> (SIdx,[Rule]) -> Double

    ) where

import Category
import Phrase
import Rule (Rule)
import Utils
import Data.List (nub, elemIndex)
import Data.Tuple.Utils
import Text.Printf
import Corpus
import Database
import Database.MySQL.Base
import qualified Data.String as DS
import qualified System.IO.Streams as S
import qualified Data.Map.Strict as M

{- Syntactic attribues of a phrase, including its syntactic category, tag of grammar rule by which the phrase is obtained,
 - structural type of the phrase, and phrasal span.
 - In Chinese, sentential parsing includes morphological parsing, so rule 'A/n->' contains the two aspects.
 -}
type PhraSyn0 = (Category, Tag, PhraStru)
type PhraSyn = (Category, Tag, PhraStru, Span)

-- Null phrasal syntax, in which grammar tag, phrasal structure and phrasal span use their not existing values.
nullPhraSyn :: PhraSyn
nullPhraSyn = (Nil, "", "", -1)

nullPhraSyn0 :: PhraSyn0
nullPhraSyn0 = (Nil, "", "")

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
type OverType = Int             -- Overlapping type, [0 .. 5]
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

-- Using for K-Means clustering.
type SIdxPrior = (SIdx, Prior)

-- Overtype context 'ContextOfOT', overtype and its context 'Context2OverType', and sample base of 'Context2OverType'.
type ContextOfOT = (LeftExtend, LeftOver, RightOver, RightExtend)
type Context2OverType = (ContextOfOT, OverType)
type Context2OverTypeBase = [Context2OverType]

{- Record which clauses select the ambiguity resolution policy (prior) under a certain stru_gene model context.
 - For a given context of stru-gene model sample, different clauses might select different resolution policies.
 - The type is used for storage of stru_gene samples.
 -}
type ClauTag = (SentIdx, ClauIdx)
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

-- Remove ClauTagPrior values whose SentIdx member equals to a given SentIdx value.
removeFromCTPListBySentIdxRange :: [ClauTagPrior] -> SentIdx -> SentIdx -> [ClauTagPrior]
removeFromCTPListBySentIdxRange cTPList startSn endSn = filter (\ctp -> elem ((fst . fst) ctp) [startSn .. endSn]) cTPList

-- Count occurence number of a given Prior value in [ClauTagPrior].
countPriorInCTPList :: Prior -> [ClauTagPrior] -> Int
countPriorInCTPList _ [] = 0
countPriorInCTPList prior (ctp:ctps)
   | prior == snd ctp = 1 + countPriorInCTPList prior ctps
   | otherwise = countPriorInCTPList prior ctps

{- Get the Prior value with highest frequency from a ClauTagPrior value list.
 - If multiple Prior values appear with same frequency, return first value according to order of Lp, Rp, to Noth.
 -}
priorWithHighestFreq :: [ClauTagPrior] -> Maybe Prior
priorWithHighestFreq [] = Nothing
priorWithHighestFreq clauTagPriorList
    | lpCount == maxFreq = Just Lp
    | rpCount == maxFreq = Just Rp
    | otherwise = Just Noth
    where
      lpCount = countPriorInCTPList Lp clauTagPriorList
      rpCount = countPriorInCTPList Rp clauTagPriorList
      nothCount = countPriorInCTPList Noth clauTagPriorList
      maxFreq = maximum [lpCount, rpCount, nothCount]

{- Decide whether attribute Prior is ambiguitious in a ClauTagPrior value list.
 -}
isPriorAmbiInClauTagPriorList :: [ClauTagPrior] -> Bool
isPriorAmbiInClauTagPriorList [] = False
isPriorAmbiInClauTagPriorList clauTagPriorList = (lpCount * rpCount /= 0) || (rpCount * nothCount /= 0) || (lpCount * nothCount /= 0)
    where
      lpCount = countPriorInCTPList Lp clauTagPriorList
      rpCount = countPriorInCTPList Rp clauTagPriorList
      nothCount = countPriorInCTPList Noth clauTagPriorList

-- Extract Prior value from Maybe Prior value.
fromMaybePrior :: Maybe Prior -> Prior
fromMaybePrior (Just x) = x
fromMaybePrior Nothing = Noth                 -- Default value of Prior

{- Decide whether there is any sample for given ClauTag value in syntax ambiguity resolution samples database.
 - ClauTag :: (SentIdx, ClauIdx)
 -}
hasClauTagInSynAmbiResol :: ClauTag -> IO Bool
hasClauTagInSynAmbiResol clauTag = do
    confInfo <- readFile "Configuration"                                        -- Read the local configuration file
    let syntax_ambig_resol_model = getConfProperty "syntax_ambig_resol_model" confInfo          -- Syntax Ambiguity Resolution Model

    conn <- getConn
    let sqlstat = DS.fromString $ "select id, clauTagPrior from " ++ syntax_ambig_resol_model ++ " where clauTagPrior like '%" ++ show clauTag ++ "%'"
    stmt <- prepareStmt conn sqlstat
    (defs, is) <- queryStmt conn stmt []
    idAndCTPStrList <- readStreamByInt32UText [] is                             -- [(Int, ClauTagPriorStr)]
    close conn
    return $ elem True $ map (hasClauTagInCTPList clauTag) $ map stringToCTPList $ map snd idAndCTPStrList       -- [[ClauTagPrior]]

{- Prior value purity.
 - Purity is the ratio of occurrence number of Prior value with highest occurrence ratio to total occurrence number of all Prior values.
 - If multiple Prior values occur with same frequency, consider first value according to order of Lp, Rp, to Noth.
 -}
type Purity = Float

{- Find Prior value with highest occurrence ratio in all Prior value occorrences, and compute its purity.
 - The Prior value is called major Prior value.
 -}
purityOfMajorPrior :: [Prior] -> (Prior, Purity)
purityOfMajorPrior priors = (majorPrior, purity)
  where
    lpNum = length $ filter (== Lp) priors          -- Int
    rpNum = length $ filter (== Rp) priors          -- Int
    nothNum = length $ filter (== Noth) priors      -- Int
    maxFreq = maximum [lpNum, rpNum, nothNum]    -- Int
    majorPrior = case (elemIndex maxFreq [lpNum, rpNum, nothNum]) of
                   Just idx -> case idx of
                                 0 -> Lp
                                 1 -> Rp
                                 2 -> Noth
                   Nothing -> error "computePurityOfACluster: Impossible"
    purity = fromIntegral maxFreq / fromIntegral (length priors) :: Float

-- Sentence serial number range from which StruGene samples were generated.
getSentRangeByStruGeneSamples :: [StruGene2Sample] -> (SentIdx, SentIdx) -> (SentIdx, SentIdx)
getSentRangeByStruGeneSamples [] origRange = origRange
getSentRangeByStruGeneSamples (x:xs) origRange = getSentRangeByStruGeneSamples xs newRange
    where
    sentSnList = nub $ map (fst . fst) (svt7 x)
    minSentSn = minimum sentSnList
    maxSentSn = maximum sentSnList
    newRange = (minimum [fst origRange, minSentSn], maximum [snd origRange, maxSentSn])

-- Remove ClauTagPrior tuples from syntax ambiguity resolution samples database.
removeClauTagPriorFromSynAmbiResol :: SentIdx -> ClauIdx -> ClauIdx -> IO ()
removeClauTagPriorFromSynAmbiResol sn clauIdxOfStart clauIdxOfEnd = do
    let clauIdxRange = [clauIdxOfStart .. clauIdxOfEnd]
    if clauIdxRange == []
      then do
        rmNullCTPRecordsFromDB                                                  -- Remove records whose clauTagPrior is '[]'.
        putStrLn "removeClauTagPriorFromSynAmbiResol: Finished."
      else do
        putStrLn $ "removeClauTagPriorFromSynAmbiResol: Removing sample tags of clause " ++ show clauIdxOfStart ++ " begins ..."
        removeClauTagPriorFromSynAmbiResol' (sn, clauIdxOfStart)
        removeClauTagPriorFromSynAmbiResol sn (clauIdxOfStart + 1) clauIdxOfEnd

-- Remove one ClauTagPrior tuple from syntax ambiguity resolution samples database.
removeClauTagPriorFromSynAmbiResol' :: ClauTag -> IO ()
removeClauTagPriorFromSynAmbiResol' clauTag = do
    confInfo <- readFile "Configuration"                                        -- Read the local configuration file
    let syntax_ambig_resol_model = getConfProperty "syntax_ambig_resol_model" confInfo          -- Syntax Ambiguity Resolution Model

    conn <- getConn
    let sqlstat = DS.fromString $ "select id, clauTagPrior from " ++ syntax_ambig_resol_model ++ " where clauTagPrior like '%" ++ show clauTag ++ "%'"
    stmt <- prepareStmt conn sqlstat
    (defs, is) <- queryStmt conn stmt []
    idAndCTPStrList <- readStreamByInt32UText [] is                             -- [(Int, ClauTagPriorStr)]
    putStrLn $"removeClauTagPriorFromSynAmbiResol': idAndCTPStrList: " ++ show idAndCTPStrList

    let cTPList2 = map (removeFromCTPListByClauTag clauTag) $ map stringToCTPList $ map snd idAndCTPStrList       -- [[ClauTagPrior]]
    putStrLn $ "removeClauTagPriorFromSynAmbiResol': cTPList2: " ++ show cTPList2

    let lpHitCounts = map toMySQLInt16U $ map (countPriorInCTPList Lp) cTPList2
    let rpHitCounts = map toMySQLInt16U $ map (countPriorInCTPList Rp) cTPList2
    let nothHitCounts = map toMySQLInt16U $ map (countPriorInCTPList Noth) cTPList2

    let cTPList2' = map (toMySQLText . show) cTPList2                           -- [MySqlText]
    let ids = map toMySQLInt32U $ map fst idAndCTPStrList                       -- [MySQLInt32U]
    let rows = compFiveLists cTPList2' lpHitCounts rpHitCounts nothHitCounts ids     -- [[MySQLText,MySQLInt16U, MySQLInt16U,MySQLInt16U,MySqlInt32U]]
    putStrLn $ "removeClauTagPriorFromSynAmbiResol': rows: " ++ show rows
    let sqlstat' = DS.fromString $ "update " ++ syntax_ambig_resol_model ++ " set clauTagPrior = ?, lpHitCount = ?, rpHitCount = ?, nothHitCount = ? where id = ?"
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

-- Decide whether there is any ClauTagPrior value with given ClauTag value in a [ClauTagPrior] list.
hasClauTagInCTPList :: ClauTag -> [ClauTagPrior] -> Bool
hasClauTagInCTPList _ [] = False
hasClauTagInCTPList clauTag (x:xs)
    | clauTag == fst x = True
    | otherwise = hasClauTagInCTPList clauTag xs

-- Decide whether there is any dulicate ClauTag value in a [ClauTagPrior] list.
hasDupClauTagInCTPList :: [ClauTagPrior] -> Bool
hasDupClauTagInCTPList [] = False
hasDupClauTagInCTPList [ctp] = False
hasDupClauTagInCTPList ctps = length clauTags /= length (nub clauTags)
    where
    clauTags = map fst (nub ctps)                                               -- [(SentIdx, ClauIdx)]

{- Filter ClauTagPrior values with given ClauTag value in a [ClauTagPrior] list.
 - This function is unnecessary and actually can be replaced with Data.List.filter.
 - Also, it is unnecessary when every clauTagPrior field has only one ClauTagPrior tuple.
 -}
filterInCTPListByClauTag :: ClauTag -> [ClauTagPrior] -> [ClauTagPrior]
filterInCTPListByClauTag _ [] = []
filterInCTPListByClauTag clauTag (x:xs)
    | clauTag == fst x = x : filterInCTPListByClauTag clauTag xs
    | otherwise = filterInCTPListByClauTag clauTag xs

-- Decide whether there is any ClauTagPrior value with given SentIdx value in a [ClauTagPrior] list, where ClauTagPrior :: ((SentIdx, ClauIdx), Prior).
hasSentIdxInCTPList :: SentIdx -> [ClauTagPrior] -> Bool
hasSentIdxInCTPList _ [] = False
hasSentIdxInCTPList sentIdx (x:xs)
    | sentIdx == fst (fst x) = True
    | otherwise = hasSentIdxInCTPList sentIdx xs

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

-- Get ContextOfSG value from StruGene value.
getContextFromStruGene :: StruGene -> ContextOfSG
getContextFromStruGene struGene = (\x -> (fst6 x, snd6 x, thd6 x, fth6 x, fif6 x)) struGene

-- Sample model of Syntax-Structural Genes, which has attribute Sample Index, named as SIdx.
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

{- Get an instance of PhraSyn from a string, where PhraSyn :: (Category, Tag, PhraStru, Span).
 - Both Tag and PhraStru are aliases of type String.
 - Here, if exist, symbol '"' in two ends of a string literal are removed.
 -}
readPhraSynFromStr :: String -> PhraSyn
readPhraSynFromStr str = (ca, ta, ps, sp)
    where
    (caStr, taStr, psStr, spStr)  = stringToQuadruple str
    ca = getCateFromString caStr
    ta = case (head taStr == '"' && last taStr == '"') of
           True -> read taStr :: Tag             -- taStr is a string literal (enclosed in quotes, with proper escaping), such as "\">\"".
           False -> taStr
    ps = case (head psStr == '"' && last psStr == '"') of
           True -> read psStr :: PhraStru        -- psStr is a string literal (enclosed in quotes, with proper escaping), such as "\"AHn\"".
           False -> psStr
    sp = read spStr :: Span

{- Get an instance of PhraSyn0 from a string, where PhraSyn0 :: (Category, Tag, PhraStru).
 - Both Tag and PhraStru are aliases of type String.
 - Here, if exist, symbol '"' in two ends of a string literal are removed.
 -}
readPhraSyn0FromStr :: String -> PhraSyn0
readPhraSyn0FromStr str = (ca, ta, ps)
    where
    (caStr, taStr, psStr)  = stringToTriple str
    ca = getCateFromString caStr
    ta = case (head taStr == '"' && last taStr == '"') of
           True -> read taStr :: Tag             -- taStr is a string literal (enclosed in quotes, with proper escaping), such as "\">\"".
           False -> taStr
    ps = case (head psStr == '"' && last psStr == '"') of
           True -> read psStr :: PhraStru        -- psStr is a string literal (enclosed in quotes, with proper escaping), such as "\"AHn\"".
           False -> psStr

readPhraSyn0ListFromStr :: String -> [PhraSyn0]
readPhraSyn0ListFromStr "[]" = []
readPhraSyn0ListFromStr str = readPhraSyn0ListFromStrList (stringToList str)

readPhraSynListFromStr :: String -> [PhraSyn]
readPhraSynListFromStr "[]" = []
readPhraSynListFromStr str = readPhraSynListFromStrList (stringToList str)

readPhraSyn0ListFromStrList :: [String] -> [PhraSyn0]
readPhraSyn0ListFromStrList [] = []
readPhraSyn0ListFromStrList (s:ss) = readPhraSyn0FromStr s : (readPhraSyn0ListFromStrList ss)

readPhraSynListFromStrList :: [String] -> [PhraSyn]
readPhraSynListFromStrList [] = []
readPhraSynListFromStrList (s:ss) = readPhraSynFromStr s : (readPhraSynListFromStrList ss)

{- Get an instance of BiTree PhraSyn0 from a string, where BiTree PhraSyn0 :: Empty | Node PhraSyn0 (BiTree PhraSyn0) (BiTree PhraSyn0)
 -}
readBiTreePhraSyn0FromStr :: String -> BiTree PhraSyn0
readBiTreePhraSyn0FromStr "()" = Empty
readBiTreePhraSyn0FromStr str = Node (readPhraSyn0FromStr rootStr) (readBiTreePhraSyn0FromStr leftSubStr) (readBiTreePhraSyn0FromStr rightSubStr)
    where
    (rootStr, leftSubStr, rightSubStr) = stringToTriple str

{- Get an instance of BiTree PhraSyn from a string, where BiTree PhraSyn :: Empty | Node PhraSyn (BiTree PhraSyn) (BiTree PhraSyn)
 -}
readBiTreePhraSynFromStr :: String -> BiTree PhraSyn
readBiTreePhraSynFromStr "()" = Empty
readBiTreePhraSynFromStr str = Node (readPhraSynFromStr rootStr) (readBiTreePhraSynFromStr leftSubStr) (readBiTreePhraSynFromStr rightSubStr)
    where
    (rootStr, leftSubStr, rightSubStr) = stringToTriple str

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

{- ContextOfSG3a0 :: (LeftOverTree0, RightOverTree0)
 - LeftOverTree0, RightOverTree0 :: BiTree PhraSyn0
 -}
readContextOfSG3a0FromStr :: String -> ContextOfSG3a0
readContextOfSG3a0FromStr str = (lot, rot)
    where
    tupleStr = stringToTuple str
    lot = readBiTreePhraSyn0FromStr (fst tupleStr)
    rot = readBiTreePhraSyn0FromStr (snd tupleStr)

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
type DMin = Double                  -- Minimal distance between a sample and all cluster members
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

-- Get the string of a PhraSyn0
phraSyn0ToString :: PhraSyn0 -> String
phraSyn0ToString phs0 = "(" ++ ca ++ "," ++ ta ++ "," ++ ps ++ ")"
    where
      ca = show (fst3 phs0)
      ta = snd3 phs0
      ps = thd3 phs0

-- Get the string of a PhraSyn
phraSynToString :: PhraSyn -> String
phraSynToString phs = "(" ++ ca ++ "," ++ ta ++ "," ++ ps ++ "," ++ sp ++ ")"
    where
      ca = show (fst4 phs)
      ta = snd4 phs
      ps = thd4 phs
      sp = show (fth4 phs)

-- Get the string of [PhraSyn]
nPhraSynToString :: [PhraSyn] -> String
nPhraSynToString nps = listToString (map phraSynToString nps)


-- Get the string of [PhraSyn0]
nPhraSyn0ToString :: [PhraSyn0] -> String
nPhraSyn0ToString nps0 = listToString (map phraSyn0ToString nps0)

-- Get the string of a ContextOfSG sample
contextOfSGToString :: ContextOfSG -> String
contextOfSGToString csg = "(" ++ le ++ "," ++ lo ++ "," ++ re ++ "," ++ ro ++ "," ++ ot ++ ")"
    where
    le = nPhraSynToString (fst5 csg)
    lo = phraSynToString (snd5 csg)
    re = phraSynToString (thd5 csg)
    ro = nPhraSynToString (fth5 csg)
    ot = show (fif5 csg)

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

-- Get banned phrases by [OverPair].
getBanPCsByOverPairs :: [OverPair] -> BanPCs
getBanPCsByOverPairs ops = nub $ getBanPCsByOverPairs' ops                      -- Remove duplicates

-- Get banned phrases by [OverPair], in which there might be same phrases.
getBanPCsByOverPairs' :: [OverPair] -> BanPCs
getBanPCsByOverPairs' [] = []
getBanPCsByOverPairs' (op:ops)
    | thd3 op == Lp = snd3 op : getBanPCsByOverPairs' ops                       -- Drop the right
    | thd3 op == Rp = fst3 op : getBanPCsByOverPairs' ops                       -- Drop the left
    | thd3 op == Noth = fst3 op : ((snd3 op) : getBanPCsByOverPairs' ops)       -- Drop both sides


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

{- 2th generation model of Structural Genes, which allows multiple priority policies for one ambiguous context,
 - and marks clausal tag on every priority policy.
 -}
type StruGene2 = (LeftExtend, LeftOver, RightOver, RightExtend, OverType, [ClauTagPrior])
type StruGene2Sample = (SIdx, LeftExtend, LeftOver, RightOver, RightExtend, OverType, [ClauTagPrior])

-- Get ContextOfSG value from StruGene2 value.
getContextFromStruGene2 :: StruGene2 -> ContextOfSG
getContextFromStruGene2 struGene2 = (\x -> (fst6 x, snd6 x, thd6 x, fth6 x, fif6 x)) struGene2

-- Get ContextOfSG value from StruGene2Sample value.
getContextFromStruGene2Sample :: StruGene2Sample -> ContextOfSG
getContextFromStruGene2Sample struGene2Sample = (\x -> (snd7 x, thd7 x, fth7 x, fif7 x, sth7 x)) struGene2Sample

-- Convert a StruGene2 value to a StruGene value by doing priorWithHighestFreq opertion.
fromStruGene2ByHighestFreqPrior :: StruGene2 -> StruGene
fromStruGene2ByHighestFreqPrior (a,b,c,d,e,clauTagPrior) = (a,b,c,d,e, (fromMaybePrior . priorWithHighestFreq) clauTagPrior)

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

{- Read a value from input stream [MySQLValue], change it into a ContextOfSG3a02ClauTagPrior value, append it
 - to existed ContextOfSG3a02ClauTagPrior list, then read the next until read Nothing.
 - Here [MySQLValue] is [MySQLText, MySQLText, MySQLText],
 - and ContextOfSG3a02ClauTagPrior is ((LeftOverTree0, RightOverTree0), ClauTagPrior).
 -}
readStreamByContextOfSG3a02ClauTagPrior :: [ContextOfSG3a02ClauTagPrior] -> S.InputStream [MySQLValue] -> IO [ContextOfSG3a02ClauTagPrior]
readStreamByContextOfSG3a02ClauTagPrior es is = do
    S.read is >>= \x -> case x of                                        -- Dumb element 'case' is an array with type [MySQLValue]
        Just x -> readStreamByContextOfSG3a02ClauTagPrior (es ++ [((readBiTreePhraSyn0FromStr (fromMySQLText (x!!0)),
                                                                    readBiTreePhraSyn0FromStr (fromMySQLText (x!!1))
                                                                   ),
                                                                   stringToCTPList (fromMySQLText (x!!2)))
                                                                  ]) is
        Nothing -> return es

{- Read a value from input stream [MySQLValue], change it into a ContextOfSG3a2ClauTagPrior value, append it
 - to existed ContextOfSG3a2ClauTagPrior list, then read the next until read Nothing.
 - Here [MySQLValue] is [MySQLText, MySQLText, MySQLText],
 - and ContextOfSG3a2ClauTagPrior is ((LeftOverTree, RightOverTree), ClauTagPrior).
 -}
readStreamByContextOfSG3a2ClauTagPrior :: [ContextOfSG3a2ClauTagPrior] -> S.InputStream [MySQLValue] -> IO [ContextOfSG3a2ClauTagPrior]
readStreamByContextOfSG3a2ClauTagPrior es is = do
    S.read is >>= \x -> case x of                                        -- Dumb element 'case' is an array with type [MySQLValue]
        Just x -> readStreamByContextOfSG3a2ClauTagPrior (es ++ [((readBiTreePhraSynFromStr (fromMySQLText (x!!0)),
                                                               readBiTreePhraSynFromStr (fromMySQLText (x!!1))
                                                              ),
                                                              stringToCTPList (fromMySQLText (x!!2)))
                                                            ]) is
        Nothing -> return es

{- Read a value from input stream [MySQLValue], change it into a StruGene2Sample value, append it
 - to existed StruGene2Sample list, then read the next until read Nothing.
 - Here [MySQLValue] is [MySQLInt32U, MySQLText, MySQLText, MySQLText, MySQLText, MySQLInt8, MySQLText],
 - and StruGene2Sample is (SIdx, LeftExtend, LeftOver, RightOver, RightExtend, OverType, [ClauTagPrior]).
 -}
readStreamByStruGene2Sample :: [StruGene2Sample] -> S.InputStream [MySQLValue] -> IO [StruGene2Sample]
readStreamByStruGene2Sample es is = do
    S.read is >>= \x -> case x of                                        -- Dumb element 'case' is an array with type [MySQLValue]
        Just x -> readStreamByStruGene2Sample (es ++ [(fromMySQLInt32U (x!!0),
                                                       readPhraSynListFromStr (fromMySQLText (x!!1)),
                                                       readPhraSynFromStr (fromMySQLText (x!!2)),
                                                       readPhraSynFromStr (fromMySQLText (x!!3)),
                                                       readPhraSynListFromStr (fromMySQLText (x!!4)),
                                                       fromMySQLInt8 (x!!5),
                                                       stringToCTPList (fromMySQLText (x!!6)))
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

{- Read a value from input stream [MySQLValue], change it into a CSG2Sim value, append it
 - to existed CSG2Sim list, then read the next until read Nothing.
 - Here [MySQLValue] is [MySQLInt16U,MySQLInt16U, MySQLFloat], CSG2Sim :: ((SIdx,SIdx),SimDeg), SimDeg :: Float.
 - Functions like 'readStreamByXXX' are very ineffient because their Implementations are recursive.
 -}
readStreamByInt16UInt16UFloat :: [((SIdx, SIdx), Float)] -> S.InputStream [MySQLValue] -> IO [((SIdx,SIdx),Float)]
readStreamByInt16UInt16UFloat es is = do
    let num = length es
    if (num `mod` 1000 == 0)
      then putStr $ " " ++ show num
      else putStr ""
    S.read is >>= \x -> case x of                             -- Dumb element 'case' is an array with type [MySQLValue]
        Just x -> readStreamByInt16UInt16UFloat (es ++ [((fromMySQLInt16U (x!!0),fromMySQLInt16U (x!!1)), fromMySQLFloat (x!!2))]) is
        Nothing -> return es

{- Read a value from input stream [MySQLValue], change it into a CCC2Sim value, append it
 - to existed CCC2Sim list, then read the next until read Nothing.
 - Here [MySQLValue] is [MySQLInt16U,MySQLInt16U, MySQLDouble], CCC2Sim :: ((SIdx,SIdx),SimDeg), SimDeg :: Double.
 - Functions like 'readStreamByXXX' are very ineffient because their Implementations are recursive.
 -}
readStreamByInt16UInt16UDouble :: [((SIdx, SIdx), Double)] -> S.InputStream [MySQLValue] -> IO [((SIdx, SIdx), Double)]
readStreamByInt16UInt16UDouble es is = do
    let num = length es
    if (num `mod` 1000 == 0)
      then putStr $ " " ++ show num
      else putStr ""
    S.read is >>= \x -> case x of                             -- Dumb element 'case' is an array with type [MySQLValue]
        Just x -> readStreamByInt16UInt16UDouble (es ++ [((fromMySQLInt16U (x!!0),fromMySQLInt16U (x!!1)), fromMySQLDouble (x!!2))]) is
        Nothing -> return es

{- For every syntactic ambiguity resolution model, there might be multiple methods of using samples to resolve syntactic ambiguity.
 - For model StruGene, methods include "StruGeneSimple" and "StruGeneEmbedded".
 -}
type SynAmbiResolMethod = String

{- Remove StruGene2 records which have null clauTagPrior value from sample database.
 -}
rmNullCTPRecordsFromDB :: IO ()
rmNullCTPRecordsFromDB = do
    confInfo <- readFile "Configuration"
    let syntax_ambig_resol_model = getConfProperty "syntax_ambig_resol_model" confInfo

    conn <- getConn
    let sqlstat = DS.fromString $ "delete from " ++ syntax_ambig_resol_model ++ " where clauTagPrior = '[]'"
    stmt <- prepareStmt conn sqlstat
    ok <- executeStmt conn stmt []
    let rn = getOkAffectedRows ok
    putStrLn $ "rmNullCTPRecordsFromDB: " ++ show rn ++ " row(s) were deleted from " ++ syntax_ambig_resol_model ++ "."
    close conn

{- Model StruGene3 and Model StruGene3a.
 -}
type LeftOverTree = BiTree PhraSyn
type RightOverTree = BiTree PhraSyn
type StruGene3 = (LeftExtend, LeftOverTree, RightOverTree, RightExtend, OverType, [ClauTagPrior])
type StruGene3Sample = (SIdx, LeftExtend, LeftOverTree, RightOverTree, RightExtend, OverType, [ClauTagPrior])
type ContextOfOT3a = (LeftExtend, LeftOverTree, RightOverTree, RightExtend)
type StruGene3a = (LeftOverTree, RightOverTree, [ClauTagPrior])
type StruGene3aSample = (SIdx, LeftOverTree, RightOverTree, [ClauTagPrior])
type ContextOfSG3a = (LeftOverTree, RightOverTree)
type ContextOfSG3a2ClauTagPrior = (ContextOfSG3a, [ClauTagPrior])
type ContextOfSG3a2ClauTagPriorBase = [ContextOfSG3a2ClauTagPrior]
type ContextOfSG3a02ClauTagPrior = (ContextOfSG3a0, [ClauTagPrior])
type ContextOfSG3a02ClauTagPriorBase = [ContextOfSG3a02ClauTagPrior]
type LeftOverTree0 = BiTree PhraSyn0
type RightOverTree0 = BiTree PhraSyn0
type StruGene3a0 = (LeftOverTree0, RightOverTree0, [ClauTagPrior])
type StruGene3a0Sample = (SIdx, LeftOverTree0, RightOverTree0, [ClauTagPrior])
type ContextOfSG3a0 = (LeftOverTree0, RightOverTree0)

{- Convert a binary tree of phrasal categories to a binary tree of phrasal syntactic structure PhraSyn.
 - PhraSyn :: (Category, Tag, PhraStru, Span)
 - Suppose all phrasal categories are atomic, namely only one element in list [(Category,Tag,Seman,PhraStru,Act)].
 -}
phraCateTree2PhraSynTree :: BiTree PhraCate -> BiTree PhraSyn
phraCateTree2PhraSynTree pcTree
    | pcTree == Empty = Empty
    | otherwise = Node (((ctpsOfCate . getRoot) pcTree)!!0) (phraCateTree2PhraSynTree (getLeftSub pcTree)) (phraCateTree2PhraSynTree (getRightSub pcTree))

{- Convert a binary tree of phrasal categories to a binary tree of phrasal syntactic structure PhraSyn0.
 - PhraSyn0 :: (Category, Tag, PhraStru)
 - Suppose all phrasal categories are atomic, namely only one element in list [(Category,Tag,Seman,PhraStru,Act)].
 -}
phraCateTree2PhraSyn0Tree :: BiTree PhraCate -> BiTree PhraSyn0
phraCateTree2PhraSyn0Tree pcTree
    | pcTree == Empty = Empty
    | otherwise = Node (((ctpOfCate . getRoot) pcTree)!!0) (phraCateTree2PhraSyn0Tree (getLeftSub pcTree)) (phraCateTree2PhraSyn0Tree (getRightSub pcTree))

{- Get the corresponding PhraSyn0 value from a PhraCate value.
 - Suppose the PhraCate value is atomic, that is, in which the list is a singleton list.
 -}
phraCate2PhraSyn0 :: PhraCate -> PhraSyn0
phraCate2PhraSyn0 ((_, _), [(cate, tag, _, phraStru, _)], _) = (cate, tag, phraStru)
phraCate2PhraSyn0 ((_, _), list, _) = error $ "phraCate2PhraSyn0: In which the list has " ++ show (length list) ++ " elements."

{- Get the corresponding PhraSyn value from a PhraCate value.
 - Suppose the PhraCate value is atomic, that is, in which the list is a singleton list.
 -}
phraCate2PhraSyn :: PhraCate -> PhraSyn
phraCate2PhraSyn ((_, span), [(cate, tag, _, phraStru, _)], _) = (cate, tag, phraStru, span)
phraCate2PhraSyn ((_, _), list, _) = error $ "phraCate2PhraSyn: In which the list has " ++ show (length list) ++ " elements."

{- Get corresponding BiTree PhraSyn instance from BiTree PhraCate instance.
 - Suppose every PhraCate instance is atomic.
 -}
biTreePhraCate2BiTreePhraSyn :: BiTree PhraCate -> BiTree PhraSyn
biTreePhraCate2BiTreePhraSyn Empty = Empty
biTreePhraCate2BiTreePhraSyn (Node r lst rst) = Node (fst3 ctp, snd3 ctp, thd3 ctp, spOfCate r) (biTreePhraCate2BiTreePhraSyn lst) (biTreePhraCate2BiTreePhraSyn rst)
    where
    ctp = (ctpOfCate r)!!0

{- Get corresponding BiTree PhraSyn0 instance from BiTree PhraCate instance.
 - Suppose every PhraCate instance is atomic.
 -}
biTreePhraCate2BiTreePhraSyn0 :: BiTree PhraCate -> BiTree PhraSyn0
biTreePhraCate2BiTreePhraSyn0 Empty = Empty
biTreePhraCate2BiTreePhraSyn0 (Node r lst rst) = Node ((ctpOfCate r)!!0) (biTreePhraCate2BiTreePhraSyn0 lst) (biTreePhraCate2BiTreePhraSyn0 rst)

phraSyns2RulesToString :: ([PhraSyn],[Rule]) -> String
phraSyns2RulesToString (nPhraSyn, rules) = "(" ++ nPhraSynToString nPhraSyn ++ "," ++ show rules ++ ")"

phraSyn0s2RulesToString :: ([PhraSyn0],[Rule]) -> String
phraSyn0s2RulesToString (nPhraSyn0, rules) = "(" ++ nPhraSyn0ToString nPhraSyn0 ++ "," ++ show rules ++ ")"

biTreePhraSynToString :: BiTree PhraSyn -> String
biTreePhraSynToString Empty = "()"
biTreePhraSynToString (Node r lst rst) = "(" ++ phraSynToString r ++ "," ++ biTreePhraSynToString lst ++ "," ++ biTreePhraSynToString rst ++ ")"

biTreePhraSyn0ToString :: BiTree PhraSyn0 -> String
biTreePhraSyn0ToString Empty = "()"
biTreePhraSyn0ToString (Node r lst rst) = "(" ++ phraSyn0ToString r ++ "," ++ biTreePhraSyn0ToString lst ++ "," ++ biTreePhraSyn0ToString rst ++ ")"

biTreePhraSyn2RulesToString :: (BiTree PhraSyn,[Rule]) -> String
biTreePhraSyn2RulesToString (biTreePhraSyn, rules) = "(" ++ biTreePhraSynToString biTreePhraSyn ++ "," ++ show rules ++ ")"

biTreePhraSyn02RulesToString :: (BiTree PhraSyn0,[Rule]) -> String
biTreePhraSyn02RulesToString (biTreePhraSyn0, rules) = "(" ++ biTreePhraSyn0ToString biTreePhraSyn0 ++ "," ++ show rules ++ ")"

stringToPhraSyns2Rules :: String -> ([PhraSyn],[Rule])
stringToPhraSyns2Rules str = (map readPhraSynFromStr phrasyn_strs, map (\x -> read x :: Rule) rule_strs)
    where
    (phrasyns_str, rules_str) = stringToTuple str                                        -- (String, String)
    (phrasyn_strs, rule_strs) = (stringToList phrasyns_str, stringToList rules_str)      -- ([String], [String])

stringToPhraSyn0s2Rules :: String -> ([PhraSyn0],[Rule])
stringToPhraSyn0s2Rules str = (map readPhraSyn0FromStr phrasyn0_strs, map (\x -> read x :: Rule) rule_strs)
    where
    (phrasyn0s_str, rules_str) = stringToTuple str                                       -- (String, String)
    (phrasyn0_strs, rule_strs) = (stringToList phrasyn0s_str, stringToList rules_str)    -- ([String], [String])

stringToBiTreePhraSyn :: String -> BiTree PhraSyn
stringToBiTreePhraSyn "()" = Empty
stringToBiTreePhraSyn str = (Node (readPhraSynFromStr rStr) (stringToBiTreePhraSyn lstStr) (stringToBiTreePhraSyn rstStr))
    where
    (rStr, lstStr, rstStr) = stringToTriple str

stringToBiTreePhraSyn0 :: String -> BiTree PhraSyn0
stringToBiTreePhraSyn0 "()" = Empty
stringToBiTreePhraSyn0 str = (Node (readPhraSyn0FromStr rStr) (stringToBiTreePhraSyn0 lstStr) (stringToBiTreePhraSyn0 rstStr))
    where
    (rStr, lstStr, rstStr) = stringToTriple str

stringToBiTreePhraSyn2Rules :: String -> (BiTree PhraSyn, [Rule])
stringToBiTreePhraSyn2Rules str = (stringToBiTreePhraSyn biTreePhraSyn_str, map (\x -> read x :: Rule) (stringToList rules_str))
    where
    (biTreePhraSyn_str, rules_str) = stringToTuple str                          -- (String, String)

stringToBiTreePhraSyn02Rules :: String -> (BiTree PhraSyn0, [Rule])
stringToBiTreePhraSyn02Rules str = (stringToBiTreePhraSyn0 biTreePhraSyn0_str, map (\x -> read x :: Rule) (stringToList rules_str))
    where
    (biTreePhraSyn0_str, rules_str) = stringToTuple str                          -- (String, String)

-- Context Models of categorial conversions
type ContextOfCC1 = [PhraSyn]
type ContextOfCC2 = [PhraSyn0]
type ContextOfCC3 = BiTree PhraSyn
type ContextOfCC4 = BiTree PhraSyn0

-- Used for calculating similarity between category-conversion lists.
type SIdxRules = (SIdx, [Rule])

{- Calculate average similarity degree between one Rules sample and every one in a set of Rules.
 -}
meanRulesSimOnOneSample :: M.Map (SIdx,SIdx) Double -> [(SIdx,[Rule])] -> (SIdx,[Rule]) -> Double
meanRulesSimOnOneSample sIdxPair2SimMap srList sIdxRules = sim
 where
   sIdx = fst sIdxRules                -- SIdx
   rules = snd sIdxRules               -- [Rule]
   otherSamples = filter (\x -> fst x /= sIdx) srList                -- [(SIdx, [Rule])]
   sims = map (\x -> case (x <= sIdx) of
                       True -> M.lookup (x, sIdx) sIdxPair2SimMap
                       False -> M.lookup (sIdx, x) sIdxPair2SimMap
              ) (map fst otherSamples)                               -- [Maybe Double]
   sims' = map (\x -> case x of
                        Just x -> x
                        Nothing -> error "countInSLRBank (Func. 7): sIdxPair2SimMap exception"
               ) sims                                                -- [Double]
   highestSim = maximum sims'
   indices = map fst $ filter (\x -> snd x == highestSim) $ zip [0..] sims'    -- [Int]
   closestSamples = [otherSamples!!idx | idx <- indices]             -- [SIdx]
   sim = jaccardSimIndex' (snd sIdxRules) (map snd otherSamples)     -- Double
