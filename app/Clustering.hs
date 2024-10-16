{-# LANGUAGE OverloadedStrings, LambdaCase #-}
{-# LANGUAGE MultiWayIf #-}

-- Copyright (c) 2019-2024 China University of Water Resources and Electric Power
-- All rights reserved.
-- This module was written by Qian-qian WANG at 2023. The original goal was to cluster the samples of ambiguity resolution.
-- To complete clustering, the distances between phrases shoule be defined firstly. Unfortunately it is not a simple problem.
-- To evaluate similarity between two values of every grammatic attribute, mutual explanation was proposed by Qing-jiang WANG at 2024 autumn.

module Clustering (
    distPhraSyn,               -- PhraSyn -> PhraSyn -> Int
    distPhraSynSet,            -- [PhraSyn] -> [PhraSyn] -> Float
    distPhraSynSet',           -- [PhraSyn] -> [PhraSyn] -> Float
    distVect4StruGene,         -- StruGene -> StruGene -> [Float]
    DistWeiRatioList,          -- [Int], namely [wle, wlo, wro, wre, wot, wpr]
    dist4StruGeneByArithAdd,            -- StruGene -> StruGene -> DistWeiRatioList -> Float
    dist4StruGeneByNormArithMean,       -- StruGene -> StruGene -> Float
    StruGene,                           -- (LeftExtend, LeftOver, RightOver, RightExtend, OverType, Prior)
    getKModeByMaxMinPoint,              -- [StruGeneSample] -> [StruGeneSample] -> Map.Map SIdx Float -> Int -> [StruGene]
    Dist,                      -- Float
    SIdxDistTuple,             -- (SIdx, Dist)
    SIdxDistList,              -- [SIdxDistTuple]
    findDistMinValOfTwoTupleLists,       -- SIdxDistList -> SIdxDistList -> SIdxDistList -> SIdxDistList
    SampleClusterMark,                   -- (StruGeneSample,DMin,CIdx,StruGene,INo)
    findCluster4ASampleByArithAdd,       -- StruGene -> [StruGene] -> Int -> DistWeiRatioList -> SampleClusterMark
    findCluster4AllSamplesByArithAdd,    -- [StruGene] -> [StruGene] -> KVal -> INo -> [SampleClusterMark] -> DistWeiRatioList -> [SampleClusterMark]
--  findCluster4ASampleByNormArithMean,  -- StruGene -> [StruGene] -> Int -> SampleClusterMark
    divideSameCluster,                   -- [SampleClusterMark] -> Map.Map CIdx [StruGene] -> Map.Map CIdx [StruGene]
    updateCentre4ACluster,               -- [StruGene] -> StruGene
    getMode4List,                        -- Ord a => [a] -> Map.Map a Freq -> a
    getFreqMap,                          -- Ord a => [a] -> Map.Map a Freq -> Map.Map a Freq
    distTotalByClust,                    -- [StruGene] -> [StruGene] -> Float -> Float
    DistMean,                            -- Float
    SIdx,                                -- Int
    ClusterMap,                          -- Map CIdx [StruGene]
    findFinalCluster4AllSamplesByArithAdd,    -- TableName -> ClusterMap -> CentreList -> KVal -> INo -> DistTotal -> DistWeiRatioList -> IO (INo, Dist)
    readStreamByInt324TextInt8Text,           -- [StruGeneSample] -> S.InputStream [MySQLValue] -> IO [StruGeneSample]
    storeOneIterationResult,                  -- TableName -> INo -> [SampleClusterMark] -> [StruGene] -> DistMean -> IO ()
    storeClusterTime,                         -- TimeTableName -> KVal -> SNum -> NominalDiffTime -> NominalDiffTime -> Int -> Float -> IO ()
    autoRunClustByChangeKValSNum,             -- String -> String -> Int -> KValRange -> SNumRange -> DistWeiRatioList -> IO ()
    autoRunGetAmbiResolAccuracyOfAllClustRes, -- String -> String -> Int -> Int -> Int -> Int -> Int -> Int -> Int -> IO ()
    getRandomsList,                           -- Int -> Int -> Int -> [Int] -> IO [Int]
    getAmbiResolAccuracyOfAClustRes,          -- IO Float
    getAmbiResolSamples,                      -- IO [StruGeneSample]

    SentClauPhraList,        -- [[[PhraCate]]]
    getTypePairSim,          -- SentClauPhraList -> (NumOfPhraSyn, NumOfCate, NumOfCatePair, [((Category, Category), SimDeg)])
    getTagPairSim,           -- SentClauPhraList -> (NumOfPhraSyn, NumOfTag, NumOfTagPair, [((Tag, Tag), SimDeg)])
    getStruPairSim,          -- SentClauPhraList -> (NumOfPhraSyn, NumOfPhraStru, NumOfStruPair, [((PhraStru, PhraStru), SimDeg)])
    toTypeTagStru2FreqMap,   -- [[[PhraCate]]] -> Map String Int -> Map String Int
    stringToPhraSyn,         -- String -> PhraSyn
    getPhraSynPairSim,
    ) where

import Category
import Phrase
import AmbiResol
import Utils
import Output
import Data.Tuple.Utils
import Data.List
import Data.Tuple.Utils
import Data.Time.Clock
import Database
import Database.MySQL.Base
import System.IO
import qualified System.IO.Streams as S
import qualified Data.String as DS
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Set (Set)
import qualified Data.Set as Set
import System.Random
import Control.Monad
import Data.Massiv.Array hiding (map, zip, maximum, minimum, read)
import qualified Data.Massiv.Array as A
--import System.Random.Stateful

{- The distance between two phrases, where only grammatical factors are considered,
 - and the correlations between different values of an identical factor and between different factors are both neglected.
 - Actually, some categories such 'np' and 's\.np' may be converted for some syntactic requirements, and phrasal
 - structures such as 'AHn' and 'HnC' have same grammatical nature, which means there may exist correlations bewteen
 - the different values of a same factor. Meanwhile, phrasal structure 'AHn' must have category 'np', so there exist
 - correlations between some different factors.
 - Principal Components Analysis (PCA) and Factor Analysis (FA) are promising methods for solving this problem.
 -}
distPhraSyn :: PhraSyn -> PhraSyn -> Int
distPhraSyn (ca1, ta1, ps1) (ca2, ta2, ps2) = foldl (+) 0 [v1, v2, v3]
    where
    v1 = case ca1==ca2 of
           True -> 0
           False -> 1
    v2 = case ta1==ta2 of
           True -> 0
           False -> 1
    v3 = case ps1==ps2 of
           True -> 0
           False -> 1

{- The distance between two phrase sets.
 - Phrase set 1 = [p1, p2, ..., pn], phrase set 2 = [q1, q2, ..., qm]
 - dij = distPhraSyn pi qj
 - return the average dij among i<-[1..n] and j<-[1..m].
 - For model 'StruGene', LeftExtend and RightExtend are left-neighbouring and right-neighbouring phrases respectively.
 - When comparing the LeftExtend or RightExtend phrases between two StruGene samples, the following algorithm is used:
 - Suppose two sets to be compared are P and Q.
 - (1) P == Q, the distance is 0;
 - (2) P is proper set of Q, the distance is card(Q-P)/card(Q);
 - (3) Q is proper set of P, the distance is card(P-Q)/card(P);
 - (4) otherwise, the distance is sigma{distPhraSyn(r,t)| r<-P-Q, t<-Q-p}/(card(P-Q) x card(Q-P)).
 -}
distPhraSynSet :: [PhraSyn] -> [PhraSyn] -> Float
distPhraSynSet [] [] = 0.0
distPhraSynSet [] _ = 3.0
distPhraSynSet _ [] = 3.0
distPhraSynSet ps qs = fromIntegral distSum / fromIntegral distNum              -- fromIntegral :: Int -> Float
    where
    distSet = [distPhraSyn pi qj | pi <- ps, qj <- qs]
    distSum = foldl (+) 0 distSet
    distNum = length distSet

{- Nomalize the distance between two PhraSyn sets.
 - When two sets are same, the distance is 0.0.
 - when one set is null and another set is not, the distance is 3.0.
 - For other situations, the distance formulas should be studied again.
 -}
distPhraSynSet' :: [PhraSyn] -> [PhraSyn] -> Float
distPhraSynSet' [] [] = 0.0
distPhraSynSet' [] _ = 3.0       -- The second PhraSyn set is not null.
distPhraSynSet' _ [] = 3.0       -- The first PhraSyn set is not null.
distPhraSynSet' ps qs
    | ps' == [] && qs' == [] = 0.0
    | ps' /= [] && qs' /= [] = fromIntegral distSum / fromIntegral distNum
    | otherwise = 3.0 / fromIntegral distNum
    where
    sameElem = [x | x <- ps, elem x qs]
    ps' = [x | x <- ps, notElem x sameElem]
    qs' = [x | x <- qs, notElem x sameElem]
    distSet = [distPhraSyn pi qj | pi <- ps', qj <- qs']
    distSum = foldl (+) 0 distSet
    distNum = length ps * length qs

{- The distance vector between two samples of model StruGene.
 - For ambiguity model StruGene = (LeftExtend, LeftOver, RightOver, RightExtend, OverType, Prior), the distance vector is obtained by following function.
 - Normalize the first 4 distances to [0,1] in distance vector.
 -}
distVect4StruGene :: StruGene -> StruGene -> [Float]
distVect4StruGene s1 s2 = [d1, d2, d3, d4, d5, d6]
    where
    d1 = distPhraSynSet (fst6 s1) (fst6 s2) / 3.0
    d2 = fromIntegral (distPhraSyn (snd6 s1) (snd6 s2)) / 3.0
    d3 = fromIntegral (distPhraSyn (thd6 s1) (thd6 s2)) / 3.0
    d4 = distPhraSynSet (fth6 s1) (fth6 s2) / 3.0
    d5 = case fif6 s1 == fif6 s2 of
            True -> 0.0
            False -> 1.0
    d6 = case sth6 s1 == sth6 s2 of
            True -> 0.0
            False -> 1.0

-- Weigth ratio list of distances on StruGene elements between two StruGene samples.
type DistWeiRatioList = [Int]     -- [wle, wlo, wro, wre, wot, wpr]

{- Arithmetically added distance between two StruGenes.
 -}
dist4StruGeneByArithAdd :: StruGene -> StruGene -> DistWeiRatioList -> Float
-- dist4StruGeneByArithAdd s1 s2 distWeiRatioList = foldl (+) 0.0 distList / fromIntegral ratioTotal
dist4StruGeneByArithAdd s1 s2 distWeiRatioList = foldl (+) 0.0 distList
    where
    distList = map (\x -> fromIntegral (fst x) * snd x) $ zip distWeiRatioList $ distVect4StruGene s1 s2
    ratioTotal = foldl (+) 0 distWeiRatioList

{- Normalized arithmetically meaned distance between two StruGenes.
 -}
dist4StruGeneByNormArithMean :: StruGene -> StruGene -> Float
dist4StruGeneByNormArithMean s1 s2 = (\x-> x / 6.0) $ foldl (+) 0.0 (distVect4StruGene s1 s2)

{- 初始化随机选取1个点,初始定k=200

type StruGene' = ([LeftExtend], [LeftOver], [RightOver], [RightExtend], [OverType], [Prior])
type StruGene1 = StruGene'
--data sumClust = 200
-}

{- 计算每个样本到各簇心的距离的最小值mvList。
   样本：(x:xs)
   给定的簇心列表：sg

minValueList :: [StruGene] -> [StruGene] -> [Float] -> [Float]
minValueList [] _ mvList = mvList
minValueList _ [] mvList = mvList
minValueList (x:xs) sg mvList = minValueList xs sg mvList1
    where
    mi = minimum $ map (\x -> dist4StruGeneByArithAdd (fst x) (snd x) ) $ zip (replicate (length sg) x) sg
    mvList1 = mvList ++ [mi]

-- type initPointSet = ([StruGene],[StruGene],[Float])

  初始化选取k个众心点，用maxmin算法.
   找出最小值列表中最大的值，并记录序号；sgs中该序号的元素即为新找的中心点，
   从sgs删除该点，向ms添加该点。
   要求ms的初值是含有一个众心的列表。


getKModeByMaxMinPoint :: [StruGene] -> [StruGene] -> Int -> [StruGene]
getKModeByMaxMinPoint sgs ms kVal
   | length ms2 == kVal = ms2
   | otherwise = getKModeByMaxMinPoint sgs2 ms2 kVal
    where
    mvList = minValueList sgs ms []
    ma = maximum mvList                                      -- mvList is not empty.
    idx = elemIndex ma mvList
    idx' = case idx of
             Just x -> x
             Nothing -> -1                                    -- It is not impossible.
    ge = sgs!!idx'
    sgs2 = delete ge sgs
    ms2 =  ms ++ [ge]
-}

{- Find inital modes.
 - sgs: sample set.
 - oldModeList: original modes.
 - origSIdxMinValueMap: original map from samples to minimal distances.
 - kVal: the number of clusters.
 - distWeiRatioList: weigth list of distances on StruGene elements between two StruGene samples.
 -}
getKModeByMaxMinPoint :: [StruGeneSample] -> [StruGeneSample] -> Map.Map SIdx Float -> Int -> DistWeiRatioList -> [StruGene]
getKModeByMaxMinPoint sgs oldModeList origSIdxMinValueMap kVal distWeiRatioList
    | length oldModeList == kVal = map (\x -> (snd7 x, thd7 x, fth7 x, fif7 x, sth7 x, svt7 x)) oldModeList
    | otherwise = getKModeByMaxMinPoint sgs' newModeList newSIdxMinValueMap kVal distWeiRatioList
    where
    lastMode = last oldModeList
    lastModeSIdx = fst7 (lastMode)
    lastModeStruGene = (snd7 lastMode, thd7 lastMode, fth7 lastMode, fif7 lastMode, sth7 lastMode, svt7 lastMode)
    li = Map.toList $ Map.delete lastModeSIdx origSIdxMinValueMap
    distListOfSamplesToLastMode = map (\x -> (fst7 x, dist4StruGeneByArithAdd lastModeStruGene (snd7 x, thd7 x, fth7 x, fif7 x, sth7 x, svt7 x) distWeiRatioList)) sgs
    newSIdxMinValueList = findDistMinValOfTwoTupleLists distListOfSamplesToLastMode li []
    newSIdxMinValueMap = Map.fromList newSIdxMinValueList
    newModeSIdx = snd $ Map.findMax $ Map.fromList $ map (\x ->(snd x, fst x)) newSIdxMinValueList
    newMode = find (\x -> fst7 x == newModeSIdx) sgs
    newMode' = case newMode of
                 Just x -> x
                 Nothing -> nullStruGeneSample
    sgs' = delete newMode' sgs
    newModeList = oldModeList ++ [newMode']                                     -- Actually, newMode' is not Null.

type Dist = Float
type SIdxDistTuple = (SIdx, Dist)
type SIdxDistList = [SIdxDistTuple]

{- find the shorter distance of every sample
   sdv1 : sample distance vector
   sdv2 : sample distance vector
   sIdxMinValuList : sample distance vector storing the smaller distances
-}
findDistMinValOfTwoTupleLists :: SIdxDistList -> SIdxDistList -> SIdxDistList -> SIdxDistList
findDistMinValOfTwoTupleLists [] _ sIdxMinValuList = sIdxMinValuList
findDistMinValOfTwoTupleLists sdv1 [] sIdxMinValuList = sdv1
findDistMinValOfTwoTupleLists sdv1 sdv2 sIdxMinValuList = findDistMinValOfTwoTupleLists sdv1' sdv2 sIdxMinValuList'
    where
    sIdxSdv1FirstElem = fst (head sdv1)
    distSdv1FirstElem = snd (head sdv1)
    sdv1' = tail sdv1
    sdv2Dist4SameSIdx = lookup sIdxSdv1FirstElem sdv2
    sdv2Dist4SameSIdx' = case sdv2Dist4SameSIdx of
                    Just x -> x
                    Nothing -> 1000              -- Maximum distance from sample to mode
    minValue = min distSdv1FirstElem sdv2Dist4SameSIdx'
--    if distL1FirstElem > l2Dist4SameSIdx'
--       then let minValue = l2Dist4SameSIdx'
--       else let minValue = distL1FirstElem
    sIdxMinValuList' = sIdxMinValuList ++ [(sIdxSdv1FirstElem, minValue)]

type DMin = Float                                        -- Minimum distance of a sample to all cluster centres.
type CIdx = Int                                          -- Index of a cluster which a sample is attributed to.
type INo= Int                                            -- The order number of an iteration.
type SampleClusterMark = (StruGeneSample,DMin,CIdx,StruGene,INo)        -- The result of clustering for a sample.
type KVal = Int                                          -- The number of clusters.

{- 最小距离（算术加）聚类：计算一个点（包括聚类中心）和k个中心点的距离，聚类到最近的一簇,
 sp :一个样本(sample)
 ccl: 簇心列表(cluster centre list)
 kVal: 簇的个数
 distWeiRatioList: weigth list of distances on StruGene elements between two StruGene samples.
 elemIndex d cs：查找cs表中值为d的索引值
 -}
findCluster4ASampleByArithAdd :: StruGeneSample -> [StruGene] -> KVal -> INo -> DistWeiRatioList -> SampleClusterMark
findCluster4ASampleByArithAdd sp ccl kVal iNo distWeiRatioList = (sp, dMin, cIdx', ge, iNo)
    where
    sps = (snd7 sp, thd7 sp, fth7 sp, fif7 sp, sth7 sp, svt7 sp)
    ds = map (\x -> dist4StruGeneByArithAdd sps x distWeiRatioList) ccl
    dMin = minimum ds
    cIdx = elemIndex dMin ds
    cIdx' = case cIdx of
              Just x -> x
              Nothing -> -1        -- Impossible
    ge = ccl!!cIdx'

{- 最小距离（算术加）聚类：计算每个点（包括聚类中心）和k个中心点的距离，将其归入到距离它最近的那一簇,
 (x:xs)所有样本
 ccl: 簇心列表(cluster centre list)
 kVal: 簇的个数
 iNo:迭代序号
 distWeiRatioList: weigth list of distances on StruGene elements between two StruGene samples.
 -}
findCluster4AllSamplesByArithAdd :: [StruGeneSample] -> [StruGene] -> KVal -> INo -> [SampleClusterMark] -> DistWeiRatioList -> [SampleClusterMark]
findCluster4AllSamplesByArithAdd [] _ _ _ origClusterMark _ = origClusterMark
findCluster4AllSamplesByArithAdd (x:xs) ccl kVal iNo origClusterMark distWeiRatioList = findCluster4AllSamplesByArithAdd xs ccl kVal iNo newClusterMark distWeiRatioList
    where
    xCM = findCluster4ASampleByArithAdd x ccl kVal iNo distWeiRatioList
    newClusterMark = origClusterMark ++ [xCM]

{- 最小距离（标准平均距离）聚类：计算每个点（包括聚类中心）和k个中心点的距离，聚类到最近的一簇,k是一个列表

findCluster4ASampleByNormArithMean :: StruGeneSample -> [StruGene] -> Int -> SampleClusterMark
findCluster4ASampleByNormArithMean sp ccl kVal = (sp, dMin, cIdx', ge, 0)
    where
    ds = map (\x -> dist4StruGeneByNormArithMean (fst x) (snd x)) $ zip (replicate kVal sp) ccl
    dMin = minimum ds
    cIdx = elemIndex dMin ds
    cIdx' = case cIdx of
           Just x -> x
           Nothing -> -1        -- Impossible
    ge = ccl!!cIdx'
 -}

{- 按簇号CIdx把所有样本划分成簇
 SampleClusterMark : (StruGeneSample,DMin,CIdx,StruGene,INo)
-}
divideSameCluster :: [SampleClusterMark] -> Map.Map CIdx [StruGeneSample] -> Map.Map CIdx [StruGeneSample]
divideSameCluster [] origMap = origMap
divideSameCluster (x:xs) origMap = divideSameCluster xs newMap
    where
    cIdxSp = (thd5 x, fst5 x)
    newMap = Map.insertWith (++) (fst cIdxSp) [snd cIdxSp] origMap

-- type Stru_Gene1 = ([LeftExtend], [LeftOver], [RightOver], [RightExtend], [OverType], [Prior])

{- 更新每簇的中心点(即众心)
   (x:xs)表示该簇的所有样本（包括中心点）
   (lE,lO,rO,rE,oT,pr)开始时每个元素为空

-}
{- updateCentre4ACluster :: [StruGene] -> StruGene1 -> StruGene
updateCentre4ACluster [] _ = nullStruGene                     -- No sample in this cluster.
updateCentre4ACluster [x] _ = x                               -- Only one sample in this cluster.
updateCentre4ACluster (x:xs) (lE,lO,rO,rE,oT,pr)              -- There are more than two samples.
    | length xs > 1 = updateCentre4ACluster xs (lE1,lO1,rO1,rE1,oT1,pr1)
    | otherwise = (leMode, loMode, roMode, reMode, otMode, prMode)
    where
    hxs = head xs
    lE1 = (fst6 x) : lE
    lO1 = (snd6 x) : lO
    rO1 = (thd6 x) : rO
    rE1 = (fth6 x) : rE
    oT1 = (fif6 x) : oT
    pr1 = (sth6 x) : pr
    leSetList = fromListList2SetList ((fst6 hxs):lE1)
    leMode = Set.toDescList $ getMode4PhraSynSetList leSetList (getFreqMap leSetList Map.empty)
    loList = ((snd6 hxs):lO1)
    loMode = getMode4PhraSynList loList (getFreqMap loList Map.empty)
    roList = ((thd6 hxs):rO1)
    roMode = getMode4PhraSynList roList (getFreqMap roList Map.empty)
    reSetList = fromListList2SetList ((fth6 hxs):rE1)
    reMode = Set.toDescList $ getMode4PhraSynSetList reSetList (getFreqMap reSetList Map.empty)
    otList = ((fif6 hxs):oT1)
    otMode = getMode4OTList otList (getFreqMap otList Map.empty)
    prList = ((sth6 hxs):pr1)
    prMode = getMode4List prList (getFreqMap prList Map.empty)
-}

updateCentre4ACluster :: [StruGene] -> StruGene
updateCentre4ACluster [] = nullStruGene                     -- No sample in this cluster.
updateCentre4ACluster [x] = x                               -- Only one sample in this cluster.
updateCentre4ACluster sgs = (leMode, loMode, roMode, reMode, otMode, prMode)
    where
    lEList = fromListList2SetList (map fst6 sgs)
    lOList = map snd6 sgs
    rOList = map thd6 sgs
    rEList = fromListList2SetList (map fth6 sgs)
    oTList = map fif6 sgs
    prList = map sth6 sgs
    leMode = Set.toDescList $ getMode4PhraSynSetList lEList (getFreqMap lEList Map.empty)
    loMode = getMode4PhraSynList lOList (getFreqMap lOList Map.empty)
    roMode = getMode4PhraSynList rOList (getFreqMap rOList Map.empty)
    reMode = Set.toDescList $ getMode4PhraSynSetList rEList (getFreqMap rEList Map.empty)
    otMode = getMode4OTList oTList (getFreqMap oTList Map.empty)
    prMode = getMode4List prList (getFreqMap prList Map.empty)

type AClusterSamples = [StruGeneSample]
type CentreList = [StruGene]

{- 更新所有簇的中心点(即众心)
   (x:xs)表示所有簇的样本，x表示一个簇的所有样本
   (lE,lO,rO,rE,oT,pr)开始时每个元素为空
-}
updateCentre4AllCluster :: [AClusterSamples] -> CentreList -> CentreList
updateCentre4AllCluster [] origCentreList = origCentreList
updateCentre4AllCluster (x:xs) origCentreList = updateCentre4AllCluster xs newCentreList
    where
    xSg = map (\y -> (snd7 y, thd7 y, fth7 y, fif7 y, sth7 y, svt7 y)) x
    xCentre = updateCentre4ACluster xSg
    newCentreList = origCentreList ++ [xCentre]

{- FreOfPhraSyn: PhraSyn三元组的频次的集合
-}
type Freq = Int      -- 频次
type PhraSynSet = Set.Set PhraSyn
type LeftExtend = PhraSynSet
type RightExtend = PhraSynSet
type FreqOfPhraSynSetMap = Map.Map (Set.Set PhraSyn) Freq
type FreqOfPhraSynMap = Map.Map PhraSyn Freq
type FreqOfOTMap = Map.Map OverType Freq

{-计算三元组集合的列表的众数
-}
getMode4PhraSynSetList :: [PhraSynSet] -> FreqOfPhraSynSetMap -> PhraSynSet
getMode4PhraSynSetList [] _ = Set.empty
getMode4PhraSynSetList [x] _ = x
getMode4PhraSynSetList phraSynSetList freqOfPhraSynSetMap = mode
    where
    freqOfPhraSynSetList = Map.toList freqOfPhraSynSetMap
    freqList = map snd freqOfPhraSynSetList
    maxFreq = maximum freqList
    idx = elemIndex maxFreq freqList
    idx' = maybe (-1) (+0) idx
    mode = fst (freqOfPhraSynSetList!!idx')

{- The following function converts a list of lists to a list of sets,
 - especially used to convert a value with type [[PhraSyn]] to a value with type [PhraSynSet].
 -}
fromListList2SetList :: Ord a => [[a]] -> [Set.Set a]
fromListList2SetList [] = []
fromListList2SetList (x:xs) = (Set.fromList x):(fromListList2SetList xs)

{-求PhraSyn三元组列表的一个众心（可能有多个）.
  PhraSynList: PhraSyn列表
  freqOfPhraSynMap: 短语句法三元组到频次的映射字典.
-}
getMode4PhraSynList :: [PhraSyn] -> FreqOfPhraSynMap -> PhraSyn
getMode4PhraSynList [] _ = nullPhraSyn
getMode4PhraSynList [x] _ = x
getMode4PhraSynList phraSynList freqOfPhraSynMap = mode
    where
    freqOfPhraSynList = Map.toList freqOfPhraSynMap
    freqList = map snd freqOfPhraSynList
    maxFreq = maximum freqList
    idx = elemIndex maxFreq freqList
    idx' = maybe (-1) (+0) idx               -- For idx == Just i, return i; For idx == Nothing, return -1.
    mode = fst (freqOfPhraSynList!!idx')

{-求列表的一个众数（可能有多个）.
  list: 列表[k]
  kFreqMap: 频次字典
-}
getMode4List :: Ord a => [a] -> Map.Map a Freq -> a
getMode4List [] _ = error "getMode4List: List is empty."
getMode4List [x] _ = x
getMode4List list kFreqMap = mode
    where
    kFreqList = Map.toList kFreqMap
    freqList = map snd kFreqList
    maxFreq = maximum freqList
    idx = elemIndex maxFreq freqList
    idx' = maybe (-1) (+0) idx
    mode = fst (kFreqList!!idx')

{- 求PhraSyn三元组的频次
 - (x:xs): PhraSyn列表
 - origFreqOfPhraSyn: 原PhraSyn的频次“字典”
 -}
getPhraSynFreqMap :: [PhraSyn] -> Map.Map PhraSyn Freq -> Map.Map PhraSyn Freq
getPhraSynFreqMap [] origFreqOfPhraSyn = origFreqOfPhraSyn
getPhraSynFreqMap (x:xs) origFreqOfPhraSyn = getPhraSynFreqMap xs newFreqOfPhraSyn
    where
    newFreqOfPhraSyn = Map.insertWith (+) x 1 origFreqOfPhraSyn

{- 求类型K的值出现的频次，即建立一个Map K V.
 - (x:xs): a值的列表
 - origMap: 原Map a Freq
 -}
getFreqMap :: Ord a => [a] -> Map.Map a Freq -> Map.Map a Freq
getFreqMap [] origMap = origMap
getFreqMap (x:xs) origMap = getFreqMap xs newMap
    where
    newMap = Map.insertWith (+) x 1 origMap


{- 删除列表中的所有x

deleteAllxInList :: PhraSyn -> [PhraSyn] -> [PhraSyn]
deleteAllxInList x [] finList = finList
deleteAllxInList x [] finList
deleteAllxInList x list finList = deleteAllxInList x list' finList
    where
    list' = tail list
    finList = case x==(head list) of
          True -> finList
          False -> finList ++ [list!!0]
-}

getMode4OTList :: [OverType] ->  FreqOfOTMap -> OverType
getMode4OTList [] _ = -1                                -- No mode
getMode4OTList [x] _ = x
getMode4OTList oTList freqOfOTMap = mode
    where
    freqOfOTList = Map.toList freqOfOTMap
    freqList = map snd freqOfOTList
    maxFreq = maximum freqList
    idx = elemIndex maxFreq freqList
    idx' = maybe (-1) (+0) idx
    mode = fst (freqOfOTList!!idx')

{- 求OverType的频次
 - (x:xs): OverType列表
 - origFreqOfOT: 原OverType到频次的映射“字典”
 -}
getOverTypeFreqMap :: [OverType] -> Map.Map OverType Freq -> Map.Map OverType Freq
getOverTypeFreqMap [] origFreqOfOT = origFreqOfOT
getOverTypeFreqMap (x:xs) origFreqOfOT = getOverTypeFreqMap xs newFreqOfOT
    where
    newFreqOfOT = Map.insertWith (+) x 1 origFreqOfOT

{-
getxCountInList :: Int -> [Int] -> Int
getxCountInList x [] _ = 0
getxCountInList x [] count = count
getxCountInList x list count = getxCountInList x list1 count1
    where
    count1 = case x==(head list) of
           True -> count + 1
           False -> count
    list1 = tail list
-}

{- 计算总距离(各个簇中样本到其所在簇的众心的距离之和)
   (x:xs): 表示除众心点外的其他样本点
 -}
distTotalByClust :: [SampleClusterMark] -> Float -> Float
distTotalByClust [] origDist = origDist
distTotalByClust (x:xs) origDist = distTotalByClust xs newDist
    where
    newDist = origDist + (snd5 x)

type DistTotal = Float                                   -- Sum of distances between all samples and their cluster centres.
type DistMean = Float
type ClusterMap = Map.Map CIdx [StruGeneSample]          -- Partition of sample set after one time of clustering iteration.

{- 发现所有样本的聚类结果
 -}
findFinalCluster4AllSamplesByArithAdd :: TableName -> ClusterMap -> CentreList -> KVal -> INo -> DistTotal -> DistWeiRatioList -> IO (INo, Dist)
findFinalCluster4AllSamplesByArithAdd tblName clusterMap origCentreList kVal iNo origDistTotal distWeiRatioList = do
    let newCentreList = updateCentre4AllCluster (Map.elems clusterMap) []
    let struGeneSampleList = Map.foldr (++) [] clusterMap                             -- recover original samples.
    let scml = findCluster4AllSamplesByArithAdd struGeneSampleList newCentreList kVal iNo [] distWeiRatioList
    let newClusterMap = divideSameCluster scml Map.empty
    let newDistTotal = distTotalByClust scml 0.0
    let distMean = newDistTotal / fromIntegral (length scml)
    storeOneIterationResult tblName iNo scml newCentreList distMean
    putStrLn $ "findFinalCluster4AllSamplesByArithAdd: iNo = " ++ show iNo ++ ", origDistTotal = " ++ show origDistTotal ++ ", newDistTotal = " ++ show newDistTotal
    if newDistTotal < origDistTotal
--    if iNo < 20
      then findFinalCluster4AllSamplesByArithAdd tblName newClusterMap newCentreList kVal (iNo + 1) newDistTotal distWeiRatioList
--      else return scml
      else do
        let finalDistMean = origDistTotal / fromIntegral (length scml)
        return (iNo, finalDistMean)

{- 发现所有样本的聚类结果
 -}
findFinalCluster4AllSamplesWithoutPri :: TableName -> ClusterMap -> CentreList -> KVal -> INo -> DistTotal -> DistWeiRatioList -> IO (INo, Dist)
findFinalCluster4AllSamplesWithoutPri tblName clusterMap origCentreList kVal iNo origDistTotal distWeiRatioList = do
    let newCentreList = updateCentre4AllCluster (Map.elems clusterMap) []
    let struGeneSampleList = Map.foldr (++) [] clusterMap                             -- recover original samples.
    let scml = findCluster4AllSamplesByArithAdd struGeneSampleList newCentreList kVal iNo [] distWeiRatioList
    let newClusterMap = divideSameCluster scml Map.empty
    let newDistTotal = distTotalByClust scml 0.0
    let distMean = newDistTotal / fromIntegral (length scml)
    storeOneIterationResult tblName iNo scml newCentreList distMean
    putStrLn $ "findFinalCluster4AllSamplesByArithAdd: iNo = " ++ show iNo ++ ", origDistTotal = " ++ show origDistTotal ++ ", newDistTotal = " ++ show newDistTotal
    if newDistTotal < origDistTotal
--    if iNo < 20
      then findFinalCluster4AllSamplesByArithAdd tblName newClusterMap newCentreList kVal (iNo + 1) newDistTotal distWeiRatioList
--      else return scml
      else do
        let finalDistMean = origDistTotal / fromIntegral (length scml)
        return (iNo, finalDistMean)

--对上面的距离和迭代次数画图表示，也可以用手肘法找到合适的迭代次数，到此聚类结束

{- Read a value from input stream [MySQLValue], change it into a StruGeneSample value, append it
 - to existed StruGeneSample list, then read the next until read Nothing.
 - Here [MySQLValue] is [MySQLText, MySQLText, MySQLText, MySQLText, MySQLInt8, MySQLText].
 -}
readStreamByInt324TextInt8Text :: [StruGeneSample] -> S.InputStream [MySQLValue] -> IO [StruGeneSample]
readStreamByInt324TextInt8Text es is = do
    S.read is >>= \case                                         -- Dumb element 'case' is an array with type [MySQLValue]
        Just x -> readStreamByInt324TextInt8Text (es ++ [(fromMySQLInt32 (x!!0),
                                                    readPhraSynListFromStr (fromMySQLText (x!!1)),
                                                    readPhraSynFromStr (fromMySQLText (x!!2)),
                                                    readPhraSynFromStr (fromMySQLText (x!!3)),
                                                    readPhraSynListFromStr (fromMySQLText (x!!4)),
                                                    fromMySQLInt8 (x!!5),
                                                    readPriorFromStr (fromMySQLText (x!!6)))]) is
        Nothing -> return es

type TableName = String
{- Store clustering result of every times of iteration.
 - SampleClusterMark :: (StruGeneSample,DMin,CIdx,StruGene,INo)
 - StruGene :: (LeftExtend, LeftOver, RightOver, RightExtend, OverType, Prior)
 - Actually, the modes in clustering have type 'StruGene'.
 -}
storeOneIterationResult :: TableName -> INo -> [SampleClusterMark] -> [StruGene] -> DistMean -> IO ()
storeOneIterationResult tblName iNo scml kCentres distMean = do
    let cIdxPriorsTuple = map (\x -> (thd5 x, [(svt7 . fst5) x])) scml
    let priorsOfSameCIdx = Map.toList $ getPriorsOfSameClusterMap cIdxPriorsTuple Map.empty
--    putStrLn $ "Priors which are from same cluster will be expressed in the following form: " ++ show priorsOfSameCIdx
    let scd = map (\x -> ((fst7 . fst5) x, thd5 x, snd5 x)) scml                 -- [(SIdx, CIdx, DMin)]
    let scdStr = nScdToString scd                                        -- Get the string of [(SIdx, CIdx, DMin)]
    let modesStr = nStruGeneToString kCentres                            -- Get the string of [StruGene]

    conn <- getConn
    let sqlstat = DS.fromString $ "insert into " ++ tblName ++ " set iNo = ?, sampleClustInfo = ?, modes = ?, distMean = ?"
--  let sqlstat = DS.fromString $ "insert into " ++ tblName ++ " values (iNo, scdStr, modesStr, distMean)"
    stmt <- prepareStmt conn sqlstat
    executeStmt conn stmt [toMySQLInt8 iNo, toMySQLText scdStr, toMySQLText modesStr, toMySQLFloat distMean]
    close conn

type CIdxPriorsTuple = (SIdx, [Prior])

{- 求显示同一聚类里的prior
 - (x:xs): PhraSyn列表
 - origFreqOfPhraSyn: 原PhraSyn的频次“字典”
 -}
getPriorsOfSameClusterMap :: [CIdxPriorsTuple] -> Map.Map CIdx [Prior] -> Map.Map CIdx [Prior]
getPriorsOfSameClusterMap [] origPriorOfSameCluster = origPriorOfSameCluster
getPriorsOfSameClusterMap (x:xs) origPriorOfSameCluster = getPriorsOfSameClusterMap xs newPriorOfSameCluster
    where
    newPriorOfSameCluster = Map.insertWith (++) (fst x) (snd x) origPriorOfSameCluster

type TimeTableName = String
type SNum = Int
storeClusterTime :: TimeTableName -> KVal -> SNum -> NominalDiffTime -> NominalDiffTime -> Int -> Float -> IO ()
storeClusterTime timeTblName kVal sNum totalRunTime iterMeanTime its finalDistMean = do
    let tRTStr = init (show totalRunTime)
    let iMTStr = init (show iterMeanTime)
    let tRTFloat = read tRTStr :: Float
    let iMTFloat = read iMTStr :: Float
    let its' = its + 1
    putStrLn $ "storeClusterTime: tRTFloat = " ++ show tRTFloat ++ ", iMTFloat = " ++ show iMTFloat ++ ", iNo = " ++ show its' ++ ", kVal = " ++ show kVal ++ ", sNum = " ++ show sNum ++ ", finalDistMean = " ++ show finalDistMean

    conn <- getConn
    let sqlstat = DS.fromString $ "select kVal, sNum from " ++ timeTblName ++ " where kVal = ? and sNum = ?"
    stmt <- prepareStmt conn sqlstat
    (defs, is) <- queryStmt conn stmt [toMySQLInt32 kVal, toMySQLInt32 sNum]
    row <- S.read is                                   -- Maybe [MySQLInt32, MySQLInt32]
    let row' = case row of
                 Just x -> [fromMySQLInt32 (x!!0), fromMySQLInt32 (x!!1)]     -- [MySQLInt32, MySQLInt32]
                 Nothing -> [0, 0]
    S.skipToEof is                                     -- Skip to end-of-stream.
    if row' /= [0, 0]
      then do
        let sqlstat = DS.fromString $ "update " ++ timeTblName ++ " set totalTime = ?, iterMeanTime = ?, iNo = ?, finalDistMean = ? where kVal = ? and sNum = ?"
        stmt <- prepareStmt conn sqlstat
        ok <- executeStmt conn stmt [toMySQLFloat tRTFloat, toMySQLFloat iMTFloat, toMySQLInt8 its', toMySQLFloat finalDistMean, toMySQLInt32 kVal, toMySQLInt32 sNum]
        putStrLn $ "storeClusterTime: Number of rows update affected = " ++ show (getOkAffectedRows ok)
        close conn
      else do
        let sqlstat = DS.fromString $ "insert into " ++ timeTblName ++ " set kVal = ?, sNum = ?, totalTime = ?, iterMeanTime = ?, iNo = ?, finalDistMean = ?"
        stmt <- prepareStmt conn sqlstat
        ok <- executeStmt conn stmt [toMySQLInt32 kVal, toMySQLInt32 sNum, toMySQLFloat tRTFloat, toMySQLFloat iMTFloat, toMySQLInt8 its', toMySQLFloat finalDistMean]
        putStrLn $ "storeClusterTime: Number of rows insert affected = " ++ show (getOkAffectedRows ok)
        close conn

type BottomKVal = Int
type DeltaKVal = Int
type TopKVal = Int
type KValRange = (BottomKVal, DeltaKVal, TopKVal)
type BottomSNum = Int
type DeltaSNum = Int
type TopSNum = Int
type SNumRange = (BottomSNum, DeltaSNum, TopSNum)

autoRunClustByChangeKValSNum :: String -> String -> Int -> KValRange -> SNumRange -> DistWeiRatioList -> IO ()
autoRunClustByChangeKValSNum arm df kVal kValRange sNumRange distWeiRatioList = do
    let sNum = fst3 sNumRange
    putStrLn $ "The clustering of kVal = " ++ show kVal ++ ", sNum = " ++ show sNum ++ " begins."
    conn <- getConn
    confInfo <- readFile "Configuration"                                        -- Read the local configuration file
    let ambi_resol_model = getConfProperty "ambi_resol_model" confInfo

    if | arm == "Nothing" -> putStrLn "autoRunClustByChangeKValSNum: 'ambi_resol_model' is not correct."
       | df == "Nothing" -> putStrLn "autoRunClustByChangeKValSNum: 'distDef' is not correct."
       | otherwise -> do
--           t1 <- getCurrentTime
{-初始聚类随机选点
           let sqlstat = DS.fromString $ "select id, leftExtend, leftOver, rightOver, rightExtend, overType, prior from " ++ ambi_resol_model ++ " where id <= ? "
           stmt <- prepareStmt conn sqlstat
           (defs, is) <- queryStmt conn stmt [toMySQLInt32 sNum]
           struGeneSampleList <- readStreamByInt324TextInt8Text [] is
           let sIdxStruGeneMap = Map.fromList $ map (\x -> (fst7 x, (snd7 x, thd7 x, fth7 x, fif7 x, sth7 x, svt7 x))) struGeneSampleList
           randomSIdsList <- getRandomsList kVal 1 sNum []
           let randomInitalModes = getStruGenebySIdsList randomSIdsList sIdxStruGeneMap []
-}
           let clustResTbl = "clust_res_k" ++ show kVal ++ "_" ++ "s" ++ show sNum ++ "_" ++ arm ++ "_" ++ df ++ "_w" ++ (foldl (++) "" (map show (init distWeiRatioList)))
        --   let timeTbl = "clust_time_" ++ arm ++ "_" ++ df ++ "_w" ++ (foldl (++) "" (map show (init distWeiRatioList)))
           let sqlstat = DS.fromString $ "drop table if exists " ++ clustResTbl
           stmt <- prepareStmt conn sqlstat
           executeStmt conn stmt []

           let sqlstat = DS.fromString $ "create table " ++ clustResTbl ++ " (iNo tinyint primary key, sampleClustInfo mediumtext, modes mediumtext, distMean float)"
           stmt <- prepareStmt conn sqlstat
           executeStmt conn stmt []                          -- Create a new MySQL table for storing clustering result.

           let sqlstat = DS.fromString $ "select id, leftExtend, leftOver, rightOver, rightExtend, overType, prior from " ++ ambi_resol_model ++ " where id >= ? and id <= ? "
           stmt <- prepareStmt conn sqlstat
           (defs, is) <- queryStmt conn stmt [toMySQLInt32 1, toMySQLInt32 sNum]

           struGeneSampleList <- readStreamByInt324TextInt8Text [] is
           let m1 = head struGeneSampleList
           let sps = tail struGeneSampleList
           let initialKPoints = getKModeByMaxMinPoint sps [m1] Map.empty kVal distWeiRatioList
           let scml = findCluster4AllSamplesByArithAdd struGeneSampleList initialKPoints kVal 0 [] distWeiRatioList
--           let scml = findCluster4AllSamplesByArithAdd struGeneSampleList randomInitalModes kVal 0 [] distWeiRatioList
           let clusterMap = divideSameCluster scml Map.empty
           let origDistTotal = distTotalByClust scml 0.0
           let distMean = origDistTotal / fromIntegral sNum
           storeOneIterationResult clustResTbl 0 scml initialKPoints distMean       -- Store the first clustering result.
--           ti1 <- getCurrentTime                                                -- Start time of iteration
           finalINoDistMean <- findFinalCluster4AllSamplesByArithAdd clustResTbl clusterMap initialKPoints kVal 1 origDistTotal distWeiRatioList
--           ti2 <- getCurrentTime                                                -- End time of iteration
--         forM_ scml' $ \scm -> putStrLn $ "autoRunClustByChangeKValSNum: " ++ show scm
           closeStmt conn stmt

           putStrLn $ "autoRunClustByChangeKValSNum: " ++ " final iNo and distMean are " ++  show finalINoDistMean
{-            t2 <- getCurrentTime
--           let totalRunTime = diffUTCTime t2 t1
--           let its = fif5 (scml'!!0)                                            -- times of iteration.
           let its = fst finalINoDistMean                                       -- times of iteration.
           let finalDistMean = snd finalINoDistMean
          let iterMeanTime = (diffUTCTime ti2 ti1) / fromIntegral its
           putStrLn $ "autoRunClustByChangeKValSNum: totalRunTime = " ++ show totalRunTime ++ ", iterMeanTime = " ++ show iterMeanTime ++ ", times of iterations = " ++ show (its + 1)

           let sqlstat = DS.fromString $ "create table if not exists " ++ timeTbl ++ " (kVal int, sNum int, totalTime float, iterMeanTime float, iNo tinyint, finalDistMean float, primary key (kVal, sNum))"
           stmt <- prepareStmt conn sqlstat
           ok <- executeStmt conn stmt []                          -- Create a new MySQL table for storing clustering time.
           putStrLn $ "autoRunClustByChangeKValSNum: okStatus = " ++ show (getOkStatus ok)
           close conn
           storeClusterTime timeTbl kVal sNum totalRunTime iterMeanTime its finalDistMean
           putStrLn $ "The clustering of kVal = " ++ show kVal ++ ", sNum = " ++ show sNum ++ " has finished."

           let struGeneSampleOfLpList = getStruGeneSampleOfLp struGeneSampleList []
           clusterToStruGeneSampleListFromDB struGeneSampleOfLpList clustResTbl arm df kVal kValRange sNumRange distWeiRatioList
           let struGeneSampleOfRpList = [x | x <- struGeneSampleList, notElem x struGeneSampleOfLpList]
           clusterToStruGeneSampleListFromDB struGeneSampleOfRpList clustResTbl arm df kVal kValRange sNumRange distWeiRatioList
-}
           let kVal' = kVal + snd3 kValRange
           let (kVal'', sNum') = case kVal' > thd3 kValRange of
                                   True -> (fst3 kValRange, sNum + snd3 sNumRange)
                                   False -> (kVal', sNum)
           if (sNum' > thd3 sNumRange || snd3 kValRange == 0)
             then putStrLn "autoRunClustByChangeKValSNum: End."
             else autoRunClustByChangeKValSNum arm df kVal'' kValRange (sNum', snd3 sNumRange, thd3 sNumRange) distWeiRatioList

getStruGeneSampleOfLp :: [StruGeneSample] -> [StruGeneSample] -> [StruGeneSample]
getStruGeneSampleOfLp [] struGeneSampleOfLpList = struGeneSampleOfLpList
getStruGeneSampleOfLp (x:xs) struGeneSampleOfLpList = getStruGeneSampleOfLp xs struGeneSampleOfLpList'
    where
    struGeneSampleOfLpList' = case svt7 x == Lp of
                               True -> struGeneSampleOfLpList ++ [x]
                               False -> struGeneSampleOfLpList

clusterToStruGeneSampleListFromDB :: [StruGeneSample] -> String -> String -> String -> Int -> KValRange -> SNumRange -> DistWeiRatioList -> IO ()
clusterToStruGeneSampleListFromDB struGeneSampleList clustResTbl arm df kVal kValRange sNumRange distWeiRatioList = do
    let sNum = fst3 sNumRange
    let m1 = head struGeneSampleList
    let sps = tail struGeneSampleList
    let actualKVal = div kVal 2
    let initialKPoints = getKModeByMaxMinPoint sps [m1] Map.empty actualKVal distWeiRatioList
    let scml = findCluster4AllSamplesByArithAdd struGeneSampleList initialKPoints actualKVal 0 [] distWeiRatioList

    let clusterMap = divideSameCluster scml Map.empty
    let origDistTotal = distTotalByClust scml 0.0
    let distMean = origDistTotal / fromIntegral sNum
    storeOneIterationResult clustResTbl 0 scml initialKPoints distMean       -- Store the first clustering result.
    finalINoDistMean <- findFinalCluster4AllSamplesByArithAdd clustResTbl clusterMap initialKPoints actualKVal 1 origDistTotal distWeiRatioList

    putStrLn $ "autoRunClustByChangeKValSNum: " ++ " final iNo and distMean are " ++  show finalINoDistMean
    putStrLn $ "The clustering of kVal = " ++ show kVal ++ ", sNum = " ++ show sNum ++ " has finished."



autoRunGetAmbiResolAccuracyOfAllClustRes :: String -> String -> Int -> Int -> Int -> Int -> Int -> Int -> Int -> IO ()
autoRunGetAmbiResolAccuracyOfAllClustRes arm df kVal bottomKVal deltaKVal topKVal bottomSNum deltaSNum topSNum = do
    let sNum = bottomSNum
--    putStrLn $ "The ambiguity resolution accuracy of kVal = " ++ show kVal ++ ", sNum = " ++ show sNum ++ " is counting."
    conn <- getConn
    confInfo <- readFile "Configuration"                                        -- Read the local configuration file
    let ambi_resol_model = getConfProperty "ambi_resol_model" confInfo

    let ambiResolAccuracyTbl = "ambi_Resol_Accuracy_for_randomModes"
    let sqlstat = DS.fromString $ "create table if not exists " ++ ambiResolAccuracyTbl ++ " (kVal int, sNum int, samplesCount int, accuracyForRandomModes1 float, accuracyForRandomModes2 float, primary key (kVal, sNum, samplesCount))"
    stmt <- prepareStmt conn sqlstat
    executeStmt conn stmt []                          -- Create a new MySQL table for storing all ambiguity resolution accuracy.

    if | arm == "Nothing" -> putStrLn "autoRunGetAmbiResolAccuracyOfAllClustRes: 'ambi_resol_model' is not correct."
       | df == "Nothing" -> putStrLn "autoRunGetAmbiResolAccuracyOfAllClustRes: 'distDef' is not correct."
       | otherwise -> do

           let sqlstat = DS.fromString $ "select id, leftExtend, leftOver, rightOver, rightExtend, overType, prior from " ++ ambi_resol_model ++ " where id <= ? "
           stmt <- prepareStmt conn sqlstat
           (defs, is) <- queryStmt conn stmt [toMySQLInt32 sNum]
           struGeneSampleList <- readStreamByInt324TextInt8Text [] is
           let sIdxStruGeneMap = Map.fromList $ map (\x -> (fst7 x, (snd7 x, thd7 x, fth7 x, fif7 x, sth7 x, svt7 x))) struGeneSampleList

           randomSIdsList1 <- getRandomsList kVal 1 sNum []
--           putStrLn $ "randoms = " ++ show randomSIdsList1
           let randomStruGene1 = getStruGenebySIdsList randomSIdsList1 sIdxStruGeneMap []
--           putStrLn $ "randomStruGene = " ++ show randomStruGene1
           randomSIdsList2 <- getRandomsList kVal 1 sNum []
           let randomStruGene2 = getStruGenebySIdsList randomSIdsList2 sIdxStruGeneMap []
{-
           let clustResTbl = "clust_res_k" ++ show kVal ++ "_" ++ "s" ++ show sNum ++ "_" ++ arm ++ "_" ++ df
           let sqlstat = DS.fromString $ "select modes from " ++ clustResTbl
           stmt <- prepareStmt conn sqlstat
           (defs, is) <- queryStmt conn stmt []
           modesListOfAllIterations <- readStreamByText [] is

           let finalModesListStr = last (init modesListOfAllIterations)
           let finalModesList = readStruGeneListFromStr finalModesListStr
           let initalModesListStr = head modesListOfAllIterations
           let initalModesList = readStruGeneListFromStr initalModesListStr
-}
           let randomModesList1 = randomStruGene1
           let randomModesList2 = randomStruGene2
{-
           let sams1 = findAmbiResolResOfAllSamples struGeneSampleList finalModesList []
           let hitResList1 = map (\x -> snd3 x == thd3 x) sams1
           let accuracyForFinalModes = fromIntegral (length [x | x <- hitResList1, x == True]) / fromIntegral sNum

           let sams2 = findAmbiResolResOfAllSamples struGeneSampleList initalModesList []
           let hitResList2 = map (\x -> snd3 x == thd3 x) sams2
           let accuracyForInitalModes = fromIntegral (length [x | x <- hitResList2, x == True]) / fromIntegral sNum
-}
           let sams3 = findAmbiResolResOfAllSamples struGeneSampleList randomModesList1 []
           let hitResList3 = map (\x -> snd3 x == thd3 x) sams3
           let accuracyForRandomModes1 = fromIntegral (length [x | x <- hitResList3, x == True]) / fromIntegral sNum
           let sams4 = findAmbiResolResOfAllSamples struGeneSampleList randomModesList2 []
           let hitResList4 = map (\x -> snd3 x == thd3 x) sams4
           let accuracyForRandomModes2 = fromIntegral (length [x | x <- hitResList4, x == True]) / fromIntegral sNum

--           putStrLn $ "The ambiguity resolution accuracy of (kVal = " ++ show kVal ++ ", sNum = " ++ show sNum ++ "), first " ++ show sNum ++ " samples is accuracyForFinalModes = "++ show accuracyForFinalModes ++ ", accuracyForInitalModes = " ++ show accuracyForInitalModes++ ", accuracyForRandomModes = " ++ show accuracyForRandomModes
           putStrLn $ "The ambiguity resolution accuracy of (kVal = " ++ show kVal ++ ", sNum = " ++ show sNum ++ "), first " ++ show sNum ++ " samples is accuracyForRandomModes1 = " ++ show accuracyForRandomModes1 ++ ", accuracyForRandomModes2 = " ++ show accuracyForRandomModes2
           let sqlstat = DS.fromString $ "replace into " ++ ambiResolAccuracyTbl ++ " set kVal = ?, sNum = ?, samplesCount = ?, accuracyForRandomModes1 = ?, accuracyForRandomModes2 = ?"
           stmt <- prepareStmt conn sqlstat
           executeStmt conn stmt [toMySQLInt32 kVal, toMySQLInt32 sNum, toMySQLInt32 sNum, toMySQLFloat accuracyForRandomModes1, toMySQLFloat accuracyForRandomModes2]

           let kVal' = kVal + deltaKVal
           let (kVal'', sNum') = case kVal' > topKVal of
                                   True -> (bottomKVal, sNum + deltaSNum)
                                   False -> (kVal', sNum)
           if sNum' > topSNum
             then putStrLn "autoRunGetAmbiResolAccuracyOfAllClustRes: End."
             else autoRunGetAmbiResolAccuracyOfAllClustRes arm df kVal'' bottomKVal deltaKVal topKVal sNum' deltaSNum topSNum

getStruGenebySIdsList :: [Int] -> Map.Map SIdx StruGene -> [StruGene] -> [StruGene]
getStruGenebySIdsList [] _ struGeneList = struGeneList
getStruGenebySIdsList (x:xs) sIdxStruGeneMap struGeneList = getStruGenebySIdsList xs sIdxStruGeneMap struGeneList'
    where
    struGene = Map.lookup x sIdxStruGeneMap
    struGene' = case struGene of
                  Just x -> x
                  Nothing -> nullStruGene
    struGeneList' = struGeneList ++ [struGene']

{- 该函数由于每次迭代都需要连接数据库，运行时会报Exception: DecodePacketFailed "\DLE\EOTToo many connections" 1 "not enough bytes"
queryStruGenebySIdsList :: [Int] -> [StruGene] -> IO [StruGene]
queryStruGenebySIdsList idsList struGeneList = do
    length idsList
    conn <- getConn
    confInfo <- readFile "Configuration"
    let ambi_resol_model = getConfProperty "ambi_resol_model" confInfo
    let sqlstat = DS.fromString $ "select id, leftExtend, leftOver, rightOver, rightExtend, overType, prior from " ++ ambi_resol_model ++ " where id = ? "
    stmt <- prepareStmt conn sqlstat
    (defs, is) <- queryStmt conn stmt [toMySQLInt32 x]

    struGeneSampleList <- readStreamByInt324TextInt8Text [] is
    let m = head struGeneSampleList       -- Only a value in struGeneSampleList
    let struGeneList' = struGeneList ++ [(snd7 m, thd7 m, fth7 m, fif7 m, sth7 m, svt7 m)]

    if length xs == 0
      then return struGeneList'
      else queryStruGenebySIdsList xs struGeneList'
-}

getRandomsList :: Int -> Int -> Int -> [Int] -> IO [Int]
getRandomsList count rangeStart rangeEnd randomList = do
    random <- randomRIO (rangeStart, rangeEnd)
    let randomList' = randomList ++ [random]
    if length randomList' == count
      then return randomList'
      else getRandomsList count rangeStart rangeEnd randomList'

{-
getRandomsList :: Int -> Int -> Int -> IO [Int]
getRandomsList count rangeStart rangeEnd = do
    let rollDice = applyAtomicGen (uniformR (rangeStart, rangeEnd)) globalStdGen
    let randomList = replicateM count (rollDice :: IO Int)
    return randomList
-}

{- 对于一个(kVal,sNum)上的聚类结果，计算其消歧的正确率.
 - 歧义消解库stru_gene的前sNum条歧义消解片段和每一个(kVal,sNum)上聚类得到的众心作为原始数据.
 - 对每条歧义消解片段找到其最近的众心，若歧义消解片段的prior和众心的prior一致，算命中，命中次数加1.
 - 最终求出的准确率accuracy为：命中次数除以总次数
-}
getAmbiResolAccuracyOfAClustRes :: IO Float
getAmbiResolAccuracyOfAClustRes = do
    conn <- getConn
    confInfo <- readFile "Configuration"                                        -- Read the local configuration file
    let clustResTbl = getConfProperty "clustResTbl" confInfo
    putStrLn $ "The current clustResTbl is " ++ show clustResTbl

    putStr "Please input the start value of 'id' in stru_gene as test datas: "
    line <- getLine
    let startId = read line :: Int
    putStr "Please input the end value of 'id' in stru_gene as test datas: "
    line <- getLine
    let endId = read line :: Int

    let sNum = endId - startId + 1

    let sqlstat = DS.fromString $ "select modes from " ++ clustResTbl
    stmt <- prepareStmt conn sqlstat
    (defs, is) <- queryStmt conn stmt []
    modesListOfAllIterations <- readStreamByText [] is
    let finalModesListStr = head modesListOfAllIterations
--    let finalModesListStr = last (init modesListOfAllIterations)
    let finalModesList = readStruGeneListFromStr finalModesListStr

    let ambi_resol_model = getConfProperty "ambi_resol_model" confInfo
    let sqlstat = DS.fromString $ "select id, leftExtend, leftOver, rightOver, rightExtend, overType, prior from " ++ ambi_resol_model ++ " where id >= ? and id <= ? "
    stmt <- prepareStmt conn sqlstat
    (defs, is) <- queryStmt conn stmt [toMySQLInt32 startId, toMySQLInt32 endId]

    struGeneSampleList <- readStreamByInt324TextInt8Text [] is
    let sams = findAmbiResolResOfAllSamples struGeneSampleList finalModesList []
--    putStrLn $ "The ambiguity resolution result is for first samples range in " ++ show startId ++ "~" ++ show endId ++ " is " ++ show sams ++ " ."
    let hitResList = map (\x -> snd3 x == thd3 x) sams
    let accuracy = fromIntegral (length [x | x <- hitResList, x == True]) / fromIntegral sNum
    putStrLn $ "The ambiguity resolution accuracy is for samples range in " ++ show startId ++ "~" ++ show endId ++ " is " ++ show accuracy ++ "."
    return accuracy

type Modes = [StruGene]
type ActualPrior = Prior
type AmbiResolPrior = Prior
type SampleAmbiMark = (SIdx, ActualPrior, AmbiResolPrior)

{- find ambiguity resolution result of all stru_gene samples.
-}
findAmbiResolResOfAllSamples :: [StruGeneSample] -> Modes -> [SampleAmbiMark] -> [SampleAmbiMark]
findAmbiResolResOfAllSamples [] _ sampleAmbiMark = sampleAmbiMark
findAmbiResolResOfAllSamples (x:xs) modes sampleAmbiMark = findAmbiResolResOfAllSamples xs modes sampleAmbiMark'
    where
    sampleAmbiMark' = sampleAmbiMark ++ [findAmbiResolResOfASample x modes]

{- find ambiguity resolution result of a stru_gene sample
-}
findAmbiResolResOfASample :: StruGeneSample -> Modes -> SampleAmbiMark
findAmbiResolResOfASample sg modes = (sIdx, actualPrior, ambiResolPrior)
    where
    sIdx = fst7 sg
    actualPrior = svt7 sg
    distList = findDistFromASampleToModes sg modes []
    minDist = minimum distList
    index = elemIndex minDist distList
    index' = case index of
               Just x -> x
               Nothing -> error "findAmbiResolResOfASample: Impossible situation."
    finalMode = modes!!index'
    ambiResolPrior = sth6 finalMode

{- find all distances from a sample to modes.
-}
findDistFromASampleToModes :: StruGeneSample -> Modes -> [Dist] -> [Dist]
findDistFromASampleToModes _ [] distList = distList
findDistFromASampleToModes sg (x:xs) distList = findDistFromASampleToModes sg xs distList'
    where
    sg' = (snd7 sg, thd7 sg, fth7 sg, fif7 sg, sth7 sg, svt7 sg)
    dist = dist4StruGeneWithoutPriByArithAdd sg' x
    distList' = distList ++ [dist]

{- Arithmetically added distance between two StruGenes besides prior.
 -}
dist4StruGeneWithoutPriByArithAdd :: StruGene -> StruGene -> Float
dist4StruGeneWithoutPriByArithAdd s1 s2 = foldl (+) 0.0 $ distVect4StruGene s1 s2

distVect4StruGeneWithoutPri :: StruGene -> StruGene -> [Float]
distVect4StruGeneWithoutPri s1 s2 = [d1, d2, d3, d4, d5]
    where
    d1 = distPhraSynSet (fst6 s1) (fst6 s2)
    d2 = fromIntegral $ distPhraSyn (snd6 s1) (snd6 s2)
    d3 = fromIntegral $ distPhraSyn (thd6 s1) (thd6 s2)
    d4 = distPhraSynSet (fth6 s1) (fth6 s2)
    d5 = case fif6 s1 == fif6 s2 of
            True -> 0.0
            False -> 1.0

{- Read original ambiguity resolution samples or their clustering result,
 - the source of which is indicated by attribute 'ambi_resol_samples' in configuration.
 -}
getAmbiResolSamples :: IO [StruGeneSample]
getAmbiResolSamples = do
    confInfo <- readFile "Configuration"
    let ambi_resol_samples = getConfProperty "ambi_resol_samples" confInfo
    conn <- getConn

    if ambi_resol_samples == "stru_gene2"
      then do
        let sqlstat = DS.fromString $ "select id, leftExtend, leftOver, rightOver, rightExtend, overType, prior from " ++ ambi_resol_samples
        stmt <- prepareStmt conn sqlstat
        (defs, is) <- queryStmt conn stmt []                    -- [int unsigned, varchar, ..]
        struGeneSampleList <- readStreamByInt324TextInt8Text [] is
        return struGeneSampleList
      else if (splitAtDeli '_' ambi_resol_samples)!!4 == "sg"
        then do
          let sqlstat = DS.fromString $ "select modes from " ++ ambi_resol_samples ++ " where iNo = (select max(iNo) from " ++ ambi_resol_samples ++ ") - 1"
                                    -- MySQL table 'ambi_resol_samples' have at least two records.
          stmt <- prepareStmt conn sqlstat
          (defs, is) <- queryStmt conn stmt []
          modeListStrList <- readStreamByText [] is
          let modeListStr = head modeListStrList      -- head of [String], which has only one element.
          let modeList = readStruGeneListFromStr modeListStr
          let len = length modeList
          let sgs = zip [1..len] modeList
          return (map (\x -> (fst x, fst6 (snd x), snd6 (snd x), thd6 (snd x), fth6 (snd x), fif6 (snd x), sth6 (snd x))) sgs)
        else do
          putStrLn "getAmbiResolSamples: Value of property 'ambi_resol_samples' does not match any MySQL table."
          return []

type SentClauPhraList = [[[PhraCate]]]        -- A list includs sentences, a sentence incluse clauses, and a clause includes phrases, a phrase has a value of PhraCate.
type SimDeg = Float           -- Similarity degree of some kind of grammatic attribue, such as syntatic type, grammatic rule, phrasal type, and so on.
type NumOfPhraSyn = Int       -- PhraSyn :: (Category, Tag, PhraStru)
type NumOfCate = Int
type NumOfCatePair = Int
type NumOfTag = Int
type NumOfTagPair = Int
type NumOfPhraStru = Int
type NumOfStruPair = Int

{- Get similarity degree bewteen any two syntatic types (namely categories).
 - numOfPhraSyn: Number of different PhraSyns, namely (Category, Tag, PhraStru).
 - numOfCate: Number of different categories.
 - numOfCatePair: Number of different category pairs.
 -}
getTypePairSim :: SentClauPhraList -> (NumOfPhraSyn, NumOfCate, NumOfCatePair, [((Category, Category), SimDeg)])
getTypePairSim sentClauPhraList = (numOfPhraSyn, numOfCate, numOfCatePair, typePair2SimList)
    where
    typeTagStru2FreqMap = toTypeTagStru2FreqMap sentClauPhraList Map.empty             -- Map String Int, namely Map <type_tag_stru> <ttsNum>.
    typeTagStru2FreqMapList = Map.toList typeTagStru2FreqMap                           -- [(String, Int)]
    phraSynList = map stringToPhraSyn $ map fst typeTagStru2FreqMapList                -- [PhraSyn]
    type2TagStruMap = toType2TagStruMap phraSynList Map.empty                          -- Map Category [(Tag, PhraStru)]
    type2TagStruMapList = Map.toList type2TagStruMap                                   -- [(Category, [(Tag, PhraStru)])]
    type2TagStruSetList = map (\x -> (fst x, (Set.fromList . snd) x)) type2TagStruMapList       -- [(Category, Set (Tag, PhraStru))]
    typePair2SimList = [((fst x, fst y), (fromIntegral . Set.size) (Set.intersection (snd x) (snd y)) / (fromIntegral . Set.size) (Set.union (snd x) (snd y))) | x<-type2TagStruSetList, y<-type2TagStruSetList]
    numOfPhraSyn = Map.size typeTagStru2FreqMap
    numOfCate = Map.size type2TagStruMap
    numOfCatePair = length typePair2SimList

{- Get the frequencies of various triple (syntactic type, CCG rule tag, Phrasal structure), actually the triple is represented by string <type_tag_stru>,
 - because the frequencies of various triples are stored in a Data.Map, where these triples need change as keys, and frequencies are thought as corrresponding values.
 - The input is the list of sentential categories, and the output is Map <type_tag_stru> <ttsNum>.
 -}
toTypeTagStru2FreqMap :: [[[PhraCate]]] -> Map String Int -> Map String Int
toTypeTagStru2FreqMap [] tts2FreqMap = tts2FreqMap                              -- No sentence
toTypeTagStru2FreqMap [[]] tts2FreqMap = tts2FreqMap                            -- One sentence has no clause to deal with.
toTypeTagStru2FreqMap [(c:cs)] tts2FreqMap = toTypeTagStru2FreqMap [cs] (insertPhraList2TtsFreqMap c tts2FreqMap)
                                                                      -- 'c' is a clause, a list of phrasal categories.
toTypeTagStru2FreqMap (sent:sents) tts2FreqMap = Map.unionWith (+) mapHead mapTail      -- "sent" means a sentence.
    where
    mapHead = toTypeTagStru2FreqMap [sent] tts2FreqMap
    mapTail = toTypeTagStru2FreqMap sents tts2FreqMap

{- Insert a series of 'type_tag_stru' into a Map to count the frequency of every 'type_tag_stru'.
 -}
insertPhraList2TtsFreqMap :: [PhraCate] -> Map String Int -> Map String Int
insertPhraList2TtsFreqMap [] tts2FreqMap = tts2FreqMap
insertPhraList2TtsFreqMap [x] tts2FreqMap
    | ruleTag == "Desig" = tts2FreqMap
    | otherwise = Map.insert type_tag_stru ttsNum tts2FreqMap
    where
    syntaxType = show $ ((!!0) . caOfCate) x
                          -- Apply 'caOfCate' to every phrasal category, and take the first element from the above result.
    ruleTag = ((!!0) . taOfCate) x
                          -- Apply 'taOfCate' to every phrasal category, and take the first element from the above result.
    phraStru = ((!!0) . psOfCate) x
                          -- Apply 'psOfCate' to every phrasal category, and take the first element from the above result.
    type_tag_stru = syntaxType ++ "_" ++ ruleTag ++ "_" ++ phraStru
    ttsNum = maybe 1 (1+) (Map.lookup type_tag_stru tts2FreqMap)
insertPhraList2TtsFreqMap (x:xs) tts2FreqMap = insertPhraList2TtsFreqMap xs (insertPhraList2TtsFreqMap [x] tts2FreqMap)

{- The string format is "type_tag_stru", type is category, tag is grammatic rule tag, and stru is phrasal structure.
 -}
stringToPhraSyn :: String -> PhraSyn
stringToPhraSyn tts = (getCateFromString (tts'!!0), tts'!!1, tts'!!2)
    where
    tts' = splitAtDeli '_' tts

{- Get similarity degree bewteen any two grammtic rules (namely tags).
 - numOfPhraSyn: Number of different PhraSyns, namely (Category, Tag, PhraStru).
 - numOfTag: Number of different grammatic rules.
 - numOfTagPair: Number of different rule pairs.
 -}
getTagPairSim :: SentClauPhraList -> (NumOfPhraSyn, NumOfTag, NumOfTagPair, [((Tag, Tag), SimDeg)])
getTagPairSim sentClauPhraList = (numOfPhraSyn, numOfTag, numOfTagPair, tagPair2SimList)
    where
    typeTagStru2FreqMap = toTypeTagStru2FreqMap sentClauPhraList Map.empty             -- Map String Int, namely Map <type_tag_stru> <ttsNum>.
    typeTagStru2FreqMapList = Map.toList typeTagStru2FreqMap                           -- [(String, Int)]
    phraSynList = map stringToPhraSyn $ map fst typeTagStru2FreqMapList                -- [PhraSyn]
    tag2TypeStruMap = toTag2TypeStruMap phraSynList Map.empty                          -- Map Tag [(Category, PhraStru)]
    tag2TypeStruMapList = Map.toList tag2TypeStruMap                                   -- [(Tag, [(Category, PhraStru)])]
    tag2TypeStruSetList = map (\x -> (fst x, (Set.fromList . snd) x)) tag2TypeStruMapList       -- [(Tag, Set (Category, PhraStru))]
    tagPair2SimList = [((fst x, fst y), (fromIntegral . Set.size) (Set.intersection (snd x) (snd y)) / (fromIntegral . Set.size) (Set.union (snd x) (snd y))) | x<-tag2TypeStruSetList, y<-tag2TypeStruSetList]
    numOfPhraSyn = Map.size typeTagStru2FreqMap
    numOfTag = Map.size tag2TypeStruMap
    numOfTagPair = length tagPair2SimList

{- Get Map Category [(Tag, PhraStru)] from PhraSyn list.
 -}
toType2TagStruMap :: [PhraSyn] -> Map Category [(Tag, PhraStru)] -> Map Category [(Tag, PhraStru)]
toType2TagStruMap [] c2TsListMap = c2TsListMap
toType2TagStruMap [phraSyn] c2TsListMap = Map.insert cate newTs c2TsListMap
    where
    cate = fst3 phraSyn
    ts = (snd3 phraSyn, thd3 phraSyn)
    newTs = maybe [ts] (ts:) (Map.lookup cate c2TsListMap)
toType2TagStruMap (ps:pss) c2TsListMap = toType2TagStruMap pss (toType2TagStruMap [ps] c2TsListMap)

{- Get Map Tag [(Category, PhraStru)] from PhraSyn list.
 -}
toTag2TypeStruMap :: [PhraSyn] -> Map Tag [(Category, PhraStru)] -> Map Tag [(Category, PhraStru)]
toTag2TypeStruMap [] t2CsListMap = t2CsListMap
toTag2TypeStruMap [phraSyn] t2CsListMap = Map.insert tag newCs t2CsListMap
    where
    tag = snd3 phraSyn
    cs = (fst3 phraSyn, thd3 phraSyn)
    newCs = maybe [cs] (cs:) (Map.lookup tag t2CsListMap)
toTag2TypeStruMap (ps:pss) t2CsListMap = toTag2TypeStruMap pss (toTag2TypeStruMap [ps] t2CsListMap)

{- Get Map PhraStru [(Category, Tag)] from PhraSyn list.
 -}
toStru2TypeTagMap :: [PhraSyn] -> Map PhraStru [(Category, Tag)] -> Map PhraStru [(Category, Tag)]
toStru2TypeTagMap [] s2CtListMap = s2CtListMap
toStru2TypeTagMap [phraSyn] s2CtListMap = Map.insert stru newCt s2CtListMap
    where
    stru = thd3 phraSyn
    ct = (fst3 phraSyn, snd3 phraSyn)
    newCt = maybe [ct] (ct:) (Map.lookup stru s2CtListMap)
toStru2TypeTagMap (ps:pss) s2CtListMap = toStru2TypeTagMap pss (toStru2TypeTagMap [ps] s2CtListMap)

{- Get similarity degree bewteen any two phrasal structures.
 - numOfPhraSyn: Number of different PhraSyns, namely (Category, Tag, PhraStru).
 - numOfPhraStru: Number of different phrasal structures.
 - numOfStruPair: Number of different phrase structure pairs.
 -}
getStruPairSim :: SentClauPhraList -> (NumOfPhraSyn, NumOfPhraStru, NumOfStruPair, [((PhraStru, PhraStru), SimDeg)])
getStruPairSim sentClauPhraList = (numOfPhraSyn, numOfPhraStru, numOfStruPair, struPair2SimList)
    where
    typeTagStru2FreqMap = toTypeTagStru2FreqMap sentClauPhraList Map.empty             -- Map String Int, namely Map <type_tag_stru> <ttsNum>.
    typeTagStru2FreqMapList = Map.toList typeTagStru2FreqMap                           -- [(String, Int)]
    phraSynList = map stringToPhraSyn $ map fst typeTagStru2FreqMapList                -- [PhraSyn]
    stru2TypeTagMap = toStru2TypeTagMap phraSynList Map.empty                          -- Map PhraStru [(Category, Tag)]
    stru2TypeTagMapList = Map.toList stru2TypeTagMap                                   -- [(PhraStru, [(Category, Tag)])]
    stru2TypeTagSetList = map (\x -> (fst x, (Set.fromList . snd) x)) stru2TypeTagMapList       -- [(PhraStru, Set (Category, Tag))]
    struPair2SimList = [((fst x, fst y), (fromIntegral . Set.size) (Set.intersection (snd x) (snd y)) / (fromIntegral . Set.size) (Set.union (snd x) (snd y))) | x<-stru2TypeTagSetList, y<-stru2TypeTagSetList]
    numOfPhraSyn = Map.size typeTagStru2FreqMap
    numOfPhraStru = Map.size stru2TypeTagMap
    numOfStruPair = length struPair2SimList

{- Get similarity degree bewteen any phrases in their PhraSyn = (Category, Tag, PhraStru).
 - numOfPhraSyn: Number of different PhraSyns.
 - numOfPhraSynPair: Number of different PhraSyn pairs.
 -}
--getPhraSynPairSim :: SentClauPhraList -> [((PhraSyn, PhraSyn), SimDeg)])
getPhraSynPairSim :: SentClauPhraList -> (NumOfCatePair, NumOfTagPair, NumOfPhraStru, [(((Category, Category), SimDeg), ((Tag, Tag), SimDeg), ((PhraStru, PhraStru), SimDeg))])
getPhraSynPairSim sentClauPhraList = (numOfCatePair, numOfTagPair, numOfStruPair, sparseSimMatrix)
    where
    typePair2SimTuple = getTypePairSim sentClauPhraList                   -- (NumOfPhraSyn, NumOfCate, NumOfCatePair, [((Category, Category), SimDeg)])
    typePair2Sim = fth4 typePair2SimTuple
    numOfCatePair = thd4 typePair2SimTuple
    tagPair2SimTuple = getTagPairSim sentClauPhraList                     -- (NumOfPhraSyn, NumOfTag, NumOfTagPair, [((Tag, Tag), SimDeg)])
    tagPair2Sim = fth4 tagPair2SimTuple
    numOfTagPair = thd4 tagPair2SimTuple
    struPair2SimTuple = getStruPairSim sentClauPhraList                   -- (NumOfPhraSyn, NumOfPhraStru, NumOfStruPair, [((PhraStru, PhraStru), SimDeg)])
    struPair2Sim = fth4 struPair2SimTuple
    numOfStruPair = thd4 struPair2SimTuple
    sparseSimMatrix = [(ccSim, ttSim, ssSim) | ccSim <- typePair2Sim, ttSim <- tagPair2Sim, ssSim <- struPair2Sim, (snd ccSim + snd ttSim + snd ssSim) /= 0]
