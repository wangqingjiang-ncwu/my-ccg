{-# LANGUAGE OverloadedStrings, LambdaCase #-}
{-# LANGUAGE MultiWayIf #-}

-- Copyright (c) 2019-2024 China University of Water Resources and Electric Power
-- All rights reserved.
-- This module was written by Qian-qian WANG at 2023. The original goal was to cluster the samples of ambiguity resolution.
-- To complete clustering, the distances between phrases shoule be defined firstly. Unfortunately it is not a simple problem.
-- To evaluate similarity between two values of every grammatic attribute, mutual explanation was proposed by Qing-jiang WANG at 2024 autumn.

module Clustering (
    distPhraSynByIdentity,     -- PhraSyn -> PhraSyn -> Double
    distPhraSynSetByIdentity,  -- [PhraSyn] -> [PhraSyn] -> Double
    distVect4StruGeneByIdentity,         -- StruGene -> StruGene -> [Double]
    DistWeiRatioList,          -- [Int], namely [wle, wlo, wro, wre, wot, wpr]
    dist4StruGeneByWeightSum,  -- StruGene -> StruGene -> DistWeiRatioList -> Double
    dist4StruGeneByArithMean,  -- StruGene -> StruGene -> Double
    StruGene,                  -- (LeftExtend, LeftOver, RightOver, RightExtend, OverType, Prior)
    getKModeByMaxMinPoint,     -- [StruGeneSample] -> [StruGeneSample] -> Map.Map SIdx Double -> Int -> [StruGene]
    Dist,                      -- Double
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
    distTotalByClust,                    -- [StruGene] -> [StruGene] -> Double -> Double
    DistMean,                            -- Double
    SIdx,                                -- Int
    ClusterMap,                          -- Map CIdx [StruGene]
    findFinalCluster4AllSamplesByArithAdd,    -- TableName -> ClusterMap -> CentreList -> KVal -> INo -> DistTotal -> DistWeiRatioList -> IO (INo, Dist)
    readStreamByInt324TextInt8Text,           -- [StruGeneSample] -> S.InputStream [MySQLValue] -> IO [StruGeneSample]
    storeOneIterationResult,                  -- TableName -> INo -> [SampleClusterMark] -> [StruGene] -> DistMean -> IO ()
    storeClusterTime,                         -- TimeTableName -> KVal -> SNum -> NominalDiffTime -> NominalDiffTime -> Int -> Double -> IO ()
    autoRunClustByChangeKValSNum,             -- String -> String -> Int -> KValRange -> SNumRange -> DistWeiRatioList -> IO ()
    autoRunGetAmbiResolAccuracyOfAllClustRes, -- String -> String -> Int -> Int -> Int -> Int -> Int -> Int -> Int -> IO ()
    getRandomsList,                           -- Int -> Int -> Int -> [Int] -> IO [Int]
    getAmbiResolAccuracyOfAClustRes,          -- IO Double
    getAmbiResolSamples,                      -- IO [StruGeneSample]

    clusteringAnalysis,      -- Int -> IO ()
    SentClauPhraList,        -- [[[PhraCate]]]
    getTypePair2SimFromSCPL,         -- SentClauPhraList -> (NumOfPhraSyn, NumOfCate, NumOfCatePair, [((Category, Category), SimDeg)])
    getTypePair2SimFromPSL,          -- [PhraSyn] -> (NumOfPhraSyn, NumOfCate, NumOfCatePair, [((Category, Category), SimDeg)])
    getTagPair2SimFromSCPL,          -- SentClauPhraList -> (NumOfPhraSyn, NumOfTag, NumOfTagPair, [((Tag, Tag), SimDeg)])
    getTagPair2SimFromPSL,           -- [PhraSyn] -> (NumOfPhraSyn, NumOfTag, NumOfTagPair, [((Tag, Tag), SimDeg)])
    getStruPair2SimFromSCPL,         -- SentClauPhraList -> (NumOfPhraSyn, NumOfPhraStru, NumOfStruPair, [((PhraStru, PhraStru), SimDeg)])
    getStruPair2SimFromPSL,          -- [PhraSyn] -> (NumOfPhraSyn, NumOfPhraStru, NumOfStruPair, [((PhraStru, PhraStru), SimDeg)])
    getPhraSynFromSents,             -- SentClauPhraList -> [PhraSyn] -> [PhraSyn]
    stringToPhraSyn,                 -- String -> PhraSyn
    getPhraSynPairSimFromSCPLBySVD,       -- [(PhraSyn, PhraSyn)] -> ([(PhraSyn, PhraSyn)], Matrix Double, Matrix Double, Matrix Double, [((PhraSyn, PhraSyn), SimDeg)])
    getPhraSynPairSimFromSCPLByRMS,       -- SentClauPhraList -> ([(PhraSyn, PhraSyn)], Matrix Double, [((PhraSyn, PhraSyn), SimDeg)])
    getPhraSynPairSimBySVD,        -- [(PhraSyn, PhraSyn)] -> ([(PhraSyn, PhraSyn)], Matrix Double, Matrix Double, Matrix Double, [((PhraSyn, PhraSyn), SimDeg)])
    getPhraSynPairSimByRMS,        -- [(PhraSyn, PhraSyn)] -> ([(PhraSyn, PhraSyn)], Matrix Double, [((PhraSyn, PhraSyn), SimDeg)])
    getPhraSynPair2SimFromTreebank,  -- SentIdx -> SentIdx -> IO (Map (PhraSyn, PhraSyn) SimDeg)
    getPhraSynPair2SimFromCOT,       -- [ContextOfOT] -> Map (PhraSyn, PhraSyn) SimDeg
    getPhraSynPair2SimFromCSG,       -- [ContextOfSG] -> Map (PhraSyn, PhraSyn) SimDeg
    getPhraSynSetSim,                -- [PhraSyn] -> [PhraSyn] -> Map (PhraSyn, PhraSyn) SimDeg -> SimDeg
    getSentClauPhraList,             -- SentIdx -> SentIdx -> IO SentClauPhraList

    getContext2OverTypeBase,         -- SIdx -> SIdx -> IO Context2OverTypeBase
    getContextOfOTPairSimBySVD,      -- Context2OverTypeBase -> ([(ContextOfOT, ContextOfOT)], Matrix Double, Matrix Double, [((ContextOfOT, ContextOfOT), SimDeg)])
    getContextOfOTPairSimByRMS,      -- Context2OverTypeBase -> IO ([(SimDeg,SimDeg,SimDeg,SimDeg)], [((ContextOfOT, ContextOfOT), SimDeg)])
    getOverTypePair2Sim,             -- SIdx -> SIdx -> [((OverType, OverType), SimDeg)]
    getContext2ClauTagPriorBase,     -- SIdx -> SIdx -> IO [Context2ClauTagPrior]
    getContextOfSGPairSimBySVD,      -- Context2ClauTagPriorBase -> IO ([(ContextOfSG, ContextOfSG)], Matrix Double, Matrix Double, [((ContextOfSG, ContextOfSG), SimDeg)])
    getContextOfSGPairSimByRMS,      -- Context2ClauTagPriorBase -> IO ([(SimDeg, SimDeg, SimDeg, SimDeg, SimDeg)], [((ContextOfSG, ContextOfSG), SimDeg)])
    getOneToAllContextOfSGSimByRMS,  -- ContextOfSG -> Context2ClauTagPriorBase -> IO ([(SimDeg, SimDeg, SimDeg, SimDeg, SimDeg)], [((ContextOfSG, ContextOfSG), SimDeg)])
    getOneToAllContextOfSGSimByRMS', -- ContextOfSG -> Context2ClauTagPriorBase -> IO ([(SimDeg, SimDeg, SimDeg, SimDeg, SimDeg)], [((ContextOfSG, ContextOfSG), SimDeg)])
    findStruGeneSampleByMaxContextSim,   -- ContextOfSG -> Context2ClauTagPriorBase -> IO (SIdx, SimDeg, Context2ClauTagPrior)
    findWhereSim1HappenAmongStruGeneSamples,   -- SIdx -> Int -> [Context2ClauTagPrior] -> IO ()

    ) where

import Category
import Phrase
import AmbiResol
import qualified AmbiResol as AR
import Corpus
import Utils
import Output
import Data.Tuple (swap)
import Data.Tuple.Utils
import Data.List
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
import Numeric.LinearAlgebra.Data hiding (find)
import Numeric.LinearAlgebra as LA hiding (find)

{- The distance between two phrases, where only grammatical factors are considered,
 - and the correlations between different values of an identical factor and between different factors are both neglected.
 - Actually, some categories such 'np' and 's\.np' may be converted for some syntactic requirements, and phrasal
 - structures such as 'AHn' and 'HnC' have same grammatical nature, which means there may exist correlations bewteen
 - the different values of a same factor. Meanwhile, phrasal structure 'AHn' must have category 'np', so there exist
 - correlations between some different factors.
 - Principal Components Analysis (PCA) and Factor Analysis (FA) are promising methods for solving this problem.
 - The distance is normalized to [0, 1].
 -}
distPhraSynByIdentity :: PhraSyn -> PhraSyn -> Double
distPhraSynByIdentity (ca1, ta1, ps1) (ca2, ta2, ps2) = sum [v1, v2, v3] / 3.0
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

{- Calculate the distance between two phrase sets by element pairing between two sets.
 - Phrase set P = [p1, p2, ..., pn], phrase set Q = [q1, q2, ..., qm]
 - sim(pi, pj) = 1 - (distPhraSynByIdentity pi qj)
 - From high to low, select sim(pi, pj) where i<-[1..n] and j<-[1..m], namely pi and qj are a pair of elements.
 - Without loss of generality, if set P has a bigger cardinality than set Q, the redundant elements in set P pairing with null elements.
 - Calculate the mean of similarity degrees of all phrasal pairs, then convert into the final distance.
 -}
distPhraSynSetByIdentity :: [PhraSyn] -> [PhraSyn] -> Double
distPhraSynSetByIdentity ps qs = 1 - (getPhraSynSetSim ps qs phraSynPair2SimMap)
    where
    phraSynPair2SimMap = Map.fromList [((pi, qj), 1 - (distPhraSynByIdentity pi qj)) | pi <- ps, qj <- qs]  -- [((pi,qj),sim(pi,qj))]

{- The distance vector between two samples of model StruGene.
 - For ambiguity model StruGene = (LeftExtend, LeftOver, RightOver, RightExtend, OverType, Prior), the distance vector is obtained by following function.
 - Elements in distance vector all are in [0, 1].
 -}
distVect4StruGeneByIdentity :: StruGene -> StruGene -> [Double]
distVect4StruGeneByIdentity s1 s2 = [d1, d2, d3, d4, d5, d6]
    where
    d1 = distPhraSynSetByIdentity (fst6 s1) (fst6 s2)
    d2 = distPhraSynByIdentity (snd6 s1) (snd6 s2)
    d3 = distPhraSynByIdentity (thd6 s1) (thd6 s2)
    d4 = distPhraSynSetByIdentity (fth6 s1) (fth6 s2)
    d5 = case fif6 s1 == fif6 s2 of
            True -> 0.0
            False -> 1.0
    d6 = case sth6 s1 == sth6 s2 of
            True -> 0.0
            False -> 1.0

-- Weigth ratio list of distances on StruGene elements between two StruGene samples.
type DistWeiRatioList = [Int]     -- [wle, wlo, wro, wre, wot, wpr]

{- Calculate distance between two StruGenes.
 - (1) Weightedly sum distances on all StruGene components;
 - (2) Normalize to range [0, 1].
 -}
dist4StruGeneByWeightSum :: StruGene -> StruGene -> DistWeiRatioList -> Double
dist4StruGeneByWeightSum s1 s2 distWeiRatioList = sum distList / fromIntegral ratioTotal
    where
    distList = map (\x -> fromIntegral (fst x) * snd x) $ zip distWeiRatioList $ distVect4StruGeneByIdentity s1 s2
    ratioTotal = sum distWeiRatioList

{- Arithmetically meaned distance between two StruGenes.
 -}
dist4StruGeneByArithMean :: StruGene -> StruGene -> Double
dist4StruGeneByArithMean s1 s2 = (\x-> x / 6.0) $ sum (distVect4StruGeneByIdentity s1 s2)

{- 初始化随机选取1个点,初始定k=200

type StruGene' = ([LeftExtend], [LeftOver], [RightOver], [RightExtend], [OverType], [Prior])
type StruGene1 = StruGene'
--data sumClust = 200
-}

{- 计算每个样本到各簇心的距离的最小值mvList。
   样本：(x:xs)
   给定的簇心列表：sg

minValueList :: [StruGene] -> [StruGene] -> [Double] -> [Double]
minValueList [] _ mvList = mvList
minValueList _ [] mvList = mvList
minValueList (x:xs) sg mvList = minValueList xs sg mvList1
    where
    mi = minimum $ map (\x -> dist4StruGeneByWeightSum (fst x) (snd x) ) $ zip (replicate (length sg) x) sg
    mvList1 = mvList ++ [mi]

-- type initPointSet = ([StruGene],[StruGene],[Double])

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
getKModeByMaxMinPoint :: [StruGeneSample] -> [StruGeneSample] -> Map.Map SIdx Double -> Int -> DistWeiRatioList -> [StruGene]
getKModeByMaxMinPoint sgs oldModeList origSIdxMinValueMap kVal distWeiRatioList
    | length oldModeList == kVal = map (\x -> (snd7 x, thd7 x, fth7 x, fif7 x, sth7 x, svt7 x)) oldModeList
    | otherwise = getKModeByMaxMinPoint sgs' newModeList newSIdxMinValueMap kVal distWeiRatioList
    where
    lastMode = last oldModeList
    lastModeSIdx = fst7 (lastMode)
    lastModeStruGene = (snd7 lastMode, thd7 lastMode, fth7 lastMode, fif7 lastMode, sth7 lastMode, svt7 lastMode)
    li = Map.toList $ Map.delete lastModeSIdx origSIdxMinValueMap
    distListOfSamplesToLastMode = map (\x -> (fst7 x, dist4StruGeneByWeightSum lastModeStruGene (snd7 x, thd7 x, fth7 x, fif7 x, sth7 x, svt7 x) distWeiRatioList)) sgs
    newSIdxMinValueList = findDistMinValOfTwoTupleLists distListOfSamplesToLastMode li []
    newSIdxMinValueMap = Map.fromList newSIdxMinValueList
    newModeSIdx = snd $ Map.findMax $ Map.fromList $ map (\x ->(snd x, fst x)) newSIdxMinValueList
    newMode = find (\x -> fst7 x == newModeSIdx) sgs
    newMode' = case newMode of
                 Just x -> x
                 Nothing -> nullStruGeneSample
    sgs' = delete newMode' sgs
    newModeList = oldModeList ++ [newMode']                                     -- Actually, newMode' is not Null.

type Dist = Double
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

type DMin = Double                                        -- Minimum distance of a sample to all cluster centres.
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
    ds = map (\x -> dist4StruGeneByWeightSum sps x distWeiRatioList) ccl
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
    ds = map (\x -> dist4StruGeneByArithMean (fst x) (snd x)) $ zip (replicate kVal sp) ccl
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
distTotalByClust :: [SampleClusterMark] -> Double -> Double
distTotalByClust [] origDist = origDist
distTotalByClust (x:xs) origDist = distTotalByClust xs newDist
    where
    newDist = origDist + (snd5 x)

type DistTotal = Double                                   -- Sum of distances between all samples and their cluster centres.
type DistMean = Double
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
    executeStmt conn stmt [toMySQLInt8 iNo, toMySQLText scdStr, toMySQLText modesStr, toMySQLDouble distMean]
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
storeClusterTime :: TimeTableName -> KVal -> SNum -> NominalDiffTime -> NominalDiffTime -> Int -> Double -> IO ()
storeClusterTime timeTblName kVal sNum totalRunTime iterMeanTime its finalDistMean = do
    let tRTStr = init (show totalRunTime)
    let iMTStr = init (show iterMeanTime)
    let tRTDouble = read tRTStr :: Double
    let iMTDouble = read iMTStr :: Double
    let its' = its + 1
    putStrLn $ "storeClusterTime: tRTDouble = " ++ show tRTDouble ++ ", iMTDouble = " ++ show iMTDouble ++ ", iNo = " ++ show its' ++ ", kVal = " ++ show kVal ++ ", sNum = " ++ show sNum ++ ", finalDistMean = " ++ show finalDistMean

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
        ok <- executeStmt conn stmt [toMySQLDouble tRTDouble, toMySQLDouble iMTDouble, toMySQLInt8 its', toMySQLDouble finalDistMean, toMySQLInt32 kVal, toMySQLInt32 sNum]
        putStrLn $ "storeClusterTime: Number of rows update affected = " ++ show (getOkAffectedRows ok)
        close conn
      else do
        let sqlstat = DS.fromString $ "insert into " ++ timeTblName ++ " set kVal = ?, sNum = ?, totalTime = ?, iterMeanTime = ?, iNo = ?, finalDistMean = ?"
        stmt <- prepareStmt conn sqlstat
        ok <- executeStmt conn stmt [toMySQLInt32 kVal, toMySQLInt32 sNum, toMySQLDouble tRTDouble, toMySQLDouble iMTDouble, toMySQLInt8 its', toMySQLDouble finalDistMean]
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
           executeStmt conn stmt [toMySQLInt32 kVal, toMySQLInt32 sNum, toMySQLInt32 sNum, toMySQLDouble accuracyForRandomModes1, toMySQLDouble accuracyForRandomModes2]

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
getAmbiResolAccuracyOfAClustRes :: IO Double
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
dist4StruGeneWithoutPriByArithAdd :: StruGene -> StruGene -> Double
dist4StruGeneWithoutPriByArithAdd s1 s2 = foldl (+) 0.0 $ distVect4StruGeneByIdentity s1 s2

distVect4StruGeneWithoutPri :: StruGene -> StruGene -> [Double]
distVect4StruGeneWithoutPri s1 s2 = [d1, d2, d3, d4, d5]
    where
    d1 = distPhraSynSetByIdentity (fst6 s1) (fst6 s2)
    d2 = distPhraSynByIdentity (snd6 s1) (snd6 s2)
    d3 = distPhraSynByIdentity (thd6 s1) (thd6 s2)
    d4 = distPhraSynSetByIdentity (fth6 s1) (fth6 s2)
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

    putStrLn $ "Get samples from " ++ ambi_resol_samples

    if ambi_resol_samples == "stru_gene2"
      then do
        let sqlstat = DS.fromString $ "select id, leftExtend, leftOver, rightOver, rightExtend, overType, prior from " ++ ambi_resol_samples
        stmt <- prepareStmt conn sqlstat
        (defs, is) <- queryStmt conn stmt []
        struGeneSampleList <- readStreamByInt324TextInt8Text [] is              -- [StruGeneSample]
        return struGeneSampleList
      else if ambi_resol_samples == "stru_gene_202408"       -- Extract highest-frequency Prior value from ClauTagPrior value List.
        then do
          let sqlstat = DS.fromString $ "select id, leftExtend, leftOver, rightOver, rightExtend, overType, clauTagPrior from " ++ ambi_resol_samples
          stmt <- prepareStmt conn sqlstat
          (defs, is) <- queryStmt conn stmt []

          struGene2SampleList <- readStreamByStruGene2Sample [] is      -- [(SIdx,LeftExtend,LeftOver,RightOVer,RightExtend,OverType,[ClauTagPrior])]
--          putStrLn $ "struGene2SampleList!!0: " ++ show (struGene2SampleList!!0)
          let struGeneSampleList = map (\x -> (fst7 x, snd7 x, thd7 x, fth7 x, fif7 x, sth7 x, (fromMaybePrior . priorWithHighestFreq . svt7) x)) struGene2SampleList
          return struGeneSampleList
        else if (length (splitAtDeli '_' ambi_resol_samples) > 4 && (splitAtDeli '_' ambi_resol_samples)!!4 == "sg")
          then do
            let sqlstat = DS.fromString $ "select modes from " ++ ambi_resol_samples ++ " where iNo = (select max(iNo) from " ++ ambi_resol_samples ++ ") - 1"
                                    -- MySQL table 'ambi_resol_samples' have at least two records.
            stmt <- prepareStmt conn sqlstat
            (defs, is) <- queryStmt conn stmt []
            modeListStrList <- readStreamByText [] is
            let modeListStr = head modeListStrList               -- head of [String], which has only one element.
            let modeList = readStruGeneListFromStr modeListStr
            let len = length modeList
            let sgs = zip [1..len] modeList
            return (map (\x -> (fst x, fst6 (snd x), snd6 (snd x), thd6 (snd x), fth6 (snd x), fif6 (snd x), sth6 (snd x))) sgs)
          else do
            putStrLn "getAmbiResolSamples: Value of property 'ambi_resol_samples' does not match any MySQL table."
            return []

-- Implementation of menu 'D. Clustering analysis'.
clusteringAnalysis :: Int -> IO ()
clusteringAnalysis funcIndex = do
    -- 6. Test function getPhraSynSetSim.
    if funcIndex == 6
       then do
         putStrLn "(1) Collect phrasal similarity"
         startSn <- getNumUntil "Please input serial_num of start sentence [RETURN for 1]: " [1 ..]
         endSn <- getNumUntil "Please input serial_num of end sentence [RETURN for last]: " [0 ..]
         if startSn > endSn
           then putStrLn "No sentence is designated."
           else do
     --        sentClauPhraList <- getSentClauPhraList startSn endSn
     --        putStrLn $ "Phrasal categries of first sentence are: " ++ show (sentClauPhraList!!0)
             phraSynPair2SimMap <- getPhraSynPair2SimFromTreebank startSn endSn
             putStrLn $ "Map (PhraSyn, PhraSyn) SimDeg: " ++ show phraSynPair2SimMap

             putStrLn "(2) Calculate similarity between PhraSyn value sets."
             let ps1 = (npCate, ">", "AHn")
             let ps2 = (npCate, "A/n->", "AHn")
             let ps3 = (sCate, "<", "SP")
             let ps4 = (verbCate, ">B", "DHv")
             putStrLn "ps1 = (np, \">\", \"AHn\"), ps2 = (np, \"A/n->\", \"AHn\"), ps3 = (s, \"<\", \"SP\"), ps4 = ((s\\.np)/.np, \">B\", \"DHv\")"
             putStrLn $ "The result of getPhraSynSetSim [] [] phraSynPair2SimMap is: " ++ show (getPhraSynSetSim [] [] phraSynPair2SimMap)
             putStrLn $ "The result of getPhraSynSetSim [] [ps1] phraSynPair2SimMap is: " ++ show (getPhraSynSetSim [] [ps1] phraSynPair2SimMap)
             putStrLn $ "The result of getPhraSynSetSim [ps1] [] phraSynPair2SimMap is: " ++ show (getPhraSynSetSim [ps1] [] phraSynPair2SimMap)
             putStrLn $ "The result of getPhraSynSetSim [ps1] [ps1] phraSynPair2SimMap is: " ++ show (getPhraSynSetSim [ps1] [ps1] phraSynPair2SimMap)
             putStrLn $ "The result of getPhraSynSetSim [ps1] [ps1, ps2] phraSynPair2SimMap is: " ++ show (getPhraSynSetSim [ps1] [ps1, ps2] phraSynPair2SimMap)
             putStrLn $ "The result of getPhraSynSetSim [ps1, ps2] [ps2] phraSynPair2SimMap is: " ++ show (getPhraSynSetSim [ps1, ps2] [ps2] phraSynPair2SimMap)
             putStrLn $ "The result of getPhraSynSetSim [ps1, ps2, ps3] [ps2, ps3, ps4] phraSynPair2SimMap is: " ++ show (getPhraSynSetSim [ps1, ps2, ps3] [ps2, ps3, ps4] phraSynPair2SimMap)
             putStrLn $ "The result of getPhraSynSetSim [ps1] [ps2] phraSynPair2SimMap is: " ++ show (getPhraSynSetSim [ps1] [ps2] phraSynPair2SimMap)
             putStrLn $ "The result of getPhraSynSetSim [ps2] [ps1] phraSynPair2SimMap is: " ++ show (getPhraSynSetSim [ps2] [ps1] phraSynPair2SimMap)
             putStrLn $ "The result of toPhraSynPair2Sim phraSynPair2SimMap (ps1, ps2) is: " ++ show (toPhraSynPair2Sim phraSynPair2SimMap (ps1, ps2))
             putStrLn $ "The result of toPhraSynPair2Sim phraSynPair2SimMap (ps2, ps1) is: " ++ show (toPhraSynPair2Sim phraSynPair2SimMap (ps2, ps1))
             putStrLn $ "The result of getPhraSynSetSim [ps1, ps3] [ps2] phraSynPair2SimMap is: " ++ show (getPhraSynSetSim [ps1, ps3] [ps2] phraSynPair2SimMap)
             putStrLn $ "The result of getPhraSynSetSim [ps2, ps3] [ps1] phraSynPair2SimMap is: " ++ show (getPhraSynSetSim [ps2, ps3] [ps1] phraSynPair2SimMap)
             putStrLn $ "The result of getPhraSynSetSim [ps1] [ps2, ps3] phraSynPair2SimMap is: " ++ show (getPhraSynSetSim [ps1] [ps2, ps3] phraSynPair2SimMap)
             putStrLn $ "The result of getPhraSynSetSim [ps2] [ps1, ps3] phraSynPair2SimMap is: " ++ show (getPhraSynSetSim [ps2] [ps1, ps3] phraSynPair2SimMap)
       else putStr ""

    -- 7. Get similarity degrees between any two contexts of overlapping types by Singular Value Decomposition.
    if funcIndex == 7
       then do
         putStrLn "(1) From stru_gene_202408, collect all contexts of overlapping types."
         startIdx <- getNumUntil "Please input index number of start overtype sample [Return for 1]: " [1 ..]
         endIdx <- getNumUntil "Please input index number of end overtype sample [Return for last]: " [0 ..]
         context2OverTypeBase <- getContext2OverTypeBase startIdx endIdx        -- [(ContextOfOT, OverType)]

         putStrLn "(2) Calculate similarity degrees between any two contexts of overlapping types."
         contextOfOTPairSimTuple <- getContextOfOTPairSimBySVD context2OverTypeBase
                                        -- (contextOfOTPairList, origSimMatrix, orthSimMatrix, contextOfOTPair2SimList)
         let numOfContextOfOTPair = length (fst4 contextOfOTPairSimTuple)
         putStrLn $ "Num. of context pairs of overtypes: " ++ show numOfContextOfOTPair

         let origSimMatrix = snd4 contextOfOTPairSimTuple
         let origSimMatrixToRows = map toList $ toRows origSimMatrix                         -- [[SimDeg]]
         let origSimByRMS = map (sqrt . (/ 4.0) . sumElements) (toRows (cmap (\x -> x*x) origSimMatrix))       -- Root Mean Square
         let origSim2RMSMatrix = fromLists $ map (\x -> fst x ++ [snd x]) $ zip origSimMatrixToRows origSimByRMS     -- hmatrix

         let orthSimMatrix = thd4 contextOfOTPairSimTuple
         let orthSimMatrixToRows = map toList $ toRows orthSimMatrix                         -- [[SimDeg]]
         let orthSimByRMS = map (sqrt . (/ 4.0) . sumElements) (toRows (cmap (\x -> x*x) orthSimMatrix))       -- Root Mean Square
         let orthSim2RMSMatrix = fromLists $ map (\x -> fst x ++ [snd x]) $ zip orthSimMatrixToRows orthSimByRMS     -- hmatrix

         if (numOfContextOfOTPair > 10)
           then do
             putStrLn " The first 10 elements of contextOfOTPairList: "
             dispList 1 (take 10 (fst4 contextOfOTPairSimTuple))
             putStrLn " The first 10 elements of origSim2RMSMatrix (Last column is Root Mean Square): "
             disp 4 (origSim2RMSMatrix ?? (Take 10, Drop 0))
             putStrLn " The first 10 rows of orthSim2RMSMatrix (Last column is Root Mean Square): "
             disp 4 (orthSim2RMSMatrix ?? (Take 10, Drop 0))
             putStrLn " The first 10 rows of contextOfOTPair2SimList: "
             dispList 1 (take 10 (fth4 contextOfOTPairSimTuple))
           else do
             putStrLn " contextOfOTPairList: "
             dispList 1 (fst4 contextOfOTPairSimTuple)
             putStrLn " origSim2RMSMatrix (Last column is Root Mean Square): "
             disp 4 origSim2RMSMatrix
             putStrLn " orthSim2RMSMatrix (Last column is Root Mean Square): "
             disp 4 orthSim2RMSMatrix
             putStrLn " contextOfOTPair2SimList: "
             dispList 1 (fth4 contextOfOTPairSimTuple)
        else putStr ""

    -- 8. Get similarity degrees between any two contexts of overlapping types directly by Euclidean metric.
    if funcIndex == 8
       then do
         putStrLn "(1) From stru_gene_202408, collect all contexts of overlapping types."
         startIdx <- getNumUntil "Please input index number of start overtype sample [Return for 1]: " [0 ..]
         endIdx <- getNumUntil "Please input index number of end overtype sample [Return for last]: " [0 ..]
         context2OverTypeBase <- getContext2OverTypeBase startIdx endIdx        -- [(ContextOfOT, OverType)]

         putStrLn "(2) Calculate similarity degrees between any two contexts of overlapping types."
         contextOfOTPairSimTuple <- getContextOfOTPairSimByRMS context2OverTypeBase

         let origSimList = fst contextOfOTPairSimTuple
         let contextOfOTPair2SimList = snd contextOfOTPairSimTuple
         let origSimListWithRMS = zip origSimList (map snd contextOfOTPair2SimList)
         let origSimMatrixWithRMS = fromLists $ map (\x -> [(fst4 . fst) x, (snd4 . fst) x, (thd4 . fst) x, (fth4 . fst) x, snd x]) origSimListWithRMS

         let numOfContextOfOTPair = length contextOfOTPair2SimList
         putStrLn $ "Num. of context pairs of overtypes: " ++ show numOfContextOfOTPair

         if (numOfContextOfOTPair > 10)
           then do
             putStrLn " The first 10 rows of origSimMatrixWithRMS (Last column is Root Mean Square): "
             disp 4 (origSimMatrixWithRMS ?? (Take 10, Drop 0))
             putStrLn " The first 10 rows of contextOfOTPair2SimList: "
             dispList 1 (take 10 contextOfOTPair2SimList)
           else do
             putStrLn " origSimMatrixWithRMS (Last column is Root Mean Square): "
             disp 4 origSimMatrixWithRMS
             putStrLn " contextOfOTPair2SimList: "
             dispList 1 contextOfOTPair2SimList
        else putStr ""

-- 9. Get similarity degrees between any two phrasal overlapping types directly by Root Mean Square.
    if funcIndex == 9
       then do
         startIdx <- getNumUntil "Please input index number of start overtype sample [Return for 1]: " [0 ..]
         endIdx <- getNumUntil "Please input index number of end overtype sample [Return for last]: " [0 ..]
         overTypePair2Sim <- getOverTypePair2Sim startIdx endIdx
         putStrLn $ "overTypePair2Sim: " ++ show overTypePair2Sim
       else putStr ""

{- 10. Get similarity degrees between any two contexts of StruGene samples by Singular Value Decomposition.
 - ContextOfSG :: (LeftExtend, LeftOver, RightOver, RightExtend, OverType)
 - ContextOfOT :: (LeftExtend, LeftOver, RightOver, RightExtend)
 -}
    if funcIndex == 10
       then do
         putStrLn "(1) From stru_gene_202408, collect all contexts of StruGene samples."
         startIdx <- getNumUntil "Please input index number of start StruGene sample [Return for 1]: " [0 ..]
         endIdx <- getNumUntil "Please input index number of end StruGene sample [Return for last]: " [0 ..]
         context2ClauTagPriorBase <- getContext2ClauTagPriorBase startIdx endIdx             -- [(ContextOfSG, ClauTagPrior)]
         let context2OverTypeBase = map ((\x -> ((fst5 x, snd5 x, thd5 x, fth5 x), fif5 x)) . fst) context2ClauTagPriorBase   -- [(ContextOfOT, OverType)]

         putStrLn "(2) Calculate similarity degrees between any two contexts of StruGene samples."
         contextOfSGPairSimTuple <- getContextOfSGPairSimBySVD context2ClauTagPriorBase

         let origSimMatrix = snd4 contextOfSGPairSimTuple
         let origSimMatrixToRows = map toList $ toRows origSimMatrix                         -- [[SimDeg]]
         let origSimByRMS = map (sqrt . (/ 5.0) . sumElements) (toRows (cmap (\x -> x*x) origSimMatrix))        --  Root Mean Square
         let origSim2RMSMatrix = fromLists $ map (\x -> fst x ++ [snd x]) $ zip origSimMatrixToRows origSimByRMS     -- hmatrix

         let orthSimMatrix = thd4 contextOfSGPairSimTuple
         let orthSimMatrixToRows = map toList $ toRows orthSimMatrix                         -- [[SimDeg]]
         let orthSimByRMS = map (sqrt . (/ 5.0) . sumElements) (toRows (cmap (\x -> x*x) orthSimMatrix))        -- Root Mean Square
         let orthSim2RMSMatrix = fromLists $ map (\x -> fst x ++ [snd x]) $ zip orthSimMatrixToRows orthSimByRMS     -- hmatrix

         let numOfContextOfSGPair = rows origSimMatrix                          -- Int, Number of rows.
         putStrLn $ "Num. of context pairs of StruGene samples: " ++ show numOfContextOfSGPair

         if (numOfContextOfSGPair > 10)
           then do
             putStrLn " The first 10 elements of contextOfSGPairList: "
             dispList 1 (take 10 (fst4 contextOfSGPairSimTuple))
             putStrLn " The first 10 elements of origSim2RMSMatrix (Last column is Root Mean Square): "
             disp 4 (origSim2RMSMatrix ?? (Take 10, Drop 0))
             putStrLn " The first 10 rows of orthSim2RMSMatrix: "
             disp 4 (orthSim2RMSMatrix ?? (Take 10, Drop 0))
             putStrLn " The first 10 rows of contextOfSGPair2SimList (Last column is Root Mean Square): "
             dispList 1 (take 10 (fth4 contextOfSGPairSimTuple))
           else do
             putStrLn " contextOfSGPairList: "
             dispList 1 (fst4 contextOfSGPairSimTuple)
             putStrLn " origSim2RMSMatrix (Last column is Root Mean Square): "
             disp 4 origSim2RMSMatrix
             putStrLn " orthSim2RMSMatrix (Last column is Root Mean Square): "
             disp 4 orthSim2RMSMatrix
             putStrLn " contextOfSGPair2SimList: "
             dispList 1 (fth4 contextOfSGPairSimTuple)
       else putStr ""

{- 11. Get similarity degrees between any two contexts of StruGene samples by Root Mean Square.
 - ContextOfSG :: (LeftExtend, LeftOver, RightOver, RightExtend, OverType)
 - ContextOfOT :: (LeftExtend, LeftOver, RightOver, RightExtend)
 -}
    if funcIndex == 11
       then do
         putStrLn "(1) From stru_gene_202408, collect all contexts of StruGene samples."
         startIdx <- getNumUntil "Please input index number of start StruGene sample [Return for 1]: " [0 ..]
         endIdx <- getNumUntil "Please input index number of end StruGene sample [Return for last]: " [0 ..]
         context2ClauTagPriorBase <- getContext2ClauTagPriorBase startIdx endIdx             -- [(ContextOfSG, ClauTagPrior)]
         let context2OverTypeBase = map ((\x -> ((fst5 x, snd5 x, thd5 x, fth5 x), fif5 x)) . fst) context2ClauTagPriorBase   -- [(ContextOfOT, OverType)]

         putStrLn "(2) Calculate similarity degrees between any two contexts of StruGene samples."
         contextOfSGPairSimTuple <- getContextOfSGPairSimByRMS context2ClauTagPriorBase

         let origSimList = fst contextOfSGPairSimTuple
         let contextOfSGPair2SimList = snd contextOfSGPairSimTuple
         let origSimListWithRMS = zip origSimList (map snd contextOfSGPair2SimList)
         let origSimMatrixWithRMS = fromLists $ map (\x -> [(fst5 . fst) x, (snd5 . fst) x, (thd5 . fst) x, (fth5 . fst) x, (fif5 . fst) x, snd x]) origSimListWithRMS

         let numOfContextOfSGPair = length contextOfSGPair2SimList                          -- Int, Number of rows.
         putStrLn $ "Num. of context pairs of StruGene samples: " ++ show numOfContextOfSGPair

         if (numOfContextOfSGPair > 10)
           then do
             putStrLn " The first 10 rows of origSimMatrixWithRMS (Last column is Root Mean Square): "
             disp 4 (origSimMatrixWithRMS ?? (Take 10, Drop 0))
             putStrLn " The first 10 rows of contextOfSGPair2SimList: "
             dispList 1 (take 10 contextOfSGPair2SimList)
           else do
             putStrLn " origSimMatrixWithRMS (Last column is Root Mean Square): "
             disp 4 origSimMatrixWithRMS
             putStrLn " contextOfSGPair2SimList: "
             dispList 1 contextOfSGPair2SimList
       else putStr ""

{- 12. Among StruGene samples, calculate similarity degrees from one to all contexts by Root Mean Square.
 - ContextOfSG :: (LeftExtend, LeftOver, RightOver, RightExtend, OverType)
 -}
    if funcIndex == 12
       then do
         startIdx <- getNumUntil "Please input index number of start StruGene sample [Return for 1]: " [0 ..]
         endIdx <- getNumUntil "Please input index number of end StruGene sample [Return for last]: " [0 ..]
         context2ClauTagPriorBase <- getContext2ClauTagPriorBase startIdx endIdx             -- [(ContextOfSG, ClauTagPrior)]

         let numOfStruGeneSample = length context2ClauTagPriorBase
         sIdx <- getNumUntil "Please select one sample whose context will compare with others' contexts [Return for 1]: " [1 .. numOfStruGeneSample]
         let contextOfSG = fst (context2ClauTagPriorBase!!(sIdx-1))             -- ContextOfSG
         putStrLn $ "Its context is: " ++ show contextOfSG

         contextOfSGPairSimTuple <- getOneToAllContextOfSGSimByRMS contextOfSG context2ClauTagPriorBase

         let origSimList = fst contextOfSGPairSimTuple
         let contextOfSGPair2SimList = snd contextOfSGPairSimTuple
         let origSimListWithRMS = zip origSimList (map snd contextOfSGPair2SimList)
         let origSimMatrixWithRMS = fromLists $ map (\x -> [(fst5 . fst) x, (snd5 . fst) x, (thd5 . fst) x, (fth5 . fst) x, (fif5 . fst) x, snd x]) origSimListWithRMS

         let numOfContextOfSGPair = length contextOfSGPair2SimList              -- Int, Number of rows.
         let listOfContextOfSGWithSim1 = filter (\x -> snd x == 1.0) contextOfSGPair2SimList              -- [(ContextOfSG, ContestOfSG)]
         putStrLn $ "Num. of ClauTagPrior context pairs: " ++ show numOfContextOfSGPair
         putStrLn $ "Num. of ClauTagPrior context pairs with similarity degree 1.0: " ++ show (length listOfContextOfSGWithSim1)

         if (numOfContextOfSGPair > 10)
           then do
             putStrLn " The first 10 rows of origSimMatrixWithRMS (Last column is Root Mean Square): "
             disp 4 (origSimMatrixWithRMS ?? (Take 10, Drop 0))
             putStrLn " The first 10 rows of contextOfSGPair2SimList: "
             dispList 1 (take 10 contextOfSGPair2SimList)
           else do
             putStrLn " origSimMatrixWithRMS (Last column is Root Mean Square): "
             disp 4 origSimMatrixWithRMS
             putStrLn " contextOfSGPair2SimList: "
             dispList 1 contextOfSGPair2SimList
        else putStr ""

{- 13. Get similarity degrees between one ClauTagPrior context to every context of StruGene samples by Root Mean Square.
 - ContextOfSG :: (LeftExtend, LeftOver, RightOver, RightExtend, OverType)
 - Input No.4 Sample , it is ([(((s\.np)/#(s\.np))/*np,"Desig","DE")],(np,">","AHn"),(s,"<","SP"),[],2)
 -}
    if funcIndex == 13
       then do
         startIdx <- getNumUntil "Please input index number of start StruGene sample [Return for 1]: " [0 ..]
         endIdx <- getNumUntil "Please input index number of end StruGene sample [Return for last]: " [0 ..]
         context2ClauTagPriorBase <- getContext2ClauTagPriorBase startIdx endIdx             -- [(ContextOfSG, ClauTagPrior)]

         putStrLn "Please input one StruGene context [LeftExtend, LeftOver, RightOVer, RigheExtend, OverType] to be compared: "
         inputStr <- getLine
         let contextOfSG = readContextOfSGFromStr inputStr                      -- ContextOfSG

         contextOfSGPairSimTuple <- getOneToAllContextOfSGSimByRMS' contextOfSG context2ClauTagPriorBase

         let origSimList = fst contextOfSGPairSimTuple
         let contextOfSGPair2SimList = snd contextOfSGPairSimTuple
         let origSimListWithRMS = zip origSimList (map snd contextOfSGPair2SimList)
         let origSimMatrixWithRMS = fromLists $ map (\x -> [(fst5 . fst) x, (snd5 . fst) x, (thd5 . fst) x, (fth5 . fst) x, (fif5 . fst) x, snd x]) origSimListWithRMS

         let numOfContextOfSGPair = length contextOfSGPair2SimList              -- Int, Number of rows.
         let listOfContextOfSGWithSim1 = filter (\x -> snd x == 1.0) contextOfSGPair2SimList              -- [(ContextOfSG, ContestOfSG)]
         putStrLn $ "Num. of ClauTagPrior context pairs: " ++ show numOfContextOfSGPair
         putStrLn $ "Num. of ClauTagPrior context pairs with similarity degree 1.0: " ++ show (length listOfContextOfSGWithSim1)

         context2ClauTagPriorTuple <- findStruGeneSampleByMaxContextSim contextOfSG context2ClauTagPriorBase
         let sIdx = fst3 context2ClauTagPriorTuple
         let simDeg = snd3 context2ClauTagPriorTuple
         let context2ClauTagPrior = thd3 context2ClauTagPriorTuple
         putStrLn $ "Highest similarity degree " ++ show simDeg ++ " is obtained by No." ++ show sIdx ++ " sample: " ++ show context2ClauTagPrior

         if (numOfContextOfSGPair > 10)
           then do
             putStrLn " The first 10 rows of origSimMatrixWithRMS (Last column is Root Mean Square): "
             disp 4 (origSimMatrixWithRMS ?? (Take 10, Drop 0))
             putStrLn " The first 10 rows of contextOfSGPair2SimList: "
             dispList 1 (take 10 contextOfSGPair2SimList)
           else do
             putStrLn " origSimMatrixWithRMS (Last column is Root Mean Square): "
             disp 4 origSimMatrixWithRMS
             putStrLn " contextOfSGPair2SimList: "
             dispList 1 contextOfSGPair2SimList
       else putStr ""

type SentClauPhraList = [[[PhraCate]]]        -- A list includs sentences, a sentence incluse clauses, and a clause includes phrases, a phrase has a value of PhraCate.
type SimDeg = Double          -- Similarity degree of some kind of grammatic attribue, such as syntatic type, grammatic rule, phrasal type, and so on.
type NumOfPhraSyn = Int       -- PhraSyn :: (Category, Tag, PhraStru)
type NumOfCate = Int
type NumOfCatePair = Int
type NumOfTag = Int
type NumOfTagPair = Int
type NumOfPhraStru = Int
type NumOfStruPair = Int
type PhraSynPair2Sim = ((PhraSyn, PhraSyn), SimDeg)

{- From SentClauPhraList, get similarity degree bewteen any two syntatic types (namely categories).
 -}
getTypePair2SimFromSCPL :: SentClauPhraList -> (NumOfPhraSyn, NumOfCate, NumOfCatePair, [((Category, Category), SimDeg)])
getTypePair2SimFromSCPL sentClauPhraList = getTypePair2SimFromPSL phraSynList
    where
    phraSynList = getPhraSynFromSents sentClauPhraList []

{- From PhraSynList, get similarity degree bewteen ALL two syntatic types (namely categories).
 - Similarity degree between two categories satisfies commutative law, sim (cate1, cate2) == sim (cate2, cate1),
 - so only sim (cate1, cate2) where cate1 <= cate2 is calculated.
 - numOfPhraSyn: Number of different PhraSyns, namely different instances of (Category, Tag, PhraStru).
 - numOfCate: Number of different categories.
 - numOfCatePair: Number of different category pairs.
 -}
getTypePair2SimFromPSL :: [PhraSyn] -> (NumOfPhraSyn, NumOfCate, NumOfCatePair, [((Category, Category), SimDeg)])
getTypePair2SimFromPSL phraSynList = (numOfPhraSyn, numOfCate, numOfCatePair, typePair2SimList)
    where
    type2TagStruMap = toType2TagStruMap phraSynList Map.empty                   -- Map Category [(Tag, PhraStru)]
    type2TagStruMapList = Map.toList type2TagStruMap                            -- [(Category, [(Tag, PhraStru)])]
    type2TagStruSetList = map (\x -> (fst x, (Set.fromList . snd) x)) type2TagStruMapList       -- [(Category, Set (Tag, PhraStru))]
    typePair2SimList = [((fst x, fst y), (fromIntegral . Set.size) (Set.intersection (snd x) (snd y))
                                       / (fromIntegral . Set.size) (Set.union (snd x) (snd y)))
                                       | x <- type2TagStruSetList, y <- type2TagStruSetList, fst x <= fst y]
    numOfPhraSyn = length phraSynList
    numOfCate = Map.size type2TagStruMap
    numOfCatePair = length typePair2SimList

{- Get all different triple (syntactic type, CCG rule tag, Phrasal structure), namely PhraSyn values.
 - The input is the list of sentential categories.
 -}
getPhraSynFromSents :: [[[PhraCate]]] -> [PhraSyn] -> [PhraSyn]
getPhraSynFromSents [] tts = tts                              -- No sentence
getPhraSynFromSents [[]] tts = tts                            -- One sentence has no clause to deal with.
getPhraSynFromSents [(c:cs)] tts = getPhraSynFromSents [cs] (insertPhraList2Tts c tts)      -- 'c' is a clause, a list of phrasal categories.
getPhraSynFromSents (sent:sents) tts = union sentTts sentsTts      -- "sent" means a sentence. Union result does not include repetitive element.
    where
    sentTts = getPhraSynFromSents [sent] tts
    sentsTts = getPhraSynFromSents sents tts

{- Insert a series of PhraSyn values into a List.
 -}
insertPhraList2Tts :: [PhraCate] -> [PhraSyn] -> [PhraSyn]
insertPhraList2Tts [] tts = tts
insertPhraList2Tts [x] tts
    | ruleTag == "Desig" = tts        -- Neglecting words
    | elem (syntaxType, ruleTag, phraStru) tts = tts     -- Same triples are omited.
    | otherwise = (syntaxType, ruleTag, phraStru) : tts
    where
    syntaxType = ((!!0) . caOfCate) x
                          -- Apply 'caOfCate' to every phrasal category, and take the first element from the above result.
    ruleTag = ((!!0) . taOfCate) x
                          -- Apply 'taOfCate' to every phrasal category, and take the first element from the above result.
    phraStru = ((!!0) . psOfCate) x
                          -- Apply 'psOfCate' to every phrasal category, and take the first element from the above result.
insertPhraList2Tts (x:xs) tts = insertPhraList2Tts xs (insertPhraList2Tts [x] tts)

{- The string format is "type_tag_stru", type is category, tag is grammatic rule tag, and stru is phrasal structure.
 -}
stringToPhraSyn :: String -> PhraSyn
stringToPhraSyn tts = (getCateFromString (tts'!!0), tts'!!1, tts'!!2)
    where
    tts' = splitAtDeli '_' tts

{- From SentClauPhraList, get similarity degree bewteen any two grammtic rules (namely tags).
 -}
getTagPair2SimFromSCPL :: SentClauPhraList -> (NumOfPhraSyn, NumOfTag, NumOfTagPair, [((Tag, Tag), SimDeg)])
getTagPair2SimFromSCPL sentClauPhraList = getTagPair2SimFromPSL phraSynList
    where
    phraSynList = getPhraSynFromSents sentClauPhraList []                             -- [PhraSyn]

{- From [PhraSyn], get similarity degree bewteen any two grammtic rules (namely tags).
 - Similarity degree between two grammatic rules satisfies commutative law, sim (tag1, tag2) == sim (tag2, tag1),
 - so only sim (tag1, tag2) where tag1 <= tag2 is calculated.
 - numOfPhraSyn: Number of different PhraSyns, namely (Category, Tag, PhraStru).
 - numOfTag: Number of different grammatic rules.
 - numOfTagPair: Number of different rule pairs.
 -}
getTagPair2SimFromPSL :: [PhraSyn] -> (NumOfPhraSyn, NumOfTag, NumOfTagPair, [((Tag, Tag), SimDeg)])
getTagPair2SimFromPSL phraSynList = (numOfPhraSyn, numOfTag, numOfTagPair, tagPair2SimList)
    where
    tag2TypeStruMap = toTag2TypeStruMap phraSynList Map.empty                   -- Map Tag [(Category, PhraStru)]
    tag2TypeStruMapList = Map.toList tag2TypeStruMap                            -- [(Tag, [(Category, PhraStru)])]
    tag2TypeStruSetList = map (\x -> (fst x, (Set.fromList . snd) x)) tag2TypeStruMapList       -- [(Tag, Set (Category, PhraStru))]
    tagPair2SimList = [((fst x, fst y), (fromIntegral . Set.size) (Set.intersection (snd x) (snd y)) / (fromIntegral . Set.size) (Set.union (snd x) (snd y))) | x<-tag2TypeStruSetList, y<-tag2TypeStruSetList, fst x <= fst y]
    numOfPhraSyn = length phraSynList
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

{- From SentClauPhraList, get similarity degree bewteen any two phrasal structures.
 -}
getStruPair2SimFromSCPL :: SentClauPhraList -> (NumOfPhraSyn, NumOfPhraStru, NumOfStruPair, [((PhraStru, PhraStru), SimDeg)])
getStruPair2SimFromSCPL sentClauPhraList = getStruPair2SimFromPSL phraSynList
    where
    phraSynList = getPhraSynFromSents sentClauPhraList []                             -- [PhraSyn]

{- From [PhraSyn], get similarity degree bewteen any two phrasal structures.
 - Similarity degree between two phrasal structures satisfies commutative law, sim (stru1, stru2) == sim (stru2, stru1),
 - so only sim (stru1, stru2) where stru1 <= stru2 is calculated.
 - numOfPhraSyn: Number of different PhraSyns, namely (Category, Tag, PhraStru).
 - numOfPhraStru: Number of different phrasal structures.
 - numOfStruPair: Number of different phrase structure pairs.
 -}
getStruPair2SimFromPSL :: [PhraSyn] -> (NumOfPhraSyn, NumOfPhraStru, NumOfStruPair, [((PhraStru, PhraStru), SimDeg)])
getStruPair2SimFromPSL phraSynList = (numOfPhraSyn, numOfPhraStru, numOfStruPair, struPair2SimList)
    where
    stru2TypeTagMap = toStru2TypeTagMap phraSynList Map.empty                   -- Map PhraStru [(Category, Tag)]
    stru2TypeTagMapList = Map.toList stru2TypeTagMap                            -- [(PhraStru, [(Category, Tag)])]
    stru2TypeTagSetList = map (\x -> (fst x, (Set.fromList . snd) x)) stru2TypeTagMapList       -- [(PhraStru, Set (Category, Tag))]
    struPair2SimList = [((fst x, fst y), (fromIntegral . Set.size) (Set.intersection (snd x) (snd y)) / (fromIntegral . Set.size) (Set.union (snd x) (snd y))) | x<-stru2TypeTagSetList, y<-stru2TypeTagSetList, fst x <= fst y]
    numOfPhraSyn = length phraSynList
    numOfPhraStru = Map.size stru2TypeTagMap
    numOfStruPair = length struPair2SimList

{- From SentClauPhraList, get similarity degree bewteen any phrases in their PhraSyn = (Category, Tag, PhraStru).
 - Singular Value Decomposition (SVD) is used in calculating similarity degrees between PhraSyn pairs.
 -}
getPhraSynPairSimFromSCPLBySVD :: SentClauPhraList -> ([(PhraSyn, PhraSyn)], Matrix Double, Matrix Double, Matrix Double, [((PhraSyn, PhraSyn), SimDeg)])
getPhraSynPairSimFromSCPLBySVD sentClauPhraList = getPhraSynPairSimBySVD phraSynPairs
    where
    phraSynList = getPhraSynFromSents sentClauPhraList []                       -- [PhraSyn]
    phraSynPairs = [(x, y) | x <- phraSynList, y <- phraSynList, x <= y]        -- [(PhraSyn, PhraSyn)]

{- From SentClauPhraList, get similarity degree bewteen any phrases in their PhraSyn = (Category, Tag, PhraStru).
 - No Singular Value Decomposition (SVD) is used in calculating similarity degrees between PhraSyn pairs, instead
 - Root Mean Square is directly used.
 -}
getPhraSynPairSimFromSCPLByRMS :: SentClauPhraList -> ([(PhraSyn, PhraSyn)], Matrix Double, [((PhraSyn, PhraSyn), SimDeg)])
getPhraSynPairSimFromSCPLByRMS sentClauPhraList = getPhraSynPairSimByRMS phraSynPairs
    where
    phraSynList = getPhraSynFromSents sentClauPhraList []                       -- [PhraSyn]
    phraSynPairs = [(x, y) | x <- phraSynList, y <- phraSynList, x <= y]        -- [(PhraSyn, PhraSyn)]

{- From [(PhraSyn, PhraSyn)], get similarity degree bewteen any phrases in their PhraSyn = (Category, Tag, PhraStru).
 - sim(phraSyn1, phraSyn2) = f(ccSim, ttSim, ssSim), where ccSim is similarity degree between categories of these two phrases,
 - ttSim is similarity degree between grammatic rule tags of these two phrases, and ssSim is similarity degree between phrase-inner structures.
 - Category, grammatic rule, and phrase-inner structure are not independent with each other, so matrix [[ccSim, ttSim, ssSim]] is converted into
 - matrix [[ccSim', ttSim', ssSim']] acoording to SVD (Singular Value Decomposition) such that ccSim', ttSim' and ssSim' are orthogonal columns.
 - Thus, sim(phraSyn1, phraSyn2) = f(ccSim, ttSim, ssSim) = f'(ccSim', ttSim', ssSim'), where f' is Euclid length of vector (ccSim', ttSim', ssSim').
 -
 - origSimList: List of (ccSim, ttSim, ssSim), in which every triple (ccSim, ttSim, ssSim) is corresponding to one tuple (phraSyn1, phraSyn2) in phraSynPairList.
 - origSimMatrix:  (k >< 3) [ccSim1, ttSim1, ssSim1, ..., ccSimk, ttSimk, ssSimk], which is matrix form of origSimList.
 - centredSimMatrix: (k >< 3) [ccSim1/ccSimMean, ttSim1/ttSimMean, ssSim1/ssSimMean, ...], which is centred similarity degree matrix.
 - covSimMatrix: (1/k)(origSimMatrix <> origSimMatrixT), where origSimMatrixT is the transpose of origSimMatrix.
 - orthSimMatrix: (tr' u) LA.<> centredSimMatrix, where u is the left singular matrix.
 -}
getPhraSynPairSimBySVD :: [(PhraSyn, PhraSyn)] -> ([(PhraSyn, PhraSyn)], Matrix Double, Matrix Double, Matrix Double, [((PhraSyn, PhraSyn), SimDeg)])
getPhraSynPairSimBySVD phraSynPairs = (phraSynPairs, origSimMatrix, covSimMatrix, orthSimMatrix, phraSynPair2SimList)
    where
    phraSynList = nub $ (\x -> fst x ++ snd x) $ unzip phraSynPairs         -- [PhraSyn], used to calculate similarity for every grammatic attribute.
    typePair2SimTuple = getTypePair2SimFromPSL phraSynList                  -- (NumOfPhraSyn, NumOfCate, NumOfCatePair, [((Category, Category), SimDeg)])
    typePair2Sim = fth4 typePair2SimTuple
    tagPair2SimTuple = getTagPair2SimFromPSL phraSynList                    -- (NumOfPhraSyn, NumOfTag, NumOfTagPair, [((Tag, Tag), SimDeg)])
    tagPair2Sim = fth4 tagPair2SimTuple
    struPair2SimTuple = getStruPair2SimFromPSL phraSynList                  -- (NumOfPhraSyn, NumOfPhraStru, NumOfStruPair, [((PhraStru, PhraStru), SimDeg)])
    struPair2Sim = fth4 struPair2SimTuple

    numOfPhraSynPair = length phraSynPairs
    origSimList = [(getSimDegFromAttPair2Sim (fst3 x) (fst3 y) typePair2Sim
                  , getSimDegFromAttPair2Sim (snd3 x) (snd3 y) tagPair2Sim
                  , getSimDegFromAttPair2Sim (thd3 x) (thd3 y) struPair2Sim) | (x, y) <- phraSynPairs]
    origSimMatrix = (numOfPhraSynPair >< 3) $ concat [[fst3 e, snd3 e, thd3 e] | e <- origSimList]    -- hmatrix

    origSimMatrixT = tr' origSimMatrix                     -- Transpose
    covSimMatrix = cmap (/ ((fromIntegral numOfPhraSynPair) :: Double)) (origSimMatrix LA.<> origSimMatrixT)  -- Not used

{- Get left singular value matrix 'u', by (u,s,v) = svd origSimMatrix
 - Then, convert origSimMatrix to another similarity matrix in which column dimensions are orthogonal, orthSimMatrix = (tr’ u) <> origSimMatrix

    (u, s, v) = svd origSimMatrix
    orthSimMatrix = (tr' u) LA.<> origSimMatrix
 - This transformations is wrong, and u should be replaced with v as follows.
 -}

    (u, s, v) = svd origSimMatrix
    orthSimMatrix = origSimMatrix LA.<> v

{- Norm of every row vector in matrix orthSimMatrix is Euclid disdance from original point to point (ccSim', ttSim', ssSim').
 -}
    phraSynPairSimList = map (sqrt . (/ 3.0) . sumElements) (toRows (cmap (\x -> x*x) orthSimMatrix))  -- [Euclid distance of row vector], Mean Square Error.
    phraSynPair2SimList = zip phraSynPairs phraSynPairSimList     -- [((PhraSyn, PhraSyn), SimDeg)]

{- From [(PhraSyn, PhraSyn)], get similarity degree bewteen any phrases in their PhraSyn = (Category, Tag, PhraStru).
 - sim(phraSyn1, phraSyn2) = f(ccSim, ttSim, ssSim), where ccSim is similarity degree between categories of these two phrases,
 - ttSim is similarity degree between grammatic rule tags of these two phrases, and ssSim is similarity degree between phrase-inner structures.
 - Category, grammatic rule, and phrase-inner structure are not independent with each other, but experiments show,
 - the columns of matrix [[ccSim, ttSim, ssSim]] are orthogonal, the possible reason of which is these similarity are defined by mutual interpretion.
 - So, f(ccSim, ttSim, ssSim) is Root Mean Square of ccSim, ttSim and ssSim.
 - origSimList: List of (ccSim, ttSim, ssSim), in which every triple (ccSim, ttSim, ssSim) is corresponding to one tuple (phraSyn1, phraSyn2) in phraSynPairList.
 - origSimMatrix:  (k >< 3) [ccSim1, ttSim1, ssSim1, ..., ccSimk, ttSimk, ssSimk], which is matrix form of origSimList.
 -}
getPhraSynPairSimByRMS :: [(PhraSyn, PhraSyn)] -> ([(PhraSyn, PhraSyn)], Matrix Double, [((PhraSyn, PhraSyn), SimDeg)])
getPhraSynPairSimByRMS phraSynPairs = (phraSynPairs, origSimMatrix, phraSynPair2SimList)
    where
    phraSynList = nub $ (\x -> fst x ++ snd x) $ unzip phraSynPairs         -- [PhraSyn], used to calculate similarity for every grammatic attribute.
    typePair2SimTuple = getTypePair2SimFromPSL phraSynList                  -- (NumOfPhraSyn, NumOfCate, NumOfCatePair, [((Category, Category), SimDeg)])
    typePair2Sim = fth4 typePair2SimTuple
    tagPair2SimTuple = getTagPair2SimFromPSL phraSynList                    -- (NumOfPhraSyn, NumOfTag, NumOfTagPair, [((Tag, Tag), SimDeg)])
    tagPair2Sim = fth4 tagPair2SimTuple
    struPair2SimTuple = getStruPair2SimFromPSL phraSynList                  -- (NumOfPhraSyn, NumOfPhraStru, NumOfStruPair, [((PhraStru, PhraStru), SimDeg)])
    struPair2Sim = fth4 struPair2SimTuple

    numOfPhraSynPair = length phraSynPairs
    origSimList = [(getSimDegFromAttPair2Sim (fst3 x) (fst3 y) typePair2Sim
                  , getSimDegFromAttPair2Sim (snd3 x) (snd3 y) tagPair2Sim
                  , getSimDegFromAttPair2Sim (thd3 x) (thd3 y) struPair2Sim) | (x, y) <- phraSynPairs]
    origSimMatrix = (numOfPhraSynPair >< 3) $ concat [[fst3 e, snd3 e, thd3 e] | e <- origSimList]    -- hmatrix

{- Norm of every row vector in matrix orthSimMatrix is Euclid disdance from original point to point (ccSim', ttSim', ssSim').
 -}
    phraSynPairSimList = map (sqrt . (/ 3.0) . sumElements) (toRows (cmap (\x -> x*x) origSimMatrix))  -- Root Mean Square
    phraSynPair2SimList = zip phraSynPairs phraSynPairSimList     -- [((PhraSyn, PhraSyn), SimDeg)]

-- Get PhraSyn value pair from a similarity degree vector.
getPhraSynPairFromSimDegVector :: (((Category, Category), SimDeg), ((Tag, Tag), SimDeg), ((PhraStru, PhraStru), SimDeg)) -> (PhraSyn, PhraSyn)
getPhraSynPairFromSimDegVector (ccSim, ttSim, ssSim) = (phraSyn1, phraSyn2)
    where
    phraSyn1 = ((fst . fst) ccSim, (fst . fst) ttSim, (fst . fst) ssSim)
    phraSyn2 = ((snd . fst) ccSim, (snd . fst) ttSim, (snd . fst) ssSim)

-- Given two attribute values, get their similarity degree from attribute Similarity degree list.
getSimDegFromAttPair2Sim :: Eq a => a -> a -> [((a, a), SimDeg)] -> SimDeg
getSimDegFromAttPair2Sim _ _ [] = error "getSimDegFromAttPair2Sim: error"
getSimDegFromAttPair2Sim a1 a2 (x:xs)
    | (a1 == (fst . fst) x && a2 == (snd . fst) x) || (a1 == (snd . fst) x && a2 == (fst . fst) x) = snd x       -- Commutative law in xx2Sim List.
    | otherwise = getSimDegFromAttPair2Sim a1 a2 xs

{- Reading parsing trees from start sentence to end sentence in treebank, get [[[PhraCate]]] from which
 - [PhraSynPair2Sim] is returned, where PhraSynPair2Sim :: ((PhraSyn, PhraSyn), SimDeg).
 - If start index and end index are both 0, then default start index and default end index are used.
 - which are defined in file Configuration.
 - The treebank is the database table property 'tree_souce' indicates.
 -}
getPhraSynPair2SimFromTreebank :: SentIdx -> SentIdx -> IO (Map (PhraSyn, PhraSyn) SimDeg)
getPhraSynPair2SimFromTreebank startIdx endIdx = do
    sentClauPhraList <- getSentClauPhraList startIdx endIdx
    let phraSynPairSimTuple = getPhraSynPairSimFromSCPLByRMS sentClauPhraList
    return $ Map.fromList (thd3 phraSynPairSimTuple)

{- Extract all pairs of PhraSyn values from [ContextOfOT], who can appear in calculating their grammatic similarities.
 - From [ContextOfOT], get [LeftExtend], [LeftOver], [RightOver] and [RightExtend].
 - Only coming from same part of overtype contexts, such as LeftExtend, two PhraSyn values has similarity calculation.
 - Form PhraSyn pair sets respectively for different grammtic parts, where first PhraSyn value is less than or equal to the second.
 - For every pair, calculate their similarity. Finally return Map (PhraSyn, PhraSyn) SimDeg.
 -}
getPhraSynPair2SimFromCOT :: [ContextOfOT] -> Map (PhraSyn, PhraSyn) SimDeg
getPhraSynPair2SimFromCOT contextOfOTList = Map.fromList (thd3 phraSynPairSimTuple)
    where
    (les, los, ros, res) = unzip4 contextOfOTList                               -- Extract [LeftExtend], [LeftOver], [RightOver] and [RightExtend].
    leps = [(x, y) | x <- les, y <- les, x <= y]                                -- [(LeftExtend, LeftExtend)]
    lops = [(x, y) | x <- los, y <- los, x <= y]                                -- [(LeftOver, LeftOver)]
    rops = [(x, y) | x <- ros, y <- ros, x <= y]                                -- [(RightOver, RightOVer)]
    reps = [(x, y) | x <- res, y <- res, x <= y]                                -- [(RightExtend, RightExtend)]
    lePhraSynPairs = nub $ [(u, v) | lep <- leps, let le1 = fst lep, let le2 = snd lep, u <- le1, v <- le2]   -- [(PhraSyn, PhraSyn)]
    loPhraSynPairs = nub $ lops
    roPhraSynPairs = nub $ rops
    rePhraSynPairs = nub $ [(u, v) | rep <- reps, let re1 = fst rep, let re2 = snd rep, u <- re1, v <- re2]   -- [(PhraSyn, PhraSyn)]
    phraSynPairs = map (\(x,y) -> if (x<=y) then (x,y) else (y,x))
                       $ nubBy (\x y -> x == swap y) $ lePhraSynPairs ++ loPhraSynPairs ++ roPhraSynPairs ++ rePhraSynPairs
    phraSynPairSimTuple = getPhraSynPairSimByRMS phraSynPairs

{- Extract all pairs of PhraSyn values from [ContextOfSG], who can appear in calculating their grammatic similarities.
 - From [ContextOfSG], get [LeftExtend], [LeftOver], [RightOver] and [RightExtend].
 - Only coming from same part of contexts, such as LeftExtend, two PhraSyn values has similarity calculation.
 - Form PhraSyn pair sets respectively for different grammtic parts, every pair in which satisfies first PhraSyn value is less than or equal to the second.
 - For every pair, calculate their similarity. Finally return Map (PhraSyn, PhraSyn) SimDeg.
 - This function have not been used yet.
 -}
getPhraSynPair2SimFromCSG :: [ContextOfSG] -> Map (PhraSyn, PhraSyn) SimDeg
getPhraSynPair2SimFromCSG contextOfSGList = Map.fromList (thd3 phraSynPairSimTuple)
    where
    (les, los, ros, res, _) = unzip5 contextOfSGList                            -- Extract [LeftExtend], [LeftOver], [RightOver] and [RightExtend].
    leps = [(x, y) | x <- les, y <- les, x <= y]                                -- [(LeftExtend, LeftExtend)]
    lops = [(x, y) | x <- los, y <- los, x <= y]                                -- [(LeftOver, LeftOver)]
    rops = [(x, y) | x <- ros, y <- ros, x <= y]                                -- [(RightOver, RightOVer)]
    reps = [(x, y) | x <- res, y <- res, x <= y]                                -- [(RightExtend, RightExtend)]
    lePhraSynPairs = nub $ [(u, v) | lep <- leps, let le1 = fst lep, let le2 = snd lep, u <- le1, v <- le2]   -- [(PhraSyn, PhraSyn)]
    loPhraSynPairs = nub $ lops
    roPhraSynPairs = nub $ rops
    rePhraSynPairs = nub $ [(u, v) | rep <- reps, let re1 = fst rep, let re2 = snd rep, u <- re1, v <- re2]   -- [(PhraSyn, PhraSyn)]
    phraSynPairs = map (\(x,y) -> if (x<=y) then (x,y) else (y,x))
                       $ nubBy (\x y -> x == swap y) $ lePhraSynPairs ++ loPhraSynPairs ++ roPhraSynPairs ++ rePhraSynPairs
    phraSynPairSimTuple = getPhraSynPairSimByRMS phraSynPairs

{- Reading parsing trees from start sentence to end sentence in treebank, get [[[PhraCate]]].
 - If start index and end index are both 0, then default start index and default end index are used, which are defined in file Configuration.
 - The treebank is the database table property 'tree_souce' indicates.
 -}
getSentClauPhraList :: SentIdx -> SentIdx -> IO SentClauPhraList
getSentClauPhraList startIdx endIdx = do
    conn <- getConn
    confInfo <- readFile "Configuration"                                        -- Read the local configuration file
    let tree_source = getConfProperty "tree_source" confInfo
    let startIdx' = case (startIdx == 0) of
                      True -> read (getConfProperty "defaultStartIdx" confInfo) :: Int
                      False -> startIdx
    let endIdx' = case (endIdx == 0) of
                    True -> read (getConfProperty "defaultEndIdx" confInfo) :: Int
                    False -> endIdx
    putStrLn $ "stardIdx = " ++ show startIdx' ++ " , endIdx = " ++ show endIdx'
    putStrLn $ "The source of parsing trees is set as: " ++ tree_source         -- Display the source of parsing trees
    let sqlstat = DS.fromString $ "select tree from " ++ tree_source ++ " where serial_num >= ? and serial_num <= ?"
    stmt <- prepareStmt conn sqlstat
    (defs, is) <- queryStmt conn stmt [toMySQLInt32 startIdx', toMySQLInt32 endIdx']

    sentStrList <- readStreamByText [] is                     -- [String], here a string is the parsing result of a sentence.
    let sentClauStrList = map stringToList sentStrList        -- [[String]], here a string is the parsing result of a clause.
    let sentClauNumList = map length sentClauStrList          -- [Int], here an integer is the number of clauses in a sentence.
    let sentNum = length sentStrList                          -- The number of sentences.
    let clauseTotalNum = foldl (+) 0 sentClauNumList          -- The total number of clauses.

    let sentClauTreeList = map (map readTree) sentClauStrList     -- [[Tree]], here Tree ::= (ClauIdx, [PhraCate])
    let sentClauPhraList = map (map snd) sentClauTreeList     -- [[[PhraCate]]], here a PhraCate is the representation in memory of a phrase.
    return sentClauPhraList

{- Get similarity degree between two phrasal sets in their grammatical features.
 - (1) Suppose set A and B are [PhraSyn]. Get upper triangular matrix of similarity degrees, where sim(a,b) is similarity of (a,b).
 -     M = [((a,b), sim(a,b)) | (a,b) in A X B, a <= b]
 - (2) Match elements between A and B according to maximum similarity degree, forming list L.
 - (3) If |A| < |B|. For every element b in B, which does not appear in L, append ((nullPhraSyn, b), 0) to L.
 - (4) If |A| > |B|. For every element a in A, which does not appear in L, append ((a, nullPhraSyn), 0) to L.
 - Finally, return mean value of similarity degrees of all matched PhraSyn value pairs.
 - In Map (PhraSyn, PhraSyn) SimDeg, every key (x,y) satisfies order of x <= y.
 -}
getPhraSynSetSim :: [PhraSyn] -> [PhraSyn] -> Map (PhraSyn, PhraSyn) SimDeg -> SimDeg
getPhraSynSetSim [] [] _ = 1.0           -- Two empty sets
getPhraSynSetSim _ [] _ = 0.0
getPhraSynSetSim [] _ _ = 0.0
getPhraSynSetSim psl1 psl2 phraSynPair2SimMap = simDeg
    where
    phraSynPairMatrix = [(a,b) | a <- psl1, b <- psl2]                  -- Matrix element must be number value type. Name matrix is borrowed.
    phraSynPair2SimMatrix = map (toPhraSynPair2Sim phraSynPair2SimMap) phraSynPairMatrix              -- [((PhraSyn, PhraSyn), SimDeg)]
    matchedPhraSynPair2SimList = getMatchedElemPair2SimList [] phraSynPair2SimMatrix                  -- [((PhraSyn, PhraSyn), SimDeg)]
    allMatchedPhraSynPair2SimList = case (signum (length psl1 - (length psl2))) of
      -1 -> matchedPhraSynPair2SimList ++ [((nullPhraSyn, x), 0.0) | x <- psl2, notElem x (map (snd . fst) matchedPhraSynPair2SimList)]
      0 -> matchedPhraSynPair2SimList
      1 -> matchedPhraSynPair2SimList ++ [((x, nullPhraSyn), 0.0) | x <- psl1, notElem x (map (fst . fst) matchedPhraSynPair2SimList)]
    simDeg = foldl (+) 0.0 (map snd allMatchedPhraSynPair2SimList) / (fromIntegral (length allMatchedPhraSynPair2SimList))

{- Convert (PhraSyn, PhraSyn) to ((PhraSyn, PhraSyn), SimDeg) by looking Map (PhraSyn, PhraSyn) SimDeg.
 - For every key (x,y) in Map (PhraSyn, PhraSyn) SimDeg, x <= y.
 -}
toPhraSynPair2Sim :: Map (PhraSyn, PhraSyn) SimDeg -> (PhraSyn, PhraSyn) -> ((PhraSyn, PhraSyn), SimDeg)
toPhraSynPair2Sim phraSynPair2SimMap (ps1, ps2) = case (Map.lookup (ps1, ps2) phraSynPair2SimMap) of
      Just x -> ((ps1, ps2), x)
      Nothing -> case (Map.lookup (ps2, ps1) phraSynPair2SimMap) of
                   Just x -> ((ps1, ps2), x)
                   Nothing -> error $ "toPhraSynPair2Sim: Key (" ++ show ps1 ++ ", " ++ show ps2 ++ ") does not exist."

-- Types for calculating similarity degree of phrasal overlapping types.
type NumOfContextOfOT = Int       -- ContextOfOT :: (LeftExtend, LeftOver, RightOver, RightExtend)
type NumOfLeftExtend = Int
type NumOfLeftOver = Int
type NumOfRightOver = Int
type NumOfRightExtend = Int
type NumOfLeftExtendPair = Int
type NumOfLeftOverPair = Int
type NumOfRightOverPair = Int
type NumOfRightExtendPair = Int
type ContextOfOTPair2Sim = ((ContextOfOT, ContextOfOT), SimDeg)

{- Reading overtype and its context from the sample database of a certain ambi_resol_model, such as 'stru_gene_202408'.
 - If start index and end index are both 0, then all records in sample database will be read.
 - Context2OverTypeBase :: [Context2OverType]
 - Context2OverType :: (ContextOfOT, OverType)
 - ContextOfOT :: (LeftExtend, LeftOver, RightOver, RightExtend)
 -}
getContext2OverTypeBase :: SIdx -> SIdx -> IO Context2OverTypeBase
getContext2OverTypeBase startIdx endIdx = do
    conn <- getConn
    confInfo <- readFile "Configuration"                                        -- Read the local configuration file
    let ambi_resol_model = getConfProperty "ambi_resol_model" confInfo
    case ambi_resol_model of
      "stru_gene_202408" -> do
          let startIdx' = case startIdx of
                            0 -> 1
                            _ -> startIdx
          let sqlstat = DS.fromString $ "select count(*) from " ++ ambi_resol_model
          (defs, is) <- query_ conn sqlstat
          rows <- readStreamByInt64 [] is
          let endIdx' = case endIdx of
                          0 -> head rows
                          _ -> endIdx

          putStrLn $ "stardIdx = " ++ show startIdx' ++ " , endIdx = " ++ show endIdx'

          putStrLn $ "The source of overtypes and their contexts is set as: " ++ ambi_resol_model     -- Display the source of overtypes and their contexts
          let sqlstat = DS.fromString $ "select leftExtend, leftOver, rightOver, rightExtend, overType from " ++ ambi_resol_model ++ " where id >= ? and id <= ?"
          stmt <- prepareStmt conn sqlstat
          (defs, is) <- queryStmt conn stmt [toMySQLInt32U startIdx', toMySQLInt32U endIdx']

          context2OverTypeBase <- readStreamByContext2OverType [] is            -- [Context2OverType]
          let context2OverTypeNum = length context2OverTypeBase                 -- The number of Context2OverType samples.
          putStrLn $ "getContext2OverTypeBase: context2OverTypeNum = " ++ show context2OverTypeNum
          return context2OverTypeBase

{- Get similarity degree between two overtype contexts, namely two vectors of (LeftExtend, LeftOver, RightOver, RightExtend).
 - sim(contextOfOT1, contextOfOT2) = f(leSim, loSim, roSim, reSim), where
 -     leSim is similarity degree between two LeftExtends, namely two PhraSyn sets.
 -     loSim is similarity degree between two LeftOvers, namely two PhraSyns.
 -     roSim is similarity degree between two RightOvers, namely two PhraSyns.
 -     reSim is similarity degree between two RightExtends, namely two PhraSyn sets.
 - LeftExtend, LeftOver, RightOver, and RightExtend, are not independent with each other, so matrix [[leSim, loSim, roSim, reSim]] is converted into
 - matrix [[leSim', loSim', roSim', reSim']] acoording to SVD (Singular Value Decomposition) such that leSim', loSim', roSim' and reSim'' are orthogonal columns.
 - Thus, sim(contextOfOT1, contextOfOT2) = f(leSim, loSim, roSim, reSim) = f'(leSim', loSim', roSim', reSim'), where f' is Euclid length of vector (leSim', loSim', roSim', reSim').
 -
 - contextOfOTPairList: List of (ContextOfOT, ContextOfOT), in every element of which first ContextOfOT value is less than or equal to the second.
 - origSimList: List of (leSim, loSim, roSim, reSim), in which every quadtuple (leSim, loSim, roSim, reSim) is corresponding to one tuple (contextOfOT1, contextOfOT2) in contextOfOTPairList.
 - origSimMatrix:  (k >< 4) [leSim1, loSim1, roSim1, reSim1, ..., leSimk, loSimk, roSimk, reSimk], which is matrix form of origSimList.
 - orthSimMatrix: origSimMatrix LA.<> v, where v is the right singular matrix.
 -}
getContextOfOTPairSimBySVD :: Context2OverTypeBase -> IO ([(ContextOfOT, ContextOfOT)], Matrix Double, Matrix Double, [((ContextOfOT, ContextOfOT), SimDeg)])
getContextOfOTPairSimBySVD context2OverTypeBase = do
    let contextOfOTList = nub $ map fst context2OverTypeBase                    -- [ContextOfOT], here there is no repetitive ContextOfOT values.
    putStrLn $ "Num. of repetitive ContextOfOT values = " ++ show (length context2OverTypeBase - (length contextOfOTList))

    let phraSynPair2SimMap = getPhraSynPair2SimFromCOT contextOfOTList          -- Map (PhraSyn, PhraSyn) SimDeg
    let leftExtendList = map fst4 contextOfOTList                                      -- [LeftExtend]
    let lePair2SimList = getPhraSynSetPair2Sim leftExtendList phraSynPair2SimMap       -- [((LeftExtend, LeftExtend), SimDeg)]
    let leftOverList = map snd4 contextOfOTList                                        -- [LeftOver]
    let loPair2SimList = getPhraSynPair2Sim leftOverList phraSynPair2SimMap            -- [((LeftOver, LeftOver), SimDeg)]
    let rightOverList = map thd4 contextOfOTList                                       -- [RightOver]
    let roPair2SimList = getPhraSynPair2Sim rightOverList phraSynPair2SimMap           -- [((RightOver, RightOver), SimDeg)]
    let rightExtendList = map fth4 contextOfOTList                                     -- [RightExtend]
    let rePair2SimList = getPhraSynSetPair2Sim rightExtendList phraSynPair2SimMap      -- [((RightExtend, RightExtend), SimDeg)]

    let contextOfOTPairList = [(x, y) | x <- contextOfOTList, y <- contextOfOTList, x <= y]   -- [(ContextOfOT, ContextOfOT)]
                                                      -- Ord definition is exhaustive, here there is no repetitive pairs.
    let numOfContextOfOTPair = length contextOfOTPairList
    let origSimList = [(getSimDegFromAttPair2Sim (fst4 x) (fst4 y) lePair2SimList
                      , getSimDegFromAttPair2Sim (snd4 x) (snd4 y) loPair2SimList
                      , getSimDegFromAttPair2Sim (thd4 x) (thd4 y) roPair2SimList
                      , getSimDegFromAttPair2Sim (fth4 x) (fth4 y) rePair2SimList) | (x, y) <- contextOfOTPairList]

    let origSimMatrix = (numOfContextOfOTPair >< 4) $ concat [[fst4 e, snd4 e, thd4 e, fth4 e] | e <- origSimList]       -- hmatrix

    let (u, s, v) = svd origSimMatrix
    let orthSimMatrix = origSimMatrix LA.<> v

-- Norm of every row vector in matrix orthSimMatrix is Euclid disdance from original point to point (leSim', loSim', roSim', reSim').
    let contextOfOTPairSimList = map (sqrt . (/ 4.0) . sumElements) (toRows (cmap (\x -> x*x) orthSimMatrix))   -- [Euclid distance of row vector], Mean Square Error.
    let contextOfOTPair2SimList = zip contextOfOTPairList contextOfOTPairSimList     -- [((ContextOfOT, ContextOfOT), SimDeg)]

    return (contextOfOTPairList, origSimMatrix, orthSimMatrix, contextOfOTPair2SimList)

{- Get similarity degree between two overtype contexts, namely two vectors of (LeftExtend, LeftOver, RightOver, RightExtend).
 - sim(contextOfOT1, contextOfOT2) = f(leSim, loSim, roSim, reSim), where
 -     leSim is similarity degree between two LeftExtends, namely two PhraSyn sets.
 -     loSim is similarity degree between two LeftOvers, namely two PhraSyns.
 -     roSim is similarity degree between two RightOvers, namely two PhraSyns.
 -     reSim is similarity degree between two RightExtends, namely two PhraSyn sets.
 -     f is Root Mean Square (RMS) metric in four-dimensional space.
 - contextOfOTPairList: List of (ContextOfOT, ContextOfOT), in every element of which first ContextOfOT value is less than or equal to the second.
 - origSimList: List of (leSim, loSim, roSim, reSim), in which every quadtuple (leSim, loSim, roSim, reSim) is corresponding to one tuple (contextOfOT1, contextOfOT2) in contextOfOTPairList.
 -}
getContextOfOTPairSimByRMS :: Context2OverTypeBase -> IO ([(SimDeg, SimDeg, SimDeg, SimDeg)], [((ContextOfOT, ContextOfOT), SimDeg)])
getContextOfOTPairSimByRMS context2OverTypeBase = do
    let contextOfOTList = nub $ map fst context2OverTypeBase                    -- [ContextOfOT], here there is no repetitive ContextOfOT values.
    putStrLn $ "Num. of repetitive ContextOfOT values = " ++ show (length context2OverTypeBase - (length contextOfOTList))

    let phraSynPair2SimMap = getPhraSynPair2SimFromCOT contextOfOTList          -- Map (PhraSyn, PhraSyn) SimDeg
    let leftExtendList = map fst4 contextOfOTList                               -- [LeftExtend]
    let lePair2SimList = getPhraSynSetPair2Sim leftExtendList phraSynPair2SimMap      -- [((LeftExtend, LeftExtend), SimDeg)]
    let leftOverList = map snd4 contextOfOTList                                       -- [LeftOver]
    let loPair2SimList = getPhraSynPair2Sim leftOverList phraSynPair2SimMap           -- [((LeftOver, LeftOver), SimDeg)]
    let rightOverList = map thd4 contextOfOTList                                      -- [RightOVer]
    let roPair2SimList = getPhraSynPair2Sim rightOverList phraSynPair2SimMap          -- [((RightOver, RightOver), SimDeg)]
    let rightExtendList = map fth4 contextOfOTList                                    -- [RightExtend]
    let rePair2SimList = getPhraSynSetPair2Sim rightExtendList phraSynPair2SimMap     -- [((RightExtend, RightExtend), SimDeg)]

    let contextOfOTPairList = [(x, y) | x <- contextOfOTList, y <- contextOfOTList, x <= y]      -- [(ContextOfOT, ContextOfOT)]
                                                                -- Ord definition is exhaustive, here there is no repetitive pairs.
    let origSimList = [(getSimDegFromAttPair2Sim (fst4 x) (fst4 y) lePair2SimList
                      , getSimDegFromAttPair2Sim (snd4 x) (snd4 y) loPair2SimList
                      , getSimDegFromAttPair2Sim (thd4 x) (thd4 y) roPair2SimList
                      , getSimDegFromAttPair2Sim (fth4 x) (fth4 y) rePair2SimList) | (x, y) <- contextOfOTPairList]

    let simList = [sqrt (sum [les * les, los * los, ros * ros, res * res] / 4.0) | (les,los,ros,res) <- origSimList]   -- Root Mean Square
    let contextOfOTPair2SimList = zip contextOfOTPairList simList     -- [((ContextOfOT, ContextOfOT), SimDeg)]
    return (origSimList, contextOfOTPair2SimList)

{- Get similarity degree bewteen any two PhraSyn sets, where PhraSyn set denotes LeftExtend and RightExtend.
 - Similarity degree between two PhraSyn sets satisfies commutative law, sim (s1, s2) == sim (s2, s1),
 - so only sim (s1, s2) where s1 <= s2 is calculated.
 -}
getPhraSynSetPair2Sim :: [[PhraSyn]] -> Map (PhraSyn, PhraSyn) SimDeg -> [(([PhraSyn], [PhraSyn]), SimDeg)]
getPhraSynSetPair2Sim phraSynSetList phraSynPair2SimMap = phraSynSetPair2SimList
    where
    phraSynSetPair2SimList = [((x, y), getPhraSynSetSim x y phraSynPair2SimMap) | x <- phraSynSetList, y <- phraSynSetList, x <= y]

{- Get similarity degree bewteen any two PhraSyn values, where PhraSyn denotes LeftOver and RightOver.
 - Similarity degree between two PhraSyn values satisfies commutative law, sim (ps1, ps2) == sim (ps2, ps1),
 - so only sim (ps1, ps2) where ps1 <= ps2 is calculated.
 -}
getPhraSynPair2Sim :: [PhraSyn] -> Map (PhraSyn, PhraSyn) SimDeg -> [((LeftOver, LeftOver), SimDeg)]
getPhraSynPair2Sim phraSynList phraSynPair2SimMap = phraSynPair2SimList
    where
    phraSynPair2SimList = [((x, y), case (Map.lookup (x, y) phraSynPair2SimMap) of
                                       Just x -> x
                                       Nothing -> 0.0
                           ) | x <- phraSynList, y <- phraSynList, x <= y]

{- Get similarity degree between two overtype contexts, namely two vectors of (LeftExtend, LeftOver, RightOver, RightExtend).
 -}
lookupContextOfOTPairSim :: [((ContextOfOT, ContextOfOT), SimDeg)] -> (ContextOfOT, ContextOfOT) -> ((ContextOfOT, ContextOfOT), SimDeg)
lookupContextOfOTPairSim [] (cot1, cot2) = ((cot1, cot2), 0.0)
lookupContextOfOTPairSim (((c1, c2), simDeg):cpsl) (cot1, cot2)
    | ((cot1, cot2) == (c1, c2)) || ((cot2, cot1) == (c1, c2)) = ((cot1, cot2), simDeg)
    | otherwise = lookupContextOfOTPairSim cpsl (cot1, cot2)

{- Get similarity degrees bewteen any two overlapping types.
 - startIdx: Index of start index of ambiguity model samples.
 - endIdx: Index of end index of ambiguity model samples.
 - overtype: 1 .. 5
 -}
getOverTypePair2Sim :: SIdx -> SIdx -> IO [((OverType, OverType), SimDeg)]
getOverTypePair2Sim startIdx endIdx = do
    context2OverTypeBase <- getContext2OverTypeBase startIdx endIdx             -- [(ContextOfOT, OverType)]
    let contexts4OT1 = [fst x | x <- context2OverTypeBase, snd x == 1]          -- [ContextOfOT]
    let contexts4OT2 = [fst x | x <- context2OverTypeBase, snd x == 2]          -- [ContextOfOT]
    let contexts4OT3 = [fst x | x <- context2OverTypeBase, snd x == 3]          -- [ContextOfOT]
    let contexts4OT4 = [fst x | x <- context2OverTypeBase, snd x == 4]          -- [ContextOfOT]
    let contexts4OT5 = [fst x | x <- context2OverTypeBase, snd x == 5]          -- [ContextOfOT]
    let overType2ContextList = [(1,contexts4OT1),(2,contexts4OT2),(3,contexts4OT3),(4,contexts4OT4),(5,contexts4OT5)]  -- [(OverType, [ContextOfOT])]
    putStrLn $ "getOverTypePair2Sim: Num. of samples per overType: " ++ show (map (\x -> (fst x, length (snd x))) overType2ContextList)
    contextOfOTPairSimTuple <- getContextOfOTPairSimByRMS context2OverTypeBase
    let contextOfOTPairSimList = snd contextOfOTPairSimTuple                    -- [((ContextOfOT, ContextOfOT), OverType)]
    let oos = [((fst o1, fst o2), getContextOfOTSetSim (snd o1) (snd o2) contextOfOTPairSimList)
                  | o1 <- overType2ContextList, o2 <- overType2ContextList, fst o1 <= fst o2]
    return oos

{- Get similarity degree between two ContextOfOT sets in their grammatical features.
 - (1) Suppose set A and B are [ContextOfOT]. Get upper triangular matrix of similarity degrees, where sim(a,b) is similarity of (a,b).
 -     M = [((a,b), sim(a,b)) | (a,b) in A X B, a <= b]
 - (2) Match elements between A and B according to maximum similarity degree, forming list L.
 - (3) If |A| < |B|. For every element b in B, which does not appear in L, append ((nullContextOfOT, b), 0) to L.
 - (4) If |A| > |B|. For every element a in A, which does not appear in L, append ((a, nullContextOfOT), 0) to L.
 - Finally, return mean value of similarity degrees of all matched PhraSyn value pairs.
 -}
getContextOfOTSetSim :: [ContextOfOT] -> [ContextOfOT] -> [((ContextOfOT, ContextOfOT), SimDeg)] -> SimDeg
getContextOfOTSetSim cotl1 cotl2 contextOfOTPair2SimList = simDeg
    where
    contextOfOTPairMatrix = nubBy (\x y -> x == swap y) [(a,b) | a <- cotl1, b <- cotl2]         -- Matrix element must be number value type. Name matrix is borrowed.
    contextOfOTPair2SimMatrix = map (lookupContextOfOTPairSim contextOfOTPair2SimList) contextOfOTPairMatrix      -- [((ContextOfOT, ContextOfOT), SimDeg)]
    matchedContextOfOTPair2SimList = getMatchedElemPair2SimList [] contextOfOTPair2SimMatrix         -- [((ContextOfOT, ContextOfOT), SimDeg)]
    allMatchedContextOfOTPair2SimList = case (signum (length cotl1 - (length cotl2))) of
      -1 -> matchedContextOfOTPair2SimList ++ [((nullContextOfOT, x), 0.0) | x <- cotl2, notElem x (map (snd . fst) matchedContextOfOTPair2SimList)]
      0 -> matchedContextOfOTPair2SimList
      1 -> matchedContextOfOTPair2SimList ++ [((x, nullContextOfOT), 0.0) | x <- cotl1, notElem x (map (fst . fst) matchedContextOfOTPair2SimList)]
    simDeg = foldl (+) 0.0 (map snd allMatchedContextOfOTPair2SimList) / (fromIntegral (length allMatchedContextOfOTPair2SimList))

-- Types for calculating similarity degree of syntactic ambiguity contexts.
type NumOfContextOfSG = Int       -- ContextOfSG :: (LeftExtend, LeftOver, RightOver, RightExtend, OverType)
type NumOfContextOfSGPair = Int
type ContextOfSGPair2Sim = ((ContextOfSG, ContextOfSG), SimDeg)

{- Reading Prior and its context from the sample database of a certain ambi_resol_model, such as 'stru_gene_202408'.
 - If start index and end index are both 0, then all records in sample database will be read.
 - Context2ClauTagPrior :: (ContextOfSG, Prior)
 - Context2ClauTagPriorBase :: [Context2ClauTagPrior]
 - ContextOfSG :: (LeftExtend, LeftOver, RightOver, RightExtend, OverType)
 -}
getContext2ClauTagPriorBase :: SIdx -> SIdx -> IO [Context2ClauTagPrior]
getContext2ClauTagPriorBase startIdx endIdx = do
    conn <- getConn
    confInfo <- readFile "Configuration"                                        -- Read the local configuration file
    let ambi_resol_model = getConfProperty "ambi_resol_model" confInfo
    case ambi_resol_model of
      "stru_gene_202408" -> do
          let startIdx' = case startIdx of
                            0 -> 1
                            _ -> startIdx
          let sqlstat = DS.fromString $ "select count(*) from " ++ ambi_resol_model
          (defs, is) <- query_ conn sqlstat
          rows <- readStreamByInt64 [] is
          let endIdx' = case endIdx of
                          0 -> head rows
                          _ -> endIdx

          putStrLn $ "stardIdx = " ++ show startIdx' ++ " , endIdx = " ++ show endIdx'

          putStrLn $ "The source of priors and their contexts is set as: " ++ ambi_resol_model     -- Display the source of priors and their contexts
          let sqlstat = DS.fromString $ "select leftExtend, leftOver, rightOver, rightExtend, overType, clauTagPrior from " ++ ambi_resol_model ++ " where id >= ? and id <= ?"
          stmt <- prepareStmt conn sqlstat
          (defs, is) <- queryStmt conn stmt [toMySQLInt32U startIdx', toMySQLInt32U endIdx']

          context2ClauTagPriorBase <- readStreamByContext2ClauTagPrior [] is                  -- [Context2ClauTagPrior]
          let context2ClauTagPriorNum = length context2ClauTagPriorBase                       -- The number of Context2OverType samples.
          putStrLn $ "getContext2ClauTagPriorBase: context2ClauTagPriorNum = " ++ show context2ClauTagPriorNum
          return context2ClauTagPriorBase

{- Get similarity degree between two prior contexts, namely two vectors of (LeftExtend, LeftOver, RightOver, RightExtend, OverType).
 - sim(contextOfSG1, contextOfSG2) = f(leSim, loSim, roSim, reSim, otSim), where
 -     leSim is similarity degree between two LeftExtends, namely two PhraSyn sets.
 -     loSim is similarity degree between two LeftOvers, namely two PhraSyns.
 -     roSim is similarity degree between two RightOvers, namely two PhraSyns.
 -     reSim is similarity degree between two RightExtends, namely two PhraSyn sets.
 -     otSim is similarity degree between two OverTypes, namely two values among [1..5].
 - LeftExtend, LeftOver, RightOver, RightExtend and OverType are not independent with each other, so matrix [[leSim, loSim, roSim, reSim, otSim]] is converted into
 - matrix [[leSim', loSim', roSim', reSim', otSim']] acoording to SVD (Singular Value Decomposition) such that leSim', loSim', roSim', reSim' and otSim' are orthogonal columns.
 - Thus, sim(contextOfSG1, contextOfSG2) = f(leSim, loSim, roSim, reSim, otSim) = f'(leSim', loSim', roSim', reSim', otSim'), where f' is Euclid length of vector (leSim', loSim', roSim', reSim').
 - Normalizedly, f' is Root Mean Square of (leSim', loSim', roSim', reSim', otSim')
 -
 - contextOfSGPairList: List of (ContextOfSG, ContextOfSG), in every element of which first ContextOfSG value is less than or equal to the second.
 - origSimList: List of (leSim, loSim, roSim, reSim, otSim), in which every quintuple (leSim, loSim, roSim, reSim, otSim) is corresponding to one tuple (contextOfSG1, contextOfSG2) in contextOfSGPairList.
 - origSimMatrix:  (k >< 4) [leSim1, loSim1, roSim1, reSim1, otSim1, ..., leSimk, loSimk, roSimk, reSimk, otSimk], which is matrix form of origSimList.
 - orthSimMatrix: origSimMatrix LA.<> v, where v is the right singular matrix.
 -}
getContextOfSGPairSimBySVD :: Context2ClauTagPriorBase -> IO ([(ContextOfSG, ContextOfSG)], Matrix Double, Matrix Double, [((ContextOfSG, ContextOfSG), SimDeg)])
getContextOfSGPairSimBySVD context2ClauTagPriorBase = do
    let contextOfSGList = nub $ map fst context2ClauTagPriorBase                -- [ContextOfSG], here there is no repetitive ContextOfSG values.
    putStrLn $ "Num. of repetitive ContextOfSG values = " ++ show (length context2ClauTagPriorBase - (length contextOfSGList))

    let contextOfOTList = nub $ map (\x -> (fst5 x, snd5 x, thd5 x, fth5 x)) contextOfSGList    -- [ContextOfOT]
    let phraSynPair2SimMap = getPhraSynPair2SimFromCOT contextOfOTList          -- Map (PhraSyn, PhraSyn) SimDeg
--    putStrLn $ "phraSynPair2SimMap: " ++ show phraSynPair2SimMap

    let leftExtendList = map fst5 contextOfSGList                               -- [LeftExtend]
    let lePair2SimList = getPhraSynSetPair2Sim leftExtendList phraSynPair2SimMap       -- [((LeftExtend, LeftExtend), SimDeg)]
    let leftOverList = map snd5 contextOfSGList                                 -- [LeftOver]
    let loPair2SimList = getPhraSynPair2Sim leftOverList phraSynPair2SimMap            -- [((LeftOver, LeftOver), SimDeg)]
    let rightOverList = map thd5 contextOfSGList                                -- [RightOver]
    let roPair2SimList = getPhraSynPair2Sim rightOverList phraSynPair2SimMap           -- [((RightOver, RightOver), SimDeg)]
    let rightExtendList = map fth5 contextOfSGList                              -- [RightExtend]
    let rePair2SimList = getPhraSynSetPair2Sim rightExtendList phraSynPair2SimMap      -- [((RightExtend, RightExtend), SimDeg)]

    let context2OverTypeBase = map ((\x -> ((fst5 x, snd5 x, thd5 x, fth5 x), fif5 x)) . fst) context2ClauTagPriorBase   -- [(ContextOfOT, OverType)]
    let contexts4OT1 = [fst x | x <- context2OverTypeBase, snd x == 1]          -- [ContextOfOT]
    let contexts4OT2 = [fst x | x <- context2OverTypeBase, snd x == 2]          -- [ContextOfOT]
    let contexts4OT3 = [fst x | x <- context2OverTypeBase, snd x == 3]          -- [ContextOfOT]
    let contexts4OT4 = [fst x | x <- context2OverTypeBase, snd x == 4]          -- [ContextOfOT]
    let contexts4OT5 = [fst x | x <- context2OverTypeBase, snd x == 5]          -- [ContextOfOT]
    let overType2ContextList = [(1,contexts4OT1),(2,contexts4OT2),(3,contexts4OT3),(4,contexts4OT4),(5,contexts4OT5)]  -- [(OverType, [ContextOfOT])]
    putStrLn $ "getContextOfSGPairSimBySVD: Num. of samples per overType: " ++ show (map (\x -> (fst x, length (snd x))) overType2ContextList)
    contextOfOTPairSimTuple <- getContextOfOTPairSimByRMS context2OverTypeBase
    let contextOfOTPairSimList = snd contextOfOTPairSimTuple                    -- [((ContextOfOT, ContextOfOT), OverType)]
    let otPair2SimList = [((fst o1, fst o2), getContextOfOTSetSim (snd o1) (snd o2) contextOfOTPairSimList)
                          | o1 <- overType2ContextList, o2 <- overType2ContextList, fst o1 <= fst o2]      -- [((OverType, OverType), SimDeg)]
    putStrLn $ "otPair2SimList: " ++ show otPair2SimList
    let contextOfSGPairList = [(x, y) | x <- contextOfSGList, y <- contextOfSGList, x <= y]   -- [(ContextOfSG, ContextOfSG)]
                                                      -- Ord definition is exhaustive, here there is no repetitive pairs.
    let numOfContextOfSGPair = length contextOfSGPairList
    let origSimList = [(getSimDegFromAttPair2Sim (fst5 x) (fst5 y) lePair2SimList
                      , getSimDegFromAttPair2Sim (snd5 x) (snd5 y) loPair2SimList
                      , getSimDegFromAttPair2Sim (thd5 x) (thd5 y) roPair2SimList
                      , getSimDegFromAttPair2Sim (fth5 x) (fth5 y) rePair2SimList
                      , getSimDegFromAttPair2Sim (fif5 x) (fif5 y) otPair2SimList
                       ) | (x, y) <- contextOfSGPairList]
    let origSimMatrix = (numOfContextOfSGPair >< 5) $ concat [[fst5 e, snd5 e, thd5 e, fth5 e, fif5 e] | e <- origSimList]       -- hmatrix

    let (u, s, v) = svd origSimMatrix
    let orthSimMatrix = origSimMatrix LA.<> v

-- Normalization: Root Mean Square of every row vector (leSim', loSim', roSim', reSim') in matrix orthSimMatrix.
    let contextOfSGPairSimList = map (sqrt . (/ 5.0) . sumElements) (toRows (cmap (\x -> x*x) orthSimMatrix))
    let contextOfSGPair2SimList = zip contextOfSGPairList contextOfSGPairSimList     -- [((ContextOfSG, ContextOfSG), SimDeg)]

    return (contextOfSGPairList, origSimMatrix, orthSimMatrix, contextOfSGPair2SimList)

{- Get similarity degree between two prior contexts, namely two vectors of (LeftExtend, LeftOver, RightOver, RightExtend, OverType).
 - sim(contextOfSG1, contextOfSG2) = f(leSim, loSim, roSim, reSim, otSim), where
 -     leSim is similarity degree between two LeftExtends, namely two PhraSyn sets.
 -     loSim is similarity degree between two LeftOvers, namely two PhraSyns.
 -     roSim is similarity degree between two RightOvers, namely two PhraSyns.
 -     reSim is similarity degree between two RightExtends, namely two PhraSyn sets.
 -     otSim is similarity degree between two OverTypes, namely two values among [1..5].
 - Suppose LeftExtend, LeftOver, RightOver, RightExtend and OverType are independent with each other,
 - namely columns of matrix [[leSim, loSim, roSim, reSim, otSim]] are orthogonal columns.
 - Thus, sim(contextOfSG1, contextOfSG2) = f(leSim, loSim, roSim, reSim, otSim), where f is Root Mean Square of (leSim, loSim, roSim, reSim, otSim)
 -
 - contextOfSGPairList: List of (ContextOfSG, ContextOfSG), in every element of which first ContextOfSG value is less than or equal to the second.
 - origSimList: List of (leSim, loSim, roSim, reSim, otSim), in which every quintuple (leSim, loSim, roSim, reSim, otSim) is corresponding to one tuple (contextOfSG1, contextOfSG2) in contextOfSGPairList.
 -}
getContextOfSGPairSimByRMS :: Context2ClauTagPriorBase -> IO ([(SimDeg, SimDeg, SimDeg, SimDeg, SimDeg)], [((ContextOfSG, ContextOfSG), SimDeg)])
getContextOfSGPairSimByRMS context2ClauTagPriorBase = do
    let contextOfSGList = nub $ map fst context2ClauTagPriorBase                -- [ContextOfSG], here there is no repetitive ContextOfSG values.
    putStrLn $ "Num. of repetitive ContextOfSG values = " ++ show (length context2ClauTagPriorBase - (length contextOfSGList))

    let contextOfOTList = nub $ map (\x -> (fst5 x, snd5 x, thd5 x, fth5 x)) contextOfSGList    -- [ContextOfOT]
    let phraSynPair2SimMap = getPhraSynPair2SimFromCOT contextOfOTList          -- Map (PhraSyn, PhraSyn) SimDeg

    let leftExtendList = map fst5 contextOfSGList                               -- [LeftExtend]
    let lePair2SimList = getPhraSynSetPair2Sim leftExtendList phraSynPair2SimMap       -- [((LeftExtend, LeftExtend), SimDeg)]
    let leftOverList = map snd5 contextOfSGList                                 -- [LeftOver]
    let loPair2SimList = getPhraSynPair2Sim leftOverList phraSynPair2SimMap            -- [((LeftOver, LeftOver), SimDeg)]
    let rightOverList = map thd5 contextOfSGList                                -- [RightOver]
    let roPair2SimList = getPhraSynPair2Sim rightOverList phraSynPair2SimMap           -- [((RightOver, RightOver), SimDeg)]
    let rightExtendList = map fth5 contextOfSGList                              -- [RightExtend]
    let rePair2SimList = getPhraSynSetPair2Sim rightExtendList phraSynPair2SimMap      -- [((RightExtend, RightExtend), SimDeg)]

    let context2OverTypeBase = map ((\x -> ((fst5 x, snd5 x, thd5 x, fth5 x), fif5 x)) . fst) context2ClauTagPriorBase   -- [(ContextOfOT, OverType)]
    let contexts4OT1 = [fst x | x <- context2OverTypeBase, snd x == 1]          -- [ContextOfOT]
    let contexts4OT2 = [fst x | x <- context2OverTypeBase, snd x == 2]          -- [ContextOfOT]
    let contexts4OT3 = [fst x | x <- context2OverTypeBase, snd x == 3]          -- [ContextOfOT]
    let contexts4OT4 = [fst x | x <- context2OverTypeBase, snd x == 4]          -- [ContextOfOT]
    let contexts4OT5 = [fst x | x <- context2OverTypeBase, snd x == 5]          -- [ContextOfOT]
    let overType2ContextList = [(1,contexts4OT1),(2,contexts4OT2),(3,contexts4OT3),(4,contexts4OT4),(5,contexts4OT5)]  -- [(OverType, [ContextOfOT])]
    putStrLn $ "getContextOfSGPairSimByRMS: Num. of samples per overType: " ++ show (map (\x -> (fst x, length (snd x))) overType2ContextList)
    contextOfOTPairSimTuple <- getContextOfOTPairSimByRMS context2OverTypeBase
    let contextOfOTPairSimList = snd contextOfOTPairSimTuple                    -- [((ContextOfOT, ContextOfOT), OverType)]
    let otPair2SimList = [((fst o1, fst o2), getContextOfOTSetSim (snd o1) (snd o2) contextOfOTPairSimList)
                          | o1 <- overType2ContextList, o2 <- overType2ContextList, fst o1 <= fst o2]      -- [((OverType, OverType), SimDeg)]
    putStrLn $ "otPair2SimList: " ++ show otPair2SimList
    let contextOfSGPairList = [(x, y) | x <- contextOfSGList, y <- contextOfSGList, x <= y]   -- [(ContextOfSG, ContextOfSG)]
                                                      -- Ord definition is exhaustive, here there is no repetitive pairs.
    let numOfContextOfSGPair = length contextOfSGPairList
    let origSimList = [(getSimDegFromAttPair2Sim (fst5 x) (fst5 y) lePair2SimList
                      , getSimDegFromAttPair2Sim (snd5 x) (snd5 y) loPair2SimList
                      , getSimDegFromAttPair2Sim (thd5 x) (thd5 y) roPair2SimList
                      , getSimDegFromAttPair2Sim (fth5 x) (fth5 y) rePair2SimList
                      , getSimDegFromAttPair2Sim (fif5 x) (fif5 y) otPair2SimList
                       ) | (x, y) <- contextOfSGPairList]

    let simList = [sqrt (sum [les * les, los * los, ros * ros, res * res, ots * ots] / 5.0) | (les,los,ros,res,ots) <- origSimList]   -- Root Mean Square
    let contextOfSGPair2SimList = zip contextOfSGPairList simList     -- [((ContextOfSG, ContextOfSG), SimDeg)]
    return (origSimList, contextOfSGPair2SimList)

{- Get similarity degrees between one ClauTagPrior context and ALL ClauTagPrior contexts.
 - contextOfSG: A ClauTagPrior context of a certain StruGene sample.
 - context2ClauTagPriorBase: The whole samples of mapping from ContextOfSG to ClauTagPrior, INCLUDING the sample of contextOfSG to its ClauTagPrior.
 - otPair2SimList: Similarity degrees between overlapping types calculated by first 500 StruGene samples.
 - origSimList: List of (leSim, loSim, roSim, reSim, otSim), in which every quintuple is corresponding to one tuple (contextOfSG1, contextOfSG2) in contextOfSGPairList.
 -}
getOneToAllContextOfSGSimByRMS :: ContextOfSG -> Context2ClauTagPriorBase -> IO ([(SimDeg, SimDeg, SimDeg, SimDeg, SimDeg)], [((ContextOfSG, ContextOfSG), SimDeg)])
getOneToAllContextOfSGSimByRMS contextOfSG context2ClauTagPriorBase = do
    let contextOfSGList = nub $ map fst context2ClauTagPriorBase                -- [ContextOfSG], here there is no repetitive ContextOfSG values.
--    putStrLn $ "contextOfSG: " ++ show contextOfSG
--    putStrLn $ "contextOfSGList: " ++ show contextOfSGList

    let (les, los, ros, res, _) = unzip5 contextOfSGList                        -- Extract [LeftExtend], [LeftOver], [RightOver] and [RightExtend].
    let leps = [(fst5 contextOfSG, y) | y <- les]                               -- [(LeftExtend, LeftExtend)]
    let lops = [(snd5 contextOfSG, y) | y <- los]                               -- [(LeftOver, LeftOver)]
    let rops = [(thd5 contextOfSG, y) | y <- ros]                               -- [(RightOver, RightOVer)]
    let reps = [(fth5 contextOfSG, y) | y <- res]                               -- [(RightExtend, RightExtend)]

    let lePhraSynPairs = nub $ [(u, v) | lep <- leps, u <- fst lep, v <- snd lep]   -- [(PhraSyn, PhraSyn)]
    let loPhraSynPairs = nub $ lops
    let roPhraSynPairs = nub $ rops
    let rePhraSynPairs = nub $ [(u, v) | rep <- reps, u <- fst rep, v <- snd rep]   -- [(PhraSyn, PhraSyn)]

    let phraSynPairs = map (\(x,y) -> if (x<=y) then (x,y) else (y,x))
                           $ nubBy (\x y -> x == swap y) $ lePhraSynPairs ++ loPhraSynPairs ++ roPhraSynPairs ++ rePhraSynPairs
                                                                                -- PhraSyn order in every pair was rectified.
--    putStrLn $ "phraSynPairs: " ++ show phraSynPairs
--    putStrLn $ "origSimMatrix: " ++ show (snd3 (getPhraSynPairSimByRMS phraSynPairs))
--    putStrLn $ "phraSynPair2SimList: " ++ show (thd3 (getPhraSynPairSimByRMS phraSynPairs))

    let phraSynPair2SimMap = Map.fromList $ thd3 $ getPhraSynPairSimByRMS phraSynPairs
--    putStrLn $ "phraSynPair2SimMap: " ++ show phraSynPair2SimMap

    let leftExtendList = map fst5 contextOfSGList                               -- [LeftExtend]
    let leftExtend = fst5 contextOfSG                                           -- LeftExtend
    let lePair2SimList = [((leftExtend, y), getPhraSynSetSim leftExtend y phraSynPair2SimMap) | y <- leftExtendList]
                                                                                -- [((LeftExtend, LeftExtend), SimDeg)]
--    putStrLn $ "lePair2SimList: " ++ show lePair2SimList

    let leftOverList = map snd5 contextOfSGList                                 -- [LeftOver]
    let leftOver = snd5 contextOfSG                                             -- LeftOver
    let loPair2SimList = [((leftOver, y), case (Map.lookup (leftOver, y) phraSynPair2SimMap) of
                                            Just x -> x
                                            Nothing -> case (Map.lookup (y, leftOver) phraSynPair2SimMap) of
                                                         Just x -> x
                                                         Nothing -> 0.0
                          ) | y <- leftOverList]                           -- [((LeftOver, LeftOver), SimDeg)]
--    putStrLn $ "loPair2SimList: " ++ show loPair2SimList

    let rightOverList = map thd5 contextOfSGList                                -- [RightOver]
    let rightOver = thd5 contextOfSG                                            -- RightOver
    let roPair2SimList = [((rightOver, y), case (Map.lookup (rightOver, y) phraSynPair2SimMap) of
                                             Just x -> x
                                             Nothing -> case (Map.lookup (y, rightOver) phraSynPair2SimMap) of
                                                          Just x -> x
                                                          Nothing -> 0.0
                          ) | y <- rightOverList]                          -- [((RightOver, RightOver), SimDeg)]
--    putStrLn $ "roPair2SimList: " ++ show roPair2SimList

    let rightExtendList = map fth5 contextOfSGList                              -- [RightExtend]
    let rightExtend = fth5 contextOfSG                                          -- RightExtend
    let rePair2SimList = [((rightExtend, y), getPhraSynSetSim rightExtend y phraSynPair2SimMap) | y <- rightExtendList]
                                                                                -- [((RightExtend, RightExtend), SimDeg)]
--    putStrLn $ "rePair2SimList: " ++ show rePair2SimList

-- Using the first 500 samples of stru_gene_202408, calculate similarity degrees between overlapping types as follows.
    let otPair2SimList = [((1,1),1.0),((1,2),7.3351553900482406e-3),((1,3),4.8666647227816856e-2),((1,4),3.3917793725912755e-2),((1,5),0.4247127394531829)
                         ,((2,2),1.0),((2,3),0.11777246281776142),((2,4),0.1761503383674159),((2,5),1.0017916991325765e-2)
                         ,((3,3),1.0),((3,4),0.5887136000291061),((3,5),7.134132733982824e-2)
                         ,((4,4),1.0),((4,5),5.1362972267575656e-2)
                         ,((5,5),1.0)]
    let contextOfSGPairList = [(contextOfSG, y) | y <- contextOfSGList]         -- [(ContextOfSG, ContextOfSG)]

    let numOfContextOfSGPair = length contextOfSGPairList
    let origSimList = [(getSimDegFromAttPair2Sim (fst5 contextOfSG) (fst5 y) lePair2SimList
                      , getSimDegFromAttPair2Sim (snd5 contextOfSG) (snd5 y) loPair2SimList
                      , getSimDegFromAttPair2Sim (thd5 contextOfSG) (thd5 y) roPair2SimList
                      , getSimDegFromAttPair2Sim (fth5 contextOfSG) (fth5 y) rePair2SimList
                      , getSimDegFromAttPair2Sim (fif5 contextOfSG) (fif5 y) otPair2SimList
                       ) | y <- contextOfSGList]

    let simList = [sqrt (sum [les * les, los * los, ros * ros, res * res, ots * ots] / 5.0) | (les,los,ros,res,ots) <- origSimList]   -- Root Mean Square
    let contextOfSGPair2SimList = zip contextOfSGPairList simList     -- [((ContextOfSG, ContextOfSG), SimDeg)]
    return (origSimList, contextOfSGPair2SimList)

{- Get similarity degrees between one ClauTagPrior context and ALL ClauTagPrior contexts.
 - contextOfSG: A ClauTagPrior context
 - context2ClauTagPriorBase: All samples of mapping from ContextOfSG to ClauTagPrior
 - contextOfSG can belong to context2ClauTagPriorBase or NOT.
 -}
getOneToAllContextOfSGSimByRMS' :: ContextOfSG -> Context2ClauTagPriorBase -> IO ([(SimDeg, SimDeg, SimDeg, SimDeg, SimDeg)], [((ContextOfSG, ContextOfSG), SimDeg)])
getOneToAllContextOfSGSimByRMS' contextOfSG context2ClauTagPriorBase = do
    let contextOfSGList = map fst context2ClauTagPriorBase                      -- [ContextOfSG]
    if (elem contextOfSG contextOfSGList)
      then getOneToAllContextOfSGSimByRMS contextOfSG context2ClauTagPriorBase
      else do
        let context2ClauTagPriorBase' = (contextOfSG, []) : context2ClauTagPriorBase     -- Insert contextOfSG at the head position of contextOfSGList
        contextOfSGPairSimTuple <- getOneToAllContextOfSGSimByRMS contextOfSG context2ClauTagPriorBase'
        return (tail (fst contextOfSGPairSimTuple), tail (snd contextOfSGPairSimTuple))

{- Given one StruGene context, get the ClauTagPrior of StruGene sample whose context is most similar to that StruGene context.
 - contextOfSG: A ClauTagPrior context.
 - context2ClauTagPriorBase: All samples of mapping from ContextOfSG to ClauTagPrior.
 - ContextOfSGPair2Sim :: ((ContextOfSG, ContextOfSG), simDeg)
 -}
findStruGeneSampleByMaxContextSim :: ContextOfSG -> Context2ClauTagPriorBase -> IO (SIdx, SimDeg, Context2ClauTagPrior)
findStruGeneSampleByMaxContextSim contextOfSG context2ClauTagPriorBase = do
    let contextOfSGList = map fst context2ClauTagPriorBase                      -- [ContextOfSG]
    if elem (contextOfSG) contextOfSGList
      then do                                                                   -- Hit the context of a certain sample
        putStrLn "findStruGeneSampleByMaxContextSim: Hit a sample."
        let idx = elemIndex (contextOfSG) contextOfSGList                         -- Find its position
        let idx' = maybe (-1) (+0) idx                                          -- '-1' is impossible.
        return $ (idx' + 1, 1.0, context2ClauTagPriorBase!!idx')                -- Similarity degree is 1.0 when hitting happens.
      else do
        putStrLn $ "findStruGeneSampleByMaxContextSim: Not hit by contextOfSG: " ++ show contextOfSG        
        let context2ClauTagPriorBase' = (contextOfSG, []) : context2ClauTagPriorBase     -- Insert contextOfSG at the head position of contextOfSGList
        contextOfSGPairSimTuple' <- getOneToAllContextOfSGSimByRMS contextOfSG context2ClauTagPriorBase'
        let contextOfSGPairSimTuple = (tail (fst contextOfSGPairSimTuple'), tail (snd contextOfSGPairSimTuple'))
        let contextOfSGPair2SimList = snd contextOfSGPairSimTuple               -- [((ContextOfSG, ContextOfSG), SimDeg)]
        let simDegList = map snd contextOfSGPair2SimList                        -- [SimDeg]
        let maxSim = maximum simDegList                                         -- 1.0 is possible.
        let idx = elemIndex maxSim simDegList                                   -- Use the first element with maximum
        let idx' = maybe (-1) (+0) idx
        return $ (idx' + 1, maxSim, context2ClauTagPriorBase!!idx')             -- SIdx values are from 1 to begin.

{- Among set of ContestOfSG values, calculate similarity degrees between one element and all elements,
 - If one element has similarity degree 1.0 only with itself, print '[OK]';
 - Otherwise, print '[Failed]' and list all ContextOfSG pairs with everyone of which has similarity degree 1.0.
 - startIdx: The SIdx value of first sample.
 - offset: Element offset in element list.
 -}
findWhereSim1HappenAmongStruGeneSamples :: SIdx -> Int -> [Context2ClauTagPrior] -> IO ()
findWhereSim1HappenAmongStruGeneSamples startIdx offset context2ClauTagPriorBase = do
    if offset < (length context2ClauTagPriorBase)
      then do
        let contextOfSG = fst (context2ClauTagPriorBase!!offset)                -- ContextOfSG
        contextOfSGPairSimTuple <- getOneToAllContextOfSGSimByRMS contextOfSG context2ClauTagPriorBase
        let contextOfSGPair2SimList = snd contextOfSGPairSimTuple               -- [((ContextOfSG, ContextOfSG), SimDeg)]
        let contextOfSGPair2SimList' = zip [0 ..] contextOfSGPair2SimList       -- [(Int, ((ContextOfSG, ContextOfSG), SimDeg))]
        let listOfContextOfSGWithSim1 = filter (\x -> (snd . snd) x == 1.0) contextOfSGPair2SimList'   -- Keep items whose similarity Degrees are 1.0
        if (length listOfContextOfSGWithSim1 == 1)
          then putStrLn $ "  Sample " ++ show (startIdx + offset) ++ " [OK]"     -- Only one pair has similarity degree 1.0
          else do
            putStr $ "  Sample " ++ show (startIdx + offset) ++ " " ++ show ((fst . fst . snd) (listOfContextOfSGWithSim1!!0))
            putStrLn $ " has similarity degree 1.0 with following samples:"     -- More than one pairs have similarity degree 1.0
            printContextOfSGPair2SimList startIdx listOfContextOfSGWithSim1
        findWhereSim1HappenAmongStruGeneSamples startIdx (offset + 1) context2ClauTagPriorBase
      else putStrLn ""

{- Print a list of [(Offset, ((ContextOfSG, ContextOfSG), SimDeg))], where Offset is index of this list.
 - startIdx: The SIdx value of first sample.
 -}
printContextOfSGPair2SimList :: SIdx -> [(Int, ((ContextOfSG, ContextOfSG), SimDeg))] -> IO ()
printContextOfSGPair2SimList _ [] = putStr ""
printContextOfSGPair2SimList startIdx (e:es) = do
    putStrLn $ "    Sample " ++ show (startIdx + fst e) ++ " " ++ show ((snd. fst . snd) e)
    printContextOfSGPair2SimList startIdx es
