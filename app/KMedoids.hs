{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

{- K-Medoids ares K sample centres, subtly different with K-Means algorithm.
 - Medoids are real samples while medoids might not be.
 -}
module KMedoids (
    readSIdxPriorFromDB,  -- SIdx -> SIdx -> IO [SIdxPrior]
    CIdx,    -- Int
    Cluster, -- (CIdx, [SIdxPrior], SIdx, Prior, Purity)
    SimDeg,  -- Float
    IterNum, -- Int
    exportClustersToCSV,  -- Int -> [Cluster] -> IO ()
    isConverged,          -- [SimDeg] -> [SimDeg] -> Bool
    isPureEnough,         -- [Float] -> Float -> Bool
    getSimFromDB,         -- MySQLConn -> String -> SIdx -> SIdx -> IO SimDeg
    computeClusterMeanSims,      -- MySQLConn -> String -> [Cluster] -> IO [SimDeg]
    getMeanSimBetwOnetoMany,     -- MySQLConn -> String -> SIdx -> [SIdx] -> IO SimDeg
    exportMeanSimsLog,    -- Int -> IterNum -> [Float] -> IO ()
    exportClustersLog,    -- Int -> IterNum -> [Cluster] -> IO ()
    findBestMedoid,       -- MySQLConn -> String -> [SIdx] -> SIdx -> IO CIdx
    getMedoidOfACluster,         -- MySQLConn -> String -> SIdx -> [SIdx] -> IO SIdx
    assignClustersOntoSamples,   -- MySQLConn -> String -> [SIdxPrior] -> [SIdx] -> IO [Cluster]
    kMedoidsClustering,     -- Int -> IterNum -> SIdx -> SIdx -> IO (Int, [Cluster])
    calProbOfClosestSampleHasSamePrior,     -- IO Float
) where

import Database.MySQL.Base
import Data.List
import Data.Ord (comparing)
import Data.Tuple.Utils (thd3)
import Data.Time.Clock
import System.Directory (createDirectoryIfMissing, doesFileExist, removeFile)
import qualified Data.Map.Strict as M
import qualified System.IO.Streams as Streams
import qualified Data.ByteString.Lazy.Char8 as LBS
import qualified Data.String as DS
import qualified Data.Set as Set
import qualified System.IO.Streams as S

import Data.Text    (Text)
import GHC.Generics (Generic)
import Data.Csv as Csv
import qualified Data.ByteString.Lazy as BL
import Text.Printf (printf)

import Data.Maybe (fromMaybe)
import Control.Monad (when, foldM, forM, forM_)

import Clustering hiding (SimDeg)
import Database
import AmbiResol
import Utils

{- Get SIdxPrior samples from database.
 - SIdxPrior :: (SIdx, Prior)
 -}
readSIdxPriorFromDB :: SIdx -> SIdx -> IO [SIdxPrior]
readSIdxPriorFromDB startId endId = do
  confInfo <- readFile "Configuration"
  let syntax_ambig_resol_model = getConfProperty "syntax_ambig_resol_model" confInfo
  conn <- getConn
  let sqlstat = DS.fromString $ "SELECT id, clauTagPrior FROM " ++ syntax_ambig_resol_model ++ " where id >= " ++ show startId ++ " and id <= " ++ show endId
  stmt <- prepareStmt conn sqlstat
  (defs, is) <- queryStmt conn stmt []
  sps <- readStreamBySIdxPrior [] is                -- [StruGeneSample]
  closeStmt conn stmt
  close conn
  return sps

-- Type of cluster index, actually is [0 .. K-1] where K is the number of clusters.
type CIdx = Int

{- One cluster is described by its index, inner samples, medoid, sample Prior value with highest frequency and its purity.
 -}
type Cluster = (CIdx, [SIdxPrior], SIdx, Prior, Purity)

-- Type of similarity degrees in clustering. Note: SimDeg is type of Double in Module Clustering.
type SimDeg = Float

-- Index of iteration, or the number of times of iterations.
type IterNum = Int

-- Type of Cluster records prepared for CSV file
data ClusterRow = ClusterRow { cIdx :: !Int
                             , spList :: !String
                             , medoid :: !Int
                             , prior :: !String
                             , purity :: !Float
                             } deriving (Generic, Show)

instance Csv.FromNamedRecord ClusterRow
instance Csv.ToNamedRecord ClusterRow
instance Csv.DefaultOrdered ClusterRow

{- Store clustering result to a CSV file, every row in which records a cluster including index, (SIdx, Prior) samples,
 - medoid, prior with highest frequency, and prior purity.
 -}
exportClustersToCSV :: Int -> [Cluster] -> IO ()
exportClustersToCSV k clusters = do
  let rows :: [ClusterRow]
      rows = [ClusterRow cIdx (show spList) medoid (show prior) purity | (cIdx, spList, medoid, prior, purity) <- clusters]
  let csvData = Csv.encodeDefaultOrderedByName rows
  confInfo <- readFile "Configuration"
  let cluster_logs_dir = (getConfProperty "cluster_logs_dir" confInfo) ++ "_k" ++ show k
  let clus_res_file = getConfProperty "clus_res_file" confInfo
  let path = cluster_logs_dir ++ "/" ++ clus_res_file
  BL.writeFile path csvData
  putStrLn $ "[INFO] Clustering result has been stored into: " ++ path

-- 收敛参数
epsilon :: Float
epsilon = 1e-6

-- 设定“相对变化率”的阈值，用于判断聚类是否收敛。
simChangeThreshhold :: Float
simChangeThreshhold = 0.03

{- 判断收敛,用于判断两个相邻迭代之间的簇平均相似度变化是否足够小，从而决定是否终止聚类过程。
 - prev: 上一轮各簇的平均相似度列表 [SimDeg]
 - curr: 当前轮各簇的平均相似度列表 [SimDeg]
 - Mean similarities in 'prev' and 'curr' both are ordered by cluster indices from 0 to K, so the previous and current
 - mean similarities of a cluster can be found easily by paring.
 -}
isConverged :: [SimDeg] -> [SimDeg] -> Bool
isConverged [] _ = True
isConverged prev curr = all (< simChangeThreshhold)   -- 判断所有变化率是否都小于 simChangeThreshold（0.03）
  [ abs (c - p) / (p + epsilon) | (p, c) <- zip prev curr ]  -- 使用 zip prev curr 将两轮数据配对

-- Prior purity threshhold, used for deciding whether clustering converges.
priorPurityThreshhold :: Float
priorPurityThreshhold = 0.9

{- Decide whether all Prior value purities of all clusters reach the given threshhold.
 -}
isPureEnough :: [Float] -> Float -> Bool
isPureEnough [] _ = True
isPureEnough purities priorPurityThreshhold = all (< priorPurityThreshhold) purities

{- Get similarity degree between two samples.
 -}
getSimFromDB :: MySQLConn -> String -> SIdx -> SIdx -> IO SimDeg
getSimFromDB conn tbl sIdx1 sIdx2 = do
  case sIdx1 == sIdx2 of
    True -> return 1.0                 -- 处理自身相似度
    False -> do                        -- (i1,i2)
      let i1 = min sIdx1 sIdx2
          i2 = max sIdx1 sIdx2
      (_, is) <- query conn (Query $ LBS.pack $ "SELECT sim FROM " ++ tbl ++ " WHERE contextofsg1idx = ? AND contextofsg2idx = ?")
                                                     [MySQLInt32 (fromIntegral i1), MySQLInt32 (fromIntegral i2)]
      rows <- Streams.toList is
      case rows of
        [[MySQLFloat sim]] -> return sim
        [] -> error $ "[ERR] 数据库中找不到样本对 " ++ show (sIdx1, sIdx2) ++ " 的相似度记录"

{- Calculate cohesion (内聚性) of every cluster by calling function 'compute', and return list of cohesions of all clusters.
 - Cohesion can be evaluated by multiple methods, in which Within-Cluster Sum of Squares (WCSS) is used for quickly evaluating clustering.
 - Similarity degrees between ContextOfSG samples are restricted to [0, 1]. Similar to WCSS, all similarities between every sample and
 - medoid in one cluster are gotten by looking up database, and their average acts as the cohesion of the cluster.
 - Compute :: (CIdx, [SIdx], SIdx, Prior, Purity) -> Float
 - Suppose the medoid of every cluster is arranged as the first element in cluster. -
 -}
computeClusterMeanSims :: MySQLConn -> String -> [Cluster] -> IO [SimDeg]
computeClusterMeanSims conn tbl clusters = mapM compute clusters
  where
    compute (_, [], _, _, _) = error "computeClusterMeanSims: Empty cluster"          -- Mean similarity on empty cluster is NOT defined.
    compute (_, spList, medoid, _, _) = getMeanSimBetwOnetoMany conn tbl medoid (map fst spList)    -- IO SimDeg

{- Calculate the mean similarity degree between one sample to many samples.
 - Similarity degrees between two samples are checked out from database table, such as 'csg_sim_202507'.
 -}
getMeanSimBetwOnetoMany :: MySQLConn -> String -> SIdx -> [SIdx] -> IO SimDeg
getMeanSimBetwOnetoMany conn tbl sIdx sampleIndices = do
  sims <- mapM (\sIdx2 -> getSimFromDB conn tbl sIdx sIdx2) sampleIndices     -- [SimDeg]
  let meanSim = sum sims / fromIntegral (length sims) :: SimDeg               -- SimDeg
--  putStrLn $ "[INFO] getMeanSimBetwOnetoMany: meanSim = " ++ show meanSim
  return meanSim

{- Export mean similarity degrees of all clusters into a file.
 - sims: List of mean similarity degrees of all clusters.
 - k: K value, used for locating directory
 - iterNum: current iteration index.
 -}
exportMeanSimsLog :: Int -> IterNum -> [Float] -> IO ()
exportMeanSimsLog k iterNum sims = do
  confInfo <- readFile "Configuration"
  let cluster_logs_dir = (getConfProperty "cluster_logs_dir" confInfo) ++ "_k" ++ show k
  let mean_sim_trace_file = getConfProperty "mean_sim_trace_file" confInfo
  let path = cluster_logs_dir ++ "/" ++ mean_sim_trace_file
  let csvRow = show iterNum ++ "," ++ intercalate "," (map (printf "%.2f") sims)
  appendFile path (csvRow ++ "\n")

{- Export clusters into a given file whenever one time of iteration is finished.
 - k: K value, used for locating directory
 - iterNum: current iteration index.
 -}
exportClustersLog :: Int -> IterNum -> [Cluster] -> IO ()
exportClustersLog k iterNum clusters = do
  confInfo <- readFile "Configuration"
  let cluster_logs_dir = (getConfProperty "cluster_logs_dir" confInfo) ++ "_k" ++ show k
  let clusters_trace_file = getConfProperty "clusters_trace_file" confInfo
  let path = cluster_logs_dir ++ "/" ++ clusters_trace_file
  let csvRow = show iterNum ++ "," ++ intercalate "," (map show clusters)
  appendFile path (csvRow ++ "\n")

{- Find the closest medoid among medoids.
 - Similarity degrees come from database table, such as csg_sim_202507.
 -}
findBestMedoid :: MySQLConn -> String -> [SIdx] -> SIdx -> IO CIdx
findBestMedoid conn tbl medoids sIdx = do
  sims <- mapM (\medoid -> getSimFromDB conn tbl medoid sIdx) medoids           -- [SimDeg]
  return $ fromMaybe (-1) $ indexOfMax' sims                                    -- '-1' means exception.

{- Get new medoid of a cluster AFTER its inner elements are determined, when old medoid is still in this cluster.
 - Here, one cluster is defined as a collection of sample includes [SIdx].
 - So the medoid is also represented by a sample index.
 - The medoid is the index with which the sample is closest to other samples among all samples.
 - The closest means the mean distance to other samples are the minimum.
 - The distance between two samples equals to 1 minus their similarity degree, the latters are obtained from database.
 - One cluster might change in its inner members after every time of iteration, while its medoid can NOT leave from
 - this cluster unless it become not being medoid firstly.
 - After one time of iteration, original medoid might become not being medoid or still be medoid of its located cluster,
 - which depends whether it has the minimum mean distance among all samples in this cluster.
 -}
getMedoidOfACluster :: MySQLConn -> String -> SIdx -> [SIdx] -> IO SIdx
getMedoidOfACluster _ _ _ [] = error "getMedoidOfACluster: Empty cluster"
getMedoidOfACluster conn tbl oldMedoid sampleIndices = do
  oldMeanSim <- getMeanSimBetwOnetoMany conn tbl oldMedoid sampleIndices        -- SimDeg. Here use similarity degree instead of distance.
  meanSimList <- mapM (\idx1 -> do                                              -- [SimDeg]
                        meanSim <- getMeanSimBetwOnetoMany conn tbl idx1 sampleIndices     -- SimDeg
                        return meanSim
                      ) sampleIndices
  let maxMeanSim = maximum meanSimList
--  putStrLn $ "getMedoidOfACluster: oldMeanSim = " ++ show oldMeanSim ++ ", maxMeanSim = " ++ show maxMeanSim
  if maxMeanSim == oldMeanSim
    then return oldMedoid                  -- The original medoid obtains maximum mean similarity degree.
    else do
           let newCent = sampleIndices !! (case (elemIndex maxMeanSim meanSimList) of    -- One member with maximum mean similarity degree is selected as new medoid.
                                             Just x -> x
                                             Nothing -> error "getMedoidOfACluster: Exception"
                                          )
           return newCent

{- Assign one cluster onto every sample.
 - The assigned cluster is the one whose medoid is the closest one from the sample among all clusters.
 - The distance between two samples equals to 1 minus their similarity degree, the latters are obtained from database.
 - Cluster :: (CIdx, [SIdxPrior], SIdx, Prior, Purity)
 - One cluster is defined as cluster index, the inner (SIdx, Prior) samples, cluster medoid, cluster Prior value with highest
 - frequency and its purity.
 -}
assignClustersOntoSamples :: MySQLConn -> String -> [SIdxPrior] -> [SIdx] -> IO [Cluster]
assignClustersOntoSamples conn tbl samples medoids = do
    clusterIndices <- mapM (findBestMedoid conn tbl medoids) $ map fst samples            -- [CIdx], according to previous medoids.
    let sampleLabelList = sortBy (\x y -> compare (snd x) (snd y)) $ zip samples clusterIndices   -- [(SIdxPrior, CIdx)] with CIdx ascending order
    let sampleLabelGroupList = groupBy (\x y -> (==) (snd x) (snd y)) sampleLabelList     -- [[(SIdxPrior, CIdx)]]
    let labelSamplesList = map (\g -> (snd (g!!0), (map fst g))) sampleLabelGroupList     -- [(CIdx, [SIdxPrior])]
    let labelSamplesPrevMedoidList = map (\(x, y) -> (fst x, snd x, y)) $ zip labelSamplesList medoids  -- [(CIdx, [SIdxPrior], SIdx)]
    clusters <- mapM (\(cIdx, spList, medoid) -> do
                      newMedoid <- getMedoidOfACluster conn tbl medoid (map fst spList)   -- New medoids
                      let (prior, purity) = purityOfMajorPrior (map snd spList)           -- (Prior, Purity)
                      return (cIdx, spList, newMedoid, prior, purity)           -- IO (CIdx, [SIdxPrior], SIdx, Prior, Purity)
                     ) labelSamplesPrevMedoidList                               -- [(CIdx, [SIdxPrior], SIdx)]
    return clusters                                                             -- IO [(CIdx, [SIdxPrior], SIdx, Prior, Purity)]

{- Entry of Algorith K-Medoids clustering.
 -}
kMedoidsClustering :: Int -> IterNum -> SIdx -> SIdx -> IO (Int, [Cluster])
kMedoidsClustering k maxIterNum startId endId = do
  putStrLn $ "[INFO] === 开始 PCK-Means 聚类任务 ==="
  putStrLn $ "[INFO] K = " ++ show k ++ ", Maximal iteration times = " ++ show maxIterNum
  confInfo <- readFile "Configuration"
  let cluster_logs_dir = (getConfProperty "cluster_logs_dir" confInfo) ++ "_k" ++ show k          -- Directory name including K value.
  let clusters_trace_file = cluster_logs_dir ++ "/" ++ (getConfProperty "clusters_trace_file" confInfo)    -- File including trace of clusters.
  let mean_sim_trace_file = cluster_logs_dir ++ "/" ++ (getConfProperty "mean_sim_trace_file" confInfo)    -- File including mean similarity of clusters.
  exist <- doesFileExist clusters_trace_file
  if exist
    then removeFile clusters_trace_file            -- Remove file
    else putStrLn "kMedoidsClustering: clusters_trace_file does NOT exist."
  exist <- doesFileExist mean_sim_trace_file
  if exist
    then removeFile mean_sim_trace_file            -- Remove file
    else putStrLn "kMedoidsClustering: mean_sim_trace_file does NOT exist."

  createDirectoryIfMissing True cluster_logs_dir               -- Assure directory for storing clustering result is created.

  samples <- readSIdxPriorFromDB startId endId                 -- [SIdxPrior]

  let tbl = getConfProperty "csg_sim_tbl" confInfo
  putStrLn $ "[INFO] 使用相似度表: " ++ tbl

  putStrLn $ "[INFO] 建立初始簇"
  conn <- getConn
  let medoids = take k $ map fst samples                                        -- [SIdx], indices of the first k samples.
  clusters <- assignClustersOntoSamples conn tbl samples medoids                -- [Cluster], do clustering according to old medoids, then update medoids.
  exportClustersLog k 0 clusters                                                -- Record initial clusters
  currMeans <- computeClusterMeanSims conn tbl clusters                         -- [SimDeg]
  exportMeanSimsLog k 0 currMeans

  putStrLn $ "[INFO] 开始迭代"
  ct1 <- getCurrentTime
  flag_clus_res <- loop conn tbl ct1 1 samples clusters currMeans               -- (Int, [Cluster])
  return flag_clus_res

  where
    loop :: MySQLConn -> String -> UTCTime -> IterNum -> [SIdxPrior] -> [Cluster] -> [SimDeg] -> IO (Int, [Cluster])
    loop conn tbl ct1 iterNum samples clusters currMeans = do
      putStr $ "[INFO] 第" ++ show iterNum ++ "轮开始 ... "

      let currMedoids = Set.fromList $ map thd5 clusters                        -- Set SIdx, the current medoids.
      newClusters <- assignClustersOntoSamples conn tbl samples (map thd5 clusters)      -- [Cluster]
      exportClustersLog k iterNum newClusters                                   -- Record new clusters

      newMeans <- computeClusterMeanSims conn tbl newClusters                   -- [SimDeg]
      exportMeanSimsLog k iterNum newMeans

      let newMedoids = Set.fromList $ map thd5 newClusters                      -- Set SIdx
      ct2 <- getCurrentTime
      let totalTime = diffUTCTime ct2 ct1                                       -- NominalDiffTime
      let totalTimeFloat = (read . init . show) (diffUTCTime ct2 ct1) :: Float
      let timePerIter = totalTimeFloat / (fromIntegral iterNum :: Float)        -- Precalculate
      let sizeVar = var $ map (length . snd5) newClusters                       -- Float

      putStrLn $ "结束, 簇纯度平均为 " ++ show (foldl (+) 0.0 (map fif5 newClusters) / (fromIntegral k))
                                      ++ ", 簇大小总体方差为 " ++ show sizeVar
                                      ++ ", 簇惯性为 " ++ show (foldl (+) 0.0 newMeans / (fromIntegral (length newMeans)))

      if currMedoids == newMedoids
        then do
          putStrLn $ "[INFO] 簇心无变化, 用时 "  ++ show totalTime ++ ", 一轮迭代平均用时 " ++ show timePerIter ++ " s."
          return (0, newClusters)              -- [(Int, Cluster)]
        else if isPureEnough (map fif5 newClusters) priorPurityThreshhold
               then do
                 putStrLn $ "[INFO] 簇纯度超过阈值, 用时 "  ++ show totalTime ++ ", 一轮迭代平均用时 " ++ show timePerIter ++ " s."
                 return (1, newClusters)       -- [(Int, Cluster)]
               else if isConverged currMeans newMeans
                      then do
                        putStrLn $ "[INFO] 簇聚合度已收敛, 用时 "  ++ show totalTime ++ ", 一轮迭代平均用时 " ++ show timePerIter ++ " s."
                        return (2, newClusters)            -- [(Int, Cluster)]
                      else if iterNum > maxIterNum
                             then do
                               putStrLn $ "[INFO] 迭代次数达到最大, 用时 "  ++ show totalTime ++ ", 一轮迭代平均用时 " ++ show timePerIter ++ " s."
                               return (3, newClusters)     -- [(Int, Cluster)]
                             else loop conn tbl ct1 (iterNum + 1) samples newClusters newMeans

{- Calculate probability of that two closest samples has same Prior value (called event) in StruGene2 samples.
 - Given a sample, if there exists only one another sample which has highest similarity degree with it, then the sample is the unique closest.
 - At this moment, if two sample has same Prior value, then the event count adds 1, otherwise adds zero.
 - If there exist multiple sample which all have highest similarity degree, then they all are the closest.
 - At this moment, suppose the ratio of samples having same Prior value with given sample in closest samples is r,
 - then the event count adds r.
 -}
calProbOfClosestSampleHasSamePrior :: String -> SIdx -> SIdx -> IO Float
calProbOfClosestSampleHasSamePrior csg_sim_tbl startId endId = do
    spList <- readSIdxPriorFromDB startId endId                                 -- [SIdxPrior], namely [(SIdx, Prior)]
    putStrLn $ "[INFO] " ++ show (endId - startId + 1) ++ " Samples are read into memory."
    conn <- getConn
    let sqlstat = DS.fromString $ "select contextofsg1idx, contextofsg2idx, sim from " ++ csg_sim_tbl ++ " where contextofsg1idx >= ? and contextofsg1idx <= ?"
    stmt <- prepareStmt conn sqlstat
    (_, is) <- queryStmt conn stmt [toMySQLInt16U startId, toMySQLInt16U endId]     -- Enough for computing.
    rows' <- S.toList is                                                        -- [[MySQLValue]]
    let rows = map (\x -> ((fromMySQLInt16U (x!!0), fromMySQLInt16U (x!!1)), fromMySQLFloat (x!!2))) rows'
    putStrLn $ "[INFO] " ++ show (length rows) ++ " rows of ((SIdx, SIdx), SimDeg) are read into memory."

    let sIdxPair2SimMap = M.fromList rows                                       -- Map (SIdx, SIdx) SimDeg
    let probList = map (probOnOneSample sIdxPair2SimMap spList) spList          -- [Float]
    let meanProb = foldl (+) 0.0 probList / (fromIntegral (length probList))    -- Float
    return meanProb

    where
      probOnOneSample :: M.Map (SIdx,SIdx) SimDeg -> [(SIdx,Prior)] -> (SIdx,Prior) -> Float
      probOnOneSample sIdxPair2SimMap spList sIdxPrior = hitRatio
        where
          sIdx = fst sIdxPrior                -- SIdx
          prior = snd sIdxPrior               -- Prior
          otherSamples = filter (\x -> fst x /= sIdx) spList                    -- [(SIdx, Prior)]
          sims = map (\x -> case (x <= sIdx) of
                              True -> M.lookup (x, sIdx) sIdxPair2SimMap
                              False -> M.lookup (sIdx, x) sIdxPair2SimMap
                     ) (map fst otherSamples)                                   -- [Maybe SimDeg]
          sims' = map (\x -> case x of
                               Just x -> x
                               Nothing -> error "calProbOfClosestSampleHasSamePrior: sIdxPair2SimMap exception"
                      ) sims                                                    -- [SimDeg]
          highestSim = maximum sims'
          indices = map fst $ filter (\x -> snd x == highestSim) $ zip [0..] sims'    -- [Int]
          closestSamples = [otherSamples!!idx | idx <- indices]                 -- [SIdx]
          hitNum = length $ filter (\x -> snd x == prior) closestSamples
          hitRatio = fromIntegral hitNum / fromIntegral (length closestSamples)
