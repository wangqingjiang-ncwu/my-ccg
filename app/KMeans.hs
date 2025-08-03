{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

module KMeans (
    readSIdxPriorFromDB,  -- SIdx -> SIdx -> IO [SIdxPrior]
    CIdx,    -- Int
    Cluster, -- (CIdx, [SIdxPrior], SIdx)
    SimDeg,  -- Float
    IterNum, -- Int
    exportClustersToCSV,  -- [Cluster] -> IO ()
    isConverged,          -- [Float] -> [Float] -> Bool
    getSimFromDB,         -- MySQLConn -> String -> SIdx -> SIdx -> IO SimDeg
    computeClusterMeanSims,      -- MySQLConn -> String -> [Cluster] -> IO [SimDeg]
    getMeanSimBetwOnetoMany,     -- MySQLConn -> String -> SIdx -> [SIdx] -> IO SimDeg
    exportMeanSimsLog,    -- IterNum -> [Float] -> IO ()
    exportClustersLog,    -- IterNum -> [Cluster] -> IO ()
    findBestCentroid,     -- MySQLConn -> String -> [SIdx] -> SIdx -> IO CIdx
    getCentroidOfACluster,       -- MySQLConn -> String -> SIdx -> [SIdx] -> IO SIdx
    assignClustersOntoSamples,   -- MySQLConn -> String -> [SIdxPrior] -> [SIdx] -> IO [Cluster]
    kMeansClustering,     -- Int -> IterNum -> SIdx -> SIdx -> IO [[SIdxPrior]]

) where

import Database.MySQL.Base
import Data.List
import Data.Ord (comparing)
import Data.Tuple.Utils (thd3)
import qualified Data.Map.Strict as M
import qualified System.IO.Streams as Streams
import qualified Data.ByteString.Lazy.Char8 as LBS
import System.Directory (createDirectoryIfMissing)
import qualified Data.String as DS

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

-- One cluster is described by its index, indices of inner samples, and index of its centroid.
type Cluster = (CIdx, [SIdxPrior], SIdx)

-- Type of similarity degrees in clustering. Note: SimDeg is type of Double in Module Clustering.
type SimDeg = Float

-- Index of iteration, or the number of times of iterations.
type IterNum = Int

-- Type of Cluster records prepared for CSV file
data ClusterRow = ClusterRow { cIdx :: !Int
                             , spList :: !String
                             , cent :: !Int
                             } deriving (Generic, Show)

instance Csv.FromNamedRecord ClusterRow
instance Csv.ToNamedRecord ClusterRow
instance Csv.DefaultOrdered ClusterRow

{- Store clustering result to a CSV file, every row in which records a cluster including index, (SIdx, Prior) samples, and centroid.
 -}
exportClustersToCSV :: [Cluster] -> IO ()
exportClustersToCSV clusters = do
  let rows :: [ClusterRow]
      rows = [ClusterRow cIdx (show spList) cent | (cIdx, spList, cent) <- clusters]
  let csvData = Csv.encodeDefaultOrderedByName rows
  confInfo <- readFile "Configuration"
  let cluster_logs_dir = getConfProperty "cluster_logs_dir" confInfo
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
 - prev: 上一轮各簇的平均相似度列表 [Float]
 - curr: 当前轮各簇的平均相似度列表 [Float]
 - Mean similarities in 'prev' and 'curr' both are ordered by cluster indices from 0 to K, so the previous and current
 - mean similarities of a cluster can be found easily by paring.
 -}
isConverged :: [Float] -> [Float] -> Bool
isConverged [] _ = False
isConverged prev curr = all (< simChangeThreshhold)   -- 判断所有变化率是否都小于 simChangeThreshold（0.03）
  [ abs (c - p) / (p + epsilon) | (p, c) <- zip prev curr ]  -- 使用 zip prev curr 将两轮数据配对

-- Get similarity degree between two samples.
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
 - centroid in one cluster are gotten by looking up database, and their average acts as the cohesion of the cluster.
 - Compute :: (CIdx, [SIdx], SIdx) -> Float
 - Suppose the centroid of every cluster is arranged as the first element in cluster. -
 -}
computeClusterMeanSims :: MySQLConn -> String -> [Cluster] -> IO [SimDeg]
computeClusterMeanSims conn tbl clusters = mapM compute clusters
  where
    compute (_, [], _) = error "computeClusterMeanSims: Empty cluster"          -- Mean similarity on empty cluster is NOT defined.
    compute (_, spList, cent) = getMeanSimBetwOnetoMany conn tbl cent (map fst spList)    -- IO SimDeg

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
 - iterNum: current iteration index.
 -}
exportMeanSimsLog :: IterNum -> [Float] -> IO ()
exportMeanSimsLog iterNum sims = do
  confInfo <- readFile "Configuration"
  let cluster_logs_dir = getConfProperty "cluster_logs_dir" confInfo
  let mean_sim_trace = getConfProperty "mean_sim_trace" confInfo
  let path = cluster_logs_dir ++ "/" ++ mean_sim_trace
  let csvRow = show iterNum ++ "," ++ intercalate "," (map (printf "%.2f") sims)
  appendFile path (csvRow ++ "\n")

{- Export clusters into a given file whenever one time of iteration is finished.
 - iterNum: current iteration index.
 -}
exportClustersLog :: IterNum -> [Cluster] -> IO ()
exportClustersLog iterNum clusters = do
  confInfo <- readFile "Configuration"
  let cluster_logs_dir = getConfProperty "cluster_logs_dir" confInfo
  let clusters_trace = getConfProperty "clusters_trace" confInfo
  let path = cluster_logs_dir ++ "/" ++ clusters_trace
  let csvRow = show iterNum ++ "," ++ intercalate "," (map show clusters)
  appendFile path (csvRow ++ "\n")

{- Find the closest centroid among centroids.
 - Similarity degrees come from database table, such as csg_sim_202507.
 -}
findBestCentroid :: MySQLConn -> String -> [SIdx] -> SIdx -> IO CIdx
findBestCentroid conn tbl cents sIdx = do
  sims <- mapM (\cent -> getSimFromDB conn tbl cent sIdx) cents             -- [SimDeg]
  return $ fromMaybe (-1) $ indexOfMax' sims                                  -- '-1' means exception.

{- Get new centroid of a cluster AFTER its inner elements are determined, when old centroid is still in this cluster.
 - Here, one cluster is defined as a collection of sample includes [SIdx].
 - So the centroid is also represented by a sample index.
 - The centroid is the index with which the sample is closest to other samples among all samples.
 - The closest means the mean distance to other samples are the minimum.
 - The distance between two samples equals to 1 minus their similarity degree, the latters are obtained from database.
 - One cluster might change in its inner members after every time of iteration, while its centroid can NOT leave from
 - this cluster unless it become not being centroid firstly.
 - After one time of iteration, original centroid might become not being centroid or still be centroid of its located cluster,
 - which depends whether it has the minimum mean distance among all samples in this cluster.
 -}
getCentroidOfACluster :: MySQLConn -> String -> SIdx -> [SIdx] -> IO SIdx
getCentroidOfACluster _ _ _ [] = error "getCentroidOfACluster: Empty cluster"
getCentroidOfACluster conn tbl oldCent sampleIndices = do
  oldMeanSim <- getMeanSimBetwOnetoMany conn tbl oldCent sampleIndices          -- SimDeg. Here use similarity degree instead of distance.
  meanSimList <- mapM (\idx1 -> do                                              -- [SimDeg]
                        meanSim <- getMeanSimBetwOnetoMany conn tbl idx1 sampleIndices     -- SimDeg
                        return meanSim
                      ) sampleIndices
  let maxMeanSim = maximum meanSimList
--  putStrLn $ "getCentroidOfACluster: oldMeanSim = " ++ show oldMeanSim ++ ", maxMeanSim = " ++ show maxMeanSim
  if maxMeanSim == oldMeanSim
    then return oldCent                  -- The original centroid obtains maximum mean similarity degree.
    else do
           let newCent = sampleIndices !! (case (elemIndex maxMeanSim meanSimList) of    -- One member with maximum mean similarity degree is selected as new centroid.
                                             Just x -> x
                                             Nothing -> error "getCentroidOfACluster: Exception"
                                          )
           return newCent

{- Assign one cluster onto every sample.
 - The assigned cluster is the one whose centroid is the closest one from the sample amone all centroids.
 - The distance between two samples equals to 1 minus their similarity degree, the latters are obtained from database.
 - Cluster :: (CIdx, [SIdxPrior], SIdx)
 - One cluster is defined as the index of this cluster, the inner (SIdx, Prior) samples, and the centroid of this cluster.
 -}
assignClustersOntoSamples :: MySQLConn -> String -> [SIdxPrior] -> [SIdx] -> IO [Cluster]
assignClustersOntoSamples conn tbl samples cents = do
    clusterIndices <- mapM (findBestCentroid conn tbl cents) $ map fst samples         -- [CIdx], according to previous centroids.
    let sampleLabelList = sortBy (\x y -> compare (snd x) (snd y)) $ zip samples clusterIndices  -- [(SIdxPrior, CIdx)] with CIdx ascending order
    let sampleLabelGroupList = groupBy (\x y -> (==) (snd x) (snd y)) sampleLabelList        -- [[(SIdxPrior, CIdx)]]
    let clustersWithoutCent = map (\g -> (snd (g!!0), (map fst g))) sampleLabelGroupList     -- [(CIdx, [SIdxPrior])]
    let clustersWithPrevCent = map (\(x, y) -> (fst x, snd x, y)) $ zip clustersWithoutCent cents     -- [(CIdx, [SIdxPrior], SIdx)]
    clusters <- mapM (\(cIdx, spList, cent) -> do
                      newCent <- getCentroidOfACluster conn tbl cent (map fst spList)        -- New centroids
                      return (cIdx, spList, newCent)                               -- IO (CIdx, [SIdxPrior], SIdx)
                     ) clustersWithPrevCent                                     -- [(CIdx, [SIdxPrior], SIdx)]
    return clusters                                                             -- IO [(CIdx, [SIdxPrior], SIdx)]

-- Entry of Algorith K-Means clustering
kMeansClustering :: Int -> IterNum -> SIdx -> SIdx -> IO [Cluster]
kMeansClustering k maxIterNum startId endId = do
  putStrLn $ "[INFO] === 开始 PCK-Means 聚类任务 ==="
  putStrLn $ "[INFO] K = " ++ show k ++ ", Maximal iteration times = " ++ show maxIterNum
  confInfo <- readFile "Configuration"
  let cluster_logs_dir = getConfProperty "cluster_logs_dir" confInfo
  createDirectoryIfMissing True cluster_logs_dir               -- Assure directory for storing clustering result is created.

  samples <- readSIdxPriorFromDB startId endId                 -- [SIdxPrior]

  let tbl = getConfProperty "csg_sim_tbl" confInfo
  putStrLn $ "[INFO] 使用相似度表: " ++ tbl

  putStrLn $ "[INFO] 建立初始簇"
  conn <- getConn
  let cents = take k $ map fst samples           -- [(SIdx, Prior)]
  clusters <- assignClustersOntoSamples conn tbl samples cents                  -- [Cluster]
  exportClustersLog 0 clusters                   -- Record initial clusters
  currMeans <- computeClusterMeanSims conn tbl clusters                         -- [SimDeg]
  exportMeanSimsLog 0 currMeans

  putStrLn $ "[INFO] 开始迭代"
  clus_res <- loop conn tbl 1 samples clusters currMeans                        -- [Cluster]
  return clus_res

  where
    loop :: MySQLConn -> String -> IterNum -> [SIdxPrior] -> [Cluster] -> [SimDeg] -> IO [Cluster]
    loop conn tbl iterNum samples clusters currMeans = do
      putStrLn $ "[INFO] 第" ++ show iterNum ++ "轮开始"
      newClusters <- assignClustersOntoSamples conn tbl samples (map thd3 clusters)      -- [Cluster]
      exportClustersLog iterNum newClusters                                     -- Record new clusters

      newMeans <- computeClusterMeanSims conn tbl newClusters
      exportMeanSimsLog iterNum newMeans

      if isConverged currMeans newMeans
        then putStrLn "[INFO] 聚类已收敛" >> return newClusters
        else if iterNum > maxIterNum
               then putStrLn "[INFO] 迭代次数达到最大" >> return newClusters
               else loop conn tbl (iterNum + 1) samples newClusters newMeans
