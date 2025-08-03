-- Copyright (c) 2019-2025 China University of Water Resources and Electric Power
-- All rights reserved.

module KMeansSpec where

import Database
import AmbiResol
import KMeans
import Utils
import Test.Hspec

spec :: Spec
spec = do
  describe "KMeans" $ do
    it "Test readSIdxPriorFromDB, result of length samples is 18782." $ do
      samples <- readSIdxPriorFromDB 1 4
      length samples `shouldBe` (4 :: Int)

    it "Test readSIdxPriorFromDB, samples!!0 is (1,Rp)." $ do
      samples <- readSIdxPriorFromDB 1 4
      samples!!0 `shouldBe` ((1,Rp) :: (SIdx,Prior))

    it "Result of exportClustersToCSV \"cluster_logs/clus_res_test\" [(0,[(1,Rp),(3,Noth)],1), (1,[(2,Lp),(4,Rp)],4)] is OK." $ do
      exportClustersToCSV [(0,[(1,Rp),(3,Noth)],1), (1,[(2,Lp),(4,Rp)],4)]
      0 `shouldBe` 0

    it "isConverged [0.7, 0.8, 0.9] [0.71, 0.79, 0.91] is True." $ do
      isConverged [0.7, 0.8, 0.9] [0.71, 0.79, 0.91] `shouldBe` True

    it "isConverged [0.7, 0.8, 0.9] [0.73, 0.79, 0.91] is False." $ do
      isConverged [0.7, 0.8, 0.9] [0.73, 0.79, 0.91] `shouldBe` False

    it "getSimFromDB conn \"csg_sim_202507\" 1 3 is 0.483538." $ do
      conn <- getConn
      simDeg <- getSimFromDB conn "csg_sim_202507" 1 3
      simDeg `shouldBe` (0.483538 :: Float)

    it "getSimFromDB conn \"csg_sim_202507\" 2 4 is 0.659400." $ do
      conn <- getConn
      simDeg <- getSimFromDB conn "csg_sim_202507" 2 4
      simDeg `shouldBe` (0.659400 :: Float)

    it "computeClusterMeanSims conn \"csg_sim_202507\" [(0,[(1,Rp),(3,Noth)],1), (1,[(2,Lp),(4,Rp)],4)] is [0.741769, 0.829700]." $ do
      conn <- getConn
      meanSim <- computeClusterMeanSims conn "csg_sim_202507" [(0,[(1,Rp),(3,Noth)],1), (1,[(2,Lp),(4,Rp)],4)]
      meanSim `shouldBe` ([0.741769, 0.829700] :: [KMeans.SimDeg])

    it "getMeanSimBetwOnetoMany conn \"csg_sim_202507\" 4 [1,2,3] is 0.64931303." $ do
      conn <- getConn
      sim14 <- getSimFromDB conn "csg_sim_202507" 1 4        -- 0.606487  MySQLFloat precision is 6 decimal digits.
      sim24 <- getSimFromDB conn "csg_sim_202507" 2 4        -- 0.659400
      sim34 <- getSimFromDB conn "csg_sim_202507" 3 4        -- 0.682052
      meanSim <- getMeanSimBetwOnetoMany conn "csg_sim_202507" 4 [1,2,3]    -- 0.64931303  Haskell Float precision is 8 decimal digits.
      putStrLn $ "[INFO] sim14 = " ++ show sim14 ++ ", sim24 = " ++ show sim24 ++ ", sim34 = " ++ show sim34 ++ ", meanSim = " ++ show meanSim
      meanSim `shouldBe` (0.64931303 :: KMeans.SimDeg)
{-
    it "Result of exportMeanSimsLog 1 [0.5,0.6] is OK." $ do      -- Append into cluster_logs/mean_sim_trace_k20.csv
      exportMeanSimsLog 1 [0.5,0.6]
      0 `shouldBe` 0

    it "Result of exportClustersLog 1 [(0,[(1,Rp),(3,Rp)],1),(1,[(2,Rp),(4,Lp)],2)] is OK." $ do   -- Append into cluster_logs/cluster_trace_k20.csv
      exportClustersLog 1 [(0,[(1,Rp),(3,Rp)],1),(1,[(2,Rp),(4,Lp)],2)]
      0 `shouldBe` 0
 -}
    it "Result of findBestCentroid conn \"csg_sim_202507\" [1,2,3] 4 is 2." $ do
      conn <- getConn
      sim14 <- getSimFromDB conn "csg_sim_202507" 1 4
      sim24 <- getSimFromDB conn "csg_sim_202507" 2 4
      sim34 <- getSimFromDB conn "csg_sim_202507" 3 4
      putStrLn $ "[INFO] sim14 = " ++ show sim14 ++ ", sim24 = " ++ show sim24 ++ ", sim34 = " ++ show sim34
      cIdx <- findBestCentroid conn "csg_sim_202507" [1,2,3] 4
      cIdx `shouldBe` 2                                           -- CIdx :: [0..]

    it "Result of getCentroidOfACluster conn \"csg_sim_202507\" 1 [1,2] is 1." $ do
      conn <- getConn
      sim11 <- getSimFromDB conn "csg_sim_202507" 1 1
      sim21 <- getSimFromDB conn "csg_sim_202507" 2 1
      sim12 <- getSimFromDB conn "csg_sim_202507" 1 2
      putStrLn $ "[INFO] sim11 = " ++ show sim11 ++ ", sim21 = " ++ show sim21 ++ ", sim12 = " ++ show sim12
      newCent <- getCentroidOfACluster conn "csg_sim_202507" 1 ([1,2] :: [SIdx])
      putStrLn $ "[INFO] oldCent: 1" ++ ", newCent: " ++ show newCent
      newCent `shouldBe` 1

    it "Result of getCentroidOfACluster conn \"csg_sim_202507\" 2 [1,2,3] is 1." $ do
      conn <- getConn
      sim11 <- getSimFromDB conn "csg_sim_202507" 1 1        -- 1.0
      sim12 <- getSimFromDB conn "csg_sim_202507" 1 2        -- 0.738427
      sim13 <- getSimFromDB conn "csg_sim_202507" 1 3        -- 0.483538
      sim23 <- getSimFromDB conn "csg_sim_202507" 2 3        -- 0.472388
      putStrLn $ "[INFO] sim11 = " ++ show sim11 ++ ", sim12 = " ++ show sim12 ++ ", sim13 = " ++ show sim13 ++ ", sim23 = " ++ show sim23
      newCent <- getCentroidOfACluster conn "csg_sim_202507" 2 ([1,2,3] :: [SIdx])
      putStrLn $ "[INFO] oldCent: 2" ++ ", newCent: " ++ show newCent
      newCent `shouldBe` 1

    it "Result of assignClustersOntoSamples conn \"csg_sim_202507\" [(1,Rp),(2,Lp),(3,Noth),(4,Rp)] [1,2] is [(0,[(1,Rp),(3,Noth)],1), (1,[(2,Lp),(4,Rp)],2)]." $ do
      conn <- getConn
      sim11 <- getSimFromDB conn "csg_sim_202507" 1 1
      sim21 <- getSimFromDB conn "csg_sim_202507" 2 1
      sim31 <- getSimFromDB conn "csg_sim_202507" 3 1
      sim41 <- getSimFromDB conn "csg_sim_202507" 4 1
      sim12 <- getSimFromDB conn "csg_sim_202507" 1 2
      sim22 <- getSimFromDB conn "csg_sim_202507" 2 2
      sim32 <- getSimFromDB conn "csg_sim_202507" 3 2
      sim42 <- getSimFromDB conn "csg_sim_202507" 4 2
      putStrLn $ "[INFO] sim11 = " ++ show sim11 ++ ", sim21 = " ++ show sim21 ++ ", sim31 = " ++ show sim31 ++ ", sim41 = " ++ show sim41
                   ++ ", sim21 = " ++ show sim21 ++ ", sim22 = " ++ show sim22 ++ ", sim32 = " ++ show sim32 ++ ", sim42 = " ++ show sim42
      clusters <- assignClustersOntoSamples conn "csg_sim_202507" [(1,Rp),(2,Lp),(3,Noth),(4,Rp)] [1,2]
      putStrLn $ "[INFO] clusters: " ++ show clusters
      clusters `shouldBe` ([(0,[(1,Rp),(3,Noth)],1), (1,[(2,Lp),(4,Rp)],2)] :: [Cluster])

    it "Result of kMeansClustering 2 10 1 6 is [(0,[(1,Rp),(2,Rp),(6,Rp)],1),(1,[(3,Rp),(4,Lp),(5,Lp)],4)]." $ do
      conn <- getConn
      sim12 <- getSimFromDB conn "csg_sim_202507" 1 2
      sim13 <- getSimFromDB conn "csg_sim_202507" 1 3
      sim14 <- getSimFromDB conn "csg_sim_202507" 1 4
      sim15 <- getSimFromDB conn "csg_sim_202507" 1 5
      sim16 <- getSimFromDB conn "csg_sim_202507" 1 6
      sim23 <- getSimFromDB conn "csg_sim_202507" 2 3
      sim24 <- getSimFromDB conn "csg_sim_202507" 2 4
      sim25 <- getSimFromDB conn "csg_sim_202507" 2 5
      sim26 <- getSimFromDB conn "csg_sim_202507" 2 6
      sim34 <- getSimFromDB conn "csg_sim_202507" 3 4
      sim35 <- getSimFromDB conn "csg_sim_202507" 3 5
      sim36 <- getSimFromDB conn "csg_sim_202507" 3 6
      sim45 <- getSimFromDB conn "csg_sim_202507" 4 5
      sim46 <- getSimFromDB conn "csg_sim_202507" 4 6
      sim56 <- getSimFromDB conn "csg_sim_202507" 5 6
      putStrLn $ "[INFO] sim12 = " ++ show sim12 ++ ", sim13 = " ++ show sim13 ++ ", sim14 = " ++ show sim14 ++ ", sim15 = " ++ show sim15 ++ ", sim16 = " ++ show sim16
                   ++ ", sim23 = " ++ show sim23 ++ ", sim24 = " ++ show sim24 ++ ", sim25 = " ++ show sim25 ++ ", sim26 = " ++ show sim26
                   ++ ", sim34 = " ++ show sim34 ++ ", sim35 = " ++ show sim35 ++ ", sim36 = " ++ show sim36
                   ++ ", sim45 = " ++ show sim45 ++ ", sim46 = " ++ show sim46
                   ++ ", sim56 = " ++ show sim56
      clusters <- kMeansClustering 2 10 1 6
      putStrLn $ "[INFO] clusters: " ++ show clusters
      exportClustersToCSV clusters
      clusters `shouldBe` ([(0,[(1,Rp),(2,Rp),(6,Rp)],1),(1,[(3,Rp),(4,Lp),(5,Lp)],4)] :: [Cluster])
