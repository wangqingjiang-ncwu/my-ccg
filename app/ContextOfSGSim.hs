{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}

-- Copyright (c) 2019-2025 China University of Water Resources and Electric Power
-- All rights reserved.
-- This module was originally written by Han DONG at 2025, which serves as creating similarity degree between every pair of ContestOfSG.

module ContextOfSGSim (
    initPairwiseSimCache,
    pairwiseSim,
    rowToContext,
    getConfProperty,
    getAllContexts,
    computeAndStoreAllSimsFromMap,
    computeAllContextSims,
    computeContextSimByBlock,
    computeContextSimByBlockFull,
) where

import System.IO
import Database.MySQL.Base
import qualified System.IO.Streams as SIO
import Control.Monad (forM_, unless, when)
import Data.List (tails)
import Data.String (fromString)
import qualified Data.Map as M
import Data.Function ((&))

import Streaming
import qualified Streaming.Prelude as S
--import qualified Streamly.Prelude as S

import Clustering (getContextOfSGPairSimWithStaticOT, getContext2ClauTagPriorBase, getOneToAllContextOfSGSim', getOneToAllContextOfSGSim)
import AmbiResol (LeftExtend, LeftOver, RightOver, RightExtend, OverType, ContextOfSG, readPhraSynListFromStr, readPhraSynFromStr, ClauTagPrior)
import Database (getConn, fromMySQLInt32U, fromMySQLText, fromMySQLInt8)

import Data.IORef
import System.IO.Unsafe (unsafePerformIO)
import Data.Maybe (fromMaybe)
import Control.Applicative ((<|>))
import System.Directory (doesFileExist)


-- 全局缓存避免重复计算
{-# NOINLINE simCache #-}
simCache :: IORef (M.Map (ContextOfSG, ContextOfSG) Double)
simCache = unsafePerformIO $ newIORef M.empty

initPairwiseSimCache :: IO ()
initPairwiseSimCache = writeIORef simCache M.empty

canonicalPair :: (ContextOfSG, ContextOfSG) -> (ContextOfSG, ContextOfSG)
canonicalPair (a, b) = if a <= b then (a, b) else (b, a)

pairwiseSim :: ContextOfSG -> ContextOfSG -> IO Double
pairwiseSim ctx1 ctx2 = do
  let key = canonicalPair (ctx1, ctx2)
  cache <- readIORef simCache
  case M.lookup key cache of
    Just sim -> return sim
    Nothing -> do
      let base = [(ctx1, []), (ctx2, [])]                                       -- [(ContextOfSG, ClauTagPrior)]
      (_, simPairs) <- getContextOfSGPairSimWithStaticOT base                   -- Only two ContextOfSG samples are used for calculate their similarity, that is wrong.
      let sim = fromMaybe 0.0 (lookup (ctx1, ctx2) simPairs <|> lookup (ctx2, ctx1) simPairs)
      modifyIORef' simCache (M.insert key sim)
      return sim


-- 将MySQL查询结果的一行数据转换为(Int, ContextOfSG)元组。
rowToContext :: [MySQLValue] -> (Int, ContextOfSG)
rowToContext [v1, v2, v3, v4, v5, v6] =
  ( fromMySQLInt32U v1
  , ( readPhraSynListFromStr (fromMySQLText v2)
    , readPhraSynFromStr (fromMySQLText v3)
    , readPhraSynFromStr (fromMySQLText v4)
    , readPhraSynListFromStr (fromMySQLText v5)
    , fromMySQLInt8 v6
    )
  )
rowToContext _ = error "rowToContext: invalid row format"


-- 从配置文件中获取某个键的值
getConfProperty :: String -> String -> String
getConfProperty key content =
  let kvs = map (break (== ':')) (lines content)
  in case lookup key [(k, drop 1 v) | (k, v) <- kvs] of
       Just val -> val
       Nothing  -> error $ "Key not found: " ++ key

{-
-- 从数据库中读取所有上下文样本
使用query_执行查询，并返回结果集；将结果集转为列表行
-}
getAllContexts :: MySQLConn -> IO [(Int, ContextOfSG)]
getAllContexts conn = do
  conf <- readFile "Configuration"
  let tableName = getConfProperty "syntax_ambig_resol_model" conf
      sqlStr = "SELECT id, leftExtend, leftOver, rightOver, rightExtend, overType FROM " ++ tableName
  (_, is) <- query_ conn (fromString sqlStr)
  rows <- SIO.toList is
  return $ map rowToContext rows


-- 插入两两相似度记录
--使用 execute 执行带参数的 SQL 插入
insertSimilarity :: MySQLConn -> String -> (Int, Int, Double) -> IO ()
insertSimilarity conn tableName (id1, id2, sim) = do
  let (idA, idB) = if id1 <= id2 then (id1, id2) else (id2, id1)
  let sql = " INSERT IGNORE INTO " ++ tableName ++ " (id1, id2, ContextOfSGSim) VALUES (?, ?, ?) "
  _ <- execute conn (fromString sql)
          [ MySQLInt32 (fromIntegral idA)
          , MySQLInt32 (fromIntegral idB)
          , MySQLDouble sim
          ]
  return ()

-- 构造 ContextOfSG -> ID 的反向 Map
--获取所有 (ID, ContextOfSG)；构造 Map，以 ContextOfSG 为键，ID 为值；
--fromList 函数，将键值对列表转换为 Map；
getContextMap :: MySQLConn -> IO (M.Map ContextOfSG Int)
getContextMap conn = do
  contextList <- getAllContexts conn
  return $ M.fromList [ (ctx, cid) | (cid, ctx) <- contextList ]

{-
-- 使用 getContextOfSGPairSimWithStaticOT 批量计算并写入数据库
-- 根据 ContextOfSG 对和相似度 Map 写入数据库
--遍历传入的相似度 Map，将其转换为列表并逐项处理。
--查找当前两个 ContextOfSG 是否在 Map 中存在对应的 ID。
--如果都存在，则调用 insertSimilarity 插入数据库；否则输出警告信息。
-}
computeAndStoreAllSimsFromMap :: IO ()
computeAndStoreAllSimsFromMap = do
  conn <- getConn
  conf <- readFile "Configuration"
  let targetTable = getConfProperty "context_sim_table" conf
--  contextMap <- getContextMap conn    -- Map ContextOfSG Int
--  putStrLn $ "computeAndStoreAllSimsFromMap: contextMap.size = " ++ show (M.size contextMap)
  -- 读取全部上下文与Prior
  contextWithPriorBase <- getContext2ClauTagPriorBase 0 0                       -- [(ContextOfSG, ClauTagPrior)]
  let contextOnly = map fst contextWithPriorBase
  putStrLn $ (++) "computeAndStoreAllSimsFromMap: length contextOnly = " $ show $ length contextOnly
  -- 执行批量相似度计算,simPairs 是 [((ctx1, ctx2), sim)] 的相似度列表；
  (_, simPairs) <- getContextOfSGPairSimWithStaticOT contextWithPriorBase       -- ([(SimDeg, SimDeg, SimDeg, SimDeg, SimDeg)], [((ContextOfSG, ContextOfSG), SimDeg)])
  putStrLn $ (++) "computeAndStoreAllSimsFromMap: length simPairs = " $ show $ length simPairs
  -- 获取 ContextOfSG -> ID 映射，用于后续插入数据库中
  contextMap <- getContextMap conn       -- M.Map ContextOfSG Int

  -- 遍历写入数据库
  forM_ simPairs $ \((ctx1, ctx2), sim) -> do
    case (M.lookup ctx1 contextMap, M.lookup ctx2 contextMap) of
      (Just id1, Just id2) -> insertSimilarity conn targetTable (id1, id2, sim)
      _ -> putStrLn "[Warning] ContextOfSG ID not found. Skipping."
  close conn

-- 不再使用（保留旧接口）
computeAllContextSims :: IO ()
computeAllContextSims = computeAndStoreAllSimsFromMap

{-1
-- 分块执行：按 blockSize 批量加载上下文并计算相似度，避免内存溢出
computeContextSimByBlock :: Int -> Int -> Int -> IO ()
computeContextSimByBlock blockSize startIdx endIdx = do
  conn <- getConn
  conf <- readFile "Configuration"
  let targetTable = getConfProperty "context_sim_table" conf
  contextMap <- getContextMap conn

  let loop i
        | i > endIdx = putStrLn "[Done] All blocks processed."
        | otherwise = do
            let j = min endIdx (i + blockSize - 1)
            putStrLn $ "\n[Block] Processing ID range: " ++ show i ++ " - " ++ show j

            contextWithPriorBase <- getContext2ClauTagPriorBase i j             -- [(ContextOfSG, ClauTagPrior)]
            putStrLn $ "[Info] Block size loaded: " ++ show (length contextWithPriorBase)

            (_, simPairs) <- getContextOfSGPairSimWithStaticOT contextWithPriorBase
            putStrLn $ "[Info] Block pairs computed: " ++ show (length simPairs)

            forM_ (zip [1..] simPairs) $ \(idx, ((ctx1, ctx2), sim)) -> do
              putStrLn $ "[Progress] In block pair " ++ show idx ++ "/" ++ show (length simPairs)
              case (M.lookup ctx1 contextMap, M.lookup ctx2 contextMap) of
                (Just id1, Just id2) -> insertSimilarity conn targetTable (id1, id2, sim)
                _ -> putStrLn "[Warning] ContextOfSG ID not found. Skipping."

            loop (j + 1)


            zipWithM_ (\idx ((ctx1, ctx2), sim) -> do
                when (idx `mod` 10 == 0) $ putStrLn $ "[Progress] In block pair " ++ show idx ++ "/" ++ show (length simPairs)
                case (M.lookup ctx1 contextMap, M.lookup ctx2 contextMap) of
                  (Just id1, Just id2) -> insertSimilarity conn targetTable (id1, id2, sim)
                  _ -> putStrLn "[Warning] ContextOfSG ID not found. Skipping."
              ) [1..] simPairs

            loop (j + 1)

  loop startIdx
  close conn
-}


{-3
-- 分块计算上下文相似度（仅使用前 200 个上下文进行组合）
computeContextSimByBlock :: Int -> Int -> Int -> IO ()
computeContextSimByBlock blockSize startIdx endIdx = do
  conn <- getConn
  conf <- readFile "Configuration"
  let targetTable = getConfProperty "context_sim_table" conf
  contextMap <- getContextMap conn                                              -- M.Map ContextOfSG Int

  let loop i
        | i > endIdx = putStrLn "[Done] All blocks processed."
        | otherwise = do
            let j = min endIdx (i + blockSize - 1)
            putStrLn $ "\n[Block] Processing ID range: " ++ show i ++ " - " ++ show j

            contextWithPriorBaseFull <- getContext2ClauTagPriorBase i j         -- [(ContextOfSG, ClauTagPrior)]
            let contextWithPriorBase = take 200 contextWithPriorBaseFull
            putStrLn $ "[Info] Block size loaded (limited to 200): " ++ show (length contextWithPriorBase)

            (_, simPairs) <- getContextOfSGPairSimWithStaticOT contextWithPriorBase
            putStrLn $ "[Info] Block pairs computed: " ++ show (length simPairs)

            zipWithM_ (\idx ((ctx1, ctx2), sim) -> do
                when (idx `mod` 100 == 0) $
                  putStrLn $ "[Progress] In block pair " ++ show idx ++ "/" ++ show (length simPairs)
                case (M.lookup ctx1 contextMap, M.lookup ctx2 contextMap) of
                  (Just id1, Just id2) -> insertSimilarity conn targetTable (id1, id2, sim)
                  _ -> putStrLn "[Warning] ContextOfSG ID not found. Skipping."
              ) [1..] simPairs

            loop (j + 1)

  loop startIdx
  close conn
-}

-- streaming 块内计算
computeContextSimByBlock :: Int -> Int -> Int -> IO ()
computeContextSimByBlock blockSize startIdx endIdx = do
  initPairwiseSimCache
  conn <- getConn
  conf <- readFile "Configuration"
  let targetTable = getConfProperty "context_sim_table" conf
  contextMap <- getContextMap conn              -- M.Map ContextOfSG Int

  let loop i
        | i > endIdx = putStrLn "[Done] All blocks processed."
        | otherwise = do
            let j = min endIdx (i + blockSize - 1)
            putStrLn $ "\n[Block] Streaming pairwise sim in ID range: " ++ show i ++ " - " ++ show j

            contextWithPriorBase <- getContext2ClauTagPriorBase i j             -- [(ContextOfSG, Prior)]
            let contextList = map fst contextWithPriorBase                      -- [ContextOfSG]

            let pairStream = S.each [ (x, y) | x <- contextList, y <- contextList, x <= y ]

            S.zip (S.each [1..]) pairStream                                     -- [(Int, (ContextOfSG, ContextOfSG))]
              & S.mapM_ (\(idx, (ctx1, ctx2)) -> do
                  when (idx `mod` 1000 == 0) $
                    putStrLn $ "[Progress] In pair " ++ show idx
                  case (M.lookup ctx1 contextMap, M.lookup ctx2 contextMap) of
                    (Just id1, Just id2) -> do
                      sim <- pairwiseSim ctx1 ctx2
                      insertSimilarity conn targetTable (id1, id2, sim)
                    _ -> return ())

            loop (j + 1)

  loop startIdx
  close conn


-- 带断点记录的全组合流式计算
computeContextSimByBlockFull :: Int -> Int -> Int -> IO ()
computeContextSimByBlockFull blockSize startIdx endIdx = do
  initPairwiseSimCache
  conn <- getConn
  conf <- readFile "Configuration"
  let targetTable = getConfProperty "context_sim_table" conf
  contextMap <- getContextMap conn

  let blocks = [(i, min endIdx (i + blockSize - 1)) | i <- [startIdx, startIdx + blockSize .. endIdx]]
  let logFile = ".context_sim_block.log"

  -- 读取已完成的块对标记
  donePairs <- doesFileExist logFile >>= \exists ->
    if exists then lines <$> readFile logFile else return []

  forM_ blocks $ \(iStart, iEnd) -> do
    contextA <- getContext2ClauTagPriorBase iStart iEnd
    forM_ blocks $ \(jStart, jEnd) -> do
      when (jStart >= iStart) $ do
        let tag = show iStart ++ "," ++ show jStart
        unless (tag `elem` donePairs) $ do
          contextB <- getContext2ClauTagPriorBase jStart jEnd
          putStrLn $ "[Info] Streaming sim for block pair (" ++ show iStart ++ "," ++ show jStart ++ ")"

          -- 正确解构 contextWithPriorBase 为 (ContextOfSG, [Prior])
          let pairStream = S.each [ (ctx1, ctx2) | (ctx1, _) <- contextA, (ctx2, _) <- contextB, iStart /= jStart || ctx1 <= ctx2 ]


          -- 流式处理所有 (ctx1, ctx2)
          S.zip (S.each [1..]) pairStream
            & S.mapM_ (\(idx, (ctx1, ctx2)) -> do
                when (idx `mod` 1000 == 0) $
                  putStrLn $ "[Progress] In pair " ++ show idx
                case (M.lookup ctx1 contextMap, M.lookup ctx2 contextMap) of
                  (Just id1, Just id2) -> do
                    sim <- pairwiseSim ctx1 ctx2
                    insertSimilarity conn targetTable (id1, id2, sim)
                  _ -> return ())

          -- 写入断点日志
          appendFile logFile (tag ++ "\n")

  close conn
