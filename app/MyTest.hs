{-# LANGUAGE OverloadedStrings #-}

module MyTest (
  testCase,
  test1,
  testMySQLRead,
  testClockTime,
  createCateConv2FreqPair,
  createPhraStru2FreqPair,
) where

import Prelude hiding (lookup)
import Data.Map hiding (map)
import Data.List
import Numeric.LinearAlgebra as LA
import qualified System.IO.Streams as S
import Data.Time.Clock
import Data.CSV as CSV
import Data.Time.Clock
import Database
import Database.MySQL.Base
import qualified Data.String as DS
import Rule
import Utils

testCase :: Int -> IO ()
testCase i = case i of
               1 -> putStrLn "1"
               2 -> putStrLn "2"
               _ -> putStrLn "Other"

test1 :: Int -> Int -> IO ()
test1 j max = do
    let loop i
          | i > max = putStrLn $ "[Done]"
          | otherwise = do
              putStrLn $ show i
              loop (i + 1)
    loop j

testClockTime :: IO ()
testClockTime = do
    ct1 <- getCurrentTime
    test1 1 1000
    ct2 <- getCurrentTime
    let time = diffUTCTime ct2 ct1
    putStrLn $ "ct1: " ++ show ct1
    putStrLn $ "ct2: " ++ show ct2
    putStrLn $ "ct1 to ct2: " ++ show time
    putStrLn $ show (read (init (show time)) :: Double)

testMySQLRead :: Int -> Int -> Int -> Int -> IO Int
testMySQLRead idx1min idx1max idx2min idx2max = do
  conn <- getConn
  putStrLn $ "  idx1 <- [" ++ show idx1min ++ ", " ++ show idx1max ++ "], idx2 <- [" ++ show idx2min ++ ", " ++ show idx2max ++ "]"
  putStrLn $ "  Many times of queryStmt ..."
  let sqlstat = DS.fromString $ "Select sim from csg_sim_202507" ++ " where contextofsg1idx = ? and contextofsg2idx = ?"
  stmt <- prepareStmt conn sqlstat
  let idPairList = [(idx1, idx2) | idx1 <- [idx1min .. idx1max], idx2 <- [idx2min .. idx2max]]     -- idx1 is primary order index
  let vList = map (\(idx1, idx2) -> (toMySQLInt16U idx1, toMySQLInt16U idx2)) idPairList  -- [(MySQLValue, MySQLValue)]

  t1 <- getCurrentTime                -- UTCTime
  mapM (\(idx1, idx2) -> do
    (_, is) <- queryStmt conn stmt [idx1, idx2]
    skipToEof is
    ) vList
  t2 <- getCurrentTime                                    -- UTCTime
  putStrLn $ "  Complete time = " ++ show (diffUTCTime t2 t1)

  putStrLn $ "  One time of queryStmt ..."
  let sqlstat = DS.fromString $ "Select sim from csg_sim_202507" ++ " where contextofsg1idx >= ? and contextofsg1idx <= ? and contextofsg2idx >= ? and contextofsg2idx <= ?"
  stmt <- prepareStmt conn sqlstat

  t1 <- getCurrentTime                                    -- UTCTime
  (_, is) <- queryStmt conn stmt [toMySQLInt16U idx1min, toMySQLInt16U idx1max, toMySQLInt16U idx2min, toMySQLInt16U idx2max]
  rows <- S.toList is
  t2 <- getCurrentTime                                    -- UTCTime
  putStrLn $ "  Complete time = " ++ show (diffUTCTime t2 t1)

  close conn
  return 0

-- For every kind of category conversion, occuring numbers in CCG-C2 treebank and Pure-CCG treebank are paired.
createCateConv2FreqPair :: IO ()
createCateConv2FreqPair = do
    let conv2FreqForCCGC2 = (map (\x -> ((getRuleFromStr .fst) x, (read (snd x) :: Int))). map stringToTuple . stringToList) "[(A/n,428),(O/v,115),(Cv/v,113),(N/v,110),(A/v,107),(Hn/v,98),(D/a,60),(Cn/n,54),(N/a,47),(P/a,44),(O/s,43),(P/vt,38),(D/v,31),(Da/d,30),(Jf/c,28),(N/s,26),(Hn/d,25),(Ca/a,25),(Cv/d,22),(D/n,20),(O/a,17),(N/d,16),(A/s,16),(Ds/d,15),(ADJ/n,15),(Cn/a,14),(Cv/a,13),(P/s,13),(N/oe,12),(S/v,11),(Hn/s,11),(O/d,10),(S/a,9),(A/d,8),(Hn/a,8),(OE/vt,8),(Vt/vi,5),(V/n,4),(P/n,4),(Cn/v,4),(N/pe,3),(Hn/oe,3),(Dx/d,3),(Da/a,3),(S/s,3),(A/q,2),(O/oe,2),(D/p,2),(Da/n,2),(Cv/n,2),(U3d/u3,1),(Jb/c,1),(Doe/d,1),(ADJ/d,1),(S/d,1),(Hn/nd,1),(V/a,1),(A/vd,1)]"
    let conv2FreqForPCCG = (map (\x -> ((getRuleFromStr .fst) x, (read (snd x) :: Int))). map stringToTuple . stringToList) "[(A/n,420),(Cv/v,112),(O/v,112),(A/v,110),(N/v,109),(Hn/v,97),(D/a,61),(Cn/n,51),(N/a,48),(P/a,46),(O/s,46),(P/vt,37),(Da/d,34),(Ca/a,26),(N/s,26),(D/v,22),(Hn/d,21),(Jf/c,19),(D/n,19),(N/d,17),(O/a,16),(A/s,16),(Ds/d,15),(ADJ/n,15),(Cv/a,15),(P/s,14),(N/oe,13),(S/v,12),(A/d,11),(Hn/s,11),(O/d,9),(S/a,9),(Hn/a,7),(N/pe,4),(V/n,4),(P/n,4),(Da/a,4),(Hn/oe,3),(O/oe,3),(Vt/vi,3),(Cn/v,3),(A/q,2),(S/s,2),(U3d/u3,1),(ADJ/d,1),(S/d,1),(Hn/nd,1),(Da/n,1),(V/a,1)]"
    putStrLn $ "conv2FreqForCCGC2: " ++ show conv2FreqForCCGC2 ++ ", count " ++ (show . length) conv2FreqForCCGC2
    putStrLn $ "conv2FreqForPCCG: " ++ show conv2FreqForPCCG ++ ", count " ++ (show . length) conv2FreqForPCCG

    let uniConvRuleList4CCGC2 = map fst conv2FreqForCCGC2
    let uniConvRuleList4PCCG = map fst conv2FreqForPCCG
    let missedConvRuleList = [x | x <- uniConvRuleList4CCGC2, notElem x uniConvRuleList4PCCG]
    putStrLn $ "missedConvRuleList: " ++ show missedConvRuleList ++ ", count " ++ (show . length) missedConvRuleList

    let missedConv2FreqList = zip missedConvRuleList (replicate (length missedConvRuleList) 0)
    let conv2FreqForPCCG' = conv2FreqForPCCG ++ missedConv2FreqList
    putStrLn $ "conv2FreqForPCCG': " ++ show conv2FreqForPCCG' ++ ", count " ++ (show . length) conv2FreqForPCCG'

    let conv2FreqPairList = [(fst x, (snd x, snd y)) | x <- conv2FreqForCCGC2, y <- conv2FreqForPCCG', fst x == fst y]
    putStrLn $ "conv2FreqPairList: " ++ show conv2FreqPairList ++ ", count " ++ (show . length) conv2FreqPairList
{-
    putStrLn "Category Conversions: "
    mapM_ (\cate -> putStrLn (show cate)) (map fst conv2FreqPairList)
    putStrLn "CCGC2 occuring numbers: "
    mapM_ (\freq -> putStrLn (show freq)) (map (fst . snd) conv2FreqPairList)
    putStrLn "PCCG occuring numbers: "
    mapM_ (\freq -> putStrLn (show freq)) (map (snd . snd) conv2FreqPairList)
 -}
    let freqTotalForCCGC2 = sum (map (fst . snd) conv2FreqPairList)
    let freqTotalForPCCG = sum (map (snd . snd) conv2FreqPairList)
    putStrLn $ "CCGC2 total occuring number: " ++ show freqTotalForCCGC2
    putStrLn $ "PCCG total occuring number: " ++ show freqTotalForPCCG

-- For every kind of phrasal structure, occuring numbers in CCG-C2 treebank and Pure-CCG treebank are paired.
createPhraStru2FreqPair :: IO ()
createPhraStru2FreqPair = do
    let phraStru2FreqForCCGC2 = (map (\x -> (fst x, (read (snd x) :: Float))). map stringToTuple . stringToList) "[(AHn,0.2503),(VO,0.1393),(DHv,0.1163),(SP,0.0818),(U1P,0.0730),(HX,0.0550),(XX,0.0546),(HvC,0.0511),(PO,0.0453),(HnC,0.0310),(MQ,0.0226),(CC,0.0125),(DHd,0.0100),(OE,0.0088),(DHa,0.0086),(HmC,0.0049),(U5P,0.0047),(MOv,0.0043),(TP,0.0037),(KP,0.0035),(DHs,0.0029),(PE,0.0029),(PQ,0.0029),(U2P,0.0027),(MOs,0.0023),(U3P,0.0021),(U6P,0.0010),(DHx,0.0008),(HaC,0.0008),(DHoe,0.0002)]"
    let phraStru2FreqForPCCG = (map (\x -> (fst x, (read (snd x) :: Float))). map stringToTuple . stringToList) "[(AHn,0.2484),(VO,0.1396),(DHv,0.1143),(SP,0.0823),(U1P,0.0729),(HX,0.0573),(HvC,0.0524),(XX,0.0520),(PO,0.0453),(HaC,0.0348),(HnC,0.0309),(DHd,0.0131),(OE,0.0092),(CC,0.0080),(DHa,0.0078),(U5P,0.0070),(MOv,0.0043),(KP,0.0037),(TP,0.0033),(DHs,0.0029),(PE,0.0029),(U2P,0.0027),(U3P,0.0022),(MOs,0.0012),(U6P,0.0010),(DHas,0.0002),(HP,0.0002)]"
    putStrLn $ "phraStru2FreqForCCGC2: " ++ show phraStru2FreqForCCGC2 ++ ", count " ++ (show . length) phraStru2FreqForCCGC2
    putStrLn $ "phraStru2FreqForPCCG: " ++ show phraStru2FreqForPCCG ++ ", count " ++ (show . length) phraStru2FreqForPCCG

    let phraStruList4CCGC2 = map fst phraStru2FreqForCCGC2
    let phraStruList4PCCG = map fst phraStru2FreqForPCCG
    let phraStruOnlyInCCGC2 = [x | x <- phraStruList4CCGC2, notElem x phraStruList4PCCG]
    let phraStruOnlyInPCCG = [x | x <- phraStruList4PCCG, notElem x phraStruList4CCGC2]
    putStrLn $ "phraStruOnlyInCCGC2: " ++ show phraStruOnlyInCCGC2 ++ ", count " ++ (show . length) phraStruOnlyInCCGC2
    putStrLn $ "phraStruOnlyInPCCG: " ++ show phraStruOnlyInPCCG ++ ", count " ++ (show . length) phraStruOnlyInPCCG

    let missedPhraStru2FreqListForPCCG = zip phraStruOnlyInCCGC2 (replicate (length phraStruOnlyInCCGC2) 0)
    let phraStru2FreqForPCCG' = phraStru2FreqForPCCG ++ missedPhraStru2FreqListForPCCG
    putStrLn $ "phraStru2FreqForPCCG': " ++ show phraStru2FreqForPCCG' ++ ", count " ++ (show . length) phraStru2FreqForPCCG'

    let missedPhraStru2FreqListForCCGC2 = zip phraStruOnlyInPCCG (replicate (length phraStruOnlyInPCCG) 0)
    let phraStru2FreqForCCGC2' = phraStru2FreqForCCGC2 ++ missedPhraStru2FreqListForCCGC2
    putStrLn $ "phraStru2FreqForCCGC2': " ++ show phraStru2FreqForCCGC2' ++ ", count " ++ (show . length) phraStru2FreqForCCGC2'

    let phraStru2FreqPairList = [(fst x, (snd x, snd y)) | x <- phraStru2FreqForCCGC2', y <- phraStru2FreqForPCCG', fst x == fst y]
    putStrLn $ "phraStru2FreqPairList: " ++ show phraStru2FreqPairList ++ ", count " ++ (show . length) phraStru2FreqPairList

    putStrLn "Phrasal structures: "
    mapM_ (\stru -> putStrLn stru) (map fst phraStru2FreqPairList)
    putStrLn "CCGC2 occuring numbers: "
    mapM_ (\freq -> putStrLn (show freq)) (map (fst . snd) phraStru2FreqPairList)
    putStrLn "PCCG occuring numbers: "
    mapM_ (\freq -> putStrLn (show freq)) (map (snd . snd) phraStru2FreqPairList)

    let freqTotalForCCGC2 = sum (map (fst . snd) phraStru2FreqPairList)
    let freqTotalForPCCG = sum (map (snd . snd) phraStru2FreqPairList)
    putStrLn $ "CCGC2 total occuring number: " ++ show freqTotalForCCGC2
    putStrLn $ "PCCG total occuring number: " ++ show freqTotalForPCCG
