{-# LANGUAGE OverloadedStrings, LambdaCase #-}

-- Copyright (c) 2019-2021 China University of Water Resources and Electric Power
-- All rights reserved.

module Statistics (
    countInTree,                    -- Int -> Int -> Int -> IO ()
    countInScript,                  -- Int -> Int -> Int -> IO ()
    countInStruGene,                -- Int -> IO ()
    formatMapListWithFloatValue,    -- [(String,Float)] -> Int -> [(String,String)]
    truncDescListByProportion       -- Float -> [(String, Int)] -> [(String, Int)]
    ) where

import Control.Monad
import qualified System.IO.Streams as S
import Database.MySQL.Base
import Data.List.Utils
import Data.Tuple.Utils
import Text.Printf
import qualified Data.Tuple as Tuple
import Data.Map (Map)
import qualified Data.Map as Map
import Category
import Phrase
--import Phrase (Tag,taOfCate,psOfCate,PhraStru,Act,PhraCate,getPhraCateFromString,nPhraCateToString,getPhraBySpan)
import Rule
import Utils
import Database
import Corpus

{- The following functions read field 'tree' of table 'corpus', then count all clauses according the input index.
 - 1. Get total number of sentences;
 - 2. Get clausal number in every sentence;
 - 3. Get averge clausal number per sentence;
 - 4. Get clausal length in every sentence;
 - 5. Get phrase number of every clause in every sentence;
 - 6. Get clausal number of different clausal lengths;
 - 7. Get clausal number of different parsing-tree depths;
 - 8. Get frequency total and normalized frequencies of different CCG tags;
 - 9. Get frequency total and normalized frequencies of different phrasal structures;
 - A. Get frequency total and normalized frequencies of different type-tag-stru(s);

 - The following functions read field 'script' of table 'corpus', then count all clauses according the input index.
 - 1. Get transitive times of every clause in all sentences;
 - 2. Get frequencies of different numbers of ransitive times in all clause parsing;
 - 3. Get conversion-ruled list in parsing every clause;
 - 4. Get the number of abandoned phrases in parsing every clause of every sentences;
 - 5. Get the number of abandoned not-recognizable phrases in parsing every clause of every sentence;

 - The following functions read Table 'stru_gene', then count all structural genes according the input index.
 - 1. Get total number of structural genes;
 - 2. Get frequencies of different overlapping types, namely [(OverType, Int)];
 - 3. Get frequencies of most common phrasal overlapping (LROs) by given common proportion;
 - 4. Get frequencies of most common unambiguous phrasal overlapping (LROPs) by given common proportion;
 - 5. Get descending hitCount
 -}

{- Get statistics about field 'tree' in Table 'corpus' whose serial numbers are less than 'topSn' and
 - bigger than or equal to 'bottomSn', here 'funcIndex' indicates which statistics will be done.
 -}
countInTree :: Int -> Int -> Int -> IO ()
countInTree bottomSn topSn funcIndex = do
    conn <- getConn
    stmt <- prepareStmt conn "select tree from corpus where serial_num >= ? and serial_num < ?"
    (defs, is) <- queryStmt conn stmt [toMySQLInt32 bottomSn, toMySQLInt32 topSn]

    sentStrList <- readStreamByText [] is                           -- [String], here a string is the parsing result of a sentence.
    let sentClauStrList = map stringToList sentStrList        -- [[String]], here a string is the parsing result of a clause.
    let sentClauNumList = map length sentClauStrList          -- [Int], here an integer is the number of clauses in a sentence.
    let sentNum = length sentStrList                          -- The number of sentences.
    let clauseTotalNum = foldl (+) 0 sentClauNumList          -- The total number of clauses.
    let sentClauPhraStrList = toSentClauPhraStrList sentClauStrList
                                                              -- [[[String]]], here a string is for a phrase.
    let sentClauPhraList = toSentClauPhraList sentClauPhraStrList
                                                              -- [[[PhraCate]]], here a PhraCate is the representation in memory of a phrase.

--  Output the following for test.
--  putStrLn $ "countInTree: The list of clauses in first sentence: " ++ show (sentClauPhraList!!0)
--  putStrLn $ "countInTree: The first clause in first sentence: " ++ show ((sentClauPhraList!!0)!!0)

    if funcIndex == 1                                          -- To get the number of sentences.
       then putStrLn $ "countInTree: The number of sentences: " ++ show sentNum
       else putStr ""                                          -- No operation for 'else' condition

    if funcIndex == 2                                          -- To get the number of clauses in every sentence.
       then putStrLn $ "countInTree: The number of clauses in every sentence: " ++ show sentClauNumList
       else putStr ""

    if funcIndex == 3                                          -- To get the number of sentences, the number of clauses in every
                                                               -- sentence, and the averge number of clauses per sentence.
       then do
         putStrLn $ "countInTree: The number of sentences: " ++ show sentNum
         putStrLn $ "countInTree: The total number of clauses: " ++ show clauseTotalNum ++ ", average clause number per sentence: " ++ show (fromIntegral clauseTotalNum / fromIntegral sentNum)
       else putStr ""

    if funcIndex == 4                                          -- To get the length of every clause in every sentence.
       then do
         let sentClauLengthList = toSentClauLengthList sentClauPhraList                 -- [[[Int]]], here every integer is the length of a clause.
         putStrLn $ "countInTree: Length of every clause in every sentence: " ++ show sentClauLengthList
      else putStr $ ""

    if funcIndex == 5                                          -- To get phrase number of every clause in every sentence. Actually, phrase number can be obtained from clause length.
       then do
         let sentClauPhraNumList = map (map length) sentClauPhraList                    -- [[[Int]]], here every integer is the phrase number of a clause.
         putStrLn $ "countInTree: Phrase number of every clause in every sentence: " ++ show sentClauPhraNumList
       else putStr ""

    if funcIndex == 6                                          -- To get the number of clauses with different lengths.
       then do
         let sentClauLengthList = toSentClauLengthList sentClauPhraList                 -- [[[Int]]], here every integer is the length of a clause.
         let clauLength2NumMap = toClauLength2NumMap sentClauLengthList Map.empty       -- Map Int Int, namely Map <clauLength> <clauNum>, and 'empty' return null Map.
         putStrLn $ "countInTree: Clause count by clause length: " ++ show clauLength2NumMap
       else putStr ""

    if funcIndex == 7                                          -- To get the number of clauses with different depths.
       then do
         let sentClauDepthList = toSentClauDepthList sentClauPhraList           -- [[Int]], here every integer is the depth of a clause.
         let clauDepth2NumMap = toClauDepth2NumMap sentClauDepthList Map.empty
                                                                -- Map Int Int, namely Map <clauDepth> <clauNum>, and 'empty' return null Map.
         putStrLn $ "countInTree: The list of depth of clauses with different lengths: " ++ show sentClauDepthList
         putStrLn $ "countInTree: The list of number of clauses with different depths: " ++ show clauDepth2NumMap

--       let clauseTotalDepth = foldl (+) 0 (concat sentClauDepthList)          -- The total number of clauses.
--       putStrLn $ "countInTree: The averge depth for all clauses: " ++ show (fromIntegral clauseTotalDepth / fromIntegral clauseTotalNum)
       else putStr ""

    if funcIndex == 8                                           -- To get the frequency of every CCG tag in all sentences.
       then do
         let tag2FreqMap = toTag2FreqMap sentClauPhraList Map.empty             -- Map String Int, namely Map Tag <tagNum>.
         let tagFreqMapList = Map.toList (Map.delete "Desig" tag2FreqMap)       -- [(String, Int)], remove tag "Desig".
         let tagFregTotal = foldl (+) 0 (map snd tagFreqMapList)
         let ascListOfTagFreqByValue = toAscListOfMapByValue tagFreqMapList
         let descListOfTagFreqByValue = toDescListOfMapByValue tagFreqMapList
         let tagNormFreqDescList = map (\x -> (fst x, ((/(fromIntegral tagFregTotal)). fromIntegral) (snd x))) descListOfTagFreqByValue   -- Normalized frequencies of different CCG tags.

--       putStrLn $ "countInTree: The list of frequencies of different CCG tags: " ++ show tag2FreqMap
--       putStrLn $ "countInTree: The ascending list of frequencies of different CCG tags: " ++ show ascListOfTagFreqByValue
         putStrLn $ "countInTree: The descending list of frequencies of different CCG tags: " ++ show descListOfTagFreqByValue
         putStrLn $ "countInTree: The number of different CCG tags: " ++ show (length tagFreqMapList)
         putStrLn $ "countInTree: The frequency total of different CCG tags: " ++ show tagFregTotal
--       putStrLn $ "countInTree: The normalized frequencies of different CCG tags: " ++ show tagNormFreqDescList
         putStrLn $ "countInTree: The normalized frequencies of different CCG tags: " ++ show (formatMapListWithFloatValue tagNormFreqDescList 4)
       else putStr ""

    if funcIndex == 9                                          -- To get the frequency of every phrasal structure in all sentences.
       then do
         let phraStru2FreqMap = toPhraStru2FreqMap sentClauPhraList Map.empty         -- Map String Int, namely Map PhraStru <psNum>.
         let phraStru2FreqMapList = Map.toList (Map.delete "DE" phraStru2FreqMap)     -- [(String, Int)], remove phrasal structure "DE".
         let phraStruFreqTotal = foldl (+) 0 (map snd phraStru2FreqMapList)
         let descListOfPhraStruFreqByValue = toDescListOfMapByValue phraStru2FreqMapList
         let phraStruNormFreqDescList = map (\x -> (fst x, ((/(fromIntegral phraStruFreqTotal)). fromIntegral) (snd x))) descListOfPhraStruFreqByValue
                                                                                      -- Normalized frequencies of different CCG tags.
         putStrLn $ "countInTree: The number of different phrasal structures: " ++ show (length phraStru2FreqMapList)
         putStrLn $ "countInTree: The frequency total of different phrasal structures: " ++ show phraStruFreqTotal
         putStrLn $ "countInTree: The frequencies of different phrasal structures: " ++ show descListOfPhraStruFreqByValue
--       putStrLn $ "countInTree: The normalized frequencies of different phrasal structures: " ++ show phraStruNormFreqDescList
         putStrLn $ "countInTree: The normalized frequencies of different phrasal structures: " ++ show (formatMapListWithFloatValue phraStruNormFreqDescList 4)
       else putStr ""

    if funcIndex == 10                                          -- To get the frequency of every triple (Syntactic type, CCG rule tag, Phrasal structure) in all sentences.
       then do
         let typeTagStru2FreqMap = toTypeTagStru2FreqMap sentClauPhraList Map.empty            -- Map String Int, namely Map <type_tag_stru> <ttsNum>.
         let typeTagStru2FreqMapList = Map.toList typeTagStru2FreqMap        -- [(String, Int)]
         let typeTagStruFreqTotal = foldl (+) 0 (map snd typeTagStru2FreqMapList)
         let descListOfTTSByValue = toDescListOfMapByValue typeTagStru2FreqMapList
         let typeTagStruNormFreqDescList = map (\x -> (fst x, ((/(fromIntegral typeTagStruFreqTotal)). fromIntegral) (snd x))) descListOfTTSByValue                                   -- Normalized frequencies of different CCG tags.
         putStrLn $ "countInTree: The number of different type-tag-stru structures: " ++ show (length typeTagStru2FreqMapList)
         putStrLn $ "countInTree: The frequency total of different type-tag-stru(s): " ++ show typeTagStruFreqTotal
         putStrLn $ "countInTree: The frequencies of different type-tag-stru(s): " ++ show descListOfTTSByValue
--       putStrLn $ "countInTree: The normalized frequencies of different type-tag-stru(s): " ++ show typeTagStruNormFreqDescList
         putStrLn $ "countInTree: The normalized frequencies of different type-tag-stru(s): " ++ show (formatMapListWithFloatValue typeTagStruNormFreqDescList 4)
       else putStr ""

{- Get statistics about field 'script' in Table 'corpus' whose serial numbers are less than 'topSn' and
 - bigger than or equal to 'bottomSn', here 'funcIndex' indicates which statistics will be done.
 -}
countInScript :: Int -> Int -> Int -> IO ()
countInScript bottomSn topSn funcIndex = do
    conn <- getConn
    stmt <- prepareStmt conn "select script from corpus where serial_num >= ? and serial_num < ?"
    (defs, is) <- queryStmt conn stmt [toMySQLInt32 bottomSn, toMySQLInt32 topSn]

    sentStrList <- readStreamByText [] is                           -- [String], here a string is the parsing script of a sentence.
    let sentClauStrList = map stringToList sentStrList        -- [[String]], here a string is the parsing script of a clause.
    let sentClauNumList = map length sentClauStrList          -- [Int], here an integer is the number of clauses in a sentence.
    let sentNum = length sentStrList                          -- The number of sentences.
    let clauseTotalNum = foldl (+) 0 sentClauNumList          -- The total number of clauses.
    let sentClauScriptList = toSentClauScriptList sentClauStrList               -- [[Script]], here a Script is the representation in memory of parsing script of a clause.

    -- Output the following for test.
    putStrLn $ "countInScript: The script list of clauses in first sentence: " ++ show (sentClauScriptList!!0)
    putStrLn $ "countInScript: The script of first clause in first sentence: " ++ show ((sentClauScriptList!!0)!!0)

    if funcIndex == 1                                         -- To get transitive times of every clause in all sentences.
       then do
         let sentClauTransTimesList = map (map (length . snd3)) sentClauScriptList
         putStrLn $ "countInScript: The transitive times of every clause in all sentence: " ++ show sentClauTransTimesList
       else putStr ""

    if funcIndex == 2                                         -- To get frequencies of different ransitive times in all clause parsing.
       then do
         let transTimes2FreqMap = toTransTimes2FreqMap sentClauScriptList Map.empty                  -- Map Int Int, namely Map <transTimes> <ttNum>.
         let descListOfTT2FreqByValue = toDescListOfMapByValue (Map.toList transTimes2FreqMap)
         putStrLn $ "countInScript: The descending list of frequencies of different numbers of transitive times: " ++ show descListOfTT2FreqByValue
       else putStr ""

    if funcIndex == 3                                         -- To get conversion-ruled list in parsing every clause.
       then do
         let sentClauConvRuleListList = map (map snd3) sentClauScriptList
         putStrLn $ "countInScript: The conversion-ruled list of every clause in all sentences: " ++ show sentClauConvRuleListList
       else putStr ""

-- To get the number of abandoned phrases in parsing every clause of every sentence, which is useful to evaluate ambiguity of clause syntax.
    if funcIndex == 4
       then do
         let sentClauAbanPhraNumList = map (map (length . thd3)) sentClauScriptList
         putStrLn $ "countInScript: The number of abandoned phrases in parsing every clause: " ++ show sentClauAbanPhraNumList
       else putStr ""

{- To get the number of abandoned not-recognizable phrases in parsing every clause of every sentence,
 - which is useful to evaluate how much proportion of phrases are not accepted by Chinese syntax.
 _ To now, no phrase is not recognizable for the present C^2-CCG.
 -}
    if funcIndex == 5
       then do
         let sentClauAbanNRPhraNumList = toSentClauAbanNRPhraNumList sentClauScriptList
         putStrLn $ "countInScript: The number of abandoned not-recognizable phrases in parsing every clause of every sentence: " ++ show sentClauAbanNRPhraNumList
       else putStr ""

{- Get statistics in table 'stru_gene', here 'funcIndex' indicates which statistics will be done.
 -}
countInStruGene :: Int -> IO ()
countInStruGene funcIndex = do
    -- 1. Get total number of structural genes.
    if funcIndex == 1
       then do
         conn <- getConn
         stmt <- prepareStmt conn "select count(*) from stru_gene"
         (defs, is) <- queryStmt conn stmt []
         row <- S.read is
         let totalNum = case row of                           -- [Int], here a string is the parsing script of a sentence.
                          Just x -> x
                          Nothing -> error "countInStruGene: Failed in executing select count(*) from stru_gene."
         S.skipToEof is
         putStrLn $ "countInStruGene: The total number of structural genes: " ++ show (fromMySQLInt64 (totalNum!!0))
       else putStr ""

    -- 2. Get frequencies of different overlapping types, namely [(OverType, Freq)].
    if funcIndex == 2
       then do
         conn <- getConn
         stmt <- prepareStmt conn "select overType, count(*) from stru_gene group by overType order by overType"
         (defs, is) <- queryStmt conn stmt []
         overType2FrequencyList <- readStreamByInt [] is                  -- [[Int]], here every row has two integers, overType and its occuring frequency.
         putStrLn $ "countInStruGene: Frequencies of different overlapping types [(OverType, Freq)]: " ++ show overType2FrequencyList
       else putStr ""

    -- 3. Get frequencies of most common phrasal overlapping, namely [(LeftOver_RightOver_OverType, Int), here the common proportion is 'prop';
    if funcIndex == 3
       then do
         conn <- getConn
         stmt <- prepareStmt conn "select leftOver,rightOver,overType from stru_gene"
         (defs, is) <- queryStmt conn stmt []
         leftOver_RightOver_OverTypeList <- readStreamByTextTextInt8 [] is           -- [String], here the string is "LeftOver_RightOver_OverType"
         let leftOver_RightOver_OverType2FreqMap = keyToMap leftOver_RightOver_OverTypeList Map.empty           -- Map Sting Int, namely Map <LRO> <LRONum>.
         putStrLn $ "countInStruGene: The total number of different LROs: " ++ show (Map.size leftOver_RightOver_OverType2FreqMap)

         let descListOfLRO2FreqByValue = toDescListOfMapByValue (Map.toList leftOver_RightOver_OverType2FreqMap)
--       putStrLn $ "countInStruGene: The descending list of frequencies of different LROs: " ++ show descListOfLRO2FreqByValue

         putStr "Please input the percent proportion of frequency of most common LROs in frequency of all LROs [0.00-1.00]: "
         prop <- readFloat0to1
         let truncatedDescListOfLRO2FreqByProp = truncDescListByProportion prop descListOfLRO2FreqByValue
         let valueTotal = foldl (+) 0 (map snd descListOfLRO2FreqByValue)
         let valueTrunc = foldl (+) 0 (map snd truncatedDescListOfLRO2FreqByProp)
         let realProp = (/) (fromIntegral valueTrunc) (fromIntegral valueTotal) :: Float
         putStrLn $ "countInStruGene: The truncated descending list of frequencies of different LROs by proportion " ++ (printf "%.02f" realProp) ++ ": " ++ show truncatedDescListOfLRO2FreqByProp
       else putStr ""

    -- 4. Get frequencies of most common unambiguous phrasal overlapping, namely [(LeftOver_RightOver_OverType_Prior, Int), here the common proportion is 'prop';
    if funcIndex == 4
          then do
            conn <- getConn
            stmt <- prepareStmt conn "select leftOver,rightOver,overType,prior from stru_gene"
            (defs, is) <- queryStmt conn stmt []
            leftOver_RightOver_OverType_PriorList <- readStreamByTextTextInt8Text [] is        -- [String], here the string is "LeftOver_RightOver_OverType_Prior"
            let leftOver_RightOver_OverType_Prior2FreqMap = keyToMap leftOver_RightOver_OverType_PriorList Map.empty
                                                                                               -- Map Sting Int, namely Map <LROP> <LROPNum>.
            putStrLn $ "countInStruGene: The total number of different LROPs: " ++ show (Map.size leftOver_RightOver_OverType_Prior2FreqMap)

            let descListOfLROP2FreqByValue = toDescListOfMapByValue (Map.toList leftOver_RightOver_OverType_Prior2FreqMap)
   --       putStrLn $ "countInStruGene: The descending list of frequencies of different LROPs: " ++ show descListOfLROP2FreqByValue

            putStr "Please input the percent proportion of frequency of most common LROPs in frequency of all LROPs [0.00-1.00]: "
            prop <- readFloat0to1
            let truncatedDescListOfLROP2FreqByProp = truncDescListByProportion prop descListOfLROP2FreqByValue
            let valueTotal = foldl (+) 0 (map snd descListOfLROP2FreqByValue)
            let valueTrunc = foldl (+) 0 (map snd truncatedDescListOfLROP2FreqByProp)
            let realProp = (/) (fromIntegral valueTrunc) (fromIntegral valueTotal) :: Float
            putStrLn $ "countInStruGene: The truncated descending list of frequencies of different LROPs by proportion " ++ (printf "%.02f" realProp) ++ ": " ++ show truncatedDescListOfLROP2FreqByProp
          else putStr ""

{- Read a value from input stream [MySQLValue], append it to existed string list, then read the next,
 - until read Nothing.
 -}
readStreamByText :: [String] -> S.InputStream [MySQLValue] -> IO [String]
readStreamByText es is = do
    S.read is >>= \case                                         -- Dumb element 'case' is an array with type [MySQLValue]
        Just [MySQLText v] -> readStreamByText (es ++ [fromMySQLText (MySQLText v)]) is
        Nothing -> return es

{- Read a value from input stream [MySQLValue], append it to existed integer list, then read the next,
 - until read Nothing.
 -}
readStreamByInt :: [[Int]] -> S.InputStream [MySQLValue] -> IO [[Int]]
readStreamByInt es is = do
    S.read is >>= \case                                         -- Dumb element 'case' is an array with type [MySQLValue]
        Just x -> readStreamByInt (es ++ [[fromMySQLInt8 (x!!0), fromMySQLInt64 (x!!1)]]) is
        Nothing -> return es

{- Read a value from input stream [MySQLValue], append it to existed integer list, then read the next,
 - until read Nothing.
 - Here [MySQLValue] is [MySQLText, MySQLText, MySQLInt8].
 -}
readStreamByTextTextInt8 :: [String] -> S.InputStream [MySQLValue] -> IO [String]
readStreamByTextTextInt8 es is = do
    S.read is >>= \case                                         -- Dumb element 'case' is an array with type [MySQLValue]
        Just x -> readStreamByTextTextInt8 (es ++ [fromMySQLText (x!!0) ++ "_" ++ fromMySQLText (x!!1) ++ "_" ++ show (fromMySQLInt8 (x!!2))]) is
        Nothing -> return es

{- Read a value from input stream [MySQLValue], append it to existed integer list, then read the next,
 - until read Nothing.
 - Here [MySQLValue] is [MySQLText, MySQLText, MySQLInt8, MySQLText].
 -}
readStreamByTextTextInt8Text :: [String] -> S.InputStream [MySQLValue] -> IO [String]
readStreamByTextTextInt8Text es is = do
    S.read is >>= \case                                         -- Dumb element 'case' is an array with type [MySQLValue]
        Just x -> readStreamByTextTextInt8Text (es ++ [fromMySQLText (x!!0) ++ "_" ++ fromMySQLText (x!!1) ++ "_" ++ show (fromMySQLInt8 (x!!2)) ++ "_" ++ fromMySQLText (x!!3)]) is
        Nothing -> return es

{- Get representations in memory of parsing results of sentences.
 - <sentsResult> ::= [<sentResult>], a <sentResult> is the parsing result for a sentence.
 - <sentResult> ::= [<clauResult>], a <clauResult> is the parsing result for a clause, where a sentence includes many clauses.
 - <clauResult> ::= [PhraCate], <PhraCate> is the representation in memory for a phrase.
 - So, <sentsResult> ::= [[[PhraCate]]]
 - From the perspective of parsing trees, the results of a clause, a sentence, and a set of sentences are a tree, a forest, and a forest of forests.
 -}
toSentClauPhraStrList :: [[String]] -> [[[String]]]
toSentClauPhraStrList [] = []
toSentClauPhraStrList (sent:sents) = (map stringToList sent) : toSentClauPhraStrList sents

{- Get representations in memory of parsing results of sentences.
 -}
toSentClauPhraList :: [[[String]]] -> [[[PhraCate]]]
toSentClauPhraList [] = []
toSentClauPhraList (sent:sents) = (map (map getPhraCateFromString) sent) : toSentClauPhraList sents

{- Get the length of every clause for a set of parsing trees.
 -}
toSentClauLengthList :: [[[PhraCate]]] -> [[Int]]
toSentClauLengthList [] = []
toSentClauLengthList (sent:sents) = (map (length . (getPhraBySpan 0)) sent) : toSentClauLengthList sents

{- Get the numbers of clauses with every different lengths, which is Data.Map.
 - The input is the list of clause lengths in every sentence, and the output is Map (clauLength, clauNum).
 -}
toClauLength2NumMap :: [[Int]] -> Map Int Int -> Map Int Int
toClauLength2NumMap [] clauLength2NumMap = clauLength2NumMap
toClauLength2NumMap [[]] clauLength2NumMap = clauLength2NumMap
toClauLength2NumMap [(cl:cls)] clauLength2NumMap = toClauLength2NumMap [cls] (Map.insert cl clauNum clauLength2NumMap)        -- "cl" means clause length.
    where
    clauNum = maybe 1 (1+) (Map.lookup cl clauLength2NumMap)
toClauLength2NumMap (sent:sents) clauLength2NumMap = Map.unionWith (+) mapHead mapTail               -- "sent" means the clause length list of a sentence.
    where
    mapHead = toClauLength2NumMap [sent] clauLength2NumMap
    mapTail = toClauLength2NumMap sents clauLength2NumMap

{- Get the depths of clauses in every sentence.
 -}
toSentClauDepthList :: [[[PhraCate]]] -> [[Int]]
toSentClauDepthList [] = []
toSentClauDepthList (sent:sents) = (map getTreeDepth sent) : toSentClauDepthList sents

{- Get the numbers of clauses with every different depths, which is Data.Map.
 - The input is the list of clause depths in every sentence, and the output is Map (clauDepth, clauNum).
 -}
toClauDepth2NumMap :: [[Int]] -> Map Int Int -> Map Int Int
toClauDepth2NumMap [] clauDepth2NumMap = clauDepth2NumMap
toClauDepth2NumMap [[]] clauDepth2NumMap = clauDepth2NumMap
toClauDepth2NumMap [(cd:cds)] clauDepth2NumMap = toClauDepth2NumMap [cds] (Map.insert cd clauNum clauDepth2NumMap)     -- 'cd' mean clause depth.
    where
    clauNum = maybe 1 (1+) (Map.lookup cd clauDepth2NumMap)
toClauDepth2NumMap (sent:sents) clauDepth2NumMap = Map.unionWith (+) mapHead mapTail               -- "sent" means the clause depth list of a sentence.
    where
    mapHead = toClauDepth2NumMap [sent] clauDepth2NumMap
    mapTail = toClauDepth2NumMap sents clauDepth2NumMap

{- Get the frequencies of various tags, which is Data.Map.
 - The input is the list of sentential categories, and the output is Map Tag <tagNum>.
 -}
toTag2FreqMap :: [[[PhraCate]]] -> Map String Int -> Map String Int
toTag2FreqMap [] tag2FreqMap = tag2FreqMap                                      -- No sentence
toTag2FreqMap [[]] tag2FreqMap = tag2FreqMap                                    -- One sentence has no clause to deal with.
toTag2FreqMap [(c:cs)] tag2FreqMap = toTag2FreqMap [cs] (insertPhraList2TagFreqMap c tag2FreqMap)
                                                             -- 'c' is a clause, a list of phrasal categories.
toTag2FreqMap (sent:sents) tag2FreqMap = Map.unionWith (+) mapHead mapTail                         -- "sent" means a sentence.
    where
    mapHead = toTag2FreqMap [sent] tag2FreqMap
    mapTail = toTag2FreqMap sents tag2FreqMap

{- Insert a series of 'tag' into a Map to count the frequency of every 'tag'.
 -}
insertPhraList2TagFreqMap :: [PhraCate] -> Map String Int -> Map String Int
insertPhraList2TagFreqMap [] tag2FreqMap = tag2FreqMap
insertPhraList2TagFreqMap [x] tag2FreqMap = Map.insert tag tagNum tag2FreqMap
    where
    tag = ((!!0) . taOfCate) x                         -- Apply 'taOfCate' to every phrasal category, and take the first element from the above result.
    tagNum = maybe 1 (1+) (Map.lookup tag tag2FreqMap)
insertPhraList2TagFreqMap (x:xs) tag2FreqMap = insertPhraList2TagFreqMap xs (insertPhraList2TagFreqMap [x] tag2FreqMap)

{- Get the format print of a Map list with given decimal places to represent float values.
 -}
formatMapListWithFloatValue :: [(String, Float)] -> Int -> [(String, String)]
formatMapListWithFloatValue mapList n = map (\x -> (fst x, printf ("%.0" ++ show n ++"f") (snd x))) mapList

{- Get the frequencies of various phrasal structures, which is Data.Map.
 - The input is the list of sentential categories, and the output is Map PhraStru <psNum>.
 -}
toPhraStru2FreqMap :: [[[PhraCate]]] -> Map String Int -> Map String Int
toPhraStru2FreqMap [] ps2FreqMap = ps2FreqMap                                   -- No sentence
toPhraStru2FreqMap [[]] ps2FreqMap = ps2FreqMap                                 -- One sentence has no clause to deal with.
toPhraStru2FreqMap [(c:cs)] ps2FreqMap = toPhraStru2FreqMap [cs] (insertPhraList2PhraStruFreqMap c ps2FreqMap)
                                                                      -- 'c' is a clause, a list of phrasal categories.
toPhraStru2FreqMap (sent:sents) ps2FreqMap = Map.unionWith (+) mapHead mapTail      -- "sent" means a sentence.
    where
    mapHead = toPhraStru2FreqMap [sent] ps2FreqMap
    mapTail = toPhraStru2FreqMap sents ps2FreqMap

{- Insert a series of 'PhraStru' into a Map to count the frequency of every 'PhraStru'.
 -}
insertPhraList2PhraStruFreqMap :: [PhraCate] -> Map String Int -> Map String Int
insertPhraList2PhraStruFreqMap [] ps2FreqMap = ps2FreqMap
insertPhraList2PhraStruFreqMap [x] ps2FreqMap = Map.insert phraStru psNum ps2FreqMap
    where
    phraStru = ((!!0) . psOfCate) x
                          -- Apply 'psOfCate' to every phrasal category, and take the first element from the above result.
    psNum = maybe 1 (1+) (Map.lookup phraStru ps2FreqMap)
insertPhraList2PhraStruFreqMap (x:xs) ps2FreqMap = insertPhraList2PhraStruFreqMap xs (insertPhraList2PhraStruFreqMap [x] ps2FreqMap)

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

{- Get representations in memory of parsing scripts of sentences.
 -}
toSentClauScriptList :: [[String]] -> [[Script]]
toSentClauScriptList [] = []
toSentClauScriptList (sent:sents) = (map readScript sent) : toSentClauScriptList sents

{- Get the frequencies of various transtive times, which is Data.Map.
 - The input is the list of sentential parsing scripts, and the output is Map <transTimes> <ttNum>.
 -}
toTransTimes2FreqMap :: [[Script]] -> Map Int Int -> Map Int Int
toTransTimes2FreqMap [] transTimes2FreqMap = transTimes2FreqMap                 -- No sentence
toTransTimes2FreqMap [[]] transTimes2FreqMap = transTimes2FreqMap               -- One sentence has no clause to deal with.
toTransTimes2FreqMap [(c:cs)] transTimes2FreqMap = toTransTimes2FreqMap [cs] (insertScript2TransTimesFreqMap c transTimes2FreqMap)
                                                                                -- 'c' is a clause, actually a parsing script.
toTransTimes2FreqMap (sent:sents) transTimes2FreqMap = Map.unionWith (+) mapHead mapTail                         -- "sent" means a sentence.
    where
    mapHead = toTransTimes2FreqMap [sent] transTimes2FreqMap
    mapTail = toTransTimes2FreqMap sents transTimes2FreqMap

{- Insert a parsing script into Map <transTimes> <ttNum>, where Script :: (ClauIdx, [[Rule]], BanPCs).
 -}
insertScript2TransTimesFreqMap :: Script -> Map Int Int -> Map Int Int
insertScript2TransTimesFreqMap (clauIdx, ruleLists, banPCs) transTimes2FreqMap = Map.insert transTimes ttNum transTimes2FreqMap
    where
    transTimes = length ruleLists
    ttNum = maybe 1 (1+) (Map.lookup transTimes transTimes2FreqMap)

{- Get the number of not-recognizable phrases in parsing every clause in every sentence.
 -}
toSentClauAbanNRPhraNumList :: [[Script]] -> [[Int]]
toSentClauAbanNRPhraNumList [] = []                                             -- No sentence
toSentClauAbanNRPhraNumList (sent:sents) = (map length sentAbanNRPhra) : toSentClauAbanNRPhraNumList sents
    where
    sentAbanPhra = map thd3 sent                                                -- [[PhraCate]], one [PhraCate] for one Script. One sentence might have multiple clauses.
    sentAbanNRPhra = map (\clauAbanPhra -> [x | x <- clauAbanPhra, (psOfCate x)!!0 == "NR"]) sentAbanPhra     -- Every phrase has only one 'ctspa'.

{- Count the frequencies of various keys, and store them in Map <String> <Int>.
 -}
keyToMap :: [String] -> Map String Int -> Map String Int
keyToMap [] m = m
keyToMap (s:ss) m = keyToMap ss (Map.insert s sNum m)
    where
    sNum = maybe 1 (1+) (Map.lookup s m)

{- Truncate a descending list such that the result list accounts for given percent proportion in original list according to value accumulation.
 - Here, list type is [(String,Int)], elements are descendingly ordered by <Int>, such as [("a",10),("b",5),("c",3),("d",2)].
 - If truncating original list by proportion 50% and 75%, [("a",10)] and [("a",10),("b",5)] will be obtained respectively.
 - If truncating original list by proportion 60%, [("a",10)] will be obtained, namely that the value accumulation proportion of resultant list is not bigger than the given proportion.
 -}
truncDescListByProportion :: Float -> [(String, Int)] -> [(String, Int)]
truncDescListByProportion _ [] = []
truncDescListByProportion prop descList
    | headValue / totalValue <= prop = (head descList) : truncDescListByProportion remainProp (tail descList)
    | otherwise = []
    where
    totalValue = fromIntegral $ foldl (+) 0 (map snd descList)
    headValue = fromIntegral $ snd (head descList)
    remainValue = (prop * totalValue) - headValue
    remainProp = remainValue / (totalValue - headValue)

{- Read a float value which is in [0, 1].
 -}
readFloat0to1 :: IO Float
readFloat0to1 = do
    line <- getLine
    let floatValue = read line :: Float
    if floatValue >= 0.0 && floatValue <= 1.0
       then return floatValue
       else do
         putStr "Please input a float value in [0, 1] again: "
         readFloat0to1
