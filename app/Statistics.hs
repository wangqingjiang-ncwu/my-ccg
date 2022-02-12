{-# LANGUAGE OverloadedStrings, LambdaCase #-}

-- Copyright (c) 2019-2021 China University of Water Resources and Electric Power
-- All rights reserved.

module Statistics (
    countInTree,                    -- Int -> Int -> Int -> IO ()
    countInScript,                  -- Int -> Int -> Int -> IO ()
    countInStruGene,                -- Int -> IO ()
    searchInTree,                   -- Int -> Int -> Int -> IO ()
    searchInScript,                 -- Int -> Int -> Int -> IO ()
    formatMapListWithFloatValue,    -- [(String,Float)] -> Int -> [(String,String)]
    truncDescListByProportion,      -- Float -> [(String, Int)] -> [(String, Int)]
    filterByString                  -- [(Int, String)] -> String -> [(Int, String)]
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
import SentParse

{- The following functions read field 'tree' of table 'corpus', then count all clauses according the input index.
 - 1. Get total number of sentences;
 - 2. Get clausal number in every sentence;
 - 3. Get averge clausal number per sentence;
 - 4. Get clausal length in every sentence and average clausal length;
 - 5. Get phrase number of every clause in every sentence;
 - 6. Get clausal number of different clausal lengths;
 - 7. Get  parsing-tree depths of every clause in every sentence;
 - 8. Get frequency totals and normalized frequencies of different CCG tags;
 - 9. Get frequencies of type conversions used in parsing every clause;
 - A. Get frequency total and normalized frequencies of different phrasal structures;
 - B. Get frequency total and normalized frequencies of different type-tag-stru(s);

 - The following functions read field 'script' of table 'corpus', then count all clauses according the input index.
 - 1. Get transitive times of every clause in all sentences;
 - 2. Get frequencies of different numbers of ransitive times in all clause parsing;
 - 3. Get the list of transitive times for every different clausal lengths;
 - 4. Get type-conversional list and type-conversional total in parsing every clause;
 - 5. Get total of using type conversions in transitive computing for every clausal length;
 - 6. Get the number of abandoned phrases in parsing every clause of every sentence;
 - 7. Get the minimum, maximum, and mean number of abandoned phrases for every clausal length;
 - 8. Get the number of abandoned not-recognizable phrases in parsing every clause of every sentence;

 - The following functions read Table 'stru_gene', then count all structural genes according the input index.
 - 1. Get total number of structural genes;
 - 2. Get frequencies of different overlapping types, namely [(OverType, Int)];
 - 3. Get frequencies of most common phrasal overlapping (LROs) by given common proportion;
 - 4. Get frequencies of most common unambiguous phrasal overlapping (LROPs) by given common proportion;
 - 5. Get descending hitCount

- The following functions read Table 'tree', then search all clauses according the input index.
- 1. Get serial_num list indicating those parsing trees which include given CCG tags;

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
         let sentClauLengthList = toSentClauLengthList sentClauPhraList                 -- [[Int]], here every integer is the length of a clause.
         let clauseLengthTotal = foldl (+) 0 $ map (foldl (+) 0) sentClauLengthList
         let averageClauseLength = (fromIntegral clauseLengthTotal :: Float) / (fromIntegral clauseTotalNum :: Float)
         putStrLn $ "countInTree: Length of every clause in every sentence: " ++ show sentClauLengthList
         putStrLn $ "countInTree: Average clause length: " ++ (printf "%.04f" averageClauseLength)

         let clauLength2LengthListMap = toClauLength2CerParaListMap sentClauLengthList sentClauLengthList Map.empty               -- Map Int [Int]
         let ascenListOfClauLength2ClauNum = map (\x -> (fst x, length (snd x))) $ Map.toAscList clauLength2LengthListMap         -- [(ClauLength, ClauNum)]
         let ascenListOfClauLength2FreqRate = map (\x -> (fst x, (fromIntegral (snd x))/(fromIntegral clauseTotalNum))) ascenListOfClauLength2ClauNum    -- [(ClauLength, FreqRate)]
         putStrLn $ "countInTree: The frequency rate of every clausal length: [" ++ showAscenListOfClauLength2FreqRate ascenListOfClauLength2FreqRate ++ "]"

       else putStr $ ""

    if funcIndex == 5                                          -- To get phrase number of every clause in every sentence. Actually, phrase number can be obtained from clause length.
       then do
         let sentClauPhraNumList = map (map length) sentClauPhraList                    -- [[Int]], here every integer is the phrase number of a clause.
         putStrLn $ "countInTree: Phrase number of every clause in every sentence: " ++ show sentClauPhraNumList
       else putStr ""

    if funcIndex == 6                                          -- To get the number of clauses with different lengths.
       then do
         let sentClauLengthList = toSentClauLengthList sentClauPhraList                 -- [[Int]], here every integer is the length of a clause.
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
         let tag2FreqMap = Map.delete "Desig" (toTag2FreqMap sentClauPhraList Map.empty)                -- Map String Int, namely Map Tag <tagNum>, not including "Desig".
         let tagFreqMapList = Map.toList tag2FreqMap                                                    -- [(String, Int)]
         let tagFregTotal = foldl (+) 0 (map snd tagFreqMapList)
         let ascListOfTagFreqByValue = toAscListOfMapByValue tagFreqMapList
         let descListOfTagFreqByValue = toDescListOfMapByValue tagFreqMapList
         let tagNormFreqDescList = map (\x -> (fst x, ((/(fromIntegral tagFregTotal)). fromIntegral) (snd x))) descListOfTagFreqByValue
                                                               -- Normalized frequencies of different C2CCG calculus tags.

--       putStrLn $ "countInTree: The list of frequencies of different C2CCG calclus tags: " ++ show tag2FreqMap
--       putStrLn $ "countInTree: The ascending list of frequencies of different C2CCG calculus tags: " ++ show ascListOfTagFreqByValue
         putStrLn $ "countInTree: The descending list of frequencies of different C2CCG calculus tags: " ++ show descListOfTagFreqByValue
         putStrLn $ "countInTree: The number of different C2CCG calculus tags: " ++ show (length tagFreqMapList)
         putStrLn $ "countInTree: The frequency total of different C2CCG calculus tags: " ++ show tagFregTotal
--       putStrLn $ "countInTree: The normalized frequencies of different C2CCG calculus tags: " ++ show tagNormFreqDescList
         putStrLn $ "countInTree: The normalized frequencies of different C2CCG calculus tags: " ++ show (formatMapListWithFloatValue tagNormFreqDescList 4)

{- Actually, C2CCG calculus tags (Abbr. tags) represent CCG rules sometimes including category type conversions,
 - which are used to deduce one new categorial type from two existed categorial types.
 - The <convCalTag> is either type conversion tag such as 'S/v' and 'P/a', or CCG calculus tag such as '>' and '>T->B'.
 - Especially, a type conversion tag, such as "S/v-P/a", represents two simultaneously used conversions, then ONLY the compound tag not every tag is counted once.
 -}
         let convCalTag2FreqMap = toConvCalTag2FreMap tagFreqMapList Map.empty                                       -- From List [(<tag>, <tagNum>)], get Map <convCalTag> <tagNum>.
         let convTag2FreqMap = Map.filterWithKey (\k _ -> (k!!0 /= '>') && (k!!0 /= '<')) convCalTag2FreqMap         -- Filter out conversional tags and their frequencies.
         let calTag2FreqMap = Map.filterWithKey (\k _ -> (k!!0 == '>') || (k!!0 == '<')) convCalTag2FreqMap          -- Filter out CCG calculus tags and their frequencies.
         let convTagTotal = Map.size convTag2FreqMap                            -- The total number of different conversional tags
         let calTagTotal = Map.size calTag2FreqMap                              -- The total number of different calculus tags
         let convCalTagFreqMapList = Map.toList convCalTag2FreqMap
         let convTagFreqMapList = Map.toList convTag2FreqMap
         let calTagFreqMapList = Map.toList calTag2FreqMap
         let descListOfConvCalTagFreqByValue = toDescListOfMapByValue convCalTagFreqMapList
         let descListOfConvTagFreqByValue = toDescListOfMapByValue convTagFreqMapList
         let descListOfCalTagFreqByValue = toDescListOfMapByValue calTagFreqMapList
         let convTagFreqTotal = foldl (+) 0 (map snd convTagFreqMapList)
         let calTagFreqTotal = foldl (+) 0 (map snd calTagFreqMapList)
         let convTagUsageRateDescList = map (\x -> (fst x, ((/(fromIntegral convTagFreqTotal)). fromIntegral) (snd x))) descListOfConvTagFreqByValue
         let calTagUsageRateDescList = map (\x -> (fst x, ((/(fromIntegral calTagFreqTotal)). fromIntegral) (snd x))) descListOfCalTagFreqByValue
         putStrLn $ "countInTree: The total number of different conversional tags: " ++ show convTagTotal
         putStrLn $ "countInTree: The frequency total of various conversional tags: " ++ show convTagFreqTotal
         putStrLn $ "countInTree: The total number of different CCG calculus tags: " ++ show calTagTotal
         putStrLn $ "countInTree: The frequency total of varions CCG calculus tages: " ++ show calTagFreqTotal
         putStrLn $ "countInTree: The descending list of frequencies of different conversional or calculus tags: " ++ show descListOfConvCalTagFreqByValue
         putStrLn $ "countInTree: The descending list of frequencies of different category type conversion tags: " ++ show descListOfConvTagFreqByValue
         putStrLn $ "countInTree: The descending list of usage rate of different category type conversion tags: " ++ show (formatMapListWithFloatValue convTagUsageRateDescList 4)
         putStrLn $ "countInTree: The descending list of frequencies of different CCG calculus tags: " ++ show descListOfCalTagFreqByValue
         putStrLn $ "countInTree: The descending list of usage rate of different CCG calculus tags: " ++ show (formatMapListWithFloatValue calTagUsageRateDescList 4)

       else putStr ""

    if funcIndex == 9                                          -- Get frequency of using type conversions in every clausal length.
       then do
         let sentClauPhraTagList = map (map (map (snd5 . (!!0) . snd3))) sentClauPhraList                     -- 'sentClauPhraList' type is [[[PhraCate]]].
         let sentClauPhraTagWithoutDesigList = map (map (filter (/= "Desig"))) sentClauPhraTagList            -- [[[Tag]]], not including "Desig" phrases.
         let sentClauPhraConvList = map (map (map toConvTagListForAPhrase)) sentClauPhraTagWithoutDesigList   -- [[[[ConvTag]]]], 'ConvTag' in "S/v-P/a-<" is "S/v-P/a".
         let sentClauPhraConvNumList = map (map (map length)) sentClauPhraConvList                      -- [[[ConvNum]]], 'ConvNum' is the number of used conversions in a phrase.
         let sentClauConvNumList = map (map (foldl (+) 0)) sentClauPhraConvNumList                      -- [[ConvNum]], 'ConvNum' is the number of used conversions in a clause.

--       putStrLn $ "countInTree: The number of used conversions in every clause: " ++ show sentClauConvNumList

         let sentClauLengthList = toSentClauLengthList sentClauPhraList                                 -- [[ClauLength]], 'ClauLength' is the length of a clause.
         putStrLn $ "countInTree: The length of every clause: " ++ show sentClauLengthList

         let clauLength2ConvNumListMap = toClauLength2CerParaListMap sentClauLengthList sentClauConvNumList Map.empty                   -- Map Int [Int]
         let ascenListOfClauLength2ConvNumList = Map.toAscList clauLength2ConvNumListMap                                                -- [(Int, [Int])]
         let ascenListOfClauLength2ConvNumMinMaxMeanList = toListOfClauLength2CerParaMinMaxMeanList ascenListOfClauLength2ConvNumList   -- [(ClauLength, [Min, Max, Mean])]
         putStrLn $ "countInTree: The list of frequencies of using type conversions in parsing trees for every clausal length: " ++ show ascenListOfClauLength2ConvNumList
         putStrLn $ "countInTree: Clausal count: " ++ show (foldl (+) 0 (map (length . snd) ascenListOfClauLength2ConvNumList))
         putStrLn $ "countInTree: The minimum, maximum, and mean value of frequencies of using type conversions in parsing trees for every clausal length: [" ++ showListOfClauLength2CerParaMinMaxMeanList ascenListOfClauLength2ConvNumMinMaxMeanList ++ "]"

       else putStr ""

    if funcIndex == 10                                         -- To get the frequency of every phrasal structure in all sentences.
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

    if funcIndex == 11                                          -- To get the frequency of every triple (Syntactic type, CCG rule tag, Phrasal structure) in all sentences.
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
         putStrLn $ "countInScript: The descending list of frequencies of different transitive times: " ++ show descListOfTT2FreqByValue

         let transTimesTotal = fromIntegral $ foldl (+) 0 $ map snd descListOfTT2FreqByValue         -- The total number of transtive times for all clauses.
         let descListOfTT2NormalizedFreqByValue = map (\x -> (fst x, ((/ transTimesTotal) . fromIntegral. snd) x)) descListOfTT2FreqByValue    -- [(transitive times, normalized freq.)]
         putStrLn $ "countInScript: Transitive times total for all clauses: " ++ show transTimesTotal
         putStrLn $ "countInScript: The descending list of normalized frequencies of different transitive times: " ++ show (formatMapListWithFloatValue descListOfTT2NormalizedFreqByValue 4)
       else putStr ""

    if funcIndex == 3                                         -- To get the list of transitive times for every different clausal lengths.
       then do
         let sentClauTransTimesList = map (map (length . snd3)) sentClauScriptList      -- [[[Int]]], here every integer is the transitive times in parsing a clause.

         stmt <- prepareStmt conn "select tree from corpus where serial_num >= ? and serial_num < ?"
         (defs, is) <- queryStmt conn stmt [toMySQLInt32 bottomSn, toMySQLInt32 topSn]
         sentStrList <- readStreamByText [] is                     -- [String], here a string is the parsing result of a sentence.
         let sentClauStrList = map stringToList sentStrList        -- [[String]], here a string is the parsing result of a clause.
         let sentClauPhraStrList = toSentClauPhraStrList sentClauStrList        -- [[[String]]], here a string is for a phrase.
         let sentClauPhraList = toSentClauPhraList sentClauPhraStrList          -- [[[PhraCate]]], here a PhraCate is the representation in memory of a phrase.
         let sentClauLengthList = toSentClauLengthList sentClauPhraList         -- [[Int]], here every integer is the length of a clause.

         let clauLength2TransTimesListMap = toClauLength2CerParaListMap sentClauLengthList sentClauTransTimesList Map.empty             -- Map Int [Int]
         let ascenListOfClauLength2TransTimesList = Map.toAscList clauLength2TransTimesListMap                                          -- [(Int, [Int])]
         let ascenListOfClauLength2TransTimesMinMaxMeanList = toListOfClauLength2CerParaMinMaxMeanList ascenListOfClauLength2TransTimesList     -- [(ClauLength, [Min, Max, Mean])]
         putStrLn $ "countInScript: The list of transitive times for every different clausal length: " ++ show ascenListOfClauLength2TransTimesList
         putStrLn $ "countInScript: Clausal count: " ++ show (foldl (+) 0 (map (length . snd) ascenListOfClauLength2TransTimesList))
         putStrLn $ "countInScript: The minimum, maximum, and mean value of transitive times for every clausal length: [" ++ showListOfClauLength2CerParaMinMaxMeanList ascenListOfClauLength2TransTimesMinMaxMeanList ++ "]"

       else putStr ""

    if funcIndex == 4                                         -- To get type-conversional list and type-conversional total in parsing every clause.
       then do
         let sentClauTransConvList = map (map snd3) sentClauScriptList
                                                              -- [[[[tag]]]], conversional tags(#4) used in every transition(#3) in parsing every clause(#2) in sentence list(#1).
         let sentClauTransConvNumList = map (map (map length)) sentClauTransConvList
                                                              -- [[[num]]], list of numbers of conversional tags (#3) in parsing every clause(#2) in sentence list(#1).
         let sentClauConvTotalList = map (map (foldl (+) 0)) sentClauTransConvNumList
                                                              -- [[total]], list of totals of conversional tags in parsing every clause(#2) in sentence list(#1).
         putStrLn $ "countInScript: The conversion-taged list of every clause in every sentences: " ++ show sentClauTransConvList
         putStrLn $ "countInScript: The total num. of conversional tags used in parsing every clause in every sentences: " ++ show sentClauConvTotalList
       else putStr ""

    if funcIndex == 5                                         -- Frequency of using type conversions in transitive computing for every clausal length.
       then do
         let sentClauTransConvList = map (map snd3) sentClauScriptList          -- [[[[tag]]]]
         let sentClauTransConvNumList = map (map (map length)) sentClauTransConvList        -- [[[num]]]
         let sentClauConvFreqList = map (map (foldl (+) 0)) sentClauTransConvNumList       -- [[total]]

         stmt <- prepareStmt conn "select tree from corpus where serial_num >= ? and serial_num < ?"
         (defs, is) <- queryStmt conn stmt [toMySQLInt32 bottomSn, toMySQLInt32 topSn]
         sentStrList <- readStreamByText [] is                     -- [String], here a string is the parsing result of a sentence.
         let sentClauStrList = map stringToList sentStrList        -- [[String]], here a string is the parsing result of a clause.
         let sentClauPhraStrList = toSentClauPhraStrList sentClauStrList        -- [[[String]]], here a string is for a phrase.
         let sentClauPhraList = toSentClauPhraList sentClauPhraStrList          -- [[[PhraCate]]], here a PhraCate is the representation in memory of a phrase.
         let sentClauLengthList = toSentClauLengthList sentClauPhraList         -- [[Int]], here every integer is the length of a clause.

         let clauLength2ConvNumListMap = toClauLength2CerParaListMap sentClauLengthList sentClauConvFreqList Map.empty                  -- Map Int [Int]
         let ascenListOfClauLength2ConvNumList = Map.toAscList clauLength2ConvNumListMap                                                -- [(Int, [Int])]
         let ascenListOfClauLength2ConvNumMinMaxMeanList = toListOfClauLength2CerParaMinMaxMeanList ascenListOfClauLength2ConvNumList   -- [(ClauLength, [Min, Max, Mean])]
         putStrLn $ "countInScript: The list of frequencies of using type conversions in transitive computing for every clausal length: " ++ show ascenListOfClauLength2ConvNumList
         putStrLn $ "countInScript: Clausal count: " ++ show (foldl (+) 0 (map (length . snd) ascenListOfClauLength2ConvNumList))
         putStrLn $ "countInScript: The minimum, maximum, and mean value of total of using type conversions in transitive computing for every clausal length: [" ++ showListOfClauLength2CerParaMinMaxMeanList ascenListOfClauLength2ConvNumMinMaxMeanList ++ "]"

       else putStr ""

{- To get the number of abandoned phrases in parsing every clause of every sentence, which is useful to evaluate ambiguity of clause syntax.
 -}
    if funcIndex == 6
       then do
         let sentClauAbanPhraNumList = map (map (length . thd3)) sentClauScriptList
         let abanedPhraTotal = foldl (+) 0 (map (foldl (+) 0) sentClauAbanPhraNumList)
         putStrLn $ "countInScript: The number of abandoned phrases in parsing every clause: " ++ show sentClauAbanPhraNumList
         putStrLn $ "countInScript: The total number of abandoned phrases: " ++ show abanedPhraTotal ++ ", the mean number of abandoned phrases per clause: " ++ show (fromIntegral abanedPhraTotal / fromIntegral clauseTotalNum)
       else putStr ""

{- To get the minimum, maximum, and mean number of abandoned phrases for every clausal length.
 -}
    if funcIndex == 7
       then do
         let sentClauAbanPhraNumList = map (map (length . thd3)) sentClauScriptList

         stmt <- prepareStmt conn "select tree from corpus where serial_num >= ? and serial_num < ?"
         (defs, is) <- queryStmt conn stmt [toMySQLInt32 bottomSn, toMySQLInt32 topSn]
         sentStrList <- readStreamByText [] is                     -- [String], here a string is the parsing result of a sentence.
         let sentClauStrList = map stringToList sentStrList        -- [[String]], here a string is the parsing result of a clause.
         let sentClauPhraStrList = toSentClauPhraStrList sentClauStrList        -- [[[String]]], here a string is for a phrase.
         let sentClauPhraList = toSentClauPhraList sentClauPhraStrList          -- [[[PhraCate]]], here a PhraCate is the representation in memory of a phrase.
         let sentClauLengthList = toSentClauLengthList sentClauPhraList         -- [[Int]], here every integer is the length of a clause.

         let clauLength2AbanPhraNumListMap = toClauLength2CerParaListMap sentClauLengthList sentClauAbanPhraNumList Map.empty               -- Map Int [Int]
         let ascenListOfClauLength2AbanPhraNumList = Map.toAscList clauLength2AbanPhraNumListMap                                            -- [(Int, [Int])]
         let ascenListOfClauLength2AbanPhraNumMinMaxMeanList = toListOfClauLength2CerParaMinMaxMeanList ascenListOfClauLength2AbanPhraNumList   -- [(ClauLength, [Min, Max, Mean])]

--       putStrLn $ "countInScript: The length of every clause in every sentence: " ++ show sentClauLengthList
--       putStrLn $ "countInScript: The number of abandoned phrases in parsing every clause: " ++ show sentClauAbanPhraNumList
         putStrLn $ "countInScript: The number of abandoned phrases in parsing every clause for every clausal length: " ++ show ascenListOfClauLength2AbanPhraNumList
         putStrLn $ "countInScript: Clausal count: " ++ show (foldl (+) 0 (map (length . snd) ascenListOfClauLength2AbanPhraNumList))
         putStrLn $ "countInScript: The minimum, maximum, and mean number of abandoned phrases for every clausal length: [" ++ showListOfClauLength2CerParaMinMaxMeanList ascenListOfClauLength2AbanPhraNumMinMaxMeanList ++ "]"

       else putStr ""

{- To get the number of abandoned not-recognizable phrases in parsing every clause of every sentence,
 - which is useful to evaluate how much proportion of phrases are not accepted by Chinese syntax.
 - To now, the phrases not recognized by the present C2CCG are filtered out automatically.
 -}
    if funcIndex == 8
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

{- Get search result in field 'tree' in Table 'corpus' whose serial numbers are less than 'topSn' and
 - bigger than or equal to 'bottomSn', here 'funcIndex' indicates which statistics will be done.
 -}
searchInTree :: Int -> Int -> Int -> IO ()
searchInTree bottomSn topSn funcIndex = do
    conn <- getConn
    stmt <- prepareStmt conn "select serial_num, tree from corpus where serial_num >= ? and serial_num < ?"
    (defs, is) <- queryStmt conn stmt [toMySQLInt32 bottomSn, toMySQLInt32 topSn]

    snTreeList <- readStreamByInt32Text [] is                          -- [(Int, String)], storing serial_num and parsing tree of every sentence.

    if funcIndex == 1                                                   -- To get serial_num list indicating those parsing trees which include given CCG tags.
       then do
         putStr "Please input the C2CCG tag used in parsing sentence: "
         tag <- getLine
         let snListMatchTag = filterByString snTreeList tag             -- [(Int, String)], matching given CCG tag.
         putStrLn $ "searchInTree: The serial_num list of trees which use tag \"" ++ tag ++ "\": " ++ show (map fst snListMatchTag)
       else putStr ""

    if funcIndex == 2                                                   -- To display parsing trees of all clauses of all sentences.
      then do
        dispTreeListOnStdout snTreeList
      else putStr ""

{-    if funcIndex == 3                                                   -- To do.
      then do
        putStr "To do."
      else putStr ""
 -}
{- Get search result in field 'script' in Table 'corpus' whose serial numbers are less than 'topSn' and
 - bigger than or equal to 'bottomSn', here 'funcIndex' indicates which statistics will be done.
 -}
searchInScript :: Int -> Int -> Int -> IO ()
searchInScript bottomSn topSn funcIndex = putStrLn "searchInScript: to do."

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

{- Read a value from input stream [MySQLValue], append it to existed list [(Int, String)], then read the next,
 - until read Nothing.
 - Here [MySQLValue] is [MySQLInt32U, MySQLText].
 -}
readStreamByInt32Text :: [(Int, String)] -> S.InputStream [MySQLValue] -> IO [(Int, String)]
readStreamByInt32Text es is = do
    S.read is >>= \case                                         -- Dumb element 'case' is an array with type [MySQLValue]
        Just x -> readStreamByInt32Text (es ++ [(fromMySQLInt32 (x!!0), fromMySQLText (x!!1))]) is
        Nothing -> return es

{- Read a value from input stream [MySQLValue], append it to existed string list, then read the next,
 - until read Nothing.
 - Here [MySQLValue] is [MySQLText, MySQLText, MySQLInt8].
 -}
readStreamByTextTextInt8 :: [String] -> S.InputStream [MySQLValue] -> IO [String]
readStreamByTextTextInt8 es is = do
    S.read is >>= \case                                         -- Dumb element 'case' is an array with type [MySQLValue]
        Just x -> readStreamByTextTextInt8 (es ++ [fromMySQLText (x!!0) ++ "_" ++ fromMySQLText (x!!1) ++ "_" ++ show (fromMySQLInt8 (x!!2))]) is
        Nothing -> return es

{- Read a value from input stream [MySQLValue], append it to existed string list, then read the next,
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
formatMapListWithFloatValue :: Show a => [(a, Float)] -> Int -> [(a, String)]       -- 'a' may be any showable type, Polymorphism!
formatMapListWithFloatValue mapList n = map (\x -> (fst x, printf ("%.0" ++ show n ++"f") (snd x))) mapList

{- Convert one Map String Int to another Map String Int, the former is {(tag, tagNum)}, and the latter is {(convCalTag, convCalTagNum)}.
 - Here, 'tag' is C2CCG calculus tag such as '>', '<', '>B', '<B', '>Bx', '<Bx', and '>T->B', sometimes including type-conversional tag such as 'S/n->', 'P/a-<B, and 'S/n-P/a-<'.
 - The 'convCalTag' is a type-conversioned tag or a CCG calculus tag. A type-conversioned tag is a primitive type-conversioned tag or a compound type-conversioned tag.
 - For an example, C2CCG calculus tag 'S/n-P/a-<' includes type-conversioned tag 'S/n', 'P/a', 'S/n-P/a', and CCG calculus tag '<'.
 - From a tag-frequency Map, a convCalTag-frequency Map is obtained.
 -}
toConvCalTag2FreMap :: [(String, Int)] -> Map String Int -> Map String Int
toConvCalTag2FreMap [] convCalTag2FreqMap = convCalTag2FreqMap
toConvCalTag2FreMap (si:sis) convCalTag2FreqMap = toConvCalTag2FreMap sis $ insertTagFreq2ConvCalTag2FreqMap si convCalTag2FreqMap

{- Insert a (<tag>, <tagNum>) into Map <convCalTag> <convCalTagNum>, here <tag> is C2CCG calculus tag, <convCalTag> is type-conversioned tag or CCG calculus tag.
 - For ">", only calculus tag ">" is there.
 - For "S/v->", the conversional tag is "S/v", and the calculus tag is ">".
 - For "S/v-P/a-<", the conversional tag is "S/v-P/a", and the calculus tag is "<".
 -}
insertTagFreq2ConvCalTag2FreqMap :: (String, Int) -> Map String Int -> Map String Int
insertTagFreq2ConvCalTag2FreqMap (tag, tagNum) convCalTag2FreqMap
    | length tags == 1 = Map.insertWith (+) (tags!!0) tagNum convCalTag2FreqMap
    | length tags == 2 = Map.insertWith (+) (tags!!0) tagNum $ Map.insertWith (+) (tags!!1) tagNum convCalTag2FreqMap
    | length tags == 3 = Map.insertWith (+) (tags!!0 ++ "-" ++ tags!!1) tagNum $ Map.insertWith (+) (tags!!2) tagNum convCalTag2FreqMap
--  | length tags == 3 = Map.insertWith (+) (tags!!0 ++ "-" ++ tags!!1) tagNum $ Map.insertWith (+) (tags!!0) tagNum $ Map.insertWith (+) (tags!!1) tagNum $ Map.insertWith (+) (tags!!2)
    | otherwise = Map.empty                                                     -- Exception !
    where
    tags = splitTagAsConvOrCal tag

{- From the tag of a phrase, filter out conversional tags.
 - The input argument is a C2CCG tag used to get this phrase, such as "S/v-P/a-<".
 - The output result is [ConvTag], such as "S/v", "P/a", "S/v-P/a".
 -}
toConvTagListForAPhrase :: String -> [String]
toConvTagListForAPhrase tag
    | length tags == 1 = []                        -- Only has CCG calculus tag.
    | length tags == 2 = [tags!!0]
    | length tags == 3 = [tags!!0 ++ "-" ++ tags!!1]
--  | length tags == 3 = [tags!!0, tags!!1, tags!!0 ++ "-" ++ tags!!1]
    | otherwise = []                                                            -- Exception !
    where
    tags = splitTagAsConvOrCal tag

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

{- From clausal length and certain statistical parameter (such as transitive times or conversioanl number) of every clause of every sentence,
 - get parameter list of every different clausal length and store it in a Map Int [Int].
 - The first argument is clausal length of every clause of every sentence (Abbr. scll), and the second is certain statistcal parameter of every clause of every sentence (Abbr. sctt).
 -}
toClauLength2CerParaListMap :: [[Int]] -> [[Int]] -> Map Int [Int] -> Map Int [Int]
toClauLength2CerParaListMap [] _ clauLength2CerParaListMap = clauLength2CerParaListMap
toClauLength2CerParaListMap _ [] clauLength2CerParaListMap = clauLength2CerParaListMap
toClauLength2CerParaListMap (cll:scll) (ctt:sctt) clauLength2CerParaListMap = toClauLength2CerParaListMap scll sctt $ insertSent2ClauLengthCerParaListMap cll ctt clauLength2CerParaListMap

{- For every clause in a sentence, insert clausal length and its certain parameters in parsing the clause into Map <clause length> <clausal certain parameter>.
 - The first argument is the list of clausal lengths, and the second is the list of certain parameters in parsing corresponding clauses.
 -}
insertSent2ClauLengthCerParaListMap:: [Int] -> [Int] -> Map Int [Int] -> Map Int [Int]
insertSent2ClauLengthCerParaListMap [] [] clauLength2CerParaListMap = clauLength2CerParaListMap
insertSent2ClauLengthCerParaListMap cll ctt clauLength2CerParaListMap = insertSent2ClauLengthCerParaListMap otherClauLength otherClauCerPara $ Map.insert clauLength cpList clauLength2CerParaListMap
    where
    clauLength = head cll
    otherClauLength = tail cll
    clauCerPara = head ctt
    otherClauCerPara = tail ctt
    cpList = maybe [clauCerPara] (clauCerPara:) (Map.lookup clauLength clauLength2CerParaListMap)

{- For every clausal length, get minimum, maximum, and mean value of certain parameter.
 - The input argument is [(clausal length, [certain parameter])], and the result is [(clausal length, (Min, Max, Mean))].
 -}
toListOfClauLength2CerParaMinMaxMeanList :: [(Int, [Int])] -> [(Int, (Int, Int, Float))]
toListOfClauLength2CerParaMinMaxMeanList [] = []
toListOfClauLength2CerParaMinMaxMeanList [(l, ttl)] = [(l, (minimum ttl, maximum ttl, ((fromIntegral (sum ttl) :: Float) / (fromIntegral (length ttl) :: Float))))]
toListOfClauLength2CerParaMinMaxMeanList (s:ss) = toListOfClauLength2CerParaMinMaxMeanList [s] ++ toListOfClauLength2CerParaMinMaxMeanList ss

{- Get the string of list 'ascenListOfClauLength2TransTimesMinMaxMeanList' and 'ascenListOfClauLength2ConvNumMinMaxMeanList'.
 -}
showListOfClauLength2CerParaMinMaxMeanList :: [(Int, (Int, Int, Float))] -> String
showListOfClauLength2CerParaMinMaxMeanList [] = ""
showListOfClauLength2CerParaMinMaxMeanList [(l, (max, min, mean))] = "(" ++ show l ++ ", (" ++ show max ++ ", " ++ show min ++ ", " ++ printf "%.02f" mean ++ ")"
showListOfClauLength2CerParaMinMaxMeanList (l:ls) = showListOfClauLength2CerParaMinMaxMeanList [l] ++ ", " ++ showListOfClauLength2CerParaMinMaxMeanList ls

{- Get the string of list 'ascenListOfClauLength2FreqRate'.
 -}
showAscenListOfClauLength2FreqRate :: [(Int, Float)] -> String
showAscenListOfClauLength2FreqRate [] = ""
showAscenListOfClauLength2FreqRate [(clauLength, freqRate)] = "(" ++ show clauLength ++ ", " ++ printf "%.04f" freqRate ++ ")"
showAscenListOfClauLength2FreqRate (cl:cls) = showAscenListOfClauLength2FreqRate [cl] ++ ", " ++ showAscenListOfClauLength2FreqRate cls

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

{- Filter [(Int, String)], and remain the elements which match pattern string.
 -}
filterByString :: [(Int, String)] -> String -> [(Int, String)]
filterByString [] _ = []
filterByString [(i1, s1)] patt
    | isSubstr patt s1 patt s1 = [(i1, s1)]
    | otherwise = []
filterByString (kv:kvs) patt = (filterByString [kv] patt) ++ (filterByString kvs patt)

{- Display parsing trees of every sentence in a set of sentences, usually every sentence includes multiple clauses, and every clause has its parsing tree.
 - The input is [(sn,treeStr)], here 'sn' is serial number of a sentence which has parsing trees represented by string 'treeStr'.
 -}
dispTreeListOnStdout :: [(Int, String)] -> IO ()
dispTreeListOnStdout [] = putStrLn "No parsing tree to display."
dispTreeListOnStdout [(sn, treeStr)] = do
    putStrLn $ "Sentence No.: " ++ show sn
    sentToClauses treeStr >>= dispTree' 1          -- The ordered number of first clause is 1.
dispTreeListOnStdout (t:ts) = do
    dispTreeListOnStdout [t]
    dispTreeListOnStdout ts
