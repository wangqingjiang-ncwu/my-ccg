{-# LANGUAGE OverloadedStrings, LambdaCase #-}

-- Copyright (c) 2019-2025 China University of Water Resources and Electric Power
-- All rights reserved.

module Statistics (
    countInTree,                    -- Int -> Int -> Int -> IO ()
    countInScript,                  -- Int -> Int -> Int -> IO ()
    countInStruGene,                -- String -> Int -> Int -> Int -> IO ()
    searchInTree,                   -- Int -> Int -> Int -> IO ()
    searchInScript,                 -- Int -> Int -> Int -> IO ()
    toTypeTagStru2FreqMap',         -- [[[PhraCate]]] -> Map PhraSyn0 Int -> Map PhraSyn0 Int
    insertPhraList2CtpFreqMap',     -- [PhraCate] -> Map PhraSyn0 Int -> Map PhraSyn0 Int
    formatMapListWithFloatValue,    -- [(String,Float)] -> Int -> [(String,String)]
    truncDescListByProportion,      -- Float -> [(String, Int)] -> [(String, Int)]
    filterByString,                 -- String -> [(Int, String)] -> [(Int, String)]
    filterTagInSentTrees,           -- Tag -> [(SentIdx, [Tree])] -> [(SentIdx, [Tree])]
    filterTagInTrees,               -- Tag -> [Tree] -> [Tree]
    hasTagInTree,                   -- Tag -> Tree -> Bool
    filterPhraStruInSentTrees,      -- PhraStru -> [(SentIdx, [Tree])] -> [(SentIdx, [Tree])]
    filterPhraStruInTrees,          -- PhraStru -> [Tree] -> [Tree]
    hasPhraStruInTree,              -- PhraStru -> Tree -> Bool
    filterCateInSentTrees,          -- Category -> [(SentIdx, [Tree])] -> [(SentIdx, [Tree])]
    filterCateInTrees,              -- Category -> [Tree] -> [Tree]
    hasCateInTree,                  -- Category -> Tree -> Bool
    quickSort4PhraSyn,              -- [PhraSyn] -> [PhraSyn]
    phraSynLt,                      -- PhraSyn -> PhraSyn -> Bool
    ) where

import Control.Monad
import qualified System.IO.Streams as S
import qualified Data.String as DS
import Database.MySQL.Base
import Data.List
import Data.List.Utils
import Data.Tuple
import Data.Tuple.Utils
import Text.Printf
import qualified Data.Tuple as Tuple
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Set (Set)
import qualified Data.Set as Set
import Category
import Phrase
import Utils
import Database
import Corpus
import SentParse (sentToClauses, dispTreeOfSent)
import AmbiResol
import Clustering
import Numeric.LinearAlgebra.Data
import Numeric.LinearAlgebra
import Output (showScripts, showScripts', showCatePair2SimList, showTagPair2SimList, showStruPair2SimList, showSpanPair2SimList)
import AmbiResol (phraSynToString, stringToCTPList, purityOfMajorPrior)

type Clau = [PhraCate]
type Sent = [Clau]
type Sents = [Sent]

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
 - C. Get similarity between every pair of categories;
 - D. Get similarity between every pair of grammatic rules;
 - E. Get similarity between every pair of phrasal structures;

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
 - 5. Get hit count of different overlapping types, namely [(OverType, sum(LpHitCount + RpHitCount + NothHitCount))];
 - 6. Get similarity degree between every pair of categories;
 - 7. Get similarity degree between every pair of grammatic rules;
 - 8. Get similarity degree between every pair of phrasal structures;
 - 9. Get similarity degree between every pair of phrasal spans;

 - The following functions read Table 'tree', then search all clauses according the input index.
 - 1. Get serial_num list indicating those parsing trees which include given CCG tags;
 - 2 -> Get serial_num list indicating those parsing trees which include given phrasal structure
 - 3 -> Get serial_num list indicating those parsing trees which include given syntactic category
 - 4 -> Display parsing trees of all clauses of all sentences.
 - 5 -> Display parsing trees in which include given grammatic rule.
 - 6 -> Display parsing trees in which include given phrasal structure.
 - 7 -> Display parsing trees in which include given syntactic category.

 -}

{- Get statistics about field 'tree' in Table <tree_source> whose serial numbers are less than 'topSn' and
 - bigger than or equal to 'bottomSn', here 'funcIndex' indicates which statistics will be done.
 - <tree_source> is the value of attribute "tree_source" in file Configuration.
 -}
countInTree :: Int -> Int -> Int -> IO ()
countInTree bottomSn topSn funcIndex = do
    conn <- getConn
    confInfo <- readFile "Configuration"                                        -- Read the local configuration file
    let phrasyn = getConfProperty "phrasyn" confInfo
    let tree_source = getConfProperty "tree_source" confInfo
    let sqlstat = DS.fromString $ "select tree from " ++ tree_source ++ " where serial_num >= ? and serial_num <= ?"
    stmt <- prepareStmt conn sqlstat
    (defs, is) <- queryStmt conn stmt [toMySQLInt32 bottomSn, toMySQLInt32 topSn]

    sentStrList <- readStreamByText [] is                     -- [String], here a string is the parsing result of a sentence.
    let sentClauStrList = map stringToList sentStrList        -- [[String]], here a string is the parsing result of a clause.
    let sentClauNumList = map length sentClauStrList          -- [Int], here an integer is the number of clauses in a sentence.
    let sentNum = length sentStrList                          -- The number of sentences.
    let clauseTotalNum = foldl (+) 0 sentClauNumList          -- The total number of clauses.

    let sentClauTreeList = map (map readTree) sentClauStrList     -- [[Tree]], here Tree ::= (ClauIdx, [PhraCate])
--    let sentClauPhraStrList = toSentClauPhraStrList sentClauStrList
                                                              -- [[[String]]], here a string is for a phrase.
    let sentClauPhraList = map (map snd) sentClauTreeList     -- [[[PhraCate]]], here a PhraCate is the representation in memory of a phrase.


--  Output the following for test.
--    putStrLn $ "countInTree: The list of clauses in first sentence: " ++ show (sentClauPhraList!!0)
--    putStrLn $ "countInTree: The first clause in first sentence: " ++ show ((sentClauPhraList!!0)!!0)

    if funcIndex == 1                                          -- To get the number of sentences.
       then putStrLn $ "countInTree: The number of sentences: " ++ show sentNum
       else putStr ""                                          -- No operation for 'else' condition

    if funcIndex == 2                                          -- To get the number of clauses in every sentence.
       then do
         putStrLn $ "countInTree: The number of clauses in every sentence: " ++ show sentClauNumList
         putStrLn $ "countInTree: The total number of clauses in sentences: " ++ show (sum sentClauNumList)
       else putStr ""

    if funcIndex == 3                                          -- To get the number of sentences, the number of clauses in every
                                                               -- sentence, and the averge number of clauses per sentence.
       then do
         putStrLn $ "countInTree: The number of sentences: " ++ show sentNum
         putStrLn $ "countInTree: The total number of clauses: " ++ show clauseTotalNum ++ ", average clause number per sentence: " ++ show (fromIntegral clauseTotalNum / fromIntegral sentNum)
       else putStr ""

    if funcIndex == 4                                          -- To get the length of every clause in every sentence.
       then do
         let sentClauLengthList = toSentClauLengthList sentClauTreeList                 -- [[Int]], here every integer is the length of a clause.
         let clauseLengthTotal = foldl (+) 0 $ map (foldl (+) 0) sentClauLengthList
         let averageClauseLength = (fromIntegral clauseLengthTotal :: Float) / (fromIntegral clauseTotalNum :: Float)
         putStrLn $ "countInTree: Length of every clause in every sentence: " ++ show sentClauLengthList
         putStrLn $ "countInTree: Total of clause lengths: " ++ show clauseLengthTotal
         putStrLn $ "countInTree: Average clause length: " ++ (printf "%.04f" averageClauseLength)

         let clauLength2LengthListMap = toClauLength2CerParaListMap sentClauLengthList sentClauLengthList Map.empty               -- Map Int [Int]
         let ascenListOfClauLength2ClauNum = map (\x -> (fst x, length (snd x))) $ Map.toAscList clauLength2LengthListMap         -- [(ClauLength, ClauNum)]
         let ascenListOfClauLength2FreqRate = map (\x -> (fst x, (fromIntegral (snd x))/(fromIntegral clauseTotalNum))) ascenListOfClauLength2ClauNum    -- [(ClauLength, FreqRate)]
         putStrLn $ "countInTree: The frequency rate of every clausal length: [" ++ showAscenListOfClauLength2FreqRate ascenListOfClauLength2FreqRate ++ "]"

       else putStr $ ""

    if funcIndex == 5                                          -- To get phrase number of every clause in every sentence. Actually, phrase number can be obtained from clause length.
       then do
         let sentClauPhraNumList = map (map length) sentClauPhraList                    -- [[Int]], here every integer is the phrase number of a clause.
         let sentPhraTotal = foldl (+) 0 $ map (foldl (+) 0) sentClauPhraNumList
         putStrLn $ "countInTree: Phrase number of every clause in every sentence: " ++ show sentClauPhraNumList
         putStrLn $ "countInTree: Phrasal total in all sentences: " ++ show sentPhraTotal
       else putStr ""

    if funcIndex == 6                                          -- To get the number of clauses with different lengths.
       then do
         let sentClauLengthList = toSentClauLengthList sentClauTreeList                 -- [[Int]], here every integer is the length of a clause.
         let clauLength2NumMap = toClauLength2NumMap sentClauLengthList Map.empty       -- Map Int Int, namely Map <clauLength> <clauNum>, and 'empty' return null Map.
         putStrLn $ "countInTree: Clause count by clause length: " ++ show clauLength2NumMap
       else putStr ""

    if funcIndex == 7                                          -- To get the number of clauses with different depths.
       then do
         let sentClauDepthList = toSentClauDepthList sentClauTreeList           -- [[Int]], here every integer is the depth of a clause.
         let clauDepth2NumMap = toClauDepth2NumMap sentClauDepthList Map.empty
                                                                -- Map Int Int, namely Map <clauDepth> <clauNum>, and 'empty' return null Map.
         putStrLn $ "countInTree: The height of parsing tree of every clause in every senetence: " ++ show sentClauDepthList
         putStrLn $ "countInTree: The list of number of clauses with different parsing tree heights: " ++ show clauDepth2NumMap
         putStrLn $ "countInTree: Sentences failing to parse are (SentIdx, [Depth]): " ++ show (getIdxOfclauWithoutParsingTree sentClauDepthList)

         let clauseTotalDepth = foldl (+) 0 (concat sentClauDepthList)          -- The total number of clauses.
         let averDepth = (fromIntegral clauseTotalDepth :: Float) / (fromIntegral clauseTotalNum :: Float)
         putStrLn $ "countInTree: The average height of parsing trees of all clauses: " ++ printf "%.04f" averDepth
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

         let sentClauLengthList = toSentClauLengthList sentClauTreeList                                 -- [[ClauLength]], 'ClauLength' is the length of a clause.
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
         let typeTagStru2FreqMap = toTypeTagStru2FreqMap sentClauPhraList Map.empty            -- Map String Int, namely Map <type_tag_stru> <ctpNum>.
         let typeTagStru2FreqMapList = Map.toList typeTagStru2FreqMap                          -- [(String, Int)]
         let typeTagStruFreqTotal = foldl (+) 0 (map snd typeTagStru2FreqMapList)
         let descListOfCTPByValue = toDescListOfMapByValue typeTagStru2FreqMapList
         let typeTagStruNormFreqDescList = map (\x -> (fst x, ((/(fromIntegral typeTagStruFreqTotal)). fromIntegral) (snd x))) descListOfCTPByValue                                   -- Normalized frequencies of different CCG tags.
         putStrLn $ "countInTree: The number of different type-tag-stru structures: " ++ show (Map.size typeTagStru2FreqMap)
         putStrLn $ "countInTree: The frequency total of different type-tag-stru(s): " ++ show typeTagStruFreqTotal
         putStrLn $ "countInTree: The frequencies of different type-tag-stru(s): " ++ show descListOfCTPByValue
--       putStrLn $ "countInTree: The normalized frequencies of different type-tag-stru(s): " ++ show typeTagStruNormFreqDescList
         putStrLn $ "countInTree: The normalized frequencies of different type-tag-stru(s): " ++ show (formatMapListWithFloatValue typeTagStruNormFreqDescList 4)
       else putStr ""

-- To calculate similarities between every pair of Categories.
    if funcIndex == 12
       then do
         let typePair2SimTuple = case phrasyn of
                                   "phrasyn" -> getTypePair2SimFromSCPL sentClauPhraList     -- (NumOfPhraSyn, NumOfCate, NumOfCatePair, [((Category, Category), SimDeg)])
                                   "phrasyn0" -> getTypePair2SimFromSCPL0 sentClauPhraList
                                   _ -> error $ "countInTree: Property " ++ phrasyn ++ " can not be recognized."
         let typePair2SimList = fth4 typePair2SimTuple
         let sparseTypePair2SimList = [x | x <- typePair2SimList, snd x /= fromIntegral 0]
         putStrLn $ "countInTree: The number of different PhraSyns: " ++ show (fst4 typePair2SimTuple)
         putStrLn $ "countInTree: The number of different categories: " ++ show (snd4 typePair2SimTuple)
         putStrLn $ "countInTree: The number of different category pairs: " ++ show (thd4 typePair2SimTuple)
         cOrS <- getLineUntil "Print complete matrix or print sparse matrix? ['c' or RETURN for complete, 's' for sparse] " ["c","s"] True
         if cOrS == "c"
           then do
             putStr $ "countInTree: The whole typePair2SimList: "
             showCatePair2SimList (formatMapListWithDoubleValue typePair2SimList 4)
           else do
             putStr $ "countInTree: sparse typePair2SimList: "
             showCatePair2SimList (formatMapListWithDoubleValue sparseTypePair2SimList 4)
       else putStr ""

-- To calculate similarities between every pair of grammatic rules.
    if funcIndex == 13
       then do
         let tagPair2SimTuple = case phrasyn of
                                  "phrasyn" -> getTagPair2SimFromSCPL sentClauPhraList       -- (NumOfPhraSyn, NumOfPhraStru, NumOfStruPair, [((Tag, Tag), SimDeg)])
                                  "phrasyn0" -> getTagPair2SimFromSCPL0 sentClauPhraList     -- (NumOfPhraSyn, NumOfPhraStru, NumOfStruPair, [((Tag, Tag), SimDeg)])
                                  _ -> error $ "countInTree: Property " ++ phrasyn ++ " can not be recognized."
         let tagPair2SimList = fth4 tagPair2SimTuple
         let sparseTagPair2SimList = [x | x <- tagPair2SimList, snd x /= fromIntegral 0]
         putStrLn $ "countInTree: The number of different PhraSyns: " ++ show (fst4 tagPair2SimTuple)
         putStrLn $ "countInTree: The number of different grammatic rule tags: " ++ show (snd4 tagPair2SimTuple)
         putStrLn $ "countInTree: The number of different tag pairs: " ++ show (thd4 tagPair2SimTuple)
         cOrS <- getLineUntil "Print complete matrix or print sparse matrix? ['c' or RETURN for complete, 's' for sparse] " ["c","s"] True
         if cOrS == "c"
           then do
             putStr $ "countInTree: The whole tagPair2SimList: "
             showTagPair2SimList (formatMapListWithDoubleValue tagPair2SimList 4)
           else do
             putStr $ "countInTree: sparse tagPair2SimList: "
             showTagPair2SimList (formatMapListWithDoubleValue sparseTagPair2SimList 4)
       else putStr ""

-- To calculate similarities between every pair of phrasal structures.
    if funcIndex == 14
       then do
         let struPair2SimTuple = case phrasyn of
                                   "phrasyn" -> getStruPair2SimFromSCPL sentClauPhraList       -- (NumOfPhraSyn, NumOfPhraStru, NumOfStruPair, [((PhraStru, PhraStru), SimDeg)])
                                   "phrasyn0" -> getStruPair2SimFromSCPL0 sentClauPhraList     -- (NumOfPhraSyn, NumOfPhraStru, NumOfStruPair, [((PhraStru, PhraStru), SimDeg)])
                                   _ -> error $ "countInTree: Property " ++ phrasyn ++ " can not be recognized."
         let struPair2SimList = fth4 struPair2SimTuple
         let sparseStruPair2SimList = [x | x <- struPair2SimList, snd x /= fromIntegral 0]
         putStrLn $ "countInTree: The number of different PhraSyns: " ++ show (fst4 struPair2SimTuple)
         putStrLn $ "countInTree: The number of different phrasal structures: " ++ show (snd4 struPair2SimTuple)
         putStrLn $ "countInTree: The number of different pairs of phrasal structures: " ++ show (thd4 struPair2SimTuple)
         cOrS <- getLineUntil "Print complete matrix or print sparse matrix? ['c' or RETURN for complete, 's' for sparse] " ["c","s"] True
         if cOrS == "c"
           then do
             putStr $ "countInTree: The whole struPair2SimList: "
             showStruPair2SimList (formatMapListWithDoubleValue struPair2SimList 4)
           else do
             putStr $ "countInTree: sparse struPair2SimList: "
             showStruPair2SimList (formatMapListWithDoubleValue sparseStruPair2SimList 4)
       else putStr ""

-- By Singular Value Decomposition (SVD), calculate similarities between every pair of phrases in their grammatic feature (Category, Tag, PhraStru, Span).
    if funcIndex == 15
       then do
         confInfo <- readFile "Configuration"
         let distAlgo = getConfProperty "phra_gram_dist_algo" confInfo

         phraSynPairSimTuple <- getPhraSynPairSimFromSCPLBySVD distAlgo sentClauPhraList
         let numOfPhraSynPair = length (fst5 phraSynPairSimTuple)
         putStrLn $ "countInTree: Num. of phraSyn pairs: " ++ show numOfPhraSynPair

         let origSimMatrix = snd5 phraSynPairSimTuple
         let origSimMatrixToRows = map toList $ toRows origSimMatrix                         -- [[SimDeg]]
         let origSimByRMS = case distAlgo of
                              "Euclidean" -> map (sqrt . (/ 4.0) . sumElements) (toRows (cmap (\x -> x*x) origSimMatrix))
                              "Manhattan" -> map ((/ 4.0) . sumElements) (toRows origSimMatrix)
         let origSim2RMSMatrix = fromLists $ map (\x -> fst x ++ [snd x]) $ zip origSimMatrixToRows origSimByRMS     -- hmatrix

         let orthSimMatrix = fth5 phraSynPairSimTuple
         let orthSimMatrixToRows = map toList $ toRows orthSimMatrix                         -- [[SimDeg]]
         let orthSimByRMS = case distAlgo of
                              "Euclidean" -> map (sqrt . (/ 4.0) . sumElements) (toRows (cmap (\x -> x*x) orthSimMatrix))
                              "Manhattan" -> map ((/ 4.0) . sumElements) (toRows orthSimMatrix)
         let orthSim2RMSMatrix = fromLists $ map (\x -> fst x ++ [snd x]) $ zip orthSimMatrixToRows orthSimByRMS     -- hmatrix

         if (numOfPhraSynPair > 10)
           then do
             putStrLn "countInTree: The first 10 elements of phraSynPairList: "
             dispList 1 (take 10 (fst5 phraSynPairSimTuple))
             putStrLn "countInTree: The first 10 elements of origSim2RMSMatrix: "
             disp 4 (origSim2RMSMatrix ?? (Take 10, Drop 0))
             putStrLn "countInTree: covSimMatrix: "
             disp 4 (thd5 phraSynPairSimTuple)
             putStrLn "countInTree: The first 10 rows of orthSim2RMSMatrix: "
             disp 4 (orthSim2RMSMatrix ?? (Take 10, Drop 0))
             putStrLn "countInTree: The first 10 rows of phraSynPair2SimList: "
             dispList 1 (take 10 (fif5 phraSynPairSimTuple))
           else do
             putStrLn "countInTree: phraSynPairList: "
             dispList 1 (fst5 phraSynPairSimTuple)
             putStrLn "countInTree: origSim2RMSMatrix: "
             disp 4 origSim2RMSMatrix
             putStrLn "countInTree: covSimMatrix: "
             disp 4 (thd5 phraSynPairSimTuple)
             putStrLn "countInTree: orthSim2RMSMatrix: "
             disp 4 orthSim2RMSMatrix
             putStrLn "countInTree: phraSynPair2SimList: "
             dispList 1 (fif5 phraSynPairSimTuple)
       else putStr ""

{- Get statistics about field 'script' in Table <script_source> whose serial numbers are less than 'topSn' and
 - bigger than or equal to 'bottomSn', here 'funcIndex' indicates which statistics will be done.
 - <script_source> is the value of attribute 'script_source' in file Configuration.
 -}
countInScript :: Int -> Int -> Int -> IO ()
countInScript bottomSn topSn funcIndex = do
    conn <- getConn
    confInfo <- readFile "Configuration"                                        -- Read the local configuration file
    let script_source = getConfProperty "script_source" confInfo
    putStrLn $ "The source of parsing scripts is set as: " ++ script_source       -- Display the source of parsing trees
    let sqlstat = DS.fromString $ "select script from " ++ script_source ++ " where serial_num >= ? and serial_num < ?"
    stmt <- prepareStmt conn sqlstat
    (defs, is) <- queryStmt conn stmt [toMySQLInt32 bottomSn, toMySQLInt32 topSn]

    sentScriptStrList <- readStreamByText [] is                     -- [String], here a string is the parsing script of a sentence.
    let sentClauScriptStrList = map stringToList sentScriptStrList        -- [[String]], here a string is the parsing script of a clause.
    let sentClauNumList = map length sentClauScriptStrList                -- [Int], here an integer is the number of clauses in a sentence.
    let sentNum = length sentScriptStrList                          -- The number of sentences.
    let clauseTotalNum = foldl (+) 0 sentClauNumList                -- The total number of clauses.
    let sentClauScriptList = map (map readScript) sentClauScriptStrList         -- [[Script]], here a Script is the representation in memory of parsing script of a clause.

    -- Output the following for test. Using 'showScripts' or else using 'show' should be decied again.
    -- putStr "countInScript: The script list of clauses in first sentence: "
    -- showScripts (sentClauScriptList!!0)
    -- putStr "countInScript: The script of first clause in first sentence: "
    -- showScripts' [((sentClauScriptList!!0)!!0)]
    -- putStrLn

    let tree_source = getConfProperty "tree_source" confInfo
    putStrLn $ "The source of parsing trees is set as: " ++ tree_source         -- Display the source of parsing trees
    let sqlstat2 = DS.fromString $ "select tree from " ++ tree_source ++ " where serial_num >= ? and serial_num < ?"
    stmt2 <- prepareStmt conn sqlstat2
    (defs2, is2) <- queryStmt conn stmt2 [toMySQLInt32 bottomSn, toMySQLInt32 topSn]
    sentTreeStrList <- readStreamByText [] is2                                  -- [String], here a string is the parsing result of a sentence.

    let sentClauTreeStrList = map stringToList sentTreeStrList             -- [[String]], here a string is the parsing result of a clause.
    let sentClauTreeList = map (map readTree) sentClauTreeStrList          -- [[Tree]], here a Tree value is the parsing result for a clause.
    let sentClauPhraList = map (map snd) sentClauTreeList                  -- [[[PhraCate]]], here a PhraCate is the representation in memory of a phrase.
    let sentClauLengthList = toSentClauLengthList sentClauTreeList         -- [[Int]], here every integer is the length of a clause.

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

         let transTimesTotal = fromIntegral $ foldl (+) 0 $ map fst descListOfTT2FreqByValue         -- The total number of transtive times for all clauses.
         let descListOfTT2NormalizedFreqByValue = map (\x -> (fst x, ((/ transTimesTotal) . fromIntegral . snd) x)) descListOfTT2FreqByValue    -- [(transitive times, normalized freq.)]
         putStrLn $ "countInScript: Transitive times total for all clauses: " ++ show transTimesTotal
         putStrLn $ "countInScript: The descending list of normalized frequencies of different transitive times: " ++ show (formatMapListWithFloatValue descListOfTT2NormalizedFreqByValue 4)
       else putStr ""

    if funcIndex == 3                                         -- To get the list of transitive times for every different clausal lengths.
       then do
         let sentClauTransTimesList = map (map (length . snd3)) sentClauScriptList      -- [[[Int]]], here every integer is the transitive times in parsing a clause.
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
                                                              -- [[[[Rule]]]], conversional rules(#4) used in every transition(#3) in parsing every clause(#2) in sentence list(#1).
         let f' = foldl (++) []
         let convRuleList = (nub . f' . f' . f') sentClauTransConvList     -- [Rule]
         let sentClauTransConvNumList = map (map (map length)) sentClauTransConvList
                                                              -- [[[num]]], list of numbers of conversional rules (#3) in parsing every clause(#2) in sentence list(#1).
         let sentClauConvTotalList = map (map (foldl (+) 0)) sentClauTransConvNumList
                                                              -- [[total]], list of totals of conversional rules in parsing every clause(#2) in sentence list(#1).
         putStrLn $ "countInScript: The conversion rules list of every clause in every sentences: " ++ show sentClauTransConvList
         putStrLn $ "countInScript: The total num. of conversional rules used in parsing every clause in every sentences: " ++ show sentClauConvTotalList
         putStrLn $ "countInScript: Conversion rules: " ++ show convRuleList ++ ", count " ++ (show . length) convRuleList
       else putStr ""

    if funcIndex == 5                                         -- Frequency of using type conversions in transitive computing for every clausal length.
       then do
         let sentClauTransConvList = map (map snd3) sentClauScriptList          -- [[[[tag]]]]
         let sentClauTransConvNumList = map (map (map length)) sentClauTransConvList        -- [[[num]]]
         let sentClauConvFreqList = map (map (foldl (+) 0)) sentClauTransConvNumList       -- [[total]]

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
countInStruGene :: String -> Int -> Int -> Int -> IO ()
countInStruGene syntax_ambig_resol_model startIdx endIdx funcIndex = do
    -- 1. Get total number of structural genes.
    if funcIndex == 1
       then do
         conn <- getConn
         let sqlstat = DS.fromString $ "select count(*) from " ++ syntax_ambig_resol_model
         stmt <- prepareStmt conn sqlstat
         (defs, is) <- queryStmt conn stmt []
         row <- S.read is
         let totalNum = case row of                           -- [Int], here a string is the parsing script of a sentence.
                          Just x -> x
                          Nothing -> error "countInStruGene: Failed in executing select count(*) from stru_gene."
         S.skipToEof is
         putStrLn $ "countInStruGene: " ++ syntax_ambig_resol_model ++ ": The total number of structural genes: " ++ show (fromMySQLInt64 (totalNum!!0))
         close conn
       else putStr ""

    -- 2. Get frequencies of different overlapping types, namely [(OverType, Freq)].
    if funcIndex == 2
       then do
         conn <- getConn
         let sqlstat = DS.fromString $ "select overType, count(*) from " ++ syntax_ambig_resol_model ++ " group by overType order by overType"
         stmt <- prepareStmt conn sqlstat
         (defs, is) <- queryStmt conn stmt []
         overType2FrequencyList <- readStreamByInt8Int64 [] is                  -- [[Int]], here every row has two integers, overType and its occuring frequency.
         putStrLn $ "countInStruGene: " ++ syntax_ambig_resol_model ++ ": Frequencies of different overlapping types [(OverType, Freq)]: " ++ show overType2FrequencyList
         close conn
       else putStr ""

    -- 3. Get frequencies of most common phrasal overlapping, namely [(LeftOver_RightOver_OverType, Int), here the common proportion is 'prop';
    if funcIndex == 3
       then do
         conn <- getConn
         let sqlstat = DS.fromString $ "select leftOver, rightOver, overType from " ++ syntax_ambig_resol_model
         stmt <- prepareStmt conn sqlstat
         (defs, is) <- queryStmt conn stmt []
         leftOver_RightOver_OverTypeList <- readStreamByTextTextInt8 [] is           -- [String], here the string is "LeftOver_RightOver_OverType"
         let leftOver_RightOver_OverType2FreqMap = keyToMap leftOver_RightOver_OverTypeList Map.empty           -- Map Sting Int, namely Map <LRO> <LRONum>.
         putStrLn $ "countInStruGene: " ++ syntax_ambig_resol_model ++ ": The total number of different LROs: " ++ show (Map.size leftOver_RightOver_OverType2FreqMap)

         let descListOfLRO2FreqByValue = toDescListOfMapByValue (Map.toList leftOver_RightOver_OverType2FreqMap)
--       putStrLn $ "countIn" ++ syntax_ambig_resol_model ++ ": The descending list of frequencies of different LROs: " ++ show descListOfLRO2FreqByValue

         putStr "Please input the percent proportion of frequency of most common LROs in frequency of all LROs [0.00-1.00]: "
         prop <- readFloat0to1
         let truncatedDescListOfLRO2FreqByProp = truncDescListByProportion prop descListOfLRO2FreqByValue
         let valueTotal = foldl (+) 0 (map snd descListOfLRO2FreqByValue)
         let valueTrunc = foldl (+) 0 (map snd truncatedDescListOfLRO2FreqByProp)
         let realProp = (/) (fromIntegral valueTrunc) (fromIntegral valueTotal) :: Float
         putStrLn $ "countInStruGene: " ++ syntax_ambig_resol_model ++ ": The truncated descending list of frequencies of different LROs by proportion " ++ (printf "%.02f" realProp) ++ ": " ++ show truncatedDescListOfLRO2FreqByProp
         close conn
       else putStr ""

    -- 4. Get frequencies of most common unambiguous phrasal overlapping, namely [(LeftOver_RightOver_OverType_Prior, Int), here the common proportion is 'prop';
    if funcIndex == 4
       then do
         conn <- getConn
         let sqlstat = DS.fromString $ "select leftOver, rightOver, overType, prior from " ++ syntax_ambig_resol_model
         stmt <- prepareStmt conn sqlstat
         (defs, is) <- queryStmt conn stmt []
         leftOver_RightOver_OverType_PriorList <- readStreamByTextTextInt8Text [] is        -- [String], here the string is "LeftOver_RightOver_OverType_Prior"
         let leftOver_RightOver_OverType_Prior2FreqMap = keyToMap leftOver_RightOver_OverType_PriorList Map.empty
                                                                                            -- Map Sting Int, namely Map <LROP> <LROPNum>.
         putStrLn $ "countInStruGene: " ++ syntax_ambig_resol_model ++ ": The total number of different LROPs: " ++ show (Map.size leftOver_RightOver_OverType_Prior2FreqMap)

         let descListOfLROP2FreqByValue = toDescListOfMapByValue (Map.toList leftOver_RightOver_OverType_Prior2FreqMap)
--       putStrLn $ "countInStruGene: " ++ syntax_ambig_resol_model ++ ": The descending list of frequencies of different LROPs: " ++ show descListOfLROP2FreqByValue

         putStr "Please input the percent proportion of frequency of most common LROPs in frequency of all LROPs [0.00-1.00]: "
         prop <- readFloat0to1
         let truncatedDescListOfLROP2FreqByProp = truncDescListByProportion prop descListOfLROP2FreqByValue
         let valueTotal = foldl (+) 0 (map snd descListOfLROP2FreqByValue)
         let valueTrunc = foldl (+) 0 (map snd truncatedDescListOfLROP2FreqByProp)
         let realProp = (/) (fromIntegral valueTrunc) (fromIntegral valueTotal) :: Float
         putStrLn $ "countInStruGene: " ++ syntax_ambig_resol_model ++ ": The truncated descending list of frequencies of different LROPs by proportion " ++ (printf "%.02f" realProp) ++ ": " ++ show truncatedDescListOfLROP2FreqByProp
         close conn
       else putStr ""

    -- 5. Get hit count of different overlapping types, namely [(OverType, HitCount)], where HitCount = LpHitCount + RpHitCount + NothHitCount.
    if funcIndex == 5
       then do
         conn <- getConn
         let sqlstat = DS.fromString $ "select overType, sum(lpHitCount + rpHitCount + nothHitCount) as hitCount from " ++ syntax_ambig_resol_model ++ " group by overType order by overType"
         stmt <- prepareStmt conn sqlstat
         (defs, is) <- queryStmt conn stmt []
         overType2HitCountList <- readStreamByInt8Decimal [] is                 -- [(Int,Double)], here every row has overType and its hit count.
         putStrLn $ "countInStruGene: " ++ syntax_ambig_resol_model ++ ": HitCounts of different overlapping types [(OverType, HitCount)]: " ++ show (map (\x->(fst x, floor (snd x))) overType2HitCountList)
         close conn
       else putStr ""

    -- 6.  Get similarity degree between every pair of categories.
    if funcIndex == 6
       then do
         confInfo <- readFile "Configuration"
         let syntax_ambig_resol_model = getConfProperty "syntax_ambig_resol_model" confInfo
         conn <- getConn
         typePair2SimTuple <- case syntax_ambig_resol_model of
           x | elem x ["stru_gene_202501"] -> do
             let sqlstat = DS.fromString $ "select leftExtend, leftOver, rightOver, rightExtend from " ++ syntax_ambig_resol_model ++ " where id >= ? and id <= ?"
             stmt <- prepareStmt conn sqlstat
             (defs, is) <- queryStmt conn stmt [toMySQLInt32U startIdx, toMySQLInt32U endIdx]

             contextOfOTStrList <- readStreamByTextTextTextText [] is               -- [(String, String, String, String)]
             let contextOfOTList = map (\x -> (readPhraSynListFromStr (fst4 x), readPhraSynFromStr (snd4 x), readPhraSynFromStr (thd4 x), readPhraSynListFromStr (fth4 x))) contextOfOTStrList
             let (les, los, ros, res) = unzip4 contextOfOTList                      -- Extract [LeftExtend], [LeftOver], [RightOver] and [RightExtend].
             let phraSynList = nub $ foldl (++) [] les ++ los ++ ros ++ (foldl (++) [] res)  -- [PhraSyn]
             return $ getTypePair2SimFromPSL phraSynList             -- (numOfPhraSyn, numOfCate, numOfCatePair, [((Category, Category), SimDeg)])

           x | elem x ["stru_gene3_202508"] -> do
             let sqlstat = DS.fromString $ "select leftExtend, leftOverTree, rightOverTree, rightExtend from " ++ syntax_ambig_resol_model ++ " where id >= ? and id <= ?"
             stmt <- prepareStmt conn sqlstat
             (defs, is) <- queryStmt conn stmt [toMySQLInt32U startIdx, toMySQLInt32U endIdx]

             contextOfOTStrList <- readStreamByTextTextTextText [] is               -- [(String, String, String, String)]
             let contextOfOTList = map (\x -> (readPhraSynListFromStr (fst4 x), (traverseBiTree . readBiTreePhraSynFromStr) (snd4 x), (traverseBiTree . readBiTreePhraSynFromStr) (thd4 x), readPhraSynListFromStr (fth4 x))) contextOfOTStrList
             let (les, lots, rots, res) = unzip4 contextOfOTList                      -- Extract [LeftExtend], [LeftOver], [RightOver] and [RightExtend].
             let phraSynList = nub $ foldl (++) [] les ++ (foldl (++) [] lots) ++ (foldl (++) [] rots) ++ (foldl (++) [] res)   -- [PhraSyn]
             return $ getTypePair2SimFromPSL phraSynList             -- (numOfPhraSyn, numOfCate, numOfCatePair, [((Category, Category), SimDeg)])

           x | elem x ["stru_gene3a_phrasyn0_202509"] -> do
             let sqlstat = DS.fromString $ "select leftOverTree, rightOverTree from " ++ syntax_ambig_resol_model ++ " where id >= ? and id <= ?"
             stmt <- prepareStmt conn sqlstat
             (defs, is) <- queryStmt conn stmt [toMySQLInt32U startIdx, toMySQLInt32U endIdx]

             contextStrList <- readStreamByTextText [] is                             -- [(String, String)]
             let contextList = map (\x -> ((traverseBiTree . readBiTreePhraSyn0FromStr) (fst x), (traverseBiTree . readBiTreePhraSyn0FromStr) (snd x))) contextStrList
             let (lots, rots) = unzip contextList                                     -- Extract ([LeftOverTree], [RightOverTree]).
             let phraSyn0List = nub $ foldl (++) [] lots ++ (foldl (++) [] rots)    -- [PhraSyn0]
             return $ getTypePair2SimFromPS0L phraSyn0List             -- (numOfPhraSyn, numOfCate, numOfCatePair, [((Category, Category), SimDeg)])

           _ -> error $ "countInStruGene: " ++ syntax_ambig_resol_model ++ " is NOT recognized."

         let typePair2SimList = fth4 typePair2SimTuple                          -- [((Category, Category), SimDeg)]
         let sparseTypePair2SimList = [x | x <- typePair2SimList, snd x /= fromIntegral 0]
         putStrLn $ "countInStruGene: The number of different PhraSyns: " ++ show (fst4 typePair2SimTuple)
         putStrLn $ "countInStruGene: The number of different categories: " ++ show (snd4 typePair2SimTuple)
         putStrLn $ "countInStruGene: The number of different category pairs: " ++ show (thd4 typePair2SimTuple)

         let tblName = "type_sim_" ++ show (endIdx - startIdx + 1)              -- "type_sim_xxx"
         putStrLn $ " Table " ++ tblName ++ " will be created again."
         let sqlstat = DS.fromString $ "DROP TABLE IF EXISTS " ++ tblName       -- Drop table if exists
         stmt <- prepareStmt conn sqlstat
         executeStmt conn stmt []
         let sqlstat = DS.fromString $ "CREATE TABLE IF NOT EXISTS " ++ tblName ++ " (id SMALLINT UNSIGNED PRIMARY KEY AUTO_INCREMENT, cate1 VARCHAR(100), cate2 VARCHAR(100), sim DOUBLE)"
         stmt <- prepareStmt conn sqlstat
         executeStmt conn stmt []
         let sqlstat = DS.fromString $ "INSERT INTO " ++ tblName ++ " SET cate1 = ?, cate2 = ?, sim = ?"

         cOrS <- getLineUntil "Store complete or sparse triangular matrix into database? ['c' or RETURN for complete, 's' for sparse] " ["c","s"] True
         if cOrS == "c"
           then do
             putStr $ "countInStruGene: Complete typePair2SimList: "
             showCatePair2SimList (formatMapListWithDoubleValue typePair2SimList 4)
             let typePair2SimMySQLValueList = map (\x -> [toMySQLText ((show . fst . fst) x), toMySQLText ((show . snd . fst) x), toMySQLDouble (snd x)]) typePair2SimList  -- [[MySQLValue]]
             oks <- executeMany conn sqlstat typePair2SimMySQLValueList                   -- Store complete typePair2SimList into type_sim_xxx
             putStrLn $ "countInStruGene: " ++ show (length oks) ++ " rows have been inserted."
             putStrLn $ "countInStruGene: Last inserted ID = " ++ show (getOkLastInsertID (last oks))
           else do
             putStr $ "countInStruGene: Sparse typePair2SimList: "
             showCatePair2SimList (formatMapListWithDoubleValue sparseTypePair2SimList 4)
             let sparseTypePair2SimMySQLValueList = map (\x -> [toMySQLText ((show . fst . fst) x), toMySQLText ((show . snd . fst) x), toMySQLDouble (snd x)]) sparseTypePair2SimList  -- [[MySQLValue]]
             oks <- executeMany conn sqlstat sparseTypePair2SimMySQLValueList              -- Store sparse typePair2SimList into type_sim_xxx
             putStrLn $ "countInStruGene: " ++ show (length oks) ++ " rows have been inserted."
             putStrLn $ "countInStruGene: Last inserted ID = " ++ show (getOkLastInsertID (last oks))
         close conn
       else putStr ""

    -- 7.  Get similarity degree between every pair of grammatic rules.
    if funcIndex == 7
       then do
         conn <- getConn
         tagPair2SimTuple <- case syntax_ambig_resol_model of
           x | elem x ["stru_gene_202501"] -> do
             let sqlstat = DS.fromString $ "select leftExtend, leftOver, rightOver, rightExtend from " ++ syntax_ambig_resol_model ++ " where id >= ? and id <= ?"
             stmt <- prepareStmt conn sqlstat
             (defs, is) <- queryStmt conn stmt [toMySQLInt32U startIdx, toMySQLInt32U endIdx]

             contextOfOTStrList <- readStreamByTextTextTextText [] is               -- [(String, String, String, String)]
             let contextOfOTList = map (\x -> (readPhraSynListFromStr (fst4 x), readPhraSynFromStr (snd4 x), readPhraSynFromStr (thd4 x), readPhraSynListFromStr (fth4 x))) contextOfOTStrList
             let (les, los, ros, res) = unzip4 contextOfOTList                      -- Extract [LeftExtend], [LeftOver], [RightOver] and [RightExtend].
             let phraSynList = nub $ foldl (++) [] les ++ los ++ ros ++ (foldl (++) [] res)  -- [PhraSyn]
             return $ getTagPair2SimFromPSL phraSynList               -- (numOfPhraSyn, numOfTag, numOfTagPair, [((Tag, Tag), SimDeg)])

           x | elem x ["stru_gene3_202508"] -> do
             let sqlstat = DS.fromString $ "select leftExtend, leftOverTree, rightOverTree, rightExtend from " ++ syntax_ambig_resol_model ++ " where id >= ? and id <= ?"
             stmt <- prepareStmt conn sqlstat
             (defs, is) <- queryStmt conn stmt [toMySQLInt32U startIdx, toMySQLInt32U endIdx]

             contextOfOTStrList <- readStreamByTextTextTextText [] is               -- [(String, String, String, String)]
             let contextOfOTList = map (\x -> (readPhraSynListFromStr (fst4 x), (traverseBiTree . readBiTreePhraSynFromStr) (snd4 x), (traverseBiTree . readBiTreePhraSynFromStr) (thd4 x), readPhraSynListFromStr (fth4 x))) contextOfOTStrList
             let (les, lots, rots, res) = unzip4 contextOfOTList                      -- Extract [LeftExtend], [LeftOver], [RightOver] and [RightExtend].
             let phraSynList = nub $ foldl (++) [] les ++ (foldl (++) [] lots) ++ (foldl (++) [] rots) ++ (foldl (++) [] res)   -- [PhraSyn]
             return $ getTagPair2SimFromPSL phraSynList               -- (numOfPhraSyn, numOfTag, numOfTagPair, [((Tag, Tag), SimDeg)])

           x | elem x ["stru_gene3a_phrasyn0_202509"] -> do
             let sqlstat = DS.fromString $ "select leftOverTree, rightOverTree from " ++ syntax_ambig_resol_model ++ " where id >= ? and id <= ?"
             stmt <- prepareStmt conn sqlstat
             (defs, is) <- queryStmt conn stmt [toMySQLInt32U startIdx, toMySQLInt32U endIdx]

             contextStrList <- readStreamByTextText [] is                             -- [(String, String)]
             let contextList = map (\x -> ((traverseBiTree . readBiTreePhraSyn0FromStr) (fst x), (traverseBiTree . readBiTreePhraSyn0FromStr) (snd x))) contextStrList
             let (lots, rots) = unzip contextList                                     -- Extract ([LeftOverTree], [RightOverTree]).
             let phraSyn0List = nub $ foldl (++) [] lots ++ (foldl (++) [] rots)                -- [PhraSyn0]
             return $ getTagPair2SimFromPS0L phraSyn0List               -- (numOfPhraSyn, numOfTag, numOfTagPair, [((Tag, Tag), SimDeg)])

           _ -> error $ "countInStruGene: " ++ syntax_ambig_resol_model ++ " is NOT recognized."

         let tagPair2SimList = fth4 tagPair2SimTuple                            -- [((Tag, Tag), SimDeg)]
         let sparseTagPair2SimList = [x | x <- tagPair2SimList, snd x /= fromIntegral 0]
         putStrLn $ "countInStruGene: The number of different phraSyns: " ++ show (fst4 tagPair2SimTuple)
         putStrLn $ "countInStruGene: The number of different tags: " ++ show (snd4 tagPair2SimTuple)
         putStrLn $ "countInStruGene: The number of different tag pairs: " ++ show (thd4 tagPair2SimTuple)

         let tblName = "tag_sim_" ++ show (endIdx - startIdx + 1)               -- "tag_sim_xxx"
         putStrLn $ " Table " ++ tblName ++ " will be created again."
         let sqlstat = DS.fromString $ "DROP TABLE IF EXISTS " ++ tblName       -- Drop table if exists
         stmt <- prepareStmt conn sqlstat
         executeStmt conn stmt []
         let sqlstat = DS.fromString $ "CREATE TABLE IF NOT EXISTS " ++ tblName ++ " (id SMALLINT UNSIGNED PRIMARY KEY AUTO_INCREMENT, tag1 VARCHAR(50), tag2 VARCHAR(50), sim DOUBLE)"
         stmt <- prepareStmt conn sqlstat
         executeStmt conn stmt []
         let sqlstat = DS.fromString $ "INSERT INTO " ++ tblName ++ " SET tag1 = ?, tag2 = ?, sim = ?"

         cOrS <- getLineUntil "Store complete or sparse triangular matrix into database? ['c' or RETURN for complete, 's' for sparse] " ["c","s"] True
         if cOrS == "c"
           then do
             putStr $ "countInStruGene: The whole tagPair2SimList: "
             showTagPair2SimList (formatMapListWithDoubleValue tagPair2SimList 4)
             let tagPair2SimMySQLValueList = map (\x -> [toMySQLText ((fst . fst) x), toMySQLText ((snd . fst) x), toMySQLDouble (snd x)]) tagPair2SimList  -- [[MySQLValue]]
             oks <- executeMany conn sqlstat tagPair2SimMySQLValueList           -- Store complete tagPair2SimList into tag_sim_xxx
             putStrLn $ "countInStruGene: " ++ show (length oks) ++ " rows have been inserted."
             putStrLn $ "countInStruGene: Last inserted ID = " ++ show (getOkLastInsertID (last oks))
           else do
             putStr $ "countInStruGene: sparse tagPair2SimList: "
             showTagPair2SimList (formatMapListWithDoubleValue sparseTagPair2SimList 4)
             let sparseTagPair2SimMySQLValueList = map (\x -> [toMySQLText ((fst . fst) x), toMySQLText ((snd . fst) x), toMySQLDouble (snd x)]) sparseTagPair2SimList  -- [[MySQLValue]]
             oks <- executeMany conn sqlstat sparseTagPair2SimMySQLValueList     -- Store sparse tagPair2SimList into tag_sim_xxx
             putStrLn $ "countInStruGene: " ++ show (length oks) ++ " rows have been inserted."
             putStrLn $ "countInStruGene: Last inserted ID = " ++ show (getOkLastInsertID (last oks))
         close conn
       else putStr ""

    -- 8.  Get similarity degree between every pair of phrasal structures.
    if funcIndex == 8
       then do
         conn <- getConn
         struPair2SimTuple <- case syntax_ambig_resol_model of
           x | elem x ["stru_gene_202501"] -> do
             let sqlstat = DS.fromString $ "select leftExtend, leftOver, rightOver, rightExtend from " ++ syntax_ambig_resol_model ++ " where id >= ? and id <= ?"
             stmt <- prepareStmt conn sqlstat
             (defs, is) <- queryStmt conn stmt [toMySQLInt32U startIdx, toMySQLInt32U endIdx]

             contextOfOTStrList <- readStreamByTextTextTextText [] is               -- [(String, String, String, String)]
             let contextOfOTList = map (\x -> (readPhraSynListFromStr (fst4 x), readPhraSynFromStr (snd4 x), readPhraSynFromStr (thd4 x), readPhraSynListFromStr (fth4 x))) contextOfOTStrList
             let (les, los, ros, res) = unzip4 contextOfOTList                      -- Extract [LeftExtend], [LeftOver], [RightOver] and [RightExtend].
             let phraSynList = nub $ foldl (++) [] les ++ los ++ ros ++ (foldl (++) [] res)  -- [PhraSyn]
             return $ getStruPair2SimFromPSL phraSynList             -- (numOfPhraSyn, numOfPhraStru, numOfStruPair, [((PhraStru, PhraStru), SimDeg)])

           x | elem x ["stru_gene3_202508"] -> do
             let sqlstat = DS.fromString $ "select leftExtend, leftOverTree, rightOverTree, rightExtend from " ++ syntax_ambig_resol_model ++ " where id >= ? and id <= ?"
             stmt <- prepareStmt conn sqlstat
             (defs, is) <- queryStmt conn stmt [toMySQLInt32U startIdx, toMySQLInt32U endIdx]

             contextOfOTStrList <- readStreamByTextTextTextText [] is               -- [(String, String, String, String)]
             let contextOfOTList = map (\x -> (readPhraSynListFromStr (fst4 x), (traverseBiTree . readBiTreePhraSynFromStr) (snd4 x), (traverseBiTree . readBiTreePhraSynFromStr) (thd4 x), readPhraSynListFromStr (fth4 x))) contextOfOTStrList
             let (les, lots, rots, res) = unzip4 contextOfOTList                      -- Extract [LeftExtend], [LeftOver], [RightOver] and [RightExtend].
             let phraSynList = nub $ foldl (++) [] les ++ (foldl (++) [] lots) ++ (foldl (++) [] rots) ++ (foldl (++) [] res)   -- [PhraSyn]
             return $ getStruPair2SimFromPSL phraSynList             -- (numOfPhraSyn, numOfPhraStru, numOfStruPair, [((PhraStru, PhraStru), SimDeg)])

           x | elem x ["stru_gene3a_phrasyn0_202509"] -> do
             let sqlstat = DS.fromString $ "select leftOverTree, rightOverTree from " ++ syntax_ambig_resol_model ++ " where id >= ? and id <= ?"
             stmt <- prepareStmt conn sqlstat
             (defs, is) <- queryStmt conn stmt [toMySQLInt32U startIdx, toMySQLInt32U endIdx]

             contextStrList <- readStreamByTextText [] is                             -- [(String, String)]
             let contextList = map (\x -> ((traverseBiTree . readBiTreePhraSyn0FromStr) (fst x), (traverseBiTree . readBiTreePhraSyn0FromStr) (snd x))) contextStrList
             let (lots, rots) = unzip contextList                                     -- Extract ([LeftOverTree], [RightOverTree]).
             let phraSyn0List = nub $ foldl (++) [] lots ++ (foldl (++) [] rots)                -- [PhraSyn0]
             return $ getStruPair2SimFromPS0L phraSyn0List             -- (numOfPhraSyn, numOfPhraStru, numOfStruPair, [((PhraStru, PhraStru), SimDeg)])

           _ -> error $ "countInStruGene: " ++ syntax_ambig_resol_model ++ " is NOT recognized."

         let struPair2SimList = fth4 struPair2SimTuple                          -- [((PhraStru, PhraStru), SimDeg)]
         let sparseStruPair2SimList = [x | x <- struPair2SimList, snd x /= fromIntegral 0]
         putStrLn $ "countInStruGene: The number of different phraSyns: " ++ show (fst4 struPair2SimTuple)
         putStrLn $ "countInStruGene: The number of different phrasal structures: " ++ show (snd4 struPair2SimTuple)
         putStrLn $ "countInStruGene: The number of different structure pairs: " ++ show (thd4 struPair2SimTuple)

         let tblName = "stru_sim_" ++ show (endIdx - startIdx + 1)              -- "stru_sim_xxx"
         putStrLn $ " Table " ++ tblName ++ " will be created again."
         let sqlstat = DS.fromString $ "DROP TABLE IF EXISTS " ++ tblName       -- Drop table if exists
         stmt <- prepareStmt conn sqlstat
         executeStmt conn stmt []
         let sqlstat = DS.fromString $ "CREATE TABLE IF NOT EXISTS " ++ tblName ++ " (id SMALLINT UNSIGNED PRIMARY KEY AUTO_INCREMENT, stru1 VARCHAR(10), stru2 VARCHAR(10), sim DOUBLE)"
         stmt <- prepareStmt conn sqlstat
         executeStmt conn stmt []
         let sqlstat = DS.fromString $ "INSERT INTO " ++ tblName ++ " SET stru1 = ?, stru2 = ?, sim = ?"

         cOrS <- getLineUntil "Store complete or sparse triangular matrix into database? ['c' or RETURN for complete, 's' for sparse] " ["c","s"] True
         if cOrS == "c"
           then do
             putStr $ "countInStruGene: The whole struPair2SimList: "
             showStruPair2SimList (formatMapListWithDoubleValue struPair2SimList 4)
             let struPair2SimMySQLValueList = map (\x -> [toMySQLText ((fst . fst) x), toMySQLText ((snd . fst) x), toMySQLDouble (snd x)]) struPair2SimList  -- [[MySQLValue]]
             oks <- executeMany conn sqlstat struPair2SimMySQLValueList          -- Store complete struPair2SimList into stru_sim_xxx
             putStrLn $ "countInStruGene: " ++ show (length oks) ++ " rows have been inserted."
             putStrLn $ "countInStruGene: Last inserted ID = " ++ show (getOkLastInsertID (last oks))
           else do
             putStr $ "countInStruGene: sparse struPair2SimList: "
             showStruPair2SimList (formatMapListWithDoubleValue sparseStruPair2SimList 4)
             let sparseStruPair2SimMySQLValueList = map (\x -> [toMySQLText ((fst . fst) x), toMySQLText ((snd . fst) x), toMySQLDouble (snd x)]) sparseStruPair2SimList  -- [[MySQLValue]]
             oks <- executeMany conn sqlstat sparseStruPair2SimMySQLValueList    -- Store sparse struPair2SimList into stru_sim_xxx
             putStrLn $ "countInStruGene: " ++ show (length oks) ++ " rows have been inserted."
             putStrLn $ "countInStruGene: Last inserted ID = " ++ show (getOkLastInsertID (last oks))
         close conn
       else putStr ""

    -- 9.  Get similarity degree between every pair of phrasal spans.
    if funcIndex == 9
       then do
         conn <- getConn
         phraSynList <- case syntax_ambig_resol_model of
           x | elem x ["stru_gene_202501"] -> do
             let sqlstat = DS.fromString $ "select leftExtend, leftOver, rightOver, rightExtend from " ++ syntax_ambig_resol_model ++ " where id >= ? and id <= ?"
             stmt <- prepareStmt conn sqlstat
             (defs, is) <- queryStmt conn stmt [toMySQLInt32U startIdx, toMySQLInt32U endIdx]

             contextOfOTStrList <- readStreamByTextTextTextText [] is               -- [(String, String, String, String)]
             let contextOfOTList = map (\x -> (readPhraSynListFromStr (fst4 x), readPhraSynFromStr (snd4 x), readPhraSynFromStr (thd4 x), readPhraSynListFromStr (fth4 x))) contextOfOTStrList
             let (les, los, ros, res) = unzip4 contextOfOTList                      -- Extract [LeftExtend], [LeftOver], [RightOver] and [RightExtend].
             return $ nub $ foldl (++) [] les ++ los ++ ros ++ (foldl (++) [] res)  -- [PhraSyn]

           x | elem x ["stru_gene3_202508"] -> do
             let sqlstat = DS.fromString $ "select leftExtend, leftOverTree, rightOverTree, rightExtend from " ++ syntax_ambig_resol_model ++ " where id >= ? and id <= ?"
             stmt <- prepareStmt conn sqlstat
             (defs, is) <- queryStmt conn stmt [toMySQLInt32U startIdx, toMySQLInt32U endIdx]

             contextOfOTStrList <- readStreamByTextTextTextText [] is               -- [(String, String, String, String)]
             let contextOfOTList = map (\x -> (readPhraSynListFromStr (fst4 x), (traverseBiTree . readBiTreePhraSynFromStr) (snd4 x), (traverseBiTree . readBiTreePhraSynFromStr) (thd4 x), readPhraSynListFromStr (fth4 x))) contextOfOTStrList
             let (les, lots, rots, res) = unzip4 contextOfOTList                      -- Extract [LeftExtend], [LeftOver], [RightOver] and [RightExtend].
             return $ nub $ foldl (++) [] les ++ (foldl (++) [] lots) ++ (foldl (++) [] rots) ++ (foldl (++) [] res)   -- [PhraSyn]

           _ -> error $ "countInStruGene: " ++ syntax_ambig_resol_model ++ " is NOT recognized."

         let spanPair2SimTuple = getSpanPair2SimFromPSL phraSynList             -- (numOfPhraSyn, numOfSpan, numOfSpanPair, [((Span, Span), SimDeg)])
         let spanPair2SimList = fth4 spanPair2SimTuple                          -- [((Span, Span), SimDeg)]
         let sparseSpanPair2SimList = [x | x <- spanPair2SimList, snd x /= fromIntegral 0]
         putStrLn $ "countInStruGene: The number of different phraSyns: " ++ show (fst4 spanPair2SimTuple)
         putStrLn $ "countInStruGene: The number of different phrasal spans: " ++ show (snd4 spanPair2SimTuple)
         putStrLn $ "countInStruGene: The number of different span pairs: " ++ show (thd4 spanPair2SimTuple)

         let tblName = "span_sim_" ++ show (endIdx - startIdx + 1)              -- "span_sim_xxx"
         putStrLn $ " Table " ++ tblName ++ " will be created again."
         let sqlstat = DS.fromString $ "DROP TABLE IF EXISTS " ++ tblName       -- Drop table if exists
         stmt <- prepareStmt conn sqlstat
         executeStmt conn stmt []
         let sqlstat = DS.fromString $ "CREATE TABLE IF NOT EXISTS " ++ tblName ++ " (id SMALLINT UNSIGNED PRIMARY KEY AUTO_INCREMENT, span1 TINYINT, span2 TINYINT, sim DOUBLE)"
         stmt <- prepareStmt conn sqlstat
         executeStmt conn stmt []
         let sqlstat = DS.fromString $ "INSERT INTO " ++ tblName ++ " SET span1 = ?, span2 = ?, sim = ?"

         cOrS <- getLineUntil "Store complete or sparse triangular matrix into database? ['c' or RETURN for complete, 's' for sparse] " ["c","s"] True
         if cOrS == "c"
           then do
             putStr $ "countInStruGene: The whole spanPair2SimList: "
             showSpanPair2SimList (formatMapListWithDoubleValue spanPair2SimList 4)
             let spanPair2SimMySQLValueList = map (\x -> [toMySQLText ((show . fst . fst) x), toMySQLText ((show . snd . fst) x), toMySQLDouble (snd x)]) spanPair2SimList  -- [[MySQLValue]]
             oks <- executeMany conn sqlstat spanPair2SimMySQLValueList          -- Store complete spanPair2SimList into span_sim_xxx
             putStrLn $ "countInStruGene: " ++ show (length oks) ++ " rows have been inserted."
             putStrLn $ "countInStruGene: Last inserted ID = " ++ show (getOkLastInsertID (last oks))
           else do
             putStr $ "countInStruGene: sparse spanPair2SimList: "
             showSpanPair2SimList (formatMapListWithDoubleValue sparseSpanPair2SimList 4)
             let sparseSpanPair2SimMySQLValueList = map (\x -> [toMySQLText ((show . fst . fst) x), toMySQLText ((show . snd . fst) x), toMySQLDouble (snd x)]) sparseSpanPair2SimList  -- [[MySQLValue]]
             oks <- executeMany conn sqlstat sparseSpanPair2SimMySQLValueList    -- Store sparse spanPair2SimList into span_sim_xxx
             putStrLn $ "countInStruGene: " ++ show (length oks) ++ " rows have been inserted."
             putStrLn $ "countInStruGene: Last inserted ID = " ++ show (getOkLastInsertID (last oks))
         close conn
       else putStr ""

    {- 10. Get similarity degree between every pair of PhraSyn (or PhraSyn0) values.
     - For Model StruGene3a0, PhraSyn0 pairs come from BiTree PhraSyn0 pairs. Suppose number of BiTree PhraSyn0 samples is n.
     - Number of BiTree PhraSyn0 pairs is n^2, and number of PhraSyn0 pairs is bigger than n^2 because a tree has more than one PhraSyn0 node.
     - So, Computation on (PhraSyn0, PhraSyn0) pairs is impossible when size of pairs increases.
     -}
    if funcIndex == 10
       then do
         conn <- getConn
         confInfo <- readFile "Configuration"
         let phra_gram_dist_algo = getConfProperty "phra_gram_dist_algo" confInfo
         let phrasyn_sim_tbl = getConfProperty "phrasyn_sim_tbl" confInfo
         let sqlstat = DS.fromString $ "CREATE TABLE IF NOT EXISTS " ++ phrasyn_sim_tbl ++ " (id INT UNSIGNED PRIMARY KEY AUTO_INCREMENT, phrasyn1 VARCHAR(70), phrasyn2 VARCHAR(70), sim DOUBLE)"
         stmt <- prepareStmt conn sqlstat
         executeStmt conn stmt []
         closeStmt conn stmt

         case syntax_ambig_resol_model of
           x | elem x ["stru_gene_202501"] -> do
             let sqlstat = DS.fromString $ "select leftExtend, leftOver, rightOver, rightExtend from " ++ syntax_ambig_resol_model ++ " where id >= ? and id <= ?"
             stmt <- prepareStmt conn sqlstat
             (defs, is) <- queryStmt conn stmt [toMySQLInt32U startIdx, toMySQLInt32U endIdx]

             contextOfOTStrList <- readStreamByTextTextTextText [] is               -- [(String, String, String, String)]
             let contextOfOTList = map (\x -> (readPhraSynListFromStr (fst4 x), readPhraSynFromStr (snd4 x), readPhraSynFromStr (thd4 x), readPhraSynListFromStr (fth4 x))) contextOfOTStrList
             putStrLn $ "countInStruGene: Num. of ContextOfOT values = " ++ (show . length) contextOfOTList

             phraSynPair2SimMap <- getPhraSynPair2SimFromCOT phra_gram_dist_algo contextOfOTList     -- Map (PhraSyn, PhraSyn) SimDeg
             let phraSynPair2SimList = Map.toList phraSynPair2SimMap            -- [((PhraSyn, PhraSyn), SimDeg)]
             forM_ phraSynPair2SimList $ \((ps1, ps2), simDeg) -> do
               let sqlstat = DS.fromString $ "SELECT id from " ++ phrasyn_sim_tbl ++ " where phrasyn1 = ? and phrasyn2 = ?"
               stmt <- prepareStmt conn sqlstat
               (_, is) <- queryStmt conn stmt [(toMySQLText . phraSynToString) ps1, (toMySQLText . phraSynToString) ps2]
               rows <- S.toList is
               closeStmt conn stmt
               if rows == []
                 then do
                   let sqlstat = DS.fromString $ "INSERT INTO " ++ phrasyn_sim_tbl ++ " SET phrasyn1 = ?, phrasyn2 = ?, sim = ?"
                   stmt <- prepareStmt conn sqlstat
                   ok <- executeStmt conn stmt [(toMySQLText . phraSynToString) ps1, (toMySQLText . phraSynToString) ps2, toMySQLDouble simDeg]
                   putStrLn $ "countInStruGene: Insert record whose id is " ++ (show . getOkLastInsertID) ok
                   closeStmt conn stmt
                 else if length rows == 1
                        then do
                          let sqlstat = DS.fromString $ "UPDATE " ++ phrasyn_sim_tbl ++ " SET sim = ? where id = ?"
                          stmt <- prepareStmt conn sqlstat
                          ok <- executeStmt conn stmt [toMySQLDouble simDeg, ((rows!!0)!!0)]
                          putStrLn $ "countInStruGene: Update record whose id is " ++ (show . fromMySQLInt32U) ((rows!!0)!!0)
                          closeStmt conn stmt
                        else error "countInStruGene: More than one time of hitting on (phrasyn1, phrasyn2)."

           x | elem x ["stru_gene3_202508"] -> do
             let sqlstat = DS.fromString $ "select leftExtend, leftOverTree, rightOverTree, rightExtend from " ++ syntax_ambig_resol_model ++ " where id >= ? and id <= ?"
             stmt <- prepareStmt conn sqlstat
             (defs, is) <- queryStmt conn stmt [toMySQLInt32U startIdx, toMySQLInt32U endIdx]

             contextOfOT3aStrList <- readStreamByTextTextTextText [] is               -- [(String, String, String, String)]
             let contextOfOT3aList = map (\x -> (readPhraSynListFromStr (fst4 x), readBiTreePhraSynFromStr (snd4 x), readBiTreePhraSynFromStr (thd4 x), readPhraSynListFromStr (fth4 x))) contextOfOT3aStrList
             putStrLn $ "countInStruGene: Num. of ContextOfOT3a values = " ++ (show . length) contextOfOT3aList

             phraSynPair2SimMap <- getPhraSynPair2SimFromCOT3a phra_gram_dist_algo contextOfOT3aList     -- Map (PhraSyn, PhraSyn) SimDeg
             let phraSynPair2SimList = Map.toList phraSynPair2SimMap            -- [((PhraSyn, PhraSyn), SimDeg)]
             forM_ phraSynPair2SimList $ \((ps1, ps2), simDeg) -> do
               let sqlstat = DS.fromString $ "SELECT id from " ++ phrasyn_sim_tbl ++ " where phrasyn1 = ? and phrasyn2 = ?"
               stmt <- prepareStmt conn sqlstat
               (_, is) <- queryStmt conn stmt [(toMySQLText . phraSynToString) ps1, (toMySQLText . phraSynToString) ps2]
               rows <- S.toList is
               closeStmt conn stmt
               if rows == []
                 then do
                   let sqlstat = DS.fromString $ "INSERT INTO " ++ phrasyn_sim_tbl ++ " SET phrasyn1 = ?, phrasyn2 = ?, sim = ?"
                   stmt <- prepareStmt conn sqlstat
                   ok <- executeStmt conn stmt [(toMySQLText . phraSynToString) ps1, (toMySQLText . phraSynToString) ps2, toMySQLDouble simDeg]
                   putStrLn $ "countInStruGene: Insert record whose id is " ++ (show . getOkLastInsertID) ok
                   closeStmt conn stmt
                 else if length rows == 1
                        then do
                          let sqlstat = DS.fromString $ "UPDATE " ++ phrasyn_sim_tbl ++ " SET sim = ? where id = ?"
                          stmt <- prepareStmt conn sqlstat
                          ok <- executeStmt conn stmt [toMySQLDouble simDeg, ((rows!!0)!!0)]
                          putStrLn $ "countInStruGene: Update record whose id is " ++ (show . fromMySQLInt32U) ((rows!!0)!!0)
                          closeStmt conn stmt
                        else error "countInStruGene: More than one time of hitting on (phrasyn1, phrasyn2)."

           x | elem x ["stru_gene3a_phrasyn0_202509"] -> do
             let sqlstat = DS.fromString $ "select leftOverTree, rightOverTree from " ++ syntax_ambig_resol_model ++ " where id >= ? and id <= ?"
             stmt <- prepareStmt conn sqlstat           -- In database table, field name is 'leftOverTree' for LeftOverTree and LeftOverTree0.
             (defs, is) <- queryStmt conn stmt [toMySQLInt32U startIdx, toMySQLInt32U endIdx]

             contextOfSG3a0StrList <- readStreamByTextText [] is                -- [(String, String)]
             let contextOfSG3a0List = map (\x -> (readBiTreePhraSyn0FromStr (fst x), readBiTreePhraSyn0FromStr (snd x))) contextOfSG3a0StrList
             putStrLn $ "countInStruGene: Num. of ContextOfSG3a0 values = " ++ (show . length) contextOfSG3a0List

             phraSyn0Pair2SimMap <- getPhraSyn0Pair2SimFromCSG3a phra_gram_dist_algo contextOfSG3a0List     -- Map (PhraSyn0, PhraSyn0) SimDeg
             let phraSyn0Pair2SimList = Map.toList phraSyn0Pair2SimMap          -- [((PhraSyn0, PhraSyn0), SimDeg)]
             putStrLn $ "countInStruGene: Similarities of PhraSyn0 pairs have been obtained."

             forM_ phraSyn0Pair2SimList $ \((ps1, ps2), simDeg) -> do
               let sqlstat = DS.fromString $ "SELECT id from " ++ phrasyn_sim_tbl ++ " where phrasyn1 = ? and phrasyn2 = ?"
               stmt <- prepareStmt conn sqlstat
               (_, is) <- queryStmt conn stmt [(toMySQLText . phraSyn0ToString) ps1, (toMySQLText . phraSyn0ToString) ps2]
               rows <- S.toList is
               closeStmt conn stmt
               if rows == []
                 then do
                   let sqlstat = DS.fromString $ "INSERT INTO " ++ phrasyn_sim_tbl ++ " SET phrasyn1 = ?, phrasyn2 = ?, sim = ?"
                   stmt <- prepareStmt conn sqlstat
                   ok <- executeStmt conn stmt [(toMySQLText . phraSyn0ToString) ps1, (toMySQLText . phraSyn0ToString) ps2, toMySQLDouble simDeg]
                   putStrLn $ "countInStruGene: Insert record whose id is " ++ (show . getOkLastInsertID) ok
                   closeStmt conn stmt
                 else if length rows == 1
                        then do
                          let sqlstat = DS.fromString $ "UPDATE " ++ phrasyn_sim_tbl ++ " SET sim = ? where id = ?"
                          stmt <- prepareStmt conn sqlstat
                          ok <- executeStmt conn stmt [toMySQLDouble simDeg, ((rows!!0)!!0)]
                          putStrLn $ "countInStruGene: Update record whose id is " ++ (show . fromMySQLInt32U) ((rows!!0)!!0)
                          closeStmt conn stmt
                        else error "countInStruGene: More than one time of hitting on (phrasyn1, phrasyn2)."

           _ -> error $ "countInStruGene: " ++ syntax_ambig_resol_model ++ " is NOT recognized."

         close conn
       else putStr ""

    -- 11. Get the mean purity of sample Prior values.
    if funcIndex == 11
       then
         case syntax_ambig_resol_model of
           x | elem x ["stru_gene3a_phrasyn0_202509","stru_gene3a_202508", "stru_gene3_202508", "stru_gene_202501"] -> do                 -- Multimodel
             conn <- getConn
             let sqlstat = DS.fromString $ "select clauTagPrior from " ++ syntax_ambig_resol_model ++ " where id >= ? and id <= ?"
             stmt <- prepareStmt conn sqlstat
             (defs, is) <- queryStmt conn stmt [toMySQLInt32U startIdx, toMySQLInt32U endIdx]

             rows <- S.toList is             -- [[MySqlText]]
             if rows == []
                then putStrLn "countInStruGene: Func. 11: ClauTagPrior list is empty."
                else do
                  let clauTagPriorList = map (\row -> (stringToCTPList . fromMySQLText) (row!!0)) rows     -- [[ClauTagPrior]]
                  let priorsList = map (\ctpList -> map snd ctpList) clauTagPriorList                      -- [[Prior]]
                  let purityList = map (snd . purityOfMajorPrior) priorsList                               -- [Purity]
                  let meanPurity = foldl (+) 0.0 purityList / fromIntegral (length purityList)             -- Float
                  putStrLn $ "countInStruGene: Func 11: Mean purity of sample Prior values = " ++ show meanPurity
           _ -> putStrLn "countInStruGene: Func. 11: syntax_ambig_resol_model is not recognized."

       else putStr ""

{- Get search result in field 'tree' in Table <tree_source> whose serial numbers are less than 'topSn' and
 - bigger than or equal to 'bottomSn', here 'funcIndex' indicates which statistics will be done.
 - <tree_source> is the value of attribue 'tree_source' in file Configuration.
 -}
searchInTree :: Int -> Int -> Int -> IO ()
searchInTree bottomSn topSn funcIndex = do
    conn <- getConn
    confInfo <- readFile "Configuration"                                        -- Read the local configuration file
    let tree_source = getConfProperty "tree_source" confInfo
    putStrLn $ "The source of parsing trees is set as: " ++ tree_source           -- Display the source of parsing trees
    let sqlstat = DS.fromString $ "select serial_num, tree from " ++ tree_source ++ " where serial_num >= ? and serial_num < ?"
    stmt <- prepareStmt conn sqlstat
    (defs, is) <- queryStmt conn stmt [toMySQLInt32 bottomSn, toMySQLInt32 topSn]
    snTreesStrList <- readStreamByInt32Text [] is                           -- [(SentIdx, TreesStr)], storing serial_num and parsing tree of every sentence.
    let snTreesList = map (\row -> (fst row, readTrees (snd row))) snTreesStrList             -- [(SendIdx, [Tree]], namely [(SendIdx, [(ClauIdx, [PhraCate])])]
    if funcIndex == 1                                                   -- To get serial_num list indicating those parsing trees which include given CCG tags.
       then do
         putStr "Please input the C2CCG tag used in parsing sentence: "
         tag <- getLine
         let snListMatchTag = filterByString tag snTreesStrList              -- [(Int, String)], matching given CCG tag.
         putStrLn $ "searchInTree: The serial_num list of trees which use tag \"" ++ tag ++ "\": " ++ show (map fst snListMatchTag)
       else putStr ""

    if funcIndex == 2                                                   -- To get serial_num list indicating those parsing trees which include given phrasal structure.
       then do
         putStr "Please input the phrasal structure used in parsing sentence: "
         ps <- getLine
         let snListMatchTag = filterByString ps snTreesStrList              -- [(Int, String)], matching given phrasal structure.
         putStrLn $ "searchInTree: The serial_num list of trees in which there are phrasal structure \"" ++ ps ++ "\": " ++ show (map fst snListMatchTag)
       else putStr ""

    if funcIndex == 3                                                   -- To get serial_num list indicating those parsing trees which include given syntactic category.
       then do
         putStr "Please input a syntactic category used in parsing sentence: "
         cateStr <- getLine
         let snListMatchTag = filterByString cateStr snTreesStrList              -- [(Int, String)], matching given phrasal structure.
         putStrLn $ "searchInTree: The serial_num list of trees in which there are syntactic category \"" ++ cateStr ++ "\": " ++ show (map fst snListMatchTag)
       else putStr ""

    if funcIndex == 4                                                   -- Display parsing trees of all clauses of all sentences.
      then do
        dispTreeListOnStdout snTreesStrList
      else putStr ""

    if funcIndex == 5                                                   -- Display parsing trees in which include given grammatic rule.
      then do
         putStr "Please input the C2CCG tag used in parsing sentence: "
         tag <- getLine
         let snTreesList' = filterTagInSentTrees tag snTreesList        -- [(SentIdx, [Tree]], matching given C2CCG tag.
         let snTreesStrList' = map (\row -> (fst row, nTreeToString (snd row))) snTreesList'
         dispTreeListOnStdout snTreesStrList'
      else putStr ""

    if funcIndex == 6                                                   -- Display parsing trees in which include given phrasal structure.
      then do
         putStr "Please input the phrasal structure used in parsing sentence: "
         ps <- getLine
         let snTreesList' = filterPhraStruInSentTrees ps snTreesList        -- [(SentIdx, [Tree]], matching given phrasal structure.
         let snTreesStrList' = map (\row -> (fst row, nTreeToString (snd row))) snTreesList'
         dispTreeListOnStdout snTreesStrList'
      else putStr ""

    if funcIndex == 7                                                   -- Display parsing trees in which include given syntactic category.
      then do
         putStr "Please input a syntactic category used in parsing sentence: "
         cateStr <- getLine
         let cate = getCateFromString cateStr
         let snTreesList' = filterCateInSentTrees cate snTreesList      -- [(SentIdx, [Tree]], matching given syntactic category.
         let snTreesStrList' = map (\row -> (fst row, nTreeToString (snd row))) snTreesList'
         dispTreeListOnStdout snTreesStrList'
      else putStr ""

    if funcIndex == 8                                                   -- To do.
      then do
        putStr "To do."
      else putStr ""

{- Get search result in field 'script' in Table <script_source> whose serial numbers are less than 'topSn' and
 - bigger than or equal to 'bottomSn', here 'funcIndex' indicates which statistics will be done.
 - <script_source> is the value of attribue "script_source" in file Configuration.
 -}
searchInScript :: Int -> Int -> Int -> IO ()
searchInScript bottomSn topSn funcIndex = putStrLn "searchInScript: to do."

{- Get representations in memory of parsing results of sentences.
 - <sentsResult> ::= [<sentResult>], a <sentResult> is the parsing result for a sentence.
 - <sentResult> ::= [<clauResult>], a <clauResult> is the parsing result for a clause, where a sentence includes many clauses.
 - <clauResult> ::= Tree, where Tree ::= (ClauIdx, [PhraCate]), PhraCate is the representation in memory for a phrase.
 - So, <sentsResult> ::= [[Tree]]
 - From the perspective of parsing trees, the results of a clause, a sentence, and a set of sentences are a tree, a forest, and a forest of forests.
 -}

{- Get the length of every clause for a set of parsing trees.
 -}
toSentClauLengthList :: [[Tree]] -> [[Int]]
toSentClauLengthList [] = []
toSentClauLengthList (sent:sents) = (map (length . (getPhraBySpan 0) . snd) sent) : toSentClauLengthList sents

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
toSentClauDepthList :: [[Tree]] -> [[Int]]
toSentClauDepthList [] = []
toSentClauDepthList (sent:sents) = (map (getTreeDepth . snd) sent) : toSentClauDepthList sents

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

{- Get the format print of a Map list with given decimal places to represent Double values.
 -}
formatMapListWithDoubleValue :: Show a => [(a, Double)] -> Int -> [(a, String)]       -- 'a' may be any showable type, Polymorphism!
formatMapListWithDoubleValue mapList n = map (\x -> (fst x, printf ("%.0" ++ show n ++"f") (snd x))) mapList

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

{- Get the frequencies of various triple PhraSyn (Category, Tag, PhraStru).
 - The various triples and their frequencies are stored in Map PhraSyn Int.
 - The input is the list of sentential categories.
 -}
toTypeTagStru2FreqMap' :: Sents -> Map PhraSyn0 Int -> Map PhraSyn0 Int
toTypeTagStru2FreqMap' sents ctp2FreqMap
   | sents == []  = ctp2FreqMap                                              -- No sentence
   | length sents == 1 && fstSent == [] = ctp2FreqMap                        -- One sentence without clause to deal with.
   | length sents == 1 && fstSent /= [] = toTypeTagStru2FreqMap' [tail fstSent] (insertPhraList2CtpFreqMap' (head fstSent) ctp2FreqMap)
   | otherwise = toTypeTagStru2FreqMap' (tail sents) (toTypeTagStru2FreqMap' [fstSent] ctp2FreqMap)
   where
     fstSent = head sents

{- Get the frequencies of various triple (syntactic type, CCG rule tag, Phrasal structure), actually the triple is represented by string <type_tag_stru>,
 - because the frequencies of various triples are stored in a Data.Map, where these triples need change as keys, and frequencies are thought as corrresponding values.
 - The input is the list of sentential categories, and the output is Map <type_tag_stru> <ctpNum>.
 -}
toTypeTagStru2FreqMap :: [[[PhraCate]]] -> Map String Int -> Map String Int
toTypeTagStru2FreqMap [] ctp2FreqMap = ctp2FreqMap                              -- No sentence
toTypeTagStru2FreqMap [[]] ctp2FreqMap = ctp2FreqMap                            -- One sentence has no clause to deal with.
toTypeTagStru2FreqMap [(c:cs)] ctp2FreqMap = toTypeTagStru2FreqMap [cs] (insertPhraList2CtpsFreqMap c ctp2FreqMap)
                                                                      -- 'c' is a clause, a list of phrasal categories.
toTypeTagStru2FreqMap (sent:sents) ctp2FreqMap = Map.unionWith (+) mapHead mapTail      -- "sent" means a sentence.
    where
    mapHead = toTypeTagStru2FreqMap [sent] ctp2FreqMap
    mapTail = toTypeTagStru2FreqMap sents ctp2FreqMap

{- Insert a series of 'type_tag_stru' into a Map to count the frequency of every 'type_tag_stru'.
 -}
insertPhraList2CtpsFreqMap :: [PhraCate] -> Map String Int -> Map String Int
insertPhraList2CtpsFreqMap [] ctp2FreqMap = ctp2FreqMap
insertPhraList2CtpsFreqMap [x] ctp2FreqMap
    | ruleTag == "Desig" = ctp2FreqMap
    | otherwise = Map.insert type_tag_stru ctpNum ctp2FreqMap
    where
    syntaxType = show $ ((!!0) . caOfCate) x
                          -- Apply 'caOfCate' to every phrasal category, and take the first element from the above result.
    ruleTag = ((!!0) . taOfCate) x
                          -- Apply 'taOfCate' to every phrasal category, and take the first element from the above result.
    phraStru = ((!!0) . psOfCate) x
                          -- Apply 'psOfCate' to every phrasal category, and take the first element from the above result.
    type_tag_stru = syntaxType ++ "_" ++ ruleTag ++ "_" ++ phraStru
    ctpNum = maybe 1 (1+) (Map.lookup type_tag_stru ctp2FreqMap)
insertPhraList2CtpsFreqMap (x:xs) ctp2FreqMap = insertPhraList2CtpsFreqMap xs (insertPhraList2CtpsFreqMap [x] ctp2FreqMap)

{- Insert a series of '(type, tag, stru)' into a Map to count the frequency of every '(type, tag, stru)'.
 - Map.insert might have a bug when deciding whether there has been a given PhraSyn value (key) in a certain map.
 -}
insertPhraList2CtpFreqMap' :: Clau -> Map PhraSyn0 Int -> Map PhraSyn0 Int
insertPhraList2CtpFreqMap' [] ctp2FreqMap = ctp2FreqMap
insertPhraList2CtpFreqMap' [x] ctp2FreqMap
    | x == nilPhra = ctp2FreqMap
    | ((!!0) . taOfCate) x == "Desig" = ctp2FreqMap                             -- 'tag' is unavailable.
    | otherwise = Map.insert cate_tag_phraStru ctpNum ctp2FreqMap
    where
    cate = ((!!0) . caOfCate) x
    tag = ((!!0) . taOfCate) x
    phraStru = ((!!0) . psOfCate) x
    cate_tag_phraStru = (cate, tag, phraStru)
    ctpNum = maybe 1 (1+) (Map.lookup cate_tag_phraStru ctp2FreqMap)
insertPhraList2CtpFreqMap' (x:xs) ctp2FreqMap = insertPhraList2CtpFreqMap' xs (insertPhraList2CtpFreqMap' [x] ctp2FreqMap)

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
showListOfClauLength2CerParaMinMaxMeanList [(l, (max, min, mean))] = "(" ++ show l ++ ", (" ++ show max ++ ", " ++ show min ++ ", " ++ printf "%.02f" mean ++ "))"
showListOfClauLength2CerParaMinMaxMeanList (l:ls) = showListOfClauLength2CerParaMinMaxMeanList [l] ++ ", " ++ showListOfClauLength2CerParaMinMaxMeanList ls

{- Get the string of list 'ascenListOfClauLength2FreqRate'.
 -}
showAscenListOfClauLength2FreqRate :: [(Int, Float)] -> String
showAscenListOfClauLength2FreqRate [] = ""
showAscenListOfClauLength2FreqRate [(clauLength, freqRate)] = "(" ++ show clauLength ++ ", " ++ printf "%.04f" freqRate ++ ")"
showAscenListOfClauLength2FreqRate (cl:cls) = showAscenListOfClauLength2FreqRate [cl] ++ ", " ++ showAscenListOfClauLength2FreqRate cls

{- Get the number of not-recognizable phrases in parsing every clause in every sentence.
 - But now, the not-recognizable phrases are removed in function 'cateComb', and not stored into database.
 - So the banned phrases in field 'script' of database table 'corpus' do not contain any 'NR' phrase.
 - One sentence might have multiple clauses.
 - Script :: (ClauIdx,[[Rule]],[BanPCs])
 - For one sentence, there is [Script]. For multiple sentences, there is [[Script]].
 -}
toSentClauAbanNRPhraNumList :: [[Script]] -> [[Int]]
toSentClauAbanNRPhraNumList [] = []                                             -- No sentence
toSentClauAbanNRPhraNumList (sent:sents) = (map length sentAbanNRPhra) : toSentClauAbanNRPhraNumList sents
    where
    sentAbanPhra = map thd3 sent                                          -- [[BanPCs]]
    sentAbanPhra' = map concat sentAbanPhra                               -- [BanPCs], one phrasal list per clause
    sentAbanNRPhra = map (\clauAbanPhra -> [x | x <- clauAbanPhra, (psOfCate x)!!0 == "NR"]) sentAbanPhra'   -- Every phrase has only one 'ctspa'.

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
filterByString :: String -> [(SentIdx, String)] -> [(SentIdx, String)]
filterByString _ [] = []
filterByString patt [(i1, s1)]
    | isSubstr patt s1 patt s1 = [(i1, s1)]
    | otherwise = []
filterByString patt (kv:kvs) = (filterByString patt [kv]) ++ (filterByString patt kvs)

{- Filter [(SentIdx, [Tree]], and remain the elements in which Tree inlcudes given C2CCG tag.
 -}
filterTagInSentTrees :: Tag -> [(SentIdx, [Tree])] -> [(SentIdx, [Tree])]
filterTagInSentTrees tag trees = filter (\st -> snd st /= []) $ map (\st -> (fst st, filterTagInTrees tag (snd st))) trees

{- Filter parsing trees with a given C2CCG tag.
 - Tree :: (ClauIdx, [PhraCate])
 -}
filterTagInTrees :: Tag -> [Tree] -> [Tree]
filterTagInTrees _ [] = []
filterTagInTrees tag (t:ts)
    | hasTagInTree tag t = t : filterTagInTrees tag ts
    | otherwise = filterTagInTrees tag ts

{- If phrasal categories among a parsing tree include a given C2CCG tag, return True.
 - Tree :: (ClauIdx, [PhraCate])
 -}
hasTagInTree :: Tag -> Tree -> Bool
hasTagInTree tag (_, []) = False
hasTagInTree tag (clauIdx, (pc:pcs))
    | hasTag = True
    | otherwise = hasTagInTree tag (clauIdx, pcs)
    where
      tags = taOfCate pc
      hasTag = elem tag tags

{- Filter [(SentIdx, [Tree]], and remain the elements in which Tree inlcudes given phrasal structure.
 -}
filterPhraStruInSentTrees :: PhraStru -> [(SentIdx, [Tree])] -> [(SentIdx, [Tree])]
filterPhraStruInSentTrees ps trees = filter (\st -> snd st /= []) $ map (\st -> (fst st, filterPhraStruInTrees ps (snd st))) trees

{- Filter parsing trees with a given phrasal structure.
 - Tree :: (ClauIdx, [PhraCate])
 -}
filterPhraStruInTrees :: PhraStru -> [Tree] -> [Tree]
filterPhraStruInTrees _ [] = []
filterPhraStruInTrees ps (t:ts)
    | hasPhraStruInTree ps t = t : filterPhraStruInTrees ps ts
    | otherwise = filterPhraStruInTrees ps ts

{- If phrasal categories among a parsing tree include a given phrasal structure, return True.
 - Tree :: (ClauIdx, [PhraCate])
 -}
hasPhraStruInTree :: PhraStru -> Tree -> Bool
hasPhraStruInTree ps (_, []) = False
hasPhraStruInTree ps (clauIdx, (pc:pcs))
    | hasPhraStru = True
    | otherwise = hasPhraStruInTree ps (clauIdx, pcs)
    where
      ps' = psOfCate pc
      hasPhraStru = elem ps ps'

{- Filter [(SentIdx, [Tree]], and remain the elements in which Tree inlcudes given syntactic category.
 -}
filterCateInSentTrees :: Category -> [(SentIdx, [Tree])] -> [(SentIdx, [Tree])]
filterCateInSentTrees cate trees = filter (\st -> snd st /= []) $ map (\st -> (fst st, filterCateInTrees cate (snd st))) trees

{- Filter parsing trees with a given syntactic category.
 - Tree :: (ClauIdx, [PhraCate])
 -}
filterCateInTrees :: Category -> [Tree] -> [Tree]
filterCateInTrees _ [] = []
filterCateInTrees cate (t:ts)
    | hasCateInTree cate t = t : filterCateInTrees cate ts
    | otherwise = filterCateInTrees cate ts

{- If phrasal categories among a parsing tree include a given syntactic category, return True.
 - Tree :: (ClauIdx, [PhraCate])
 -}
hasCateInTree :: Category -> Tree -> Bool
hasCateInTree cate (_, []) = False
hasCateInTree cate (clauIdx, (pc:pcs))
    | hasCate = True
    | otherwise = hasCateInTree cate (clauIdx, pcs)
    where
      cas = caOfCate pc
      hasCate = elem cate cas

{- Display parsing trees of every sentence in a set of sentences, usually every sentence includes multiple clauses, and every clause has its parsing tree.
 - The input is [(sn,treeStr)], here 'sn' is serial number of a sentence which has parsing trees represented by string 'treeStr'.
 -}
dispTreeListOnStdout :: [(Int, String)] -> IO ()
dispTreeListOnStdout [] = putStrLn "No parsing tree to display."
dispTreeListOnStdout [(sn, treeStr)] = do
    putStrLn $ "Sentence No.: " ++ show sn
    sentToClauses treeStr >>= dispTreeOfSent
dispTreeListOnStdout (t:ts) = do
    dispTreeListOnStdout [t]
    dispTreeListOnStdout ts

{- Ascendingly sort (Category, Tag, PhraStru).
 - Result is stable, that is, if ctp1 equals with ctp2, then their order is same between before ordering and after ordering.
 -}
quickSort4PhraSyn :: [PhraSyn] -> [PhraSyn]
quickSort4PhraSyn  [] = []
quickSort4PhraSyn  [x] = [x]
quickSort4PhraSyn  (x:xs) = (quickSort4PhraSyn  [y|y<-xs, phraSynLt y x]) ++ [x] ++ (quickSort4PhraSyn  [y|y<-xs, not (phraSynLt y x)])

{- Define relation 'less than' between two values of PhraSyn, so any set of PhraSyns can be linearly ordered.
 -}
phraSynLt :: PhraSyn -> PhraSyn -> Bool
phraSynLt (ca1, ta1, ps1, sp1) (ca2, ta2, ps2, sp2) =
      (ca1 < ca2)
      || (ca1 == ca2) && (ta1 < ta2)
      || (ca1 == ca2) && (ta1 == ta2) && (ps1 < ps2)
      || (ca1 == ca2) && (ta1 == ta2) && (ps1 == ps2) && sp1 < sp2

{- From depth List of clauses of every sentence, get indices of clauses whose depth is -1, namely no root node was found.
 -}
getIdxOfclauWithoutParsingTree :: [[Int]] -> [(SentIdx, [Int])]
getIdxOfclauWithoutParsingTree sentClauDepthList = filter ((elem (-1)) . snd) depthListWithSentIdx
    where
    depthListWithSentIdx = zip [1..] sentClauDepthList                          -- Affix sentence indices
