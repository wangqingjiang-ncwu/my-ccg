-- Copyright (c) 2019-2020 China University of Water Resources and Electric Power,
-- All rights reserved.

module Parse (
    cateComb_CS,       -- OnOff -> PhraCate -> PhraCate -> [PhraCate]
    initPhraCate,      -- [(Category, Seman)] -> [PhraCate]
    createPhraCate,    -- Start -> Span -> Category -> Tag -> Seman -> ComName -> Act -> SecStart -> PhraCate
    createPhraCate2,   -- Start -> Span -> [(Category,Tag,Seman,ComName,Act)] -> SecStart -> PhraCate
    newSpanPCs,        -- OnOff -> [PhraCate] -> [PhraCate]
    parse,             -- OnOff -> [PhraCate] -> [PhraCate]
    priorList,         -- [ComName]
    PriElem,           -- (ComName,ComName,ComName,ComName,Bool)
    priorList2,        -- [PriElem]
    isPrior2,          -- [PhraCate] -> PhraCate -> PhraCate -> Bool
    getCN,             -- Int -> [PhraCate] -> ComName
    match,             -- [PriElem] -> (ComName,ComName,ComName,ComName) -> Int
    prune,             -- OnOff -> [PhraCate] -> [PhraCate]
    findCombWithLowestPrio,    -- [PhraCate] -> [(PhraCate,PhraCate)] -> PhraCate
    getOverlap,        -- [PhraCate] -> [(PhraCate, PhraCate)]
    removeTuple,       -- [(a,a)] -> [(a,a)]
    isOverlap,         -- PhraCate -> PhraCate -> Bool
    isPrior,           -- PhraCate -> PhraCate -> Bool
    removeOnePC,       -- OnOff -> PhraCate -> [PhraCate] -> [PhraCate]
    removeOnePC2,      -- OnOff -> PhraCate -> [PhraCate] -> [PhraCate]
    changeAct,         -- OnOff -> [PhraCate] -> [PhraCate]
    deactOnePC,        -- PhraCate -> PhraCate
    actOnePC,          -- PhraCate -> PhraCate
    atomizePhraCate,         -- [PhraCate] -> [PhraCate]
    getNuOfInputCates,       -- [PhraCate] -> Int 
    growForest,              -- String -> [[PhraCate]] -> [PhraCate] -> [[PhraCate]]
    growTree,          -- OnOff -> [PhraCate] -> [PhraCate] -> [[PhraCate]]
    findCate,                -- (Int, Int) -> [PhraCate] -> [PhraCate]
    findActCateByStart,      -- Start -> [PhraCate] -> PhraCate
    findSplitCate,           -- OnOff -> PhraCate -> [PhraCate] -> [(PhraCate,PhraCate)]
    findDescen,              -- OnOff -> PhraCate -> [PhraCate] -> [PhraCate]
    findTipsOfTree,          -- OnOff -> [PhraCate] -> [PhraCate] -> [PhraCate]
    findCateBySpan,          -- Int -> [PhraCate] -> [PhraCate]
    divPhraCateBySpan,       -- [PhraCate] -> [[PhraCate]]
    pclt,                    -- PhraCate -> PhraCate -> Bool
    quickSort,               -- [PhraCate] -> [PhraCate]
    sortBySpan,              -- [PhraCate] -> [PhraCate]
    throwBrac,               -- String -> String
    splitAtDeli,             -- String -> Char -> [String]
    getPhraCateFromString    -- OnOff -> PhraCate
    ) where

import Data.Tuple
import Data.Tuple.Utils
import Data.List
import Category
import Rule
import Utils

-- Function cateComb_CS combines two input (phrasal) categories into one.
-- The two input categories satisfies concatenative requirements, and may have multiple resultant categories when multiple rules are available.
-- After introduing categorial conversion for Chinese structure overlapping, there might be even more categories.
-- Results ((-1,-1),[],-1) and ((x,y),[],z) respectively denote concatenative failure and no rule available.
-- Here includes a set of Context-Sensitive category conversion rules.
-- The first input parameter is On/Off string, for turning on/off Np/s-, Np/v-, Np/a-, P/a-, C/a-and A/n- rules.
-- For examples, "+-++--" means using Np/s-, Np/a-, and P/a- rules, but denying Np/v- and A/n- rules.

cateComb_CS :: OnOff -> PhraCate -> PhraCate -> PhraCate
cateComb_CS onOff pc1 pc2
    | st1 + sp1 + 1 /= st2 = ((-1,-1),[],-1)
    | otherwise = ((st1, sp1 + sp2 + 1), rcs, st2)
    where
    st1 = stOfCate pc1   -- Start position of pc1
    sp1 = spOfCate pc1   -- Span of pc1
    st2 = stOfCate pc2   -- Start position of pc2
    sp2 = spOfCate pc2   -- Span of pc2
    csc1 = cscOfCate pc1   -- [(Category, Seman, ComName)], categorial activity is not checked.  
    csc2 = cscOfCate pc2   -- [(Category, Seman, ComName)]
    
    -- Categories getten by CCG standard rules.
    catesBasic = [rule cate1 cate2 | rule <- rules, cate1 <- csc1, cate2 <- csc2]

    -- Categories getten by firstly converting sentence into nominal phrase, then using standard CCG rules. For each result (<category>, <tag>, <seman>, <cn>, <act>), the <tag> is changed as "Np/s-"++<tag> to remember the category conversion s->Np which happens before using the standard rule <tag>. Actually, category "s" appears amid a sentence, that means it relates to a clause (Not a Chinese clause). The rule Np/s is always used together with a certain standard rule to implement two-category combination.
    cateS1 = removeDup [(npCate, snd3 csc, thd3 csc) | csc <- csc1, fst3 csc == sCate]     -- [(npCate, Seman, ComName)]
    cateS2 = removeDup [(npCate, snd3 csc, thd3 csc) | csc <- csc2, fst3 csc == sCate]     -- [(npCate, Seman, ComName)]
    ctscaBysToNp = [rule cate1 cate2 | rule <- rules, cate1 <- cateS1, cate2 <- csc2, onOff!!0 == '+'] ++ [rule cate1 cate2 | rule <- rules, cate1 <- csc1, cate2 <- cateS2, onOff!!0 == '+']
    catesBysToNp = [(fst5 cate, "Np/s-" ++ snd5 cate, thd5 cate, fth5 cate, fif5 cate) | cate <- ctscaBysToNp]

    -- According to Jia-xuan Shen's theory, successive inclusions from noun to verb, and to adjective, the conversion from s\.np, (s\.np)/.np, ((s\.np)/.np)/.np, or np/.np to np is allowed, also is from np/.np to s\.np, noted as Np/v, Np/a and P/a respectively. Besides, the conversion from np to np/.np is always allowed, noted as A/n. When used with some standard rules, two-category combination is labelled as "Np/v"++<tag>, "Np/a"++<tag>, "P/a"++<tag>, or "A/n"++<tag>. But now, category conversions only happen in particular contexts.

    -- The conversion from s\.np, (s\.np)/.np, or ((s\.np)/.np)/.np to np is noted as Np/v.
    cateV1 = removeDup [(npCate, snd3 csc, thd3 csc) | csc <- csc1, elem True (map (\x-> cateEqual x (fst3 csc)) cas)]  -- [(npCate, Seman, ComName)]
        where
        cas = map getCateFromString ["s\\.np","(s\\.np)/.np","((s\\.np)/.np)/.np"]
    -- cateV1 happens verbal phrases act as subject constituent.
    cateV2 = removeDup [(npCate, snd3 csc, thd3 csc) | csc <- csc2, elem True (map (\x-> cateEqual x (fst3 csc)) cas)]  -- [(npCate, Seman, ComName)]
        where
        cas = map getCateFromString ["s\\.np","(s\\.np)/.np","((s\\.np)/.np)/.np"]
    -- cateV2 happens verbal phrases act as object constituent.
    ctscaByvToNp = [rule cate1 cate2 | rule <- [appB,raiFh], cate1 <- cateV1, cate2 <- csc_2, onOff!!1 == '+'] ++ [rule cate1 cate2 | rule <- [appF], cate1 <- csc_1, cate2 <- cateV2, onOff!!1 == '+']
        where
        cas1 = map getCateFromString ["s\\.np","(s\\.np)/.np"]      -- No object extraction for double-objects verbs. 
        csc_2 = removeDup [x| x <- csc2, elem True (map (\y-> cateEqual y (fst3 x)) cas1)]
        cas2 = map getCateFromString ["(s\\.np)/.np","((s\\.np)/.np)/.np"]
        csc_1 = removeDup [x| x <- csc1, elem True (map (\y-> cateEqual y (fst3 x)) cas2)]
    catesByvToNp = [(fst5 cate, "Np/v-" ++ snd5 cate, thd5 cate, fth5 cate, fif5 cate) | cate <- ctscaByvToNp]
   
    -- The conversion from np/.np to np is noted as Np/a.
    cateA1 = removeDup [(npCate, snd3 csc, thd3 csc) | csc <- csc1, cateEqual (fst3 csc) cateAdj]  -- [(npCate, Seman, ComName)]
        where
        cateAdj = getCateFromString "np/.np"
    -- cateA1 happens adjective phrases act as subject constituent.
    cateA2 = removeDup [(npCate, snd3 csc, thd3 csc) | csc <- csc2, cateEqual (fst3 csc) cateAdj]  -- [(npCate, Seman, ComName)]
        where
        cateAdj = getCateFromString "np/.np"
    -- cateA2 happens adjective phrases act as object constituent.
    ctscaByaToNp = [rule cate1 cate2 | rule <- [appB, raiFh], cate1 <- cateA1, cate2 <- csc_2, onOff!!2 == '+'] ++ [rule cate1 cate2 | rule <- [appF], cate1 <- csc_1, cate2 <- cateA2, onOff!!2 == '+']
        where
        cas1 = map getCateFromString ["s\\.np","(s\\.np)/.np"]     -- No object extraction for double-objects verbs.
        csc_2 = removeDup [x| x <- csc2, elem True (map (\y-> cateEqual y (fst3 x)) cas1)]
        cas2 = map getCateFromString ["(s\\.np)/.np","((s\\.np)/.np)/.np"]
        csc_1 = removeDup [x| x <- csc1, elem True (map (\y-> cateEqual y (fst3 x)) cas2)]
    catesByaToNp = [(fst5 cate, "Np/a-" ++ snd5 cate, thd5 cate, fth5 cate, fif5 cate) | cate <- ctscaByaToNp]
 
    -- The conversion from np/.np to s\.np is noted as P/a.
    cateA3 = removeDup [(catePred, snd3 csc, thd3 csc) | csc <- csc2, cateEqual (fst3 csc) cateAdj]  -- [(s\.np, Seman, ComName)]
        where
        cateAdj = getCateFromString "np/.np"
        catePred = getCateFromString "s\\.np"
    -- cateA3 happens adjective phrases act as predicate constituent, namely cate1 is npCate.
    ctscaByaToP = [rule cate1 cate2 | rule <- [appB], cate1 <- csc_1, cate2 <- cateA3, onOff!!3 == '+']
        where
        csc_1 = removeDup [x| x<- csc1, fst3 x == npCate]
    catesByaToP = [(fst5 cate, "P/a-" ++ snd5 cate, thd5 cate, fth5 cate, fif5 cate) | cate <- ctscaByaToP]

    -- The conversion from np/.np to np\.np is noted as C/a.
    cateA4 = removeDup [(cateComp, snd3 csc, thd3 csc) | csc <- csc2, cateEqual (fst3 csc) cateAdj]  -- [(np\.np, Seman, ComName)]
        where
        cateAdj = getCateFromString "np/.np"
        cateComp = getCateFromString "np\\.np"
    -- cateA4 happens adjective phrases act as complement of noun constituent, namely cate1 is 'np'.
    ctscaByaToC = [rule cate1 cate2 | rule <- [appB], cate1 <- csc_1, cate2 <- cateA4, onOff!!4 == '+']
        where
        csc_1 = removeDup [x| x<- csc1, fst3 x == npCate]
    catesByaToC = [(fst5 cate, "C/a-" ++ snd5 cate, thd5 cate, fth5 cate, fif5 cate) | cate <- ctscaByaToC]
    
    -- The conversion from np to np/.np is ONLY allowed when nouns act as attribute, and noted as A/n.
    cateN1 = removeDup [(cateAdj, snd3 csc, thd3 csc) | csc <- csc1, (fst3 csc) == npCate]  -- [(np/.np, Seman, ComName)]
        where
        cateAdj = getCateFromString "np/.np"
    ctscaBynToA = [rule cate1 cate2 | rule <- [appF], cate1 <- cateN1, cate2 <- csc_2, onOff!!5 == '+']
        where
        csc_2 = removeDup [x| x<- csc2, fst3 x == npCate]
    catesBynToA = [(fst5 cate, "A/n-" ++ snd5 cate, thd5 cate, fth5 cate, fif5 cate) | cate <- ctscaBynToA]

    -- The categories getten by all rules.
    cates = catesBasic ++ catesBysToNp ++ catesByvToNp ++ catesByaToNp ++ catesByaToP ++ catesByaToC ++ catesBynToA

    -- Remove Nil's resultant cateories and duplicate ones.
    rcs = removeDup [rc | rc <- cates, (fst5 rc) /= nilCate]

-- Context-based category conversion might be human brain's mechanism for syntax parsing, similiar to Chinese phrase-centric syntactic view. In the past, a phrase usually has a sequence of categories with the first as its classical category followed by non-classical ones.
-- To suitable for two kinds of category views, Phrase category is defined as a triple, including start position, span, and a categorial list. There might be more than one category in the list under category conversion.
-- To remember the parent categories, the start position of the second category is also recorded. For initial word categories, their parents don't exist.

-- Words are considered as minimal phrases, thus an universal phrase category models both words and phrases.
-- Initialize the values of PhraCate for each word of a given sentence.
-- Initial categories are designated manually, so their tags are "Desig".
-- Initial categories are all active, and have same name "DE".
initPhraCate :: [(Category, Seman)] -> [PhraCate]
initPhraCate [] = []
initPhraCate [c] = [((0,0),[(fst c, "Desig", snd c, "DE", True)],0)]    -- Categories start at index 0
initPhraCate (c:cs) = [((0,0),[(fst c,"Desig",snd c, "DE", True)],0)] ++ [(((stOfCate pc)+1, 0), ctscaOfCate pc, (stOfCate pc)+1) | pc <- (initPhraCate cs)]

-- Create a phrasal category according to its specified contents, here the phrasal category has only one category.
createPhraCate :: Start -> Span -> Category -> Tag -> Seman -> ComName -> Act -> SecStart -> PhraCate
createPhraCate start span c tag seman cn act secStart
    | c == nilCate = ((start,span),[],secStart)
    | otherwise = ((start,span),[(c, tag, seman, cn, act)],secStart)

-- Create a phrasal category that includes more than one category, respectively via different rules and with different semantic components.
createPhraCate2 :: Start -> Span -> [(Category, Tag, Seman, ComName, Act)] -> SecStart -> PhraCate
createPhraCate2 start span ctsca secStart = ((start,span),ctsca,secStart)

-- Find the number of input categories from the closure of phrase categories.

getNuOfInputCates :: [PhraCate] -> Int
getNuOfInputCates phraCateClosure = length [pc | pc <- phraCateClosure, spOfCate pc == 0]

-- New span's phrasal categories generted from One trip of transition.
-- The first input parameter is On/Off string, for turning on/off Np/s-, Np/v-, Np/a-, P/a-, C/a-, and A/n- rules.
-- For examples, "+-++--" means using Np/s-, Np/a-, and P/a- rules, but denying Np/v- and A/n- rules.

newSpanPCs :: OnOff -> [PhraCate] -> [PhraCate]
newSpanPCs onOff trans = trans2
    where
      combs = removeDup $ atomizePhraCate [cateComb_CS onOff pc1 pc2 | pc1 <- trans, pc2 <- trans, stOfCate pc1 + spOfCate pc1 + 1 == stOfCate pc2, (acOfCate pc1)!!0 || (acOfCate pc2)!!0]
      trans2 = prune onOff $ trans ++ [cb | cb <- combs, ctscaOfCate cb /= []]

-- Parsing a sequence of categories is actually to generate the category closure from the initial phrase categories. 
-- From the scratch, phrasal categories have no activity attribute, and during each transition, every category tries to combine with others, which results in explosive increase of new generated categories.
-- Later, phrasal categories have the attribute of activity. Only active categories can take part in combination with other active categories, while inactive categories can't. True means categories are active, while False means categories inactive. Those who have taken part in some combinations are inactive, and can become active when the categories they generated are removed later. 
-- In every transition, all existing categories including initial categories are inputed, from which every pair of categories will be tried to combine. Then, every two active categories will be checked whether they are overlapping. For every pair of overlapping categories, lower-priority category is removed, and its parent categories are recovered active. Recursively calling the above check, until no overlapping active categories exist.
-- When transition creates no new category, the categorial set is closed.
-- The first input parameter is On/Off string, for turning on/off Np/s-, Np/v-, Np/a-, P/a-, C/a-, and A/n- rules.
-- For examples, "+-++--" means using Np/s-, Np/a-, and P/a- rules, but denying Np/v- and A/n- rules.
-- To reduce the size of phrasal closure as much as possible, linguistic knowledge about parsing tree is again introduced. From so-called semantic distance, adverbals close verbs nearer than complements, and objects close verbs nearer than subjects. A priority about categorial combinations is built. If a phrase takes part in two category combinations, usually with left phrase and right phrase respectively, the two combination should compare their priorities, and only higher one is conducted while the lower one is banned, the categories forming the higher combination will be set inactive, namely they are no longer allowed to form new category combiantions. The final closure is comprised of an root category and other inactive phrasal categories.

parse :: OnOff -> [PhraCate] -> [PhraCate]
parse onOff trans
    | combs == [] = trans                  -- No new combination
    | otherwise = parse onOff trans2
        where
        combs = removeDup $ atomizePhraCate [cateComb_CS onOff pc1 pc2 | pc1 <- trans, pc2 <- trans, stOfCate pc1 + spOfCate pc1 + 1 == stOfCate pc2, (acOfCate pc1)!!0 || (acOfCate pc2)!!0]     
            -- Not only try to combine two active categories, but also one active and one inactive.
        trans2 = prune onOff $ trans ++ [cb | cb <- combs, ctscaOfCate cb /= []]

-- Originally, a high-to-low priority list of categorial combinations is used, in which all kinds of categorial combinations are listed. MQ: quantity phrase, XX: conjunction phrase; DHv: adverbial-verb (headword) phrase; HvC: verb (headword)-complement phrase; DHa: adverbial-adjective (headword) phrase; AHn: attribute-noun (headword) phrase; HnC: noun (headword)-complement phrase; VO: verb-object phrase; OE: object extraction phrase; U1P: 1-auxiliary word phrase; U2P: 2-auxiliary word phrase; U3P: 3-auxiliary word phrase; PO: preposition object phrase; SP: subject-predicate phrase; EM: exclamation mood. For uniformity, initial categories are named "DE", meaning designated, with the lowest priority.

priorList :: [ComName]
priorList = ["MQ","XX","DHv","HvC","DHa","HaC","AHn","HnC","VO","OE","U1P","U2P","U3P","PO","SP","EM","CC","DE","NR"]

-- Compare two phrasal categories and point out which one is prior. True for the first, and False for the second.
-- The lower priority combination is baned, for an example, (s\.np)/.np np -> (s\.np)/.np [A/n ->B]

isPrior :: PhraCate -> PhraCate -> Bool
isPrior pc1 pc2
    | not (isOverlap pc1 pc2) = error $ "isPrior2: not overlapping " ++ show pc1 ++ show pc2
--  | n1 == "AHn" && n2 == "SP" = False  
    | i1 < i2 = True
    | otherwise = False
    where
    n1 = (cnOfCate pc1)!!0          -- pc1 and pc2 are both atomic. 
    n2 = (cnOfCate pc2)!!0
    ind1 = elemIndex n1 priorList
    i1 = maybe (-1) (0+) ind1        -- Result -1 for no <ind1>
    ind2 = elemIndex n2 priorList
    i2 = maybe (-1) (0+) ind2

-- Priorities of combinations do not fully follow a linear relation, and to handle overlappings is sometimes involved with their particular contexts, such as left-adjacent active phrase and right-adjacent active phrase.
-- Given triple (<cn1>, <cn2>, <prior>), there exists a pair of overlapping phrases <pc1> and <pc2>, they have <cn1> named combination and <cn2> named combination respectively, while <pc1> is to the left of <pc2> or they are fully overlapping. When <prior> is True, <cn1> named combination is prior to <cn2>'s, otherwise <cn2> named combination is prior to <cn1>'s.
-- Using wildcard "ANY" to represent any combination. 
-- PriElem is 5-tuple (<leftNeigh>, <leftOver>, <rightOver>, <rightNeigh>, <prior>), here <leftNeigh> is the active phrasal category immediately neighbouring with <leftOver>, <rightNeigh> is the active phrasal category immediately neighbouring with <rightOver>, <leftOver> and <rightOver are the left-to-right overlapping phrases, which is prior is indicated by <prior>.

type PriElem = (ComName,ComName,ComName,ComName,Int)
priorList2 :: [PriElem]

-- For easy to begin, let 'priorList' effective. After all, the linear 'priorList' captures common situations.
-- Then, do a mandatory check for the same pair of combinations on non-linear 'priorList2'. If a opposite indication is obtained, it's final.

priorList2 = [
    ("ANY","AHn","AHn","ANY",0),          -- Two overlapping AHn(s), the right AHn is prior.
    ("ANY","HaC","HaC","ANY",1)           -- Two overlapping HnC(s), the left HaC is prior.

    ]

-- Decide which combination is more prior than the other.

isPrior2 :: [PhraCate] -> PhraCate -> PhraCate -> Bool
isPrior2 trans pc1 pc2
    | not (isOverlap pc1 pc2) = error $ "isPrior2: not overlapping " ++ show pc1 ++ show pc2
    | mt == -1 = isPrior pc1 pc2            -- No match in 'priorList2', priority is from 'priorList'. 
    | mt == 1 = True
    | otherwise = False
    where
    st1 = stOfCate pc1
    st2 = stOfCate pc2
    sp2 = spOfCate pc2
    ln = getCN (st1 - 1) trans              -- <leftNeigh>
    lo = (cnOfCate pc1)!!0                  -- <leftOver>
    ro = (cnOfCate pc2)!!0                  -- <rightOver>
    rn = getCN (st2 + sp2 + 1) trans        -- <leftNeigh>
    mt = match priorList2 (ln,lo,ro,rn)

-- Decide combination name, and "NON" for ((-1,-1),[],-1).
getCN :: Int -> [PhraCate] -> ComName
getCN st trans
    | pc == ((-1,-1),[],-1) = "NON"
    | otherwise = (cnOfCate pc)!!0
    where
      pc = findActCateByStart st trans

-- Pattern match on 'priorList2'. "1" means prior, "0" means not prior, and "-1" means failed.
match :: [PriElem] -> (ComName,ComName,ComName,ComName) -> Int
match [] _ = -1
match (x:xs) pe
    | mt = prix
    | otherwise = match xs pe
    where
      lnx = fst5 x
      lox = snd5 x
      rox = thd5 x
      rnx = fth5 x
      prix = fif5 x
      lnp = fst4 pe
      lop = snd4 pe
      rop = thd4 pe
      rnp = fth4 pe
      mt = (lnx == "ANY" || lnx == lnp) && (lox == lop) && (rox == rop) && (rox == "ANY" || rox == rop)

-- Like pruning in game search, any phrasal category not appearing in the final parsing tree is thrown out after just generated, and any phrasal category having taken part in category combination should be set inactive, not allowed to combine with other categories again. When removing a category, its parent categories should be set active.
-- The first parameter is On/Off string, for turning on/off Np/s-, Np/v-, Np/a-, P/a-, C/a-, and A/n- rules.
-- The second parameter is one-way transition result without pruning. Among <trans2>, every two elements overlapping their spans will be compared on their combination priorities, and the lower one will be removed. 
 
prune :: OnOff -> [PhraCate] -> [PhraCate]
prune onOff cbs
    | cb == ((-1,-1),[],-1) = changeAct onOff cbs                    -- No overlapping phrases.
    | otherwise = prune onOff $ removeOnePC onOff cb cbs
    where
      cbps = getOverlap [x| x <- cbs]   
               -- Get overlapping phrase pairs, and at least one phrase is active in each pair.
      cb = findCombWithLowestPrio cbs cbps    -- Get the combination which has lowest priority among all combinations.

-- Find the categorial combination which has the lowest priority among a combination set formed via overlapping.
findCombWithLowestPrio :: [PhraCate] -> [(PhraCate,PhraCate)] -> PhraCate
findCombWithLowestPrio _ [] = ((-1,-1),[],-1)
findCombWithLowestPrio trans (x:xs) 
    | pri && pcps2 /= [] = findCombWithLowestPrio trans pcps2
    | pri && pcps2 == [] = cb2
    | pri == False && pcps1 /= [] = findCombWithLowestPrio trans pcps1
    | otherwise = cb1
    where
      cb1 = fst x
      cb2 = snd x
      pri = isPrior2 trans cb1 cb2
      pcps1 = [y| y <- xs, (fst y == cb1) || (snd y == cb1)]     -- [(PhraCate,PhraCate)] related with cb1
      pcps2 = [y| y <- xs, (fst y == cb2) || (snd y == cb2)]     -- [(PhraCate,PhraCate)] related with cb2

-- Get all pairs of overlapping phrases. In every pair, at least phrase is active.
--  of the first phrase is less than that of the second.
getOverlap :: [PhraCate] -> [(PhraCate, PhraCate)]
getOverlap [] = []
getOverlap pcs = [(x,y)| x<-pcs, y<-pcs, (acOfCate x)!!0 || (acOfCate y)!!0, x/=y, pclt x y, isOverlap x y]

-- Decide whether two phrasal categories are overlapped.
isOverlap :: PhraCate -> PhraCate -> Bool
isOverlap pc1 pc2
    | st1 == st2 && sp1 == sp2 = True       -- Full overlapping. One phrase has only one combination.
    | (st1 < st2 && st2 <= (st1 + sp1) && (st1 + sp1) < (st2 + sp2)) || (st2 < st1 && st1 <= (st2 + sp2) && (st2 + sp2) < (st1 + sp1)) = True
    | otherwise = False
    where
    st1 = stOfCate pc1
    sp1 = spOfCate pc1
    st2 = stOfCate pc2
    sp2 = spOfCate pc2

-- Remove a given phrasal category, and active its parents.
-- If the category to be removed is inactive, it combined with aother category to a bigger-phrased category, and the category should be removed too. Recursively, the new bigger-phrased category is removed. Apparently, the final removed category is active.
-- The first parameter is On/Off string, for turning on/off Np/s-, Np/v-, Np/a-, P/a-, C/a-, and A/n- rules.

removeOnePC :: OnOff -> PhraCate -> [PhraCate] -> [PhraCate]
removeOnePC onOff pc clo = changeAct onOff [x| x <- clo, notElem x (pc:descens)]
    where
      descens = findDescen onOff pc clo                    -- Descendants of 'pc'

-- The following function is only considering the removed category is active.
removeOnePC2 :: OnOff -> PhraCate  -> [PhraCate] -> [PhraCate]
removeOnePC2 _ _ [] = []       -- Nothing to remove.
removeOnePC2 onOff pc clo
    | not ((acOfCate pc)!!0) = error "removeOnePC2: The category to be removed is inactive."
    | otherwise = non_pars ++ actPars                      -- Removing 'pc', and activate its parents.
    where
      pars = tupToList $ findSplitCate onOff pc clo           -- Finding parents (PhraCate,PhraCate).
      actPars = [actOnePC x| x <- pars]                       -- Activating parents.
      non_pars = [x| x <- clo, x /= pc, notElem x pars]       -- Non-parents.

-- Check every category among a transition result, and set its correct activity.
-- For those phrasal categories used for generating some other categories, they are set inactive, and the others are set active. Any two phrasal categories are impossible of overlapping.
-- Here, every phrasal category has ONLY ONE element in its component [(category, tag, seman, act)].
-- The first parameter is On/Off string, for turning on/off Np/s-, Np/v-, Np/a-, P/a-, C/a-, and A/n- rules.
-- For examples, "+-++--" means using Np/s-, Np/a-, and P/a- rules, but denying Np/v- and A/n- rules.

changeAct :: OnOff -> [PhraCate] -> [PhraCate]
changeAct onOff trans = [deactOnePC x | x <- parents] ++ [actOnePC x | x <- not_married]
    where
    parents = removeDup [x| pc <- trans, (taOfCate pc)!!0 /= "Desig", parpair <- findSplitCate onOff pc trans, x <- [fst parpair, snd parpair]]
      -- Owing to overlapping, a parent would be created not only once.
    not_married = [y| y <- trans, notElem y parents]
    
-- Deactivate a phrasal category.
deactOnePC :: PhraCate -> PhraCate
deactOnePC ((st,sp),[],ss) = ((st,sp),[],ss)
deactOnePC ((st,sp),[x],ss) = ((st,sp),[(fst5 x,snd5 x, thd5 x, fth5 x, False)],ss)
deactOnePC ((_,_),(x:xs),_) = error "Failed to deactivate a non-atomized phrasal category." 

-- activate a phrasal category.
actOnePC :: PhraCate -> PhraCate
actOnePC ((st,sp),[],ss) = ((st,sp),[],ss)
actOnePC ((st,sp),[x],ss) = ((st,sp),[(fst5 x,snd5 x, thd5 x, fth5 x, True)],ss)
actOnePC ((_,_),(x:xs),_) = error "Failed to activate a non-atomized phrasal category." 

-- In the result of syntactic parsing, one phrasal category is defined as ((Start,Span),[(Category,Tag,Seman)],SecStart), allowing one phrasal category to have more than one triple (<category>,<tag>,<seman>).
-- The following function atomizePhraCate is used to unpack one phrasal category into multiple phrasal categories, each of which has only one triple (<category>,<tag>,<seman>). 
atomizePhraCate :: [PhraCate] -> [PhraCate]
atomizePhraCate [] = []
atomizePhraCate [pc] = removeDup [((st,sp),[ctsca1],ss) | ctsca1 <- ctsca]
    where
    st = stOfCate pc
    sp = spOfCate pc
    ctsca = ctscaOfCate pc
    ss = ssOfCate pc
atomizePhraCate (x:xs) = removeDup $ (atomizePhraCate [x]) ++ (atomizePhraCate xs)

-- Generate syntactic trees (forest) from the closure of phrase categories.
-- Here is a recursived forest-growing algorithm: For the input forest, 
-- (1) If it is an empty forest without any tree, an empty forest is returned;
-- (2) Otherwise, one forest is created from a tree in input forest, return the union of all created forests.
 
growForest :: OnOff -> [[PhraCate]] -> [PhraCate] -> [[PhraCate]]
growForest _ [] _ = []                        -- Empty forest
growForest onOff (t:ts) phraCateClosure       -- nonempty forest
    = (growTree onOff t phraCateClosure) ++ (growForest onOff ts phraCateClosure) 

-- One tree can grow at all tips to send forth a layer of leaves. Every parsing tree in Combinatory Categorial Grammar is one binary tree, namely from every tip, only two leaces can grow out. It makes growing process complicated that there might be more than one pair of leaves to grow out for every tip, Selecting different pairs would create different trees. When more than one rule can be used to combine two parent categories, multiple resultant categories are created.
-- The forest growing from a tree is done by the following:
-- (1) Find all tips able to send forth leaves (The tips for initial categoies are no longer to grow);
-- (2) For every such tip, find all splits (namely all pairs of leaves), and create a forest, of which every tree include the input tree and a distinc pair of leaves;
-- (3) Based on merging two forests, all forests are merged into one forest. 

-- The first input parameter is On/Off string, for turning on/off Np/s-, Np/v-, Np/a-, P/a-, C/a-, and A/n- rules.
-- For examples, "+-++--" means using Np/s-, Np/a-, and P/a- rules, but denying Np/v- and A/n- rules.

growTree :: OnOff -> [PhraCate] -> [PhraCate] -> [[PhraCate]]
growTree onOff t phraCateClosure
    | findTipsOfTree onOff t phraCateClosure == [] = [t]      -- No tip can grow.
    | [t] == gf = [t]                                         -- Not grow.
    | otherwise = growForest onOff gf phraCateClosure
        where
        splOfAllTips = [findSplitCate onOff tip phraCateClosure | tip <- (findTipsOfTree onOff t phraCateClosure)]   -- [[(PhraCate, PhraCate)]]
        
        -- For every tip, there may be multiple splits， so it may have multiple pairs of parent categories.
        -- For every split, there may be multiple pairs of parent categories owing to multiple rules available.
        -- The function uniForest is used for composing grows at different tips, not for composing two kind of grows at an identical tip.
        -- Before growing a tree or a forest from a root category, the category closure has been atomized.
        -- When considering growing at a certain tip, every pair of parent categories will be selected.
        
        forestByTipGrow = [map (\x -> [fst x | elem (fst x) t == False] ++ [snd x | elem (snd x) t == False] ++ t ) splOfATip | splOfATip <- splOfAllTips]
        gf = uniForest forestByTipGrow

-- By growing at each tip, a tree grows and might become multiple trees because more than one split exists. 
-- Suppose tree t becomes ti = [ti1,ti2,...tin] by growing at No.i tip, and tj = [tj1,tj2,...tjm] by growing at No.j tip. Both the two forests are from the same tree t, and should merge into forest tk, tk = ti X tj. Merging tix and tjy, x<-[1..n], y<-[1..m], is actually to do an union operation on two sets.

uniForest :: [[[PhraCate]]] -> [[PhraCate]]
uniForest [] = []                -- No forest
uniForest [f] = f                -- Just one forest
uniForest (f:fs)                 -- At least two forests
    = foldl uniTwoForest f fs 

-- Merging two forest.
uniTwoForest :: [[PhraCate]] -> [[PhraCate]] -> [[PhraCate]]
uniTwoForest f1 f2 = [uniTwoTree t1 t2 | t1<-f1, t2<-f2]

-- Merging two trees.
uniTwoTree :: [PhraCate] -> [PhraCate] -> [PhraCate]
uniTwoTree t1 t2 = t1 ++ [x | x<-t2, elem x t1 /= True]

-- Find a phrase category by its (Start, Span). If does not, return []. 
findCate :: (Start, Span) -> [PhraCate] -> [PhraCate]
findCate (_, -1) _ = []      -- For a leaf node, its non-existing parents have span -1 and 0.
findCate (st, sp) [] = []
findCate (st, sp) [x]
    | st == stOfCate x && sp == spOfCate x = [x]
    | otherwise = []
findCate (st, sp) (x:xs)
    | st == stOfCate x && sp == spOfCate x = x:(findCate (st, sp) xs)
    | otherwise = findCate (st, sp) xs

-- Find an active phrasal category by its start position. If does not, return []
-- Here, all categories are atomic.
findActCateByStart :: Start -> [PhraCate] -> PhraCate
findActCateByStart st1 [] = ((-1,-1),[],-1)
findActCateByStart st1 (x:xs)
    | ac && (st == st1) = x
    | otherwise = findActCateByStart st1 xs
    where
      st = stOfCate x
      ac = (acOfCate x)!!0

-- Find splited (namely parent) categories for a given phrase category from the closure of phrase categories. 
-- Here, every phrasal category has only one element in its component [(category, tag, seman, comName, act)].
-- The first input parameter is On/Off string, for turning on/off Np/s-, Np/v-, Np/a-, P/a-, C/a-, and A/n- rules.
-- For examples, "+-++--" means using Np/s-, Np/a-, and P/a- rules, but denying Np/v- and A/n- rules.

findSplitCate :: OnOff -> PhraCate -> [PhraCate] -> [(PhraCate, PhraCate)]
findSplitCate onOff pc phraCateClosure
    = [pct | pct <- pcTuples, pcBelong' pc (cateComb_CS onOff (fst pct) (snd pct))]  
                      -- Using pcBelong' not pcBelong is for neglecting the active attribute.
        where
        st1 = stOfCate pc
        st2 = ssOfCate pc
        sp1 = st2 - st1 - 1                -- When pc is a leaf, sp1 is -1.
        sp2 = spOfCate pc - sp1 - 1        -- When pc is a leaf, sp2 is 0, that is, the second parent is pc.
        pcTuples = [(x, y) | x <- (findCate (st1, sp1) phraCateClosure), y <- (findCate (st2, sp2) phraCateClosure)]
                                           -- When pc is a leaf, pcTuples is []. 

-- Find descendants of a given phrasal category from the transition closure of phrasal categories.
-- The first input parameter is On/Off string, for turning on/off Np/s-, Np/v-, Np/a-, P/a-, C/a-, and A/n- rules.
-- For examples, "+-++--" means using Np/s-, Np/a-, and P/a- rules, but denying Np/v- and A/n- rules.

findDescen :: OnOff -> PhraCate -> [PhraCate] -> [PhraCate]
findDescen onOff pc clo
    | children == [] = [] 
    | otherwise = children ++ (foldr (++) [] (map (\x -> findDescen onOff x clo) children))
      where
        children = [x| x <- clo, (taOfCate x)!!0 /= "Desig", y <- findSplitCate onOff x clo, pcBelong' pc (fst y) || pcBelong' pc (snd y)]
         -- There is only one child of 'pc' when lower-priority combinations are removed timely.
 
-- Find all growable tips of a tree from the closure of phrase categories.
-- Those nodes whose parents already exist in the tree can't grow again.
-- Those tips corresponding to initial categories are not growable.

findTipsOfTree :: OnOff -> [PhraCate] -> [PhraCate] -> [PhraCate]
findTipsOfTree _ [] _ = []
findTipsOfTree onOff t phraCateClosure
    | ppcs == [] = findTipsOfTree onOff (tail ot) phraCateClosure      -- Leaves have no parents and can't grow.
    | foldl (||) False (map (\x -> elem (snd x) ot) ppcs) = findTipsOfTree onOff (tail ot) phraCateClosure  
          -- If a node already grew, then its right parent should be in ordered phrasal series ot.
          -- But, if the node grew out multiple pairs of parent nodes, all parent node pairs must be checked.
          -- If there exists a certain right parent in ot, it can be concluded that the node is not a tip.
    | otherwise = (head ot):findTipsOfTree onOff (tail ot) phraCateClosure -- Head node which doesn't yet grow.
       where
       ot = quickSort t    -- Such that there exists left parent << node << right parent for any node. 
       ppcs = findSplitCate onOff (head ot) phraCateClosure    -- Find the parent pairs of node (head ot).
       
-- Find phrase categories with given span.
findCateBySpan :: Span -> [PhraCate] -> [PhraCate]
findCateBySpan sp  pcs = [x|x<-pcs, spOfCate x == sp]

-- Has Not implemented. Find phrase categories with same position and span.
-- findCateBySameStartSpan :: [PhraCate] -> [[PhraCate]]
-- findCateBySameStartSpan pcs = [s|s<-powerSet pcs, x<-s, y<-s, x/=y, stOfCate x == stOfCate y, spOfCate x == spOfCate y]

-- Phrasal categories in a tree are divided into some lists, each of which includes 
-- those categories with same span, and every such list is arranged in ascending 
-- order on values of element Start, while these lists is arranged from 0 to 
-- (getNuOfInputCates - 1).

divPhraCateBySpan :: [PhraCate] -> [[PhraCate]]
divPhraCateBySpan t = map quickSort (map (\sp -> findCateBySpan sp t) [0..(getNuOfInputCates t - 1)])

-- Define relation 'less than' for two phrasal categories.
-- When fully overlapping, the phrase with high combination priority is less.
-- Here, the purpose is giving a definite order.
pclt :: PhraCate -> PhraCate -> Bool
pclt x y = (stx < sty) || ((stx == sty) && (spx < spy)) || ((stx == sty) && (spx == spy)) && isPrior x y
    where
    stx = stOfCate x
    sty = stOfCate y
    spx = spOfCate x
    spy = spOfCate y

-- Quick sort a list of phrasal categoies. This is a stable sort.
-- Allowing full overlapping of categories where pclt is False.
quickSort :: [PhraCate] -> [PhraCate]
quickSort [] = []
quickSort [x] = [x]
quickSort (x:xs) = (quickSort [y|y<-xs, pclt y x]) ++ [x] ++ (quickSort [y|y<-xs, pclt y x == False])

-- Sort phrasal categories according spans.
sortBySpan :: [PhraCate] -> [PhraCate]
sortBySpan pcClo = [pc| sp <- divPhraCateBySpan pcClo, pc <- sp]

-- Throw out brackets.
throwBrac :: String -> String
throwBrac [] = []
throwBrac (c:cs)
    | elem c ['(',')','[',']'] = throwBrac cs
    | otherwise = c:(throwBrac cs)

-- Split a string with designated delimiter.
splitAtDeli :: String -> Char -> [String]
splitAtDeli "" _ = []
splitAtDeli cs c
    | i /= -1 = (take i cs) : splitAtDeli (drop (i+1) cs) c
    | otherwise = [cs]
    where
      ind = elemIndex c cs
      i = maybe (-1) (0+) ind     -- Result -1 for no ';'

-- Get a phraCate from a string.
-- The string has format "((start,span),[(cate,tag,sem,cn,act)],secStart)".
getPhraCateFromString :: String -> PhraCate
getPhraCateFromString str = ((st,sp),[(cate,tag,sem,cn,act)],ss)
    where
    s = splitAtDeli str ',' 
    st = read (throwBrac (s!!0)) :: Int
    sp = read (throwBrac (s!!1)) :: Int
    cate = getCateFromString (drop 2 (s!!2))
    tag = s!!3
    sem = s!!4
    cn = s!!5
    act = read (throwBrac (s!!6)) :: Bool
    ss = read (throwBrac (s!!7)) :: Int



