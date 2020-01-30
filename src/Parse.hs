-- Copyright (c) 2019-2020 China University of Water Resources and Electric Power,
-- All rights reserved.

module Parse (
    Start,         -- Int
    Span,          -- Int
    Act,           -- Bool
    SecStart,      -- Int
    PhraCate,      -- ((Start, Span), [(Category, Tag, Seman, Act)], SecStart)
    pclt,          -- PhraCate -> PhraCate -> Bool
    pcBelong,      -- PhraCate -> PhraCate -> Bool
    stOfCate,      -- PhraCate -> Start
    spOfCate,      -- PhraCate -> Span
    ctsOfCate,     -- PhraCate -> [(Category, Tag, Seman, Act)]
    caOfCate,      -- PhraCate -> [Category]
    caOfActCate,   -- PhraCate -> [Category]
    taOfCate,      -- PhraCate -> [String]
    taOfActCate,   -- PhraCate -> [String]
    seOfCate,      -- PhraCate -> [Seman]
    seOfActCate,   -- PhraCate -> [Seman]
    acOfCate,      -- PhraCate -> [Act]
    acOfActCate,   -- PhraCate -> [Act]
    csOfCate,      -- PhraCate -> [(Category, Seman)]
    csOfActCate,   -- PhraCate -> [(Category, Seman)]
    ssOfCate,      -- PhraCate -> SecStart
    removeDup,     -- Eq a => [a] -> [a]
    cateComb_CS,   -- String -> PhraCate -> PhraCate -> [PhraCate]
    initPhraCate,  -- [(Category, Seman)] -> [PhraCate]
    createPhraCate,-- Start -> Span -> Category -> Tag -> Seman -> SecStart -> Act -> PhraCate
    createPhraCate2, -- Start -> Span -> [(Category,Tag,Seman,Act)] -> SecStart -> PhraCate
    parse,         -- [PhraCate] -> [PhraCate] -> [PhraCate]
    priorList,     -- [String]
    isOverlap,     -- PhraCate -> PhraCate -> Bool
    getOverlap,    -- [PhraCate] -> [(PhraCate, PhraCate)]
    isPrior,       -- PhraCate -> PhraCate -> Bool
    remLowPriCate, -- [PhraCate] -> [PhraCate]
    remOneFromList,          -- Eq a => a -> [a] -> [a]
    getNameOfPhraCate,       -- PhraCate -> String
    deactPhraCate, -- String -> [PhraCate] -> [PhraCate] -> [PhraCate]
    deactOnePC，   -- PhraCate -> PhraCate



    atomizePhraCate,         -- [PhraCate] -> [PhraCate]
    newSpanPCs,    -- [PhraCate] -> [PhraCate] -> [PhraCate]
    getNuOfInputCates,       -- [PhraCate] -> Int 
    growForest,              -- String -> [[PhraCate]] -> [PhraCate] -> [[PhraCate]]
    growTree,      -- String -> [PhraCate] -> [PhraCate] -> [[PhraCate]]
    findCate,                -- (Int, Int) -> [PhraCate] -> [PhraCate]
    findSplitCate,           -- String -> PhraCate -> [PhraCate] -> [PhraCate]
    findTipsOfTree,          -- String -> [PhraCate] -> [PhraCate] -> [PhraCate]
    findCateBySpan,          -- Int -> [PhraCate] -> [PhraCate]
    divPhraCateBySpan,       -- [PhraCate] -> [[PhraCate]]
    quickSort,               -- [PhraCate] -> [PhraCate]
    throwBrac,               -- String -> String
    splitAtDeli,             -- String -> Char -> [String]
    getPhraCateFromString    -- String -> PhraCate
    ) where

import Data.Tuple.Utils
import Data.List
import Category
import Rule

type Start = Int         -- The start position of a phrase (category) in sentences.
type Span = Int          -- The span distance of a phrase (category) in sentences.
type SecStart = Int      -- The position of spliting a phrase (category).

-- When combining two phrase categories, there might be more than one rule available, resulting in multiple categories (Usually the resultant categories are same).

type PhraCate = ((Start, Span), [(Category, Tag, Seman, Act)], SecStart)

-- Define relation 'less than' for two phrasal categories.
pclt :: PhraCate -> PhraCate -> Bool
pclt x y = (stx < sty) || ((stx == sty) && (spx < spy))
    where
    stx = stOfCate x
    sty = stOfCate y
    spx = spOfCate x
    spy = spOfCate y

-- Define relation 'belong to' between two phrasal categories. They have same Start, Span, and SecStart, except that the component [(Category,Tag,Seman)] of first phrasal category is subset of that of the second one. 
pcBelong :: PhraCate -> PhraCate -> Bool
pcBelong x y = (stx == sty) && (spx == spy) && (ssx == ssy) && belong
    where
    stx = stOfCate x
    sty = stOfCate y
    spx = spOfCate x
    spy = spOfCate y
    ssx = ssOfCate x
    ssy = ssOfCate y
    ctsx = ctsOfCate x
    ctsy = ctsOfCate y
    belong = foldr (&&) True (map (\x -> elem x ctsy) ctsx)

-- Four tuple functions.
fst4 :: (Category,String,String,Bool) -> Category
fst4 (a,_,_,_) = a

snd4 :: (Category,String,String,Bool) -> String
snd4 (_,b,_,_) = b

thd4 :: (Category,String,String,Bool) -> String
thd4 (_,_,c,_) = c

fth4 :: (Category,String,String,Bool) -> Bool
fth4 (_,_,_,d) = d

-- The following functions are used to select an element from tuple PhraCate.
stOfCate :: PhraCate -> Start
stOfCate (s, _, _) = fst s

spOfCate :: PhraCate -> Span
spOfCate (s, _, _) = snd s

ctsOfCate :: PhraCate -> [(Category, Tag, Seman, Act)]
ctsOfCate (_, cts, _) = cts

caOfCate :: PhraCate -> [Category]
caOfCate pc = [fst4 c | c <- cts]
    where
    cts = ctsOfCate pc

caOfActCate :: PhraCate -> [Category]
caOfActCate pc = [fst4 c | c <- cts, fth4 c == True]
    where
    cts = ctsOfCate pc

taOfCate :: PhraCate -> [Tag]
taOfCate pc = [snd4 c | c <- cts]
    where
    cts = ctsOfCate pc

taOfActCate :: PhraCate -> [Tag]
taOfActCate pc = [snd4 c | c <- cts, fth4 c == True]
    where
    cts = ctsOfCate pc

seOfCate :: PhraCate -> [Seman]
seOfCate pc = [thd4 c | c <- cts]
    where
    cts = ctsOfCate pc

seOfActCate :: PhraCate -> [Seman]
seOfActCate pc = [thd4 c | c <- cts, fth4 c == True]
    where
    cts = ctsOfCate pc

acOfCate :: PhraCate -> [Act]
acOfCate pc = [fth4 c | c <- cts]
    where
    cts = ctsOfCate pc

acOfActCate :: PhraCate -> [Act]
acOfActCate pc = [fth4 c | c <- cts, fth4 c == True]
    where
    cts = ctsOfCate pc

csOfCate :: PhraCate -> [(Category, Seman)]
csOfCate pc = zip (caOfCate pc) (seOfCate pc)

csOfActCate :: PhraCate -> [(Category, Seman)]
csOfActCate pc = zip (caOfActCate pc) (seOfActCate pc)

ssOfCate :: PhraCate -> SecStart
ssOfCate (_, _, s) = s

-- Rremove duplicate elements in a list.
removeDup :: Eq a => [a] -> [a]
removeDup [] = []
removeDup [x] = [x]
removeDup (x:xs)
    | elem x xs = removeDup xs
    | otherwise = x:(removeDup xs)

-- Function cateComb_CS combines two input (phrasal) categories into one.
-- The two input categories satisfies concatenative requirements, and may have multiple resultant categories when multiple rules are available.
-- After introduing categorial conversion for Chinese structure overlapping, there might be even more categories.
-- Results ((-1,-1),[],-1) and ((x,y),[],z) respectively denote concatenative failure and no rule available.
-- Here includes a set of Context-Sensitive category conversion rules.
-- The first input parameter is On/Off string, for turning on/off Np/s-, Np/v-, Np/a-, P/a-, and A/n- rules.
-- For examples, "+-++-" means using Np/s-, Np/a-, and P/a- rules, but denying Np/v- and A/n- rules.

cateComb_CS :: String -> PhraCate -> PhraCate -> PhraCate
cateComb_CS onOff pc1 pc2
    | st1 + sp1 + 1 /= st2 = ((-1,-1),[],-1)
    | otherwise = ((st1, sp1 + sp2 + 1), rcs, st2)
    where
    st1 = stOfCate pc1   -- Start position of pc1
    sp1 = spOfCate pc1   -- Span of pc1
    st2 = stOfCate pc2   -- Start position of pc2
    sp2 = spOfCate pc2   -- Span of pc2
    cs1 = csOfActCate pc1   -- [(Category, Seman)], only allow active categories to join combination.  
    cs2 = csOfActCate pc2   -- [(Category, Seman)], only allow active categories to join combination.
    
    -- Categories getten by CCG standard rules.
    catesBasic = [rule cate1 cate2 | rule <- rules, cate1 <- cs1, cate2 <- cs2]

    -- Categories getten by firstly converting sentence into nominal phrase, then using standard CCG rules. For each result (<category>, <tag>, <seman>, <act>), the <tag> is changed as "Np/s-"++<tag> to remember the category conversion s->Np which happens before using the standard rule <tag>. Actually, category "s" appears amid a sentence, that means it relates to a clause (Not a Chinese clause). The rule Np/s is always used together with a certain standard rule to implement two-category combination.
    cateS1 = [(npCate, snd cs) | cs <- cs1, fst cs == sCate]     -- [(npCate, Seman)]
    cateS2 = [(npCate, snd cs) | cs <- cs2, fst cs == sCate]     -- [(npCate, Seman)]
    ctsBysToNp = [rule cate1 cate2 | rule <- rules, cate1 <- cateS1, cate2 <- cs2, onOff!!0 == '+'] ++ [rule cate1 cate2 | rule <- rules, cate1 <- cs1, cate2 <- cateS2, onOff!!0 == '+']
    catesBysToNp = [(fst4 cate, "Np/s-" ++ snd4 cate, thd4 cate, fth4 cate) | cate <- ctsBysToNp]

    -- According to Jia-xuan Shen's theory, successive inclusions from noun to verb, and to adjective, the conversion from s\.np, (s\.np)/.np, ((s\.np)/.np)/.np, or np/.np to np is allowed, also is from np/.np to s\.np, noted as Np/v, Np/a and P/a respectively. Besides, the conversion from np to np/.np is always allowed, noted as A/n. When used with some standard rules, two-category combination is labelled as "Np/v"++<tag>, "Np/a"++<tag>, "P/a"++<tag>, or "A/n"++<tag>. Category conversions only happen in particular contexts.

    -- The conversion from s\.np, (s\.np)/.np, or ((s\.np)/.np)/.np to np is always allowed, and noted as Np/v.
    cateV1 = removeDup [(npCate, snd cs) | cs <- cs1, elem True (map (\x-> cateEqual x (fst cs)) cas)]  -- [(npCate, Seman)]
        where
        cas = map getCateFromString ["s\\.np","(s\\.np)/.np","((s\\.np)/.np)/.np"]
    -- cateV1 happens verbal phrases act as subject constituent.
    cateV2 = removeDup [(npCate, snd cs) | cs <- cs2, elem True (map (\x-> cateEqual x (fst cs)) cas)]  -- [(npCate, Seman)]
        where
        cas = map getCateFromString ["s\\.np","(s\\.np)/.np","((s\\.np)/.np)/.np"]
    -- cateV2 happens verbal phrases act as object constituent.
    ctsByvToNp = [rule cate1 cate2 | rule <- [appB,raiFh], cate1 <- cateV1, cate2 <- cs_2, onOff!!1 == '+'] ++ [rule cate1 cate2 | rule <- [appF], cate1 <- cs_1, cate2 <- cateV2, onOff!!1 == '+']
        where
        cas1 = map getCateFromString ["s\\.np","(s\\.np)/.np"]
        cs_2 = removeDup [x| x <- cs2, elem True (map (\y-> cateEqual y (fst x)) cas1)]
        cas2 = map getCateFromString ["(s\\.np)/.np","((s\\.np)/.np)/.np"]
        cs_1 = removeDup [x| x <- cs1, elem True (map (\y-> cateEqual y (fst x)) cas2)]
    catesByvToNp = [(fst4 cate, "Np/v-" ++ snd4 cate, thd4 cate, fth4 cate) | cate <- ctsByvToNp]
   
    -- The conversion from np/.np to np is always allowed, and noted as Np/a.
    cateA1 = removeDup [(npCate, snd cs) | cs <- cs1, cateEqual (fst cs) cateAdj]  -- [(npCate, Seman)]
        where
        cateAdj = getCateFromString "np/.np"
    -- cateA1 happens adjective phrases act as subject constituent.
    cateA2 = removeDup [(npCate, snd cs) | cs <- cs2, cateEqual (fst cs) cateAdj]  -- [(npCate, Seman)]
        where
        cateAdj = getCateFromString "np/.np"
    -- cateA2 happens adjective phrases act as object constituent.
    ctsByaToNp = [rule cate1 cate2 | rule <- [appB, raiFh], cate1 <- cateA1, cate2 <- cs_2, onOff!!2 == '+'] ++ [rule cate1 cate2 | rule <- [appF], cate1 <- cs_1, cate2 <- cateA2, onOff!!2 == '+']
        where
        cas1 = map getCateFromString ["s\\.np","(s\\.np)/.np"]
        cs_2 = removeDup [x| x <- cs2, elem True (map (\y-> cateEqual y (fst x)) cas1)]
        cas2 = map getCateFromString ["(s\\.np)/.np","((s\\.np)/.np)/.np"]
        cs_1 = removeDup [x| x <- cs1, elem True (map (\y-> cateEqual y (fst x)) cas2)]
    catesByaToNp = [(fst4 cate, "Np/a-" ++ snd4 cate, thd4 cate, fth4 cate) | cate <- ctsByaToNp]
 
    -- The conversion from np/.np to s\.np is always allowed, and noted as P/a.
    cateA3 = removeDup [(catePred, snd cs) | cs <- cs2, cateEqual (fst cs) cateAdj]  -- [(s\.np, Seman)]
        where
        cateAdj = getCateFromString "np/.np"
        catePred = getCateFromString "s\\.np"
    -- cateA3 happens adjective phrases act as predicate constituent, namely cate1 is npCate.
    ctsByaToP = [rule cate1 cate2 | rule <- [appB], cate1 <- cs_1, cate2 <- cateA3, onOff!!3 == '+']
        where
        cs_1 = removeDup [x| x<- cs1, fst x == npCate]
    catesByaToP = [(fst4 cate, "P/a-" ++ snd4 cate, thd4 cate, fth4 cate) | cate <- ctsByaToP]

    -- The conversion from np to np/.np is ONLY allowed when nouns act as attribute, and noted as A/n.
    cateN1 = removeDup [(cateAdj, snd cs) | cs <- cs1, (fst cs) == npCate]  -- [(np/.np, Seman)]
        where
        cateAdj = getCateFromString "np/.np"
    ctsBynToA = [rule cate1 cate2 | rule <- [appF], cate1 <- cateN1, cate2 <- cs_2, onOff!!4 == '+']
        where
        cs_2 = removeDup [x| x<- cs2, fst x == npCate]
    catesBynToA = [(fst4 cate, "A/n-" ++ snd4 cate, thd4 cate, fth4 cate) | cate <- ctsBynToA]

    -- The categories getten by all rules.
    cates = catesBasic ++ catesBysToNp ++ catesByvToNp ++ catesByaToNp ++ catesByaToP ++ catesBynToA

    -- Remove Nil's resultant cateories and duplicate ones.
    rcs = removeDup [rc | rc <- cates, (fst4 rc) /= nilCate]

-- Context-based category conversion might be human brain's mechanism for syntax parsing, similiar to Chinese phrase-centric syntactic view. In the past, a phrase usually has a sequence of categories with the first as its classical category followed by non-classical ones.
-- To suitable for two kinds of category views, Phrase category is defined as a triple, including start position, span, and a categorial list. There might be more than one category in the list under category conversion.
-- To remember the parent categories, the start position of the second category is also recorded. For initial word categories, their parents don't exist.

-- Words are considered as minimal phrases, thus an universal phrase category models both words and phrases.
-- Initialize the values of PhraCate for each word of a given sentence.
-- Initial categories are designated manually, so their tags are "Desig".
-- Initial categories are all active.
initPhraCate :: [(Category, Seman)] -> [PhraCate]
initPhraCate [] = []
initPhraCate [c] = [((0,0),[(fst c, "Desig", snd c, True)],0)]    -- categories start at index 0
initPhraCate (c:cs) = [((0,0),[(fst c,"Desig",snd c, True)],0)] ++ [(((stOfCate pc)+1, 0), ctsOfCate pc, (stOfCate pc)+1) | pc <- (initPhraCate cs)]

-- Create a phrasal category according to its specified contents, here the phrasal category has only one category.
createPhraCate :: Start -> Span -> Category -> Tag -> Seman -> Act -> SecStart -> PhraCate
createPhraCate start span c tag seman act secStart
    | c == nilCate = ((start,span),[],secStart)
    | otherwise = ((start,span),[(c, tag, seman, act)],secStart)

-- Create a phrasal category that includes more than one category, respectively via different rules and with different semantic components.
createPhraCate2 :: Start -> Span -> [(Category, Tag, Seman, Act)] -> SecStart -> PhraCate
createPhraCate2 start span cts secStart = ((start,span),cts,secStart)

-- Find the number of input categories from the closure of phrase categories.

getNuOfInputCates :: [PhraCate] -> Int
getNuOfInputCates phraCateClosure = length [pc | pc <- phraCateClosure, spOfCate pc == 0]

-- New span's phrasal categories generted from One trip of transition.
-- The first input parameter is On/Off string, for turning on/off Np/s-, Np/v-, Np/a-, P/a-, and A/n- rules.
-- For examples, "+-++-" means using Np/s-, Np/a-, and P/a- rules, but denying Np/v- and A/n- rules.
newSpanPCs :: String -> [PhraCate] -> [PhraCate] -> [PhraCate]
newSpanPCs onOff origPhraCate newPhraCate = cbs
    where
      cur = removeDup $ origPhraCate ++ newPhraCate
      combs1 = [cateComb_CS onOff pc1 pc2 | pc1 <- cur, pc2 <- newPhraCate, pc1 /= pc2]
      combs2 = [cateComb_CS onOff pc1 pc2 | pc1 <- newPhraCate, pc2 <- cur, pc1 /= pc2]
      cbs = prune cur $ removeDup [cb | cb <- (combs1 ++ combs2), ctsOfCate cb /= [] ]

-- Parsing a sequence of categories is actually to generate the category closure from the initial phrase categories.
-- To avoid repeatedly try to combine the same two phrasal categories, input two lists of phrasal categories, origPhraCates and newPhraCates.
-- Initially, origPhraCates is [], and newPhraCates is initial phrasal categories.
-- For each trip of transition, try to combine two phrasal categories, one from newPhraCates, and anothor from newPhraCates ++ origPhraCates.
-- Recursively call function 'parse' on origPhraCates and newPhraCates,origPhraCates = origPhraCates ++ newPhraCates, and newPhraCates is the trip-generating ones.
-- The first input parameter is On/Off string, for turning on/off Np/s-, Np/v-, Np/a-, P/a-, and A/n- rules.
-- For examples, "+-++-" means using Np/s-, Np/a-, and P/a- rules, but denying Np/v- and A/n- rules.
-- To reduce the size of phrasal closure as much as possible, linguistic knowledge about parsing tree is again introduced. From so-called semantic distance, adverbals close verbs nearer than complements, and objects close verbs nearer than subjects. A priority about categorial combinations is built. If a phrase takes part in two category combinations, usually with left phrase and right phrase respectively, the two combination should compare their priorities, and only higher one is conducted while the lower one is banned, the categories forming the higher combination will be set inactive, namely they are no longer considered to form new category combiantions. The final closure is comprised of an root category and other inactive phrasal categories.

parse :: String -> [PhraCate] -> [PhraCate] -> [PhraCate]
parse onOff origPhraCate newPhraCate
    | cbs == [] = cur
    | otherwise = parse onOff cur2 cbs
        where
        cur = removeDup $ origPhraCate ++ newPhraCate  
        combs1 = [cateComb_CS onOff pc1 pc2 | pc1 <- cur, pc2 <- newPhraCate, pc1 /= pc2]
        combs2 = [cateComb_CS onOff pc1 pc2 | pc1 <- newPhraCate, pc2 <- cur, pc1 /= pc2]
        cbs = prune onOff cur $ removeDup [cb | cb <- (combs1 ++ combs2), ctsOfCate cb /= [] ]
        cur2 = deactPhraCate cur 

-- High-to-low priority list of categorial combinations 
-- Here, all kinds of categorial combinations should be listed.
-- MQ: quantity phrase, XX: conjunction phrase; DHv: adverbial-verb (headword) phrase; HvC: verb (headword)-complement phrase; DHa: adverbial-adjective (headword) phrase; AHn: attribute-noun (headword) phrase; HnC: noun (headword)-complement phrase; VO: verb-object phrase; OE: object extraction phrase; U1P: 1-auxiliary word phrase; U2P: 2-auxiliary word phrase; U3P: 3-auxiliary word phrase; PO: preposition object phrase; SP: subject-predicate phrase; EM: exclamation mood.
-- Some phrasal combinations are banned, ex. (s\.np)/.np np -> (s\.np)/.np [A/n ->B]
priorList :: [String]
priorList = ["MQ","XX","DHv","HvC","DHa","AHn","HnC","VO","OE","U1P","U2P","U3P","PO","SP","EM"]

-- Like pruning in game search, any phrasal category not appearing in the fianl parsing tree is thrown off when just generated, and any phrasal category having taken part in category combination is not tried to combine with other categoriesagain.
-- The first input parameter is On/Off string, for turning on/off Np/s-, Np/v-, Np/a-, P/a-, and A/n- rules.
-- Input the last transition <cur> and the new generated categories <cbs>. Among <cbs>, every two elements overlapping their spans will be compared on their combination priorities, and the lower one will be removed. 
 
prune :: String -> [PhraCate] -> [PhraCate] -> [PhraCate]
prune onOff cur cbs
    | cbs == [] = []
    | cbs == [x] = [x]
    | otherwise  == remLowPriCate onOff cur cbs

-- Remove all low-priority phrasal categories from a set of phrasal categories.
-- The first input parameter is On/Off string, for turning on/off Np/s-, Np/v-, Np/a-, P/a-, and A/n- rules.
-- Input the last transition <cur> and the new generated cagtegories <cbs>. 
remLowPriCate :: String -> [PhraCate] -> [PhraCate] -> [PhraCate]
remLowPriCate _ _ [x] = [x]     -- End condition of recursive calling.
remLowPriCate onOff cur cbs
    | pcps == [] = cbs          -- No overlapping phrasal categories.
    | isPrior onOff cur c1 c2 = remLowPriCate onOff cur $ remOneFromList c2 cbs
    | otherwise = remLowPriCate onOff cur $ remOneFromList c1 cbs
    where
      pcps = getOverlap cbs     -- Get all pairs of overlapping phrasal categories.
      c1 = fst (head pcps)      -- Get the first category from the first pair of ones.
      c2 = snd (head pcps)      -- Get the second category from the first pair of ones.

-- Get all overlapping two phrasal categories.
getOverlap :: [PhraCate] -> [(PhraCate, PhraCate)]
getOverlap pcs = [(x,y)| x <- pcs, y <- pcs, x /= y, isOverlap x y]


-- Decide whether two phrasal categories are overlapped.
isOverlap :: PhraCate -> PhraCate -> Bool
isOverlap pc1 pc2
    | (st1 <= st2 && st2 <= (st1 + sp1)) || (st1 <= (st2 + sp2) && (st2 + sp2) <= (st1 + sp1)) = True
    | otherwise = False
    where
    st1 = stOfCate pc1
    sp1 = spOfCate pc1
    st2 = stOfCate pc2
    sp2 = spOfCate pc2

-- Compare two phrasal categories and give which one is prior. True for the first, and False for the second.
-- The first input parameter is On/Off string, for turning on/off Np/s-, Np/v-, Np/a-, P/a-, and A/n- rules.
-- Input the last transition <cur> for getting names of phrasal categories.
isPrior :: String -> [PhraCate] -> PhraCate -> PhraCate -> Bool
isPrior onOff cur pc1 pc2
    | i1 < i2 = True
    | otherwise = False
    where
    n1 = getNameOfPhraCate onOff cur pc1
    n2 = getNameOfPhraCate onOff cur pc2
    ind1 = elemIndex n1 priorList
    i1 = maybe (-1) (0+) ind1        -- Result -1 for no <ind1>
    ind2 = elemIndex n2 priorList
    i2 = maybe (-1) (0+) ind2

-- Remove the first element equal to a given value from a list of elements.
remOneFromList :: Eq a => a -> [a] -> [a]
remOneFromList c [] = []
remOneFromList c (p:ps)
    | c == p = ps
    | otherwise = p : remOneFromList c ps

-- Identify a given phrasal category, and find its string name.
-- The first input parameter is On/Off string, for turning on/off Np/s-, Np/v-, Np/a-, P/a-, and A/n- rules.
-- Input a list of phrasal categories, from which the parent categories of the given category can be found.
getNameOfPhraCate :: String -> PhraCate -> [PhraCate] -> String
getNameOfPhraCate onOff pc cur
    | lca == getCateFromString "s/*s" && rca == getCateFromString "(np/*np)\\*(s/*s)" = "MQ"
    | lca == getCateFromString "(X\\*X)/*X" || rca == getCateFromString "X\\*X" = "XX"
    | lca == getCateFromString "(s\\.np)/#(s\\.np)" && (ta == ">" | ta == ">B") = "DHv"
    | rca == getCateFromString "(s\\.np)\\x(s\\.np)" && (ta == "<" | ta == "<Bx") = "HvC"
    | lca == getCateFromString "(np/.np)/*(np/.np)" && cateEqual rca (getCateFromString "np/.np") = "DHa"
    | (cateEqual lca (getCateFromString "np/.np") && ta == ">") || (lca == npCate && ta == "A/n->") = "AHn"
    | lca == npCate && cateEqual rca (getCateFromString "np\.np") = "HnC"
    | (lca == getCateFromString "(s\\.np)/.np" || lca == getCateFromString "((s\.np)/.np)/.np") && ta == ">" = "VO"
    | lca == npCate && ta == ">T->B" = "OE"
    | rca == getCateFromString "(np/*np)\\*np" || rca == getCateFromString "(np/*np)\\*(np/.np)" || lca == getCateFromString "(np/*np)\\*(s\\.np)" = "U1P"
    | rca == getCateFromString "((s\\.np)/#(s\\.np))\\*(np/.np)" = "U2P"
    | lca == getCateFromString "((s\\.np)\\x(s\\.np))/*(np/.np)" = "U3P"
    | lca == getCateFromString "((s\\.np)/#(s\\.np))/*np" || lca == getCateFromString "((s\\.np)\\x(s\\.np))/*np" || lca == getCateFromString "(s/*s)/*np" = "PO"
    | lca == npCate && (rca == getCateFromString "s\\.np" || ta == "P/a-<" = "SP"
    | rca == getCateFromString "s\\*s" = "EM"
    where
      parents = findSplitCate onOff pc cur     -- [(PhraCate,PhraCate)]
      lp = fst (parents!!0)       -- Only one pair of parents
      rp = snd (parents!!0)       -- The right parent
      lca = caOfCate lp           -- Category of left parent
      rca = caOfCate rp           -- Category of right parent 
      ca = (caOfCate pc)!!0       -- Category
      ta = (taOfCate pc)!!0       -- Tag 
      ac = (acOfCate pc)!!0       -- Act
    
-- Deactivate those phrasal categories used for generating some given phrasal categories.
-- Search <cur> to find those categories used for generating ones from <cbs>.
-- Here, every phrasal category has ONLY ONE element in its component [(category, tag, seman, act)].
-- The first input parameter is On/Off string, for turning on/off Np/s-, Np/v-, Np/a-, P/a-, and A/n- rules.
-- For examples, "+-++-" means using Np/s-, Np/a-, and P/a- rules, but denying Np/v- and A/n- rules.

deactPhraCate :: String -> [PhraCate] -> [PhraCate] -> [PhraCate]
deactPhraCate onOff cur cbs = non_parents : map deactOnePC parents
    where
    parents = [x| pc <- cbs, pps <- findSplitCate onOff pc cur, psp <- unzip pps,  x <- (fst psp)++(snd psp)]
    non_parents = [y| y <- cur, notElem y parents]
    
-- Deactivate a phrasal category.
deactOnePC :: PhraCate -> PhraCate
deactOnePC ((_,_),[],_) = ((_,_),[],_)
deactOnePC ((_,_),[x],_) = ((_,_),[(caOfCate x,taOfCate x, seOfCate x, False)],_)
deactOnePC ((_,_),(x:xs),_) = error "Failed to deactivate a non-atomized phrasal category." 

-- In the result of syntactic parsing, one phrasal category is defined as ((Start,Span),[(Category,Tag,Seman)],SecStart), allowing one phrasal category to have more than one triple (<category>,<tag>,<seman>).
-- The following function atomizePhraCate is used to unpack one phrasal category into multiple phrasal categories, each of which has only one triple (<category>,<tag>,<seman>). 
atomizePhraCate :: [PhraCate] -> [PhraCate]
atomizePhraCate [] = []
atomizePhraCate [pc] = removeDup [((st,sp),[cts1],ss) | cts1 <- cts]
    where
    st = stOfCate pc
    sp = spOfCate pc
    cts = ctsOfCate pc
    ss = ssOfCate pc
atomizePhraCate (x:xs) = removeDup $ (atomizePhraCate [x]) ++ (atomizePhraCate xs)

-- Generate syntactic trees (forest) from the closure of phrase categories.
-- Here is a recursived forest-growing algorithm: For the input forest, 
-- (1) If it is an empty forest without any tree, an empty forest is returned;
-- (2) Otherwise, one forest is created from a tree in input forest, return the union of all created forests.
 
growForest :: String -> [[PhraCate]] -> [PhraCate] -> [[PhraCate]]
growForest _ [] _ = []                    -- Empty forest
growForest onOff (t:ts) phraCateClosure       -- nonempty forest
    = (growTree onOff t phraCateClosure) ++ (growForest onOff ts phraCateClosure) 

-- One tree can grow at all tips to send forth a layer of leaves. Every parsing tree in Combinatory Categorial Grammar is one binary tree, namely from every tip, only two leaces can grow out. It makes growing process complicated that there might be more than one pair of leaves to grow out for every tip, Selecting different pairs would create different trees. When more than one rule can be used to combine two parent categories, multiple resultant categories are created.
-- The forest growing from a tree is done by the following:
-- (1) Find all tips able to send forth leaves (The tips for initial categoies are no longer to grow);
-- (2) For every such tip, find all splits (namely all pairs of leaves), and create a forest, of which every tree include the input tree and a distinc pair of leaves;
-- (3) Based on merging two forests, all forests are merged into one forest. 

-- The first input parameter is On/Off string, for turning on/off Np/s-, Np/v-, Np/a-, P/a-, and A/n- rules.
-- For examples, "+-++-" means using Np/s-, Np/a-, and P/a- rules, but denying Np/v- and A/n- rules.

growTree :: String -> [PhraCate] -> [PhraCate] -> [[PhraCate]]
growTree onOff t phraCateClosure
    | findTipsOfTree onOff t phraCateClosure == [] = [t]      -- No tip can grow.
    | [t] == gf = [t]                                   -- Not grow.
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

-- Find splited (namely parent) categories for a given phrase category from the closure of phrase categories. 
-- Here, every phrasal category has only one element in its component [(category, tag, seman, act)].
-- The first input parameter is On/Off string, for turning on/off Np/s-, Np/v-, Np/a-, P/a-, and A/n- rules.
-- For examples, "+-++-" means using Np/s-, Np/a-, and P/a- rules, but denying Np/v- and A/n- rules.

findSplitCate :: String -> PhraCate -> [PhraCate] -> [(PhraCate, PhraCate)]
findSplitCate onOff pc phraCateClosure
    = [pct | pct <- pcTuples, pcBelong pc (cateComb_CS onOff (fst pct) (snd pct))]  
                      -- Using pcBelong not (==),The function cateComb_CS might create multiple categories.
        where
        st1 = stOfCate pc
        st2 = ssOfCate pc
        sp1 = st2 - st1 - 1                -- When pc is a leaf, sp1 is -1.
        sp2 = spOfCate pc - sp1 - 1        -- When pc is a leaf, sp2 is 0, that is, the second parent is pc.
        pcTuples = [(x, y) | x <- (findCate (st1, sp1) phraCateClosure), y <- (findCate (st2, sp2) phraCateClosure)]
                                           -- When pc is a leaf, pcTuples is []. 
 
-- Find all growable tips of a tree from the closure of phrase categories.
-- Those nodes whose parents already exist in the tree can't grow again.
-- Those tips corresponding to initial categories are not growable.

findTipsOfTree :: String -> [PhraCate] -> [PhraCate] -> [PhraCate]
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

-- Quick sort a list of phrasal categoies.
quickSort :: [PhraCate] -> [PhraCate]
quickSort [] = []
quickSort [x] = [x]
quickSort (x:xs) = (quickSort [y|y<-xs, pclt y x]) ++ [x] ++ (quickSort [y|y<-xs, pclt x y])

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
-- The string has format "((start,span),[(cate,tag,sem,act)],secStart)".
getPhraCateFromString :: String -> PhraCate
getPhraCateFromString str = ((st,sp),[(cate,tag,sem,act)],ss)
    where
    s = splitAtDeli str ',' 
    st = read (throwBrac (s!!0)) :: Int
    sp = read (throwBrac (s!!1)) :: Int
    cate = getCateFromString (drop 2 (s!!2))
    tag = s!!3
    sem = s!!4
    act = read (throwBrac (s!!5)) :: Bool
    ss = read (throwBrac (s!!6)) :: Int



