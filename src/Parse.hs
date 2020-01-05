-- Copyright China University of Water Resources and Electric Power (c) 2019
-- All rights reserved.

module Parse (
    Start,         -- Int
    Span,          -- Int
    SecStart,      -- Int
    PhraCate,      -- ((Start, Span), [(Category, Tag, Seman)], SecStart)
    pclt,          -- PhraCate -> PhraCate -> Bool
    pcBelong,      -- PhraCate -> PhraCate -> Bool
    stOfCate,      -- PhraCate -> Start
    spOfCate,      -- PhraCate -> Span
    ctsOfCate,     -- PhraCate -> [(Category, Tag, Seman)]
    caOfCate,      -- PhraCate -> [Category]
    taOfCate,      -- PhraCate -> [String]
    seOfCate,      -- PhraCate -> [Seman]
    csOfCate,      -- PhraCate -> [(Category, Seman)]
    ssOfCate,      -- PhraCate -> SecStart
    cateComb,      -- PhraCate -> PhraCate -> [PhraCate]
    initPhraCate,  -- [(Category, Seman)] -> [PhraCate]
    createPhraCate,-- Start -> Span -> Category -> Tag -> Seman -> SecStart -> PhraCate
    createPhraCate2, -- Start -> Span -> [(Category,Tag,Seman)] -> SecStart -> PhraCate
    parse,         -- [PhraCate] -> [PhraCate]
    atomizePhraCate,         -- [PhraCate] -> [PhraCate]
    getNuOfInputCates,       -- [PhraCate] -> Int 
    growForest,              -- [[PhraCate]] -> [PhraCate] -> [[PhraCate]]
    growTree,      -- [PhraCate] -> [PhraCate] -> [[PhraCate]]
    findCate,                -- (Int, Int) -> [PhraCate] -> [PhraCate]
    findSplitCate,           -- PhraCate -> [PhraCate] -> [PhraCate]
    findTipsOfTree,          -- [PhraCate] -> [PhraCate] -> [PhraCate]
    findCateBySpan,          -- Int -> [PhraCate] -> [PhraCate]
    divPhraCateBySpan,       -- [PhraCate] -> [[PhraCate]]
    quickSort                -- [PhraCate] -> [PhraCate]
    ) where

import Data.Tuple.Utils
import Category
import Rule

type Start = Int         -- The start position of a phrase (category) in sentences.
type Span = Int          -- The span distance of a phrase (category) in sentences.
type SecStart = Int      -- The position of spliting a phrase (category).

-- When combining two phrase categories, there might be more than one rule available, resulting in multiple categories (Usually the resultant categories are same).

type PhraCate = ((Start, Span), [(Category, Tag, Seman)], SecStart)

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

-- The following functions are used to select an element from tuple PhraCate.
stOfCate :: PhraCate -> Start
stOfCate (s, _, _) = fst s

spOfCate :: PhraCate -> Span
spOfCate (s, _, _) = snd s

ctsOfCate :: PhraCate -> [(Category, Tag, Seman)]
ctsOfCate (_, cts, _) = cts

caOfCate :: PhraCate -> [Category]
caOfCate pc = [fst3 c | c <- cts]
    where
    cts = ctsOfCate pc

taOfCate :: PhraCate -> [Tag]
taOfCate pc = [snd3 c | c <- cts]
    where
    cts = ctsOfCate pc

seOfCate :: PhraCate -> [Seman]
seOfCate pc = [thd3 c | c <- cts]
    where
    cts = ctsOfCate pc

csOfCate :: PhraCate -> [(Category, Seman)]
csOfCate pc = zip (caOfCate pc) (seOfCate pc)

ssOfCate :: PhraCate -> SecStart
ssOfCate (_, _, s) = s

-- Function cateComb combines two input (phrasal) categories into one.
-- The two input categories satisfies concatenative requirements, and may have multiple resultant categories when multiple rules are available.
-- After introduing categorial conversion for Chinese structure overlapping, there might be even more categories.
-- Results ((-1,-1),[],-1) and ((x,y),[],z) respectively denote concatenative failure and no rule available.

cateComb :: PhraCate -> PhraCate -> PhraCate
cateComb pc1 pc2
    | st1 + sp1 + 1 /= st2 = ((-1,-1),[],-1)
    | otherwise = ((st1, sp1 + sp2 + 1), rcs, st2)
    where
    st1 = stOfCate pc1   -- Start position of pc1
    sp1 = spOfCate pc1   -- Span of pc1
    st2 = stOfCate pc2   -- Start position of pc2
    sp2 = spOfCate pc2   -- Span of pc2
    cs1 = csOfCate pc1   -- [(Category, Seman)]
    cs2 = csOfCate pc2   -- [(Category, Seman)]
    
    -- Categories getten by CCG standard rules.
    catesBasic = [rule cate1 cate2 | rule <- rules, cate1 <- cs1, cate2 <- cs2]

    -- Categories getten by firstly converting sentence into nominal phrase, then using standard CCG rules. For each result (<category>, <tag>, <seman>), the <tag> is changed as "Np/s-"++<tag> to remember the category conversion s->Np which happens before using the standard rule <tag>. Actually, category "s" appears amid a sentence, that means it relates to a clause. The rule Np/s is always used together with a certain standard rule to implement two-category combination.
    cateS1 = [(npCate, snd cs) | cs <- cs1, fst cs == sCate]     -- [(npCate, Seman)]
    cateS2 = [(npCate, snd cs) | cs <- cs2, fst cs == sCate]     -- [(npCate, Seman)]
    ctsBysToNp = [rule cate1 cate2 | rule <- rules, cate1 <- cateS1, cate2 <- cs2] ++ [rule cate1 cate2 | rule <- rules, cate1 <- cs1, cate2 <- cateS2]
    catesBysToNp = [(fst3 cate, "Np/s-" ++ snd3 cate, thd3 cate) | cate <- ctsBysToNp]

    -- According to Jia-xuan Shen's theory, successive inclusions from noun to verb, and to adjective, the conversion from s\.np, (s\.np)/.np, ((s\.np)/.np)/.np, or np/.np to np is allowed, also is from np/.np to s\.np, noted as Np/v, Np/a and P/a respectively. Besides, the conversion from np to np/.np is always allowed, noted as A/n. When used with some standard rules, two-category combination is labelled as "Np/v"++<tag>, "Np/a"++<tag>, "P/a"++<tag>, or "A/n"++<tag>.

    -- The conversion from s\.np, (s\.np)/.np, or ((s\.np)/.np)/.np to np is always allowed, and noted as Np/v.
    cateV1 = [(npCate, snd cs) | cs <- cs1, elem True (map (\x-> cateEqual x (fst cs)) cas)]  -- [(npCate, Seman)]
        where
        cas = map getCateFromString ["s\\.np","(s\\.np)/.np","((s\\.np)/.np)/.np"]
    cateV2 = [(npCate, snd cs) | cs <- cs2, elem True (map (\x-> cateEqual x (fst cs)) cas)]  -- [(npCate, Seman)]
        where
        cas = map getCateFromString ["s\\.np","(s\\.np)/.np","((s\\.np)/.np)/.np"]
    ctsByvToNp = [rule cate1 cate2 | rule <- rules, cate1 <- cateV1, cate2 <- cs2] ++ [rule cate1 cate2 | rule <- rules, cate1 <- cs1, cate2 <- cateV2]
    catesByvToNp = [(fst3 cate, "Np/v-" ++ snd3 cate, thd3 cate) | cate <- ctsByvToNp]
   
    -- The conversion from np/.np to np is always allowed, and noted as Np/a.
    cateA1 = [(npCate, snd cs) | cs <- cs1, cateEqual (fst cs) cateAdj]  -- [(npCate, Seman)]
        where
        cateAdj = getCateFromString "np/.np"
    cateA2 = [(npCate, snd cs) | cs <- cs2, cateEqual (fst cs) cateAdj]  -- [(npCate, Seman)]
        where
        cateAdj = getCateFromString "np/.np"
    ctsByaToNp = [rule cate1 cate2 | rule <- rules, cate1 <- cateA1, cate2 <- cs2] ++ [rule cate1 cate2 | rule <- rules, cate1 <- cs1, cate2 <- cateA2]
    catesByaToNp = [(fst3 cate, "Np/a-" ++ snd3 cate, thd3 cate) | cate <- ctsByaToNp]
 
    -- The conversion from np/.np to s\.np is always allowed, and noted as P/a.
    cateA_1 = [(catePred, snd cs) | cs <- cs1, cateEqual (fst cs) cateAdj]  -- [(s\.np, Seman)]
        where
        cateAdj = getCateFromString "np/.np"
        catePred = getCateFromString "s\\.np"
    cateA_2 = [(catePred, snd cs) | cs <- cs2, cateEqual (fst cs) cateAdj]  -- [(s\.np, Seman)]
        where
        cateAdj = getCateFromString "np/.np"
        catePred = getCateFromString "s\\.np"
    ctsByaToP = [rule cate1 cate2 | rule <- rules, cate1 <- cateA_1, cate2 <- cs2] ++ [rule cate1 cate2 | rule <- rules, cate1 <- cs1, cate2 <- cateA_2]
    catesByaToP = [(fst3 cate, "P/a-" ++ snd3 cate, thd3 cate) | cate <- ctsByaToP]

    -- The conversion from np to np/.np is always allowed, and noted as A/n.
    cateN1 = [(cateAdj, snd cs) | cs <- cs1, (fst cs) == npCate]  -- [(np/.np, Seman)]
        where
        cateAdj = getCateFromString "np/.np"
    cateN2 = [(cateAdj, snd cs) | cs <- cs2, (fst cs) == npCate]  -- [(np/.np, Seman)]
        where
        cateAdj = getCateFromString "np/.np"
    ctsBynToA = [rule cate1 cate2 | rule <- rules, cate1 <- cateN1, cate2 <- cs2] ++ [rule cate1 cate2 | rule <- rules, cate1 <- cs1, cate2 <- cateN2]
    catesBynToA = [(fst3 cate, "A/n-" ++ snd3 cate, thd3 cate) | cate <- ctsBynToA]

    -- The categories getten by all rules.
    cates = catesBasic ++ catesBysToNp ++ catesByvToNp ++ catesByaToNp ++ catesByaToP ++ catesBynToA

    -- Remove Nil's resultant cateories and duplicate ones.
    rcs = ctsRemoveDup [rc | rc <- cates, (fst3 rc) /= nilCate]
        where
        ctsRemoveDup [] = []
        ctsRemoveDup [x] = [x]
        ctsRemoveDup (x:xs)
            | elem x xs = ctsRemoveDup xs
            | otherwise = x:(ctsRemoveDup xs)
        -- ctsRemoveDup is used to remove duplicate elements in a list.

-- Context-based category conversion might be human brain's mechanism for syntax parsing, similiar to Chinese phrase-centric syntactic view. In the past, a phrase usually has a sequence of categories with the first as its classical category followed by non-classical ones.
-- To suitable for two kinds of category views, Phrase category is defined as a triple, including start position, span, and a categorial list. There might be more than one category in the list under category conversion.
-- To remember the parent categories, the start position of the second category is recorded. For initial word categories, their parents don't exist.

-- Words are considered as minimal phrases, thus an universal phrase category models both words and phrases.
-- Initialize the values of PhraCate for each word of a given sentence.
-- Initial categories are designated manually, so their tags are "Desig"
initPhraCate :: [(Category, Seman)] -> [PhraCate]
initPhraCate [] = []
initPhraCate [c] = [((0,0),[(fst c, "Desig", snd c)],0)]    -- categories start at index 0
initPhraCate (c:cs) = [((0,0),[(fst c,"Desig",snd c)],0)] ++ [(((stOfCate pc)+1, 0), ctsOfCate pc, (stOfCate pc)+1) | pc <- (initPhraCate cs)]

-- Create a phrasal category according to its specified contents, here the phrasal category has only one category.
createPhraCate :: Start -> Span -> Category -> Tag -> Seman -> SecStart -> PhraCate
createPhraCate start span c tag seman secStart
    | c == nilCate = ((start,span),[],secStart)
    | otherwise = ((start,span),[(c, tag, seman)],secStart)

-- Create a phrasal category that includes more than one category, respectively via different rules and with different semantic components.
createPhraCate2 :: Start -> Span -> [(Category, Tag, Seman)] -> SecStart -> PhraCate
createPhraCate2 start span cts secStart = ((start,span),cts,secStart)

-- Find the number of input categories from the closure of phrase categories.

getNuOfInputCates :: [PhraCate] -> Int
getNuOfInputCates phraCateClosure = length [pc | pc <- phraCateClosure, spOfCate pc == 0]

-- Parsing a sequence of categories is actually to generate the category closure from the initial phrase categories.

parse :: [PhraCate] -> [PhraCate]
parse phraCateInput
    | phraCateInput == transition = phraCateInput
    | otherwise = parse transition
        where
        combs = [cateComb pc1 pc2 | pc1 <- phraCateInput, pc2 <- phraCateInput, pc1 /= pc2]
        cbs = [cb | cb <- combs, cb /= ((-1,-1),[],-1), elem cb phraCateInput == False]
        transition = phraCateInput ++ (cbsRemoveDup cbs)
            where
            cbsRemoveDup [] = []
            cbsRemoveDup [x] = [x]
            cbsRemoveDup (x:xs)
                | elem x xs = cbsRemoveDup xs
                | otherwise = x:(cbsRemoveDup xs)
        -- cbsRemoveDup is used to remove duplicate elements in a list.

-- In the result of syntactic parsing, one phrasal category is defined as ((Start,Span),[(Category,Tag,Seman)],SecStart), allowing one phrasal category to have more than one triple (<category>,<tag>,<seman>).
-- The following function atomizePhraCate is used to unpack one phrasal category into multiple phrasal categories, each of which has only one triple (<category>,<tag>,<seman>). 
atomizePhraCate :: [PhraCate] -> [PhraCate]
atomizePhraCate [] = []
atomizePhraCate [pc] = [((st,sp),[cts1],ss) | cts1 <- cts]
    where
    st = stOfCate pc
    sp = spOfCate pc
    cts = ctsOfCate pc
    ss = ssOfCate pc
atomizePhraCate (x:xs) = (atomizePhraCate [x]) ++ (atomizePhraCate xs)

-- Generate syntactic trees (forest) from the closure of phrase categories.
-- Here is a recursived forest-growing algorithm: For the input forest, 
-- (1) If it is an empty forest without any tree, an empty forest is returned;
-- (2) Otherwise, one forest is created from a tree in input forest, return the union of all created forests.
 
growForest :: [[PhraCate]] -> [PhraCate] -> [[PhraCate]]
growForest [] _ = []                    -- Empty forest
growForest (t:ts) phraCateClosure       -- nonempty forest
    = (growTree t phraCateClosure) ++ (growForest ts phraCateClosure) 

-- One tree can grow at all tips to send forth a layer of leaves. Every parsing tree in Combinatory Categorial Grammar is one binary tree, namely from every tip, only two leaces can grow out. It makes growing process complicated that there might be more than one pair of leaves to grow out for every tip, Selecting different pairs would create different trees. When more than one rule can be used to combine two parent categories, multiple resultant categories are created.
-- The forest growing from a tree is done by the following:
-- (1) Find all tips able to send forth leaves (The tips for initial categoies are no longer to grow);
-- (2) For every such tip, find all splits (namely all pairs of leaves), and create a forest, of which every tree include the input tree and a distinc pair of leaves;
-- (3) Based on merging two forests, all forests are merged into one forest. 
growTree :: [PhraCate] -> [PhraCate] -> [[PhraCate]]
growTree t phraCateClosure
    | findTipsOfTree t phraCateClosure == [] = [t]      -- No tip can grow.
    | [t] == gf = [t]                                   -- Not grow.
    | otherwise = growForest gf phraCateClosure
        where
        splOfAllTips = [findSplitCate tip phraCateClosure | tip <- (findTipsOfTree t phraCateClosure)]   -- [[(PhraCate, PhraCate)]]
        
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
-- Here, every phrasal category has only one element in its component [(category, tag, seman)].
findSplitCate :: PhraCate -> [PhraCate] -> [(PhraCate, PhraCate)]
findSplitCate pc phraCateClosure
    = [pct | pct <- pcTuples, pcBelong pc (cateComb (fst pct) (snd pct))]  
                      -- Using pcBelong not (==),The function cateComb might create multiple categories.
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

findTipsOfTree :: [PhraCate] -> [PhraCate] -> [PhraCate]
findTipsOfTree [] _ = []
findTipsOfTree t phraCateClosure
    | ppcs == [] = findTipsOfTree (tail ot) phraCateClosure      -- Leaves have no parents and can't grow.
    | foldl (||) False (map (\x -> elem (snd x) ot) ppcs) = findTipsOfTree (tail ot) phraCateClosure  
          -- If a node already grew, then its right parent should be in ordered phrasal series ot.
          -- But, if the node grew out multiple pairs of parent nodes, all parent node pairs must be checked.
          -- If there exists a certain right parent in ot, it can be concluded that the node is not a tip.
    | otherwise = (head ot):findTipsOfTree (tail ot) phraCateClosure -- Head node which doesn't yet grow.
       where
       ot = quickSort t    -- Such that there exists left parent << node << right parent for any node. 
       ppcs = findSplitCate (head ot) phraCateClosure    -- Find the parent pairs of node (head ot).
       
-- Find phrase categories with given span.
findCateBySpan :: Span -> [PhraCate] -> [PhraCate]
findCateBySpan sp  pcs = [x|x<-pcs, spOfCate x == sp]

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


