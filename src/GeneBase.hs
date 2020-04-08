-- Copyright (c) 2019-2020 China University of Water Resources and Electric Power,
-- All rights reserved.

module GeneBase (
    priorList,         -- [PhraStru]
    PriElem,           -- (PhraStru,PhraStru,PhraStru,PhraStru,Bool)
    priorList2,        -- [PriElem]
    isPrior2,          -- [PhraCate] -> PhraCate -> PhraCate -> Bool
    getStru,             -- Int -> [PhraCate] -> PhraStru
    getTag,            -- Int -> [PhraCate] -> PhraStru
    match,             -- [PriElem] -> ((Tag,PhraStru),(Tag,PhraStru),(Tag,PhraStru),(Tag,PhraStru),Ovt) -> Int
    getOverlap,        -- OnOff -> [PhraCate] -> [(PhraCate, PhraCate)]
    removeTuple,       -- [(a,a)] -> [(a,a)]
    getOverType,       -- OnOff -> [PhraCate] -> PhraCate -> PhraCate -> Int
    isPrior,           -- PhraCate -> PhraCate -> Bool
    ) where

import Data.Tuple
import Data.Tuple.Utils
import Data.List
import Category
import Rule
import AssignCate
import Utils

-- To now, the recognizable phrasal structures are as following.
-- MQ: quantity phrase, XX: conjunction phrase; DHv: adverbial-verb (headword) phrase; HvC: verb (headword)-complement phrase; DHa: adverbial-adjective (headword) phrase; AHn: attribute-noun (headword) phrase; HnC: noun (headword)-complement phrase; VO: verb-object phrase; OE: object extraction phrase; U1P: 1-auxiliary word phrase; U2P: 2-auxiliary word phrase; U3P: 3-auxiliary word phrase; PO: preposition object phrase; SP: subject-predicate phrase; EM: exclamation mood. For uniformity, word by word in a sentence is considered primitive phrase, noted as "DE", meaning designated.

phraStruList :: [PhraStru]
phraStruList = ["MQ","XX","DHv","HvC","DHa","HaC","AHn","HnC","VO","OE","U1P","U2P","U3P","PO","SP","EM","CC","DE","NR"]

-- To indicate which phrasal structure is more prior in an overlapping pair, a left-extending overlapping phrase and a right-extending overlapping phrase should be considered. As basic fragments, such four overlapping phrasal structures would exist in many sentences, and act like human body genes.
-- the structural gene StruGene is a 6-tuple (<leftExtend>, <leftOver>, <rightOver>, <rightExtend>, <overType>, <prior>), here <leftExtend> is the longest phrase with the first word of <leftOver> as end, <rightExtend> is the longest phrase with the last word of <rightOver> as head, and <leftOver> and <rightOver> are the left-to-right overlapping phrases, with <overType> to indicate overlapping type, and with <prior> to indicate which is prior to exist. Selecting the longest phrase among all those phrases with one word as end or head might be owing that the longest phrase is certainly active, and whether it is reasonable waits for demonstrations. 

type LeftExtend = (Tag, PhraStru)    -- Active left extend, including rule tag and structural name.
type LeftOver = (Tag, PhraStru)      -- Overlapping left phrase, including rule tag and structural name.
type RightOver = (Tag, PhraStru)     -- Overlapping right phrase.
type RightExtend = (Tag, PhraStru)   -- Active right extend.
type OverType = Int                  -- Overlapping type.
type Prior = Int               -- 1 means overlapping left phrase prior, 0 means right prior, and -1 means nothing.

type StruGene = (LeftExtend, LeftOver, RightOver, RightExtend, OverType, Prior)

-- The structural genes are stored in table stru_gene of MySQL database ccg4c.
-- The table stru_gene is like the following list. 
--    (("Desig", "DE"), (">", "VO"), ("A/n->", "AHn"), ("<", "SP"), 1, 0)
--    ((">", "VO"), ("A/n->", "AHn"), ("<", "SP"), ("A/v->", "AHn"), 1, 1)
--    (("A/n->", "AHn"), ("<", "SP"), ("A/v->", "AHn"), ("<", "HnC"), 1, 1)
--    (("<", "SP"), ("A/v->", "AHn"), ("<", "HnC"), ("Desig", "DE"),  1, 1)

-- Get structural name of a active phrase starting at 'st', and "NON" for ((-1,-1),[],-1).
getStru :: Int -> [PhraCate] -> PhraStru
getStru st trans
    | pc == ((-1,-1),[],-1) = "NON"
    | otherwise = (cnOfCate pc)!!0
    where
      pc = findActCateByStart st trans

-- Get the tag of rule used to generate the active phrase starting at 'st', return "NON" for ((-1,-1),[],-1).
getTag :: Int -> [PhraCate] -> PhraStru
getTag st trans
    | pc == ((-1,-1),[],-1) = "NON"
    | otherwise = (taOfCate pc)!!0
    where
      pc = findActCateByStart st trans

-- Select <prior> from 'stru_gene' where matching given four overlapping phrases.
-- "1" means <leftOver> exists while <rightOver> does not; "0" means the contrary. "2" means ether <leftOver> or <rightOver> exists but not both. "-1" means no matches.
-- <let>: Tag of left extending phrase,<les>),(),(),(),<overType>)
match :: ((Tag,PhraStru),(Tag,PhraStru),(Tag,PhraStru),(Tag,PhraStru),OverType) -> Int
match ((let,
match (x:xs) pe
    | mt = prix
    | otherwise = match xs pe
    where
      lntx = fst $ fst6 x            -- Left neighbour's tag
      lnnx = snd $ fst6 x            -- Left neighbour's name
      lotx = fst $ snd6 x            -- Left overlapping phrase's tag  
      lonx = snd $ snd6 x            -- Left overlapping phrase's name
      rotx = fst $ thd6 x            -- Right overlapping phrase's tag
      ronx = snd $ thd6 x            -- Right overlapping phrase's name
      rntx = fst $ fth6 x            -- Right neighbour's tag
      rnnx = snd $ fth6 x            -- Right neighbour's name
      ovtx = fif6 x                  -- Overlapping type
      prix = sth6 x                  -- Priority of item 'x'
      lntp = fst $ fst5 pe
      lnnp = snd $ fst5 pe
      lotp = fst $ snd5 pe
      lonp = snd $ snd5 pe
      rotp = fst $ thd5 pe
      ronp = snd $ thd5 pe
      rntp = fst $ fth5 pe
      rnnp = snd $ fth5 pe
      ovt = fif5 pe
      mt = (lntx == "_" || lntx == lntp) && (lnnx == "_" || lnnx == lnnp) &&
           (lotx == "_" || lotx == lotp) && (lonx == lonp) && 
           (rotx == "_" || rotx == rotp) && (ronx == ronp) && 
           (rntx == "_" || rntx == rntp) && (rnnx == "_" || rnnx == rnnp) &&
           (ovtx == -1  || ovtx == ovt )

-- Like pruning in game search, any phrasal category not appearing in the final parsing tree is thrown out after just generated, and any phrasal category having taken part in category combination should be set inactive, not allowed to combine with other categories again. When removing a category, its parent categories should be set active.
-- The first parameter is On/Off string, for turning on/off Np/s-, A/s-, Np/v-, A/v-, Np/a-, P/a-, Ca/a-, Cv/a-, and A/n- rules.
-- The second parameter is one-way transition result without pruning. Among <trans2>, every two elements overlapping their spans will be compared on their combination priorities, and the lower one will be removed. 
 
prune :: OnOff -> [PhraCate] -> [PhraCate]
prune onOff cbs
    | cb == ((-1,-1),[],-1) = cbs                    -- No overlapping phrases.
    | otherwise = prune onOff $ removeOnePC onOff cb cbs
    where
      cbps = getOverlap onOff cbs   
               -- Get overlapping phrase pairs, and at least one phrase is active in each pair.
      cb = findCombWithLowestPrio onOff cbs cbps    -- Get the combination which has lowest priority among all combinations.

-- Find the categorial combination which has the lowest priority among a combination set formed via overlapping.
-- For every combination, there is a pair of phrase categories, and the two categories must be overlapping.
-- The Overlapping has position's and span's meanings, see description of function 'getOverType'.
-- One thing to understand is that one phrase TRUELY includes another one is not considered overlapping.
-- Another key point is that two overlapping phrases must have at least one as active.
-- (1) A overlapping relation-based set can be formed. For a set of phrases, maybe multiple closures exists.
-- For each closure, there exists one phrase with lowest priority of combination.
-- (2) After removing the priority-lowest phrase, the overlapping closure is built again.
-- Iteratively do (1)(2), until overlapping closure is empty, namely no overlapping phrases.

findCombWithLowestPrio :: OnOff -> [PhraCate] -> [(PhraCate,PhraCate)] -> PhraCate
findCombWithLowestPrio _ _ [] = ((-1,-1),[],-1)
findCombWithLowestPrio onOff trans (x:xs) 
    | pri && pcps2 /= [] = findCombWithLowestPrio onOff trans pcps2
    | pri && pcps2 == [] = cb2
    | not pri && pcps1 /= [] = findCombWithLowestPrio onOff trans pcps1
    | otherwise = cb1
    where
      cb1 = fst x
      cb2 = snd x
      pri = isPrior2 onOff trans cb1 cb2
      pcps1 = [y| y <- xs, (fst y == cb1) || (snd y == cb1)]     -- [(PhraCate,PhraCate)] related with cb1
      pcps2 = [y| y <- xs, (fst y == cb2) || (snd y == cb2)]     -- [(PhraCate,PhraCate)] related with cb2

-- Get all pairs of overlapping phrases. In every pair, at least phrase is active.
-- The first parameter is On/Off string, for turning on/off Np/s-, A/s-, Np/v-, A/v-, Np/a-, P/a-, Ca/a-, Cv/a-, and A/n- rules.

getOverlap :: OnOff -> [PhraCate] -> [(PhraCate, PhraCate)]
getOverlap _ [] = []
getOverlap onOff pcs = [(x,y)| x<-pcs, y<-pcs, (acOfCate x)!!0 || (acOfCate y)!!0, x/=y, pclt x y, getOverType onOff pcs x y /= -1]
                                                -- Using pclt to avoid (a,b) and (b,a) concurrent.

-- Decide whether two phrasal categories are overlapping. If overlapping, give its type.
-- Type 0: st1==st2,sp1==sp2
--          |~~~~~|
--          |~~~~~|
-- Type 1: st1 < st2 && st2 <= (st1 + sp1) && (st1 + sp1) < (st2 + sp2)
--          |~~~~~|
--             |~~~~~|
--         st2 < st1 && st1 <= (st2 + sp2) && (st2 + sp2) < (st1 + sp1)
--             |~~~~~|
--          |~~~~~|
-- Type 2: st1==st2 && st1 + sp1 > st2 + sp2
--          |~~~~~~~~~~|          
--          |~~~~~|     
-- Type 3: st2==st1 && st2 + sp2 > st1 + sp1 
--          |~~~~~| 
--          |~~~~~~~~~~|
-- Type -1: Other situations.
-- Parameter On/Off for turning on/off Np/s-, A/s-, Np/v-, A/v-, Np/a-, P/a-, Ca/a-, Cv/a-, and A/n- rules.

getOverType :: OnOff -> [PhraCate] -> PhraCate -> PhraCate -> Int
getOverType onOff pcs pc1 pc2
    | st1 == st2 && sp1 == sp2 = 0       -- Full overlapping. One phrase has only one combination.
    | (st1 < st2 && st2 <= (st1 + sp1) && (st1 + sp1) < (st2 + sp2))
        || (st2 < st1 && st1 <= (st2 + sp2) && (st2 + sp2) < (st1 + sp1)) = 1            -- Partial overlapping.
    | st1 == st2 && st1 + sp1 > st2 + sp2 && notElem pc1 (findDescen onOff pc2 pcs) = 2  -- Containing
    | st2 == st1 && st2 + sp2 > st1 + sp1 && notElem pc2 (findDescen onOff pc1 pcs) = 3  -- Containing
    | otherwise = -1
    where
    st1 = stOfCate pc1
    sp1 = spOfCate pc1
    st2 = stOfCate pc2
    sp2 = spOfCate pc2

-- Remove a given phrasal category, and active its parents.
-- If the category to be removed is inactive, it combined with aother category to a bigger-phrased category, and the category should be removed too. Recursively, the new bigger-phrased category is removed. Apparently, the final removed category is active.
-- The first parameter is On/Off string, for turning on/off Np/s-, A/s-, Np/v-, A/v-, Np/a-, P/a-, Ca/a-, Cv/a-, and A/n- rules.

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
-- The first parameter is On/Off string, for turning on/off Np/s-, A/s-, Np/v-, A/v-, Np/a-, P/a-, Ca/a-, Cv/a-, and A/n- rules.
-- For examples, "++-+++---" means using Np/s-, A/s-, Np/a-, and P/a- rules, but denying Np/v- and A/n- rules.

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

-- The first input parameter is On/Off string, for turning on/off Np/s-, A/s-, Np/v-, A/v-, Np/a-, P/a-, Ca/a-, Cv/a-, and A/n- rules.
-- For examples, "++-+++---" means using Np/s-, A/s-, Np/a-, and P/a- rules, but denying Np/v- and A/n- rules.

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
-- The first input parameter is On/Off string, for turning on/off Np/s-, A/s-, Np/v-, A/v-, Np/a-, P/a-, Ca/a-, Cv/a-, and A/n- rules.
-- For examples, "++-+++---" means using Np/s-, A/s-, Np/a-, and P/a- rules, but denying Np/v- and A/n- rules.

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
-- The first input parameter is On/Off string, for turning on/off Np/s-, A/s-, Np/v-, A/v-, Np/a-, P/a-, Ca/a-, Cv/a-, and A/n- rules.
-- For examples, "++-+++---" means using Np/s-, A/s-, Np/a-, and P/a- rules, but denying Np/v- and A/n- rules.

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



