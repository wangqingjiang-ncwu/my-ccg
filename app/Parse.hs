module Parse (
    Start,         -- Int
    Span,          -- Int
    SecStart,      -- Int
    PhraCate,      -- ((Start, Span), Category a, SecStart)
    pclt,           -- PhraCate a -> PhraCate a -> Bool
    stOfCate,      -- PhraCate a -> Start
    spOfCate,      -- PhraCate a -> Span
    caOfCate,      -- PhraCate a -> [Category a]
    ssOfCate,      -- PhraCate a -> SecStart
    cateComb,      -- PhraCate a -> PhraCate a -> [PhraCate a]
    initPhraCate,  -- [Category a] -> [PhraCate a]
    parse,         -- [PhraCate a] -> [PhraCate a]
    getNuOfInputCates,       -- [PhraCate a] -> Int 
    growForest,              -- [[PhraCate a]] -> [PhraCate a] -> [[PhraCate a]]
    findCate,                -- (Int, Int) -> [PhraCate a] -> [PhraCate a]
    findSplitCate,           -- PhraCate a -> [PhraCate a] -> [PhraCate a]
    findTipsOfTree,          -- [PhraCate a] -> [PhraCate a] -> [PhraCate a]
    findCateBySpan,          -- Int -> [PhraCate a] -> [PhraCate a]
    divPhraCateBySpan,       -- [PhraCate a] -> [[PhraCate a]]
    quickSort                -- [PhraCate a] -> [PhraCate a]
    ) where

import Category
import Rule

type Start = Int   -- The start position of a phrase (category) in sentences.
type Span = Int    -- The span distance of a phrase (category) in sentences.
type SecStart = Int      -- The position of spliting a phrase (category).

-- When combining two phrase categories, there might be more than one rule available, resulting in multiple categories (Usually the resultant categories are same).

type PhraCate a = ((Start, Span), [Category a], SecStart)

-- Define relation 'less than' for two phrasal categories.
pclt :: PhraCate a -> PhraCate a -> Bool
pclt x y = (stx < sty) || ((stx == sty) && (spx < spy))
    where
    stx = stOfCate x
    sty = stOfCate y
    spx = spOfCate x
    spy = spOfCate y

-- The following functions are used to select an element from tuple PhraCate.
stOfCate :: PhraCate a -> Start
stOfCate (s, _, _) = fst s

spOfCate :: PhraCate a -> Span
spOfCate (s, _, _) = snd s

caOfCate :: PhraCate a -> [Category a]
caOfCate (_, c, _) = c

ssOfCate :: PhraCate a -> SecStart
ssOfCate (_, _, s) = s

--- Function cateComb combines two input (phrasal) categories into one result.
-- The two input categories satisfies concatenative requirements, and may have multiple resultant categories when multiple rules are available.
-- Here phrase's categories are still modelled by list, actually there usually is only one category.
-- Results ((-1,-1),[],-1) and ((x,y),[],z) respectively denote concatenative failure and no rule available.

cateComb :: PhraCate a -> PhraCate a -> PhraCate a
cateComb pc1 pc2
    | st1 + sp1 + 1 /= st2 = ((-1,-1),[],-1)
    | otherwise = ((st1, sp1 + sp2 + 1), rcs, st2)
        where
        st1 = stOfCate pc1
        sp1 = spOfCate pc1
        st2 = stOfCate pc2
        sp2 = spOfCate pc2
        cates = [rule cate1 cate2 | rule <- rules, cate1 <- caOfCate pc1, cate2 <- caOfCate pc2]
        rcs = [rc | rc <- cates, rc /= nilCate]

-- Context-based category conversion might be human brain's mechanism for syntax parsing, similiar to Chinese phrase-centric syntactic view. In the past, a phrase usually has a sequence of categories with the first as its classical category followed by non-classical ones.
-- To suitable for two kinds of category views, Phrase category is defined as a triple, including start position, span, and a categorial list, although there is only one category in the list under category conversion.
-- To remember the parent categories, the start position of the second category is recorded. For initial word categories, their parents don't exist.

-- Words are considered as minimal phrases, thus an universal phrase category models both words and phrases.
-- Initialize the values of PhraCate for each word of a given sentence.
initPhraCate :: [Category a] -> [PhraCate a]
initPhraCate [] = []
initPhraCate [c] = [((0,0),[c],0)]          -- categories start at index 0
initPhraCate (c:cs) = [((0,0),[c],0)] ++ [(((stOfCate pc)+1, 0), caOfCate pc, (stOfCate pc)+1) | pc <- (initPhraCate cs)]

-- Find the number of input categories from the closure of phrase categories.

getNuOfInputCates :: [PhraCate a] -> Int
getNuOfInputCates phraCateClosure = length [pc | pc <- phraCateClosure, spOfCate pc == 0]

-- Parsing a sequence of categories is actually to generate the clousure of phrase categories with categorial combinations.

parse :: [PhraCate a] -> [PhraCate a]
parse phraCateInput
    | phraCateInput == transition = phraCateInput
    | otherwise = parse transition
        where
        combs = [cateComb pc1 pc2 | pc1 <- phraCateInput, pc2 <- phraCateInput, pc1 /= pc2]
        cbs = [cb | cb <- combs, cb /= ((-1,-1),[],-1), elem cb phraCateInput == False]
        transition = phraCateInput ++ cbs

-- Merge all possible splits into the list [SecStart] for every (Start, Span).
-- This function is obsolete. For identical (Start, Span), merging categories and merging SecStart's are terrible opertions because of losing relations between every resultant category and its parents, just like a room with many children and an another room with many pairs of parents.
--mergeSplit :: [PhraCate a] -> [PhraCate a]
--mergeSplit [] = []
--mergeSplit [x] = [x]
--mergeSplit (x:xs)
--    | y == ((-1-1,)[],[]) = x:(mergeSplit xs)
--    | otherwise = ((stx, spx), cax ++ [c | c <- caOfCate y, elem c cax == False], ssx ++ [ss | ss <- ssOfCate y, elem ss ssx == False]):(mergeSplit xs)
--        where
--        stx = stOfCate x
--        spx = spOfCate x
--        y = findCate (stx, spx) xs
--        cax = caOfCate x
--        ssx = ssOfCate x

-- Generate syntactic trees (forest) from the closure of phrase categories.
-- Here is a recursived forest-growing algorithm: For the input forest, 
-- (1) If it is an empty forest without any tree, an empty forest is returned;
-- (2) Otherwise, one forest is created for every tree in input forest, return the union of all created forests.
 
growForest :: [[PhraCate a]] -> [PhraCate a] -> [[PhraCate a]]
growForest [] _ = []                    -- Empty forest
growForest (t:ts) phraCateClosure       -- nonempty forest
    = (growTree t phraCateClosure) ++ (growForest ts phraCateClosure) 

-- One tree can grow at all tips to send forth a layer of leaves. Every parsing tree in Combinatory Categorial Grammar is one binary tree, namely from every tip, only two leaces can grow out. It makes growing process complicated that there might be more than one pair of leaves to grow out for every tip, Selecting different pairs would create different trees.
-- The forest growing from a tree is done by the following:
-- (1) Find all tips able to send forth leaves (The tips for initial categoies are no longer to grow);
-- (2) For every such tip, find all splits (namely all pairs of leaves), and create a forest, of which every tree include the input tree and a distinc pair of leaves;
-- (3) Based on merging two forests, all forests are mering into one forest. 
growTree :: [PhraCate a] -> [PhraCate a] -> [[PhraCate a]]
growTree t phraCateClosure
    | findTipsOfTree t phraCateClosure == [] = [t]      -- No tip can grow.
    | [t] == gf = [t]                                   -- Not grow.
    | otherwise = growForest gf phraCateClosure
        where
        splOfAllTips = [findSplitCate tip phraCateClosure | tip <- (findTipsOfTree t phraCateClosure)]   -- [[(PhraCate a, PhraCate a)]]
        forestByTipGrow = [map (\x -> [fst x | elem (fst x) t == False] ++ [snd x | elem (snd x) t == False] ++ t ) splOfATip | splOfATip <- splOfAllTips]
        gf = uniForest forestByTipGrow

-- By growing at each tip, a tree grows and might become multiple trees because more than one split exists. 
-- Suppose tree t becomes ti = [ti1,ti2,...tin] by growing at No.i tip, and tj = [tj1,tj2,...tjm] by growing at No.j tip. Both the two forests are from the same tree t, and should merge into forest tk, tk = ti X tj. Merging tix and tjy, x<-[1..n], y<-[1..m], is actually to do an union operation on two sets.

uniForest :: [[[PhraCate a]]] -> [[PhraCate a]]
uniForest [] = []                -- No forest
uniForest [f] = f                -- Just one forest
uniForest (f:fs)                 -- At least two forests
    = uniForest ((uniTwoForest f (head fs)):(tail fs)) 

-- Merging two forest.
uniTwoForest :: [[PhraCate a]] -> [[PhraCate a]] -> [[PhraCate a]]
uniTwoForest f1 f2 = [uniTwoTree t1 t2 | t1<-f1, t2<-f2]

-- Merging two trees.
uniTwoTree :: [PhraCate a] -> [PhraCate a] -> [PhraCate a]
uniTwoTree t1 t2 = t1 ++ [x | x<-t2, elem x t1 /= True]

-- Find a phrase category by its (Start, Span). If does not, return []. 
findCate :: (Start, Span) -> [PhraCate a] -> [PhraCate a]
findCate (_, -1) _ = []         -- No categoy has span -1, used for find parents.
findCate (st, sp) [] = []
findCate (st, sp) [x]
    | st == stOfCate x && sp == spOfCate x = [x]
    | otherwise = []
findCate (st, sp) (x:xs)
    | st == stOfCate x && sp == spOfCate x = x:(findCate (st, sp) xs)
    | otherwise = findCate (st, sp) xs

-- Find splited (namely parent) categories for a given phrase category from the closure of phrase categories.
findSplitCate :: PhraCate a -> [PhraCate a] -> [(PhraCate a, PhraCate a)]
findSplitCate pc phraCateClosure
    = zip (findCate (st1, sp1) phraCateClosure) (findCate (st2, sp2) phraCateClosure)
        where
        st1 = stOfCate pc
        st2 = ssOfCate pc
        sp1 = st2 - st1 - 1
        sp2 = spOfCate pc - sp1 - 1
 
-- Find all growable tips of a tree from the closure of phrase categories.
-- Those tips corresponding to initial categories are not growable.

findTipsOfTree :: [PhraCate a] -> [PhraCate a] -> [PhraCate a]
findTipsOfTree [] _ = []
findTipsOfTree [x] phraCateClosure
    | findSplitCate x phraCateClosure /= [] = [x]
    | otherwise = []
findTipsOfTree (x:xs) phraCateClosure
    | findSplitCate x phraCateClosure /= [] = x:(findTipsOfTree xs phraCateClosure)
    | otherwise = findTipsOfTree xs phraCateClosure

-- Find phrase categories with given span.
findCateBySpan :: Int -> [PhraCate a] -> [PhraCate a]
findCateBySpan sp  pcs = [x|x<-pcs, spOfCate x == sp]

-- Phrasal categories in a tree are divided into some lists, each of which includes 
-- those categories with same span, and every such list is arranged in ascending 
-- order on values of element Start, while these lists is arranged from 0 to 
-- (getNuOfInputCates - 1).

divPhraCateBySpan :: [PhraCate a] -> [[PhraCate a]]
divPhraCateBySpan t = map quickSort (map (\sp -> findCateBySpan sp t) [0..(getNuOfInputCates t - 1)])

-- Quick sort a list of phrasal categoies.
quickSort :: [PhraCate a] -> [PhraCate a]
quickSort [] = []
quickSort [x] = [x]
quickSort (x:xs) = (quickSort [y|y<-xs, pclt y x]) ++ [x] ++ (quickSort [y|y<-xs, pclt x y])

    
        
        
    


