-- Copyright (c) 2019-2021 China University of Water Resources and Electric Power,
-- All rights reserved.

module Phrase (
    Start,         -- Int
    Span,          -- Int
    End,           -- Int
    Tag,           -- String
    Seman,         -- String
    PhraStru,      -- String
    Act,           -- Bool
    SecStart,      -- Int
    PhraCate,      -- ((Start, Span), [(Category, Tag, Seman, PhraStru, Act)], SecStart)
    isPhrase,      -- PhraCate -> Bool
    nilPhra,       -- Phracate
    getPhraCateFromString,     -- String -> PhraCate
    phraCateToString,          -- PhraCate -> String
    nPhraCateToString,         -- [PhraCate] -> String
    createPhraCate,            -- Start -> Span -> [(Category,Tag,Seman,PhraStru,Act)] -> SecStart -> PhraCate
    stOfCate,      -- PhraCate -> Start
    spOfCate,      -- PhraCate -> Span
    enOfCate,      -- PhraCate -> End
    ctspaOfCate,               -- PhraCate -> [(Category, Tag, Seman, PhraStru, Act)]
    ctspaOfActCate,            -- PhraCate -> [(Category, Tag, Seman, PhraStru, Act)]
    ctpaOfCate,    -- PhraCate -> [(Category, Tag, PhraStru, Act)]
    ctpOfCate,     -- PhraCate -> [(Category, Tag, PhraStru)]
    tpaOfCate,     -- PhraCate -> [(Tag, PhraStru, Act)]
    ctspOfCate,    -- PhraCate -> [(Category, Tag, Seman, PhraStru)]
    ctspOfActCate, -- PhraCate -> [(Category, Tag, Seman, PhraStru)]
    cspOfCate,     -- PhraCate -> [(Category, Seman, PhraStru)]
    cspOfActCate,  -- PhraCate -> [(Category, Seman, PhraStru)]
    caOfCate,      -- PhraCate -> [Category]
    caOfActCate,   -- PhraCate -> [Category]
    taOfCate,      -- PhraCate -> [Tag]
    taOfActCate,   -- PhraCate -> [Tag]
    seOfCate,      -- PhraCate -> [Seman]
    seOfActCate,   -- PhraCate -> [Seman]
    psOfCate,      -- PhraCate -> [PhraStru]
    psOfActCate,   -- PhraCate -> [PhraStru]
    acOfCate,      -- PhraCate -> [Act]
    acOfActCate,   -- PhraCate -> [Act]
    csOfCate,      -- PhraCate -> [(Category, Seman)]
    csOfActCate,   -- PhraCate -> [(Category, Seman)]
    ssOfCate,      -- PhraCate -> SecStart
    equalPhra,     -- PhraCate -> PhraCate -> Bool
    pcBelong,      -- PhraCate -> PhraCate -> Bool
    pcBelong',     -- PhraCate -> PhraCate -> Bool
    getPhraBySS,   -- (Start, Span) -> [PhraCate] -> [PhraCate]
    getPhraByStart,-- Start -> [PhraCate] -> [PhraCate]
    getPhraBySpan, -- Span -> [PhraCate] -> [PhraCate]
    getPhraByEnd,  -- End -> [PhraCate] -> [PhraCate]
    getNuOfInputCates,    -- [PhraCate] -> Int
    atomizePhraCate,      -- PhraCate -> [PhraCate]
    atomizePhraCateList,  -- [PhraCate] -> [PhraCate]
    deactOnePC,    -- PhraCate -> PhraCate
    actOnePC,      -- PhraCate -> PhraCate
    pclt,          -- PhraCate -> PhraCate -> Bool
    quickSort,     -- [PhraCate] -> [PhraCate]
    divPhraCateBySpan,            -- [PhraCate] -> [[PhraCate]]
    sortPhraCateBySpan,           -- [PhraCate] -> [PhraCate]
    notElem',                     -- PhraCate -> [PhraCate] -> Bool
    equalSortedPhraList,          -- [PhraCate] -> [PhraCate] -> Bool
    ) where

import Data.Tuple
import Category
import Utils

-- The following types are defined for easy reading.

type Start = Int         -- The start position of a phrase (category) in sentences.
type Span = Int          -- The span distance of a phrase (category) in sentences.
type End = Int           -- The end position of a phrase (category) in sentences.
type Tag = String        -- The tag of rule used for creating this category.
type Seman = String      -- The semantic component of this phrasal category.
type PhraStru = String   -- The name of phrasal structure.
type Act = Bool          -- The activity of a category, True for active, and False for inactive.
type SecStart = Int      -- The position of spliting a phrase (category).

{- When combining two phrase categories, there might be more than one rule available,
   so the phrasal category type PhraCate allows more than one category.
-}

type PhraCate = ((Start, Span), [(Category, Tag, Seman, PhraStru, Act)], SecStart)

-- Decide whether a phrase exists. If there includes at least tuple (<cate,tag,seman,phraStru,act>), it is a phrase.
isPhrase :: PhraCate -> Bool
isPhrase pc = length (ctspaOfCate pc) /= 0

-- The null phrasal category does not correspond to a real phrase, just for technical processing.
nilPhra :: PhraCate
nilPhra = ((-1,-1),[],-1)

-- Get a phraCate from a string. The string has format "((start,span),[(cate,tag,sem,phraStru,act)],secStart)".
getPhraCateFromString :: String -> PhraCate
getPhraCateFromString str = ((st,sp),[(cate,tag,sem,phraStru,act)],ss)
    where
    s = splitAtDeli ',' str
    st = read (throwBrac (s!!0)) :: Int
    sp = read (throwBrac (s!!1)) :: Int
    cate = getCateFromString (throwHTSpace (throwFstBrac 2 (s!!2)))
    tag = throwHTSpace (s!!3)
    sem = throwHTSpace (s!!4)
    phraStru = throwHTSpace (s!!5)
    act = read (throwBrac (s!!6)) :: Bool
    ss = read (throwBrac (s!!7)) :: Int

-- Get the string of a phrasal category.
phraCateToString :: PhraCate -> String
phraCateToString pc = "((" ++ show st ++ "," ++ show sp ++ "),[(" ++ show (ca!!0) ++ "," ++ (ta!!0) ++ "," ++ (se!!0) ++ "," ++ (ps!!0) ++ "," ++ show (ac!!0) ++ ")]," ++ show ss ++ ")"
    where
    st = stOfCate pc
    sp = spOfCate pc
    ca = caOfCate pc
    ta = taOfCate pc
    se = seOfCate pc
    ps = psOfCate pc
    ac = acOfCate pc
    ss = ssOfCate pc

-- Get the string of [PhraCate].
nPhraCateToString :: [PhraCate] -> String
nPhraCateToString nPCs = listToString $ map phraCateToString nPCs

-- Create a phrasal category by given member components.
createPhraCate :: Start -> Span -> [(Category,Tag,Seman,PhraStru,Act)] -> SecStart -> PhraCate
createPhraCate start span ctspa secStart
    | ctspa == [] = ((start,span),[],secStart)
    | fst5 (ctspa!!0) == nilCate = ((start,span),[],secStart)
    | otherwise = ((start,span),ctspa,secStart)

-- Functions for extracting a member component of phrasal category.

stOfCate :: PhraCate -> Start
stOfCate (s, _, _) = fst s      -- Start

spOfCate :: PhraCate -> Span
spOfCate (s, _, _) = snd s      -- Span

enOfCate :: PhraCate -> End     -- End
enOfCate (s, _, _) = fst s + (snd s)

ctspaOfCate :: PhraCate -> [(Category, Tag, Seman, PhraStru, Act)]
ctspaOfCate (_, ctspa, _) = ctspa      -- CTSPA

ctspaOfActCate :: PhraCate -> [(Category, Tag, Seman, PhraStru, Act)]
ctspaOfActCate (_, ctspa, _) = [x| x <- ctspa, fif5 x]            -- CTSPA of active phrases

ctpaOfCate :: PhraCate -> [(Category, Tag, PhraStru, Act)]        -- Get syntactic information of a phrase
ctpaOfCate (_, ctspa, _) = map (\x -> (fst5 x, snd5 x, fth5 x, fif5 x)) ctspa

ctpOfCate :: PhraCate -> [(Category, Tag, PhraStru)]              -- Get syntactic information of a phrase
ctpOfCate (_, ctspa, _) = map (\x -> (fst5 x, snd5 x, fth5 x)) ctspa

tpaOfCate :: PhraCate -> [(Tag, PhraStru, Act)]                   -- Get syntactic information of a phrase
tpaOfCate (_, ctspa, _) = map (\x -> (snd5 x, fth5 x, fif5 x)) ctspa

ctspOfCate :: PhraCate -> [(Category, Tag, Seman, PhraStru)]
ctspOfCate (_, ctspa, _) = map (\x -> (fst5 x, snd5 x, thd5 x, fth5 x)) ctspa

ctspOfActCate :: PhraCate -> [(Category, Tag, Seman, PhraStru)]
ctspOfActCate (_, ctspa, _) = map (\x -> (fst5 x, snd5 x, thd5 x, fth5 x)) [y| y <- ctspa, fif5 y]

cspOfCate :: PhraCate -> [(Category, Seman, PhraStru)]
cspOfCate (_, ctspa, _) = map (\x -> (fst5 x, thd5 x, fth5 x)) ctspa

cspOfActCate :: PhraCate -> [(Category, Seman, PhraStru)]
cspOfActCate (_, ctspa, _) = map (\x -> (fst5 x, thd5 x, fth5 x)) [y| y <- ctspa, fif5 y]

caOfCate :: PhraCate -> [Category]
caOfCate pc = [fst5 c | c <- ctspa]
    where
    ctspa = ctspaOfCate pc

caOfActCate :: PhraCate -> [Category]
caOfActCate pc = [fst5 c | c <- ctspa, fif5 c == True]
    where
    ctspa = ctspaOfCate pc

taOfCate :: PhraCate -> [Tag]
taOfCate pc = [snd5 c | c <- ctspa]
    where
    ctspa = ctspaOfCate pc

taOfActCate :: PhraCate -> [Tag]
taOfActCate pc = [snd5 c | c <- ctspa, fif5 c == True]
    where
    ctspa = ctspaOfCate pc

seOfCate :: PhraCate -> [Seman]
seOfCate pc = [thd5 c | c <- ctspa]
    where
    ctspa = ctspaOfCate pc

seOfActCate :: PhraCate -> [Seman]
seOfActCate pc = [thd5 c | c <- ctspa, fif5 c == True]
    where
    ctspa = ctspaOfCate pc

psOfCate :: PhraCate -> [PhraStru]
psOfCate pc = [fth5 c | c <- ctspa]
    where
    ctspa = ctspaOfCate pc

psOfActCate :: PhraCate -> [PhraStru]
psOfActCate pc = [fth5 c | c <- ctspa, fif5 c == True]
    where
    ctspa = ctspaOfCate pc

acOfCate :: PhraCate -> [Act]
acOfCate pc = [fif5 c | c <- ctspa]
    where
    ctspa = ctspaOfCate pc

acOfActCate :: PhraCate -> [Act]
acOfActCate pc = [fif5 c | c <- ctspa, fif5 c == True]
    where
    ctspa = ctspaOfCate pc

csOfCate :: PhraCate -> [(Category, Seman)]
csOfCate pc = zip (caOfCate pc) (seOfCate pc)

csOfActCate :: PhraCate -> [(Category, Seman)]
csOfActCate pc = zip (caOfActCate pc) (seOfActCate pc)

ssOfCate :: PhraCate -> SecStart
ssOfCate (_, _, s) = s

-- Without considering activities, decide two phrasal categories equal or not.
equalPhra :: PhraCate -> PhraCate -> Bool
equalPhra pc1 pc2 = (stOfCate pc1 == stOfCate pc2)
                 && (spOfCate pc1 == spOfCate pc2)
                 && (ctspOfCate pc1 == ctspOfCate pc2)
                 && (ssOfCate pc1 == ssOfCate pc2)

{- Define relation 'belong to' between two phrasal categories. They have same Start, Span, and SecStart,
   except that the component [(Category,Tag,Seman,PhraStru,Act)] of first phrasal category is subset of
   that of the second one.
 -}

pcBelong :: PhraCate -> PhraCate -> Bool
pcBelong x y = (stx == sty) && (spx == spy) && (ssx == ssy) && belong
    where
      stx = stOfCate x
      sty = stOfCate y
      spx = spOfCate x
      spy = spOfCate y
      ssx = ssOfCate x
      ssy = ssOfCate y
      ctspax = ctspaOfCate x
      ctspay = ctspaOfCate y
      belong = foldr (&&) True (map (\x -> elem x ctspay) ctspax)

-- Another version of Function pcBelong which does not consider Act attribute.
pcBelong' :: PhraCate -> PhraCate -> Bool
pcBelong' x y = (stx == sty) && (spx == spy) && (ssx == ssy) && belong
    where
      stx = stOfCate x
      sty = stOfCate y
      spx = spOfCate x
      spy = spOfCate y
      ssx = ssOfCate x
      ssy = ssOfCate y
      ctspx = ctspOfCate x
      ctspy = ctspOfCate y
      belong = foldr (&&) True (map (\x -> elem x ctspy) ctspx)

-- Get all phrases with given position and span among a set of phrases.
getPhraBySS :: (Start, Span) -> [PhraCate] -> [PhraCate]
getPhraBySS (st, sp) pcs = [x|x<-pcs, stOfCate x == st, spOfCate x == sp]

-- Get all phrases with given start position among a set of phrases.
getPhraByStart :: Start -> [PhraCate] -> [PhraCate]
getPhraByStart st pcs = [x|x<-pcs, stOfCate x == st]

-- Get phrases with given span among a set of phrases.
getPhraBySpan :: Span -> [PhraCate] -> [PhraCate]
getPhraBySpan sp pcs = [x|x<-pcs, spOfCate x == sp]

-- Get phrases with given end position among a set of phrases.
getPhraByEnd :: End -> [PhraCate] -> [PhraCate]
getPhraByEnd end pcs = [x|x<-pcs, (stOfCate x) + (spOfCate x) == end]

-- Get the number of input categories which have span 0 from transitive closure of phrase categories.
getNuOfInputCates :: [PhraCate] -> Int
getNuOfInputCates pcClo = length [pc | pc <- pcClo, spOfCate pc == 0]

{- One phrasal category can have more than one tuple (<cate>,<tag>,<seman>,<phraStru>,<act>).
   Function atomizePhraCate unpacks one phrasal category into multiple phrasal categories,
   each of which has only one tuple (<cate>,<tag>,<seman>,<phraStru>,<act>).
 -}
atomizePhraCate :: PhraCate -> [PhraCate]
atomizePhraCate pc = [((st,sp),[ctspa1],ss) | ctspa1 <- ctspa]
    where
    st = stOfCate pc
    sp = spOfCate pc
    ctspa = ctspaOfCate pc
    ss = ssOfCate pc

-- Function atomizePhraCateList is used to atomize a list of phrasal categories.
atomizePhraCateList :: [PhraCate] -> [PhraCate]
atomizePhraCateList [] = []
atomizePhraCateList (x:xs) = atomizePhraCate x ++ (atomizePhraCateList xs)

-- Deactivate an atomic phrasal category.
deactOnePC :: PhraCate -> PhraCate
deactOnePC ((st,sp),[],ss) = ((st,sp),[],ss)
deactOnePC ((st,sp),[x],ss) = ((st,sp),[(fst5 x,snd5 x, thd5 x, fth5 x, False)],ss)
deactOnePC ((_,_),(x:xs),_) = error "Failed to deactivate a non-atomized phrasal category."

-- activate an atomic phrasal category.
actOnePC :: PhraCate -> PhraCate
actOnePC ((st,sp),[],ss) = ((st,sp),[],ss)
actOnePC ((st,sp),[x],ss) = ((st,sp),[(fst5 x,snd5 x, thd5 x, fth5 x, True)],ss)
actOnePC ((_,_),(x:xs),_) = error "Failed to activate a non-atomized phrasal category."

{- Define relation 'less than' between two atomic phrases, so any set of atomic phrases can be linearly ordered.
   The relation is also used to decide overlap type between two atomic phrases.
 -}
pclt :: PhraCate -> PhraCate -> Bool
pclt x y
    | x == nilPhra || y == nilPhra = error "pclt: Null phrasal category"
    | x == y = error $ "pclt: Two phrasal categories are same, i.e. " ++ (show x)
    | otherwise = (stx < sty)
      || (stx == sty)&&(spx < spy)
      || (stx == sty)&&(spx == spy)&&(ssx < ssy)
      || (stx == sty)&&(spx == spy)&&(ssx == ssy)&&(cax < cay)
      || (stx == sty)&&(spx == spy)&&(ssx == ssy)&&(cax == cay)&&(tax < tay)
      || (stx == sty)&&(spx == spy)&&(ssx == ssy)&&(cax == cay)&&(tax == tay)&&(sex < sey)
      || (stx == sty)&&(spx == spy)&&(ssx == ssy)&&(cax == cay)&&(tax == tay)&&(sex == sey)&&(psx < psy)
      || (stx == sty)&&(spx == spy)&&(ssx == ssy)&&(cax == cay)&&(tax == tay)&&(sex == sey)&&(psx == psy)&&(acx < acy)
      where
        stx = stOfCate x
        sty = stOfCate y
        spx = spOfCate x
        spy = spOfCate y
        ssx = ssOfCate x
        ssy = ssOfCate y
        cax = (caOfCate x)!!0
        cay = (caOfCate y)!!0
        tax = (taOfCate x)!!0
        tay = (taOfCate y)!!0
        sex = (seOfCate x)!!0
        sey = (seOfCate y)!!0
        psx = (psOfCate x)!!0
        psy = (psOfCate y)!!0
        acx = (acOfCate x)!!0
        acy = (acOfCate y)!!0

-- Quick sort a list of phrasal categoies. This is a stable sort.
quickSort :: [PhraCate] -> [PhraCate]
quickSort [] = []
quickSort [x] = [x]
quickSort (x:xs) = (quickSort [y|y<-xs, pclt y x]) ++ [x] ++ (quickSort [y|y<-xs, pclt x y])

{- Divide a set of phrases into groups such that phrases in identical group have equivalent span.
   These groups are ordered by increasing spans, and phrases in each group are ordered by relation "less than".
 -}
divPhraCateBySpan :: [PhraCate] -> [[PhraCate]]
divPhraCateBySpan t = map quickSort (map (\sp -> getPhraBySpan sp t) [0..(getNuOfInputCates t - 1)])

-- Sort phrasal categories according phrasal spans.
sortPhraCateBySpan :: [PhraCate] -> [PhraCate]
sortPhraCateBySpan pcClo = [pc| sp <- divPhraCateBySpan pcClo, pc <- sp]

-- Without considering Act attribute, decide whether a phrase is NOT in a certain phrasal list.
notElem' :: PhraCate -> [PhraCate] -> Bool
notElem' x [] = True
notElem' x (y:ys)
    | (stx==sty)&&(spx==spy)&&(ssx==ssy)&&(ctspx==ctspy) = False
    | otherwise = notElem' x ys
    where
      stx = stOfCate x
      spx = spOfCate x
      ssx = ssOfCate x
      ctspx = ctspOfCate x
      sty = stOfCate y
      spy = spOfCate y
      ssy = ssOfCate y
      ctspy = ctspOfCate y

-- Compare two lists sorted by function quickSort, return True when they have same set of phrasal categories.
equalSortedPhraList :: [PhraCate] -> [PhraCate] -> Bool
equalSortedPhraList pc1 pc2
    | pc1 == [] && pc2 == [] = True
    | (pc1 /= [] && pc2 == []) || (pc1 == [] && pc2 /= []) = False
    | head pc1 /= head pc2 = False
    | otherwise = equalSortedPhraList (tail pc1) (tail pc2)
