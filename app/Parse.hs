-- Copyright (c) 2019-2021 China University of Water Resources and Electric Power,
-- All rights reserved.

module Parse (
    cateComb,          -- OnOff -> PhraCate -> PhraCate -> PhraCate
    initPhraCate,      -- [(Category, Seman)] -> [PhraCate]
    trans,             -- OnOff -> [PhraCate] -> [PhraCate]
    transWithPruning,  -- OnOff -> [PhraCate] -> [PhraCate]
--  parse,             -- OnOff -> [PhraCate] -> [PhraCate] -> [PhraCate]
    prune,             -- [PhraCate] -> [PhraCate]
--  prune',            -- [PhraCate] -> [PhraCate] -> IO ([PhraCate],[PhraCate])
    getOverlap,        -- [PhraCate] -> [(PhraCate,PhraCate)]
    findPhraWithLowestPrio,  -- [PhraCate] -> [(PhraCate,PhraCate)] -> PhraCate
    getPrior,          -- [PhraCate] -> PhraCate -> PhraCate -> IO Prior
    getOverType,       -- [PhraCate] -> PhraCate -> PhraCate -> Int
    removeOnePC,       -- PhraCate -> [PhraCate] -> [PhraCate]
    updateAct,         -- [PhraCate] -> [PhraCate]
    findSplitCate,     -- PhraCate -> [PhraCate] -> [(PhraCate,PhraCate)]
    findDescen,        -- PhraCate -> [PhraCate] -> [PhraCate]
    growForest,        -- OnOff -> [[PhraCate]] -> [PhraCate] -> [[PhraCate]]
    growTree,          -- OnOff -> [PhraCate] -> [PhraCate] -> [[PhraCate]]
    findTipsOfTree,    -- OnOff -> [PhraCate] -> [PhraCate] -> [PhraCate]
    uniForest,         -- [[[PhraCate]]] -> [[PhraCate]]
    uniTwoForest,      -- [[PhraCate]] -> [[PhraCate]] -> [[PhraCate]]
    uniTwoTree         -- [PhraCate] -> [PhraCate] -> [PhraCate]
    ) where

import Control.Monad
import Data.Tuple
import Data.Tuple.Utils
import Data.List
import Category
import Phrase
import Rule
import Corpus (OverPair,Prior(..))
import Utils

{- Function cateComb combines two input (phrasal) categories into resultant one.
   The two input categories satisfies concatenative requirements, namely <st1> + <sp2> + 1 = <st2>.
   When multiple rules are available, there might be more than one resultant category.
   Introduing category-converting rules makes this phenomenon more serious.
   Results ((-1,-1),[],-1) and ((x,y),[],z) respectively denote concatenative failure and no rule available.
 -}

cateComb :: OnOff -> PhraCate -> PhraCate -> PhraCate
cateComb onOff pc1 pc2
    | st1 + sp1 + 1 /= st2 = nilPhra                         -- nilPhra ::= ((-1,-1),[],-1)
    | otherwise = ((st1, sp1 + sp2 + 1), rcs, st2)
    where
      st1 = stOfCate pc1      -- Start position of pc1
      sp1 = spOfCate pc1      -- Span of pc1
      st2 = stOfCate pc2      -- Start position of pc2
      sp2 = spOfCate pc2      -- Span of pc2
      csp1 = cspOfCate pc1    -- [(Category, Seman, PhraStru)], rule input
      csp2 = cspOfCate pc2    -- [(Category, Seman, PhraStru)], rule input

-- Output of CCG standard rules, namely [(<Category>,<Tag>,<Seman>,<PhraStru>,<Act>)]
      catesBasic = [rule cate1 cate2 | rule <- rules, cate1 <- csp1, cate2 <- csp2]

{- Context-based category conversion might be human brain's mechanism for syntax parsing, similiar to Chinese phrase-
   centric syntactic view. By converting sentence into nominal phrase, then using standard CCG rules, some categories can be gotten. For each result (<category>, <tag>, <seman>, <cn>, <act>), the <tag> is changed as "S/s-"++<tag> or "O/s-"++<tag> to remember the category conversion s->np which happens before using the standard rule <tag>.
   Actually, category "s" appearing amid a sentence means it relates to a clause (i.e. a Chinese clause). Converting clause to adjective phrase implies category conversion s->np/.np, noted as "A/s-". Rule S/s, O/s, and A/s are always used together with a certain standard rule to implement two-category combination.
 -}
      s_S = removeDup [(npCate, snd3 csp, thd3 csp) | csp <- csp1, fst3 csp == sCate]
      ctspaBysToS = [rule cate1 cate2 | rule <- [appB,raiFh], cate1 <- s_S, cate2 <- csp2, elem Ss onOff]
      catesBysToS = [(fst5 cate, "S/s-" ++ snd5 cate, thd5 cate, fth5 cate, fif5 cate) | cate <- ctspaBysToS]

      s_O = removeDup [(npCate, snd3 csp, thd3 csp) | csp <- csp2, fst3 csp == sCate]
      ctspaBysToO = [rule cate1 cate2 | rule <- [appF], cate1 <- csp1, cate2 <- s_O, elem Os onOff]
      catesBysToO = [(fst5 cate, "O/s-" ++ snd5 cate, thd5 cate, fth5 cate, fif5 cate) | cate <- ctspaBysToO]

      s_A = removeDup [(adjCate, snd3 csp, thd3 csp) | csp <- csp1, fst3 csp == sCate]
      ctspaBysToA = [rule cate1 cate2 | rule <- [appF], cate1 <- s_A, cate2 <- csp2, fst3 cate2 == npCate, elem As onOff] ++ [rule cate1 cate2 | rule <- [appB], cate1 <- s_A, cate2 <- csp2, fst3 cate2 == aux1Cate, elem As onOff]
      catesBysToA = [(fst5 cate, "A/s-" ++ snd5 cate, thd5 cate, fth5 cate, fif5 cate) | cate <- ctspaBysToA]

{- According to Jia-xuan Shen's theory, successive inclusions from noun to verb, and to adjective, the conversion
   from s\.np, (s\.np)/.np, ((s\.np)/.np)/.np, or np/.np to np is allowed, also is from np/.np to s\.np, noted as
   S/v, O/v, S/a, O/a, Hn/a, and P/a respectively. The conversion from np to np/.np is allowed, noted as A/n. Besides, two adjacent categories can convert to their new categories respectively and simultaneously, such as "np np/.np -> np/.np np" noted as A/n-Hn/a. When used with some standard rules, two-category combination is labelled as "S/v-"++<tag>, "O/v-"++<tag>, "S/a-"++<tag>, "O/a-"++<tag>, "P/a-"++<tag>, "Hn/a-"++<tag>, "A/n-"++<tag>, and "A/n-Hn/a-"++<tag> . Now, category conversions only happen in acting as particular syntactic constituents.
 -}

 -- The conversion from verb to noun happens when verbal phrase occupies subject or object position.
      v_S = removeDup [(npCate, snd3 csp, thd3 csp)|csp <- csp1, elem True (map (\x-> cateEqual x (fst3 csp)) vCate)]
          where
          vCate = [predCate, verbCate, verbCate2]
      ctspaByvToS = [rule cate1 cate2 | rule <- [appB,raiFh], cate1 <- v_S, cate2 <- csp_2, elem Sv onOff]
          where
          vCate = [predCate, verbCate, verbCate2]                                     -- Need a subject
          csp_2 = removeDup [x| x <- csp2, elem True (map (\y-> cateEqual y (fst3 x)) vCate)]
      catesByvToS = [(fst5 cate, "S/v-" ++ snd5 cate, thd5 cate, fth5 cate, fif5 cate) | cate <- ctspaByvToS]

      v_O = removeDup [(npCate, snd3 csp, thd3 csp)|csp <- csp2, elem True (map (\x-> cateEqual x (fst3 csp)) vCate)]
          where
          vCate = [predCate, verbCate, verbCate2]
      ctspaByvToO = [rule cate1 cate2 | rule <- [appF], cate1 <- csp_1, cate2 <- v_O, elem Ov onOff]
          where
          vpCate = [verbCate, verbCate2, prep2AdvCate, prep2CompCate]                 -- Need an object
          csp_1 = removeDup [x| x <- csp1, elem True (map (\y-> cateEqual y (fst3 x)) vpCate)]
      catesByvToO = [(fst5 cate, "O/v-" ++ snd5 cate, thd5 cate, fth5 cate, fif5 cate) | cate <- ctspaByvToO]

{- The conversion from intransitive verb to adjective happens when the verb occupies attribute position or be next to
   No.1 auxiliary word of the right.
 -}
      v_A = removeDup [(adjCate, snd3 csp, thd3 csp) | csp <- csp1, fst3 csp == predCate] -- Only consider preCate
      ctspaByvToA = [rule cate1 cate2 | rule <- [appF], cate1 <- v_A, cate2 <- csp2, fst3 cate2 == npCate, elem Av onOff] ++ [rule cate1 cate2 | rule <- [appB], cate1 <- v_A, cate2 <- csp2, fst3 cate2 == aux1Cate, elem Av onOff]
      catesByvToA = [(fst5 cate, "A/v-" ++ snd5 cate, thd5 cate, fth5 cate, fif5 cate) | cate <- ctspaByvToA]

{- The conversion from verb to noun happens when the verb occupies nominal head word position of structure AHn, HnC
   and auxiliary '的'.
 -}
      v_Hn_A = removeDup [(npCate, snd3 csp, thd3 csp) | csp <- csp2, elem True (map (\x-> cateEqual x (fst3 csp)) vCate)]
          where
          vCate = [predCate, verbCate, verbCate2]
      ctspaByvToHn_A = [rule cate1 cate2 | rule <- [appF], cate1 <- csp1, cate2 <- v_Hn_A, fst3 cate1 == adjCate, elem Hnv onOff]

      v_Hn_C = removeDup [(npCate, snd3 csp, thd3 csp) | csp <- csp1, elem True (map (\x-> cateEqual x (fst3 csp)) vCate)]
          where
          vCate = [predCate, verbCate, verbCate2]
      ctspaByvToHn_C = [rule cate1 cate2 | rule <- [appB], cate1 <- v_Hn_C, cate2 <- csp2, fst3 cate2 == ndCate, elem Hnv onOff]

      v_Hn_u1 = removeDup [(npCate, snd3 csp, thd3 csp) | csp <- csp1, elem True (map (\x-> cateEqual x (fst3 csp)) vCate)]
          where
          vCate = [predCate, verbCate, verbCate2]
      ctspaByvToHn_u1 = [rule cate1 cate2 | rule <- [appB], cate1 <- v_Hn_u1, cate2 <- csp2, cateEqual (fst3 cate2) aux1Cate, elem Hnv onOff]

      ctspaByvToHn = ctspaByvToHn_A ++ ctspaByvToHn_C ++ ctspaByvToHn_u1
      catesByvToHn = [(fst5 cate, "Hn/v-" ++ snd5 cate, thd5 cate, fth5 cate, fif5 cate) | cate <- ctspaByvToHn]

-- The conversion from verb to adverb happens when the verb occupies adverbial position.
      v_D = removeDup [(adverbalCate, snd3 csp, thd3 csp) | csp <- csp1, elem True (map (\x-> cateEqual x (fst3 csp)) vCate)]
          where
          vCate = [predCate, verbCate, verbCate2]
      ctspaByvToD = [rule cate1 cate2 | rule <- [appF,comFh], cate1 <- v_D, cate2 <- csp_2, elem Dv onOff]
          where
          vCate = [predCate, verbCate, verbCate2]
          csp_2 = removeDup [x| x <- csp2, elem True (map (\y-> cateEqual y (fst3 x)) vCate)]
      catesByvToD = [(fst5 cate, "D/v-" ++ snd5 cate, thd5 cate, fth5 cate, fif5 cate) | cate <- ctspaByvToD]

{- The conversion from adjective to noun happens when the adjective occupies subject, object, AHn's headword position, or followed by auxiliary '的'.
 -}
      a_S = removeDup [(npCate, snd3 csp, thd3 csp) | csp <- csp1, cateEqual (fst3 csp) adjCate]
      ctspaByaToS = [rule cate1 cate2 | rule <- [appB, raiFh], cate1 <- a_S, cate2 <- csp_2, elem Sa onOff]
          where
          vCate = [predCate, verbCate, verbCate2]
          csp_2 = removeDup [x| x <- csp2, elem True (map (\y-> cateEqual y (fst3 x)) vCate)]
      catesByaToS = [(fst5 cate, "S/a-" ++ snd5 cate, thd5 cate, fth5 cate, fif5 cate) | cate <- ctspaByaToS]

      a_O = removeDup [(npCate, snd3 csp, thd3 csp) | csp <- csp2, cateEqual (fst3 csp) adjCate]
      ctspaByaToO = [rule cate1 cate2 | rule <- [appF], cate1 <- csp_1, cate2 <- a_O, elem Oa onOff]
          where
          vCate = [verbCate, verbCate2]
          csp_1 = removeDup [x| x <- csp1, elem True (map (\y-> cateEqual y (fst3 x)) vCate)]
      catesByaToO = [(fst5 cate, "O/a-" ++ snd5 cate, thd5 cate, fth5 cate, fif5 cate) | cate <- ctspaByaToO]

      a_Hn = removeDup [(npCate, snd3 csp, thd3 csp) | csp <- csp2, cateEqual (fst3 csp) adjCate]
      ctspaByaToHn = [rule cate1 cate2 | rule <- [appF], cate1 <- csp_1, cate2 <- a_Hn, elem Hna onOff]
          where
          csp_1 = removeDup [x| x<- csp1, fst3 x == adjCate]
      catesByaToHn = [(fst5 cate, "Hn/a-" ++ snd5 cate, thd5 cate, fth5 cate, fif5 cate) | cate <- ctspaByaToHn]

-- The conversion from adjective to predicate verb happens when the adjective occupies predicate position.
      a_P = removeDup [(predCate, snd3 csp, thd3 csp) | csp <- csp2, cateEqual (fst3 csp) adjCate]
      ctspaByaToP = [rule cate1 cate2 | rule <- [appB], cate1 <- csp_1, cate2 <- a_P, elem Pa onOff]
          where
          csp_1 = removeDup [x| x<- csp1, fst3 x == npCate]
      catesByaToP = [(fst5 cate, "P/a-" ++ snd5 cate, thd5 cate, fth5 cate, fif5 cate) | cate <- ctspaByaToP]

-- The category conversion from np/.np to (s\.np)\x(s\.np) happens when the adjective occupies completment position.
      a_Cv = removeDup [(adj2VerbCompCate, snd3 csp, thd3 csp) | csp <- csp2, cateEqual (fst3 csp) adjCate]
      ctspaByaToCv = [rule cate1 cate2 | rule <- [appB,comBc], cate1 <- csp_1, cate2 <- a_Cv, elem Cva onOff]
          where
          vCate = [predCate, verbCate, verbCate2]
          csp_1 = removeDup [x| x<- csp1, elem True (map (\y-> cateEqual y (fst3 x)) vCate)]
      catesByaToCv = [(fst5 cate, "Cv/a-" ++ snd5 cate, thd5 cate, fth5 cate, fif5 cate) | cate <- ctspaByaToCv]

-- The category conversion from np/.np to np\*np happens when a (measured) numeral occupies completment position.
      a_Cn = removeDup [(adj2NounCompCate, snd3 csp, thd3 csp) | csp <- csp2, cateEqual (fst3 csp) adjCate]
      ctspaByaToCn = [rule cate1 cate2 | rule <- [appB], cate1 <- csp_1, cate2 <- a_Cn, elem Cna onOff]
          where
          csp_1 = removeDup [x| x<- csp1, fst3 x == npCate]
      catesByaToCn = [(fst5 cate, "Cn/a-" ++ snd5 cate, thd5 cate, fth5 cate, fif5 cate) | cate <- ctspaByaToCn]

-- The conversion from noun to adjective is ONLY allowed when nouns act as attribute or are followed by auxiliary '的'.
      n_A = removeDup [(adjCate, snd3 csp, thd3 csp) | csp <- csp1, (fst3 csp) == npCate]
      ctspaBynToA = [rule cate1 cate2 | rule <- [appF, appB], cate1 <- n_A, cate2 <- csp_2, elem An onOff]
          where
          csp_2 = removeDup [x| x<- csp2, elem True (map (\y-> cateEqual y (fst3 x)) [npCate, aux1Cate])]
      catesBynToA = [(fst5 cate, "A/n-" ++ snd5 cate, thd5 cate, fth5 cate, fif5 cate) | cate <- ctspaBynToA]

{- The two adjacent categories "np np/.np" convert to "np/.np np", forming structure AHn, here noun-to-adjective and
   adjective-to-noun conversions happen simultaneously.
 -}
      n_A' = removeDup [(adjCate, snd3 csp, thd3 csp) | csp <- csp1, cateEqual (fst3 csp) npCate]
      a_Hn' = removeDup [(npCate, snd3 csp, thd3 csp) | csp <- csp2, cateEqual (fst3 csp) adjCate]
      ctspaBynToA_aToHn = [rule cate1 cate2 | rule <- [appF], cate1 <- n_A', cate2 <- a_Hn', elem An onOff, elem Hna onOff]
          where
          csp_1 = removeDup [x| x<- csp1, fst3 x == npCate]
          csp_2 = removeDup [x| x<- csp2, fst3 x == adjCate]
      catesBynToA_aToHn = [(fst5 cate, "A/n-Hn/a-" ++ snd5 cate, thd5 cate, fth5 cate, fif5 cate) | cate <- ctspaBynToA_aToHn]

{- The two adjacent categoies "np <verb>" convert to "np/.np np", forming structure AHn, here noun-to-adjective and
   verb-to-noun conversions happen simultaneously.
 -}
      n_A'' = removeDup [(adjCate, snd3 csp, thd3 csp) | csp <- csp1, (fst3 csp) == npCate]
      v_Hn'' = removeDup [(npCate, snd3 csp, thd3 csp) | csp <- csp2, elem True (map (\x-> cateEqual x (fst3 csp)) vCate)]
          where
          vCate = [predCate, verbCate, verbCate2]
      ctspaBynToA_vToHn = [rule cate1 cate2 | rule <- [appF], cate1 <- n_A'', cate2 <- v_Hn'', elem An onOff, elem Hnv onOff]
      catesBynToA_vToHn = [(fst5 cate, "A/n-Hn/v-" ++ snd5 cate, thd5 cate, fth5 cate, fif5 cate) | cate <- ctspaBynToA_vToHn]

-- The categories gotten by all rules.
      cates = catesBasic ++ catesBysToS ++ catesBysToO ++ catesBysToA ++ catesByvToS ++ catesByvToO ++ catesByvToA ++ catesByvToHn ++ catesByvToD ++ catesByaToS ++ catesByaToO ++ catesByaToHn ++ catesByaToP ++ catesByaToCv ++ catesByaToCn ++ catesBynToA ++ catesBynToA_aToHn ++ catesBynToA_vToHn

    -- Remove Nil's resultant cateories, NR phrase-structural categories, and duplicate ones.
      rcs = removeDup [rc | rc <- cates, fst5 rc /= nilCate, fth5 rc /= "NR"]

{- Words are considered as phrases with span 0. Word category and semantics are designated by manual input, so word
   tag is "Desig". Besides, words all have phrasal structure "DE", and are all active.
 -}
initPhraCate :: [(Category, Seman)] -> [PhraCate]
initPhraCate [] = []
initPhraCate [c] = [((0,0),[(fst c, "Desig", snd c, "DE", True)],0)]     -- Categories start at index 0
initPhraCate (c:cs) = [((0,0),[(fst c,"Desig",snd c, "DE", True)],0)] ++ [(((stOfCate pc)+1, 0), ctspaOfCate pc, (stOfCate pc)+1) | pc <- (initPhraCate cs)]

{- One trip of transition without further pruning, but based on the result of transition with pruning before, so only
   when every pair of phrases have at least one phrase active, they can combine together. Meanwhile, phrases created
   in this trip of transition will be thrown away if they are phrases pruned in previous transitions.
 -}
trans :: OnOff -> [PhraCate] -> [PhraCate] -> [PhraCate]
trans onOff pcs banPCs = pcs2
    where
      combs = removeDup $ atomizePhraCateList [cateComb onOff pc1 pc2 | pc1 <- pcs, pc2 <- pcs, stOfCate pc1 + spOfCate pc1 + 1 == stOfCate pc2, (acOfCate pc1)!!0 || (acOfCate pc2)!!0]
      newCbs = [cb| cb <- combs, ctspaOfCate cb /= [], notElem' cb banPCs]   -- Not consider phrasal activity
      pcs2 = updateAct $ pcs ++ newCbs

{- One trip of transition with pruning. Some new phrases removed timely are placed into banned phrasal list, and some
   new structural genes are added into the list of structural genes.
 -}
transWithPruning :: [Rule] -> [PhraCate] -> [PhraCate] -> [OverPair] -> IO ([PhraCate],[PhraCate])
transWithPruning onOff pcs banPCs overPairs = do
    let combs = removeDup $ atomizePhraCateList [cateComb onOff pc1 pc2 | pc1 <- pcs, pc2 <- pcs, stOfCate pc1 + spOfCate pc1 + 1 == stOfCate pc2, (acOfCate pc1)!!0 || (acOfCate pc2)!!0]
    let newCbs = [cb| cb <- combs, ctspaOfCate cb /= [], notElem' cb banPCs]    -- Not consider phrasal activity
    let pcs1 = updateAct $ pcs ++ newCbs                  -- Before pruning
    pcs2 <- prune overPairs pcs1                          -- After pruning, Attr. activity is corrected.
    let banPCs2 = banPCs ++ [cb| cb <- pcs1, notElem' cb pcs2]     -- Update the list of banned phrasal categories.
    return (pcs2, banPCs2)

{- Parsing a sequence of categories is actually to generate the category closure from the initial phrase categories.
   Originally designed in every transition, every two phrases are tested whether they can be combined into a new
   phrase under the CCG for Chinese. In the later experiments, an explosive increase of phrase number is always
   observed, sometimes the transitive closure can not be obtained after limited time. So, a pruning process is
   introduced in every trip of transition, and the pruned phrases are placed in list 'banPCs'. Apparently, the removed   phrases should be not generated again.
   From the scratch, words are all active. In later every transition, two adjacent active phrases can be combined into   a new phrase, while one active phrase and one inactive phrase can also be combined. Once participating in phrase
   building, no matter a word or a phrase will become inactive. As a result, only words or phrases without descendants   are active. By the way, if an inactive phrase take part in combination with one active phrase, the descendants of
   the inactive phrase will be removed out and thrown into list 'banPCs'. Both an active and an inactive phrase can be   removed later, owing that they overlap some phrases with higher priorities. An inactive phrase can become active
   again after its child phrase is removed. Only allowing two active phrases to combine implies all inactive phrases
   are in final parsing tree, but it is not true.
   Every transition consists of two steps, the first step is to generate new phrases, and the second step is to
   remove redundant phrases called pruning. In the two steps, there exists the at-least-one-active phenomenon. That
   is, For the first step, when two phrases are to be combined into a new phrase, they include at least one active
   phrase. For the second step, when one of two overlapping phrases is to be removed, at least one of which has to be
   active phrase. Actually, it is impossible that two inactive phrases are overlapping. For every pair of overlapping
   phrases, lower-priority phrase and its descendants are removed, and its parent phrase turns back active.
   Linguistic knowledge can be used in pruning, for examples, adverbals close verbs nearer than complements, objects
   close verbs nearer than subjects, but inter-phrases priority relations may vary in different context.
   The final closure is comprised of an root category and other inactive phrasal categories.
   When interactively creating every transition of phrasal categorial combination, every trip of transition begins
   from no category-converted rules available, and ends after adopting some category-converted rules. When the stable
   phrasal closure is gotten, the interactive process terminates. Function parse uses same set of category-converted
   rules for all trips of transitions, and becomes useless under interactive mode.
 -}
{-
parse :: OnOff -> [PhraCate] -> [PhraCate] -> IO [PhraCate]
parse onOff trans banPCs = do
    let combs = removeDup $ atomizePhraCateList [cateComb onOff pc1 pc2 | pc1 <- trans, pc2 <- trans, stOfCate pc1 + spOfCate pc1 + 1 == stOfCate pc2, (acOfCate pc1)!!0 || (acOfCate pc2)!!0]      -- At least one is active.
    let newCbs = [cb| cb <- combs, ctspaOfCate cb /= [], notElem cb banPCs]
    let trans1 = trans ++ newCbs                                    -- Before pruning
    trans2 <- prune $ updateAct $ trans1                            -- After pruning, Attr. activity is corrected.
    let banPCs2 = banPCs ++ [cb| cb <-trans1, notElem cb trans2]    -- Update the list of banned phrasal categories.
    if newCbs == []
      then return trans                  -- No new combination
      else parse onOff trans2 banPCs2
-}

{- We adopt pruning method to remove those banned phrases, based on some axioms.
   Axiom 1. The pruned phrases are no longer generated.
   Axiom 2. After pruning, Type-0 and Type-1 overlaps certainly disappear, but Type-2, -3, -4 overlaps might remain.
   Axiom 3. Two phrases in a Type-2, -3, and -4 overlap are not construct (namely blood) relation, then one of which must be removed out.
   Like pruning in game search, any phrasal category not appearing in the final parsing tree is removed out after
   just generated, and any phrasal category having taken part in category combination should be set inactive, which
   can still combine with active categories. When removing a category, its parent categories should be set active.
 -}
prune :: [OverPair] -> [PhraCate] -> IO [PhraCate]
prune overPairs pcs = do
     let pcps = getOverlap pcs                      -- Get overlapping phrase pairs
     if pcps == []                                  -- No overlapping phrases
       then return pcs
       else do
         pc <- findPhraWithLowestPrio pcs pcps overPairs   -- Find the phrase with lowest priority among all phrases.
         prune overPairs $ removeOnePC pc pcs

{- A wrapper of Function <prune> to input the phrases to be pruned and the banned phrases, return the result and
   the updated banned phrases.
 -}
{-
prune' :: [PhraCate] -> [PhraCate] -> IO ([PhraCate],[PhraCate])
prune' pcs banPCs = do
    pcs1 <- prune pcs                                              -- After pruning, Attr. activity is corrected.
    let banPCs1 = banPCs ++ [pc| pc <- pcs, notElem' pc pcs1]      -- Update the list of banned phrasal categories.
    return (pcs1, banPCs1)
 -}

{- Get all pairs of overlapping phrases. In every pair, there is no blood relation, and at least one phrase is active.
   Using pclt to avoid (a,b) and (b,a) concurrent, only considering 'a' is left of 'b'. Here, phrases' spans > 0.
 -}
getOverlap :: [PhraCate] -> [(PhraCate, PhraCate)]
getOverlap [] = []
getOverlap pcs = [(x,y)| x<-pcs, y<-pcs, spOfCate x > 0, spOfCate y > 0, x/=y, pclt x y, (acOfCate x)!!0 || (acOfCate y)!!0, getOverType pcs x y /= 0]

{- Overlapping relation is unidirectional. (<pc1>, <pc2>) is overlapping, then (<pc2>, <pc1>) is not overlapping.
   The relation has no transitivity. Without loss of generality, let AB and BC be overlapping pairs, then AC might be
   not overlapping, one possible reason of which is both A and C are inactive. A phrase has the lowest priority means
   its priority is lower than that of its every overlapping phrases.
   For a list of overlapping-phrasal tuples,
   (1) If there is not any overlapping phrase, return ((-1,-1),[],-1), namely 'nilPhra';
   (2) From the first pair of overlapping phrases, select the lower-priority phrase by GeneBase, get the phrase's
       related overlapping pairs from the remaining pairs. If there is no related pair, return the phrase; otherwise
       go (1).
 -}

findPhraWithLowestPrio :: [PhraCate] -> [(PhraCate,PhraCate)] -> [OverPair] -> IO PhraCate
findPhraWithLowestPrio trans ops overPairs = do
    if ops == []
      then return nilPhra
      else do
        let x = head ops
        let xs = tail ops
        let pc1 = fst x
        let pc2 = snd x
        pri <- getPrior overPairs pc1 pc2                              -- Find priority from a list of 'OverPair'
        let pcps1 = [y| y <- xs, (fst y == pc1) || (snd y == pc1)]     -- [(PhraCate,PhraCate)] related with pc1
        let pcps2 = [y| y <- xs, (fst y == pc2) || (snd y == pc2)]     -- [(PhraCate,PhraCate)] related with pc2
        if pri == Lp && pcps2 /= []
          then findPhraWithLowestPrio trans pcps2 overPairs
          else if pri == Lp && pcps2 == []
                 then return pc2
                 else if pri == Rp && pcps1 /= []
                        then findPhraWithLowestPrio trans pcps1 overPairs
                        else if pri == Rp && pcps1 == []
                               then return pc1
                               else error "findPhraWithLowestPrio: pri == Noth."

{- Select <prior> from a list of 'OverPair' where matching given an overlapping pair of phrases.
   <Just Lp> means <leftOver> should remains while <rightOver> should be abandoned, and <Just Rp> means the contrary.
   If inquire fails, return Nothing.
   Pruning is a recursive process. After removing one phrase from transitive closure, neighbours of some phrases will
   change. Before pruning, all overlapping phrases are assigned with priorities, and these priorities are hoped to
   keep valid during the whole process of pruning. So, querying structural genes in Table stru_gene is replaced by
   querying overlapping phrases in a list of 'OverPair', which is created before pruning.
 -}

getPrior :: [OverPair] -> PhraCate -> PhraCate -> IO Prior
getPrior [] _ _ = return Noth
getPrior (op:ops) pc1 pc2 = do
    let lo = fst3 op                            -- Left-overlapping phrase
    let ro = snd3 op                            -- Right-overlapping phrase
    if (equalPhra lo pc1 && equalPhra ro pc2)
      then return (thd3 op)
      else getPrior ops pc1 pc2

{- Decide whether two phrasal categories are overlapping. If overlapping, give its type.
   Type 1: st1==st2,sp1==sp2         (Equal overlap)
          |~~~~~|
          |~~~~~|
   Type 2: st1 < st2 && st2 <= (st1 + sp1) && (st1 + sp1) < (st2 + sp2) && pclt pc1 pc2   (Cross overlap)
          |~~~~~|
             |~~~~~|
   Type 3: st1==st2 && st1 + sp1 < st2 + sp2 && pclt pc1 pc2       (Left-inclusive overlap)
          |~~~~~|
          |~~~~~~~~~~|
   Type 4:  st1 < st2 && st1 + sp1 == st2 + sp2 && pclt pc1 pc2    (Right-inclusive overlap)
          |~~~~~~~~~~|
               |~~~~~|
   Type 5:  st1 < st2 && st1 + sp1 > st2 + sp2 && pclt pc1 pc2     (Two-end inclusive overlap)
          |~~~~~~~~~~|
             |~~~~~|
   Type 0: Other situations                                       (Non-overlap)
   Note: There is no blood relation between <pc1> and <pc2>.
 -}

getOverType :: [PhraCate] -> PhraCate -> PhraCate -> Int
getOverType pcs pc1 pc2
    | st1 == st2 && sp1 == sp2 = 1                                                     -- Equal overlap
    | st1 < st2  && st2 <= (st1 + sp1) && (st1 + sp1) < (st2 + sp2) = 2                -- Cross overlap
    | st1 == st2 && st1 + sp1 < st2 + sp2 && notElem pc2 (findDescen pc1 pcs) = 3      -- Left-inclusive overlap
    | st1 < st2  && st1 + sp1 == st2 + sp2 && notElem pc1 (findDescen pc2 pcs) = 4     -- Right-inclusive overlap
    | st1 < st2  && st1 + sp1 > st2 + sp2 && notElem pc1 (findDescen pc2 pcs) = 5      -- Two-end inclusive overlap
    | otherwise = 0                        -- Non-overlap or blood relation
    where
    st1 = stOfCate pc1
    sp1 = spOfCate pc1
    st2 = stOfCate pc2
    sp2 = spOfCate pc2

-- Remove a phrasal category together with its descendants, then update activity of all phrasal categories.

removeOnePC :: PhraCate -> [PhraCate] -> [PhraCate]
removeOnePC pc clo = updateAct [x| x <- clo, notElem x (pc:descens)]
    where
      descens = findDescen pc clo                    -- Descendants of 'pc'

{- Check every category among a transitive result, and set its correct activity. Before a trip of transition, parsing
   tree is partial and has been pruned. Just after a trip of transition, some phrases in parsing tree need change
   their activities. For those phrases taking part in other phrases, they are set inactive, and the others are set
   active.
   Here, every phrasal category is atomic, namely has ONLY ONE element in its CTSPA component.
 -}

updateAct :: [PhraCate] -> [PhraCate]
updateAct trans = [deactOnePC x | x <- married] ++ [actOnePC x | x <- not_married]
    where
      married = removeDup [x| pc <- trans, (taOfCate pc)!!0 /= "Desig", parpair <- findSplitCate pc trans, x <- [fst parpair, snd parpair]]
              -- Owing to overlapping, a parent would be created not only once.
      not_married = [y| y <- trans, notElem y married]

{- Find splited (namely parent) categories for a given phrase category from the closure of phrase categories.
   Here, every phrasal category is atomic, namely has only one element in its CTSPA component.
 -}

findSplitCate :: PhraCate -> [PhraCate] -> [(PhraCate, PhraCate)]
findSplitCate pc clo
    = [pct | pct <- pcTuples, pcBelong' pc (cateComb onOff (fst pct) (snd pct))]
                                           -- Using pcBelong' not pcBelong is for neglecdting the active attribute.
      where
        st1 = stOfCate pc
        st2 = ssOfCate pc
        sp1 = st2 - st1 - 1                -- When pc is a leaf, sp1 will be -1.
        sp2 = spOfCate pc - sp1 - 1        -- When pc is a leaf, sp2 will be 0, that is, the second parent is pc.
        pcTuples = [(x, y) | x <- (getPhraBySS (st1, sp1) clo), y <- (getPhraBySS (st2, sp2) clo)]
                                           -- When pc is a leaf, pcTuples will be [] because span -1 does not exist.
        tags = splitAtDeli '-' ((taOfCate pc)!!0)    -- All tags used
        cctags = [x| x <- tags, elem x ccTags]       -- Tags of category-converted rules, maybe empty.
        onOff = updateOnOff [] (map ("+" ++) cctags)

-- Find descendants of a given phrasal category from the transitive closure of phrasal categories.

findDescen :: PhraCate -> [PhraCate] -> [PhraCate]
findDescen pc clo
    | children == [] = []
    | otherwise = children ++ (foldr (++) [] (map (\x -> findDescen x clo) children))
      where
        children = [x| x <- clo, (taOfCate x)!!0 /= "Desig", y <- findSplitCate x clo, pcBelong' pc (fst y) || pcBelong' pc (snd y)]
         -- There is only one child of 'pc' if lower-priority combinations are removed timely.
         -- Apparently, initial word categories are not children of any other category.

{- Generate syntactic trees (forest) from the closure of phrase categories which has been atomized.
   Here is a recursived forest-growing algorithm: For the input forest,
   (1) If it is an empty forest without any tree, an empty forest is returned;
   (2) Otherwise, one forest is created from a tree in input forest, return the union of all created forests.
   After adopting pruning, at most one tree might exist, which can be obtained directly and it is not necessary to
   generate it again.
 -}

growForest :: OnOff -> [[PhraCate]] -> [PhraCate] -> [[PhraCate]]
growForest _ [] _ = []                        -- Empty forest
growForest onOff (t:ts) phraCateClosure       -- nonempty forest
    = (growTree onOff t phraCateClosure) ++ (growForest onOff ts phraCateClosure)

{- One tree can grow at all tips to send forth a layer of leaves. Every parsing tree in Combinatory Categorial
   Grammar is one binary tree, namely from every tip, only two leaces can grow out. It makes growing process
   complicated that there might be more than one pair of leaves to grow out for every tip, Selecting different pairs
   would create different trees. When more than one rule can be used to combine two parent categories, multiple
   resultant categories are created.
   The forest growing from a tree is done by the following:
   (1) Find all tips able to send forth leaves (The tips for initial categoies are no longer to grow);
   (2) For every such tip, find all splits (namely all pairs of leaves), and create a forest, of which every tree
       include the input tree and a distinc pair of leaves;
   (3) Based on merging two forests, all forests are merged into one forest.
   After adopting pruning, only one pair of leaves can grow out for every tip, so one tree can't tiller out other
   trees. This function is obsolete under interactive parsing mode.
 -}

growTree :: OnOff -> [PhraCate] -> [PhraCate] -> [[PhraCate]]
growTree onOff t pcClo
    | tips == [] = [t]                                        -- No growable tip.
    | [t] == gf = [t]                                         -- Not grow.
    | otherwise = growForest onOff gf pcClo
      where
        tips = findTipsOfTree onOff t pcClo                            -- Find growable tips
        splOfAllTips = [findSplitCate tip pcClo | tip <- tips]   -- [[(PhraCate, PhraCate)]]

{- For every tip, there may be multiple splits, so it may have multiple pairs of parent categories.
   For every split, there may be multiple pairs of parent categories owing to multiple rules available.
 -}

        forestByTipGrow = [map (\x -> [fst x | notElem (fst x) t] ++ [snd x | notElem (snd x) t] ++ t ) splOfATip | splOfATip <- splOfAllTips]                                               -- [[[PhraCate]]]

-- It's necessary of composing grows at different tips, while not composing two kinds of grows at an identical tip.

        gf = uniForest forestByTipGrow

{- Find all growable tips of a tree from the closure of phrase categories which has been atomized. Those nodes whose
   parents already exist in the tree can't grow again. The tips corresponding to initial categories are not growable.
   This function is obsolete under interactive parsing mode.
 -}

findTipsOfTree :: OnOff -> [PhraCate] -> [PhraCate] -> [PhraCate]
findTipsOfTree _ [] _ = []
findTipsOfTree onOff t pcClo
    | ppcs == [] = findTipsOfTree onOff (tail ot) pcClo        -- Leaves have no parents and can't grow.

{- If a node already grew, then its right parent should be in ordered phrasal series <ot>.
   But, if the node grew out multiple pairs of parent nodes, all parent node pairs must be checked.
   If there exists a certain right parent in <ot>, it can be concluded that the node is not a tip.
 -}

    | foldl (||) False (map (\x -> elem (snd x) ot) ppcs) = findTipsOfTree onOff (tail ot) pcClo
    | otherwise = (head ot):findTipsOfTree onOff (tail ot) pcClo      -- Head node which doesn't yet grow.
      where
        ot = quickSort t       -- Such that there exists left parent << node << right parent for any node.
        ppcs = findSplitCate (head ot) pcClo            -- Find the parent pairs of node (head ot).

{- By growing at each tip, a tree grows and might become multiple trees because more than one split exists.
   Suppose tree t becomes ti = [ti1,ti2,...tin] by growing at No.i tip, and tj = [tj1,tj2,...tjm] by growing at No.j
   tip. Both the two forests are from the same tree t, and should merge into forest tk, tk = ti X tj. Merging tix and
   tjy, x<-[1..n], y<-[1..m], is actually to do an union operation on two sets.
   This function is obsolete under interactive parsing mode.
 -}

uniForest :: [[[PhraCate]]] -> [[PhraCate]]
uniForest [] = []                -- No forest
uniForest [f] = f                -- Just one forest, usually tillered out of one leave.
uniForest (f:fs)                 -- At least two forests
    = foldl uniTwoForest f fs

-- Merging two forest.
uniTwoForest :: [[PhraCate]] -> [[PhraCate]] -> [[PhraCate]]
uniTwoForest f1 f2 = [uniTwoTree t1 t2 | t1<-f1, t2<-f2]

-- Merging two trees.
uniTwoTree :: [PhraCate] -> [PhraCate] -> [PhraCate]
uniTwoTree t1 t2 = t1 ++ [x | x<-t2, notElem x t1]
