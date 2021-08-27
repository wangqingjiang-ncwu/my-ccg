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
    findPhraWithLowestPrio,  -- [(PhraCate,PhraCate)] -> [(PhraCate,PhraCate)] -> [OverPair] -> PhraCate
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
   centric syntactic view. By converting subject-predicate phrase into nominal phrase, then using standard CCG rules,
   some syntactic types can be gotten. For each result (<category>, <tag>, <seman>, <cn>, <act>), the <tag> is changed
   as "S/s-"++<tag> or "O/s-"++<tag> to remember type conversion s->np which happens before using the standard rule <tag>.
   Subject-predicate structure can act as predicate of the whole clause, which needs conversion s -> s\.np, taged
   as "P/s". When acting as attribute, subject-predicate structure needs type np/.np, namely conversion "A/s".
   Conversion S/s, P/s, O/s, or A/s is always used together with a certain standard rule to implement two-type combination.
 -}
      s_S = removeDup [(npCate, snd3 csp, thd3 csp) | csp <- csp1, fst3 csp == sCate]
      ctspaBysToS = [rule cate1 cate2 | rule <- [appB,raiFh], cate1 <- s_S, cate2 <- csp2, elem Ss onOff]
      catesBysToS = [(fst5 cate, "S/s-" ++ snd5 cate, thd5 cate, fth5 cate, fif5 cate) | cate <- ctspaBysToS]

-- Use P/s only when subject-predicate structure appears at predicate position or head-word position of DHv.
      s_P = removeDup [(predCate, snd3 csp, thd3 csp) | csp <- csp2, fst3 csp == sCate]
      ctspaBysToP = [rule cate1 cate2 | rule <- [appF,appB,raiFh], cate1 <- csp1, cate2 <- s_P, elem Ps onOff]
      catesBysToP = [(fst5 cate, "P/s-" ++ snd5 cate, thd5 cate, fth5 cate, fif5 cate) | cate <- ctspaBysToP]

      s_O = removeDup [(npCate, snd3 csp, thd3 csp) | csp <- csp2, fst3 csp == sCate]
      ctspaBysToO = [rule cate1 cate2 | rule <- [appF], cate1 <- csp1, cate2 <- s_O, elem Os onOff]
      catesBysToO = [(fst5 cate, "O/s-" ++ snd5 cate, thd5 cate, fth5 cate, fif5 cate) | cate <- ctspaBysToO]

-- Use N/s only when meeting "<conjunction> s", "s 的", "s <nounCompCate>", "<prep2AdvCate> s", and "<adjective> s".
      s_N1 = removeDup [(npCate, snd3 csp, thd3 csp) | csp <- csp2, fst3 csp == sCate]
      ctspaBysToN1 = [rule cate1 cate2 | rule <- [appF], cate1 <- csp_1, cate2 <- s_N1, elem Ns onOff]
          where
          csp_1 = removeDup [x| x <- csp1, fst3 x == conjCate1]
      s_N2 = removeDup [(npCate, snd3 csp, thd3 csp) | csp <- csp1, fst3 csp == sCate]
      ctspaBysToN2 = [rule cate1 cate2 | rule <- [appB], cate1 <- s_N2, cate2 <- csp_2, elem Ns onOff]
          where
          csp_2 = removeDup [x| x <- csp2, fst3 x == aux1Cate]
      s_N3 = removeDup [(npCate, snd3 csp, thd3 csp) | csp <- csp1, fst3 csp == sCate]
      ctspaBysToN3 = [rule cate1 cate2 | rule <- [appB], cate1 <- s_N3, cate2 <- csp_2, elem Ns onOff]
          where
          csp_2 = removeDup [x| x <- csp2, fst3 x == nounCompCate]
      s_N4 = removeDup [(npCate, snd3 csp, thd3 csp) | csp <- csp2, fst3 csp == sCate]
      ctspaBysToN4 = [rule cate1 cate2 | rule <- [appF], cate1 <- csp_1, cate2 <- s_N4, elem Ns onOff]
          where
          csp_1 = removeDup [x| x <- csp1, fst3 x == prep2AdvCate]
      s_N5 = removeDup [(npCate, snd3 csp, thd3 csp) | csp <- csp2, fst3 csp == sCate]
      ctspaBysToN5 = [rule cate1 cate2 | rule <- [appF], cate1 <- csp_1, cate2 <- s_N5, elem Ns onOff]
          where
          csp_1 = removeDup [x| x <- csp1, cateEqual (fst3 x) adjCate]
      ctspaBysToN = ctspaBysToN1 ++ ctspaBysToN2 ++ ctspaBysToN3 ++ ctspaBysToN4 ++ ctspaBysToN5
      catesBysToN = [(fst5 cate, "N/s-" ++ snd5 cate, thd5 cate, fth5 cate, fif5 cate) | cate <- ctspaBysToN]

      s_A = removeDup [(adjCate, snd3 csp, thd3 csp) | csp <- csp1, fst3 csp == sCate]
      ctspaBysToA = [rule cate1 cate2 | rule <- [appF], cate1 <- s_A, cate2 <- csp2, fst3 cate2 == npCate, elem As onOff] ++ [rule cate1 cate2 | rule <- [appB], cate1 <- s_A, cate2 <- csp2, fst3 cate2 == aux1Cate, elem As onOff]
      catesBysToA = [(fst5 cate, "A/s-" ++ snd5 cate, thd5 cate, fth5 cate, fif5 cate) | cate <- ctspaBysToA]

{- According to Jia-xuan Shen's theory, successive inclusions from noun to verb, and to adjective, and non-inflectionship
   of Chinese words, the following syntax-typed conversions exist,
   S/s, P/s, O/s, N/s, A/s, S/v, O/v, A/v, Hn/v, N/v, D/v, Cn/v, Cv/v, S/a, O/a, Hn/a, N/a, P/a, V/a, D/a, Cv/a, Cn/a, A/n, P/n, V/n, Cn/n, Dn, D/p, N/oe, N/pe, A/q, N/d, A/d, Ds/d.
   Besides, two adjacent syntactic types can convert to their new types respectively and simultaneously,
   such as "np np/.np -> np/.np np" noted as A/n-Hn/a. When used with some standard rules, two-typed combination is
   labelled as "S/v-"++<tag>, "O/v-"++<tag>, "A/v-"++<tag>, and so on. Now, type conversions only happen in
   acting as particular syntactic constituents.
 -}
 -- The conversion from verb to noun happens when verbal phrase occupies subject or object position.
      v_S = removeDup [(npCate, snd3 csp, thd3 csp)|csp <- csp1, elem True (map (\x-> cateEqual x (fst3 csp)) vCate)]
      ctspaByvToS = [rule cate1 cate2 | rule <- [appB,raiFh], cate1 <- v_S, cate2 <- csp_2, elem Sv onOff]
          where
          csp_2 = removeDup [x| x <- csp2, elem True (map (\y-> cateEqual y (fst3 x)) vCate)]
      catesByvToS = [(fst5 cate, "S/v-" ++ snd5 cate, thd5 cate, fth5 cate, fif5 cate) | cate <- ctspaByvToS]

      v_O = removeDup [(npCate, snd3 csp, thd3 csp)|csp <- csp2, elem True (map (\x-> cateEqual x (fst3 csp)) vpCate)]
          where
          vpCate = [predCate, verbCate, verbCate2, prep2AdvCate]                       -- Verbal or Prepositional object
      ctspaByvToO = [rule cate1 cate2 | rule <- [appF], cate1 <- csp_1, cate2 <- v_O, elem Ov onOff]
          where
          vpCate2 = [verbCate, verbCate2, prep2AdvCate, prep2CompCate]                 -- Need an object
          csp_1 = removeDup [x| x <- csp1, elem True (map (\y-> cateEqual y (fst3 x)) vpCate2)]
      catesByvToO = [(fst5 cate, "O/v-" ++ snd5 cate, thd5 cate, fth5 cate, fif5 cate) | cate <- ctspaByvToO]

{- The conversion from verbal types to np/.np happens when the verb occupies attribute position.
 -}
      v_A = removeDup [(adjCate, snd3 csp, thd3 csp) | csp <- csp1, elem True (map (\x-> cateEqual x (fst3 csp)) vCate)]
      ctspaByvToA = [rule cate1 cate2 | rule <- [appF], cate1 <- v_A, cate2 <- csp2, fst3 cate2 == npCate, elem Av onOff]
      catesByvToA = [(fst5 cate, "A/v-" ++ snd5 cate, thd5 cate, fth5 cate, fif5 cate) | cate <- ctspaByvToA]

{- The conversion from verb to noun happens when the verb occupies nominal head word position of structure AHn and HnC.
 -}
      v_Hn_A = removeDup [(npCate, snd3 csp, thd3 csp) | csp <- csp2, elem True (map (\x-> cateEqual x (fst3 csp)) vCate)]
      ctspaByvToHn_A = [rule cate1 cate2 | rule <- [appF], cate1 <- csp1, cate2 <- v_Hn_A, cateEqual (fst3 cate1) adjCate, elem Hnv onOff]

      v_Hn_C = removeDup [(npCate, snd3 csp, thd3 csp) | csp <- csp1, elem True (map (\x-> cateEqual x (fst3 csp)) vCate)]
      ctspaByvToHn_C = [rule cate1 cate2 | rule <- [appB], cate1 <- v_Hn_C, cate2 <- csp2, fst3 cate2 == ndCate, elem Hnv onOff]

      ctspaByvToHn = ctspaByvToHn_A ++ ctspaByvToHn_C
      catesByvToHn = [(fst5 cate, "Hn/v-" ++ snd5 cate, thd5 cate, fth5 cate, fif5 cate) | cate <- ctspaByvToHn]

{- The conversion from verb to noun happens when the verb occupies nominal position of Preposition-Object structure PO,
 - '的' structure U1P, or Coordination structure XX.
 -}
      v_N_PO = removeDup [(npCate, snd3 csp, thd3 csp) | csp <- csp2, elem True (map (\x-> cateEqual x (fst3 csp)) vCate)]
      ctspaByvToN_PO = [rule cate1 cate2 | rule <- [appF], cate1 <- csp1, cate2 <- v_N_PO, elem (fst3 cate1) [prep2AdvCate, prep2CompCate], elem Nv onOff]

      v_N_U1P = removeDup [(npCate, snd3 csp, thd3 csp) | csp <- csp1, elem True (map (\x-> cateEqual x (fst3 csp)) vCate)]
      ctspaByvToN_U1P = [rule cate1 cate2 | rule <- [appB], cate1 <- v_N_U1P, cate2 <- csp2, fst3 cate2 == aux1Cate, elem Nv onOff]

-- When a verb follows (X\*X)/*X.
      v_N_XX1 = removeDup [(npCate, snd3 csp, thd3 csp) | csp <- csp2, elem True (map (\x-> cateEqual x (fst3 csp)) vCate)]
      ctspaByvToN_XX1 = [rule cate1 cate2 | rule <- [appF], cate1 <- csp1, fst3 cate1 == conjCate1, cate2 <- v_N_XX1, elem Nv onOff]

-- When a verb is followed by X\*X.
      v_N_XX2 = removeDup [(npCate, snd3 csp, thd3 csp) | csp <- csp1, elem True (map (\x-> cateEqual x (fst3 csp)) vCate)]
      ctspaByvToN_XX2 = [rule cate1 cate2 | rule <- [appB], cate1 <- v_N_XX2, cate2 <- csp2, fst3 cate2 == conjCate2, elem Nv onOff]

      ctspaByvToN = ctspaByvToN_PO ++ ctspaByvToN_U1P ++ ctspaByvToN_XX1 ++ ctspaByvToN_XX2
      catesByvToN = [(fst5 cate, "N/v-" ++ snd5 cate, thd5 cate, fth5 cate, fif5 cate) | cate <- ctspaByvToN]

-- The conversion from verb to adverb happens when the verb occupies adverbial position.
      v_D1 = removeDup [(adverbalCate, snd3 csp, thd3 csp) | csp <- csp1, elem True (map (\x-> cateEqual x (fst3 csp)) vCate)]
      ctspaByvToD1 = [rule cate1 cate2 | rule <- [appF,comFh], cate1 <- v_D1, cate2 <- csp_2, elem Dv onOff]
          where
          csp_2 = removeDup [x| x <- csp2, elem True (map (\y-> cateEqual y (fst3 x)) vCate)]
      v_D2 = removeDup [(adverbalCate, snd3 csp, thd3 csp) | csp <- csp1, elem True (map (\x-> cateEqual x (fst3 csp)) vCate)]
      ctspaByvToD2 = [rule cate1 cate2 | rule <- [appF,comFh], cate1 <- v_D2, cate2 <- csp_2, elem Dv onOff]
          where
          csp_2 = removeDup [x| x <- csp2, fst3 x == adverbalCate]
      ctspaByvToD = ctspaByvToD1 ++ ctspaByvToD2
      catesByvToD = [(fst5 cate, "D/v-" ++ snd5 cate, thd5 cate, fth5 cate, fif5 cate) | cate <- ctspaByvToD]

-- The conversion from verb types to noun-complemented types happens when verb phrases occupy noun-complemented position.
      v_Cn = removeDup [(nounCompCate, snd3 csp, thd3 csp) | csp <- csp2, elem True (map (\x-> cateEqual x (fst3 csp)) vCate)]
      ctspaByvToCn = [rule cate1 cate2 | rule <- [appB], cate1 <- csp_1, cate2 <- v_Cn, elem Cnv onOff]
          where
          csp_1 = removeDup [x| x <- csp1, fst3 x == npCate]
      catesByvToCn = [(fst5 cate, "Cn/v-" ++ snd5 cate, thd5 cate, fth5 cate, fif5 cate) | cate <- ctspaByvToCn]

-- The conversion from verb types to verb-complemented types happens when verb phrases occupy verb-complemented position.
      v_Cv = removeDup [(verbCompCate, snd3 csp, thd3 csp) | csp <- csp2, elem True (map (\x-> cateEqual x (fst3 csp)) vCate)]
      ctspaByvToCv = [rule cate1 cate2 | rule <- [appB,comBc], cate1 <- csp_1, cate2 <- v_Cv, elem Cvv onOff]
          where
          csp_1 = removeDup [x| x <- csp1, elem True (map (\y-> cateEqual y (fst3 x)) vCate)]
      catesByvToCv = [(fst5 cate, "Cv/v-" ++ snd5 cate, thd5 cate, fth5 cate, fif5 cate) | cate <- ctspaByvToCv]

{- The conversion from adjective to noun happens when the adjective occupies subject, object, AHn's headword position, or HnC's headword position.
 -}
      a_S = removeDup [(npCate, snd3 csp, thd3 csp) | csp <- csp1, cateEqual (fst3 csp) adjCate]
      ctspaByaToS = [rule cate1 cate2 | rule <- [appB, raiFh], cate1 <- a_S, cate2 <- csp_2, elem Sa onOff]
          where
          csp_2 = removeDup [x| x <- csp2, elem True (map (\y-> cateEqual y (fst3 x)) vCate)]
      catesByaToS = [(fst5 cate, "S/a-" ++ snd5 cate, thd5 cate, fth5 cate, fif5 cate) | cate <- ctspaByaToS]

      a_O = removeDup [(npCate, snd3 csp, thd3 csp) | csp <- csp2, cateEqual (fst3 csp) adjCate]
      ctspaByaToO = [rule cate1 cate2 | rule <- [appF], cate1 <- csp_1, cate2 <- a_O, elem Oa onOff]
          where
          vCate2 = [verbCate, verbCate2]
          csp_1 = removeDup [x| x <- csp1, elem True (map (\y-> cateEqual y (fst3 x)) vCate2)]
      catesByaToO = [(fst5 cate, "O/a-" ++ snd5 cate, thd5 cate, fth5 cate, fif5 cate) | cate <- ctspaByaToO]

      a_Hn1 = removeDup [(npCate, snd3 csp, thd3 csp) | csp <- csp2, cateEqual (fst3 csp) adjCate]
      ctspaByaToHn1 = [rule cate1 cate2 | rule <- [appF], cate1 <- csp_1, cate2 <- a_Hn1, elem Hna onOff]
          where
          csp_1 = removeDup [x| x<- csp1, cateEqual (fst3 x) adjCate]
      a_Hn2 = removeDup [(npCate, snd3 csp, thd3 csp) | csp <- csp1, cateEqual (fst3 csp) adjCate]
      ctspaByaToHn2 = [rule cate1 cate2 | rule <- [appB], cate1 <- a_Hn2, cate2 <- csp_2, elem Hna onOff]
          where
          csp_2 = removeDup [x| x<-csp2, fst3 x == nounCompCate]
      ctspaByaToHn = ctspaByaToHn1 ++ ctspaByaToHn2
      catesByaToHn = [(fst5 cate, "Hn/a-" ++ snd5 cate, thd5 cate, fth5 cate, fif5 cate) | cate <- ctspaByaToHn]

{- The conversion from np/*np to np happens when adjective words are followed by auxiliary word '的',
 - or numeral words (with type np/*np) have prefix (with type np/*np) modification.
 -}
      a_N1 = removeDup [(npCate, snd3 csp, thd3 csp) | csp <- csp1, cateEqual (fst3 csp) adjCate]
      ctspaByaToN1 = [rule cate1 cate2 | rule <- [appB], cate1 <- a_N1, cate2 <- csp_2, elem Na onOff]
          where
          csp_2 = removeDup [x| x<-csp2, fst3 x == aux1Cate]
      a_N2 = removeDup [(npCate, snd3 csp, thd3 csp) | csp <- csp2, cateEqual (fst3 csp) adjCate]
      ctspaByaToN2 = [rule cate1 cate2 | rule <- [appF], cate1 <- csp_1, cate2 <- a_N2, elem Na onOff]
          where
          csp_1 = removeDup [x| x<-csp1, fst3 x == prefixCate]
      ctspaByaToN = ctspaByaToN1 ++ ctspaByaToN2
      catesByaToN = [(fst5 cate, "N/a-" ++ snd5 cate, thd5 cate, fth5 cate, fif5 cate) | cate <- ctspaByaToN]

{- The conversion from adjective to predicate verb happens when the adjective occupies predicate position or headword position of DHv and HvC.
 - such as, "高兴a 一辈子mq", "会d 高兴a 一辈子", "高兴a 极d 了u4"
 - The conversion also happens when the adjective follows <conjCate1> or <conjCate2> follows the adjective, and the coordination phrase acts as predicate.
 - such as, "迷惘a、孤独a" => "迷惘a、孤独v" => "迷惘v、孤独v"
 -}
      a_P1 = removeDup [(predCate, snd3 csp, thd3 csp) | csp <- csp2, cateEqual (fst3 csp) adjCate]
      ctspaByaToP1 = [rule cate1 cate2 | rule <- [appB], cate1 <- csp_1, cate2 <- a_P1, elem Pa onOff]
          where
          csp_1 = removeDup [x| x<- csp1, fst3 x == npCate]
      a_P2 = removeDup [(predCate, snd3 csp, thd3 csp) | csp <- csp2, cateEqual (fst3 csp) adjCate]
      ctspaByaToP2 = [rule cate1 cate2 | rule <- [appF], cate1 <- csp_1, cate2 <- a_P2, elem Pa onOff]
          where
          csp_1 = removeDup [x| x<- csp1, fst3 x == adverbalCate]
      a_P3 = removeDup [(predCate, snd3 csp, thd3 csp) | csp <- csp1, cateEqual (fst3 csp) adjCate]
      ctspaByaToP3 = [rule cate1 cate2 | rule <- [appB], cate1 <- a_P3, cate2 <- csp_2, elem Pa onOff]
          where
          csp_2 = removeDup [x| x<- csp2, fst3 x == verbCompCate]
      a_P4 = removeDup [(predCate, snd3 csp, thd3 csp) | csp <- csp2, cateEqual (fst3 csp) adjCate]
      ctspaByaToP4 = [rule cate1 cate2 | rule <- [appF], cate1 <- csp_1, cate2 <- a_P4, elem Pa onOff]
          where
          csp_1 = removeDup [x| x<- csp1, fst3 x == conjCate1]
      a_P5 = removeDup [(predCate, snd3 csp, thd3 csp) | csp <- csp1, cateEqual (fst3 csp) adjCate]
      ctspaByaToP5 = [rule cate1 cate2 | rule <- [appB], cate1 <- a_P5, cate2 <- csp_2, elem Pa onOff]
          where
          csp_2 = removeDup [x| x<- csp2, fst3 x == getCateFromString "(s\\.np)\\*(s\\.np)"]
      ctspaByaToP = ctspaByaToP1 ++ ctspaByaToP2 ++ ctspaByaToP3 ++ ctspaByaToP4 ++ ctspaByaToP5
      catesByaToP = [(fst5 cate, "P/a-" ++ snd5 cate, thd5 cate, fth5 cate, fif5 cate) | cate <- ctspaByaToP]

{- During the past, adjective is only considered to use predicate's syntax type. Now adjective is also allowed to use transitive verbs's syntax types.
 - To restrict syntactic ambiguity, the conversions only happen when the adjective occupies verb position or headword position of DHv or HvC.
 - Such as, "快乐a 着u4 你的快乐np", "要vu 快乐a 自己r". The conversion should be seldom used.
 -}
      a_V_VO = removeDup [(verbCate, snd3 csp, thd3 csp) | csp <- csp1, cateEqual (fst3 csp) adjCate]
      ctspaByaToV_VO = [rule cate1 cate2 | rule <- [appF], cate1 <- a_V_VO, cate2 <- csp_2, elem Va onOff]
          where
          csp_2 = removeDup [x| x<- csp2, fst3 x == npCate]

      a_V_DHv = removeDup [(verbCate, snd3 csp, thd3 csp) | csp <- csp2, cateEqual (fst3 csp) adjCate]
      ctspaByaToV_DHv = [rule cate1 cate2 | rule <- [comFh], cate1 <- csp_1, cate2 <- a_V_DHv, elem Va onOff]
          where
          csp_1 = removeDup [x| x<- csp1, fst3 x == adverbalCate]

      a_V_HvC = removeDup [(verbCate, snd3 csp, thd3 csp) | csp <- csp1, cateEqual (fst3 csp) adjCate]
      ctspaByaToV_HvC = [rule cate1 cate2 | rule <- [comBc], cate1 <- a_V_HvC, cate2 <- csp_2, elem Va onOff]
          where
          csp_2 = removeDup [x| x<- csp2, fst3 x == verbCompCate]
      ctspaByaToV = ctspaByaToV_VO ++ ctspaByaToV_DHv ++ ctspaByaToV_HvC
      catesByaToV = [(fst5 cate, "V/a-" ++ snd5 cate, thd5 cate, fth5 cate, fif5 cate) | cate <- ctspaByaToV]

-- The conversion from adjective to adverb happens when the adjective occupies adverbial modifier's position.
      a_D1 = removeDup [(adverbalCate, snd3 csp, thd3 csp) | csp <- csp1, cateEqual (fst3 csp) adjCate]
      ctspaByaToD1 = [rule cate1 cate2 | rule <- [appF,comFh,comFh2], cate1 <- a_D1, cate2 <- csp_2, elem Da onOff]
          where
          csp_2 = removeDup [x| x<- csp2, elem True (map (\y-> cateEqual y (fst3 x)) vCate)]
      a_D2 = removeDup [(adverbalCate, snd3 csp, thd3 csp) | csp <- csp1, cateEqual (fst3 csp) adjCate]
      ctspaByaToD2 = [rule cate1 cate2 | rule <- [comFh], cate1 <- a_D2, cate2 <- csp_2, elem Da onOff]
          where
          csp_2 = removeDup [x| x<- csp2, fst3 x == adverbalCate]
      ctspaByaToD = ctspaByaToD1 ++ ctspaByaToD2
      catesByaToD = [(fst5 cate, "D/a-" ++ snd5 cate, thd5 cate, fth5 cate, fif5 cate) | cate <- ctspaByaToD]

-- The category conversion from np/.np to (s\.np)\x(s\.np) happens when the adjective occupies completment position.
      a_Cv = removeDup [(verbCompCate, snd3 csp, thd3 csp) | csp <- csp2, cateEqual (fst3 csp) adjCate]
      ctspaByaToCv = [rule cate1 cate2 | rule <- [appB,comBc], cate1 <- csp_1, cate2 <- a_Cv, elem Cva onOff]
          where
          csp_1 = removeDup [x| x<- csp1, elem True (map (\y-> cateEqual y (fst3 x)) vCate)]
      catesByaToCv = [(fst5 cate, "Cv/a-" ++ snd5 cate, thd5 cate, fth5 cate, fif5 cate) | cate <- ctspaByaToCv]

-- The category conversion from np/.np to np\*np happens when a (measured) numeral occupies completment position.
      a_Cn = removeDup [(nounCompCate, snd3 csp, thd3 csp) | csp <- csp2, cateEqual (fst3 csp) adjCate]
      ctspaByaToCn = [rule cate1 cate2 | rule <- [appB], cate1 <- csp_1, cate2 <- a_Cn, elem Cna onOff]
          where
          csp_1 = removeDup [x| x<- csp1, fst3 x == npCate]
      catesByaToCn = [(fst5 cate, "Cn/a-" ++ snd5 cate, thd5 cate, fth5 cate, fif5 cate) | cate <- ctspaByaToCn]

{- The category conversion from np/.np to (np/.np)\*(np/.np) happens when np/.np (adjective classic type) complemently modify np/.np (adjective or numeral).
 - For examples, "好a 多a 了", and "二十m 多a 元q 钱n".
 -}
      a_Ca = removeDup [(adjCompCate, snd3 csp, thd3 csp) | csp <- csp2, cateEqual (fst3 csp) adjCate]
      ctspaByaToCa = [rule cate1 cate2 | rule <- [appB], cate1 <- csp_1, cate2 <- a_Ca, elem Caa onOff]
          where
          csp_1 = removeDup [x| x<- csp1, cateEqual (fst3 x) adjCate]
      catesByaToCa = [(fst5 cate, "Ca/a-" ++ snd5 cate, thd5 cate, fth5 cate, fif5 cate) | cate <- ctspaByaToCa]

-- The conversion from 'np' to 'np/.np' is ONLY allowed when nouns act as attribute, pronouns are followed by quantifiers, or nouns are followed by '地'.
      n_A = removeDup [(adjCate, snd3 csp, thd3 csp) | csp <- csp1, (fst3 csp) == npCate]
      ctspaBynToA1 = [rule cate1 cate2 | rule <- [appF], cate1 <- n_A, cate2 <- csp_2, elem An onOff]
          where
          csp_2 = removeDup [x| x<- csp2, fst3 x == npCate]
      ctspaBynToA2 = [rule cate1 cate2 | rule <- [appB], cate1 <- n_A, cate2 <- csp_2, elem An onOff]
          where
          csp_2 = removeDup [x| x<- csp2, fst3 x == quantifierCate]
      ctspaBynToA3 = [rule cate1 cate2 | rule <- [appB], cate1 <- n_A, cate2 <- csp_2, elem An onOff]
          where
          csp_2 = removeDup [x| x<- csp2, fst3 x == aux2Cate]
      ctspaBynToA = ctspaBynToA1 ++ ctspaBynToA2 ++ ctspaBynToA3
      catesBynToA = [(fst5 cate, "A/n-" ++ snd5 cate, thd5 cate, fth5 cate, fif5 cate) | cate <- ctspaBynToA]

-- The conversion from noun to predicate is ONLY allowed when the noun acts as predicate or the headword of DHv.
      n_P1 = removeDup [(predCate, snd3 csp, thd3 csp) | csp <- csp2, (fst3 csp) == npCate]
      ctspaBynToP1 = [rule cate1 cate2 | rule <- [appB], cate1 <- csp_1, cate2 <- n_P1, elem Pn onOff]
          where
          csp_1 = removeDup [x| x<- csp1, fst3 x == npCate]
      n_P2 = removeDup [(predCate, snd3 csp, thd3 csp) | csp <- csp2, (fst3 csp) == npCate]
      ctspaBynToP2 = [rule cate1 cate2 | rule <- [appF], cate1 <- csp_1, cate2 <- n_P2, elem Pn onOff]
          where
          csp_1 = removeDup [x| x<- csp1, fst3 x == adverbalCate]
      ctspaBynToP = ctspaBynToP1 ++ ctspaBynToP2
      catesBynToP = [(fst5 cate, "P/n-" ++ snd5 cate, thd5 cate, fth5 cate, fif5 cate) | cate <- ctspaBynToP]

-- The conversion from noun to verb is ONLY allowed when the noun acts as verb.
      n_V = removeDup [(verbCate, snd3 csp, thd3 csp) | csp <- csp1, (fst3 csp) == npCate]
      ctspaBynToV = [rule cate1 cate2 | rule <- [appF], cate1 <- n_V, cate2 <- csp_2, elem Vn onOff]
          where
          csp_2 = removeDup [x| x<- csp2, fst3 x == npCate]
      catesBynToV = [(fst5 cate, "V/n-" ++ snd5 cate, thd5 cate, fth5 cate, fif5 cate) | cate <- ctspaBynToV]

-- The conversion from noun to noun's completment is ONLY allowed when the noun acts as noun's completment.
      n_Cn = removeDup [(nounCompCate, snd3 csp, thd3 csp) | csp <- csp2, (fst3 csp) == npCate]
      ctspaBynToCn = [rule cate1 cate2 | rule <- [appB], cate1 <- csp_1, cate2 <- n_Cn, elem Cnn onOff]
          where
          csp_1 = removeDup [x| x<- csp1, fst3 x == npCate]
      catesBynToCn = [(fst5 cate, "Cn/n-" ++ snd5 cate, thd5 cate, fth5 cate, fif5 cate) | cate <- ctspaBynToCn]

-- The conversion from noun to adberbal type is ONLY allowed when the noun acts as adverbal.
      n_D = removeDup [(adverbalCate, snd3 csp, thd3 csp) | csp <- csp1, (fst3 csp) == npCate]
      ctspaBynToD = [rule cate1 cate2 | rule <- [appF, comFh, comFh2], cate1 <- n_D, cate2 <- csp_2, elem Dn onOff]
          where
          csp_2 = removeDup [x| x<- csp2, elem True (map (\y-> cateEqual y (fst3 x)) vCate)]
      catesBynToD = [(fst5 cate, "D/n-" ++ snd5 cate, thd5 cate, fth5 cate, fif5 cate) | cate <- ctspaBynToD]

-- The conversion from preposition to adverbial is ONLY allowed when the noun following the preposition is elliptical.
      p_D = removeDup [(adverbalCate, snd3 csp, thd3 csp) | csp <- csp1, (fst3 csp) == prep2AdvCate]
      ctspaBypToD = [rule cate1 cate2 | rule <- [appF,comFh,comFh2], cate1 <- p_D, cate2 <- csp_2, elem Dp onOff]
          where
          csp_2 = removeDup [x| x<- csp2, elem True (map (\y-> cateEqual y (fst3 x)) vCate)]
      catesBypToD = [(fst5 cate, "D/p-" ++ snd5 cate, thd5 cate, fth5 cate, fif5 cate) | cate <- ctspaBypToD]

{- The conversion from object-extractioned type "s/.np" to nominal constituent is ONLY allowed when '的' follows，whose typical syntactic type is (np/*np)\*np,
 - or noun completment follows, whose typical syntactic type is np\*np.
 -}
      oe_N = removeDup [(npCate, snd3 csp, thd3 csp) | csp <- csp1, (fst3 csp) == objectExtractionCate]
      ctspaByoeToN = [rule cate1 cate2 | rule <- [appB], cate1 <- oe_N, cate2 <- csp_2, elem Noe onOff]
          where
          csp_2 = removeDup [x| x<- csp2, fst3 x == aux1Cate || fst3 x == nounCompCate]
      catesByoeToN = [(fst5 cate, "N/oe-" ++ snd5 cate, thd5 cate, fth5 cate, fif5 cate) | cate <- ctspaByoeToN]

{- The conversion from Predicate-extractioned type "s/#(s\.np)" to nominal constituent is ONLY allowed when '的' follows，whose typical syntactic type is (np/*np)\*np.
 -}
      pe_N = removeDup [(npCate, snd3 csp, thd3 csp) | csp <- csp1, (fst3 csp) == predicateExtractionCate]
      ctspaBypeToN = [rule cate1 cate2 | rule <- [appB], cate1 <- pe_N, cate2 <- csp_2, elem Npe onOff]
          where
          csp_2 = removeDup [x| x<- csp2, fst3 x == aux1Cate]
      catesBypeToN = [(fst5 cate, "N/pe-" ++ snd5 cate, thd5 cate, fth5 cate, fif5 cate) | cate <- ctspaBypeToN]

{- The conversion from quantifier type (np/*np)\*(np/*np) to adjective type np/*np is ONLY allowed when a noun follows.
 - Actually, the conversion is used only when there is no numeral to the left.
 - If the context detection is implemented, the stntactic ambiguity can be restricted further.
 -}
      q_A = removeDup [(adjCate, snd3 csp, thd3 csp) | csp <- csp1, (fst3 csp) == quantifierCate]
      ctspaByqToA = [rule cate1 cate2 | rule <- [appF], cate1 <- q_A, cate2 <- csp_2, elem Aq onOff]
          where
          csp_2 = removeDup [x| x<- csp2, fst3 x == npCate]
      catesByqToA = [(fst5 cate, "A/q-" ++ snd5 cate, thd5 cate, fth5 cate, fif5 cate) | cate <- ctspaByqToA]

{- The conversion from adverbal type (s\.np)/#(s\.np) to noun type np is ONLY allowed when an auxiliary word '的' follows,
 - or occupies the objective position.
 -}
      d_N1 = removeDup [(npCate, snd3 csp, thd3 csp) | csp <- csp1, (fst3 csp) == adverbalCate]
      ctspaBydToN1 = [rule cate1 cate2 | rule <- [appB], cate1 <- d_N1, cate2 <- csp_2, elem Nd onOff]
          where
          csp_2 = removeDup [x| x<- csp2, fst3 x == aux1Cate]
      d_N2 = removeDup [(npCate, snd3 csp, thd3 csp) | csp <- csp2, (fst3 csp) == adverbalCate]
      ctspaBydToN2 = [rule cate1 cate2 | rule <- [appF], cate1 <- csp_1, cate2 <- d_N2, elem Nd onOff]
          where
          csp_1 = removeDup [x| x<- csp1, fst3 x == verbCate]
      ctspaBydToN = ctspaBydToN1 ++ ctspaBydToN2
      catesBydToN = [(fst5 cate, "N/d-" ++ snd5 cate, thd5 cate, fth5 cate, fif5 cate) | cate <- ctspaBydToN]

{- The conversion from adverbal type (s\.np)/#(s\.np) to adjective type np/.np is ONLY allowed when an auxiliary word '地' follows.
 -}
      d_A = removeDup [(adjCate, snd3 csp, thd3 csp) | csp <- csp1, (fst3 csp) == adverbalCate]
      ctspaBydToA = [rule cate1 cate2 | rule <- [appB], cate1 <- d_A, cate2 <- csp_2, elem Ad onOff]
          where
          csp_2 = removeDup [x| x<- csp2, fst3 x == aux2Cate]
      catesBydToA = [(fst5 cate, "A/d-" ++ snd5 cate, thd5 cate, fth5 cate, fif5 cate) | cate <- ctspaBydToA]

{- The conversion from adverbal type (s\.np)/#(s\.np) to sentential adjective type s/*s is ONLY allowed when a sentential type 's' follows.
 -}
      d_Ds = removeDup [(advCate4Sent, snd3 csp, thd3 csp) | csp <- csp1, (fst3 csp) == adverbalCate]
      ctspaBydToDs = [rule cate1 cate2 | rule <- [appF], cate1 <- d_Ds, cate2 <- csp_2, elem Dsd onOff]
          where
          csp_2 = removeDup [x| x<- csp2, fst3 x == sCate]
      catesBydToDs = [(fst5 cate, "Ds/d-" ++ snd5 cate, thd5 cate, fth5 cate, fif5 cate) | cate <- ctspaBydToDs]

{- The two adjacent types "np np/.np" convert to "np/.np np", forming structure AHn, here noun-to-adjective and
   adjective-to-noun conversions happen simultaneously.
 -}
      n_A1 = removeDup [(adjCate, snd3 csp, thd3 csp) | csp <- csp1, cateEqual (fst3 csp) npCate]
      a_Hn' = removeDup [(npCate, snd3 csp, thd3 csp) | csp <- csp2, cateEqual (fst3 csp) adjCate]
      ctspaBynToA_aToHn = [rule cate1 cate2 | rule <- [appF], cate1 <- n_A1, cate2 <- a_Hn', elem An onOff, elem Hna onOff]
          where
          csp_1 = removeDup [x| x<- csp1, fst3 x == npCate]
          csp_2 = removeDup [x| x<- csp2, fst3 x == adjCate]
      catesBynToA_aToHn = [(fst5 cate, "A/n-Hn/a-" ++ snd5 cate, thd5 cate, fth5 cate, fif5 cate) | cate <- ctspaBynToA_aToHn]

{- The two adjacent types "np <verb>" convert to "np/.np np", forming structure AHn, here noun-to-adjective and
   verb-to-noun conversions happen simultaneously.
 -}
      n_A'' = removeDup [(adjCate, snd3 csp, thd3 csp) | csp <- csp1, (fst3 csp) == npCate]
      v_Hn'' = removeDup [(npCate, snd3 csp, thd3 csp) | csp <- csp2, elem True (map (\x-> cateEqual x (fst3 csp)) vCate)]
      ctspaBynToA_vToHn = [rule cate1 cate2 | rule <- [appF], cate1 <- n_A'', cate2 <- v_Hn'', elem An onOff, elem Hnv onOff]
      catesBynToA_vToHn = [(fst5 cate, "A/n-Hn/v-" ++ snd5 cate, thd5 cate, fth5 cate, fif5 cate) | cate <- ctspaBynToA_vToHn]

{- The two adjacent types "<verb> np/.np" convert to "np s\.np", forming structure SP, here verb-to-Subject and
   adjective-to-predicate conversions happen simultaneously.
 -}
      v_S' = removeDup [(npCate, snd3 csp, thd3 csp) | csp <- csp1, elem True (map (\y-> cateEqual y (fst3 csp)) vCate)]
      a_P' = removeDup [(predCate, snd3 csp, thd3 csp) | csp <- csp2, cateEqual (fst3 csp) adjCate]
      ctspaByvToS_aToP = [rule cate1 cate2 | rule <- [appB], cate1 <- v_S', cate2 <- a_P', elem Sv onOff, elem Pa onOff]
      catesByvToS_aToP = [(fst5 cate, "S/v-P/a-" ++ snd5 cate, thd5 cate, fth5 cate, fif5 cate) | cate <- ctspaByvToS_aToP]

{- The two adjacent verbal types "<verb> <verb>" convert to "np/.np np", forming structure AHn, here verb-to-Attribute and
   verb-to-nominal headword conversions happen simultaneously.
 -}
      v_A' = removeDup [(adjCate, snd3 csp, thd3 csp) | csp <- csp1, elem True (map (\y-> cateEqual y (fst3 csp)) vCate)]
      v_Hn' = removeDup [(npCate, snd3 csp, thd3 csp) | csp <- csp2, elem True (map (\y-> cateEqual y (fst3 csp)) vCate)]
      ctspaByvToA_vToHn = [rule cate1 cate2 | rule <- [appF], cate1 <- v_A', cate2 <- v_Hn', elem Av onOff, elem Hnv onOff]
      catesByvToA_vToHn = [(fst5 cate, "A/v-Hn/v-" ++ snd5 cate, thd5 cate, fth5 cate, fif5 cate) | cate <- ctspaByvToA_vToHn]

{- The two adjacent types "np <verb>" convert to "<verb> np", forming structure VO, here V/n and O/v happen simultaneously.
 -}
      n_V' = removeDup [(verbCate, snd3 csp, thd3 csp) | csp <- csp1, elem True (map (\y-> cateEqual y (fst3 csp)) [npCate])]
      v_O' = removeDup [(npCate, snd3 csp, thd3 csp) | csp <- csp2, elem True (map (\y-> cateEqual y (fst3 csp)) vCate)]
      ctspaBynToV_vToO = [rule cate1 cate2 | rule <- [appF], cate1 <- n_V', cate2 <- v_O', elem Vn onOff, elem Ov onOff]
      catesBynToV_vToO = [(fst5 cate, "V/n-O/v-" ++ snd5 cate, thd5 cate, fth5 cate, fif5 cate) | cate <- ctspaBynToV_vToO]

{- The two adjacent types "<verb> <adjective>" convert to "s s\.np", forming structure SP, here S/s and P/a happen simultaneously.
 -}
      s_S' = removeDup [(npCate, snd3 csp, thd3 csp) | csp <- csp1, elem True (map (\y-> cateEqual y (fst3 csp)) vCate)]
      a_P'' = removeDup [(predCate, snd3 csp, thd3 csp) | csp <- csp2, cateEqual (fst3 csp) adjCate]
      ctspaBysToS_aToP = [rule cate1 cate2 | rule <- [appB], cate1 <- s_S', cate2 <- a_P'', elem Ss onOff, elem Pa onOff]
      catesBysToS_aToP = [(fst5 cate, "S/s-P/a-" ++ snd5 cate, thd5 cate, fth5 cate, fif5 cate) | cate <- ctspaBysToS_aToP]

{- The two adjacent types "<noun> <adverb>" convert to "np/.np np", forming structure AHn, here A/n and N/d happen simultaneously.
 -}
      n_A' = removeDup [(adjCate, snd3 csp, thd3 csp) | csp <- csp1, (fst3 csp) == npCate]
      d_N' = removeDup [(npCate, snd3 csp, thd3 csp) | csp <- csp2, (fst3 csp) == adverbalCate]
      ctspaBynToA_dToN = [rule cate1 cate2 | rule <- [appF], cate1 <- n_A', cate2 <- d_N', elem An onOff, elem Nd onOff]
      catesBynToA_dToN = [(fst5 cate, "A/n-N/d-" ++ snd5 cate, thd5 cate, fth5 cate, fif5 cate) | cate <- ctspaBynToA_dToN]

-- The categories gotten by all rules.
      cates = catesBasic ++ catesBysToS ++ catesBysToP ++ catesBysToO ++ catesBysToN ++ catesBysToA ++ catesByvToS ++ catesByvToO ++ catesByvToA ++ catesByvToHn
        ++ catesByvToN ++ catesByvToD ++ catesByvToCn ++ catesByvToCv ++ catesByaToS ++ catesByaToO ++ catesByaToHn ++ catesByaToN ++ catesByaToP ++ catesByaToV++ catesByaToD
        ++ catesByaToCv ++ catesByaToCn ++ catesByaToCa ++ catesBynToA ++ catesBynToP ++ catesBynToV ++ catesBynToCn ++ catesBynToD ++ catesBypToD ++ catesByoeToN
        ++ catesBypeToN ++ catesByqToA ++ catesBydToN ++ catesBydToA ++ catesBydToDs
        ++ catesBynToA_aToHn ++ catesBynToA_vToHn ++ catesByvToS_aToP ++ catesByvToA_vToHn ++ catesBynToV_vToO ++ catesBysToS_aToP ++ catesBynToA_dToN

    -- Remove Nil's resultant cateories, NR phrase-structural categories, and duplicate ones.
      rcs = [rc | rc <- cates, fst5 rc /= nilCate, fth5 rc /= "NR"]

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
   Fix: Before every trip of transitivity, syntactic-typed transformations can be selected on demand. If all transformations
   are available during every trip of transitivity, some phrases are banned at ambiguity resolution, the other phrases remain,
   at this time, the combinations of two inactive phrases will create banned phrases again. But now, two inactive phrases may
   combine to form new phrase via type transformations which are not allowed in previous transitivities. The inactive attribue
   is still important in indicating its having existed in certain phrases.
 -}
trans :: OnOff -> [PhraCate] -> [PhraCate] -> [PhraCate]
trans onOff pcs banPCs = pcs2
    where
--    combs = removeDup $ atomizePhraCateList [cateComb onOff pc1 pc2 | pc1 <- pcs, pc2 <- pcs, stOfCate pc1 + spOfCate pc1 + 1 == stOfCate pc2, (acOfCate pc1)!!0 || (acOfCate pc2)!!0]
--    Allowing two inactive phrases to combine.
      combs = atomizePhraCateList [cateComb onOff pc1 pc2 | pc1 <- pcs, pc2 <- pcs, stOfCate pc1 + spOfCate pc1 + 1 == stOfCate pc2]
      newCbs = [cb| cb <- combs, ctspaOfCate cb /= [], notElem' cb banPCs, notElem' cb pcs]
                 -- The banned phrases might be created again, here they are filtered out.
                 -- The non-banned phrases also might be created again, here those reduplicates are removed out.
      pcs2 = pcs ++ newCbs

{- One trip of transition with pruning. Some new phrases removed timely are placed into banned phrasal list, and some
   new structural genes are added into the list of structural genes.
   Fix: Allow two inactive phrases to combine.
 -}
transWithPruning :: [Rule] -> [PhraCate] -> [PhraCate] -> [OverPair] -> IO ([PhraCate],[PhraCate])
transWithPruning onOff pcs banPCs overPairs = do
--  let combs = removeDup $ atomizePhraCateList [cateComb onOff pc1 pc2 | pc1 <- pcs, pc2 <- pcs, stOfCate pc1 + spOfCate pc1 + 1 == stOfCate pc2, (acOfCate pc1)!!0 || (acOfCate pc2)!!0]
    let combs = atomizePhraCateList [cateComb onOff pc1 pc2 | pc1 <- pcs, pc2 <- pcs, stOfCate pc1 + spOfCate pc1 + 1 == stOfCate pc2]
                                                                  -- Not consider phrasal activity
    let newCbs = [cb| cb <- combs, ctspaOfCate cb /= [], notElem' cb banPCs, notElem' cb pcs]
    if newCbs /= []
      then do
        let pcs1 = pcs ++ newCbs                                      -- Before pruning
        pcs1' <- prune overPairs pcs1 newCbs                          -- After pruning
        let pcs2 = updateAct pcs1'                                    -- Attr. activity is corrected.
        let banPCs2 = banPCs ++ [cb| cb <- pcs1, notElem' cb pcs2]    -- Update the list of banned phrasal categories.
        return (pcs2, banPCs2)
      else return (pcs, banPCs)

{- Parsing a sequence of categories is actually to generate the category closure from the initial phrase categories.
   Originally designed in every transition, every two phrases are tested whether they can be combined into a new
   phrase under the CCG for Chinese. In the later experiments, an explosive increase of phrase number is always
   observed, sometimes the transitive closure can not be obtained after limited time. So, a pruning process is
   introduced in every trip of transition, and the pruned phrases are placed in list 'banPCs'. Apparently, the removed
   phrases should be not generated again.
   From the scratch, words are all active. In later every transition, two adjacent active phrases can be combined into
   a new phrase, while one active phrase and one inactive phrase can also be combined. Once participating in phrase
   building, no matter a word or a phrase will become inactive. As a result, only words or phrases without descendants
   are active. By the way, if an inactive phrase take part in combination with one active phrase, the descendants of
   the inactive phrase will be removed out and thrown into list 'banPCs'. Both an active and an inactive phrase can be
   removed later, owing that they overlap some phrases with higher priorities. An inactive phrase can become active
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
   After one trip of transitive computing among an unambiguous partial tree, the ambiguous overlaps only exist between new generated phrases,
   or between a new generated phrase and an old generated phrase.
   Axiom 4. For a phrase to be removed, if it is generated before the current transitive computing, its children should be removed also.
   Here, "newCbs" is the set of newly generated phrases.
 -}
prune :: [OverPair] -> [PhraCate] -> [PhraCate] -> IO [PhraCate]
prune overPairs pcs newCbs = do
     let pcps = getOverlap pcs                      -- Get overlapping phrase pairs
     if pcps == []                                  -- No overlapping phrases
       then return pcs
       else do
         pc <- findPhraWithLowestPrio pcps pcps overPairs   -- Find the phrase with lowest priority among all phrases.
         let pcs' = removeOnePC pc pcs newCbs               -- Remove phrase <pc> and its descendants.
         putStr "The removed phrase(s):"
         showNPhraCate' [x| x<-pcs, notElem x pcs']         -- Show all phrases in List <pcs> but not in List <pcs'>.
         prune overPairs pcs' newCbs

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

{- Here, overlapping relation is unidirectional. (<pc1>, <pc2>) is overlapping, then (<pc2>, <pc1>) is not overlapping.
   The relation has no transitivity. Without loss of generality, let AB and BC be overlapping pairs, then AC might be
   not overlapping, one possible reason of which is both A and C are inactive. One phrase has the lowest priority means
   its priority is lower than that of its every overlapping phrases.
   For a list of overlapping-phrasal tuples,
   (1) If there is not any overlapping phrase, return ((-1,-1),[],-1), namely 'nilPhra';
   (2) From the first pair of overlapping phrases, select the lower-priority phrase by GeneBase, get the phrase's
       related overlapping pairs from all unChecked pairs. If there is no related pair, return the phrase; otherwise
       recursively call this function on all unChecked Overlapping pairs and the low priority phrase-related overlapping pairs.
 -}

findPhraWithLowestPrio :: [(PhraCate,PhraCate)] -> [(PhraCate,PhraCate)] -> [OverPair] -> IO PhraCate
findPhraWithLowestPrio unCheckedOps ops overPairs = do
    if ops == []
      then return nilPhra      -- This is the border condition, usually not occurs.
      else do
        let x = head ops
        let xs = [op| op <-unCheckedOps, op /= x]
        let pc1 = fst x
        let pc2 = snd x
        pri <- getPrior overPairs pc1 pc2                              -- Find priority from a list of 'OverPair'
        let pcps1 = [y| y <- xs, (fst y == pc1) || (snd y == pc1)]     -- [(PhraCate,PhraCate)] related with pc1
        let pcps2 = [y| y <- xs, (fst y == pc2) || (snd y == pc2)]     -- [(PhraCate,PhraCate)] related with pc2
        if pri == Lp && pcps2 /= []
          then findPhraWithLowestPrio xs pcps2 overPairs
          else if pri == Lp && pcps2 == []
                 then return pc2
                 else if pri == Rp && pcps1 /= []
                        then findPhraWithLowestPrio xs pcps1 overPairs
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
    | st1 == st2 && st1 + sp1 < st2 + sp2 && notElem' pc2 (findDescen pc1 pcs) = 3      -- Left-inclusive overlap
    | st1 < st2  && st1 + sp1 == st2 + sp2 && notElem' pc1 (findDescen pc2 pcs) = 4     -- Right-inclusive overlap
    | st1 < st2  && st1 + sp1 > st2 + sp2 && notElem' pc1 (findDescen pc2 pcs) = 5      -- Two-end inclusive overlap
    | otherwise = 0                        -- Non-overlap or blood relation
    where
    st1 = stOfCate pc1
    sp1 = spOfCate pc1
    st2 = stOfCate pc2
    sp2 = spOfCate pc2

{- Remove a phrasal category together with its descendants.
 - Do not update activities of phrasal categories, because there might be ambiguities among the remaining phrases.
 - Original definition: removeOnePC pc clo = updateAct [x| x <- clo, notElem x (pc:descens)]
 - Here, "newCbs" is the set of newly generated phrases.
 -}
removeOnePC :: PhraCate -> [PhraCate] -> [PhraCate] -> [PhraCate]
removeOnePC pc clo newCbs
    | elem pc newCbs = [x| x <- clo, x /= pc]                                   -- Phrase <pc> is a newly genreated phrase.
    | otherwise = [x| x <- clo, notElem x (pc:descens)]
    where
      descens = findDescen pc clo                   -- Descendants of 'pc'

{- Check every category among a transitive result, and set its correct activity. Before a trip of transition, parsing
   tree is partial and has been pruned. Just after a trip of transition, some phrases in parsing tree need change
   their activities. For those phrases taking part in other phrases, they are set inactive, and the others are set
   active.
   Here, every phrasal category is atomic, namely has ONLY ONE element in its CTSPA component.
   To be fixed: If phrases A and B overlap each other with Type 1 (Equal overlap), and have same syntactic type, then they would be constituents of same phrase, which could not be distincted by 'findSplitCate'.
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
                                           -- Using pcBelong' instead of pcBelong is for neglecting the active attribute.
      where
        st1 = stOfCate pc
        st2 = ssOfCate pc
        sp1 = st2 - st1 - 1                -- When pc is a leaf, sp1 will be -1.
        sp2 = spOfCate pc - sp1 - 1        -- When pc is a leaf, sp2 will be 0, that is, the second parent is pc.
        pcTuples = [(x, y) | x <- (getPhraBySS (st1, sp1) clo), y <- (getPhraBySS (st2, sp2) clo)]
                                           -- When pc is a leaf, pcTuples will be [] because span -1 does not exist.
        tags = splitAtDeli '-' ((taOfCate pc)!!0)    -- All tags used
        cctags = [x| x <- tags, elem x ccTags]       -- Tags of syntax-typed tranformations, maybe empty.
        onOff = updateOnOff [] (map ("+" ++) cctags)

{- Find descendants of a given phrasal category from the transitive closure of phrasal categories.
 - Here, phrase B is a child of phrase A only if B can be obtained from A and anthor phrase via Function "cateComb".
 - Probably, B is not a real child of A instead generated from other phrases.
 -}
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

-- Show all phrases.
showNPhraCate' :: [PhraCate] -> IO ()
showNPhraCate' [] = return ()
showNPhraCate' (pc:pcs) = do
    showPhraCate' pc
    putStrLn ""
    showNPhraCate' pcs

showPhraCate' :: PhraCate -> IO ()
showPhraCate' pc = do
--  putStr (show pc)       -- Function 'show' converts Chinese characters to [char].
    putStr $ "((" ++ show (stOfCate pc) ++ "," ++ show (spOfCate pc) ++ "),["
    putCtsca' (ctspaOfCate pc)
    putStr $ "]," ++ show (ssOfCate pc) ++ ")"

putCtsca' :: [(Category,Tag,Seman,PhraStru,Act)] -> IO ()
putCtsca' [] = putStr ""
putCtsca' [x] = putStr $ "(" ++ show (fst5 x) ++ "," ++ (snd5 x) ++ "," ++ (thd5 x) ++ "," ++ (fth5 x) ++ "," ++ show (fif5 x) ++ ")"
putCtsca' (x:xs) = do
    putStr $ "(" ++ show (fst5 x) ++ "," ++ (snd5 x) ++ "," ++ (thd5 x) ++ "," ++ (fth5 x) ++ "," ++ show (fif5 x) ++ "),"
    putCtsca' xs
