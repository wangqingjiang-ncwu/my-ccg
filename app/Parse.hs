-- Copyright (c) 2019-2025 China University of Water Resources and Electric Power
-- All rights reserved.

module Parse (
    cateComb,          -- OnOff -> PhraCate -> PhraCate -> PhraCate
    initPhraCate,      -- [(Category, Seman)] -> [PhraCate]
    trans,             -- OnOff -> [PhraCate] -> [[PhraCate]] -> [PhraCate]
    transWithPruning,  -- [Rule] -> [PhraCate] -> [[PhraCate]] -> [OverPair] -> IO ([PhraCate],[[PhraCate]])
--  parse,             -- OnOff -> [PhraCate] -> [PhraCate] -> [PhraCate]
    prune,             -- [PhraCate] -> [PhraCate]
--  prune',            -- [PhraCate] -> [PhraCate] -> IO ([PhraCate],[PhraCate])
    getOverlap,        -- [PhraCate] -> [(PhraCate,PhraCate)]
    findPhraWithLowestPrio,  -- [(PhraCate,PhraCate)] -> [(PhraCate,PhraCate)] -> [OverPair] -> IO (PhraCate, PhraCate)
    getPrior,          -- [PhraCate] -> PhraCate -> PhraCate -> IO Prior
    getPrior',         -- [OverPair] -> PhraCate -> PhraCate -> Maybe Prior
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
    uniTwoTree,        -- [PhraCate] -> [PhraCate] -> [PhraCate]
    findCate           -- (Start, Span) -> [PhraCate] -> [PhraCate]
    ) where

import Control.Monad
import Data.Tuple
import Data.Tuple.Utils
import Data.List
import Category
import Phrase
import Rule
import AmbiResol (OverPair,Prior(..))
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

{- Use S/s only when subject-predicate structure appears at subject's position.
 -}
      s_S_SP = removeDup [(npCate, snd3 csp, thd3 csp) | csp <- csp1, fst3 csp == sCate]
      ctspaBysToS_SP = [rule cate1 cate2 | rule <- [appB,raiFh], cate1 <- s_S_SP, cate2 <- csp_2, elem Ss onOff]
          where
          csp_2 = removeDup [x| x <- csp2, elem True (map (\y-> cateEqual y (fst3 x)) vCate)]
      ctspaBysToS = ctspaBysToS_SP
      catesBysToS = [(fst5 cate, "S/s-" ++ snd5 cate, thd5 cate, fth5 cate, fif5 cate) | cate <- ctspaBysToS]

{- Use P/s only when
 - (1) subject-predicate structure appears at predicate position,
 - (2) subject-predicate structure occupies the head-word position of DHv,
 - (2) a phrase with structure XX follows.
 -}
      s_P_SP = removeDup [(predCate, snd3 csp, thd3 csp) | csp <- csp2, fst3 csp == sCate]
      ctspaBysToP_SP = [rule cate1 cate2 | rule <- [appB], cate1 <- csp1, cate2 <- s_P_SP, elem Ps onOff]
          where
          csp_1 = removeDup [x| x<- csp1, fst3 x == npCate]
      s_P_DHv = removeDup [(predCate, snd3 csp, thd3 csp) | csp <- csp2, fst3 csp == sCate]
      ctspaBysToP_DHv = [rule cate1 cate2 | rule <- [appF], cate1 <- csp1, cate2 <- s_P_DHv, elem Ps onOff]
          where
          csp_1 = removeDup [x| x<- csp1, fst3 x == advCate]
      s_P_XX = removeDup [(predCate, snd3 csp, thd3 csp) | csp <- csp1, fst3 csp == sCate]
      ctspaBysToP_XX = [rule cate1 cate2 | rule <- [appB], cate1 <- s_P_XX, cate2 <- csp_2, elem Ps onOff]
          where
          csp_2 = removeDup [x| x<- csp2, thd3 x == "HX"]
      s_P_HX = removeDup [(predCate, snd3 csp, thd3 csp) | csp <- csp2, fst3 csp == sCate]
      ctspaBysToP_HX = [rule cate1 cate2 | rule <- [appF], cate1 <- csp_1, cate2 <- s_P_HX, elem Ps onOff]
          where
          csp_1 = removeDup [x| x<- csp1, fst3 x == conjCate]
      ctspaBysToP = ctspaBysToP_SP ++ ctspaBysToP_DHv ++ ctspaBysToP_XX ++ ctspaBysToP_HX
      catesBysToP = [(fst5 cate, "P/s-" ++ snd5 cate, thd5 cate, fth5 cate, fif5 cate) | cate <- ctspaBysToP]

{- Use O/s only when subject-predicate structure appears at object's position.
 -}
      s_O_VO = removeDup [(npCate, snd3 csp, thd3 csp) | csp <- csp2, fst3 csp == sCate]
      ctspaBysToO_VO = [rule cate1 cate2 | rule <- [appF], cate1 <- csp_1, cate2 <- s_O_VO, elem Os onOff]
          where
          vCate2 = [verbCate, verbCate2]
          csp_1 = removeDup [x| x <- csp1, elem True (map (\y-> cateEqual y (fst3 x)) vCate2)]
      s_O_PO = removeDup [(npCate, snd3 csp, thd3 csp) | csp <- csp2, fst3 csp == sCate]
      ctspaBysToO_PO = [rule cate1 cate2 | rule <- [appF], cate1 <- csp_1, cate2 <- s_O_PO, elem Os onOff]         -- Prepositional object
          where
          csp_1 = removeDup [x| x <- csp1, fst3 x == prep2AdvCate]
      s_O_MOv = removeDup [(npCate, snd3 csp, thd3 csp) | csp <- csp2, fst3 csp == sCate]
      ctspaBysToO_MOv = [rule cate1 cate2 | rule <- [raiBh2], cate1 <- csp_1, cate2 <- s_O_MOv, elem Os onOff]     -- Prepositional object
          where
          csp_1 = removeDup [x| x <- csp1, fst3 x == prep4BaCate]
      ctspaBysToO = ctspaBysToO_VO ++ ctspaBysToO_PO ++ ctspaBysToO_MOv
      catesBysToO = [(fst5 cate, "O/s-" ++ snd5 cate, thd5 cate, fth5 cate, fif5 cate) | cate <- ctspaBysToO]

{- Use A/s only when subject-predicate structure appears at attribue's position.
 -}
      s_A_AHn = removeDup [(adjCate, snd3 csp, thd3 csp) | csp <- csp1, fst3 csp == sCate]
      ctspaBysToA = [rule cate1 cate2 | rule <- [appF], cate1 <- s_A_AHn, cate2 <- csp2, fst3 cate2 == npCate, elem As onOff]
      catesBysToA = [(fst5 cate, "A/s-" ++ snd5 cate, thd5 cate, fth5 cate, fif5 cate) | cate <- ctspaBysToA]

{- Use Hn/s only when subject-predicate structure appears at
 - headword of AHn,
 - headword of HnC.
 -}
      s_Hn_AHn = removeDup [(npCate, snd3 csp, thd3 csp) | csp <- csp2, fst3 csp == sCate]
      ctspaBysToHn_AHn = [rule cate1 cate2 | rule <- [appF], cate1 <- csp_1, cate2 <- s_Hn_AHn, elem Hns onOff]
          where
          csp_1 = removeDup [x| x <- csp1, cateEqual (fst3 x) adjCate]          -- Including np/*np in structure U1P.
      s_Hn_HnC = removeDup [(npCate, snd3 csp, thd3 csp) | csp <- csp1, fst3 csp == sCate]
      ctspaBysToHn_HnC = [rule cate1 cate2 | rule <- [appB], cate1 <- s_Hn_HnC, cate2 <- csp_2, elem Hns onOff]
          where
          csp_2 = removeDup [x| x <- csp2, fst3 x == nounCompCate]
      ctspaBysToHn = ctspaBysToHn_AHn ++ ctspaBysToHn_HnC
      catesBysToHn = [(fst5 cate, "Hn/s-" ++ snd5 cate, thd5 cate, fth5 cate, fif5 cate) | cate <- ctspaBysToHn]

{- Use N/s only when
 - (1) "<conjunction> s",
 - (2) "s np\*np", where the phrase with category np\*np has structure HX;
 - (3) "np/*np s", where the phrase with category np/*np has structure U1P; obsoleted!
 - (4) "s 的",
 - (5) "'把' s", obsoleted!
 -}
      s_N_Conj = removeDup [(npCate, snd3 csp, thd3 csp) | csp <- csp2, fst3 csp == sCate]
      ctspaBysToN_Conj = [rule cate1 cate2 | rule <- [appF], cate1 <- csp_1, cate2 <- s_N_Conj, elem Ns onOff]
          where
          csp_1 = removeDup [x| x <- csp1, fst3 x == conjCate]
      s_N_XX = removeDup [(npCate, snd3 csp, thd3 csp) | csp <- csp1, fst3 csp == sCate]
      ctspaBysToN_XX = [rule cate1 cate2 | rule <- [appB], cate1 <- s_N_XX, cate2 <- csp_2, elem Ns onOff]
          where
          csp_2 = removeDup [x| x <- csp2, fst3 x == ndCate, thd3 x == "HX"]
{-
      s_N_AHn = removeDup [(npCate, snd3 csp, thd3 csp) | csp <- csp2, fst3 csp == sCate]                                -- replaced with Hn/s
      ctspaBysToN_AHn = [rule cate1 cate2 | rule <- [appF], cate1 <- csp_1, cate2 <- s_N_AHn, elem Ns onOff]
          where
          csp_1 = removeDup [x| x <- csp1, thd3 x == "U1P"]
 -}
      s_N_U1P = removeDup [(npCate, snd3 csp, thd3 csp) | csp <- csp1, fst3 csp == sCate]
      ctspaBysToN_U1P = [rule cate1 cate2 | rule <- [appB], cate1 <- s_N_U1P, cate2 <- csp_2, elem Ns onOff]
          where
          csp_2 = removeDup [x| x <- csp2, fst3 x == aux1Cate]
{-
      s_N_Ba = removeDup [(npCate, snd3 csp, thd3 csp) | csp <- csp2, fst3 csp == sCate]                                 -- replaced with O/s
      ctspaBysToN_Ba = [rule cate1 cate2 | rule <- [raiBh2], cate1 <- csp_1, cate2 <- s_N_Ba, elem Ns onOff]
          where
          csp_1 = removeDup [x| x <- csp1, fst3 x == prep4BaCate]
 -}
      ctspaBysToN = ctspaBysToN_Conj ++ ctspaBysToN_XX ++ ctspaBysToN_U1P
      catesBysToN = [(fst5 cate, "N/s-" ++ snd5 cate, thd5 cate, fth5 cate, fif5 cate) | cate <- ctspaBysToN]

{- According to Jia-xuan Shen's theory, successive inclusions from noun to verb, and to adjective, and non-inflectionship
   of Chinese words, the following syntax-typed conversions exist,
   S/s, P/s, O/s, A/s, Hn/s, N/s,
   S/v, O/v, A/v, Hn/v, D/v, Cn/v, Cv/v, N/v,
   P/vt, OE/vt, Vt/vi, A/vd,
   S/a, P/a, V/a, O/a, D/a, Da/a, Cn/a, Cv/a, Ca/a, Hn/a, N/a,
   P/n, V/n, A/n, Cn/n, Cv/n, D/n, Da/n, ADJ/n,
   S/nd, O/nd, Hn/nd,
   S/d, O/d, A/d, Hn/d, Cv/d, N/d, Da/d, Ds/d, Dx/d, Doe/d,
   D/p,
   O/oe, Hn/oe, N/oe,
   N/pe, A/q, Jf/c, Jb/c, U3d/u3.
   Besides, two adjacent syntactic types can convert to their new types respectively and simultaneously,
   such as "np np/.np -> np/.np np" noted as A/n-Hn/a. When used with some standard rules, two-typed combination is
   labelled as "N/v-"++<tag>, "A/v-"++<tag>, and so on. Now, type conversions only happen in
   acting as particular syntactic constituents.
 -}

 {- The conversion "S/v" from verb's types to "np" happens when verbal phrase occupies subject position.
  -}
      v_S_SP = removeDup [(npCate, snd3 csp, thd3 csp)|csp <- csp1, elem True (map (\x-> cateEqual x (fst3 csp)) vCate)]
      ctspaByvToS_SP = [rule cate1 cate2 | rule <- [appB,raiFh], cate1 <- v_S_SP, cate2 <- csp_2, elem Sv onOff]
          where
          csp_2 = removeDup [x| x <- csp2, elem True (map (\y-> cateEqual y (fst3 x)) vCate)]
      ctspaByvToS = ctspaByvToS_SP
      catesByvToS = [(fst5 cate, "S/v-" ++ snd5 cate, thd5 cate, fth5 cate, fif5 cate) | cate <- ctspaByvToS]

{- The conversion "O/v" from verb's types to "np" happens when verbal phrase occupies object position
 - (1) verb's object,
 - (2) object of prepositions except of preposition '把',
 - (3) object of preposition '把'.
 -}
      v_O_VO = removeDup [(npCate, snd3 csp, thd3 csp) | csp <- csp2, elem True (map (\x-> cateEqual x (fst3 csp)) vCate)]
      ctspaByvToO_VO = [rule cate1 cate2 | rule <- [appF], cate1 <- csp_1, cate2 <- v_O_VO, elem Ov onOff]
          where
          csp_1 = removeDup [x| x <- csp1, elem True (map (\y-> cateEqual y (fst3 x)) vpCate)]
      v_O_PO = removeDup [(npCate, snd3 csp, thd3 csp) | csp <- csp2, elem True (map (\x-> cateEqual x (fst3 csp)) vCate)]        -- Prepositional object
      ctspaByvToO_PO = [rule cate1 cate2 | rule <- [appF], cate1 <- csp_1, cate2 <- v_O_PO, elem Ov onOff]
          where
          csp_1 = removeDup [x| x <- csp1, fst3 x == prep2AdvCate]
      v_O_MOv = removeDup [(npCate, snd3 csp, thd3 csp) | csp <- csp2, elem True (map (\x-> cateEqual x (fst3 csp)) vCate)]        -- object of '把'
      ctspaByvToO_MOv = [rule cate1 cate2 | rule <- [raiBh2], cate1 <- csp_1, cate2 <- v_O_MOv, elem Ov onOff]
          where
          csp_1 = removeDup [x| x <- csp1, fst3 x == prep4BaCate]
      ctspaByvToO = ctspaByvToO_VO ++ ctspaByvToO_PO ++ ctspaByvToO_MOv
      catesByvToO = [(fst5 cate, "O/v-" ++ snd5 cate, thd5 cate, fth5 cate, fif5 cate) | cate <- ctspaByvToO]

{- The conversion from verbal types to np/.np happens when the verb occupies attribute position,
 - or used to form '得' structure.
 -}
      v_A_AHn = removeDup [(adjCate, snd3 csp, thd3 csp) | csp <- csp1, elem True (map (\x-> cateEqual x (fst3 csp)) vCate)]
      ctspaByvToA_AHn = [rule cate1 cate2 | rule <- [appF], cate1 <- v_A_AHn, cate2 <- csp2, fst3 cate2 == npCate, elem Av onOff]
      v_A_U3P = removeDup [(adjCate, snd3 csp, thd3 csp) | csp <- csp2, elem True (map (\x-> cateEqual x (fst3 csp)) vCate)]
      ctspaByvToA_U3P = [rule cate1 cate2 | rule <- [appF], cate1 <- csp1, fst3 cate1 == aux3Cate, cate2 <- v_A_U3P, elem Av onOff]
      ctspaByvToA = ctspaByvToA_AHn ++ ctspaByvToA_U3P
      catesByvToA = [(fst5 cate, "A/v-" ++ snd5 cate, thd5 cate, fth5 cate, fif5 cate) | cate <- ctspaByvToA]

{- The conversion from verb to noun happens when the verb occupies nominal head word position of structure AHn and HnC.
 - ctspaByvToN_XX and ctspaByvToN_HnC will create two phrase with different structure. One is HX, and the other is HnC.
 -}
      v_Hn_AHn = removeDup [(npCate, snd3 csp, thd3 csp) | csp <- csp2, elem True (map (\x-> cateEqual x (fst3 csp)) vCate)]
      ctspaByvToHn_AHn = [rule cate1 cate2 | rule <- [appF], cate1 <- csp_1, cate2 <- v_Hn_AHn, elem Hnv onOff]
          where
          csp_1 =  removeDup [(adjCate, snd3 csp, thd3 csp) | csp <- csp1, cateEqual (fst3 csp) adjCate]
      v_Hn_HnC = removeDup [(npCate, snd3 csp, thd3 csp) | csp <- csp1, elem True (map (\x-> cateEqual x (fst3 csp)) vCate)]
      ctspaByvToHn_HnC = [rule cate1 cate2 | rule <- [appB], cate1 <- v_Hn_HnC, cate2 <- csp2, fst3 cate2 == nounCompCate, elem Hnv onOff]
      ctspaByvToHn = ctspaByvToHn_AHn ++ ctspaByvToHn_HnC
      catesByvToHn = [(fst5 cate, "Hn/v-" ++ snd5 cate, thd5 cate, fth5 cate, fif5 cate) | cate <- ctspaByvToHn]

-- The conversion from verb to adverb happens when the verb occupies adverbial position.
      v_D_DHv = removeDup [(advCate, snd3 csp, thd3 csp) | csp <- csp1, elem True (map (\x-> cateEqual x (fst3 csp)) vCate)]
      ctspaByvToD_DHv = [rule cate1 cate2 | rule <- [appF,comFh], cate1 <- v_D_DHv, cate2 <- csp_2, elem Dv onOff]
          where
          csp_2 = removeDup [x| x <- csp2, elem True (map (\y-> cateEqual y (fst3 x)) vCate)]
      v_D_DHd = removeDup [(advCate, snd3 csp, thd3 csp) | csp <- csp1, elem True (map (\x-> cateEqual x (fst3 csp)) vCate)]
      ctspaByvToD_DHd = [rule cate1 cate2 | rule <- [comFh], cate1 <- v_D_DHd, cate2 <- csp_2, elem Dv onOff]             -- appF is removed, 2024-9-15.
          where
          csp_2 = removeDup [x| x <- csp2, fst3 x == advCate]
      ctspaByvToD = ctspaByvToD_DHv ++ ctspaByvToD_DHd
      catesByvToD = [(fst5 cate, "D/v-" ++ snd5 cate, thd5 cate, fth5 cate, fif5 cate) | cate <- ctspaByvToD]

-- The conversion from verb types to noun-complemented types happens when verb phrases occupy noun-complemented position.
      v_Cn_HnC = removeDup [(nounCompCate, snd3 csp, thd3 csp) | csp <- csp2, elem True (map (\x-> cateEqual x (fst3 csp)) vCate)]
      ctspaByvToCn_HnC = [rule cate1 cate2 | rule <- [appB], cate1 <- csp_1, cate2 <- v_Cn_HnC, elem Cnv onOff]
          where
          csp_1 = removeDup [x| x <- csp1, fst3 x == npCate]
      ctspaByvToCn = ctspaByvToCn_HnC
      catesByvToCn = [(fst5 cate, "Cn/v-" ++ snd5 cate, thd5 cate, fth5 cate, fif5 cate) | cate <- ctspaByvToCn]

-- The conversion from verb types to verb-complemented types happens when verb phrases occupy verb-complemented position.
      v_Cv_HvC = removeDup [(verbCompCate, snd3 csp, thd3 csp) | csp <- csp2, elem True (map (\x-> cateEqual x (fst3 csp)) vCate)]
      ctspaByvToCv_HvC = [rule cate1 cate2 | rule <- [appB,comBc], cate1 <- csp_1, cate2 <- v_Cv_HvC, elem Cvv onOff]
          where
          csp_1 = removeDup [x| x <- csp1, elem True (map (\y-> cateEqual y (fst3 x)) vCate)]
      ctspaByvToCv = ctspaByvToCv_HvC
      catesByvToCv = [(fst5 cate, "Cv/v-" ++ snd5 cate, thd5 cate, fth5 cate, fif5 cate) | cate <- ctspaByvToCv]

{- The conversion from verb to noun happens when
 - (1) '的' structure U1P,
 - (2) Coordination structure XX，
 -}
      v_N_U1P = removeDup [(npCate, snd3 csp, thd3 csp) | csp <- csp1, elem True (map (\x-> cateEqual x (fst3 csp)) vCate)]
      ctspaByvToN_U1P = [rule cate1 cate2 | rule <- [appB], cate1 <- v_N_U1P, cate2 <- csp2, fst3 cate2 == aux1Cate, elem Nv onOff]

-- When a verb follows conjunction (X\*X)/*X.
      v_N_HX = removeDup [(npCate, snd3 csp, thd3 csp) | csp <- csp2, elem True (map (\x-> cateEqual x (fst3 csp)) vCate)]
      ctspaByvToN_HX = [rule cate1 cate2 | rule <- [appF], cate1 <- csp1, fst3 cate1 == conjCate, cate2 <- v_N_HX, elem Nv onOff]

-- When a verb is followed by backward conjunction X\*X.
      v_N_CC = removeDup [(npCate, snd3 csp, thd3 csp) | csp <- csp1, elem True (map (\x-> cateEqual x (fst3 csp)) vCate)]
      ctspaByvToN_CC = [rule cate1 cate2 | rule <- [appB], cate1 <- v_N_CC, cate2 <- csp2, fst3 cate2 == conjCate4Backward, elem Nv onOff]

-- When a verb is followed by HX-structured phrases and they are type np\*np.
      v_N_XX = removeDup [(npCate, snd3 csp, thd3 csp) | csp <- csp1, elem True (map (\x-> cateEqual x (fst3 csp)) vCate)]
      ctspaByvToN_XX = [rule cate1 cate2 | rule <- [appB], cate1 <- v_N_XX, cate2 <- csp2, fst3 cate2 == nounCompCate, thd3 cate2 == "HX", elem Nv onOff]

      ctspaByvToN = ctspaByvToN_U1P ++ ctspaByvToN_HX ++ ctspaByvToN_CC ++ ctspaByvToN_XX
      catesByvToN = [(fst5 cate, "N/v-" ++ snd5 cate, thd5 cate, fth5 cate, fif5 cate) | cate <- ctspaByvToN]

{- The conversion from transitive verb's types to predicate type happens when
 - (1) The verb's type follows conjunction type (X\*X)/*X,
 - (2) A phrase with structure HX follows,
 - (3) An object acts as the subject, and no '被' makes the verb change into a predicate, namely “受事做主语” (Object as Subject).
 - (4) The verb's type follows adverb type (s\.np)/#(s\.np),
 - (5) '被' makes transitive verbs change into a predicate. This rule is obsoleted.
 -}
      vt_P_Conj = removeDup [(predCate, snd3 csp, thd3 csp) | csp <- csp2, elem True (map (\x-> cateEqual x (fst3 csp)) [verbCate,verbCate2])]
      ctspaByvtToP_Conj = [rule cate1 cate2 | rule <- [appF], cate1 <- csp_1, cate2 <- vt_P_Conj, elem Pvt onOff]
          where
          csp_1 = removeDup [x| x <- csp1, fst3 x == conjCate]
      vt_P_XX = removeDup [(predCate, snd3 csp, thd3 csp) | csp <- csp1, elem True (map (\x-> cateEqual x (fst3 csp)) [verbCate,verbCate2])]
      ctspaByvtToP_XX = [rule cate1 cate2 | rule <- [appB], cate1 <- vt_P_XX, cate2 <- csp_2, elem Pvt onOff]
          where
          csp_2 = removeDup [x| x <- csp2, thd3 x == "HX"]
      vt_P_SP = removeDup [(predCate, snd3 csp, thd3 csp) | csp <- csp2, elem True (map (\x-> cateEqual x (fst3 csp)) [verbCate,verbCate2])]
      ctspaByvtToP_SP = [rule cate1 cate2 | rule <- [appB], cate1 <- csp_1, cate2 <- vt_P_SP, elem Pvt onOff]
          where
          csp_1 = removeDup [x| x <- csp1, fst3 x == npCate]
      vt_P_DHv = removeDup [(predCate, snd3 csp, thd3 csp) | csp <- csp2, elem True (map (\x-> cateEqual x (fst3 csp)) [verbCate,verbCate2])]
      ctspaByvtToP_DHv = [rule cate1 cate2 | rule <- [appF], cate1 <- csp_1, cate2 <- vt_P_SP, elem Pvt onOff]
          where
          csp_1 = removeDup [x| x <- csp1, fst3 x == advCate]
{-
 - Early category of '被' was advCate. When modifing a transitive verb without object, the verb was converted into predicate verb.
 - Present category of '被' is (s/#(s/.np))\.np. When modifing a transitive verb without object, the verb is converted into OE category s/.np.
 - So P/vt is not ever used to '被' + transitive verb.

      vt_P_Bei = removeDup [(predCate, snd3 csp, thd3 csp) | csp <- csp2, elem True (map (\x-> cateEqual x (fst3 csp)) [verbCate,verbCate2])]
      ctspaByvtToP_Bei = [rule cate1 cate2 | rule <- [appF], cate1 <- csp_1, cate2 <- vt_P_Bei, elem Pvt onOff]
          where
          csp_1 = removeDup [x| x <- csp1, fst3 x == advCate]
 -}
      ctspaByvtToP = ctspaByvtToP_Conj ++ ctspaByvtToP_XX ++ ctspaByvtToP_SP ++ ctspaByvtToP_DHv
      catesByvtToP = [(fst5 cate, "P/vt-" ++ snd5 cate, thd5 cate, fth5 cate, fif5 cate) | cate <- ctspaByvtToP]

{- The conversion from transitive verb's types to object-extractioned type happens when the preposition '被' type happens to the left of this verb.
 -}
      vt_OE = removeDup [(objectExtractionCate, snd3 csp, thd3 csp) | csp <- csp2, elem True (map (\x-> cateEqual x (fst3 csp)) [verbCate,verbCate2])]
      ctspaByvtToOE = [rule cate1 cate2 | rule <- [raiBh], cate1 <- csp_1, cate2 <- vt_OE, elem OEvt onOff]
          where
          csp_1 = removeDup [x| x <- csp1, fst3 x == prep4BeiCate]
      catesByvtToOE = [(fst5 cate, "OE/vt-" ++ snd5 cate, thd5 cate, fth5 cate, fif5 cate) | cate <- ctspaByvtToOE]

{- The conversion from intransitive verb's type to transitive verb's type happens when
   (1) The intransitive verb is followed by a phrase with structure "HX" or with category "np";
   (2) The intransitive verb follows a phrase with category "np".
 -}
      vi_Vt_XX = removeDup [(verbCate, snd3 csp, thd3 csp) | csp <- csp1, fst3 csp == predCate]
      ctspaByviToVt_XX = [rule cate1 cate2 | rule <- [appB], cate1 <- vi_Vt_XX, cate2 <- csp_2, elem Vtvi onOff]
          where
          csp_2 = removeDup [x| x <- csp2, thd3 x == "HX"]
      vi_Vt_VO = removeDup [(verbCate, snd3 csp, thd3 csp) | csp <- csp1, fst3 csp == predCate]
      ctspaByviToVt_VO = [rule cate1 cate2 | rule <- [appF], cate1 <- vi_Vt_VO, cate2 <- csp_2, elem Vtvi onOff]
          where
          csp_2 = removeDup [x| x <- csp2, fst3 x == npCate]
      vi_Vt_OE = removeDup [(verbCate, snd3 csp, thd3 csp) | csp <- csp2, fst3 csp == predCate]
      ctspaByviToVt_OE = [rule cate1 cate2 | rule <- [raiFh], cate1 <- csp_1, cate2 <- vi_Vt_OE, elem Vtvi onOff]
          where
          csp_1 = removeDup [x| x <- csp1, fst3 x == npCate]
      ctspaByviToVt =  ctspaByviToVt_XX ++ ctspaByviToVt_VO ++ ctspaByviToVt_OE
      catesByviToVt = [(fst5 cate, "Vt/vi-" ++ snd5 cate, thd5 cate, fth5 cate, fif5 cate) | cate <- ctspaByviToVt]

{- The conversion from verb-complemented type to adjective type happens when
 - A directional verb follows the auxiliary word '得', such as "回vt 得u3 来vd".
 -}
      vd_A = removeDup [(adjCate, snd3 csp, thd3 csp) | csp <- csp2, fst3 csp == verbCompCate]
      ctspaByvdToA = [rule cate1 cate2 | rule <- [appF], cate1 <- csp_1, cate2 <- vd_A, elem Avd onOff]
          where
          csp_1 = removeDup [x| x <- csp1, fst3 x == aux3Cate]
      catesByvdToA = [(fst5 cate, "A/vd-" ++ snd5 cate, thd5 cate, fth5 cate, fif5 cate) | cate <- ctspaByvdToA]

{- The conversion from adjective to noun happens when the adjective occupies subject position.
 -}
      a_S_SP = removeDup [(npCate, snd3 csp, thd3 csp) | csp <- csp1, cateEqual (fst3 csp) adjCate]
      ctspaByaToS_SP = [rule cate1 cate2 | rule <- [appB, raiFh], cate1 <- a_S_SP, cate2 <- csp_2, elem Sa onOff]
          where
          csp_2 = removeDup [x| x <- csp2, elem True (map (\y-> cateEqual y (fst3 x)) vCate)]
      ctspaByaToS = ctspaByaToS_SP
      catesByaToS = [(fst5 cate, "S/a-" ++ snd5 cate, thd5 cate, fth5 cate, fif5 cate) | cate <- ctspaByaToS]

{- The conversion from adjective to predicate happens when
 - (1) The adjective occupies predicate position,
 - (2) The adjective occupies headword position of DHv,
 - (3) The adjective occupies headword position of HvC, such as, "高兴a 一辈子mq", "会d 高兴a 一辈子", "高兴a 极d 了u4";
 - (4) The conversion also happens when the adjective follows <conjCate> or <conjCate4Backward> follows the adjective, and the coordination phrase acts as predicate.
 -     such as, "迷惘a、孤独a" => "迷惘a、孤独v" => "迷惘v、孤独v"
 -}
      a_P_SP = removeDup [(predCate, snd3 csp, thd3 csp) | csp <- csp2, cateEqual (fst3 csp) adjCate]
      ctspaByaToP_SP = [rule cate1 cate2 | rule <- [appB], cate1 <- csp_1, cate2 <- a_P_SP, elem Pa onOff]
          where
          csp_1 = removeDup [x| x<- csp1, fst3 x == npCate]
      a_P_DHv = removeDup [(predCate, snd3 csp, thd3 csp) | csp <- csp2, cateEqual (fst3 csp) adjCate]
      ctspaByaToP_DHv = [rule cate1 cate2 | rule <- [appF], cate1 <- csp_1, cate2 <- a_P_DHv, elem Pa onOff]
          where
          csp_1 = removeDup [x| x<- csp1, fst3 x == advCate]
      a_P_HvC = removeDup [(predCate, snd3 csp, thd3 csp) | csp <- csp1, cateEqual (fst3 csp) adjCate]
      ctspaByaToP_HvC = [rule cate1 cate2 | rule <- [appB], cate1 <- a_P_HvC, cate2 <- csp_2, elem Pa onOff]
          where
          csp_2 = removeDup [x| x<- csp2, fst3 x == verbCompCate]
      a_P_HX = removeDup [(predCate, snd3 csp, thd3 csp) | csp <- csp2, cateEqual (fst3 csp) adjCate]
      ctspaByaToP_HX = [rule cate1 cate2 | rule <- [appF], cate1 <- csp_1, cate2 <- a_P_HX, elem Pa onOff]
          where
          csp_1 = removeDup [x| x<- csp1, fst3 x == conjCate]
      a_P_XX = removeDup [(predCate, snd3 csp, thd3 csp) | csp <- csp1, cateEqual (fst3 csp) adjCate]
      ctspaByaToP_XX = [rule cate1 cate2 | rule <- [appB], cate1 <- a_P_XX, cate2 <- csp_2, elem Pa onOff]
          where
          csp_2 = removeDup [x| x<- csp2, fst3 x == getCateFromString "(s\\.np)\\*(s\\.np)", thd3 x == "HX"]
      ctspaByaToP = ctspaByaToP_SP ++ ctspaByaToP_DHv ++ ctspaByaToP_HvC ++ ctspaByaToP_HX ++ ctspaByaToP_XX
      catesByaToP = [(fst5 cate, "P/a-" ++ snd5 cate, thd5 cate, fth5 cate, fif5 cate) | cate <- ctspaByaToP]

{- During the past, adjective is only considered to use predicate's syntax type. Now adjective is also allowed to use transitive verbs's syntax types.
 - To restrict syntactic ambiguity, the conversions only happen when the adjective occupies verb position or headword position of DHv or HvC.
 - Such as, "快乐a 着u4 你的快乐np", "要vu 快乐a 自己r". The conversion should be seldom used.
 - The conversion also happens when the adjective occupies
 -}
      a_V_VO = removeDup [(verbCate, snd3 csp, thd3 csp) | csp <- csp1, cateEqual (fst3 csp) adjCate]
      ctspaByaToV_VO = [rule cate1 cate2 | rule <- [appF], cate1 <- a_V_VO, cate2 <- csp_2, elem Va onOff]
          where
          csp_2 = removeDup [x| x<- csp2, fst3 x == npCate]

      a_V_OE = removeDup [(verbCate, snd3 csp, thd3 csp) | csp <- csp2, cateEqual (fst3 csp) adjCate]
      ctspaByaToV_OE = [rule cate1 cate2 | rule <- [raiFh], cate1 <- csp_1, cate2 <- a_V_OE, elem Va onOff]
          where
          csp_1 = removeDup [x| x<- csp1, fst3 x == npCate]

      a_V_DHv = removeDup [(verbCate, snd3 csp, thd3 csp) | csp <- csp2, cateEqual (fst3 csp) adjCate]
      ctspaByaToV_DHv = [rule cate1 cate2 | rule <- [comFh], cate1 <- csp_1, cate2 <- a_V_DHv, elem Va onOff]
          where
          csp_1 = removeDup [x| x<- csp1, fst3 x == advCate]

      a_V_HvC = removeDup [(verbCate, snd3 csp, thd3 csp) | csp <- csp1, cateEqual (fst3 csp) adjCate]
      ctspaByaToV_HvC = [rule cate1 cate2 | rule <- [comBc], cate1 <- a_V_HvC, cate2 <- csp_2, elem Va onOff]
          where
          csp_2 = removeDup [x| x<- csp2, fst3 x == verbCompCate]

      ctspaByaToV = ctspaByaToV_VO ++ ctspaByaToV_OE ++ ctspaByaToV_DHv ++ ctspaByaToV_HvC
      catesByaToV = [(fst5 cate, "V/a-" ++ snd5 cate, thd5 cate, fth5 cate, fif5 cate) | cate <- ctspaByaToV]

{- The conversion from adjective to noun happens when
 - (1) The adjective occupies object position,
 - (2) The adjective follows a preposition, such as "比p 收到的a".
 -}
      a_O_VO = removeDup [(npCate, snd3 csp, thd3 csp) | csp <- csp2, cateEqual (fst3 csp) adjCate]
      ctspaByaToO_VO = [rule cate1 cate2 | rule <- [appF], cate1 <- csp_1, cate2 <- a_O_VO, elem Oa onOff]
          where
          vCate2 = [verbCate, verbCate2]
          csp_1 = removeDup [x| x <- csp1, elem True (map (\y-> cateEqual y (fst3 x)) vCate2)]
      a_O_PO = removeDup [(npCate, snd3 csp, thd3 csp) | csp <- csp2, cateEqual (fst3 csp) adjCate]
      ctspaByaToO_PO = [rule cate1 cate2 | rule <- [appF], cate1 <- csp_1, cate2 <- a_O_PO, elem Oa onOff]
          where
          csp_1 = removeDup [x| x<-csp1, fst3 x == prep2AdvCate]
      ctspaByaToO = ctspaByaToO_VO ++ ctspaByaToO_PO
      catesByaToO = [(fst5 cate, "O/a-" ++ snd5 cate, thd5 cate, fth5 cate, fif5 cate) | cate <- ctspaByaToO]

{- The conversion from adjective to adverb happens when
 - (1) The adjective occupies adverbial position to modify a verb,
 - (2) The adjective occupies adverbial position to modify a adverb,
 - (3) The adjective occupies the headword position of DHa.
 -}
      a_D_DHv = removeDup [(advCate, snd3 csp, thd3 csp) | csp <- csp1, cateEqual (fst3 csp) adjCate]
      ctspaByaToD_DHv = [rule cate1 cate2 | rule <- [appF,comFh,comFh2], cate1 <- a_D_DHv, cate2 <- csp_2, elem Da onOff]
          where
          csp_2 = removeDup [x| x<- csp2, elem True (map (\y-> cateEqual y (fst3 x)) vCate)]
      a_D_DHd = removeDup [(advCate, snd3 csp, thd3 csp) | csp <- csp1, cateEqual (fst3 csp) adjCate]
      ctspaByaToD_DHd = [rule cate1 cate2 | rule <- [comFh], cate1 <- a_D_DHd, cate2 <- csp_2, elem Da onOff]
          where
          csp_2 = removeDup [x| x<- csp2, fst3 x == advCate]
      a_Hd_DHd = removeDup [(advCate, snd3 csp, thd3 csp) | csp <- csp2, cateEqual (fst3 csp) adjCate]
      ctspaByaToHd_DHd = [rule cate1 cate2 | rule <- [comFh], cate1 <- csp_1, cate2 <- a_Hd_DHd, elem Da onOff]
          where
          csp_1 = removeDup [x| x<- csp1, fst3 x == advCate]
      ctspaByaToD = ctspaByaToD_DHv ++ ctspaByaToD_DHd ++ ctspaByaToHd_DHd
      catesByaToD = [(fst5 cate, "D/a-" ++ snd5 cate, thd5 cate, fth5 cate, fif5 cate) | cate <- ctspaByaToD]

{- The category conversion from np/.np to (np/.np)/*(np/.np) happens when
 - the adjective occupies adjective-adverbial position.
 -}
      a_Da_DHa = removeDup [(advCate4Adj, snd3 csp, thd3 csp) | csp <- csp1, cateEqual (fst3 csp) adjCate]
      ctspaByaToDa_DHa = [rule cate1 cate2 | rule <- [appF], cate1 <- a_Da_DHa, cate2 <- csp_2, elem Daa onOff]
          where
          csp_2 = removeDup [x| x<- csp2, cateEqual (fst3 x) adjCate]
      ctspaByaToDa = ctspaByaToDa_DHa
      catesByaToDa = [(fst5 cate, "Da/a-" ++ snd5 cate, thd5 cate, fth5 cate, fif5 cate) | cate <- ctspaByaToDa]

-- The category conversion from np/.np or np/*np to np\*np happens when a (measured) numeral occupies completment position.
      a_Cn_HnC = removeDup [(nounCompCate, snd3 csp, thd3 csp) | csp <- csp_2]
          where
          csp_2 = removeDup [x| x <- csp2, elem True (map (\y-> cateEqual y (fst3 x)) [adjCate, numeralCate])]
      ctspaByaToCn_HnC = [rule cate1 cate2 | rule <- [appB], cate1 <- csp_1, cate2 <- a_Cn_HnC, elem Cna onOff]
          where
          csp_1 = removeDup [x| x<- csp1, fst3 x == npCate]

      ctspaByaToCn = ctspaByaToCn_HnC
      catesByaToCn = [(fst5 cate, "Cn/a-" ++ snd5 cate, thd5 cate, fth5 cate, fif5 cate) | cate <- ctspaByaToCn]

-- The category conversion from np/.np to (s\.np)\x(s\.np) happens when the adjective occupies completment position.
      a_Cv_HvC = removeDup [(verbCompCate, snd3 csp, thd3 csp) | csp <- csp2, cateEqual (fst3 csp) adjCate]
      ctspaByaToCv_HvC = [rule cate1 cate2 | rule <- [appB,comBc], cate1 <- csp_1, cate2 <- a_Cv_HvC, elem Cva onOff]
          where
          csp_1 = removeDup [x| x<- csp1, elem True (map (\y-> cateEqual y (fst3 x)) vCate)]
      ctspaByaToCv = ctspaByaToCv_HvC
      catesByaToCv = [(fst5 cate, "Cv/a-" ++ snd5 cate, thd5 cate, fth5 cate, fif5 cate) | cate <- ctspaByaToCv]

{- The category conversion from np/.np to (np/.np)\*(np/.np) happens when np/.np (adjective classic type) complemently modify np/.np (adjective or numeral).
 - For examples, "好a 多a 了", and "二十m 多a 元q 钱n".
 -}
      a_Ca_HaC = removeDup [(adjCompCate, snd3 csp, thd3 csp) | csp <- csp2, cateEqual (fst3 csp) adjCate]
      ctspaByaToCa_HaC = [rule cate1 cate2 | rule <- [appB], cate1 <- csp_1, cate2 <- a_Ca_HaC, elem Caa onOff]
          where
          csp_1 = removeDup [x| x<- csp1, cateEqual (fst3 x) adjCate]
      ctspaByaToCa = ctspaByaToCa_HaC
      catesByaToCa = [(fst5 cate, "Ca/a-" ++ snd5 cate, thd5 cate, fth5 cate, fif5 cate) | cate <- ctspaByaToCa]

{- The conversion from adjective to noun happens when the adjective occupies AHn's or HnC's headword position.
 -}
      a_Hn_AHn = removeDup [(npCate, snd3 csp, thd3 csp) | csp <- csp2, cateEqual (fst3 csp) adjCate]
      ctspaByaToHn_AHn = [rule cate1 cate2 | rule <- [appF], cate1 <- csp_1, cate2 <- a_Hn_AHn, elem Hna onOff]
          where
          csp_1 = removeDup [x| x<- csp1, cateEqual (fst3 x) adjCate]
      a_Hn_HnC = removeDup [(npCate, snd3 csp, thd3 csp) | csp <- csp1, cateEqual (fst3 csp) adjCate]
      ctspaByaToHn_HnC = [rule cate1 cate2 | rule <- [appB], cate1 <- a_Hn_HnC, cate2 <- csp_2, elem Hna onOff]
          where
          csp_2 = removeDup [x| x<-csp2, fst3 x == nounCompCate]
      ctspaByaToHn = ctspaByaToHn_AHn ++ ctspaByaToHn_HnC
      catesByaToHn = [(fst5 cate, "Hn/a-" ++ snd5 cate, thd5 cate, fth5 cate, fif5 cate) | cate <- ctspaByaToHn]

{- The conversion from np/*np to np happens when
 - (1) adjective words are followed by auxiliary word '的',
 - (2) numeral words (with type np/*np) have prefix (with type np/*np) modification.
 -}
      a_N_U1P = removeDup [(npCate, snd3 csp, thd3 csp) | csp <- csp1, cateEqual (fst3 csp) adjCate]
      ctspaByaToN_U1P = [rule cate1 cate2 | rule <- [appB], cate1 <- a_N_U1P, cate2 <- csp_2, elem Na onOff]
          where
          csp_2 = removeDup [x| x<-csp2, fst3 x == aux1Cate]
      a_N_HP = removeDup [(npCate, snd3 csp, thd3 csp) | csp <- csp2, cateEqual (fst3 csp) adjCate]
      ctspaByaToN_HP = [rule cate1 cate2 | rule <- [appF], cate1 <- csp_1, cate2 <- a_N_HP, elem Na onOff]
          where
          csp_1 = removeDup [x| x<-csp1, fst3 x == prefixCate]
      ctspaByaToN = ctspaByaToN_U1P ++ ctspaByaToN_HP
      catesByaToN = [(fst5 cate, "N/a-" ++ snd5 cate, thd5 cate, fth5 cate, fif5 cate) | cate <- ctspaByaToN]

-- The conversion from noun to predicate is ONLY allowed when the noun acts as predicate or the headword of DHv.
      n_P1 = removeDup [(predCate, snd3 csp, thd3 csp) | csp <- csp2, (fst3 csp) == npCate]
      ctspaBynToP1 = [rule cate1 cate2 | rule <- [appB], cate1 <- csp_1, cate2 <- n_P1, elem Pn onOff]
          where
          csp_1 = removeDup [x| x<- csp1, fst3 x == npCate]
      n_P2 = removeDup [(predCate, snd3 csp, thd3 csp) | csp <- csp2, (fst3 csp) == npCate]
      ctspaBynToP2 = [rule cate1 cate2 | rule <- [appF], cate1 <- csp_1, cate2 <- n_P2, elem Pn onOff]
          where
          csp_1 = removeDup [x| x<- csp1, fst3 x == advCate]
      ctspaBynToP = ctspaBynToP1 ++ ctspaBynToP2
      catesBynToP = [(fst5 cate, "P/n-" ++ snd5 cate, thd5 cate, fth5 cate, fif5 cate) | cate <- ctspaBynToP]

{- The conversion from noun to verb is ONLY allowed when the noun acts as verb,
 - (1) to form VO structure;
 - (2) to form OE structure.
 -}
      n_V_VO = removeDup [(verbCate, snd3 csp, thd3 csp) | csp <- csp1, (fst3 csp) == npCate]
      ctspaBynToV_VO = [rule cate1 cate2 | rule <- [appF], cate1 <- n_V_VO, cate2 <- csp_2, elem Vn onOff]
          where
          csp_2 = removeDup [x| x<- csp2, fst3 x == npCate]
      n_V_OE = removeDup [(verbCate, snd3 csp, thd3 csp) | csp <- csp2, (fst3 csp) == npCate]
      ctspaBynToV_OE = [rule cate1 cate2 | rule <- [raiFh], cate1 <- csp_1, cate2 <- n_V_OE, elem Vn onOff]
          where
          csp_1 = removeDup [x| x<- csp1, fst3 x == npCate]
      n_V_DHv = removeDup [(verbCate, snd3 csp, thd3 csp) | csp <- csp2, (fst3 csp) == npCate]
      ctspaBynToV_DHv = [rule cate1 cate2 | rule <- [comFh], cate1 <- csp_1, cate2 <- n_V_DHv, elem Vn onOff]
          where
          csp_1 = removeDup [x| x<- csp1, fst3 x == advCate]
      ctspaBynToV = ctspaBynToV_VO ++ ctspaBynToV_OE ++ ctspaBynToV_DHv
      catesBynToV = [(fst5 cate, "V/n-" ++ snd5 cate, thd5 cate, fth5 cate, fif5 cate) | cate <- ctspaBynToV]

{- The conversion from 'np' to 'np/.np' is ONLY allowed when a noun acts as an attribue.
 -}
      n_A_AHn = removeDup [(adjCate, snd3 csp, thd3 csp) | csp <- csp1, (fst3 csp) == npCate]
      ctspaBynToA_AHn = [rule cate1 cate2 | rule <- [appF], cate1 <- n_A_AHn, cate2 <- csp_2, elem An onOff]
          where
          csp_2 = removeDup [x| x<- csp2, fst3 x == npCate]
      ctspaBynToA = ctspaBynToA_AHn
      catesBynToA = [(fst5 cate, "A/n-" ++ snd5 cate, thd5 cate, fth5 cate, fif5 cate) | cate <- ctspaBynToA]

-- The conversion from noun to noun's completment is ONLY allowed when the noun acts as noun's completment.
      n_Cn = removeDup [(nounCompCate, snd3 csp, thd3 csp) | csp <- csp2, (fst3 csp) == npCate]
      ctspaBynToCn = [rule cate1 cate2 | rule <- [appB], cate1 <- csp_1, cate2 <- n_Cn, elem Cnn onOff]
          where
          csp_1 = removeDup [x| x<- csp1, fst3 x == npCate]
      catesBynToCn = [(fst5 cate, "Cn/n-" ++ snd5 cate, thd5 cate, fth5 cate, fif5 cate) | cate <- ctspaBynToCn]

{- The conversion from noun to verb's completment is ONLY allowed when the noun acts as verb's completment.
 - Time noun has this requirement. Such as "走v 了u 一m 个q 月nt" and "吃v 半m 个q 月nt 馍n".
 - Actually, nouns except of time nouns seldom need this conversion. If time words become one class of words,
 - ambiguity of noun types will become less.
 -}
      n_Cv = removeDup [(verbCompCate, snd3 csp, thd3 csp) | csp <- csp2, (fst3 csp) == npCate]
      ctspaBynToCv = [rule cate1 cate2 | rule <- [appB, comBc], cate1 <- csp_1, cate2 <- n_Cv, elem Cvn onOff]
          where
          csp_1 = removeDup [x| x<- csp1, elem True (map (\y -> (cateEqual y (fst3 x))) vCate)]
      catesBynToCv = [(fst5 cate, "Cv/n-" ++ snd5 cate, thd5 cate, fth5 cate, fif5 cate) | cate <- ctspaBynToCv]

{- The conversion from noun to verb's adberbial type is ONLY allowed when
 - (1) the noun acts as verb's adverbial, using type (s\.np)/#(s\.np),
 - (2) the noun acts as headword of DHd, using type (s\.np)/#(s\.np),
 - (3) the noun acts as adverbial of DHd, using type (s\.np)/#(s\.np), such as "大学n 期间nt"
 - (4) the noun acts as adverbial to form coordinate structure, namely followed by advCompCate ((s\.np)/#(s\.np))\*((s\.np)/#(s\.np));
 - (5) the noun acts as sentential adverbial, using type s/*s,
 - (6) the noun acts as predicate adverbial, using type (s\.np)/#(s\.np), but combines with subject to get predicate-extracted phrase before combining with predicate.
 - Time nouns usually need this conversion.
 -}
      n_D_DHv = removeDup [(advCate, snd3 csp, thd3 csp) | csp <- csp1, (fst3 csp) == npCate]
      ctspaBynToD_DHv = [rule cate1 cate2 | rule <- [appF, comFh, comFh2], cate1 <- n_D_DHv, cate2 <- csp_2, elem Dn onOff]
          where
          csp_2 = removeDup [x| x<- csp2, elem True (map (\y-> cateEqual y (fst3 x)) vCate)]
      n_Hd_DHd = removeDup [(advCate, snd3 csp, thd3 csp) | csp <- csp2, (fst3 csp) == npCate]
      ctspaBynToHd_DHd = [rule cate1 cate2 | rule <- [comFh], cate1 <- csp_1, cate2 <- n_Hd_DHd, elem Dn onOff]
          where
          csp_1 = removeDup [x| x<- csp1, fst3 x == advCate]
      n_D_DHd = removeDup [(advCate, snd3 csp, thd3 csp) | csp <- csp1, (fst3 csp) == npCate]
      ctspaBynToD_DHd = [rule cate1 cate2 | rule <- [comFh], cate1 <- n_D_DHd, cate2 <- csp_2, elem Dn onOff]
          where
          csp_2 = removeDup [x| x<- csp2, fst3 x == advCate]
      n_D_DHs = removeDup [(advCate4Sent, snd3 csp, thd3 csp) | csp <- csp1, (fst3 csp) == npCate]
      ctspaBynToD_DHs = [rule cate1 cate2 | rule <- [appF], cate1 <- n_D_DHs, cate2 <- csp_2, elem Dn onOff]
          where
          csp_2 = removeDup [x| x<- csp2, fst3 x == sCate]
      n_D_XX = removeDup [(advCate, snd3 csp, thd3 csp) | csp <- csp1, (fst3 csp) == npCate]
      ctspaBynToD_XX = [rule cate1 cate2 | rule <- [appB], cate1 <- n_D_XX, cate2 <- csp_2, elem Dn onOff]
          where
          csp_2 = removeDup [x| x<- csp2, fst3 x == advCompCate, thd3 x == "HX"]
      n_D_PE = removeDup [(advCate, snd3 csp, thd3 csp) | csp <- csp2, (fst3 csp) == npCate]
      ctspaBynToD_PE = [rule cate1 cate2 | rule <- [raiFh], cate1 <- csp_1, cate2 <- n_D_PE, elem Dn onOff]
          where
          csp_1 = removeDup [x| x<- csp1, fst3 x == npCate]
      ctspaBynToD = ctspaBynToD_DHv ++ ctspaBynToHd_DHd ++ ctspaBynToD_DHd ++ ctspaBynToD_DHs ++ ctspaBynToD_XX ++ ctspaBynToD_PE
      catesBynToD = [(fst5 cate, "D/n-" ++ snd5 cate, thd5 cate, fth5 cate, fif5 cate) | cate <- ctspaBynToD]

{- The conversion from noun to adjective adberbial type is ONLY allowed when
 - Te noun acts as adjective adverbial, using type (np/.np)/*(np/.np).
 - Such as "那么 好"
 -}
      n_Da_DHa = removeDup [(advCate4Adj, snd3 csp, thd3 csp) | csp <- csp1, (fst3 csp) == npCate]
      ctspaBynToDa_DHa = [rule cate1 cate2 | rule <- [appF], cate1 <- n_Da_DHa, cate2 <- csp_2, elem Dan onOff]
          where
          csp_2 = removeDup [x| x<- csp2, fst3 x == adjCate]
      ctspaBynToDa = ctspaBynToDa_DHa
      catesBynToDa = [(fst5 cate, "Da/n-" ++ snd5 cate, thd5 cate, fth5 cate, fif5 cate) | cate <- ctspaBynToDa]

{- The conversion from 'np' to 'np/.np' is ONLY allowed when pronouns are followed by quantifiers, or
 - nouns are followed by '地'.
 -}
      n_ADJ_PQ = removeDup [(adjCate, snd3 csp, thd3 csp) | csp <- csp1, (fst3 csp) == npCate]
      ctspaBynToADJ_PQ = [rule cate1 cate2 | rule <- [appB], cate1 <- n_ADJ_PQ, cate2 <- csp_2, elem ADJn onOff]
          where
          csp_2 = removeDup [x| x<- csp2, fst3 x == quantifierCate]
      n_ADJ_U2P = removeDup [(adjCate, snd3 csp, thd3 csp) | csp <- csp1, (fst3 csp) == npCate]
      ctspaBynToADJ_U2P = [rule cate1 cate2 | rule <- [appB], cate1 <- n_ADJ_U2P, cate2 <- csp_2, elem ADJn onOff]
          where
          csp_2 = removeDup [x| x<- csp2, fst3 x == aux2Cate]
      ctspaBynToADJ = ctspaBynToADJ_PQ ++ ctspaBynToADJ_U2P
      catesBynToADJ = [(fst5 cate, "ADJ/n-" ++ snd5 cate, thd5 cate, fth5 cate, fif5 cate) | cate <- ctspaBynToADJ]

{- The conversion from noun-complemented type "np\*np" to noun's type "np" is ONLY allowed when
 - (1) it occupies the subject's position of phrase SP,
 - (2) it occupies the object's position of phrase VO.
 - (3) it occupies the wordhead position of phrase AHn,
 -}
      nd_S_SP = removeDup [(npCate, snd3 csp, thd3 csp) | csp <- csp1, (fst3 csp) == nounCompCate]
      ctspaByndToS_SP = [rule cate1 cate2 | rule <- [appB], cate1 <- nd_S_SP, cate2 <- csp_2, elem Snd onOff]
          where
          csp_2 = removeDup [x| x<- csp2, fst3 x == predCate]
      ctspaByndToS = ctspaByndToS_SP
      catesByndToS = [(fst5 cate, "S/nd-" ++ snd5 cate, thd5 cate, fth5 cate, fif5 cate) | cate <- ctspaByndToS]

      nd_O_VO = removeDup [(npCate, snd3 csp, thd3 csp) | csp <- csp2, (fst3 csp) == nounCompCate]
      ctspaByndToO_VO = [rule cate1 cate2 | rule <- [appF], cate1 <- csp_1, cate2 <- nd_O_VO, elem Ond onOff]
          where
          csp_1 = removeDup [x| x<- csp1, fst3 x == verbCate]
      ctspaByndToO = ctspaByndToO_VO
      catesByndToO = [(fst5 cate, "O/nd-" ++ snd5 cate, thd5 cate, fth5 cate, fif5 cate) | cate <- ctspaByndToO]

      nd_Hn_AHn = removeDup [(npCate, snd3 csp, thd3 csp) | csp <- csp2, (fst3 csp) == nounCompCate]
      ctspaByndToHn_AHn = [rule cate1 cate2 | rule <- [appF], cate1 <- csp_1, cate2 <- nd_Hn_AHn, elem Hnnd onOff]
          where
          csp_1 = removeDup [x| x<- csp1, cateEqual (fst3 x) adjCate]
      ctspaByndToHn = ctspaByndToHn_AHn
      catesByndToHn = [(fst5 cate, "Hn/nd-" ++ snd5 cate, thd5 cate, fth5 cate, fif5 cate) | cate <- ctspaByndToHn]

{- The conversion from adverbial type (s\.np)/#(s\.np) to the subjective position
 -}
      d_S_SP = removeDup [(npCate, snd3 csp, thd3 csp) | csp <- csp1, (fst3 csp) == advCate]
      ctspaBydToS_SP = [rule cate1 cate2 | rule <- [appB], cate1 <- d_S_SP, cate2 <- csp_2, elem Sd onOff]
          where
          csp_2 = removeDup [x| x<- csp2, fst3 x == predCate]
      ctspaBydToS = ctspaBydToS_SP
      catesBydToS = [(fst5 cate, "S/d-" ++ snd5 cate, thd5 cate, fth5 cate, fif5 cate) | cate <- ctspaBydToS]

{- The conversion from adverbial type (s\.np)/#(s\.np) to the object's position
 -}
      d_O_VO = removeDup [(npCate, snd3 csp, thd3 csp) | csp <- csp2, (fst3 csp) == advCate]
      ctspaBydToO_VO = [rule cate1 cate2 | rule <- [appF], cate1 <- csp_1, cate2 <- d_O_VO, elem Od onOff]
          where
          csp_1 = removeDup [x| x<- csp1, fst3 x == verbCate]
      d_O_PO = removeDup [(npCate, snd3 csp, thd3 csp) | csp <- csp2, (fst3 csp) == advCate]                      -- prepositional object
      ctspaBydToO_PO = [rule cate1 cate2 | rule <- [appF], cate1 <- csp_1, cate2 <- d_O_PO, elem Od onOff]
          where
          csp_1 = removeDup [x| x<- csp1, cateEqual (fst3 x) prep2AdvCate]
      ctspaBydToO = ctspaBydToO_VO ++ ctspaBydToO_PO
      catesBydToO = [(fst5 cate, "O/d-" ++ snd5 cate, thd5 cate, fth5 cate, fif5 cate) | cate <- ctspaBydToO]

{- The conversion from verb-adverbial type (s\.np)/#(s\.np) to attribue type np/.np is ONLY allowed when a noun follows.
 -}
      d_A_AHn = removeDup [(adjCate, snd3 csp, thd3 csp) | csp <- csp1, (fst3 csp) == advCate]
      ctspaBydToA_AHn = [rule cate1 cate2 | rule <- [appF], cate1 <- d_A_AHn, cate2 <- csp_2, elem Ad onOff]
          where
          csp_2 = removeDup [x| x<- csp2, fst3 x == npCate]
      ctspaBydToA = ctspaBydToA_AHn
      catesBydToA = [(fst5 cate, "A/d-" ++ snd5 cate, thd5 cate, fth5 cate, fif5 cate) | cate <- ctspaBydToA]


{- The conversion from adverbial type (s\.np)/#(s\.np) to nominal headword is ONLY allowed when
 - (1) A time noun occupies the headword position of AHn, such as "三m 天nt";
 - (2) a noun's complement follows the adverb,
 -}
      d_Hn_AHn = removeDup [(npCate, snd3 csp, thd3 csp) | csp <- csp2, (fst3 csp) == advCate]
      ctspaBydToHn_AHn = [rule cate1 cate2 | rule <- [appF], cate1 <- csp_1, cate2 <- d_Hn_AHn, elem Hnd onOff]
          where
          csp_1 = removeDup [x| x<- csp1, cateEqual (fst3 x) adjCate]
      d_Hn_HnC = removeDup [(npCate, snd3 csp, thd3 csp) | csp <- csp1, (fst3 csp) == advCate]
      ctspaBydToHn_HnC = [rule cate1 cate2 | rule <- [appB], cate1 <- d_Hn_HnC, cate2 <- csp_2, elem Hnd onOff]
          where
          csp_2 = removeDup [x| x<- csp2, cateEqual (fst3 x) nounCompCate]
      ctspaBydToHn = ctspaBydToHn_AHn ++ ctspaBydToHn_HnC
      catesBydToHn = [(fst5 cate, "Hn/d-" ++ snd5 cate, thd5 cate, fth5 cate, fif5 cate) | cate <- ctspaBydToHn]

{- The conversion from verb-dverbial type (s\.np)/#(s\.np) to the type of verb's complement (s\.np)\x(s\.np) is ONLY allowed when a verb is on the left.
 -}
      d_Cv_HvC = removeDup [(verbCompCate, snd3 csp, thd3 csp) | csp <- csp2, (fst3 csp) == advCate]
      ctspaBydToCv_HvC = [rule cate1 cate2 | rule <- [appB,comBc], cate1 <- csp_1, cate2 <- d_Cv_HvC, elem Cvd onOff]
          where
          csp_1 = removeDup [x| x<- csp1, elem True (map (\y-> cateEqual y (fst3 x)) vCate)]
      ctspaBydToCv = ctspaBydToCv_HvC
      catesBydToCv = [(fst5 cate, "Cv/d-" ++ snd5 cate, thd5 cate, fth5 cate, fif5 cate) | cate <- ctspaBydToCv]

{- The conversion from adverbial type (s\.np)/#(s\.np) to noun type np is ONLY allowed when an auxiliary word '的' follows.
 -}
      d_N_U1P = removeDup [(npCate, snd3 csp, thd3 csp) | csp <- csp1, (fst3 csp) == advCate]
      ctspaBydToN_U1P = [rule cate1 cate2 | rule <- [appB], cate1 <- d_N_U1P, cate2 <- csp_2, elem Nd onOff]
          where
          csp_2 = removeDup [x| x<- csp2, fst3 x == aux1Cate]
      ctspaBydToN = ctspaBydToN_U1P
      catesBydToN = [(fst5 cate, "N/d-" ++ snd5 cate, thd5 cate, fth5 cate, fif5 cate) | cate <- ctspaBydToN]

{- The conversion from verb-adverbial type (s\.np)/#(s\.np) to attribue type np/.np is ONLY allowed when auxiliary word '地' follows.
 -}
      d_ADJ_U2P = removeDup [(adjCate, snd3 csp, thd3 csp) | csp <- csp1, (fst3 csp) == advCate]
      ctspaBydToADJ_U2P = [rule cate1 cate2 | rule <- [appB], cate1 <- d_ADJ_U2P, cate2 <- csp_2, elem ADJd onOff]
          where
          csp_2 = removeDup [x| x<- csp2, fst3 x == aux2Cate]
      ctspaBydToADJ = ctspaBydToADJ_U2P
      catesBydToADJ = [(fst5 cate, "ADJ/d-" ++ snd5 cate, thd5 cate, fth5 cate, fif5 cate) | cate <- ctspaBydToADJ]

{- The conversion from verb-adverbial type (s\.np)/#(s\.np) to adjective-adverbial type (np/.np)/*(np/.np) is ONLY allowed when an adjective type 'np/.np' follows.
 -}
      d_Da_DHa = removeDup [(advCate4Adj, snd3 csp, thd3 csp) | csp <- csp1, (fst3 csp) == advCate]
      ctspaBydToDa_DHa = [rule cate1 cate2 | rule <- [appF], cate1 <- d_Da_DHa, cate2 <- csp_2, elem Dad onOff]
          where
          csp_2 = removeDup [x| x<- csp2, cateEqual (fst3 x) adjCate]
      ctspaBydToDa = ctspaBydToDa_DHa
      catesBydToDa = [(fst5 cate, "Da/d-" ++ snd5 cate, thd5 cate, fth5 cate, fif5 cate) | cate <- ctspaBydToDa]

{- The conversion from verb-adverbial type (s\.np)/#(s\.np) to sentence-adverbial type s/*s is ONLY allowed when sentential type 's' follows.
 -}
      d_Ds_DHs = removeDup [(advCate4Sent, snd3 csp, thd3 csp) | csp <- csp1, (fst3 csp) == advCate]
      ctspaBydToDs_DHs = [rule cate1 cate2 | rule <- [appF], cate1 <- d_Ds_DHs, cate2 <- csp_2, elem Dsd onOff]
          where
          csp_2 = removeDup [x| x<- csp2, fst3 x == sCate]
      ctspaBydToDs = ctspaBydToDs_DHs
      catesBydToDs = [(fst5 cate, "Ds/d-" ++ snd5 cate, thd5 cate, fth5 cate, fif5 cate) | cate <- ctspaBydToDs]

{- The conversion from verb-adverbial type (s\.np)/#(s\.np) to directional verb-adverbial type (s\.np)/x(s\.np) is ONLY allowed when
 - a directioanl verb follows.
 - Such as, 分(s\.np)/.np 不(s\.np)/#(s\.np) 开(s\.np)\x(s\.np) => 分(s\.np)/.np (不 开)(s\.np)\x(s\.np)
 -}
      d_Dx_DHx = removeDup [(advCate4DirecVerb, snd3 csp, thd3 csp) | csp <- csp1, (fst3 csp) == advCate]
      ctspaBydToDx_DHx = [rule cate1 cate2 | rule <- [comFc], cate1 <- d_Dx_DHx, cate2 <- csp_2, elem Dxd onOff]
          where
          csp_2 = removeDup [x| x<- csp2, fst3 x == verbCompCate]
      ctspaBydToDx = ctspaBydToDx_DHx
      catesBydToDx = [(fst5 cate, "Dx/d-" ++ snd5 cate, thd5 cate, fth5 cate, fif5 cate) | cate <- ctspaBydToDx]

{- The conversion from verb-adverbial type (s\.np)/#(s\.np) to object extraction-adverbial type (s/.np)/*(s/.np) is ONLY allowed when
 - an object extraction phrase follows.
 - Such as, 最近(s\.np)/#(s\.np) 他np 说(s\.np)/.np 的 => 最近(s/.np)/*(s/.np) (他 说)s/.np 的=> (最近 (他 说))s/.np 的
 -}
      d_Doe_DHoe = removeDup [(advCate4OE, snd3 csp, thd3 csp) | csp <- csp1, (fst3 csp) == advCate]
      ctspaBydToDoe_DHoe = [rule cate1 cate2 | rule <- [appF], cate1 <- d_Doe_DHoe, cate2 <- csp_2, elem Doed onOff]
          where
          csp_2 = removeDup [x| x<- csp2, fst3 x == objectExtractionCate]
      ctspaBydToDoe = ctspaBydToDoe_DHoe
      catesBydToDoe = [(fst5 cate, "Doe/d-" ++ snd5 cate, thd5 cate, fth5 cate, fif5 cate) | cate <- ctspaBydToDoe]

-- The conversion from preposition to adverbial is ONLY allowed when the noun following the preposition is elliptical (省略).
      p_D_DHv = removeDup [(advCate, snd3 csp, thd3 csp) | csp <- csp1, (fst3 csp) == prep2AdvCate]
      ctspaBypToD_DHv = [rule cate1 cate2 | rule <- [appF,comFh,comFh2], cate1 <- p_D_DHv, cate2 <- csp_2, elem Dp onOff]
          where
          csp_2 = removeDup [x| x<- csp2, elem True (map (\y-> cateEqual y (fst3 x)) vCate)]
      ctspaBypToD = ctspaBypToD_DHv
      catesBypToD = [(fst5 cate, "D/p-" ++ snd5 cate, thd5 cate, fth5 cate, fif5 cate) | cate <- ctspaBypToD]

{- The conversion from object-extractioned type "s/.np" to nominal constituent is ONLY allowed when
 - (1) the object-extractioned phrase follows a transitive verb, such as "遭到vt 老师n 批评vt".
 - (2) noun completment follows, whose typical syntactic type is np\*np,
 - (3) '的' follows，whose typical syntactic type is (np/*np)\*np,
 - (4) the object-extractioned phrase follows a preposition, such as "由p 政府n 补助vt".

 -}
      oe_O_VO = removeDup [(npCate, snd3 csp, thd3 csp) | csp <- csp2, (fst3 csp) == objectExtractionCate]
      ctspaByoeToO_VO = [rule cate1 cate2 | rule <- [appF], cate1 <- csp_1, cate2 <- oe_O_VO, elem Ooe onOff]
          where
          csp_1 = removeDup [x| x<- csp1, fst3 x == verbCate]
      oe_O_PO = removeDup [(npCate, snd3 csp, thd3 csp) | csp <- csp2, (fst3 csp) == objectExtractionCate]
      ctspaByoeToO_PO = [rule cate1 cate2 | rule <- [appF], cate1 <- csp_1, cate2 <- oe_O_PO, elem Ooe onOff]
          where
          csp_1 = removeDup [x| x<- csp1, fst3 x == prep2AdvCate]
      ctspaByoeToO = ctspaByoeToO_VO ++ ctspaByoeToO_PO
      catesByoeToO = [(fst5 cate, "O/oe-" ++ snd5 cate, thd5 cate, fth5 cate, fif5 cate) | cate <- ctspaByoeToO]

      oe_Hn_HnC = removeDup [(npCate, snd3 csp, thd3 csp) | csp <- csp1, (fst3 csp) == objectExtractionCate]
      ctspaByoeToHn_HnC = [rule cate1 cate2 | rule <- [appB], cate1 <- oe_Hn_HnC, cate2 <- csp_2, elem Hnoe onOff]
          where
          csp_2 = removeDup [x| x<- csp2, fst3 x == nounCompCate]
      ctspaByoeToHn = ctspaByoeToHn_HnC
      catesByoeToHn = [(fst5 cate, "Hn/oe-" ++ snd5 cate, thd5 cate, fth5 cate, fif5 cate) | cate <- ctspaByoeToHn]

      oe_N_U1P = removeDup [(npCate, snd3 csp, thd3 csp) | csp <- csp1, (fst3 csp) == objectExtractionCate]
      ctspaByoeToN_U1P = [rule cate1 cate2 | rule <- [appB], cate1 <- oe_N_U1P, cate2 <- csp_2, elem Noe onOff]
          where
          csp_2 = removeDup [x| x<- csp2, fst3 x == aux1Cate]
      ctspaByoeToN = ctspaByoeToN_U1P
      catesByoeToN = [(fst5 cate, "N/oe-" ++ snd5 cate, thd5 cate, fth5 cate, fif5 cate) | cate <- ctspaByoeToN]

{- The conversion from Predicate-extractioned type "s/#(s\.np)" to nominal constituent is ONLY allowed when '的' follows，whose typical syntactic type is (np/*np)\*np.
 -}
      pe_N_U1P = removeDup [(npCate, snd3 csp, thd3 csp) | csp <- csp1, (fst3 csp) == predicateExtractionCate]
      ctspaBypeToN_U1P = [rule cate1 cate2 | rule <- [appB], cate1 <- pe_N_U1P, cate2 <- csp_2, elem Npe onOff]
          where
          csp_2 = removeDup [x| x<- csp2, fst3 x == aux1Cate]
      ctspaBypeToN = ctspaBypeToN_U1P
      catesBypeToN = [(fst5 cate, "N/pe-" ++ snd5 cate, thd5 cate, fth5 cate, fif5 cate) | cate <- ctspaBypeToN]

{- The conversion from quantifier type (np/*np)\*(np/*np) to adjective type np/*np is ONLY allowed when a noun follows.
 - Actually, the conversion is used only when there is no numeral to the left.
 - If the context detection is implemented, the stntactic ambiguity can be restricted further.
 -}
      q_A_AHn = removeDup [(adjCate, snd3 csp, thd3 csp) | csp <- csp1, (fst3 csp) == quantifierCate]
      ctspaByqToA_AHn = [rule cate1 cate2 | rule <- [appF], cate1 <- q_A_AHn, cate2 <- csp_2, elem Aq onOff]
          where
          csp_2 = removeDup [x| x<- csp2, fst3 x == npCate]
      ctspaByqToA = ctspaByqToA_AHn
      catesByqToA = [(fst5 cate, "A/q-" ++ snd5 cate, thd5 cate, fth5 cate, fif5 cate) | cate <- ctspaByqToA]

{- The conversion from conjunction type (X\*X)/*X to the forward conjunction's type X/*X is ONLY allowed when it's at the head of a clause.
 - For combination of two types, there is no method to indicate the conjunction is at the head of a clause.
 - But a clause does not certainly have type 's', and probably has various types. Now there is no restriction on clause type.
 -}
      c_Jf_CC = removeDup [(conjCate4Forward, snd3 csp, thd3 csp) | csp <- csp1, fst3 csp == conjCate]
      ctspaBycToJf_CC = [rule cate1 cate2 | rule <- [appF], cate1 <- c_Jf_CC, cate2 <- csp2, elem Jfc onOff]
      ctspaBycToJf = ctspaBycToJf_CC
      catesBycToJf = [(fst5 cate, "Jf/c-" ++ snd5 cate, thd5 cate, fth5 cate, fif5 cate) | cate <- ctspaBycToJf]

{- The conversion from conjunction type (X\*X)/*X to the backward conjunction's type X\*X is ONLY allowed when it's at the end of a clause.
 - or when it follows a prepositional phrase used as an adverbial, such as '为 荣誉 而 战'.
 -}
      c_Jb_CC = removeDup [(conjCate4Backward, snd3 csp, thd3 csp) | csp <- csp2, fst3 csp == conjCate]
      ctspaBycToJb_CC1 = [rule cate1 cate2 | rule <- [appB], cate1 <- csp_1, cate2 <- c_Jb_CC, elem Jbc onOff]
          where
          csp_1 = removeDup [x| x<- csp1, fst3 x == sCate]
      ctspaBycToJb_CC2 = [rule cate1 cate2 | rule <- [appB], cate1 <- csp_1, cate2 <- c_Jb_CC, elem Jbc onOff]
          where
          csp_1 = removeDup [x| x<- csp1, fst3 x == advCate]
      ctspaBycToJb = ctspaBycToJb_CC1 ++ ctspaBycToJb_CC2
      catesBycToJb = [(fst5 cate, "Jb/c-" ++ snd5 cate, thd5 cate, fth5 cate, fif5 cate) | cate <- ctspaBycToJb]

{- The conversion from '得' typical type ((s\.np)\x(s\.np))/*(np/.np) to its non-typical type ((np/.np)\*(np/.np))/*((np/.np)/*(np/.np)" is ONLY allowed when
 - an adjective-adverbial follows, such as, 好a 得u 很d.
 -}
      u3_U3d_U3P = removeDup [(aux3dCate, snd3 csp, thd3 csp) | csp <- csp1, fst3 csp == aux3Cate]
      ctspaByu3ToU3d_U3P = [rule cate1 cate2 | rule <- [appF], cate1 <- u3_U3d_U3P, cate2 <- csp_2, elem U3du3 onOff]
          where
          csp_2 = removeDup [x| x<- csp2, fst3 x == advCate4Adj]
      ctspaByu3ToU3d = ctspaByu3ToU3d_U3P
      catesByu3ToU3d = [(fst5 cate, "U3d/u3-" ++ snd5 cate, thd5 cate, fth5 cate, fif5 cate) | cate <- ctspaByu3ToU3d]

{- The two adjacent types "s np/.np" or "s np/*np " convert to "np s\.np", forming structure SP, here S/s and P/a happen simultaneously.
 - Here, s_S_SP and a_P_SP have been defined before.
 -}
      ctspaBysToS_aToP = [rule cate1 cate2 | rule <- [appB], cate1 <- s_S_SP, cate2 <- a_P_SP, elem Ss onOff, elem Pa onOff]
      catesBysToS_aToP = [(fst5 cate, "S/s-P/a-" ++ snd5 cate, thd5 cate, fth5 cate, fif5 cate) | cate <- ctspaBysToS_aToP]

{- The two adjacent types "s <verb>" convert to "np/.np np", forming structure AHn, here A/s and Hn/v happen simultaneously.
 -}
      ctspaBysToA_vToHn = [rule cate1 cate2 | rule <- [appF], cate1 <- s_A_AHn, cate2 <- v_Hn_AHn, elem As onOff, elem Hnv onOff]
      catesBysToA_vToHn = [(fst5 cate, "A/s-Hn/v-" ++ snd5 cate, thd5 cate, fth5 cate, fif5 cate) | cate <- ctspaBysToA_vToHn]


{- The two adjacent types "s <adverbial>" convert to "np/.np np", forming structure AHn, here A/s and Hn/d happen simultaneously.
 -}
      ctspaBysToA_dToHn = [rule cate1 cate2 | rule <- [appF], cate1 <- s_A_AHn, cate2 <- d_Hn_AHn, elem As onOff, elem Hnd onOff]
      catesBysToA_dToHn = [(fst5 cate, "A/s-Hn/d-" ++ snd5 cate, thd5 cate, fth5 cate, fif5 cate) | cate <- ctspaBysToA_dToHn]

{- The two adjacent types "s np" convert to "np np\*np", forming structure HnC, here Hn/s and Cn/n happen simultaneously.
 -}
      n_Cn_HnC = removeDup [(nounCompCate, snd3 csp, thd3 csp) | csp <- csp2, fst3 csp == npCate]
      ctspaBysToHn_nToCn = [rule cate1 cate2 | rule <- [appB], cate1 <- s_Hn_HnC, cate2 <- n_Cn_HnC, elem Hns onOff, elem Cnn onOff]
      catesBysToHn_nToCn = [(fst5 cate, "Hn/s-Cn/n-" ++ snd5 cate, thd5 cate, fth5 cate, fif5 cate) | cate <- ctspaBysToHn_nToCn]

{- The two adjacent types "np <verb>" convert to "<verb> np", forming structure VO, here V/n and O/v happen simultaneously.
 -}
      ctspaBynToV_vToO = [rule cate1 cate2 | rule <- [appF], cate1 <- n_V_VO, cate2 <- v_O_VO, elem Vn onOff, elem Ov onOff]
      catesBynToV_vToO = [(fst5 cate, "V/n-O/v-" ++ snd5 cate, thd5 cate, fth5 cate, fif5 cate) | cate <- ctspaBynToV_vToO]

{- The two adjacent types "np s" convert to "np/.np np", forming structure AHn, here A/n and Hn/s happen simultaneously.
 - To now, no example has been found.
 -}
      ctspaBynToA_sToHn = [rule cate1 cate2 | rule <- [appF], cate1 <- n_A_AHn, cate2 <- s_Hn_AHn, elem An onOff, elem Hns onOff]
      catesBynToA_sToHn = [(fst5 cate, "A/n-Hn/s-" ++ snd5 cate, thd5 cate, fth5 cate, fif5 cate) | cate <- ctspaBynToA_sToHn]

{- The two adjacent types "np np/.np" convert to "np/.np np", forming structure AHn, here noun-to-adjective and
   adjective-to-noun conversions happen simultaneously.
 -}
      n_A1 = removeDup [(adjCate, snd3 csp, thd3 csp) | csp <- csp1, fst3 csp == npCate]
      a_Hn' = removeDup [(npCate, snd3 csp, thd3 csp) | csp <- csp2, cateEqual (fst3 csp) adjCate]
      ctspaBynToA_aToHn = [rule cate1 cate2 | rule <- [appF], cate1 <- n_A1, cate2 <- a_Hn', elem An onOff, elem Hna onOff]
      catesBynToA_aToHn = [(fst5 cate, "A/n-Hn/a-" ++ snd5 cate, thd5 cate, fth5 cate, fif5 cate) | cate <- ctspaBynToA_aToHn]

{- The two adjacent types "np <verb>" convert to "np/.np np", forming structure AHn, here conversions noun-to-attribue and
   verb-to-headword of AHn happen simultaneously.
 -}
      ctspaBynToA_vToHn = [rule cate1 cate2 | rule <- [appF], cate1 <- n_A_AHn, cate2 <- v_Hn_AHn, elem An onOff, elem Hnv onOff]
      catesBynToA_vToHn = [(fst5 cate, "A/n-Hn/v-" ++ snd5 cate, thd5 cate, fth5 cate, fif5 cate) | cate <- ctspaBynToA_vToHn]

{- The two adjacent types "<noun> <adverb>" convert to "np/.np np", forming structure AHn, here A/n and Hn/d happen simultaneously.
 - To now, no example has been found.
 -}
      ctspaBynToA_dToHn = [rule cate1 cate2 | rule <- [appF], cate1 <- n_A_AHn, cate2 <- d_Hn_AHn, elem An onOff, elem Hnd onOff]
      catesBynToA_dToHn = [(fst5 cate, "A/n-Hn/d-" ++ snd5 cate, thd5 cate, fth5 cate, fif5 cate) | cate <- ctspaBynToA_dToHn]

{- The two adjacent types "np <verb>" convert to "(np/.np)/*(np/.np) np/.np", forming structure DHa, here conversions noun-to-adverbial and
   verb-to-headword of DHa happen simultaneously. Such as, "那么r 死vi" => "那么np 死s\.np" => "那么(np/.np)/*(np/.np) 死np/.np"
 -}
      v_A_DHa = removeDup [(adjCate, snd3 csp, thd3 csp) | csp <- csp2, elem True (map (\y-> cateEqual y (fst3 csp)) vCate)]
      ctspaBynToDa_vToA = [rule cate1 cate2 | rule <- [appF], cate1 <- n_Da_DHa, cate2 <- v_A_DHa, elem Dan onOff, elem Av onOff]
      catesBynToDa_vToA = [(fst5 cate, "Da/n-A/v-" ++ snd5 cate, thd5 cate, fth5 cate, fif5 cate) | cate <- ctspaBynToDa_vToA]

{- The two adjacent types "<verb> np/.np" convert to "np s\.np", forming structure SP, here conversions verb-to-subject and
   adjective-to-predicate happen simultaneously.
 -}
      ctspaByvToS_aToP = [rule cate1 cate2 | rule <- [appB], cate1 <- v_S_SP, cate2 <- a_P_SP, elem Sv onOff, elem Pa onOff]
      catesByvToS_aToP = [(fst5 cate, "S/v-P/a-" ++ snd5 cate, thd5 cate, fth5 cate, fif5 cate) | cate <- ctspaByvToS_aToP]

{- The two adjacent verbal types "<verb> <verb>" convert to "np/.np np", forming structure AHn, here verb-to-Attribute and
   verb-to-nominal headword conversions happen simultaneously.
 -}
      ctspaByvToA_vToHn = [rule cate1 cate2 | rule <- [appF], cate1 <- v_A_AHn, cate2 <- v_Hn_AHn, elem Av onOff, elem Hnv onOff]
      catesByvToA_vToHn = [(fst5 cate, "A/v-Hn/v-" ++ snd5 cate, thd5 cate, fth5 cate, fif5 cate) | cate <- ctspaByvToA_vToHn]

{- The two adjacent types "<verb> <adverb>" convert to "np/.np np", forming structure AHn, here A/v and Hn/d happen simultaneously.
 -}
      ctspaByvToA_dToHn = [rule cate1 cate2 | rule <- [appF], cate1 <- v_A_AHn, cate2 <- d_Hn_AHn, elem Av onOff, elem Hnd onOff]
      catesByvToA_dToHn = [(fst5 cate, "A/v-Hn/d-" ++ snd5 cate, thd5 cate, fth5 cate, fif5 cate) | cate <- ctspaByvToA_dToHn]

{- The two adjacent types "(s\.np)/.np s\.np" convert to "s\.np (s\.np)\x(s\.np)", forming structure HvC, here P/vt and Cv/v happen simultaneously.
 - Such as "他 分配vt 到北京工作vi"，缺少'被'字，实为病句。
 -}
      vt_P_HvC = removeDup [(predCate, snd3 csp, thd3 csp) | csp <- csp1, fst3 csp == verbCate]
      ctspaByvtToP_vToCv = [rule cate1 cate2 | rule <- [appB], cate1 <- vt_P_HvC, cate2 <- v_Cv_HvC, elem Pvt onOff, elem Cvv onOff]
      catesByvtToP_vToCv = [(fst5 cate, "P/vt-Cv/v-" ++ snd5 cate, thd5 cate, fth5 cate, fif5 cate) | cate <- ctspaByvtToP_vToCv]

{- The two adjacent types "<adjective> <verb>" or "<adjective> <verb2>" convert to "np s\.np", forming structure SP, here S/a and P/vt happen simultaneously.
 - The double conversions happen in sentences where the object acts as the subject.
 - a_S_SP = removeDup [(npCate, snd3 csp, thd3 csp) | csp <- csp1, cateEqual (fst3 csp) adjCate]
 - vt_P_SP = removeDup [(predCate, snd3 csp, thd3 csp) | csp <- csp2, elem True (map (\x-> cateEqual x (fst3 csp)) [verbCate,verbCate2])], which has been defined before.
 -}
      ctspaByaToS_vtToP = [rule cate1 cate2 | rule <- [appB], cate1 <- a_S_SP, cate2 <- vt_P_SP, elem Sa onOff, elem Pvt onOff]
      catesByaToS_vtToP = [(fst5 cate, "S/a-P/vt-" ++ snd5 cate, thd5 cate, fth5 cate, fif5 cate) | cate <- ctspaByaToS_vtToP]

-- The categories gotten by all rules.
      cates = catesBasic ++ catesBysToS ++ catesBysToP ++ catesBysToO ++ catesBysToA ++ catesBysToHn ++ catesBysToN
        ++ catesByvToS ++ catesByvToO ++ catesByvToA ++ catesByvToHn ++ catesByvToD ++ catesByvToCn ++ catesByvToCv ++ catesByvToN
        ++ catesByvtToP ++ catesByvtToOE ++ catesByviToVt ++ catesByvdToA
        ++ catesByaToS ++ catesByaToP ++ catesByaToV ++ catesByaToO ++ catesByaToD ++ catesByaToDa ++ catesByaToCn ++ catesByaToCv ++ catesByaToCa ++ catesByaToHn ++ catesByaToN
        ++ catesBynToP ++ catesBynToV ++ catesBynToA ++ catesBynToCn ++ catesBynToCv ++ catesBynToD ++ catesBynToDa ++ catesBynToADJ
        ++ catesByndToS ++ catesByndToO ++ catesByndToHn
        ++ catesBydToS ++ catesBydToO ++ catesBydToA ++ catesBydToHn ++ catesBydToCv ++ catesBydToN ++ catesBydToADJ ++ catesBydToDa ++ catesBydToDs ++ catesBydToDx ++ catesBydToDoe
        ++ catesBypToD
        ++ catesByoeToO ++ catesByoeToHn ++ catesByoeToN
        ++ catesBypeToN
        ++ catesByqToA
        ++ catesBycToJf ++ catesBycToJb
        ++ catesByu3ToU3d
        ++ catesBysToS_aToP ++ catesBysToA_vToHn ++ catesBysToA_dToHn ++ catesBysToHn_nToCn
        ++ catesBynToV_vToO ++ catesBynToA_sToHn ++ catesBynToA_aToHn ++ catesBynToA_vToHn ++ catesBynToA_dToHn ++ catesBynToDa_vToA
        ++ catesByvToS_aToP ++ catesByvToA_vToHn ++ catesByvToA_dToHn ++ catesByvtToP_vToCv
        ++ catesByaToS_vtToP

{- Remove Nil's resultant cateories, NR phrase-structural categories, and duplicate ones.
 - It's not yet clear how duplicate phrases are created.
 -}
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
   Fix: Before every trip of transitivity, syntactic-typed transformations can be selected on demand. If all transformations
   are available during every trip of transitivity, some phrases are banned at ambiguity resolution, the other phrases remain,
   at this time, the combinations of two inactive phrases will create banned phrases again. But now, two inactive phrases may
   combine to form new phrase via type transformations which are not allowed in previous transitivities. The inactive attribue
   is still important in indicating its having existed in certain phrases.
   Phrases not belonging to the final parsing tree can temperarily stay in parsing tree, provided that they do not incur syntactic ambiguaties.
   The component phrases in such phrases have attribue Act with value 'false', and are banned to join in and compose other new phrases.
   So phrasal attribute Act are NOT checked before trying to combine two phrases.
   Banned phrases are grouped by transitions.
 -}
trans :: OnOff -> [PhraCate] -> [[PhraCate]] -> [PhraCate]
trans onOff pcs banPCsList = pcs2
    where
--      combs = atomizePhraCateList [cateComb onOff pc1 pc2 | pc1 <- pcs, pc2 <- pcs, stOfCate pc1 + spOfCate pc1 + 1 == stOfCate pc2, (acOfCate pc1)!!0 || (acOfCate pc2)!!0]
--    Allowing two inactive phrases to combine.
      combs = atomizePhraCateList [cateComb onOff pc1 pc2 | pc1 <- pcs, pc2 <- pcs, stOfCate pc1 + spOfCate pc1 + 1 == stOfCate pc2]
      newCbs = [cb| cb <- combs, ctspaOfCate cb /= [], notElem4Phrase cb (concat banPCs), notElem4Phrase cb pcs]
                 -- The banned phrases might be created again, here they are filtered out.
                 -- The non-banned phrases also might be created again, here those reduplicates are removed out.
      pcs2 = pcs ++ newCbs

{- One trip of transition with pruning. Some new phrases removed timely are placed into banned phrasal list, and some
   new structural genes are added into the list of structural genes.
   Fix: Allow two inactive phrases to combine.
 -}
transWithPruning :: [Rule] -> [PhraCate] -> [[PhraCate]] -> [OverPair] -> IO ([PhraCate],[[PhraCate]])
transWithPruning onOff pcs banPCsList overPairs = do
--    let combs = atomizePhraCateList [cateComb onOff pc1 pc2 | pc1 <- pcs, pc2 <- pcs, stOfCate pc1 + spOfCate pc1 + 1 == stOfCate pc2, (acOfCate pc1)!!0 || (acOfCate pc2)!!0]
    let combs = atomizePhraCateList [cateComb onOff pc1 pc2 | pc1 <- pcs, pc2 <- pcs, stOfCate pc1 + spOfCate pc1 + 1 == stOfCate pc2]
                                                                  -- Not consider phrasal activity
    let newCbs = [cb| cb <- combs, ctspaOfCate cb /= [], notElem4Phrase cb (concat banPCs), notElem4Phrase cb pcs]
    if newCbs /= []
      then do
        let pcs1 = pcs ++ newCbs                                      -- Before pruning
        pcs1' <- prune overPairs pcs1 newCbs                          -- After pruning
        let pcs2 = updateAct pcs1'                                    -- Attr. activity is corrected.
        let banPCsList2 = banPCsList ++ [[cb| cb <- pcs1, notElem4Phrase cb pcs2]]    -- Add a banned phrase set
        return (pcs2, banPCsList2)
      else return (pcs, banPCsList)

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
   Linguistic knowledge can be used in pruning, for examples, adverbials close verbs nearer than complements, objects
   close verbs nearer than subjects, but inter-phrases priority relations may vary in different context.
   The final closure is comprised of an root category and other inactive phrasal categories.
   When interactively creating every transition of phrasal categorial combination, every trip of transition begins
   from no category-converted rules available, and ends after adopting some category-converted rules. When the stable
   phrasal closure is gotten, the interactive process terminates. Function parse uses same set of category-converted
   rules for all trips of transitions, and becomes useless under interactive mode.
 -}
{-
parse :: OnOff -> [PhraCate] -> [[PhraCate]] -> IO [PhraCate]
parse onOff trans banPCsList = do
    let combs = removeDup $ atomizePhraCateList [cateComb onOff pc1 pc2 | pc1 <- trans, pc2 <- trans, stOfCate pc1 + spOfCate pc1 + 1 == stOfCate pc2, (acOfCate pc1)!!0 || (acOfCate pc2)!!0]     -- At least one is active.
    let newCbs = [cb| cb <- combs, ctspaOfCate cb /= [], notElem4Phrase cb (concat banPCsList)]
    let trans1 = trans ++ newCbs                                    -- Before pruning
    trans2 <- prune $ updateAct $ trans1                            -- After pruning, Attr. activity is corrected.
    let banPCs2 = banPCsList ++ [[cb| cb <-trans1, notElem4Phrase cb trans2]]    -- Update the list of banned phrasal categories.
    if newCbs == []
      then return trans                  -- No new combination
      else parse onOff trans2 banPCsList2
-}

{- We adopt pruning method to remove those banned phrases, based on some axioms.
   Axiom 1. The pruned phrases are no longer generated.
   Axiom 2. After pruning, Type-1 and Type-2 overlaps certainly disappear, but Type-3, -4, -5 overlaps might remain.
   Axiom 3. Two phrases in a Type-3, -4, and -5 overlap are not construct (namely blood) relation, then one of which must be removed out.
   Any phrasal category not appearing in the final parsing tree does not be removed out until it overlaps other phrases incuring syntactic ambiguity.
   Any phrasal category having taken part in category combination should be set inactive, which
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
         (pc1, pc2) <- findPhraWithLowestPrio pcps pcps overPairs   -- Find the phrase with lowest priority among all phrases.
         let pcs' = removeOnePC pc1 pcs newCbs                      -- Remove phrase <pc1> and its descendants.
         let pcs'' = removeOnePC pc2 pcs' newCbs                    -- Remove phrase <pc2> and its descendants.
         putStr "The removed phrase(s):"
         showNPhraCate' [x| x<-pcs, notElem x pcs'']                -- Show all phrases in List <pcs> but not in List <pcs'>.
         prune overPairs pcs'' newCbs

{- A wrapper of Function <prune> to input the phrases to be pruned and the banned phrases, return the result and
   the updated banned phrases.
 -}
{-
prune' :: [PhraCate] -> [PhraCate] -> IO ([PhraCate],[PhraCate])
prune' pcs banPCs = do
    pcs1 <- prune pcs                                              -- After pruning, Attr. activity is corrected.
    let banPCs1 = banPCs ++ [pc| pc <- pcs, notElem4Phrase pc pcs1]      -- Update the list of banned phrasal categories.
    return (pcs1, banPCs1)
 -}

{- Get all pairs of overlapping phrases. In every pair, there is no blood relation, and at least one phrase is active.
 - If a phrase is active, the attribute Act is True; Otherwise, False.
 - Using pclt to avoid (a,b) and (b,a) concurrent, only considering 'a' is left of 'b'. Here, phrases' spans > 0.
 -}
getOverlap :: [PhraCate] -> [(PhraCate, PhraCate)]
getOverlap [] = []
getOverlap pcs = [(x,y)| x<-pcs, y<-pcs, spOfCate x > 0, spOfCate y > 0, x/=y, pclt x y, (acOfCate x)!!0 || (acOfCate y)!!0, getOverType pcs x y /= 0]

{- Here, overlapping relation is unidirectional. (<pc1>, <pc2>) is overlapping, then (<pc2>, <pc1>) is not overlapping.
   The relation has no transitivity. Without loss of generality, let AB and BC be overlapping pairs, then AC might be
   not overlapping, one possible reason of which is both A and C are inactive. One phrase has the lowest priority means
   its priority is lower than that of its every overlapping phrases.
   For a list of overlapping-phrasal tuples,
   (1) If there is not any overlapping phrase, return ((-1,-1),[],-1), namely 'nilPhra'.
   (2) From the first pair of overlapping phrases, select the lower-priority phrase by GeneBase, get the phrase's
       related overlapping pairs from all unChecked pairs. If there is no related pair, return the phrase; otherwise
       recursively call this function on all unChecked Overlapping pairs and the low priority phrase-related overlapping pairs.
   (3) To support prior value 'Noth', which means the two checked overlapping phrases should be thrown away, this function return a tuple of phrases.
 -}

findPhraWithLowestPrio :: [(PhraCate,PhraCate)] -> [(PhraCate,PhraCate)] -> [OverPair] -> IO (PhraCate, PhraCate)
findPhraWithLowestPrio unCheckedOps ops overPairs = do
    if ops == []
      then return (nilPhra, nilPhra)                                   -- This is the border condition, usually not occurs.
      else do
        let x = head ops
        let xs = [op| op <-unCheckedOps, op /= x]
        let pc1 = fst x
        let pc2 = snd x
        pri <- getPrior overPairs pc1 pc2                              -- Find priority from a list of 'OverPair'
        let pcps1 = [y| y <- xs, (fst y == pc1) || (snd y == pc1)]     -- [(PhraCate,PhraCate)] related with pc1
        let pcps2 = [y| y <- xs, (fst y == pc2) || (snd y == pc2)]     -- [(PhraCate,PhraCate)] related with pc2
        let pcps = pcps1 ++ pcps2
        case pri of
          Lp -> if pcps2 /= []
                  then findPhraWithLowestPrio xs pcps2 overPairs
                  else return (nilPhra, pc2)
          Rp -> if pcps1 /= []
                  then findPhraWithLowestPrio xs pcps1 overPairs
                  else return (pc1, nilPhra)
          Noth -> if pcps /= []                                        -- Prior value Noth means the pair of phrases should be removed.
                    then findPhraWithLowestPrio xs pcps overPairs
                    else return (pc1, pc2)                             -- Both Left and right phrases should be removed.

{- Select <prior> from a list of 'OverPair' where matching given an overlapping pair of phrases.
   <Just Lp> means <leftOver> should remains while <rightOver> should be abandoned, and <Just Rp> means the contrary.
   If inquire fails, return Nothing.
   Pruning is a recursive process. After removing one phrase from transitive closure, neighbours of some phrases will
   change. Before pruning, all overlapping phrases are assigned with priorities, and these priorities are hoped to
   keep valid during the whole process of pruning. So, querying structural genes in Table stru_gene is replaced by
   querying overlapping phrases in a list of 'OverPair', which is created before pruning.
 -}

getPrior :: [OverPair] -> PhraCate -> PhraCate -> IO Prior
getPrior [] _ _ = return Noth                   -- Defaultly, two overlapping phrases are not willingly to remain.
getPrior (op:ops) pc1 pc2 = do
    let lo = fst3 op                            -- Left-overlapping phrase
    let ro = snd3 op                            -- Right-overlapping phrase
    if (equalPhra lo pc1 && equalPhra ro pc2)
      then return (thd3 op)
      else getPrior ops pc1 pc2

getPrior' :: [OverPair] -> PhraCate -> PhraCate -> Maybe Prior
getPrior' [] _ _ = Nothing                      -- Fail in searching the given overlapping phrases in the set of [(PhraCate, PhraCate, Prior)].
getPrior' (op:ops) pc1 pc2 = do
    let lo = fst3 op                            -- Left-overlapping phrase
    let ro = snd3 op                            -- Right-overlapping phrase
    if (equalPhra lo pc1 && equalPhra ro pc2)
      then Just (thd3 op)
      else getPrior' ops pc1 pc2

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
    | st1 == st2 && st1 + sp1 < st2 + sp2 &&  notElem4Phrase pc2 (findDescen pc1 pcs) = 3      -- Left-inclusive overlap
    | st1 < st2  && st1 + sp1 == st2 + sp2 && notElem4Phrase pc1 (findDescen pc2 pcs) = 4     -- Right-inclusive overlap
    | st1 < st2  && st1 + sp1 > st2 + sp2 && notElem4Phrase pc1 (findDescen pc2 pcs) = 5      -- Two-end inclusive overlap
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
    | pc == nilPhra = clo
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

        forestByTipGrow = [map (\x -> [fst x | notElem (fst x) t] ++ [snd x | notElem (snd x) t] ++ t ) splOfATip | splOfATip <- splOfAllTips]           -- [[[PhraCate]]]

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
        ot = quickSort4Phrase  t       -- Such that there exists left parent << node << right parent for any node.
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

-- Show all phrases.
showNPhraCate' :: [PhraCate] -> IO ()
showNPhraCate' [] = return ()
showNPhraCate' (pc:pcs) = do
    showPhraCate' pc
    putStrLn ""
    showNPhraCate' pcs

showPhraCate' :: PhraCate -> IO ()
showPhraCate' pc = do
--  putStr (show pc)           -- Function 'show' converts Chinese characters to [char].
    putStr $ "((" ++ show (stOfCate pc) ++ "," ++ show (spOfCate pc) ++ "),["
    putNCtsca' (ctspaOfCate pc)
    putStr $ "]," ++ show (ssOfCate pc) ++ ")"

putNCtsca' :: [(Category,Tag,Seman,PhraStru,Act)] -> IO ()
putNCtsca' [] = putStr ""
putNCtsca' [x] = putStr $ "(" ++ show (fst5 x) ++ "," ++ (snd5 x) ++ "," ++ (thd5 x) ++ "," ++ (fth5 x) ++ "," ++ show (fif5 x) ++ ")"
putNCtsca' (x:xs) = do
    putStr $ "(" ++ show (fst5 x) ++ "," ++ (snd5 x) ++ "," ++ (thd5 x) ++ "," ++ (fth5 x) ++ "," ++ show (fif5 x) ++ "),"
    putNCtsca' xs
