-- Copyright (c) 2019-2025 China University of Water Resources and Electric Power
-- All rights reserved.

module OutputTest (
    outputTest        -- IO()
    ) where

import Category
import Phrase
import Parse
import Output
import Utils
import Clustering

outputTest :: IO()
outputTest = do
    putStrLn "Output:"

    putStrLn "The result of showNCate [(np,\"Frank'\"),((s\\.np)/.np,\"loves'\"),(np,\"Mary'\")] is"
    let c1 = (getCateFromString "np", "Frank'")
    let c2 = (getCateFromString "(s\\.np)/.np", "loves'")
    let c3 = (getCateFromString "np", "Mary'")
    showNCate [c1,c2,c3]
    putStrLn "[Here line feed is added manually]"
    putStrLn ""

    putStrLn "The result of showPhraCate ((1,1),[(s\\.np, \">\", \"loves' Mary'\", \"VO\", True)],2) is"
    let c = getCateFromString "s\\.np"
    showPhraCate ((1,1),[(c, ">", "loves' Mary'", "VO", True)],2)
    putStrLn ""

    putStrLn "The result of showNPhraCateLn [((0,0),[(np, \"Desig\", \"Frank'\",\"DE\",True)],0),((1,1),[(s\\.np, \">\", \"makes' a_joke'\",\"VO\",True)],2)] is"
    let c1 = getCateFromString "np"
    let c2 = getCateFromString "s\\.np"
    showNPhraCate [((0,0),[(c1, "Desig", "Frank'", "DE", True)],0), ((1,1),[(c2, ">", "makes' a_joke'", "VO", True)],2)]
    putStrLn ""

    putStrLn "The result of showNPhraCateListLn [[]] is"
    showNPhraCateListLn [[]]
    putStrLn ""

    putStrLn "The result of showNPhraCatePair [(((0,0),[(np, \"Desig\", \"Frank'\",\"DE\", True)],0),((1,0),[(s\\.np, \"Desig\", \"loves'\", \"DE\", True)],1)), (((2,0),[(np/.np, \"Desig\", \"beautiful'\", \"DE\", True)],2),((3,0),[(np, \"Desig\", \"Mary'\", \"DE\", True)],3))] is"
    let c1 = getCateFromString "np"
    let c2 = getCateFromString "s\\.np"
    let c3 = getCateFromString "np/.np"
    let c4 = getCateFromString "np"
    showNPhraCatePair [(((0,0),[(c1, "Desig", "Frank'","DE",True)],0),((1,0),[(c2, "Desig", "loves'","DE",True)],1)),(((2,0),[(c3, "Desig", "beautiful'", "DE", True)],2),((3,0),[(c4, "Desig", "Mary'", "DE", True)],3))]
    putStrLn ""

    putStrLn "The result of showNPhraCatePairList [[(((0,0),[(np, \"Desig\", \"Frank'\",\"DE\",True)],0),((1,0),[((s\\.np)/.np, \"Desig\", \"loves'\",\"DE\",True)],1)), (((2,0),[(np/.np, \"Desig\", \"beautiful'\",\"DE\",True)],2),((3,0),[(np, \"Desig\", \"Mary'\",\"DE\",True)],3))],[(((0,1),[(s\\.np, \">T->B\", \"Frank' loves'\",\"OE\",True)],1),((2,1),[(np, \">\", \"beautiful' Mary'\",\"AHn\",True)],3))]] is"
    let c11 = getCateFromString "np"
    let c12 = getCateFromString "(s\\.np)/.np"
    let c13 = getCateFromString "np/.np"
    let c14 = getCateFromString "np"
    let c21 = getCateFromString "s\\.np"
    let c22 = getCateFromString "np"
    let pc11 = createPhraCate 0 0 [(c11, "Desig", "Frank'", "DE", True)] 0
    let pc12 = createPhraCate 1 0 [(c12, "Desig", "loves'", "DE", True)] 1
    let pc13 = createPhraCate 2 0 [(c13, "Desig", "beautiful'", "DE", True)] 2
    let pc14 = createPhraCate 3 0 [(c14, "Desig", "Mary'", "DE", True)] 3
    let pc21 = createPhraCate 0 1 [(c21, ">T->B", "Frank', loves'", "OE", True)] 1
    let pc22 = createPhraCate 2 1 [(c22, ">", "beautiful', Mary'", "AHn", True)] 3
    showNPhraCatePairList [[(pc11,pc12),(pc13,pc14)],[(pc21,pc22)]]
    putStrLn ""

    putStrLn "The result of showForest [[((0,0),[(np, \"Desig\", \"Frank'\",\"DE\",True)],0),((1,0),[((s\\.np)/.np, \"Desig\", \"loves'\",\"DE\",True)],1), ((2,0),[(np, \"Desig\", \"Mary'\",\"DE\",True)],2),((0,1),[(s/.np, \">T->B\", \"Frank' loves'\",\"OE\",True)],1),((0,2),[(s, \">\", \"(Frank' loves') Mary'\",\"NR\",True)],2)] [((0,0),[(np, \"Desig\", \"Frank'\",\"DE\",True)],0),((1,0),[((s\\.np)/.np, \"Desig\", \"loves'\",\"DE\",True)],1),((2,0),[(np, \"Desig\", \"Mary'\",\"DE\",True)],2),((1,1),[(s\\.np, \">\", \"loves' Mary'\",\"VO\",True)],2),((0,2),[(s, \"<\", \"(loves' Mary') Frank'\",\"SP\",True)],1)]] is"
    let c11 = getCateFromString "np"
    let c12 = getCateFromString "(s\\.np)/.np"
    let c13 = getCateFromString "np"
    let c21 = getCateFromString "s\\.np"
    let c22 = getCateFromString "s/.np"
    let c31 = getCateFromString "s"
    let pc11 = createPhraCate 0 0 [(c11, "Desig", "Frank'", "DE", True)] 0
    let pc12 = createPhraCate 1 0 [(c12, "Desig", "loves'", "DE", True)] 1
    let pc13 = createPhraCate 2 0 [(c13, "Desig", "Mary'", "DE", True)] 2
    let pc21 = createPhraCate 0 1 [(c21,">", "loves' Mary'","DE", True)] 1
    let pc22 = createPhraCate  1 1 [(c22, ">T->B", "Frank' loves'" , "OE", True)] 2
    let pc31 = createPhraCate 0 2 [(c31,">", "(Frank' loves') Mary'" , "NR", True)] 2
    let pc32 = createPhraCate 0 2 [(c31,"<", "(loves' Mary') Frank'" , "SP", True)] 1
    showForest [[pc11,pc12,pc13,pc22,pc31],[pc11,pc12,pc13,pc21,pc32]]
    putStrLn ""

    putStrLn "The result of showTree [[((0,0),[(np, \"Desig\", \"Frank'\", \"DE\",True)],0),((1,0),[((s\\.np)/.np, \"Desig\", \"loves'\",\"DE\",True)],1), ((2,0),[(np, \"Desig\", \"Mary'\",\"DE\",True)],2)],[((0,1),[(s/.np, \">T->B\", \"Frank' loves'\",\"OE\",True)],1)],[((0,2),[(s, \">\", \"(Frank' loves') Mary'\",\"NR\",True)],2)]] is"
    let c11 = getCateFromString "np"
    let c12 = getCateFromString "(s\\.np)/.np"
    let c13 = getCateFromString "np"
    let c21 = getCateFromString "s/.np"
    let c31 = getCateFromString "s"
    let pc11 = createPhraCate 0 0 [(c11, "Desig", "Frank'", "DE", True)] 0
    let pc12 = createPhraCate 1 0 [(c12, "Desig", "loves'", "DE", True)] 1
    let pc13 = createPhraCate 2 0 [(c13, "Desig", "Mary'", "DE", True)] 2
    let pc21 = createPhraCate 0 1 [(c21, ">T->B", "Frank' loves'" , "OE", True)] 1
    let pc31 = createPhraCate 0 2 [(c31,">", "(Frank' loves') Mary'" , "NR", True)] 2
    showTree [[pc11,pc12,pc13],[pc21],[pc31]]
    putStrLn ""

    putStrLn "The result of drawLine 7 is"
    drawLine 7
    putStrLn "[Here line feed is added]"
    putStrLn ""

    putStrLn "The result of nSpace 7 is"
    nSpace 7
    putStrLn "[Here line feed is added]"
    putStrLn ""

    putStrLn "The result of getCateWidth ((0,0),[(np, \"Desig\", \"Frank'\", \"DE\",True)],0) [[((0,0),[(np, \"Desig\", \"Frank'\", \"DE\",True)],0),((1,0),[((s\\.np)/.np, \"Desig\", \"loves'\",\"DE\",True)],1), ((2,0),[(np, \"Desig\", \"Mary'\",\"DE\",True)],2)],[((0,1),[(s/.np, \">T->B\"), \"Frank' loves'\",\"OE\",True],1)],[((0,2),[(s, \">\", \"(Frank' loves') Mary'\",\"NR\",True)],2)]] is "
    let c00 = getCateFromString "np"
    let c10 = getCateFromString "(s\\.np)/.np"
    let c20 = getCateFromString "np"
    let c01 = getCateFromString "s/.np"
    let c02 = getCateFromString "s"
    let pc00 = createPhraCate 0 0 [(c11, "Desig", "Frank'", "DE", True)] 0
    let pc10 = createPhraCate 1 0 [(c12, "Desig", "loves'", "DE", True)] 1
    let pc20 = createPhraCate 2 0 [(c13, "Desig", "Mary'", "DE", True)] 2
    let pc01 = createPhraCate 0 1 [(c21, ">T->B", "Frank' loves'" , "OE", True)] 1
    let pc02 = createPhraCate 0 2 [(c31,">", "(Frank' loves') Mary'" , "NR", True)] 2
    let width = getCateWidth pc00 [[pc00,pc10,pc20],[pc01],[pc02]]
    putStrLn (show width)

    putStrLn "The result of findPhraStartPos ((0,0),[(np, \"Desig\", \"Frank\",\"DE\",True)],0) [[((0,0),[(np, \"Desig\", \"Frank'\",\"DE\",True)],0),((1,0),[((s\\.np)/.np, \"Desig\", \"loves'\",\"DE\",True)],1), ((2,0),[(np, \"Desig\", \"Mary'\",\"DE\",True)],2)],[((0,1),[(s/.np, \">T->B\", \"Frank' loves'\",\"OE\",True)],1)],[((0,2),[(s, \">\", \"(Frank' loves') Mary'\",\"NR\",True)],2)]] is"
    let c00 = getCateFromString "np"
    let c10 = getCateFromString "(s\\.np)/.np"
    let c20 = getCateFromString "np"
    let c01 = getCateFromString "s/.np"
    let c02 = getCateFromString "s"
    let pc00 = createPhraCate 0 0 [(c00, "Desig", "Frank'", "DE", True)] 0
    let pc10 = createPhraCate 1 0 [(c10, "Desig", "loves'", "DE", True)] 1
    let pc20 = createPhraCate 2 0 [(c20, "Desig", "Mary'", "DE", True)] 2
    let pc01 = createPhraCate 0 1 [(c01, ">T->B", "Frank' loves'" , "OE", True)] 1
    let pc02 = createPhraCate 0 2 [(c02,">", "(Frank' loves') Mary'" , "NR", True)] 2
    let pos = findPhraStartPos pc00 [[pc00,pc10,pc20],[pc01],[pc02]]
    putStrLn (show pos)
    putStrLn ""

    putStrLn "The result of findPhraStartPos ((1,0),[((s\\.np)/.np, \"Desig\", \"loves'\",\"DE\",True)],1) [[((0,0),[(np, \"Desig\", \"Frank'\",\"DE\",True)],0),((1,0),[((s\\.np)/.np, \"Desig\", \"loves'\",\"DE\",True)],1), ((2,0),[(np, \"Desig\", \"Mary'\",\"DE\",True)],2)],[((0,1),[(s/.np, \">T->B\", \"Frank' loves'\",\"OE\",True)],1)],[((0,2),[(s, \">\", \"(Frank' loves') Mary'\",\"NR\",True)],2)]] is"
    let c00 = getCateFromString "np"
    let c10 = getCateFromString "(s\\.np)/.np"
    let c20 = getCateFromString "np"
    let c01 = getCateFromString "s/.np"
    let c02 = getCateFromString "s"
    let pc00 = createPhraCate 0 0 [(c00, "Desig", "Frank'", "DE", True)] 0
    let pc10 = createPhraCate 1 0 [(c10, "Desig", "loves'", "DE", True)] 1
    let pc20 = createPhraCate 2 0 [(c20, "Desig", "Mary'", "DE", True)] 2
    let pc01 = createPhraCate 0 1 [(c01, ">T->B", "Frank' loves'" , "OE", True)] 1
    let pc02 = createPhraCate 0 2 [(c02,">", "(Frank' loves') Mary'" , "NR", True)] 2
    let pos = findPhraStartPos pc10 [[pc00,pc10,pc20],[pc01],[pc02]]
    putStrLn (show pos)
    putStrLn ""

    putStrLn "The result of findPhraStartPos ((1,1),[(s\\.np, \">\", \"loves' Mary'\",\"VO\",True)],2) [[((0,0),[(np, \"Desig\", \"Frank'\",\"DE\",True)],0),((1,0),[((s\\.np)/.np, \"Desig\", \"loves'\",\"DE\",True)],1), ((2,0),[(np, \"Desig\", \"Mary'\",\"DE\",True)],2)],[((0,1),[(s/.np, \">T->B\", \"Frank' loves'\",\"OE\",True)],1)],[((0,2),[(s, \">\", \"(Frank' loves') Mary'\",\"NR\",True)],2)]] is"
    let c00 = getCateFromString "np"
    let c10 = getCateFromString "(s\\.np)/.np"
    let c20 = getCateFromString "np"
    let c01 = getCateFromString "s/.np"
    let c02 = getCateFromString "s"
    let pc00 = createPhraCate 0 0 [(c00, "Desig", "Frank'", "DE", True)] 0
    let pc10 = createPhraCate 1 0 [(c10, "Desig", "loves'", "DE", True)] 1
    let pc20 = createPhraCate 2 0 [(c20, "Desig", "Mary'", "DE", True)] 2
    let pc01 = createPhraCate 0 1 [(c01, ">T->B", "Frank' loves'" , "OE", True)] 1
    let pc02 = createPhraCate 0 2 [(c02,">", "(Frank' loves') Mary'" , "NR", True)] 2
    let pos = findPhraStartPos pc01 [[pc00,pc10,pc20],[pc01],[pc02]]
    putStrLn (show pos)
    putStrLn ""

    putStrLn "The result of showNCateLine 0 [((0,0),[(np, \"Desig\", \"Frank'\",\"DE\",True)],0),((1,0),[((s\\.np)/.np, \"Desig\", \"loves'\",\"DE\",True)],1),((2,0),[(np, \"Desig\", \"Mary'\",\"DE\",True)],2)] [[((0,0),[(np, \"Desig\", \"Frank'\",\"DE\",True)],0),((1,0),[((s\\.np)/.np, \"Desig\", \"loves'\",\"DE\",True)],1), ((2,0),[(np, \"Desig\", \"Mary'\",\"DE\",True)],2)],[((1,1),[(s\\.np, \">\",\"loves' Mary'\",\"VO\",True)],2)],[((0,2),[(s, \"<\", \"(loves' Mary') Frank'\",\"SP\",True)],1)]] is"
    let c11 = getCateFromString "np"
    let c12 = getCateFromString "(s\\.np)/.np"
    let c13 = getCateFromString "np"
    let c21 = getCateFromString "s\\.np"
    let c31 = getCateFromString "s"
    let pc11 = createPhraCate 0 0 [(c11, "Desig", "Frank'", "DE", True)] 0
    let pc12 = createPhraCate 1 0 [(c12, "Desig", "loves'", "DE", True)] 1
    let pc13 = createPhraCate 2 0 [(c13, "Desig", "Mary'", "DE", True)] 2
    let pc21 = createPhraCate 1 1 [(c21, ">", "loves' Mary'","VO", True)] 2
    let pc31 = createPhraCate 0 2 [(c31,"<", "(loves' Mary') Frank'" , "SP", True)] 1
    showNCateLine 0 [pc11,pc12,pc13] [[pc11,pc12,pc13],[pc21],[pc31]]
    putStrLn "[Here line feed is added]"
    putStrLn ""

    putStrLn "The result of showNCateSymb 0 [((0,0),[(np, \"Desig\", \"Frank'\",\"DE\",True)],0),((1,0),[((s\\.np)/.np, \"Desig\", \"loves'\",\"DE\",True)],1),((2,0),[(np, \"Desig\", \"Mary'\",\"DE\",True)],2)] [[((0,0),[(np, \"Desig\", \"Frank'\",\"DE\",True)],0),((1,0),[((s\\.np)/.np, \"Desig\", \"loves'\",\"DE\",True)],1), ((2,0),[(np, \"Desig\", \"Mary'\",\"DE\",True)],2)],[((1,1),[(s\\.np, \">\",\"loves' Mary'\",\"VO\",True)],2)],[((0,2),[(s, \"<\", \"(loves' Mary') Frank'\",\"SP\",True)],1)]] is"
    let c11 = getCateFromString "np"
    let c12 = getCateFromString "(s\\.np)/.np"
    let c13 = getCateFromString "np"
    let c21 = getCateFromString "s\\.np"
    let c31 = getCateFromString "s"
    let pc11 = createPhraCate 0 0 [(c11, "Desig", "Frank'", "DE", True)] 0
    let pc12 = createPhraCate 1 0 [(c12, "Desig", "loves'", "DE", True)] 1
    let pc13 = createPhraCate 2 0 [(c13, "Desig", "Mary'", "DE", True)] 2
    let pc21 = createPhraCate 1 1 [(c21, ">", "loves' Mary'","VO", True)] 2
    let pc31 = createPhraCate 0 2 [(c31,"<", "(loves' Mary') Frank'" , "SP", True)] 1
    showNCateSymb 0 [pc11,pc12,pc13] [[pc11,pc12,pc13],[pc21],[pc31]]
    putStrLn "[Here line feed is added]"
    putStrLn ""

    putStrLn "The result of showNCateLine 0 [((1,1),[(s\\.np, \">\", \"loves' Mary'\",\"VO\",True)],2)] [[((0,0),[(np, \"Desig\", \"Frank'\",\"DE\",True)],0),((1,0),[((s\\.np)/.np, \"Desig\", \"loves'\",\"DE\",True)],1), ((2,0),[(np, \"Desig\", \"Mary'\",\"DE\",True)],2)],[((1,1),[(s\\.np, \">\",\"loves' Mary'\",\"VO\",True)],2)],[((0,2),[(s, \"<\", \"(loves' Mary') Frank'\",\"SP\",True)],1)]] is"
    let c11 = getCateFromString "np"
    let c12 = getCateFromString "(s\\.np)/.np"
    let c13 = getCateFromString "np"
    let c21 = getCateFromString "s\\.np"
    let c31 = getCateFromString "s"
    let pc11 = createPhraCate 0 0 [(c11, "Desig", "Frank'", "DE", True)] 0
    let pc12 = createPhraCate 1 0 [(c12, "Desig", "loves'", "DE", True)] 1
    let pc13 = createPhraCate 2 0 [(c13, "Desig", "Mary'", "DE", True)] 2
    let pc21 = createPhraCate 1 1 [(c21, ">", "loves' Mary'","VO", True)] 2
    let pc31 = createPhraCate 0 2 [(c31,"<", "(loves' Mary') Frank'" , "SP", True)] 1
    showNCateLine 0 [pc21] [[pc11,pc12,pc13],[pc21],[pc31]]
    putStrLn "[Here line feed is added]"
    putStrLn ""

    putStrLn "The result of showNCateSymb 0 [((1,1),[(s\\.np, \">\", \"loves' Mary'\",\"VO\",True)],2)] [[((0,0),[(np, \"Desig\", \"Frank'\",\"DE\",True)],0),((1,0),[((s\\.np)/.np, \"Desig\", \"loves'\",\"DE\",True)],1), ((2,0),[(np, \"Desig\", \"Mary'\",\"DE\",True)],2)],[((1,1),[(s\\.np, \">\",\"loves' Mary'\",\"VO\",True)],2)],[((0,2),[(s, \"<\", \"(loves' Mary') Frank'\",\"SP\",True)],1)]] is"
    let c11 = getCateFromString "np"
    let c12 = getCateFromString "(s\\.np)/.np"
    let c13 = getCateFromString "np"
    let c21 = getCateFromString "s\\.np"
    let c31 = getCateFromString "s"
    let pc11 = createPhraCate 0 0 [(c11, "Desig", "Frank'", "DE", True)] 0
    let pc12 = createPhraCate 1 0 [(c12, "Desig", "loves'", "DE", True)] 1
    let pc13 = createPhraCate 2 0 [(c13, "Desig", "Mary'", "DE", True)] 2
    let pc21 = createPhraCate 1 1 [(c21, ">", "loves' Mary'","VO", True)] 2
    let pc31 = createPhraCate 0 2 [(c31,"<", "(loves' Mary') Frank'" , "SP", True)] 1
    showNCateSymb 0 [pc21] [[pc11,pc12,pc13],[pc21],[pc31]]
    putStrLn "[Here line feed is added]"
    putStrLn ""

    putStrLn "The result of showNCateLine 0 [((0,2),[(s, \"<\", \"(loves' Mary') Frank'\",True)],1)] [[((0,0),[(np, \"Desig\", \"Frank'\",\"DE\",True)],0),((1,0),[((s\\.np)/.np, \"Desig\", \"loves'\",\"DE\",True)],1), ((2,0),[(np, \"Desig\", \"Mary'\",\"DE\",True)],2)],[((1,1),[(s\\.np, \">\",\"loves' Mary'\",\"VO\",True)],2)],[((0,2),[(s, \"<\", \"(loves' Mary') Frank'\",\"SP\",True)],1)]] is"
    let c11 = getCateFromString "np"
    let c12 = getCateFromString "(s\\.np)/.np"
    let c13 = getCateFromString "np"
    let c21 = getCateFromString "s\\.np"
    let c31 = getCateFromString "s"
    let pc11 = createPhraCate 0 0 [(c11, "Desig", "Frank'", "DE", True)] 0
    let pc12 = createPhraCate 1 0 [(c12, "Desig", "loves'", "DE", True)] 1
    let pc13 = createPhraCate 2 0 [(c13, "Desig", "Mary'", "DE", True)] 2
    let pc21 = createPhraCate 1 1 [(c21, ">", "loves' Mary'","VO", True)] 2
    let pc31 = createPhraCate 0 2 [(c31,"<", "(loves' Mary') Frank'" , "SP", True)] 1
    showNCateLine 0 [pc31] [[pc11,pc12,pc13],[pc21],[pc31]]
    putStrLn "[Here line feed is added]"
    putStrLn ""

    putStrLn "The result of showNCateSymb 0 [((0,2),[(s, \"<\", \"(loves' Mary') Frank'\",True)],1)] [[((0,0),[(np, \"Desig\", \"Frank'\",\"DE\",True)],0),((1,0),[((s\\.np)/.np, \"Desig\", \"loves'\",\"DE\",True)],1), ((2,0),[(np, \"Desig\", \"Mary'\",\"DE\",True)],2)],[((1,1),[(s\\.np, \">\",\"loves' Mary'\",\"VO\",True)],2)],[((0,2),[(s, \"<\", \"(loves' Mary') Frank'\",\"SP\",True)],1)]] is"
    let c11 = getCateFromString "np"
    let c12 = getCateFromString "(s\\.np)/.np"
    let c13 = getCateFromString "np"
    let c21 = getCateFromString "s\\.np"
    let c31 = getCateFromString "s"
    let pc11 = createPhraCate 0 0 [(c11, "Desig", "Frank'", "DE", True)] 0
    let pc12 = createPhraCate 1 0 [(c12, "Desig", "loves'", "DE", True)] 1
    let pc13 = createPhraCate 2 0 [(c13, "Desig", "Mary'", "DE", True)] 2
    let pc21 = createPhraCate 1 1 [(c21, ">", "loves' Mary'","VO", True)] 2
    let pc31 = createPhraCate 0 2 [(c31,"<", "(loves' Mary') Frank'" , "SP", True)] 1
    showNCateSymb 0 [pc31] [[pc11,pc12,pc13],[pc21],[pc31]]
    putStrLn "[Here line feed is added]"
    putStrLn ""

    putStrLn "The result of showTreeStru [[((0,0),[(np, \"Desig\", \"Frank'\",\"DE\",True)],0),((1,0),[((s\\.np)/.np, \"Desig\", \"loves'\",\"DE\",True)],1), ((2,0),[(np, \"Desig\", \"Mary'\",\"DE\",True)],2)],[((1,1),[(s\\.np, \">\",\"loves' Mary'\",\"VO\",True)],2)],[((0,2),[(s, \"<\", \"(loves' Mary') Frank'\",\"SP\",True)],1)]] [[((0,0),[(np, \"Desig\", \"Frank'\",\"DE\",True)],0),((1,0),[((s\\.np)/.np, \"Desig\", \"loves'\",\"DE\",True)],1), ((2,0),[(np, \"Desig\", \"Mary'\",\"DE\",True)],2)],[((1,1),[(s\\.np, \">\",\"loves' Mary'\",\"VO\",True)],2)],[((0,2),[(s, \"<\", \"(loves' Mary') Frank'\",\"SP\",True)],1)]] is"
    let c11 = getCateFromString "np"
    let c12 = getCateFromString "(s\\.np)/.np"
    let c13 = getCateFromString "np"
    let c21 = getCateFromString "s\\.np"
    let c31 = getCateFromString "s"
    let pc11 = createPhraCate 0 0 [(c11, "Desig", "Frank'", "DE", True)] 0
    let pc12 = createPhraCate 1 0 [(c12, "Desig", "loves'", "DE", True)] 1
    let pc13 = createPhraCate 2 0 [(c13, "Desig", "Mary'", "DE", True)] 2
    let pc21 = createPhraCate 1 1 [(c21, ">", "loves' Mary'","VO", True)] 2
    let pc31 = createPhraCate 0 2 [(c31,"<", "(loves' Mary') Frank'" , "SP", True)] 1
    showTreeStru [[pc11,pc12,pc13],[pc21],[pc31]] [[pc11,pc12,pc13],[pc21],[pc31]]
    putStrLn ""

    putStrLn "The result of showTreeStru [[((0,0),[(np, \"Desig\", \"Frank'\", \"DE\",True)],0),((1,0),[((s\\.np)/.np, \"Desig\", \"loves'\",\"DE\",True)],1), ((2,0),[(np, \"Desig\", \"Mary'\",\"DE\",True)],2)],[((0,1),[(s/.np, \">T->B\"), \"Frank' loves'\",\"OE\",True],1)],[((0,2),[(s, \">\", \"(Frank' loves') Mary'\",\"NR\",True)],2)]] [[((0,0),[(np, \"Desig\", \"Frank'\", \"DE\",True)],0),((1,0),[((s\\.np)/.np, \"Desig\", \"loves'\",\"DE\",True)],1), ((2,0),[(np, \"Desig\", \"Mary'\",\"DE\",True)],2)],[((0,1),[(s/.np, \">T->B\"), \"Frank' loves'\",\"OE\",True],1)],[((0,2),[(s, \">\", \"(Frank' loves') Mary'\",\"NR\",True)],2)]] is"
    let c11 = getCateFromString "np"
    let c12 = getCateFromString "(s\\.np)/.np"
    let c13 = getCateFromString "np"
    let c21 = getCateFromString "s/.np"
    let c31 = getCateFromString "s"
    let pc11 = createPhraCate 0 0 [(c11, "Desig", "Frank'", "DE", True)] 0
    let pc12 = createPhraCate 1 0 [(c12, "Desig", "loves'", "DE", True)] 1
    let pc13 = createPhraCate 2 0 [(c13, "Desig", "Mary'", "DE", True)] 2
    let pc21 = createPhraCate 0 1 [(c21, ">T->B", "Frank' loves'" , "OE", True)] 1
    let pc31 = createPhraCate 0 2 [(c31,">", "(Frank' loves') Mary'" , "NR", True)] 2
    showTreeStru [[pc11,pc12,pc13],[pc21],[pc31]] [[pc11,pc12,pc13],[pc21],[pc31]]
    putStrLn ""

    putStrLn "The result of showForestWithTreeStru [[((0,0),[(np, \"Desig\", \"Frank'\",\"DE\",True)],0),((1,0),[((s\\.np)/.np, \"Desig\", \"loves'\",\"DE\",True)],1), ((2,0),[(np, \"Desig\", \"Mary'\",\"DE\",True)],2), ((0,1),[(s/.np, \">T->B\", \"Frank' loves'\",\"OE\",True)],1), ((0,2),[(s, \">\", \"(Frank' loves') Mary'\",\"NR\",True)],2)] [((0,0),[(np, \"Desig\", \"Frank'\",\"DE\",True)],0),((1,0),[((s\\.np)/.np, \"Desig\", \"loves'\",\"DE\",True)],1), ((2,0),[(np, \"Desig\", \"Mary'\",\"DE\",True)],2)],[((1,1),[(s\\.np, \">\", \"loves' Mary'\",\"VO\",True)],2)],[((0,2),[(s, \"<\", \"(loves' Mary') Frank'\",\"SP\",True)],1)]] ] is"
    let c11 = getCateFromString "np"
    let c12 = getCateFromString "(s\\.np)/.np"
    let c13 = getCateFromString "np"
    let c21 = getCateFromString "s/.np"
    let c22 = getCateFromString "s\\.np"
    let c31 = getCateFromString "s"
    let c32 = getCateFromString "s"
    let pc11 = createPhraCate 0 0 [(c11, "Desig", "Frank'", "DE", True)] 0
    let pc12 = createPhraCate 1 0 [(c12, "Desig", "loves'", "DE", True)] 1
    let pc13 = createPhraCate 2 0 [(c13, "Desig", "Mary'", "DE", True)] 2
    let pc21 = createPhraCate 0 1 [(c21, ">T->B", "Frank' loves'" , "OE", True)] 1
    let pc22 = createPhraCate  1 1 [(c22,">", "loves' Mary'","VO", True)] 2
    let pc31 = createPhraCate 0 2 [(c31,">", "(Frank' loves') Mary'" , "NR", True)] 1
    let pc32 = createPhraCate 0 2 [(c32,"<", "(loves' Mary') Frank'" , "SP", True)] 2
    showForestWithTreeStru [[pc11,pc12,pc13,pc21,pc32], [pc11,pc12,pc13,pc22,pc31]]
    putStrLn ""

    putStrLn "The result of showCateStartPos [[((0,0),[(np, \"Desig\", \"Frank'\", \"DE\",True)],0),((1,0),[((s\\.np)/.np, \"Desig\", \"loves'\",\"DE\",True)],1), ((2,0),[(np, \"Desig\", \"Mary'\",\"DE\",True)],2)],[((0,1),[(s/.np, \">T->B\"), \"Frank' loves'\",\"OE\",True],1)],[((0,2),[(s, \">\", \"(Frank' loves') Mary'\",\"NR\",True)],2)]] [[((0,0),[(np, \"Desig\", \"Frank'\", \"DE\",True)],0),((1,0),[((s\\.np)/.np, \"Desig\", \"loves'\",\"DE\",True)],1), ((2,0),[(np, \"Desig\", \"Mary'\",\"DE\",True)],2)],[((0,1),[(s/.np, \">T->B\"), \"Frank' loves'\",\"OE\",True],1)],[((0,2),[(s, \">\", \"(Frank' loves') Mary'\",\"NR\",True)],2)]] is"
    let c11 = getCateFromString "np"
    let c12 = getCateFromString "(s\\.np)/.np"
    let c13 = getCateFromString "np"
    let c21 = getCateFromString "s/.np"
    let c31 = getCateFromString "s"
    let pc11 = createPhraCate 0 0 [(c11, "Desig", "Frank'", "DE", True)] 0
    let pc12 = createPhraCate 1 0 [(c12, "Desig", "loves'", "DE", True)] 1
    let pc13 = createPhraCate 2 0 [(c13, "Desig", "Mary'", "DE", True)] 2
    let pc21 = createPhraCate 0 1 [(c21, ">T->B", "Frank' loves'" , "OE", True)] 1
    let pc31 = createPhraCate 0 2 [(c31,">", "(Frank' loves') Mary'" , "NR", True)] 2
    showCateStartPos [[pc11,pc12,pc13],[pc21],[pc31]] [[pc11,pc12,pc13],[pc21],[pc31]]
    putStrLn ""

    putStrLn "The result of showForestCateStartPos [[((0,0),[(np, \"Desig\", \"Frank'\",\"DE\",True)],0),((1,0),[((s\\.np)/.np, \"Desig\", \"loves'\",\"DE\",True)],1), ((2,0),[(np, \"Desig\", \"Mary'\",\"DE\",True)],2), ((0,1),[(s/.np, \">T->B\", \"Frank' loves'\",\"OE\",True)],1), ((0,2),[(s, \">\", \"(Frank' loves') Mary'\",\"NR\",True)],2)] [((0,0),[(np, \"Desig\", \"Frank'\",\"DE\",True)],0),((1,0),[((s\\.np)/.np, \"Desig\", \"loves'\",\"DE\",True)],1), ((2,0),[(np, \"Desig\", \"Mary'\",\"DE\",True)],2)],[((1,1),[(s\\.np, \">\", \"loves' Mary'\",\"VO\",True)],2)],[((0,2),[(s, \"<\", \"(loves' Mary') Frank'\",\"SP\",True)],1)]] ] is"
    let c11 = getCateFromString "np"
    let c12 = getCateFromString "(s\\.np)/.np"
    let c13 = getCateFromString "np"
    let c21 = getCateFromString "s/.np"
    let c22 = getCateFromString "s\\.np"
    let c31 = getCateFromString "s"
    let c32 = getCateFromString "s"
    let pc11 = createPhraCate 0 0 [(c11, "Desig", "Frank'", "DE", True)] 0
    let pc12 = createPhraCate 1 0 [(c12, "Desig", "loves'", "DE", True)] 1
    let pc13 = createPhraCate 2 0 [(c13, "Desig", "Mary'", "DE", True)] 2
    let pc21 = createPhraCate 0 1 [(c21, ">T->B", "Frank' loves'" , "OE", True)] 1
    let pc22 = createPhraCate  1 1 [(c22,">", "loves' Mary'","VO", True)] 2
    let pc31 = createPhraCate 0 2 [(c31,">", "(Frank' loves') Mary'" , "NR", True)] 1
    let pc32 = createPhraCate 0 2 [(c32,"<", "(loves' Mary') Frank'" , "SP", True)] 2
    showForestCateStartPos [[pc11,pc12,pc13,pc21,pc32], [pc11,pc12,pc13,pc22,pc31]]
    putStrLn ""

    putStrLn "The result of distPhraSynByIdentity (getCateFromString \"np\", \">\", \"AHn\")) (getCateFromString \"(s\\.np)/.np\", \"<B\", \"AHn\") is"
    let c1 = getCateFromString "np"
    let c2 = getCateFromString "(s\\.np)/.np"
    let t1 = ">"
    let t2 = "<B"
    let p1 = "AHn"
    let p2 = "AHn"
    putStrLn $ show $ distPhraSynByIdentity (c1,t1,p1) (c2,t2,p2)
    putStrLn ""
