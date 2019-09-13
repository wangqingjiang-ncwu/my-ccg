-- Copyright China University of Water Resources and Electric Power (c) 2019
-- All rights reserved.

module OutputTest (
    outputTest        -- IO()
    ) where

import Category
import Parse
import Output

outputTest :: IO()
outputTest = do
    putStrLn "Output:"

    putStrLn "The result of showNStr [\"np\",\"(s\\.np)/.np\",\"np\"] is" 
    showNStr ["np","(s\\.np)/.np","np"]
    putStrLn ""

    putStrLn "The result of showNCate [np,(s\\.np)/.np,np] is"
    let c1 = getCateFromString "np"
    let c2 = getCateFromString "(s\\.np)/.np"
    let c3 = getCateFromString "np"
    showNCate [c1,c2,c3]
    putStrLn "[Here line feed is added manually]"
    putStrLn ""

    putStrLn "The result of showPhraCate ((1,1),[(s\\.np, \">\")],2) is"
    let c = getCateFromString "s\\.np"
    showPhraCate ((1,1),[(c, ">")],2)
    putStrLn ""

    putStrLn "The result of showNPhraCate [((0,0),[(np, \"Desig\")],0),((1,1),[(s\\.np, \">\")],2)] is"
    let c1 = getCateFromString "np"
    let c2 = getCateFromString "s\\.np"
    showNPhraCate [((0,0),[(c1, "Desig")],0), ((1,1),[(c2, ">")],2)]
    putStrLn ""

    putStrLn "The result of showNSplitCate [(((0,0),[(np, \"Desig\")],0),((1,0),[(s\\.np, \"Desig\")],1)), (((2,0),[(np/.np, \"Desig\")],2),((3,0),[(np, \"Desig\")],3))] is"
    let c1 = getCateFromString "np"
    let c2 = getCateFromString "s\\.np"
    let c3 = getCateFromString "np/.np"
    let c4 = getCateFromString "np"
    showNSplitCate [(((0,0),[(c1, "Desig")],0),((1,0),[(c2, "Desig")],1)),(((2,0),[(c3, "Desig")],2),((3,0),[(c4, "Desig")],3))]
    putStrLn ""

    putStrLn "The result of showAllSplitCate [[(((0,0),[(np, \"Desig\")],0),((1,0),[((s\\.np)/.np, \"Desig\")],1)), (((2,0),[(np/.np, \"Desig\")],2),((3,0),[(np, \"Desig\")],3))],[(((0,1),[(s\\.np, \">T->B\")],1),((2,1),[(np, \">\")],3))]] is"
    let c11 = getCateFromString "np"
    let c12 = getCateFromString "(s\\.np)/.np"
    let c13 = getCateFromString "np/.np"
    let c14 = getCateFromString "np"
    let c21 = getCateFromString "s\\.np"
    let c22 = getCateFromString "np"
    let pc11 = createPhraCate 0 0 c11 "Desig" 0
    let pc12 = createPhraCate 1 0 c12 "Desig" 1
    let pc13 = createPhraCate 2 0 c13 "Desig" 2
    let pc14 = createPhraCate 3 0 c14 "Desig" 3
    let pc21 = createPhraCate 0 1 c21 ">T->B" 1
    let pc22 = createPhraCate 2 1 c22 ">" 3
    showAllSplitCate [[(pc11,pc12),(pc13,pc14)],[(pc21,pc22)]]
    putStrLn ""

    putStrLn "The result of showForest [[((0,0),[(np, \"Desig\")],0),((1,0),[((s\\.np)/.np, \"Desig\")],1), ((2,0),[(np, \"Desig\")],2),((0,1),[(s/.np, \">T->B\")],1),((0,2),[(s, \">\")],2)], [((0,0),[(np, \"Desig\")],0),((1,0),[((s\\.np)/.np, \"Desig\")],1),((2,0),[(np, \"Desig\")],2),((1,1),[(s\\.np, \">\")],2),((0,2),[(s, \"<\")],1)]] is"
    let c11 = getCateFromString "np"
    let c12 = getCateFromString "(s\\.np)/.np"
    let c13 = getCateFromString "np"
    let c21 = getCateFromString "s\\.np"
    let c22 = getCateFromString "s/.np"
    let c31 = getCateFromString "s"
    let pc11 = createPhraCate 0 0 c11 "Desig" 0
    let pc12 = createPhraCate 1 0 c12 "Desig" 1
    let pc13 = createPhraCate 2 0 c13 "Desig" 2
    let pc21 = createPhraCate 0 1 c21 ">" 1
    let pc22 = createPhraCate 1 1 c22 ">T->B" 2
    let pc31 = createPhraCate 0 2 c31 ">" 2
    let pc32 = createPhraCate 0 2 c31 "<" 1
    showForest [[pc11,pc12,pc13,pc22,pc31],[pc11,pc12,pc13,pc21,pc32]]
    putStrLn ""

    putStrLn "The result of showTree [[((0,0),[(np, \"Desig\")],0),((1,0),[((s\\.np)/.np, \"Desig\")],1), ((2,0),[(np, \"Desig\")],2)],[((0,1),[(s/.np, \">T->B\")],1)],[((0,2),[(s, \">\")],2)]] is"
    let c11 = getCateFromString "np"
    let c12 = getCateFromString "(s\\.np)/.np"
    let c13 = getCateFromString "np"
    let c21 = getCateFromString "s/.np"
    let c31 = getCateFromString "s"
    let pc11 = createPhraCate 0 0 c11 "Desig" 0
    let pc12 = createPhraCate 1 0 c12 "Desig" 1
    let pc13 = createPhraCate 2 0 c13 "Desig" 2
    let pc21 = createPhraCate 0 1 c21 ">T->B" 1
    let pc31 = createPhraCate 0 2 c31 ">" 2
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

    putStrLn "The result of getCateWidth ((0,0),[(np, \"Desig\")],0) [[((0,0),[(np, \"Desig\")],0),((1,0),[((s\\.np)/.np, \"Desig\")],1), ((2,0),[(np, \"Desig\")],2)],[((0,1),[(s/.np, \">T->B\")],1)],[((0,2),[(s, \">\")],2)]] is"
    let c11 = getCateFromString "np"
    let c12 = getCateFromString "(s\\.np)/.np"
    let c13 = getCateFromString "np"
    let c21 = getCateFromString "s/.np"
    let c31 = getCateFromString "s"
    let pc11 = createPhraCate 0 0 c11 "Desig" 0
    let pc12 = createPhraCate 1 0 c12 "Desig" 1
    let pc13 = createPhraCate 2 0 c13 "Desig" 2
    let pc21 = createPhraCate 0 1 c21 ">T->B" 1
    let pc31 = createPhraCate 0 2 c31 ">" 1
    let width = getCateWidth pc11 [[pc11,pc12,pc13],[pc21],[pc31]]
    putStrLn (show width)
    
    putStrLn "The result of getCateStartPos ((0,0),[(np, \"Desig\")],0) [[((0,0),[(np, \"Desig\")],0),((1,0),[((s\\.np)/.np, \"Desig\")],1), ((2,0),[(np, \"Desig\")],2)],[((0,1),[(s/.np, \">T->B\")],1)],[((0,2),[(s, \">\")],2)]] is"
    let c11 = getCateFromString "np"
    let c12 = getCateFromString "(s\\.np)/.np"
    let c13 = getCateFromString "np"
    let c21 = getCateFromString "s/.np"
    let c31 = getCateFromString "s"
    let pc11 = createPhraCate 0 0 c11 "Desig" 0
    let pc12 = createPhraCate 1 0 c12 "Desig" 1
    let pc13 = createPhraCate 2 0 c13 "Desig" 2
    let pc21 = createPhraCate 0 1 c21 ">T->B" 1
    let pc31 = createPhraCate 0 2 c31 ">" 1
    let pos = getCateStartPos pc11 [[pc11,pc12,pc13],[pc21],[pc31]]
    putStrLn (show pos)
    putStrLn ""

    putStrLn "The result of getCateStartPos ((1,0),[((s\\.np)/.np, \"Desig\")],1) [[((0,0),[(np, \"Desig\")],0),((1,0),[((s\\.np)/.np, \"Desig\")],1), ((2,0),[(np, \"Desig\")],2)],[((0,1),[(s/.np, \">T->B\")],1)],[((0,2),[(s, \">\")],2)]] is"
    let c11 = getCateFromString "np"
    let c12 = getCateFromString "(s\\.np)/.np"
    let c13 = getCateFromString "np"
    let c21 = getCateFromString "s/.np"
    let c31 = getCateFromString "s"
    let pc11 = createPhraCate 0 0 c11 "Desig" 0
    let pc12 = createPhraCate 1 0 c12 "Desig" 1
    let pc13 = createPhraCate 2 0 c13 "Desig" 2
    let pc21 = createPhraCate 0 1 c21 ">T->B" 1
    let pc31 = createPhraCate 0 2 c31 ">" 1
    let pos = getCateStartPos pc12 [[pc11,pc12,pc13],[pc21],[pc31]]
    putStrLn (show pos)
    putStrLn ""

    putStrLn "The result of getCateStartPos ((1,1),[(s\\.np, \">\")],2) [[((0,0),[(np, \"Desig\")],0),((1,0),[((s\\.np)/.np, \"Desig\")],1), ((2,0),[(np, \"Desig\")],2)],[((1,1),[(s\\.np, \">\")],2)],[((0,2),[(s, \"<\")],1)]] is"
    let c11 = getCateFromString "np"
    let c12 = getCateFromString "(s\\.np)/.np"
    let c13 = getCateFromString "np"
    let c21 = getCateFromString "s\\.np"
    let c31 = getCateFromString "s"
    let pc11 = createPhraCate 0 0 c11 "Desig" 0
    let pc12 = createPhraCate 1 0 c12 "Desig" 1
    let pc13 = createPhraCate 2 0 c13 "Desig" 2
    let pc21 = createPhraCate 1 1 c21 ">" 2
    let pc31 = createPhraCate 0 2 c31 "<" 1
    let pos = getCateStartPos pc21 [[pc11,pc12,pc13],[pc21],[pc31]]
    putStrLn (show pos)
    putStrLn ""

    putStrLn "The result of showNCateLine 0 [((0,0),[(np, \"Desig\")],0),((1,0),[((s\\.np)/.np, \"Desig\")],1),((2,0),[(np, \"Desig\")],2)] [[((0,0),[(np, \"Desig\")],0),((1,0),[((s\\.np)/.np, \"Desig\")],1), ((2,0),[(np, \"Desig\")],2)],[((1,1),[(s\\.np, \">\")],2)],[((0,2),[(s, \"<\")],1)]] is"
    let c11 = getCateFromString "np"
    let c12 = getCateFromString "(s\\.np)/.np"
    let c13 = getCateFromString "np"
    let c21 = getCateFromString "s\\.np"
    let c31 = getCateFromString "s"
    let pc11 = createPhraCate 0 0 c11 "Desig" 0
    let pc12 = createPhraCate 1 0 c12 "Desig" 1
    let pc13 = createPhraCate 2 0 c13 "Desig" 2
    let pc21 = createPhraCate 1 1 c21 ">" 2
    let pc31 = createPhraCate 0 2 c31 "<" 1
    showNCateLine 0 [pc11,pc12,pc13] [[pc11,pc12,pc13],[pc21],[pc31]]
    putStrLn "[Here line feed is added]"
    putStrLn ""

    putStrLn "The result of showNCateSymb 0 [((0,0),[(np, \"Desig\")],0),((1,0),[((s\\.np)/.np, \"Desig\")],1),((2,0),[(np, \"Desig\")],2)] [[((0,0),[(np, \"Desig\")],0),((1,0),[((s\\.np)/.np, \"Desig\")],1), ((2,0),[(np, \"Desig\")],2)],[((1,1),[(s\\.np, \">\")],2)],[((0,2),[(s, \"<\")],1)]] is"
    let c11 = getCateFromString "np"
    let c12 = getCateFromString "(s\\.np)/.np"
    let c13 = getCateFromString "np"
    let c21 = getCateFromString "s\\.np"
    let c31 = getCateFromString "s"
    let pc11 = createPhraCate 0 0 c11 "Desig" 0
    let pc12 = createPhraCate 1 0 c12 "Desig" 1
    let pc13 = createPhraCate 2 0 c13 "Desig" 2
    let pc21 = createPhraCate 1 1 c21 ">" 2
    let pc31 = createPhraCate 0 2 c31 "<" 1
    showNCateSymb 0 [pc11,pc12,pc13] [[pc11,pc12,pc13],[pc21],[pc31]]
    putStrLn "[Here line feed is added]"
    putStrLn ""
    
    putStrLn "The result of showNCateLine 0 [((1,1),[(s\\.np, \">\")],2)] [[((0,0),[(np, \"Desig\")],0),((1,0),[((s\\.np)/.np, \"Desig\")],1), ((2,0),[(np, \"Desig\")],2)],[((1,1),[(s\\.np, \">\")],2)],[((0,2),[(s, \"<\")],1)]] is"
    let c11 = getCateFromString "np"
    let c12 = getCateFromString "(s\\.np)/.np"
    let c13 = getCateFromString "np"
    let c21 = getCateFromString "s\\.np"
    let c31 = getCateFromString "s"
    let pc11 = createPhraCate 0 0 c11 "Desig" 0
    let pc12 = createPhraCate 1 0 c12 "Desig" 1
    let pc13 = createPhraCate 2 0 c13 "Desig" 2
    let pc21 = createPhraCate 1 1 c21 ">" 2
    let pc31 = createPhraCate 0 2 c31 "<" 1
    showNCateLine 0 [pc21] [[pc11,pc12,pc13],[pc21],[pc31]]
    putStrLn "[Here line feed is added]"
    putStrLn ""

    putStrLn "The result of showNCateSymb 0 [((1,1),[(s\\.np, \">\")],2)] [[((0,0),[(np, \"Desig\")],0),((1,0),[((s\\.np)/.np, \"Desig\")],1), ((2,0),[(np, \"Desig\")],2)],[((1,1),[(s\\.np, \">\")],2)],[((0,2),[(s, \"<\")],1)]] is"
    let c11 = getCateFromString "np"
    let c12 = getCateFromString "(s\\.np)/.np"
    let c13 = getCateFromString "np"
    let c21 = getCateFromString "s\\.np"
    let c31 = getCateFromString "s"
    let pc11 = createPhraCate 0 0 c11 "Desig" 0
    let pc12 = createPhraCate 1 0 c12 "Desig" 1
    let pc13 = createPhraCate 2 0 c13 "Desig" 2
    let pc21 = createPhraCate 1 1 c21 ">" 2
    let pc31 = createPhraCate 0 2 c31 "<" 1
    showNCateSymb 0 [pc21] [[pc11,pc12,pc13],[pc21],[pc31]]
    putStrLn "[Here line feed is added]"
    putStrLn ""
    
    putStrLn "The result of showNCateLine 0 [((0,2),[(s, \"<\")],1)] [[((0,0),[(np, \"Desig\")],0),((1,0),[((s\\.np)/.np, \"Desig\")],1), ((2,0),[(np, \"Desig\")],2)],[((1,1),[(s\\.np, \">\")],2)],[((0,2),[(s, \"<\")],1)]] is"
    let c11 = getCateFromString "np"
    let c12 = getCateFromString "(s\\.np)/.np"
    let c13 = getCateFromString "np"
    let c21 = getCateFromString "s\\.np"
    let c31 = getCateFromString "s"
    let pc11 = createPhraCate 0 0 c11 "Desig" 0
    let pc12 = createPhraCate 1 0 c12 "Desig" 1
    let pc13 = createPhraCate 2 0 c13 "Desig" 2
    let pc21 = createPhraCate 1 1 c21 ">" 2
    let pc31 = createPhraCate 0 2 c31 "<" 1
    showNCateLine 0 [pc31] [[pc11,pc12,pc13],[pc21],[pc31]]
    putStrLn "[Here line feed is added]"
    putStrLn ""

    putStrLn "The result of showNCateSymb 0 [((0,2),[(s, \"<\")],1)] [[((0,0),[(np, \"Desig\")],0),((1,0),[((s\\.np)/.np, \"Desig\")],1), ((2,0),[(np, \"Desig\")],2)],[((1,1),[(s\\.np, \">\")],2)],[((0,2),[(s, \"<\")],1)]] is"
    let c11 = getCateFromString "np"
    let c12 = getCateFromString "(s\\.np)/.np"
    let c13 = getCateFromString "np"
    let c21 = getCateFromString "s\\.np"
    let c31 = getCateFromString "s"
    let pc11 = createPhraCate 0 0 c11 "Desig" 0
    let pc12 = createPhraCate 1 0 c12 "Desig" 1
    let pc13 = createPhraCate 2 0 c13 "Desig" 2
    let pc21 = createPhraCate 1 1 c21 ">" 2
    let pc31 = createPhraCate 0 2 c31 "<" 1
    showNCateSymb 0 [pc31] [[pc11,pc12,pc13],[pc21],[pc31]]
    putStrLn "[Here line feed is added]"
    putStrLn ""

    putStrLn "The result of showTreeStru [[((0,0),[(np, \"Desig\")],0),((1,0),[((s\\.np)/.np, \"Desig\")],1), ((2,0),[(np, \"Desig\")],2)],[((1,1),[(s\\.np, \">\")],2)],[((0,2),[(s, \"<\")],1)]] [[((0,0),[(np, \"Desig\")],0),((1,0),[((s\\.np)/.np, \"Desig\")],1), ((2,0),[(np, \"Desig\")],2)],[((1,1),[(s\\.np, \">\")],2)],[((0,2),[(s, \"<\")],1)]] is"
    let c11 = getCateFromString "np"
    let c12 = getCateFromString "(s\\.np)/.np"
    let c13 = getCateFromString "np"
    let c21 = getCateFromString "s\\.np"
    let c31 = getCateFromString "s"
    let pc11 = createPhraCate 0 0 c11 "Desig" 0
    let pc12 = createPhraCate 1 0 c12 "Desig" 1
    let pc13 = createPhraCate 2 0 c13 "Desig" 2
    let pc21 = createPhraCate 1 1 c21 ">" 2
    let pc31 = createPhraCate 0 2 c31 "<" 1
    showTreeStru [[pc11,pc12,pc13],[pc21],[pc31]] [[pc11,pc12,pc13],[pc21],[pc31]]
    putStrLn ""

    putStrLn "The result of showTreeStru [[((0,0),[(np, \"Desig\")],0),((1,0),[((s\\.np)/.np, \"Desig\")],1), ((2,0),[(np, \"Desig\")],2)],[((0,1),[(s/.np, \">T->B\")],1)],[((0,2),[(s, \">\")],2)]] [[((0,0),[(np, \"Desig\")],0),((1,0),[((s\\.np)/.np, \"Desig\")],1), ((2,0),[(np, \"Desig\")],2)],[((0,1),[(s/.np, \">T->B\")],1)],[((0,2),[(s, \">\")],2)]] is"
    let c11 = getCateFromString "np"
    let c12 = getCateFromString "(s\\.np)/.np"
    let c13 = getCateFromString "np"
    let c21 = getCateFromString "s/.np"
    let c31 = getCateFromString "s"
    let pc11 = createPhraCate 0 0 c11 "Desig" 0
    let pc12 = createPhraCate 1 0 c12 "Desig" 1
    let pc13 = createPhraCate 2 0 c13 "Desig" 2
    let pc21 = createPhraCate 0 1 c21 ">T->B" 1
    let pc31 = createPhraCate 0 2 c31 ">" 2
    showTreeStru [[pc11,pc12,pc13],[pc21],[pc31]] [[pc11,pc12,pc13],[pc21],[pc31]]
    putStrLn ""

    putStrLn "The result of showForestWithTreeStru [[((0,0),[(np, \"Desig\")],0),((1,0),[((s\\.np)/.np, \"Desig\")],1), ((2,0),[(np, \"Desig\")],2), ((0,1),[(s/.np, \">T->B\")],1), ((0,2),[(s, \">\")],2)], [((0,0),[(np, \"Desig\")],0),((1,0),[((s\\.np)/.np, \"Desig\")],1), ((2,0),[(np, \"Desig\")],2)],[((1,1),[(s\\.np, \">\")],2)],[((0,2),[(s, \"<\")],1)]] ] is"
    let c11 = getCateFromString "np"
    let c12 = getCateFromString "(s\\.np)/.np"
    let c13 = getCateFromString "np"
    let c21 = getCateFromString "s/.np"
    let c22 = getCateFromString "s\\.np"
    let c31 = getCateFromString "s"
    let c32 = getCateFromString "s"
    let pc11 = createPhraCate 0 0 c11 "Desig" 0
    let pc12 = createPhraCate 1 0 c12 "Desig" 1
    let pc13 = createPhraCate 2 0 c13 "Desig" 2
    let pc21 = createPhraCate 0 1 c21 ">T->B" 1
    let pc22 = createPhraCate 1 1 c22 ">" 2
    let pc31 = createPhraCate 0 2 c31 ">" 1
    let pc32 = createPhraCate 0 2 c32 "<" 2
    showForestWithTreeStru [[pc11,pc12,pc13,pc21,pc32], [pc11,pc12,pc13,pc22,pc31]]
    putStrLn ""

    putStrLn "The result of showCateStartPos [[((0,0),[(np, \"Desig\")],0),((1,0),[((s\\.np)/.np, \"Desig\")],1), ((2,0),[(np, \"Desig\")],2)],[((0,1),[(s/.np, \">T->B\")],1)],[((0,2),[(s, \">\")],2)]] [[((0,0),[(np, \"Desig\")],0),((1,0),[((s\\.np)/.np, \"Desig\")],1), ((2,0),[(np, \"Desig\")],2)],[((0,1),[(s/.np, \">T->B\")],1)],[((0,2),[(s, \">\")],2)]] is"
    let c11 = getCateFromString "np"
    let c12 = getCateFromString "(s\\.np)/.np"
    let c13 = getCateFromString "np"
    let c21 = getCateFromString "s/.np"
    let c31 = getCateFromString "s"
    let pc11 = createPhraCate 0 0 c11 "Desig" 0
    let pc12 = createPhraCate 1 0 c12 "Desig" 1
    let pc13 = createPhraCate 2 0 c13 "Desig" 2
    let pc21 = createPhraCate 0 1 c21 ">T->B" 1
    let pc31 = createPhraCate 0 2 c31 ">" 2
    showCateStartPos [[pc11,pc12,pc13],[pc21],[pc31]] [[pc11,pc12,pc13],[pc21],[pc31]]
    putStrLn ""

    putStrLn "The result of showForestCateStartPos [[((0,0),[(np, \"Desig\")],0),((1,0),[((s\\.np)/.np, \"Desig\")],1), ((2,0),[(np, \"Desig\")],2), ((0,1),[(s/.np, \">T->B\")],1), ((0,2),[(s, \">\")],2)], [((0,0),[(np, \"Desig\")],0),((1,0),[((s\\.np)/.np, \"Desig\")],1), ((2,0),[(np, \"Desig\")],2)],[((1,1),[(s\\.np, \">\")],2)],[((0,2),[(s, \"<\")],1)]] ] is"
    let c11 = getCateFromString "np"
    let c12 = getCateFromString "(s\\.np)/.np"
    let c13 = getCateFromString "np"
    let c21 = getCateFromString "s/.np"
    let c22 = getCateFromString "s\\.np"
    let c31 = getCateFromString "s"
    let c32 = getCateFromString "s"
    let pc11 = createPhraCate 0 0 c11 "Desig" 0
    let pc12 = createPhraCate 1 0 c12 "Desig" 1
    let pc13 = createPhraCate 2 0 c13 "Desig" 2
    let pc21 = createPhraCate 0 1 c21 ">T->B" 1
    let pc22 = createPhraCate 1 1 c22 ">" 2
    let pc31 = createPhraCate 0 2 c31 ">" 1
    let pc32 = createPhraCate 0 2 c32 "<" 2
    showForestCateStartPos [[pc11,pc12,pc13,pc21,pc32], [pc11,pc12,pc13,pc22,pc31]]
    putStrLn ""
