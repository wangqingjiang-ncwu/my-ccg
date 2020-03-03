-- Copyright (c) 2019-2020 China University of Water Resources and Electric Power,
-- All rights reserved.

module Test (
    pc0,pc1,pc2,pc3,pc4,pc5,pc6,clo1

    ) where

import Category
import Rule
import Parse
import Output
import Utils

pc0 = createPhraCate 0 0 (getCateFromString "((s\\.np)/#(s\\.np))/*np") "Desig" "在'" "DE" True 0
pc1 = createPhraCate 1 0 (getCateFromString "(s\\.np)/.np") "Desig" "开展'" "DE" True 1
pc2 = createPhraCate 2 0 npCate "Desig" "文明'" "DE" True 2
pc3 = createPhraCate 3 0 npCate "Desig" "班级'" "DE" True 3
pc4 = createPhraCate 4 0 (getCateFromString "s\\.np") "Desig" "评比'" "DE"  True 4
pc5 = createPhraCate 5 0 npCate "Desig" "活动'" "DE" True 5
pc6 = createPhraCate 6 0 (getCateFromString "np\\*np") "Desig" "中'" "DE" True 6

clo1 = [pc0,pc1,pc2,pc3,pc4,pc5,pc6]



