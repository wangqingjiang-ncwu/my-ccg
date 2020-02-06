-- Copyright (c) 2019-2020 China University of Water Resources and Electric Power,
-- All rights reserved.

module Test (
    pc01,pc02,pc03,pc04,pc05,pc06,pc07,clo1,clo2

    ) where

import Category
import Rule
import Parse
import Output

pc01 = createPhraCate 0 0 (getCateFromString "np/.np") "Desig" "good'" True 0
pc02 = createPhraCate 1 0 (getCateFromString "(s\\.np)/.np") "Desig" "than'" True 1
pc03 = createPhraCate 2 0 (getCateFromString "np/.np") "Desig" "bad'" True 2
pc04 = createPhraCate 0 1 (getCateFromString "s/.np") "Np/a->T->B" "good' than'" True 1
pc05 = createPhraCate 1 1 (getCateFromString "(s\\.np)/.np") ">B" "than' bad'" True 2
pc06 = createPhraCate 1 1 (getCateFromString "s\\.np") "Np/a->" "than' bad'" True 2
pc07 = createPhraCate 0 2 sCate "Np/a-<" "(than' bad') good'" True 1

clo1 = [pc01,pc02,pc03,pc04,pc05,pc06]
clo2 = [deactOnePC pc01,deactOnePC pc02,deactOnePC pc03,deactOnePC pc06,pc07]
clo3 = [deactOnePC pc01,deactOnePC pc02,deactOnePC pc03,deactOnePC pc04,deactOnePC pc05,deactOnePC pc06, pc07]

