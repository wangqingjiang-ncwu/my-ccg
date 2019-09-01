-- Copyright China University of Water Resources and Electric Power (c) 2019
-- All rights reserved.

module Rule (
    rules,         -- [Category -> Category -> Category]
    appF,          -- Category -> Category -> Category
    appB,          -- Category -> Category -> Category
    comFh,         -- Category -> Category -> Category
    comFh2,        -- Category -> Category -> Category
    comBh,         -- Category -> Category -> Category
    comFc,         -- Category -> Category -> Category
    comBc,         -- Category -> Category -> Category
    raiFh,         -- Category -> Category -> Category
    raiFc,         -- Category -> Category -> Category
    raiBh,         -- Category -> Category -> Category
    raiBc,         -- Category -> Category -> Category
    ) where

import Category 

-- CCG rules constitute a functional list.
rules :: [Category -> Category -> Category]
rules = [appF, appB, comFh, comFh2, comBh, comFc, comBc, raiFh, raiBh, raiFc, raiBc] 

-- CCG forward application
appF :: Category -> Category -> Category
appF cate1 cate2
    | isPrimitive cate1 = nilCate
    | head (midSlash cate1) == '/' && rightCate cate1 == cate2 = leftCate cate1
    | otherwise = nilCate

-- CCG backward application
appB :: Category -> Category -> Category
appB cate1 cate2
    | isPrimitive cate2 = nilCate
    | head (midSlash cate2) == '\\' && rightCate cate2 == cate1 = leftCate cate2
    | otherwise = nilCate


-- CCG forward harmonic composition
comFh :: Category -> Category -> Category
comFh cate1 cate2
    | isPrimitive cate1 || isPrimitive cate2 = nilCate
    | (midSlash cate1 == "/#" || midSlash cate1 == "/.") && (midSlash cate2 == "/#" || midSlash cate2 == "/.") && rightCate cate1 == leftCate cate2 = derivate (leftCate cate1) (midSlash cate2) (rightCate cate2)
    | otherwise = nilCate

-- CCG forward harmonic composition^2
comFh2 :: Category -> Category -> Category
comFh2 cate1 cate2
    | isPrimitive cate1 || isPrimitive cate2 = nilCate
    | isPrimitive (leftCate cate2) = nilCate
    | (midSlash cate1 == "/#" || midSlash cate1 == "/.") && (midSlash lCate2 == "/#" || midSlash lCate2 == "/.") && rightCate cate1 == leftCate lCate2 = derivate (derivate (leftCate cate1) (midSlash lCate2) (rightCate lCate2)) (midSlash cate2) (rightCate cate2)
    | otherwise = nilCate
        where
        lCate2 = leftCate cate2

-- CCG backward harmonic composition
comBh :: Category -> Category -> Category
comBh cate1 cate2
    | isPrimitive cate1 || isPrimitive cate2 = nilCate
    | (midSlash cate1 == "\\#" || midSlash cate1 == "\\.") && (midSlash cate2 == "\\#" || midSlash cate2 == "\\.") && rightCate cate2 == leftCate cate1 = derivate (leftCate cate2) (midSlash cate1) (rightCate cate1)
    | otherwise = nilCate

-- CCG forward crossing composition
comFc :: Category -> Category -> Category
comFc cate1 cate2
    | isPrimitive cate1 || isPrimitive cate2 = nilCate
    | (midSlash cate1 == "/x" || midSlash cate1 == "/.") && (midSlash cate2 == "\\x" || midSlash cate2 == "\\.") && rightCate cate1 == leftCate cate2 = derivate (leftCate cate1) (midSlash cate2) (rightCate cate2)
    | otherwise = nilCate

-- CCG backward crossing composition
comBc :: Category -> Category -> Category
comBc cate1 cate2
    | isPrimitive cate1 || isPrimitive cate2 = nilCate
    | (midSlash cate1 == "/x" || midSlash cate1 == "/.") && (midSlash cate2 == "\\x" || midSlash cate2 == "\\.") && leftCate cate1 == rightCate cate2 = derivate (leftCate cate2) (midSlash cate1) (rightCate cate1)
    | otherwise = nilCate

-- CCG Forward type raising and harmonic composition: X (Y\X)/Z -> Y/(Y\X) (Y\X)/Z -> Y/Z
raiFh :: Category -> Category -> Category
raiFh cate1 cate2
    | isPrimitive cate2 || isPrimitive lcate2 = nilCate
    | head (midSlash lcate2) /= '\\' || (midSlash cate2 /= "/#" && midSlash cate2 /= "/.") = nilCate
    | cate1 == rightCate lcate2 = derivate (leftCate lcate2) (midSlash cate2) (rightCate cate2)
    | otherwise = nilCate 
        where
        lcate2 = leftCate cate2

-- Forward type raising and crossing composition: X (Y\X)\Z -> Y/(Y\X) (Y\X)\Z -> Y\Z
raiFc :: Category -> Category -> Category
raiFc cate1 cate2
    | isPrimitive cate2 || isPrimitive lcate2 = nilCate
    | head (midSlash lcate2) /= '\\' || (midSlash cate2 /= "\\x" && midSlash cate2 /= "\\.") = nilCate
    | cate1 == rightCate lcate2 = derivate (leftCate lcate2) (midSlash cate2) (rightCate cate2)
    | otherwise = nilCate 
        where
        lcate2 = leftCate cate2

-- Backward type raising and harmonic composition: (Y/X)\Z X -> (Y/X)\Z Y\(Y/X)-> Y\Z
raiBh :: Category -> Category -> Category
raiBh cate1 cate2
    | isPrimitive cate1 || isPrimitive lcate1 = nilCate
    | head (midSlash lcate1) /= '/' || (midSlash cate1 /= "\\#" && midSlash cate1 /= "\\.") = nilCate
    | rightCate lcate1 == cate2 = derivate (leftCate lcate1) (midSlash cate1) (rightCate cate1)
    | otherwise = nilCate 
        where
        lcate1 = leftCate cate1

-- Backward type raising and crossing composition: (Y/X)/Z X -> (Y/X)/Z Y\(Y/X)-> Y/Z
raiBc :: Category -> Category -> Category
raiBc cate1 cate2
    | isPrimitive cate1 || isPrimitive lcate1 = nilCate
    | head (midSlash lcate1) /= '/' || (midSlash cate1 /= "/x" && midSlash cate1 /= "/.") = nilCate
    | rightCate lcate1 == cate2 = derivate (leftCate lcate1) (midSlash cate1) (rightCate cate1)
    | otherwise = nilCate 
        where
        lcate1 = leftCate cate1

