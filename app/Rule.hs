module Rule (
    rules,         -- [Category a -> Category a -> Category a]
    appF,          -- Category a -> Category a -> Category a
    appB,          -- Category a -> Category a -> Category a
    comFh,         -- Category a -> Category a -> Category a
    comFh2,        -- Category a -> Category a -> Category a
    comBh,         -- Category a -> Category a -> Category a
    comFc,         -- Category a -> Category a -> Category a
    comBc,         -- Category a -> Category a -> Category a
    raiFh,         -- Category a -> Category a -> Category a
    raiFc,         -- Category a -> Category a -> Category a
    raiBh,         -- Category a -> Category a -> Category a
    raiBc,         -- Category a -> Category a -> Category a
    ) where

import Category 

-- CCG rules constitute a functional list.
rules :: [Category a -> Category a -> Category a]
rules = [appF, appB, comFh, comFh2, comBh, comFc, comBc, raiFh, raiBh, raiFc, raiBc] 

-- CCG forward application
appF :: Category a -> Category a -> Category a
appF cate1 cate2
    | isPrimitive cate1 = nilCate
    | head (midSlash cate1) == '/' && rightCate cate1 == cate2 = leftCate cate1
    | otherwise = nilCate

-- CCG backward application
appB :: Category a -> Category a -> Category a
appB cate1 cate2
    | isPrimitive cate2 = nilCate
    | head (midSlash cate2) == '\\' && rightCate cate2 == cate1 = leftCate cate2
    | otherwise = nilCate


-- CCG forward harmonic composition
comFh :: Category a -> Category a -> Category a
comFh cate1 cate2
    | isPrimitive cate1 || isPrimitive cate2 = nilCate
    | (midSlash cate1 == "/#" || midSlash cate1 == "/.") && (midSlash cate2 == "/#" || midSlash cate2 == "/.") && rightCate cate1 == leftCate cate2 = derivate (leftCate cate1) (midSlash cate2) (rightCate cate2)
    | otherwise = nilCate

-- CCG forward harmonic composition^2
comFh2 :: Category a -> Category a -> Category a
comFh2 cate1 cate2
    | isPrimitive cate1 || isPrimitive cate2 = nilCate
    | isPrimitive (leftCate cate2) = nilCate
    | (midSlash cate1 == "/#" || midSlash cate1 == "/.") && (midSlash lCate2 == "/#" || midSlash lCate2 == "/.") && rightCate cate1 == leftCate lCate2 = derivate (derivate (leftCate cate1) (midSlash lCate2) (rightCate lCate2)) (midSlash cate2) (rightCate cate2)
    | otherwise = nilCate
        where
        lCate2 = leftCate cate2

-- CCG backward harmonic composition
comBh :: Category a -> Category a -> Category a
comBh cate1 cate2
    | isPrimitive cate1 || isPrimitive cate2 = nilCate
    | (midSlash cate1 == "\\#" || midSlash cate1 == "\\.") && (midSlash cate2 == "\\#" || midSlash cate2 == "\\.") && rightCate cate2 == leftCate cate1 = derivate (leftCate cate2) (midSlash cate1) (rightCate cate1)
    | otherwise = nilCate

-- CCG forward crossing composition
comFc :: Category a -> Category a -> Category a
comFc cate1 cate2
    | isPrimitive cate1 || isPrimitive cate2 = nilCate
    | (midSlash cate1 == "/x" || midSlash cate1 == "/.") && (midSlash cate2 == "\\x" || midSlash cate2 == "\\.") && rightCate cate1 == leftCate cate2 = derivate (leftCate cate1) (midSlash cate2) (rightCate cate2)
    | otherwise = nilCate

-- CCG backward crossing composition
comBc :: Category a -> Category a -> Category a
comBc cate1 cate2
    | isPrimitive cate1 || isPrimitive cate2 = nilCate
    | (midSlash cate1 == "/x" || midSlash cate1 == "/.") && (midSlash cate2 == "\\x" || midSlash cate2 == "\\.") && leftCate cate1 == rightCate cate2 = derivate (leftCate cate2) (midSlash cate1) (rightCate cate1)
    | otherwise = nilCate

-- CCG Forward type raising and harmonic composition: X (Y\X)/Z -> Y/(Y\X) (Y\X)/Z -> Y/Z
raiFh :: Category a -> Category a -> Category a
raiFh cate1 cate2
    | isPrimitive cate2 || isPrimitive lcate2 = nilCate
    | head (midSlash lcate2) /= '\\' || (midSlash cate2 /= "/#" && midSlash cate2 /= "/.") = nilCate
    | cate1 == rightCate lcate2 = derivate (leftCate lcate2) (midSlash cate2) (rightCate cate2)
    | otherwise = nilCate 
        where
        lcate2 = leftCate cate2

-- Forward type raising and crossing composition: X (Y\X)\Z -> Y/(Y\X) (Y\X)\Z -> Y\Z
raiFc :: Category a -> Category a -> Category a
raiFc cate1 cate2
    | isPrimitive cate2 || isPrimitive lcate2 = nilCate
    | head (midSlash lcate2) /= '\\' || (midSlash cate2 /= "\\x" && midSlash cate2 /= "\\.") = nilCate
    | cate1 == rightCate lcate2 = derivate (leftCate lcate2) (midSlash cate2) (rightCate cate2)
    | otherwise = nilCate 
        where
        lcate2 = leftCate cate2

-- Backward type raising and harmonic composition: (Y/X)\Z X -> (Y/X)\Z Y\(Y/X)-> Y\Z
raiBh :: Category a -> Category a -> Category a
raiBh cate1 cate2
    | isPrimitive cate1 || isPrimitive lcate1 = nilCate
    | head (midSlash lcate1) /= '/' || (midSlash cate1 /= "\\#" && midSlash cate1 /= "\\.") = nilCate
    | rightCate lcate1 == cate2 = derivate (leftCate lcate1) (midSlash cate1) (rightCate cate1)
    | otherwise = nilCate 
        where
        lcate1 = leftCate cate1

-- Backward type raising and crossing composition: (Y/X)/Z X -> (Y/X)/Z Y\(Y/X)-> Y/Z
raiBc :: Category a -> Category a -> Category a
raiBc cate1 cate2
    | isPrimitive cate1 || isPrimitive lcate1 = nilCate
    | head (midSlash lcate1) /= '/' || (midSlash cate1 /= "/x" && midSlash cate1 /= "/.") = nilCate
    | rightCate lcate1 == cate2 = derivate (leftCate lcate1) (midSlash cate1) (rightCate cate1)
    | otherwise = nilCate 
        where
        lcate1 = leftCate cate1

