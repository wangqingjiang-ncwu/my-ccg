-- Copyright China University of Water Resources and Electric Power (c) 2019
-- All rights reserved.

module Category (
    Category,      -- 范畴类型
    Slash,         -- String
    slashes,       -- [Slash]
    Prim,          -- String
    primitives,    -- [Prim]
    nilCate,       -- Category
    sCate,         -- Category
    npCate,        -- Category
    isNil,         -- Category -> Bool
    isPrimitive,   -- Category -> Bool
    isDerivative,  -- Category -> Bool
    veriStrForCate,      -- String -> Bool
    getCateFromString,   --String -> Category
    indexOfSlash,        -- Int -> Int -> String -> Int
    leftStr,       -- String -> String
    rightStr,      -- String -> String
    midSlashStr,   -- String -> Slash
    leftCate,      -- Category -> Category
    rightCate,     -- Category -> Category
    midSlash,      -- Category -> a
    derivate       -- Category -> Slash -> Category -> Category
    ) where

type Slash = String
slashes :: [Slash]
slashes = ["/.","\\.","/#","\\#","/x","\\x","/*","\\*"]

type Prim = String
primitives :: [Prim]
primitives = ["s", "np"]

-- Define data type Category which is enumerable.
data Category = Nil | Primitive Prim | Derivative Category Slash Category deriving (Eq)

-- Define relation Ord between two categories such that two phrasal cagories also can be compared.

instance Ord Category where 
    Nil < Nil = False 
    Primitive a < Primitive b = (a<b)
    Derivative a _ b < Derivative c _ d = (a < c)||((a==c)&&(b<d))
    Nil < Primitive _ = True
    Primitive _ < Nil = False
    Primitive _ < Derivative _ _ _ = True
    Derivative _ _ _ < Primitive _ = False
    Nil < Derivative _ _ _ = True
    Derivative _ _ _ < Nil = False
    Nil <= Primitive _ = False
    Nil <= Derivative _ _ _ = False
    Primitive _ <= Derivative _ _ _ = False

instance Show Category where
    show Nil = "Nil"
    show (Primitive p) = p
    show (Derivative c1 s c2)
        | isPrimitive c1 && isPrimitive c2 = (show c1)++s++(show c2) 
        | isDerivative c1 && isPrimitive c2 = "("++(show c1)++")"++s++(show c2) 
        | isPrimitive c1 && isDerivative c2 = (show c1)++s++"("++(show c2)++")" 
        | otherwise = "("++(show c1)++")"++s++"("++(show c2)++")"


-- Besides interior functions, data constructors are not seen from outside of modules. To have access to these constructors, related functions are defined.
nilCate :: Category
nilCate = Nil

sCate :: Category
sCate = getCateFromString "s"

npCate :: Category
npCate = getCateFromString "np"

isNil :: Category -> Bool
isNil Nil = True
isNil _ = False

isPrimitive :: Category -> Bool
isPrimitive (Primitive _) = True
isPrimitive _ = False

isDerivative :: Category -> Bool
isDerivative (Derivative _ _ _) = True
isDerivative _ = False

veriStrForCate :: String -> Bool
veriStrForCate str
    | indexOfSlash 0 0 str == -1 = elem str ["Nil","s","np"] 
    | otherwise = veriStrForCate (leftStr str) && elem (midSlashStr str) slashes && veriStrForCate (rightStr str)

getCateFromString :: String -> Category
getCateFromString str
    | veriStrForCate str /= True = error "getCateFromString"
    | index == -1 && str == "Nil" = Nil
    | index == -1 && str == "s" = Primitive "s"
    | index == -1 && str == "np" = Primitive "np"
    | otherwise = derivate (getCateFromString (leftStr str)) (midSlashStr str) (getCateFromString (rightStr str))
        where 
        index = indexOfSlash 0 0 str 

-- Get the index of middle slash, which will be -1 for category "s" and "np".
-- To remember how many left brackets have been met, the integer nlb is needed. 
-- The index is initialized as 0.
indexOfSlash :: Int -> Int -> String -> Int
indexOfSlash nlb i str
    | i == length str = -1
    | x == '(' = indexOfSlash (nlb + 1) (i+1) str
    | x == ')' = indexOfSlash (nlb - 1) (i+1) str
    | (x == '/' || x == '\\') && nlb == 0 = i
    | otherwise = indexOfSlash nlb (i+1) str
        where
        x = str!!i 

leftStr :: String -> String
leftStr str
    | index == -1 = error "leftStr"
    | head(lStr)=='(' = tail (init lStr)  
    | otherwise = lStr
        where
        index = indexOfSlash 0 0 str
        lStr = take index str

rightStr :: String -> String
rightStr str
    | index == -1 = error "rightStr"
    | head(rStr)=='(' = tail (init rStr)
    | otherwise = rStr
        where
        index = indexOfSlash 0 0 str
        rStr = drop (index + 2) str

midSlashStr :: String -> Slash
midSlashStr str
    | index == 0 = error "midSlashStr"
    | otherwise = [str!!index, str!!(index + 1)]
        where
        index = indexOfSlash 0 0 str

leftCate :: Category -> Category
leftCate (Primitive a) = error "leftCate"
leftCate (Derivative cate1 _ _) = cate1

rightCate :: Category -> Category
rightCate (Primitive a) = error "rightCate"
rightCate (Derivative _ _ cate2) = cate2

midSlash :: Category -> Slash
midSlash (Primitive _) = error "midSlash"
midSlash (Derivative _ s _) = s

derivate :: Category -> Slash -> Category -> Category
derivate  cate1 slash cate2 = Derivative cate1 slash cate2

