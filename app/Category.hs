-- Copyright (c) 2019-2021 China University of Water Resources and Electric Power,
-- All rights reserved.

module Category (
    Category(..),  -- Category and its all Constructors
    Slash,         -- String
    slashes,       -- [Slash]
    Prim,          -- String
    primitives,    -- [Prim]
    cateEqual,     -- Category -> Category -> Bool
    nilCate,       -- Category
    xCate,         -- Category
    sCate,         -- Category
    npCate,        -- Category
    isNil,         -- Category -> Bool
    isX,           -- Category -> Bool
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
    derivate,      -- Category -> Slash -> Category -> Category
    ndCate,        -- Category, "np\*np"
    adjCate,       -- Category, "np/.np"
    predCate,      -- Category, "s\.np"
    verbCate,      -- Category, "(s\.np)/.np"
    verbCate2,     -- Category, "((s\.np)/.np)/.np"
    vCate,         -- [predCate, verbCate, verbCate2]
    adverbalCate,  -- Category, "(s\.np)/#(s\.np)"
    prep2AdvCate,  -- Category, "((s\.np)/#(s\.np))/*np"
    prep2CompCate, -- Category, "((s\.np)\x(s\.np))/*np"
    verbCompCate,         -- Category, "(s\.np)\x(s\.np)"
    nounCompCate,         -- Category, "np\*np"
    adjCompCate,          -- Category, "(np/.np)\*(np/.np)"
    numeralCate,          -- Category, "np/*np"
    quantifierCate,       -- Category, "(np/*np)\*(np/*np)"
    objectExtractionCate, -- Category, "s/.np"
    aux1Cate,             -- Category, "(np/*np)\*np"
    aux2Cate,             -- Category, "((s\.np)/#(s\.np))\*(np/.np)"
    aux3Cate,             -- Category, "((s\.np)\x(s\.np))/*(np/.np)"
    aux4Cate,             -- Category, "(s\.np)\x(s\.np)"
    aux5Cate,             -- Category, "X\*X"
    aux6Cate,             -- Caregory, "np/*((s\.np)/.np)"
    conjCate1,            -- Category, "(X\*X)/*X"
    conjCate2,            -- Category, "X\*X"
    conjCate3,            -- Category, "X/*X"
    prefixCate,           -- Category, "np/*np"
    postfixCate           -- Category, "np\*np"
    ) where

type Slash = String
slashes :: [Slash]
slashes = ["/.","\\.","/#","\\#","/x","\\x","/*","\\*"]

type Prim = String
primitives :: [Prim]
primitives = ["s", "np"]

-- Define data type Category which is enumerable.
data Category = Nil | X | Primitive Prim | Derivative Category Slash Category deriving (Eq)

-- Define relation Ord between two categories such that two phrasal cagories also can be compared.
instance Ord Category where
    Nil < Nil = False
    X < X = False
    Primitive a < Primitive b = (a<b)
    Derivative a s1 b < Derivative c s2 d = (a < c)||((a==c)&&(b<d))||((a==c)&&(b==d)&&(s1<s2))
    Nil < X = True
    Nil < Primitive _ = True
    Nil < Derivative _ _ _ = True
    X < Primitive _ = True
    X < Derivative _ _ _ = True
    Primitive _ < Derivative _ _ _ = True
    X < Nil = False
    Primitive _ < Nil = False
    Derivative _ _ _ < Nil = False
    Primitive _ < X = False
    Derivative _ _ _ < X = False
    Derivative _ _ _ < Primitive _ = False
    Nil <= X = False
    Nil <= Primitive _ = False
    Nil <= Derivative _ _ _ = False
    X <= Primitive _ = False
    X <= Derivative _ _ _ = False
    Primitive _ <= Derivative _ _ _ = False

-- Define how a category shows as a letter string.
instance Show Category where
    show Nil = "Nil"
    show X = "X"
    show (Primitive p) = p
    show (Derivative c1 s c2)
        | (isPrimitive c1 || isX c1) && (isPrimitive c2 || isX c2) = (show c1)++s++(show c2)
        | isDerivative c1 && (isPrimitive c2 || isX c2) = "("++(show c1)++")"++s++(show c2)
        | (isPrimitive c1 || isX c1) && isDerivative c2 = (show c1)++s++"("++(show c2)++")"
        | otherwise = "("++(show c1)++")"++s++"("++(show c2)++")"

-- Define the nonstrict equality between two categories, namely not considering slash types.
cateEqual :: Category -> Category -> Bool
cateEqual cate1 cate2
    | isX cate1 && isX cate2 = True
    | isX cate1 && not (isX cate2) = False
    | not (isX cate1) && isX cate2 = False
    | isPrimitive cate1 && isPrimitive cate2 = cate1 == cate2
    | isPrimitive cate1 && not (isPrimitive cate2) = False
    | not (isPrimitive cate1) && isPrimitive cate2 = False
    | otherwise = ((midSlash cate1)!!0 == (midSlash cate2)!!0) && (cateEqual (leftCate cate1) (leftCate cate2)) && (cateEqual (rightCate cate1) (rightCate cate2))

-- Besides interior functions, data constructors are not seen from outside of modules. To have access to these constructors, related functions are defined.
nilCate :: Category
nilCate = Nil

xCate :: Category
xCate = X

sCate :: Category
sCate = getCateFromString "s"

npCate :: Category
npCate = getCateFromString "np"

isNil :: Category -> Bool
isNil Nil = True
isNil _ = False

isX :: Category -> Bool
isX X = True
isX _ = False

isPrimitive :: Category -> Bool
isPrimitive (Primitive _) = True
isPrimitive _ = False

isDerivative :: Category -> Bool
isDerivative (Derivative _ _ _) = True
isDerivative _ = False

veriStrForCate :: String -> Bool
veriStrForCate str
    | indexOfSlash 0 0 str == -1 = elem str ["Nil","s","np","X"]
    | otherwise = veriStrForCate (leftStr str) && elem (midSlashStr str) slashes && veriStrForCate (rightStr str)

getCateFromString :: String -> Category
getCateFromString str
    | veriStrForCate str /= True = error $ "getCateFromString: " ++ str
    | index == -1 && str == "Nil" = Nil
    | index == -1 && str == "X" = X
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
    | (x == '/' || x == '\\') && nlb == 0 && indexOfSlash nlb (i+1) str == -1 = i
    | (x == '/' || x == '\\') && nlb == 0 && indexOfSlash nlb (i+1) str /= -1 = error $ "indexOfSlash: Category symbol \"" ++ str ++ "\" does not conform two-division style."
    | otherwise = indexOfSlash nlb (i+1) str
        where
        x = str!!i

leftStr :: String -> String
leftStr str
    | index == -1 = error "leftStr"
    | head lStr == '(' && last lStr /= ')' = error "leftStr lost ')'"
    | head lStr /= '(' && last lStr == ')' = error "leftStr lost '('"
    | head lStr == '(' && last lStr == ')' = tail (init lStr)
    | otherwise = lStr
        where
        index = indexOfSlash 0 0 str
        lStr = take index str

rightStr :: String -> String
rightStr str
    | index == -1 = error "rightStr"
    | head rStr == '(' && last rStr /= ')' = error "rightStr lost ')'"
    | head rStr /= '(' && last rStr == ')' = error "rightStr lost '('"
    | head rStr == '(' && last rStr == ')' = tail (init rStr)
    | otherwise = rStr
        where
        index = indexOfSlash 0 0 str
        rStr = drop (index + 2) str

midSlashStr :: String -> Slash
midSlashStr str
    | index == 0 = error "midSlashStr: No slash."
    | otherwise = [str!!index, str!!(index + 1)]
        where
        index = indexOfSlash 0 0 str

leftCate :: Category -> Category
leftCate Nil = error "leftCate: Nil"
leftCate X = error "leftCate: X"
leftCate (Primitive a) = error "leftCate"
leftCate (Derivative cate1 _ _) = cate1

rightCate :: Category -> Category
rightCate Nil = error "rightCate: Nil"
rightCate X = error "rightCate: X"
rightCate (Primitive a) = error $ "rightCate: " ++ show (Primitive a)
rightCate (Derivative _ _ cate2) = cate2

midSlash :: Category -> Slash
midSlash (Primitive _) = error "midSlash"
midSlash (Derivative _ s _) = s

derivate :: Category -> Slash -> Category -> Category
derivate  cate1 slash cate2 = Derivative cate1 slash cate2

ndCate :: Category
ndCate = getCateFromString "np\\*np"

adjCate :: Category
adjCate = getCateFromString "np/.np"

predCate :: Category
predCate = getCateFromString "s\\.np"

verbCate :: Category
verbCate = getCateFromString "(s\\.np)/.np"

verbCate2 :: Category
verbCate2 = getCateFromString "((s\\.np)/.np)/.np"

vCate :: [Category]
vCate = [predCate, verbCate, verbCate2]

adverbalCate :: Category
adverbalCate = getCateFromString "(s\\.np)/#(s\\.np)"

prep2AdvCate :: Category
prep2AdvCate = getCateFromString "((s\\.np)/#(s\\.np))/*np"

prep2CompCate :: Category
prep2CompCate = getCateFromString "((s\\.np)\\x(s\\.np))/*np"

verbCompCate :: Category
verbCompCate = getCateFromString "(s\\.np)\\x(s\\.np)"

nounCompCate :: Category
nounCompCate = getCateFromString "np\\*np"

adjCompCate :: Category
adjCompCate = getCateFromString "(np/.np)\\*(np/.np)"

numeralCate :: Category
numeralCate = getCateFromString "np/*np"

quantifierCate :: Category
quantifierCate = getCateFromString "(np/*np)\\*(np/*np)"

objectExtractionCate :: Category
objectExtractionCate = getCateFromString "s/.np"

-- Auxiliary word #1 is '的'
aux1Cate :: Category
aux1Cate = getCateFromString "(np/*np)\\*np"

-- Auxiliary word #2 is '地'
aux2Cate :: Category
aux2Cate = getCateFromString "((s\\.np)/#(s\\.np))\\*(np/.np)"

-- Auxiliary word #3 is '得'
aux3Cate :: Category
aux3Cate = getCateFromString "((s\\.np)\\x(s\\.np))/*(np/.np)"

-- Auxiliary word #4 is '着', '了', or '过'
aux4Cate :: Category
aux4Cate = getCateFromString "(s\\.np)\\x(s\\.np)"

-- Auxiliary word #5 is '等', '似的', '一样', and so on.
aux5Cate :: Category
aux5Cate = getCateFromString "X\\*X"

-- Auxiliary word #6 is '所', followed by transitive verb.
aux6Cate :: Category
aux6Cate = getCateFromString "np/*((s\\.np)/.np)"

-- Conjunction 1
conjCate1 :: Category
conjCate1 = getCateFromString "(X\\*X)/*X"

-- Conjunction 2
conjCate2 :: Category
conjCate2 = getCateFromString "X\\*X"

-- Inter-clause conjunction
conjCate3 :: Category
conjCate3 = getCateFromString "X/*X"

-- Prefix words are '第'、'阿'、'初'
prefixCate :: Category
prefixCate = getCateFromString "np/*np"

-- Postfix words are '者'、'们'、'性'、'儿'
postfixCate :: Category
postfixCate = getCateFromString "np\\*np"
