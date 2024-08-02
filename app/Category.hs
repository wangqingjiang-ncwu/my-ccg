-- Copyright (c) 2019-2023 China University of Water Resources and Electric Power,
-- All rights reserved.

module Category (
    Category(..),  -- Category and its all Constructors
    Slash,         -- String
    slashes,       -- [Slash]
    Prim,          -- String
    primitives,    -- [Prim]
    cateEqual,     -- Category -> Category -> Bool
    cateMatch,     -- Category -> Category -> Bool
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
    pronCate,      -- Category, "np"
    pronCate4Numeral,     -- Category, "np/#np"
    ndCate,        -- Category, "np\*np"
    adjCate,       -- Category, "np/.np"
    predCate,      -- Category, "s\.np"
    verbCate,      -- Category, "(s\.np)/.np"
    verbCate2,     -- Category, "((s\.np)/.np)/.np"
    vCate,         -- [predCate, verbCate, verbCate2]
    vpCate,        -- [verbCate, verbCate2]
    advCate,       -- Category, "(s\.np)/#(s\.np)"
    advCate4Verb,  -- Category, "(s\.np)/#(s\.np)"
    advCate4Adj,   -- Category, "(np/.np)/*(np/.np)"
    advCate4Sent,  -- Category, "s/*s"
    advCate4DirecVerb, -- Category, "(s\.np)/x(s\.np)"
    advCate4OE,    -- Category, "(s/.np)/*(s/.np)"
    advCompCate,   -- ((s\.np)/#(s\.np))\*((s\.np)/#(s\.np))
    prep2AdvCate,  -- Category, "((s\.np)/#(s\.np))/*np"
    prep2CompCate, -- Category, "((s\.np)\x(s\.np))/*np"
    prep4BaCate,          -- Category, "((s/.np)\#np)/#((s\.np)/.np)"
    prep4BeiCate,         -- Category, "(s/#(s\.np))\#np"
    verbCompCate,         -- Category, "(s\.np)\x(s\.np)"
    nounCompCate,         -- Category, "np\*np"
    adjCompCate,          -- Category, "(np/.np)\*(np/.np)"
    numeralCate,          -- Category, "np/*np"
    quantifierCate,       -- Category, "(np/*np)\*(np/*np)"
    objectExtractionCate, -- Category, "s/.np"
    predicateExtractionCate,       -- Category, "s/#(s\.np)"
    aux1Cate,             -- Category, "(np/*np)\*np"
    aux2Cate,             -- Category, "((s\.np)/#(s\.np))\*(np/.np)"
    aux3Cate,             -- Category, "((s\.np)\x(s\.np))/*(np/.np)"
    aux3dCate,            -- Category, "((np/.np)\*(np/.np))/*((np/.np)/*(np/.np))"
    aux4Cate,             -- Category, "(s\.np)\x(s\.np)"
    aux5Cate,             -- Category, "X\#X"
    aux6Cate,             -- Category, "np/*((s\.np)/.np)"
    toneCate,             -- Category, "X\.X"
    conjCate,            -- Category, "(X\*X)/*X"
    conjCate4Backward,            -- Category, "X\*X"
    conjCate4Forward,            -- Category, "X/*X"
    prefixCate,           -- Category, "np/*np"
    postfixCate,          -- Category, "np\*X"
    baPhraseCate,         -- Category, "((s\#np)/#((s\.np)/.np)"
    poPhraseCate          -- Category, "(s\#np)/#(s\.np)"
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
-- Maybe the following instance is redundant, if deriving Category from type class Ord.
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
    X <= Nil = False
    X <= Primitive _ = False
    X <= Derivative _ _ _ = False
    Primitive _ <= Nil = False
    Primitive _ <= X = False
    Primitive a <= Primitive c = (a < c)||(a==c)
    Primitive _ <= Derivative _ _ _ = False
    Derivative _ _ _ <= Nil = False
    Derivative _ _ _ <= X = False
    Derivative _ _ _ <= Primitive _ = False
    Derivative a s1 b <= Derivative c s2 d = (a < c)||((a==c)&&(b<d))||((a==c)&&(b==d)&&(s1==s2))

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

{- Decide whether a category matches a pattern.
 - Such as, category s\.np matches pattern s\#np, while s\#np doesn't match s\.np.
 -}
cateMatch :: Category -> Category -> Bool
cateMatch cate patt
    | isX cate && isX patt = True
    | isX cate && not (isX patt) = False
    | not (isX cate) && isX patt = False
    | isPrimitive cate && isPrimitive patt = cate == patt
    | isPrimitive cate && not (isPrimitive patt) = False
    | not (isPrimitive cate) && isPrimitive patt = False
    | otherwise = (ms1!!0 == ms2!!0)
               && (ms1!!1 == '.' || (ms1!!1 == '#' && (ms2!!1 == '#' || ms2!!1 == '*'))
                                 || (ms1!!1 == 'x' && (ms2!!1 == 'x' || ms2!!1 == '*'))
                                 || (ms1!!1 == '*' && ms2!!1 == '*')
                  )
               && (cateMatch (leftCate cate) (leftCate patt)) && (cateMatch (rightCate cate) (rightCate patt))
    where
      ms1 = midSlash cate
      ms2 = midSlash patt

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
    | index == -1 = error "midSlashStr: No slash."
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

pronCate :: Category
pronCate = getCateFromString "np"

pronCate4Numeral :: Category
pronCate4Numeral = getCateFromString "np/#np"

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

vpCate :: [Category]
vpCate = [verbCate, verbCate2]

advCate :: Category
advCate = getCateFromString "(s\\.np)/#(s\\.np)"

advCate4Verb :: Category
advCate4Verb = getCateFromString "(s\\.np)/#(s\\.np)"

advCate4Adj :: Category
advCate4Adj = getCateFromString "(np/.np)/*(np/.np)"

advCate4Sent :: Category
advCate4Sent = getCateFromString "s/*s"

advCate4DirecVerb :: Category
advCate4DirecVerb = getCateFromString "(s\\.np)/x(s\\.np)"

advCate4OE :: Category
advCate4OE = getCateFromString "(s/.np)/*(s/.np)"

advCompCate :: Category
advCompCate = getCateFromString "((s\\.np)/#(s\\.np))\\*((s\\.np)/#(s\\.np))"

-- Category of preposition which combines one following object to construct a adverbial, such as "走v 到p 华水n"
prep2AdvCate :: Category
prep2AdvCate = getCateFromString "((s\\.np)/#(s\\.np))/*np"

-- Category of preposition which combines one following object to construct a complement, such as "走v 到p 华水n"
prep2CompCate :: Category
prep2CompCate = getCateFromString "((s\\.np)\\x(s\\.np))/*np"

-- '把'
prep4BaCate :: Category
prep4BaCate = getCateFromString "((s/.np)\\.np)/#((s\\.np)/.np)"

-- '被'
prep4BeiCate :: Category
prep4BeiCate = getCateFromString "(s/#(s/.np))\\.np"

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

predicateExtractionCate :: Category
predicateExtractionCate = getCateFromString "s/#(s\\.np)"

-- Auxiliary word #1 is '的'
aux1Cate :: Category
aux1Cate = getCateFromString "(np/*np)\\*np"

-- Auxiliary word #2 is '地'
aux2Cate :: Category
aux2Cate = getCateFromString "((s\\.np)/#(s\\.np))\\*(np/.np)"

-- Auxiliary word #3 is '得'
aux3Cate :: Category
aux3Cate = getCateFromString "((s\\.np)\\x(s\\.np))/*(np/.np)"

-- Auxiliary word #3d is also '得', here 'd' means an adjective adverb follows.
aux3dCate :: Category
aux3dCate = getCateFromString "((np/.np)\\*(np/.np))/*((np/.np)/*(np/.np))"

-- Auxiliary word #4 is '着', '了', or '过'
aux4Cate :: Category
aux4Cate = getCateFromString "(s\\.np)\\x(s\\.np)"

-- Auxiliary word #5 is '等', '似的', '一样', and so on.
aux5Cate :: Category
aux5Cate = getCateFromString "X\\#X"

-- Auxiliary word #6 is '所', followed by transitive verb.
aux6Cate :: Category
aux6Cate = getCateFromString "np/*((s\\.np)/.np)"

-- Tone word is '呢', '啊', '了', '的', and so on.
toneCate :: Category
toneCate = getCateFromString "X\\.X"

-- Conjunction the left and the right.
conjCate :: Category
conjCate = getCateFromString "(X\\*X)/*X"

-- Conjunction the left.
conjCate4Backward :: Category
conjCate4Backward = getCateFromString "X\\*X"

-- Conjunction the right.
conjCate4Forward :: Category
conjCate4Forward = getCateFromString "X/*X"

-- Prefix words are '第'、'阿'、'初'
prefixCate :: Category
prefixCate = getCateFromString "np/*np"

-- Postfix words are '者'、'们'、'性'、'儿'
postfixCate :: Category
postfixCate = getCateFromString "np\\*X"

-- '把' phrase
baPhraseCate :: Category
baPhraseCate = getCateFromString "(s\\.np)/#((s\\.np)/.np)"

-- 'PO' phrase
poPhraseCate :: Category
poPhraseCate = getCateFromString "(s\\.np)/#(s\\.np)"
