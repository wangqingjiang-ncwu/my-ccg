-- Copyright (c) 2019-2025 China University of Water Resources and Electric Power.
-- All rights reserved.

{- This module includes types and functions about Combinatory Logic, the original version of which was written by Liu Pan-pan and Wang Qing-jiang at 2023
 - in developping Combinatory Categorial Grammar, a CCG exploiting CL combinators as much as possible. To remedy semantic calclus in CCG-C2,
 - Wang Qing-jiang transplanted this module here at 2025.
 -}

module CL (
    Term(..),    -- Term and its all Constructors
    nullTerm,    -- Nil term
    isNullTerm,  -- Term -> Bool
    TermType,    -- Term type
    isPrimTerm,  -- Term -> Bool
    isCompoundTerm,   -- Term -> Bool
    isConstTerm,      -- Term -> Bool
    isVarTerm,        -- Term -> Bool
    isJuxTerm,        -- Term -> Bool
    fstTerm,     -- Term -> Maybe Term
    sndTerm,     -- Term -> Maybe Term
    subTermOfTerm,    -- Term -> [Term]
    fvOfTerm,    -- Term -> [Term]
    app,         -- Term -> Term -> Term
    CombAxiom,   -- Term -> Term
    combAxiom,   -- [CombAxiom]
    isSTerm,     -- Term -> Bool
    sAxiom,      -- Term -> Term
    isKTerm,     -- Term -> Bool
    kAxiom,      -- Term -> Term
    isITerm,     -- Term -> Bool
    iAxiom,      -- Term -> Term
    isBTerm,     -- Term -> Bool
    bAxiom,      -- Term -> Term
    isTTerm,     -- Term -> Bool
    tAxiom,      -- Term -> Term
    isCTerm,     -- Term -> Bool
    cAxiom,      -- Term -> Term
    isWTerm,     -- Term -> Bool
    wAxiom,      -- Term -> Term
    isMTerm,     -- Term -> Bool
    mAxiom,      -- Term -> Term
    isYTerm,     -- Term -> Bool
    yAxiom,      -- Term -> Term
    isJTerm,     -- Term -> Bool
    jAxiom,      -- Term -> Term
    isB'Term,    -- Term -> Bool
    b'Axiom,     -- Term -> Term
    isVTerm,     -- Term -> Bool
    vAxiom,      -- Term -> Term
    isS'Term,    -- Term -> Bool
    s'Axiom,     -- Term -> Term
    isRTerm,     -- Term -> Bool
    rAxiom,      -- Term -> Term
    isATerm,     -- Term -> Bool
    aAxiom,      -- Term -> Term
    isB3Term,    -- Term -> Bool
    b3Axiom,     -- Term -> Term
    isB3'Term,   -- Term -> Bool
    b3'Axiom,    -- Term -> Term
    termSeq2Term,     -- [Term] -> Term
    getTermFromStr,   -- String -> Term
    sortTerms,   -- [Term] -> [Term]
    doCombAxiom,      -- Term -> (Term, Bool)
    oneStepReduct,    -- Term -> (Term, Bool)
    reduct,      -- Int -> Int - > Term -> Term

    SimpleType(..),   -- Simple Type for CL terms
    isBasicType,      -- SimpleType -> Bool
    isImplicationalType,   -- SimpleType -> Bool
    antecedent,       -- SimpleType -> Maybe SimpleType
    consequent,       -- SimpleType -> Maybe SimpleType
    isStrOfSimpleType,     -- String -> Bool
    indexOfArrow,     -- Int -> Int -> String -> Int
    indexOfArrow',    -- Int -> Int -> String -> Int
    getSimpleTypeFromStr,   -- String -> SimpleType
    getCanonicalStrOfSimpleType,   -- String -> String

    LambdaTerm(..),  -- Term | Lambda AvVar LambdaTerm
    isStrOfLambdaTerm,      -- String -> Bool
    getLTermFromStr, -- String -> LambdaTerm
    getLTermFromSimpleType, -- Int -> Map LambdaTerm SimpleType -> SimpleType -> IO (Maybe LambdaTerm)
  ) where

import Utils
import Data.List (isPrefixOf)
import Data.List.Utils (replace)
import Data.Map (Map)
import qualified Data.Map as Map
import Text.Regex.Posix

-- Terms are divided into constant, variable and juxtapositioned ones.
data Term = ConstTerm String | VarTerm String | JuxTerm Term Term deriving (Eq)

-- Null term is needed sometimes, and let it be ConstTerm "".
nullTerm :: Term
nullTerm = ConstTerm ""

-- Decide a term is a null term or not.
isNullTerm :: Term -> Bool
isNullTerm (ConstTerm "") = True
isNullTerm _ = False

-- Term type may be primitive or compound, and the primitive terms include constant and variable terms.
data TermType = PrimTerm | CompoundTerm deriving (Eq, Show)

-- Decide a term is primitive or not.
isPrimTerm :: Term -> Bool
isPrimTerm (ConstTerm _) = True
isPrimTerm (VarTerm _) = True
isPrimTerm _ = False

-- Decide a term is compound or not.
isCompoundTerm :: Term -> Bool
isCompoundTerm t = not $ isPrimTerm t

-- Decide a term is a constant or not.
isConstTerm :: Term -> Bool
isConstTerm (ConstTerm _) = True
isConstTerm _ = False

-- Decide a term is a variable or not.
isVarTerm :: Term -> Bool
isVarTerm (VarTerm _) = True
isVarTerm _ = False

-- Decide a term is a juXtaposition or not.
isJuxTerm :: Term -> Bool
isJuxTerm (JuxTerm _ _) = True
isJuxTerm _ = False

-- For juxtapositioned terms, fstTerm return the first term, the function term from view of point of application.
fstTerm :: Term -> Maybe Term
fstTerm (JuxTerm a _) = Just a
fstTerm _ = Nothing

-- For juxtapositioned terms, sndTerm return the second term, the parameter term from view of point of application.
sndTerm :: Term -> Maybe Term
sndTerm (JuxTerm _ a) = Just a
sndTerm _ = Nothing

{- Subterms of a term are recursively defined as follows.
 - (s1) M is a subterm of M;
 - (s2)	if M is a subterm of N or of P, then M is a subterm of NP.
 -}
subTermOfTerm :: Term -> [Term]
subTermOfTerm (ConstTerm t) = [ConstTerm t]
subTermOfTerm (VarTerm t) = [VarTerm t]
subTermOfTerm (JuxTerm a b) = [JuxTerm a b] ++ (subTermOfTerm a) ++ (subTermOfTerm b)

{- Get all free variables in a term.
 - x is a free variable of M iff x is a subterm of M and x is a variable term.
 -}
fvOfTerm :: Term -> [Term]
fvOfTerm t = [x | x <- subTermOfTerm t, isVarTerm x]

-- Define relation Ord between two Terms such that two terms can be compared.
instance Ord Term where
    ConstTerm a < ConstTerm b = a < b
    ConstTerm _ < VarTerm _ = True
    ConstTerm _ < JuxTerm _ _ = True
    VarTerm _ < ConstTerm _ = False
    VarTerm a < VarTerm b = a < b
    VarTerm _ < JuxTerm _ _ = True
    JuxTerm _ _ < ConstTerm _ = False
    JuxTerm _ _ < VarTerm _ = False
    JuxTerm a b < JuxTerm c d =  a < c || a == c && b < d
    ConstTerm a <= ConstTerm b = a <= b
    ConstTerm _ <= VarTerm _ = True
    ConstTerm _ <= JuxTerm _ _ = True
    VarTerm _ <= ConstTerm _ = False
    VarTerm a <= VarTerm b = a <= b
    VarTerm _ <= JuxTerm _ _ = True
    JuxTerm _ _ <= ConstTerm _ = False
    JuxTerm _ _ <= VarTerm _ = False
    JuxTerm a b <= JuxTerm c d =  a < c || a == c && b <= d

-- Define how a term shows.
instance Show Term where
    show (ConstTerm a) = a
    show (VarTerm a) = a
    show (JuxTerm a b) = "(" ++ show a ++ " " ++ show b ++ ")"

{- The following definition conceals the binary operation that conjoins the two terms M and N.
 - This operation is called application, and it is often denoted by juxtaposition, that is,
 - by placing its two arguments next to each other as in (M N).
 -}
app :: Term -> Term -> Term
app a b = JuxTerm a b

{- From base {S, K}, other combinators can be obtained. So S and K are axioms, and others are theorems.
 - But Stanford philosophy web article calls all combinators as axioms. Actually they all are functions.
 - The basic and usual combinators are considered in this module, in which combinator A is just the function application.
 -   Sxyz > xz(yz)      Kxy > x         Ix > x
 -   Bxyz > x(yz)       Txy > yx        Cxyz > xzy
 -   Wxy > xyy          Mx > xx         Yx > x(Yx)
 -   Jxyzv > xy(xvz)    B'xyz > y(xz)   Vxyz > zxy
 -   S'xyz > yz(xz)     Rxyz > yzx      Axy > xy
 -   B3xyzw > x(yzw)    B3'xyzw > y(xzw)    Uxyzw > xzwy
 - here, B3 denoes BBB, B3' denotes C(BBB).
 -}
type CombAxiom = Term -> Term
combAxiom :: [CombAxiom]
combAxiom = [sAxiom, kAxiom, iAxiom, bAxiom, tAxiom, cAxiom, wAxiom, mAxiom, yAxiom, jAxiom, b'Axiom, vAxiom, s'Axiom, rAxiom, aAxiom, b3Axiom, b3'Axiom, uAxiom]

-- Decide a term is a saturated combinator S or not, namely combinator S is followed by three terms.
isSTerm :: Term -> Bool
isSTerm (JuxTerm (JuxTerm (JuxTerm (ConstTerm "S") _) _) _) = True
isSTerm _ = False

-- Execute Axiom S on a term. If the term is a saturated combinator S, the S reduction is done. Otherwise, no changes happen to this term.
sAxiom :: Term -> Term
sAxiom (JuxTerm (JuxTerm (JuxTerm (ConstTerm "S") x) y) z) = JuxTerm (JuxTerm x z) (JuxTerm y z)
sAxiom t = t

-- Decide a term is a saturated combinator K or not, namely combinator K is followed by two terms.
isKTerm :: Term -> Bool
isKTerm (JuxTerm (JuxTerm (ConstTerm "K") _) _) = True
isKTerm _ = False

-- Execute Axiom K on a term. If the term is a saturated combinator K, the K reduction is done. Otherwise, no changes happen to this term.
kAxiom :: Term -> Term
kAxiom (JuxTerm (JuxTerm (ConstTerm "K") x) y) = JuxTerm x y
kAxiom t = t

-- Decide a term is a saturated combinator I or not, namely combinator I is followed by one term.
isITerm :: Term -> Bool
isITerm (JuxTerm (ConstTerm "I") _) = True
isITerm _ = False

-- Execute Axiom I on a term. If the term is a saturated combinator I, the I reduction is done. Otherwise, no changes happen to this term.
iAxiom :: Term -> Term
iAxiom (JuxTerm (ConstTerm "I") x) = x
iAxiom t = t

-- Decide a term is a saturated combinator B or not, namely combinator B is followed by three terms.
isBTerm :: Term -> Bool
isBTerm (JuxTerm (JuxTerm (JuxTerm (ConstTerm "B") _) _) _) = True
isBTerm _ = False

-- Execute Axiom B on a term. If the term is a saturated combinator B, the B reduction is done. Otherwise, no changes happen to this term.
bAxiom :: Term -> Term
bAxiom (JuxTerm (JuxTerm (JuxTerm (ConstTerm "B") x) y) z) = JuxTerm x (JuxTerm y z)
bAxiom t = t

-- Decide a term is a saturated combinator T or not, namely combinator T is followed by two terms.
isTTerm :: Term -> Bool
isTTerm (JuxTerm (JuxTerm (ConstTerm "T") _) _) = True
isTTerm _ = False

-- Execute Axiom T on a term. If the term is a saturated combinator T, the T reduction is done. Otherwise, no changes happen to this term.
tAxiom :: Term -> Term
tAxiom (JuxTerm (JuxTerm (ConstTerm "T") x) y) = JuxTerm y x
tAxiom t = t

-- Decide a term is a saturated combinator C or not, namely combinator C is followed by three terms.
isCTerm :: Term -> Bool
isCTerm (JuxTerm (JuxTerm (JuxTerm (ConstTerm "C") _) _) _) = True
isCTerm _ = False

-- Execute Axiom C on a term. If the term is a saturated combinator C, the C reduction is done. Otherwise, no changes happen to this term.
cAxiom :: Term -> Term
cAxiom (JuxTerm (JuxTerm (JuxTerm (ConstTerm "C") x) y) z) = JuxTerm x (JuxTerm z y)
cAxiom t = t

-- Decide a term is a saturated combinator W or not, namely combinator W is followed by two terms.
isWTerm :: Term -> Bool
isWTerm (JuxTerm (JuxTerm (ConstTerm "W") _) _) = True
isWTerm _ = False

-- Execute Axiom W on a term. If the term is a saturated combinator W, the W reduction is done. Otherwise, no changes happen to this term.
wAxiom :: Term -> Term
wAxiom (JuxTerm (JuxTerm (ConstTerm "W") x) y) = JuxTerm (JuxTerm x y) y
wAxiom t = t

-- Decide a term is a saturated combinator M or not, namely combinator M is followed by one term.
isMTerm :: Term -> Bool
isMTerm (JuxTerm (ConstTerm "M") _) = True
isMTerm _ = False

-- Execute Axiom M on a term. If the term is a saturated combinator M, the M reduction is done. Otherwise, no changes happen to this term.
mAxiom :: Term -> Term
mAxiom (JuxTerm (ConstTerm "M") x) = JuxTerm x x
mAxiom t = t

-- Decide a term is a saturated combinator Y or not, namely combinator Y is followed by one term.
isYTerm :: Term -> Bool
isYTerm (JuxTerm (ConstTerm "Y") _) = True
isYTerm _ = False

-- Execute Axiom Y on a term. If the term is a saturated combinator Y, the Y reduction is done. Otherwise, no changes happen to this term.
yAxiom :: Term -> Term
yAxiom (JuxTerm (ConstTerm "Y") x) = JuxTerm x (JuxTerm (ConstTerm "Y") x)
yAxiom t = t

-- Decide a term is a saturated combinator J or not, namely combinator J is followed by four terms.
isJTerm :: Term -> Bool
isJTerm (JuxTerm (JuxTerm (JuxTerm (JuxTerm (ConstTerm "J") _) _) _) _) = True
isJTerm _ = False

-- Execute Axiom J on a term. If the term is a saturated combinator J, the J reduction is done. Otherwise, no changes happen to this term.
jAxiom :: Term -> Term
jAxiom (JuxTerm (JuxTerm (JuxTerm (JuxTerm (ConstTerm "J") x) y) z) v) = JuxTerm (JuxTerm x y) (JuxTerm (JuxTerm x v) z)
jAxiom t = t

-- Decide a term is a saturated combinator B' or not, namely combinator B' is followed by three terms.
isB'Term :: Term -> Bool
isB'Term (JuxTerm (JuxTerm (JuxTerm (ConstTerm "B'") _) _) _) = True
isB'Term _ = False

-- Execute Axiom B' on a term. If the term is a saturated combinator B', the B' reduction is done. Otherwise, no changes happen to this term.
b'Axiom :: Term -> Term
b'Axiom (JuxTerm (JuxTerm (JuxTerm (ConstTerm "B'") x) y) z) = JuxTerm y (JuxTerm x z)
b'Axiom t = t

-- Decide a term is a saturated combinator V or not, namely combinator V is followed by three terms.
isVTerm :: Term -> Bool
isVTerm (JuxTerm (JuxTerm (JuxTerm (ConstTerm "V") _) _) _) = True
isVTerm _ = False

-- Execute Axiom V on a term. If the term is a saturated combinator V, the V reduction is done. Otherwise, no changes happen to this term.
vAxiom :: Term -> Term
vAxiom (JuxTerm (JuxTerm (JuxTerm (ConstTerm "V") x) y) z) = JuxTerm z (JuxTerm x y)
vAxiom t = t

-- Decide a term is a saturated combinator S' or not, namely combinator S' is followed by three terms.
isS'Term :: Term -> Bool
isS'Term (JuxTerm (JuxTerm (JuxTerm (ConstTerm "S'") _) _) _) = True
isS'Term _ = False

-- Execute Axiom S' on a term. If the term is a saturated combinator S', the S' reduction is done. Otherwise, no changes happen to this term.
s'Axiom :: Term -> Term
s'Axiom (JuxTerm (JuxTerm (JuxTerm (ConstTerm "S'") x) y) z) = JuxTerm (JuxTerm y z) (JuxTerm x z)
s'Axiom t = t

-- Decide a term is a saturated combinator R or not, namely combinator R is followed by three terms.
isRTerm :: Term -> Bool
isRTerm (JuxTerm (JuxTerm (JuxTerm (ConstTerm "R") _) _) _) = True
isRTerm _ = False

-- Execute Axiom R on a term. If the term is a saturated combinator R, the R reduction is done. Otherwise, no changes happen to this term.
rAxiom :: Term -> Term
rAxiom (JuxTerm (JuxTerm (JuxTerm (ConstTerm "R") x) y) z) = JuxTerm y (JuxTerm z x)
rAxiom t = t

-- Decide a term is a saturated combinator A or not, namely combinator A is followed by two terms.
isATerm :: Term -> Bool
isATerm (JuxTerm (JuxTerm (ConstTerm "A") _) _) = True
isATerm _ = False

-- Execute Axiom A on a term. If the term is a saturated combinator A, the A reduction is done. Otherwise, no changes happen to this term.
aAxiom :: Term -> Term
aAxiom (JuxTerm (JuxTerm (ConstTerm "A") x) y) = JuxTerm x y
aAxiom t = t

-- Decide a term is a saturated combinator B3 (namely BBB) or not, namely combinator B3 is followed by four terms.
isB3Term :: Term -> Bool
isB3Term (JuxTerm (JuxTerm (JuxTerm (JuxTerm (ConstTerm "B3") _) _) _) _) = True
isB3Term _ = False

-- Execute Axiom B3 on a term. If the term is a saturated combinator B3, the B3 reduction is done. Otherwise, no changes happen to this term.
b3Axiom :: Term -> Term
b3Axiom (JuxTerm (JuxTerm (JuxTerm (JuxTerm (ConstTerm "B3") x) y) z) w)= JuxTerm x (JuxTerm (JuxTerm y z) w)
b3Axiom t = t

-- Decide a term is a saturated combinator B3' (namely C(BBB)) or not, namely combinator B3' is followed by four terms.
isB3'Term :: Term -> Bool
isB3'Term (JuxTerm (JuxTerm (JuxTerm (JuxTerm (ConstTerm "B3'") _) _) _) _) = True
isB3'Term _ = False

-- Execute Axiom B3' on a term. If the term is a saturated combinator B3', the B3' reduction is done. Otherwise, no changes happen to this term.
b3'Axiom :: Term -> Term
b3'Axiom (JuxTerm (JuxTerm (JuxTerm (JuxTerm (ConstTerm "B3'") x) y) z) w)= JuxTerm y (JuxTerm (JuxTerm x z) w)
b3'Axiom t = t

-- Decide a term is a saturated combinator U (namely BC(BB)) or not, namely combinator U is followed by four terms.
isUTerm :: Term -> Bool
isUTerm (JuxTerm (JuxTerm (JuxTerm (JuxTerm (ConstTerm "U") _) _) _) _) = True
isUTerm _ = False

-- Execute Axiom U on a term. If the term is a saturated combinator U, the U reduction is done. Otherwise, no changes happen to this term.
uAxiom :: Term -> Term
uAxiom (JuxTerm (JuxTerm (JuxTerm (JuxTerm (ConstTerm "U") x) y) z) w)= JuxTerm (JuxTerm (JuxTerm x z) w) y
uAxiom t = t

-- Create a term by a sequence of terms, and the sequence is considered as left combination priority.
termSeq2Term :: [Term] -> Term
termSeq2Term [] = nullTerm
termSeq2Term [x] = x
termSeq2Term (x:xs) = JuxTerm (x) (termSeq2Term xs)

{- Suppose the well-formated string of a term is strict, namely not including any redundant space character.
 - (1) A null string is well-formatted;
 - (2) A string satisfies the following conditions is well-formatted: Starts with not '(' and contain no space character.
 - (3) A string satisfies the following conditions is well-formatted: Starts with '(', ends with ')',
 -     and contains two juxtapositioned string which are well-formatted and seperated by one space character;
 - (4) Other strings except the above are not well-formatted.
 - To now, single character or character string is considered constant term instead of variable term.
 -}
getTermFromStr :: String -> Term
getTermFromStr "" = nullTerm
getTermFromStr [x] = ConstTerm [x]
getTermFromStr str
    | head str /= '(' && indexOfDelimiter 0 0 0 ' ' str == -1 = ConstTerm str
    | head str == '(' && last str == ')' && spaceIdx > 0 && spaceIdx < length str' - 1 = JuxTerm (getTermFromStr mStr) (getTermFromStr nStr)
    | otherwise = error $ "getTermFromStr: Malformat at '" ++ str ++ "'"
    where
    str' = init (tail str)
    spaceIdx = indexOfDelimiter 0 0 0 ' ' str'
    mStr = take spaceIdx str'
    nStr = drop (spaceIdx + 1) str'

{- Sort terms according to non-descending order.
 - It should be replaced with function 'sort' in Data.List.
 -}
sortTerms :: [Term] -> [Term]
sortTerms [] = []
sortTerms [x] = [x]
sortTerms (x:xs) = (sortTerms [y | y <- xs, y <= x]) ++ [x] ++ (sortTerms [y | y <- xs, x < y])

{- Do a certain combinatory axiom on a term.
 - If this term is a redex, namely a certain saturated combinator, return reduction result and True.
 - Otherwise, return this term and False.
 -}
doCombAxiom :: Term -> (Term, Bool)
doCombAxiom t
    | isATerm t = (aAxiom t, True)
    | isSTerm t = (sAxiom t, True)
    | isKTerm t = (kAxiom t, True)
    | isITerm t = (iAxiom t, True)
    | isBTerm t = (bAxiom t, True)
    | isTTerm t = (tAxiom t, True)
    | isCTerm t = (cAxiom t, True)
    | isWTerm t = (wAxiom t, True)
    | isMTerm t = (mAxiom t, True)
    | isYTerm t = (yAxiom t, True)
    | isJTerm t = (jAxiom t, True)
    | isB'Term t = (b'Axiom t, True)
    | isVTerm t = (vAxiom t, True)
    | isS'Term t = (s'Axiom t, True)
    | isRTerm t = (rAxiom t, True)
    | isB3Term t = (b3Axiom t, True)
    | isB3'Term t = (b3'Axiom t, True)
    | isUTerm t = (uAxiom t, True)
    | otherwise = (t, False)

{- Do one-step reduction on a term.
 - If this term has any redex, choose first redex by preorder traversal to reduct, return reduction result and True.
 - Otherwise, return this term and False.
 -}
oneStepReduct :: Term -> (Term, Bool)
oneStepReduct (ConstTerm t) = (ConstTerm t, False)
oneStepReduct (VarTerm t) = (VarTerm t, False)
oneStepReduct (JuxTerm t1 t2)
    | flag == True = (res, flag)
    | flag1 == True = (JuxTerm (res1) t2, flag1)
    | flag2 == True = (JuxTerm t1 (res2), flag2)
    | otherwise = (JuxTerm t1 t2, False)
    where
      (res, flag) = doCombAxiom (JuxTerm t1 t2)
      (res1, flag1) = doCombAxiom t1
      (res2, flag2) = doCombAxiom t2

{- Reduct a term till there is no redex inside it, or a maximal number of reduction steps is reached.
 - Even if a term has a normal form, reducting this term might not terminate.
 - So, the maximal number of reduction steps is needed, which is idxMax.
 -}
reduct :: Int -> Int -> Term -> Term
reduct idx idxMax t
    | idx >= idxMax = t
    | otherwise = case flag of
                    True -> reduct (idx + 1) idxMax t'
                    False -> t
    where
      (t', flag) = oneStepReduct t

{- Define simple type for CL terms.
 - Given a set of basic types (that we denote by P), types are defined as follows.
 - If p <- P then p is a type;
 - If A, B are types then (A -> B) is a type.
 - Constructor Basic creates basic type, for instances, Basic "A", Basic "s", and Basic "np".
 - Constructor Implicational creates implicational type, for instances, Implicational (Basic "A") (Basic "B").
 -}
data SimpleType = Basic String | Implicational SimpleType SimpleType deriving (Eq)

-- Define relation Ord between two simple types such that two types can be compared.
instance Ord SimpleType where
    Basic a < Basic b = a < b
    Basic _ < Implicational _ _ = True
    Implicational _ _ < Basic _ = False
    Implicational a b < Implicational c d = (a < c) || (a == c && b < d)
    Basic a <= Basic b = a < b || a == b
    Basic _ <= Implicational _ _ = True
    Implicational _ _ <= Basic _ = False
    Implicational a b <= Implicational c d = (Implicational a b < Implicational c d) || (Implicational a b == Implicational c d)

-- Define how a simple type shows as a letter string, where no brackets are omitted.
instance Show SimpleType where
    show (Basic t) = t
    show (Implicational t1 t2) = "(" ++ (show t1) ++ " -> " ++ (show t2) ++ ")"

isBasicType :: SimpleType -> Bool
isBasicType (Basic _) = True
isBasicType _ = False

isImplicationalType :: SimpleType -> Bool
isImplicationalType (Implicational _ _) = True
isImplicationalType _ = False

{- Besides interior functions, data constructors are not seen from outside of modules.
 - To have access to these constructors, related functions are defined.
 -}

-- Get antecedent of an implicational formula.
antecedent :: SimpleType -> Maybe SimpleType
antecedent (Basic _) = Nothing
antecedent (Implicational ante _) = Just ante

-- Get consequent of an implicational formula.
consequent :: SimpleType -> Maybe SimpleType
consequent (Basic _) = Nothing
consequent (Implicational _ cons) = Just cons

{- Decide whether a string is well-formatted for a simple type.
 - "A" is a simple type, "(A -> B)" is a simple type, and "((A -> B) -> C)" is also simple type.
 -}
isStrOfSimpleType :: String -> Bool
isStrOfSimpleType str
    | indexOfArrow 0 0 str == -1 = str =~ "^[A-Za-z]+$"
    | otherwise = isStrOfSimpleType anteStr && isStrOfSimpleType consStr
    where
      idx = indexOfArrow 0 0 str
      anteStr = tail $ take idx str
      consStr = init $ drop (idx + 4) str

{- Get the index of " -> ", which will be -1 for basic types.
 - To remember how many left brackets have been met, the integer nlb is needed.
 - The index is initialized as 0.
 - If there are the outest brackets, they are NOT omitted.
 -}
indexOfArrow :: Int -> Int -> String -> Int
indexOfArrow nlb i str
    | i > length str - 4 = -1
    | x == '(' = indexOfArrow (nlb + 1) (i+1) str
    | x == ')' = indexOfArrow (nlb - 1) (i+1) str
    | arrow == " -> " && nlb == 1 = i
    | otherwise = indexOfArrow nlb (i+1) str
        where
        x = str!!i
        arrow = take 4 $ drop i str   -- Substring with lenghth 4 beginning at index i

{- Get the index of " -> ", which will be -1 for basic types.
 - To remember how many left brackets have been met, the integer nlb is needed.
 - The index is initialized as 0.
 - If there are the outest brackets, they are omitted..
 -}
indexOfArrow' :: Int -> Int -> String -> Int
indexOfArrow' nlb i str
    | i > length str - 4 = -1
    | x == '(' = indexOfArrow' (nlb + 1) (i+1) str
    | x == ')' = indexOfArrow' (nlb - 1) (i+1) str
    | arrow == " -> " && nlb == 0 = i
    | otherwise = indexOfArrow' nlb (i+1) str
        where
        x = str!!i
        arrow = take 4 $ drop i str   -- Substring with lenghth 4 beginning at index i

{- Get a simple type from its string.
 - The format of string conforms the output of show command on the simple type.
 - Basic type such as "A", and Implicational type such as "(A -> B)".
 -}
getSimpleTypeFromStr :: String -> SimpleType
getSimpleTypeFromStr str
    | not (isStrOfSimpleType str) = error $ "getSimpleTypeFromStr: Format error in " ++ str
    | head str /= '(' && last str /= ')' = Basic str
    | otherwise = Implicational (getSimpleTypeFromStr anteStr) (getSimpleTypeFromStr consStr)
      where
        idx = indexOfArrow 0 0 str
        anteStr = tail $ take idx str
        consStr = init $ drop (idx + 4) str

{- Remedy a string of a simple type such that no brackets are omitted, that is, it conforms the output of Show command on this type.
 - (1) every arrow "->" is seperated to adjacent type names by one space,
 - (2) all brackets '(' and ')' omitted by priority of right association are added.
 - The result string may be called canonical string.
 - Algo: If the checked string is of a basic type
 -         then return the checked string
 -         else ensure the checked string bracketed with '(' and ')'
 -              recursively check the antecedent and the consequent of this implicational type.
 -}
getCanonicalStrOfSimpleType :: String -> IO String
getCanonicalStrOfSimpleType str =
    if str =~ "^[A-Za-z][A-Za-z0-9]*$"
      then return str                                -- Basic type
      else do
        let str1 = filter (/= ' ') str               -- Throw off space chars
        let str2 = replace "->" " -> " str1          -- Implement (1)
--        putStrLn $ "str2 = " ++ str2 ++ ", indexOfArrow' = " ++ show (indexOfArrow' 0 0 str2)
        let str3 = case indexOfArrow' 0 0 str2 of
                     -1 -> str2                      -- Outest brackets are there
                     _ -> "(" ++ str2 ++ ")"
        let idx = indexOfArrow 0 0 str3
--        putStrLn $ "idx = " ++ show idx ++ ", str3 = " ++ str3
        let ante = tail $ take idx str3
        let cons = init $ drop (idx + 4) str3
        ante' <- getCanonicalStrOfSimpleType ante
        cons' <- getCanonicalStrOfSimpleType cons
        return $ "(" ++ ante' ++ " -> " ++ cons' ++ ")"

{- Lambda term
 - A variable is a term, a lambda abstract is a term, and application of one term to another is also a term.
 -}
data LambdaTerm = Var String | Lambda String LambdaTerm | Apply LambdaTerm LambdaTerm deriving Eq

{- Define how a lambda term shows.
 - For variable terms, show them literally, such as "x", "x12", etc.
 - For abstracted terms, show as "(\v. t)".
 - For application terms, show as "(M N)".
 -}
instance Show LambdaTerm where
    show (Var t) = t
    show (Lambda v t) = "(\\" ++ v ++ ". " ++ show t ++ ")"
    show (Apply t1 t2) = "(" ++ show t1 ++ " " ++ show t2 ++ ")"

-- Define relation Ord between two lambda terms.
instance Ord LambdaTerm where
    Var a < Var b = a < b
    Var _ < Lambda _ _ = True
    Var _ < Apply _ _ = True
    Lambda _ a < Lambda _ b = a < b
    Lambda _ _ < Apply _ _ = True
    Apply a b < Apply c d = a < c || a == c && b < d
    Var a <= Var b = a <= b
    Var _ <= Lambda _ _ = True
    Var _ <= Apply _ _ = True
    Lambda _ a <= Lambda _ b = a <= b
    Lambda _ _ <= Apply _ _ = True
    Apply a b <= Apply c d = a <= c || a == c && b <= d

-- Decide a string as well-formatted or not if for a lambda term.
isStrOfLambdaTerm :: String -> Bool
isStrOfLambdaTerm str
    | str =~ "^[A-Za-z][A-Za-z0-9]*$" = True
    | str =~ "^\\(\\\\[a-z][a-z0-9]*\\.[ ][A-Za-z][A-Za-z0-9]*\\)$" = True
    | str =~ "^\\([A-Za-z][A-Za-z0-9]*[ ][A-Za-z][A-Za-z0-9]*\\)$" = True
    | otherwise = False

{- Get lambda term from its string.
 - Variable term: x, x1, ...
 - Abstracted term: (\x. t)
 - Apply term: (M N)
 -}
getLTermFromStr :: String -> LambdaTerm
getLTermFromStr str
    | not (isStrOfLambdaTerm str) = error $ "getLTermFromStr: Format error: " ++ str
    | head str /= '(' = Var str
    | not (isPrefixOf "(\\" str) = Apply (getLTermFromStr (tail t1Str)) (getLTermFromStr (init t2Str))
    | otherwise = Lambda (init (drop 2 t1Str)) (getLTermFromStr (init t2Str))
    where
       ws = words str
       t1Str = ws!!0
       t2Str = ws!!1

{- Get lambda term from its simple type under context of {(LambdaTerm : SimpleType)}.
 - To get closed lambda term, the initial context should be empty.
 - To ensure a new variable is selected, an integer counter is used whose initial value may be 0.
 - If no lambda term exists, return Nothing.
 - Algo.
 -   if the asked type is T = Implicational A B, then x:A is introduced into the context, and
 -     recursively construct term t for type B, return \x. t.
 -   else -- The asked type is Basic T
 -     look up term h with type T in current context.
 -     if success, then return h;
 -     else
 -       For each term g with type U -> T in current context,
 -       recursively construct term u for type U.
 -       if success, then return g u;
 -       else return Nothing       -- No inhabitant for this type
 - Backtracing is needed whenever constructing term u fails.
 - If initial context is empty, only closed lambda terms can be constructed.
 -}
getLTermFromSimpleType :: Int -> Map LambdaTerm SimpleType -> SimpleType -> IO (Maybe LambdaTerm)
getLTermFromSimpleType intCnt termTypeContext (Implicational ante cons) = do
    let abVar = "x" ++ show intCnt                                        -- New variable
    let newContext = Map.insert (Var abVar) ante termTypeContext          -- Introduce new variable and its type into context
--    putStrLn $ "[INFO] New context member: " ++ abVar ++ ":" ++ show ante
    term <- getLTermFromSimpleType (intCnt + 1) newContext cons           -- Construct term of new target type in new context
    case term of
      Just t -> return (Just (Lambda abVar t))                            -- \x. t
      Nothing -> return Nothing
getLTermFromSimpleType intCnt termTypeContext (Basic target) = do
    let termTargetList = Map.toList $ Map.filterWithKey (\k v -> v == Basic target) termTypeContext    -- [(term1, Basic target), ..]
    if termTargetList /= []
      then do
        putStrLn $ show (length termTargetList) ++ " term(s) is(are) found, and they are:\n" ++ show termTargetList ++"\nOnly first term is returned."
        return (Just (fst (termTargetList!!0)))                           -- Just LambdaTerm, the first term with type target
      else do
        let termUTList = Map.toList $ Map.filterWithKey (\k v -> case consequent v of
                                                                   Just t -> t == Basic target
                                                                   Nothing -> False
                                                        ) termTypeContext    -- [(term1, U1->T), ..]
        putStrLn $ "getLTermFromSimpleType: termUTList: " ++ show termUTList
        term <- processTermUTList termUTList intCnt termTypeContext
        return term
          where
            processTermUTList :: [(LambdaTerm, SimpleType)] -> Int -> Map LambdaTerm SimpleType -> IO (Maybe LambdaTerm)
            processTermUTList [] _ _ = return Nothing
            processTermUTList ((g, typeUT):tuts) intCnt termTypeContext = do
                u <- getLTermFromSimpleType intCnt termTypeContext (case antecedent typeUT of
                                                                      Just t -> t
                                                                      Nothing -> error "getLTermFromSimpleType: Impossible"
                                                                   )     -- To get term u with type U
                case u of
                  Just x -> return (Just (Apply g x))
                  Nothing -> processTermUTList tuts intCnt termTypeContext
